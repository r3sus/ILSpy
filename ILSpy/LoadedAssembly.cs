// Copyright (c) 2011 AlphaSierraPapa for the SharpDevelop Team
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Threading;
using dnlib.DotNet;
using ICSharpCode.Decompiler;
using ICSharpCode.ILSpy.Options;

namespace ICSharpCode.ILSpy
{
	/// <summary>
	/// Represents an assembly loaded into ILSpy.
	/// </summary>
	[DebuggerDisplay("[LoadedAssembly {shortName}]")]
	public sealed class LoadedAssembly
	{
		readonly Task<dnlib.DotNet.ModuleDef> assemblyTask;
		readonly AssemblyList assemblyList;
		readonly string fileName;
		readonly string shortName;

		public LoadedAssembly(AssemblyList assemblyList, string fileName, Stream stream = null)
		{
			if (assemblyList == null)
				throw new ArgumentNullException(nameof(assemblyList));
			if (fileName == null)
				throw new ArgumentNullException(nameof(fileName));
			this.assemblyList = assemblyList;
			this.fileName = fileName;

			this.assemblyTask = Task.Factory.StartNew<dnlib.DotNet.ModuleDef>(LoadAssembly, stream); // requires that this.fileName is set
			this.shortName = Path.GetFileNameWithoutExtension(fileName);
		}

		/// <summary>
		/// Returns a target framework identifier in the form '&lt;framework&gt;Version=v&lt;version&gt;'.
		/// Returns an empty string if no TargetFrameworkAttribute was found or the file doesn't contain an assembly header, i.e., is only a module.
		/// </summary>
		public async Task<string> GetTargetFrameworkIdAsync()
		{
			var assembly = await GetAssemblyDefinitionAsync().ConfigureAwait(false);
			return assembly?.DetectTargetFrameworkId() ?? string.Empty;
		}

		public ReferenceLoadInfo LoadedAssemblyReferencesInfo { get; } = new ReferenceLoadInfo();

		/// <summary>
		/// Gets the Cecil ModuleDefinition.
		/// </summary>
		public Task<dnlib.DotNet.ModuleDef> GetModuleDefinitionAsync()
		{
			return assemblyTask;
		}

		/// <summary>
		/// Gets the Cecil ModuleDefinition.
		/// Returns null in case of load errors.
		/// </summary>
		public dnlib.DotNet.ModuleDef GetModuleDefinitionOrNull()
		{
			try {
				return GetModuleDefinitionAsync().Result;
			} catch (Exception ex) {
				System.Diagnostics.Trace.TraceError(ex.ToString());
				return null;
			}
		}

		/// <summary>
		/// Gets the Cecil AssemblyDefinition.
		/// </summary>
		public async Task<dnlib.DotNet.AssemblyDef> GetAssemblyDefinitionAsync()
		{
			var module = await assemblyTask.ConfigureAwait(false);
			return module != null ? module.Assembly : null;
		}

		/// <summary>
		/// Gets the Cecil AssemblyDefinition.
		/// Returns null when there was a load error; or when opening a netmodule.
		/// </summary>
		public dnlib.DotNet.AssemblyDef GetAssemblyDefinitionOrNull()
		{
			try {
				return GetAssemblyDefinitionAsync().Result;
			} catch (Exception ex) {
				System.Diagnostics.Trace.TraceError(ex.ToString());
				return null;
			}
		}

		public AssemblyList AssemblyList => assemblyList;

		public string FileName => fileName;

		public string ShortName => shortName;

		public string Text {
			get {
				if (IsLoaded && !HasLoadError) {
					string version = GetAssemblyDefinitionOrNull()?.Version.ToString();
					if (version == null)
						return ShortName;
					return String.Format("{0} ({1})", ShortName, version);
				} else {
					return ShortName;
				}
			}
		}

		public bool IsLoaded => assemblyTask.IsCompleted;

		public bool HasLoadError => assemblyTask.IsFaulted;

		public bool IsAutoLoaded { get; set; }

		ModuleDef LoadAssembly(object state)
		{
			var stream = state as Stream;
			ModuleDefMD module;

			// runs on background thread
			var assemblyResolver = new MyAssemblyResolver(this);
			var ctx = new ModuleContext(assemblyResolver);

			if (stream != null)
			{
				// Read the module from a precrafted stream
				module = ModuleDefMD.Load(stream, ctx);
			}
			else
			{
				// Read the module from disk (by default)
				module = ModuleDefMD.Load(fileName, ctx);
			}

			if (DecompilerSettingsPanel.CurrentDecompilerSettings.UseDebugSymbols) {
				try {
					LoadSymbols(module);
				} catch (IOException) {
				} catch (UnauthorizedAccessException) {
				} catch (InvalidOperationException) {
					// ignore any errors during symbol loading
				}
			}
			return module;
		}

		private void LoadSymbols(ModuleDefMD module)
		{
			// search for pdb in same directory as dll
			string pdbName = Path.Combine(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".pdb");
			if (File.Exists(pdbName)) {
				module.LoadPdb(pdbName);
				return;
			}

			// TODO: use symbol cache, get symbols from microsoft
		}

		[ThreadStatic]
		static int assemblyLoadDisableCount;

		public static IDisposable DisableAssemblyLoad()
		{
			assemblyLoadDisableCount++;
			return new DecrementAssemblyLoadDisableCount();
		}

		sealed class DecrementAssemblyLoadDisableCount : IDisposable
		{
			bool disposed;

			public void Dispose()
			{
				if (!disposed) {
					disposed = true;
					assemblyLoadDisableCount--;
					// clear the lookup cache since we might have stored the lookups failed due to DisableAssemblyLoad()
					MainWindow.Instance.CurrentAssemblyList.ClearCache();
				}
			}
		}

		sealed class MyAssemblyResolver : IAssemblyResolver
		{
			readonly LoadedAssembly parent;

			public MyAssemblyResolver(LoadedAssembly parent)
			{
				this.parent = parent;
			}

			public AssemblyDef Resolve(IAssembly assembly, ModuleDef sourceModule)
			{
				return parent.LookupReferencedAssembly(assembly)?.GetAssemblyDefinitionOrNull();
			}
		}

		public IAssemblyResolver GetAssemblyResolver()
		{
			return new MyAssemblyResolver(this);
		}

		public LoadedAssembly LookupReferencedAssembly(IAssembly name)
		{
			if (name == null)
				throw new ArgumentNullException(nameof(name));
			if (name.IsContentTypeWindowsRuntime) {
				return assemblyList.assemblyLookupCache.GetOrAdd((name.Name, true), key => LookupReferencedAssemblyInternal(name, true));
			} else {
				return assemblyList.assemblyLookupCache.GetOrAdd((name.FullName, false), key => LookupReferencedAssemblyInternal(name, false));
			}
		}

		class MyUniversalResolver : UniversalAssemblyResolver
		{
			public MyUniversalResolver(LoadedAssembly assembly)
				: base(assembly.FileName, false)
			{
			}
		}

		static Dictionary<string, LoadedAssembly> loadingAssemblies = new Dictionary<string, LoadedAssembly>();

		LoadedAssembly LookupReferencedAssemblyInternal(IAssembly fullName, bool isWinRT)
		{
			string GetName(IAssembly name) => isWinRT ? name.Name.String : name.FullName;

			string file;
			LoadedAssembly asm;
			lock (loadingAssemblies) {
				foreach (LoadedAssembly loaded in assemblyList.GetAssemblies()) {
					var asmDef = loaded.GetAssemblyDefinitionOrNull();
					if (asmDef != null && GetName(fullName).Equals(GetName(asmDef), StringComparison.OrdinalIgnoreCase)) {
						LoadedAssemblyReferencesInfo.AddMessageOnce(fullName.ToString(), MessageKind.Info, "Success - Found in Assembly List");
						return loaded;
					}
				}

				if (isWinRT) {
					file = Path.Combine(Environment.SystemDirectory, "WinMetadata", fullName.Name + ".winmd");
				} else {
					var resolver = new MyUniversalResolver(this) { TargetFramework = GetTargetFrameworkIdAsync().Result };
					file = resolver.FindAssemblyFile(fullName);
				}

				foreach (LoadedAssembly loaded in assemblyList.GetAssemblies()) {
					if (loaded.FileName.Equals(file, StringComparison.OrdinalIgnoreCase)) {
						return loaded;
					}
				}

				if (file != null && loadingAssemblies.TryGetValue(file, out asm))
					return asm;

				if (assemblyLoadDisableCount > 0)
					return null;

				if (file != null) {
					LoadedAssemblyReferencesInfo.AddMessage(fullName.ToString(), MessageKind.Info, "Success - Loading from: " + file);
					asm = new LoadedAssembly(assemblyList, file) { IsAutoLoaded = true };
				} else {
					LoadedAssemblyReferencesInfo.AddMessageOnce(fullName.ToString(), MessageKind.Error, "Could not find reference: " + fullName);
					return null;
				}
				loadingAssemblies.Add(file, asm);
			}
			App.Current.Dispatcher.BeginInvoke((Action)delegate() {
				lock (assemblyList.assemblies) {
					assemblyList.assemblies.Add(asm);
				}
				lock (loadingAssemblies) {
					loadingAssemblies.Remove(file);
				}
			});
			return asm;
		}

		public Task ContinueWhenLoaded(Action<Task<ModuleDef>> onAssemblyLoaded, TaskScheduler taskScheduler)
		{
			return this.assemblyTask.ContinueWith(onAssemblyLoaded, default(CancellationToken), TaskContinuationOptions.RunContinuationsAsynchronously, taskScheduler);
		}

		/// <summary>
		/// Wait until the assembly is loaded.
		/// Throws an AggregateException when loading the assembly fails.
		/// </summary>
		public void WaitUntilLoaded()
		{
			assemblyTask.Wait();
		}

	}
}
