using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using dnlib.DotNet;

namespace ICSharpCode.Decompiler
{
	public static class DotNetCorePathFinderExtensions
	{
		static readonly string RefPathPattern =
			@"(Reference Assemblies[/\\]Microsoft[/\\]Framework[/\\](?<1>.NETFramework)[/\\]v(?<2>[^/\\]+)[/\\])" +
			@"|(NuGetFallbackFolder[/\\](?<1>[^/\\]+)\\(?<2>[^/\\]+)([/\\].*)?[/\\]ref[/\\])";

		public static string DetectTargetFrameworkId(this ModuleDefMD module, string assemblyPath = null)
		{
			if (module.Assembly != null) {
				if (module.Assembly.TryGetOriginalTargetFrameworkAttribute(out var fw, out var version2, out var profile)) {
					if (profile is null)
						return fw + ",Version=v" + version2;
					return fw + ",Version=v" + version2 + ",Profile=" + profile;
				}
			}

			foreach (var r in module.GetAssemblyRefs())
			{
				try
				{
					if (r.PublicKeyOrToken.IsNullOrEmpty)
						continue;
					string version;
					switch (r.Name)
					{
						case "netstandard":
							version = r.Version.ToString(3);
							return $".NETStandard,Version=v{version}";
						case "System.Runtime":
							// System.Runtime.dll uses the following scheme:
							// 4.2.0 => .NET Core 2.0
							// 4.2.1 => .NET Core 2.1 / 3.0
							// 4.2.2 => .NET Core 3.1
							if (r.Version >= new Version(4, 2, 0))
							{
								version = "2.0";
								if (r.Version >= new Version(4, 2, 1))
								{
									version = "3.0";
								}
								if (r.Version >= new Version(4, 2, 2))
								{
									version = "3.1";
								}
								return $".NETCoreApp,Version=v{version}";
							}
							else
							{
								continue;
							}
						case "mscorlib":
							version = r.Version.ToString(2);
							return $".NETFramework,Version=v{version}";
					}
				}
				catch (BadImageFormatException)
				{
					// ignore malformed references
				}
			}

			// Optionally try to detect target version through assembly path as a fallback (use case: reference assemblies)
			if (assemblyPath != null) {
				/*
				 * Detected path patterns (examples):
				 *
				 * - .NETFramework -> C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\mscorlib.dll
				 * - .NETCore      -> C:\Program Files\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.1.0\ref\netcoreapp2.1\System.Console.dll
				 * - .NETStandard  -> C:\Program Files\dotnet\sdk\NuGetFallbackFolder\netstandard.library\2.0.3\build\netstandard2.0\ref\netstandard.dll
				 */
				var pathMatch = Regex.Match(assemblyPath, RefPathPattern,
					RegexOptions.IgnoreCase | RegexOptions.Compiled | RegexOptions.ExplicitCapture);
				if (pathMatch.Success) {
					var type = pathMatch.Groups[1].Value;
					var version = pathMatch.Groups[2].Value;

					if (type == ".NETFramework") {
						return $".NETFramework,Version=v{version}";
					} else if (type.Contains("netcore")) {
						return $".NETCoreApp,Version=v{version}";
					} else if (type.Contains("netstandard")) {
						return $".NETStandard,Version=v{version}";
					}
				}
			}

			return string.Empty;
		}
	}

	public class ReferenceLoadInfo
	{
		readonly Dictionary<string, UnresolvedAssemblyNameReference> loadedAssemblyReferences = new Dictionary<string, UnresolvedAssemblyNameReference>();

		public void AddMessage(string fullName, MessageKind kind, string message)
		{
			lock (loadedAssemblyReferences) {
				if (!loadedAssemblyReferences.TryGetValue(fullName, out var referenceInfo)) {
					referenceInfo = new UnresolvedAssemblyNameReference(fullName);
					loadedAssemblyReferences.Add(fullName, referenceInfo);
				}
				referenceInfo.Messages.Add((kind, message));
			}
		}

		public void AddMessageOnce(string fullName, MessageKind kind, string message)
		{
			lock (loadedAssemblyReferences) {
				if (!loadedAssemblyReferences.TryGetValue(fullName, out var referenceInfo)) {
					referenceInfo = new UnresolvedAssemblyNameReference(fullName);
					loadedAssemblyReferences.Add(fullName, referenceInfo);
					referenceInfo.Messages.Add((kind, message));
				} else {
					var lastMsg = referenceInfo.Messages.LastOrDefault();
					if (kind != lastMsg.Item1 && message != lastMsg.Item2)
						referenceInfo.Messages.Add((kind, message));
				}
			}
		}

		public bool TryGetInfo(string fullName, out UnresolvedAssemblyNameReference info)
		{
			lock (loadedAssemblyReferences) {
				return loadedAssemblyReferences.TryGetValue(fullName, out info);
			}
		}

		public bool HasErrors {
			get {
				lock (loadedAssemblyReferences) {
					return loadedAssemblyReferences.Any(i => i.Value.HasErrors);
				}
			}
		}
	}
}
