using System;
using System.Collections.Generic;
using System.IO;
using dnlib.DotNet;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem
{
	/// <summary>
	/// PEFile is the main class the decompiler uses to represent a metadata assembly/module.
	/// Every file on disk can be loaded into a standalone PEFile instance.
	///
	/// A PEFile can be combined with its referenced assemblies/modules to form a type system,
	/// in that case the <see cref="MetadataModule"/> class is used instead.
	/// </summary>
	/// <remarks>
	/// In addition to wrapping a <c>System.Reflection.Metadata.PEReader</c>, this class
	/// contains a few decompiled-specific caches to allow efficiently constructing a type
	/// system from multiple PEFiles. This allows the caches to be shared across multiple
	/// decompiled type systems.
	/// </remarks>
	public class PEFile : IDisposable, IModuleReference
	{
		public ModuleDef Module { get; }

		public PEFile(string fileName)
			: this(new FileStream(fileName, FileMode.Open, FileAccess.Read))
		{
		}

		public PEFile(Stream stream)
			: this(ModuleDefMD.Load(stream))
		{
		}

		public PEFile(ModuleDef reader)
		{
			this.Module = reader ?? throw new ArgumentNullException(nameof(reader));
		}

		public void Dispose()
		{
			Module.Dispose();
		}

		Dictionary<TopLevelTypeName, TypeDef> typeLookup;

		/// <summary>
		/// Finds the top-level-type with the specified name.
		/// </summary>
		public TypeDef GetTypeDefinition(TopLevelTypeName typeName)
		{
			var lookup = LazyInit.VolatileRead(ref typeLookup);
			if (lookup == null) {
				lookup = new Dictionary<TopLevelTypeName, TypeDef>();
				foreach (var td in Module.Types) {
					if (td.IsNested) {
						continue; // nested type
					}
					string ns = td.Namespace;
					string name = ReflectionHelper.SplitTypeParameterCountFromReflectionName(td.Name, out int typeParameterCount);
					lookup[new TopLevelTypeName(ns, name, typeParameterCount)] = td;
				}
				lookup = LazyInit.GetOrSet(ref typeLookup, lookup);
			}
			if (lookup.TryGetValue(typeName, out var resultHandle))
				return resultHandle;
			else
				return default;
		}

		Dictionary<FullTypeName, ExportedType> typeForwarderLookup;

		/// <summary>
		/// Finds the type forwarder with the specified name.
		/// </summary>
		public ExportedType GetTypeForwarder(FullTypeName typeName)
		{
			var lookup = LazyInit.VolatileRead(ref typeForwarderLookup);
			if (lookup == null) {
				lookup = new Dictionary<FullTypeName, ExportedType>();
				foreach (var handle in Module.ExportedTypes) {
					string ns = handle.Namespace;
					string name = ReflectionHelper.SplitTypeParameterCountFromReflectionName(handle.Name, out int typeParameterCount);
					lookup[new FullTypeName(new TopLevelTypeName(ns, name, typeParameterCount))] = handle;
				}
				lookup = LazyInit.GetOrSet(ref typeForwarderLookup, lookup);
			}
			if (lookup.TryGetValue(typeName, out var resultHandle))
				return resultHandle;
			else
				return default;
		}


		public IModuleReference WithOptions(TypeSystemOptions options)
		{
			return new PEFileWithOptions(this, options);
		}

		IModule IModuleReference.Resolve(ITypeResolveContext context)
		{
			return new MetadataModule(context.Compilation, this, TypeSystemOptions.Default);
		}

		private class PEFileWithOptions : IModuleReference
		{
			readonly PEFile peFile;
			readonly TypeSystemOptions options;

			public PEFileWithOptions(PEFile peFile, TypeSystemOptions options)
			{
				this.peFile = peFile;
				this.options = options;
			}

			IModule IModuleReference.Resolve(ITypeResolveContext context)
			{
				return new MetadataModule(context.Compilation, peFile, options);
			}
		}
	}
}
