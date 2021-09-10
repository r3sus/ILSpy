using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
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
	public sealed class PEFile : IDisposable, IModuleReference
	{
		public StringBuilder StringBuilder { get; }

		public ModuleDef Module { get; }

		public PEFile(ModuleDef reader)
		{
			StringBuilder = new StringBuilder();
			this.Module = reader ?? throw new ArgumentNullException(nameof(reader));
		}

		public void Dispose()
		{
			Module.Dispose();
		}

		/// <summary>
		/// Finds the top-level-type with the specified name.
		/// </summary>
		public TypeDef GetTypeDefinition(TopLevelTypeName typeName)
		{
			return Module.Find(typeName.GetReflectionName(StringBuilder), true);
		}

		Dictionary<FullTypeName, ExportedType> typeForwarderLookup;

		/// <summary>
		/// Finds the type forwarder with the specified name.
		/// </summary>
		public ExportedType GetTypeForwarder(FullTypeName typeName)
		{
			var lookup = LazyInit.VolatileRead(ref typeForwarderLookup);
			if (lookup == null) {
				lookup = new Dictionary<FullTypeName, ExportedType>(Module.ExportedTypes.Count);
				foreach (var handle in Module.ExportedTypes) {
					lookup[handle.GetFullTypeName()] = handle;
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
