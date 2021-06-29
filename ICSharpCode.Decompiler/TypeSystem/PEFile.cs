using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
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
	public class PEFile : IDisposable, TypeSystem.IModuleReference
	{
		public ModuleDef Reader { get; }
		public ModuleDef Metadata { get; }

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
			this.Reader = reader ?? throw new ArgumentNullException(nameof(reader));
			this.Metadata = Reader;
		}

		public bool IsAssembly => Metadata.Assembly != null;
		public string Name => GetName();
		public string FullName => IsAssembly ? Metadata.Assembly.FullName : Name;

		public TargetRuntime GetRuntime()
		{
			string version = Metadata.RuntimeVersion;
			switch (version[1]) {
				case '1':
					if (version[3] == 1)
						return TargetRuntime.Net_1_0;
					else
						return TargetRuntime.Net_1_1;
				case '2':
					return TargetRuntime.Net_2_0;
				case '4':
					return TargetRuntime.Net_4_0;
				default:
					return TargetRuntime.Unknown;
			}

		}

		string GetName()
		{
			var metadata = Metadata;
			if (metadata.Assembly != null)
				return metadata.Assembly.Name.String;
			return metadata.Name.String;
		}

		public ImmutableArray<AssemblyReference> AssemblyReferences => Metadata.GetAssemblyRefs().Select(r => new AssemblyReference(this, r)).ToImmutableArray();

		public void Dispose()
		{
			Reader.Dispose();
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
				foreach (var td in Metadata.Types) {
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
				foreach (var handle in Metadata.ExportedTypes) {
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


		public TypeSystem.IModuleReference WithOptions(TypeSystemOptions options)
		{
			return new PEFileWithOptions(this, options);
		}

		IModule TypeSystem.IModuleReference.Resolve(ITypeResolveContext context)
		{
			return new MetadataModule(context.Compilation, this, TypeSystemOptions.Default);
		}

		private class PEFileWithOptions : TypeSystem.IModuleReference
		{
			readonly PEFile peFile;
			readonly TypeSystemOptions options;

			public PEFileWithOptions(PEFile peFile, TypeSystemOptions options)
			{
				this.peFile = peFile;
				this.options = options;
			}

			IModule TypeSystem.IModuleReference.Resolve(ITypeResolveContext context)
			{
				return new MetadataModule(context.Compilation, peFile, options);
			}
		}
	}

	public enum TargetRuntime
	{
		Unknown,
		Net_1_0,
		Net_1_1,
		Net_2_0,
		Net_4_0
	}

	public interface IAssemblyReference
	{
		string Name { get; }
		string FullName { get; }
		Version Version { get; }
		string Culture { get; }
		byte[] PublicKeyToken { get; }

		bool IsWindowsRuntime { get; }
		bool IsRetargetable { get; }
	}

	public class AssemblyNameReference : IAssemblyReference
	{
		string fullName;

		public string Name { get; private set; }

		public string FullName {
			get {
				if (fullName != null)
					return fullName;

				const string sep = ", ";

				var builder = new StringBuilder();
				builder.Append(Name);
				builder.Append(sep);
				builder.Append("Version=");
				builder.Append((Version ?? UniversalAssemblyResolver.ZeroVersion).ToString(fieldCount: 4));
				builder.Append(sep);
				builder.Append("Culture=");
				builder.Append(string.IsNullOrEmpty(Culture) ? "neutral" : Culture);
				builder.Append(sep);
				builder.Append("PublicKeyToken=");

				var pk_token = PublicKeyToken;
				if (pk_token != null && pk_token.Length > 0) {
					for (int i = 0; i < pk_token.Length; i++) {
						builder.Append(pk_token[i].ToString("x2"));
					}
				} else
					builder.Append("null");

				if (IsRetargetable) {
					builder.Append(sep);
					builder.Append("Retargetable=Yes");
				}

				return fullName = builder.ToString();
			}
		}

		public Version Version { get; private set; }

		public string Culture { get; private set; }

		public byte[] PublicKeyToken { get; private set; }

		public bool IsWindowsRuntime { get; private set; }

		public bool IsRetargetable { get; private set; }

		public static AssemblyNameReference Parse(string fullName)
		{
			if (fullName == null)
				throw new ArgumentNullException("fullName");
			if (fullName.Length == 0)
				throw new ArgumentException("Name can not be empty");

			var name = new AssemblyNameReference();
			var tokens = fullName.Split(',');
			for (int i = 0; i < tokens.Length; i++) {
				var token = tokens[i].Trim();

				if (i == 0) {
					name.Name = token;
					continue;
				}

				var parts = token.Split('=');
				if (parts.Length != 2)
					throw new ArgumentException("Malformed name");

				switch (parts[0].ToLowerInvariant()) {
					case "version":
						name.Version = new Version(parts[1]);
						break;
					case "culture":
						name.Culture = parts[1] == "neutral" ? "" : parts[1];
						break;
					case "publickeytoken":
						var pk_token = parts[1];
						if (pk_token == "null")
							break;

						name.PublicKeyToken = new byte[pk_token.Length / 2];
						for (int j = 0; j < name.PublicKeyToken.Length; j++)
							name.PublicKeyToken[j] = Byte.Parse(pk_token.Substring(j * 2, 2), System.Globalization.NumberStyles.HexNumber);

						break;
				}
			}

			return name;
		}
	}

	public class AssemblyReference : IAssemblyReference
	{
		static readonly SHA1 sha1 = SHA1.Create();

		public PEFile Module { get; }
		public IAssembly Handle { get; }

		IAssembly This() => Handle;

		public bool IsWindowsRuntime => Handle.IsContentTypeWindowsRuntime;
		public bool IsRetargetable => Handle.IsRetargetable;

		public string Name => Handle.Name;
		public string FullName => Handle.FullName;
		public Version Version => This().Version;
		public string Culture => Handle.Culture;
		byte[] IAssemblyReference.PublicKeyToken => GetPublicKeyToken();

		public byte[] GetPublicKeyToken()
		{
			var inst = This();
			if (inst.PublicKeyOrToken.IsNullOrEmpty)
				return Empty<byte>.Array;
			var bytes = inst.PublicKeyOrToken.Token.Data;
			if (inst.HasPublicKey) {
				return sha1.ComputeHash(bytes).Skip(12).ToArray();
			}
			return bytes;
		}

		public AssemblyReference(PEFile module, IAssembly handle)
		{
			Module = module ?? throw new ArgumentNullException(nameof(module));
			Handle = handle ?? throw new ArgumentNullException(nameof(handle));
		}

		public override string ToString()
		{
			return FullName;
		}
	}
}
