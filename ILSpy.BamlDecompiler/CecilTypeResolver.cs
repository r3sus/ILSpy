// Copyright (c) AlphaSierraPapa for the SharpDevelop Team
// This code is distributed under the MS-PL (for details please see \doc\MS-PL.txt)

using System;
using dnlib.DotNet;
using Ricciolo.StylesExplorer.MarkupReflection;
using IType = Ricciolo.StylesExplorer.MarkupReflection.IType;
using ITypeResolver = Ricciolo.StylesExplorer.MarkupReflection.ITypeResolver;

namespace ILSpy.BamlDecompiler
{
	/// <summary>
	/// Description of CecilTypeResolver.
	/// </summary>
	public class CecilTypeResolver : ITypeResolver
	{
		IAssemblyResolver resolver;
		AssemblyDef thisAssembly;

		public CecilTypeResolver(IAssemblyResolver resolver, AssemblyDef asm)
		{
			this.resolver = resolver;
			this.thisAssembly = asm;
		}

		public bool IsLocalAssembly(string name)
		{
			return MakeShort(name) == this.thisAssembly.Name;
		}

		string MakeShort(string name)
		{
			int endOffset = name.IndexOf(',');
			if (endOffset == -1)
				return name;

			return name.Substring(0, endOffset);
		}

		public IType GetTypeByAssemblyQualifiedName(string name)
		{
			int bracket = name.LastIndexOf(']');
			int comma = bracket > -1 ? name.IndexOf(',', bracket) : name.IndexOf(',');

			if (comma == -1)
				throw new ArgumentException("invalid name");

			string fullName = bracket > -1 ? name.Substring(0, name.IndexOf('[')) : name.Substring(0, comma);
			string assemblyName = name.Substring(comma + 1).Trim();

			var type = thisAssembly.ManifestModule.Find(fullName, false);

			if (type == null) {
				type = TryFindInExportedTypes(fullName, thisAssembly);
			}

			if (type == null) {
				var otherAssembly = resolver.Resolve(new AssemblyRefUser(new AssemblyNameInfo(fullName)), thisAssembly.ManifestModule);
				if (otherAssembly == null)
					return new UnresolvableType(name);
				type = otherAssembly.ManifestModule.Find(fullName.Replace('+', '/'), false);

				if (type == null) {
					type = TryFindInExportedTypes(fullName, otherAssembly);
				}
			}

			if (type == null)
				return new UnresolvableType(name);

			return new CecilType(type);
		}

		TypeDef TryFindInExportedTypes(string fullName, AssemblyDef asm)
		{
			foreach (var exportedType in asm.ManifestModule.ExportedTypes) {
				if (exportedType.IsForwarder && exportedType.FullName == fullName) {
					return exportedType.Resolve();
				}
			}

			return null;
		}

		public IDependencyPropertyDescriptor GetDependencyPropertyDescriptor(string name, IType ownerType, IType targetType)
		{
			if (ownerType == null)
				throw new ArgumentNullException("ownerType");

			if (ownerType is CecilType)
				return new CecilDependencyPropertyDescriptor(name, ((CecilType)ownerType).type);
			if (ownerType is UnresolvableType)
				return new UnresolvableDependencyPropertyDescriptor();

			throw new ArgumentException("Invalid IType: " + ownerType.GetType());
		}

		public string RuntimeVersion {
			get {
				return thisAssembly.ManifestModule.RuntimeVersion;
			}
		}
	}
}
