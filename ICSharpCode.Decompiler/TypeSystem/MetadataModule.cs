﻿// Copyright (c) 2018 Daniel Grunwald
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
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using dnlib.DotNet;
using dnlib.DotNet.MD;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem
{
	/// <summary>
	/// Type system implementation for Metadata.PEFile.
	/// </summary>
	[DebuggerDisplay("<MetadataModule: {AssemblyName}>")]
	public class MetadataModule : IModule
	{
		public ICompilation Compilation { get; }
		internal readonly ModuleDef metadata;
		readonly TypeSystemOptions options;
		internal readonly Nullability NullableContext;

		readonly MetadataNamespace rootNamespace;

		private readonly Dictionary<TypeDef, MetadataTypeDefinition> typeDefDict;
		private readonly Dictionary<FieldDef, MetadataField> fieldDefDict;
		private readonly Dictionary<MethodDef, MetadataMethod> methodDefDict;
		private readonly Dictionary<PropertyDef, MetadataProperty> propertyDefDict;
		private readonly Dictionary<EventDef, MetadataEvent> eventDefDict;

		internal MetadataModule(ICompilation compilation, PEFile peFile, TypeSystemOptions options)
		{
			this.Compilation = compilation;
			this.PEFile = peFile;
			this.metadata = peFile.Module;
			this.options = options;

			// assembly metadata
			if (metadata.Assembly != null) {
				var asmdef = metadata.Assembly;
				this.AssemblyName = asmdef.Name;
				this.FullAssemblyName = asmdef.FullName;
			} else {
				var moddef = metadata;
				this.AssemblyName = moddef.Name;
				this.FullAssemblyName = this.AssemblyName;
			}

			var customAttrs = metadata.CustomAttributes;
			this.NullableContext = customAttrs.GetNullableContext() ?? Nullability.Oblivious;
			this.minAccessibilityForNRT = FindMinimumAccessibilityForNRT(customAttrs);
			this.rootNamespace = new MetadataNamespace(this, null, string.Empty,
				NamespaceDefinition.GetRootNamespace(compilation.NameComparer, metadata.Types));

			if (!options.HasFlag(TypeSystemOptions.Uncached)) {
				typeDefDict = new Dictionary<TypeDef, MetadataTypeDefinition>();
				fieldDefDict = new Dictionary<FieldDef, MetadataField>();
				methodDefDict = new Dictionary<MethodDef, MetadataMethod>();
				propertyDefDict = new Dictionary<PropertyDef, MetadataProperty>();
				eventDefDict = new Dictionary<EventDef, MetadataEvent>();
			}
		}

		public TypeSystemOptions TypeSystemOptions => options;

		#region IAssembly interface

		public PEFile PEFile { get; }

		public bool IsMainModule => this == Compilation.MainModule;

		public string AssemblyName { get; }
		public string FullAssemblyName { get; }
		string ISymbol.Name => AssemblyName;
		SymbolKind ISymbol.SymbolKind => SymbolKind.Module;

		public INamespace RootNamespace => rootNamespace;

		public IEnumerable<ITypeDefinition> TopLevelTypeDefinitions {
			get {
				foreach (var tdHandle in metadata.Types) {
					yield return GetDefinition(tdHandle);
				}
			}
		}

		public ITypeDefinition GetTypeDefinition(TopLevelTypeName topLevelTypeName)
		{
			var typeDefHandle = PEFile.GetTypeDefinition(topLevelTypeName);
			if (typeDefHandle == null) {
				var forwarderHandle = PEFile.GetTypeForwarder(topLevelTypeName);
				if (forwarderHandle != null) {
					return ResolveForwardedType(forwarderHandle).GetDefinition();
				}
			}

			return GetDefinition(typeDefHandle);
		}

		#endregion

		#region InternalsVisibleTo

		public bool InternalsVisibleTo(IModule module)
		{
			if (this == module)
				return true;
			foreach (string shortName in GetInternalsVisibleTo()) {
				if (string.Equals(module.AssemblyName, shortName, StringComparison.OrdinalIgnoreCase))
					return true;
			}

			return false;
		}

		private string[] internalsVisibleTo;

		private string[] GetInternalsVisibleTo()
		{
			var result = LazyInit.VolatileRead(ref this.internalsVisibleTo);
			if (result != null) {
				return result;
			}

			if (metadata.Assembly != null) {
				var list = new List<string>();
				foreach (var attr in metadata.Assembly.CustomAttributes) {
					if (attr.IsKnownAttribute(KnownAttribute.InternalsVisibleTo) && attr.ConstructorArguments.Count == 1 &&
						attr.ConstructorArguments[0].Value is UTF8String s) {
						list.Add(GetShortName(s));
					}
				}

				result = list.ToArray();
			} else {
				result = Empty<string>.Array;
			}

			return LazyInit.GetOrSet(ref this.internalsVisibleTo, result);
		}

		private static string GetShortName(string fullAssemblyName)
		{
			int pos = fullAssemblyName.IndexOf(',');
			return pos < 0 ? fullAssemblyName : fullAssemblyName.Substring(0, pos);
		}

		#endregion

		#region GetDefinition

		/// <summary>
		/// Gets all types in the assembly, including nested types.
		/// </summary>
		public IEnumerable<ITypeDefinition> TypeDefinitions {
			get {
				foreach (var tdHandle in metadata.GetTypes()) {
					yield return GetDefinition(tdHandle);
				}
			}
		}

		public ITypeDefinition GetDefinition(TypeDef handle)
		{
			if (handle == null)
				return null;
			if (typeDefDict == null)
				return new MetadataTypeDefinition(this, handle);
			lock (typeDefDict) {
				if (typeDefDict.TryGetValue(handle, out var tsType))
					return tsType;
				tsType = new MetadataTypeDefinition(this, handle);
				return typeDefDict[handle] = tsType;
			}
		}

		public IField GetDefinition(FieldDef handle)
		{
			if (handle == null)
				return null;
			if (fieldDefDict == null)
				return new MetadataField(this, handle);
			lock (fieldDefDict) {
				if (fieldDefDict.TryGetValue(handle, out var tsField))
					return tsField;
				tsField = new MetadataField(this, handle);
				return fieldDefDict[handle] = tsField;
			}
		}

		public IMethod GetDefinition(MethodDef handle)
		{
			if (handle == null)
				return null;
			if (methodDefDict == null)
				return new MetadataMethod(this, handle);
			lock (methodDefDict) {
				if (methodDefDict.TryGetValue(handle, out var tsMethod))
					return tsMethod;
				tsMethod = new MetadataMethod(this, handle);
				return methodDefDict[handle] = tsMethod;
			}
		}

		public IProperty GetDefinition(PropertyDef handle)
		{
			if (handle == null)
				return null;
			if (propertyDefDict == null)
				return new MetadataProperty(this, handle);
			lock (propertyDefDict) {
				if (propertyDefDict.TryGetValue(handle, out var tsProperty))
					return tsProperty;
				tsProperty = new MetadataProperty(this, handle);
				return propertyDefDict[handle] = tsProperty;
			}
		}

		public IEvent GetDefinition(EventDef handle)
		{
			if (handle == null)
				return null;
			if (eventDefDict == null)
				return new MetadataEvent(this, handle);
			lock (eventDefDict) {
				if (eventDefDict.TryGetValue(handle, out var tsEvent))
					return tsEvent;
				tsEvent = new MetadataEvent(this, handle);
				return eventDefDict[handle] = tsEvent;
			}
		}

		#endregion

		#region Resolve Type

		public IType ResolveType(ITypeDefOrRef typeRefDefSpec, GenericContext context,
			IHasCustomAttribute typeAttributes = null, Nullability nullableContext = Nullability.Oblivious)
		{
			return ResolveType(typeRefDefSpec, context, options, typeAttributes, nullableContext);
		}

		public IType ResolveType(TypeSig typeRefDefSpec, GenericContext context,
			IHasCustomAttribute typeAttributes = null, Nullability nullableContext = Nullability.Oblivious)
		{
			return ResolveType(typeRefDefSpec, context, options, typeAttributes, nullableContext);
		}

		public IType ResolveType(ITypeDefOrRef typeDefOrRef, GenericContext context, TypeSystemOptions customOptions,
			IHasCustomAttribute typeAttributes = null, Nullability nullableContext = Nullability.Oblivious)
		{
			if (typeDefOrRef == null)
				return SpecialType.UnknownType;

			IType ty = typeDefOrRef.DecodeSignature(this, context);
			ty = ApplyAttributeTypeVisitor.ApplyAttributesToType(ty, Compilation, typeAttributes, customOptions,
				nullableContext);
			return ty;
		}

		public IType ResolveType(TypeSig typeSpec, GenericContext context, TypeSystemOptions customOptions,
			IHasCustomAttribute typeAttributes = null, Nullability nullableContext = Nullability.Oblivious)
		{
			if (typeSpec == null)
				return SpecialType.UnknownType;

			IType ty = typeSpec.DecodeSignature(this, context);
			ty = ApplyAttributeTypeVisitor.ApplyAttributesToType(ty, Compilation, typeAttributes, customOptions,
				nullableContext);
			return ty;
		}

		IType ResolveDeclaringType(ITypeDefOrRef declaringTypeReference, GenericContext context)
		{
			// resolve without substituting dynamic/tuple types
			var ty = ResolveType(declaringTypeReference, context,
				options & ~(TypeSystemOptions.Dynamic | TypeSystemOptions.Tuple | TypeSystemOptions.NullabilityAnnotations));
			// but substitute tuple types in type arguments:
			ty = ApplyAttributeTypeVisitor.ApplyAttributesToType(ty, Compilation, null, options, Nullability.Oblivious,
				typeChildrenOnly: true);
			return ty;
		}

		private IType IntroduceTupleTypes(IType ty)
		{
			// run ApplyAttributeTypeVisitor without attributes, in order to introduce tuple types
			return ApplyAttributeTypeVisitor.ApplyAttributesToType(ty, Compilation, null, options,
				Nullability.Oblivious);
		}

		#endregion

		#region Resolve Method

		public IMethod ResolveMethod(dnlib.DotNet.IMethod methodReference, GenericContext context)
		{
			if (methodReference == null)
				throw new ArgumentNullException(nameof(methodReference));
			switch (methodReference.MDToken.Table) {
				case Table.Method:
					return ResolveMethodDefinition((MethodDef)methodReference, expandVarArgs: true);
				case Table.MemberRef:
					return ResolveMethodReference((MemberRef)methodReference, context, expandVarArgs: true);
				case Table.MethodSpec:
					return ResolveMethodSpecification((MethodSpec)methodReference, context, expandVarArgs: true);
				default:
					throw new BadImageFormatException("Metadata token must be either a methoddef, memberref or methodspec");
			}
		}

		IMethod ResolveMethodDefinition(MethodDef methodDefHandle, bool expandVarArgs)
		{
			var method = GetDefinition(methodDefHandle);
			if (expandVarArgs && methodDefHandle.CallingConvention == CallingConvention.VarArg) {
				method = new VarArgInstanceMethod(method, EmptyList<IType>.Instance);
			}

			return method;
		}

		IMethod ResolveMethodSpecification(MethodSpec methodSpec, GenericContext context, bool expandVarArgs)
		{
			var methodTypeArgs = methodSpec.GenericInstMethodSig.GenericArguments
										   .Select(sig => sig.DecodeSignature(this, context))
										   .Select(IntroduceTupleTypes).ToArray();
			IMethod method;
			if (methodSpec.Method is MethodDef methodDef) {
				// generic instance of a methoddef (=generic method in non-generic class in current assembly)
				method = ResolveMethodDefinition(methodDef, expandVarArgs)
					.Specialize(new TypeParameterSubstitution(null, methodTypeArgs));
			} else {
				method = ResolveMethodReference((MemberRef)methodSpec.Method, context, methodTypeArgs, expandVarArgs);
			}

			return method;
		}

		/// <summary>
		/// Resolves a method reference.
		/// </summary>
		/// <remarks>
		/// Class type arguments are provided by the declaring type stored in the memberRef.
		/// Method type arguments are provided by the caller.
		/// </remarks>
		IMethod ResolveMethodReference(MemberRef memberRef, GenericContext context,
			IReadOnlyList<IType> methodTypeArguments = null, bool expandVarArgs = true)
		{
			Debug.Assert(memberRef.IsMethodRef);
			IReadOnlyList<IType> classTypeArguments = null;
			IMethod method;
			GenericContext vaRAgCtx;
			if (memberRef.Class is MethodDef methodDef) {
				method = ResolveMethodDefinition(methodDef, expandVarArgs: false);
				vaRAgCtx = context;
			} else {
				var declaringType = ResolveDeclaringType(memberRef.DeclaringType, context);
				var declaringTypeDefinition = declaringType.GetDefinition();
				if (declaringType.TypeArguments.Count > 0) {
					classTypeArguments = declaringType.TypeArguments;
				}

				// Note: declaringType might be parameterized, but the signature is for the original method definition.
				// We'll have to search the member directly on declaringTypeDefinition.
				string name = memberRef.Name;
				vaRAgCtx = new GenericContext(declaringTypeDefinition?.TypeParameters);
				var returnType = memberRef.MethodSig.RetType.DecodeSignature(this, vaRAgCtx);
				if (declaringTypeDefinition != null) {
					// Find the set of overloads to search:
					IEnumerable<IMethod> methods;
					if (name == ".ctor") {
						methods = declaringTypeDefinition.GetConstructors();
					} else if (name == ".cctor") {
						methods = declaringTypeDefinition.Methods.Where(m => m.IsConstructor && m.IsStatic);
					} else {
						methods = declaringTypeDefinition.GetMethods(m => m.Name == name, GetMemberOptions.IgnoreInheritedMembers)
														 .Concat(declaringTypeDefinition.GetAccessors(m => m.Name == name,
															 GetMemberOptions.IgnoreInheritedMembers));
					}

					// Determine the expected parameters from the signature:
					ImmutableArray<IType> parameterTypes;
					if (memberRef.CallingConvention == CallingConvention.VarArg) {
						parameterTypes = memberRef.MethodSig.Params.Select(x => x.DecodeSignature(this, vaRAgCtx))
												  .Concat(new[] { SpecialType.ArgList })
												  .ToImmutableArray();
					} else {
						parameterTypes = memberRef.MethodSig.Params.Select(x => x.DecodeSignature(this, vaRAgCtx))
												  .ToImmutableArray();
					}

					// Search for the matching method:
					method = null;
					foreach (var m in methods) {
						if (m.TypeParameters.Count != ((dnlib.DotNet.IMethod)memberRef).NumberOfGenericParameters)
							continue;
						if (CompareSignatures(m.Parameters, parameterTypes) && CompareTypes(m.ReturnType, returnType)) {
							method = m;
							break;
						}
					}
				} else {
					method = null;
				}

				if (method == null) {
					var param = memberRef.MethodSig.Params.Select(x => x.DecodeSignature(this, vaRAgCtx))
										 .ToList();
					method = CreateFakeMethod(declaringType, name, !memberRef.HasThis, returnType,
						((dnlib.DotNet.IMethod)memberRef).NumberOfGenericParameters, param);
				}
			}

			if (classTypeArguments != null || methodTypeArguments != null) {
				method = method.Specialize(new TypeParameterSubstitution(classTypeArguments, methodTypeArguments));
			}

			if (expandVarArgs && memberRef.CallingConvention == CallingConvention.VarArg) {
				method = new VarArgInstanceMethod(method,
					memberRef.MethodSig.ParamsAfterSentinel is null
						? new List<IType>()
						: memberRef.MethodSig.ParamsAfterSentinel.Select(x => x.DecodeSignature(this, vaRAgCtx)));
			}

			return method;
		}

		static readonly NormalizeTypeVisitor normalizeTypeVisitor = new NormalizeTypeVisitor {
			ReplaceClassTypeParametersWithDummy = true,
			ReplaceMethodTypeParametersWithDummy = true,
		};

		static bool CompareTypes(IType a, IType b)
		{
			IType type1 = a.AcceptVisitor(normalizeTypeVisitor);
			IType type2 = b.AcceptVisitor(normalizeTypeVisitor);
			return type1.Equals(type2);
		}

		static bool CompareSignatures(IReadOnlyList<IParameter> parameters, ImmutableArray<IType> parameterTypes)
		{
			if (parameterTypes.Length != parameters.Count)
				return false;
			for (int i = 0; i < parameterTypes.Length; i++) {
				if (!CompareTypes(parameterTypes[i], parameters[i].Type))
					return false;
			}

			return true;
		}

		/// <summary>
		/// Create a dummy IMethod from the specified MethodReference
		/// </summary>
		IMethod CreateFakeMethod(IType declaringType, string name, bool isStatic, IType retType, int numOfGeneric,
			List<IType> param)
		{
			SymbolKind symbolKind = SymbolKind.Method;
			if (name == ".ctor" || name == ".cctor")
				symbolKind = SymbolKind.Constructor;
			var m = new FakeMethod(Compilation, symbolKind);
			m.DeclaringType = declaringType;
			m.Name = name;
			m.ReturnType = retType;
			m.IsStatic = isStatic;

			TypeParameterSubstitution substitution = null;
			if (numOfGeneric > 0) {
				var typeParameters = new List<ITypeParameter>();
				for (int i = 0; i < numOfGeneric; i++) {
					typeParameters.Add(new DefaultTypeParameter(m, i));
				}

				m.TypeParameters = typeParameters;
				substitution = new TypeParameterSubstitution(declaringType.TypeArguments, typeParameters);
			} else if (declaringType.TypeArguments.Count > 0) {
				substitution = declaringType.GetSubstitution();
			}

			var parameters = new List<IParameter>();
			for (int i = 0; i < param.Count; i++) {
				var type = param[i];
				if (substitution != null) {
					// replace the dummy method type parameters with the owned instances we just created
					type = type.AcceptVisitor(substitution);
				}

				parameters.Add(new DefaultParameter(type, ""));
			}

			m.Parameters = parameters;
			return m;
		}

		#endregion

		#region Resolve Entity

		/// <summary>
		/// Resolves a symbol.
		/// </summary>
		/// <remarks>
		/// * Types are resolved to their definition, as IType does not implement ISymbol.
		///    * types without definition will resolve to <c>null</c>
		///    * use ResolveType() to properly resolve types
		/// * When resolving methods, varargs signatures are not expanded.
		///    * use ResolveMethod() instead to get an IMethod instance suitable for call-sites
		/// * May return specialized members, where generics are involved.
		/// * Other types of handles that don't correspond to TS entities, will return <c>null</c>.
		/// </remarks>
		public IEntity ResolveEntity(IMDTokenProvider entityHandle, GenericContext context = default)
		{
			if (entityHandle is TypeDef typeDef)
				return GetDefinition(typeDef);
			if (entityHandle is MethodDef methodDef)
				return GetDefinition(methodDef);
			if (entityHandle is FieldDef fieldDef)
				return GetDefinition(fieldDef);
			if (entityHandle is EventDef eventDef)
				return GetDefinition(eventDef);
			if (entityHandle is PropertyDef propertyDef)
				return GetDefinition(propertyDef);
			if (entityHandle is ITypeDefOrRef typeDefOrRef)
				return ResolveType(typeDefOrRef, context).GetDefinition();
			if (entityHandle is MemberRef memberRef) {
				if (memberRef.IsMethodRef) {
					// for consistency with the MethodDefinition case, never expand varargs
					return ResolveMethodReference(memberRef, context, expandVarArgs: false);
				}
				if (memberRef.IsFieldRef)
					return ResolveFieldReference(memberRef, context);
			}
			if (entityHandle is MethodSpec methodSpec)
				return ResolveMethodSpecification(methodSpec, context, expandVarArgs: false);
			return null;
		}

		IField ResolveFieldReference(MemberRef memberRef, GenericContext context)
		{
			var declaringType = ResolveDeclaringType(memberRef.DeclaringType, context);
			var declaringTypeDefinition = declaringType.GetDefinition();
			string name = memberRef.Name;
			// field signature is for the definition, not the generic instance
			var signature = memberRef.FieldSig.Type.DecodeSignature(this,
				new GenericContext(declaringTypeDefinition?.TypeParameters));
			// 'f' in the predicate is also the definition, even if declaringType is a ParameterizedType
			var field = declaringType.GetFields(f => f.Name == name && CompareTypes(f.ReturnType, signature),
				GetMemberOptions.IgnoreInheritedMembers).FirstOrDefault();
			if (field == null) {
				// If it's a field in a generic type, we need to substitute the type arguments:
				if (declaringType.TypeArguments.Count > 0) {
					signature = signature.AcceptVisitor(declaringType.GetSubstitution());
				}

				field = new FakeField(Compilation) {
					ReturnType = signature,
					Name = name,
					DeclaringType = declaringType,
				};
			}

			return field;
		}

		#endregion

		#region Decode Standalone Signature

		public FunctionPointerType DecodeMethodSignature(MethodSig standaloneSignature, GenericContext genericContext)
		{
			if (standaloneSignature is null)
				throw new BadImageFormatException("Expected Method signature");
			var retType = standaloneSignature.RetType.DecodeSignature(this, genericContext);
			var paramTypes = standaloneSignature.Params.Select(t => t.DecodeSignature(this, genericContext)).ToList();
			var fpt = FunctionPointerType.FromSignature(retType, paramTypes, standaloneSignature.CallingConvention, this);
			return (FunctionPointerType)IntroduceTupleTypes(fpt);
		}

		#endregion

		#region Module / Assembly attributes

		/// <summary>
		/// Gets the list of all assembly attributes in the project.
		/// </summary>
		public IEnumerable<IAttribute> GetAssemblyAttributes()
		{
			var b = new AttributeListBuilder(this);
			if (metadata.Assembly != null) {
				var assembly = metadata.Assembly;
				b.Add(assembly.CustomAttributes, SymbolKind.Module);
				b.AddSecurityAttributes(assembly.DeclSecurities);

				// AssemblyVersionAttribute
				if (assembly.Version != null) {
					b.Add(KnownAttribute.AssemblyVersion, KnownTypeCode.String, assembly.Version.ToString());
				}

				AddTypeForwarderAttributes(ref b);
			}

			return b.Build();
		}

		/// <summary>
		/// Gets the list of all module attributes in the project.
		/// </summary>
		public IEnumerable<IAttribute> GetModuleAttributes()
		{
			var b = new AttributeListBuilder(this);
			b.Add(metadata.CustomAttributes, SymbolKind.Module);
			if (metadata.Assembly == null) {
				AddTypeForwarderAttributes(ref b);
			}

			return b.Build();
		}

		void AddTypeForwarderAttributes(ref AttributeListBuilder b)
		{
			foreach (ExportedType type in metadata.ExportedTypes) {
				if (type.IsForwarder) {
					b.Add(KnownAttribute.TypeForwardedTo, KnownTypeCode.Type, ResolveForwardedType(type));
				}
			}
		}

		IType ResolveForwardedType(ExportedType forwarder)
		{
			var resolved = forwarder.Resolve();
			if (resolved != null && Compilation.GetOrAddModule(resolved.Module) is MetadataModule mod) {
				return mod.GetDefinition(resolved);
			}
			return new UnknownType(forwarder.GetFullTypeName());
		}

		#endregion

		#region Attribute Helpers

		/// <summary>
		/// Cache for parameterless known attribute types.
		/// </summary>
		readonly IType[] knownAttributeTypes = new IType[KnownAttributes.Count];

		internal IType GetAttributeType(KnownAttribute attr)
		{
			var ty = LazyInit.VolatileRead(ref knownAttributeTypes[(int)attr]);
			if (ty != null)
				return ty;
			ty = Compilation.FindType(attr.GetTypeName());
			return LazyInit.GetOrSet(ref knownAttributeTypes[(int)attr], ty);
		}

		/// <summary>
		/// Cache for parameterless known attributes.
		/// </summary>
		readonly IAttribute[] knownAttributes = new IAttribute[KnownAttributes.Count];

		/// <summary>
		/// Construct a builtin attribute.
		/// </summary>
		internal IAttribute MakeAttribute(KnownAttribute type)
		{
			var attr = LazyInit.VolatileRead(ref knownAttributes[(int)type]);
			if (attr != null)
				return attr;
			attr = new DefaultAttribute(GetAttributeType(type),
				ImmutableArray.Create<CustomAttributeTypedArgument<IType>>(),
				ImmutableArray.Create<CustomAttributeNamedArgument<IType>>());
			return LazyInit.GetOrSet(ref knownAttributes[(int)type], attr);
		}

		#endregion

		#region Visibility Filter

		internal bool IncludeInternalMembers => (options & TypeSystemOptions.OnlyPublicAPI) == 0;

		internal bool IsVisible(FieldAttributes att)
		{
			att &= FieldAttributes.FieldAccessMask;
			return IncludeInternalMembers
				   || att == FieldAttributes.Public
				   || att == FieldAttributes.Family
				   || att == FieldAttributes.FamORAssem;
		}

		internal bool IsVisible(MethodAttributes att)
		{
			att &= MethodAttributes.MemberAccessMask;
			return IncludeInternalMembers
				   || att == MethodAttributes.Public
				   || att == MethodAttributes.Family
				   || att == MethodAttributes.FamORAssem;
		}

		#endregion

		#region Nullability Reference Type Support

		readonly Accessibility minAccessibilityForNRT;

		static Accessibility FindMinimumAccessibilityForNRT(CustomAttributeCollection customAttributes)
		{
			// Determine the minimum effective accessibility an entity must have, so that the metadata stores the nullability for its type.
			foreach (var customAttribute in customAttributes) {
				if (customAttribute.IsKnownAttribute(KnownAttribute.NullablePublicOnly)) {
					if (customAttribute.ConstructorArguments.Count == 1 &&
						customAttribute.ConstructorArguments[0].Value is bool includesInternals) {
						return includesInternals ? Accessibility.ProtectedAndInternal : Accessibility.Protected;
					}
				}
			}

			return Accessibility.None;
		}

		internal bool ShouldDecodeNullableAttributes(IEntity entity)
		{
			if ((options & TypeSystemOptions.NullabilityAnnotations) == 0)
				return false;
			if (minAccessibilityForNRT == Accessibility.None || entity == null)
				return true;
			return minAccessibilityForNRT.LessThanOrEqual(entity.EffectiveAccessibility());
		}

		internal TypeSystemOptions OptionsForEntity(IEntity entity)
		{
			var opt = this.options;
			if ((opt & TypeSystemOptions.NullabilityAnnotations) != 0) {
				if (!ShouldDecodeNullableAttributes(entity)) {
					opt &= ~TypeSystemOptions.NullabilityAnnotations;
				}
			}

			return opt;
		}

		#endregion
	}
}
