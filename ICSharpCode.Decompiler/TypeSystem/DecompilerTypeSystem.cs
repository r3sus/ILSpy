using System;
using System.Collections.Generic;
using System.Linq;
using dnlib.DotNet;
using dnlib.DotNet.MD;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem
{
	/// <summary>
	/// Manages the NRefactory type system for the decompiler.
	/// </summary>
	/// <remarks>
	/// This class is thread-safe.
	/// </remarks>
	public class DecompilerTypeSystem : IDecompilerTypeSystem
	{
		readonly ModuleDef moduleDefinition;
		readonly ICompilation compilation;
		readonly ITypeResolveContext context;

		/// <summary>
		/// CecilLoader used for converting cecil type references to ITypeReference.
		/// May only be accessed within lock(typeReferenceCecilLoader).
		/// </summary>
		readonly CecilLoader typeReferenceCecilLoader;

		/// <summary>
		/// Dictionary for NRefactory->Cecil lookup.
		/// May only be accessed within lock(entityDict)
		/// </summary>
		Dictionary<IUnresolvedEntity, dnlib.DotNet.IMemberRef> entityDict = new Dictionary<IUnresolvedEntity, dnlib.DotNet.IMemberRef>();

		Dictionary<dnlib.DotNet.IField, IField> fieldLookupCache = new Dictionary<dnlib.DotNet.IField, IField>();
		Dictionary<PropertyDef, IProperty> propertyLookupCache = new Dictionary<PropertyDef, IProperty>();
		Dictionary<dnlib.DotNet.IMethod, IMethod> methodLookupCache = new Dictionary<dnlib.DotNet.IMethod, IMethod>();
		Dictionary<EventDef, IEvent> eventLookupCache = new Dictionary<EventDef, IEvent>();

		public DecompilerTypeSystem(ModuleDef moduleDefinition) : this(moduleDefinition, new DecompilerSettings())
		{
		}

		public DecompilerTypeSystem(ModuleDef moduleDefinition, DecompilerSettings settings)
		{
			if (moduleDefinition == null)
				throw new ArgumentNullException(nameof(moduleDefinition));
			if (settings == null)
				throw new ArgumentNullException(nameof(settings));
			this.moduleDefinition = moduleDefinition;
			typeReferenceCecilLoader = new CecilLoader {
				UseDynamicType = settings.Dynamic,
				UseTupleTypes = settings.TupleTypes,
			};
			CecilLoader cecilLoader = new CecilLoader {
				IncludeInternalMembers = true,
				LazyLoad = true,
				OnEntityLoaded = StoreMemberReference,
				ShortenInterfaceImplNames = false,
				UseDynamicType = settings.Dynamic,
				UseTupleTypes = settings.TupleTypes,
			};
			typeReferenceCecilLoader.SetCurrentModule(moduleDefinition);
			IUnresolvedAssembly mainAssembly = cecilLoader.LoadModule(moduleDefinition);
			// Load referenced assemblies and type-forwarder references.
			// This is necessary to make .NET Core/PCL binaries work better.
			var referencedAssemblies = new List<IUnresolvedAssembly>();
			var assemblyReferenceQueue = new Queue<dnlib.DotNet.IAssembly>(moduleDefinition.GetAssemblyRefs());
			var processedAssemblyReferences = new HashSet<dnlib.DotNet.IAssembly>(KeyComparer.Create((dnlib.DotNet.IAssembly reference) => reference.FullName));
			while (assemblyReferenceQueue.Count > 0) {
				var asmRef = assemblyReferenceQueue.Dequeue();
				if (!processedAssemblyReferences.Add(asmRef))
					continue;
				var asm = moduleDefinition.Context.AssemblyResolver.Resolve(asmRef, moduleDefinition);
				if (asm != null) {
					referencedAssemblies.Add(cecilLoader.LoadAssembly(asm));
					foreach (var forwarder in asm.ManifestModule.ExportedTypes) {
						if (!forwarder.IsForwarder || !(forwarder.Scope is dnlib.DotNet.IAssembly forwarderRef)) continue;
						assemblyReferenceQueue.Enqueue(forwarderRef);
					}
				}
			}
			compilation = new SimpleCompilation(mainAssembly, referencedAssemblies);
			// Primitive types are necessary to avoid assertions in ILReader.
			// Fallback to MinimalCorlib to provide the primitive types.
			if (compilation.FindType(KnownTypeCode.Void).Kind == TypeKind.Unknown || compilation.FindType(KnownTypeCode.Int32).Kind == TypeKind.Unknown) {
				referencedAssemblies.Add(MinimalCorlib.Instance);
				compilation = new SimpleCompilation(mainAssembly, referencedAssemblies);
			}
			context = new SimpleTypeResolveContext(compilation.MainAssembly);
		}

		public ICompilation Compilation {
			get { return compilation; }
		}

		public IAssembly MainAssembly {
			get { return compilation.MainAssembly; }
		}

		public ModuleDef ModuleDefinition {
			get { return moduleDefinition; }
		}

		void StoreMemberReference(IUnresolvedEntity entity, IMemberRef mr)
		{
			// This is a callback from the type system, which is multi-threaded and may be accessed externally
			lock (entityDict)
				entityDict[entity] = mr;
		}

		/// <summary>
		/// Retrieves the Cecil member definition for the specified member.
		/// </summary>
		/// <remarks>
		/// Returns null if the member is not defined in the module being decompiled.
		/// </remarks>
		public dnlib.DotNet.IMemberRef GetCecil(IUnresolvedEntity member)
		{
			if (member == null)
				return null;
			lock (entityDict) {
				dnlib.DotNet.IMemberRef mr;
				if (entityDict.TryGetValue(member, out mr))
					return mr;
				return null;
			}
		}

		/// <summary>
		/// Retrieves the Cecil member definition for the specified member.
		/// </summary>
		/// <remarks>
		/// Returns null if the member is not defined in the module being decompiled.
		/// </remarks>
		public dnlib.DotNet.IMemberRef GetCecil(IMember member)
		{
			if (member == null)
				return null;
			return GetCecil(member.UnresolvedMember);
		}

		/// <summary>
		/// Retrieves the Cecil type definition.
		/// </summary>
		/// <remarks>
		/// Returns null if the type is not defined in the module being decompiled.
		/// </remarks>
		public TypeDef GetCecil(ITypeDefinition typeDefinition)
		{
			if (typeDefinition == null)
				return null;
			return GetCecil(typeDefinition.Parts[0]) as TypeDef;
		}

		#region Resolve Type
		public IType Resolve(ITypeDefOrRef typeReference, bool isFromSignature = false)
		{
			if (typeReference == null)
				return SpecialType.UnknownType;
			// We need to skip SentinelType and PinnedType.
			// But PinnedType can be nested within modopt, so we'll also skip those.
			var typeSig = typeReference.ToTypeSig();
			while (typeSig is CModOptSig || typeSig is CModReqdSig) {
				typeSig = typeSig.Next;
				isFromSignature = true;
			}
			if (typeSig is SentinelSig || typeSig is PinnedSig) {
				typeSig = typeSig.Next;
				isFromSignature = true;
			}
			ITypeReference typeRef;
			lock (typeReferenceCecilLoader)
				typeRef = typeReferenceCecilLoader.ReadTypeReference(typeSig, isFromSignature: isFromSignature);
			return typeRef.Resolve(context);
		}

		IType ResolveInSignature(TypeSig typeReference)
		{
			return Resolve(typeReference.ToTypeDefOrRef(), isFromSignature: true);
		}
		#endregion

		#region Resolve Field
		public IField Resolve(dnlib.DotNet.IField fieldReference)
		{
			if (fieldReference == null)
				throw new ArgumentNullException(nameof(fieldReference));
			lock (fieldLookupCache) {
				IField field;
				if (!fieldLookupCache.TryGetValue(fieldReference, out field)) {
					field = FindNonGenericField(fieldReference);
					if (fieldReference.DeclaringType.ToTypeSig() is GenericInstSig git) {
						var typeArguments = git.GenericArguments.SelectArray(ResolveInSignature);
						field = (IField)field.Specialize(new TypeParameterSubstitution(typeArguments, null));
					}
					fieldLookupCache.Add(fieldReference, field);
				}
				return field;
			}
		}

		IField FindNonGenericField(dnlib.DotNet.IField fieldReference)
		{
			ITypeDefinition typeDef = Resolve(fieldReference.DeclaringType).GetDefinition();
			if (typeDef == null)
				return CreateFakeField(fieldReference);
			foreach (IField field in typeDef.Fields)
				if (field.Name == fieldReference.Name)
					return field;
			return CreateFakeField(fieldReference);
		}

		IField CreateFakeField(dnlib.DotNet.IField fieldReference)
		{
			var declaringType = Resolve(fieldReference.DeclaringType);
			var f = new DefaultUnresolvedField();
			f.Name = fieldReference.Name;
			lock (typeReferenceCecilLoader) {
				f.ReturnType = typeReferenceCecilLoader.ReadTypeReference(fieldReference.FieldSig.Type);
			}
			return new ResolvedFakeField(f, context.WithCurrentTypeDefinition(declaringType.GetDefinition()), declaringType);
		}

		class ResolvedFakeField : DefaultResolvedField
		{
			readonly IType declaringType;

			public ResolvedFakeField(DefaultUnresolvedField unresolved, ITypeResolveContext parentContext, IType declaringType)
				: base(unresolved, parentContext)
			{
				this.declaringType = declaringType;
			}

			public override IType DeclaringType
			{
				get { return declaringType; }
			}
		}
		#endregion

		#region Resolve Method
		public IMethod Resolve(dnlib.DotNet.IMethod methodReference)
		{
			if (methodReference == null)
				throw new ArgumentNullException(nameof(methodReference));
			lock (methodLookupCache) {
				IMethod method;
				if (!methodLookupCache.TryGetValue(methodReference, out method)) {
					method = FindNonGenericMethod(methodReference);
					if (methodReference.MethodSig.CallingConvention == CallingConvention.VarArg) {
						method = new VarArgInstanceMethod(
							method,
							methodReference.MethodSig.ParamsAfterSentinel is null ? new List<IType>() : methodReference.MethodSig.ParamsAfterSentinel.Select(ResolveInSignature)
						);
					} else if (methodReference is MethodSpec || methodReference.DeclaringType.ToTypeSig() is GenericInstSig) {
						IReadOnlyList<IType> classTypeArguments = null;
						IReadOnlyList<IType> methodTypeArguments = null;
						if (methodReference is MethodSpec gim && gim.GenericInstMethodSig != null) {
							methodTypeArguments = gim.GenericInstMethodSig.GenericArguments.SelectArray(ResolveInSignature);
						}
						if (methodReference.DeclaringType.ToTypeSig() is GenericInstSig git) {
							classTypeArguments = git.GenericArguments.SelectArray(ResolveInSignature);
						}
						method = method.Specialize(new TypeParameterSubstitution(classTypeArguments, methodTypeArguments));
					}
					methodLookupCache.Add(methodReference, method);
				}
				return method;
			}
		}

		IMethod FindNonGenericMethod(dnlib.DotNet.IMethod methodReference)
		{
			ITypeDefinition typeDef = Resolve(methodReference.DeclaringType).GetDefinition();
			if (typeDef == null)
				return CreateFakeMethod(methodReference);
			IEnumerable<IMethod> methods;
			if (methodReference.Name == ".ctor") {
				methods = typeDef.GetConstructors();
			} else if (methodReference.Name == ".cctor") {
				return typeDef.Methods.FirstOrDefault(m => m.IsConstructor && m.IsStatic);
			} else {
				methods = typeDef.GetMethods(m => m.Name == methodReference.Name, GetMemberOptions.IgnoreInheritedMembers)
					.Concat(typeDef.GetAccessors(m => m.Name == methodReference.Name, GetMemberOptions.IgnoreInheritedMembers));
			}
			if (methodReference.MDToken.Table == Table.Method) {
				foreach (var method in methods) {
					if (method.MetadataToken == methodReference.MDToken)
						return method;
				}
			}
			IType[] parameterTypes;
			if (methodReference.MethodSig.CallingConvention == CallingConvention.VarArg) {
				parameterTypes = methodReference.MethodSig.Params
					.TakeWhile(p => !p.IsSentinel)
					.Select(p => ResolveInSignature(p))
					.Concat(new[] { SpecialType.ArgList })
					.ToArray();
			} else {
				parameterTypes = methodReference.MethodSig.Params.SelectArray(p => ResolveInSignature(p));
			}
			var returnType = ResolveInSignature(methodReference.MethodSig.RetType);
			foreach (var method in methods) {
				if (method.TypeParameters.Count != methodReference.NumberOfGenericParameters)
					continue;
				if (!CompareSignatures(method.Parameters, parameterTypes) || !CompareTypes(method.ReturnType, returnType))
					continue;
				return method;
			}
			return CreateFakeMethod(methodReference);
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

		static bool IsVarArgMethod(IMethod method)
		{
			return method.Parameters.Count > 0 && method.Parameters[method.Parameters.Count - 1].Type.Kind == TypeKind.ArgList;
		}

		static bool CompareSignatures(IReadOnlyList<IParameter> parameters, IType[] parameterTypes)
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
		IMethod CreateFakeMethod(dnlib.DotNet.IMethod methodReference)
		{
			var m = new DefaultUnresolvedMethod();
			ITypeReference declaringTypeReference;
			lock (typeReferenceCecilLoader) {
				declaringTypeReference = typeReferenceCecilLoader.ReadTypeReference(methodReference.DeclaringType.ToTypeSig());
				if (methodReference.Name == ".ctor" || methodReference.Name == ".cctor")
					m.SymbolKind = SymbolKind.Constructor;
				m.Name = methodReference.Name;
				m.ReturnType = typeReferenceCecilLoader.ReadTypeReference(methodReference.MethodSig.RetType);
				m.IsStatic = !methodReference.MethodSig.HasThis;
				// not sure if empty is ok !?
				for (int i = 0; i < methodReference.NumberOfGenericParameters; i++) {
					m.TypeParameters.Add(new DefaultUnresolvedTypeParameter(SymbolKind.Method, i, string.Empty));
				}
				foreach (var p in methodReference.MethodSig.Params) {
					m.Parameters.Add(new DefaultUnresolvedParameter(typeReferenceCecilLoader.ReadTypeReference(p), string.Empty));
				}
			}
			var type = declaringTypeReference.Resolve(context);
			return new ResolvedFakeMethod(m, context.WithCurrentTypeDefinition(type.GetDefinition()), type);
		}

		class ResolvedFakeMethod : DefaultResolvedMethod
		{
			readonly IType declaringType;

			public ResolvedFakeMethod(DefaultUnresolvedMethod unresolved, ITypeResolveContext parentContext, IType declaringType)
				: base(unresolved, parentContext)
			{
				this.declaringType = declaringType;
			}

			public override IType DeclaringType
			{
				get { return declaringType; }
			}
		}
		#endregion

		#region Resolve Property
		public IProperty Resolve(PropertyDef propertyReference)
		{
			if (propertyReference == null)
				throw new ArgumentNullException(nameof(propertyReference));
			lock (propertyLookupCache) {
				IProperty property;
				if (!propertyLookupCache.TryGetValue(propertyReference, out property)) {
					property = FindNonGenericProperty(propertyReference);
					if (propertyReference.DeclaringType.ToTypeSig() is GenericInstSig git) {
						var typeArguments = git.GenericArguments.SelectArray(ResolveInSignature);
						property = (IProperty)property.Specialize(new TypeParameterSubstitution(typeArguments, null));
					}
					propertyLookupCache.Add(propertyReference, property);
				}
				return property;
			}
		}

		IProperty FindNonGenericProperty(PropertyDef propertyReference)
		{
			ITypeDefinition typeDef = Resolve(propertyReference.DeclaringType).GetDefinition();
			if (typeDef == null)
				return null;

			var parameters = propertyReference.GetParameters().ToList();
			var parameterTypes = parameters.SelectArray(p => ResolveInSignature(p.Type));
			var returnType = Resolve(propertyReference.PropertySig.RetType.ToTypeDefOrRef());
			foreach (IProperty property in typeDef.Properties) {
				if (property.Name == propertyReference.Name
				    && CompareTypes(property.ReturnType, returnType)
				    && CompareSignatures(property.Parameters, parameterTypes))
					return property;
			}
			return null;
		}
		#endregion

		#region Resolve Event
		public IEvent Resolve(EventDef eventReference)
		{
			if (eventReference == null)
				throw new ArgumentNullException("propertyReference");
			lock (eventLookupCache) {
				IEvent ev;
				if (!eventLookupCache.TryGetValue(eventReference, out ev)) {
					ev = FindNonGenericEvent(eventReference);
					if (eventReference.DeclaringType.ToTypeSig() is GenericInstSig git) {
						var typeArguments = git.GenericArguments.SelectArray(ResolveInSignature);
						ev = (IEvent)ev.Specialize(new TypeParameterSubstitution(typeArguments, null));
					}
					eventLookupCache.Add(eventReference, ev);
				}
				return ev;
			}
		}

		IEvent FindNonGenericEvent(EventDef eventReference)
		{
			ITypeDefinition typeDef = Resolve(eventReference.DeclaringType).GetDefinition();
			if (typeDef == null)
				return null;
			var returnType = Resolve(eventReference.EventType);
			foreach (IEvent ev in typeDef.Events) {
				if (ev.Name == eventReference.Name && CompareTypes(ev.ReturnType, returnType))
					return ev;
			}
			return null;
		}
		#endregion

		public IDecompilerTypeSystem GetSpecializingTypeSystem(TypeParameterSubstitution substitution)
		{
			if (substitution.Equals(TypeParameterSubstitution.Identity)) {
				return this;
			} else {
				return new SpecializingDecompilerTypeSystem(this, substitution);
			}
		}
	}
}
