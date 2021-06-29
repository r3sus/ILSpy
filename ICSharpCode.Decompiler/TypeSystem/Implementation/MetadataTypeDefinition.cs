// Copyright (c) 2018 Daniel Grunwald
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
using System.Linq;
using System.Runtime.InteropServices;
using dnlib.DotNet;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem.Implementation
{
	/// <summary>
	/// Type definition backed by System.Reflection.Metadata
	/// </summary>
	sealed class MetadataTypeDefinition : ITypeDefinition
	{
		readonly MetadataModule module;
		readonly TypeDef handle;

		// eagerly loaded:
		readonly FullTypeName fullTypeName;
		readonly TypeAttributes attributes;
		public TypeKind Kind { get; }
		public bool IsByRefLike { get; }
		public ITypeDefinition DeclaringTypeDefinition { get; }
		public IReadOnlyList<ITypeParameter> TypeParameters { get; }
		public KnownTypeCode KnownTypeCode { get; }
		public IType EnumUnderlyingType { get; }
		public bool HasExtensionMethods { get; }

		// lazy-loaded:
		IMember[] members;
		IField[] fields;
		IProperty[] properties;
		IEvent[] events;
		IMethod[] methods;
		List<IType> directBaseTypes;
		string defaultMemberName;

		internal MetadataTypeDefinition(MetadataModule module, TypeDef handle)
		{
			Debug.Assert(module != null);
			Debug.Assert(handle != null);
			this.module = module;
			this.handle = handle;
			var metadata = module.metadata;
			var td = handle;
			this.attributes = td.Attributes;
			this.fullTypeName = td.GetFullTypeName();
			// Find DeclaringType + KnownTypeCode:
			if (fullTypeName.IsNested) {
				this.DeclaringTypeDefinition = module.GetDefinition(td.DeclaringType);

				// Create type parameters:
				this.TypeParameters = MetadataTypeParameter.Create(module, this.DeclaringTypeDefinition, this, td.GenericParameters);
			} else {
				// Create type parameters:
				this.TypeParameters = MetadataTypeParameter.Create(module, this, td.GenericParameters);

				var topLevelTypeName = fullTypeName.TopLevelTypeName;
				for (int i = 0; i < KnownTypeReference.KnownTypeCodeCount; i++) {
					var ktr = KnownTypeReference.Get((KnownTypeCode)i);
					if (ktr != null && ktr.TypeName == topLevelTypeName) {
						this.KnownTypeCode = (KnownTypeCode)i;
						break;
					}
				}
			}
			// Find type kind:
			if ((attributes & TypeAttributes.ClassSemanticsMask) == TypeAttributes.Interface) {
				this.Kind = TypeKind.Interface;
			} else if (td.IsEnum) {
				this.Kind = TypeKind.Enum;
				this.EnumUnderlyingType = module.Compilation.FindType(td.GetEnumUnderlyingType().ElementType.ToKnownTypeCode());
			} else if (td.IsValueType) {
				if (KnownTypeCode == KnownTypeCode.Void) {
					this.Kind = TypeKind.Void;
				} else {
					this.Kind = TypeKind.Struct;
					this.IsByRefLike = td.CustomAttributes.HasKnownAttribute(metadata, KnownAttribute.IsByRefLike);
				}
			} else if (td.IsDelegate) {
				this.Kind = TypeKind.Delegate;
			} else {
				this.Kind = TypeKind.Class;
				this.HasExtensionMethods = this.IsStatic
					&& td.CustomAttributes.HasKnownAttribute(metadata, KnownAttribute.Extension);
			}
		}

		public override string ToString()
		{
			return $"{handle.MDToken.Raw:X8} {fullTypeName}";
		}

		ITypeDefinition[] nestedTypes;

		public IReadOnlyList<ITypeDefinition> NestedTypes {
			get {
				var nestedTypes = LazyInit.VolatileRead(ref this.nestedTypes);
				if (nestedTypes != null)
					return nestedTypes;
				var nestedTypeCollection = handle.NestedTypes;
				var nestedTypeList = new List<ITypeDefinition>(nestedTypeCollection.Count);
				foreach (TypeDef h in nestedTypeCollection) {
					nestedTypeList.Add(module.GetDefinition(h));
				}
				if ((module.TypeSystemOptions & TypeSystemOptions.Uncached) != 0)
					return nestedTypeList;
				return LazyInit.GetOrSet(ref this.nestedTypes, nestedTypeList.ToArray());
			}
		}

		#region Members
		public IReadOnlyList<IMember> Members {
			get {
				var members = LazyInit.VolatileRead(ref this.members);
				if (members != null)
					return members;
				members = this.Fields.Concat<IMember>(this.Methods).Concat(this.Properties).Concat(this.Events).ToArray();
				if ((module.TypeSystemOptions & TypeSystemOptions.Uncached) != 0)
					return members;
				return LazyInit.GetOrSet(ref this.members, members);
			}
		}

		public IEnumerable<IField> Fields {
			get {
				var fields = LazyInit.VolatileRead(ref this.fields);
				if (fields != null)
					return fields;
				var fieldCollection = handle.Fields;
				var fieldList = new List<IField>(fieldCollection.Count);
				foreach (FieldDef field in fieldCollection) {
					var attr = field.Attributes;
					if (module.IsVisible(attr) && (attr & FieldAttributes.SpecialName) == 0) {
						fieldList.Add(module.GetDefinition(field));
					}
				}
				if ((module.TypeSystemOptions & TypeSystemOptions.Uncached) != 0)
					return fieldList;
				return LazyInit.GetOrSet(ref this.fields, fieldList.ToArray());
			}
		}

		public IEnumerable<IProperty> Properties {
			get {
				var properties = LazyInit.VolatileRead(ref this.properties);
				if (properties != null)
					return properties;
				var propertyCollection = handle.Properties;
				var propertyList = new List<IProperty>(propertyCollection.Count);
				foreach (PropertyDef property in propertyCollection) {
					bool getterVisible = property.GetMethod != null && module.IsVisible(property.GetMethod.Attributes);
					bool setterVisible = property.SetMethod != null && module.IsVisible(property.SetMethod.Attributes);
					if (getterVisible || setterVisible) {
						propertyList.Add(module.GetDefinition(property));
					}
				}
				if ((module.TypeSystemOptions & TypeSystemOptions.Uncached) != 0)
					return propertyList;
				return LazyInit.GetOrSet(ref this.properties, propertyList.ToArray());
			}
		}

		public IEnumerable<IEvent> Events {
			get {
				var events = LazyInit.VolatileRead(ref this.events);
				if (events != null)
					return events;
				var eventCollection = handle.Events;
				var eventList = new List<IEvent>(eventCollection.Count);
				foreach (EventDef ev in eventCollection) {
					if (ev.AddMethod == null)
						continue;
					var addMethod = ev.AddMethod;
					if (module.IsVisible(addMethod.Attributes)) {
						eventList.Add(module.GetDefinition(ev));
					}
				}
				if ((module.TypeSystemOptions & TypeSystemOptions.Uncached) != 0)
					return eventList;
				return LazyInit.GetOrSet(ref this.events, eventList.ToArray());
			}
		}

		public IEnumerable<IMethod> Methods {
			get {
				var methods = LazyInit.VolatileRead(ref this.methods);
				if (methods != null)
					return methods;
				var methodsCollection = handle.Methods;
				var methodsList = new List<IMethod>(methodsCollection.Count);
				foreach (MethodDef md in methodsCollection) {
					if (md.SemanticsAttributes == MethodSemanticsAttributes.None && module.IsVisible(md.Attributes)) {
						methodsList.Add(module.GetDefinition(md));
					}
				}
				if (this.Kind == TypeKind.Struct || this.Kind == TypeKind.Enum) {
					methodsList.Add(FakeMethod.CreateDummyConstructor(Compilation, this, IsAbstract ? Accessibility.Protected : Accessibility.Public));
				}
				if ((module.TypeSystemOptions & TypeSystemOptions.Uncached) != 0)
					return methodsList;
				return LazyInit.GetOrSet(ref this.methods, methodsList.ToArray());
			}
		}
		#endregion

		public IType DeclaringType => DeclaringTypeDefinition;

		public bool? IsReferenceType {
			get {
				switch (Kind) {
					case TypeKind.Struct:
					case TypeKind.Enum:
					case TypeKind.Void:
						return false;
					default:
						return true;
				}
			}
		}

		public int TypeParameterCount => TypeParameters.Count;

		IReadOnlyList<IType> IType.TypeArguments => TypeParameters;

		public IEnumerable<IType> DirectBaseTypes {
			get {
				var baseTypes = LazyInit.VolatileRead(ref this.directBaseTypes);
				if (baseTypes != null)
					return baseTypes;
				var context = new GenericContext(TypeParameters);
				var interfaceImplCollection = handle.Interfaces;
				baseTypes = new List<IType>(1 + interfaceImplCollection.Count);
				ITypeDefOrRef baseType = handle.BaseType;
				if (baseType != null) {
					baseTypes.Add(module.ResolveType(baseType, context));
				} else if (Kind == TypeKind.Interface) {
					// td.BaseType.IsNil is always true for interfaces,
					// but the type system expects every interface to derive from System.Object as well.
					baseTypes.Add(Compilation.FindType(KnownTypeCode.Object));
				}
				foreach (var iface in interfaceImplCollection) {
					baseTypes.Add(module.ResolveType(iface.Interface, context, iface));
				}
				return LazyInit.GetOrSet(ref this.directBaseTypes, baseTypes);
			}
		}

		public IMDTokenProvider MetadataToken => handle;

		public FullTypeName FullTypeName => fullTypeName;
		public string Name => fullTypeName.Name;

		public IModule ParentModule => module;

		#region Type Attributes
		public IEnumerable<IAttribute> GetAttributes()
		{
			var b = new AttributeListBuilder(module);

			// SerializableAttribute
			if ((handle.Attributes & TypeAttributes.Serializable) != 0)
				b.Add(KnownAttribute.Serializable);

			// ComImportAttribute
			if ((handle.Attributes & TypeAttributes.Import) != 0)
				b.Add(KnownAttribute.ComImport);

			#region StructLayoutAttribute
			LayoutKind layoutKind = LayoutKind.Auto;
			switch (handle.Attributes & TypeAttributes.LayoutMask) {
				case TypeAttributes.SequentialLayout:
					layoutKind = LayoutKind.Sequential;
					break;
				case TypeAttributes.ExplicitLayout:
					layoutKind = LayoutKind.Explicit;
					break;
			}
			CharSet charSet = CharSet.None;
			switch (handle.Attributes & TypeAttributes.StringFormatMask) {
				case TypeAttributes.AnsiClass:
					charSet = CharSet.Ansi;
					break;
				case TypeAttributes.AutoClass:
					charSet = CharSet.Auto;
					break;
				case TypeAttributes.UnicodeClass:
					charSet = CharSet.Unicode;
					break;
			}

			LayoutKind defaultLayoutKind = Kind == TypeKind.Struct ? LayoutKind.Sequential : LayoutKind.Auto;
			if (layoutKind != defaultLayoutKind || charSet != CharSet.Ansi || (handle.PackingSize > 0 && handle.PackingSize != ushort.MaxValue) || (handle.ClassSize > 0 && handle.ClassSize != uint.MaxValue)) {
				var structLayout = new AttributeBuilder(module, KnownAttribute.StructLayout);
				structLayout.AddFixedArg(
					new TopLevelTypeName("System.Runtime.InteropServices", "LayoutKind"),
					(int)layoutKind);
				if (charSet != CharSet.Ansi) {
					var charSetType = Compilation.FindType(new TopLevelTypeName("System.Runtime.InteropServices", "CharSet"));
					structLayout.AddNamedArg("CharSet", charSetType, (int)charSet);
				}
				if (handle.PackingSize > 0 && handle.PackingSize != ushort.MaxValue) {
					structLayout.AddNamedArg("Pack", KnownTypeCode.Int32, (int)handle.PackingSize);
				}
				if (handle.ClassSize > 0 && handle.ClassSize != uint.MaxValue) {
					structLayout.AddNamedArg("Size", KnownTypeCode.Int32, (int)handle.ClassSize);
				}
				b.Add(structLayout.Build());
			}

			#endregion

			b.Add(handle.CustomAttributes);
			b.AddSecurityAttributes(handle.DeclSecurities);

			return b.Build();
		}

		public string DefaultMemberName {
			get {
				string defaultMemberName = LazyInit.VolatileRead(ref this.defaultMemberName);
				if (defaultMemberName != null)
					return defaultMemberName;
				foreach (var a in handle.CustomAttributes) {
					if (!a.IsKnownAttribute(KnownAttribute.DefaultMember))
						continue;
					if (a.ConstructorArguments.Count == 1 && a.ConstructorArguments[0].Value is UTF8String name) {
						defaultMemberName = name;
						break;
					}
				}
				return LazyInit.GetOrSet(ref this.defaultMemberName, defaultMemberName ?? "Item");
			}
		}
		#endregion

		public Accessibility Accessibility {
			get {
				switch (attributes & TypeAttributes.VisibilityMask) {
					case TypeAttributes.NotPublic:
					case TypeAttributes.NestedAssembly:
						return Accessibility.Internal;
					case TypeAttributes.Public:
					case TypeAttributes.NestedPublic:
						return Accessibility.Public;
					case TypeAttributes.NestedPrivate:
						return Accessibility.Private;
					case TypeAttributes.NestedFamily:
						return Accessibility.Protected;
					case TypeAttributes.NestedFamANDAssem:
						return Accessibility.ProtectedAndInternal;
					case TypeAttributes.NestedFamORAssem:
						return Accessibility.ProtectedOrInternal;
					default:
						return Accessibility.None;
				}
			}
		}

		public bool IsStatic => (attributes & (TypeAttributes.Abstract | TypeAttributes.Sealed)) == (TypeAttributes.Abstract | TypeAttributes.Sealed);
		public bool IsAbstract => (attributes & TypeAttributes.Abstract) != 0;
		public bool IsSealed => (attributes & TypeAttributes.Sealed) != 0;

		public SymbolKind SymbolKind => SymbolKind.TypeDefinition;

		public ICompilation Compilation => module.Compilation;

		public string FullName {
			get {
				if (DeclaringType != null)
					return DeclaringType.FullName + "." + Name;
				if (!string.IsNullOrEmpty(this.Namespace))
					return this.Namespace + "." + Name;
				return Name;
			}
		}

		public string ReflectionName => fullTypeName.ReflectionName;
		public string Namespace => fullTypeName.TopLevelTypeName.Namespace;

		ITypeDefinition IType.GetDefinition() => this;
		TypeParameterSubstitution IType.GetSubstitution() => TypeParameterSubstitution.Identity;

		public IType AcceptVisitor(TypeVisitor visitor)
		{
			return visitor.VisitTypeDefinition(this);
		}

		IType IType.VisitChildren(TypeVisitor visitor)
		{
			return this;
		}

		public override bool Equals(object obj)
		{
			if (obj is MetadataTypeDefinition td) {
				return handle == td.handle && module.PEFile == td.module.PEFile;
			}
			return false;
		}

		public override int GetHashCode()
		{
			return 0x2e0520f2 ^ module.PEFile.GetHashCode() ^ handle.GetHashCode();
		}

		bool IEquatable<IType>.Equals(IType other)
		{
			return Equals(other);
		}

		#region GetNestedTypes
		public IEnumerable<IType> GetNestedTypes(Predicate<ITypeDefinition> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			const GetMemberOptions opt = GetMemberOptions.IgnoreInheritedMembers | GetMemberOptions.ReturnMemberDefinitions;
			if ((options & opt) == opt) {
				return GetFiltered(this.NestedTypes, filter);
			} else {
				return GetMembersHelper.GetNestedTypes(this, filter, options);
			}
		}

		public IEnumerable<IType> GetNestedTypes(IReadOnlyList<IType> typeArguments, Predicate<ITypeDefinition> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			return GetMembersHelper.GetNestedTypes(this, typeArguments, filter, options);
		}
		#endregion

		#region GetMembers()
		IEnumerable<T> GetFiltered<T>(IEnumerable<T> input, Predicate<T> filter) where T : class
		{
			if (filter == null)
				return input;
			else
				return ApplyFilter(input, filter);
		}

		IEnumerable<T> ApplyFilter<T>(IEnumerable<T> input, Predicate<T> filter) where T : class
		{
			foreach (var member in input) {
				if (filter(member))
					yield return member;
			}
		}

		public IEnumerable<IMethod> GetMethods(Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Methods, ExtensionMethods.And(m => !m.IsConstructor, filter));
			} else {
				return GetMembersHelper.GetMethods(this, filter, options);
			}
		}

		public IEnumerable<IMethod> GetMethods(IReadOnlyList<IType> typeArguments, Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			return GetMembersHelper.GetMethods(this, typeArguments, filter, options);
		}

		public IEnumerable<IMethod> GetConstructors(Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.IgnoreInheritedMembers)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if (ComHelper.IsComImport(this)) {
				IType coClass = ComHelper.GetCoClass(this);
				using (var busyLock = BusyManager.Enter(this)) {
					if (busyLock.Success) {
						return coClass.GetConstructors(filter, options)
							.Select(m => new SpecializedMethod(m, m.Substitution) { DeclaringType = this });
					}
				}
				return EmptyList<IMethod>.Instance;
			}
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Methods, ExtensionMethods.And(m => m.IsConstructor && !m.IsStatic, filter));
			} else {
				return GetMembersHelper.GetConstructors(this, filter, options);
			}
		}

		public IEnumerable<IProperty> GetProperties(Predicate<IProperty> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IProperty>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Properties, filter);
			} else {
				return GetMembersHelper.GetProperties(this, filter, options);
			}
		}

		public IEnumerable<IField> GetFields(Predicate<IField> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IField>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Fields, filter);
			} else {
				return GetMembersHelper.GetFields(this, filter, options);
			}
		}

		public IEnumerable<IEvent> GetEvents(Predicate<IEvent> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IEvent>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Events, filter);
			} else {
				return GetMembersHelper.GetEvents(this, filter, options);
			}
		}

		public IEnumerable<IMember> GetMembers(Predicate<IMember> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFiltered(this.Members, filter);
			} else {
				return GetMembersHelper.GetMembers(this, filter, options);
			}
		}

		public IEnumerable<IMethod> GetAccessors(Predicate<IMethod> filter = null, GetMemberOptions options = GetMemberOptions.None)
		{
			if (Kind == TypeKind.Void)
				return EmptyList<IMethod>.Instance;
			if ((options & GetMemberOptions.IgnoreInheritedMembers) == GetMemberOptions.IgnoreInheritedMembers) {
				return GetFilteredAccessors(filter);
			} else {
				return GetMembersHelper.GetAccessors(this, filter, options);
			}
		}

		IEnumerable<IMethod> GetFilteredAccessors(Predicate<IMethod> filter)
		{
			foreach (var prop in this.Properties) {
				var getter = prop.Getter;
				if (getter != null && (filter == null || filter(getter)))
					yield return getter;
				var setter = prop.Setter;
				if (setter != null && (filter == null || filter(setter)))
					yield return setter;
			}
			foreach (var ev in this.Events) {
				var adder = ev.AddAccessor;
				if (adder != null && (filter == null || filter(adder)))
					yield return adder;
				var remover = ev.RemoveAccessor;
				if (remover != null && (filter == null || filter(remover)))
					yield return remover;
				var invoker = ev.InvokeAccessor;
				if (invoker != null && (filter == null || filter(invoker)))
					yield return remover;
			}
		}
		#endregion
	}
}
