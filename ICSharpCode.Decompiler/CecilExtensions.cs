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
using System.Linq;
using dnlib.DotNet;
using dnlib.DotNet.Emit;

namespace ICSharpCode.Decompiler
{
	/// <summary>
	/// Cecil helper methods.
	/// </summary>
	public static class CecilExtensions
	{
		/// <summary>
		/// Gets the (exclusive) end offset of this instruction.
		/// </summary>
		public static int GetEndOffset(this Instruction inst)
		{
			if (inst == null)
				throw new ArgumentNullException(nameof(inst));
			return (int)inst.Offset + inst.GetSize();
		}

		public static string OffsetToString(int offset)
		{
			return string.Format("IL_{0:x4}", offset);
		}

		public static HashSet<MethodDef> GetAccessorMethods(this TypeDef type)
		{
			HashSet<MethodDef> accessorMethods = new HashSet<MethodDef>();
			foreach (var property in type.Properties) {
				accessorMethods.Add(property.GetMethod);
				accessorMethods.Add(property.SetMethod);
				if (property.HasOtherMethods) {
					foreach (var m in property.OtherMethods)
						accessorMethods.Add(m);
				}
			}
			foreach (var ev in type.Events) {
				accessorMethods.Add(ev.AddMethod);
				accessorMethods.Add(ev.RemoveMethod);
				accessorMethods.Add(ev.InvokeMethod);
				if (ev.HasOtherMethods) {
					foreach (var m in ev.OtherMethods)
						accessorMethods.Add(m);
				}
			}
			return accessorMethods;
		}

		public static FieldDef ResolveFieldWithinSameModule(this IField field)
		{
			if (field != null && field.DeclaringType != null && field.DeclaringType.Scope == field.Module)
				return field is FieldDef ? (FieldDef)field : ((MemberRef)field).ResolveField();
			else
				return null;
		}

		public static MethodDef Resolve(this IMethod method)
		{
			if (method is MethodSpec)
				method = ((MethodSpec)method).Method;
			if (method is MemberRef)
				return ((MemberRef)method).ResolveMethod();
			else
				return (MethodDef)method;
		}

		public static FieldDef Resolve(this IField field)
		{
			if (field is MemberRef)
				return ((MemberRef)field).ResolveField();
			else
				return (FieldDef)field;
		}

		public static TypeDef Resolve(this IType type)
		{
			return type == null ? null : type.GetScopeTypeDefOrRef().ResolveTypeDef();
		}

		public static ITypeDefOrRef GetScopeTypeDefOrRef(this IType type) {
			var t = type.GetScopeType();
			if (t is ITypeDefOrRef tdr)
				return tdr;
			if (t is TypeSig sig)
				return sig.ToTypeDefOrRef();
			return null;
		}

		public static IType GetScopeType(this IType type) {
			if (type is TypeDef td)
				return td;
			if (type is TypeRef tr)
				return tr;
			if (!(type is TypeSig sig)) {
				if (!(type is TypeSpec ts))
					return type;
				sig = ts.TypeSig;
			}
			sig = sig.RemovePinnedAndModifiers();
			if (sig is GenericInstSig gis)
				return gis.GenericType?.TypeDefOrRef;
			if (sig is TypeDefOrRefSig tdrs)
				return tdrs.TypeDefOrRef;
			return type;
		}

		public static bool IsCompilerGeneratedOrIsInCompilerGeneratedClass(this MethodDef method)
		{
			if (method.IsCompilerGenerated())
				return true;
			return method.DeclaringType != null && method.DeclaringType.IsCompilerGenerated();
		}

		public static bool IsCompilerGeneratedOrIsInCompilerGeneratedClass(this TypeDef type)
		{
			if (type.IsCompilerGenerated())
				return true;
			TypeDef declaringTypeHandle = type.DeclaringType;
			if (declaringTypeHandle != null && declaringTypeHandle.IsCompilerGenerated())
				return true;
			return false;
		}

		public static bool IsCompilerGenerated(this IHasCustomAttribute  provider)
		{
			if (provider != null && provider.HasCustomAttributes) {
				foreach (CustomAttribute a in provider.CustomAttributes) {
					if (a.AttributeType.FullName == "System.Runtime.CompilerServices.CompilerGeneratedAttribute")
						return true;
				}
			}
			return false;
		}

		public static bool IsAnonymousType(this ITypeDefOrRef type)
		{
			if (type == null)
				return false;
			if (string.IsNullOrEmpty(type.Namespace) && type.HasGeneratedName() && (type.Name.Contains("AnonType") || type.Name.Contains("AnonymousType"))) {
				TypeDef td = type.Resolve();
				return td != null && td.IsCompilerGenerated();
			}
			return false;
		}

		public static bool HasGeneratedName(this IMemberRef member)
		{
			return member.Name.StartsWith("<", StringComparison.Ordinal);
		}

		public static string GetDefaultMemberName(this TypeDef type, out CustomAttribute defaultMemberAttribute)
		{
			if (type != null)
				foreach (CustomAttribute ca in type.CustomAttributes.FindAll("System.Reflection.DefaultMemberAttribute"))
					if (ca.Constructor != null && ca.Constructor.FullName == @"System.Void System.Reflection.DefaultMemberAttribute::.ctor(System.String)" &&
						ca.ConstructorArguments.Count == 1 &&
						ca.ConstructorArguments[0].Value is UTF8String) {
						defaultMemberAttribute = ca;
						return (UTF8String)ca.ConstructorArguments[0].Value;
					}
			defaultMemberAttribute = null;
			return null;
		}

		public static bool IsIndexer(this PropertyDef property)
		{
			CustomAttribute attr;
			return property.IsIndexer(out attr);
		}

		public static bool IsIndexer(this PropertyDef property, out CustomAttribute defaultMemberAttribute)
		{
			defaultMemberAttribute = null;
			if (property != null && property.PropertySig.GetParamCount() > 0) {
				var accessor = property.GetMethod ?? property.SetMethod;
				PropertyDef basePropDef = property;
				if (accessor.HasOverrides) {
					// if the property is explicitly implementing an interface, look up the property in the interface:
					MethodDef baseAccessor = accessor.Overrides.First().MethodDeclaration.Resolve();
					if (baseAccessor != null) {
						foreach (PropertyDef baseProp in baseAccessor.DeclaringType.Properties) {
							if (baseProp.GetMethod == baseAccessor || baseProp.SetMethod == baseAccessor) {
								basePropDef = baseProp;
								break;
							}
						}
					} else
						return false;
				}
				CustomAttribute attr;
				var defaultMemberName = basePropDef.DeclaringType.GetDefaultMemberName(out attr);
				if (defaultMemberName == basePropDef.Name) {
					defaultMemberAttribute = attr;
					return true;
				}
			}
			return false;
		}

		public static bool IsUnconditionalBranch(this OpCode opcode)
		{
			if (opcode.OpCodeType == OpCodeType.Prefix)
				return false;
			switch (opcode.FlowControl) {
				case FlowControl.Branch:
				case FlowControl.Throw:
				case FlowControl.Return:
					return true;
				case FlowControl.Next:
				case FlowControl.Call:
				case FlowControl.Cond_Branch:
					return false;
				default:
					throw new NotSupportedException(opcode.FlowControl.ToString());
			}
		}

		public static TypeSystem.FullTypeName GetFullTypeName(this IType typeDef)
		{
			return new TypeSystem.FullTypeName(typeDef.FullName, true);
		}

		public static bool IsDelegate(this TypeDef type)
		{
			if (type.BaseType != null && type.BaseType.Namespace == "System") {
				if (type.BaseType.Name == "MulticastDelegate")
					return true;
				if (type.BaseType.Name == "Delegate" && type.Name != "MulticastDelegate")
					return true;
			}
			return false;
		}

		public static int GetCodeSize(this CilBody body)
		{
			if (body.Instructions.Count == 0)
				return 0;
			var instr = body.Instructions.Last();
			return instr.GetEndOffset();
		}

		public static IList<Parameter> GetParameters(this IMethod method) {
			if (method is null || method.MethodSig is null)
				return new List<Parameter>();

			if (method is MethodDef md)
				return md.Parameters;

			var list = new List<Parameter>();
			int paramIndex = 0, methodSigIndex = 0;
			if (method.MethodSig.HasThis)
				list.Add(new Parameter(paramIndex++, Parameter.HIDDEN_THIS_METHOD_SIG_INDEX, method.DeclaringType.ToTypeSig()));
			foreach (var type in method.MethodSig.GetParams())
				list.Add(new Parameter(paramIndex++, methodSigIndex++, type));
			return list;
		}

		public static IEnumerable<Parameter> GetParameters(this PropertyDef property)
		{
			if (property == null)
				yield break;
			if (property.GetMethod != null)
			{
				foreach (var param in property.GetMethod.Parameters)
					yield return param;
				yield break;
			}
			if (property.SetMethod != null)
			{
				int last = property.SetMethod.Parameters.Count - 1;
				foreach (var param in property.SetMethod.Parameters)
				{
					if (param.Index != last)
						yield return param;
				}
				yield break;
			}

			int i = 0;
			foreach (TypeSig param in property.PropertySig.GetParameters())
			{
				yield return new Parameter(i,i,param);
				i++;
			}
		}

		public static IList<TypeSig> GetParameters(this MethodBaseSig methodSig)
		{
			if (methodSig == null)
				return new List<TypeSig>();
			if (methodSig.ParamsAfterSentinel != null)
				return methodSig.Params
								.Concat(new TypeSig[] { new SentinelSig() })
								.Concat(methodSig.ParamsAfterSentinel)
								.ToList();
			else
				return methodSig.Params;
		}

		public static int GetParametersSkip(this IList<Parameter> parameters)
		{
			if (parameters == null || parameters.Count == 0)
				return 0;
			if (parameters[0].IsHiddenThisParameter)
				return 1;
			return 0;
		}

		public static int GetNumberOfNormalParameters(this IList<Parameter> parameters)
		{
			if (parameters == null)
				return 0;
			return parameters.Count - GetParametersSkip(parameters);
		}

		public static Instruction GetPrevious(this CilBody body, Instruction instr)
		{
			int index = body.Instructions.IndexOf(instr);
			if (index <= 0)
				return null;
			return body.Instructions[index - 1];
		}

		public static bool IsValueType(ITypeDefOrRef tdr)
		{
			if (tdr == null)
				return false;
			var ts = tdr as TypeSpec;
			if (ts != null)
				return IsValueType(ts.TypeSig);
			return tdr.IsValueType;
		}

		public static bool IsValueType(TypeSig ts) => ts?.IsValueType ?? false;

		public static string GetScopeName(this IScope scope)
		{
			if (scope == null)
				return string.Empty;
			if (scope is IFullName)
				return ((IFullName)scope).Name;
			else
				return scope.ScopeName;	// Shouldn't be reached
		}

		internal static bool HasKnownAttribute(this CustomAttributeCollection customAttributes, TypeSystem.KnownAttribute type)
		{
			foreach (var customAttribute in customAttributes) {
				if (customAttribute.IsKnownAttribute(type))
					return true;
			}
			return false;
		}

		internal static bool IsKnownAttribute(this CustomAttribute attr, TypeSystem.KnownAttribute attrType)
		{
			return attr.AttributeType.IsKnownType(attrType);
		}

		internal static bool IsKnownType(this ITypeDefOrRef handle, TypeSystem.KnownAttribute knownType)
		{
			return handle.GetFullTypeName() == TypeSystem.KnownAttributes.GetTypeName(knownType);
		}

		public static TypeSystem.Nullability? GetNullableContext(this CustomAttributeCollection customAttributes)
		{
			foreach (var customAttribute in customAttributes) {
				if (customAttribute.IsKnownAttribute(TypeSystem.KnownAttribute.NullableContext)) {
					if (customAttribute.ConstructorArguments.Count == 1 && customAttribute.ConstructorArguments[0].Value is byte b && b <= 2) {
						return (TypeSystem.Nullability)b;
					}
				}
			}
			return null;
		}

		public static TypeSystem.KnownTypeCode ToKnownTypeCode(this ElementType typeCode)
		{
			switch (typeCode) {
				case ElementType.Boolean:
					return TypeSystem.KnownTypeCode.Boolean;
				case ElementType.U1:
					return TypeSystem.KnownTypeCode.Byte;
				case ElementType.I1:
					return TypeSystem.KnownTypeCode.SByte;
				case ElementType.Char:
					return TypeSystem.KnownTypeCode.Char;
				case ElementType.I2:
					return TypeSystem.KnownTypeCode.Int16;
				case ElementType.U2:
					return TypeSystem.KnownTypeCode.UInt16;
				case ElementType.I4:
					return TypeSystem.KnownTypeCode.Int32;
				case ElementType.U4:
					return TypeSystem.KnownTypeCode.UInt32;
				case ElementType.I8:
					return TypeSystem.KnownTypeCode.Int64;
				case ElementType.U8:
					return TypeSystem.KnownTypeCode.UInt64;
				case ElementType.R4:
					return TypeSystem.KnownTypeCode.Single;
				case ElementType.R8:
					return TypeSystem.KnownTypeCode.Double;
				case ElementType.I:
					return TypeSystem.KnownTypeCode.IntPtr;
				case ElementType.U:
					return TypeSystem.KnownTypeCode.UIntPtr;
				case ElementType.Object:
					return TypeSystem.KnownTypeCode.Object;
				case ElementType.String:
					return TypeSystem.KnownTypeCode.String;
				case ElementType.TypedByRef:
					return TypeSystem.KnownTypeCode.TypedReference;
				case ElementType.Void:
					return TypeSystem.KnownTypeCode.Void;
				default:
					return TypeSystem.KnownTypeCode.None;
			}
		}
	}
}
