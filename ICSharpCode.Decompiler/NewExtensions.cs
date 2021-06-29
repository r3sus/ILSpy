using System;
using dnlib.DotNet;
using ICSharpCode.Decompiler.TypeSystem;

namespace ICSharpCode.Decompiler
{
	public static class NewExtensions
	{
		internal static bool HasKnownAttribute(this CustomAttributeCollection customAttributes, ModuleDef metadata, KnownAttribute type)
		{
			foreach (var customAttribute in customAttributes) {
				if (customAttribute.IsKnownAttribute(metadata, type))
					return true;
			}
			return false;
		}

		internal static bool IsKnownAttribute(this dnlib.DotNet.CustomAttribute attr, ModuleDef metadata, KnownAttribute attrType)
		{
			return attr.AttributeType.IsKnownType(metadata, attrType);
		}

		public static bool IsKnownType(this ITypeDefOrRef handle, ModuleDef reader, KnownTypeCode knownType)
		{
			string name = ReflectionHelper.SplitTypeParameterCountFromReflectionName(handle.Name, out var typeParameterCount);
			return new TopLevelTypeName(handle.Namespace, name, typeParameterCount) == KnownTypeReference.Get(knownType).TypeName;
		}

		internal static bool IsKnownType(this ITypeDefOrRef handle, ModuleDef reader, KnownAttribute knownType)
		{
			string name = ReflectionHelper.SplitTypeParameterCountFromReflectionName(handle.Name, out var typeParameterCount);
			return new TopLevelTypeName(handle.Namespace, name, typeParameterCount) == knownType.GetTypeName();
		}

		public static ElementType ToPrimitiveTypeCode(this KnownTypeCode typeCode)
		{
			switch (typeCode) {
				case KnownTypeCode.Object:
					return ElementType.Object;
				case KnownTypeCode.Boolean:
					return ElementType.Boolean;
				case KnownTypeCode.Char:
					return ElementType.Char;
				case KnownTypeCode.SByte:
					return ElementType.I1;
				case KnownTypeCode.Byte:
					return ElementType.U1;
				case KnownTypeCode.Int16:
					return ElementType.I2;
				case KnownTypeCode.UInt16:
					return ElementType.U2;
				case KnownTypeCode.Int32:
					return ElementType.I4;
				case KnownTypeCode.UInt32:
					return ElementType.U4;
				case KnownTypeCode.Int64:
					return ElementType.I8;
				case KnownTypeCode.UInt64:
					return ElementType.U8;
				case KnownTypeCode.Single:
					return ElementType.R4;
				case KnownTypeCode.Double:
					return ElementType.R8;
				case KnownTypeCode.String:
					return ElementType.String;
				case KnownTypeCode.Void:
					return ElementType.Void;
				case KnownTypeCode.TypedReference:
					return ElementType.TypedByRef;
				case KnownTypeCode.IntPtr:
					return ElementType.I;
				case KnownTypeCode.UIntPtr:
					return ElementType.U;
				default:
					throw new ArgumentOutOfRangeException();
			}
		}

		public static KnownTypeCode ToKnownTypeCode(this ElementType typeCode)
		{
			switch (typeCode) {
				case ElementType.Boolean:
					return KnownTypeCode.Boolean;
				case ElementType.U1:
					return KnownTypeCode.Byte;
				case ElementType.I1:
					return KnownTypeCode.SByte;
				case ElementType.Char:
					return KnownTypeCode.Char;
				case ElementType.I2:
					return KnownTypeCode.Int16;
				case ElementType.U2:
					return KnownTypeCode.UInt16;
				case ElementType.I4:
					return KnownTypeCode.Int32;
				case ElementType.U4:
					return KnownTypeCode.UInt32;
				case ElementType.I8:
					return KnownTypeCode.Int64;
				case ElementType.U8:
					return KnownTypeCode.UInt64;
				case ElementType.R4:
					return KnownTypeCode.Single;
				case ElementType.R8:
					return KnownTypeCode.Double;
				case ElementType.I:
					return KnownTypeCode.IntPtr;
				case ElementType.U:
					return KnownTypeCode.UIntPtr;
				case ElementType.Object:
					return KnownTypeCode.Object;
				case ElementType.String:
					return KnownTypeCode.String;
				case ElementType.TypedByRef:
					return KnownTypeCode.TypedReference;
				case ElementType.Void:
					return KnownTypeCode.Void;
				default:
					return KnownTypeCode.None;
			}
		}

		public static FullTypeName GetFullTypeName(this ExportedType type, ModuleDef metadata)
		{
			string ns = type.Namespace;
			string name = ReflectionHelper.SplitTypeParameterCountFromReflectionName(type.Name, out int typeParameterCount);
			return new TopLevelTypeName(ns, name, typeParameterCount);
		}
	}
}
