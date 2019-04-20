using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using dnlib.DotNet;
using dnlib.DotNet.Emit;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using IField = ICSharpCode.Decompiler.TypeSystem.IField;
using IMethod = ICSharpCode.Decompiler.TypeSystem.IMethod;
using IType = ICSharpCode.Decompiler.TypeSystem.IType;

namespace ICSharpCode.Decompiler.CSharp
{
	class RequiredNamespaceCollector
	{
		public static void CollectNamespaces(MetadataModule module, HashSet<string> namespaces)
		{
			foreach (var type in module.TypeDefinitions) {
				CollectNamespaces(type, module, namespaces);
			}
			CollectAttributeNamespaces(module, namespaces);
		}

		public static void CollectAttributeNamespaces(MetadataModule module, HashSet<string> namespaces)
		{
			HandleAttributes(module.GetAssemblyAttributes(), namespaces);
			HandleAttributes(module.GetModuleAttributes(), namespaces);
		}

		static readonly GenericContext genericContext = default;

		public static void CollectNamespaces(IEntity entity, MetadataModule module,
			HashSet<string> namespaces, CodeMappingInfo mappingInfo = null)
		{
			if (entity == null || entity.MetadataToken is null)
				return;
			switch (entity) {
				case ITypeDefinition td:
					if (mappingInfo == null)
						mappingInfo = CSharpDecompiler.GetCodeMappingInfo(entity.ParentModule.PEFile, entity.MetadataToken);
					namespaces.Add(td.Namespace);
					HandleAttributes(td.GetAttributes(), namespaces);
					HandleTypeParameters(td.TypeParameters, namespaces);

					foreach (var baseType in td.DirectBaseTypes) {
						CollectNamespacesForTypeReference(baseType, namespaces);
					}

					foreach (var nestedType in td.NestedTypes) {
						CollectNamespaces(nestedType, module, namespaces, mappingInfo);
					}

					foreach (var field in td.Fields) {
						CollectNamespaces(field, module, namespaces, mappingInfo);
					}

					foreach (var property in td.Properties) {
						CollectNamespaces(property, module, namespaces, mappingInfo);
					}

					foreach (var @event in td.Events) {
						CollectNamespaces(@event, module, namespaces, mappingInfo);
					}

					foreach (var method in td.Methods) {
						CollectNamespaces(method, module, namespaces, mappingInfo);
					}
					break;
				case IField field:
					HandleAttributes(field.GetAttributes(), namespaces);
					CollectNamespacesForTypeReference(field.ReturnType, namespaces);
					break;
				case IMethod method:
					HandleAttributes(method.GetAttributes(), namespaces);
					HandleAttributes(method.GetReturnTypeAttributes(), namespaces);
					CollectNamespacesForTypeReference(method.ReturnType, namespaces);
					foreach (var param in method.Parameters) {
						HandleAttributes(param.GetAttributes(), namespaces);
						CollectNamespacesForTypeReference(param.Type, namespaces);
					}
					HandleTypeParameters(method.TypeParameters, namespaces);
					if (method.MetadataToken != null) {
						if (mappingInfo == null)
							mappingInfo = CSharpDecompiler.GetCodeMappingInfo(entity.ParentModule.PEFile, entity.MetadataToken);
						var parts = mappingInfo.GetMethodParts((MethodDef)method.MetadataToken).ToList();
						foreach (var methodDef in parts) {
							HandleOverrides(methodDef.Overrides, module, namespaces);
							if (methodDef.HasBody)
								CollectNamespacesFromMethodBody(methodDef.Body, module, namespaces);
						}
					}
					break;
				case IProperty property:
					HandleAttributes(property.GetAttributes(), namespaces);
					CollectNamespaces(property.Getter, module, namespaces);
					CollectNamespaces(property.Setter, module, namespaces);
					break;
				case IEvent @event:
					HandleAttributes(@event.GetAttributes(), namespaces);
					CollectNamespaces(@event.AddAccessor, module, namespaces);
					CollectNamespaces(@event.RemoveAccessor, module, namespaces);
					break;
			}
		}

		static void HandleOverrides(IList<MethodOverride> immutableArray, MetadataModule module, HashSet<string> namespaces)
		{
			foreach (var methodImpl in immutableArray) {
				CollectNamespacesForTypeReference(module.ResolveType(methodImpl.MethodDeclaration.DeclaringType, genericContext), namespaces);
				CollectNamespacesForMemberReference(module.ResolveMethod(methodImpl.MethodBody, genericContext), namespaces);
				CollectNamespacesForMemberReference(module.ResolveMethod(methodImpl.MethodDeclaration, genericContext), namespaces);
			}
		}

		static void CollectNamespacesForTypeReference(IType type, HashSet<string> namespaces)
		{
			switch (type) {
				case ParameterizedType parameterizedType:
					namespaces.Add(parameterizedType.Namespace);
					CollectNamespacesForTypeReference(parameterizedType.GenericType, namespaces);
					foreach (var arg in parameterizedType.TypeArguments)
						CollectNamespacesForTypeReference(arg, namespaces);
					break;
				case TypeWithElementType typeWithElementType:
					CollectNamespacesForTypeReference(typeWithElementType.ElementType, namespaces);
					break;
				case TupleType tupleType:
					foreach (var elementType in tupleType.ElementTypes) {
						CollectNamespacesForTypeReference(elementType, namespaces);
					}
					break;
				default:
					namespaces.Add(type.Namespace);
					break;
			}
			foreach (var baseType in type.GetAllBaseTypes()) {
				namespaces.Add(baseType.Namespace);
			}
		}

		public static void CollectNamespaces(IMDTokenProvider entity, MetadataModule module, HashSet<string> namespaces)
		{
			if (entity is null) return;
			CollectNamespaces(module.ResolveEntity(entity, genericContext), module, namespaces);
		}

		public static void HandleAttributes(IEnumerable<IAttribute> attributes, HashSet<string> namespaces)
		{
			foreach (var attr in attributes) {
				namespaces.Add(attr.AttributeType.Namespace);
				foreach (var arg in attr.FixedArguments) {
					HandleAttributeValue(arg.Type, arg.Value, namespaces);
				}
				foreach (var arg in attr.NamedArguments) {
					HandleAttributeValue(arg.Type, arg.Value, namespaces);
				}
			}
		}

		static void HandleAttributeValue(IType type, object value, HashSet<string> namespaces)
		{
			CollectNamespacesForTypeReference(type, namespaces);
			if (value is IType typeofType)
				CollectNamespacesForTypeReference(typeofType, namespaces);
			if (value is ImmutableArray<CustomAttributeTypedArgument<IType>> arr) {
				foreach (var element in arr) {
					HandleAttributeValue(element.Type, element.Value, namespaces);
				}
			}
		}

		static void HandleTypeParameters(IEnumerable<ITypeParameter> typeParameters, HashSet<string> namespaces)
		{
			foreach (var typeParam in typeParameters) {
				HandleAttributes(typeParam.GetAttributes(), namespaces);

				foreach (var constraint in typeParam.DirectBaseTypes) {
					CollectNamespacesForTypeReference(constraint, namespaces);
				}
			}
		}

		static void CollectNamespacesFromMethodBody(CilBody method, MetadataModule module, HashSet<string> namespaces)
		{
			foreach (Local local in method.Variables) {
				var decoded = local.Type.DecodeSignature(module, genericContext);
				CollectNamespacesForTypeReference(decoded, namespaces);
			}

			foreach (var region in method.ExceptionHandlers) {
				if (region.CatchType is null)
					continue;
				IType ty;
				try {
					ty = module.ResolveType(region.CatchType, genericContext);
				} catch (BadImageFormatException) {
					continue;
				}
				CollectNamespacesForTypeReference(ty, namespaces);
			}

			for (int i = 0; i < method.Instructions.Count; i++) {
				var instr = method.Instructions[i];
				switch (instr.OpCode.OperandType) {
					case OperandType.InlineType when instr.Operand != null: {
						IType type;
						try {
							type = module.ResolveType(instr.Operand as dnlib.DotNet.IType, genericContext);
						} catch (BadImageFormatException) {
							break;
						}
						CollectNamespacesForTypeReference(type, namespaces);
						break;
					}
					case OperandType.InlineField when instr.Operand != null:
					case OperandType.InlineMethod when instr.Operand != null: {
						IMember member;
						try {
							member = module.ResolveEntity(instr.Operand as IMDTokenProvider, genericContext) as IMember;
						} catch (BadImageFormatException) {
							break;
						}
						CollectNamespacesForMemberReference(member, namespaces);
						break;
					}
					case OperandType.InlineSig when instr.Operand != null:
						if (instr.Operand is MethodBaseSig baseSig) {
							try {
								CollectNamespacesForTypeReference(baseSig.RetType.DecodeSignature(module, genericContext),
									namespaces);
								foreach (var paramType in baseSig.Params) {
									CollectNamespacesForTypeReference(paramType.DecodeSignature(module, genericContext),
										namespaces);
								}
							} catch (BadImageFormatException) {

							}
						}
						break;
					case OperandType.InlineTok when instr.Operand != null: {
						if (instr.Operand is dnlib.DotNet.IType dnType) {
							IType type;
							try {
								type = module.ResolveType(dnType, genericContext);
							} catch (BadImageFormatException) {
								break;
							}
							CollectNamespacesForTypeReference(type, namespaces);
						}
						else if (instr.Operand is IMemberRef dnMemberRef) {
							IMember member;
							try {
								member = module.ResolveEntity(dnMemberRef, genericContext) as IMember;
							} catch (BadImageFormatException) {
								break;
							}
							CollectNamespacesForMemberReference(member, namespaces);
						}
						break;
					}
				}
			}
		}

		static void CollectNamespacesForMemberReference(IMember member, HashSet<string> namespaces)
		{
			switch (member) {
				case IField field:
					CollectNamespacesForTypeReference(field.DeclaringType, namespaces);
					CollectNamespacesForTypeReference(field.ReturnType, namespaces);
					break;
				case IMethod method:
					CollectNamespacesForTypeReference(method.DeclaringType, namespaces);
					CollectNamespacesForTypeReference(method.ReturnType, namespaces);
					foreach (var param in method.Parameters)
						CollectNamespacesForTypeReference(param.Type, namespaces);
					foreach (var arg in method.TypeArguments)
						CollectNamespacesForTypeReference(arg, namespaces);
					break;
			}
		}
	}
}
