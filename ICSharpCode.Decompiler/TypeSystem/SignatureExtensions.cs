using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using dnlib.DotNet;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.TypeSystem
{
	internal static class SignatureExtensions
	{
		internal static IType DecodeSignature(this TypeSpec spec, TypeProvider provider, GenericContext context)
		{
			return spec?.TypeSig.DecodeSignature(provider, context);
		}

		internal static IType DecodeSignature(this TypeSig sig, TypeProvider provider, GenericContext context)
		{
			if (sig is null)
				return null;

			switch (sig) {
				case CorLibTypeSig corLibTypeSig:
					return provider.Compilation.FindType(corLibTypeSig.ElementType.ToKnownTypeCode());
				case GenericMVar mVar:
					return context.GetMethodTypeParameter((int)mVar.Number);
				case GenericVar tVar:
					return context.GetClassTypeParameter((int)tVar.Number);
				case FnPtrSig _:
					return provider.Compilation.FindType(KnownTypeCode.IntPtr);
				case GenericInstSig instSig:
					return new ParameterizedType(instSig.GenericType.DecodeSignature(provider, context),
						instSig.GenericArguments.Select(x => x.DecodeSignature(provider, context)));
				case ByRefSig byRefSig:
					return new ByReferenceType(byRefSig.Next.DecodeSignature(provider, context));
				case PinnedSig pinnedSig:
					return new PinnedType(pinnedSig.Next.DecodeSignature(provider, context));
				case CModOptSig cModOptSig:
					return new ModifiedType(cModOptSig.Next.DecodeSignature(provider, context), cModOptSig.Modifier.DecodeSignature(provider, context), false);
				case CModReqdSig cModReqdSig:
					return new ModifiedType(cModReqdSig.Next.DecodeSignature(provider, context), cModReqdSig.Modifier.DecodeSignature(provider, context), true);
				case PtrSig ptrSig:
					return new PointerType(ptrSig.Next.DecodeSignature(provider, context));
				case ArraySigBase arraySigBase:
					return new ArrayType(provider.Compilation, arraySigBase.Next.DecodeSignature(provider, context), (int)arraySigBase.Rank);
				case ClassOrValueTypeSig classOrValueTypeSig:
					ThreeState isVT = ThreeState.Unknown;
					if (classOrValueTypeSig is ClassSig)
						isVT = ThreeState.No;
					else if (classOrValueTypeSig is ValueTypeSig)
						isVT = ThreeState.Yes;

					return classOrValueTypeSig.TypeDefOrRef.DecodeSignature(provider, context, isVT);
				default:
					throw new ArgumentOutOfRangeException();
			}
		}

		internal static IType DecodeSignature(this ITypeDefOrRef typeDefOrRef, TypeProvider provider, GenericContext context, ThreeState isVT = ThreeState.Unknown)
		{
			if (typeDefOrRef is null)
				return null;

			if (typeDefOrRef is TypeSpec spec) {
				return spec.DecodeSignature(provider, context);
			}
			CorLibTypeSig corLibTypeSig = typeDefOrRef.Module?.CorLibTypes.GetCorLibTypeSig(typeDefOrRef);
			if (corLibTypeSig != null) {
				return corLibTypeSig.DecodeSignature(provider, context);
			}

			if (typeDefOrRef is TypeDef def) {
				var resolved = provider.Module.GetDefinition(def);
				return resolved;
			} else {
				bool? isReferenceType;
				if (isVT != ThreeState.Unknown)
					isReferenceType = isVT == ThreeState.No;
				else
					isReferenceType = null;
				var gctr = new GetClassTypeReference(typeDefOrRef.GetFullTypeName(),
					new DefaultAssemblyReference(typeDefOrRef.Scope.ScopeName), isReferenceType);
				return gctr.Resolve(provider.Module != null
					? new SimpleTypeResolveContext(provider.Module)
					: new SimpleTypeResolveContext(provider.Compilation));
			}
		}

		internal static ImmutableArray<IType> DecodeSignature(this MethodSpec spec, TypeProvider provider, GenericContext context)
		{
			if (spec is null || spec.GenericInstMethodSig is null)
				return ImmutableArray<IType>.Empty;

			return spec.GenericInstMethodSig.GenericArguments.Select(sig => sig.DecodeSignature(provider, context))
					   .ToImmutableArray();
		}
	}
}
