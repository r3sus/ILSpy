using System;
using System.Text.RegularExpressions;
using dnlib.DotNet;

namespace ICSharpCode.Decompiler.IL.Transforms
{
	class LocalFunctionDecompiler : IILTransform
	{
		public void Run(ILFunction function, ILTransformContext context)
		{
			throw new NotImplementedException();
		}

		public static bool IsLocalFunctionMethod(MethodDef method)
		{
			if ((method.Attributes & MethodAttributes.Assembly) == 0 || !method.IsCompilerGenerated())
				return false;

			if (!ParseLocalFunctionName(method.Name, out _, out _))
				return false;

			return true;
		}

		public static bool IsLocalFunctionDisplayClass(TypeDef type)
		{
			if ((type.Attributes & TypeAttributes.NestedPrivate) == 0)
				return false;
			if (!type.HasGeneratedName())
				return false;

			var declaringType = type.DeclaringType;

			foreach (var method in declaringType.Methods) {
				if (!IsLocalFunctionMethod(method))
					continue;

				bool canBe = false;
				foreach (TypeSig sig in method.MethodSig.Params) {
					canBe |= VisitSig(sig, type);
				}
				if (canBe)
					return true;
			}

			return false;
		}

		private static bool VisitSig(TypeSig sig, TypeDef type)
		{
			switch (sig) {
				case SZArraySig _:
				case CorLibTypeSig _:
				case GenericVar _:
				case FnPtrSig _:
					return false;
				case PtrSig ptrSig:
					return VisitSig(ptrSig.Next, type);
				case PinnedSig pinnedSig:
					return VisitSig(pinnedSig.Next, type);
				case ModifierSig modifierSig:
					return VisitSig(modifierSig.Next, type);
				case GenericInstSig genericInstSig:
					return VisitSig(genericInstSig.GenericType, type);
				case ByRefSig byRefSig:
					return VisitSig(byRefSig.Next, type);
				case ArraySig arraySig:
					return VisitSig(arraySig.Next, type);
				case ClassOrValueTypeSig classOrValueTypeSig:
					if (classOrValueTypeSig.IsTypeSpec)
						return VisitSig(classOrValueTypeSig.TypeSpec.TypeSig, type);
					else if (classOrValueTypeSig.IsTypeRef)
						return false;
					else
						return classOrValueTypeSig.TypeDef == type;
				default:
					throw new ArgumentOutOfRangeException();
			}
		}

		/// <summary>
		/// Newer Roslyn versions use the format "&ltcallerName&gtg__functionName|x_y"
		/// Older versions use "&ltcallerName&gtg__functionNamex_y"
		/// </summary>
		static readonly Regex functionNameRegex = new Regex(@"^<(.*)>g__(.*)\|{0,1}\d+_\d+$", RegexOptions.Compiled);

		static bool ParseLocalFunctionName(string name, out string callerName, out string functionName)
		{
			callerName = null;
			functionName = null;
			if (string.IsNullOrWhiteSpace(name))
				return false;
			var match = functionNameRegex.Match(name);
			callerName = match.Groups[1].Value;
			functionName = match.Groups[2].Value;
			return match.Success;
		}
	}
}
