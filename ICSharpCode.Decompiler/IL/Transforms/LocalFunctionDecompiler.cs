// Copyright (c) 2019 Siegfried Pammer
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
using System.Text;
using System.Text.RegularExpressions;
using dnlib.DotNet;
using ICSharpCode.Decompiler.CSharp;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.TypeSystem.Implementation;
using ICSharpCode.Decompiler.Util;

namespace ICSharpCode.Decompiler.IL.Transforms
{
	/// <summary>
	/// Decompiler step for C# 7.0 local functions
	/// </summary>
	class LocalFunctionDecompiler : IILTransform
	{
		ILTransformContext context;
		ITypeResolveContext resolveContext;

		struct LocalFunctionInfo
		{
			public List<CallInstruction> UseSites;
			public TypeSystem.IMethod Method;
			public ILFunction Definition;
		}

		/// <summary>
		/// The transform works like this:
		///
		/// <para>
		/// local functions can either be used in method calls, i.e., call and callvirt instructions,
		/// or can be used as part of the "delegate construction" pattern, i.e.,
		/// <c>newobj Delegate(&lt;target-expression&gt;, ldftn &lt;method&gt;)</c>.
		/// </para>
		/// As local functions can be declared practically anywhere, we have to take a look at
		/// all use-sites and infer the declaration location from that. Use-sites can be call,
		/// callvirt and ldftn instructions.
		/// After all use-sites are collected we construct the ILAst of the local function
		/// and add it to the parent function.
		/// Then all use-sites of the local-function are transformed to a call to the
		/// <c>LocalFunctionMethod</c> or a ldftn of the <c>LocalFunctionMethod</c>.
		/// In a next step we handle all nested local functions.
		/// After all local functions are transformed, we move all local functions that capture
		/// any variables to their respective declaration scope.
		/// </summary>
		public void Run(ILFunction function, ILTransformContext context)
		{
			if (!context.Settings.LocalFunctions)
				return;
			// Disable the transform if we are decompiling a display-class or local function method:
			// This happens if a local function or display class is selected in the ILSpy tree view.
			if (IsLocalFunctionMethod(function.Method, context) || IsLocalFunctionDisplayClass(function.Method.ParentModule.PEFile, (TypeDef)function.Method.DeclaringTypeDefinition.MetadataToken, context))
				return;
			this.context = context;
			this.resolveContext = new SimpleTypeResolveContext(function.Method);
			var localFunctions = new Dictionary<MethodDef, LocalFunctionInfo>();
			var cancellationToken = context.CancellationToken;
			// Find all local functions declared inside this method, including nested local functions or local functions declared in lambdas.
			FindUseSites(function, context, localFunctions);
			foreach (var (_, info) in localFunctions) {
				cancellationToken.ThrowIfCancellationRequested();
				if (info.Definition == null) {
					function.Warnings.Add($"Could not decode local function '{info.Method}'");
					continue;
				}

				context.StepStartGroup($"Transform " + info.Definition.Name, info.Definition);
				try {
					var localFunction = info.Definition;
					if (!localFunction.Method.IsStatic) {
						var thisVar = localFunction.Variables.SingleOrDefault(VariableKindExtensions.IsThis);
						var target = info.UseSites.Where(us => us.Arguments[0].MatchLdLoc(out _)).FirstOrDefault()?.Arguments[0];
						if (target == null) {
							target = info.UseSites[0].Arguments[0];
							if (target.MatchLdFld(out var target1, out var field) && thisVar.Type.Equals(field.Type) && field.Type.Kind == TypeKind.Class && TransformDisplayClassUsage.IsPotentialClosure(context, field.Type.GetDefinition())) {
								var variable = function.Descendants.OfType<ILFunction>().SelectMany(f => f.Variables).Where(v => !v.IsThis() && TransformDisplayClassUsage.IsClosure(context, v, out var varType, out _) && varType.Equals(field.Type)).OnlyOrDefault();
								if (variable != null) {
									target = new LdLoc(variable);
									HandleArgument(localFunction, 1, 0, target);
								}
							}
						}
						context.Step($"Replace 'this' with {target}", localFunction);
						localFunction.AcceptVisitor(new DelegateConstruction.ReplaceDelegateTargetVisitor(target, thisVar));
					}

					foreach (var useSite in info.UseSites) {
						DetermineCaptureAndDeclarationScope(localFunction, useSite);

						if (function.Method.IsConstructor && localFunction.DeclarationScope == null) {
							localFunction.DeclarationScope = BlockContainer.FindClosestContainer(useSite);
						}
					}

					if (localFunction.DeclarationScope == null) {
						localFunction.DeclarationScope = (BlockContainer)function.Body;
					} else if (localFunction.DeclarationScope != function.Body && localFunction.DeclarationScope.Parent is ILFunction declaringFunction) {
						function.LocalFunctions.Remove(localFunction);
						declaringFunction.LocalFunctions.Add(localFunction);
					}

					if (TryValidateSkipCount(info, out int skipCount) && skipCount != localFunction.ReducedMethod.NumberOfCompilerGeneratedTypeParameters) {
						Debug.Assert(false);
						function.Warnings.Add($"Could not decode local function '{info.Method}'");
						if (localFunction.DeclarationScope != function.Body && localFunction.DeclarationScope.Parent is ILFunction declaringFunction) {
							declaringFunction.LocalFunctions.Remove(localFunction);
						}
						continue;
					}

					foreach (var useSite in info.UseSites) {
						context.Step($"Transform use site at IL_{useSite.StartILOffset:x4}", useSite);
						if (useSite.OpCode == OpCode.NewObj) {
							TransformToLocalFunctionReference(localFunction, useSite);
						} else {
							TransformToLocalFunctionInvocation(localFunction.ReducedMethod, useSite);
						}
					}
				} finally {
					context.StepEndGroup();
				}
			}
		}

		bool TryValidateSkipCount(LocalFunctionInfo info, out int skipCount)
		{
			skipCount = 0;
			var localFunction = info.Definition;
			if (localFunction.Method.TypeParameters.Count == 0)
				return true;
			var parentMethod = ((ILFunction)localFunction.Parent).Method;
			var method = localFunction.Method;
			skipCount = parentMethod.DeclaringType.TypeParameterCount - method.DeclaringType.TypeParameterCount;

			if (skipCount > 0)
				return false;
			skipCount += parentMethod.TypeParameters.Count;
			if (skipCount < 0 || skipCount > method.TypeArguments.Count)
				return false;

			if (skipCount > 0) {
				#if DEBUG
				foreach (var useSite in info.UseSites) {
					var callerMethod = useSite.Ancestors.OfType<ILFunction>().First().Method;
					callerMethod = callerMethod.ReducedFrom ?? callerMethod;
					IMethod method0;
					if (useSite.OpCode == OpCode.NewObj) {
						method0 = ((LdFtn)useSite.Arguments[1]).Method;
					} else {
						method0 = useSite.Method;
					}
					var totalSkipCount = skipCount + method0.DeclaringType.TypeParameterCount;
					var methodSkippedArgs = method0.DeclaringType.TypeArguments.Concat(method0.TypeArguments).Take(totalSkipCount);
					Debug.Assert(methodSkippedArgs.SequenceEqual(callerMethod.DeclaringType.TypeArguments.Concat(callerMethod.TypeArguments).Take(totalSkipCount)));
					Debug.Assert(methodSkippedArgs.All(p => p.Kind == TypeKind.TypeParameter));
					Debug.Assert(methodSkippedArgs.Select(p => p.Name).SequenceEqual(method0.DeclaringType.TypeParameters.Concat(method0.TypeParameters).Take(totalSkipCount).Select(p => p.Name)));
				}
				#endif
			}
			return true;
		}

		void FindUseSites(ILFunction function, ILTransformContext context, Dictionary<MethodDef, LocalFunctionInfo> localFunctions)
		{
			foreach (var inst in function.Body.Descendants) {
				context.CancellationToken.ThrowIfCancellationRequested();
				if (inst is CallInstruction call && !call.Method.IsLocalFunction && IsLocalFunctionMethod(call.Method, context)) {
					HandleUseSite(call.Method, call);
				} else if (inst is LdFtn ldftn && !ldftn.Method.IsLocalFunction && ldftn.Parent is NewObj newObj && IsLocalFunctionMethod(ldftn.Method, context) && DelegateConstruction.IsDelegateConstruction(newObj)) {
					HandleUseSite(ldftn.Method, newObj);
				}
			}

			void HandleUseSite(TypeSystem.IMethod targetMethod, CallInstruction inst)
			{
				if (!localFunctions.TryGetValue((MethodDef)targetMethod.MetadataToken, out var info)) {
					context.StepStartGroup($"Read local function '{targetMethod.Name}'", inst);
					info = new LocalFunctionInfo() {
						UseSites = new List<CallInstruction>() { inst },
						Method = (TypeSystem.IMethod)targetMethod.MemberDefinition,
					};
					var rootFunction = context.Function;
					int skipCount = GetSkipCount(rootFunction, targetMethod);
					info.Definition = ReadLocalFunctionDefinition(rootFunction, targetMethod, skipCount);
					localFunctions.Add((MethodDef)targetMethod.MetadataToken, info);
					if (info.Definition != null) {
						FindUseSites(info.Definition, context, localFunctions);
					}
					context.StepEndGroup();
				} else {
					info.UseSites.Add(inst);
				}
			}
		}

		ILFunction ReadLocalFunctionDefinition(ILFunction rootFunction, TypeSystem.IMethod targetMethod, int skipCount)
		{
			var methodDefinition = (MethodDef)targetMethod.MetadataToken;
			if (!methodDefinition.HasBody)
				return null;
			var ilReader = context.CreateILReader();
			var genericContext = GenericContextFromTypeArguments(targetMethod, skipCount);
			if (genericContext == null)
				return null;
			var function = ilReader.ReadIL(methodDefinition, genericContext.GetValueOrDefault(), ILFunctionKind.LocalFunction, context.CancellationToken);
			// Embed the local function into the parent function's ILAst, so that "Show steps" can show
			// how the local function body is being transformed.
			rootFunction.LocalFunctions.Add(function);
			function.DeclarationScope = (BlockContainer)rootFunction.Body;
			function.CheckInvariant(ILPhase.Normal);
			var nestedContext = new ILTransformContext(context, function);
			function.RunTransforms(CSharpDecompiler.GetILTransforms().TakeWhile(t => !(t is LocalFunctionDecompiler)), nestedContext);
			function.DeclarationScope = null;
			function.ReducedMethod = ReduceToLocalFunction(function.Method, skipCount);
			return function;
		}

		int GetSkipCount(ILFunction rootFunction, TypeSystem.IMethod targetMethod)
		{
			targetMethod = (TypeSystem.IMethod)targetMethod.MemberDefinition;
			var skipCount = rootFunction.Method.DeclaringType.TypeParameters.Count + rootFunction.Method.TypeParameters.Count - targetMethod.DeclaringType.TypeParameters.Count;
			if (skipCount < 0) {
				skipCount = 0;
			}
			if (targetMethod.TypeParameters.Count > 0) {
				var lastParams = targetMethod.Parameters.Where(p => IsClosureParameter(p, this.resolveContext)).SelectMany(p => UnwrapByRef(p.Type).TypeArguments)
					.Select(pt => (int?)targetMethod.TypeParameters.IndexOf(pt)).DefaultIfEmpty().Max();
				if (lastParams != null && lastParams.GetValueOrDefault() + 1 > skipCount)
					skipCount = lastParams.GetValueOrDefault() + 1;
			}
			return skipCount;
		}

		static TypeSystem.GenericContext? GenericContextFromTypeArguments(TypeSystem.IMethod targetMethod, int skipCount)
		{
			if (skipCount < 0 || skipCount > targetMethod.TypeParameters.Count) {
				Debug.Assert(false);
				return null;
			}
			int total = targetMethod.DeclaringType.TypeParameters.Count + skipCount;
			if (total == 0)
				return default(TypeSystem.GenericContext);

			var classTypeParameters = new List<ITypeParameter>(targetMethod.DeclaringType.TypeParameters);
			var methodTypeParameters = new List<ITypeParameter>(targetMethod.TypeParameters);
			var skippedTypeArguments = targetMethod.DeclaringType.TypeArguments.Concat(targetMethod.TypeArguments).Take(total);
			int idx = 0;
			foreach (var skippedTA in skippedTypeArguments) {
				int curIdx;
				List<ITypeParameter> curParameters;
				IReadOnlyList<TypeSystem.IType> curArgs;
				if (idx < classTypeParameters.Count) {
					curIdx = idx;
					curParameters = classTypeParameters;
					curArgs = targetMethod.DeclaringType.TypeArguments;
				} else {
					curIdx = idx - classTypeParameters.Count;
					curParameters = methodTypeParameters;
					curArgs = targetMethod.TypeArguments;
				}
				if (curArgs[curIdx].Kind != TypeKind.TypeParameter)
					break;
				curParameters[curIdx] = (ITypeParameter)skippedTA;
				idx++;
			}
			if (idx != total) {
				Debug.Assert(false);
				return null;
			}

			return new TypeSystem.GenericContext(classTypeParameters, methodTypeParameters);
		}

		static T FindCommonAncestorInstruction<T>(ILInstruction a, ILInstruction b)
			where T : ILInstruction
		{
			var ancestorsOfB = new HashSet<T>(b.Ancestors.OfType<T>());
			return a.Ancestors.OfType<T>().FirstOrDefault(ancestorsOfB.Contains);
		}

		internal static bool IsClosureParameter(IParameter parameter, ITypeResolveContext context)
		{
			if (!parameter.IsRef)
				return false;
			var type = ((ByReferenceType)parameter.Type).ElementType.GetDefinition();
			return type != null
				&& type.Kind == TypeKind.Struct
				&& TransformDisplayClassUsage.IsPotentialClosure(context.CurrentTypeDefinition, type);
		}

		static TypeSystem.IType UnwrapByRef(TypeSystem.IType type)
		{
			if (type is ByReferenceType byRef) {
				type = byRef.ElementType;
			}
			return type;
		}

		internal static ILInstruction GetStatement(ILInstruction inst)
		{
			while (inst.Parent != null) {
				if (inst.Parent is Block b && b.Kind == BlockKind.ControlFlow)
					return inst;
				inst = inst.Parent;
			}
			return inst;
		}

		LocalFunctionMethod ReduceToLocalFunction(TypeSystem.IMethod method, int skipCount)
		{
			int parametersToRemove = 0;
			for (int i = method.Parameters.Count - 1; i >= 0; i--) {
				if (!IsClosureParameter(method.Parameters[i], resolveContext))
					break;
				parametersToRemove++;
			}
			return new LocalFunctionMethod(method, parametersToRemove, skipCount);
		}

		static void TransformToLocalFunctionReference(ILFunction function, CallInstruction useSite)
		{
			useSite.Arguments[0].ReplaceWith(new LdNull().WithILRange(useSite.Arguments[0]));
			var fnptr = (IInstructionWithMethodOperand)useSite.Arguments[1];
			var specializeMethod = function.ReducedMethod.Specialize(fnptr.Method.Substitution);
			var replacement = new LdFtn(specializeMethod).WithILRange((ILInstruction)fnptr);
			useSite.Arguments[1].ReplaceWith(replacement);
		}

		void TransformToLocalFunctionInvocation(LocalFunctionMethod reducedMethod, CallInstruction useSite)
		{
			var specializeMethod = reducedMethod.Specialize(useSite.Method.Substitution);
			bool wasInstanceCall = !useSite.Method.IsStatic;
			var replacement = new Call(specializeMethod);
			int firstArgumentIndex = wasInstanceCall ? 1 : 0;
			int argumentCount = useSite.Arguments.Count;
			int reducedArgumentCount = argumentCount - (reducedMethod.NumberOfCompilerGeneratedParameters + firstArgumentIndex);
			replacement.Arguments.AddRange(useSite.Arguments.Skip(firstArgumentIndex).Take(reducedArgumentCount));
			// copy flags
			replacement.ConstrainedTo = useSite.ConstrainedTo;
			replacement.ILStackWasEmpty = useSite.ILStackWasEmpty;
			replacement.IsTail = useSite.IsTail;
			// copy IL ranges
			replacement.AddILRange(useSite);
			if (wasInstanceCall) {
				replacement.AddILRange(useSite.Arguments[0]);
				if (useSite.Arguments[0].MatchLdLocRef(out var variable) && variable.Kind == VariableKind.NamedArgument) {
					// remove the store instruction of the simple load, if it is a named argument.
					var storeInst = (ILInstruction)variable.StoreInstructions[0];
					((Block)storeInst.Parent).Instructions.RemoveAt(storeInst.ChildIndex);
				}
			}
			for (int i = 0; i < reducedMethod.NumberOfCompilerGeneratedParameters; i++) {
				replacement.AddILRange(useSite.Arguments[argumentCount - i - 1]);
			}
			useSite.ReplaceWith(replacement);
		}

		void DetermineCaptureAndDeclarationScope(ILFunction function, CallInstruction useSite)
		{
			int firstArgumentIndex = function.Method.IsStatic ? 0 : 1;
			for (int i = useSite.Arguments.Count - 1; i >= firstArgumentIndex; i--) {
				if (!HandleArgument(function, firstArgumentIndex, i, useSite.Arguments[i]))
					break;
			}
			if (firstArgumentIndex > 0) {
				HandleArgument(function, firstArgumentIndex, 0, useSite.Arguments[0]);
			}
		}

		bool HandleArgument(ILFunction function, int firstArgumentIndex, int i, ILInstruction arg)
		{
			ILVariable closureVar;
			if (!(arg.MatchLdLoc(out closureVar) || arg.MatchLdLoca(out closureVar)))
				return false;
			if (closureVar.Kind == VariableKind.NamedArgument)
				return false;
			if (!TransformDisplayClassUsage.IsClosure(context, closureVar, out _, out var initializer))
				return false;
			if (i - firstArgumentIndex >= 0) {
				Debug.Assert(i - firstArgumentIndex < function.Method.Parameters.Count && IsClosureParameter(function.Method.Parameters[i - firstArgumentIndex], resolveContext));
			}
			// determine the capture scope of closureVar and the declaration scope of the function
			var additionalScope = BlockContainer.FindClosestContainer(initializer);
			if (closureVar.CaptureScope == null)
				closureVar.CaptureScope = additionalScope;
			else
				closureVar.CaptureScope = FindCommonAncestorInstruction<BlockContainer>(closureVar.CaptureScope, additionalScope);
			if (function.DeclarationScope == null)
				function.DeclarationScope = closureVar.CaptureScope;
			else if (!IsInNestedLocalFunction(function.DeclarationScope, closureVar.CaptureScope.Ancestors.OfType<ILFunction>().First()))
				function.DeclarationScope = FindCommonAncestorInstruction<BlockContainer>(function.DeclarationScope, closureVar.CaptureScope);
			return true;
		}

		bool IsInNestedLocalFunction(BlockContainer declarationScope, ILFunction function)
		{
			return TreeTraversal.PreOrder(function, f => f.LocalFunctions).Any(f => declarationScope.IsDescendantOf(f.Body));
		}

		internal static bool IsLocalFunctionReference(NewObj inst, ILTransformContext context)
		{
			if (inst == null || inst.Arguments.Count != 2 || inst.Method.DeclaringType.Kind != TypeKind.Delegate)
				return false;
			var opCode = inst.Arguments[1].OpCode;

			return opCode == OpCode.LdFtn
				&& IsLocalFunctionMethod(((IInstructionWithMethodOperand)inst.Arguments[1]).Method, context);
		}

		public static bool IsLocalFunctionMethod(TypeSystem.IMethod method, ILTransformContext context)
		{
			if (method.MetadataToken is null)
				return false;
			return IsLocalFunctionMethod(method.ParentModule.PEFile, (MethodDef)method.MetadataToken, context);
		}

		public static bool IsLocalFunctionMethod(PEFile module, MethodDef method, ILTransformContext context = null)
		{
			if (context != null && context.PEFile != module)
				return false;

			var declaringType = method.DeclaringType;
			if ((method.Attributes & MethodAttributes.Assembly) == 0 || !(method.IsCompilerGenerated() || declaringType.IsCompilerGenerated()))
				return false;

			if (!ParseLocalFunctionName(method.Name, out _, out _))
				return false;

			return true;
		}

		public static bool LocalFunctionNeedsAccessibilityChange(PEFile module, MethodDef method)
		{
			if (!IsLocalFunctionMethod(module, method))
				return false;

			List<TypeDef> refStructTypes = new List<TypeDef>();
			VisitFindRefStruct(method.ReturnType, refStructTypes);
			foreach (TypeSig sig in method.MethodSig.Params) {
				VisitFindRefStruct(sig, refStructTypes);
			}

			foreach (var td in refStructTypes) {
				if (td.IsCompilerGenerated() && td.IsValueType)
					return true;
			}

			return false;
		}

		public static bool IsLocalFunctionDisplayClass(PEFile module, TypeDef type, ILTransformContext context = null)
		{
			if (context != null && context.PEFile != module)
				return false;

			if ((type.Attributes & TypeAttributes.VisibilityMask) != TypeAttributes.NestedPrivate)
				return false;
			if (!type.HasGeneratedName())
				return false;

			var declaringType = type.DeclaringType;

			foreach (var method in declaringType.Methods) {
				if (!IsLocalFunctionMethod(module, method, context))
					continue;

				bool canBe = false;
				foreach (TypeSig sig in method.MethodSig.Params) {
					canBe |= VisitFindType(sig, type);
				}
				if (canBe)
					return true;
			}

			return false;
		}

		private static bool VisitFindType(TypeSig sig, TypeDef type)
		{
			switch (sig) {
				case SZArraySig _:
				case CorLibTypeSig _:
				case GenericVar _:
				case FnPtrSig _:
				case GenericSig _:
					return false;
				case PtrSig ptrSig:
					return VisitFindType(ptrSig.Next, type);
				case PinnedSig pinnedSig:
					return VisitFindType(pinnedSig.Next, type);
				case ModifierSig modifierSig:
					return VisitFindType(modifierSig.Next, type);
				case GenericInstSig genericInstSig:
					return VisitFindType(genericInstSig.GenericType, type);
				case ByRefSig byRefSig:
					return VisitFindType(byRefSig.Next, type);
				case ArraySig arraySig:
					return VisitFindType(arraySig.Next, type);
				case ClassOrValueTypeSig classOrValueTypeSig:
					if (classOrValueTypeSig.IsTypeSpec)
						return VisitFindType(classOrValueTypeSig.TypeSpec.TypeSig, type);
					else if (classOrValueTypeSig.IsTypeRef)
						return false;
					else
						return classOrValueTypeSig.TypeDef == type;
				default:
					throw new ArgumentOutOfRangeException();
			}
		}

		private static void VisitFindRefStruct(TypeSig sig, List<TypeDef> refTypes)
		{
			switch (sig) {
				case SZArraySig _:
				case CorLibTypeSig _:
				case GenericVar _:
				case FnPtrSig _:
				case PtrSig _:
				case GenericSig _:
				case PinnedSig _:
				case ModifierSig _:
				case GenericInstSig _:
				case ArraySig _:
					return;
				case ByRefSig byRefSig:
					VisitFindRefStruct(byRefSig.Next, refTypes);
					return;
				case ClassOrValueTypeSig classOrValueTypeSig when classOrValueTypeSig.IsTypeDef:
					refTypes.Add(classOrValueTypeSig.TypeDef);
					return;
				default:
					return;
			}
		}

		/// <summary>
		/// Newer Roslyn versions use the format "&lt;callerName&gt;g__functionName|x_y"
		/// Older versions use "&lt;callerName&gt;g__functionNamex_y"
		/// </summary>
		static readonly Regex functionNameRegex = new Regex(@"^<(.*)>g__([^\|]*)\|{0,1}\d+(_\d+)?$", RegexOptions.Compiled);

		internal static bool ParseLocalFunctionName(string name, out string callerName, out string functionName)
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
