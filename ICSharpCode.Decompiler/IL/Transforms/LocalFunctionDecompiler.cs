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
		/// or can be used as part of the "delegate construction" pattern, i.e., <c>newobj Delegate(&lt;target-expression&gt;, ldftn &lt;method&gt;)</c>.
		/// </para>
		/// As local functions can be declared practically anywhere, we have to take a look at all use-sites and infer the declaration location from that. Use-sites can be call, callvirt and ldftn instructions.
		/// After all use-sites are collected we construct the ILAst of the local function and add it to the parent function.
		/// Then all use-sites of the local-function are transformed to a call to the <c>LocalFunctionMethod</c> or a ldftn of the <c>LocalFunctionMethod</c>.
		/// In a next step we handle all nested local functions.
		/// After all local functions are transformed, we move all local functions that capture any variables to their respective declaration scope.
		/// </summary>
		public void Run(ILFunction function, ILTransformContext context)
		{
			if (!context.Settings.LocalFunctions)
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

				var firstUseSite = info.UseSites[0];
				context.StepStartGroup($"Transform " + info.Definition.Name, info.Definition);
				try {
					var localFunction = info.Definition;
					if (!localFunction.Method.IsStatic) {
						var target = firstUseSite.Arguments[0];
						context.Step($"Replace 'this' with {target}", localFunction);
						var thisVar = localFunction.Variables.SingleOrDefault(VariableKindExtensions.IsThis);
						localFunction.AcceptVisitor(new DelegateConstruction.ReplaceDelegateTargetVisitor(target, thisVar));
					}

					foreach (var useSite in info.UseSites) {
						context.Step($"Transform use site at IL_{useSite.StartILOffset:x4}", useSite);
						if (useSite.OpCode == OpCode.NewObj) {
							TransformToLocalFunctionReference(localFunction, useSite);
						} else {
							DetermineCaptureAndDeclarationScope(localFunction, useSite);
							TransformToLocalFunctionInvocation(localFunction.ReducedMethod, useSite);
						}

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
				} finally {
					context.StepEndGroup();
				}
			}
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
						Method = targetMethod,
						Definition = ReadLocalFunctionDefinition(context.Function, targetMethod)
					};
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

		ILFunction ReadLocalFunctionDefinition(ILFunction rootFunction, TypeSystem.IMethod targetMethod)
		{
			var methodDefinition = (MethodDef)targetMethod.MetadataToken;
			if (!methodDefinition.HasBody)
				return null;
			var genericContext = DelegateConstruction.GenericContextFromTypeArguments(targetMethod.Substitution);
			if (genericContext == null)
				return null;
			var ilReader = context.CreateILReader();
			var function = ilReader.ReadIL(methodDefinition, genericContext.Value, ILFunctionKind.LocalFunction, context.CancellationToken);
			// Embed the local function into the parent function's ILAst, so that "Show steps" can show
			// how the local function body is being transformed.
			rootFunction.LocalFunctions.Add(function);
			function.DeclarationScope = (BlockContainer)rootFunction.Body;
			function.CheckInvariant(ILPhase.Normal);
			var nestedContext = new ILTransformContext(context, function);
			function.RunTransforms(CSharpDecompiler.GetILTransforms().TakeWhile(t => !(t is LocalFunctionDecompiler)), nestedContext);
			function.DeclarationScope = null;
			function.ReducedMethod = ReduceToLocalFunction(targetMethod);
			return function;
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

		LocalFunctionMethod ReduceToLocalFunction(TypeSystem.IMethod method)
		{
			int parametersToRemove = 0;
			for (int i = method.Parameters.Count - 1; i >= 0; i--) {
				if (!IsClosureParameter(method.Parameters[i], resolveContext))
					break;
				parametersToRemove++;
			}
			return new LocalFunctionMethod(method, parametersToRemove);
		}

		static void TransformToLocalFunctionReference(ILFunction function, CallInstruction useSite)
		{
			useSite.Arguments[0].ReplaceWith(new LdNull().WithILRange(useSite.Arguments[0]));
			var fnptr = (IInstructionWithMethodOperand)useSite.Arguments[1];
			var replacement = new LdFtn(function.ReducedMethod).WithILRange((ILInstruction)fnptr);
			useSite.Arguments[1].ReplaceWith(replacement);
		}

		void TransformToLocalFunctionInvocation(LocalFunctionMethod reducedMethod, CallInstruction useSite)
		{
			bool wasInstanceCall = !useSite.Method.IsStatic;
			var replacement = new Call(reducedMethod);
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
				if (!HandleArgument(i, useSite.Arguments[i]))
					break;
			}
			if (firstArgumentIndex > 0) {
				HandleArgument(0, useSite.Arguments[0]);
			}

			bool HandleArgument(int i, ILInstruction arg)
			{
				ILVariable closureVar;
				if (!(arg.MatchLdLoc(out closureVar) || arg.MatchLdLoca(out closureVar)))
					return false;
				if (closureVar.Kind == VariableKind.NamedArgument)
					return false;
				ITypeDefinition potentialDisplayClass = UnwrapByRef(closureVar.Type).GetDefinition();
				if (!TransformDisplayClassUsage.IsPotentialClosure(context, potentialDisplayClass))
					return false;
				if (i - firstArgumentIndex >= 0) {
					Debug.Assert(i - firstArgumentIndex < function.Method.Parameters.Count && IsClosureParameter(function.Method.Parameters[i - firstArgumentIndex], resolveContext));
				}
				if (closureVar.AddressCount == 0 && closureVar.StoreInstructions.Count == 0)
					return true;
				// determine the capture scope of closureVar and the declaration scope of the function
				var instructions = closureVar.StoreInstructions.OfType<ILInstruction>()
					.Concat(closureVar.AddressInstructions).OrderBy(inst => inst.StartILOffset).ToList();
				var additionalScope = BlockContainer.FindClosestContainer(instructions.First());
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
