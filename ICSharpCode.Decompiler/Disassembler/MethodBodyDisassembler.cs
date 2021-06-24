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
using System.Threading;
using dnlib.DotNet;
using dnlib.DotNet.Emit;
using dnlib.DotNet.Pdb;

namespace ICSharpCode.Decompiler.Disassembler
{
	/// <summary>
	/// Disassembles a method body.
	/// </summary>
	public class MethodBodyDisassembler
	{
		readonly ITextOutput output;
		readonly CancellationToken cancellationToken;

		/// <summary>
		/// Show .try/finally as blocks in IL code; indent loops.
		/// </summary>
		public bool DetectControlStructure { get; set; } = true;

		/// <summary>
		/// Show sequence points if debug information is loaded in Cecil.
		/// </summary>
		public bool ShowSequencePoints { get; set; }


		public MethodBodyDisassembler(ITextOutput output, CancellationToken cancellationToken)
		{
			this.output = output ?? throw new ArgumentNullException(nameof(output));
			this.cancellationToken = cancellationToken;
		}

		public virtual void Disassemble(MethodDef method, CilBody body)
		{
			// start writing IL code
			output.WriteLine("// Method begins at RVA 0x{0:x4}", (uint)method.RVA);
			output.WriteLine("// Code size {0} (0x{0:x})", body.GetCodeSize());
			output.WriteLine(".maxstack {0}", body.MaxStack);
			if (method.DeclaringType.Module.Assembly != null && method.DeclaringType.Module.EntryPoint == method)
				output.WriteLine(".entrypoint");

			DisassembleLocalsBlock(body);
			output.WriteLine();

			if (DetectControlStructure && body.Instructions.Count > 0) {
				int index = 0;
				HashSet<int> branchTargets = GetBranchTargets(body.Instructions);
				WriteStructureBody(body, new ILStructure(body), branchTargets, ref index, method.Body.GetCodeSize());
			} else {
				foreach (var inst in method.Body.Instructions) {
					WriteInstruction(output, inst);
					output.WriteLine();
				}
				WriteExceptionHandlers(body);
			}
		}

		private void DisassembleLocalsBlock(CilBody body)
		{
			if (body.HasVariables) {
				output.Write(".locals ");
				if (body.InitLocals)
					output.Write("init ");
				output.WriteLine("(");
				output.Indent();
				foreach (var v in body.Variables) {
					output.WriteDefinition("[" + v.Index + "] ", v);
					v.Type.WriteTo(output);
					if (v.Index + 1 < body.Variables.Count)
						output.Write(',');
					output.WriteLine();
				}
				output.Unindent();
				output.WriteLine(")");
			}
		}

		internal void WriteExceptionHandlers(CilBody body)
		{
			if (body.HasExceptionHandlers) {
				output.WriteLine();
				foreach (var eh in body.ExceptionHandlers) {
					eh.WriteTo(output);
					output.WriteLine();
				}
			}
		}

		HashSet<int> GetBranchTargets(IEnumerable<Instruction> instructions)
		{
			HashSet<int> branchTargets = new HashSet<int>();
			foreach (var inst in instructions) {
				Instruction target = inst.Operand as Instruction;
				if (target != null)
					branchTargets.Add((int)target.Offset);
				Instruction[] targets = inst.Operand as Instruction[];
				if (targets != null)
					foreach (Instruction t in targets)
						branchTargets.Add((int)t.Offset);
			}
			return branchTargets;
		}

		void WriteStructureHeader(ILStructure s)
		{
			switch (s.Type) {
				case ILStructureType.Loop:
					output.Write("// loop start");
					if (s.LoopEntryPoint != null) {
						output.Write(" (head: ");
						DisassemblerHelpers.WriteOffsetReference(output, s.LoopEntryPoint);
						output.Write(')');
					}
					output.WriteLine();
					break;
				case ILStructureType.Try:
					output.WriteLine(".try");
					output.WriteLine("{");
					break;
				case ILStructureType.Handler:
					switch (s.ExceptionHandler.HandlerType) {
						case ExceptionHandlerType.Catch:
						case ExceptionHandlerType.Filter:
							output.Write("catch");
							if (s.ExceptionHandler.CatchType != null) {
								output.Write(' ');
								s.ExceptionHandler.CatchType.WriteTo(output, ILNameSyntax.TypeName);
							}
							output.WriteLine();
							break;
						case ExceptionHandlerType.Finally:
							output.WriteLine("finally");
							break;
						case ExceptionHandlerType.Fault:
							output.WriteLine("fault");
							break;
						default:
							throw new NotSupportedException();
					}
					output.WriteLine("{");
					break;
				case ILStructureType.Filter:
					output.WriteLine("filter");
					output.WriteLine("{");
					break;
				default:
					throw new NotSupportedException();
			}
			output.Indent();
		}

		void WriteStructureBody(CilBody body, ILStructure s, HashSet<int> branchTargets, ref int index, int codeSize)
		{
			bool isFirstInstructionInStructure = true;
			bool prevInstructionWasBranch = false;
			int childIndex = 0;
			var instructions = body.Instructions;
			while (index < instructions.Count) {
				Instruction inst = instructions[index];
				if (inst.Offset >= s.EndOffset)
					break;
				int offset = (int)inst.Offset;
				if (childIndex < s.Children.Count && s.Children[childIndex].StartOffset <= offset && offset < s.Children[childIndex].EndOffset) {
					ILStructure child = s.Children[childIndex++];
					WriteStructureHeader(child);
					WriteStructureBody(body, child, branchTargets, ref index, codeSize);
					WriteStructureFooter(child);
				} else {
					if (!isFirstInstructionInStructure && (prevInstructionWasBranch || branchTargets.Contains(offset))) {
						output.WriteLine();	// put an empty line after branches, and in front of branch targets
					}
					WriteInstruction(output, inst);
					output.WriteLine();

					prevInstructionWasBranch = inst.OpCode.FlowControl == FlowControl.Branch
						|| inst.OpCode.FlowControl == FlowControl.Cond_Branch
						|| inst.OpCode.FlowControl == FlowControl.Return
						|| inst.OpCode.FlowControl == FlowControl.Throw;

					index++;
				}
				isFirstInstructionInStructure = false;
			}
		}

		void WriteStructureFooter(ILStructure s)
		{
			output.Unindent();
			switch (s.Type) {
				case ILStructureType.Loop:
					output.WriteLine("// end loop");
					break;
				case ILStructureType.Try:
					output.WriteLine("} // end .try");
					break;
				case ILStructureType.Handler:
					output.WriteLine("} // end handler");
					break;
				case ILStructureType.Filter:
					output.WriteLine("} // end filter");
					break;
				default:
					throw new NotSupportedException();
			}
		}

		protected virtual void WriteInstruction(ITextOutput output, Instruction instruction)
		{
			if (ShowSequencePoints && instruction.SequencePoint != null) {
				SequencePoint sp = instruction.SequencePoint;
				output.Write("// sequence point: ");
				if (sp.StartLine == 0xFEEFEE && sp.StartLine == sp.EndLine) {
					output.WriteLine("hidden");
				} else {
					output.WriteLine($"(line {sp.StartLine}, col {sp.StartColumn}) to (line {sp.EndLine}, col {sp.EndColumn}) in {sp.Document?.Url}");
				}
			}
			instruction.WriteTo(output);
		}
	}
}
