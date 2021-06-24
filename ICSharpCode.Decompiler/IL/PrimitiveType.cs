// Copyright (c) 2014 Daniel Grunwald
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

using dnlib.DotNet;

namespace ICSharpCode.Decompiler.IL
{
	public enum PrimitiveType : byte
	{
		None = 0,
		I1 = ElementType.I1,
		I2 = ElementType.I2,
		I4 = ElementType.I4,
		I8 = ElementType.I8,
		R4 = ElementType.R4,
		R8 = ElementType.R8,
		U1 = ElementType.U1,
		U2 = ElementType.U2,
		U4 = ElementType.U4,
		U8 = ElementType.U8,
		I = ElementType.I,
		U = ElementType.U,
		Ref = ElementType.ByRef,
		Unknown = 255
	}
}
