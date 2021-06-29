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

using dnlib.DotNet;
using ICSharpCode.Decompiler.TypeSystem.Implementation;

namespace ICSharpCode.Decompiler.TypeSystem
{
	/// <summary>
	/// Allows decoding signatures using decompiler types.
	/// </summary>
	sealed class TypeProvider : ICompilationProvider
	{
		readonly MetadataModule module;
		readonly ICompilation compilation;

		public TypeProvider(MetadataModule module)
		{
			this.module = module;
			this.compilation = module.Compilation;
		}

		public MetadataModule Module => module;
		public ICompilation Compilation => compilation;

		public IType GetTypeFromDefinition(ModuleDef reader, TypeDef handle, byte rawTypeKind)
		{
			return handle.DecodeSignature(this, new GenericContext());
		}

		public IType GetTypeFromReference(ModuleDef reader, TypeRef handle, byte rawTypeKind)
		{
			return handle.DecodeSignature(this, new GenericContext());
		}
	}
}
