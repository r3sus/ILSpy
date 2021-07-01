using System.Collections.Generic;
using dnlib.DotNet;
using ICSharpCode.Decompiler.TypeSystem;

namespace ICSharpCode.Decompiler.CSharp
{
	public class CodeMappingInfo
	{
		public PEFile Module { get; }
		public TypeDef TypeDefinition { get; }

		Dictionary<MethodDef, List<MethodDef>> parts;
		Dictionary<MethodDef, MethodDef> parents;

		public CodeMappingInfo(PEFile module, TypeDef type)
		{
			this.Module = module;
			this.TypeDefinition = type;
			this.parts = new Dictionary<MethodDef, List<MethodDef>>();
			this.parents = new Dictionary<MethodDef, MethodDef>();
		}

		public IEnumerable<MethodDef> GetMethodParts(MethodDef method)
		{
			if (parts.TryGetValue(method, out var p))
				return p;
			return new[] { method };
		}

		public MethodDef GetParentMethod(MethodDef method)
		{
			if (parents.TryGetValue(method, out var p))
				return p;
			return method;
		}

		public void AddMapping(MethodDef parent, MethodDef part)
		{
			//Debug.Print("Parent: " + MetadataTokens.GetRowNumber(parent) + " Part: " + MetadataTokens.GetRowNumber(part));
			if (parents.ContainsKey(part))
				return;
			parents.Add(part, parent);
			if (!parts.TryGetValue(parent, out var list)) {
				list = new List<MethodDef>();
				parts.Add(parent, list);
			}
			list.Add(part);
		}
	}
}
