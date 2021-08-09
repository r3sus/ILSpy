using System;
using System.Collections.Generic;
using dnlib.DotNet;

namespace ICSharpCode.Decompiler
{
	internal sealed class NamespaceDefinition
	{
		internal readonly string FullName;
		internal readonly string Name;
		internal readonly List<NamespaceDefinition> Children = new List<NamespaceDefinition>();
		internal readonly List<TypeDef> Types = new List<TypeDef>();

		private NamespaceDefinition(string fullName, string name)
		{
			this.FullName = fullName;
			this.Name = name;
		}

		internal static NamespaceDefinition GetRootNamespace(StringComparer nameComparer, IEnumerable<TypeDef> typeDefs)
		{
			var root = new NamespaceDefinition(string.Empty, string.Empty);
			var dict = new Dictionary<string, NamespaceDefinition>(nameComparer);
			dict.Add(string.Empty, root);
			foreach (var typeName in typeDefs) {
				var ns = GetOrAddNamespace(dict, typeName.Namespace);
				ns.Types.Add(typeName);
			}
			return root;
		}

		private static NamespaceDefinition GetOrAddNamespace(Dictionary<string, NamespaceDefinition> dict, string nsFullName)
		{
			if (dict.TryGetValue(nsFullName, out NamespaceDefinition nsDefinition))
				return nsDefinition;
			int pos = nsFullName.LastIndexOf('.');
			NamespaceDefinition parent;
			string name;
			if (pos < 0) {
				parent = dict[string.Empty]; // root
				name = nsFullName;
			} else {
				parent = GetOrAddNamespace(dict, nsFullName.Substring(0, pos));
				name = nsFullName.Substring(pos + 1);
			}
			nsDefinition = new NamespaceDefinition(nsFullName, name);
			parent.Children.Add(nsDefinition);
			dict.Add(nsFullName, nsDefinition);
			return nsDefinition;
		}
	}
}
