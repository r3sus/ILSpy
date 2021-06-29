using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using dnlib.DotNet;

namespace ICSharpCode.Decompiler
{
	sealed class NamespaceDefinition
	{
		internal readonly string FullName;
		internal readonly string Name;
		internal readonly List<NamespaceDefinition> Children = new List<NamespaceDefinition>();
		internal readonly List<TypeDef> Types = new List<TypeDef>();

		public NamespaceDefinition(string fullName, string name)
		{
			this.FullName = fullName;
			this.Name = name;
		}

		public static NamespaceDefinition GetUnresolvedRootNamespace(StringComparer nameComparer, IList<TypeDef> defs)
		{
			var root = new NamespaceDefinition(string.Empty, string.Empty);
			var dict = new Dictionary<string, NamespaceDefinition>(nameComparer);
			dict.Add(root.FullName, root);
			foreach (var typeName in defs) {
				var ns = GetOrAddNamespace(dict, typeName.Namespace);
				//ns.Types.Add(typeName);
			}

			foreach (TypeDef typeDef in defs) {
				dict[typeDef.Namespace].Types.Add(typeDef);
			}
			return root;
		}

		private static NamespaceDefinition GetOrAddNamespace(Dictionary<string, NamespaceDefinition> dict, string fullName)
		{
			if (dict.TryGetValue(fullName, out NamespaceDefinition ns))
				return ns;
			int pos = fullName.LastIndexOf('.');
			NamespaceDefinition parent;
			string name;
			if (pos < 0) {
				parent = dict[string.Empty]; // root
				name = fullName;
			} else {
				parent = GetOrAddNamespace(dict, fullName.Substring(0, pos));
				name = fullName.Substring(pos + 1);
			}
			ns = new NamespaceDefinition(fullName, name);
			parent.Children.Add(ns);
			dict.Add(fullName, ns);
			return ns;
		}
	}
}
