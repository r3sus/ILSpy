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
using System.Linq;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;
using dnlib.DotNet;
using ICSharpCode.Decompiler;

namespace ICSharpCode.ILSpy.TreeNodes
{
	/// <summary>
	/// Tree Node representing a field, method, property, or event.
	/// </summary>
	public sealed class MethodTreeNode : ILSpyTreeNode, IMemberTreeNode
	{
		public MethodDef MethodDefinition { get; }

		public MethodTreeNode(MethodDef method)
		{
			if (method == null)
				throw new ArgumentNullException(nameof(method));
			this.MethodDefinition = method;
		}

		public override object Text => GetText(MethodDefinition, Language) + MethodDefinition.MDToken.ToSuffixString();

		public static object GetText(MethodDef method, Language language)
		{
			StringBuilder b = new StringBuilder();
			b.Append('(');
			for (int i = 0; i < method.Parameters.Count; i++) {
				if (i > 0)
					b.Append(", ");
				b.Append(language.TypeToString(method.Parameters[i].Type.ToTypeDefOrRef(), false, method.Parameters[i].ParamDef));
			}
			if (method.CallingConvention == CallingConvention.VarArg) {
				if (method.MethodSig.Params.Count > 0)
					b.Append(", ");
				b.Append("...");
			}
			if (method.IsConstructor) {
				b.Append(')');
			} else {
				b.Append(") : ");
				b.Append(language.TypeToString(method.ReturnType.ToTypeDefOrRef(), false, method.Parameters.ReturnParameter.ParamDef));
			}
			return HighlightSearchMatch(language.FormatMethodName(method), b.ToString());
		}

		public override object Icon => GetIcon(MethodDefinition);

		public static ImageSource GetIcon(MethodDef method)
		{
			if (method.IsSpecialName && method.Name.StartsWith("op_", StringComparison.Ordinal)) {
				return Images.GetIcon(MemberIcon.Operator, GetOverlayIcon(method.Attributes), false);
			}

			if (method.IsStatic && method.HasCustomAttributes) {
				foreach (var ca in method.CustomAttributes) {
					if (ca.AttributeType.FullName == "System.Runtime.CompilerServices.ExtensionAttribute") {
						return Images.GetIcon(MemberIcon.ExtensionMethod, GetOverlayIcon(method.Attributes), false);
					}
				}
			}

			if (method.IsSpecialName &&
				(method.Name == ".ctor" || method.Name == ".cctor")) {
				return Images.GetIcon(MemberIcon.Constructor, GetOverlayIcon(method.Attributes), method.IsStatic);
			}

			if (method.HasImplMap)
				return Images.GetIcon(MemberIcon.PInvokeMethod, GetOverlayIcon(method.Attributes), true);

			bool showAsVirtual = method.IsVirtual && !(method.IsNewSlot && method.IsFinal) && !method.DeclaringType.IsInterface;

			return Images.GetIcon(
				showAsVirtual ? MemberIcon.VirtualMethod : MemberIcon.Method,
				GetOverlayIcon(method.Attributes),
				method.IsStatic);
		}

		private static AccessOverlayIcon GetOverlayIcon(MethodAttributes methodAttributes)
		{
			switch (methodAttributes & MethodAttributes.MemberAccessMask) {
				case MethodAttributes.Public:
					return AccessOverlayIcon.Public;
				case MethodAttributes.Assembly:
					return AccessOverlayIcon.Internal;
				case MethodAttributes.FamANDAssem:
					return AccessOverlayIcon.PrivateProtected;
				case MethodAttributes.Family:
					return AccessOverlayIcon.Protected;
				case MethodAttributes.FamORAssem:
					return AccessOverlayIcon.ProtectedInternal;
				case MethodAttributes.Private:
					return AccessOverlayIcon.Private;
				case MethodAttributes.CompilerControlled:
					return AccessOverlayIcon.CompilerControlled;
				default:
					throw new NotSupportedException();
			}
		}

		public override void Decompile(Language language, ITextOutput output, DecompilationOptions options)
		{
			language.DecompileMethod(MethodDefinition, output, options);
		}

		public override FilterResult Filter(FilterSettings settings)
		{
			if (settings.ShowApiLevel == ApiVisibility.PublicOnly && !IsPublicAPI)
				return FilterResult.Hidden;
			if (settings.SearchTermMatches(MethodDefinition.Name) && (settings.ShowApiLevel == ApiVisibility.All || settings.Language.ShowMember(MethodDefinition)))
				return FilterResult.Match;
			else
				return FilterResult.Hidden;
		}

		public override bool IsPublicAPI {
			get {
				return MethodDefinition.IsPublic || MethodDefinition.IsFamily || MethodDefinition.IsFamilyOrAssembly;
			}
		}

		IMemberRef IMemberTreeNode.Member => MethodDefinition;
	}
}
