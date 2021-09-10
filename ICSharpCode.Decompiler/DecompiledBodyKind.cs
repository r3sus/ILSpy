namespace ICSharpCode.Decompiler
{
	public enum DecompiledBodyKind {
		/// <summary>
		/// Decompile the body
		/// </summary>
		Full,

		/// <summary>
		/// Create an empty body, but add extra statements if necessary in order for the code to compile.
		/// </summary>
		Empty,

		/// <summary>
		/// Don't use a body
		/// </summary>
		None,
	}
}
