using dnlib.DotNet;
using dnlib.DotNet.Writer;

namespace ICSharpCode.Decompiler.Util
{
	public class CustomAttributeSerializationHelper : ICustomAttributeWriterHelper
	{
		public void Error(string message)
		{
			throw new CABlobParserException(message);
		}

		public bool MustUseAssemblyName(IType type)
		{
			return true;
		}
	}
}
