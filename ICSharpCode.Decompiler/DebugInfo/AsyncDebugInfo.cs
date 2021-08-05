using System.Collections.Immutable;

namespace ICSharpCode.Decompiler.DebugInfo
{
	public readonly struct AsyncDebugInfo
	{
		public readonly int CatchHandlerOffset;
		public readonly ImmutableArray<Await> Awaits;

		public AsyncDebugInfo(int catchHandlerOffset, ImmutableArray<Await> awaits)
		{
			this.CatchHandlerOffset = catchHandlerOffset;
			this.Awaits = awaits;
		}

		public readonly struct Await
		{
			public readonly int YieldOffset;
			public readonly int ResumeOffset;

			public Await(int yieldOffset, int resumeOffset)
			{
				this.YieldOffset = yieldOffset;
				this.ResumeOffset = resumeOffset;
			}
		}
	}
}
