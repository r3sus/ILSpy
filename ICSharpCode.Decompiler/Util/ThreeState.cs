namespace ICSharpCode.Decompiler.Util
{
	public enum ThreeState : byte
	{
		Unknown,
		False,
		True
	}

	public static class ThreeStateExtensions
	{
		public static ThreeState ToThreeState(this bool boolean)
		{
			return boolean ? ThreeState.True : ThreeState.False;
		}

		public static ThreeState ToThreeState(this bool? boolean)
		{
			if (boolean.HasValue)
				return boolean.Value ? ThreeState.True : ThreeState.False;
			return ThreeState.Unknown;
		}

		public static bool? ToNullableBool(this ThreeState threeState)
		{
			if (threeState == ThreeState.Unknown)
				return null;
			return threeState == ThreeState.True;
		}
	}
}
