using System;
using dnlib.DotNet;

namespace ICSharpCode.Decompiler.TypeSystem.Implementation
{
	sealed class BlobReader
	{
		internal static int GetBlobHashCode(byte[] blob)
		{
			unchecked {
				int hash = 0;
				foreach (byte b in blob) {
					hash *= 257;
					hash += b;
				}
				return hash;
			}
		}

		internal static bool BlobEquals(byte[] a, byte[] b)
		{
			if (a.Length != b.Length)
				return false;
			for (int i = 0; i < a.Length; i++) {
				if (a[i] != b[i])
					return false;
			}
			return true;
		}

		byte[] buffer;
		int position;
		readonly IAssembly currentResolvedAssembly;

		public BlobReader(byte[] buffer, IAssembly currentResolvedAssembly)
		{
			if (buffer == null)
				throw new ArgumentNullException("buffer");
			this.buffer = buffer;
			this.currentResolvedAssembly = currentResolvedAssembly;
		}

		public byte ReadByte()
		{
			return buffer[position++];
		}

		public sbyte ReadSByte()
		{
			unchecked {
				return(sbyte) ReadByte();
			}
		}

		public byte[] ReadBytes(int length)
		{
			var bytes = new byte[length];
			Buffer.BlockCopy(buffer, position, bytes, 0, length);
			position += length;
			return bytes;
		}

		public ushort ReadUInt16()
		{
			unchecked {
				ushort value =(ushort)(buffer[position]
				                       |(buffer[position + 1] << 8));
				position += 2;
				return value;
			}
		}

		public short ReadInt16()
		{
			unchecked {
				return(short) ReadUInt16();
			}
		}

		public uint ReadUInt32()
		{
			unchecked {
				uint value =(uint)(buffer[position]
				                   |(buffer[position + 1] << 8)
				                   |(buffer[position + 2] << 16)
				                   |(buffer[position + 3] << 24));
				position += 4;
				return value;
			}
		}

		public int ReadInt32()
		{
			unchecked {
				return(int) ReadUInt32();
			}
		}

		public ulong ReadUInt64()
		{
			unchecked {
				uint low = ReadUInt32();
				uint high = ReadUInt32();

				return(((ulong) high) << 32) | low;
			}
		}

		public long ReadInt64()
		{
			unchecked {
				return(long) ReadUInt64();
			}
		}

		public uint ReadCompressedUInt32()
		{
			unchecked {
				byte first = ReadByte();
				if((first & 0x80) == 0)
					return first;

				if((first & 0x40) == 0)
					return((uint)(first & ~0x80) << 8)
						| ReadByte();

				return((uint)(first & ~0xc0) << 24)
					|(uint) ReadByte() << 16
					|(uint) ReadByte() << 8
					| ReadByte();
			}
		}

		public float ReadSingle()
		{
			unchecked {
				if(!BitConverter.IsLittleEndian) {
					var bytes = ReadBytes(4);
					Array.Reverse(bytes);
					return BitConverter.ToSingle(bytes, 0);
				}

				float value = BitConverter.ToSingle(buffer, position);
				position += 4;
				return value;
			}
		}

		public double ReadDouble()
		{
			unchecked {
				if(!BitConverter.IsLittleEndian) {
					var bytes = ReadBytes(8);
					Array.Reverse(bytes);
					return BitConverter.ToDouble(bytes, 0);
				}

				double value = BitConverter.ToDouble(buffer, position);
				position += 8;
				return value;
			}
		}

		public string ReadSerString ()
		{
			if (buffer [position] == 0xff) {
				position++;
				return null;
			}

			int length = (int) ReadCompressedUInt32();
			if (length == 0)
				return string.Empty;

			string @string = System.Text.Encoding.UTF8.GetString(
				buffer, position,
				buffer [position + length - 1] == 0 ? length - 1 : length);

			position += length;
			return @string;
		}
	}
}
