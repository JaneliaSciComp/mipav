package gov.nih.mipav.model.algorithms.t2mapping.cj.utilities;

public class Unsigned
{
  final static public short toUByte(short t) { return toUByte((int)t); }
  final static public short toUByte(float t) { return toUByte((int)t); }
  final static public short toUShort(float t) { return toUShort((int)t); }
  final static public short toUShort(double t) { return toUShort((int)t); }
  final static public short toUShort(int t)
  { 
    if (t<0) return 0; else if (t >= 1<<16) return -1;
    else if (t >= 1<<15) return (short)(t-(1<<16));
    else return (short)t;
  }
  final static public byte toUByte(int t)
  { 
    if (t<0) return 0; else if (t >= 1<<8) return -1;
    else if (t >= 1<<7) return (byte)(t-(1<<8));
    else return (byte)t;
  }

  final static public float floatFromUByte(byte x)
    { return (x<0)?x+(1<<8):x; }
  final static public float floatFromUShort(short x)
    { return (float)intFromUShort(x); }
  final static public double doubleFromUShort(short x)
    { return (double)intFromUShort(x); }
  final static public int intFromUShort(short x)
    { return (x<0)?x+(1<<16):x; }
}
