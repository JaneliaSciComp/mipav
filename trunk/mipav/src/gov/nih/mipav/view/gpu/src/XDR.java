package gov.nih.mipav.view.gpu.src;

/**
 * This class provides a set of simple methods to change primitive
 * values to and from a linear array of bytes in the XDR format.
 * <p>
 * All methods take as parameters a byte array to read from or to write to and
 * offset at which they should start operating on the array. All put*
 * methods also take
 * one additional parameter - a primitive to be stored in the array. All get*
 * methods return a primitive according to their name/signature.</p>
 *
 * @author Tomasz Wrzosek
 * @version 1.0
 * @see XDR format specification in RFC 1832
 */

final class XDR {

  static void putBoolean(byte[] b, int off, boolean bool) {
    b[off + 3] = (byte) (bool ? 1 : 0);
    b[off + 2] = 0;
    b[off + 1] = 0;
    b[off + 0] = 0;
  }

  static void putChar(byte[] b, int off, char chr) {
    b[off + 1] = (byte) (chr >>> 0);
    b[off + 0] = (byte) (chr >>> 8);
  }

  static void putShort(byte[] b, int off, short shrt) {
    b[off + 1] = (byte) (shrt >>> 0);
    b[off + 0] = (byte) (shrt >>> 8);
  }

  static void putInt(byte[] b, int off, int val) {
    b[off + 3] = (byte) (val >>> 0);
    b[off + 2] = (byte) (val >>> 8);
    b[off + 1] = (byte) (val >>> 16);
    b[off + 0] = (byte) (val >>> 24);
  }

  static void putFloat(byte[] b, int off, float flt) {
    int i = Float.floatToIntBits(flt);
    b[off + 3] = (byte) (i >>> 0);
    b[off + 2] = (byte) (i >>> 8);
    b[off + 1] = (byte) (i >>> 16);
    b[off + 0] = (byte) (i >>> 24);
  }

  static void putLong(byte[] b, int off, long lng) {
    b[off + 7] = (byte) (lng >>> 0);
    b[off + 6] = (byte) (lng >>> 8);
    b[off + 5] = (byte) (lng >>> 16);
    b[off + 4] = (byte) (lng >>> 24);
    b[off + 3] = (byte) (lng >>> 32);
    b[off + 2] = (byte) (lng >>> 40);
    b[off + 1] = (byte) (lng >>> 48);
    b[off + 0] = (byte) (lng >>> 56);
  }

  static void putDouble(byte[] b, int off, double dbl) {
    long l = Double.doubleToLongBits(dbl);
    b[off + 7] = (byte) (l >>> 0);
    b[off + 6] = (byte) (l >>> 8);
    b[off + 5] = (byte) (l >>> 16);
    b[off + 4] = (byte) (l >>> 24);
    b[off + 3] = (byte) (l >>> 32);
    b[off + 2] = (byte) (l >>> 40);
    b[off + 1] = (byte) (l >>> 48);
    b[off + 0] = (byte) (l >>> 56);
  }


  static boolean getBoolean(byte[] b, int off) {
    return b[off + 3] != 0;
  }

  static char getChar(byte[] b, int off) {
    return (char) (((b[off + 1] & 0xff) << 0) +
                   ((b[off + 0] & 0xff) << 8));
  }

  static short getShort(byte[] b, int off) {
    return (short) (((b[off + 1] & 0xff) << 0) +
                    ((b[off + 0] & 0xff) << 8));
  }

  static int getInt(byte[] b, int off) {
    return ((b[off + 3] & 0xff) << 0) +
           ((b[off + 2] & 0xff) << 8) +
           ((b[off + 1] & 0xff) << 16) +
           ((b[off + 0] & 0xff) << 24);
  }

  static float getFloat(byte[] b, int off) {
    int i = ((b[off + 3] & 0xff) << 0) +
            ((b[off + 2] & 0xff) << 8) +
            ((b[off + 1] & 0xff) << 16) +
            ((b[off + 0] & 0xff) << 24);
    return Float.intBitsToFloat(i);
  }

  static long getLong(byte[] b, int off) {
    return ((b[off + 7] & 0xffL) << 0) +
           ((b[off + 6] & 0xffL) << 8) +
           ((b[off + 5] & 0xffL) << 16) +
           ((b[off + 4] & 0xffL) << 24) +
           ((b[off + 3] & 0xffL) << 32) +
           ((b[off + 2] & 0xffL) << 40) +
           ((b[off + 1] & 0xffL) << 48) +
           ((b[off + 0] & 0xffL) << 56);
  }

  static double getDouble(byte[] b, int off) {
    long l = ((b[off + 7] & 0xffL) << 0) +
             ((b[off + 6] & 0xffL) << 8) +
             ((b[off + 5] & 0xffL) << 16) +
             ((b[off + 4] & 0xffL) << 24) +
             ((b[off + 3] & 0xffL) << 32) +
             ((b[off + 2] & 0xffL) << 40) +
             ((b[off + 1] & 0xffL) << 48) +
             ((b[off + 0] & 0xffL) << 56);
    return Double.longBitsToDouble(l);
  }
}
