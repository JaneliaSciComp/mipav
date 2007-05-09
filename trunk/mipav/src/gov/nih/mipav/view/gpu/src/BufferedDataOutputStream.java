package gov.nih.mipav.view.gpu.src;

import java.io.*;


/**
 * Buffered data output stream that supports serialization of arrays of
 * primitive types.
 *
 * @author Tomasz Wrzosek
 * @version 1.0
 * @deprecated
 */

public class BufferedDataOutputStream extends OutputStream implements DataOutput {

  private final OutputStream out;

  private final int BUFFER_SIZE;

  private int pos = 0;

  private final byte [] buffer;

  /** Constructs buffered data output stream.
   *  @param out the underlying output stream.
   */

  public BufferedDataOutputStream(OutputStream out) {
    this(out, 1024);
  }

  /** Constructs buffered data output stream.
   *  @param out the underlying output stream.
   *  @param buffer_size the size of the data buffer
   */

  public BufferedDataOutputStream(OutputStream out, int buffer_size) {
    this.out = out;
    BUFFER_SIZE = buffer_size;
    buffer = new byte [BUFFER_SIZE];
  }


  /** Writes an array of bytes into the output stream.
   */

  public void write(byte [] b) throws IOException {
    write (b, 0, b.length);
  }

  /** Writes lng of bytes into the output stream. It starts writing the bytes from b[off].
   */

  public void write(byte [] b, int off, int lng) throws IOException {
    if (lng == 0 || b.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > b.length) {
      throw new IndexOutOfBoundsException("Array size "+b.length+" trying to read from "+(off+lng));
    }
    if (lng > BUFFER_SIZE || pos + lng > BUFFER_SIZE) {
      drain(0, pos);
      out.write(b, off, lng);
    } else {
      System.arraycopy(b, off, buffer, pos, lng);
      pos += lng;
    }
  }

  /** Writes a byte into the output stream. The 24 high-order bits are ignored.
   */

  public void write(int b) throws IOException {
    if (pos == BUFFER_SIZE) {
      drain(0, pos);
    }
    buffer[pos++] = (byte) b;
  }

  /** Closes the underlying output stream.
   */

  public void close() throws IOException {
    drain(0, pos);
    out.close();
  }

  /** Flushes not written (possibly buffered) data into the stream.
   */

  public void flush() throws IOException {
    drain(0, pos);
    out.flush();
  }

  /** Empties the buffer into the underlying stream. Does not flush the stream. */

  protected void flushBuffer() throws IOException {
    drain(0, pos);
  }

  /* Drain the buffer into the underlying OutputStream */

  private void drain() throws IOException {
    drain (0, buffer.length);
  }

  private void drain(int lng) throws IOException {
    drain (0, lng);
  }

  private void drain(int off, int lng) throws IOException {
    out.write(buffer, off, lng);
    pos = 0;
  }

  /** Writes a byte into the output stream. The 24 high-order bits are ignored.
   */

  public void writeByte(int b) throws IOException {
    if (pos == BUFFER_SIZE) {
      drain(0, pos);
    }
    buffer[pos++] = (byte) b;
  }

  /** Writes a boolean into the output stream.
   */

  public void writeBoolean(boolean b) throws IOException {
    if (pos > BUFFER_SIZE - 4) {
      drain(0, pos);
    }
    XDR.putBoolean(buffer, pos, b);
    pos += 4;
  }

  /** Writes a short into the output stream.
   */

  public void writeShort(int s) throws IOException {
    if (pos > BUFFER_SIZE - 2) {
      drain(0, pos);
    }
    XDR.putShort(buffer, pos, (short) s);
    pos += 2;
  }

  /** Writes a char into the output stream.
   */

  public void writeChar(int c) throws IOException {
    if (pos > BUFFER_SIZE - 2) {
      drain(0, pos);
    }
    XDR.putChar(buffer, pos, (char) c);
    pos += 2;
  }

  /** Writes an integer into the output stream.
   */

  public void writeInt(int i) throws IOException {
    if (pos > BUFFER_SIZE - 4) {
      drain(0, pos);
    }
    XDR.putInt(buffer, pos, i);
    pos += 4;
  }

  /** Writes a long into the output stream.
   */

  public void writeLong(long l) throws IOException {
    if (pos > BUFFER_SIZE - 8) {
      drain(0, pos);
    }
    XDR.putLong(buffer, pos, l);
    pos += 8;
  }

  /** Writes a float into the output stream.
   */

  public void writeFloat(float f) throws IOException {
    if (pos > BUFFER_SIZE - 4) {
      drain(0, pos);
    }
    XDR.putFloat(buffer, pos, f);
    pos += 4;
  }

  /** Writes a double into the output stream.
   */

  public void writeDouble(double d) throws IOException {
    if (pos > BUFFER_SIZE - 8) {
      drain(0, pos);
    }
    XDR.putDouble(buffer, pos, d);
    pos += 8;
  }

  /** Writes exactly lng of bytes to the stream starting from bytes[off].
   *  <p>If bytes is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than bytes length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or bytes length is 0 then no bytes are written.</p>
   *  @param bytes the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeBytes(byte [] bytes, int off, int lng) throws IOException {
    write(bytes, off, lng);
  }

  /** Writes exactly lng of booleans to the stream starting from booleans[off].
   *  <p>If booleans is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than booleans length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or booleans length is 0 then no booleans are written.</p>
   *  @param booleans the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeBooleans(boolean [] booleans, int off, int lng) throws IOException {
    if (lng == 0 || booleans.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > booleans.length) {
      throw new IndexOutOfBoundsException("Array size "+booleans.length+" trying to read from "+(off+lng));
    }
    int pos = this.pos;
    int endPos = off + lng;
    while (off < endPos) {
      int begPos = off;
      int toWrite = Math.min((BUFFER_SIZE - pos) >> 2, endPos - off);
      while ((off - begPos) < toWrite) {
        XDR.putBoolean(buffer, pos, booleans[off++]);
        pos += 4;
      }
      if (pos > BUFFER_SIZE - 4) {
        drain(0, pos);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }

  /** Writes exactly lng of chars to the stream starting from chars[off].
   *  <p>If chars is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than chars length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or chars length is 0 then no chars are written.</p>
   *  @param chars the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeChars(char [] chars, int off, int lng) throws IOException {
    if (lng == 0 || chars.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > chars.length) {
      throw new IndexOutOfBoundsException("Array size "+chars.length+" trying to read from "+(off+lng));
    }
    int pos = this.pos;
    int endPos = off + lng;
    while (off < endPos) {
      int begPos = off;
      int toWrite = Math.min((BUFFER_SIZE - pos) >> 1, endPos - off);
      while ((off - begPos) < toWrite) {
        XDR.putChar(buffer, pos, chars[off++]);
        pos+=2;
      }
      if (pos > BUFFER_SIZE - 2) {
        drain(0, pos);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }

  /** Writes exactly lng of shorts to the stream starting from shorts[off].
   *  <p>If shorts is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than shorts length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or shorts length is 0 then no shorts are written.</p>
   *  @param shorts the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeShorts(short [] shorts, int off, int lng) throws IOException {
    if (lng == 0 || shorts.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > shorts.length) {
      throw new IndexOutOfBoundsException("Array size "+shorts.length+" trying to read from "+(off+lng));
    }
    int pos = this.pos;
    int endPos = off + lng;
    while (off < endPos) {
      int begPos = off;
      int toWrite = Math.min((BUFFER_SIZE - pos) >> 1, endPos - off);
      while ((off - begPos) < toWrite) {
        XDR.putShort(buffer, pos, shorts[off++]);
        pos+=2;
      }
      if (pos > BUFFER_SIZE - 2) {
        drain(0, pos);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }

  /** Writes exactly lng of integers to the stream starting from ints[off].
   *  <p>If ints is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than ints length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or ints length is 0 then no integers are written.</p>
   *  @param ints the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeInts(int [] ints, int off, int lng) throws IOException {
    if (lng == 0 || ints.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > ints.length) {
      throw new IndexOutOfBoundsException("Array size "+ints.length+" trying to read from "+(off+lng));
    }
    int pos = this.pos;
    int endPos = off + lng;
    while (off < endPos) {
      int begPos = off;
      int toWrite = Math.min((BUFFER_SIZE - pos) >> 2, endPos - off);
      while ((off - begPos) < toWrite) {
        XDR.putInt(buffer, pos, ints[off++]);
        pos+=4;
      }
      if (pos > BUFFER_SIZE - 4) {
        drain(0, pos);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }

  /** Writes exactly lng of floats to the stream starting from floats[off].
   *  <p>If floats is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than floats length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or floats length is 0 then no floats are written.</p>
   *  @param floats the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeFloats(float [] floats, int off, int lng) throws IOException {
    if (lng == 0 || floats.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > floats.length) {
      throw new IndexOutOfBoundsException("Array size "+floats.length+" trying to read from "+(off+lng));
    }
    int pos = this.pos;
    int endPos = off + lng;
    while (off < endPos) {
      int begPos = off;
      int toWrite = Math.min((BUFFER_SIZE - pos) >> 2, endPos - off);
      while ((off - begPos) < toWrite) {
        XDR.putFloat(buffer, pos, floats[off++]);
        pos+=4;
      }
      if (pos > BUFFER_SIZE - 4) {
        drain(0, pos);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }

  /** Writes exactly lng of longs to the stream starting from longs[off].
   *  <p>If longs is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than longs length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or longs length is 0 then no longs are written.</p>
   *  @param longs the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeLongs(long [] longs, int off, int lng) throws IOException {
    if (lng == 0 || longs.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > longs.length) {
      throw new IndexOutOfBoundsException("Array size "+longs.length+" trying to read from "+(off+lng));
    }
    int pos = this.pos;
    int endPos = off + lng;
    while (off < endPos) {
      int begPos = off;
      int toWrite = Math.min((BUFFER_SIZE - pos) >> 3, endPos - off);
      while ((off - begPos) < toWrite) {
        XDR.putLong(buffer, pos, longs[off++]);
        pos+=8;
      }
      if (pos > BUFFER_SIZE - 8) {
        drain(0, pos);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }

  /** Writes exactly lng of doubles to the stream starting from doubles[off].
   *  <p>If doubles is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than doubles length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or doubles length is 0 then no doubles are written.</p>
   *  @param doubles the source array
   *  @param off starting index in array
   *  @param lng number of items to write
   *  @throws IOException when I/O error occurs.
   */

  public void writeDoubles(double [] doubles, int off, int lng) throws IOException {
    if (lng == 0 || doubles.length == 0) {
      return;
    } //else
    if (off < 0 || lng < 0 || off+lng > doubles.length) {
      throw new IndexOutOfBoundsException("Array size "+doubles.length+" trying to read from "+(off+lng));
    }
    int pos = this.pos;
    int endPos = off + lng;
    while (off < endPos) {
      int begPos = off;
      int toWrite = Math.min((BUFFER_SIZE - pos) >> 3, endPos - off);
      while ((off - begPos) < toWrite) {
        XDR.putDouble(buffer, pos, doubles[off++]);
        pos+=8;
      }
      if (pos > BUFFER_SIZE - 8) {
        drain(0, pos);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }

  /** Writes a string as an array of chars into the output stream.
   */

  public void writeChars(String str) throws IOException {
    int lng = str.length();
    char [] chars = new char[lng];
    str.getChars(0,lng,chars,0);
    writeChars(chars,0,lng);
  }

  /** Writes a string as an array of bytes into the output stream. High order bytes of each char are ignored.
   */

  public void writeBytes(String str) throws IOException {
    byte [] bytes = str.getBytes();
    write(bytes,0,bytes.length);
  }

  /** Writes a string in the UTF into the output stream.
   */

  public void writeUTF(String str)  throws IOException {
    int utfLng = getUTFStringLength(str);
    if (utfLng == 0) {
      return;
    } else if (utfLng > 0xFFFF) {
      throw new UTFDataFormatException("String too long. Length: "+utfLng);
    } else if (utfLng == str.length()) {
      byte [] bytes = str.getBytes();
      writeShort((short)utfLng);
      write(bytes,0,bytes.length);
      return;
    } //else
    byte [] bytes = new byte[utfLng];
    char [] chars = new char[str.length()];
    str.getChars(0,str.length(),chars,0);
    int pos=0; char c;
    for (int i=0;i<chars.length;i++) {
      c = chars[i];
      if (c > 0 && c < 0x80) {
        bytes[pos++] = (byte) c;
      } else if (c > 0x800) {
        bytes[pos++] = (byte)(0xe0 | (0x0f & (c >> 12)));
        bytes[pos++] = (byte)(0x80 | (0x3f & (c >>  6)));
        bytes[pos++] = (byte)(0x80 | (0x3f & c));
      } else {
        bytes[pos++] = (byte)(0xc0 | (0x1f & (c >> 6)));
        bytes[pos++] = (byte)(0x80 | (0x3f & c));
      }
    }
    writeShort((short)utfLng);
    write(bytes,0,bytes.length);
  }


  protected int getUTFStringLength(String str) throws IOException{
    char [] chars = new char[str.length()];
    str.getChars(0,str.length(),chars,0);
    int lng=0;
    for (int i=0;i<chars.length;i++) {
      if (chars[i]>0 && chars[i]<0x80) {
        lng++;
      } else if (chars[i]>0x800) {
        lng+=3;
      } else {
        lng+=2;
      }
    }
    return lng;
  }
}
