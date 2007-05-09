package gov.nih.mipav.view.gpu.src;

import java.io.*;


/**
 * Buffered data input that additionally supports deserialization of arrays of
 * primitive types.
 *
 * @author Tomasz Wrzosek
 * @version 1.0
 * @deprecated
 */

public class BufferedDataInputStream extends InputStream implements DataInput {

  private final InputStream in;

  /** Internal buffer size */
  private final int BUFFER_SIZE;

  private int pos;
  private int end;

  /** Internal buffer */
  private final byte [] buffer;


  /** Constructs buffered data input stream.
   *  @param in the underlying input stream.
   */

  public BufferedDataInputStream(InputStream in) throws IOException {
    this(in, 1024);
  }

  /** Constructs buffered data input stream.
   *  @param in the underlying input stream.
   *  @param buffer_size the size of the data buffer
   */

  public BufferedDataInputStream(InputStream in, int buffer_size) throws IOException {
    this.in = in;
    BUFFER_SIZE = buffer_size;
    buffer = new byte [BUFFER_SIZE];
  }


  /** Reads at most array.length bytes into the array array.
   *  @return number of actually read bytes
   */

  public int read(byte [] array) throws IOException {
    return read(array, 0, array.length);
  }

  /** Reads lng bytes into the array array begining at offset off.
   *  @return number of actually read bytes
   */

  public int read(byte [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return 0;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int read = end - pos;
    if (pos == end) {
      read = refill();
    }
    if (read < 0) {
      return read;
    }
    int toCopy = Math.min(end - pos, lng);
    System.arraycopy(buffer, pos, array, off, toCopy);
    lng -= toCopy;
    pos += toCopy;
    off += toCopy;
    if (lng > 0) {
      read = in.read(array, off, lng);
    } else {
      read = 0;
    }
    return toCopy + read;
  }

  /** Reads one byte of range 0 to 255, inclusive.
   */

  public int read() throws IOException {
    int read = 0;
    if (pos == end) {
      read = refill();
    }
    if (read < 0) {
      return read;
    } else { /* (read > 0) because refill never returns 0 */
      return buffer[pos++];
    }
  }

  /** Returns whether mark operation is supported. Current implementation dos not support mark/reset.
   *  @return false
   */

  public boolean markSupported() {
    return false;
  }

  /*
  public void mark(int readLimit) {
  }

  public void reset() throws IOException {
  }
  */

  /** Returns the number of available bytes in the stream.
   */

  public int available() throws IOException {
    return in.available() + Math.max(end-pos,0);
  }

  /** Skips at most skip bytes.
   *  @return the number of bytes actually skipped.
   */

  public long skip(long skip) throws IOException  {
    if (skip <= 0) {
      return 0;
    }
    if (skip < (end - pos)) {
      pos += (int) skip;
      return skip;
    } // else
    skip = skip - Math.max(end-pos,0);
    long ret = in.skip(skip) + Math.max(end-pos,0);
    pos = end;
    return ret;
  }

  /** Skips at most skip bytes.
   *  @return the number of bytes actually skipped.
   */

  public int skipBytes(int skip) throws IOException {
    return (int) skip(skip);
  }

  /** Closes input stream
   */

  public void close() throws IOException {
    in.close();
  }

  /*  Refills the buffer by reading from the underlying stream at most BUFFER_SIZE bytes.
   *  Sets the correct, new values of <tt>pos</tt> and <tt>end</tt> fields.
   *  @throws IOException from the underlying stream or when number of read bytes is zero
   *  @return number of actually read bytes: -1 when EOF has been reached or <b>positive</b>
   *  number of read bytes.
   */

  private int refill() throws IOException {
    end = in.read(buffer,0,BUFFER_SIZE);
    switch (end) {
      case 0: {
        throw new IOException("Non-zero length but zero bytes read");
      }
      case -1: { //EOF reached
        pos = -1;
        break;
      }
      default: {
        pos = 0;
        break;
      }
    }
    return end;
  }

  /** Reads a boolean from the stream.
   */

  public boolean readBoolean () throws IOException {
    int b = readInt();
    if (b == -1) {
      throw new EOFException ();
    }
    return (b != 0);
  }

  /** Reads a byte from the stream.
   */

  public byte readByte () throws IOException {
    int b = read();
    if (b == -1) {
      throw new EOFException ();
    }
    return (byte) b;
  }

  /** Reads an unsigned byte from the stream.
   */

  public int readUnsignedByte () throws IOException {
    int b = read();
    if (b == -1) {
      throw new EOFException ();
    }
    return b & 0x000000FF;
  }

  protected final byte [] arr = new byte[8];

  /** Reads a char from the stream.
   */

  public char readChar () throws IOException {
    if (pos > end - 2) {
      System.arraycopy(buffer, pos, arr, 0, end - pos);
      int arroff = end - pos;
      int read = refill();
      if (read == -1) {
        throw new EOFException ();
      }
      System.arraycopy(buffer, pos, arr, arroff, 2 - arroff);
      pos += 2 - arroff;
      return XDR.getChar(arr,0);
    } else {
      char chr = XDR.getChar(buffer, pos);
      pos +=2;
      return chr;
    }
  }

  /** Reads a short from the stream.
   */

  public short readShort () throws IOException {
    if (pos > end - 2) {
      System.arraycopy(buffer, pos, arr, 0, end - pos);
      int arroff = end - pos;
      int read = refill();
      if (read == -1) {
        throw new EOFException ();
      }
      System.arraycopy(buffer, pos, arr, arroff, 2 - arroff);
      pos += 2 - arroff;
      return XDR.getShort(arr,0);
    } else {
      short shrt = XDR.getShort(buffer, pos);
      pos +=2;
      return shrt;
    }
  }

  /** Reads an unsigned short from the stream.
   */

  public int readUnsignedShort () throws IOException {
    if (pos > end - 2) {
      System.arraycopy(buffer, pos, arr, 0, end - pos);
      int arroff = end - pos;
      int read = refill();
      if (read == -1) {
        throw new EOFException ();
      }
      System.arraycopy(buffer, pos, arr, arroff, 2 - arroff);
      pos += 2 - arroff;
      return XDR.getShort(arr,0) & 0xFFFF;
    } else {
      short shrt = XDR.getShort(buffer, pos);
      pos +=2;
      return shrt & 0xFFFF;
    }
  }

  /** Reads an integer from the stream.
   */

  public int readInt () throws IOException {
    if (pos > end - 4) {
      System.arraycopy(buffer, pos, arr, 0, end - pos);
      int arroff = end - pos;
      int read = refill();
      if (read == -1) {
        throw new EOFException ();
      }
      System.arraycopy(buffer, pos, arr, arroff, 4 - arroff);
      pos += 4 - arroff;
      return XDR.getInt(arr,0);
    } else {
      int i = XDR.getInt(buffer, pos);
      pos +=4;
      return i;
    }
  }

  /** Reads a long from the stream.
   */

  public long readLong () throws IOException {
    if (pos > end - 8) {
      System.arraycopy(buffer, pos, arr, 0, end - pos);
      int arroff = end - pos;
      int read = refill();
      if (read == -1) {
        throw new EOFException ();
      }
      System.arraycopy(buffer, pos, arr, arroff, 8 - arroff);
      pos += 8 - arroff;
      return XDR.getLong(arr,0);
    } else {
      long l = XDR.getLong(buffer, pos);
      pos +=8;
      return l;
    }
  }

  /** Reads a float from the stream.
   */

  public float readFloat () throws IOException {
    if (pos > end - 4) {
      System.arraycopy(buffer, pos, arr, 0, end - pos);
      int arroff = end - pos;
      int read = refill();
      if (read == -1) {
        throw new EOFException ();
      }
      System.arraycopy(buffer, pos, arr, arroff, 4 - arroff);
      pos += 4 - arroff;
      return XDR.getFloat(arr,0);
    } else {
      float flt = XDR.getFloat(buffer, pos);
      pos +=4;
      return flt;
    }
  }

  /** Reads a double from the stream.
   */

  public double readDouble () throws IOException {
    if (pos > end - 8) {
      System.arraycopy(buffer, pos, arr, 0, end - pos);
      int arroff = end - pos;
      int read = refill();
      if (read == -1) {
        throw new EOFException ();
      }
      System.arraycopy(buffer, pos, arr, arroff, 8 - arroff);
      pos += 8 - arroff;
      return XDR.getDouble(arr,0);
    } else {
      double dbl = XDR.getDouble(buffer, pos);
      pos +=8;
      return dbl;
    }
  }

  /** Reads exactly lng of bytes from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no bytes are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public byte[] readBytes(byte [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min(end - pos, left);
      System.arraycopy(buffer, pos, array, off, toCopy);
      pos += toCopy;
      off += toCopy;
      left -= toCopy;
    }
    this.pos = pos;
    return array;
  }

  /** Reads exactly lng of booleans from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no booleans are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public boolean[] readBooleans(boolean [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min((end - pos) >> 2, left);
      if ((toCopy == 0) && (left > 0) && (end > pos)) {
        byte [] b = rescueRead(4, pos, end);
        array[off++] = XDR.getBoolean(b, 0);
        pos = this.pos;
        left --;
        continue;
      }
      while (toCopy > 0) {
        array[off++] = XDR.getBoolean(buffer,pos);
        pos += 4;
        left--;
        toCopy--;
      }
    }
    this.pos = pos;
    return array;
  }

  /** Reads exactly lng of chars (2*lng bytes) from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no chars are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public char[] readChars(char [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min((end - pos) >> 1, left);
      if ((toCopy == 0) && (left > 0) && (end > pos)) {
        byte[] b = rescueRead(2, pos, end);
        array[off++] = XDR.getChar(b, 0);
        pos = this.pos;
        left--;
        continue;
      }
      while (toCopy > 0) {
        array[off++] = XDR.getChar(buffer,pos);
        pos += 2;
        left--;
        toCopy--;
      }
    }
    this.pos = pos;
    return array;
  }

  /** Reads exactly lng of shorts (2*lng bytes) from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no shorts are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public short[] readShorts(short [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min((end - pos) >> 1, left);
      if ((toCopy == 0) && (left > 0) && (end > pos)) {
        byte[] b = rescueRead(2, pos, end);
        array[off++] = XDR.getShort(b, 0);
        pos = this.pos;
        left--;
        continue;
      }
      while (toCopy > 0) {
        array[off++] = XDR.getShort(buffer,pos);
        pos += 2;
        left--;
        toCopy--;
      }
    }
    this.pos = pos;
    return array;
  }

  /** Reads exactly lng of integers (4*lng bytes) from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no ints are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public int[] readInts(int [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min((end - pos) >> 2, left);
      if ((toCopy == 0) && (left > 0) && (end > pos)) {
        byte [] b = rescueRead(4, pos, end);
        array[off++] = XDR.getInt(b, 0);
        pos = this.pos;
        left --;
        continue;
      }
      while (toCopy > 0) {
        array[off++] = XDR.getInt(buffer,pos);
        pos += 4;
        left--;
        toCopy--;
      }
    }
    this.pos = pos;
    return array;
  }

  /** Reads exactly lng of floats (4*lng bytes) from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no floats are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public float[] readFloats(float [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min((end - pos) >> 2, left);
      if ((toCopy == 0) && (left > 0) && (end > pos)) {
        byte [] b = rescueRead(4, pos, end);
        array[off++] = XDR.getFloat(b, 0);
        pos = this.pos;
        left --;
        continue;
      }
      while (toCopy > 0) {
        array[off++] = XDR.getFloat(buffer,pos);
        pos += 4;
        left--;
        toCopy--;
      }
    }
    this.pos = pos;
    return array;
  }

  /** Reads exactly lng of longs (8*lng bytes) from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no longs are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public long[] readLongs(long [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min((end - pos) >> 3, left);
      if ((toCopy == 0) && (left > 0) && (end > pos)) {
        byte [] b = rescueRead(8, pos, end);
        array[off++] = XDR.getLong(b, 0);
        pos = this.pos;
        left --;
        continue;
      }
      while (toCopy > 0) {
        array[off++] = XDR.getLong(buffer,pos);
        pos += 8;
        left--;
        toCopy--;
      }
    }
    this.pos = pos;
    return array;
  }

  /** Reads exactly lng of doubles (8*lng bytes) from the stream and stores them in the array starting from off.
   *  <p>If array is null NullPointerException is thrown.</p>
   *  <p>If off is negative, or lng is negative, or off+lng is grater than array length then
   *  IndexOutOfBoundsException is thrown.</p>
   *  <p>If lng is 0 or array length is 0 then no doubles are read.</p>
   *  <p>If EOF is reached before the lng of bytes is read then EOFException is thrown.</p>
   *  @return an array of read bytes
   *  @throws IOException when I/O error occurs, or EOFException when EOF is reached during the reading.
   */

  public double[] readDoubles(double [] array, int off, int lng) throws IOException {
    if (lng == 0 || array.length == 0) {
      return array;
    } else if ( off < 0 || lng < 0 || off+lng > array.length) {
      throw new IndexOutOfBoundsException("Array size "+array.length+" trying to write at "+(off+lng));
    }
    int pos = this.pos;
    int left = lng;
    while (left > 0) {
      if (pos == end) {
        int read = refill();
        if (read == -1 && left > 0) {
          throw new EOFException ();
        }
        pos = this.pos;
      }
      int toCopy = Math.min((end - pos) >> 3, left);
      if ((toCopy == 0) && (left > 0) && (end > pos)) {
        byte [] b = rescueRead(8, pos, end);
        array[off++] = XDR.getDouble(b, 0);
        pos = this.pos;
        left --;
        continue;
      }
      while (toCopy > 0) {
        array[off++] = XDR.getDouble(buffer,pos);
        pos += 8;
        left--;
        toCopy--;
      }
    }
    this.pos = pos;
    return array;
  }

  /** Internal char buffer size */
  private final static int CHAR_BUFFER_SIZE = 1024;
  /** Internal char buffer */
  private final char [] cbuffer = new char[CHAR_BUFFER_SIZE];

  /** Reads a string in the UTF format from the stream.
   */

  public String readUTF() throws IOException {
    int lng = readUnsignedShort();
    return readStringUTFBytes(lng);
  }

  protected String readStringUTFBytes(int lng) throws IOException {
    byte [] bytes = new byte[lng];
    readBytes(bytes,0,lng);
    int cbufpos = 0;
    int b1,b2,b3;
    StringBuffer sbuf = new StringBuffer();
    for (int i=0;i<lng;) {
      b1 =  bytes[i++] & 0xFF;
      if ((b1 & 0x80) == 0) { //1byte char 0xxxxxxx
        cbuffer[cbufpos++] = (char) (b1 & 0xFF);
      } else if ((b1 & 0xe0) == 0xc0) { //2byte char 110xxxxx 10xxxxxx
        b2 = bytes[i++] & 0xFF;
        if ((b2 & 0xc0) != 0x80) {
          throw new UTFDataFormatException("Malformed byte format");
        }
        cbuffer[cbufpos++] = (char)(((b1 & 0x1F) << 6) | (b2 & 0x3F));
      } else if ((b1 & 0xf0) == 0xe0) { //3byte char 1110xxxx 10xxxxxx 10xxxxxx
        b2 = bytes[i++] & 0xFF;
        b3 = bytes[i++] & 0xFF;
        if (((b2 & 0xc0) != 0x80) || ((b3 & 0xc0) != 0x80)) {
          throw new UTFDataFormatException("Malformed byte format");
        }
        cbuffer[cbufpos++] = (char)(((b1 & 0x0F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F));
      } else {
        throw new UTFDataFormatException("Malformed byte format");
      }
      if (cbufpos == CHAR_BUFFER_SIZE) {
        sbuf.append(cbuffer);
        cbufpos = 0;
      }
    }
    sbuf.append(cbuffer,0,cbufpos);
    return sbuf.toString();
  }


  /** <p>Reads a string from the stream.</p>
   *  @deprecated Does not convert chars properly.
   */

  public String readLine()  throws IOException {
    if (pos == end) {
      int read = refill();
      if (read == -1) {
        return null;
      }
    }
    StringBuffer sbuf = new StringBuffer();
    int lineEnd;
    while(true) {
      lineEnd = searchEOL(buffer,pos,end);
      if (lineEnd != -1) {  //lineEnd found
        for (int i=0;i<lineEnd - pos;i++) { //copy bytes up to lineEnd-1
          cbuffer[i] = (char) (buffer[pos+i] & 0x00FF);
        }
        sbuf.append(cbuffer,0,lineEnd-pos);
        // discard also '\n' at buffer[lineEnd+1] if only buffer[lineEnd]=='\r'
        if (buffer[lineEnd] == 0x0d) {
          if (lineEnd == end - 1) { //last byte in the buffer -> have to check first one in new buffer
            int read = refill();
            if (read != -1) {
              if (buffer[0] == 0x0a) { //discard '\n' char
                pos = 1;
              }
            }
          } else { //not the last byte
            if (buffer[lineEnd+1] == 0x0a) {  //check next
              if (lineEnd + 2 > end) { //set pos correctly
                refill();
              } else {
                pos = lineEnd + 2;
              }
            }
          }
        }
        return sbuf.toString();
      }
      // else //lineEnd not yet found
      for (int i=0;i<end - pos;i++) { //copy whole available buffer
        cbuffer[i] = (char) (buffer[pos+i] & 0x00FF);
      }
      sbuf.append(cbuffer,0,end-pos);
      int read = refill();  //and refill to check new bytes
      if (read == -1) {
        return sbuf.toString(); //if EOF return collected bytes
      }
    }
  }

  private int searchEOL(byte [] bytes, int beg, int end) {
    for (;beg<end;beg++) {
      if ((bytes[beg] == 0x0d) || (bytes[beg] == 0x0a)) {
        return beg;
      }
    }
    return -1;
  }


  /** Reads an array of bytes from the stream into the array. This method blocks until bytes length of bytes is available.
   *  It calls readBytes(bytes, 0, bytes.length).
   *  @see #readBytes(byte[],int,int)
   */

  public void readFully(byte [] bytes)  throws IOException {
    readBytes(bytes,0,bytes.length);
  }

  /** Reads an array of bytes from the stream into the array. This method blocks until bytes lng of bytes is available.
   *  It calls readBytes(bytes, off, lng).
   *  @see #readBytes(byte[],int,int)
   */

  public void readFully(byte [] bytes, int off, int lng)  throws IOException {
    readBytes(bytes,off,lng);
  }

  private byte[] rescueRead(int count, int pos, int end) throws IOException {
    int i=0;
    byte [] b = new byte[count];
    while (true) {
      while (pos < end && i < count) {
        b[i++] = buffer[pos++];
      }
      if (pos == end) {
        int read = refill();
        if (read == -1) {
          throw new EOFException();
        }
        pos = this.pos;
        end = this.end;
      }
      if (i == count) {
        this.pos = pos;
        return b;
      }
    }
  }
}
