package gov.nih.mipav.model.file.rawjp2;

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.IOException;

import jj2000.j2k.io.RandomAccessIO;
/**
 * Big Endian Input Array Stream, works as a replacement for BERandomAccessFile 
 * at the input of Jpeg2K decoder.  
 * @author Dzung Nguyen
 * @see RandomAccessIO
 * @see ByteArrayInputStream
 */
public class BEByteArrayInputStream extends ByteArrayInputStream
		implements RandomAccessIO {
	// Our JP2 format is Big Endian
	
	public BEByteArrayInputStream(byte[] buf, int offset, int length) {
		super(buf, offset, length);
		// TODO Auto-generated constructor stub
	}

	public BEByteArrayInputStream(byte[] buf) {
		super(buf);
		// TODO Auto-generated constructor stub
	}

	public int getPos() throws IOException {
		return pos;
	}

	public int length() throws IOException {
		return buf.length;
	}

	public void readFully(byte[] b, int off, int len) throws IOException {
		//System.arraycopy( b, off, buf, pos, len );
		//pos += len;
		for(int i=0;i<len;i++) b[off+i] = readByte();

	}

	public void seek(int off) throws IOException {
		if ((off<0)||(off>buf.length)) throw 
					new IllegalArgumentException("Index out of range!");
		pos = off;

	}

	public void write(int b) throws IOException {
		// TODO Auto-generated method stub

	}

	public int getByteOrdering() {
		return 0; // = BIG-ENDIAN 
	}

	public byte readByte() throws EOFException, IOException {
        return buf[pos++];
	}

	public double readDouble() throws EOFException, IOException {
		return Double.longBitsToDouble( ((long)read()<<56)|
                ((long)read()<<48)|
                ((long)read()<<40)|
                ((long)read()<<32)|
                ((long)read()<<24)|
                ((long)read()<<16)|
                ((long)read()<<8)|
                ((long)read()) );
	}

	public float readFloat() throws EOFException, IOException {
		return Float.intBitsToFloat( (read()<<24) | (read()<<16)|
                (read()<<8) | (read()) );
	}

	public int readInt() throws EOFException, IOException {
		return ( (read()<<24) | (read()<<16) | (read()<<8) | read() );
	}

	public long readLong() throws EOFException, IOException {
		return ( ((long)read()<<56) | ((long)read()<<48) | ((long)read()<<40)|
				((long)read()<<32) | ((long)read()<<24) | ((long)read()<<16)|
				((long)read()<<8) | ((long)read()) );
	}

	public short readShort() throws EOFException, IOException {
		return (short)( (read()<<8) | (read()) );
	}

	public int readUnsignedByte() throws EOFException, IOException {
        return read();
	}

	public long readUnsignedInt() throws EOFException, IOException {
		return (long)( (read()<<24) | (read()<<16) | (read()<<8) | read() );
	}

	public int readUnsignedShort() throws EOFException, IOException {
		return ( (read()<<8) | read() );
	}

	public int skipBytes(int n) throws EOFException, IOException {
			if(n<0)
			    throw new IllegalArgumentException("Can not skip negative number of bytes");
			//skip(n);
			pos+=n;
			return n;
	//		}

	}

	public void flush() throws IOException {
		// TODO Auto-generated method stub

	}

	public void writeByte(int v) throws IOException {
		// TODO Auto-generated method stub

	}

	public void writeDouble(double v) throws IOException {
		// TODO Auto-generated method stub

	}

	public void writeFloat(float v) throws IOException {
		// TODO Auto-generated method stub

	}

	public void writeInt(int v) throws IOException {
		// TODO Auto-generated method stub

	}

	public void writeLong(long v) throws IOException {
		// TODO Auto-generated method stub

	}

	public void writeShort(int v) throws IOException {
		// TODO Auto-generated method stub

	}

}
