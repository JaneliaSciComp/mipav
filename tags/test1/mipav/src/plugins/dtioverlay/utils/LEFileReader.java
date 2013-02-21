package dtioverlay.utils;

import java.io.*;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Dec 6, 2005
 * Time: 5:03:56 PM
 * To change this template use Options | File Templates.
 */
public class LEFileReader {
    String readFilename;
    private BufferedInputStream  in;
    char[] cbuf;

    int offset;
    //private RandomAccessFile in;

    public LEFileReader(String filename) throws FileNotFoundException {
        //in = new RandomAccessFile(filename,"r");
        readFilename = filename;
        in = new BufferedInputStream (new FileInputStream(filename),1<<12);
        cbuf = new char[1];
        offset = 0;
    }

    public int readInt() throws IOException {
       int byte1, byte2, byte3, byte4;
       synchronized (this) {
         byte1 = readByte();
         byte2 = readByte();
         byte3 = readByte();
         byte4 = readByte();
       }
       if (byte4 == -1) {
         throw new EOFException();
       }
       return (byte4 << 24)
        + ((byte3 << 24) >>> 8)
        + ((byte2 << 24) >>> 16)
        + ((byte1 << 24) >>> 24);
     }

    public short readShort() throws IOException {
       int byte1, byte2;
       synchronized (this) {
         byte1 = readByte();
         byte2 = readByte();
       }
       if (byte2 == -1) {
         throw new EOFException();
       }
       return (short)(((byte2 << 24) >>> 16)
        + ((byte1 << 24) >>> 24));
     }

    public float readFloat() throws IOException {
        return Float.intBitsToFloat(readInt());
    }

    public int readByte() throws IOException {
        int c = in.read(); //in.read(cbuf,0,1);
        if(c<0) {
            c = (127-c);
        }
        if(c>255)
             c=255;
        offset++;
        return c;
    }

    public void seek(int newOffset) throws IOException {
        if(newOffset>offset) {
            while(newOffset>offset)
                readByte();
        } else {
            in.close();
            offset =0;
             in = new BufferedInputStream (new FileInputStream(this.readFilename),1<<16);
            seek(newOffset);
        }
        //in.seek(offset);
    }

}
