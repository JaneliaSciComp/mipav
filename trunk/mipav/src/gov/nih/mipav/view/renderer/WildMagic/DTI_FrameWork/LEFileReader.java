package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import java.io.BufferedInputStream;
import java.io.EOFException;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

// TODO: Auto-generated Javadoc
/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Dec 6, 2005
 * Time: 5:03:56 PM
 * To change this template use Options | File Templates.
 */
public class LEFileReader {
    
    /** The read filename. */
    String readFilename;
    
    /** The in. */
    private BufferedInputStream  in;
    
    /** The cbuf. */
    char[] cbuf;

    /** The offset. */
    int offset;
    //private RandomAccessFile in;

    /**
     * Instantiates a new lE file reader.
     * 
     * @param filename the filename
     * 
     * @throws FileNotFoundException the file not found exception
     */
    public LEFileReader(String filename) throws FileNotFoundException {
        //in = new RandomAccessFile(filename,"r");
        readFilename = filename;
        in = new BufferedInputStream (new FileInputStream(filename),1<<12);
        cbuf = new char[1];
        offset = 0;
    }

    /**
     * Read int.
     * 
     * @return the int
     * 
     * @throws IOException Signals that an I/O exception has occurred.
     */
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

    /**
     * Read short.
     * 
     * @return the short
     * 
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public short readShort() throws IOException {
       int byte1, byte2, byte3, byte4;
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

    /**
     * Read float.
     * 
     * @return the float
     * 
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public float readFloat() throws IOException {
        return Float.intBitsToFloat(readInt());
    }

    /**
     * Read byte.
     * 
     * @return the int
     * 
     * @throws IOException Signals that an I/O exception has occurred.
     */
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

    /**
     * Seek.
     * 
     * @param newOffset the new offset
     * 
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public void seek(int newOffset) throws IOException {
    	int thisbyte = 100;
    	//System.out.println("jist.io"+"\t"+"At Start, offset is:" + offset);
        if(newOffset>offset) {
        	//int i=1;
        	while(newOffset>offset){
        		thisbyte = readByte();
        		//System.out.println("jist.io"+"\t"+"******** Reading " +  thisbyte + " for offset ********");
        	}
        } else {
            in.close();
            offset =0;
             in = new BufferedInputStream (new FileInputStream(this.readFilename),1<<16);
            seek(newOffset);
        }
        //in.seek(offset);
    }

}
