package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;


// TODO: Auto-generated Javadoc
/**
 * The Class LEFileWriter.
 */
public class LEFileWriter {
	
	/** The wfilename. */
	String wfilename;
	
	/** The out. */
	private DataOutputStream out;
	
	/** The temp. */
	private byte[] temp;
	
	/** The offset. */
	int offset;
	//IM INCREMENTING OFFSET INCORRECTLY IN THE METHODS...ARGH!
	
	/**
	 * Instantiates a new lE file writer.
	 * 
	 * @param filename the filename
	 * 
	 * @throws FileNotFoundException the file not found exception
	 */
	public LEFileWriter(String filename) throws FileNotFoundException{
		wfilename = filename;
		out = new DataOutputStream(new FileOutputStream(filename));
		temp = new byte[8];
		offset=0;
	}
	
	/**
	 * Write byte.
	 * 
	 * @param i the i
	 * 
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public void writeByte(int i) throws IOException{ out.writeByte(i); offset++;}
	
	/**
	 * Write bytes.
	 * 
	 * @param s the s
	 * 
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public void writeBytes(String s) throws IOException{ 
		out.writeBytes(s); 
		offset+=s.length();
	}
	
	/**
	 * Write char.
	 * 
	 * @param c the c
	 * 
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public void writeChar(int c) throws IOException{ out.writeChar(c); offset+=2; }
	
	/**
	 * Close.
	 * 
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public void close() throws IOException{ out.close(); }
	
	/**
	 * Write int.
	 * 
	 * @param v the v
	 * 
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public void writeInt(int v) throws IOException{
		 	temp[0]=(byte)v;
	        temp[1]=(byte)(v>>8);
	        temp[2]=(byte)(v>>16);
	        temp[3]=(byte)(v>>24);
	        out.write(temp,0,4 );
	        offset+=4;
	}
	
	/**
	 * Write float.
	 * 
	 * @param v the v
	 * 
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public void writeFloat(float v) throws IOException{
		writeInt(Float.floatToIntBits(v));
		offset+=4;
	}
	
    /**
     * Seek.
     * 
     * @param newOffset the new offset
     * 
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public void seek(int newOffset) throws IOException {
    	offset = out.size();
        if(newOffset>offset) {
//        	int i = 1;
        	while(newOffset>offset){
        		writeByte(0);
//        		System.out.println("jist.io"+"\t"+i + " bytes written");
//        		i++;
        	}
        } else {
            out.close();
            offset=0;
            out = new DataOutputStream(new FileOutputStream(wfilename));
            seek(newOffset);
        }
    }


}
