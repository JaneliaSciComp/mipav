package gov.nih.mipav.model.file.rawjp2;

//import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import jj2000.j2k.fileformat.*;
//import jj2000.j2k.io.RandomAccessIO;

/**
 * Big Endian Output Array Stream, works as a replacement for FileFormatWriter 
 * at the output of Jpeg2K encoder.  
 * @author Dzung Nguyen
 * @see ByteArrayOutputStream
 * @see FileFormatBoxes
 */
public class BEByteArrayOutputStream extends ByteArrayOutputStream  
	implements FileFormatBoxes{
 
	/** Image height */
    private int height;

    /** Image width */
    private int width;
	/** Length of codestream */
    private int clength;

    /** Length of Colour Specification Box */
    private static final int CSB_LENGTH = 15;

    /** Length of File Type Box */
    private static final int FTB_LENGTH = 20;

    /** Length of Image Header Box */
    private static final int IHB_LENGTH = 22;

public BEByteArrayOutputStream() {
	super();
}
public BEByteArrayOutputStream(ByteArrayOutputStream bs, int w, int h) throws IOException{
	super();
	clength = bs.size();
	height = h;
	width = w;
	bs.writeTo(this);
}

public void writeInt(int b) {
	write(b>>>24);
 	write(b>>>16);
	write(b>>>8);
	write(b);

}
public void writeShortInt(int b) {
	write(b>>>8);
	write(b);
}
/** 
 * This method reads the codestream and writes the file format wrapper and
 * the codestream to the same file
 *
 * @return The number of bytes increases because of the file format
 *
 * @exception java.io.IOException If an I/O error ocurred.
 * */
public int writeFileFormat() throws IOException {
    byte[] codestream;

    try{
        // Read and buffer the codestream
//        fi = new BEBufferedRandomAccessFile(filename,"rw+");
//       codestream= new byte[clength];
//        fi.readFully(codestream, 0, clength);

    	codestream = this.toByteArray(); 
    	this.reset();
    	
        // Write the JP2_SINATURE_BOX
//        fi.seek(0);
        writeInt(0x0000000c);
        writeInt(JP2_SIGNATURE_BOX);
        writeInt(0x0d0a870a);
        
        // Write File Type box
        writeFileTypeBox();
        
        // Write JP2 Header box
        writeJP2HeaderBox();

        // Write the Codestream box 
        writeContiguousCodeStreamBox(codestream);

        close();

    }
    catch(Exception e){
        throw new Error("Error while writing JP2 file format");
    }
//    if(bpcVaries)
//        return 12+FTB_LENGTH+8+IHB_LENGTH+CSB_LENGTH+BPC_LENGTH+nc+8;
//    else
        return 12+FTB_LENGTH+8+IHB_LENGTH+CSB_LENGTH+8;

}

/** 
 * This method writes the File Type box
 *
 * @exception java.io.IOException If an I/O error ocurred.
 * */
public void writeFileTypeBox()throws IOException {
    // Write box length (LBox)
    // LBox(4) + TBox (4) + BR(4) + MinV(4) + CL(4) = 20
    writeInt(FTB_LENGTH);

    // Write File Type box (TBox)
    writeInt(FILE_TYPE_BOX);

    // Write File Type data (DBox)
    // Write Brand box (BR)
    writeInt(FT_BR);

    // Write Minor Version
    writeInt(0);
    
    // Write Compatibility list
    writeInt(FT_BR);
    
}
/** 
 * This method writes the JP2Header box
 *
 * @exception java.io.IOException If an I/O error ocurred.
 * */
public void writeJP2HeaderBox()throws IOException {

    // Write box length (LBox)
    // if the number of bits per components varies, a bpcc box is written
//    if(bpcVaries)
//        fi.writeInt(8+IHB_LENGTH+CSB_LENGTH+BPC_LENGTH+nc);
//    else
//        fi.writeInt(8+IHB_LENGTH+CSB_LENGTH);
    writeInt(8+IHB_LENGTH+CSB_LENGTH);
    
    // Write a JP2Header (TBox)
    writeInt(JP2_HEADER_BOX);

    // Write image header box 
    writeImageHeaderBox();

    // Write Colour Bpecification Box
    writeColourSpecificationBox();

    // if the number of bits per components varies write bpcc box
//    if(bpcVaries)
//        writeBitsPerComponentBox();
}

/** 
 * This method writes the Bits Per Component box
 *
 * @exception java.io.IOException If an I/O error ocurred.
 *
 */

/** 
 * This method writes the Colour Specification box
 *
 * @exception java.io.IOException If an I/O error ocurred.
 *
 */
public void writeColourSpecificationBox()throws IOException {
	int nc = 1;
    // Write box length (LBox)
    writeInt(CSB_LENGTH);

    // Write a Bits Per Component box (TBox)
    writeInt(COLOUR_SPECIFICATION_BOX);

    // Write METH field
    write(CSB_METH);

    // Write PREC field
    write(CSB_PREC);

    // Write APPROX field
    write(CSB_APPROX);

    // Write EnumCS field
    if(nc>1)
        writeInt(CSB_ENUM_SRGB);
    else
        writeInt(CSB_ENUM_GREY);       
}

/** 
 * This method writes the Image Header box
 *
 * @exception java.io.IOException If an I/O error ocurred.
 * */
public void writeImageHeaderBox()throws IOException {

    // Write box length
    writeInt(IHB_LENGTH);

    // Write ihdr box name
    writeInt(IMAGE_HEADER_BOX);

    // Write HEIGHT field
    writeInt(height);

    // Write WIDTH field
    writeInt(width);

    // Write NC field
    writeShortInt(1);

    // Write BPC field
    // if the number of bits per component varies write 0xff else write
    // number of bits per components
//    if(bpcVaries)
//        write(0xff);
//    else
//        write(bpc[0]-1);
    write(16-1);
    
    // Write C field
    write(IMB_C);

    // Write UnkC field
    write(IMB_UnkC);

    // Write IPR field
    write(IMB_IPR);

}

/** 
 * This method writes the Contiguous codestream box
 *
 * @param cs The contiguous codestream
 *
 * @exception java.io.IOException If an I/O error ocurred.
 * */
public void writeContiguousCodeStreamBox(byte[] cs)throws IOException {

    // Write box length (LBox)
    // This value is set to 0 since in this implementation, this box is
    // always last
    writeInt(clength+8);

    // Write contiguous codestream box name (TBox)
    writeInt(CONTIGUOUS_CODESTREAM_BOX);
            
    // Write codestream
//    for(int i=0; i<clength ;i++)
//        write(cs[i]);
    write(cs,0, cs.length);
}      

}
