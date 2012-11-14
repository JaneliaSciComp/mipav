package gov.nih.mipav.model.file.rawjp2;

import jj2000.j2k.image.*;
import jj2000.j2k.image.input.*;
import jj2000.j2k.*;

import java.io.*;

import gov.nih.mipav.model.structures.*;

/**
 * This class implements the ImgData interface for reading slices of 16 bit unsigned data
 * from a 3D RAW image file. 
 *
 * <p>After being read the coefficients are level shifted by subtracting
 * 2^(nominal bit range-1)</p>
 *
 * <p>The TransferType (see ImgData) of this class is TYPE_INT.</p>
 *
 * <P>NOTE: This class is not thread safe, for reasons of internal buffering.
 *
 * @see jj2000.j2k.image.ImgData
 * */
public class ImgReaderRAWSlice extends ImgReader {

    /** DC offset value used when reading image */
    public static int DC_OFFSET = 0;

    /** Where to read the data from */
    protected RandomAccessFile in;
    
    /** The offset of the raw pixel data in the RAW file */
    private int offset;
    
    /** image store*/
	protected ModelImage image;    

    /** The number of bits that determine the nominal dynamic range */
    protected int rb;
    
    /** The line buffer. */
    // This makes the class not thrad safe
    // (but it is not the only one making it so)
    private byte buf[];

    /** Temporary DataBlkInt object (needed when encoder uses floating-point
        filters). This avoid allocating new DataBlk at each time */
    protected DataBlkInt intBlk;
    
    protected boolean useModImg = false;
    
    protected String fName;
    
    @SuppressWarnings("unused")
    private boolean savingAsEncJP2 = false;
    
    private int blah = 0;

    /**
     * Creates a new PGM file reader from the specified file.
     *
     * @param file The input file.
     *
     * @exception IOException If an error occurs while opening the file.
     * */
    public ImgReaderRAWSlice(File file, int si) throws IOException {
        this(new RandomAccessFile(file,"r"), si);
        fName = file.getPath() + "/" + file.getName();
    }

    /**
     * Creates a new PGM file reader from the specified file name.
     *
     * @param fname The input file name.
     *
     * @exception IOException If an error occurs while opening the file.
     * */
    public ImgReaderRAWSlice(String fname, int si) throws IOException {
    	this(new RandomAccessFile(fname.substring(0,fname.lastIndexOf('.')) + ".raw","r"), si);
    	fName = fname.substring(0,fname.lastIndexOf('.')) + ".raw";
  	
    }

    /**
     * Creates a new PGM file reader from the specified RandomAccessFile
     * object. The file header is read to acquire the image size.
     *
     * @param in From where to read the data 
     *
     * @exception EOFException if an EOF is read
     * @exception IOException if an error occurs when opening the file
     * */
    public ImgReaderRAWSlice(RandomAccessFile in, int si) throws EOFException, IOException {
        this.in = in;
        this.w = 256;
        this.h = 256;
/*
        confirmFileType();
        skipCommentAndWhiteSpace();
        this.w = readHeaderInt();
        skipCommentAndWhiteSpace();
        this.h = readHeaderInt();
        skipCommentAndWhiteSpace();
        //Read the highest pixel value from header (not used)
        readHeaderInt();
*/
        this.nc=1;
        this.rb=16;
		this.image = null;
		
        this.offset = si * (rb>>3)* w * h;
        if(!isOrigSigned(0)){
        	DC_OFFSET = 1 << (rb-1);
        } else {
        	DC_OFFSET = 0;
        }
        this.useModImg = false;        
    }
                
    public ImgReaderRAWSlice(ModelImage image, int si, boolean savingAsEncJP2){
    	int[] imgExtents;
		
		this.image = image;
		imgExtents = image.getExtents();
		this.savingAsEncJP2 = savingAsEncJP2;
		
		this.w = imgExtents[0];
		this.h = imgExtents[1];
		//if(image.isColorImage()) {
		//		this.w = this.w * 4;
		//}
//		this.numlice = imgExtents[2];
		this.nc=1;
        this.rb=16;
        
		//this.nc=3;//testing for color
        //this.rb=8;//testing for color
        
        //this.offset = si * (rb>>3)* w * h;
        this.offset = si * w * h;
        if(!isOrigSigned(0)){
        	DC_OFFSET = 1 << (rb-1);
        } else {
        	DC_OFFSET = 0;
        }
        this.useModImg = true;
//        JOptionPane.showMessageDialog(null, "ImgReaderRAWSlice OK", "Info", JOptionPane.INFORMATION_MESSAGE);
    }
    /**
     * Closes the underlying RandomAccessFile from where the image data is
     * being read. No operations are possible after a call to this method.
     *
     * @exception IOException If an I/O error occurs.
     * */
    public void close() throws IOException {
        if (useModImg) return;
    	in.close();
        in = null;
    }

    /**
     * Returns the number of bits corresponding to the nominal range of the
     * data in the specified component. This is the value rb (range bits) that
     * was specified in the constructor, which normally is 8 for non bilevel
     * data, and 1 for bilevel data.
     *
     * <P>If this number is <i>b</b> then the nominal range is between
     * -2^(b-1) and 2^(b-1)-1, since unsigned data is level shifted to have a
     * nominal average of 0.
     *
     * @param c The index of the component.
     *
     * @return The number of bits corresponding to the nominal range of the
     * data. Fro floating-point data this value is not applicable and the
     * return value is undefined.
     * */
    public int getNomRangeBits(int c) {
        // Check component index
//        if (c != 0)
//            throw new IllegalArgumentException();

        return rb;
    }

    
    /**
     * Returns the position of the fixed point in the specified component
     * (i.e. the number of fractional bits), which is always 0 for this
     * ImgReader.
     *
     * @param c The index of the component.
     *
     * @return The position of the fixed-point (i.e. the number of fractional
     * bits). Always 0 for this ImgReader.
     * */
    public int getFixedPoint(int c) {
        // Check component index
//        if (c != 0)
//            throw new IllegalArgumentException();
        return 0;
    }
  
    
    /**
     * Returns, in the blk argument, the block of image data containing the
     * specifed rectangular area, in the specified component. The data is
     * returned, as a reference to the internal data, if any, instead of as a
     * copy, therefore the returned data should not be modified.
     *
     * <P> After being read the coefficients are level shifted by subtracting
     * 2^(nominal bit range - 1)
     *
     * <P>The rectangular area to return is specified by the 'ulx', 'uly', 'w'
     * and 'h' members of the 'blk' argument, relative to the current
     * tile. These members are not modified by this method. The 'offset' and
     * 'scanw' of the returned data can be arbitrary. See the 'DataBlk' class.
     *
     * <P>If the data array in <tt>blk</tt> is <tt>null</tt>, then a new one
     * is created if necessary. The implementation of this interface may
     * choose to return the same array or a new one, depending on what is more
     * efficient. Therefore, the data array in <tt>blk</tt> prior to the
     * method call should not be considered to contain the returned data, a
     * new array may have been created. Instead, get the array from
     * <tt>blk</tt> after the method has returned.
     *
     * <P>The returned data always has its 'progressive' attribute unset
     * (i.e. false).
     *
     * <P>When an I/O exception is encountered the JJ2KExceptionHandler is
     * used. The exception is passed to its handleException method. The action
     * that is taken depends on the action that has been registered in
     * JJ2KExceptionHandler. See JJ2KExceptionHandler for details.
     *
     * @param blk Its coordinates and dimensions specify the area to
     * return. Some fields in this object are modified to return the data.
     *
     * @param c The index of the component from which to get the data. Only 0
     * is valid.
     *
     * @return The requested DataBlk
     *
     * @see #getCompData
     *
     * @see JJ2KExceptionHandler
     * */
    public DataBlk getInternCompData(DataBlk blk, int c) {
    	blah++;
        int k,j,i,mi;
        int barr[];

 
        // Check component index
        if (c != 0)
            throw new IllegalArgumentException();

        
	// Check type of block provided as an argument
	if(blk.getDataType()!=DataBlk.TYPE_INT){

	    if(intBlk==null)
		intBlk = new DataBlkInt(blk.ulx,blk.uly,blk.w,blk.h);
	    else{
		intBlk.ulx = blk.ulx;
		intBlk.uly = blk.uly;
		intBlk.w = blk.w;
		intBlk.h = blk.h;
	    }
	    blk = intBlk;
	}

	// Get data array
	barr = (int[]) blk.getData();

	if (barr == null || barr.length < blk.w*blk.h) {

		//if(image.isColorImage()) { //color test...added if by NISH
		//	barr = new int[blk.w*blk.h*3];
		//}else {
		//	barr = new int[blk.w*blk.h];
		//}
		barr = new int[blk.w*blk.h];
	    blk.setData(barr); 
	}
       
        // Check line buffer
        if (buf == null || buf.length < blk.w) {
            buf = new byte[2*blk.w];
        }
        int[] linebuf = new int[blk.w];
        int lineOffset=0;
        if (useModImg) {
            try{
            	// Read line by line
            	mi = blk.uly + blk.h;
                for (i = blk.uly; i < mi; i++) {
     //           	image.exportData(offset + i*w + blk.ulx, blk.w*blk.h, barr);
                	image.exportData(offset + i*w + blk.ulx, blk.w, linebuf);
                	lineOffset = (i-blk.uly)*w;
                	for(j=0;j<blk.w;j++) barr[lineOffset + j]=linebuf[j]; 
                }
                //color testing---added by NiSH
                //if(image.isColorImage()) {

                	//int rgbSize = (barr.length/4)*3;
                	//int[] barrRGB = new int[rgbSize];
                	//int v=0;
                	//for(int z=0;z<barr.length;z=z+4) {
                	//	barrRGB[v] = barr[z+1];
                	//	barrRGB[v+1] = barr[z+2];
                	//	barrRGB[v+2] = barr[z+3];
                	//	v = v+3;
                	//}
                	//barr = null;
                	//barr = barrRGB;
 
                	//barrRGB = null;
                //}
            } catch (IOException e) {
                JJ2KExceptionHandler.handleException(e);
            }
            // Turn off the progressive attribute
            blk.progressive = false;
            // Set buffer attributes
            blk.offset = 0;//offset;
            offset = offset - 2 * c * w*h;
            blk.scanw = blk.w;
            return blk;
        }
        
        //locate the correct layer/component
   //     offset = offset + 2 * c * w*h;
        try {
            
            mi = blk.uly + blk.h;
            
            for (i = blk.uly; i < mi; i++) {
                // Reposition in input
                in.seek( offset+ 2*i*w+blk.ulx); // Short.SIZE = 2 bytes
                in.read(buf,0,2*blk.w);
                for (k = (i-blk.uly)*blk.w+blk.w-1, j = 2*blk.w-1;
                     j >= 0; j -=2, k--) {
                    //barr[k] = (((int)buf[j])&0xFF)-DC_OFFSET;
                    
                	// good: barr[k] = ((((int)buf[j])&0xFF)*256) + ((int)buf[j-1]&0xFF) -DC_OFFSET;
                	barr[k] = (short)(((buf[j]&0xFF)<<8) + (buf[j-1]&0xFF) -DC_OFFSET);
                }
            }
//            if (useModImg) {
//                for (i = blk.uly; i < mi; i++) {
//                	image.exportData(offset + i*w + blk.ulx, blk.w*blk.h, barr);
//                }
//            }
        }
        catch (IOException e) {
            JJ2KExceptionHandler.handleException(e);
        }
        
        // Turn off the progressive attribute
        blk.progressive = false;
        // Set buffer attributes
        blk.offset = 0;//offset;
        offset = offset - 2 * c * w*h;
        blk.scanw = blk.w;
	return blk;
    }

    /**
     * Returns, in the blk argument, a block of image data containing the
     * specifed rectangular area, in the specified component. The data is
     * returned, as a copy of the internal data, therefore the returned data
     * can be modified "in place".
     *
     * <P> After being read the coefficients are level shifted by subtracting
     * 2^(nominal bit range - 1)
     *
     * <P>The rectangular area to return is specified by the 'ulx', 'uly', 'w'
     * and 'h' members of the 'blk' argument, relative to the current
     * tile. These members are not modified by this method. The 'offset' of
     * the returned data is 0, and the 'scanw' is the same as the block's
     * width. See the 'DataBlk' class.
     *
     * <P>If the data array in 'blk' is 'null', then a new one is created. If
     * the data array is not 'null' then it is reused, and it must be large
     * enough to contain the block's data. Otherwise an 'ArrayStoreException'
     * or an 'IndexOutOfBoundsException' is thrown by the Java system.
     *
     * <P>The returned data has its 'progressive' attribute unset
     * (i.e. false).
     *
     * <P>This method just calls 'getInternCompData(blk, n)'.
     *
     * <P>When an I/O exception is encountered the JJ2KExceptionHandler is
     * used. The exception is passed to its handleException method. The action
     * that is taken depends on the action that has been registered in
     * JJ2KExceptionHandler. See JJ2KExceptionHandler for details.
     *
     * @param blk Its coordinates and dimensions specify the area to
     * return. If it contains a non-null data array, then it must have the
     * correct dimensions. If it contains a null data array a new one is
     * created. The fields in this object are modified to return the data.
     *
     * @param c The index of the component from which to get the data. Only 0
     * is valid.
     *
     * @return The requested DataBlk
     *
     * @see #getInternCompData
     *
     * @see JJ2KExceptionHandler
     * */
    public DataBlk getCompData(DataBlk blk, int c) {
        return getInternCompData(blk,c);
    }

    public void setSliceIndex(int si, boolean useModImg) throws IOException{
    	if (useModImg) {
            this.offset = si * w * h;   
            this.useModImg = true;
    	} else {
            this.offset = si * (rb>>3)* w * h;
            in = new RandomAccessFile(fName,"r");
    	}
    	
    }
    /**
     * Returns true if the data read was originally signed in the specified
     * component, false if not. This method returns always false since PGM
     * data is always unsigned.
     *
     * @param c The index of the component, from 0 to N-1.
     *
     * @return always false, since PGM data is always unsigned.
     * */
    public boolean isOrigSigned(int c) {
        // Check component index
//        if (c != 0)
//            throw new IllegalArgumentException();
        return false;
    }

    /**
     * Returns a string of information about the object, more than 1 line
     * long. The information string includes information from the underlying
     * RandomAccessIO (its toString() method is called in turn).
     *
     * @return A string of information about the object.  
     * */
    public String toString() {
        return "ImgReaderPGM: WxH = " + w + "x" + h + ", Component = 0" +
            "\nUnderlying RandomAccessIO:\n" + in.toString();
    }
}
