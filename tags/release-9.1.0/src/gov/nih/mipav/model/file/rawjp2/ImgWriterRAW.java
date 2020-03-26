package gov.nih.mipav.model.file.rawjp2;


import gov.nih.mipav.model.structures.*;
import java.io.*;

import jj2000.j2k.image.*;
import jj2000.j2k.image.output.*;

/**
 * This class extends the ImgWriter abstract class for writing JP2 3D files. 
 * Each slice of the 3D image is written to the current offset in the ouput file.
 * The class can take input data in ModelImage structure or BlkImgDataSrc (which is 
 * prefered in JJ2K context :-) 
 * 
 * <u>Data:</u> The image binary values appear one after the other (in raster
 * order) immediately after the last header character ('\n') and are
 * byte-aligned (they are packed into 1,2 or 4 bytes per sample, depending
 * upon the bit-depth value).
 * </p>
 *
 * <p> If the data is unsigned, level shifting is applied adding 2^(bit depth
 * - 1)</p>
 *
 * <p><u>NOTE</u>: This class is not thread safe, for reasons of internal
 * buffering.</p>
 *
 * @see ImgWriter
 *
 * @see BlkImgDataSrc
 * */
public class ImgWriterRAW extends ImgWriter {

    /** Used during saturation (2^bitdepth-1 if unsigned, 2^(bitdepth-1)-1 if
     * signed)*/
    int maxVal;

    /** Used during saturation (0 if unsigned, -2^(bitdepth-1) if signed) */
    int minVal;

    /** Used with level-shifting */
    int levShift;

    /** Whether the data must be signed when writing or not. In the latter
     * case inverse level shifting must be applied */
    boolean isSigned;

    /** The bit-depth of the input file (must be between 1 and 31)*/
    protected int bitDepth;

    /** Where to write the data */
    private RandomAccessFile out;
    
    /** The current offset of the raw pixel data in the RAW file */
    protected int offset;

    /** The current offset of the raw pixel data in the RAW file */
    protected int modImgOffset;

    /** A DataBlk, just used to avoid allocating a new one each time it is
        needed */
    protected DataBlkInt db = new DataBlkInt();

    /** The number of fractional bits in the source data */
    private int fb;

    /** The index of the component from where to get the data */
    protected int c;
    /** The number of layers/components */
    protected int nc;

    /** The pack length of one sample (in bytes, according to the output
        bit-depth */
    protected int packBytes;

    /** The line buffer. */
    // This makes the class not thrad safe
    // (but it is not the only one making it so)
    private byte buf[];
    
    /** Model image pointer*/
    protected ModelImage modImg = null;
    
    protected int imgType;
    /**
     * Creates a new writer to the specified File object, to write data from
     * the specified component.
     *
     * <p>The size of the image that is written to the file is the size of the
     * component from which to get the data, specified by b, not the size of
     * the source image (they differ if there is some sub-sampling).</p>
     *
     * <p>All the header informations are given by the BlkImgDataSrc source
     * (component width, component height, bit-depth) and sign flag, which are
     * provided to the constructor. The endianness is always big-endian (MSB
     * first).</p>
     *
     * @param out The file where to write the data
     *
     * @param imgSrc The source from where to get the image data to write.
     *
     * @param c The index of the component from where to get the data.
     *
     * @param isSigned Whether the datas are signed or not (needed only when
     * writing header).
     *
     * @see DataBlk
     * */
    public ImgWriterRAW(File out, BlkImgDataSrc imgSrc, 
			int c, boolean isSigned) throws IOException {
        //Initialize
        this.nc = 1;
    	this.c = c;
        if(out.exists() && !out.delete()) {
            throw new IOException("Could not reset file");
        }
        this.out = new RandomAccessFile(out,"rw");
        this.isSigned = isSigned;
        src = imgSrc;
        w = src.getImgWidth();
        h = src.getImgHeight();
        fb = imgSrc.getFixedPoint(c);
        
        bitDepth = src.getNomRangeBits(c);
        if((bitDepth<=0)||(bitDepth>31)) {
            throw new IOException("RAW supports only bit-depth between "+
                                  "1 and 31");
	}
        if(bitDepth<=8) {
            packBytes = 1;
        } else if(bitDepth<=16) {
            packBytes = 2;
        } else { // <= 31
            packBytes = 4;
	}

        offset = 0;
        
        maxVal = this.isSigned ? (( 1<<(src.getNomRangeBits(c)-1) )-1):
            ((1<<src.getNomRangeBits(c))-1);
        minVal = this.isSigned ? (-1 * ( 1<<(src.getNomRangeBits(c)-1) )) : 0;
            
        levShift = (this.isSigned) ? 0 : 1<<(src.getNomRangeBits(c)-1);
    }
    public ImgWriterRAW(ModelImage mi, BlkImgDataSrc imgSrc, boolean isSigned) throws IOException{
    	this.nc = 1;
    	this.modImg = mi;
    	
    	this.c = 0;
   
        this.isSigned = isSigned;
        src = imgSrc;
        w = src.getImgWidth();
        h = src.getImgHeight();
        fb = imgSrc.getFixedPoint(0);
        
        bitDepth = src.getNomRangeBits(c);
        
        if(bitDepth<=8) {
            packBytes = 1;
        } else if(bitDepth<=16) {
            packBytes = 2;
        } else { // <= 31
            packBytes = 4;
	}

        offset = 0;
        modImgOffset=0;
        
        maxVal = this.isSigned ? (( 1<<(src.getNomRangeBits(c)-1) )-1):
            ((1<<src.getNomRangeBits(c))-1);
        minVal = this.isSigned ? (-1 * ( 1<<(src.getNomRangeBits(c)-1) )) : 0;
            
        levShift = (this.isSigned) ? 0 : 1<<(src.getNomRangeBits(c)-1);
  	
    }

    /**
     * Creates a new writer to the specified file, to write data from the
     * specified component.
     *
     * <p>The size of the image that is written to the file is the size of the
     * component from which to get the data, specified by b, not the size of
     * the source image (they differ if there is some sub-sampling).</p>
     *
     * <p>All header information is given by the BlkImgDataSrc source
     * (component width, component height, bit-depth) and sign flag, which are
     * provided to the constructor. The endianness is always big-endian (MSB
     * first).
     *
     * @param fname The name of the file where to write the data
     *
     * @param imgSrc The source from where to get the image data to write.
     *
     * @param c The index of the component from where to get the data.
     *
     * @param isSigned Whether the datas are signed or not (needed only when
     * writing header).
     *
     * @see DataBlk
     * */
    public ImgWriterRAW(String fname, BlkImgDataSrc imgSrc, 
			int c, boolean isSigned) throws IOException{
        this(new File(fname),imgSrc,c,isSigned);
    }
              
    /**
     * Closes the underlying file or netwrok connection to where the data is
     * written. Any call to other methods of the class become illegal after a
     * call to this one.
     *
     * @exception IOException If an I/O error occurs.
     * */
    public void close() throws IOException {
        int i;
        // Finish writing the file, writing 0s at the end if the data at end
        // has not been written.
        if(out.length() != w*h*packBytes+offset) {
            // Goto end of file
            out.seek(out.length());
            // Fill with 0s
            for(i=offset+w*h*packBytes-(int)out.length(); i>0; i--) {
                out.writeByte(0);
            }
        }
        out.close();
        src = null;
        out = null;
        db = null;
    }

    /**
     * Writes all buffered data to the file or resource.
     *
     * @exception IOException If an I/O error occurs.
     * */
    public void flush() throws IOException {
        // No flush is needed since we use RandomAccessFile
        // Get rid of line buffer (is this a good choice?)
        buf = null;
    }

    /**
     * Writes the data of the specified area to the file, coordinates are
     * relative to the current tile of the source. Before writing, the
     * coefficients are limited to the nominal range and packed into 1,2 or 4
     * bytes (according to the bit-depth).
     *
     * <p>If the data is unisigned, level shifting is applied adding 2^(bit
     * depth - 1)</p>
     *
     * <p>This method may not be called concurrently from different
     * threads.</p> 
     *
     * <p>If the data returned from the BlkImgDataSrc source is progressive,
     * then it is requested over and over until it is not progressive
     * anymore.</p>
     *
     * @param ulx The horizontal coordinate of the upper-left corner of the
     * area to write, relative to the current tile.
     *
     * @param uly The vertical coordinate of the upper-left corner of the area
     * to write, relative to the current tile.
     *
     * @param width The width of the area to write.
     *
     * @param height The height of the area to write.
     *
     * @exception IOException If an I/O error occurs.
     * */
    public void write(int ulx, int uly, int w, int h) throws IOException {
        int k,i,j;
        int fracbits = fb;     // In local variable for faster access
        int tOffx, tOffy;      // Active tile offset in the X and Y direction

        // Initialize db
        db.ulx = ulx;
        db.uly = uly;
        db.w = w;
        db.h = h;
        // Get the current active tile offset
        tOffx = src.getCompULX(c)-
            (int)Math.ceil(src.getImgULX()/(double)src.getCompSubsX(c));
        tOffy = src.getCompULY(c)-
            (int)Math.ceil(src.getImgULY()/(double)src.getCompSubsY(c));
        // Check the array size
        if(db.data!=null && db.data.length<w*h) {
            // A new one will be allocated by getInternCompData()
            db.data = null;
        }
        // Request the data and make sure it is not
        // progressive
        do {
            db = (DataBlkInt) src.getInternCompData(db,c);
        } while (db.progressive);

        int tmp;


        // Check line buffer
        if(buf==null || buf.length<packBytes*w) {
            buf = new byte[packBytes*w]; // Expand buffer
        }
        
        switch(packBytes) {

        case 1: // Samples packed into 1 byte
            // Write line by line
            for(i=0; i<h; i++) {
                // Skip to beggining of line in file
                out.seek(offset+this.w*(uly+tOffy+i)+ulx+tOffx);
                // Write all bytes in the line
                if(fracbits==0) {
                    for(k=db.offset+i*db.scanw+w-1, j=w-1; j>=0; k--) {
                        tmp = db.data[k]+levShift;
                        buf[j--] = (byte)((tmp < minVal) ? minVal :
                                          ((tmp>maxVal)? maxVal: tmp));
                    }
                } else {
                    for (k=db.offset+i*db.scanw+w-1, j=w-1; j>=0; k--) {
                        tmp = (db.data[k]>>>fracbits)+levShift;
                        buf[j--] = (byte)((tmp < minVal) ? minVal :
                                          ((tmp>maxVal)? maxVal: tmp));
                    }
                }
                out.write(buf,0,w);
            }
            break;
            
        case 2: // Samples packed in to 2 bytes (short)
            // Write line by line
            for(i=0; i<h; i++) {
              
                // Skip to beggining of line in file
                out.seek(offset+2*(this.w*(uly+tOffy+i)+ulx+tOffx));
                // Write all bytes in the line
                if(fracbits==0) {
                    for (k=db.offset+i*db.scanw+w-1, j=(w<<1)-1; j>=0; k--) {
                        tmp = db.data[k]+levShift;
                        tmp = (tmp<minVal) ? minVal :
                            ((tmp>maxVal)? maxVal: tmp);
                        buf[j--] = (byte)(tmp>>>8);
                        buf[j--] = (byte)tmp; // no need for 0xFF mask since
                                              // truncation will do it already

                    }
                } else {
                    for (k=db.offset+i*db.scanw+w-1, j=(w<<1)-1; j>=0; k--) {
                        tmp = (db.data[k]>>>fracbits)+levShift;
                        tmp = (tmp<minVal) ? minVal :
                            ((tmp>maxVal)? maxVal: tmp);
                        buf[j--] = (byte)(tmp>>>8);
                        buf[j--] = (byte)tmp; // no need for 0xFF mask since
                                              // truncation will do it already

                    }
               }
               out.write(buf,0,w<<1);
            }
            break;

        case 4:
            // Write line by line
            for(i=0; i<h; i++) {
                // Skip to beggining of line in file
                out.seek(offset+4*(this.w*(uly+tOffy+i)+ulx+tOffx));
                // Write all bytes in the line
                if(fracbits==0) {
                    for(k=db.offset+i*db.scanw+w-1, j=(w<<2)-1; j>=0; k--) {
                        tmp = db.data[k]+levShift;
                        tmp = (tmp<minVal) ? minVal :
                            ((tmp>maxVal)? maxVal: tmp);
                        buf[j--] = (byte)tmp;        // No need to use 0xFF
                        buf[j--] = (byte)(tmp>>>8);  // masks since truncation
                        buf[j--] = (byte)(tmp>>>16); // will have already the
                        buf[j--] = (byte)(tmp>>>24); // same effect
                    }
                } else {
                    for(k=db.offset+i*db.scanw+w-1, j=(w<<2)-1; j>=0; k--) {
                        tmp = (db.data[k]>>>fracbits)+levShift;
                        tmp = (tmp<minVal) ? minVal : 
			    ((tmp>maxVal)? maxVal: tmp);
                        buf[j--] = (byte)tmp;        // No need to use 0xFF
                        buf[j--] = (byte)(tmp>>>8);  // masks since truncation
                        buf[j--] = (byte)(tmp>>>16); // will have already the
                        buf[j--] = (byte)(tmp>>>24); // same effect
                    }
                }
                out.write(buf,0,w<<2);
            }
            break;

        default:
            throw new IOException("I support only bit-depth between "+
                                  "1 and 31");
        }

    }
  /**
     * Writes the source's current tile to the output. The requests of data
     * issued to the source BlkImgDataSrc object are done by strips, in order
     * to reduce memory usage.
     *
     * <p>If the data returned from the BlkImgDataSrc source is progressive,
     * then it is requested over and over until it is not progressive
     * anymore.</p>
     *
     * @exception IOException If an I/O error occurs.
     *
     * @see DataBlk
     * */
    public void write() throws IOException {
        int i;
        int tIdx = src.getTileIdx();
        int tw = src.getTileCompWidth(tIdx,c);  // Tile width
        int th = src.getTileCompHeight(tIdx,c);  // Tile height
        // Write in strips
        //offset = -(bitDepth>>3)* (w*h);
        for (int l=0;l<nc;l++) {
 //       	offset += 2* (w*h);
            c = l;
        	for(i=0; i<th ; i+=DEF_STRIP_HEIGHT) {
        		write(0,i,tw,(th-i<DEF_STRIP_HEIGHT) ? th-i : DEF_STRIP_HEIGHT);
            }
        	
        }
    }
    public void writeModImg() throws IOException {
        db.ulx = 0;
        db.uly = 0;
        db.w = w;
        db.h = h;
        if(db.data!=null && db.data.length<w*h) {
            // A new one will be allocated by getInternCompData()
            db.data = null;
        }
        // Request the data and make sure it is not
        // progressive
        do {
            db = (DataBlkInt) src.getInternCompData(db,0);
        } while (db.progressive);

        //Push data to ModelImage 
        modImg.importData(modImgOffset,db.data,false);

    }
    
    /**
     * Returns a string of information about the object, more than 1 line
     * long. The information string includes information from the underlying
     * RandomAccessFile (its toString() method is called in turn).
     *
     * @return A string of information about the object.
     * */
    public String toString() {
        return "ImgWriterRAW: WxH = " + w + "x" + h + ", Component = "+
            c + ", Bit-depth = "+bitDepth + ", signed = "+isSigned + 
            "\nUnderlying RandomAccessFile:\n" + out.toString();
    }

	public int getOffset() {
		return offset;
	}

	public void setOffset(int slcIdx) {
		this.offset = slcIdx * w * h * (bitDepth>>3);
		this.modImgOffset = slcIdx * w * h;
	}
	public void setSrc(BlkImgDataSrc slc) {
		this.src = slc;
	}
	public void writeSlice() throws IOException {
        // Find the list of tile to decode.
        Coord nT = src.getNumTiles(null);

        // Loop on vertical tiles
        for(int y=0; y<nT.y; y++){
            // Loop on horizontal tiles
            for(int x=0; x<nT.x; x++){
            	src.setTile(x,y);
            	writeModImg();
            } // End loop on horizontal tiles            
        } // End loop on vertical tiles
		
	}
	public int getImgType() {
		return imgType;
	}

	public void setImgType(int imgType) {
		this.imgType = imgType;
	}
}
