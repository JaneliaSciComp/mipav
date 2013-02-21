package gov.nih.mipav.model.file.rawjp2;

import gov.nih.mipav.model.structures.ModelImage;

import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import jj2000.j2k.JJ2KExceptionHandler;
import jj2000.j2k.image.DataBlk;
import jj2000.j2k.image.DataBlkInt;

public class ImgReaderRAWColorSlice extends ImgReaderRAWSlice {
	
	private int cFactor=4;
	private int linebuf[];
//	private byte linebuf[];	
	private int sliceOffset;
	
	public ImgReaderRAWColorSlice(File file, int si) throws IOException {
		super(file, si);
		// TODO Auto-generated constructor stub
	}

	public ImgReaderRAWColorSlice(String fname, int si) throws IOException {
		super(fname, si);
		// TODO Auto-generated constructor stub
	}

	public ImgReaderRAWColorSlice(RandomAccessFile in, int si)
	throws EOFException, IOException {
		super(in, si);
		// TODO Auto-generated constructor stub
	}

	public ImgReaderRAWColorSlice(ModelImage image, int si, boolean savingAsEncJP2) {
		super(image, si, savingAsEncJP2);
//		int[] imgExtents;

		this.nc=3;
		
        this.rb=8;
        
//        this.offset = si * (rb>>3)* w * h;
        this.sliceOffset = si * w * h * cFactor;
        if(!isOrigSigned(0)){
        	DC_OFFSET = 1 << (rb-1);
        } else {
        	DC_OFFSET = 0;
        }
        this.useModImg = true;
	}

	public void setSliceIndex(int si, boolean useModImg) throws IOException{
		int cFactor=1;
		if (useModImg) {
			if (image.isColorImage()) {
				cFactor = 4;
			}
			this.sliceOffset = si * w * h *cFactor;   
			this.useModImg = true;
		} else {
			this.sliceOffset = si * (rb>>3)* w * h;
			in = new RandomAccessFile(fName,"r");
		}

	}
	
	
	public DataBlk getInternCompData(DataBlk blk, int c) {
		int j,i,mi;
		int barr[];


		// Check component index, COLOR IMAGE
		if (c <0||c>2)
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
			barr = new int[blk.w*blk.h];
			blk.setData(barr); 
		}

		if (!useModImg) return blk;
//		barr.
		// Check line buffer
		if (linebuf == null || linebuf.length < cFactor*blk.w) {
			linebuf = new int[cFactor*blk.w];
		}

		//		if (useModImg) {
		try{
			// Read line by line
			mi = blk.uly + blk.h;
			for (i = blk.uly; i < mi; i++) {
				image.exportData(sliceOffset + cFactor*(i*w + blk.ulx), cFactor*blk.w, linebuf);
				for(j=0;j<blk.w;j++) barr[(i-blk.uly)*w + j]=linebuf[cFactor*j+c+1]-DC_OFFSET;
			}
		} catch (IOException e) {
			JJ2KExceptionHandler.handleException(e);
		}
		// Turn off the progressive attribute
		blk.progressive = false;
		// Set buffer attributes
		blk.offset = 0;//offset;
//		offset = offset - 2 * c * w*h;
		blk.scanw = blk.w;
		return blk;
//		}

//		return blk;
	}
	public int getNomRangeBits(int c) {
		// Check component index
		if (c<0||c>2)
			throw new IllegalArgumentException();

		return rb;
	}	
    /**
     * Returns true if the data read was originally signed in the specified
     * component, false if not. This method always returns false since PPM
     * data is always unsigned.
     *
     * @param c The index of the component, from 0 to N-1.
     *
     * @return always false, since PPM data is always unsigned.
     * */
    public boolean isOrigSigned(int c) {
        // Check component index
        if (c < 0 || c > 2)
            throw new IllegalArgumentException();
        return false;
    }	
}

