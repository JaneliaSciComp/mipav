package gov.nih.mipav.model.file.rawjp2;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.File;
import java.io.IOException;

import jj2000.j2k.image.BlkImgDataSrc;
import jj2000.j2k.image.Coord;
import jj2000.j2k.image.DataBlkInt;

public class ImgWriterRAWColor extends ImgWriterRAW {
	
	private int fb[] = new int[3];
	private int[] levShift = new int[3];
	   
	protected int cFactor=4;

	protected DataBlkInt db0 = new DataBlkInt(),db1 = new DataBlkInt(),db2 = new DataBlkInt();
	
	public ImgWriterRAWColor(File out, BlkImgDataSrc imgSrc, int c,
			boolean isSigned) throws IOException {
		super(out, imgSrc, c, isSigned);
		imgType = ModelStorageBase.INTEGER;		
	}

	public ImgWriterRAWColor(ModelImage mi, BlkImgDataSrc imgSrc, boolean isSigned) throws IOException {
		super(mi, imgSrc, isSigned);

    	this.nc = 3;
    	this.modImg = mi;
    	
//    	this.c = 2;
   
        this.isSigned = isSigned;
        src = imgSrc;
        w = src.getImgWidth();
        h = src.getImgHeight();
        
        fb[0] = imgSrc.getFixedPoint(0);
        fb[1] = imgSrc.getFixedPoint(1);
        fb[2] = imgSrc.getFixedPoint(2);

        levShift[0] = 1<< (imgSrc.getNomRangeBits(0)-1);
        levShift[1] = 1<< (imgSrc.getNomRangeBits(1)-1);
        levShift[2] = 1<< (imgSrc.getNomRangeBits(2)-1);
        
        
        bitDepth = src.getNomRangeBits(0);
        
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
            
//        levShift = (this.isSigned) ? 0 : 1<<(src.getNomRangeBits(c)-1);
        
		imgType = ModelStorageBase.INTEGER;		
	}

	public ImgWriterRAWColor(String fname, BlkImgDataSrc imgSrc, int c,
			boolean isSigned) throws IOException {
		super(fname, imgSrc, c, isSigned);
		// TODO Auto-generated constructor stub
		imgType = ModelStorageBase.INTEGER;
	}
	public void setOffset(int slcIdx) {
		this.offset = slcIdx * w * h * (bitDepth>>3);

		this.modImgOffset = slcIdx * w * h; //for gray image
		if (imgType == ModelStorageBase.ARGB) {	// color image ARGB
			modImgOffset = modImgOffset*cFactor;
		}
	}
	public void writeSlice() throws IOException {
		// Find the list of tile to decode.
        Coord nT = src.getNumTiles(null);

        // Loop on vertical tiles
        for(int y=0; y<nT.y; y++){
            // Loop on horizontal tiles
            for(int x=0; x<nT.x; x++){
            	src.setTile(x,y);
            	writeModImg(x,y);
            } // End loop on horizontal tiles            
        } // End loop on vertical tiles

	}	
	
	
	public void writeModImg(int x, int y) throws IOException {
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
//		do {
//			db = (DataBlkInt) src.getInternCompData(db,0);
//		} while (db.progressive);
		
		if(db0.data!=null && db0.data.length<w*h) {
			// A new one will be allocated by getInternCompData()
			db0.data = null;
		}
		if(db1.data!=null && db1.data.length<w*h) {
			// A new one will be allocated by getInternCompData()
			db1.data = null;
		}		
		if(db2.data!=null && db2.data.length<w*h) {
			// A new one will be allocated by getInternCompData()
			db2.data = null;
		}		
		int dataLength;

		
		db0 = (DataBlkInt) src.getInternCompData(db,0);
		dataLength = db0.data.length;
		int[] barr = new int[cFactor * dataLength];
		
		if (fb[0]==0) {
			for(int i=0;i<dataLength;i++){
				barr[cFactor*i+1]=db0.data[i]+ levShift[0];	
				db0.data[i] = db0.data[i] + levShift[0];
			
			}
		}else{
			for(int i=0;i<dataLength;i++){
				barr[cFactor*i+1] = db0.data[i]>>>fb[0] + levShift[0];
			}
		}
		
		db1 = (DataBlkInt) src.getInternCompData(db,1);
		if (fb[1]==0) {
			for(int i=0;i<dataLength;i++){
				barr[cFactor*i+2]=db1.data[i]+ levShift[1];				
				db1.data[i] = db1.data[i] + levShift[1];
			}
		}else{
			for(int i=0;i<dataLength;i++){
				barr[cFactor*i+2] = db1.data[i]>>>fb[1] + levShift[1];
			}
		}
	
		db2 = (DataBlkInt) src.getInternCompData(db,2);
		if (fb[2]==0) {
			for(int i=0;i<dataLength;i++){
				barr[cFactor*i+3]=db2.data[i]+ levShift[2];				
				db2.data[i] = db2.data[i] + levShift[2];
			}
		}else{
			for(int i=0;i<dataLength;i++){
				barr[cFactor*i+3] = db2.data[i]>>>fb[2] + levShift[2];
			}
		}		
		
		//Push data to ModelImage 
//		modImg.importData(modImgOffset/4,db.data,false);
		if(imgType == ModelStorageBase.ARGB){


			for(int i=0;i<dataLength;i++) {
				barr[cFactor*i]=1;//alpha channel
//				barr[cFactor*i+1]=db0.data[i];
//				barr[cFactor*i+2]=db1.data[i];
//				barr[cFactor*i+3]=db2.data[i];			
			}
			modImg.importData(modImgOffset,barr,false);
		} else
			modImg.importData(modImgOffset,db0.data,false);		
	}
	
	public void setSrc(BlkImgDataSrc slc) {
		this.src = slc;

        fb[0] = slc.getFixedPoint(0);
        fb[1] = slc.getFixedPoint(1);
        fb[2] = slc.getFixedPoint(2);
		
	}
	

}

