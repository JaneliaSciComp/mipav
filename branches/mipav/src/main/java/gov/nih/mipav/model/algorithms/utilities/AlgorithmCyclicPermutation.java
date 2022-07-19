package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmCyclicPermutation extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	int shiftX;
	int shiftY;
	int shiftZ;
	int shiftT;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------	
	
	/**
	 * 
	 * @param destImg
	 * @param srcImg
	 * @param shiftX
	 * @param shiftY
	 * @param shiftZ
	 * @param shiftT
	 */
	public AlgorithmCyclicPermutation(ModelImage destImg, ModelImage srcImg, int shiftX, int shiftY, int shiftZ, int shiftT) {
	    super(destImg, srcImg);
	    this.shiftX = shiftX;
	    this.shiftY = shiftY;
	    this.shiftZ = shiftZ;
	    this.shiftT = shiftT;
	}
	
	//~ Methods --------------------------------------------------------------------------------------------------------

	/**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int colorFactor = 1;
        double buffer[];
        double buffer2[];
        int xDim;
        int yDim;
        int zDim = 1;
        int tDim = 1;
        int nDims;
        int length;
        int x;
        int y;
        int z;
        int t;
        int c;
        int yScale;
        int zScale = 0;
        int tScale = 0;
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Cyclic permutation on image ...");
        
        if (srcImage.isComplexImage()) {
            colorFactor = 2;	
        }
        else if (srcImage.isColorImage()) {
            colorFactor = 4;	
        }
        nDims = srcImage.getNDims();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        yScale = colorFactor * xDim;
        length = colorFactor * xDim * yDim;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        	length = length * zDim;
        	zScale = yScale * yDim;
        }
        if (srcImage.getNDims() > 3) {
        	tDim = srcImage.getExtents()[3];
        	length = length * tDim;
        	tScale = zScale * zDim;
        }
        buffer = new double[length];
        buffer2 = new double[length];
        try {
        	srcImage.exportData(0, length, buffer);
        }
        catch (IOException e) {
        	buffer = null;
        	buffer2 = null;
        	MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, length, buffer)");
        	setCompleted(false);
        	return;
        }
        shiftX = shiftX % xDim;
        shiftY = shiftY % yDim;
        shiftZ = shiftZ % zDim;
        shiftT = shiftT % tDim;
        if (shiftX < 0) {
        	shiftX = xDim + shiftX;
        }
        if (shiftY < 0) {
        	shiftY = yDim + shiftY;
        }
        if (shiftZ < 0) {
        	shiftZ = zDim + shiftZ;
        }
        if (shiftT < 0) {
        	shiftT = tDim + shiftT;
        }
    	
    	if (nDims == 2) {	
            for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					for (c = 0; c < colorFactor; c++) {
						buffer2[c + colorFactor*((x + shiftX)%xDim) + yScale*((y + shiftY)%yDim)] =
					    buffer[c + colorFactor*x + yScale*y];
					} // for (c = 0; c < colorFactor; c++) {
				} // for (x = 0; x < xDim; x++) { 
			} // for (y = 0; y < yDim; y++) {
    	} // if (nDims == 2)
    	else if (nDims == 3) {
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					for (c = 0; c < colorFactor; c++) {
    						buffer2[c + colorFactor*((x + shiftX)%xDim) + yScale*((y + shiftY)%yDim) + zScale*((z + shiftZ)%zDim)] =
    					    buffer[c + colorFactor*x + yScale*y + zScale*z];
    					} // for (c = 0; c < colorFactor; c++) {
    				} // for (x = 0; x < xDim; x++) { 
    			} // for (y = 0; y < yDim; y++) {	
    		} // for (z = 0; z < zDim; z++)
    	} // else if (nDims == 3)
    	else { // nDims == 4
    		for (t = 0; t < tDim; t++) {
    			for (z = 0; z < zDim; z++) {
        			for (y = 0; y < yDim; y++) {
        				for (x = 0; x < xDim; x++) {
        					for (c = 0; c < colorFactor; c++) {
        						buffer2[c + colorFactor*((x + shiftX)%xDim) + yScale*((y + shiftY)%yDim) + zScale*((z + shiftZ)%zDim)
        						        + tScale*((t + shiftT)%tDim)] =
        					    buffer[c + colorFactor*x + yScale*y + zScale*z + tScale*t];
        					} // for (c = 0; c < colorFactor; c++) {
        				} // for (x = 0; x < xDim; x++) { 
        			} // for (y = 0; y < yDim; y++) {	
        		} // for (z = 0; z < zDim; z++)	
    		} // for (t = 0; t < tDim; t++)
    	} // else nDims == 4
    	buffer = null;
    	
    	if (destImage != null) {
    		try {
    		    destImage.importData(0, buffer2, true);
    		}
    		catch (IOException e) {
            	buffer2 = null;
            	MipavUtil.displayError("IOException " + e + " on destImage.importData(0, buffer2, true)");
            	setCompleted(false);
            	return;	
    		}
    	}
    	else {
    		try {
    		    srcImage.importData(0, buffer2, true);
    		}
    		catch (IOException e) {
            	buffer2 = null;
            	MipavUtil.displayError("IOException " + e + " on srcImage.importData(0, buffer2, true)");
            	setCompleted(false);
            	return;	
    		}
    	}
    	
    	setCompleted(true);
    	return;
    }
}