package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;

import java.io.*;
import java.util.*;

public class AlgorithmLocalVariance extends AlgorithmBase {
	
	private boolean entireImage;
	
	private int kernelSize;
	
	private int halfK;
	
	private boolean do25D;
	
	private boolean isColorImage;
	
	private int valuesPerPixel = 1;
	
	 /**
     * Constructor for 3D images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg       Image model where result image is stored.
     * @param  srcImg        Source image model.
     * @param  kernelSize         Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  do25D  Each slice in a volume image is filtered separately (when true), else the volume will use a
     *                       kernel with 3 dimensions.
     * @param  maskFlag      Flag that indicates that the mean filtering will be performed for the whole image if equal
     *                       to true.
     */
    public AlgorithmLocalVariance(ModelImage destImg, ModelImage srcImg, int kernelSize, boolean do25D, boolean entireImage) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        this.entireImage = entireImage;
        this.kernelSize = kernelSize; // dimension of the kernel
        halfK = (kernelSize - 1) / 2;
        this.do25D = do25D;
        if (!entireImage) {
            mask = srcImage.generateVOIMask();
        }
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        fireProgressStateChanged(0, null, "Filtering image ...");
        
        if ((srcImage.getNDims() == 2) || do25D) {
        	run2D();
        }
        else {
        	run3D();
        }
    }
    
    private void run2D() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int length;
    	double buffer[];
    	int x;
    	int y;
    	int z;
    	double variance[];
    	int yloc;
    	int xloc;
    	int num;
    	double sum;
    	double mean;
    	double diff;
    	
    	xDim = srcImage.getExtents()[0];
    	yDim = srcImage.getExtents()[1];
    	length = xDim * yDim;
    	buffer = new double[length];
    	variance = new double[length];
    	if (srcImage.getNDims() > 2) {
    		zDim = srcImage.getExtents()[2];
    	}
    	else {
    		zDim = 1;
    	}
    	
    	for (z = 0; z < zDim; z++) {
    	    try {
    	    	srcImage.exportData(z*length, length, buffer);
    	    }
    	    catch(IOException e) {
    	    	MipavUtil.displayError("IOException " + e + " on srcImage.exportData(z*length, length, buffer)");
    	    	setCompleted(false);
    	    	return;
    	    }
    	    
    	    
    	    for (y = 0; y < yDim; y++) {
    	    	for (x = 0; x < xDim; x++) {
    	    		num = 0;
    	    		sum = 0.0;
    	    	    for (yloc = Math.max(0, y - halfK); yloc <= Math.min(yDim-1, y + halfK); yloc++) {
    	    	    	for (xloc = Math.max(0, x - halfK); xloc <= Math.min(xDim-1, x + halfK); xloc++) {
    	    	    		num++;
    	    	    		sum += buffer[xloc + xDim * yloc];
    	    	    	}
    	    	    }
    	    	    mean = sum / num;
    	    	    sum = 0.0;
    	    	    for (yloc = Math.max(0, y - halfK); yloc <= Math.min(yDim-1, y + halfK); yloc++) {
    	    	    	for (xloc = Math.max(0, x - halfK); xloc <= Math.min(xDim-1, x + halfK); xloc++) {
    	    	    	    diff = buffer[xloc + xDim * yloc] - mean;
    	    	    	    sum += diff * diff;
    	    	    	}
    	    	    }
    	    	    variance[x + xDim * y] = sum /num;
    	    	}
    	    }
    	    
    	    if (destImage != null) {
    	    	try {
    	    	    destImage.importData(z * length, variance, false);
    	    	}
    	    	catch(IOException e) {
        	    	MipavUtil.displayError("IOException " + e + " on destImage.importData(z*length, result, false)");
        	    	setCompleted(false);
        	    	return;
        	    }
    	    }
    	    else {
    	    	try {
    	    	    srcImage.importData(z * length, variance, false);
    	    	}
    	    	catch(IOException e) {
        	    	MipavUtil.displayError("IOException " + e + " on srcImage.importData(z*length, result, false)");
        	    	setCompleted(false);
        	    	return;
        	    }	
    	    }
    	} // for (z = 0; z < zDim; z++)
    	
    	if (destImage != null) {
    		destImage.calcMinMax();
    	}
    	else {
    		srcImage.calcMinMax();
    	}
    	setCompleted(true);
    	return;
    	
    }
    
    private void run3D() {
    	
    }
	
}