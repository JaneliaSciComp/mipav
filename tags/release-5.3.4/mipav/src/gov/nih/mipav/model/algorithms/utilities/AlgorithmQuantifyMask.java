package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * Algorithm that does basic calculations on Mask images (boolean/ubyte/short)
 * 
 * calculates Center of Mass, area (in resolutions), and number of pixels per mask
 *
 */
public class AlgorithmQuantifyMask extends AlgorithmBase {


    //~ Constructors ---------------------------------------------------------------------------------------------------

	
	
    /**
     * Creates a new AlgorithmMask object.
     *
     * @param  srcImg    source image model
     */
    public AlgorithmQuantifyMask(ModelImage srcImg ) {
        super(null, srcImg);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    
    public void runAlgorithm() {
    	
    	if (srcImage.getType() != ModelStorageBase.BOOLEAN &&
    			srcImage.getType() != ModelStorageBase.UBYTE &&	
    			srcImage.getType() != ModelStorageBase.SHORT) {
    		MipavUtil.displayError("Image type (mask) must be boolean, short, or ubyte");
    		return;
    	} else if ((srcImage.getMax() - srcImage.getMin()) > 100) {
    		MipavUtil.displayError("Verify that image is a mask:  image max intensity - image min intensity is greater than 100");
    		return;
    	}
    	
    	if (srcImage.getNDims() == 2) {
    		calc2D();
    	} else {
    		calc34D();
    	}
    	  		
    	
    }
    
    private void calc2D() {
        //    	run through image once to determine # of unique values (mask values)
    	int min = (int)srcImage.getMin();
    	int max = (int)srcImage.getMax();
    	
    	int dif = max - min + 1;
    	
    	long [] xCenter = new long[dif];
    	long [] yCenter = new long[dif];
    	int [] area = new int[dif];
    	
    	
    	int [] data  = new int[srcImage.getSliceSize()];
    	
    	try {
    		srcImage.exportData(0, data.length, data);
    	} catch (Exception e) {
    		return;
    	}
    	    	
    	int xDim = srcImage.getExtents()[0];
    	int yDim = srcImage.getExtents()[1];
    	
    	int val = 0;
    	
    	for (int y = 0; y < yDim; y++) {
    		
    		for (int x = 0; x < xDim; x++) {
    			
    			val = data[(y * xDim) + x] - min;
    			
    			area[val]++;
    			xCenter[val] += (x + 1);
    			
    			yCenter[val] += (y + 1);
    		}    		   		
    	}
    	
    	String outputString = "\nOutput from QuantifyMasks: " + srcImage.getImageName() + "\n";
        String consoleString = "\nOutput from QuantifyMasks: " + srcImage.getImageName() + "\n";
    	
    	outputString += "Object\t# of voxels\tArea(" + srcImage.getFileInfo(0).getAreaUnitsOfMeasureStr() + ")"
    		+ "\tCenter of mass\n";
        consoleString += "Object\t# of voxels\tArea(" + srcImage.getFileInfo(0).getAreaUnitsOfMeasureStr() + ")"
        + "\tCenter of mass\n";
    	
    	for (int i = 0; i < dif; i++) {
    		if (area[i] != 0) {
    			xCenter[i] = xCenter[i] / area[i];
    			yCenter[i] = yCenter[i] / area[i];
    	
    			outputString += (i + min) + "\t" + area[i] + "\t" + 
    			(area[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1]) + "\t(" + xCenter[i] + "," + yCenter[i] + ")\n";
                consoleString += (i + min) + "\t" + area[i] + "\t\t" + 
                (area[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1]) + "\t\t(" + xCenter[i] + "," + yCenter[i] + ")\n";                                                                      
    			
    		}
    	}
        System.err.println(consoleString);
    	ViewUserInterface.getReference().getMessageFrame().append(outputString, ViewJFrameMessage.DATA);
    	
    	setCompleted(true);
    }
    
    private void calc34D() {
//    	run through image once to determine # of unique values (mask values)
    	int min = (int)srcImage.getMin();
    	int max = (int)srcImage.getMax();
    	
    	int dif = max - min + 1;
    	
    	long [] xCenter = new long[dif];
    	long [] yCenter = new long[dif];
    	long [] zCenter = new long[dif];
    	int [] volume = new int[dif];
    	
    	int sliceSize = srcImage.getSliceSize();
    	
    	int [] data  = new int[sliceSize];
    	
    	
    	    	
    	int xDim = srcImage.getExtents()[0];
    	int yDim = srcImage.getExtents()[1];
    	int zDim = srcImage.getExtents()[2];
    	
    	int val = 0;
    	
    	for (int z = 0; z < zDim; z++) {
    		try {
        		srcImage.exportData((z * sliceSize), data.length, data);
        	} catch (Exception e) {
        		return;
        	}
        	for (int y = 0; y < yDim; y++) {
    		
        		for (int x = 0; x < xDim; x++) {
    			
        			val = data[(y * xDim) + x] - min;
    			
        			volume[val]++;
        			xCenter[val] += (x + 1);
    			
        			yCenter[val] += (y + 1);
        			zCenter[val] += (z + 1);
        		}    		   		
        	}
    	}
    	
    	String outputString = "\nOutput from QuantifyMasks: " + srcImage.getImageName() + "\n";
        String consoleString = "\nOutput from QuantifyMasks: " + srcImage.getImageName() + "\n";
    	
    	outputString += "Object\t# of voxels\tVolume(" + srcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr() + ")"
    		+ "\t\tCenter of mass\n";
        consoleString += "Object\t# of voxels\tVolume(" + srcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr() + ")"
        + "\tCenter of mass\n";
    	for (int i = 0; i < dif; i++) {
    		if (volume[i] != 0) {
    			xCenter[i] = xCenter[i] / volume[i];
    			yCenter[i] = yCenter[i] / volume[i];
    			zCenter[i] = zCenter[i] / volume[i];
    	
    			  
    			outputString += (i + min) + "\t" + volume[i] + "\t" + 
    			(volume[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1] * srcImage.getResolutions(0)[2]) + 
    			"\t\t(" + xCenter[i] + "," + yCenter[i] + "," + zCenter[i] + ")\n";
                consoleString += (i + min) + "\t" + volume[i] + "\t\t" + 
                (volume[i] * srcImage.getResolutions(0)[0] * srcImage.getResolutions(0)[1] * srcImage.getResolutions(0)[2]) + 
                "\t(" + xCenter[i] + "," + yCenter[i] + "," + zCenter[i] + ")\n";
    			
    			
    		}
    	}
        
        System.err.println(consoleString);
    	ViewUserInterface.getReference().getMessageFrame().append(outputString, ViewJFrameMessage.DATA);
    	
    	setCompleted(true);
    }
}
