package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Extracts slices from a mosaic in a 2D image
 *
 * @version  1.0 September 2, 2010
 * @author   William Gandler
 */
public class AlgorithmMosaicToSlices extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Source image */
    private ModelImage srcImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMosaicToSlices object.
     *
     * @param  srcIm  source image model
     * @param  dest    destination image
     */
    public AlgorithmMosaicToSlices(ModelImage srcIm, ModelImage dest) {
        super(dest, srcIm);
        srcImage = srcIm; 
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
        super.finalize();
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  Result image.
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int subXDim;
    	int subYDim;
    	int cFactor = 1;
    	double buffer[] = null;
    	double subBuffer[] = null;
    	int length;
    	int subLength;
    	int sliceNum;
    	int x;
    	int y;
    	int xs;
    	int ys;
    	int index;
    	int subIndex;
    	int c;
        if (srcImage == null) {
        	displayError("Source Image is null");
            setCompleted(false);

            return;
        }
        
        if (destImage == null) {
        	displayError("Destination Image is null");
            setCompleted(false);

            return;
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        
        if (srcImage.isComplexImage()) {
        	cFactor = 2;
        }
        else if (srcImage.isColorImage()) {
        	cFactor = 4;
        }
        
        length = cFactor * xDim * yDim;
        buffer = new double[length];
        
        try {
        	srcImage.exportData(0, length, buffer);
        }
        catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("AlgorithmMosaicToSlices. srcImage locked", true);

            return;
        } 
        
        subXDim = destImage.getExtents()[0];
        subYDim = destImage.getExtents()[1];
        subLength = cFactor * subXDim * subYDim;
        subBuffer = new double[subLength];
        sliceNum = 0;
        for (y = 0; y < yDim; y += subYDim) {
        	for (x = 0; x < xDim; x += subXDim) {
        	    for (ys = 0; ys < subYDim; ys++) {
        	    	for (xs = 0; xs <  subXDim; xs++) {
        	    	    for (c = 0; c < cFactor; c++) {
        	    	    	subIndex = c + cFactor*(xs + ys*subXDim);
        	    	    	index = c + cFactor*(x + xs + (y + ys)*xDim);
        	    	    	subBuffer[subIndex] = buffer[index];
        	    	    } // for (c = 0; c < cFactor; c++)
        	    	} // for (xs = 0; xs <  subXDim; xs++)
        	    } // for (ys = 0; ys < subYDim; ys++)
        	    try {
        	    	destImage.importData(sliceNum*subLength, subBuffer, false);
        	    }
        	    catch (IOException error) {
                    buffer = null;
                    destImage.disposeLocal(); // Clean up memory of result image
                    destImage = null;
                    errorCleanUp("AlgorithmMosaicToSlices destImage locked", true);

                    return;
                }
        	    sliceNum++;
        	}
        }
        destImage.calcMinMax();
        setCompleted(true);
        
    }

   
}
