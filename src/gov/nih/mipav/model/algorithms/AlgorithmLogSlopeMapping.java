package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 
 */
public class AlgorithmLogSlopeMapping extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

	private ModelImage[] srcImages;
	
	private ModelImage destImage;
	
	private double[] xValueArray;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmLogSlopeMapping(ModelImage destImage, ModelImage[] srcImages, double[] xValueArray) {

        this.destImage = destImage;
        this.srcImages = srcImages;
        this.xValueArray = xValueArray;

    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }

    

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        int nDims;
        int xDim;
        int yDim; 
        int sliceSize;
        int zDim;
        int volSize;
        int tDim;
        int i;
        int z;
        int t;
        int nImages;
        double yVal[][];
        double slope[];
        int n;
        double sumX;
        double sumXSquared;
        double denom;
        double sumY;
        double sumXY;
        
        nImages = srcImages.length;
        sumX = 0.0;
        sumXSquared = 0.0;
        for (n = 0; n < nImages; n++) {
            sumX += xValueArray[n];
            sumXSquared += (xValueArray[n] * xValueArray[n]);
        }
        denom = sumXSquared - sumX * sumX / nImages;
        nDims = srcImages[0].getNDims();
        xDim = srcImages[0].getExtents()[0];
        yDim = srcImages[0].getExtents()[1];
        sliceSize = xDim * yDim;
        if (nDims > 2) {
        	zDim = srcImages[0].getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        volSize = zDim * sliceSize;
        if (nDims > 3) {
        	tDim = srcImages[0].getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        
        yVal = new double[nImages][sliceSize];
        slope = new double[sliceSize];
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
        		for (i = 0; i < sliceSize; i++) {
        			slope[i] = 0.0;
        		}
        		
                for (n = 0; n < nImages; n++) {
                	try {
                		srcImages[n].exportData(t*volSize + z*sliceSize, sliceSize, yVal[n]);
                	}
                	catch (IOException e) {
                		MipavUtil.displayError("IOException on srcImages[" + n + "].exportData("+ t+ "*volSize + " + z +
                				"*sliceSize, sliceSize, yVal[" + n + "]");
                		setCompleted(false);
                		return;
                	}
                	
                	for (i = 0; i < sliceSize; i++) {
                		if (!Double.isNaN(slope[i])) {
                			if (yVal[n][i] > 0.0) {
                			    yVal[n][i] = Math.log(yVal[n][i]);	
                			}
                			else {
                				slope[i] = Double.NaN;
                			}
                		}	
                    } // for (i = 0; i < sliceSize; i++)
                } // for (n = 0; n < nImages; n++)
                
                for (i = 0; i < sliceSize; i++) {
                    if (!Double.isNaN(slope[i])) {
                        sumY = 0.0;
                        sumXY = 0.0;
                        for (n = 0; n < nImages; n++) {
                        	sumY += yVal[n][i];
                        	sumXY += (xValueArray[n] * yVal[n][i]);
                        }
                        slope[i] = (sumXY - sumX * sumY/nImages)/denom;
                    }
                } // for (i = 0; i < sliceSize; i++)
                
                try {
                	destImage.importData(t*volSize + z*sliceSize, slope, false);
                }
                catch (IOException e) {
            		MipavUtil.displayError("IOException on destImage.importData("+ t+ "*volSize + " + z +
            				"*sliceSize, slope, false");
            		setCompleted(false);
            		return;
            	}
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        
        destImage.calcMinMax();
        setCompleted(true);
        return;
    }


    
}
