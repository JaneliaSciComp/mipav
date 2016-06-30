package gov.nih.mipav.model.algorithms;
import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

public class AlgorithmEfficientWatershed extends AlgorithmBase {
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmEfficientWatershed(ModelImage destImage, ModelImage srcImage) {
		super(destImage, srcImage);
	}
	
	public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int nDims;
    	int length;
    	double imgBuffer[];
    	double sortBuffer[];
    	int intBuffer[] = null;
    	int x;
    	int y;
    	int z;
    	int t;
    	double minValue = Double.MAX_VALUE;
    	double maxValue = -Double.MAX_VALUE;
    	int i;
    	int j;
    	int k;
    	int numValues;
    	double histBins[];
    	int indexBins[][];
    	int lasti;
    	double currentValue;
    	int numLowestValues;
    	int labelNum;
    	boolean foundBins[];
    	boolean labelFound;
    	int numLowestFound;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Efficient Watershed ...");
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        nDims = srcImage.getNDims();
    	
        if (nDims > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        
        try {
            imgBuffer = new double[length];
            sortBuffer = new double[length];
            intBuffer = new int[length];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Efficient Watershed: Out of memory creating buffers");
            setCompleted(false);

            return;
        }

        for (t = 0; t < tDim; t++) {
        for (z = 0; z < zDim; z++) {

            try {
                srcImage.exportData((z + t*zDim)*length, length, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm Efficient Watershed: image bounds exceeded");
                setCompleted(false);
                
                srcImage.releaseLock();

                return;
            }
            
            for (i = 0; i < length; i++) {
                sortBuffer[i] = imgBuffer[i];	
            }
            
            Arrays.sort(sortBuffer);
            numValues = 1;
            for (i = 1; i < length; i++) {
            	if (sortBuffer[i] > sortBuffer[i-1]) {
            		numValues++;
            	}
            }
            histBins = new double[numValues];
            indexBins = new int[numValues][];
            histBins[0] = sortBuffer[0];
            lasti = 0;
            for (i = 1, j = 1; i < length; i++) {
            	if (sortBuffer[i] > sortBuffer[i-1]) {
            		histBins[j] = sortBuffer[i];
            		indexBins[j-1] = new int[i - lasti];
            		lasti = i;
            		j++;
            	}
            }
            indexBins[numValues-1] = new int[length-1-lasti];
            for (i = 0; i < numValues; i++) {
            	k = 0;
                currentValue = histBins[i];
                for (j = 0; j < length; j++) {
                	if (imgBuffer[j] == currentValue) {
                		indexBins[i][k++] = j;
                	}
                }
            }
            
            numLowestValues = indexBins[0].length;
            foundBins = new boolean[numLowestValues];
            labelNum = 0;
            for (i = 0; i < length; i++) {
            	intBuffer[i] = Integer.MIN_VALUE;
            }
            intBuffer[indexBins[0][0]] = labelNum;
            foundBins[0] = true;
            labelFound = true;
            numLowestFound = 1;
            while (labelFound) {
            	labelFound = false;
            	for (i = 0; i < numLowestValues; i++) {
            		for (j = 0; j < numLowestValues; j++) {
            			if (j != i) {
	            			if ((foundBins[i]) && (!foundBins[j]) && 
	            					(((indexBins[0][i] % xDim < xDim-1) && (indexBins[0][j] == indexBins[0][i]+1)) ||
	            					((indexBins[0][i] % xDim > 0) && (indexBins[0][j] == indexBins[0][i]-1)) ||
	            					(indexBins[0][j] == indexBins[0][i]+yDim) ||
	            					(indexBins[0][j] == indexBins[0][i]-yDim))) {
	            				labelFound = true;
	            				foundBins[j] = true;
	            				intBuffer[indexBins[0][j]] = labelNum;
	            				numLowestFound++;
	            			}
            			}
            		}
            	}
            	
            	if ((!labelFound) && (numLowestFound < numLowestValues)) {
            		labelNum++;
            		for (i = 0; i < numLowestValues && (!labelFound); i++) {
            			if (!foundBins[i]) {
            				labelFound = true;
            				foundBins[i] = true;
            				intBuffer[indexBins[0][i]] = labelNum;
            				numLowestFound++;
            			}
            		}
            		if (numLowestFound == numLowestValues) {
            			labelFound = false;
            		}
            	}
            } // while (labelFound)
            
            try {
			    destImage.importData((z + t*zDim)*length, intBuffer, false);
			}
			catch(IOException e) {
				MipavUtil.displayError("IOException " + e + " on destImage.importData");
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