package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

/**
 * 
 * @author ilb
 * 
 * Steps:
 * 1.) 1D masks are multiplied to construct 2D masks.
 * 2.) Remove the effects of illumination by subtracting the neighborhood mean from each pixel
 *     in the neighborhood.
 * 3.) Each of the 3 X 3, 5 X 5, or 7 X 7 masks are used to filter a neighborhood of the same size.
 * 4.) Compute the energy of each pixel by summing the absolute values of the filter outputs in the
 *     pixel neighborhoods.  Symmetric energy pairs, such as E5L5 and L5E5, are averaged together
 *     to form one result.
 * References: 1.) Image Processing Dealing with Texture by Maria Petrou and Pedro Garcia-Sevilla,
 * John Wiley & Sons, Ltd., copyright 2006, pp. 539-545 and 564-567.
 *
 * 2.) Computer Vision: March 2000, Chapter 7 Texture, Shapiro and Stockman.
 */

public class AlgorithmLawsTexture extends AlgorithmBase {
	
	 /** Red channel. */
    private static final int RED_OFFSET = 1;
	
	/** Level 3 */
	private static final int L3[] = new int[]{1, 2, 1};
	
	/** Edge 3 */
	private static final int E3[] = new int[]{-1, 0, 1};
	
	/** Spot 3 */
	private static final int S3[] = new int[]{-1, 2, -1};
	
	/** Level 5 */
	private static final int L5[] = new int[]{1, 4, 6, 4, 1};
	
	/** Edge 5 */
	private static final int E5[] = new int[]{-1, -2, 0 , 2, 1};
	
	/** Spot 5 */
	private static final int S5[] = new int[]{-1, 0, 2, 0, -1};
	
	/** Wave 5 */
	private static final int W5[] = new int[]{-1, 2, 0, -2, 1};
	
	/** Ripple 5 */
	private static final int R5[] = new int[]{1, -4, 6, -4, 1};
	
	/** Level 7 */
	private static final int L7[] = new int[]{1, 6, 15, 20, 15, 6, 1};
	
	/** Edge 7 */
	private static final int E7[] = new int[]{-1, -4, -5, 0, 5, 4, 1};
	
	/** Spot 7 */
	private static final int S7[] = new int[]{-1, -2, 1, 4, 1, -2, -1};
	
	/** Wave 7 */
	private static final int W7[] = new int[]{-1, 0, 3, 0, -3, 0, 1};
	
	/** Ripple 7 */
	private static final int R7[] = new int[]{1, -2, -1, 4, -1, -2, 1};
	
	/** Oscillation 7 */
	private static final int O7[] = new int[]{-1, 6, -15, 20, -15, 6, -1};
	
	private ModelImage[] destImage = null;
	
	/** Size of square window must be 3, 5, or 7 */
	private int windowSize = 5;
	
	private int RGBOffset = RED_OFFSET;
	
	/**
	 * 
	 * @param destImg
	 * @param srcImg
	 * @param windowSize
	 * @param RGBOffset
	 */
	public AlgorithmLawsTexture(ModelImage[] destImg, ModelImage srcImg, int windowSize,
			int RGBOffset) {
		super(null, srcImg);
        destImage = destImg;
        this.windowSize = windowSize;
		this.RGBOffset = RGBOffset;
	}
	
	/**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();


            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        

        fireProgressStateChanged(0, null, "Running Laws textures ...");
        
        calculateLaws();
    }

    /**
     * DOCUMENT ME!
     */
    private void calculateLaws() {
    	int i;
    	int j;
    	int k;
    	int m;
    	int n;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim;
        int mask[][] = null;
        String label = null;
        int rowVal[];
        int colVal[];
        String rowString = null;
        String colString = null;
        int index;
        int originalMaps;
        int finalMaps;
        int sqrtOriginalMaps;
        int windowPixels;
        int x;
        int y;
        int z;
        double sourceBuffer[];
        float floatBuffer[];
        int xStart;
        int xEnd;
        int yStart;
        int yEnd;
        int windowHalf;
        double sum;
        double mean;
        double neighborhoodBuffer[];
        int filterSize;
        double filterBuffer[][];
        int filterXDim;
        int filterYDim;
        int energyXDim;
        int energyYDim;
        int energySize;
        int xEnergyEnd;
        int yEnergyEnd;
        double energyBuffer[][];
        int finalIndex;
        int energyNum;
        
        if (windowSize == 3) {
        	originalMaps = 9;
        	sqrtOriginalMaps = 3;
        	finalMaps = 6;
        	windowPixels = 9;
        	mask = new int[originalMaps][windowPixels];
        	rowVal = new int[3];
        	colVal = new int[3];
        	for (i = 0, index = 0, finalIndex = 0; i <= 2; i++) {
        	    if (i == 0) {
        	        for (j = 0; j <= 2; j++) {
        	            rowVal[j] = L3[j];	
        	        }
        	        rowString = new String("L3");
        	    }
        	    else if (i == 1) {
        	    	for (j = 0; j <= 2; j++) {
        	    		rowVal[j] = E3[j];
        	    	}
        	    	rowString = new String("E3");
        	    }
        	    else {
        	    	for (j = 0; j <= 2; j++) {
        	    		rowVal[j] = S3[j];
        	    	}
        	    	rowString = new String("S3");
        	    }
        	    for (j = 0; j <= 2; j++, index++) {
        	        if (j == 0) {
        	            for (k = 0; k <= 2; k++) {
        	            	colVal[k] = L3[k];
        	            }
        	            colString = new String("L3");
        	        }
        	        else if (j == 1) {
        	        	for (k = 0; k <= 2; k++) {
        	        		colVal[k] = E3[k];
        	        	}
        	        	colString = new String("E3");
        	        }
        	        else {
        	        	for (k = 0; k <= 2; k++) {
        	        		colVal[k] = S3[k];
        	        	}
        	        	colString = new String("S3");
        	        }
        	        for (k = 0; k <= 2; k++) {
        	            for (m = 0; m <= 2; m++) {
        	            	mask[index][3*k + m] = rowVal[k]*colVal[m];
        	            }
        	        }
        	        if (j >= i) {
        	            label = new String(rowString+colString);
        	            destImage[finalIndex++].setImageName(srcImage.getImageName() + "_" + label);
        	        }
        	    } // for (j = 0; j <= 2; j++, index++)
        	} // for (i = 0, index = 0, finalIndex = 0; i <= 2; i++)
        } // if (windowSize == 3)
        else if (windowSize == 5) {
        	originalMaps = 25;
        	sqrtOriginalMaps = 5;
        	finalMaps = 15;
        	windowPixels = 25;
        	mask = new int[originalMaps][windowPixels];
        	rowVal = new int[5];
        	colVal = new int[5];
        	for (i = 0, index = 0, finalIndex = 0; i <= 4; i++) {
        	    if (i == 0) {
        	        for (j = 0; j <= 4; j++) {
        	            rowVal[j] = L5[j];	
        	        }
        	        rowString = new String("L5");
        	    }
        	    else if (i == 1) {
        	    	for (j = 0; j <= 4; j++) {
        	    		rowVal[j] = E5[j];
        	    	}
        	    	rowString = new String("E5");
        	    }
        	    else if (i == 2) {
        	    	for (j = 0; j <= 4; j++) {
        	    		rowVal[j] = S5[j];
        	    	}
        	    	rowString = new String("S5");
        	    }
        	    else if (i == 3) {
        	    	for (j = 0; j <= 4; j++) {
        	    		rowVal[j] = W5[j];
        	    	}
        	    	rowString = new String("W5");
        	    }
        	    else {
        	    	for (j = 0; j <= 4; j++) {
        	    		rowVal[j] = R5[j];
        	    	}
        	    	rowString = new String("R5");
        	    }
        	    for (j = 0; j <= 4; j++, index++) {
        	        if (j == 0) {
        	            for (k = 0; k <= 4; k++) {
        	            	colVal[k] = L5[k];
        	            }
        	            colString = new String("L5");
        	        }
        	        else if (j == 1) {
        	        	for (k = 0; k <= 4; k++) {
        	        		colVal[k] = E5[k];
        	        	}
        	        	colString = new String("E5");
        	        }
        	        else if (j == 2) {
        	        	for (k = 0; k <= 4; k++) {
        	        		colVal[k] = S5[k];
        	        	}
        	        	colString = new String("S5");
        	        }
        	        else if (j == 3) {
        	        	for (k = 0; k <= 4; k++) {
        	        		colVal[k] = W5[k];
        	        	}
        	        	colString = new String("W5");
        	        }
        	        else {
        	        	for (k = 0; k <= 4; k++) {
        	        		colVal[k] = R5[k];
        	        	}
        	        	colString = new String("R5");
        	        }
        	        for (k = 0; k <= 4; k++) {
        	            for (m = 0; m <= 4; m++) {
        	            	mask[index][5*k + m] = rowVal[k]*colVal[m];
        	            }
        	        }
        	        if (j >= i) {
        	        	label = new String(rowString+colString);
        	            destImage[finalIndex++].setImageName(srcImage.getImageName() + "_" + label);
        	        }
        	    } // for (j = 0; j <= 4; j++, index++)
        	} // for (i = 0, index = 0, finalIndex = 0; i <= 4; i++)	
        } // else if (windowSize == 5)
        else { // windowSize == 7
        	originalMaps = 36;
        	sqrtOriginalMaps = 6;
        	finalMaps = 21;
        	windowPixels = 49;
        	mask = new int[originalMaps][windowPixels];
        	rowVal = new int[7];
        	colVal = new int[7];
        	for (i = 0, index = 0, finalIndex = 0; i <= 5; i++) {
        	    if (i == 0) {
        	        for (j = 0; j <= 6; j++) {
        	            rowVal[j] = L7[j];	
        	        }
        	        rowString = new String("L7");
        	    }
        	    else if (i == 1) {
        	    	for (j = 0; j <= 6; j++) {
        	    		rowVal[j] = E7[j];
        	    	}
        	    	rowString = new String("E7");
        	    }
        	    else if (i == 2) {
        	    	for (j = 0; j <= 6; j++) {
        	    		rowVal[j] = S7[j];
        	    	}
        	    	rowString = new String("S7");
        	    }
        	    else if (i == 3) {
        	    	for (j = 0; j <= 6; j++) {
        	    		rowVal[j] = W7[j];
        	    	}
        	    	rowString = new String("W7");
        	    }
        	    else if (i == 4) {
        	    	for (j = 0; j <= 6; j++) {
        	    		rowVal[j] = R7[j];
        	    	}
        	    	rowString = new String("R7");
        	    }
        	    else {
        	    	for (j = 0; j <= 6; j++) {
        	    		rowVal[j] = O7[j];
        	    	}
        	    	rowString = new String("O7");
        	    }
        	    for (j = 0; j <= 5; j++, index++) {
        	        if (j == 0) {
        	            for (k = 0; k <= 6; k++) {
        	            	colVal[k] = L7[k];
        	            }
        	            colString = new String("L7");
        	        }
        	        else if (j == 1) {
        	        	for (k = 0; k <= 6; k++) {
        	        		colVal[k] = E7[k];
        	        	}
        	        	colString = new String("E7");
        	        }
        	        else if (j == 2) {
        	        	for (k = 0; k <= 6; k++) {
        	        		colVal[k] = S7[k];
        	        	}
        	        	colString = new String("S7");
        	        }
        	        else if (j == 3) {
        	        	for (k = 0; k <= 6; k++) {
        	        		colVal[k] = W7[k];
        	        	}
        	        	colString = new String("W7");
        	        }
        	        else if (j == 4) {
        	        	for (k = 0; k <= 6; k++) {
        	        		colVal[k] = R7[k];
        	        	}
        	        	colString = new String("R7");
        	        }
        	        else {
        	        	for (k = 0; k <= 6; k++) {
        	        		colVal[k] = O7[k];
        	        	}
        	        	colString = new String("O7");
        	        }
        	        for (k = 0; k <= 6; k++) {
        	            for (m = 0; m <= 6; m++) {
        	            	mask[index][7*k + m] = rowVal[k]*colVal[m];
        	            }
        	        }
        	        if (j >= i) {
        	        	label = new String(rowString+colString);
        	            destImage[finalIndex++].setImageName(srcImage.getImageName() + "_" + label);
        	        }
        	    } // for (j = 0; j <= 5; j++, index++)
        	} // for (i = 0, index = 0, finalIndex = 0; i <= 5; i++)		
        } // else windowSize == 7
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }
        
        sourceBuffer = new double[sliceSize];
        neighborhoodBuffer = new double[windowPixels];
        windowHalf = (windowSize - 1)/2;
        xStart = windowHalf;
        xEnd = xDim - 1 - windowHalf;
        yStart = windowHalf;
        yEnd = yDim - 1 - windowHalf;
        filterXDim = xEnd - xStart + 1;
        filterYDim = yEnd - yStart + 1;
        filterSize = filterXDim * filterYDim;
        filterBuffer = new double[originalMaps][filterSize];
        xEnergyEnd = filterXDim - 1 - windowHalf;
        yEnergyEnd = filterYDim - 1 - windowHalf;
        energyXDim = xEnergyEnd - xStart + 1;
        energyYDim = yEnergyEnd - yStart + 1;
        energySize = energyXDim * energyYDim;
        energyBuffer = new double[finalMaps][energySize];
        
        for (z = 0; z < zDim; z++) {
        	
            try {
            	if (srcImage.isColorImage()) {
                    floatBuffer = new float[sliceSize];
                    srcImage.exportRGBData(RGBOffset, 4*z*sliceSize, sliceSize, floatBuffer);  
                    for (i = 0; i < sliceSize; i++) {
                        sourceBuffer[i] = (double)floatBuffer[i];
                    }
                    floatBuffer = null;
                }
                else {
                    srcImage.exportData(z*sliceSize, sliceSize, sourceBuffer);
                }     
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmLawsTexture: IOException on srcImage.exportData(z*sliceSize,sliceSize,sourceBuffer)");
                setCompleted(false);

                return;
            }
            
            for (y = yStart; y <= yEnd; y++) {
            	for (x = xStart; x <= xEnd; x++) {
            	    index = x + y * xDim;
            	    sum = 0.0;
            	    for (j = -windowHalf; j <= windowHalf; j++) {
            	        for (i = -windowHalf; i <= windowHalf; i++) {
            	        	sum += sourceBuffer[index + j*xDim + i];
            	        } // for (i = -windowHalf; i <= windowHalf; i++)
            	    } // for (j = -windowHalf; j <= windowHalf; j++)
            	    mean = sum/windowPixels;
            	    
            	    for (j = -windowHalf; j <= windowHalf; j++) {
            	        for (i = -windowHalf; i <= windowHalf; i++) {
            	        	neighborhoodBuffer[(j+windowHalf)*windowSize + (i + windowHalf)] = sourceBuffer[index + j*xDim + i] - mean;
            	        } // for (i = -windowHalf; i <= windowHalf; i++)
            	    } // for (j = -windowHalf; j <= windowHalf; j++)
            	    
            	    for (k = 0; k < originalMaps; k++) {
            	        sum = 0.0;
            	        for (i = 0; i < windowPixels; i++) {
            	        	sum += mask[k][i] * neighborhoodBuffer[i];
            	        }
            	        filterBuffer[k][x - xStart + (filterXDim * (y - yStart))] = sum;
            	    } // for (k = 0; k < originalMaps; k++)
            	} // for (x = xStart; x <= xEnd; x++)
            } // for (y = yStart; y <= yEnd; y++)
            
            for (y = yStart; y <= yEnergyEnd; y++) {
                for (x = xStart; x <= xEnergyEnd; x++) {
                	index = x + filterXDim * y;
                    for (i = 0, energyNum = 0; i < sqrtOriginalMaps; i++) {
                    	for (j = i; j < sqrtOriginalMaps; j++, energyNum++) {
                    	    k = i * sqrtOriginalMaps + j;
                    	    sum = 0.0;
                    	    for (n = -windowHalf; n <= windowHalf; n++) {
                    	    	for (m = -windowHalf; m <= windowHalf; m++) {
                    	    	    sum += Math.abs(filterBuffer[k][index + m + (n * filterXDim)]);	
                    	    	}
                    	    }
                    	    if (i != j) {
                    	        k = j * sqrtOriginalMaps + i;
                    	        for (n = -windowHalf; n <= windowHalf; n++) {
                        	    	for (m = -windowHalf; m <= windowHalf; m++) {
                        	    	    sum += Math.abs(filterBuffer[k][index + m + (n * filterXDim)]);	
                        	    	}
                        	    }
                    	        sum = sum/2.0;
                    	    } // if (i != j)
                    	    energyBuffer[energyNum][x - xStart + (y - yStart) * energyXDim] = sum;
                    	} // for (j = i; j <= sqrtOriginalMaps; j++, energyNum++)
                    } // for (i = 0, energyNum = 0; i < sqrtOriginalMaps; i++)
                } // for (x = xStart; x <= xEnergyEnd; x++)
            } // for (y = yStart; y <= yEnergyEnd; y++)
            
            for (k = 0; k < finalMaps; k++) {
                for (y = 0; y < energyYDim; y++) {
                    for (x = 0; x < energyXDim; x++) {
                    	sourceBuffer[x + 2*windowHalf + (y + 2 * windowHalf) * xDim] = energyBuffer[k][x + y * energyXDim];
                    }
                } // for (y = 0; y < energyYDim; y++)
                try {
                    destImage[k].importData(z*sliceSize, sourceBuffer, false);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmLawsTexture: IOException on destImage[" + k +
                                           "].importData(0,sourceBuffer,false)");
                    setCompleted(false);
    
                    return;
                }
            } // for (k = 0; k < finalMaps; k++)
        } // for (z = 0; z < zDim; z++)
        
        for (k = 0; k < finalMaps; k++) {
        	destImage[k].calcMinMax();
        }
        
        setCompleted(true);
        return;
    } // calculateLaws
	
}