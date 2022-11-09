package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewJProgressBar;

import java.io.*;

import java.util.*;
/**
 * 
 * @author ilb
 * Reference: A new class of edge-preserving smoothing filters by David Harwood, Muralidhara Subbarao, 
 * Hannu Hakalahti, and Larry S. Davis, Pattern recognition Letters, Vol. 6, 1987, pp. 155-162.
 *
 */

public class AlgorithmSymmetricNearestNeighbor extends AlgorithmBase {
	private int iterations;
	private boolean entireImage;
	/** contains VOI. */
    private BitSet mask = null;
    // If true do median, otherwise do mean
    private boolean doMedian;
    private int windowSize;
	
	public AlgorithmSymmetricNearestNeighbor(ModelImage destImg, ModelImage srcImg, boolean doMedian, int windowSize,
			int iters, boolean entireImage) {
		super(destImg, srcImg);
		this.doMedian = doMedian;
		this.windowSize = windowSize;
		iterations = iters;
		this.entireImage = entireImage;
		if (!entireImage) {
            mask = srcImage.generateVOIMask();
        }
	}
	
	/**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	int iter;
    	int x;
    	int y;
    	int index;
    	int z;
        int t;
        int xDim;
        int yDim;
        int sliceSize;
        int zDim;
        int tDim;
        double buffer[];
        double result[];
        double temp[];
        int halfWin = (windowSize - 1)/2;
        int yoff;
        int xoff;
        int numPairs = (windowSize*windowSize - 1)/2;
        int currentPair;
        double valueSelected[];
        double currentVal;
        double val1;
        double val2;
        double valDiff1;
        double valDiff2;
        int lowerIndex = (numPairs-2)/2;
        int higherIndex = lowerIndex + 1;
        double sum;
        int i;
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Filtering ...");
        
        valueSelected = new double[numPairs];
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        if (srcImage.getNDims() > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        buffer = new double[sliceSize];
        result = new double[sliceSize];
        
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
        	    try {
        	    	srcImage.exportData((t*zDim + z)*sliceSize, sliceSize, buffer); 
        	    }
        	    catch (IOException e) {
                    displayError("IOException on srcImage.exportData");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                }
        	    for (y = 0; y < halfWin; y++) {
        	    	for (x = 0; x < xDim; x++) {
        	    		index = x + y * xDim;
        	    		result[index] = buffer[index];
        	    	}
        	    }
        	    for (y = yDim - halfWin; y < yDim; y++) {
        	    	for (x = 0; x < xDim; x++) {
        	    		index = x + y * xDim;
        	    		result[index] = buffer[index];
        	    	}
        	    }
        	    for (y = halfWin; y < yDim - halfWin; y++) {
        	    	for (x = 0; x < halfWin; x++) {
        	    		index = x + y * xDim;
        	    		result[index] = buffer[index];
        	    	}
        	    	for (x = xDim - halfWin; x < xDim; x++) {
        	    		index = x + y * xDim;
        	    		result[index] = buffer[index];
        	    	}
        	    }
        	    for (iter = 0; iter < iterations; iter++) {
        	    	if (iter >= 1) {
        	    		temp = buffer;
        	    		buffer = result;
        	    		result = temp;
        	    	}
        	    	for (y = halfWin; y < yDim - halfWin; y++) {
        	    		for (x = halfWin; x < xDim - halfWin; x++) {
        	    		    index = x + y * xDim;
        	    		    if (entireImage || mask.get(index)) {
        	    		    	currentPair = 0;
        	    		    	currentVal = buffer[index];
        	    		        for (yoff = -halfWin; yoff <= halfWin; yoff++) {
        	    		            for (xoff = -halfWin; xoff <= 0; xoff++) {
        	    		                if ((yoff <= 0) && (xoff == 0)) {
        	    		                	continue;
        	    		                }
        	    		                else {
        	    		                	val1 = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		                	val2 = buffer[(x - xoff) + xDim*(y - yoff)];
        	    		                	valDiff1 = Math.abs(currentVal - val1);
        	    		                	valDiff2 = Math.abs(currentVal - val2);
        	    		                	if (valDiff1 < valDiff2) {
        	    		                		valueSelected[currentPair++] = val1;
        	    		                	}
        	    		                	else if (valDiff1 > valDiff2) {
        	    		                		valueSelected[currentPair++] = val2;
        	    		                	}
        	    		                	else {
        	    		                		valueSelected[currentPair++] = currentVal;
        	    		                	}
        	    		                }
        	    		            } // for (xoff = -halfWin; xoff <= 0; xoff++)
        	    		        } // for (yoff = -halfWin; yoff <= halfWin; yoff++)
        	    		        if (doMedian) {
        	    		        	Arrays.sort(valueSelected);
        	    		        	result[index] = (valueSelected[lowerIndex] + valueSelected[higherIndex])/2.0;
        	    		        }
        	    		        else {
        	    		            sum = 0.0;
        	    		            for (i = 0; i < numPairs; i++) {
        	    		                sum += valueSelected[i];	
        	    		            }
        	    		            result[index] = sum/numPairs;
        	    		        }
        	    		    } // if (entireImage || mask.get(index))
        	    		    else {
        	    		    	result[index] = buffer[index];
        	    		    }
        	    		} // for (x = halfWin; x < xDim - halfWin; x++)
        	    	} // for (y = halfWin; y < yDim - halfWin; y++)
        	    } // for (iter = 0; iter < iterations; iter++)
        	    if (destImage != null) {
        	    	try {
        	    		destImage.importData((t*zDim + z)*sliceSize, result, false);
        	    	}
        	    	catch (IOException e) {
        	    		displayError("IOException on destImage.importData");
        	    		setCompleted(false);
        	    		fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        	    		return;
        	    	}
        	    } // if (destImage != null)
        	    else {
        	    	try {
        	    		srcImage.importData((t*zDim + z)*sliceSize, result, false);
        	    	}
        	    	catch (IOException e) {
        	    		displayError("IOException on srcImage.importData");
        	    		setCompleted(false);
        	    		fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        	    		return;
        	    	}	
        	    }
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        if (destImage != null) {
        	destImage.calcMinMax();
        }
        else {
        	srcImage.calcMinMax();
        }
        setCompleted(true);
        return;
    }
}