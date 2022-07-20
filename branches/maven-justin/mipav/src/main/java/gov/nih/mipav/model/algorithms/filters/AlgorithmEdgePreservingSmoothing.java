package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewJProgressBar;

import java.io.*;

import java.util.*;

/**
 * These are Maximum Homogeneity Neighbor Filters.
 * The standard from Nagao:
 * At each point the variance of 9 masks are compared with each other, and the average gray level
 * of the least variance mask is given to the point.  There is one traditional 3 by 3 mask, an up 
 * 7 point pentagonal mask, a down 7 point pentagonal mask, a left 7 point pentagonal mask, a
 * right 7 point pentagonal mask, and 4 diagonal 7 point hexagonal masks.
 * Extended From Wang:
 * The 9 masks used all have 9 points.  5 are traditional 3 by 3 masks.  4 are 5,3,1 stacks.
 * @author ilb
 * References: 1.) Edge Preserving Smoothing by Makoto Nagao and Takashi Matsuyama, Proceedings of the
 * Fourth International Joint Conference on Pattern Recognition, November, 1978, pp. 518-520.
 * 2.) A New Approach to Edge-Preserving Smoothing for Edge Extraction and Image Segmentation by
 * Carsten Garnica, Frank Boochs, and Marek Twardochlib, International Archives of Photogrammetry
 * and Remote Sensing, Vol. XXXIII, Part B3., Amsterdam, 2000.
 * 
 *
 */

public class AlgorithmEdgePreservingSmoothing extends AlgorithmBase {
	private int iterations;
	private boolean entireImage;
	/** contains VOI. */
    private BitSet mask = null;
    private boolean standard;
	
	public AlgorithmEdgePreservingSmoothing(ModelImage destImg, ModelImage srcImg, boolean standard, int iters, boolean entireImage) {
		super(destImg, srcImg);
		this.standard = standard;
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
        int region;
        double minVariance;
        double mean;
        double regionVariance;
        int xoff;
        int yoff;
        double sum;
        double sumOfSquares;
        double var;
        int extendedOffset;
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Filtering ...");
        
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
        if (standard) {
        	extendedOffset = 0;
        }
        else {
        	extendedOffset = 8;
        }
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
        	    for (iter = 0; iter < iterations; iter++) {
        	    	if (iter >= 1) {
        	    		temp = buffer;
        	    		buffer = result;
        	    		result = temp;
        	    	}
        	    	for (y = 0; y < yDim; y++) {
        	    		for (x = 0; x < xDim; x++) {
        	    		    index = x + y * xDim;
        	    		    if (entireImage || mask.get(index)) {
        	    		    	minVariance = Double.MAX_VALUE;
        	    		    	mean = 0.0;
        	    		        for (region = extendedOffset; region < extendedOffset + 9; region++) {
        	    		        	sum = 0.0;
        	    		        	sumOfSquares = 0.0;
        	    		            switch(region) {
        	    		            case 0:
        	    		            	if ((x >= 1) && (x < xDim-1) && (y >= 2)) {
        	    		            		for (yoff = -2; yoff <= -1; yoff++) {
        	    		            	    	for (xoff = -1; xoff <= 1; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = -2; yoff <= -1; yoff++)
        	    		            		var = buffer[x + xDim*y];
        	    		            		sum += var;
        	    		            		sumOfSquares += var*var;
        	    		            		regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x >= 1) && (x < xDim-1) && (y >= 2))
        	    		            	break;
        	    		            case 1:
        	    		            	if ((x >= 1) && (x < xDim-1) && (y < yDim-2)) {
        	    		            		for (yoff = 1; yoff <= 2; yoff++) {
        	    		            	    	for (xoff = -1; xoff <= 1; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = 1; yoff <= 2; yoff++)
        	    		            		var = buffer[x + xDim*y];
        	    		            		sum += var;
        	    		            		sumOfSquares += var*var;
        	    		            		regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x >= 1) && (x < xDim-1) && (y < yDim-2))
        	    		            	break;
        	    		            case 2:
        	    		            	if ((x >= 2) && (y >= 1) && (y < yDim-1)) {
        	    		            		for (yoff = -1; yoff <= 1; yoff++) {
        	    		            	    	for (xoff = -2; xoff <= -1; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = -1; yoff <= 1; yoff++)
        	    		            		var = buffer[x + xDim*y];
        	    		            		sum += var;
        	    		            		sumOfSquares += var*var;
        	    		            		regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x >= 2) && (y >= 1) && (y < yDim-1))
        	    		            	break;
        	    		            case 3:
        	    		            	if ((x < xDim-2) && (y >= 1) && (y < yDim-1)) {
        	    		            		for (yoff = -1; yoff <= 1; yoff++) {
        	    		            	    	for (xoff = 1; xoff <= 2; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = -1; yoff <= 1; yoff++)
        	    		            		var = buffer[x + xDim*y];
        	    		            		sum += var;
        	    		            		sumOfSquares += var*var;
        	    		            		regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x < xDim-2) && (y >= 1) && (y < yDim-1))
        	    		            	break;
        	    		            case 4:
        	    		            	if ((x >= 2) && (y >= 2)) {
        	    		            	    for (yoff = -2; yoff <= 0; yoff++) {
        	    		            	        for (xoff = -2; xoff <= 0; xoff++) {
        	    		            	        	if ((xoff == -2 && yoff == 0) || (xoff == 0 && yoff == -2)) {
        	    		            	        		continue;
        	    		            	        	}
        	    		            	        	else {
        	    		            	        		var = buffer[(x + xoff) + xDim*(y + yoff)];
             	    		            	    	    sum += var;
             	    		            	    	    sumOfSquares += var*var;	
        	    		            	        	}
        	    		            	        }
        	    		            	    } // for (yoff = -2; yoff <= 0; yoff++)
        	    		            	    regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x >= 2) && (y >= 2))
        	    		            	break;
        	    		            case 5:
        	    		            	if ((x < xDim-2) && (y >= 2)) {
        	    		            	    for (yoff = -2; yoff <= 0; yoff++) {
        	    		            	        for (xoff = 0; xoff <= 2; xoff++) {
        	    		            	        	if ((xoff == 2 && yoff == 0) || (xoff == 0 && yoff == -2)) {
        	    		            	        		continue;
        	    		            	        	}
        	    		            	        	else {
        	    		            	        		var = buffer[(x + xoff) + xDim*(y + yoff)];
             	    		            	    	    sum += var;
             	    		            	    	    sumOfSquares += var*var;	
        	    		            	        	}
        	    		            	        }
        	    		            	    } // for (yoff = -2; yoff <= 0; yoff++)
        	    		            	    regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x < xDim-2) && (y >= 2))
        	    		            	break;
        	    		            case 6:
        	    		            	if ((x >= 2) && (y < yDim-2)) {
        	    		            	    for (yoff = 0; yoff <= 2; yoff++) {
        	    		            	        for (xoff = -2; xoff <= 0; xoff++) {
        	    		            	        	if ((xoff == -2 && yoff == 0) || (xoff == 0 && yoff == 2)) {
        	    		            	        		continue;
        	    		            	        	}
        	    		            	        	else {
        	    		            	        		var = buffer[(x + xoff) + xDim*(y + yoff)];
             	    		            	    	    sum += var;
             	    		            	    	    sumOfSquares += var*var;	
        	    		            	        	}
        	    		            	        }
        	    		            	    } // for (yoff = 0; yoff <= 2; yoff++)
        	    		            	    regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x >= 2) && (y < yDim-2))
        	    		            	break;
        	    		            case 7:
        	    		            	if ((x < xDim-2) && (y < yDim-2)) {
        	    		            	    for (yoff = 0; yoff <= 2; yoff++) {
        	    		            	        for (xoff = 0; xoff <= 2; xoff++) {
        	    		            	        	if ((xoff == 2 && yoff == 0) || (xoff == 0 && yoff == 2)) {
        	    		            	        		continue;
        	    		            	        	}
        	    		            	        	else {
        	    		            	        		var = buffer[(x + xoff) + xDim*(y + yoff)];
             	    		            	    	    sum += var;
             	    		            	    	    sumOfSquares += var*var;	
        	    		            	        	}
        	    		            	        }
        	    		            	    } // for (yoff = 0; yoff <= 2; yoff++)
        	    		            	    regionVariance = (sumOfSquares - sum*sum/7.0)/6.0;
            	    		            	if (regionVariance < minVariance) {
            	    		            		minVariance = regionVariance;
            	    		            		mean = sum/7.0;
            	    		            	}
        	    		            	} // if ((x < xDim-2) && (y < yDim-2))
        	    		            	break;
        	    		            case 8:
        	    		            	if ((x >= 1) && (x < xDim-1) && (y >= 1) && (y < yDim-1)) {
        	    		            	    for (yoff = -1; yoff <= 1; yoff++) {
        	    		            	    	for (xoff = -1; xoff <= 1; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = -1; yoff <= 1; yoff++)
	        	    		            	regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x >= 1) && (x < xDim-1) && (y >= 1) && (y < yDim-1))
        	    		            	break;
        	    		            case 9:
        	    		            	if ((x >= 2) && (y >= 2)) {
        	    		            	    for (yoff = -2; yoff <= 0; yoff++) {
        	    		            	    	for (xoff = -2; xoff <= 0; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = -2; yoff <= 0; yoff++)
	        	    		            	regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x >= 2) && (y >= 2))
        	    		            	break;
        	    		            case 10:
        	    		            	if ((x >= 2) && (y < yDim-2)) {
        	    		            	    for (yoff = 0; yoff <= 2; yoff++) {
        	    		            	    	for (xoff = -2; xoff <= 0; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = 0; yoff <= 2; yoff++)
	        	    		            	regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x >= 2) && (y < yDim-2))
        	    		            	break;
        	    		            case 11:
        	    		            	if ((x < xDim-2) && (y >= 2)) {
        	    		            	    for (yoff = -2; yoff <= 0; yoff++) {
        	    		            	    	for (xoff = 0; xoff <= 2; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = -2; yoff <= 0; yoff++)
	        	    		            	regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x < xDim-2) && (y >= 2))
        	    		            	break;
        	    		            case 12:
        	    		            	if ((x < xDim-2) && (y < yDim-2)) {
        	    		            	    for (yoff = 0; yoff <= 2; yoff++) {
        	    		            	    	for (xoff = 0; xoff <= 2; xoff++) {
        	    		            	    	    var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;
        	    		            	    	}
        	    		            	    } // for (yoff = 0; yoff <= 2; yoff++)
	        	    		            	regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x < xDim-2) && (y < yDim-2))
        	    		            	break;
        	    		            case 13:
        	    		            	if ((x >= 2) && (x < xDim-2) && (y >= 2)) {
        	    		            	    for (yoff = -2; yoff <= -1; yoff++) {
        	    		            	        for (xoff = -1; xoff <= 1; xoff++) {
        	    		            	        	var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;	
        	    		            	        }
        	    		            	    } // for (yoff = -2; yoff <= -1; yoff++)
        	    		            	    var = buffer[(x-2) + xDim*(y-2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[(x+2) + xDim*(y-2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[x + xDim*y];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x >= 2) && (x < xDim-2) && (y >= 2))
        	    		            	break;
        	    		            case 14:
        	    		            	if ((x >= 2) && (y >= 2) && (y < yDim-2)) {
        	    		            	    for (yoff = -1; yoff <= 1; yoff++) {
        	    		            	        for (xoff = -2; xoff <= -1; xoff++) {
        	    		            	        	var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;	
        	    		            	        }
        	    		            	    } // for (yoff = -1; yoff <= 1; yoff++)
        	    		            	    var = buffer[(x-2) + xDim*(y-2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[(x-2) + xDim*(y+2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[x + xDim*y];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x >= 2) && (y >= 2) && (y < yDim-2))
        	    		            	break;
        	    		            case 15:
        	    		            	if ((x >= 2) && (x < xDim-2) && (y < yDim-2)) {
        	    		            	    for (yoff = 1; yoff <= 2; yoff++) {
        	    		            	        for (xoff = -1; xoff <= 1; xoff++) {
        	    		            	        	var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;	
        	    		            	        }
        	    		            	    } // for (yoff = 1; yoff <= 2; yoff++)
        	    		            	    var = buffer[(x-2) + xDim*(y+2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[(x+2) + xDim*(y+2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[x + xDim*y];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x >= 2) && (x < xDim-2) && (y < yDim-2))
        	    		            	break;
        	    		            case 16:
        	    		            	if ((x < xDim-2) && (y >= 2) && (y < yDim-2)) {
        	    		            	    for (yoff = -1; yoff <= 1; yoff++) {
        	    		            	        for (xoff = 1; xoff <= 2; xoff++) {
        	    		            	        	var = buffer[(x + xoff) + xDim*(y + yoff)];
        	    		            	    	    sum += var;
        	    		            	    	    sumOfSquares += var*var;	
        	    		            	        }
        	    		            	    } // for (yoff = -1; yoff <= 1; yoff++)
        	    		            	    var = buffer[(x+2) + xDim*(y-2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[(x+2) + xDim*(y+2)];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    var = buffer[x + xDim*y];
        	    		            	    sum += var;
        	    		            	    sumOfSquares += var*var;
        	    		            	    regionVariance = (sumOfSquares - sum*sum/9.0)/8.0;
	        	    		            	if (regionVariance < minVariance) {
	        	    		            		minVariance = regionVariance;
	        	    		            		mean = sum/9.0;
	        	    		            	}
        	    		            	} // if ((x < xDim-2) && (y >= 2) && (y < yDim-2))
        	    		            	break;
        	    		            } // switch(region)
        	    		        } // for (region = extendedOffset; region < extendedOffset + 9; region++)
        	    		        result[index] = mean;
        	    		    } // if (entireImage || mask.get(index))
        	    		    else {
        	    		    	result[index] = buffer[index];
        	    		    }
        	    		} // for (x = 0; x < xDim; x++)
        	    	} // for (y = 0; y < yDim; y++)
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