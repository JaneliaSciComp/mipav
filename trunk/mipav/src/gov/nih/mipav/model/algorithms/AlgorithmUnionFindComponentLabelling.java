package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

/**
 * 
 * @author ilb
 * 1.) The Watershed Transform: Definitions, Algorithms, and Parallelization Strategies by 
 * Jos B.T.M. Roerdink and Arnold Meijster, Fundamentals Informaticae 41 (2001), pp. 187-228.
 * Algorithm 4.7 Scan-line algorithm for labelling level components based on disjoint sets.
 * The UNION-FIND algorithm for implementing disjoint sets can be used for computing connected
 * components, and therefore for minima detection as well.  This algorithm can be used for the 
 * computation of connected components in images of any dimension, size, and connectivity.
 * regional maxima Connected set of pixels of constant intensity from which it is impossible to reach
 * a point with higher intensity without first descending; that is, a connected component of pixels with
 * the same intensity value, t, surrounded by pixels that all have a value less than t.
 * regional minima Connected set of pixels of constant intensity from which it is impossible to reach a
 * point with lower intensity without first ascending; that is, a connected component of pixels with the
 * same intensity value, t, surrounded by pixels that all have a value greater than t.
*/

public class AlgorithmUnionFindComponentLabelling extends AlgorithmBase {
	
    private int numNeighbor;
	
	boolean limitBins;
	
	int binNumber;
	
	int parentBuffer[];
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmUnionFindComponentLabelling(ModelImage destImage, ModelImage srcImage, int numNeighbor, boolean limitBins,
			int binNumber) {
		super(destImage, srcImage);
		this.numNeighbor = numNeighbor;
		this.limitBins = limitBins;
		this.binNumber = binNumber;
	}
	
	public void runAlgorithm() {
	    if ((numNeighbor == 4) || (numNeighbor == 8)) {
	    	run2D();
	    }
	    else {
	    	run3D();
	    }
	}
	
	public void run2D() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int nDims;
    	int length;
    	double imgBuffer[];
    	int labelBuffer[] = null;
    	int x;
    	int y;
    	int x2;
    	int y2;
    	int z;
    	int t;
    	int i;
    	int yi;
    	int xi;
    	int j;
    	int r;
    	int r2;
    	int foundNeighbors;
    	int neighbors[];
    	int allNeighbors[][];
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	int curlab;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Union Find Component Labelling ...");
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        nDims = srcImage.getNDims();
    	
        neighbors = new int[numNeighbor];
        
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
            labelBuffer = new int[length];
            parentBuffer = new int[length];
            allNeighbors = new int[length][];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Union Find Component Labelling: Out of memory creating buffers");
            setCompleted(false);

            return;
        }
        
        for (i = 0; i < length; i++) {
        	x = i % xDim;
        	y = i / xDim;
        	foundNeighbors = 0;
        	// The neighbor must have either
        	// 1.) A x value less than the original pixel x value
        	// 2.) A x value equal to the original pixel x value and
        	//     a y value less than the original pixel y value
        	if (x > 0) {
        		neighbors[foundNeighbors++] = i-1;
        	}
        	if (y > 0) {
        		neighbors[foundNeighbors++] = i-xDim;
        	}
        	if (numNeighbor == 8) {
        		if ((x > 0) && (y > 0)) {
        			neighbors[foundNeighbors++] = i-xDim-1;
        		}
        		if ((x > 0) && (y < yDim-1)) {
        			neighbors[foundNeighbors++] = i+xDim-1;
        		}
        	} // if (numNeighbor == 8)
        	allNeighbors[i] = new int[foundNeighbors];
        	for (j = 0; j < foundNeighbors; j++) {
        		allNeighbors[i][j] = neighbors[j];
        	}
        }


        for (t = 0; t < tDim; t++) {
        for (z = 0; z < zDim; z++) {

            try {
                srcImage.exportData((z + t*zDim)*length, length, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm Union Find Component Labelling: image bounds exceeded");
                setCompleted(false);
                
                srcImage.releaseLock();

                return;
            }
            
            if (limitBins) {
            	minValue = Double.MAX_VALUE;
            	maxValue = -Double.MAX_VALUE;
            	for (i = 0; i < length; i++) {
            	    if (imgBuffer[i] < minValue) {
            	    	minValue = imgBuffer[i];
            	    }
            	    if (imgBuffer[i] > maxValue) {
            	    	maxValue = imgBuffer[i];
            	    }
            	}
            	
            	range = maxValue - minValue;
        	    scale = (binNumber-1)/range;
        	    for (i = 0; i < length; i++) {
        	    	imgBuffer[i] = Math.min((binNumber-1), Math.floor((imgBuffer[i]-minValue)*scale + 0.5));
        	    }
            } // if (limitBins)
            
            for (i = 0; i < length; i++) {
            	parentBuffer[i] = i;
            }
            
            // Initialize queue with pixels that have a lower neighbor
            for (xi = 0; xi < xDim; xi++) {
                for (yi = 0; yi < yDim; yi++) {
            	    i = xi + yi * xDim;
	            	r = i;
	            	foundNeighbors = allNeighbors[i].length;
	        	    for (j = 0; j < foundNeighbors; j++) {
	        	    	if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) {
	        	    	   r2 =  findRoot(allNeighbors[i][j]);
	        	    	   // Obtain the minimum of r and r2 with respect to lexicographical order
	        	    	   x = r % xDim;
	        	    	   y = r / xDim;
	        	    	   x2 = r2 % xDim;
	        	    	   y2 = r2 / xDim;
	        	    	   if ((x2 < x) || ((x2 == x) && (y2 < y))) {
	        	    		   r = r2;
	        	    	   }
	        	    	} // if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) 
	        	    } // for (j = 0; j < foundNeighbors; j++)
	        	    parentBuffer[i] = r;
	        	    for (j = 0; j < foundNeighbors; j++) {
	        	    	if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) {
	        	    	    pathCompress(allNeighbors[i][j], r);	
	        	    	} // if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i])
	        	    } // for (j = 0; j < foundNeighbors; j++)'
                } // for (yi = 0; yi < yDim; yi++)
            } // for (xi = 0; xi < xDim; xi++)
            
            // curlab is the current label
            curlab = 1;
            for (xi = 0; xi < xDim; xi++) {
                for (yi = 0; yi < yDim; yi++) {
            	    i = xi + yi * xDim;
	            	if (parentBuffer[i] == i) {
	            	    // i is a root pixel
	            		labelBuffer[i] = curlab;
	            		curlab++;
	            	}
	            	else {
	            		// Resolve unresolved equivalences
	            		parentBuffer[i] = parentBuffer[parentBuffer[i]];
	            		labelBuffer[i] = labelBuffer[parentBuffer[i]];
	            	}
                } // for (yi = 0; yi < yDim; yi++)
            } // for (xi = 0; xi < xDim; xi++)
            
            try {
			    destImage.importData((z + t*zDim)*length, labelBuffer, false);
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
	
	public void run3D() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int nDims;
    	int length;
    	double imgBuffer[];
    	int labelBuffer[] = null;
    	int x;
    	int y;
    	int x2;
    	int y2;
    	int z2;
    	int z;
    	int t;
    	int i;
    	int zi;
    	int yi;
    	int xi;
    	int j;
    	int r;
    	int r2;
    	int foundNeighbors;
    	int neighbors[];
    	int allNeighbors[][];
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	int curlab;
    	int sliceSize;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Union Find Component Labelling ...");
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        length = sliceSize * zDim;
        nDims = srcImage.getNDims();
    	
        neighbors = new int[numNeighbor];
        
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        
        try {
            imgBuffer = new double[length];
            labelBuffer = new int[length];
            parentBuffer = new int[length];
            allNeighbors = new int[length][];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Union Find Component Labelling: Out of memory creating buffers");
            setCompleted(false);

            return;
        }
        
        for (i = 0; i < length; i++) {
        	x = i % xDim;
        	y = (i % sliceSize) / xDim;
            z = i / sliceSize;
        	foundNeighbors = 0;
        	// The neighbor must have either
        	// 1.) A x value less than the original pixel x value
        	// 2.) A x value equal to the original pixel x value and
        	//     a y value less than the original pixel y value
        	// 3.) A x value equal to the original pixel x value,
        	//     a y value equal to the original pixel y value
        	//     a z value less than the original pixel z value.
        	if (x > 0) {
        		neighbors[foundNeighbors++] = i-1;
        	}
        	if (y > 0) {
        		neighbors[foundNeighbors++] = i-xDim;
        	}
        	if (z > 0) {
        		neighbors[foundNeighbors++] = i-sliceSize;
        	}
        	if (numNeighbor >= 18) {
        		if ((x > 0) && (y > 0)) {
        			neighbors[foundNeighbors++] = i-xDim-1;
        		}
        		if ((x > 0) && (y < yDim-1)) {
        			neighbors[foundNeighbors++] = i+xDim-1;
        		}
        		if ((x > 0) && (z > 0)) {
        			neighbors[foundNeighbors++] = i-sliceSize-1;
        		}
        		if ((x > 0) && (z < zDim-1)) {
        			neighbors[foundNeighbors++] = i+sliceSize-1;
        		}
        		if ((y > 0) && (z > 0)) {
        			neighbors[foundNeighbors++] = i - sliceSize - xDim;
        		}
        		if ((y > 0) && (z < zDim-1)) {
        			neighbors[foundNeighbors++] = i + sliceSize - xDim;
        		}
        		if (numNeighbor == 26) {
        			if ((x > 0) && (y > 0) && (z > 0)) {
        				neighbors[foundNeighbors++] = i - sliceSize - xDim - 1;
        			}
        			if ((x > 0) && (y > 0) && (z < zDim-1)) {
        				neighbors[foundNeighbors++] = i + sliceSize - xDim - 1;
        			}
        			if ((x > 0) && (y < yDim-1) && (z > 0)) {
        				neighbors[foundNeighbors++] = i - sliceSize + xDim - 1;
        			}
        			if ((x > 0) && (y < yDim-1) && (z < zDim-1)) {
        				neighbors[foundNeighbors++] = i + sliceSize + xDim - 1;
        			}
        		} // if (numNeighbor == 26)
        	} // if (numNeighbor >= 18)
        	allNeighbors[i] = new int[foundNeighbors];
        	for (j = 0; j < foundNeighbors; j++) {
        		allNeighbors[i][j] = neighbors[j];
        	}
        }


        for (t = 0; t < tDim; t++) {

            try {
                srcImage.exportData(t*length, length, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm Union Find Component Labelling: image bounds exceeded");
                setCompleted(false);
                
                srcImage.releaseLock();

                return;
            }
            
            if (limitBins) {
            	minValue = Double.MAX_VALUE;
            	maxValue = -Double.MAX_VALUE;
            	for (i = 0; i < length; i++) {
            	    if (imgBuffer[i] < minValue) {
            	    	minValue = imgBuffer[i];
            	    }
            	    if (imgBuffer[i] > maxValue) {
            	    	maxValue = imgBuffer[i];
            	    }
            	}
            	
            	range = maxValue - minValue;
        	    scale = (binNumber-1)/range;
        	    for (i = 0; i < length; i++) {
        	    	imgBuffer[i] = Math.min((binNumber-1), Math.floor((imgBuffer[i]-minValue)*scale + 0.5));
        	    }
            } // if (limitBins)
            
            for (i = 0; i < length; i++) {
            	parentBuffer[i] = i;
            }
            
            // Initialize queue with pixels that have a lower neighbor
            for (xi = 0; xi < xDim; xi++) {
                for (yi = 0; yi < yDim; yi++) {
                	for (zi = 0; zi < zDim; zi++) {
	            	    i = xi + yi * xDim + zi * sliceSize;
		            	r = i;
		            	foundNeighbors = allNeighbors[i].length;
		        	    for (j = 0; j < foundNeighbors; j++) {
		        	    	if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) {
		        	    	   r2 =  findRoot(allNeighbors[i][j]);
		        	    	   // Obtain the minimum of r and r2 with respect to lexicographical order
		        	    	   x = r % xDim;
		        	    	   y = (r % sliceSize) / xDim;
		        	    	   z = r / sliceSize;
		        	    	   x2 = r2 % xDim;
		        	    	   y2 = (r2 % sliceSize) / xDim;
		        	    	   z2 = r2 / sliceSize;
		        	    	   if ((x2 < x) || ((x2 == x) && (y2 < y)) || ((x2 == x) && (y2 == y) && (z2 < z))) {
		        	    		   r = r2;
		        	    	   }
		        	    	} // if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) 
		        	    } // for (j = 0; j < foundNeighbors; j++)
		        	    parentBuffer[i] = r;
		        	    for (j = 0; j < foundNeighbors; j++) {
		        	    	if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) {
		        	    	    pathCompress(allNeighbors[i][j], r);	
		        	    	} // if (imgBuffer[allNeighbors[i][j]] == imgBuffer[i])
		        	    } // for (j = 0; j < foundNeighbors; j++)'
                	} // for (zi = 0; zi < zDim; zi++)
                } // for (yi = 0; yi < yDim; yi++)
            } // for (xi = 0; xi < xDim; xi++)
            
            // curlab is the current label
            curlab = 1;
            for (xi = 0; xi < xDim; xi++) {
                for (yi = 0; yi < yDim; yi++) {
                	for (zi = 0; zi < zDim; zi++) {
	            	    i = xi + yi * xDim + zi * sliceSize;
		            	if (parentBuffer[i] == i) {
		            	    // i is a root pixel
		            		labelBuffer[i] = curlab;
		            		curlab++;
		            	}
		            	else {
		            		// Resolve unresolved equivalences
		            		parentBuffer[i] = parentBuffer[parentBuffer[i]];
		            		labelBuffer[i] = labelBuffer[parentBuffer[i]];
		            	}
                	} // for (zi = 0; zi < zDim; zi++)
                } // for (yi = 0; yi < yDim; yi++)
            } // for (xi = 0; xi < xDim; xi++)
            
            try {
			    destImage.importData(t*length, labelBuffer, false);
			}
			catch(IOException e) {
				MipavUtil.displayError("IOException " + e + " on destImage.importData");
				setCompleted(false);
				return;
			}
        } // for (t = 0; t < tDim; t++)
        destImage.calcMinMax();
        
        setCompleted(true);
        return;
	}
	
	private int findRoot(int p) {
		int r = p;
		while (parentBuffer[p] != p) {
		    r = parentBuffer[p];
		    p = r;
		}
		return r;
	}
	
	private void pathCompress(int p, int r) {
		int h;
		while (parentBuffer[p] != r) {
		    h = parentBuffer[p];
		    parentBuffer[p] = r;
		    p = h;
		}
	}
}