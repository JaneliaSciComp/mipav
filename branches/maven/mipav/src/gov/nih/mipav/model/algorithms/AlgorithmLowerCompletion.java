package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

/**
 * 
 * @author ilb
 * 1.) The Watershed Transform: Definitions, Algorithms, and Parallelization Strategies by 
 * Jos B.T.M. Roerdink and Arnold Meijster, Fundamentals Informaticae 41 (2001), pp. 187-228.
 * Algorithm 4.5 Algorithm for lowercompletion using a FIFO queue.
 * A valued graph is called lower complete when each node which is not in a minimum has a
 * neighboring node of lower value.  If an image is lower complete, each pixel which is not in
 * a minimum has a neighbor of lower grey value.  This algorithm transforms an image to a lower
 * complete image.  Lower complete images are often used as inputs to watershed transforms.
 * */

public class AlgorithmLowerCompletion extends AlgorithmBase {
	
    private int numNeighbor;
	
	private boolean limitBins;
	
	private int binNumber;
	
	private boolean minBuffer[][];
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmLowerCompletion(ModelImage destImage, ModelImage srcImage, int numNeighbor, boolean limitBins,
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
    	int intBuffer[];
    	int labelBuffer[] = null;
    	int dist;
    	final int fictitious = -100;
    	int x;
    	int y;
    	int z;
    	int t;
    	int i;
    	int j;
    	boolean exists;
    	boolean added;
    	Queue <Integer> fifo = new LinkedList<Integer>();
    	int foundNeighbors;
    	int neighbors[];
    	int allNeighbors[][];
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Lower Completion ...");
    	
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
            intBuffer = new int[length];
            labelBuffer = new int[length];
            minBuffer = new boolean[tDim*zDim][length];
            allNeighbors = new int[length][];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Lower Completion: Out of memory creating buffers");
            setCompleted(false);

            return;
        }
        
        for (i = 0; i < length; i++) {
        	x = i % xDim;
        	y = i / xDim;
        	foundNeighbors = 0;
        	if (x > 0) {
        		neighbors[foundNeighbors++] = i-1;
        	}
        	if (x < xDim-1) {
        		neighbors[foundNeighbors++] = i+1;
        	}
        	if (y > 0) {
        		neighbors[foundNeighbors++] = i-xDim;
        	}
        	if (y < yDim-1) {
        		neighbors[foundNeighbors++] = i+xDim;
        	}
        	if (numNeighbor == 8) {
        		if ((x > 0) && (y > 0)) {
        			neighbors[foundNeighbors++] = i-xDim-1;
        		}
        		if ((x > 0) && (y < yDim-1)) {
        			neighbors[foundNeighbors++] = i+xDim-1;
        		}
        		if ((x < xDim-1) && ( y > 0)) {
        			neighbors[foundNeighbors++] = i-xDim+1;
        		}
        		if ((x < xDim-1) && (y < yDim-1)) {
        			neighbors[foundNeighbors++] = i+xDim+1;
        		}
        	} // if (neighbor8)
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
                displayError("Algorithm Lower Completion: image bounds exceeded");
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
        	    	intBuffer[i] = Math.min((binNumber-1), (int)Math.floor((imgBuffer[i]-minValue)*scale + 0.5));
        	    }
            } // if (limitBins)
            else {
            	for (i = 0; i < length; i++) {
            		intBuffer[i] = (int)Math.round(imgBuffer[i]);
            		minBuffer[t*zDim+z][i] = false;
            	}
            }
          
            fifo.clear();
            
            // Initialize queue with pixels that have a lower neighbor
            for (i = 0; i < length; i++) {
            	labelBuffer[i] = 0;
            	foundNeighbors = allNeighbors[i].length;
        	    exists = false;
        	    for (j = 0; j < foundNeighbors; j++) {
        	    	if (imgBuffer[allNeighbors[i][j]] < imgBuffer[i]) {
        	    		exists = true;
        	    		break;
        	    	}
        	    } // for (j = 0; j < foundNeighbors; j++)
        	    // If imgBuffer[i] has a lower neighbor
        	    if (exists) {
        	    	added = fifo.offer(i);
        			if (!added) {
        				MipavUtil.displayError("Failure to add " + i + " to the fifo");
        				setCompleted(false);
        				return;
        			}
        			labelBuffer[i] = -1;
        	    } // if (exists)
            } // for (i = 0; i < length; i++)
            dist = 1;
            // Insert fictitious pixel
            added = fifo.offer(fictitious);
            if (!added) {
				MipavUtil.displayError("Failure to add fictitious pixel to the fifo");
				setCompleted(false);
				return;
			}
            while (!fifo.isEmpty()) {
			    i = fifo.poll();
			    if (i == fictitious) {
			    	if (!fifo.isEmpty()) {
			    		added = fifo.offer(fictitious);
			            if (!added) {
							MipavUtil.displayError("Failure to add fictitious pixel to the fifo");
							setCompleted(false);
							return;
						}
			            dist++;
			    	} // if (!fifo.isEmpty())
			    } // if (i == fictitious)
			    else {
			        labelBuffer[i] = dist;
			        foundNeighbors = allNeighbors[i].length;
			        for (j = 0; j < foundNeighbors; j++) {
	        	    	if ((imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) && (labelBuffer[allNeighbors[i][j]] == 0)) {
	        	    	    added = fifo.offer(allNeighbors[i][j]);
	        	    	    if (!added) {
								MipavUtil.displayError("Failure to add pixel allNeighbors["+i+"]["+j+"] to the fifo");
								setCompleted(false);
								return;
							}
	        	    	    // To prevent from queueing twice
	        	    	    labelBuffer[allNeighbors[i][j]] = -1;
	        	    	} // if ((imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) && (labelBuffer[allNeighbors[i][j]] == 0)) 
	        	    } // for (j = 0; j < foundNeighbors; j++)
			    } // else
            } // while (!fifo.isEmpty())
            
            // Put the lower complete values in the output image
            for (i = 0; i < length; i++) {
            if (labelBuffer[i] != 0) {
            	labelBuffer[i] = dist * intBuffer[i] + labelBuffer[i] - 1;
            } // if (labelBuffer[i] != 0)
            else {
                labelBuffer[i] = dist * intBuffer[i];	
                minBuffer[t*zDim+z][i] = true;
            } // else
            } // for (i = 0; i < length; i++)
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
    	int intBuffer[];
    	int labelBuffer[] = null;
    	int dist;
    	final int fictitious = -100;
    	int x;
    	int y;
    	int z;
    	int t;
    	int i;
    	int j;
    	boolean exists;
    	boolean added;
    	Queue <Integer> fifo = new LinkedList<Integer>();
    	int foundNeighbors;
    	int neighbors[];
    	int allNeighbors[][];
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	int sliceSize;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Lower Completion ...");
    	
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
            intBuffer = new int[length];
            labelBuffer = new int[length];
            minBuffer = new boolean[tDim][length];
            allNeighbors = new int[length][];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Lower Completion: Out of memory creating buffers");
            setCompleted(false);

            return;
        }
        
        for (i = 0; i < length; i++) {
    		x = i % xDim;
    		y = (i % sliceSize) / xDim;
    		z = i / sliceSize;
    		foundNeighbors = 0;
    		if (x > 0) {
    			neighbors[foundNeighbors++] = i-1;
    		}
    		if (x < xDim-1) {
    			neighbors[foundNeighbors++] = i+1;
    		}
    		if (y > 0) {
    		    neighbors[foundNeighbors++] = i-xDim;	
    		} // if (y > 0)
    		if (y < yDim-1) {
    		    neighbors[foundNeighbors++] = i+xDim;
    		} // if (y < yDim-1)
    		if (z > 0) {
    			neighbors[foundNeighbors++] = i-sliceSize;
    		}
    		if (z < zDim-1) {
    			neighbors[foundNeighbors++] = i + sliceSize;
    		}
    		if ((numNeighbor == 18) || (numNeighbor == 26)) {
    		    if ((x > 0) && (y > 0)) {
    		        neighbors[foundNeighbors++] = i-xDim-1;
    		    } // if ((x > 0) && (y > 0))
    		    if ((x < xDim-1) && (y > 0)) {
    		       neighbors[foundNeighbors++] = i-xDim+1;
    		    } // if ((x < xDim-1) && (y > 0))
    		    if ((x > 0) & ( y < yDim-1)) {
    		       neighbors[foundNeighbors++] = i+xDim-1;
    		    } // if ((x > 0) & ( y < yDim-1))
    		    if ((x < xDim-1) & ( y < yDim-1)) {
    		       neighbors[foundNeighbors++] = i+xDim+1;
    		    } // if ((x < xDim-1) & ( y < yDim-1))
    		    if ((x > 0) && (z > 0)) {
    		    	neighbors[foundNeighbors++] = i - sliceSize - 1;
    		    }
    		    if ((x < xDim-1) && (z > 0)) {
    		    	neighbors[foundNeighbors++] = i - sliceSize + 1;
    		    }
    		    if ((x > 0) && (z < zDim-1)) {
    		    	neighbors[foundNeighbors++] = i + sliceSize - 1;
    		    }
    		    if ((x < xDim-1) && (z < zDim-1)) {
    		    	neighbors[foundNeighbors++] = i + sliceSize + 1;
    		    }
    		    if ((y > 0) && (z > 0)) {
    		    	neighbors[foundNeighbors++] = i - sliceSize - xDim;
    		    }
    		    if ((y < yDim-1) && (z > 0)) {
    		    	neighbors[foundNeighbors++] = i - sliceSize + xDim;
    		    }
    		    if ((y > 0) && (z < zDim-1)) {
    		    	neighbors[foundNeighbors++] = i + sliceSize - xDim;
    		    }
    		    if ((y < yDim-1) && (z < zDim-1)) {
    		    	neighbors[foundNeighbors++] = i + sliceSize + xDim;
    		    }
    		    if (numNeighbor == 26) {
                    if ((x > 0) && (y > 0) && (z > 0)) {
                    	neighbors[foundNeighbors++] = i - sliceSize - xDim - 1;
                    }
                    if ((x < xDim-1) && (y > 0) && (z > 0)) {
                    	neighbors[foundNeighbors++] = i - sliceSize - xDim + 1;
                    }
                    if ((x > 0) && (y < yDim-1) && (z > 0)) {
                    	neighbors[foundNeighbors++] = i - sliceSize + xDim - 1;
                    }
                    if ((x < xDim-1) && (y < yDim-1) && (z > 0)) {
                    	neighbors[foundNeighbors++] = i - sliceSize + xDim + 1;
                    }
                    if ((x > 0) && (y > 0) && (z < zDim-1)) {
                    	neighbors[foundNeighbors++] = i + sliceSize - xDim - 1;
                    }
                    if ((x < xDim-1) && (y > 0) && (z < zDim-1)) {
                    	neighbors[foundNeighbors++] = i + sliceSize - xDim + 1;
                    }
                    if ((x > 0) && (y < yDim-1) && (z < zDim-1)) {
                    	neighbors[foundNeighbors++] = i + sliceSize + xDim - 1;
                    }
                    if ((x < xDim-1) && (y < yDim-1) && (z < zDim-1)) {
                    	neighbors[foundNeighbors++] = i + sliceSize + xDim + 1;
                    }
    		    } // if (numNeighbor == 26)
    		} // if ((numNeighbor == 18) || (numNeighbor == 26))
			allNeighbors[i] = new int[foundNeighbors];
			for (j = 0; j < foundNeighbors; j++) {
				allNeighbors[i][j] = neighbors[j];
			}
        } // for (i = 0; i < length; i++)
        
        


        for (t = 0; t < tDim; t++) {

            try {
                srcImage.exportData(t*length, length, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm Lower Completion: image bounds exceeded");
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
        	    	intBuffer[i] = Math.min((binNumber-1), (int)Math.floor((imgBuffer[i]-minValue)*scale + 0.5));
        	    }
            } // if (limitBins)
            else {
            	for (i = 0; i < length; i++) {
            		intBuffer[i] = (int)Math.round(imgBuffer[i]);
            		minBuffer[t][i] = false;
            	}
            }
          
            fifo.clear();
            
            // Initialize queue with pixels that have a lower neighbor
            for (i = 0; i < length; i++) {
            	labelBuffer[i] = 0;
            	foundNeighbors = allNeighbors[i].length;
        	    exists = false;
        	    for (j = 0; j < foundNeighbors; j++) {
        	    	if (imgBuffer[allNeighbors[i][j]] < imgBuffer[i]) {
        	    		exists = true;
        	    		break;
        	    	}
        	    } // for (j = 0; j < foundNeighbors; j++)
        	    // If imgBuffer[i] has a lower neighbor
        	    if (exists) {
        	    	added = fifo.offer(i);
        			if (!added) {
        				MipavUtil.displayError("Failure to add " + i + " to the fifo");
        				setCompleted(false);
        				return;
        			}
        			labelBuffer[i] = -1;
        	    } // if (exists)
            } // for (i = 0; i < length; i++)
            dist = 1;
            // Insert fictitious pixel
            added = fifo.offer(fictitious);
            if (!added) {
				MipavUtil.displayError("Failure to add fictitious pixel to the fifo");
				setCompleted(false);
				return;
			}
            while (!fifo.isEmpty()) {
			    i = fifo.poll();
			    if (i == fictitious) {
			    	if (!fifo.isEmpty()) {
			    		added = fifo.offer(fictitious);
			            if (!added) {
							MipavUtil.displayError("Failure to add fictitious pixel to the fifo");
							setCompleted(false);
							return;
						}
			            dist++;
			    	} // if (!fifo.isEmpty())
			    } // if (i == fictitious)
			    else {
			        labelBuffer[i] = dist;
			        foundNeighbors = allNeighbors[i].length;
			        for (j = 0; j < foundNeighbors; j++) {
	        	    	if ((imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) && (labelBuffer[allNeighbors[i][j]] == 0)) {
	        	    	    added = fifo.offer(allNeighbors[i][j]);
	        	    	    if (!added) {
								MipavUtil.displayError("Failure to add pixel allNeighbors["+i+"]["+j+"] to the fifo");
								setCompleted(false);
								return;
							}
	        	    	    // To prevent from queueing twice
	        	    	    labelBuffer[allNeighbors[i][j]] = -1;
	        	    	} // if ((imgBuffer[allNeighbors[i][j]] == imgBuffer[i]) && (labelBuffer[allNeighbors[i][j]] == 0)) 
	        	    } // for (j = 0; j < foundNeighbors; j++)
			    } // else
            } // while (!fifo.isEmpty())
            
            // Put the lower complete values in the output image
            for (i = 0; i < length; i++) {
            if (labelBuffer[i] != 0) {
            	labelBuffer[i] = dist * intBuffer[i] + labelBuffer[i] - 1;
            } // if (labelBuffer[i] != 0)
            else {
                labelBuffer[i] = dist * intBuffer[i];	
                minBuffer[t][i] = true;
            } // else
            } // for (i = 0; i < length; i++)
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
	
	public boolean[][] getMinBuffer() {
		return minBuffer;
	}
	
}