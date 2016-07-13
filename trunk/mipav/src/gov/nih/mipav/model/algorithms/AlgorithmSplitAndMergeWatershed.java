package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

/**
 * 
 * @author ilb
 * Reference:
 * 1.) Fast watershed algorithms: analysis and extensions by Bogdan P. Dobrin, Timo Viero, and Moncef
 * Gabbouj.
 */

public class AlgorithmSplitAndMergeWatershed extends AlgorithmBase {
	
	/* Value of a watershed pixel */
	private static final int WSHED = 0;
	
	/* Not a Regional Minimum, An unprocessed pixel*/
	private static final int NARM= -1;
	
	/* IN Ordered Queue */
	private static final int INOQ = -2;
	
	/* Initial value of the output image */
	private static final int INIT = -3;
	
	/* Potential Isolated Area Pixel */
	private static final int PIAP = -4;
	
	private boolean neighbor8;
	
	boolean limitBins;
	
	int binNumber;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmSplitAndMergeWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8, boolean limitBins,
			int binNumber) {
		super(destImage, srcImage);
		this.neighbor8 = neighbor8;
		this.limitBins = limitBins;
		this.binNumber = binNumber;
	}
	
	@SuppressWarnings("unchecked")
	public void runAlgorithm() {
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
    	int z;
    	int t;
    	int i;
    	int j;
    	int k = 0;
    	int currentLabel;
    	Queue <Integer> fifo = new LinkedList<Integer>();
    	Comparator<indexValueItem> comparator = new indexValueComparator();
    	PriorityQueue<indexValueItem> pfifo;
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	int numNeighbor;
    	int neighbors[];
    	int allNeighbors[][];
    	int foundNeighbors = 0;
    	boolean exists;
    	boolean added;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Split And Merge Watershed ...");
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        pfifo = new PriorityQueue<indexValueItem>(length, comparator);
        nDims = srcImage.getNDims();
        if (neighbor8) {
        	numNeighbor = 8;
        }
        else {
        	numNeighbor = 4;
        }
    	
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
            allNeighbors = new int[length][];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Split And Merge Watershed: Out of memory creating buffers");
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
        	if (neighbor8) {
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
                displayError("Algorithm Split And Merge Watershed: image bounds exceeded");
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
        	    	imgBuffer[i] = Math.min((double)(binNumber-1), Math.floor((imgBuffer[i]-minValue)*scale + 0.5));
        	    }
            } // if (limitBins)
            
            for (i = 0; i < length; i++) {
            	labelBuffer[i] = INIT;
            }
            
          
            fifo.clear();
            
            for (i = 0; i < length; i++) {
            	if (labelBuffer[i] == INIT) {
            	    foundNeighbors = allNeighbors[i].length;
            	    exists = false;
            	    for (j = 0; j < foundNeighbors; j++) {
            	    	if (imgBuffer[allNeighbors[i][j]] < imgBuffer[i]) {
            	    		exists = true;
            	    		break;
            	    	}
            	    } // for (j = 0; j < foundNeighbors; j++)
            	    if (exists) {
            	    	labelBuffer[i] = NARM;
            	    	added = fifo.offer(i);
            			if (!added) {
            				MipavUtil.displayError("Failure to add " + i + " to the fifo");
            				setCompleted(false);
            				return;
            			}
            			while (!fifo.isEmpty()) {
            			    j = fifo.poll();
            			    for (k = 0; k < allNeighbors[j].length; k++) {
            			    	if ((labelBuffer[allNeighbors[j][k]] == INIT) && (imgBuffer[allNeighbors[j][k]] == imgBuffer[i])) {
            			    		labelBuffer[allNeighbors[j][k]] = NARM;
            			    		added = fifo.offer(allNeighbors[j][k]);
            			    		if (!added) {
                        				MipavUtil.displayError("Failure to add " + allNeighbors[j][k] + " to the fifo");
                        				setCompleted(false);
                        				return;
                        			}
            			    	}
            			    }
            			} // while (!fifo.isEmpty())
            	    } // if (exists)
            	} // if (labelBuffer[i] == INIT)
            } // for (i = 0; i < length; i++)
            
            currentLabel = 0;
            pfifo.clear();
            
            for (i = 0; i < length; i++) {
            	if (labelBuffer[i] == INIT) {
            	    currentLabel++;
            	    labelBuffer[i] = currentLabel;
            	    added = fifo.offer(i);
        			if (!added) {
        				MipavUtil.displayError("Failure to add " + i + " to the fifo");
        				setCompleted(false);
        				return;
        			}
        			while (!fifo.isEmpty()) {
        				j = fifo.poll();
        				for (k = 0; k < allNeighbors[j].length; k++) {
        					if (labelBuffer[allNeighbors[j][k]] == INIT) {
        						labelBuffer[allNeighbors[j][k]] = currentLabel;
        						added = fifo.offer(allNeighbors[j][k]);
        			    		if (!added) {
                    				MipavUtil.displayError("Failure to add " + allNeighbors[j][k] + " to the fifo");
                    				setCompleted(false);
                    				return;
                    			}
        					}
        					else if (labelBuffer[allNeighbors[j][k]] == NARM) {
        						labelBuffer[allNeighbors[j][k]] = INOQ;
        						pfifo.add(new indexValueItem(allNeighbors[j][k],imgBuffer[allNeighbors[j][k]]));
        					}
        				}
        			} // while (!fifo.isEmpty())
            	} // if (labelBuffer[i] == INIT)
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
	
	private class indexValueComparator implements Comparator<indexValueItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(indexValueItem o1, indexValueItem o2) {
        	double a = o1.getValue();
            double b = o2.getValue();
            if (a < b) {
            	return 1;
            }
            else if (a > b) {
            	return -1;
            }
            else {
            	return 0;
            }
        }
	}
        
    private class indexValueItem {
		private int index;
		private double value;
		
		public indexValueItem(int index, double value) {
			this.index = index;
			this.value = value;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getValue() {
			return value;
		}
		
		
	}

	
	
}