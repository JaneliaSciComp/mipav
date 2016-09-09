package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

/**
 * 
 * @author ilb
 * Retiring for now because now make this algorithm work properly.
 * Reference:
 * 1.) Fast watershed algorithms: analysis and extensions by Bogdan P. Dobrin, Timo Viero, and Moncef
 * Gabbouj.
 * 2.) The Watershed Transform: Definitions, Algorithms, and Parallelization Strategies by 
 * Jos B.T.M. Roerdink and Arnold Meijster, Fundamentals Informaticae 41 (2001), pp. 187-228.
 * 
 * Reference 2 makes the following criticism about this algorithm:
 *  'Isolated' regions.  When computing the watershed transform, regions in the image may arise
 *  which are completely surrounded by watershed pixels.  In some implementations of watershed
 *  transforms by topographical distance, such regions may in fact become temporarily or
 *  permanently 'isolated'.  This is a defect of the particular implementation, since, according
 *  to Corollary 3.1, watershed pixels should be propagated.  Such 'problems' are often solved
 *  by ad hoc modifications of the implementation, which still do not correctly implement
 *  the definition.
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
	
	boolean doMeyer1 = false;
	boolean doMeyer2 = true;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmSplitAndMergeWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8, boolean limitBins,
			int binNumber) {
		super(destImage, srcImage);
		this.neighbor8 = neighbor8;
		this.limitBins = limitBins;
		this.binNumber = binNumber;
	}
	
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
    	int m;
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
    	indexValueItem p;
    	boolean uniqueLabelFound;
    	boolean labelFound;
    	boolean testpfifo = false;
    	boolean testfifo = false;
    	
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
        if (testpfifo) {
        	pfifo.add(new indexValueItem(0, 1.0));
        	pfifo.add(new indexValueItem(1, 100.0));
        	pfifo.add(new indexValueItem(2, 50.0));
        	pfifo.add(new indexValueItem(3, 20.0));
        	pfifo.add(new indexValueItem(4, 80.0));
        	while (!pfifo.isEmpty()) {
        		p = pfifo.poll();
        		i = p.getIndex();
        		double value = p.getValue();
        		System.out.println("i = " + i + " value = " + value);
        	}
        	return;
        } // if (testpfifo)
        if (testfifo) {
        	fifo.offer(5);
        	fifo.offer(0);
        	fifo.offer(100);
        	fifo.offer(1);
        	fifo.offer(2);
        	fifo.offer(-1);
        	while(!fifo.isEmpty()) {
        		i = fifo.poll();
        		System.out.println("i = " + i);
        	}
        	return;
        }
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
            
            // Algorithm 1. Detection and labeling of minima
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
        					else if (labelBuffer[allNeighbors[j][k]] == NARM) { /* A beta(Mi) pixel */
        						if (doMeyer1) {
        							labelBuffer[allNeighbors[j][k]] = currentLabel;
        						}
        						else {
        						    labelBuffer[allNeighbors[j][k]] = INOQ;
        						}
        						pfifo.add(new indexValueItem(allNeighbors[j][k],imgBuffer[allNeighbors[j][k]]));
        					}
        				}
        			} // while (!fifo.isEmpty())
            	} // if (labelBuffer[i] == INIT)
            } // for (i = 0; i < length; i++)
            
            if (doMeyer1) {
            	while (!pfifo.isEmpty()) {
	            	p = pfifo.poll();
	            	i = p.getIndex();
	            	foundNeighbors = allNeighbors[i].length;
	            	for (j = 0; j < foundNeighbors; j++) {
            	    	if (labelBuffer[allNeighbors[i][j]] == NARM) {
            	    		labelBuffer[allNeighbors[i][j]] = labelBuffer[i];
            	    		pfifo.add(new indexValueItem(allNeighbors[i][j],imgBuffer[allNeighbors[i][j]]));
            	    	}
            	    }
            	} // while (!pfifo.isEmpty())
            }
            else if (doMeyer2){ // Meyer2
	            // Meyer2 watershed algorithm with Algorithm 4. split-and-merge placed after the assignment of WSHED.
	            while (!pfifo.isEmpty()) {
	            	p = pfifo.poll();
	            	i = p.getIndex();
	            	foundNeighbors = allNeighbors[i].length;
	            	exists = false;
	        	    loop1: for (j = 0; j < foundNeighbors-1; j++) {
	        	        for (k = j+1; k < foundNeighbors; k++) {
	        	        	if ((labelBuffer[allNeighbors[i][j]] > 0) && (labelBuffer[allNeighbors[i][k]] > 0) &&
	        	        			(labelBuffer[allNeighbors[i][j]] != labelBuffer[allNeighbors[i][k]])) {
	        	        		labelBuffer[i] = WSHED;
	        	        		exists = true;
	        	        		break loop1;
	        	        	}
	        	        }
	        	    } // loop1: for (j = 0; j < foundNeighbors-1; j++)
	            	if (exists) {
	            	    for (j = 0; j < foundNeighbors; j++) {
	            	    	if (labelBuffer[allNeighbors[i][j]] == NARM) { /* an unprocessed pixel */
	            	    		labelFound = false;
	            	    		labelBuffer[allNeighbors[i][j]] = PIAP;
	            	    		added = fifo.offer(allNeighbors[i][j]);
	    			    		if (!added) {
	                				MipavUtil.displayError("Failure to add " + allNeighbors[i][j] + " to the fifo");
	                				setCompleted(false);
	                				return;
	                			}
	    			    		while ((!labelFound) && (!fifo.isEmpty())) {
	    			    		    k = fifo.poll();
	    			    		    for (m = 0; m < allNeighbors[k].length; m++) {
	    			    		    	if ((labelBuffer[allNeighbors[k][m]] == NARM) && 
	    			    		    			(imgBuffer[allNeighbors[k][m]] == imgBuffer[allNeighbors[i][j]])) {
	    			    		    		labelBuffer[allNeighbors[k][m]] = PIAP;
	    			    		    		added = fifo.offer(allNeighbors[k][m]);
	    			    		    		if (!added) {
	    		                				MipavUtil.displayError("Failure to add " + allNeighbors[k][m] + " to the fifo");
	    		                				setCompleted(false);
	    		                				return;
	    		                			}
	    			    		    	}
	    			    		    	else if ((labelBuffer[allNeighbors[k][m]] == NARM) && 
	    			    		    			(imgBuffer[allNeighbors[k][m]] < imgBuffer[allNeighbors[i][j]])) {
	    			    		    		labelFound = true;
	    			    		    	}
	    			    		    	else if ((labelBuffer[allNeighbors[k][m]] == INOQ) && 
	    			    		    			(imgBuffer[allNeighbors[k][m]] <= imgBuffer[allNeighbors[i][j]])) {
	    			    		    		labelFound = true;
	    			    		    	}
	    			    		    } // for (m = 0; m < allNeighbors[k].length; m++)
	    			    		} // while ((!labelFound) && (!fifo.isEmpty()))
	    			    		if (!labelFound) { /* an isolated area I detected */
	    			    			currentLabel++;
	    			    			labelBuffer[allNeighbors[i][j]] = currentLabel;
	    			    			added = fifo.offer(allNeighbors[i][j]);
	        			    		if (!added) {
	                    				MipavUtil.displayError("Failure to add " + allNeighbors[i][j] + " to the fifo");
	                    				setCompleted(false);
	                    				return;
	                    			}
	        			    		while (!fifo.isEmpty()) {
	        			    		    k = fifo.poll();
	        			    		    for (m = 0; m < allNeighbors[k].length; m++) {
	        			    		        if (labelBuffer[allNeighbors[k][m]] == PIAP) {
	        			    		            labelBuffer[allNeighbors[k][m]] = currentLabel;
	        			    		            added = fifo.offer(allNeighbors[k][m]);
	        			    		    		if (!added) {
	        		                				MipavUtil.displayError("Failure to add " + allNeighbors[k][m] + " to the fifo");
	        		                				setCompleted(false);
	        		                				return;
	        		                			}
	        			    		        } // if (labelBuffer[allNeighbors[k][m]] == PIAP)
	        			    		        // Put pixels of the set beta(I) to the priority queue
	        			    		        else if (labelBuffer[allNeighbors[k][m]] == NARM) {
	        			    		        	pfifo.add(new indexValueItem(allNeighbors[k][m],imgBuffer[allNeighbors[k][m]]));
	        			    		        	labelBuffer[allNeighbors[k][m]] = INOQ;
	        			    		        } // else if (labelBuffer[allNeighbors[k][m]] == NARM)
	        			    		    } // for (m = 0; m < allNeighbors[k].length; m++)
	        			    		} // while (!fifo.isEmpty())
	    			    		} // if (!labelFound)
	    			    		else { /* restoring the original state of the area */
	    			    		    fifo.clear();
	    			    		    labelBuffer[allNeighbors[i][j]] = NARM;
	    			    		    added = fifo.offer(allNeighbors[i][j]);
	        			    		if (!added) {
	                    				MipavUtil.displayError("Failure to add " + allNeighbors[i][j] + " to the fifo");
	                    				setCompleted(false);
	                    				return;
	                    			}
	        			    		while (!fifo.isEmpty()) {
	        			    		    k = fifo.poll();
	        			    		    for (m = 0; m < allNeighbors[k].length; m++) {
	        			    		        if (labelBuffer[allNeighbors[k][m]] == PIAP) {
	        			    		            labelBuffer[allNeighbors[k][m]] = NARM;
	        			    		            added = fifo.offer(allNeighbors[k][m]);
	        			    		    		if (!added) {
	        		                				MipavUtil.displayError("Failure to add " + allNeighbors[k][m] + " to the fifo");
	        		                				setCompleted(false);
	        		                				return;
	        		                			}
	        			    		        } // if (labelBuffer[allNeighbors[k][m]] == PIAP)
	        			    		    } // for (m = 0; m < allNeighbors[k].length; m++)
	        			    		} // while (!fifo.isEmpty())
	    			    		} // else restoring the original state of the area
	            	    	} // if (labelBuffer[allNeighbors[i][j]] == NARM)
	            	    } // for (j = 0; j < foundNeighbor; j++)
	            	} // if (exists)
	            	else { // !exists
	            		uniqueLabelFound = false;
	            	    for (j = 0; j < foundNeighbors; j++) {
	            	    	if ((!uniqueLabelFound) && (labelBuffer[allNeighbors[i][j]] > 0)) {
	            	    		uniqueLabelFound = true;
	            	    		labelBuffer[i] = labelBuffer[allNeighbors[i][j]];
	            	    	}
	            	    	else if (labelBuffer[allNeighbors[i][j]] == NARM) {
	            	    		labelBuffer[allNeighbors[i][j]] = INOQ;
	            	    		pfifo.add(new indexValueItem(allNeighbors[i][j],imgBuffer[allNeighbors[i][j]]));
	            	    	}
	            	    } // for (j = 0; j < foundNeighbors; j++)
	            	} // else !exists
	            } // while (!pfifo.isEmpty())
            } // else if (doMeyer2)
            
            
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
            	return -1;
            }
            else if (a > b) {
            	return 1;
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