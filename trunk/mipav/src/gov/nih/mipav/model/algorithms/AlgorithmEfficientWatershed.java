package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

/**
 * 
 * @author ilb
 * Reference:
 * 1.) Image Processing, Analysis, and Machine Vision 4th edition International Edition by Milan Sonka,
 * Vaclav Hlavac, and Roger Boyle, Section 6.3.4 Watershed segmentation, pp. 229-232.
 * 2.) Watersheds in Digital Spaces: An Efficient Algorithm Based on Immersion Simulations by Luc Vincent
 * and Pierre Soille, IEEE Transactions on Pattern Analysis and Machine Intelligence, Vol. 13, No. 6,
 * June, 1991, pp. 583-598.
 * 3.) Fast watershed algorithms: analysis and extensions by Bogdan P. Dobrin, Timo Viero, and Moncef
 * Gabbouj.
 * The ImageJ version has 3 differences with the original article pseudocode as noted in comments.
 * 
 * Reference 1 states "Further improvements of the watershed segmentation based on immersion simulations
 * are given in [Dobrin et al., 1994]."  Reference 3 states: "Vincent & Soille algorithm produces a tesselation
 * of the image into its disjoint catchment basins.  Pixels lying exactly half-way between the two catchment
 * basins, should be assigned to watersheds.  Studying the operation of Vincent and Soille algorithms, we will
 * show that this is not always the case.
 *     The algorithm has a tendency to generate large watershed regions, because it does not propagate only
 * catchment basin labels but also watershed labels.... Therefore, the algorithm produces an ambiguous result
 * when propagating watershed lines to a region.
 *     Another drawback of this algorithm is given by the oscillation in the pixel assignment rule. It is relatively
 * easy to see ... that the value of the current pixel, i.e. the pixel whose value is to be determined, can
 * change multiple times depending on its local neighborhood.  For example, the current pixel can firstly be
 * assigned to watersheds and then immediately to a catchment basin label or vice versa.  As a result, the operation
 * of the algorithm is inconsistent in several critical pixel configurations."
 */

public class AlgorithmEfficientWatershed extends AlgorithmBase {
	
	/* Initial value of a threshold level */
	private static final int MASK = -2;
	
	/* Value of the pixels belonging to the watersheds */
	private static final int WSHED = 0;
	
	/* Initial value for labelBuffer */
	private static final int INIT = -1;
	
	
	private boolean neighbor8;
	
	boolean limitBins;
	
	int binNumber;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmEfficientWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8, boolean limitBins,
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
    	int distanceBuffer[] = null;
    	int x;
    	int y;
    	int z;
    	int t;
    	int i;
    	int j;
    	int k = 0;
    	int m;
    	int numValues;
    	double histBins[];
    	int indexBins[][];
    	int neighborBins[][];
    	int lasti;
    	int currentLabel;
    	int currentDist;
    	ArrayList <indexValueItem> indexValueList = new ArrayList<indexValueItem>();
    	ArrayList <Integer> frequencyCount = new ArrayList<Integer>();
    	int ip;
    	int index;
    	boolean exists;
    	boolean added;
    	Queue <Integer> fifo = new LinkedList<Integer>();
    	int fictitiousIndex = -3;
    	int indexn;
    	int indexn2 = 0;
    	int numNeighbor;
    	int foundNeighbors;
    	int neighbors[];
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	boolean test = false;
    	
    	if (test) {
    	    indexValueList.add(new indexValueItem(5, 23.0));
    	    indexValueList.add(new indexValueItem(2, -33.0));
    	    indexValueList.add(new indexValueItem(65, 1.25));
    	    indexValueList.add(new indexValueItem(34, -6.3));
    	    indexValueList.add(new indexValueItem(31, 7.89));
    	    indexValueList.add(new indexValueItem(23, 62.1));
    	    indexValueList.add(new indexValueItem(11, 5.6));
    	    Collections.sort(indexValueList, new indexValueComparator());
    	    for (i = 0; i < indexValueList.size(); i++) {
    	    	indexValueItem item = indexValueList.get(i);
    	    	System.out.println("index = " + item.getIndex() + " value = " + item.getValue());
    	    }
    	    setCompleted(true);
    	    return;
    	}
    	
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
        if (neighbor8) {
        	numNeighbor = 8;
        }
        else {
        	numNeighbor = 4;
        }
    	
        neighbors = new int[numNeighbor];
        neighborBins = new int[length][];
        
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
    		} // if (y > 0)
    		if (y < yDim-1) {
    		    neighbors[foundNeighbors++] = i+xDim;
    		} // if (y < yDim-1)
    		if (neighbor8) {
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
    		} // if (neighbor8)
			neighborBins[i] = new int[foundNeighbors];
			for (j = 0; j < foundNeighbors; j++) {
				neighborBins[i][j] = neighbors[j];
			}
        } // for (i = 0; i < numValues; i++)
        
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
            distanceBuffer = new int[length];
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
          
            fifo.clear();
            
            for (i = 0; i < length; i++) {
            	indexValueList.add(new indexValueItem(i, imgBuffer[i]));	
            }
            
            Collections.sort(indexValueList, new indexValueComparator());
            numValues = 1;
            lasti = 0;
            for (i = 1; i < length; i++) {
            	if (indexValueList.get(i).getValue() > indexValueList.get(i-1).getValue()) {
            		numValues++;
            		frequencyCount.add(i-lasti);
            		lasti = i;
            	}
            }
            frequencyCount.add(length-lasti);
            histBins = new double[numValues];
            indexBins = new int[numValues][];
            
            for (i = 0; i < frequencyCount.size(); i++) {
            	indexBins[i] = new int[frequencyCount.get(i)];
            }
            histBins[0] = indexValueList.get(0).getValue();
            indexBins[0][0] = indexValueList.get(0).getIndex();
            for (i = 1, j = 0, ip = 1; i < length; i++) {
            	if (indexValueList.get(i).getValue() > indexValueList.get(i-1).getValue()) {
            		histBins[++j] = indexValueList.get(i).getValue();
            		ip = 0;
            		indexBins[j][ip++] = indexValueList.get(i).getIndex();
            	}
            	else {
            		indexBins[j][ip++] = indexValueList.get(i).getIndex();
            	}
            }
            indexValueList.clear();
            frequencyCount.clear();
            
            
            for (i = 0; i < length; i++) {
            	labelBuffer[i] = INIT;
            	distanceBuffer[i] = 0;
            }
            currentLabel = 0;
            
            for (i = 0; i < numValues; i++) {
            	// geodesic SKIZ of level h-1 inside level h
            	for (j = 0; j < indexBins[i].length; j++) {
            		index = indexBins[i][j];
            		labelBuffer[index] = MASK;
            		exists = false;
        			for (k = 0; k < neighborBins[index].length && (!exists); k++) {
        				indexn = neighborBins[index][k];
        				// ImageJ code has if (labelBuffer[indexn] >= 0)
        				if ((labelBuffer[indexn] > 0) || (labelBuffer[indexn] == WSHED)) {
        					exists = true;
        				}
        			}
            		if (exists) {
            			distanceBuffer[index] = 1;
            			added = fifo.offer(index);
            			if (!added) {
            				MipavUtil.displayError("Failure to add " + index + " to the fifo");
            				setCompleted(false);
            				return;
            			}
            		} // if (exists)
            	} // for (j = 0; j < indexBins[i].length; j++)
            	
            	currentDist = 1;
            	added = fifo.offer(fictitiousIndex);
            	if (!added) {
    				MipavUtil.displayError("Failure to add fictitiousIndex to the fifo");
    				setCompleted(false);
    				return;
    			}
            	while (true) {
            		index = fifo.poll();
            		if (index == fictitiousIndex) {
            			if (fifo.isEmpty()) {
            				break;
            			}
            			else {
            				added = fifo.offer(fictitiousIndex);
            				if (!added) {
                				MipavUtil.displayError("Failure to add fictitiousIndex to the fifo");
                				setCompleted(false);
                				return;
                			}
            				currentDist++;
            				index = fifo.poll();
            			}
            		} // if (index == fictiousIndex)	
            		
        		    for (k = 0; k < neighborBins[index].length; k++) {
        		    	indexn = neighborBins[index][k];
        		
            			// ImageJ code has if ((distanceBuffer[indexn] <= curDist) && (labelBuffer[indexn] >= 0))
        		    	if ((distanceBuffer[indexn] < currentDist) && ((labelBuffer[indexn] > 0) || (labelBuffer[indexn] == WSHED))) {
            			    // i.e., indexn belongs to an already labeled basin or to the watersheds
            				if (labelBuffer[indexn] > 0) {
            				   // ImageJ code has if (labelBuffer[index] == MASK)
            				   if ((labelBuffer[index] == MASK) || (labelBuffer[index] == WSHED)) {
            				       labelBuffer[index] = labelBuffer[indexn];   
            				   } // if ((labelBuffer[index] == MASK) || (labelBuffer[index] == WSHED))
            				   else if (labelBuffer[index] != labelBuffer[indexn]) {
            					    labelBuffer[index] = WSHED;   
            				   } // else if (labelBuffer[index] != labelBuffer[indexn])
            				} // if (labelBuffer[indexn] > 0)
            				else if (labelBuffer[index] == MASK) {
            				    labelBuffer[index] = WSHED;	
            				} // else if (labelBuffer[index] == MASK)
            			} // if ((distanceBuffer[indexn] < currentDist) && ((labelBuffer[indexn] > 0) || (labelBuffer[indexn] == WSHED)))
            			else if ((labelBuffer[indexn] == MASK) && (distanceBuffer[indexn] == 0)) {
            			    distanceBuffer[indexn] = currentDist + 1;	
            			    added = fifo.offer(indexn);
            				if (!added) {
                				MipavUtil.displayError("Failure to add indexn to the fifo");
                				setCompleted(false);
                				return;
                			}
            			} // else if ((labelBuffer[indexn] == MASK) && (distanceBuffer[indexn] == 0))
        		    } // for (k = 0; k < neighborBins[index].length; k++)
            	} // while (true)
            	
            	// Checks if new minima have been discovered
            	for (j = 0; j < indexBins[i].length; j++) {
            		index = indexBins[i][j];
            		// The distance associated with index is reset to 0
            		distanceBuffer[index] = 0;
            		if (labelBuffer[index] == MASK) {
            		    currentLabel++;	
            		    added = fifo.offer(index);
        				if (!added) {
            				MipavUtil.displayError("Failure to add index to the fifo");
            				setCompleted(false);
            				return;
            			}
        				labelBuffer[index] = currentLabel;
        				while (!fifo.isEmpty()) {
        					indexn = fifo.poll();
                	    	for (m = 0; m < neighborBins[indexn].length; m++) {
                	    	    indexn2 = neighborBins[indexn][m];
                    			if (labelBuffer[indexn2] == MASK) {
                        			added = fifo.offer(indexn2);
                    				if (!added) {
                        				MipavUtil.displayError("Failure to add indexn2 to the fifo");
                        				setCompleted(false);
                        				return;
                        			}
                    				labelBuffer[indexn2] = currentLabel;
                        		} // if (labelBuffer[indexn2] == MASK)
                		    } // for (m = 0; m < neighborBins[indexn].length; m++)
        				} // while (!fifo.isEmpty())
            		} // if (labelBuffer[index] == MASK)
            	} // for (j = 0; j < indexBins[i].length; j++)
            } // for (i = 0; i < numValues; i++)
            
            
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
            int i = o1.getIndex();
            int j = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (i < j) {
            	return -1;
            } else if (i > j) {
            	return 1;
            } else {
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