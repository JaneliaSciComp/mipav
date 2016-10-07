package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

/**
 * 
 * @author ilb
 * Separate dark VOIs on a constant light background only works if you do lower completion first.
 * Reference:
 * 1.) Fast watershed algorithms: analysis and extensions by Bogdan P. Dobrin, Timo Viero, and Moncef
 * Gabbouj.
 * 2.) The Watershed Transform: Definitions, Algorithms, and Parallelization Strategies by 
 * Jos B.T.M. Roerdink and Arnold Meijster, Fundamentals Informaticae 41 (2001), pp. 187-228.
 * 3.) Automated Sulcal Segmentation Using Watersheds on the Cortical Surface by Maryam E. Rettmann,
 * Xiao Han, Chenyang Xu, and Jerry L. Prince, NeuroImage 15, 2002, Section Merging of 
 * Catchment Basins, p.338.
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
	
	private int numNeighbor;
	
	private boolean limitBins;
	
	private int binNumber;
	
	private boolean createWatershedLines;
	
    private boolean merge;
	
	private double mergeThreshold;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmSplitAndMergeWatershed(ModelImage destImage, ModelImage srcImage, int numNeighbor, boolean limitBins,
			int binNumber, boolean createWatershedLines, boolean merge, double mergeThreshold) {
		super(destImage, srcImage);
		this.numNeighbor = numNeighbor;
		this.limitBins = limitBins;
		this.binNumber = binNumber;
		this.createWatershedLines = createWatershedLines;
		this.merge = merge;
		this.mergeThreshold = mergeThreshold;
	}
	
	public void runAlgorithm() {
		if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Split And Merge Watershed ...");	
    	if (!srcImage.isColorImage()) {
    		runBW();
    	}
    	else {
    		runColor();
    	}
	}
	
	public void runBW() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int nDims;
    	int length;
    	int imgBuffer[];
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
    	AlgorithmLowerCompletion lcAlgo;
    	ModelImage lcImage;
    	int maxLabel;
    	double labelMin[];
    	int numMerges;
    	int listIndex = 0;
    	int listLabel;
    	int orderedLabel[];
    	int smallestLabel;
    	boolean hasListLabelNeighbor;
    	double smallestBorderValue;
    	boolean hasSmallestLabelNeighbor;
    	boolean removed;
    	boolean firstFound;
    	int firstNeighbor;
    	boolean secondFound;
    	ArrayList <indexValueItem> indexValueList = new ArrayList<indexValueItem>();
    	double threshold;
    	double minValue;
    	double maxValue;
    	
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
        lcImage = new ModelImage(ModelStorageBase.INTEGER, srcImage.getExtents(), 
        		srcImage.getImageName());
        lcAlgo = new AlgorithmLowerCompletion(lcImage, srcImage, numNeighbor, limitBins, binNumber);
        lcAlgo.run();
        lcAlgo.finalize();
        lcAlgo = null;
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
            imgBuffer = new int[length];
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
        	} // if (numNeighbor == 8)
        	allNeighbors[i] = new int[foundNeighbors];
        	for (j = 0; j < foundNeighbors; j++) {
        		allNeighbors[i][j] = neighbors[j];
        	}
        }

        for (t = 0; t < tDim; t++) {
        for (z = 0; z < zDim; z++) {

            try {
                lcImage.exportData((z + t*zDim)*length, length, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm Split And Merge Watershed: image bounds exceeded");
                setCompleted(false);
                
                lcImage.releaseLock();

                return;
            }
            
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
        	threshold = mergeThreshold * (maxValue - minValue);
            
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
        						if (!createWatershedLines) {
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
            
            if (!createWatershedLines) {
            	// Meyer1
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
            else { // Meyer2
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
            } // else Meyer2
            
            if (merge) {
            	// Put the catchment basins in a list going from smallest minimum to largest minimum
            	maxLabel = 0;
            	for (i = 0; i < length; i++) {
            		if (labelBuffer[i] > maxLabel) {
            			maxLabel = labelBuffer[i];
            		}
            	} // for (i = 0; i < length; i++)
            	labelMin = new double[maxLabel];
            	for (i = 0; i < maxLabel; i++) {
            		labelMin[i] = Double.MAX_VALUE;
            	}
            	for (i = 0; i < length; i++) {
            		if (labelBuffer[i] > 0) {
            		    if (imgBuffer[i] < labelMin[labelBuffer[i]-1]) {
            		    	labelMin[labelBuffer[i]-1] = imgBuffer[i];
            		    }
            		} // if (labelBuffer[i] > 0)
            	} // for (i = 0; i < length; i++)
            	indexValueList.clear();
            	for (i = 0; i < maxLabel; i++) {
            		indexValueList.add(new indexValueItem(i+1, labelMin[i]));
            	}
            	Collections.sort(indexValueList, new indexValueComparator());
            	orderedLabel = new int[maxLabel];
            	for (i = 0; i < maxLabel; i++) {
            	    orderedLabel[indexValueList.get(i).index-1] = i+1;
            	    labelMin[i] = indexValueList.get(i).value;
            	}
            	indexValueList.clear();
            	for (i = 0; i < maxLabel; i++) {
            		indexValueList.add(new indexValueItem(i+1, labelMin[i]));
            	}
            	// (i = 0; i < indexValueList.size(); i++) {
            		//indexValueItem item = indexValueList.get(i);
            		//System.out.println("index = " + item.index + " value = " + item.value);
            	//}
            	// Labels 1 to maxLabel will now correspond from labels with smallest to largest minima
            	for (i = 0; i < length; i++) {
            		if (labelBuffer[i] > 0) {
            		    labelBuffer[i] = orderedLabel[labelBuffer[i]-1];
            		}
            	}
            	numMerges = 1;
            	while (numMerges > 0) {
	        	    numMerges = 0;
	        	    listIndex = 0;
	        	    while (listIndex < indexValueList.size()) {
	        	        listLabel = indexValueList.get(listIndex).index;
	        	        smallestLabel = Integer.MAX_VALUE;
	        	        for (i = 0; i < length; i++) {
	        	        	if (labelBuffer[i] == listLabel) {
	        	        	    for (j = 0; j < allNeighbors[i].length; j++) {
	        	        	        if ((labelBuffer[allNeighbors[i][j]] != listLabel) && (labelBuffer[allNeighbors[i][j]] > 0) &&
	        	        	        		(labelBuffer[allNeighbors[i][j]] < smallestLabel)) {
	        	        	        	smallestLabel = labelBuffer[allNeighbors[i][j]];
	        	        	        }
	        	        	    } // for (j = 0; j < allNeighbors[i].length; j++)
	        	        	} // if (labelBuffer[i] == listLabel)
	        	        	else if (labelBuffer[i] == WSHED) {
	        	        		// Remove watershed with only 1 type of neighbor
	        	        		firstFound = false;
	                			secondFound = false;
	                			firstNeighbor = -1;
	                			for (j = 0; j < allNeighbors[i].length && (!secondFound); j++) {
	                			    if ((!firstFound) && (labelBuffer[allNeighbors[i][j]] > 0)) {
	                			    	firstFound = true;
	                			    	firstNeighbor = labelBuffer[allNeighbors[i][j]];
	                			    }
	                			    else if (firstFound && (labelBuffer[allNeighbors[i][j]] > 0) &&
	                			    		labelBuffer[allNeighbors[i][j]] != firstNeighbor) {
	                			        secondFound = true;	
	                			    }
	                			} // for (j = 0; j < allNeighbors[i].length && (!secondFound); j++)
	                			if ((!secondFound) && (firstNeighbor > 0)) {
	                				labelBuffer[i] = firstNeighbor;
	                			}
	                			else {
    	        	        	    hasListLabelNeighbor = false;
    	        	        	    for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++) {
    	        	        	    	if (labelBuffer[allNeighbors[i][j]] == listLabel) {
    	        	        	    		hasListLabelNeighbor = true;
    	        	        	    	}
    	        	        	    } // for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++)
    	        	        	    if (hasListLabelNeighbor) {
    	        	        	    	for (j = 0; j < allNeighbors[i].length; j++) {
    	        	        	    		if ((labelBuffer[allNeighbors[i][j]] != listLabel) && (labelBuffer[allNeighbors[i][j]] > 0) &&
    	            	        	        		(labelBuffer[allNeighbors[i][j]] < smallestLabel)) {
    	            	        	        	smallestLabel = labelBuffer[allNeighbors[i][j]];
    	            	        	        }	
    	        	        	    	} // for (j = 0; j < allNeighbors[i].length; j++)
    	        	        	    } // if (hasListLabelNeighbor)
	                			}
	        	        	} // else if (labelBuffer[i] == WSHED)
	        	        } // for (i = 0; i < length; i++)
	        	        if (smallestLabel < Integer.MAX_VALUE) {
	        	        	smallestBorderValue = Double.MAX_VALUE;
	        	        	for (i = 0; i < length; i++) {
	        	        	    if (labelBuffer[i] == listLabel) {
	        	        	    	hasSmallestLabelNeighbor = false;
	        	        	        for (j = 0; j < allNeighbors[i].length && (!hasSmallestLabelNeighbor); j++) {
	        	        	        	if (labelBuffer[allNeighbors[i][j]] == smallestLabel) {
	        	        	        		hasSmallestLabelNeighbor = true;
	        	        	        		if (imgBuffer[i] < smallestBorderValue) {
	        	        	        			smallestBorderValue = imgBuffer[i];
	        	        	        		}
	        	        	        	}
	        	        	        } // for (j = 0; j < allNeighbors[i].length && (!hasSmallestLabelNeighbor); j++)
	        	        	    } // if (labelBuffer[i] == listLabel)
	        	        	    else if (labelBuffer[i] == smallestLabel) {
	        	        	    	hasListLabelNeighbor = false;
	        	        	        for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++) {
	        	        	        	if (labelBuffer[allNeighbors[i][j]] == listLabel) {
	        	        	        		hasListLabelNeighbor = true;
	        	        	        		if (imgBuffer[i] < smallestBorderValue) {
	        	        	        			smallestBorderValue = imgBuffer[i];
	        	        	        		}
	        	        	        	}
	        	        	        } // for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++)	
	        	        	    } // else if (labelBuffer[i] == smallestLabel)
	        	        	    else if (labelBuffer[i] == WSHED) {
	        	        	    	hasListLabelNeighbor = false;
	        	        	    	hasSmallestLabelNeighbor = false;
	        	        	    	for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++) {
	        	        	    	    if (labelBuffer[allNeighbors[i][j]] == listLabel) {
	        	        	    	    	hasListLabelNeighbor = true;
	        	        	    	    }
	        	        	    	    else if (labelBuffer[allNeighbors[i][j]] == smallestLabel) {
	        	        	    	    	hasSmallestLabelNeighbor = true;
	        	        	    	    }
	        	        	    	} // for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++)
	        	        	    	if (hasListLabelNeighbor && hasSmallestLabelNeighbor) {
	        	        	    		if (imgBuffer[i] < smallestBorderValue) {
	        	        	    			smallestBorderValue = imgBuffer[i];
	        	        	    		}
	        	        	    	}
	        	        	    } // else if (labelBuffer[i] == WSHED)
	        	        	} //for (i = 0; i < length; i++)
	        	        	if (smallestBorderValue < Double.MAX_VALUE) {
		        	        	if ((smallestBorderValue - labelMin[listLabel-1] < threshold) && 
		        	        			(smallestBorderValue - labelMin[smallestLabel-1] < threshold)) {
		        	        		for (i = 0; i < length; i++) {
		        	        			if (labelBuffer[i] == WSHED) {
		            	        	    	hasListLabelNeighbor = false;
		            	        	    	hasSmallestLabelNeighbor = false;
		            	        	    	for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++) {
		            	        	    	    if (labelBuffer[allNeighbors[i][j]] == listLabel) {
		            	        	    	    	hasListLabelNeighbor = true;
		            	        	    	    }
		            	        	    	    else if (labelBuffer[allNeighbors[i][j]] == smallestLabel) {
		            	        	    	    	hasSmallestLabelNeighbor = true;
		            	        	    	    }
		            	        	    	} // for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++)
		            	        	    	if (hasListLabelNeighbor && hasSmallestLabelNeighbor) {
		            	        	    		labelBuffer[i] = Math.min(listLabel, smallestLabel);
		            	        	    	}
		            	        	    } // if (labelBuffer[i] == WSHED)
		        	        		} // for (i = 0; i < length; i++)
		        	        		for (i = 0; i < length; i++) {
		        	        			if ((labelBuffer[i] == smallestLabel) || (labelBuffer[i] == listLabel)) {
		        	        				labelBuffer[i] = Math.min(listLabel, smallestLabel);
		        	        			}
		        	        		} // for (i = 0; i < length; i++)
		        	        		removed = false;
		        	        		labelMin[Math.min(smallestLabel-1, listLabel-1)] = Math.min(labelMin[listLabel-1], 
		        	        				                                                    labelMin[smallestLabel-1]);
		        	        		for (i = 0; i < indexValueList.size() && (!removed); i++) {
		        	        			if (indexValueList.get(i).index == Math.max(smallestLabel, listLabel)) {
		        	        				indexValueList.remove(i);
		        	        				removed = true;
		        	        				if (i > listIndex) {
		        	        					listIndex++;
		        	        				}
		        	        				numMerges++;
		        	        			}
		        	        		}
		        	        	} // if ((smallestBorderValue - labelMin[listLabel-1] < mergeThreshold) && 
		        	        	else {
		        	        		listIndex++;
		        	        	}
	        	        	} // if (smallestBorderValue < Double.MAX_VALUE) {
	        	        	else {
	        	        		listIndex++;
	        	        	}
	        	        } // if (smallestLabel < Integer.MAX_VALUE)
	        	        else {
	        	        	listIndex++;
	        	        }
	        	    } // while (listIndex < indexValueList.size())
            	} // while (numMerges > 0)
            	for (i = 0; i < indexValueList.size(); i++) {
            	    orderedLabel[indexValueList.get(i).index-1] = i+1;
            	}
            	for (i = 0; i < length; i++) {
            		if (labelBuffer[i] > 0) {
            		    labelBuffer[i] = orderedLabel[labelBuffer[i]-1];
            		}
            	}
            } // if (merge)
            
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
        lcImage.disposeLocal();
        lcImage = null;
        destImage.calcMinMax();
        
        setCompleted(true);
        return;
	}
	
	private void runColor() {
		int i;
		int j;
		int k;
		int m;
		int xDim;
		int yDim;
		int zDim;
		int tDim;
		int x;
		int y;
		int z;
		int t;
		int length;
		int nDims;
		int imgBuffer[];
		int labelBuffer[];
		double varR;
		double varG;
		double varB;
		double X;
		double Y;
		double Z;
		double varU;
		double varV;
		double varY;
		// Observer = 2 degrees, Illuminant = D65
		double refX = 95.047;
		double refY = 100.000;
		double refZ = 108.883;
		double refU = (4.0 * refX) / (refX + (15.0 * refY) + (3.0 * refZ));
		double refV = (9.0 * refY) / (refX + (15.0 * refY) + (3.0 * refZ));
		double L[];
		double u[];
		double v[];
		double grad[];
		double diffLx;
		double diffLy;
		double diffux;
		double diffuy;
		double diffvx;
		double diffvy;
		int allNeighbors[][];
		int neighbors[];
		int foundNeighbors;
		double minValue;
		double maxValue;
		double threshold;
		Queue <Integer> fifo = new LinkedList<Integer>();
		boolean exists;
		Comparator<indexValueItem> comparator = new indexValueComparator();
    	PriorityQueue<indexValueItem> pfifo;
    	boolean added;
    	int currentLabel;
    	indexValueItem p;
    	boolean labelFound;
    	boolean uniqueLabelFound;
    	int maxLabel;
    	double labelMin[];
    	ArrayList <indexValueItem> indexValueList = new ArrayList<indexValueItem>();
    	int orderedLabel[];
    	int numMerges;
    	int listIndex;
    	int listLabel;
    	int smallestLabel;
    	boolean firstFound;
    	boolean secondFound;
    	int firstNeighbor;
    	boolean hasListLabelNeighbor;
    	double smallestBorderValue;
    	boolean hasSmallestLabelNeighbor;
    	boolean removed;
    	ModelImage gradImage;
    	ModelImage lcImage;
    	int extents2D[] = new int[2];
    	AlgorithmLowerCompletion lcAlgo;
    	int levelBuffer[];
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        extents2D[0] = xDim;
        extents2D[1] = yDim;
        gradImage = new ModelImage(ModelStorageBase.DOUBLE, extents2D, 
        		srcImage.getImageName());
        lcImage = new ModelImage(ModelStorageBase.INTEGER, extents2D, 
        		srcImage.getImageName());
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
        pfifo = new PriorityQueue<indexValueItem>(length, comparator);
        
        neighbors = new int[numNeighbor];
        
        try {
            imgBuffer = new int[4*length];
            labelBuffer = new int[length];
            allNeighbors = new int[length][];
            L = new double[length];
            u = new double[length];
            v = new double[length];
            grad = new double[length];
            levelBuffer = new int[length];
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
        	} // if (numNeighbor == 8)
        	allNeighbors[i] = new int[foundNeighbors];
        	for (j = 0; j < foundNeighbors; j++) {
        		allNeighbors[i][j] = neighbors[j];
        	}
        }
        
        for (t = 0; t < tDim; t++) {
            for (z = 0; z < zDim; z++) {

                try {
                    srcImage.exportData((z + t*zDim)*4*length, 4*length, imgBuffer);
                } catch (IOException error) {
                    displayError("Algorithm Split And Merge Watershed: image bounds exceeded");
                    setCompleted(false);
                    
                    srcImage.releaseLock();

                    return;
                }
                
                for (i = 0; i < length; i++) {
                    varR = imgBuffer[4*i + 1]/255.0;
                    varG = imgBuffer[4*i + 2]/255.0;
                    varB = imgBuffer[4*i + 3]/255.0;
                    if (varR > 0.04045) {
                    	varR = Math.pow(((varR + 0.055)/1.055), 2.4);
                    }
                    else {
                    	varR = varR/12.92;
                    }
                    if (varG > 0.04045) {
                    	varG = Math.pow(((varG + 0.055)/1.055), 2.4);
                    }
                    else {
                    	varG = varG/12.92;
                    }
                    if (varB > 0.04045) {
                    	varB = Math.pow(((varB + 0.055)/1.055), 2.4);
                    }
                    else {
                    	varB = varB/12.92;
                    }
                    varR = varR * 100.0;
                    varG = varG * 100.0;
                    varB = varB * 100.0;
                    
                    // Observer = 2 degrees, Illuminant = D65
                    X = varR * 0.4124 + varG * 0.3576 + varB * 0.1805;
                    Y = varR * 0.2126 + varG * 0.7152 + varB * 0.0722;
                    Z = varR * 0.0193 + varG * 0.1192 + varB * 0.9505;
                    
                    varU = (4.0 * X) / (X + (15.0 * Y) + (3.0 * Z));
                    varV = (9.0 * Y) / (X + (15.0 * Y) + (3.0 * Z));
                    
                    varY = Y / 100.0;
                    if (varY > 0.008856) {
                    	varY = Math.pow(varY, (1.0/3.0));
                    }
                    else {
                    	varY = (7.787 * varY) + (16.0/116.0);
                    }
                    L[i] = (116.0 * varY) - 16.0;
                    u[i] = 13.0 * L[i] * (varU - refU);
                    v[i] = 13.0 * L[i] * (varV - refV);
                } // for (i = 0; i < length; i++)
                
                for (i = 0; i < length; i++) {
                	x = i % xDim;
                	y = i / xDim;
                	if (x == 0) {
                		diffLx = L[i+1] - L[i];
                		diffux = u[i+1] - u[i];
                		diffvx = v[i+1] - v[i];
                	}
                	else if (x == xDim-1) {
                		diffLx = L[i] - L[i-1];
                		diffux = u[i] - u[i-1];
                		diffvx = v[i] - v[i-1];
                	}
                	else {
                		diffLx = (L[i+1] - L[i-1])/2.0;
                		diffux = (u[i+1] - u[i-1])/2.0;
                		diffvx = (v[i+1] - v[i-1])/2.0;
                	}
                	if (y == 0) {
                		diffLy = L[i+xDim] - L[i];
                		diffuy = u[i+xDim] - u[i];
                		diffvy = v[i+xDim] - v[i];
                	}
                	else if (y == yDim-1) {
                		diffLy = L[i] - L[i-xDim];
                		diffuy = u[i] - u[i-xDim];
                		diffvy = v[i] - v[i-xDim];
                	}
                	else {
                		diffLy = (L[i+xDim] - L[i-xDim])/2.0;
                		diffuy = (u[i+xDim] - u[i-xDim])/2.0;
                		diffvy = (v[i+xDim] - v[i-xDim])/2.0;
                	}
                	grad[i] = Math.sqrt(diffLx*diffLx + diffux*diffux + diffvx*diffvx + diffLy*diffLy + diffuy*diffuy + diffvy*diffvy);
                	
                } // for (i = 0; i < length; i++)
                
                try {
                	gradImage.importData(0, grad, true);
                }
                catch (IOException e) {
                	MipavUtil.displayError("IOException " + e + " on gradImage.importData");
    				setCompleted(false);
    				return;	
                }
                
                lcAlgo = new AlgorithmLowerCompletion(lcImage, gradImage, numNeighbor, limitBins, binNumber);
                lcAlgo.run();
                lcAlgo.finalize();
                lcAlgo = null;
                
                try {
                	lcImage.exportData(0, length, levelBuffer);
                }
                catch(IOException e) {
                	MipavUtil.displayError("IOException " + e + " on lcImage.exportData");
    				setCompleted(false);
    				return;		
                }
                
                minValue = Double.MAX_VALUE;
            	maxValue = -Double.MAX_VALUE;
            	for (i = 0; i < length; i++) {
            	    if (levelBuffer[i] < minValue) {
            	    	minValue = levelBuffer[i];
            	    }
            	    if (levelBuffer[i] > maxValue) {
            	    	maxValue = levelBuffer[i];
            	    }
            	}
            	threshold = mergeThreshold * (maxValue - minValue);
                
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
                	    	if (levelBuffer[allNeighbors[i][j]] < levelBuffer[i]) {
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
                			    	if ((labelBuffer[allNeighbors[j][k]] == INIT) && (levelBuffer[allNeighbors[j][k]] == levelBuffer[i])) {
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
            						if (!createWatershedLines) {
            							labelBuffer[allNeighbors[j][k]] = currentLabel;
            						}
            						else {
            						    labelBuffer[allNeighbors[j][k]] = INOQ;
            						}
            						pfifo.add(new indexValueItem(allNeighbors[j][k],levelBuffer[allNeighbors[j][k]]));
            					}
            				}
            			} // while (!fifo.isEmpty())
                	} // if (labelBuffer[i] == INIT)
                } // for (i = 0; i < length; i++)
                
                if (!createWatershedLines) {
                	// Meyer1
                	while (!pfifo.isEmpty()) {
    	            	p = pfifo.poll();
    	            	i = p.getIndex();
    	            	foundNeighbors = allNeighbors[i].length;
    	            	for (j = 0; j < foundNeighbors; j++) {
                	    	if (labelBuffer[allNeighbors[i][j]] == NARM) {
                	    		labelBuffer[allNeighbors[i][j]] = labelBuffer[i];
                	    		pfifo.add(new indexValueItem(allNeighbors[i][j],levelBuffer[allNeighbors[i][j]]));
                	    	}
                	    }
                	} // while (!pfifo.isEmpty())
                }
                else { // Meyer2
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
    	    			    		    			(levelBuffer[allNeighbors[k][m]] == levelBuffer[allNeighbors[i][j]])) {
    	    			    		    		labelBuffer[allNeighbors[k][m]] = PIAP;
    	    			    		    		added = fifo.offer(allNeighbors[k][m]);
    	    			    		    		if (!added) {
    	    		                				MipavUtil.displayError("Failure to add " + allNeighbors[k][m] + " to the fifo");
    	    		                				setCompleted(false);
    	    		                				return;
    	    		                			}
    	    			    		    	}
    	    			    		    	else if ((labelBuffer[allNeighbors[k][m]] == NARM) && 
    	    			    		    			(levelBuffer[allNeighbors[k][m]] < levelBuffer[allNeighbors[i][j]])) {
    	    			    		    		labelFound = true;
    	    			    		    	}
    	    			    		    	else if ((labelBuffer[allNeighbors[k][m]] == INOQ) && 
    	    			    		    			(levelBuffer[allNeighbors[k][m]] <= levelBuffer[allNeighbors[i][j]])) {
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
    	        			    		        	pfifo.add(new indexValueItem(allNeighbors[k][m],levelBuffer[allNeighbors[k][m]]));
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
    	            	    		pfifo.add(new indexValueItem(allNeighbors[i][j],levelBuffer[allNeighbors[i][j]]));
    	            	    	}
    	            	    } // for (j = 0; j < foundNeighbors; j++)
    	            	} // else !exists
    	            } // while (!pfifo.isEmpty())
                } // else Meyer2
                
                if (merge) {
                	// Put the catchment basins in a list going from smallest minimum to largest minimum
                	maxLabel = 0;
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > maxLabel) {
                			maxLabel = labelBuffer[i];
                		}
                	} // for (i = 0; i < length; i++)
                	labelMin = new double[maxLabel];
                	for (i = 0; i < maxLabel; i++) {
                		labelMin[i] = Double.MAX_VALUE;
                	}
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > 0) {
                		    if (levelBuffer[i] < labelMin[labelBuffer[i]-1]) {
                		    	labelMin[labelBuffer[i]-1] = levelBuffer[i];
                		    }
                		} // if (labelBuffer[i] > 0)
                	} // for (i = 0; i < length; i++)
                	indexValueList.clear();
                	for (i = 0; i < maxLabel; i++) {
                		indexValueList.add(new indexValueItem(i+1, labelMin[i]));
                	}
                	Collections.sort(indexValueList, new indexValueComparator());
                	orderedLabel = new int[maxLabel];
                	for (i = 0; i < maxLabel; i++) {
                	    orderedLabel[indexValueList.get(i).index-1] = i+1;
                	    labelMin[i] = indexValueList.get(i).value;
                	}
                	indexValueList.clear();
                	for (i = 0; i < maxLabel; i++) {
                		indexValueList.add(new indexValueItem(i+1, labelMin[i]));
                	}
                	// (i = 0; i < indexValueList.size(); i++) {
                		//indexValueItem item = indexValueList.get(i);
                		//System.out.println("index = " + item.index + " value = " + item.value);
                	//}
                	// Labels 1 to maxLabel will now correspond from labels with smallest to largest minima
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > 0) {
                		    labelBuffer[i] = orderedLabel[labelBuffer[i]-1];
                		}
                	}
                	numMerges = 1;
                	while (numMerges > 0) {
    	        	    numMerges = 0;
    	        	    listIndex = 0;
    	        	    while (listIndex < indexValueList.size()) {
    	        	        listLabel = indexValueList.get(listIndex).index;
    	        	        smallestLabel = Integer.MAX_VALUE;
    	        	        for (i = 0; i < length; i++) {
    	        	        	if (labelBuffer[i] == listLabel) {
    	        	        	    for (j = 0; j < allNeighbors[i].length; j++) {
    	        	        	        if ((labelBuffer[allNeighbors[i][j]] != listLabel) && (labelBuffer[allNeighbors[i][j]] > 0) &&
    	        	        	        		(labelBuffer[allNeighbors[i][j]] < smallestLabel)) {
    	        	        	        	smallestLabel = labelBuffer[allNeighbors[i][j]];
    	        	        	        }
    	        	        	    } // for (j = 0; j < allNeighbors[i].length; j++)
    	        	        	} // if (labelBuffer[i] == listLabel)
    	        	        	else if (labelBuffer[i] == WSHED) {
    	        	        		// Remove watershed with only 1 type of neighbor
    	        	        		firstFound = false;
    	                			secondFound = false;
    	                			firstNeighbor = -1;
    	                			for (j = 0; j < allNeighbors[i].length && (!secondFound); j++) {
    	                			    if ((!firstFound) && (labelBuffer[allNeighbors[i][j]] > 0)) {
    	                			    	firstFound = true;
    	                			    	firstNeighbor = labelBuffer[allNeighbors[i][j]];
    	                			    }
    	                			    else if (firstFound && (labelBuffer[allNeighbors[i][j]] > 0) &&
    	                			    		labelBuffer[allNeighbors[i][j]] != firstNeighbor) {
    	                			        secondFound = true;	
    	                			    }
    	                			} // for (j = 0; j < allNeighbors[i].length && (!secondFound); j++)
    	                			if ((!secondFound) && (firstNeighbor > 0)) {
    	                				labelBuffer[i] = firstNeighbor;
    	                			}
    	                			else {
        	        	        	    hasListLabelNeighbor = false;
        	        	        	    for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++) {
        	        	        	    	if (labelBuffer[allNeighbors[i][j]] == listLabel) {
        	        	        	    		hasListLabelNeighbor = true;
        	        	        	    	}
        	        	        	    } // for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++)
        	        	        	    if (hasListLabelNeighbor) {
        	        	        	    	for (j = 0; j < allNeighbors[i].length; j++) {
        	        	        	    		if ((labelBuffer[allNeighbors[i][j]] != listLabel) && (labelBuffer[allNeighbors[i][j]] > 0) &&
        	            	        	        		(labelBuffer[allNeighbors[i][j]] < smallestLabel)) {
        	            	        	        	smallestLabel = labelBuffer[allNeighbors[i][j]];
        	            	        	        }	
        	        	        	    	} // for (j = 0; j < allNeighbors[i].length; j++)
        	        	        	    } // if (hasListLabelNeighbor)
    	                			}
    	        	        	} // else if (labelBuffer[i] == WSHED)
    	        	        } // for (i = 0; i < length; i++)
    	        	        if (smallestLabel < Integer.MAX_VALUE) {
    	        	        	smallestBorderValue = Double.MAX_VALUE;
    	        	        	for (i = 0; i < length; i++) {
    	        	        	    if (labelBuffer[i] == listLabel) {
    	        	        	    	hasSmallestLabelNeighbor = false;
    	        	        	        for (j = 0; j < allNeighbors[i].length && (!hasSmallestLabelNeighbor); j++) {
    	        	        	        	if (labelBuffer[allNeighbors[i][j]] == smallestLabel) {
    	        	        	        		hasSmallestLabelNeighbor = true;
    	        	        	        		if (levelBuffer[i] < smallestBorderValue) {
    	        	        	        			smallestBorderValue = levelBuffer[i];
    	        	        	        		}
    	        	        	        	}
    	        	        	        } // for (j = 0; j < allNeighbors[i].length && (!hasSmallestLabelNeighbor); j++)
    	        	        	    } // if (labelBuffer[i] == listLabel)
    	        	        	    else if (labelBuffer[i] == smallestLabel) {
    	        	        	    	hasListLabelNeighbor = false;
    	        	        	        for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++) {
    	        	        	        	if (labelBuffer[allNeighbors[i][j]] == listLabel) {
    	        	        	        		hasListLabelNeighbor = true;
    	        	        	        		if (levelBuffer[i] < smallestBorderValue) {
    	        	        	        			smallestBorderValue = levelBuffer[i];
    	        	        	        		}
    	        	        	        	}
    	        	        	        } // for (j = 0; j < allNeighbors[i].length && (!hasListLabelNeighbor); j++)	
    	        	        	    } // else if (labelBuffer[i] == smallestLabel)
    	        	        	    else if (labelBuffer[i] == WSHED) {
    	        	        	    	hasListLabelNeighbor = false;
    	        	        	    	hasSmallestLabelNeighbor = false;
    	        	        	    	for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++) {
    	        	        	    	    if (labelBuffer[allNeighbors[i][j]] == listLabel) {
    	        	        	    	    	hasListLabelNeighbor = true;
    	        	        	    	    }
    	        	        	    	    else if (labelBuffer[allNeighbors[i][j]] == smallestLabel) {
    	        	        	    	    	hasSmallestLabelNeighbor = true;
    	        	        	    	    }
    	        	        	    	} // for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++)
    	        	        	    	if (hasListLabelNeighbor && hasSmallestLabelNeighbor) {
    	        	        	    		if (levelBuffer[i] < smallestBorderValue) {
    	        	        	    			smallestBorderValue = levelBuffer[i];
    	        	        	    		}
    	        	        	    	}
    	        	        	    } // else if (labelBuffer[i] == WSHED)
    	        	        	} //for (i = 0; i < length; i++)
    	        	        	if (smallestBorderValue < Double.MAX_VALUE) {
    		        	        	if ((smallestBorderValue - labelMin[listLabel-1] < threshold) && 
    		        	        			(smallestBorderValue - labelMin[smallestLabel-1] < threshold)) {
    		        	        		for (i = 0; i < length; i++) {
    		        	        			if (labelBuffer[i] == WSHED) {
    		            	        	    	hasListLabelNeighbor = false;
    		            	        	    	hasSmallestLabelNeighbor = false;
    		            	        	    	for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++) {
    		            	        	    	    if (labelBuffer[allNeighbors[i][j]] == listLabel) {
    		            	        	    	    	hasListLabelNeighbor = true;
    		            	        	    	    }
    		            	        	    	    else if (labelBuffer[allNeighbors[i][j]] == smallestLabel) {
    		            	        	    	    	hasSmallestLabelNeighbor = true;
    		            	        	    	    }
    		            	        	    	} // for (j = 0; j < allNeighbors[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++)
    		            	        	    	if (hasListLabelNeighbor && hasSmallestLabelNeighbor) {
    		            	        	    		labelBuffer[i] = Math.min(listLabel, smallestLabel);
    		            	        	    	}
    		            	        	    } // if (labelBuffer[i] == WSHED)
    		        	        		} // for (i = 0; i < length; i++)
    		        	        		for (i = 0; i < length; i++) {
    		        	        			if ((labelBuffer[i] == smallestLabel) || (labelBuffer[i] == listLabel)) {
    		        	        				labelBuffer[i] = Math.min(listLabel, smallestLabel);
    		        	        			}
    		        	        		} // for (i = 0; i < length; i++)
    		        	        		removed = false;
    		        	        		labelMin[Math.min(smallestLabel-1, listLabel-1)] = Math.min(labelMin[listLabel-1], 
    		        	        				                                                    labelMin[smallestLabel-1]);
    		        	        		for (i = 0; i < indexValueList.size() && (!removed); i++) {
    		        	        			if (indexValueList.get(i).index == Math.max(smallestLabel, listLabel)) {
    		        	        				indexValueList.remove(i);
    		        	        				removed = true;
    		        	        				if (i > listIndex) {
    		        	        					listIndex++;
    		        	        				}
    		        	        				numMerges++;
    		        	        			}
    		        	        		}
    		        	        	} // if ((smallestBorderValue - labelMin[listLabel-1] < mergeThreshold) && 
    		        	        	else {
    		        	        		listIndex++;
    		        	        	}
    	        	        	} // if (smallestBorderValue < Double.MAX_VALUE) {
    	        	        	else {
    	        	        		listIndex++;
    	        	        	}
    	        	        } // if (smallestLabel < Integer.MAX_VALUE)
    	        	        else {
    	        	        	listIndex++;
    	        	        }
    	        	    } // while (listIndex < indexValueList.size())
                	} // while (numMerges > 0)
                	for (i = 0; i < indexValueList.size(); i++) {
                	    orderedLabel[indexValueList.get(i).index-1] = i+1;
                	}
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > 0) {
                		    labelBuffer[i] = orderedLabel[labelBuffer[i]-1];
                		}
                	}
                } // if (merge)
                
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
        gradImage.disposeLocal();
        lcImage.disposeLocal();
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