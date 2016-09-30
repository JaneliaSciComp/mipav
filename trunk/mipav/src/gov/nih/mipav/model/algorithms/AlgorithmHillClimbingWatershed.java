package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
 * 
 * @author ilb
 * Reference:
 * 1.) The Watershed Transform: Definitions, Algorithms, and Parallelization Strategies by 
 * Jos B.T.M. Roerdink and Arnold Meijster, Fundamentals Informaticae 41 (2001), pp. 187-228.
 * Algorithm 4.3 Watershed transform w.r.t. topographcial distance by hill climbing
 * From Section 4.2.3 Hill Climbing use distance values of 1 for both 4 and 8 connected
 * in calculating steepest upper neighbors.
 * 2.) Automated Sulcal Segmentation Using Watersheds on the Cortical Surface by Maryam E. Rettmann,
 * Xiao Han, Chenyang Xu, and Jerry L. Prince, NeuroImage 15, 2002, Section Merging of 
 * Catchment Basins, p.338.
 */

public class AlgorithmHillClimbingWatershed extends AlgorithmBase {
	
    private boolean neighbor8;
	
    private boolean limitBins;
	
    private int binNumber;
    
    boolean merge;
	
	double mergeThreshold;
    	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmHillClimbingWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8, boolean limitBins,
			int binNumber, boolean merge, double mergeThreshold) {
		super(destImage, srcImage);
		this.neighbor8 = neighbor8;
		this.limitBins = limitBins;
		this.binNumber = binNumber;
		this.merge = merge;
		this.mergeThreshold = mergeThreshold;
	}
	
	public void runAlgorithm() {
		 int xDim;
		 int yDim;
		 int imgBuffer[];
		 int labelBuffer[];
		 // Label of the watershed pixels
	    final int WSHED = 0;
	    final int MASK = -1;
    	int zDim;
    	int tDim;
    	int nDims;
    	int length;
    	int levelBuffer[];
    	int x;
    	int y;
    	int z;
    	int t;
    	int i;
    	int j;
    	AlgorithmLowerCompletion lcAlgo;
    	ModelImage lcImage;
    	int numLevels;
    	boolean isMin[];
    	int minLabel[];
    	int numMins;
    	AlgorithmUnionFindComponentLabelling ufclAlgo;
    	ModelImage ufclImage;
    	boolean S[];
    	int numberS;
    	boolean memberS;
    	int smallestIndex;
    	int smallestValue;
    	int steepestUpperNeighbors;
    	int v[];
    	double steepestSlope;
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
    	int neighborBins[][];
    	int neighbors[];
    	int numNeighbor;
    	int foundNeighbors;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Hill Climbing Watershed ...");
    	
    	if (neighbor8) {
    		numNeighbor = 8;
    	}
    	else {
    		numNeighbor = 4;
    	}
    	
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
    		if (numNeighbor == 8) {
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
        } // for (i = 0; i < length; i++)
        
        lcImage = new ModelImage(ModelStorageBase.INTEGER, srcImage.getExtents(), 
        		srcImage.getImageName());
        lcAlgo = new AlgorithmLowerCompletion(lcImage, srcImage, neighbor8, limitBins, binNumber);
        lcAlgo.run();
        lcAlgo.finalize();
        lcAlgo = null;
        
        ufclImage = new ModelImage(ModelStorageBase.INTEGER, srcImage.getExtents(), 
        		srcImage.getImageName());
        ufclAlgo = new AlgorithmUnionFindComponentLabelling(ufclImage, lcImage, neighbor8, false, binNumber);
        ufclAlgo.run();
        ufclAlgo.finalize();
        ufclAlgo = null;
        
        try {
            imgBuffer = new int[length];
            labelBuffer = new int[length];
            levelBuffer = new int[length];
            S = new boolean[length];
            if (neighbor8) {
            	v = new int[8];
            }
            else {
            	v = new int[4];
            }
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Hill Climbing Watershed: Out of memory creating buffers");
            setCompleted(false);

            return;
        }
        
        for (t = 0; t < tDim; t++) {
            for (z = 0; z < zDim; z++) {

                try {
                    lcImage.exportData((z + t*zDim)*length, length, imgBuffer);
                } catch (IOException error) {
                    displayError("Algorithm Hill Climbing Watershed: image bounds exceeded");
                    setCompleted(false);
                    
                    lcImage.releaseLock();

                    return;
                }
                
                try {
                    ufclImage.exportData((z + t*zDim)*length, length, levelBuffer);
                } catch (IOException error) {
                    displayError("Algorithm Hill Climbing Watershed: image bounds exceeded");
                    setCompleted(false);
                    
                    ufclImage.releaseLock();

                    return;
                }
                
               numLevels = 0;
               for (i = 0; i < length; i++) {
            	   if (levelBuffer[i] > numLevels) {
            		   numLevels = levelBuffer[i];
            	   }
               }
               // levelBuffer[i] goes from 1 to numLevels
               isMin = new boolean[numLevels];
               for (i = 0; i < numLevels; i++) {
            	   isMin[i] = true;
               }
               for (i = 0; i < length; i++) {
                   x = i % xDim;
                   y = i / xDim;
                   if (isMin[levelBuffer[i]-1]) {
	                   if ((x > 0) && (imgBuffer[i] > imgBuffer[i-1])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if ((x < xDim-1) && (imgBuffer[i] > imgBuffer[i+1])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if ((y > 0) && (imgBuffer[i] > imgBuffer[i-xDim])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if ((y < yDim-1) && (imgBuffer[i] > imgBuffer[i+xDim])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if (neighbor8) {
	                	   if ((x > 0) && (y > 0) && (imgBuffer[i] > imgBuffer[i-xDim-1])) {
	                		   isMin[levelBuffer[i]-1] = false;   
	                	   }
	                	   if ((x < xDim-1) && (y > 0) && (imgBuffer[i] > imgBuffer[i-xDim+1])) {
	                		   isMin[levelBuffer[i]-1] = false;   
	                	   }
	                	   if ((x > 0) && (y < yDim-1) && (imgBuffer[i] > imgBuffer[i+xDim-1])) {
	                		   isMin[levelBuffer[i]-1] = false;   
	                	   }
	                	   if ((x < xDim-1) && (y < yDim-1) && (imgBuffer[i] > imgBuffer[i+xDim+1])) {
	                		   isMin[levelBuffer[i]-1] = false;   
	                	   }
	                   } // if (neighbor8)
                   } // if (isMin[levelBuffer[i]-1])
               }
               
               minLabel = new int[numLevels];
               numMins = 0;
               for (i = 0, j = 1; i < numLevels; i++) {
            	   if (isMin[i]) {
            		    minLabel[i] = j++;  
            		    numMins++;
            	   }
               }
               System.out.println("numMins = " + numMins);
               System.out.println("numLevels = " + numLevels);
                
                for (i = 0; i < length; i++) {
                	labelBuffer[i] = MASK;
                }
                
                for (i = 0; i < length; i++) {
                	if (isMin[levelBuffer[i]-1]) {
                		labelBuffer[i] = minLabel[levelBuffer[i]-1];
                	}
                }
                
                for ( i = 0; i < length; i++) {
                	S[i] = true;
                }
                numberS = length;
                for (i = 0; i < length; i++) {
                	x = i % xDim;
                	y = i / xDim;
                	memberS = false;
                	if ((x > 0) && (imgBuffer[i] != imgBuffer[i-1])) {
                		memberS = true;
                	}
                	else if ((x < xDim-1) && (imgBuffer[i] != imgBuffer[i+1])) {
                		memberS = true;
                	}
                	else if ((y > 0) && (imgBuffer[i] != imgBuffer[i-xDim])) {
                		memberS = true;
                	}
                	else if ((y < yDim-1) && (imgBuffer[i] != imgBuffer[i+xDim])) {
                		memberS = true;
                	}
                	else if (neighbor8) {
                		if ((x > 0) && (y > 0) && (imgBuffer[i] != imgBuffer[i-xDim-1])) {
                			memberS = true;
                		}
                		else if ((x > 0) && (y < yDim-1) && (imgBuffer[i] != imgBuffer[i+xDim-1])) {
                			memberS = true;
                		}
                		else if ((x < xDim-1) && (y > 0) && (imgBuffer[i] != imgBuffer[i-xDim+1])) {
                			memberS = true;
                		}
                		else if ((x < xDim-1) && (y < yDim-1) && (imgBuffer[i] != imgBuffer[i+xDim+1])) {
                			memberS = true;
                		}
                	} // else if (neighbor8)
                	if (!memberS) {
                		S[i] = false;
                		numberS--;
                	}
                }  // for (i = 0; i < length; i++)
                
                while (numberS > 0) {
                	smallestIndex = -1;
                	smallestValue = Integer.MAX_VALUE;
                    for (i = 0; i < length; i++) {
                        if (S[i]) {
                            if (imgBuffer[i] < smallestValue) {
                            	smallestValue = imgBuffer[i];
                            	smallestIndex = i;
                            }
                        } // if (S[i])
                    } // for (i = 0; i < length; i++)
                    S[smallestIndex] = false;
                    numberS--;
                    x = smallestIndex % xDim;
                    y = smallestIndex / xDim;
                    steepestUpperNeighbors = 0;
                    steepestSlope = 0.0;
                    if ((x > 0) && (S[smallestIndex-1]) && (imgBuffer[smallestIndex-1] - imgBuffer[smallestIndex] > 0.0)) {
                    	steepestSlope = imgBuffer[smallestIndex-1] - imgBuffer[smallestIndex];
                    	v[steepestUpperNeighbors++] = smallestIndex-1;
                    }
                    if ((x < xDim-1) && (S[smallestIndex+1]) && (imgBuffer[smallestIndex+1] - imgBuffer[smallestIndex] > 0.0)) {
                    	if (imgBuffer[smallestIndex+1] - imgBuffer[smallestIndex] > steepestSlope) {
                    		steepestSlope = imgBuffer[smallestIndex+1] - imgBuffer[smallestIndex];
                    		v[0] = smallestIndex+1;
                    		steepestUpperNeighbors = 1;
                    	}
                    	else if (imgBuffer[smallestIndex+1] - imgBuffer[smallestIndex] == steepestSlope) {
                    		v[steepestUpperNeighbors++] = smallestIndex+1;
                    	}
                    } // if ((x < xDim-1) && (S[smallestIndex+1]) && (imgBuffer[smallestIndex+1] - imgBuffer[smallestIndex] > 0.0))
                    if ((y > 0) && (S[smallestIndex-xDim]) && (imgBuffer[smallestIndex-xDim] - imgBuffer[smallestIndex] > 0.0)) {
                    	if (imgBuffer[smallestIndex-xDim] - imgBuffer[smallestIndex] > steepestSlope) {
                    		steepestSlope = imgBuffer[smallestIndex-xDim] - imgBuffer[smallestIndex];
                    		v[0] = smallestIndex-xDim;
                    		steepestUpperNeighbors = 1;
                    	}
                    	else if (imgBuffer[smallestIndex-xDim] - imgBuffer[smallestIndex] == steepestSlope) {
                    		v[steepestUpperNeighbors++] = smallestIndex-xDim;
                    	}
                    } // if ((y > 0) && (S[smallestIndex-xDim]) && (imgBuffer[smallestIndex-xDim] - imgBuffer[smallestIndex] > 0.0))
                    if ((y < yDim-1) && (S[smallestIndex+xDim]) && (imgBuffer[smallestIndex+xDim] - imgBuffer[smallestIndex] > 0.0)) {
                    	if (imgBuffer[smallestIndex+xDim] - imgBuffer[smallestIndex] > steepestSlope) {
                    		steepestSlope = imgBuffer[smallestIndex+xDim] - imgBuffer[smallestIndex];
                    		v[0] = smallestIndex+xDim;
                    		steepestUpperNeighbors = 1;
                    	}
                    	else if (imgBuffer[smallestIndex+xDim] - imgBuffer[smallestIndex] == steepestSlope) {
                    		v[steepestUpperNeighbors++] = smallestIndex+xDim;
                    	}
                    } // if ((y < yDim-1) && (S[smallestIndex+xDim]) && (imgBuffer[smallestIndex+xDim] - imgBuffer[smallestIndex] > 0.0))
                    if (neighbor8) {
                    	if ((x > 0) && ( y > 0) && (S[smallestIndex-xDim-1]) && 
                    			(imgBuffer[smallestIndex-xDim-1] - imgBuffer[smallestIndex] > 0.0)) {
                    	    if (imgBuffer[smallestIndex-xDim-1] - imgBuffer[smallestIndex] > steepestSlope) {
                    	        steepestSlope = imgBuffer[smallestIndex-xDim-1]-imgBuffer[smallestIndex];
                    	        v[0] = smallestIndex-xDim-1;
                    	        steepestUpperNeighbors = 1;
                    	    }
                    	    else if (imgBuffer[smallestIndex-xDim-1] -imgBuffer[smallestIndex] == steepestSlope) {
                    	    	v[steepestUpperNeighbors++] = smallestIndex-xDim-1;
                    	    }
                    	} // if ((x > 0) && ( y > 0) && (S[smallestIndex-xDim-1]) && (imgBuffer[smallestIndex-xDim-1] - imgBuffer[smallestIndex] > 0.0))
                    	if ((x > 0) && ( y < yDim-1) && (S[smallestIndex+xDim-1]) && 
                    			(imgBuffer[smallestIndex+xDim-1] - imgBuffer[smallestIndex] > 0.0)) {
                    	    if (imgBuffer[smallestIndex+xDim-1] - imgBuffer[smallestIndex] > steepestSlope) {
                    	        steepestSlope = imgBuffer[smallestIndex+xDim-1]-imgBuffer[smallestIndex];
                    	        v[0] = smallestIndex+xDim-1;
                    	        steepestUpperNeighbors = 1;
                    	    }
                    	    else if (imgBuffer[smallestIndex+xDim-1] -imgBuffer[smallestIndex] == steepestSlope) {
                    	    	v[steepestUpperNeighbors++] = smallestIndex+xDim-1;
                    	    }
                    	} // if ((x > 0) && ( y < yDim-1) && (S[smallestIndex+xDim-1]) 
                    	  // && (imgBuffer[smallestIndex+xDim-1] - imgBuffer[smallestIndex] > 0.0))
                    	if ((x < xDim-1) && ( y > 0) && (S[smallestIndex-xDim+1]) && 
                    			(imgBuffer[smallestIndex-xDim+1] - imgBuffer[smallestIndex] > 0.0)) {
                    	    if (imgBuffer[smallestIndex-xDim+1] - imgBuffer[smallestIndex] > steepestSlope) {
                    	        steepestSlope = imgBuffer[smallestIndex-xDim+1]-imgBuffer[smallestIndex];
                    	        v[0] = smallestIndex-xDim+1;
                    	        steepestUpperNeighbors = 1;
                    	    }
                    	    else if (imgBuffer[smallestIndex-xDim+1] -imgBuffer[smallestIndex] == steepestSlope) {
                    	    	v[steepestUpperNeighbors++] = smallestIndex-xDim+1;
                    	    }
                    	} // if ((x < xDim-1) && ( y > 0) && (S[smallestIndex-xDim+1]) 
                    	  // && (imgBuffer[smallestIndex-xDim+1] - imgBuffer[smallestIndex] > 0.0))
                    	if ((x < xDim-1) && ( y < yDim-1) && (S[smallestIndex+xDim+1]) &&
                    			(imgBuffer[smallestIndex+xDim+1] - imgBuffer[smallestIndex] > 0.0)) {
                    	    if (imgBuffer[smallestIndex+xDim+1] - imgBuffer[smallestIndex] > steepestSlope) {
                    	        steepestSlope = imgBuffer[smallestIndex+xDim+1]-imgBuffer[smallestIndex];
                    	        v[0] = smallestIndex+xDim+1;
                    	        steepestUpperNeighbors = 1;
                    	    }
                    	    else if (imgBuffer[smallestIndex+xDim+1] -imgBuffer[smallestIndex] == steepestSlope) {
                    	    	v[steepestUpperNeighbors++] = smallestIndex+xDim+1;
                    	    }
                    	} // if ((x < xDim-1) && ( y < yDim-1) && (S[smallestIndex+xDim+1]) 
                    	  // && (imgBuffer[smallestIndex+xDim+1] - imgBuffer[smallestIndex] > 0.0))
                    } // if (neighbor8)
                    for (j = 0; j < steepestUpperNeighbors; j++) {
                        if (labelBuffer[v[j]] == MASK) {
                            labelBuffer[v[j]] = labelBuffer[smallestIndex];	
                        }
                        else if ((labelBuffer[v[j]] != WSHED) && (labelBuffer[v[j]] != labelBuffer[smallestIndex])) {
                        	labelBuffer[v[j]] = WSHED;
                        }
                    } // for (j = 0; j < steepestUpperNeighbors; j++)
                } // while (numberS > 0)
                
                if (merge) {
                	// Put the catchment basins in a list going from smallest minimum to largest minimum
                	maxLabel = 0;
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > maxLabel) {
                			maxLabel = labelBuffer[i];
                		}
                	} // for (i = 0; i < length; i++)
                	labelMin = new double[maxLabel+1];
                	for (i = 1; i <= maxLabel; i++) {
                		labelMin[i] = Double.MAX_VALUE;
                	}
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > 0) {
                		    if (imgBuffer[i] < labelMin[labelBuffer[i]]) {
                		    	labelMin[labelBuffer[i]] = imgBuffer[i];
                		    }
                		} // if (labelBuffer[i] > 0)
                	} // for (i = 0; i < length; i++)
                	indexValueList.clear();
                	for (i = 1; i <= maxLabel; i++) {
                		indexValueList.add(new indexValueItem(i, labelMin[i]));
                	}
                	Collections.sort(indexValueList, new indexValueComparator());
                	orderedLabel = new int[maxLabel+1];
                	for (i = 1; i <= maxLabel; i++) {
                	    orderedLabel[indexValueList.get(i-1).index] = i;
                	    labelMin[i] = indexValueList.get(i-1).value;
                	}
                	indexValueList.clear();
                	for (i = 1; i <= maxLabel; i++) {
                		indexValueList.add(new indexValueItem(i, labelMin[i]));
                	}
                	// (i = 0; i < indexValueList.size(); i++) {
                		//indexValueItem item = indexValueList.get(i);
                		//System.out.println("index = " + item.index + " value = " + item.value);
                	//}
                	// Labels 1 to maxLabel will now correspond from labels with smallest to largest minima
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > 0) {
                		    labelBuffer[i] = orderedLabel[labelBuffer[i]];
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
    	        	        	    for (j = 0; j < neighborBins[i].length; j++) {
    	        	        	        if ((labelBuffer[neighborBins[i][j]] != listLabel) && (labelBuffer[neighborBins[i][j]] > 0) &&
    	        	        	        		(labelBuffer[neighborBins[i][j]] < smallestLabel)) {
    	        	        	        	smallestLabel = labelBuffer[neighborBins[i][j]];
    	        	        	        }
    	        	        	    } // for (j = 0; j < neighborBins[i].length; j++)
    	        	        	} // if (labelBuffer[i] == listLabel)
    	        	        	else if (labelBuffer[i] == WSHED) {
    	        	        	    hasListLabelNeighbor = false;
    	        	        	    for (j = 0; j < neighborBins[i].length && (!hasListLabelNeighbor); j++) {
    	        	        	    	if (labelBuffer[neighborBins[i][j]] == listLabel) {
    	        	        	    		hasListLabelNeighbor = true;
    	        	        	    	}
    	        	        	    } // for (j = 0; j < neighborBins[i].length && (!hasListLabelNeighbor); j++)
    	        	        	    if (hasListLabelNeighbor) {
    	        	        	    	for (j = 0; j < neighborBins[i].length; j++) {
    	        	        	    		if ((labelBuffer[neighborBins[i][j]] != listLabel) && (labelBuffer[neighborBins[i][j]] > 0) &&
    	            	        	        		(labelBuffer[neighborBins[i][j]] < smallestLabel)) {
    	            	        	        	smallestLabel = labelBuffer[neighborBins[i][j]];
    	            	        	        }	
    	        	        	    	} // for (j = 0; j < neighborBins[i].length; j++)
    	        	        	    } // if (hasListLabelNeighbor)
    	        	        	} // else if (labelBuffer[i] == WSHED)
    	        	        } // for (i = 0; i < length; i++)
    	        	        //System.out.println("listIndex = " + listIndex + " indexValueList.size() = " + indexValueList.size() +
    	        	        		//" smallestLabel = " + smallestLabel);
    	        	        if (smallestLabel < Integer.MAX_VALUE) {
    	        	        	smallestBorderValue = Double.MAX_VALUE;
    	        	        	for (i = 0; i < length; i++) {
    	        	        	    if (labelBuffer[i] == listLabel) {
    	        	        	        hasSmallestLabelNeighbor = false;
    	        	        	        for (j = 0; j < neighborBins[i].length && (!hasSmallestLabelNeighbor); j++) {
    	        	        	        	if (labelBuffer[neighborBins[i][j]] == smallestLabel) {
    	        	        	        		hasSmallestLabelNeighbor = true;
    	        	        	        		if (imgBuffer[i] < smallestBorderValue) {
    	        	        	        			smallestBorderValue = imgBuffer[i];
    	        	        	        		}
    	        	        	        	}
    	        	        	        } // for (j = 0; j < neighborBins[i].length && (!hasSmallestLabelNeighbor); j++)
    	        	        	    } // if (labelBuffer[i] == listLabel)
    	        	        	    else if (labelBuffer[i] == smallestLabel) {
    	        	        	    	hasListLabelNeighbor = false;
    	        	        	        for (j = 0; j < neighborBins[i].length && (!hasListLabelNeighbor); j++) {
    	        	        	        	if (labelBuffer[neighborBins[i][j]] == listLabel) {
    	        	        	        		hasListLabelNeighbor = true;
    	        	        	        		if (imgBuffer[i] < smallestBorderValue) {
    	        	        	        			smallestBorderValue = imgBuffer[i];
    	        	        	        		}
    	        	        	        	}
    	        	        	        } // for (j = 0; j < neighborBins[i].length && (!hasListLabelNeighbor); j++)	
    	        	        	    } // else if (labelBuffer[i] == smallestLabel)
    	        	        	    else if (labelBuffer[i] == WSHED) {
    	        	        	    	hasListLabelNeighbor = false;
    	        	        	    	hasSmallestLabelNeighbor = false;
    	        	        	    	for (j = 0; j < neighborBins[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++) {
    	        	        	    	    if (labelBuffer[neighborBins[i][j]] == listLabel) {
    	        	        	    	    	hasListLabelNeighbor = true;
    	        	        	    	    }
    	        	        	    	    else if (labelBuffer[neighborBins[i][j]] == smallestLabel) {
    	        	        	    	    	hasSmallestLabelNeighbor = true;
    	        	        	    	    }
    	        	        	    	} // for (j = 0; j < neighborBins[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++)
    	        	        	    	if (hasListLabelNeighbor && hasSmallestLabelNeighbor) {
    	        	        	    		if (imgBuffer[i] < smallestBorderValue) {
    	        	        	    			smallestBorderValue = imgBuffer[i];
    	        	        	    		}
    	        	        	    	}
    	        	        	    } // else if (labelBuffer[i] == WSHED)
    	        	        	} //for (i = 0; i < length; i++)
    	        	        	if (smallestBorderValue < Double.MAX_VALUE) {
    		        	        	if ((smallestBorderValue - labelMin[listLabel] < mergeThreshold) && 
    		        	        			(smallestBorderValue - labelMin[smallestLabel] < mergeThreshold)) {
    		        	        		for (i = 0; i < length; i++) {
    		        	        			if (labelBuffer[i] == WSHED) {
    		            	        	    	hasListLabelNeighbor = false;
    		            	        	    	hasSmallestLabelNeighbor = false;
    		            	        	    	for (j = 0; j < neighborBins[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++) {
    		            	        	    	    if (labelBuffer[neighborBins[i][j]] == listLabel) {
    		            	        	    	    	hasListLabelNeighbor = true;
    		            	        	    	    }
    		            	        	    	    else if (labelBuffer[neighborBins[i][j]] == smallestLabel) {
    		            	        	    	    	hasSmallestLabelNeighbor = true;
    		            	        	    	    }
    		            	        	    	} // for (j = 0; j < neighborBins[i].length && (!(hasListLabelNeighbor && hasSmallestLabelNeighbor)); j++)
    		            	        	    	if (hasListLabelNeighbor && hasSmallestLabelNeighbor) {
    		            	        	    		labelBuffer[i] = listLabel;
    		            	        	    	}
    		            	        	    } // if (labelBuffer[i] == WSHED)
    		        	        		} // for (i = 0; i < length; i++)
    		        	        		for (i = 0; i < length; i++) {
    		        	        			if (labelBuffer[i] == smallestLabel) {
    		        	        				labelBuffer[i] = listLabel;
    		        	        			}
    		        	        		} // for (i = 0; i < length; i++)
    		        	        		removed = false;
    		        	        		if (labelMin[smallestLabel] < labelMin[listLabel]) {
    		        	        			labelMin[listLabel] = labelMin[smallestLabel];
    		        	        		}
    		        	        		for (i = 0; i < indexValueList.size() && (!removed); i++) {
    		        	        			if (indexValueList.get(i).index == smallestLabel) {
    		        	        				indexValueList.remove(i);
    		        	        				removed = true;
    		        	        				if (i > listIndex) {
    		        	        					listIndex++;
    		        	        				}
    		        	        				numMerges++;
    		        	        			}
    		        	        		}
    		        	        	} // if ((smallestBorderValue - labelMin[listLabel] < mergeThreshold) && 
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
                	// Remove watershed with only 1 type of neighbor
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] == WSHED) {
                			firstFound = false;
                			secondFound = false;
                			firstNeighbor = -1;
                			for (j = 0; j < neighborBins[i].length && (!secondFound); j++) {
                			    if ((!firstFound) && (labelBuffer[neighborBins[i][j]] > 0)) {
                			    	firstFound = true;
                			    	firstNeighbor = labelBuffer[neighborBins[i][j]];
                			    }
                			    else if (firstFound && (labelBuffer[neighborBins[i][j]] > 0) &&
                			    		labelBuffer[neighborBins[i][j]] != firstNeighbor) {
                			        secondFound = true;	
                			    }
                			} // for (j = 0; j < neighborBins[i].length && (!secondFound); j++)
                			if ((!secondFound) && (firstNeighbor > 0)) {
                				labelBuffer[i] = firstNeighbor;
                			}
                		} // if (labelBuffer[i] == WSHED)
                	} // for (i = 0; i < length; i++)
                	for (i = 1; i <= indexValueList.size(); i++) {
                	    orderedLabel[indexValueList.get(i-1).index] = i;
                	}
                	for (i = 0; i < length; i++) {
                		if (labelBuffer[i] > 0) {
                		    labelBuffer[i] = orderedLabel[labelBuffer[i]];
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
            ufclImage.disposeLocal();
            ufclImage = null;                                 
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