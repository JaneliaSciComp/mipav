package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.LinkedList;
import java.util.Queue;

/**
 * 
 * @author ilb
 * Reference:
 * 1.) The Watershed Transform: Definitions, Algorithms, and Parallelization Strategies by 
 * Jos B.T.M. Roerdink and Arnold Meijster, Fundamentals Informaticae 41 (2001), pp. 187-228.
 * Algorithm 4.4 Watershed transform w.r.t. topographical distance based on disjoint sets
 */

public class AlgorithmUnionFindWatershed extends AlgorithmBase {
	
    private boolean neighbor8;
	
    private boolean limitBins;
	
    private int binNumber;
    
    private int xDim;
    
    private int yDim;
    
    private int imgBuffer[];
    
	private int labelBuffer[];
	
	// sln[p][i]is a pointer to the ith steepest lowest neighbor of pixel p
	// The number of steepest lower neighbors is at most the connectivity
	private int sln[][];
	
	private final double sqrt2 = Math.sqrt(2.0);
	
	// Label of the watershed pixels
	private final int WSHED = 0;
	
	// Fictitious index of the watershed pixels
	private final int W = -1;
	
	private final int INIT = -2;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmUnionFindWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8, boolean limitBins,
			int binNumber) {
		super(destImage, srcImage);
		this.neighbor8 = neighbor8;
		this.limitBins = limitBins;
		this.binNumber = binNumber;
	}
	
	public void runAlgorithm() {
    	int zDim;
    	int tDim;
    	int nDims;
    	int length;
    	int x;
    	int y;
    	int z;
    	int t;
    	int i;
    	int j;
    	AlgorithmLowerCompletion lcAlgo;
    	ModelImage lcImage;
    	AlgorithmUnionFindComponentLabelling ufclAlgo;
    	ModelImage ufclImage;
    	double srcBuffer[];
    	int levelBuffer[];
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	int numLevels;
    	boolean isMin[];
    	int minLabel[];
    	int numMins;
    	int rep;
    	int smallestMinIndex[];
    	int smallestMinValue[];
    	double maxSlope; 
    	int minIndex[];
    	int numLowestNeighbors = 0;
    	double epsilon = 1.0e-6;
    	boolean equalPresent;
    	Queue <Integer> fifo = new LinkedList<Integer>();
    	boolean added;
    	int numberEqual;
    	int equalIndex[];
    	int k;
    	int gamma[][];
    	int con;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Union Find Watershed ...");
        
        lcImage = new ModelImage(ModelStorageBase.INTEGER, srcImage.getExtents(), 
        		srcImage.getImageName());
        lcAlgo = new AlgorithmLowerCompletion(lcImage, srcImage, neighbor8, limitBins, binNumber);
        lcAlgo.run();
        lcAlgo.finalize();
        lcAlgo = null;
        
        ufclImage = new ModelImage(ModelStorageBase.INTEGER, srcImage.getExtents(), 
        		srcImage.getImageName());
        ufclAlgo = new AlgorithmUnionFindComponentLabelling(ufclImage, srcImage, neighbor8, limitBins, binNumber);
        ufclAlgo.run();
        ufclAlgo.finalize();
        ufclAlgo = null;
        
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
        
        if (neighbor8) {
        	con = 8;
        }
        else {
        	con = 4;
        }
        
        try {
            imgBuffer = new int[length];
            labelBuffer = new int[length];
            sln = new int[length][];
            gamma = new int[length][];
            srcBuffer = new double[length];
            levelBuffer = new int[length];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Union Find Watershed: Out of memory creating buffers");
            setCompleted(false);

            return;
        }
        
        for (t = 0; t < tDim; t++) {
            for (z = 0; z < zDim; z++) {
            	
            	try {
                    srcImage.exportData((z + t*zDim)*length, length, srcBuffer);
                } catch (IOException error) {
                    displayError("Algorithm Sequential Scanning Watershed: image bounds exceeded");
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
            	    	srcBuffer[i] = Math.min((binNumber-1), Math.floor((srcBuffer[i]-minValue)*scale + 0.5));
            	    }
                } // if (limitBins)

            	
            	try {
                    lcImage.exportData((z + t*zDim)*length, length, imgBuffer);
                } catch (IOException error) {
                    displayError("Algorithm Union Find Watershed: image bounds exceeded");
                    setCompleted(false);
                    
                    lcImage.releaseLock();

                    return;
                }
            	
            	try {
                    ufclImage.exportData((z + t*zDim)*length, length, levelBuffer);
                } catch (IOException error) {
                    displayError("Algorithm Sequential Scanning Watershed: image bounds exceeded");
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
	                   if ((x > 0) && (srcBuffer[i] > srcBuffer[i-1])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if ((x < xDim-1) && (srcBuffer[i] > srcBuffer[i+1])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if ((y > 0) && (srcBuffer[i] > srcBuffer[i-xDim])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if ((y < yDim-1) && (srcBuffer[i] > srcBuffer[i+xDim])) {
	                	   isMin[levelBuffer[i]-1] = false;
	                   }
	                   if (neighbor8) {
	                	   if ((x > 0) && (y > 0) && (srcBuffer[i] > srcBuffer[i-xDim-1])) {
	                		   isMin[levelBuffer[i]-1] = false;   
	                	   }
	                	   if ((x < xDim-1) && (y > 0) && (srcBuffer[i] > srcBuffer[i-xDim+1])) {
	                		   isMin[levelBuffer[i]-1] = false;   
	                	   }
	                	   if ((x > 0) && (y < yDim-1) && (srcBuffer[i] > srcBuffer[i+xDim-1])) {
	                		   isMin[levelBuffer[i]-1] = false;   
	                	   }
	                	   if ((x < xDim-1) && (y < yDim-1) && (srcBuffer[i] > srcBuffer[i+xDim+1])) {
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
                	labelBuffer[i] = INIT;
                }
                
               smallestMinIndex = new int[numMins];
                smallestMinValue = new int[numMins];
                for (i = 0; i < numMins; i++) {
                	smallestMinValue[i] = Integer.MAX_VALUE;
                }
                for (i = 0; i < length; i++) {
                	if (isMin[levelBuffer[i]-1]) {
                		labelBuffer[i] = minLabel[levelBuffer[i]-1];
                		if (imgBuffer[i] < smallestMinValue[labelBuffer[i]-1]) {
                			smallestMinValue[labelBuffer[i]-1] = imgBuffer[i];
                			smallestMinIndex[labelBuffer[i]-1] = i;
                		}
                	}
                }
                
                minIndex = new int[con];
                equalIndex = new int[con];
                fifo.clear();
                for (i = 0; i < length; i++) {
                	if (gamma[i] == null) {
                	    x = i % xDim;
                	    y = i / xDim;
                	    maxSlope = 0.0;
                	    numLowestNeighbors = 0;
                	    if ((x > 0) && (imgBuffer[i]-imgBuffer[i-1] > 0)) {
                	    	maxSlope = imgBuffer[i] - imgBuffer[i-1];
                	        numLowestNeighbors = 1;
                	        minIndex[0] = i-1;
                	    }
                	    if ((x < xDim - 1) && (imgBuffer[i] - imgBuffer[i+1] > 0)) {
	                	    if (imgBuffer[i] - imgBuffer[i+1] > maxSlope) {
	                	    	maxSlope = imgBuffer[i+1] - imgBuffer[i];
	                	    	numLowestNeighbors = 1;
	                	    	minIndex[0] = i+1;
	                	    }
	                	    else if (imgBuffer[i] - imgBuffer[i+1] == maxSlope) {
	                	    	minIndex[numLowestNeighbors++] = i+1;
	                	    }
                	    } // if ((x < xDim - 1) && (imgBuffer[i] - imgBuffer[i+1] > 0))
                	    if ((y > 0) && (imgBuffer[i] - imgBuffer[i-xDim] > 0)) {
                	    	if (imgBuffer[i] - imgBuffer[i-xDim] > maxSlope) {
                	    		maxSlope = imgBuffer[i] - imgBuffer[i-xDim];
	                	    	numLowestNeighbors = 1;
	                	    	minIndex[0] = i-xDim;
	                	    }
	                	    else if (imgBuffer[i] - imgBuffer[i-xDim] == maxSlope) {
	                	    	minIndex[numLowestNeighbors++] = i-xDim;
	                	    }	
                	    } // if ((y > 0) && (imgBuffer[i] - imgBuffer[i-xDim] > 0))
                	    if ((y < yDim-1) && (imgBuffer[i] - imgBuffer[i+xDim] > 0)) {
                	        if (imgBuffer[i] - imgBuffer[i+xDim] > maxSlope) {
                	        	maxSlope = imgBuffer[i] - imgBuffer[i+xDim];
                	        	numLowestNeighbors = 1;
                	        	minIndex[0] = i+xDim;
                	        }
                	        else if (imgBuffer[i] - imgBuffer[i+xDim] == maxSlope) {
                	        	minIndex[numLowestNeighbors++] = i+xDim;
                	        }
                	    } // if ((y < yDim-1) && (imgBuffer[i] - imgBuffer[i+xDim] > 0))
                	    if (neighbor8) {
                	    	if ((x > 0) && (y > 0) && (imgBuffer[i] - imgBuffer[i-xDim-1] > 0)) {
	                	    	if (((imgBuffer[i] - imgBuffer[i-xDim-1])/sqrt2) - epsilon > maxSlope) {
	                	    		maxSlope = (imgBuffer[i] - imgBuffer[i-xDim-1])/sqrt2;
	                	    		numLowestNeighbors = 1;
	                	    		minIndex[0] = i-xDim-1;
	                	    	}
	                	    	else if (Math.abs(((imgBuffer[i] - imgBuffer[i-xDim-1])/sqrt2) - maxSlope) <= epsilon) {
	                	    		minIndex[numLowestNeighbors++] = i-xDim-1;
	                	    	}
                	    	} // if ((x > 0) && (y > 0) && (imgBuffer[i] - imgBuffer[i-xDim-1] > 0))
                	    	if ((x > 0) && (y < yDim-1) && (imgBuffer[i] - imgBuffer[i+xDim-1] > 0)) {
	                	    	if (((imgBuffer[i] - imgBuffer[i+xDim-1])/sqrt2) - epsilon > maxSlope) {
	                	    		maxSlope = (imgBuffer[i] - imgBuffer[i+xDim-1])/sqrt2;
	                	    		numLowestNeighbors = 1;
	                	    		minIndex[0] = i+xDim-1;
	                	    	}
	                	    	else if (Math.abs(((imgBuffer[i] - imgBuffer[i+xDim-1])/sqrt2) - maxSlope) <= epsilon) {
	                	    		minIndex[numLowestNeighbors++] = i+xDim-1;
	                	    	}
                	    	} // if ((x > 0) && (y < yDim-1) && (imgBuffer[i] - imgBuffer[i+xDim-1] > 0))
                	    	if ((x < xDim-1) && (y > 0) && (imgBuffer[i] - imgBuffer[i-xDim+1] > 0)) {
	                	    	if (((imgBuffer[i] - imgBuffer[i-xDim+1])/sqrt2) - epsilon > maxSlope) {
	                	    		maxSlope = (imgBuffer[i] - imgBuffer[i-xDim+1])/sqrt2;
	                	    		numLowestNeighbors = 1;
	                	    		minIndex[0] = i-xDim+1;
	                	    	}
	                	    	else if (Math.abs(((imgBuffer[i] - imgBuffer[i-xDim+1])/sqrt2) - maxSlope) <= epsilon) {
	                	    		minIndex[numLowestNeighbors++] = i-xDim+1;
	                	    	}
                	    	} // if ((x < xDim-1) && (y > 0) && (imgBuffer[i] - imgBuffer[i-xDim+1] > 0))
                	    	if ((x < xDim-1) && (y < yDim-1) && (imgBuffer[i] - imgBuffer[i+xDim+1] > 0)) {
	                	    	if (((imgBuffer[i] - imgBuffer[i+xDim+1])/sqrt2) - epsilon > maxSlope) {
	                	    		maxSlope = (imgBuffer[i] - imgBuffer[i+xDim+1])/sqrt2;
	                	    		numLowestNeighbors = 1;
	                	    		minIndex[0] = i+xDim+1;
	                	    	}
	                	    	else if (Math.abs(((imgBuffer[i] - imgBuffer[i+xDim+1])/sqrt2) - maxSlope) <= epsilon) {
	                	    		minIndex[numLowestNeighbors++] = i+xDim+1;
	                	    	}
                	    	} // if ((x < xDim-1) && (y < yDim-1) && (imgBuffer[i] - imgBuffer[i+xDim+1] > 0))
                	    } // if (neighbor8)
                	    if (numLowestNeighbors > 0) {
                	    	gamma[i] = new int[numLowestNeighbors];
                	    	for (j = 0; j < numLowestNeighbors; j++) {
                	    		gamma[i][j] = minIndex[j];
                	    	}
                	    } // if (numLowestNeighbors > 0)
                	    else { // numNeighbors == 0
                	    	gamma[i] = new int[1];
                	    	gamma[i][0] = i;
                	    	equalPresent = false;
                	    	if ((x > 0) && (imgBuffer[i] == imgBuffer[i-1])) {
                	    		equalPresent = true;
                	    	}
                	    	else if ((x < xDim-1) && (imgBuffer[i] == imgBuffer[i+1])) {
                	    		equalPresent = true;
                	    	}
                	    	else if ((y > 0) && (imgBuffer[i] == imgBuffer[i-xDim])) {
                	    		equalPresent = true;
                	    	}
                	    	else if ((y < yDim-1) && (imgBuffer[i] == imgBuffer[i+xDim])) {
                	    		equalPresent = true;
                	    	}
                	    	else if (neighbor8) {
                	    		if ((x > 0) && (y > 0) && (Math.abs((imgBuffer[i] - imgBuffer[i-xDim-1])/sqrt2) <= epsilon)) {
                	    		    equalPresent = true;	
                	    		}
                	    		else if ((x > 0) && (y < yDim-1) && (Math.abs((imgBuffer[i] - imgBuffer[i+xDim-1])/sqrt2) <= epsilon)) {
                	    		    equalPresent = true;	
                	    		}
                	    		else if ((x < xDim-1) && (y > 0) && (Math.abs((imgBuffer[i] - imgBuffer[i-xDim+1])/sqrt2) <= epsilon)) {
                	    		    equalPresent = true;	
                	    		}
                	    		else if ((x < xDim-1) && (y < yDim-1) && (Math.abs((imgBuffer[i] - imgBuffer[i+xDim+1])/sqrt2) <= epsilon)) {
                	    		    equalPresent = true;	
                	    		}
                	    	} // else if (neighbor8)
                	    	if (equalPresent) {
                	    		added = fifo.offer(i);
                            	if (!added) {
                    				MipavUtil.displayError("Failure to add " + i + " to the fifo");
                    				setCompleted(false);
                    				return;
                    			}	
                            	while (!fifo.isEmpty()) {
                            	    j = fifo.poll();
                            	    numberEqual = 0;;
                        	    	if ((x > 0) && (imgBuffer[j] == imgBuffer[j-1])) {
                        	    		equalIndex[numberEqual++] = j-1;
                        	    	}
                        	    	if ((x < xDim-1) && (imgBuffer[j] == imgBuffer[j+1])) {
                        	    		equalIndex[numberEqual++] = j+1;
                        	    	}
                        	    	if ((y > 0) && (imgBuffer[j] == imgBuffer[j-xDim])) {
                        	    		equalIndex[numberEqual++] = j-xDim;
                        	    	}
                        	    	if ((y < yDim-1) && (imgBuffer[j] == imgBuffer[j+xDim])) {
                        	    		equalIndex[numberEqual++] = j+xDim;
                        	    	}
                        	    	if (neighbor8) {
                        	    		if ((x > 0) && (y > 0) && (Math.abs((imgBuffer[j] - imgBuffer[j-xDim-1])/sqrt2) <= epsilon)) {
                        	    		    equalIndex[numberEqual++] = j-xDim-1;	
                        	    		}
                        	    		if ((x > 0) && (y < yDim-1) && (Math.abs((imgBuffer[j] - imgBuffer[j+xDim-1])/sqrt2) <= epsilon)) {
                        	    		    equalIndex[numberEqual++] = j + xDim-1;	
                        	    		}
                        	    		if ((x < xDim-1) && (y > 0) && (Math.abs((imgBuffer[j] - imgBuffer[j-xDim+1])/sqrt2) <= epsilon)) {
                        	    		    equalIndex[numberEqual++] = j-xDim+1;	
                        	    		}
                        	    		if ((x < xDim-1) && (y < yDim-1) && (Math.abs((imgBuffer[j] - imgBuffer[j+xDim+1])/sqrt2) <= epsilon)) {
                        	    		    equalIndex[numberEqual++] = j+xDim+1;	
                        	    		}
                        	    	} // else if (neighbor8)
                        	    	for (k = 0; k < numberEqual; k++) {
                        	    	    if (gamma[equalIndex[k]] == null) {
                        	    	        gamma[equalIndex[k]] = new int[1];
                        	    	        gamma[equalIndex[k]][0] = i;
                        	    	        added = fifo.offer(equalIndex[k]);
                                        	if (!added) {
                                				MipavUtil.displayError("Failure to add " + equalIndex[k] + " to the fifo");
                                				setCompleted(false);
                                				return;
                                			}	
                        	    	    } //  if (gamma[equalIndex[k]] == null) 
                        	    	} // for (k = 0; k < numberEqual; k++)
                            	} // while (!fifo.isEmpty())
                	    	} // if (equalPresent)
                	    } // else numNeighbors == 0
                	} // if (gamma[i] == null)
                	if (gamma[i] != null) {
                		sln[i] = new int[gamma[i].length];
                		for (j = 0; j < gamma[i].length; j++) {
                		     sln[i][j] = gamma[i][j];	
                		}
                	} // if (gamma[i] != null)
                } // for (i = 0; i < length; i++)
            	
            	
            	
            	// Give i the label of its representative
            	for (i = 0; i < length; i++) {
            	    rep = resolve(i);
            	    if (rep != W) {
            	    	labelBuffer[i] = labelBuffer[rep];
            	    }
            	    else {
            	    	labelBuffer[i] = WSHED;
            	    }
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
        lcImage.disposeLocal();
        lcImage = null;
        destImage.calcMinMax();
        
        setCompleted(true);
        return;
	}
	
	private int resolve(int p) {
		// Recursive function for resolving the downstream paths of the lower complete graph
		// Returns the representative element of pixel p, or W if p is a watershed pixel
		int i;
		int j;
		int rep;
		int conn;
		i = 0;
		rep = 0; // some value such that rep != W
		if (sln[p] == null) {
			conn = 0;
		}
		else {
			conn = sln[p].length;
		}
		while ((i < conn) && (rep != W)) {
		    if ((sln[p][i] != p) && (sln[p][i] != W)) {
		    	sln[p][i] = resolve(sln[p][i]);
		    }
		    if (i == 0) {
		    	rep = sln[p][0];
		    } // if (i == 0)
		    else if (sln[p][i] != rep) {
		        rep = W;
		        for (j = 0; j < conn; j++) {
		            sln[p][j] = W;	
		        } // for (j = 0; j < con; j++)
		    } // else if (sln[p][i] != rep)
		    i++;
		} // while ((i < con) && (rep != W))
		return rep; 
	}
}