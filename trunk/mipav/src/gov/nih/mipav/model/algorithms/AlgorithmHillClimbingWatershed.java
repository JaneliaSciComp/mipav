package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

/**
 * 
 * @author ilb
 * Reference:
 * 1.) The Watershed Transform: Definitions, Algorithms, and Parallelization Strategies by 
 * Jos B.T.M. Roerdink and Arnold Meijster, Fundamentals Informaticae 41 (2001), pp. 187-228.
 * Algorithm 4.3 Watershed transform w.r.t. topographcial distance by hill climbing
 * From Section 4.2.3 Hill Climbing use distance values of 1 for both 4 and 8 connected
 * in calculating steepest upper neighbors.
 */

public class AlgorithmHillClimbingWatershed extends AlgorithmBase {
	
    private boolean neighbor8;
	
    private boolean limitBins;
	
    private int binNumber;
    	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmHillClimbingWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8, boolean limitBins,
			int binNumber) {
		super(destImage, srcImage);
		this.neighbor8 = neighbor8;
		this.limitBins = limitBins;
		this.binNumber = binNumber;
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
    	double srcBuffer[];
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
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	boolean S[];
    	int numberS;
    	boolean memberS;
    	int smallestIndex;
    	int smallestValue;
    	int steepestUpperNeighbors;
    	int v[];
    	double steepestSlope;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Hill Climbing Watershed ...");
        
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
        
        try {
            imgBuffer = new int[length];
            labelBuffer = new int[length];
            levelBuffer = new int[length];
            srcBuffer = new double[length];
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
                    srcImage.exportData((z + t*zDim)*length, length, srcBuffer);
                } catch (IOException error) {
                    displayError("Algorithm Hill Climbing Watershed: image bounds exceeded");
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
}