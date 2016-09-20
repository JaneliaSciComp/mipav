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
 * Algorithm 4.2 Watershed transform w.r.t. topographcial distance based on image integration
 * via the Dijkstra-Moore shortest paths algorithms
 */

public class AlgorithmShortestPathWatershed extends AlgorithmBase {
	
    private boolean neighbor8;
	
    private boolean limitBins;
	
    private int binNumber;
    
    private int xDim;
    
    private int yDim;
    
    private int imgBuffer[];
    
	private int labelBuffer[];
	
	// On output distBuffer[i] = imgBuffer[i] for all i.
	private double distBuffer[];
	
	private final double sqrt2 = Math.sqrt(2.0);
	
	// Label of the watershed pixels
	private final int WSHED = 0;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	public AlgorithmShortestPathWatershed(ModelImage destImage, ModelImage srcImage, boolean neighbor8, boolean limitBins,
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
    	boolean pixelsCalculated[];
    	int pixelsLeft;
    	double smallestDistance;
    	int smallestIndex;
    	int v[];
    	int numNeighbors;
    	double c;
    	double eps = 1.0e-6;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Sequential Scanning Watershed ...");
        
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
            distBuffer = new double[length];
            srcBuffer = new double[length];
            pixelsCalculated = new boolean[length];
            if (neighbor8) {
            	v = new int[8];
            }
            else {
            	v = new int[4];
            }
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Sequential Scanning Watershed: Out of memory creating buffers");
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
                    displayError("Algorithm Sequential Scanning Watershed: image bounds exceeded");
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
                	labelBuffer[i] = 0;
                	distBuffer[i] = Double.MAX_VALUE;
                }
                
                for (i = 0; i < length; i++) {
                	if (isMin[levelBuffer[i]-1]) {
                		labelBuffer[i] = minLabel[levelBuffer[i]-1];
                		distBuffer[i] = imgBuffer[i];
                	}
                }
                
                for (i = 0; i < length; i++) {
                	pixelsCalculated[i] = false;
                }
                pixelsLeft = length;
                while (pixelsLeft > 0) {
                    smallestDistance = Double.MAX_VALUE;
                    smallestIndex = -1;
                    for (i = 0; i < length; i++) {
                    	if (!pixelsCalculated[i]) {
                    		if (distBuffer[i] < smallestDistance) {
                    			smallestDistance = distBuffer[i];
                    			smallestIndex = i;
                    		}
                    	} // if (!pixelsCalculated[i])
                    } // for (i = 0; i < length; i++)
                    pixelsCalculated[smallestIndex] = true;
                    pixelsLeft--;
                    x = smallestIndex % xDim;
                    y = smallestIndex / xDim;
                    numNeighbors = 0;
                    if ((x > 0) && (!pixelsCalculated[smallestIndex-1])) {
                    	v[numNeighbors++] = smallestIndex-1;
                    }
                    if ((x < xDim-1) && (!pixelsCalculated[smallestIndex+1])) {
                    	v[numNeighbors++] = smallestIndex+1;
                    }
                    if ((y > 0) && (!pixelsCalculated[smallestIndex-xDim])) {
                    	v[numNeighbors++] = smallestIndex-xDim;
                    }
                    if ((y < yDim-1) && (!pixelsCalculated[smallestIndex+xDim])) {
                    	v[numNeighbors++] = smallestIndex+xDim;
                    }
                    if (neighbor8) {
                    	if ((x > 0) && (y > 0) && (!pixelsCalculated[smallestIndex-xDim-1])) {
                    		v[numNeighbors++] = smallestIndex-xDim-1;
                    	}
                    	if ((x > 0) && (y < yDim-1) && (!pixelsCalculated[smallestIndex+xDim-1])) {
                    		v[numNeighbors++] = smallestIndex+xDim-1;
                    	}
                    	if ((x < xDim-1) && (y > 0) && (!pixelsCalculated[smallestIndex-xDim+1])) {
                    		v[numNeighbors++] = smallestIndex-xDim+1;
                    	}
                    	if ((x < xDim-1) && (y < yDim-1) && (!pixelsCalculated[smallestIndex+xDim+1])) {
                    	    v[numNeighbors++] = smallestIndex+xDim+1;	
                    	}
                    } // if (neighbor8)
                    for (j = 0; j < numNeighbors; j++) {
            	    	c = cost(smallestIndex,v[j]);
            	        if (distBuffer[smallestIndex] + c < distBuffer[v[j]] - eps) {
            	        	distBuffer[v[j]] = distBuffer[smallestIndex] + c;
            	        	labelBuffer[v[j]] = labelBuffer[smallestIndex];
            	        }
            	        else if ((labelBuffer[v[j]] != WSHED) && (Math.abs(distBuffer[smallestIndex] + c - distBuffer[v[j]]) < eps) && 
            	        		      (labelBuffer[v[j]] != labelBuffer[smallestIndex])) {
            	        	labelBuffer[v[j]] = WSHED;
            	        }
                    } // for (j = 0; j < numNeighbors; j++) 
                } // while (pixelsLeft > 0)
                
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
	
	private double cost(int p, int q) {
		double dist = 0.0;
		double result = 0.0;
	    if (Math.abs(p-q) == 1) {
	        dist = 1.0;	
	    }
	    else if (Math.abs(p-q) == xDim) {
	    	dist = 1.0;
	    }
	    else if (Math.abs(p-q) == xDim+1) {
	    	dist = sqrt2;
	    }
	    if (imgBuffer[p] > imgBuffer[q]) {
	    	result = LS(p) * dist;
	    }
	    else if (imgBuffer[p] < imgBuffer[q]) {
	    	result = LS(q) * dist;
	    }
	    else {
	    	result = 0.5 * (LS(p) + LS(q)) * dist;
	    }
	    return result;
	}
	
	private double LS(int p) {
		int x;
		int y;
		double lowerSlope = 0.0;
		x = p % xDim;
		y = p /xDim;
		if ((x > 0) && (imgBuffer[p] > imgBuffer[p-1])) {
			lowerSlope = imgBuffer[p]-imgBuffer[p-1];
		}
		if ((x < xDim-1) && (imgBuffer[p] > imgBuffer[p+1])) {
			lowerSlope = Math.max(lowerSlope, (imgBuffer[p]-imgBuffer[p+1]));
		}
		if ((y > 0) && (imgBuffer[p] > imgBuffer[p-xDim])) {
			lowerSlope = Math.max(lowerSlope, (imgBuffer[p]-imgBuffer[p-xDim]));
		}
		if ((y < yDim-1) && (imgBuffer[p] > imgBuffer[p+xDim])) {
			lowerSlope = Math.max(lowerSlope, (imgBuffer[p]-imgBuffer[p+xDim]));	
		}
		if (neighbor8) {
			if ((x > 0) && (y > 0) && (imgBuffer[p] > imgBuffer[p-xDim-1])) {
				lowerSlope = Math.max(lowerSlope, (imgBuffer[p]-imgBuffer[p-xDim-1])/sqrt2);
			}
			if ((x > 0) && (y < yDim-1) && (imgBuffer[p] > imgBuffer[p+xDim-1])) {
				lowerSlope = Math.max(lowerSlope, (imgBuffer[p]-imgBuffer[p+xDim-1])/sqrt2);
			}
			if ((x < xDim-1) && (y > 0) && (imgBuffer[p] > imgBuffer[p-xDim+1])) {
				lowerSlope = Math.max(lowerSlope, (imgBuffer[p]-imgBuffer[p-xDim+1])/sqrt2);
			}
			if ((x < xDim-1) && (y < yDim-1) && (imgBuffer[p] > imgBuffer[p+xDim+1])) {
				lowerSlope = Math.max(lowerSlope, (imgBuffer[p]-imgBuffer[p+xDim+1])/sqrt2);
			}
		} // if (neighbor8)
		return lowerSlope;
	}
}