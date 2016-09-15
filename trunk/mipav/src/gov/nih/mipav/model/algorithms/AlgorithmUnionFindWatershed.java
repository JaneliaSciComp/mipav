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
    	int numLowerNeighbors;
    	ArrayList <indexSlopeItem> indexSlopeList = new ArrayList<indexSlopeItem>();
    	double minValue;
    	double maxValue;
    	double range;
    	double scale;
    	int numLevels;
    	boolean isMin[];
    	int minLabel[];
    	int numMins;
    	int rep;
    	
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
        
        try {
            imgBuffer = new int[length];
            labelBuffer = new int[length];
            sln = new int[length][];
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
                	labelBuffer[i] = 0;
                }
                
                for (i = 0; i < length; i++) {
                	if (isMin[levelBuffer[i]-1]) {
                		labelBuffer[i] = minLabel[levelBuffer[i]-1];
                	}
                }
            	
            	for (i = 0; i < length; i++) {
            		x = i % xDim;
            		y = i / xDim;
            		numLowerNeighbors = 0;
            		indexSlopeList.clear();
            		if ((x > 0) && (imgBuffer[i] > imgBuffer[i-1])) {
            			indexSlopeList.add(new indexSlopeItem(i-1, imgBuffer[i] - imgBuffer[i-1]));
            			numLowerNeighbors++;
            		}
            		if ((x < xDim-1) && (imgBuffer[i] > imgBuffer[i+1])) {
            			indexSlopeList.add(new indexSlopeItem(i+1, imgBuffer[i] - imgBuffer[i+1]));
            			numLowerNeighbors++;	
            		}
            		if ((y > 0) && (imgBuffer[i] > imgBuffer[i-xDim])) {
            			indexSlopeList.add(new indexSlopeItem(i-xDim, imgBuffer[i] - imgBuffer[i-xDim]));
            			numLowerNeighbors++;	
            		}
            		if ((y < yDim-1) && (imgBuffer[i] > imgBuffer[i+xDim])) {
            			indexSlopeList.add(new indexSlopeItem(i+xDim, imgBuffer[i] - imgBuffer[i+xDim]));
            			numLowerNeighbors++;	
            		}
            		if (neighbor8) {
            			if ((x > 0) && (y > 0) && (imgBuffer[i] > imgBuffer[i-xDim-1])) {
                			indexSlopeList.add(new indexSlopeItem(i-xDim-1, (imgBuffer[i] - imgBuffer[i-xDim-1])/sqrt2));
                			numLowerNeighbors++;
                		}
            			if ((x > 0) && (y < yDim-1) && (imgBuffer[i] > imgBuffer[i+xDim-1])) {
                			indexSlopeList.add(new indexSlopeItem(i+xDim-1, (imgBuffer[i] - imgBuffer[i+xDim-1])/sqrt2));
                			numLowerNeighbors++;
                		}
            			if ((x < xDim-1) && (y > 0) && (imgBuffer[i] > imgBuffer[i-xDim+1])) {
                			indexSlopeList.add(new indexSlopeItem(i-xDim+1, (imgBuffer[i] - imgBuffer[i-xDim+1])/sqrt2));
                			numLowerNeighbors++;
                		}
            			if ((x < xDim-1) && (y < yDim-1) && (imgBuffer[i] > imgBuffer[i+xDim+1])) {
                			indexSlopeList.add(new indexSlopeItem(i+xDim+1, (imgBuffer[i] - imgBuffer[i+xDim+1])/sqrt2));
                			numLowerNeighbors++;
                		}
            		} // if (neighbor8)
            		if (numLowerNeighbors > 1) {
            			Collections.sort(indexSlopeList, new indexSlopeComparator());
            		}
            		if (numLowerNeighbors > 0) {
            			sln[i] = new int[numLowerNeighbors];
            			for (j = 0; j < numLowerNeighbors; j++) {
            				sln[i][j] = indexSlopeList.get(i).index;
            			}
            		}
            	} // for (i = 0; i < length; i++)
            	
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
		int i;
		int rep;
		i = 1;
		rep = 0;
		return rep;
	}
	
	private class indexSlopeComparator implements Comparator<indexSlopeItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(indexSlopeItem o1, indexSlopeItem o2) {
            double a = o1.getSlope();
            double b = o2.getSlope();
            int i = o1.getIndex();
            int j = o2.getIndex();

            
            // Slopes in descending order
            if (a < b) {
            	return 1;
            }
            else if (b > a) {
            	return -1;
            }
            // Indices in ascending order
            else if (i > j) {
            	return 1;
            }
            else if (i < j) {
            	return -1;
            }
            else {
            	return 0;
            }
        }

    }
	
	private class indexSlopeItem {
		private int index;
		private double slope;
		
		public indexSlopeItem(int index, double slope) {
			this.index = index;
			this.slope = slope;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getSlope() {
			return slope;
		}
		
		
	}
}