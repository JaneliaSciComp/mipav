package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;

/**  This software implements the factorization-based segmentation algorithm. 
 *   The original code was written in MATLAB and C by:
 *   Jiangye Yuan
 *   Computational Sciences and and Engineering Division
 *   Oak Ridge National Laboratory, Oak Ridge, Tennessee 37831
 *   yuanj@ornl.gov
 *   
 *   Reference:

     [1] J. Yuan and D. L. Wang. Factorization-based texture segmentation. Technical Report OSU-CISRC-1/13 -TR01, 2013.
     The website for this code is:
     https://sites.google.com/site/factorizationsegmentation/
     
     This code was ported to Java by William Gandler
 */

public class AlgorithmTextureSegmentation extends AlgorithmBase implements AlgorithmInterface {
	
	private static final int logOp = 1;
	
	private int operationType = logOp;
	
	private double log[];
	
	private int windowSize = 25;
	
	// Number of segments.  Determined automatically if set to 0.
	private int segmentNumber = 0;
	
	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------
	public AlgorithmTextureSegmentation(ModelImage destImage, ModelImage srcImage,
			                            int windowSize, int segmentNumber) {
		super(destImage, srcImage);
		this.windowSize = windowSize;
		this.segmentNumber = segmentNumber;
	}
	
	public void runAlgorithm() {
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[0];
		int length = xDim * yDim;
		double srcBuffer[];
		double Ig[][];
		int i;
		int kExtents[] = new int[2];
		int halfMask;
		double GData[];
		double denom;
		double sigma;
		double kd;
		int x;
		int y;
		double distSquared;
		AlgorithmDConvolver convolver;
		boolean entireImage = true;
		int expandedSize;
		double expandedBuffer[];
		int extents[] = new int[2];
		ModelImage expandedImage;
		if (srcImage.isColorImage()) {
			
		}
		else {
			// Segment images with heavy texture
			// This code segments gray level images\
			srcBuffer = new double[length];
			try {
				srcImage.exportData(0, length, srcBuffer);
			}
			catch(IOException e) {
				MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, length, srcBuffer");
				setCompleted(false);
				return;
			}
			
			Ig = new double[length][11];
			for (i = 0; i < length; i++) {
			    Ig[i][0] = srcBuffer[i];	
			}
			
			for (i = 1; i <= 2; i++) {
				if (i == 1) {
				    halfMask = 1;
				    sigma = 0.5;
				}
				else {
					halfMask = 2;
					sigma = 1.0;
				}
				expandedSize = (xDim + 2 * halfMask) * (yDim + 2 * halfMask);
	        	expandedBuffer = new double[expandedSize];
	        	for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			expandedBuffer[x + halfMask + (y + halfMask) * (xDim + 2 * halfMask)] = srcBuffer[x + y * xDim];
	        		}
	        	}
	        	for (x = 0; x < halfMask; x++) {
	        		for (y = 0; y < halfMask; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[0];	
	        		}
	        		
	        		for (y = halfMask; y <= yDim + halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(halfMask - 1 - x) + 
	        			                                                          (y - halfMask)* xDim];	
	        		}
	        		
	        		for (y = yDim + halfMask; y <= yDim + 2*halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(yDim - 1)* xDim];	
	        		}
	        	} // for (x = 0; x < halfMask; x++)
	        	
	        	for (x = xDim + halfMask; x < xDim + 2 * halfMask; x++) {
	        		for (y = 0; y < halfMask; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[xDim-1];	
	        		}
	        		
	        		for (y = halfMask; y <= yDim + halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(y - halfMask)* xDim + 
	        			                                                          + (xDim- 1 - (x - xDim - halfMask))];	
	        		}
	        		
	        		for (y = yDim + halfMask; y <= yDim + 2*halfMask - 1; y++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(yDim - 1)* xDim + xDim - 1];	
	        		}	
	        	} // for (x = xDim + halfMask; x < xDim + 2 * halfMask; x++)
	        	
	        	for (y = 0; y < halfMask; y++) {
	        		for (x = halfMask; x <= xDim + halfMask - 1; x++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[x - halfMask +
	        			                                                          (halfMask - 1 - y) * xDim];		
	        		}
	        	}
	        	
	        	for (y = yDim + halfMask; y < yDim + 2*halfMask; y++) {
	        		for (x = halfMask; x <= xDim + halfMask - 1; x++) {
	        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = 
	        					srcBuffer[(yDim - 1 - (y - yDim - halfMask))* xDim + x - halfMask];		
	        		}
	        	}
	        	
	        	extents[0] = xDim + 2 * halfMask;
	        	extents[1] = yDim + 2 * halfMask;
	        	expandedImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "expandedImage");
	        	try {
	        		expandedImage.importData(0, expandedBuffer, true);
	        	}
	        	catch(IOException e) {
	        		MipavUtil.displayError("IOException " + e + " on expandedImage.importData(0, expandedBuffer, true)");
	        		setCompleted(false);
	        		return;
	        	}
				kExtents[0] = 2*halfMask + 1;
	        	kExtents[1] = 2*halfMask + 1;
	        	GData = new double[kExtents[0] * kExtents[1]];
	        	denom = 2.0 * sigma * sigma;
	        	kd = -1.0/(Math.PI * sigma * sigma);
	        	for (y = -halfMask; y <= halfMask; y++) {
	        		for (x = -halfMask; x <= halfMask; x++) {
	        		    distSquared = x * x + y * y;
	        		    GData[(x + halfMask) + (y + halfMask) * kExtents[0]] = (kd * (1.0 - distSquared/denom) *
	        		    		         Math.exp(-distSquared/denom));
	        		}
	        	} // for (y = -halfMask; y <= halfMask; y++)
	        	
	        	convolver = new AlgorithmDConvolver(expandedImage, GData, kExtents,entireImage, image25D);
		        convolver.addListener(this);
		        operationType = logOp;
		        convolver.run();
		        for (y = halfMask; y <= yDim + halfMask -1; y++) {
		        	for (x = halfMask; x <= xDim + halfMask - 1; x++) {
		        	    Ig[(x - halfMask) + (y - halfMask)*xDim][i]= log[x + y * (xDim + 2 * halfMask)];	
		        	}
		        }
			} // for (i = 1; i <= 2; i++)
		} // else 
		
	}
	
	public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmDConvolver) {
            AlgorithmDConvolver convolver = (AlgorithmDConvolver) algorithm;
            if (operationType == logOp) {
               log = convolver.getOutputBuffer();
            }
            
        }
    }
	
}