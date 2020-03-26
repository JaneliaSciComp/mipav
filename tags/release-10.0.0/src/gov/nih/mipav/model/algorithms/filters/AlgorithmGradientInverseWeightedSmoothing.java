package gov.nih.mipav.model.algorithms.filters;

import java.io.IOException;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


// Reference: "Gradient Inverse Weighted Smoothing Scheme and the Evaluation of its Performance" by David C. C. Wang,
// Anthony H. Vagnucci, and C. C. Li, Computer Graphics and Image processing, 15, 1981, pp. 167-181.

public class AlgorithmGradientInverseWeightedSmoothing extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
    private int iterations;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------
	
	/**
	 * 
	 * @param srcImg
	 * @param iterations
	 */
    public AlgorithmGradientInverseWeightedSmoothing(ModelImage srcImg, int iterations) {
		super(null, srcImg);
		this.iterations = iterations;
	}
	
	/**
	 * 
	 * @param destImg
	 * @param srcImg
	 * @param iterations
	 */
    public AlgorithmGradientInverseWeightedSmoothing(ModelImage destImg, ModelImage srcImg, int iterations) {
		super(destImg, srcImg);
		this.iterations = iterations;
	}
	
	/**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int nDims;
    	int sliceSize;
    	double buffer[];
    	double result[];
    	double temp[];
    	int x, y, z, t;
    	int delx, dely;
    	double sum;
    	double mask[][] = new double[3][3];
    	int iter;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(), "Gradient Inverse Weighted Smoothing ...");
        
        nDims = srcImage.getNDims();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        if (nDims > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        sliceSize = xDim * yDim;
        buffer = new double[sliceSize];
        result = new double[sliceSize];
        mask[1][1] = 0.5;
        
        for (iter = 0; iter < iterations; iter++) {
	        for (t = 0; t < tDim; t++) {
	        	for (z = 0; z < zDim; z++) {
	        		if (iter == 0) {
		        		try {
		        			srcImage.exportData((z + t*zDim)*sliceSize, sliceSize, buffer);
		        		}
		        		catch(IOException e) {
		        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
		        			setCompleted(false);
		        			return;
		        		}
	        		} // if (iter == 0)
	        		for (x = 0; x < xDim; x++) {
	        			// y = 0;
	        			result[x] = buffer[x];
	        			// y = yDim -1
	        			result[(yDim-1)*xDim + x] = buffer[(yDim-1)*xDim+x];
	        		}
	        		
	        		for (y = 1; y < yDim-1; y++) {
	        			// x = 0;
	        			result[y*xDim] = buffer[y*xDim];
	        			// x = xDim-1
	        			result[y*xDim + xDim-1] = buffer[y*xDim + xDim-1];
	        		}
	        		
	        		for (y = 1; y < yDim-1; y++) {
	        			for (x = 1; x < xDim-1; x++) {
	        				sum = 0.0;
	        			    for (dely = -1; dely <= 1; dely++) {
	        			    	for (delx = -1; delx <= 1; delx++) {
	        			    		if ((dely != 0) || (delx != 0)) {
	        			    			if (buffer[(y+dely)*xDim + (x+delx)] == buffer[y*xDim + x]) {
	        			    				// Gradient is zero
	        			    				mask[dely+1][delx+1] = 2.0;
	        			    			}
	        			    			else {
	        			    				mask[dely+1][delx+1] =
	        			    						1.0/Math.abs(buffer[(y+dely)*xDim + (x+delx)] - buffer[y*xDim + x]);
	        			    			}
	        			    			sum += mask[dely+1][delx+1];
	        			    		}
	        			    	}
	        			    } // for (dely = -1; dely <= 1; dely++)
	        			    for (dely = -1; dely <= 1; dely++) {
	        			    	for (delx = -1; delx <= 1; delx++) {
	        			    		if ((dely != 0) || (delx != 0)) {
	        			    			mask[dely+1][delx+1] = mask[dely+1][delx+1]/(2.0*sum);
	        			    		}
	        			    	}
	        			    } // for (dely = -1; dely <= 1; dely++)
	        			    result[y*xDim+x] = 0;
	        			    for (dely = -1; dely <= 1; dely++) {
	        			    	for (delx = -1; delx <= 1; delx++) {
	        			    		result[y*xDim + x] += (mask[dely+1][delx+1] * buffer[(y+dely)*xDim + (x+delx)]);
	        			    	}
	        			    }
	        			} // for (x = 1; x < xDim-1; x++)
	        		} // for (y = 1; y < yDim-1; y++)
	        		if (iter == iterations-1) {
		        		if (destImage != null) {
		        			try {
		        			    destImage.importData((z + t*zDim)*sliceSize, result, false);
		        			}
		        			catch(IOException e) {
		        				MipavUtil.displayError("IOException " + e + " on destImage.importData");
		        				setCompleted(false);
		        				return;
		        			}
		        		}
		        		else {
		        			try {
		        			    srcImage.importData((z + t*zDim)*sliceSize, result, false);
		        			}
		        			catch(IOException e) {
		        				MipavUtil.displayError("IOException " + e + " on srcImage.importData");
		        				setCompleted(false);
		        				return;
		        			}
		        		}
	        		} // if (iter == iterations-1)
	        		else {
	        			temp = result;
	        			result = buffer;
	        			buffer = temp;
	        		}
	        	} // for (z = 0; z < zDim; z++)
	        } // for (t = 0; t < tDim; t++)
        } // for (iter = 0; iter < iterations; iter++)
        if (destImage != null) {
        	destImage.calcMinMax();
        }
        else {
        	srcImage.calcMinMax();
        }
        setCompleted(true);
        return;
    }
}