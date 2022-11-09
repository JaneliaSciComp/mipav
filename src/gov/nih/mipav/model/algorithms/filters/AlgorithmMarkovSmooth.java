package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;

import gov.nih.mipav.view.ViewJProgressBar;

/**
 * 
 * @author ilb
 * This is a port of the MATLAB program restore_image.m written by PeterOrchard to Java.
 * This port is performed with the permission of Peter Orchard.
 * The original Markov Random Field Optimisation text with accompanying code is found at
 * http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/AV0809/ORCHARD/.
 * The code uses a technique invented specifically for Markov Random Field optimization
 * called Iterated Conditional Modes (ICM).  This code is used to smooth an image.
 * This restoration method does a good job of smoothing the noise from surfaces of constant
 * or slowly-varying intensity.  It does not do so well with thin, sharp features.  This is
 * due to the energy function punishing local differences in pixel values.
 */

public class AlgorithmMarkovSmooth extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	 /** The known covariance of the Gaussian noise */
    private double covariance;
    
    /** The maximum contribution to the potential of the difference between two neighboring pixel values. */
    private double max_diff;
    
    /** The weighting attached to the component of the potential due to the difference between two
        neighboring pixel values. */
    private double weight_diff;
    
    /** The number of iterations to perform */
    private int iterations;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * 
     * @param srcImg
     * @param covariance
     * @param max_diff
     * @param weight_diff
     * @param iterations
     */
    public AlgorithmMarkovSmooth(ModelImage srcImg, double covariance, double max_diff, 
			double weight_diff, int iterations) {
		this(null, srcImg, covariance, max_diff, weight_diff, iterations);
	}
	
	/**
	 * 
	 * @param destImg
	 * @param srcImg
	 * @param covariance
	 * @param max_diff
	 * @param weight_diff
	 * @param iterations
	 */
    public AlgorithmMarkovSmooth(ModelImage destImg, ModelImage srcImg, double covariance, double max_diff, 
			double weight_diff, int iterations) {
		super(destImg, srcImg);
		this.covariance = covariance;
		this.max_diff = max_diff;
		this.weight_diff = weight_diff;
		this.iterations = iterations;
	}
	
	public void runAlgorithm() {

	    /* Assigned to srcImage if replace image, assigned to destImage if new image */
	    ModelImage targetImage = null;
	    int xDim;
	    int yDim;
	    int sliceSize;
	    double src[];
	    double buffer[][];
	    int zDim = 1;
	    int tDim = 1;
	    int z;
	    int t;
	    int s;
	    int d;
	    long sliceMin;
	    long sliceMax;
	    int i;
	    double V_max;
	    double V_local;
	    long val;
	    int x;
	    int y;
	    long min_val;
	    double diff;
	    double V_data;
	    double V_diff;
	    double V_current;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(), "Markov smooth on image ...");
        
        if (destImage == null) {
            targetImage = srcImage;
        }
        else {
            targetImage = destImage;
        }
        
        if (srcImage.getNDims() >= 3) {
            zDim = srcImage.getExtents()[2];
        }
        if (srcImage.getNDims() >= 4) {
            tDim = srcImage.getExtents()[3];
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        // Maintain two buffer images.
        // In alternate iterations, one will be the 
        // source buffer, the other will be the destination buffer.
        buffer = new double[2][sliceSize];
        src = new double[sliceSize];
        
        for (t = 0; (t < tDim) && !threadStopped; t++) {
            for (z = 0; (z < zDim) && !threadStopped; z++) {
            	s = 1;
            	d = 0;
                try {
                    srcImage.exportData((t * zDim + z) * sliceSize, sliceSize, src); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm Markov Smooth: Image(s) locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                }
                
                sliceMin = Long.MAX_VALUE;
                sliceMax = Long.MIN_VALUE;
                for (i = 0; i < sliceSize; i++) {
                	buffer[0][i] = src[i];
                	if (src[i] < sliceMin) {
                	    sliceMin = (long)Math.floor(src[i]);	
                	}
                	if (src[i] > sliceMax) {
                		sliceMax = (long)Math.ceil(src[i]);
                	}
                }
                
                // This value is guaranteed to be larger than the
                // potential of any configuration of pixel values
                V_max = sliceSize * ((sliceMax+1)*(sliceMax+1)/(2 * covariance) + 4  * weight_diff * max_diff);
                
                for (i = 1; i <= iterations; i++) {
                
                    // Switch source and destination buffers.
                	if (s == 0) {
                		s = 1;
                		d = 0;
                	}
                	else {
                		s = 0;
                		d = 1;
                	}
                	
                	// Vary each pixel individually to find the
                	// values that minimize the local potentials.
                	for (y = 0; y < yDim; y++) {
                		for (x = 0; x < xDim; x++) {
                		
                			V_local = V_max;
                			min_val = sliceMin - 1;
                			for (val = sliceMin; val <= sliceMax; val++) {
                			
                				// The component of the potential due to the known data.
                				diff = val - src[x + y*xDim];
                				V_data = (diff * diff)/ ( 2.0 * covariance);
                				
                				// The component of the pixel due to the 
                				// difference between neighboring pixel values.
                				V_diff = 0.0;
                				if (y >= 1) {
                					diff = val - buffer[s][x + (y-1)*xDim];
                					V_diff = V_diff + Math.min(diff*diff, max_diff);
                				}
                				if (y < yDim-1) {
                					diff = val - buffer[s][x + (y+1)*xDim];
                					V_diff = V_diff + Math.min(diff*diff, max_diff);
                				}
                				if (x >= 1) {
                					diff = val - buffer[s][(x-1) + y*xDim];
                					V_diff = V_diff + Math.min(diff*diff, max_diff);
                				}
                				if (x < xDim-1) {
                					diff = val - buffer[s][(x+1) + y*xDim];
                					V_diff = V_diff + Math.min(diff*diff, max_diff);	
                				}
                				
                				V_current = V_data + weight_diff * V_diff;
                				
                				if (V_current < V_local) {
                					min_val = val;
                					V_local = V_current;
                				}
                			} // for (val = sliceMin; val <= sliceMax; val++)
                			
                			buffer[d][x + y*xDim] = min_val;
                		} // for (x = 0; x < xDim; x++)
                	} // for (y = 0; y < yDim; y++)
                } // for (i = 1; i <= iterations; i++)
                
                try {
                    targetImage.importData((t * zDim + z) * sliceSize, buffer[d], false);
                } catch (IOException error) {
                    errorCleanUp("Algorithm Markov Smooth: Image(s) locked", false);

                    return;
                }
                
            } // for (z = 0; (z < zDim) && !threadStopped; z++)
        } // for (t = 0; (t < tDim) && !threadStopped; t++)
        
        targetImage.calcMinMax();
        setCompleted(true);
        return;
	}
	
}