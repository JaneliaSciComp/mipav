package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.*;


/**
 * Computes the maximum intensity along each projection of a 3D image. 
 * @author joshim2
 *
 */
public class AlgorithmMaximumIntensityProjection extends AlgorithmBase {

//	~ Instance fields ------------------------------------------------------------------------------------------------
	/** Maximum Intensity Projection in X-direction */
	private ModelImage XProjectionImage;
	
	/** Maximum Intensity Projection in Y-direction */
	private ModelImage YProjectionImage;
	
	/** Maximum Intensity Projection in Z-direction */
	private ModelImage ZProjectionImage;
	
	/** Array of Result Images */
	private ArrayList resultImages;
	
	/** Source Image Resolutions */
	float [] imResolutions;
	
	/** Extents for X Projection Image */
	private int [] XExtents;
	
	/** Extents for Y Projection Image */
	private int [] YExtents;
	
	/** Extents for Z Projection Image */
	private int [] ZExtents;
	
	/** Minimum intensity value. */
	private float min;
	
	/** Maximum intensity value. */
	private float max;
	
//	~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Estimates the maximum intensity projection in each direction of a 3D Image
     * @param  srcImg    source image model
     */
    public AlgorithmMaximumIntensityProjection(ModelImage srcImg, float _min, float _max) {
        super(null, srcImg);
        min = _min;
        max = _max;
        imResolutions = srcImg.getResolutions(0);

    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Runs the Maximum Intensity Projection algorithm.
     */
    public void runAlgorithm() {
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();
            return;
        } else {
        	calc();
        }
    }
    
    /**
     * This method computes the maximum intensity projection of a 3D image.
     */
    
    private void calc() {
    	
      	int length;
      	int i, j, k;
    	float [] buffer;
    	float [] resultBufferX;
    	float [] resultBufferY;
    	float [] resultBufferZ;
    	float maxIntensityValue = 0f;
    	int [] dim = srcImage.getExtents();
    	
    	try {
    		length = srcImage.getSliceSize() * srcImage.getExtents() [2];
    		buffer = new float[length];
    		srcImage.exportData(0, length, buffer);
    	} catch (IOException error) {
    		buffer = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
    		return;
    	} catch (OutOfMemoryError e) {
    		buffer = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
    		return;
    	}
    	
    	int totalLength = (dim[0]*dim[1]) + (dim[2]*dim[0]) + (dim[1]*dim[2]);
    	int mod = 0;
    	fireProgressStateChanged(mod, srcImage.getImageName(), "Computing Maximum Intensity Projection ...");
        
        // For maximum intensity along Z-axis
        
        int lengthZ = dim[0] * dim[1]; // No. of pixels Z projection image
        resultBufferZ = new float[lengthZ];
        ZExtents = new int[2];
        System.arraycopy(dim, 0, ZExtents, 0, 2);
                
        for (i = 0; i < lengthZ; i++) {
        	
        	// Compute the max intensity value along Z-axis
        	for (j = 0; j < dim[2]; j++) {
        		
        		if ((buffer[i + (j * lengthZ)] < min) || (buffer[i + (j * lengthZ)] > max)) {
        			buffer[i + (j * lengthZ)] = 0;
        		}
        		
        		if (buffer[i + (j * lengthZ)] > maxIntensityValue) {
        			maxIntensityValue = buffer[i + (j * lengthZ)];
        		}
          	}
        	
        	resultBufferZ[i] = maxIntensityValue;
        	maxIntensityValue = 0f;
        	mod = mod + 1; // For progressbar purposes
        }
        
        // Reconstruct Z Projection image from result buffer
        
        ZProjectionImage = new ModelImage(ModelStorageBase.FLOAT, ZExtents, "ZProjectionImage");
        
        try {
        	
        	ZProjectionImage.importData(0, resultBufferZ, true);
        } catch (IOException error) {
        	
    		resultBufferZ = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
    		return;
    	} catch (OutOfMemoryError e) {
    		
    		resultBufferZ = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
    		return;
    	}
    	
    	// Set resolutions in line with the original source image resolutions
    	float [] ZRes = new float[2];
    	System.arraycopy(imResolutions, 0, ZRes, 0, 2);
    	ZProjectionImage.setResolutions(0, ZRes);
    	
    	//Update progress bar
    	fireProgressStateChanged(Math.round((mod / totalLength) * 100));
    	
    	// For maximum intensity along Y-axis
    	
    	int lengthY = dim[2] * dim[0]; // No. of pixels Y projection image
    	resultBufferY = new float[lengthY];
    	YExtents = new int[2];
    	YExtents[0] = dim[0];
    	YExtents[1] = dim[2];
    	maxIntensityValue = 0;
        
    	for (i = 0; i < dim[2]; i++) {
    		
    		for (j = 0; j < dim[0]; j++) {
    			
    			for (k = 0; k < dim[1]; k++) {
    				
    				if ((buffer[(dim[0]*dim[1]*i) + j + (dim[0]*k)] < min) ||
    						(buffer[(dim[0]*dim[1]*i) + j + (dim[0]*k)] > max)) {
    					buffer[(dim[0]*dim[1]*i) + j + (dim[0]*k)] = 0;
    				}
    				
    				if (buffer[(dim[0]*dim[1]*i) + j + (dim[0]*k)] > maxIntensityValue) {
            			maxIntensityValue = buffer[(dim[0]*dim[1]*i) + j + (dim[0]*k)];
            		}
    			}
                resultBufferY[j + i*dim[0]] = maxIntensityValue;
				maxIntensityValue = 0f;
				mod = mod + 1; // For progress bar purposes
    		} 			
    	}
    	
    	// Reconstruct Y Projection image from result buffer
        
        YProjectionImage = new ModelImage(ModelStorageBase.FLOAT, YExtents, "YProjectionImage");
        
        try {
        	
        	YProjectionImage.importData(0, resultBufferY, true);
        } catch (IOException error) {
        	
    		resultBufferY = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
    		return;
    	} catch (OutOfMemoryError e) {
    		
    		resultBufferY = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
    		return;
    	}
    	
    	// Set resolutions in line with the original source image resolutions
    	float [] YRes = new float[2];
    	YRes[0] = imResolutions[0];
    	YRes[1] = imResolutions[2];
    	YProjectionImage.setResolutions(0, YRes);
    	
    	// Update progress bar
    	fireProgressStateChanged(Math.round((mod / totalLength) * 100));
    	
    	// For maximum intensity along X-axis
    	
    	int lengthX = dim[1] * dim[2]; // No. of pixels X projection image
    	resultBufferX = new float[lengthX];
    	XExtents = new int[2];
    	XExtents[0] = dim[2];
    	XExtents[1] = dim[1];
    	maxIntensityValue = 0;
    	    	
    	for (i = 0; i < dim[2]; i++) {
    		
    		for (j = 0; j < dim[1]; j++) {
        		
        		for (k = 0; k < dim[0]; k++) {
        			
        			if ((buffer[(i*dim[1]*dim[0]) + (dim[0]*j) + k] < min) ||
        					(buffer[(i*dim[1]*dim[0]) + (dim[0]*j) + k] > max)) {
        				buffer[(i*dim[1]*dim[0]) + (dim[0]*j) + k] = 0;
        			}
        			
        			if (buffer[(i*dim[1]*dim[0]) + (dim[0]*j) + k] > maxIntensityValue) {
            			maxIntensityValue = buffer[(i*dim[1]*dim[0]) + (dim[0]*j) + k];
        			}
        		}
        		resultBufferX[i + (j*dim[2])] = maxIntensityValue;
        		maxIntensityValue = 0f;
        		mod = mod + 1; // For progress bar purposes
        	}
    	}
    	    	   	  	
    	// Construct X Projection image from result buffer
        
        XProjectionImage = new ModelImage(ModelStorageBase.FLOAT, XExtents, "XProjectionImage");
          
        try {
        	
        	XProjectionImage.importData(0, resultBufferX, true);
        } catch (IOException error) {
        	
    		resultBufferX = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
    		return;
    	} catch (OutOfMemoryError e) {
    		
    		resultBufferX = null;
    		errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
    		return;
    	}
    	
    	// Set resolutions in line with the original source image resolutions
    	float [] XRes = new float[2];
    	XRes[0] = imResolutions[2];
    	XRes[1] = imResolutions[1];
    	XProjectionImage.setResolutions(0, XRes);
    	
    	// Update progress bar
    	fireProgressStateChanged(Math.round((mod / totalLength) * 100));
    }
    
   
    /**
     * This method returns the projection images in an arraylist
     */
    public ArrayList getResultImage() {
    	
    	resultImages = new ArrayList(3);
    	resultImages.add(0, XProjectionImage);
    	resultImages.add(1, YProjectionImage);
    	resultImages.add(2, ZProjectionImage);
    	return resultImages;
    	
    }
}

    	
    

    


