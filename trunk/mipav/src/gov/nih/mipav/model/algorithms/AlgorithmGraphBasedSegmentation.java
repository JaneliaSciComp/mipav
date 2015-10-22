package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.LUSOL;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

public class AlgorithmGraphBasedSegmentation extends AlgorithmBase {
	
	/**
	 * This code is ported with the permission of Pedro F. Felzenszwalb
	 * Reference: Pedro F. Felzenszwalb and Daniel P. Huttenlocher, 
	 * International Journal of Computer Vision, 59(2), September, 2004.
	 * 
	 * This program takes a color image and produces a segmentation
	 * with a random color assigned to each region
	 * 
	 * Typical parameters are: sigma = 0.5, k = 500, minSize = 20.
	 * Larger values for k result in larger components in the result.
	 */

    //~ Static fields/initializers -------------------------------------------------------------------------------------
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	private float sigma;  // Used to smooth the input image before segmenting it
	
	private float k;  // Value for the threshold function
	
	private int minSize; // Minimum component size enforced by post-processing
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs graph based segmentation algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  sigma    Used to smooth the input image before segmenting it
     * @param  k        Value for the threshold function
     * @param  minSize  Minimum component size enforced by post-processing
     
     */
    public AlgorithmGraphBasedSegmentation(ModelImage destImg, ModelImage srcImg, float sigma,
    		float k, int minSize) {

        super(destImg, srcImg);
        this.sigma = sigma;
        this.k = k;
        this.minSize = minSize;
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;

        
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        
        fireProgressStateChanged("Graph Based Segmentation ...");
        
        setCompleted(true);
        return;
    	
    }
}