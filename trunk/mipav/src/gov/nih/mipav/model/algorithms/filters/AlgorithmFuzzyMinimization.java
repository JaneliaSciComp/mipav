package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 
 */
public class AlgorithmFuzzyMinimization extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    
    //~ Instance fields ------------------------------------------------------------------------------------------------
	

    
    /** true means apply to entire image, false only region. */
    private boolean entireImage;

    /** number of times to filter the image. */
    private int iterations;
    
    private double crossVal;
    
    private double expFuzzifier;

    /** contains VOI. */
    private BitSet mask = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  iters     Number of interations of the fuzzy minimization filter.
     * @param  crossVal
     * @param  expFuzzifier    exponential fuzzifier
     * @param  maskFlag  Flag that indicates that the median filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmFuzzyMinimization(ModelImage srcImg, int iters, double crossVal, double expFuzzifier, boolean maskFlag) {
        super(null, srcImg);

        // else, already false
        entireImage = maskFlag;
        iterations = iters;
        this.crossVal = crossVal;
        this.expFuzzifier = expFuzzifier; // inside magnitude bounds of pixel value to adjust

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  iters     Number of iterations of the median filter.
     * @param crossVal
     * @param  expFuzzifier exponential fuzzifier
     * @param  maskFlag  Flag that indicates that the median filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmFuzzyMinimization(ModelImage destImg, ModelImage srcImg, int iters, double crossVal, double expFuzzifier,
                           boolean maskFlag) {

        super(destImg, srcImg);

        // else, already false
        entireImage = maskFlag;
        iterations = iters;
        this.crossVal = crossVal;
        this.expFuzzifier = expFuzzifier; // inside magnitude bounds of pixel value

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        mask = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), " Fuzzy Minimization ...");
        
        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
            
        } else { // there is no image but the original source.
            calcStoreInPlace();
            
        }
    }


    /**
     * Median filters the source image. Replaces the original image with the filtered image.
     */
    private void calcStoreInPlace() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        double maxVal;
        double denomFuzzifier;
        double diff;
        double expVal;
        int index;
        int iters;

        try {

           
            length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            if (srcImage.getNDims() > 2) {
            	length *= srcImage.getExtents()[2];
            	if (srcImage.getNDims() > 3) {
            		length *= srcImage.getExtents()[3];
            	}
            }

            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization reports: Out of memory when creating image buffer", true);

            return;
        }
        
        for (iters = 0; iters < iterations; iters++) {
	        for (i = 0; i < length; i++) {
	        	if (entireImage || mask.get(i)) {
	        	    if (!srcList.contains(buffer[i])) {
	        	        srcList.add(buffer[i]);	
	        	    }
	        	}
	        }
	        Collections.sort(srcList);
	        uniqueValues = srcList.size();
	        resultBuffer = new double[uniqueValues];
	        maxVal = srcList.get(uniqueValues-1);
	        denomFuzzifier = (maxVal - crossVal)/(Math.exp(-Math.log(0.5)/expFuzzifier) - 1.0);
	        for (i = 0; i < uniqueValues; i++) {
	        	resultBuffer[i] = Math.pow(1.0 + (maxVal - srcList.get(i))/denomFuzzifier, -expFuzzifier);
	        	if (resultBuffer[i] <= 0.5) {
	        		resultBuffer[i] = 2.0*resultBuffer[i]*resultBuffer[i];
	        	}
	        	else {
	        		diff = 1.0 - resultBuffer[i];
	        		resultBuffer[i] = 1.0 - 2.0*diff*diff;
	        	}
	        	expVal = Math.pow(resultBuffer[i],-1.0/expFuzzifier);
	        	resultBuffer[i] = maxVal - denomFuzzifier*(expVal - 1.0);
	        }
	        
	        for (i = 0; i < length; i++) {
	        	if (entireImage || mask.get(i)) {
	        	    index = Collections.binarySearch(srcList, buffer[i]);
	        	    buffer[i] = resultBuffer[index];
	        	}
            }
	        srcList.clear();
        
        } // for (iters = 0; iters < iterations; iters++)
        
        if (threadStopped) {
            finalize();

            return;
        }

        try { // place buffer data into the image
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization: Source image locked", true);

            return;
        }

        setCompleted(true);
    }

    

    /**
     * This function produces a new image that has been median filtered and places filtered image in the destination
     * image.
     */
    private void calcStoreInDest() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        double maxVal;
        double denomFuzzifier;
        double diff;
        double expVal;
        int index;
        int iters;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Median reports: destination image locked", false);

            return;
        }

        try {
        	
        	length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            if (srcImage.getNDims() > 2) {
            	length *= srcImage.getExtents()[2];
            	if (srcImage.getNDims() > 3) {
            		length *= srcImage.getExtents()[3];
            	}
            }

            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Fuzzy Minimization reports: out of memory", true);

            return;
        }
        
        for (iters = 0; iters < iterations; iters++) {
	        for (i = 0; i < length; i++) {
	        	if (entireImage || mask.get(i)) {
	        	    if (!srcList.contains(buffer[i])) {
	        	        srcList.add(buffer[i]);	
	        	    }
	        	}
	        }
	        Collections.sort(srcList);
	        uniqueValues = srcList.size();
	        resultBuffer = new double[uniqueValues];
	        maxVal = srcList.get(uniqueValues-1);
	        denomFuzzifier = (maxVal - crossVal)/(Math.exp(-Math.log(0.5)/expFuzzifier) - 1.0);
	        for (i = 0; i < uniqueValues; i++) {
	        	resultBuffer[i] = Math.pow(1.0 + (maxVal - srcList.get(i))/denomFuzzifier, -expFuzzifier);
	        	if (resultBuffer[i] <= 0.5) {
	        		resultBuffer[i] = 2.0*resultBuffer[i]*resultBuffer[i];
	        	}
	        	else {
	        		diff = 1.0 - resultBuffer[i];
	        		resultBuffer[i] = 1.0 - 2.0*diff*diff;
	        	}
	        	expVal = Math.pow(resultBuffer[i],-1.0/expFuzzifier);
	        	resultBuffer[i] = maxVal - denomFuzzifier*(expVal - 1.0);
	        }
	        
	        for (i = 0; i < length; i++) {
	        	if (entireImage || mask.get(i)) {
	        	    index = Collections.binarySearch(srcList, buffer[i]);
	        	    buffer[i] = resultBuffer[index];
	        	}
	        }
	        srcList.clear();

        } // for (iters = 0; iters < iterations; iters++)
        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
       
        if (threadStopped) {
            finalize();

            return;
        }

        try { // but now place buffer data into the image
            destImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }

   
}
