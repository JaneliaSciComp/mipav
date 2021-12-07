package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
   This algorithm performs minimization of fuzziness, a reduction of the amount of fuzziness.  This method is probably the first
   fuzzy approach to image enhancement and is also known in the literature as contrast intensification operator.  The main idea
   is to reduce the amount of image fuzziness.  The algorithm is as follows:
   1.) Gray-level fuzzification (u(g) denotes the degree of brightness)
        u(g) = pow(1 + (gmax - g)/Fd, -Fe)
        where Fd and Fe are the exponential and denominational fuzzifiers that control the amount of grayness ambiguity in the 
        membership plane.  Suitable values for Fe are 1 and 2.  Fd is computed from a user selected crossover gray scale value, gc,
        at which u(gc) = 0.5.  gmax is the maximum gray level in the new image.  gmax >= srcMax.
        alpha = pow(1 + (gmax - gmin)/Fd, -Fe)
        gmin <= srcMin
   2.) Membership modification using successive application of Zadeh's intensification operator
       for (iters = 1; iters <= iterations; iters++) {
           u(g) = 2*u(g)*u(g) if 0 <= u(g) <= 0.5
           u(g) = 1 - 2*[1 - u(g)]*[1 - u(g)] if 0.5 < u(g) <= 1
           if (u(g) < alpha) {
           // This prevents the gray level from decreasing below gmin
               u(g) = alpha
           }
       } // for (iters = 1; iters <= iterations; iters++)
       This operation decreases values of u(g) which are <= 0.5 and increases those values of u(g)which are above 0.5.
       As the number of iterations approaches infinity, a 2 level binary image is produced.  The amount of contrast
       enhancement can be increased either by increasing the number of iterations or by increasing the value of Fe.
       Values below the crossover value will be decreased and values above the crossover value will be increased. 
   3.) New gray-levels by defuzzification 
       g' = gmax - Fd*(pow(u(g), -1.0/Fe) - 1.0) 
   
   References: 1.) Tizhoosh, H. R., "Fuzzy Image Enhancement: An Overview," in Fuzzy Techniques in Image Processing,
   E. Kerre and M. Nachtegael, eds., Springer-Verlag, New York, 2000, pp. 140-141.
   2.) Sankar K. Pal and Robert A. King, "Image Enhancement Using Smoothing with Fuzzy Sets", IEEE Transactions on
   Systems, Man, and Cybernetics, vol. smc-11, no. 7, July, 1981.
 */
public class AlgorithmFuzzyMinimization extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    
    //~ Instance fields ------------------------------------------------------------------------------------------------
	

    
    /** true means apply to entire image, false only region. */
    private boolean entireImage;

    /** number of times to filter the image. */
    private int iterations;
    
    /** The crossover gray scale value at which u(crossVal) = 0.5 */
    private double crossVal;
    
    /** Exponential fuzzifier between 1.0 and 2.0 */
    private double expFuzzifier;
    
    /** The new minimum level gmin <= srcMin */
    private double gmin;
    
    /** The new maximum level gmax >= srcMax */
    private double gmax;

    /** contains VOI. */
    private BitSet mask = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  iters     Number of iterations of the fuzzy minimization filter.
     * @param  crossVal  crossover gray scale value at which u(crossVal) = 0.5
     * @param  expFuzzifier    exponential fuzzifier between 1.0 and 2.0
     * @param  gmin      The new minimum level gmin <= srcMin
     * @param  gmax      The new maximum level gmax >= srcMax
     * @param  maskFlag  Flag that indicates that the fuzzy minimization filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmFuzzyMinimization(ModelImage srcImg, int iters, double crossVal, double expFuzzifier, 
    		double gmin, double gmax, boolean maskFlag) {
        super(null, srcImg);

        entireImage = maskFlag;
        iterations = iters;
        this.crossVal = crossVal;
        this.expFuzzifier = expFuzzifier;
        this.gmin = gmin;
        this.gmax = gmax;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  iters     Number of iterations of the fuzzy minimization filter.
     * @param crossVal   crossover gray scale value at which u(crossVal) = 0.5
     * @param  expFuzzifier exponential fuzzifier between 1.0 and 2.0
     * @param  gmin      The new minimum level gmin <= srcMin
     * @param  gmax      The new maximum level gmax >= srcMax
     * @param  maskFlag  Flag that indicates that the fuzzy minimization filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmFuzzyMinimization(ModelImage destImg, ModelImage srcImg, int iters, double crossVal, double expFuzzifier,
                           double gmin, double gmax, boolean maskFlag) {

        super(destImg, srcImg);

        entireImage = maskFlag;
        iterations = iters;
        this.crossVal = crossVal;
        this.expFuzzifier = expFuzzifier;
        this.gmin = gmin;
        this.gmax = gmax;

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
     * Fuzzy minimization filtering of the source image. Replaces the original image with the filtered image.
     */
    private void calcStoreInPlace() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] resultBuffer;
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        double denomFuzzifier;
        double diff;
        double expVal;
        int index;
        int iters;
        double alpha;
        double maxVal;
        int newType;

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
        alpha = Math.pow(1.0 + (gmax - gmin)/denomFuzzifier, -expFuzzifier);
        for (i = 0; i < uniqueValues; i++) {
        	resultBuffer[i] = Math.pow(1.0 + (gmax - srcList.get(i))/denomFuzzifier, -expFuzzifier);
        	for (iters = 0; iters < iterations; iters++) {
	        	if (resultBuffer[i] <= 0.5) {
	        		resultBuffer[i] = 2.0*resultBuffer[i]*resultBuffer[i];
	        	}
	        	else {
	        		diff = 1.0 - resultBuffer[i];
	        		resultBuffer[i] = 1.0 - 2.0*diff*diff;
	        	}
	        	if (resultBuffer[i] < alpha) {
	        		// Keep the lowest gray scale level from being less than gmin
	        		resultBuffer[i] = alpha;
	        	}
        	} // for (iters = 0; iters < iterations; iters++)
        	expVal = Math.pow(resultBuffer[i],-1.0/expFuzzifier);
        	resultBuffer[i] = maxVal - denomFuzzifier*(expVal - 1.0);
        } // for (i = 0; i < uniqueValues; i++)
        
        for (i = 0; i < length; i++) {
        	if (entireImage || mask.get(i)) {
        	    index = Collections.binarySearch(srcList, buffer[i]);
        	    buffer[i] = resultBuffer[index];
        	}
        }
        srcList.clear();
        
        newType = srcImage.getType();
        if (newType == ModelStorageBase.DOUBLE) {
        	
        }
        else if (newType == ModelStorageBase.FLOAT) {
        	if ((gmin < -Float.MAX_VALUE) || (gmax > Float.MAX_VALUE)) {
        		newType = ModelStorageBase.DOUBLE;
        	}
        }
        else if ((gmin < Long.MIN_VALUE) || (gmax > Long.MAX_VALUE)) {
        	newType = ModelStorageBase.DOUBLE;
        }
        else if (newType == ModelStorageBase.LONG) {
        	
        }
        else if (newType == ModelStorageBase.UINTEGER) {
        	if ((gmin < 0) || (gmax > 4294967295L)) {
        	    newType = ModelStorageBase.LONG;
        	}
        }
        else if ((gmin < Integer.MIN_VALUE) || (gmax > Integer.MAX_VALUE)) {
        	newType = ModelStorageBase.LONG;
        }
        else if (newType == ModelStorageBase.INTEGER) {
        	
        }
        else if (newType == ModelStorageBase.USHORT) {
        	if ((gmin < 0) || (gmax > 65535)) {
        		newType = ModelStorageBase.INTEGER;
        	}
        }
        else if ((gmin < -32768) || (gmax > 32767)) {
        	newType = ModelStorageBase.INTEGER;
        }
        else if (newType == ModelStorageBase.SHORT) {
        	
        }
        else if (newType == ModelStorageBase.UBYTE) {
        	if ((gmin < 0) || (gmax > 255)) {
        		newType = ModelStorageBase.SHORT;
        	}
        }
        else if (newType == ModelStorageBase.BYTE) {
        	if ((gmin < -128) || (gmax > 127)) {
        		newType = ModelStorageBase.SHORT;
        	}
        }
        
        if (srcImage.getType() != newType) {
        	AlgorithmChangeType convertType = new AlgorithmChangeType(srcImage, newType, srcImage.getMin(),
                    srcImage.getMax(), gmin, gmax, false);
            convertType.run();
            convertType = null;
        }
        
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
     * This function produces a new image that has had fuzzy minimization filtering
     */
    private void calcStoreInDest() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] resultBuffer; 
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        double maxVal;
        double denomFuzzifier;
        double diff;
        double expVal;
        int index;
        int iters;
        double alpha;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Fuzzy Minimization reports: destination image locked", false);

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
        alpha = Math.pow(1.0 + (gmax - gmin)/denomFuzzifier, -expFuzzifier);
        for (i = 0; i < uniqueValues; i++) {
        	resultBuffer[i] = Math.pow(1.0 + (gmax - srcList.get(i))/denomFuzzifier, -expFuzzifier);
        	for (iters = 0; iters < iterations; iters++) {
	        	if (resultBuffer[i] <= 0.5) {
	        		resultBuffer[i] = 2.0*resultBuffer[i]*resultBuffer[i];
	        	}
	        	else {
	        		diff = 1.0 - resultBuffer[i];
	        		resultBuffer[i] = 1.0 - 2.0*diff*diff;
	        	}
	        	if (resultBuffer[i] < alpha) {
	        		// Keep the lowest gray scale level from being less than gmin
	        		resultBuffer[i] = alpha;
	        	}
        	} // for (iters = 0; iters < iterations; iters++)
        	expVal = Math.pow(resultBuffer[i],-1.0/expFuzzifier);
        	resultBuffer[i] = maxVal - denomFuzzifier*(expVal - 1.0);
        } // for (i = 0; i < uniqueValues; i++)
        
        for (i = 0; i < length; i++) {
        	if (entireImage || mask.get(i)) {
        	    index = Collections.binarySearch(srcList, buffer[i]);
        	    buffer[i] = resultBuffer[index];
        	}
        }
        srcList.clear();
        
        
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
            errorCleanUp("Algorithm Fuzzy Minimization reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }

   
}
