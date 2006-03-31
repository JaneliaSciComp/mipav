package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import  java.io.*;


/**
 *  Calculates the Unsharp Mask of an image at a scale defined
 *  by the user ( unsharp image = original image -  weight * blurred image);
 *  weight < 1;
 *
 *		@version 1.0 Feb 11, 2000
 *		@author Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmUnsharpMask extends AlgorithmBase {

    private boolean     entireImage;

    private int         kExtents[];
    private float       sigmas[];
    private float       weightA = 0.75f;
    private float       gaussData[];

    /**
    *   Constructor
    *   @param destImg  image model where result image is to stored
    *   @param srcImg   source image model
    *   @param sigmas   Gaussian's standard deviations in the each dimension
    *   @param weight   weighting factor, should be less than 1
    *   @param maskFlag Flag that indicates that the unsharp mask will be
    *                   calculated for the whole image if equal to true
    *   @param img25D Flag, if true, indicates that each slice of the 3D volume
    *                   should be processed independently. 2D images disregard this flag.
    */
	public AlgorithmUnsharpMask(ModelImage destImg, ModelImage srcImg,
	                          float sigmas[], float weight, boolean maskFlag, boolean img25D) {

        super(destImg, srcImg);
	    this.sigmas  = sigmas;
        weightA      = weight;
	    entireImage  = maskFlag;
	    image25D     = img25D;
	    if (entireImage == false) mask = srcImage.generateVOIMask();

	}

    /**
    *   Constructor
    *   @param srcImg   source image model
    *   @param sigmas   Gaussian's standard deviations in the each dimension
    *   @param weight   weighting factor, should be less than 1
    *   @param maskFlag Flag that indicates that the unsharp mask will be
    *                   calculated for the whole image if equal to true
    *   @param img25D Flag, if true, indicates that each slice of the 3D volume
    *                   should be processed independently. 2D images disregard this flag.
    */
    public AlgorithmUnsharpMask(ModelImage srcImg, float sigmas[], float weight,
                                boolean maskFlag, boolean img25D) {

        super(null, srcImg);
	    this.sigmas  = sigmas;
	    entireImage  = maskFlag;
        weightA      = weight;
	    image25D     = img25D;
	    if (entireImage == false) mask = srcImage.generateVOIMask();
	}

	/**
    *   prepares this class for destruction
    */
	public void finalize(){

	    gaussData   = null;
	    destImage   = null;
	    srcImage    = null;
	    super.finalize();
	}

    /**
    *   creates Gaussian kernels for the blurring process.
    */
	private void makeKernels2D() {
	    int xkDim, ykDim;
	    int derivOrder[]  = new int[2];

	    kExtents = new int[2];
	    derivOrder[0]   = 0;
	    derivOrder[1]   = 0;

        xkDim = Math.round(3*sigmas[0]);
        if (xkDim%2 == 0) xkDim++;
        kExtents[0] = xkDim;

        ykDim = Math.round(3*sigmas[1]);
        if (ykDim%2 == 0) ykDim++;
        kExtents[1]= ykDim;

	    gaussData = new float[xkDim*ykDim];
	    GenerateGaussian Gauss = new GenerateGaussian(gaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
	}

	/**
    *   creates Gaussian kernels for the blurring process.
    */
	private void makeKernels3D() {
	    int xkDim, ykDim, zkDim;
	    int derivOrder[]  = new int[3];

	    kExtents = new int[3];
	    derivOrder[0]   = 0;
	    derivOrder[1]   = 0;
	    derivOrder[2]   = 0;

        xkDim = Math.round(3*sigmas[0]);
        if (xkDim%2 == 0) xkDim++;
        kExtents[0] = xkDim;

        ykDim = Math.round(3*sigmas[1]);
        if (ykDim%2 == 0) ykDim++;
        kExtents[1]= ykDim;

        zkDim = Math.round(3*sigmas[2]);
        if (zkDim%2 == 0) zkDim++;
        kExtents[2]= zkDim;

	    gaussData = new float[xkDim*ykDim*zkDim];
	    GenerateGaussian Gauss = new GenerateGaussian(gaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
	}


	/**
    * constructs a string of the contruction parameters and
    * outputs the string to the messsage frame if the logging
    * procedure is turned on.
    */
    private void constructLog() {
        String sigmaStr = " [";
        for ( int i = 0; i < sigmas.length; i++){
            sigmaStr += String.valueOf(sigmas[i]);
            sigmaStr += (i < sigmas.length-1)? ", " : "";  // append a comma when there are more sigmas
        }
        sigmaStr += "]";

        historyString = new String( "UnsharpMask(" + sigmaStr + ", " +
                                    String.valueOf(weightA) + ", " +
                                    String.valueOf(entireImage) + ", " +
                                    String.valueOf(image25D)    +
                                    ")\n");

    }



    /**
    *   starts the program
    */
	public void runAlgorithm() {

        if (srcImage  == null) {
            displayError("Source Image is null");
            return;
        }

        if (srcImage.getNDims() == 2){
	        makeKernels2D();
	    }
	    else if (srcImage.getNDims() == 3 && image25D == false) {
	        makeKernels3D();
	    }
	    else if (srcImage.getNDims() == 3 && image25D == true) {
	        makeKernels2D();
	    }

	    if (threadStopped) {
        	finalize();
        	return;
        }

        constructLog();

        if (destImage != null){
            if (srcImage.getNDims() == 2){
	           calcStoreInDest2D(1);
	        }
	        else if (srcImage.getNDims() == 3 && image25D == false) {
	            calcStoreInDest3D();
	        }
	        else if (srcImage.getNDims() == 3 && image25D == true) {
                calcStoreInDest2D(srcImage.getExtents()[2]);
            }
        }
        else {
            if (srcImage.getNDims() == 2){
	           calcInPlace2D(1);
	        }
	        else if (srcImage.getNDims() == 3 && image25D == false) {
	            calcInPlace3D();
	        }
	        else if (srcImage.getNDims() == 3 && image25D == true) {
	            calcInPlace2D(srcImage.getExtents()[2]);
	        }
        }
    }


    /**
    *   calculates the UnsharpMask and replaces the source image
    *   with the new image.
    */
    private void calcInPlace3D(){

        int    i;
        int    length;
        float  buffer[];
        float  resultBuffer[];
        float  blur;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer          = new float[length];
            resultBuffer    = new float[length];
            srcImage.exportData(0,length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "UnsharpMasking ...", 0, 100);
        }
        catch (IOException error) {
            buffer          = null;
            resultBuffer    = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Image(s) locked", true);
            return;
        }
        catch (OutOfMemoryError e) {
            buffer          = null;
            resultBuffer    = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Out of memory", true);
            return;
        }

        initProgressBar();

        int mod = length/100; // mod is 1 percent of length
        for ( i = 0; i < length && !threadStopped; i++){
            if (i%mod == 0 && isProgressBarVisible())
                progressBar.updateValue( Math.round((float)i/(length-1) * 100), activeImage);
            if (entireImage == true || mask.get(i) ) {
                blur = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(),
                                                       buffer, kExtents, gaussData);
                resultBuffer[i] =  buffer[i] - weightA*blur;

            }
            else {
                resultBuffer[i] = buffer[i];
                //resultBuffer[i] = 0;
            }
        }

        buffer = null;
        System.gc();

        if (threadStopped) {
        	finalize();
        	return;
        }

        try {
            // Will not work for UBYTE and USHORT
            if (srcImage.getType() != ModelImage.FLOAT) {
                srcImage.reallocate(ModelImage.FLOAT);
            }
            srcImage.importData(0, resultBuffer, true);
        }
        catch (IOException error) {
            buffer          = null;
            resultBuffer    = null;
            errorCleanUp("Algorithm UnsharpMask importData: Image(s) locked", true);
            return;
        }

        setCompleted(true);
        disposeProgressBar();
    }


    /**
    *   this function produces the UnsharpMask of
    *   input image.
    */
    private void calcStoreInDest3D(){

        int    i;
        int    length;
        float  buffer[];
        float  blur;

        try { destImage.setLock(); }
        catch (IOException error) {
            errorCleanUp("Algorithm UnsharpMask: Image(s) locked", false);
            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            srcImage.exportData(0,length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "UnsharpMasking ...", 0, 100);
        }
        catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Image(s) locked", true);
            return;
        }
        catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Out of memory", true);
            return;
        }
        initProgressBar();

        int mod = length/100; // mod is 1 percent of length
        for ( i = 0; i < length && !threadStopped; i++){
            if (i%mod == 0 && isProgressBarVisible())
                progressBar.updateValue( Math.round((float)i/(length-1) * 100), activeImage);
            if (entireImage == true || mask.get(i) ) {
                blur = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(),
                                                       buffer, kExtents, gaussData);
                destImage.set(i, buffer[i]-weightA*blur);
            }
            else {
                destImage.set(i, buffer[i]);
            }
        }

        if (threadStopped) {
        	finalize();
        	return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
    *   this function produces a new image that has been blurred
    *   @param nImages      number of images to be processed. If 2D image then nImage = 1,
    *                       if 3D image where each image is to processed independently
    *                       then nImages equals the number of images in the volume.
    */
    private void calcStoreInDest2D(int nImages){

        int i, s, idx;
        int length, totalLength;
        int start;
        float  blur;
        float buffer[];

        try { destImage.setLock(ModelStorageBase.RW_LOCKED); }
        catch (IOException error){
            errorCleanUp(" Unsharp Mask: Image(s) locked", false);
            return;
        }
        try {
            length = srcImage.getSliceSize();
            totalLength = length*nImages;
            buffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Unsharp image ...", 0, 100);
        }
        catch (OutOfMemoryError e){
            buffer = null;
            errorCleanUp("Algorithm Unsharp Mask:  Out of memory", true);
            return;
        }

        int mod = totalLength/100; // mod is 1 percent of length
        initProgressBar();
        for (s = 0; s < nImages && !threadStopped; s++) {
            start = s*length;
            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            }
            catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Unsharp Mask: Image(s) locked", true);
                return;
            }

            for (i = 0, idx = start; i < length && !threadStopped; i++, idx++){
                if ((start+i)%mod==0 && isProgressBarVisible())
                    progressBar.updateValue(Math.round((float)(start+i)/(totalLength-1) * 100), activeImage);

                if (entireImage == true || mask.get(i) ) {
                    blur = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(),
                                                        buffer, kExtents, gaussData);

                    destImage.set(idx, buffer[i]-weightA*blur);
                }
                else {
                    destImage.set(idx, buffer[i]);
                }
            }
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        if (threadStopped) {
        	finalize();
        	return;
        }

        disposeProgressBar();
        setCompleted(true);
    }


    /**
    *   calculates the blurred image and replaces the source image
    *   with the blurred image.
    *   @param nImages      number of images to be processed. If 2D image then nImage = 1,
    *                       if 3D image where each image is to processed independently
    *                       then nImages equals the number of images in the volume.
    */
    private void calcInPlace2D(int nImages){

        int i, s;
        int length, totalLength;
        int start;
        float  blur;
        float buffer[];
        float resultBuffer[];

        try {
            length       = srcImage.getSliceSize();
            totalLength  = length * nImages;
            buffer       = new float[length];
            resultBuffer = new float[totalLength];
            buildProgressBar(srcImage.getImageName(), "Unsharp mask image ...", 0, 100);
        }
        catch (OutOfMemoryError e){
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm UnsharpMask: Out of memory", true);
            return;
        }

        int mod = totalLength/100; // mod is 1 percent of length
        initProgressBar();
        for (s = 0; s < nImages && !threadStopped; s++) {
            start = s*length;
            try {
                srcImage.exportData(start,length, buffer); // locks and releases lock
            }
            catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm UnsharpMask: Image(s) locked", true);
                return;
            }

            for ( i = 0; i < length && !threadStopped; i++){

                if ((start+i)%mod==0 && isProgressBarVisible())
                    progressBar.updateValue(Math.round((float)(start+i)/(totalLength-1) * 100), activeImage);

                if ( entireImage == true || mask.get(i)) {
                    blur = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(),
                                                           buffer, kExtents, gaussData);
                    resultBuffer[start+i] = buffer[i] - weightA*blur;
                }
                else {
                    resultBuffer[start+i] = buffer[i];
                    //resultBuffer[i] = 0;
                }
            }
        }

        if (threadStopped) {
        	finalize();
        	return;
        }

        try {
            // Will not work for UBYTE and USHORT
            if (srcImage.getType() != ModelImage.FLOAT) {
                srcImage.reallocate(ModelImage.FLOAT);
            }
            srcImage.importData(0, resultBuffer, true);
        }
        catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm UnsharpMask: Image(s) locked", true);
            return;
        }

        disposeProgressBar();
        setCompleted(true);
    }

}
