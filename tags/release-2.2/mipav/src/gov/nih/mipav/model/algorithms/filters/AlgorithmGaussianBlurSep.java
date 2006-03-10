package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import  java.io.*;

/**
 *  Calculates the gaussian blur of an image at a scale
 *  defined by the user (using separable convolutions).  Adapted
 *	from AlgorithmGaussianBlur.  This version should be faster but
 *	uses significantly more memory.
 *
 *  The application of this algorithm blurs an image or VOI region of the image with
 *  a Gaussian function at a user defined scale (sigma - standard deviation).
 *  In essence, convolving a Gaussian function produces the same result as a
 *  low-pass or smoothing filter. A low-pass filter attenuates high frequency
 *  components of the image (i.e. edges) and passes low frequency components
 *  and thus results in the blurring of the image. Smoothing filters are typically
 *  used for noise reduction and for blurring. The standard deviation (SD)
 *  of the Gaussian function controls the amount of blurring:a large SD (i.e. > 2)
 *  significantly blurs while a small SD (i.e. 0.5) blurs less. If the objective
 *  is to achieve noise reduction, a rank filter (median) might be more useful.
 *
 *  1D Gaussian = (1/sqrt(2*PI*sigma*sigma))*exp(-x*x/(2*sigma*sigma));
 *  <p>
 *  Advantages to convolving the Gaussian function to blur an image include:
 *  <p>
 *      1. Structure will not be added to the image.
 *      2. Can be analytically calculated, as well as the Fourier Transform
 *         of the Gaussian.
 *      3. By varying the SD a Gaussian scale-space can easily be constructed.
 *
 *		@version 0.1 Feb 11, 1998
 *		@author Matthew J. McAuliffe, Ph.D.
 *      @see GenerateGaussian
 *      @see AlgorithmConvolver
 */
public class AlgorithmGaussianBlurSep extends AlgorithmBase {


    /**
    *   Dimensionality of the kernel
    */
    private int         kExtents[];

    /**
    *   Standard deviations of the gaussian used to calculate the kernels
    */
    private float       sigmas[];

    /**
    *   Storage location of the Gaussian kernel. Rounding 1D kernels
    */
	private float       GxDataRound[];
	private float       GyDataRound[];
	private float       GzDataRound[];

    /**
	*   Flag, if true, indicates that the whole image should be processed. If
	*   false on process the image over the mask areas.
	*/
    private boolean     entireImage;


    /**
    *   Flags indicate which color channel to process. True indicates the
    *   channel should be processed.
    */
    private boolean     red, green, blue;



    /**
    *
    *   @param destImg  image model where result image is to stored
    *   @param srcImg   source image model
    *   @param sigmas   Gaussian's standard deviations in the each dimension
    *   @param maskFlag Flag that indicates that the gaussian convolution will be
    *                   performed over the whole image if equal to true
    *   @param img25D   Flag, if true, indicates that each slice of the 3D volume
    *                   should be processed independently. 2D images disregard this flag.
    */
	public AlgorithmGaussianBlurSep(ModelImage destImg, ModelImage srcImg,
	                             float sigmas[], boolean maskFlag, boolean img25D) {
        super(destImg, srcImg);

	    this.sigmas = sigmas;
	    entireImage = maskFlag;
	    image25D    = img25D;

	}

    /**
    *
    *   @param srcImg   source image model
    *   @param sigmas   Gaussian's standard deviations in the each dimension
    *   @param maskFlag Flag that indicates that the gaussian convolution will be
    *                   performed over the whole image if equal to true
    *   @param img25D Flag, if true, indicates that each slice of the 3D volume
    *                   should be processed independently. 2D images disregard this flag.
    */
    public AlgorithmGaussianBlurSep(ModelImage srcImg, float sigmas[],
                                 boolean maskFlag, boolean img25D) {
        super(null, srcImg);

	    this.sigmas  = sigmas;
	    entireImage  = maskFlag;
	    image25D     = img25D;
	}

    /**
    *   Prepares this class for destruction
    */
	public void finalize() {
	    destImage   = null;
	    srcImage    = null;
	    kExtents    = null;
	    sigmas      = null;
	    GxDataRound = null;
	    GyDataRound = null;
	    GzDataRound = null;
	    super.finalize();
	}

	private void makeKernels1D(boolean do3D) {
	    int xkDim, ykDim, zkDim;
	    int derivOrder[]  = new int[1];
	    derivOrder[0] = 0;
	    kExtents = new int[1];

        xkDim = Math.round(5*sigmas[0]);
        if (xkDim%2 == 0) xkDim++;
        if (xkDim  < 3) xkDim = 3;

        ykDim = Math.round(5*sigmas[1]);
        if (ykDim%2 == 0) ykDim++;
        if (ykDim  < 3) ykDim = 3;

        kExtents[0] = xkDim;
        GxDataRound = new float[xkDim];
        float sigmasX[] = new float[1];
        sigmasX[0] = sigmas[0];
        GenerateGaussian Gx = new GenerateGaussian(GxDataRound, kExtents, sigmasX, derivOrder);
        Gx.calc(false);
        Gx.finalize();
        Gx = null;

        kExtents[0] = ykDim;
        GyDataRound = new float[ykDim];
        float sigmasY[] = new float[1];
        sigmasY[0] = sigmas[1];
        GenerateGaussian Gy = new GenerateGaussian(GyDataRound, kExtents, sigmasY, derivOrder);
        Gy.calc(false);
        Gy.finalize();
        Gy = null;

        if (do3D) {
            zkDim = Math.round(5*sigmas[2]);
            if (zkDim%2 == 0) zkDim++;
            if (zkDim  < 3) zkDim = 3;

            kExtents[0]= zkDim;
            GzDataRound = new float[zkDim];
            float sigmasZ[] = new float[1];
	        sigmasZ[0] = sigmas[2];
            GenerateGaussian Gz = new GenerateGaussian(GzDataRound, kExtents, sigmasZ, derivOrder);
            Gz.calc(false);
            Gz.finalize();
            Gz = null;
        }

	}


	/**
    * Constructs a string of the contruction parameters and out puts
    *  the string to the messsage frame if the logging procedure is turned on.
    */
    private void constructLog() {
        String sigmaStr = new String();

        for ( int i = 0; i < sigmas.length; i++){
            sigmaStr += (" " +String.valueOf(sigmas[i])+ ", ");
        }


      if ( srcImage.isColorImage() ) {
          historyString = new String( "GaussianBlurSep(" + sigmaStr  +
                                  String.valueOf(entireImage) + ", " +
                                  String.valueOf(image25D)    + ", " +
                                  red + ", " + green + ", " + blue + ")\n" );
      }
      else {
          historyString = new String( "GaussianBlurSep(" + sigmaStr  +
                                  String.valueOf(entireImage) + ", " +
                                  String.valueOf(image25D)    +
                                  ")\n");
      }

    }


    /**
    *   Starts the program
    */
	public void runAlgorithm() {
        if (srcImage  == null) {
            displayError("Source Image is null");
        	finalize();
            return;
        }

        if (srcImage.getNDims() == 2){
	        makeKernels1D(false);
	    }
	    else if (srcImage.getNDims() == 3 && image25D == false) {
	        makeKernels1D(true);
	    }
	    else if (srcImage.getNDims() == 3 && image25D == true) {
	        makeKernels1D(false);
	    }
	    else if (srcImage.getNDims() == 4) {
	        makeKernels1D(true);
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
	            calcStoreInDest3D(1);
	        }
	        else if (srcImage.getNDims() == 3 && image25D == true) {
                calcStoreInDest2D(srcImage.getExtents()[2]);
            }
            else if (srcImage.getNDims() == 4) {
	            calcStoreInDest3D(srcImage.getExtents()[3]);
	        }
        }
        else {
            if (srcImage.getNDims() == 2){
	           calcInPlace2D(1);
	        }
	        else if (srcImage.getNDims() == 3 && image25D == false) {
	            calcInPlace3D(1);
	        }
	        else if (srcImage.getNDims() == 3 && image25D == true) {
	            calcInPlace2D(srcImage.getExtents()[2]);
	        }
	        else if (srcImage.getNDims() == 4 ) {
	            calcInPlace3D(srcImage.getExtents()[3]);
	        }
        }
    }

    /**
    *   Sets the flag for the red channel.
    *   @param flag if set to true then the red channel is processed.
    */
    public void setRed(boolean flag) { red = flag;}

    /**
    *   Sets the flag for the green channel.
    *   @param flag if set to true then the green channel is processed.
    */
    public void setGreen(boolean flag) { green = flag;}

    /**
    *   Sets the flag for the blue channel.
    *   @param flag if set to true then the blue channel is processed.
    */
    public void setBlue(boolean flag) { blue = flag;}




    /**
    *   This function produces a new image that has been blurred
    *   @param nImages      number of images to be blurred. If 2D image then nImage = 1,
    *                       if 3D image where each image is to processed independently
    *                       then nImages equals the number of images in the volume.
    */
    private void calcStoreInDest2D(int nImages){

        int s;
        int length;
        int start;
        float buffer[];
        float resultBuffer[];
        boolean color   = false;
        int     cFactor = 1;

        if ( srcImage.isColorImage() ) {
            color = true;
            cFactor = 4;
        }

        try {
            length = cFactor * srcImage.getSliceSize();
            buffer = new float[length];
            resultBuffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Blurring image ...", 0, 100);
        }
        catch (OutOfMemoryError e){
            buffer = null;
            errorCleanUp("Algorithm Gaussian Blur:  Out of memory", true);
            return;
        }

        float inc = 100.0f / nImages;
        initProgressBar();
        for (s = 0; s < nImages; s++) {
            start = s*length;
            if (isProgressBarVisible()) {
                progressBar.setMessage("Calculating the gaussian blur of slice " +
                                        (s+1) + "...");
                progressBar.updateValue(Math.round(s*inc), activeImage);
            }
            try {
                srcImage.exportData(start,length, buffer); // locks and releases lock
            }
            catch (IOException error) {
                buffer       = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);
                return;
            }


            AlgorithmSeparableConvolver convolver = new AlgorithmSeparableConvolver(
            	resultBuffer, buffer, new int[] {srcImage.getExtents()[0],
            	srcImage.getExtents()[1]},
            	GxDataRound, GyDataRound, color);
            convolver.setActiveImage(activeImage);
            if ( !entireImage ) {
            	convolver.setMask(mask);
            }
            if (color) {
                convolver.setColorChannels(red, green, blue);
            }
            if (isProgressBarVisible()) {
                convolver.setProgressBar(progressBar, Math.round(s*inc), Math.min(100,
                                                     Math.round((s+1)*inc)), true);
            }
            convolver.run();
            convolver.finalize();
            convolver = null;

            try {
                destImage.importData(start, resultBuffer, false);
            }
            catch (IOException error) {
                buffer       = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);
                return;
            }

        } // for (s = 0; s < nImages; s++)

		if (threadStopped) {
        	finalize();
			return;
		}

        destImage.calcMinMax();
        disposeProgressBar();
        setCompleted(true);
    }


    /**
    *   Calculates the blurred image and replaces the source image with the blurred image.
    *   @param nImages      number of images to be blurred. If 2D image then nImage = 1,
    *                       if 3D image where each image is to processed independently
    *                       then nImages equals the number of images in the volume.
    */
    private void calcInPlace2D(int nImages){

        int s;
        int length;
        int start;
        float buffer[];
        float resultBuffer[];
        boolean color   = false;
        int     cFactor = 1;

        if ( srcImage.isColorImage() ) {
            color = true;
            cFactor = 4;
        }

        try {
            length       = cFactor * srcImage.getSliceSize();
            buffer       = new float[length];
            resultBuffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Blurring image ...", 0, 100);
        }
        catch (OutOfMemoryError e){
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);
            return;
        }

        float inc = 100.0f / nImages;
        initProgressBar();
        for (s = 0; s <  nImages && !threadStopped; s++) {
            start = s*length;
            if (isProgressBarVisible()) {
                progressBar.setMessage("Calculating the gaussian blur of slice " +
                                        (s+1) + "...");
                progressBar.updateValue(Math.round(s*inc), activeImage);
            }
            try {
                srcImage.exportData(start,length, buffer); // locks and releases lock
            }
            catch (IOException error) {
                buffer       = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);
                return;
            }


            AlgorithmSeparableConvolver convolver = new AlgorithmSeparableConvolver(
            	resultBuffer, buffer, new int[] {srcImage.getExtents()[0],
            	srcImage.getExtents()[1]},
            	GxDataRound, GyDataRound, color);
            convolver.setActiveImage(activeImage);

            if ( !entireImage ) {
            	convolver.setMask(mask);
            }
            if (color) {
                convolver.setColorChannels(red, green, blue);
            }
            if (isProgressBarVisible()) {
                convolver.setProgressBar(progressBar, Math.round(s*inc), Math.min(100,
                                         Math.round((s+1)*inc)), true);
            }
            convolver.run();
            convolver.finalize();
            convolver = null;

            try {
                srcImage.importData(start, resultBuffer, false);
            }
            catch (IOException error) {
                buffer       = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);
                return;
            }

        } // for (s = 0; s <  nImages && !threadStopped; s++)

        if (threadStopped) {
        	finalize();
        	return;
        }

        srcImage.calcMinMax();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
    *  Calculates the blurred image and replaces the source image
    *  with the blurred image.
    */
    private void calcInPlace3D(int nVolumes){

        int t;
        int length;
        float buffer[];
        float resultBuffer[];
        boolean color   = false;
        int     cFactor = 1;

        if ( srcImage.isColorImage() ) {
            color = true;
            cFactor = 4;
        }

        try {
            length = cFactor *  srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer          = new float[length];
            resultBuffer    = new float[length];
            buildProgressBar(srcImage.getImageName(), "Blurring image ...", 0, 100);
        }
        catch (OutOfMemoryError e){
            buffer       = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);
            return;
        }

        initProgressBar();
        float inc = 100.0f / nVolumes;
        for (t = 0; t < nVolumes && !threadStopped; t++) {
            if (isProgressBarVisible()) {
                progressBar.setMessage("Calculating the gaussian blur of volume " +
                                        (t+1) + "...");
                progressBar.updateValue(Math.round(t*inc), activeImage);
            }
            try {
                srcImage.exportData(t*length, length, buffer); // locks and releases lock
            }
            catch (IOException error) {
                displayError("Algorithm Gaussian Blur: Image(s) locked");
                setCompleted(false);
                disposeProgressBar();
                return;
            }


            AlgorithmSeparableConvolver convolver = new AlgorithmSeparableConvolver(
                resultBuffer, buffer, new int[] {srcImage.getExtents()[0],
                srcImage.getExtents()[1], srcImage.getExtents()[2]},
                GxDataRound, GyDataRound, GzDataRound, color);
            convolver.setActiveImage(activeImage);

            if ( !entireImage ) {
                convolver.setMask(mask);
            }
            if (color) {
                convolver.setColorChannels(red, green, blue);
            }
            if (isProgressBarVisible()) {
                convolver.setProgressBar(progressBar, Math.round(t*inc), Math.min(100,
                                         Math.round((t+1)*inc)), true);
            }

            convolver.run();
            convolver.finalize();
            convolver = null;


			if (threadStopped) {
        	    finalize();
				return;
			 }

            try {
                srcImage.importData(t*length, resultBuffer, false);
            }
            catch (IOException error) {
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);
                return;
            }
        } // for (t = 0; t < nVolumes && !threadStopped; t++)

		if (threadStopped) {
            finalize();
			return;
		}

        buffer = null;
        resultBuffer = null;
        System.gc();
        srcImage.calcMinMax();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
    *   Produces a new image that has been blurred
    */
    private void calcStoreInDest3D(int nVolumes){

        int t;
        int length;
        float buffer[];
        float resultBuffer[];
        boolean color   = false;
        int     cFactor = 1;

        if ( srcImage.isColorImage() ) {
            color = true;
            cFactor = 4;
        }



        try {
            length = cFactor *  srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Blurring image ...", 0, 100);
        }
        catch (OutOfMemoryError e){
            buffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);
            return;
        }

        initProgressBar();
        float inc = 100.0f / nVolumes;
        for (t = 0; t < nVolumes && !threadStopped; t++) {
            if (isProgressBarVisible()) {
                progressBar.setMessage("Calculating the gaussian blur of volume " +
                                        (t+1) + "...");
                progressBar.updateValue(Math.round(t*inc), activeImage);
            }
            try {
                srcImage.exportData(t*length,length, buffer); // locks and releases lock
            }
            catch (IOException error) {
                displayError("Algorithm Gaussian Blur: Image(s) locked");
                setCompleted(false);
                disposeProgressBar();
                destImage.releaseLock();
                return;
            }


            AlgorithmSeparableConvolver convolver = new AlgorithmSeparableConvolver(
                resultBuffer, buffer, new int[] {srcImage.getExtents()[0],
                srcImage.getExtents()[1], srcImage.getExtents()[2]},
                GxDataRound, GyDataRound, GzDataRound, color);
            convolver.setActiveImage(activeImage);

            if ( !entireImage ) {
                convolver.setMask(mask);
            }
            if (color) {
                convolver.setColorChannels(red, green, blue);
            }

            if (isProgressBarVisible()) {
                convolver.setProgressBar(progressBar, Math.round(t*inc), Math.min(100,
                                         Math.round((t+1)*inc)), true);
            }
            convolver.run();
            convolver.finalize();
            convolver = null;


			if (threadStopped) {
        	    finalize();
				return;
			 }

            try {
                destImage.importData(t*length, resultBuffer, false);
            }
            catch (IOException error) {
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);
                return;
            }

        } // for (t = 0; t < nVolumes && !threadStopped; t++)

        if (threadStopped) {
        	finalize();
        	return;
        }

        destImage.calcMinMax();
        disposeProgressBar();
        setCompleted(true);
    }

}
