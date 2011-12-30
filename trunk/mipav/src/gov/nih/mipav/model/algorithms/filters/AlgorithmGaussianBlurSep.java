package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmSeparableConvolver;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewJProgressBar;

import java.io.IOException;


/**
 * Calculates the gaussian blur of an image at a scale defined by the user (using separable convolutions). Adapted from
 * AlgorithmGaussianBlur. This version should be faster but uses significantly more memory.
 *
 * <p>The application of this algorithm blurs an image or VOI region of the image with a Gaussian function at a user
 * defined scale (sigma - standard deviation). In essence, convolving a Gaussian function produces the same result as a
 * low-pass or smoothing filter. A low-pass filter attenuates high frequency components of the image (i.e. edges) and
 * passes low frequency components and thus results in the blurring of the image. Smoothing filters are typically used
 * for noise reduction and for blurring. The standard deviation (SD) of the Gaussian function controls the amount of
 * blurring:a large SD (i.e. > 2) significantly blurs while a small SD (i.e. 0.5) blurs less. If the objective is to
 * achieve noise reduction, a rank filter (median) might be more useful.</p>
 *
 * <p>1D Gaussian = (1/sqrt(2*PI*sigma*sigma))*exp(-x*x/(2*sigma*sigma));</p>
 *
 * <p>Advantages to convolving the Gaussian function to blur an image include:</p>
 *
 * <p>1. Structure will not be added to the image. 2. Can be analytically calculated, as well as the Fourier Transform
 * of the Gaussian. 3. By varying the SD a Gaussian scale-space can easily be constructed.</p>
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      GenerateGaussian
 * @see      AlgorithmConvolver
 */
public class AlgorithmGaussianBlurSep extends AlgorithmBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Flags indicate which color channel to process. True indicates the channel should be processed. */
    private boolean red, green, blue;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;

    private Kernel gaussianKernel;
    private float[] inputBuffer;
    private float[] outputBuffer;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmGaussianBlurSep object.
     *
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the gaussian convolution will be performed over the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmGaussianBlurSep(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D) {
        super(null, srcImg);

		image25D = img25D;
		if(image25D){
			this.sigmas = new float[]{sigmas[0], sigmas[1]};
		}else{
			this.sigmas = sigmas;
		}
        entireImage = maskFlag;
        image25D = img25D;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        sigmas = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	beforeExecute();
    	execute();
    	afterExecute();
    }

    public void beforeExecute(){
    	GaussianKernelFactory kernelFactory = GaussianKernelFactory.getInstance(sigmas);
    	kernelFactory.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
    	gaussianKernel = kernelFactory.createKernel();
    }
    
    public void execute() {
		boolean color = false;
		int cFactor = 1;

		if (srcImage.isColorImage()) {
			color = true;
			cFactor = 4;
		}

		int imgLength = cFactor
				* AlgorithmBase.calculateImageSize(srcImage.getExtents());

		try {
			inputBuffer = new float[imgLength];
		} catch (OutOfMemoryError e) {
			inputBuffer = null;
			errorCleanUp("Algorithm Separable Gaussian Blur: Out of memory",
					true);

			return;
		}

		fireProgressStateChanged(0, srcImage.getImageName(),
				"Blurring image ...");

		int stepProgressValuePerVolume = 100;

		fireProgressStateChanged(0, srcImage.getImageName(),
				"Calculating the gaussian blur of slice " + 1 + "...");

		try {
			srcImage.exportData(0, imgLength, inputBuffer); // locks and releases lock
		} catch (IOException error) {
			displayError("Algorithm Separable Gaussian Blur: Image(s) locked");
			setCompleted(false);
			fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

			return;
		}

		AlgorithmSeparableConvolver convolver = new AlgorithmSeparableConvolver(
				inputBuffer, srcImage.getExtents(), gaussianKernel.getData(), color);

		convolver.setProgressValues(generateProgressValues(
				getMinProgressValue(), getMinProgressValue()
						+ stepProgressValuePerVolume));
		linkProgressToAlgorithm(convolver);

		convolver.setRunningInSeparateThread(runningInSeparateThread);

		convolver.addListener(this);
		if (!entireImage) {
			convolver.setMask(mask);
		}

		if (color) {
			convolver.setColorChannels(red, green, blue);
		}

		convolver.run();
		if (threadStopped) {
			return;
		}
	}
    
    public void afterExecute(){
		setCompleted(true);
    }
    /**
	 * Sets the flag for the blue channel.
	 * 
	 * @param flag
	 *            if set to true then the blue channel is processed.
	 */
    public void setBlue(boolean flag) {
        blue = flag;
    }

    /**
	 * Sets the flag for the green channel.
	 * 
	 * @param flag
	 *            if set to true then the green channel is processed.
	 */
    public void setGreen(boolean flag) {
        green = flag;
    }

    /**
     * Sets the flag for the red channel.
     *
     * @param  flag  if set to true then the red channel is processed.
     */
    public void setRed(boolean flag) {
        red = flag;
    }

	public synchronized float[] getResultBuffer() {
		return outputBuffer;
	}

    public void algorithmPerformed(AlgorithmBase algorithm){
    	if(algorithm instanceof AlgorithmSeparableConvolver){
    		AlgorithmSeparableConvolver convolver = (AlgorithmSeparableConvolver)algorithm;
        	this.outputBuffer = convolver.getOutputBuffer();
    	}
    }
    /**
     * Calculates the blurred image and replaces the source image with the blurred image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
//    private void calcInPlace2D(int nImages) {
//
//        int s;
//        int length;
//        int start;
//        float[] buffer;
//        float[] resultBuffer;
//        boolean color = false;
//        int cFactor = 1;
//
//        if (srcImage.isColorImage()) {
//            color = true;
//            cFactor = 4;
//        }
//
//        try {
//            length = cFactor * srcImage.getSliceSize();
//            buffer = new float[length];
//            resultBuffer = new float[length];
//        } catch (OutOfMemoryError e) {
//            buffer = null;
//            resultBuffer = null;
//            errorCleanUp("Algorithm Separable Gaussian Blur: Out of memory", true);
//
//            return;
//        }
//
//        float stepProgressValuePerImage = (float) 100 / nImages;
//        fireProgressStateChanged(0, srcImage.getImageName(), "Blurring image ...");
//
//        for (s = 0; (s < nImages) && !threadStopped; s++) {
//            start = s * length;
//
//            fireProgressStateChanged(0 + Math.round(s * stepProgressValuePerImage), srcImage.getImageName(),
//                                     "Calculating the gaussian blur of slice " + (s + 1) + "...");
//
//            try {
//                srcImage.exportData(start, length, buffer); // locks and releases lock
//            } catch (IOException error) {
//                buffer = null;
//                resultBuffer = null;
//                errorCleanUp("Algorithm Separable Gaussian Blur: Image(s) locked", true);
//
//                return;
//            }
//
//
//            AlgorithmSeparableConvolver convolver = null;
//
//            if (stepProgressValuePerImage < 2) {
//                convolver = new AlgorithmSeparableConvolver(buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1]
//                                                            }, GxDataRound, GyDataRound, color);
//            } else {
//                convolver = new AlgorithmSeparableConvolver(resultBuffer, buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1]
//                                                            }, GxDataRound, GyDataRound, color);
//
//                convolver.setProgressValues(generateProgressValues(getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerImage * s),
//                                                                   getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerImage * (s + 1))));
//                linkProgressToAlgorithm(convolver);
//            }
//
//            convolver.setRunningInSeparateThread(runningInSeparateThread);
//
//            if (!entireImage) {
//                convolver.setMask(mask);
//            }
//
//            if (color) {
//                convolver.setColorChannels(red, green, blue);
//            }
//
//            convolver.run();
//            convolver.finalize();
//            convolver = null;
//
//            try {
//                srcImage.importData(start, resultBuffer, false);
//            } catch (IOException error) {
//                buffer = null;
//                resultBuffer = null;
//                errorCleanUp("Algorithm Separable Gaussian Blur: Image(s) locked", true);
//
//                return;
//            }
//
//        } // for (s = 0; s <  nImages && !threadStopped; s++)
//
//        if (threadStopped) {
//            finalize();
//
//            return;
//        }
//
//        srcImage.calcMinMax();
//
//        setCompleted(true);
//    }

    
    /**
     * Calculates the blurred image and replaces the source image with the blurred image.
     *
     * @param  nVolumes  DOCUMENT ME!
     */
//    private void calcInPlace3D(int nVolumes) {
//
//        int t;
//        int length;
//        float[] buffer;
//        float[] resultBuffer;
//        boolean color = false;
//        int cFactor = 1;
//
//        if (srcImage.isColorImage()) {
//            color = true;
//            cFactor = 4;
//        }
//
//        try {
//            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
//            buffer = new float[length];
//            resultBuffer = new float[length];
//        } catch (OutOfMemoryError e) {
//            buffer = null;
//            resultBuffer = null;
//            errorCleanUp("Algorithm Separable Gaussian Blur: Out of memory", true);
//
//            return;
//        }
//
//        fireProgressStateChanged(0, srcImage.getImageName(), "Blurring image ...");
//
//        float stepProgressValuePerVolume = (float) 100 / nVolumes;
//
//        for (t = 0; (t < nVolumes) && !threadStopped; t++) {
//
//            fireProgressStateChanged(0 + Math.round(t * stepProgressValuePerVolume), srcImage.getImageName(),
//                                     "Calculating the gaussian blur of slice " + (t + 1) + "...");
//
//            try {
//                srcImage.exportData(t * length, length, buffer); // locks and releases lock
//            } catch (IOException error) {
//                displayError("Algorithm Separable Gaussian Blur: Image(s) locked");
//                setCompleted(false);
//                fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
//
//                return;
//            }
//
//            AlgorithmSeparableConvolver convolver = null;
//
//            if (stepProgressValuePerVolume < 2) {
//                convolver = new AlgorithmSeparableConvolver(buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1],
//                                                                srcImage.getExtents()[2]
//                                                            }, GxDataRound, GyDataRound, GzDataRound, color);
//            } else {
//                convolver = new AlgorithmSeparableConvolver(resultBuffer, buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1],
//                                                                srcImage.getExtents()[2]
//                                                            }, GxDataRound, GyDataRound, GzDataRound, color);
//
//                convolver.setProgressValues(generateProgressValues(getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerVolume * t),
//                                                                   getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerVolume * (t + 1))));
//                linkProgressToAlgorithm(convolver);
//            }
//
//            convolver.setRunningInSeparateThread(runningInSeparateThread);
//
//            if (!entireImage) {
//                convolver.setMask(mask);
//            }
//
//            if (color) {
//                convolver.setColorChannels(red, green, blue);
//            }
//
//            convolver.run();
//            convolver.finalize();
//            convolver = null;
//
//
//            if (threadStopped) {
//                finalize();
//
//                return;
//            }
//
//            try {
//                srcImage.importData(t * length, buffer, false);
//            } catch (IOException error) {
//                errorCleanUp("Algorithm Separable Gaussian Blur: Image(s) locked", false);
//
//                return;
//            }
//        } // for (t = 0; t < nVolumes && !threadStopped; t++)
//
//
//        if (threadStopped) {
//            finalize();
//
//            return;
//        }
//
//        buffer = null;
//        resultBuffer = null;
//        srcImage.calcMinMax();
//
//        setCompleted(true);
//    }


    /**
     * This function produces a new image that has been blurred.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
//    private void calcStoreInDest2D(int nImages) {
//
//        int s;
//        int length;
//        int start;
//        float[] buffer;
//        boolean color = false;
//        int cFactor = 1;
//
//        if (srcImage.isColorImage()) {
//            color = true;
//            cFactor = 4;
//        }
//
//        try {
//            length = cFactor * srcImage.getSliceSize();
//            buffer = new float[length];
//        } catch (OutOfMemoryError e) {
//            buffer = null;
//            errorCleanUp("Algorithm Gaussian Blur:  Out of memory", true);
//
//            return;
//        }
//
//        float stepProgressValuePerImage = (float) 100 / nImages;
//        fireProgressStateChanged(0, srcImage.getImageName(), "Blurring image ...");
//
//        for (s = 0; (s < nImages) && !threadStopped; s++) {
//            start = s * length;
//
//            fireProgressStateChanged(0 + Math.round(s * stepProgressValuePerImage), srcImage.getImageName(),
//                                     "Calculating the gaussian blur of slice " + (s + 1) + "...");
//
//            try {
//                srcImage.exportData(start, length, buffer); // locks and releases lock
//            } catch (IOException error) {
//                buffer = null;
//                errorCleanUp("Algorithm Separable Gaussian Blur: Image(s) locked", true);
//
//                return;
//            }
//
//            AlgorithmSeparableConvolver convolver = null;
//
//            if (stepProgressValuePerImage < 2) {
//                convolver = new AlgorithmSeparableConvolver(buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1]
//                                                            }, GxDataRound, GyDataRound, color);
//            } else {
//                convolver = new AlgorithmSeparableConvolver(buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1]
//                                                            }, GxDataRound, GyDataRound, color);
//
//                convolver.setProgressValues(generateProgressValues(getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerImage * s),
//                                                                   getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerImage * (s + 1))));
//                linkProgressToAlgorithm(convolver);
//            }
//
//            convolver.setRunningInSeparateThread(runningInSeparateThread);
//
//            if (!entireImage) {
//                convolver.setMask(mask);
//            }
//
//            if (color) {
//                convolver.setColorChannels(red, green, blue);
//            }
//
//            // convolver.setProgressBar(progressBar, Math.round(s * inc), Math.min(100, Math.round((s + 1) * inc)),
//            // true);
//
//            convolver.run();
//            convolver.finalize();
//            convolver = null;
//
//            try {
//                destImage.importData(start, buffer, false);
//            } catch (IOException error) {
//                buffer = null;
//                errorCleanUp("Algorithm Separable Gaussian Blur: Image(s) locked", true);
//
//                return;
//            }
//
//        } // for (s = 0; s < nImages; s++)
//
//        if (threadStopped) {
//            finalize();
//
//            return;
//        }
//
//        destImage.calcMinMax();
//
//        setCompleted(true);
//    }

    /**
     * Produces a new image that has been blurred.
     *
     * @param  nVolumes  DOCUMENT ME!
     */
//    private void calcStoreInDest3D(int nVolumes) {
//
//        int t;
//        int length;
//        float[] buffer;
//        boolean color = false;
//        int cFactor = 1;
//
//        if (srcImage.isColorImage()) {
//            color = true;
//            cFactor = 4;
//        }
//
//
//        try {
//            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
//            buffer = new float[length];
//        } catch (OutOfMemoryError e) {
//            buffer = null;
//            errorCleanUp(" Separable Algorithm Gaussian Blur: Out of memory", true);
//
//            return;
//        }
//
//        fireProgressStateChanged(0, srcImage.getImageName(), "Blurring image ...");
//
//        float stepProgressValuePerVolume = (float) 100 / nVolumes;
//
//        for (t = 0; (t < nVolumes) && !threadStopped; t++) {
//
//            fireProgressStateChanged(0 + Math.round(t * stepProgressValuePerVolume), srcImage.getImageName(),
//                                     "Calculating the gaussian blur of slice " + (t + 1) + "...");
//
//            try {
//                srcImage.exportData(t * length, length, buffer); // locks and releases lock
//            } catch (IOException error) {
//                displayError("Algorithm Separable Gaussian Blur: Image(s) locked");
//                setCompleted(false);
//                fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
//
//                return;
//            }
//
//            AlgorithmSeparableConvolver convolver = null;
//
//            if (stepProgressValuePerVolume < 2) {
//                convolver = new AlgorithmSeparableConvolver(buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1],
//                                                                srcImage.getExtents()[2]
//                                                            }, GxDataRound, GyDataRound, GzDataRound, color, true);
//            } else {
//                convolver = new AlgorithmSeparableConvolver(buffer,
//                                                            new int[] {
//                                                                srcImage.getExtents()[0], srcImage.getExtents()[1],
//                                                                srcImage.getExtents()[2]
//                                                            }, GxDataRound, GyDataRound, GzDataRound, color, true);
//
//                convolver.setProgressValues(generateProgressValues(getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerVolume * t),
//                                                                   getMinProgressValue() +
//                                                                   Math.round(stepProgressValuePerVolume * (t + 1))));
//                linkProgressToAlgorithm(convolver);
//            }
//
//            convolver.setRunningInSeparateThread(runningInSeparateThread);
//
//            if (!entireImage) {
//                convolver.setMask(mask);
//            }
//
//            if (color) {
//                convolver.setColorChannels(red, green, blue);
//            }
//            convolver.run();
//            float[] resultBuffer = convolver.getResultBuffer();
//            convolver.finalize();
//            convolver = null;
//
//
//            if (threadStopped) {
//                finalize();
//
//                return;
//            }
//
//            try {
//                destImage.importData(t * length, resultBuffer, false);
//            } catch (IOException error) {
//                errorCleanUp("Algorithm Separable Gaussian Blur: Import 3D Image(s) locked", false);
//
//                return;
//            }
//
//        } // for (t = 0; t < nVolumes && !threadStopped; t++)
//
//        if (threadStopped) {
//            finalize();
//
//            return;
//        }
//
//        destImage.calcMinMax();
//
//        setCompleted(true);
//    }

    /**
     * DOCUMENT ME!
     *
     * @param  do3D  DOCUMENT ME!
     */
//    private void makeKernels1D(boolean do3D) {
//        int xkDim, ykDim, zkDim;
//        int[] derivOrder = new int[1];
//        derivOrder[0] = 0;
//        kExtents = new int[1];
//
//        xkDim = Math.round(5 * sigmas[0]);
//
//        if ((xkDim % 2) == 0) {
//            xkDim++;
//        }
//
//        if (xkDim < 3) {
//            xkDim = 3;
//        }
//
//        ykDim = Math.round(5 * sigmas[1]);
//
//        if ((ykDim % 2) == 0) {
//            ykDim++;
//        }
//
//        if (ykDim < 3) {
//            ykDim = 3;
//        }
//
//        kExtents[0] = xkDim;
//        GxDataRound = new float[xkDim];
//
//        float[] sigmasX = new float[1];
//        sigmasX[0] = sigmas[0];
//
//        GenerateGaussian Gx = new GenerateGaussian(GxDataRound, kExtents, sigmasX, derivOrder);
//        Gx.calc(false);
//        Gx.finalize();
//        Gx = null;
//
//        kExtents[0] = ykDim;
//        GyDataRound = new float[ykDim];
//
//        float[] sigmasY = new float[1];
//        sigmasY[0] = sigmas[1];
//
//        GenerateGaussian Gy = new GenerateGaussian(GyDataRound, kExtents, sigmasY, derivOrder);
//        Gy.calc(false);
//        Gy.finalize();
//        Gy = null;
//
//        if (do3D) {
//            zkDim = Math.round(5 * sigmas[2]);
//
//            if ((zkDim % 2) == 0) {
//                zkDim++;
//            }
//
//            if (zkDim < 3) {
//                zkDim = 3;
//            }
//
//            kExtents[0] = zkDim;
//            GzDataRound = new float[zkDim];
//
//            float[] sigmasZ = new float[1];
//            sigmasZ[0] = sigmas[2];
//
//            GenerateGaussian Gz = new GenerateGaussian(GzDataRound, kExtents, sigmasZ, derivOrder);
//            Gz.calc(false);
//            Gz.finalize();
//            Gz = null;
//        }
//
//    }

}
