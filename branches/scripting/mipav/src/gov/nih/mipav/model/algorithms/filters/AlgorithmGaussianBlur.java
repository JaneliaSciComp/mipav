package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

/**
 * The application of this algorithm blurs an image or VOI region of the image with a Gaussian function at a user
 * defined scale (sigma - standard deviation). In essence, convolving a Gaussian function produces the same result as a
 * low-pass or smoothing filter. A low-pass filter attenuates high frequency components of the image (i.e. edges) and
 * passes low frequency components and thus results in the blurring of the image. Smoothing filters are typically used
 * for noise reduction and for blurring. The standard deviation (SD) of the Gaussian function controls the amount of
 * blurring:a large SD (i.e. > 2) significantly blurs while a small SD (i.e. 0.5) blurs less. If the objective is to
 * achieve noise reduction, a rank filter (median) might be more useful.
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
public class AlgorithmGaussianBlur extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Storage location of the Gaussian kernel. */
    private float[] GaussData;

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** Flags indicate which color channel to process. True indicates the channel should be processed. */
    private boolean red, green, blue;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmGaussianBlur object.
     *
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the gaussian convolution will be performed over the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmGaussianBlur(ModelImage srcImg, float[] sigmas,
            boolean maskFlag, boolean img25D) {
        this(null, srcImg, sigmas, maskFlag, img25D);
    }

    /**
     * Creates a new AlgorithmGaussianBlur object.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the gaussian convolution will be performed over the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmGaussianBlur(ModelImage destImg, ModelImage srcImg,
            float[] sigmas, boolean maskFlag, boolean img25D) {
        this(destImg, srcImg, sigmas, maskFlag, img25D, 0, 100);
    }

    /**
     * Constructor which sets the source and destination images, the minimum and maximum 
     * progress value.
     * @param destImg            the destination image
     * @param srcImg             the source image
     * @param sigmas             the sigmas
     * @param maskFlag           the mask flag
     * @param img25D             the 2.5D indicator
     * @param minProgressValue   the minimum progress value.
     * @param maxProgressValue   the maximum progress value.
     */
    public AlgorithmGaussianBlur(ModelImage destImg, ModelImage srcImg,
            float[] sigmas, boolean maskFlag, boolean img25D, int minProgressValue, int maxProgressValue) {
        super(destImg, srcImg, minProgressValue, maxProgressValue);

        this.sigmas = sigmas;
        entireImage = maskFlag;
        image25D = img25D;

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        GaussData = null;
        destImage = null;
        srcImage = null;
        kExtents = null;
        sigmas = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (srcImage.getNDims() == 2) {
            makeKernels2D();
        } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
            makeKernels3D();
        } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
            makeKernels2D();
        } else if (srcImage.getNDims() == 4) {
            makeKernels3D();
        }

        if (threadStopped) {
            setCompleted(false);
            return;
        }

        constructLog();

        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcStoreInDest3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcStoreInDest2D(srcImage.getExtents()[2]);
            } else if (srcImage.getNDims() == 4) {
                calcStoreInDest4D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcInPlace3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcInPlace2D(srcImage.getExtents()[2]);
            } else if (srcImage.getNDims() == 4) {
                calcInPlace4D();
            }
        }
    }

    /**
     * Sets the flag for the blue channel.
     *
     * @param  flag  if set to true then the blue channel is processed.
     */
    public void setBlue(boolean flag) {
        blue = flag;
    }

    /**
     * Sets the flag for the green channel.
     *
     * @param  flag  if set to true then the green channel is processed.
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

    /**
     * Calculates the blurred image and replaces the source image with the blurred image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace2D(int nImages) {

        int i, s;
        int length, totalLength;
        int start;
        float[] buffer;
        float[] resultBuffer;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        try {
            length = cFactor * srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            resultBuffer = new float[length * nImages];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

            return;
        }

        int mod = totalLength / (maxProgressValue - minProgressValue); // mod is 1 percent of length

        fireProgressStateChanged(minProgressValue, srcImage.getImageName(),
                "Blurring image ...");

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);

                return;
            }

            if (color == true) {

                for (i = 0; (i < length) && !threadStopped; i += 4) {

                    if (((start + i) % mod) == 0) {
                        fireProgressStateChanged(minProgressValue + Math.round((float) (start + i) / (totalLength - 1) * (maxProgressValue - minProgressValue)));
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        resultBuffer[start + i] = buffer[i];

                        if (red) {
                            resultBuffer[start + i + 1] = AlgorithmConvolver
                                    .convolve2DRGBPt(i + 1, srcImage
                                            .getExtents(), buffer, kExtents,
                                            GaussData);
                        } else {
                            resultBuffer[start + i + 1] = buffer[i + 1];
                        }

                        if (green) {
                            resultBuffer[start + i + 2] = AlgorithmConvolver
                                    .convolve2DRGBPt(i + 2, srcImage
                                            .getExtents(), buffer, kExtents,
                                            GaussData);
                        } else {
                            resultBuffer[start + i + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            resultBuffer[start + i + 3] = AlgorithmConvolver
                                    .convolve2DRGBPt(i + 3, srcImage
                                            .getExtents(), buffer, kExtents,
                                            GaussData);
                        } else {
                            resultBuffer[start + i + 3] = buffer[i + 3];
                        }
                    } else {
                        resultBuffer[start + i] = buffer[i];
                        resultBuffer[start + i + 1] = buffer[i + 1];
                        resultBuffer[start + i + 2] = buffer[i + 2];
                        resultBuffer[start + i + 3] = buffer[i + 3];
                    }
                }
            } else {

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if ((((start + i) % mod) == 0)) {
                        fireProgressStateChanged(minProgressValue + Math.round((float) (start + i) / (totalLength - 1) * (maxProgressValue - minProgressValue)));
                    }

                    if ((entireImage == true) || mask.get(i)) {
                        resultBuffer[start + i] = AlgorithmConvolver
                                .convolve2DPt(i, srcImage.getExtents(), buffer,
                                        kExtents, GaussData);
                    } else {
                        resultBuffer[start + i] = buffer[i];
                        // resultBuffer[i] = 0;
                    }
                }
            }
        }

        fireProgressStateChanged(maxProgressValue);

        if (threadStopped) {
            return;
        }

        try {

            // if (srcImage.getType() != ModelImage.FLOAT) {
            // srcImage.reallocate(ModelImage.FLOAT);
            // }
            srcImage.importData(0, resultBuffer, false);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);

            return;
        }

        if(maxProgressValue == 100){
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        }
        setCompleted(true);
    }

    /**
     * Calculates the blurred image and replaces the source image with the blurred image.
     */
    private void calcInPlace3D() {

        int i;
        int length;
        float[] buffer;
        float[] resultBuffer;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        try {
            length = cFactor * srcImage.getSliceSize()
                    * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            ;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

            return;
        }

        int mod = length / (maxProgressValue - minProgressValue); // mod is 1 percent of length

        fireProgressStateChanged(minProgressValue, srcImage.getImageName(),
                "Blurring image ...");

        if (color == true) {

            for (i = 0; (i < length) && !threadStopped; i += 4) {

                if ((((i) % mod) == 0)) {
                    fireProgressStateChanged(minProgressValue + Math.round((float) i / (length - 1) * (maxProgressValue-minProgressValue)));
                }

                if ((entireImage == true) || mask.get(i / 4)) {
                    resultBuffer[i] = buffer[i];

                    if (red) {
                        resultBuffer[i + 1] = AlgorithmConvolver
                                .convolve3DRGBPt(i + 1, srcImage.getExtents(),
                                        buffer, kExtents, GaussData);
                    } else {
                        resultBuffer[i + 1] = buffer[i + 1];
                    }

                    if (green) {
                        resultBuffer[i + 2] = AlgorithmConvolver
                                .convolve3DRGBPt(i + 2, srcImage.getExtents(),
                                        buffer, kExtents, GaussData);
                    } else {
                        resultBuffer[i + 2] = buffer[i + 2];
                    }

                    if (blue) {
                        resultBuffer[i + 3] = AlgorithmConvolver
                                .convolve3DRGBPt(i + 3, srcImage.getExtents(),
                                        buffer, kExtents, GaussData);
                    } else {
                        resultBuffer[i + 3] = buffer[i + 3];
                    }
                } else {
                    resultBuffer[i] = buffer[i];
                    resultBuffer[i + 1] = buffer[i + 1];
                    resultBuffer[i + 2] = buffer[i + 2];
                    resultBuffer[i + 3] = buffer[i + 3];
                }
            }
        } else {

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged(minProgressValue + Math.round((float) i / (length - 1) * (maxProgressValue - minProgressValue)));
                }

                if ((entireImage == true) || mask.get(i)) {
                    resultBuffer[i] = AlgorithmConvolver.convolve3DPt(i,
                            srcImage.getExtents(), buffer, kExtents, GaussData);
                } else {
                    resultBuffer[i] = buffer[i];
                    // resultBuffer[i] = 0;
                }
            }
        }

        fireProgressStateChanged(maxProgressValue);

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, resultBuffer, false);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);

            return;
        }
        if(maxProgressValue == 100){
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        }
        setCompleted(true);
    }

    /**
     * Calculates the blurred image and replaces the source image with the blurred image.
     */
    private void calcInPlace4D() {

        int i, t;
        int length;
        float[] buffer;
        float[] resultBuffer;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        try {
            length = cFactor * srcImage.getSliceSize()
                    * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

            return;
        }

        fireProgressStateChanged(minProgressValue, srcImage.getImageName(),
                "Blurring image ...");

        int index;
        int end = srcImage.getExtents()[3];
        int mod = length * end/(maxProgressValue - minProgressValue);
        for (t = 0; (t < end) && !threadStopped; t++) {

            try {
                srcImage.exportData(t * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Gaussian Blur: Image(s) locked");
                setCompleted(false);
                disposeProgressBar();

                return;
            }

            fireProgressStateChanged(minProgressValue + Math.round((float) t / end * (maxProgressValue - minProgressValue)));

            index = t * length;

            if (color == true) {

                for (i = 0; (i < length) && !threadStopped; i += 4) {
                    if(i % mod == 0){
                        fireProgressStateChanged(minProgressValue + (t*length+i)/mod);                        
                    }
                    if ((entireImage == true) || mask.get(i / 4)) {
                        resultBuffer[i] = buffer[i];

                        if (red) {
                            resultBuffer[i + 1] = AlgorithmConvolver
                                    .convolve3DRGBPt(i + 1, srcImage
                                            .getExtents(), buffer, kExtents,
                                            GaussData);
                        } else {
                            resultBuffer[i + 1] = buffer[i + 1];
                        }

                        if (green) {
                            resultBuffer[i + 2] = AlgorithmConvolver
                                    .convolve3DRGBPt(i + 2, srcImage
                                            .getExtents(), buffer, kExtents,
                                            GaussData);
                        } else {
                            resultBuffer[i + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            resultBuffer[i + 3] = AlgorithmConvolver
                                    .convolve3DRGBPt(i + 3, srcImage
                                            .getExtents(), buffer, kExtents,
                                            GaussData);
                        } else {
                            resultBuffer[i + 3] = buffer[i + 3];
                        }
                    } else {
                        resultBuffer[i] = buffer[i];
                        resultBuffer[i + 1] = buffer[i + 1];
                        resultBuffer[i + 2] = buffer[i + 2];
                        resultBuffer[i + 3] = buffer[i + 3];
                    }
                }
            } else {

                for (i = 0; (i < length) && !threadStopped; i++) {
                    if(i % mod == 0){
                        fireProgressStateChanged(minProgressValue + (t*length+i)/mod);                        
                    }
                    if ((entireImage == true) || mask.get(i)) {
                        resultBuffer[i] = AlgorithmConvolver.convolve3DPt(i,
                                srcImage.getExtents(), buffer, kExtents,
                                GaussData);
                    } else {
                        resultBuffer[i] = buffer[i];
                    }
                }
            }

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                srcImage.importData(index, resultBuffer, false);
            } catch (IOException error) {
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);

                return;
            }
        }

        fireProgressStateChanged(maxProgressValue);

        if (threadStopped) {
            finalize();
            return;
        }

        buffer = null;
        resultBuffer = null;
        if(maxProgressValue == 100){
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        }
        setCompleted(true);
    }

    /**
     * This function produces a new image that has been blurred.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
private void calcStoreInDest2D(int nImages) {

        int i, s, idx;
        int length, totalLength;
        int start;
        float[] buffer;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp(" GaussianBlur: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Gaussian Blur:  Out of memory", true);

            return;
        }

        int mod = totalLength / (maxProgressValue - minProgressValue); // mod is 1 percent of length
        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Blurring image ...");

        for (s = 0; s < nImages; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);

                return;
            }

            if (color == true) {

                for (i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {

                    if ( ((start + i) % mod) == 0) {
                        fireProgressStateChanged(minProgressValue + Math.round((float) (start + i) / (totalLength - 1) * (maxProgressValue - minProgressValue)));
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        destImage.set(idx, buffer[i]); // alpha

                        if (red) {
                            destImage.set(idx + 1,
                                          AlgorithmConvolver.convolve2DRGBPt(i + 1, srcImage.getExtents(), buffer,
                                                                             kExtents, GaussData));
                        } else {
                            destImage.set(idx + 1, buffer[i + 1]);
                        }

                        if (green) {
                            destImage.set(idx + 2,
                                          AlgorithmConvolver.convolve2DRGBPt(i + 2, srcImage.getExtents(), buffer,
                                                                             kExtents, GaussData));
                        } else {
                            destImage.set(idx + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            destImage.set(idx + 3,
                                          AlgorithmConvolver.convolve2DRGBPt(i + 3, srcImage.getExtents(), buffer,
                                                                             kExtents, GaussData));
                        } else {
                            destImage.set(idx + 3, buffer[i + 3]);
                        }
                    } else {
                        destImage.set(idx, buffer[i]);
                        destImage.set(idx + 1, buffer[i + 1]);
                        destImage.set(idx + 2, buffer[i + 2]);
                        destImage.set(idx + 3, buffer[i + 3]);
                    }
                }
            } else {

                for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                    if (((start + i) % mod) == 0) {
                        fireProgressStateChanged(minProgressValue + Math.round((float) (start + i) / (totalLength - 1) * (maxProgressValue - minProgressValue)));
                    }

                    if ((entireImage == true) || mask.get(i)) {
                        destImage.set(idx,
                                      AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents,
                                                                      GaussData));
                    } else {
                        destImage.set(idx, buffer[i]);
                    }
                }
            }
        }

        fireProgressStateChanged(maxProgressValue);
        
        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        if(maxProgressValue == 100){
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        }
        setCompleted(true);
    }
    /**
     * Produces a new image that has been blurred.
     */
private void calcStoreInDest3D() {

        int i;
        int length;
        float[] buffer;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("GaussianBlur: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];

            // System.out.println ("sliceSize = " + srcImage.getSliceSize() + "  Length = " + length);
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

            return;
        }

        int mod = length / (maxProgressValue - minProgressValue); // mod is 1 percent of length
        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Blurring image ...");

        if (color == true) {

            for (i = 0; (i < length) && !threadStopped; i += 4) {

                if ((i % mod) == 0) {
                    fireProgressStateChanged(minProgressValue + Math.round((float) (i) / (length - 1) * (maxProgressValue - minProgressValue)));
                }

                if ((entireImage == true) || mask.get(i / 4)) {
                    destImage.set(i, buffer[i]); // alpha

                    if (red) {
                        destImage.set(i + 1,
                                      AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                         GaussData));
                    } else {
                        destImage.set(i + 1, buffer[i + 1]);
                    }

                    if (green) {
                        destImage.set(i + 2,
                                      AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                         GaussData));
                    } else {
                        destImage.set(i + 2, buffer[i + 2]);
                    }

                    if (blue) {
                        destImage.set(i + 3,
                                      AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                         GaussData));
                    } else {
                        destImage.set(i + 3, buffer[i + 3]);
                    }
                } else {
                    destImage.set(i, buffer[i]);
                    destImage.set(i + 1, buffer[i + 1]);
                    destImage.set(i + 2, buffer[i + 2]);
                    destImage.set(i + 3, buffer[i + 3]);
                }
            }
        } else {

            for (i = 0; (i < length) && !threadStopped; i++) {

                if ((i % mod) == 0) {
                    fireProgressStateChanged(minProgressValue + Math.round((float) i / (length - 1) * (maxProgressValue - minProgressValue)));
                    // System.out.println("Entire = " + entireImage);
                }

                if ((entireImage == true) || mask.get(i)) {
                    destImage.set(i,
                                  AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents,
                                                                  GaussData));
                } else {
                    destImage.set(i, buffer[i]);
                }
            }
        }

        fireProgressStateChanged(maxProgressValue);

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        if(maxProgressValue == 100){
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        }
        setCompleted(true);
    }
    /**
     * Produces a new image that has been blurred.
     */
private void calcStoreInDest4D() {

        int i, t;
        int length;
        float[] buffer;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }


        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp(" GaussianBlur: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

            return;
        }

        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Blurring image ...");

        int index;
        int end = srcImage.getExtents()[3];

        int mod = length*end / (maxProgressValue - minProgressValue);
        float progressRange = (maxProgressValue - minProgressValue)/end;
        for (t = 0; (t < end) && !threadStopped; t++) {

            try {
                srcImage.exportData(t * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Gaussian Blur: Image(s) locked");
                setCompleted(false);
                fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                destImage.releaseLock();

                return;
            }

            fireProgressStateChanged(minProgressValue + Math.round((float) t / end * (maxProgressValue - minProgressValue)));

            index = t * length;

            if (color == true) {

                for (i = 0; (i < length) && !threadStopped; i += 4) {
                    if(i % mod == 0){
                        fireProgressStateChanged(minProgressValue + Math.round((t*length+i)/mod));                        
                    }
                    if ((entireImage == true) || mask.get(i / 4)) {
                        destImage.set(i + index, buffer[i]); // alpha

                        if (red) {
                            destImage.set(i + index + 1,
                                          AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer,
                                                                             kExtents, GaussData));
                        } else {
                            destImage.set(i + index + 1, buffer[i + 1]);
                        }

                        if (green) {
                            destImage.set(i + index + 2,
                                          AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer,
                                                                             kExtents, GaussData));
                        } else {
                            destImage.set(i + index + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            destImage.set(i + index + 3,
                                          AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer,
                                                                             kExtents, GaussData));
                        } else {
                            destImage.set(i + index + 3, buffer[i + 3]);
                        }
                    } else {
                        destImage.set(i + index, buffer[i]);
                        destImage.set(i + index + 1, buffer[i + 1]);
                        destImage.set(i + index + 2, buffer[i + 2]);
                        destImage.set(i + index + 3, buffer[i + 3]);
                    }
                }
            } else {

                for (i = 0; (i < length) && !threadStopped; i++) {
                    if(i % mod == 0){
                        fireProgressStateChanged(minProgressValue + Math.round((t*length+i)/mod));                        
                    }
                    if ((entireImage == true) || mask.get(i)) {
                        destImage.set(index + i,
                                      AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents,
                                                                      GaussData));
                    } else {
                        destImage.set(index + i, buffer[i]);
                    }
                }
            }
        }
        fireProgressStateChanged(maxProgressValue);
        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        if(maxProgressValue == 100){
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
        }
        setCompleted(true);
    }
    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {
        String sigmaStr = new String();

        for (int i = 0; i < sigmas.length; i++) {
            sigmaStr += (" " + String.valueOf(sigmas[i]) + ", ");
        }

        if (srcImage.isColorImage()) {
            historyString = new String("GaussianBlur(" + sigmaStr
                    + String.valueOf(entireImage) + ", "
                    + String.valueOf(image25D) + ", " + red + ", " + green
                    + ", " + blue + ")\n");
        } else {
            historyString = new String("GaussianBlur(" + sigmaStr
                    + String.valueOf(entireImage) + ", "
                    + String.valueOf(image25D) + ")\n");
        }
    }

    /**
     * Creates 2D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (6X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 0;
        derivOrder[1] = 0;

        xkDim = Math.round(6 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(6 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        GaussData = new float[xkDim * ykDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData, kExtents,
                sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;
    }

    /**
     * Creates 3D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (6X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(6 * sigmas[0]);
        // System.out.println("Sigma 0 = " + sigmas[0]);
        // System.out.println("Sigma 1 = " + sigmas[1]);
        // System.out.println("Sigma 2 = " + sigmas[2]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(6 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(6 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        kExtents[2] = zkDim;

        GaussData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData, kExtents,
                sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;
    }

}
