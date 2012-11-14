package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.*;

import java.util.BitSet;
import java.util.concurrent.CountDownLatch;


/**
 * Convolves an image with a separable (symmetric) kernel and returns the result. The Gaussian and its derivatives are
 * separable.
 * 
 * <p>
 * Faster than the regualar convolver -- reg 2D: n^d*m^d, sep 2D: d*n^d*m ; d = img dimension, n = img size, m = kern
 * size -- but requires more memory -- ~2 times more for 2D, ~3 times for 3D (plus significant extra memory required for
 * algorithms which use this class (2 or 3 times more, depending on the dimension)).
 * </p>
 * 
 * <p>
 * Also note that this convolver has a different interface which must be used than the static point convolution
 * functions which most algorithms use from the AlgorithmConvolver.
 * </p>
 * 
 * <p>
 * Color and voi mask code not tested (although the voi code is pretty straight-forward).
 * </p>
 * 
 * @see AlgorithmConvolver
 * @author Evan McCreedy
 */
public class AlgorithmSeparableConvolver extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Buffer size adjustment for color images. */
    private int cFactor = 1;

    /** Flag to indicate if the source image is color. */
    private boolean colorImage = false;

    int curPercent;

    int incIndex;

    /** Holds the result image data. */
    private double[] outputBuffer;

    /** Whether to convolve the whole image or just pixels inside a mask. */
    private boolean entireImage = true;

    /** Holds the original image data. */
    private double[] inputBuffer;
    
    /** Holds the original image mask data. */
    private boolean[] inputMask;

    /** The dimensions of the both source and destination images. */
    private int[] imgExtents;

    /** The dimensions of the kernel. */
    private int[] kernelExtents;

    private float[][] kernelBuffer;

    /** Flags to indicate which color channels to process. */
    private boolean red = true, green = true, blue = true;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Sets destination, source, and kernel buffers. Call run() to convolve image.
     * 
     * @param srcBuffer source image data buffer
     * @param iExtents source and destination image dimensions
     * @param kernBuffer kernel image data buffer (kernel must be symmetric)
     * @param kExtents kernel dimensions
     * @param color whether the image is color
     * @param minProgressValue the minimum progress value.
     * @param maxProgressValue the maximum progress value.
     */
    public AlgorithmSeparableConvolver(final float[] srcBuffer, final int[] iExtents, final float[][] kernBuffer,
            final boolean colorImage) {
        super(null, null);
        this.inputBuffer = new double[srcBuffer.length];
        ArrayUtil.arrayCopy(srcBuffer, 0, inputBuffer, 0, srcBuffer.length);

        if (multiThreadingEnabled) {
            this.outputBuffer = new double[inputBuffer.length];
        } else {
            this.outputBuffer = this.inputBuffer;
        }

        this.imgExtents = iExtents;
        this.kernelBuffer = kernBuffer;
        kernelExtents = new int[kernBuffer.length];
        for (int i = 0; i < kernelBuffer.length; i++) {
            kernelExtents[i] = kernelBuffer[i].length;
        }
        this.colorImage = colorImage;

        if (colorImage) {
            cFactor = 4;
        }
    }

    public AlgorithmSeparableConvolver(final float[] srcBuffer, final int[] iExtents, final float[] kernBuffers,
            final int[] kExtents, final boolean colorImage) {
        super(null, null);
        this.inputBuffer = new double[srcBuffer.length];
        for (int i = 0; i < srcBuffer.length; i++) {
            inputBuffer[i] = srcBuffer[i];
        }
        // System.arraycopy(srcBuffer, 0, inputBuffer, 0, srcBuffer.length);

        if (multiThreadingEnabled) {
            this.outputBuffer = new double[inputBuffer.length];
        } else {
            this.outputBuffer = this.inputBuffer;
        }

        this.imgExtents = iExtents;
        this.kernelBuffer = new float[kExtents.length][];
        this.kernelExtents = kExtents;

        int interval = 1;
        for (int i = 0; i < kernelExtents.length; i++) {
            kernelBuffer[i] = new float[kernelExtents[i]];
            for (int j = 0; j < kernelExtents[i]; j++) {
                kernelBuffer[i][j] = kernBuffers[j * interval];
            }
            interval *= kernelExtents[i];
        }
        this.colorImage = colorImage;

        if (colorImage) {
            cFactor = 4;
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        inputBuffer = null;
        imgExtents = null;
        kernelExtents = null;
        kernelBuffer = null;
        outputBuffer = null;

        super.finalize();
    }

    /**
     * Perform one-dimension convolution.
     * 
     * @param imageBuffer
     * @param kernelBuffer
     * @param resultBuffer
     */
    @SuppressWarnings("unused")
    private void convolve(final double[] imageBuffer, final float[] kernelBuffer, final double[] resultBuffer) {
        boolean skipRed = false;
        boolean skipGreen = false;
        boolean skipBlue = false;

        if (colorImage && !red) {
            skipRed = true;
        }

        if (colorImage && !green) {
            skipGreen = true;
        }

        if (colorImage && !blue) {
            skipBlue = true;
        }
        final int kernelDim = kernelBuffer.length;
        final int halfKernelDimTimesCFactor = kernelDim / 2 * cFactor;
        for (int i = 0; i < imageBuffer.length; i++) {
            if (skipRed && ( (i % 4) == 1)) {
                resultBuffer[i] = imageBuffer[i];
            } else if (skipGreen && ( (i % 4) == 2)) {
                resultBuffer[i] = imageBuffer[i];
            } else if (skipBlue && ( (i % 4) == 3)) {
                resultBuffer[i] = imageBuffer[i];
            } else if (entireImage || mask.get(i / cFactor)) {
                int count = 0;
                double sum = 0;
                double norm = 0;
                int start = i - halfKernelDimTimesCFactor;
                int end = start + kernelDim * cFactor;
                if (start < 0) {
                    count = count - ( (start - (cFactor - 1)) / cFactor);
                    if (cFactor > 1) {
                        start = i % cFactor;
                    } else {
                        start = 0;
                    }
                }
                if (end > imageBuffer.length) {
                    end = imageBuffer.length;
                }
                for (int j = start; j < end; j += cFactor) {
                    sum += kernelBuffer[count] * imageBuffer[j];
                    if (kernelBuffer[count] > 0) {
                        norm += kernelBuffer[count];
                    } else {
                        norm -= kernelBuffer[count];
                    }
                    count++;
                }
                resultBuffer[i] = sum / norm;
            } else {
                resultBuffer[i] = imageBuffer[i];
            }
        }
    }

    /**
     * Perform one-dimension convolution.
     * 
     * @param imageBuffer
     * @param kernelBuffer
     * @param resultBuffer
     */
    private void convolveNoColor(final double[] imageBuffer, final float[] kernelBuffer, final double[] resultBuffer,
            boolean[] maskBuffer) {

        final int kernelDim = kernelBuffer.length;
        final int halfKernelDim = kernelDim / 2;
        for (int i = 0; i < imageBuffer.length; i++) {
            if (entireImage || maskBuffer[i]) {
                int count = 0;
                double sum = 0;
                double norm = 0;
                int start = i - halfKernelDim;
                int end = start + kernelDim;
                if (start < 0) {
                    count = count - start;
                    start = 0;
                }
                if (end > imageBuffer.length) {
                    end = imageBuffer.length;
                }
                for (int j = start; j < end; j++) {
                    sum += kernelBuffer[count] * imageBuffer[j];
                    if (kernelBuffer[count] > 0) {
                        norm += kernelBuffer[count];
                    } else {
                        norm -= kernelBuffer[count];
                    }
                    count++;
                }
                resultBuffer[i] = sum / norm;
            } else {
                resultBuffer[i] = imageBuffer[i];
            }
        }
    }

    public void performMT() {
        final int size = inputBuffer.length;
        if (imgExtents.length < 3 || kernelBuffer.length < 3) {
            setProgressStep( (imgExtents[0] * imgExtents[1] * cFactor * 2) / (maxProgressValue - minProgressValue));
        } else {
            setProgressStep( (imgExtents[0] * imgExtents[1] * imgExtents[2] * cFactor * 3)
                    / (maxProgressValue - minProgressValue));
        }

        progress = minProgressValue;

        // convolve the image with the X dimension kernel
        final CountDownLatch doneSignalx = new CountDownLatch(nthreads);
        final float step2 = size / nthreads;
        for (int i = 0; i < nthreads; i++) {
            final int start2 = (int) (step2 * i);
            final int end2 = (int) (step2 * (i + 1));
            final Runnable task = new Runnable() {
                public void run() {
                    convolveX(start2, end2);
                    doneSignalx.countDown();
                }
            };
            ThreadUtil.mipavThreadPool.execute(task);
        }
        try {
            doneSignalx.await();
        } catch (final InterruptedException e) {
            gov.nih.mipav.view.MipavUtil.displayError(e.getMessage());
            return;
        }

        // y kernel dimensions
        final CountDownLatch doneSignaly = new CountDownLatch(nthreads);
        for (int i = 0; i < nthreads; i++) {
            final int start2 = (int) (step2 * i);
            final int end2 = (int) (step2 * (i + 1));
            final Runnable task = new Runnable() {
                public void run() {
                    convolveY(start2, end2);
                    doneSignaly.countDown();
                }
            };
            ThreadUtil.mipavThreadPool.execute(task);
        }
        try {
            doneSignaly.await();
        } catch (final InterruptedException e) {
            gov.nih.mipav.view.MipavUtil.displayError(e.getMessage());
            return;
        }

        if (imgExtents.length < 3 || kernelBuffer.length < 3) {
            System.arraycopy(inputBuffer, 0, outputBuffer, 0, inputBuffer.length);
            return;
        }
        // z kernel dimensions
        final CountDownLatch doneSignalz = new CountDownLatch(nthreads);
        for (int i = 0; i < nthreads; i++) {
            final int start2 = (int) (step2 * i);
            final int end2 = (int) (step2 * (i + 1));
            final Runnable task = new Runnable() {
                public void run() {
                    convolveZ(start2, end2);
                    doneSignalz.countDown();
                }
            };
            ThreadUtil.mipavThreadPool.execute(task);
        }
        try {
            doneSignalz.await();
        } catch (final InterruptedException e) {
            gov.nih.mipav.view.MipavUtil.displayError(e.getMessage());
            return;
        }
    }

    /**
     * Begins the excution of the 3D convolver.
     */
    public void perform() {
        final int xDim = imgExtents[0];
        final int yDim = imgExtents[1];
        final int zDim = (imgExtents.length == 2) ? 1 : imgExtents[2];
        final int sliceSize = xDim * yDim * cFactor;

        final int xoffset = xDim * cFactor;

        int numColors = 1;
        if (colorImage) {
            numColors = 0;
            if (red) {
                numColors++;
            }
            if (green) {
                numColors++;
            }
            if (blue) {
                numColors++;
            }
        }

        int colorIn;

        if (zDim == 1 || kernelBuffer.length == 2) {
            progressStep = numColors * (yDim * zDim + xDim * zDim) / (maxProgressValue - minProgressValue);
        } else {
            progressStep = numColors * (yDim * zDim + xDim * zDim + xDim * yDim)
                    / (maxProgressValue - minProgressValue);
        }

        progress = minProgressValue;
        final double[] xrowBuffer = new double[xDim];
        final double[] xresultBuffer = new double[xDim];
        final boolean[] maskBuffer = new boolean[xDim];

        // Convolve the image with the X dimension kernel
        colorIn = -1;
        for (int c = 0; c < cFactor; c++) {
            if ( ( !colorImage) || (red && c == 1) || (green && c == 2) || (blue && c == 3)) {
                colorIn++;
                for (int row = 0; (row < yDim * zDim) && !threadStopped; row++) {

                    if ( (colorIn * yDim * zDim + row) % progressStep == 0) {
                        fireProgressStateChanged((int) progress++);
                    }
                    ArrayUtil.rowCopy(inputBuffer, row * xoffset + c, xrowBuffer, 0, xDim, cFactor, 1);
                    if ( !entireImage )
                    {
                        ArrayUtil.rowCopy(inputMask, row * xoffset + c, maskBuffer, 0, xDim, cFactor, 1);
                    }
                    convolveNoColor(xrowBuffer, kernelBuffer[0], xresultBuffer, maskBuffer);
                    ArrayUtil.rowCopy(xresultBuffer, 0, inputBuffer, row * xoffset + c, xDim, 1, cFactor);
                }
            }
        }

        // Convolve the result image from above with the Y dimension kernel
        final double[] yrowBuffer = new double[yDim];
        final double[] yresultBuffer = new double[yDim];
        colorIn = -1;
        for (int c = 0; c < cFactor; c++) {
            if ( ( !colorImage) || (red && c == 1) || (green && c == 2) || (blue && c == 3)) {
                colorIn++;
                for (int z = 0; z < zDim; z++) {
                    for (int row = 0; (row < xDim) && !threadStopped; row++) {
                        if ( (colorIn * xDim * zDim + row) % progressStep == 0) {
                            fireProgressStateChanged((int) progress++);
                        }
                        ArrayUtil.rowCopy(inputBuffer, z * sliceSize + row * cFactor + c, yrowBuffer, 0, yDim, xoffset,
                                1);
                        if ( !entireImage )
                        {
                            ArrayUtil.rowCopy(inputMask, z * sliceSize + row * cFactor + c, maskBuffer, 0, yDim, xoffset,
                                    1);
                        }
                        convolveNoColor(yrowBuffer, kernelBuffer[1], yresultBuffer, maskBuffer);
                        ArrayUtil.rowCopy(yresultBuffer, 0, inputBuffer, z * sliceSize + row * cFactor + c, yDim, 1,
                                xoffset);
                    }
                }
            }
        }

        // Convolve the result image from above with the Z dimension kernel
        if (zDim > 1 && kernelBuffer.length > 2) {
            final double[] zrowBuffer = new double[zDim];
            final double[] zresultBuffer = new double[zDim];
            colorIn = -1;
            for (int c = 0; c < cFactor; c++) {
                if ( ( !colorImage) || (red && c == 1) || (green && c == 2) || (blue && c == 3)) {
                    colorIn++;
                    for (int row = 0; (row < xDim * yDim) && !threadStopped; row++) {
                        if ( (colorIn * xDim * yDim + row) % progressStep == 0) {
                            fireProgressStateChanged((int) progress++);
                        }
                        ArrayUtil.rowCopy(inputBuffer, row * cFactor + c, zrowBuffer, 0, zDim, sliceSize, 1);
                        if ( !entireImage )
                        {
                            ArrayUtil.rowCopy(inputMask, row * cFactor + c, maskBuffer, 0, zDim, sliceSize, 1);
                        }
                        convolveNoColor(zrowBuffer, kernelBuffer[2], zresultBuffer, maskBuffer);
                        ArrayUtil.rowCopy(zresultBuffer, 0, inputBuffer, row * cFactor + c, zDim, 1, sliceSize);
                    }
                }
            }
        }
    }

    private void convolveX(final int from, final int to) {
        final int xDim = imgExtents[0];
        final int yDim = imgExtents[1];

        final int offset = xDim * cFactor;
        final int sliceSize = xDim * yDim * cFactor;
        int offsetX, offsetY, offsetZ;
        final int kDim = kernelBuffer[0].length;
        final int halfKDim = kDim / 2;
        final int halfKDimTimesCFactor = halfKDim * cFactor;
        int combined, start, end;
        double sum, norm;
        int index = 0;
        for (int pix = from; (pix < to) && !threadStopped; pix++) {
            if (colorImage && !red && ( (pix % 4) == 1)) {
                outputBuffer[pix] = inputBuffer[pix];
            } else if (colorImage && !green && ( (pix % 4) == 2)) {
                outputBuffer[pix] = inputBuffer[pix];
            } else if (colorImage && !blue && ( (pix % 4) == 3)) {
                outputBuffer[pix] = inputBuffer[pix];
            } else if (entireImage || mask.get(pix / cFactor)) {
                offsetX = (pix % offset) - halfKDimTimesCFactor;
                offsetY = (pix % sliceSize) / offset;
                offsetZ = (pix / sliceSize);

                combined = (offsetY * offset) + (offsetZ * sliceSize);

                int count = 0;
                sum = 0;
                norm = 0;
                start = offsetX;
                end = start + (kDim * cFactor);

                if (start < 0) {
                    count = - ( (offsetX - (cFactor - 1)) / cFactor);
                    if (cFactor > 1) {
                        start = (pix % cFactor);
                    } else {
                        start = 0;
                    }
                }

                if (end > offset) {
                    end = offset;
                }

                for (int i = start; i < end; i += cFactor) {
                    sum += kernelBuffer[0][count] * inputBuffer[i + combined];

                    if (kernelBuffer[0][count] >= 0) {
                        norm += kernelBuffer[0][count];
                    } else {
                        norm -= kernelBuffer[0][count];
                    }

                    count++;
                }

                outputBuffer[pix] = sum / norm;
            } else {
                outputBuffer[pix] = inputBuffer[pix];
            }
            index++;
            if (index % progressStep == 0) {
                makeProgress(1);
                fireProgressStateChanged((int) getProgress());
            }
        }
    }

    private void convolveY(final int from, final int to) {
        final int xDim = imgExtents[0];
        final int yDim = imgExtents[1];

        final int offset = xDim * cFactor;
        final int sliceSize = xDim * yDim * cFactor;
        int offsetX, offsetY, offsetZ;
        final int kDim = kernelBuffer[1].length;
        final int halfKDim = kDim / 2;
        int combined, start, end;
        final int step = kDim * offset;
        double sum, norm;
        int index = 0;
        for (int pix = from; (pix < to) && !threadStopped; pix++) {
            if (colorImage && !red && ( (pix % 4) == 1)) {
                inputBuffer[pix] = outputBuffer[pix];
            } else if (colorImage && !green && ( (pix % 4) == 2)) {
                inputBuffer[pix] = outputBuffer[pix];
            } else if (colorImage && !blue && ( (pix % 4) == 3)) {
                inputBuffer[pix] = outputBuffer[pix];
            } else if (entireImage || mask.get(pix / cFactor)) {
                offsetX = (pix % offset);
                offsetY = ( (pix % sliceSize) / offset) - halfKDim;
                offsetZ = (pix / sliceSize);

                combined = offsetX + (offsetZ * sliceSize);

                int count = 0;
                sum = 0;
                norm = 0;
                start = offsetY * offset;
                end = start + step;

                if (start < 0) {
                    count = -offsetY;
                    start = 0;
                }

                if (end > (offset * yDim)) {
                    end = offset * yDim;
                }

                for (int i = start; i < end; i += offset) {
                    sum += kernelBuffer[1][count] * outputBuffer[i + combined];

                    if (kernelBuffer[1][count] >= 0) {
                        norm += kernelBuffer[1][count];
                    } else {
                        norm -= kernelBuffer[1][count];
                    }

                    count++;
                }

                // use imgBuffer as a temp buffer since we won't need to use it again
                inputBuffer[pix] = sum / norm;
            } else {
                inputBuffer[pix] = outputBuffer[pix];
            }
            index++;
            if (index % progressStep == 0) {
                makeProgress(1);
                fireProgressStateChanged((int) getProgress());
            }
        }
    }

    private void convolveZ(final int from, final int to) {
        final int xDim = imgExtents[0];
        final int yDim = imgExtents[1];
        final int zDim = imgExtents[2];

        final int offset = xDim * cFactor;
        final int sliceSize = xDim * yDim * cFactor;
        int offsetX, offsetY, offsetZ;
        final int kDim = kernelBuffer[2].length;
        final int halfKDim = kDim / 2;
        int combined, start, end;
        double sum, norm;
        final int step = kDim * sliceSize;
        int index = 0;

        for (int pix = from; (pix < to) && !threadStopped; pix++) {
            if (colorImage && !red && ( (pix % 4) == 1)) {
                outputBuffer[pix] = inputBuffer[pix];
            } else if (colorImage && !green && ( (pix % 4) == 2)) {
                outputBuffer[pix] = inputBuffer[pix];
            } else if (colorImage && !blue && ( (pix % 4) == 3)) {
                outputBuffer[pix] = inputBuffer[pix];
            } else if (entireImage || mask.get(pix / cFactor)) {
                offsetX = (pix % offset);
                offsetY = (pix % sliceSize) / offset;
                offsetZ = (pix / sliceSize) - halfKDim;

                combined = (offsetY * offset) + offsetX;

                int count = 0;
                sum = 0;
                norm = 0;
                start = offsetZ * sliceSize;
                end = start + step;

                if (start < 0) {
                    count = -offsetZ;
                    start = 0;
                }

                if (end > (sliceSize * zDim)) {
                    end = sliceSize * zDim;
                }

                for (int i = start; i < end; i += sliceSize) {

                    // imgBuffer now holds the result of convolving with X and Y kernels
                    sum += kernelBuffer[2][count] * inputBuffer[i + combined];

                    if (kernelBuffer[2][count] >= 0) {
                        norm += kernelBuffer[2][count];
                    } else {
                        norm -= kernelBuffer[2][count];
                    }

                    count++;
                }

                outputBuffer[pix] = sum / norm;
            } else {
                outputBuffer[pix] = inputBuffer[pix];
            }
            index++;
            if (index % progressStep == 0) {
                makeProgress(1);
                fireProgressStateChanged((int) getProgress());
            }
        }
    }

    public void beforeExecute() {
        fireProgressStateChanged(minProgressValue);
    }

    public void execute() {
        if (multiThreadingEnabled) {
            performMT();
        } else {
            perform();
        }
    }

    public void afterExecute() {
        fireProgressStateChanged(maxProgressValue);
        setCompleted(true);
    }

    /**
     * Starts the convolution of the source image with the provided kernel.
     */
    public void runAlgorithm() {
        beforeExecute();
        execute();
        afterExecute();
    }

    /**
     * Sets what color channels to convolve and tells the convolver that it is working on a color image.
     * 
     * @param _red process the red channel
     * @param _green process the green channel
     * @param _blue process the blue channel
     */
    public void setColorChannels(final boolean _red, final boolean _green, final boolean _blue) {
        red = _red;
        green = _green;
        blue = _blue;
        colorImage = true;
    }

    /**
     * Sets the mask to convolve within.
     * 
     * @param newMask mask to convolve within
     */
    public void setMask(final BitSet newMask) {
        mask = newMask;
        entireImage = false;
        this.inputMask = new boolean[inputBuffer.length];
        for ( int i = 0; i < inputMask.length; i++ )
        {
            this.inputMask[i] = newMask.get(i);
        }
    }

    public float[] getOutputBuffer() {
        final float[] buffer = new float[outputBuffer.length];
        ArrayUtil.arrayCopy(outputBuffer, 0, buffer, 0, outputBuffer.length);
        return buffer;
    }
}
