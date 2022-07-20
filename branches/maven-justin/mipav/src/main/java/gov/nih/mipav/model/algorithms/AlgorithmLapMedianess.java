package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Calculates the Laplacian of the gaussian of an image at a scale defined by the user.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmLapMedianess extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** An amplification factor greater than 1.0 causes this filter to act like a highpass filter. */
    private float amplificationFactor = 1.0f;

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Storage location of the second derivative of the Gaussian in the X direction. */
    private float[] GxxData;

    /** Storage location of the second derivative of the Gaussian in the Y direction. */
    private float[] GyyData;

    /** Storage location of the second derivative of the Gaussian in the Z direction. */
    private float[] GzzData;

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** Buffer that stores the s value in calcInPlace2DBuffer that contributed to resultBuffer. */
    private byte[] sBuffer;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a Laplacian algorithm object.
     *
     * @param  srcImg     source image model
     * @param  sigmas     Gaussian's standard deviations in the each dimension
     * @param  maskFlag   Flag that indicates that the Laplacian will be calculated for the whole image if equal to true
     * @param  img25D     Flag, if true, indicates that each slice of the 3D volume should be processed independently.
     *                    2D images disregard this flag.
     * @param  ampFactor  An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
     */
    public AlgorithmLapMedianess(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D, float ampFactor) {
        this(srcImg, sigmas, maskFlag, img25D, ampFactor, true);
    }

    /**
     * Constructs a Laplacian algorithm object.
     *
     * @param  srcImg       source image model
     * @param  sigmas       Gaussian's standard deviations in the each dimension
     * @param  maskFlag     Flag that indicates that the Laplacian will be calculated for the whole image if equal to
     *                      true
     * @param  img25D       Flag, if true, indicates that each slice of the 3D volume should be processed independently.
     *                      2D images disregard this flag.
     * @param  ampFactor    An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
     * @param  runningInSeparateThread  whether the algorithm is running in a separate thread
     */
    public AlgorithmLapMedianess(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D, float ampFactor,
                                 boolean activeImage) {
        super(null, srcImg);

        destImage = null; // Calc in place
        srcImage = srcImg;
        this.sigmas = sigmas;
        entireImage = maskFlag;
        image25D = img25D;
        amplificationFactor = ampFactor;
        this.runningInSeparateThread = activeImage;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructs a Laplacian algorithm object.
     *
     * @param  destImg    image model where result image is to stored
     * @param  srcImg     source image model
     * @param  sigmas     Gaussian's standard deviations in the each dimension
     * @param  maskFlag   Flag that indicates that the Laplacian will be calculated for the whole image if equal to true
     * @param  img25D     Flag, if true, indicates that each slice of the 3D volume should be processed independently.
     *                    2D images disregard this flag.
     * @param  ampFactor  An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
     */
    public AlgorithmLapMedianess(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
                                 boolean img25D, float ampFactor) {
        this(destImg, srcImg, sigmas, maskFlag, img25D, ampFactor, true);
    }

    /**
     * Constructs a Laplacian algorithm object.
     *
     * @param  destImg      image model where result image is to stored
     * @param  srcImg       source image model
     * @param  sigmas       Gaussian's standard deviations in the each dimension
     * @param  maskFlag     Flag that indicates that the Laplacian will be calculated for the whole image if equal to
     *                      true
     * @param  img25D       Flag, if true, indicates that each slice of the 3D volume should be processed independently.
     *                      2D images disregard this flag.
     * @param  ampFactor    An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
     * @param  runningInSeparateThread  whether the algorithm is running in a separate thread
     */
    public AlgorithmLapMedianess(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
                                 boolean img25D, float ampFactor, boolean activeImage) {
        super(destImg, srcImg);

        destImage = destImg; // Put results in destination image.
        srcImage = srcImg;
        this.sigmas = sigmas;
        entireImage = maskFlag;
        image25D = img25D;
        amplificationFactor = ampFactor;
        this.runningInSeparateThread = activeImage;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the laplacian medianess of a 2D image and returns it as a float buffer.
     *
     * @param   buffer   DOCUMENT ME!
     * @param   extents  DOCUMENT ME!
     *
     * @return  Buffer with the laplacian medianess of the image.
     */
    public float[] calcInBuffer2D(float[] buffer, int[] extents) {
        return calcInPlace2DBuffer(1, buffer, extents);
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        GxxData = null;
        GyyData = null;
        GzzData = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  sBuffer
     */
    public byte[] getSBuffer() {
        return sBuffer;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (srcImage.getNDims() == 2) {
            makeKernels2D(sigmas);
        } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
            makeKernels3D(sigmas);
        } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
            makeKernels2D(sigmas);
        }

        if (threadStopped) {
            finalize();

            return;
        }

        

        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcStoreInDest3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcStoreInDest2D(srcImage.getExtents()[2]);
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcInPlace3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcInPlace2D(srcImage.getExtents()[2]);
            }
        }
    }

    /**
     * Calculates the gradient image and replaces the source image with the new image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace2D(int nImages) {
        float[] buffer = calcInPlace2DBuffer(nImages, null, srcImage.getExtents());

        if (threadStopped || (buffer == null)) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Laplacian importData: Image(s) locked", false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Calculates the Laplacian image and replaces the source image with the new image.
     *
     * @param   nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                   processed independently then nImages equals the number of images in the volume.
     * @param   buffer   DOCUMENT ME!
     * @param   extents  DOCUMENT ME!
     *
     * @return  resultBuffer
     */
    private float[] calcInPlace2DBuffer(int nImages, float[] buffer, int[] extents) {

        int i, s;
        int length;
        float[] resultBuffer;
        float lap;

        try {

            if (buffer == null) {
                length = srcImage.getSliceSize();
                buffer = new float[length];
            } else {
                length = buffer.length;
            }

            resultBuffer = new float[length * nImages];
            sBuffer = new byte[length * nImages];

            if (srcImage != null) {
                fireProgressStateChanged(srcImage.getImageName(), "Calculating the Laplacian ...");
            } else {
                fireProgressStateChanged("Medialness", "Calculating the Laplacian ...");
            }
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            sBuffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return null;
        }

        

        try {

            if (srcImage != null) {
                srcImage.exportData(0, length, buffer); // locks and releases lock
            }
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            System.gc();
            errorCleanUp("Algorithm Laplacian: " + error, false);

            return null;
        }

        float[] sigs = new float[2];

        for (s = 1; (s <= 8) && !threadStopped; s++) {
            sigs[0] = s;
            sigs[1] = s;
            makeKernels2D(sigs);

            fireProgressStateChanged(Math.round((float) (s) / 8 * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                lap = AlgorithmConvolver.convolve2DPtMed(i, extents, buffer, kExtents, GxxData);

                if (lap > resultBuffer[i]) {
                    resultBuffer[i] = lap;
                }

                sBuffer[i] = (byte) s;
            }
        }

        

        return resultBuffer;
    }

    /**
     * Calculates the Laplacian and replaces the source image with the new image.
     */
    private void calcInPlace3D() {

        int i;
        int length;
        float[] buffer;
        float[] resultBuffer;
        float lap;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Laplacian ...");
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return;
        }

        

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if ((entireImage == true) || mask.get(i)) {
                lap = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                resultBuffer[i] = lap;
            } else {
                resultBuffer[i] = buffer[i];
            }
        }

        buffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Laplacian importData: Image(s) locked", true);

            return;
        }

        setCompleted(true);
        
    }

    /**
     * Calculates the Laplacian image and replaces the source image with the new image.
     *
     * @param   buffer   DOCUMENT ME!
     * @param   extents  DOCUMENT ME!
     *
     * @return  resultBuffer
     */
    @SuppressWarnings("unused")
    private float[] calcInPlace3DBuffer(float[] buffer, int[] extents) {

        int i, s;
        int length;
        float[] resultBuffer;
        float lap;

        try {

            if (buffer == null) {
                length = srcImage.getSliceSize() * srcImage.getExtents()[2];
                buffer = new float[length];
            } else {
                length = buffer.length;
            }

            resultBuffer = new float[length];
            sBuffer = new byte[length];

            if (srcImage != null) {
                fireProgressStateChanged(srcImage.getImageName(), "Calculating the Laplacian ...");
            } else {
                fireProgressStateChanged("Medialness", "Calculating the Laplacian ...");
            }
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            sBuffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return null;
        }

        

        try {

            if (srcImage != null) {
                srcImage.exportData(0, length, buffer); // locks and releases lock
            }
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            System.gc();
            errorCleanUp("Algorithm Laplacian: " + error, false);

            return null;
        }

        float[] sigs = new float[3];

        for (s = 1; (s <= 8) && !threadStopped; s++) {
            sigs[0] = s;
            sigs[1] = s;
            sigs[2] = s;
            makeKernels3D(sigs);

            fireProgressStateChanged(Math.round((float) (s) / 8 * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                // if (entireImage == true || mask.get(i)) {

                lap = AlgorithmConvolver.convolve3DPtMed(i, extents, buffer, kExtents, GxxData);

                if (lap > resultBuffer[i]) {
                    resultBuffer[i] = lap;
                }

                sBuffer[i] = (byte) s;
                // }
                // else {
                // resultBuffer[i] = 0;
                // }
            }
        }

        

        return resultBuffer;
    }

    /**
     * This function produces the Laplacian of input image. See this Neva
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcStoreInDest2D(int nImages) {
        int i, s;
        int length;
        float[] buffer;
        float[] resultBuffer;
        float lap;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            resultBuffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Laplacian ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return;
        }

        

        try {
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm Gaussian Blur: Image(s) locked");
            setCompleted(false);
            
            destImage.releaseLock();

            return;
        }

        float[] sigs = new float[2];

        for (s = 1; (s <= 8) && !threadStopped; s++) {
            sigs[0] = s;
            sigs[1] = s;
            makeKernels2D(sigs);

            fireProgressStateChanged(Math.round((float) (s) / 8 * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {
                lap = AlgorithmConvolver.convolve2DPtMed(i, srcImage.getExtents(), buffer, kExtents, GxxData);

                if (lap > resultBuffer[i]) {
                    resultBuffer[i] = lap;
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);

            return;
        }

        
        setCompleted(true);
    }

    /**
     * This function produces the Laplacian of input image.
     */
    private void calcStoreInDest3D() {

        int i;
        int length;
        float[] buffer;
        float lap;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Laplacian: Image(s) locked");
            setCompleted(false);
            destImage.releaseLock();

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Laplacian ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return;
        }

        

        int mod = length / 100; // mod is 1 percent of length

        float min, max;

        min = Float.MAX_VALUE;
        max = -Float.MAX_VALUE;

        float minL, maxL;

        minL = Float.MAX_VALUE;
        maxL = -Float.MAX_VALUE;

        if (entireImage == false) {

            for (i = 0; i < length; i++) {

                if (mask.get(i)) {

                    if (buffer[i] > max) {
                        max = buffer[i];
                    } else if (buffer[i] < min) {
                        min = buffer[i];
                    }
                }
            }
        }

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if ((entireImage == true) || mask.get(i)) {
                lap = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);

                if (entireImage == false) {

                    if (mask.get(i)) {

                        if (lap > maxL) {
                            maxL = lap;
                        } else if (lap < minL) {
                            minL = lap;
                        }
                    }
                }

                destImage.set(i, lap);
            } else {
                destImage.set(i, buffer[i]);
            }
        }

        if (entireImage == false) {

            for (i = 0; i < length; i++) {

                if (mask.get(i)) {
                    lap = destImage.getFloat(i);
                    lap = (((lap - minL) / (maxL - minL)) * (max - min)) + min;
                    destImage.set(i, lap);
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        
        setCompleted(true);
    }

    /**
     * Creates Gaussian derivative kernels.
     *
     * @param  sigmas  DOCUMENT ME!
     */
    private void makeKernels2D(float[] sigmas) {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 2;
        derivOrder[1] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        GxxData = new float[xkDim * ykDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);

        Gxx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        GyyData = new float[xkDim * ykDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);

        Gyy.calc(false);

        float tmp;

        for (int i = 0; i < GyyData.length; i++) {
            tmp = -(GxxData[i] + GyyData[i]);

            if (tmp > 0) {
                tmp *= amplificationFactor;
            }

            GxxData[i] = tmp;
        }
    }

    /**
     * Creates Gaussian derivative kernels.
     *
     * @param  sigmas  DOCUMENT ME!
     */
    private void makeKernels3D(float[] sigmas) {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 2;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        float scaleFactor = sigmas[2];

        sigmas[2] = sigmas[1];
        zkDim = Math.round(8 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        if (zkDim < 3) {
            zkDim = 3;
        }

        kExtents[2] = zkDim;

        GxxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);

        Gxx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        derivOrder[2] = 0;
        GyyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);

        Gyy.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 2;
        GzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gzz = new GenerateGaussian(GzzData, kExtents, sigmas, derivOrder);

        Gzz.calc(false);

        float tmp;

        for (int i = 0; i < GyyData.length; i++) {
            tmp = -(GxxData[i] + GyyData[i] + (GzzData[i] * scaleFactor));

            if (tmp > 0) {
                tmp *= amplificationFactor;
            }

            GxxData[i] = tmp;
        }
    }

}
