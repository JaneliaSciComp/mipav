package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Calculates the Unsharp Mask of an image at a scale defined by the user ( unsharp image = original image - weight *
 * blurred image); weight < 1;
 *
 * @version  1.0 Feb 11, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmUnsharpMask extends AlgorithmBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean entireImage;

    /** DOCUMENT ME! */
    private float[] gaussData;

    /** DOCUMENT ME! */
    private int[] kExtents;

    /** DOCUMENT ME! */
    private float[] sigmas;

    /** DOCUMENT ME! */
    private float weightA = 0.75f;
    
    //  Buffer to receive result of convolution operation
    private float[] outputBuffer;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  weight    weighting factor, should be less than 1
     * @param  maskFlag  Flag that indicates that the unsharp mask will be calculated for the whole image if equal to
     *                   true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmUnsharpMask(ModelImage srcImg, float[] sigmas, float weight, boolean maskFlag, boolean img25D) {

        super(null, srcImg);
        this.sigmas = sigmas;
        entireImage = maskFlag;
        weightA = weight;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  weight    weighting factor, should be less than 1
     * @param  maskFlag  Flag that indicates that the unsharp mask will be calculated for the whole image if equal to
     *                   true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmUnsharpMask(ModelImage destImg, ModelImage srcImg, float[] sigmas, float weight, boolean maskFlag,
                                boolean img25D) {

        super(destImg, srcImg);
        this.sigmas = sigmas;
        weightA = weight;
        entireImage = maskFlag;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * prepares this class for destruction.
     */
    public void finalize() {

        gaussData = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }


    /**
     * starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (srcImage.getNDims() == 2) {
            makeKernels2D();
        } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
            makeKernels3D();
        } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
            makeKernels2D();
        }

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged(0, null, "Unsharp mask image ...");

        AlgorithmConvolver convolver = new AlgorithmConvolver(srcImage, gaussData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(0);
        convolver.setMaxProgressValue(80);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        convolver.run();

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
     * calculates the blurred image and replaces the source image with the blurred image.
     *
     * @param  nImages  number of images to be processed. If 2D image then nImage = 1, if 3D image where each image is
     *                  to processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace2D(int nImages) {

        int i, s;
        int length, totalLength;
        int start;
        float blur;
        float[] buffer;
        float[] resultBuffer;

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            resultBuffer = new float[totalLength];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm UnsharpMask: Out of memory", true);

            return;
        }

        int mod = totalLength / 20; // since progress bar is already at 80
        

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm UnsharpMask: Image(s) locked", true);

                return;
            }

            for (i = 0; (i < length) && !threadStopped; i++) {

                if ((((start + i) % mod) == 0)) {
                    fireProgressStateChanged(80 + (20 * (start + i)) / (totalLength - 1));
                }

                if ((entireImage == true) || mask.get(i)) {
                    blur = outputBuffer[start + i];
                    resultBuffer[start + i] = buffer[i] - (weightA * blur);
                } else {
                    resultBuffer[start + i] = buffer[i];
                    // resultBuffer[i] = 0;
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
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm UnsharpMask: Image(s) locked", true);

            return;
        }

        
        setCompleted(true);
    }


    /**
     * calculates the UnsharpMask and replaces the source image with the new image.
     */
    private void calcInPlace3D() {

        int i;
        int length;
        float[] buffer;
        float[] resultBuffer;
        float blur;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Out of memory", true);

            return;
        }

        

        int mod = length / 20; // since progress bar is already at 80

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(80 + (20 * i) / (length - 1));
            }

            if ((entireImage == true) || mask.get(i)) {
                blur = outputBuffer[i];
                resultBuffer[i] = buffer[i] - (weightA * blur);

            } else {
                resultBuffer[i] = buffer[i];
                // resultBuffer[i] = 0;
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
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm UnsharpMask importData: Image(s) locked", true);

            return;
        }

        setCompleted(true);
        
    }

    /**
     * this function produces a new image that has been blurred.
     *
     * @param  nImages  number of images to be processed. If 2D image then nImage = 1, if 3D image where each image is
     *                  to processed independently then nImages equals the number of images in the volume.
     */
    private void calcStoreInDest2D(int nImages) {

        int i, s, idx;
        int length, totalLength;
        int start;
        float blur;
        float[] buffer;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp(" Unsharp Mask: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Unsharp Mask:  Out of memory", true);

            return;
        }

        int mod = totalLength / 20; // since progress bar is already at 80
        

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Unsharp Mask: Image(s) locked", true);

                return;
            }

            for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                if ((((start + i) % mod) == 0)) {
                    fireProgressStateChanged(80 + (20 * (start + i))/ (totalLength - 1));
                }

                if ((entireImage == true) || mask.get(i)) {
                    blur = outputBuffer[start + i];

                    destImage.set(idx, buffer[i] - (weightA * blur));
                } else {
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

        
        setCompleted(true);
    }


    /**
     * this function produces the UnsharpMask of input image.
     */
    private void calcStoreInDest3D() {

        int i;
        int length;
        float[] buffer;
        float blur;

        try {
            destImage.setLock();
        } catch (IOException error) {
            errorCleanUp("Algorithm UnsharpMask: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm UnsharpMask exportData: Out of memory", true);

            return;
        }

        

        int mod = length / 20; // since progress bar is already at 80

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(80 + (20 * i) / (length - 1));
            }

            if ((entireImage == true) || mask.get(i)) {
                blur = outputBuffer[i];
                destImage.set(i, buffer[i] - (weightA * blur));
            } else {
                destImage.set(i, buffer[i]);
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
     * creates Gaussian kernels for the blurring process.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 0;
        derivOrder[1] = 0;

        xkDim = Math.round(3 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(3 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        gaussData = new float[xkDim * ykDim];

        GenerateGaussian Gauss = new GenerateGaussian(gaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
    }

    /**
     * creates Gaussian kernels for the blurring process.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(3 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(3 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(3 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        kExtents[2] = zkDim;

        gaussData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gauss = new GenerateGaussian(gaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmConvolver) {
            AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            outputBuffer = convolver.getOutputBuffer();
        }
    }

}
