package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;


/**
 * Algorithm to apply Regularized Isotropic Nonlinear Diffusion as described by:
 *
 * <P>Joachim Weickert <I>Chapter 15</I> in Handbook of Computer Vision and Applications, Volume 2 Signal Processing and
 * Pattern Recognition. The book is published by Academic Press, ISBN 0-12-379772-1</P>
 *
 * <P>Here is a listing of the author's terminology used in the title.</P>
 *
 * <UL>
 *   <LI><B>Regularized</B> the computed image is blurred with a Gaussian kernel prior to taking finite derivatives</LI>
 *   <LI> <B>Isotropic</B> the gradient and flux are parallel</LI>
 *   <LI> <B>Nonlinear</B> the weights used for combining pixels change over the image, space variant.</LI>
 *   <LI><B>Diffusion</B> this model uses the heat equation</LI>
 * </UL>
 *
 * @version  1.0; 31, July 2003
 * @author   Paul F. Hemler, Ph.D.
 */
public class AlgorithmRegularizedIsotropicDiffusion extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The number of iterations. */
    int numIterations = 0;

    /**
     * The time parameter step size, called tau in the algorithm description In 2D for stability the time step should be
     * <= 0.25. In 3D for stability the time step should be <= 1/6.
     */
    float timeStep = 0.25f;


    /** Handle to the seperable convolution kernel. */
    private AlgorithmSeparableConvolver algoSepConvolver;

    /** Handle to the seperable convolution kernel. */
    private AlgorithmSeparableConvolver algoSepConvolverB;

    /** Handle to the seperable convolution kernel. */
    private AlgorithmSeparableConvolver algoSepConvolverG;

    /** Handle to the seperable convolution kernel. */
    private AlgorithmSeparableConvolver algoSepConvolverR;

    /** Diffusion contrast parameter. */
    private float contrast = 0.15f;

    /** Flag indicating 2.5D processing. */
    private boolean do25D = true;

    /** Contrast multiplied by the maximum gradient. */
    private float lambda; //

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas = null;

    /** Standard deviation used for the Gaussians. */
    private float stdDev = 1.0f;

    /** Storage location for the 1D gaussian kernels. */
    private float[] xDataRound = null;

    /** Image dimensions. */
    private int xDim, yDim, zDim;

    /** Storage location for the 1D gaussian kernels. */
    private float[] yDataRound = null;

    /** Storage location for the 1D gaussian kernels. */
    private float[] zDataRound = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmRegularizedIsotropicDiffusion object.
     *
     * @param  destImg   ModelImage image model where result image is to stored
     * @param  srcImg    ModelImage source image model
     * @param  numI      int number of iterations
     * @param  stdDev    float standard deviation used in the Gaussians
     * @param  contrast  float diffusion contrast parameter
     * @param  do25D     boolean If true, process each slice separately
     */
    public AlgorithmRegularizedIsotropicDiffusion(ModelImage destImg, ModelImage srcImg, int numI, float stdDev,
                                                  float contrast, boolean do25D) {
        super(destImg, srcImg);
        numIterations = numI;
        this.stdDev = stdDev;
        this.contrast = contrast;
        this.do25D = do25D;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        sigmas = null;
        xDataRound = null;
        yDataRound = null;
        zDataRound = null;
        super.finalize();
    } // end finalize()

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            MipavUtil.displayError("AlgorithmRegularizedIsotropicDiffusion.run()  Source Image is null");

            return;
        }

        if ((srcImage.getNDims() == 2) || do25D) {
            timeStep = 0.2f;
        } else {
            timeStep = 0.15f;
        }

        if (srcImage.isColorImage()) {

            if (srcImage.getNDims() == 2) {
                run2DC(1);
            } else if ((srcImage.getNDims() == 3) && (do25D == true)) {
                run2DC(srcImage.getExtents()[2]);
            } else {
                run3DC();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                run2D(1);
            } else if ((srcImage.getNDims() == 3) && (do25D == true)) {
                run2D(srcImage.getExtents()[2]);
            } else {
                run3D();
            }
        }
    } // end run()

    /**
     * Returns the weighting for values used in computing new (diffused) pixel values.
     *
     * @param   d  float Value to compute the diffusivitivy
     *
     * @return  float The value of the diffusitivy corresponding to the d value
     */
    private float diffusitivity(float d) {
        return (1.0f / (1.0f + ((d * d) / (lambda * lambda))));
    } // end diffusitivity(...)

    /**
     * Returns the value in a 2D image buffer of the pixel at location row, col.
     *
     * @param   buffer  float[] The image buffer
     * @param   col     int Column index
     * @param   row     int Row index
     *
     * @return  float The pixel value
     */
    private float getVal(float[] buffer, int col, int row) {
        float rtnVal;

        if (col < 0) {
            col = 0;
        }

        if (col >= xDim) {
            col = xDim - 1;
        }

        if (row < 0) {
            row = 0;
        }

        if (row >= yDim) {
            row = yDim - 1;
        }

        rtnVal = buffer[(row * xDim) + col];

        return rtnVal;
    } // end getVal(...)

    /**
     * Returns the value in a 3D image buffer of the pixel at location slice, row, col.
     *
     * @param   buffer  float[] The image buffer
     * @param   col     int Column index
     * @param   row     int Row index
     * @param   slice   int Slice index
     *
     * @return  float The pixel value
     */
    private float getVal(float[] buffer, int col, int row, int slice) {
        float rtnVal;

        if (col < 0) {
            col = 0;
        }

        if (col >= xDim) {
            col = xDim - 1;
        }

        if (row < 0) {
            row = 0;
        }

        if (row >= yDim) {
            row = yDim - 1;
        }

        if (slice < 0) {
            slice = 0;
        }

        if (slice >= zDim) {
            slice = zDim - 1;
        }

        rtnVal = buffer[(slice * yDim * xDim) + (row * xDim) + col];

        return rtnVal;
    } // end getVal(...)

    /**
     * Computes the gradient magnitude of the buffer passed in. This version computes values for the image borders and
     * corners
     *
     * @param  img  float [] source buffer
     * @param  mag  float [] a buffer where the gradient magnitude will be placed
     */
    private void gradientMagnitude(float[] img, float[] mag) {
        int row, col, cIndex;
        float lVal, rVal, tVal, bVal;

        for (row = 0; row < yDim; row++) {
            cIndex = row * xDim;

            for (col = 0, cIndex += col; col < xDim; cIndex++, col++) {

                rVal = getVal(img, col + 1, row);
                lVal = getVal(img, col - 1, row);
                tVal = getVal(img, col, row + 1);
                bVal = getVal(img, col, row - 1);

                mag[cIndex] = (float) Math.sqrt(((rVal - lVal) * (rVal - lVal)) + ((tVal - bVal) * (tVal - bVal)));
            } // for (col = 0; ...)
        } // for (row = 0; ...)
    } // end gradientMagnitude(...)

    /**
     * Computes the gradient magnitude of the buffer passed in. This version computes values for the image borders and
     * corners
     *
     * @param  img  float [] source buffer
     * @param  mag  float [] a buffer where the gradient magnitude will be placed
     */
    private void gradientMagnitude3D(float[] img, float[] mag) {
        int slice, row, col, cIndex, sIndex;
        float lVal, rVal, tVal, bVal, zLowVal, zHighVal;
        int sliceSize = yDim * xDim;

        for (slice = 0; slice < zDim; slice++) {
            sIndex = slice * sliceSize;

            for (row = 0; row < yDim; row++) {
                cIndex = sIndex + (row * xDim);

                for (col = 0, cIndex += col; col < xDim; cIndex++, col++) {

                    rVal = getVal(img, col + 1, row, slice);
                    lVal = getVal(img, col - 1, row, slice);
                    tVal = getVal(img, col, row + 1, slice);
                    bVal = getVal(img, col, row - 1, slice);
                    zLowVal = getVal(img, col, row, slice - 1);
                    zHighVal = getVal(img, col, row, slice + 1);

                    mag[cIndex] = (float) Math.sqrt(((rVal - lVal) * (rVal - lVal)) + ((tVal - bVal) * (tVal - bVal)) +
                                                    ((zHighVal - zLowVal) * (zHighVal - zLowVal)));
                } // for (col = 0; ...)
            } // for (row = 0; ...)
        } // for (slice = 0; ...)
    } // end gradientMagnitude3D(...)

    /**
     * DOCUMENT ME!
     *
     * @param  do3D  boolean Flag indicating if a 3D kernel should be computed
     */
    private void makeKernels1D(boolean do3D) {

        // make 1D rounding kernels with no derivatives
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[1];
        int[] kExtents = new int[1];

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[0] = xkDim;

        float[] sigmasX = new float[1];
        sigmasX[0] = sigmas[0];
        derivOrder[0] = 0;
        xDataRound = new float[xkDim];

        GenerateGaussian Gx = new GenerateGaussian(xDataRound, kExtents, sigmasX, derivOrder);
        Gx.calc(false);

        kExtents[0] = ykDim;

        float[] sigmasY = new float[1];
        sigmasY[0] = sigmas[1];
        derivOrder[0] = 0;
        yDataRound = new float[ykDim];

        GenerateGaussian Gy = new GenerateGaussian(yDataRound, kExtents, sigmasY, derivOrder);
        Gy.calc(true);

        if (do3D) {
            zkDim = Math.round(5 * sigmas[2]);

            if ((zkDim % 2) == 0) {
                zkDim++;
            }

            if (zkDim < 3) {
                zkDim = 3;
            }

            kExtents[0] = zkDim;

            float[] sigmasZ = new float[1];
            sigmasZ[0] = sigmas[2];
            derivOrder[0] = 0;
            zDataRound = new float[zkDim];

            GenerateGaussian Gz = new GenerateGaussian(zDataRound, kExtents, sigmasZ, derivOrder);
            Gz.calc(true);
        }

    }


    /**
     * Iterates the Regularized Isotropic Nonlinear Diffusion algorithm for 2D and 2.5D images.
     *
     * @param  numImages  int number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image
     *                    is to processed independently then nImages equals the number of slices in the volume.
     */
    private void run2D(int numImages) {
        fireProgressStateChanged(0, srcImage.getImageName(), "Regularized Isotropic Diffusion ...");

        int computationCount = 0;

        // OK, here is where the meat of the algorithm goes

        int length;
        int[] extents = new int[2];
        extents[0] = srcImage.getExtents()[0];
        extents[1] = srcImage.getExtents()[1];
        xDim = extents[0];
        yDim = extents[1];
        length = xDim * yDim;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;
        float[] gaussianBuffer;
        float[] gradientBuffer;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            resultBuffer = new float[length];
            gradientBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            sourceBuffer = resultBuffer = gaussianBuffer = gradientBuffer = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Out of memory when creating image buffer", true);

            return;
        } // catch{}

        // Gaussian blur the input image as a 2.5D image set
        sigmas = new float[2];
        sigmas[0] = sigmas[1] = stdDev;

        makeKernels1D(false);

        // source image is in sourceBuffer, gaussian smoothed image is in gaussianBuffer


        int startIndex;

        float stepProgressValue = ((float)100)/(numImages * numIterations);
        
        for (int imgNumber = 0; imgNumber < numImages; imgNumber++) {
            startIndex = imgNumber * length;

            try {
                srcImage.exportData(startIndex, length, sourceBuffer);
            } catch (IOException error) {
                sourceBuffer = resultBuffer = gaussianBuffer = gradientBuffer = null;
                errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: could NOT export source image", true);

                return;
            } // catch()

            // make the magnitude of the gradient image of the gaussian smoothed source image
            algoSepConvolver = new AlgorithmSeparableConvolver(sourceBuffer, extents, new float[][]{xDataRound,
                                                               yDataRound}, srcImage.isColorImage());

            for (int iterNum = 0; iterNum < numIterations; iterNum++) {
                fireProgressStateChanged(0 + Math.round(stepProgressValue * computationCount));
                algoSepConvolver.run();
                gaussianBuffer = algoSepConvolver.getOutputBuffer();
                gradientMagnitude(gaussianBuffer, gradientBuffer);
                upDateImage(resultBuffer, sourceBuffer, gradientBuffer);

                // copy resultBuffer to sourceBuffer for the next iteration
                if (iterNum < (numIterations - 1)) {

                    for (int i = 0; i < length; i++) {
                        sourceBuffer[i] = resultBuffer[i];
                    }
                }

                computationCount++;
                
            } // end for (int iterNum = 0; ...)

            algoSepConvolver.finalize();
            algoSepConvolver = null;

            // OK, the resultBuffer is filled with the results of the algorithm,
            // put this data into the destination image so it will be displayed in
            // in the ViewJFrameWizard
            try {
                destImage.importData(startIndex, resultBuffer, true);
            } catch (IOException error) {
                sourceBuffer = resultBuffer = gaussianBuffer = gradientBuffer = null;
                errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Could NOT import resultBuffer to the image",
                             true);

                return;
            } // end try{}-catch{}

        } // end for (imgNumber = 0; ...)

        fireProgressStateChanged(100);
        

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    } // end run2D

    /**
     * Iterates the Regularized Isotropic Nonlinear Diffusion algorithm for 2D and 2.5D color images.
     *
     * @param  numImages  int number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image
     *                    is to processed independently then nImages equals the number of slices in the volume.
     */
    private void run2DC(int numImages) {
        fireProgressStateChanged(0, srcImage.getImageName(), "Regularized Isotropic Diffusion ...");

        int totalComputation = numImages * numIterations;
        int computationCount = 0;

        // OK, here is where the meat of the algorithm goes

        int length;
        int i;
        int[] extents = new int[2];
        extents[0] = srcImage.getExtents()[0];
        extents[1] = srcImage.getExtents()[1];
        xDim = extents[0];
        yDim = extents[1];
        length = xDim * yDim;

        // buffers for the image data
        float[] sourceBufferR = null;
        float[] sourceBufferG = null;
        float[] sourceBufferB = null;
        float[] resultBufferR = null;
        float[] resultBufferG = null;
        float[] resultBufferB = null;
        float[] gaussianBufferR = null;
        float[] gaussianBufferG = null;
        float[] gaussianBufferB = null;
        float[] gradientBuffer;
        float[] gradientBufferR = null;
        float[] gradientBufferG = null;
        float[] gradientBufferB = null;

        boolean useRed = true;
        boolean useGreen = true;
        boolean useBlue = true;
        int colorsPresent = 3;
        srcImage.calcMinMax();

        if (srcImage.getMinR() == srcImage.getMaxR()) {
            useRed = false;
            colorsPresent--;
        }

        if (srcImage.getMinG() == srcImage.getMaxG()) {
            useGreen = false;
            colorsPresent--;
        }

        if (srcImage.getMinB() == srcImage.getMaxB()) {
            useBlue = false;
            colorsPresent--;
        }

        // copy the image data into the sourceBuffer so we can access it
        try {

            if (useRed) {
                sourceBufferR = new float[length];
                resultBufferR = new float[length];
                gaussianBufferR = new float[length];
                gradientBufferR = new float[length];
            }

            if (useGreen) {
                sourceBufferG = new float[length];
                resultBufferG = new float[length];
                gaussianBufferG = new float[length];
                gradientBufferG = new float[length];
            }

            if (useBlue) {
                sourceBufferB = new float[length];
                resultBufferB = new float[length];
                gaussianBufferB = new float[length];
                gradientBufferB = new float[length];
            }

            gradientBuffer = new float[length];

            for (i = 0; i < length; i++) {
                gradientBuffer[i] = 0.0f;
            }
        } catch (OutOfMemoryError e) {
            sourceBufferR = sourceBufferG = sourceBufferB = null;
            resultBufferR = resultBufferG = resultBufferB = null;
            gaussianBufferR = gaussianBufferG = gaussianBufferB = null;
            gradientBuffer = gradientBufferR = gradientBufferG = gradientBufferB = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Out of memory when creating image buffer", true);

            return;
        } // catch{}

        // Gaussian blur the input image as a 2.5D image set
        sigmas = new float[2];
        sigmas[0] = sigmas[1] = stdDev;

        makeKernels1D(false);

        // source image is in sourceBuffer, gaussian smoothed image is in gaussianBuffer


        int startIndex;

        float stepProgressValue = ((float)100)/(numImages * numIterations);

        for (int imgNumber = 0; imgNumber < numImages; imgNumber++) {
            startIndex = 4 * imgNumber * length;

            try {

                if (useRed) {
                    srcImage.exportRGBData(1, startIndex, length, sourceBufferR);
                }

                if (useGreen) {
                    srcImage.exportRGBData(2, startIndex, length, sourceBufferG);
                }

                if (useBlue) {
                    srcImage.exportRGBData(3, startIndex, length, sourceBufferB);
                }
            } catch (IOException error) {
                sourceBufferR = sourceBufferG = sourceBufferB = null;
                resultBufferR = resultBufferG = resultBufferB = null;
                gaussianBufferR = gaussianBufferG = gaussianBufferB = null;
                gradientBuffer = gradientBufferR = gradientBufferG = gradientBufferB = null;
                errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: could NOT export source image", true);

                return;
            } // catch()

            // make the magnitude of the gradient image of the gaussian smoothed source image
            if (useRed) {
                algoSepConvolverR = new AlgorithmSeparableConvolver(sourceBufferR, extents, new float[][]{xDataRound,
                                                                    yDataRound}, srcImage.isColorImage());
            }

            if (useGreen) {
                algoSepConvolverG = new AlgorithmSeparableConvolver(sourceBufferG, extents, new float[][]{xDataRound,
                                                                    yDataRound}, srcImage.isColorImage());
            }

            if (useBlue) {
                algoSepConvolverB = new AlgorithmSeparableConvolver(sourceBufferB, extents, new float[][]{xDataRound,
                                                                    yDataRound}, srcImage.isColorImage());
            }

            for (int iterNum = 0; iterNum < numIterations; iterNum++) {
                fireProgressStateChanged(0 + Math.round(stepProgressValue * computationCount));
                if (useRed) {
                    algoSepConvolverR.run();
                    gaussianBufferR = algoSepConvolverR.getOutputBuffer();
                    gradientMagnitude(gaussianBufferR, gradientBufferR);

                    for (i = 0; i < length; i++) {
                        gradientBuffer[i] = gradientBufferR[i];
                    }
                }

                if (useGreen) {
                    algoSepConvolverG.run();
                    gaussianBufferG = algoSepConvolverG.getOutputBuffer();
                    gradientMagnitude(gaussianBufferG, gradientBufferG);

                    for (i = 0; i < length; i++) {
                        gradientBuffer[i] += gradientBufferG[i];
                    }
                }

                if (useBlue) {
                    algoSepConvolverB.run();
                    gaussianBufferB = algoSepConvolverB.getOutputBuffer();
                    gradientMagnitude(gaussianBufferB, gradientBufferB);

                    for (i = 0; i < length; i++) {
                        gradientBuffer[i] += gradientBufferB[i];
                    }
                }

                for (i = 0; i < length; i++) {
                    gradientBuffer[i] /= colorsPresent;
                }

                if (useRed) {
                    upDateImage(resultBufferR, sourceBufferR, gradientBuffer);
                }

                if (useGreen) {
                    upDateImage(resultBufferG, sourceBufferG, gradientBuffer);
                }

                if (useBlue) {
                    upDateImage(resultBufferB, sourceBufferB, gradientBuffer);
                }

                // copy resultBuffer to sourceBuffer for the next iteration
                if (iterNum < (numIterations - 1)) {

                    if (useRed) {

                        for (i = 0; i < length; i++) {
                            sourceBufferR[i] = resultBufferR[i];
                        }
                    } // if (useRed)

                    if (useGreen) {

                        for (i = 0; i < length; i++) {
                            sourceBufferG[i] = resultBufferG[i];
                        }
                    } // if (useGreen)

                    if (useBlue) {

                        for (i = 0; i < length; i++) {
                            sourceBufferB[i] = resultBufferB[i];
                        }
                    } // if (useBlue)
                } // if (iterNum < (numIterations - 1))

                computationCount++;
            } // end for (int iterNum = 0; ...)

            if (useRed) {
                algoSepConvolverR.finalize();
                algoSepConvolverR = null;
            }

            if (useGreen) {
                algoSepConvolverG.finalize();
                algoSepConvolverG = null;
            }

            if (useBlue) {
                algoSepConvolverB.finalize();
                algoSepConvolverB = null;
            }

            // OK, the resultBuffer is filled with the results of the algorithm,
            // put this data into the destination image so it will be displayed in
            // in the ViewJFrameWizard
            try {

                if (useRed) {

                    for (i = 0; i < length; i++) {

                        if ((resultBufferR[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                            resultBufferR[i] = 255.0f;
                        } else if (resultBufferR[i] < 0.0f) {
                            resultBufferR[i] = 0.0f;
                        }
                    }

                    destImage.importRGBData(1, startIndex, resultBufferR, false);
                }

                if (useGreen) {

                    for (i = 0; i < length; i++) {

                        if ((resultBufferG[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                            resultBufferG[i] = 255.0f;
                        } else if (resultBufferG[i] < 0.0f) {
                            resultBufferG[i] = 0.0f;
                        }
                    }

                    destImage.importRGBData(2, startIndex, resultBufferG, false);
                }

                if (useBlue) {

                    for (i = 0; i < length; i++) {

                        if ((resultBufferB[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                            resultBufferB[i] = 255.0f;
                        } else if (resultBufferB[i] < 0.0f) {
                            resultBufferB[i] = 0.0f;
                        }
                    }

                    destImage.importRGBData(3, startIndex, resultBufferB, false);
                }
            } catch (IOException error) {
                sourceBufferR = sourceBufferG = sourceBufferB = null;
                resultBufferR = resultBufferG = resultBufferB = null;
                gaussianBufferR = gaussianBufferG = gaussianBufferB = null;
                gradientBuffer = gradientBufferR = gradientBufferG = gradientBufferB = null;
                errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Could NOT import resultBuffer to the image",
                             true);

                return;
            } // end try{}-catch{}

        } // end for (imgNumber = 0; ...)
        
        fireProgressStateChanged(100);
        
        destImage.calcMinMax();


        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    } // end run2DC


    /**
     * Iterates the Regularized Isotropic Nonlinear Diffusion algorithm for 3D images.
     */
    private void run3D() {
        fireProgressStateChanged(0, srcImage.getImageName(), "Regularized Isotropic Diffusion ...");

        // OK, here is where the meat of the algorithm goes

        int length;
        int[] extents = srcImage.getExtents();
        xDim = extents[0];
        yDim = extents[1];
        zDim = extents[2];
        length = xDim * yDim * zDim;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;
        float[] gaussianBuffer;
        float[] gradientBuffer;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            resultBuffer = new float[length];
            gaussianBuffer = new float[length];
            gradientBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            sourceBuffer = resultBuffer = gaussianBuffer = gradientBuffer = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Out of memory when creating image buffer", true);

            return;
        } // catch{}

        // Gaussian blur the input image as a 3D image
        sigmas = new float[3];
        sigmas[0] = sigmas[1] = sigmas[2] = stdDev;

        makeKernels1D(true);

        // source image is in sourceBuffer, gaussian smoothed image is in gaussianBuffer


        try {
            srcImage.exportData(0, length, sourceBuffer);
        } catch (IOException error) {
            sourceBuffer = resultBuffer = gaussianBuffer = gradientBuffer = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: could NOT export source image", true);

            return;
        } // catch()

        float stepProgressValue = ((float)100)/numIterations;

        
        // make the magnitude of the gradient image of the gaussian smoothed
        // source image
        algoSepConvolver = new AlgorithmSeparableConvolver(sourceBuffer, extents, new float[][]{xDataRound, 
        		yDataRound, zDataRound}, srcImage.isColorImage());
        linkProgressToAlgorithm(algoSepConvolver);

        for (int iterNum = 0; iterNum < numIterations; iterNum++) {
        	algoSepConvolver.setProgressValues(generateProgressValues(getMinProgressValue() + Math.round(stepProgressValue * iterNum),
        			getMinProgressValue() + Math.round(stepProgressValue * (iterNum+1))));
            algoSepConvolver.run();
            gaussianBuffer = algoSepConvolver.getOutputBuffer();

            gradientMagnitude3D(gaussianBuffer, gradientBuffer);
            upDateImage3D(resultBuffer, sourceBuffer, gradientBuffer);

            // copy resultBuffer to sourceBuffer for the next iteration
            if (iterNum < (numIterations - 1)) {

                for (int i = 0; i < length; i++) {
                    sourceBuffer[i] = resultBuffer[i];
                }
            }
        } // end for (int iterNum = 0; ...)

        fireProgressStateChanged(100);
        
        algoSepConvolver.finalize();
        algoSepConvolver = null;

        // OK, the resultBuffer is filled with the results of the algorithm,
        // put this data into the destination image so it will be displayed in
        // in the ViewJFrameWizard
        try {
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            sourceBuffer = resultBuffer = gaussianBuffer = gradientBuffer = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Could NOT import resultBuffer to the image", true);

            return;
        } // end try{}-catch{}

        
        if (threadStopped) {
            finalize();

            return;
        }
        
        setCompleted(true);
    } // end run3D

    /**
     * Iterates the Regularized Isotropic Nonlinear Diffusion algorithm for 3D color images.
     */
    private void run3DC() {
        fireProgressStateChanged(0, srcImage.getImageName(), "Regularized Isotropic Diffusion ...");

        // OK, here is where the meat of the algorithm goes

        int length;
        int i;
        int[] extents = srcImage.getExtents();
        xDim = extents[0];
        yDim = extents[1];
        zDim = extents[2];
        length = xDim * yDim * zDim;

        // buffers for the image data
        float[] sourceBufferR = null;
        float[] sourceBufferG = null;
        float[] sourceBufferB = null;
        float[] resultBufferR = null;
        float[] resultBufferG = null;
        float[] resultBufferB = null;
        float[] gaussianBufferR = null;
        float[] gaussianBufferG = null;
        float[] gaussianBufferB = null;
        float[] gradientBuffer;
        float[] gradientBufferR = null;
        float[] gradientBufferG = null;
        float[] gradientBufferB = null;

        boolean useRed = true;
        boolean useGreen = true;
        boolean useBlue = true;
        int colorsPresent = 3;
        srcImage.calcMinMax();

        if (srcImage.getMinR() == srcImage.getMaxR()) {
            useRed = false;
            colorsPresent--;
        }

        if (srcImage.getMinG() == srcImage.getMaxG()) {
            useGreen = false;
            colorsPresent--;
        }

        if (srcImage.getMinB() == srcImage.getMaxB()) {
            useBlue = false;
            colorsPresent--;
        }

        // copy the image data into the sourceBuffer so we can access it
        try {

            if (useRed) {
                sourceBufferR = new float[length];
                resultBufferR = new float[length];
                gaussianBufferR = new float[length];
                gradientBufferR = new float[length];
            }

            if (useGreen) {
                sourceBufferG = new float[length];
                resultBufferG = new float[length];
                gaussianBufferG = new float[length];
                gradientBufferG = new float[length];
            }

            if (useBlue) {
                sourceBufferB = new float[length];
                resultBufferB = new float[length];
                gaussianBufferB = new float[length];
                gradientBufferB = new float[length];
            }

            gradientBuffer = new float[length];

            for (i = 0; i < length; i++) {
                gradientBuffer[i] = 0.0f;
            }
        } catch (OutOfMemoryError e) {
            sourceBufferR = sourceBufferG = sourceBufferB = null;
            resultBufferR = resultBufferG = resultBufferB = null;
            gaussianBufferR = gaussianBufferG = gaussianBufferB = null;
            gradientBuffer = gradientBufferR = gradientBufferG = gradientBufferB = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Out of memory when creating image buffer", true);

            return;
        } // catch{}

        // Gaussian blur the input image as a 3D image
        sigmas = new float[3];
        sigmas[0] = sigmas[1] = sigmas[2] = stdDev;

        makeKernels1D(true);

        // source image is in sourceBuffer, gaussian smoothed image is in gaussianBuffer


        try {

            if (useRed) {
                srcImage.exportRGBData(1, 0, length, sourceBufferR);
            }

            if (useGreen) {
                srcImage.exportRGBData(2, 0, length, sourceBufferG);
            }

            if (useBlue) {
                srcImage.exportRGBData(3, 0, length, sourceBufferB);
            }
        } catch (IOException error) {
            sourceBufferR = sourceBufferG = sourceBufferB = null;
            resultBufferR = resultBufferG = resultBufferB = null;
            gaussianBufferR = gaussianBufferG = gaussianBufferB = null;
            gradientBuffer = gradientBufferR = gradientBufferG = gradientBufferB = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: could NOT export source image", true);

            return;
        } // catch()

        float stepProgressValue = ((float)100)/numIterations;
        // make the magnitude of the gradient image of the gaussian smoothed source image
        if (useRed) {
            algoSepConvolverR = new AlgorithmSeparableConvolver(sourceBufferR, extents, new float[][]{xDataRound,
                                                                yDataRound, zDataRound}, srcImage.isColorImage());
        }

        if (useGreen) {
            algoSepConvolverG = new AlgorithmSeparableConvolver(sourceBufferG, extents, new float[][]{xDataRound,
                                                                yDataRound, zDataRound}, srcImage.isColorImage());
        }

        if (useBlue) {
            algoSepConvolverB = new AlgorithmSeparableConvolver(sourceBufferB, extents, new float[][]{xDataRound,
                                                                yDataRound, zDataRound}, srcImage.isColorImage());
        }

        for (int iterNum = 0; iterNum < numIterations; iterNum++) {
            fireProgressStateChanged(0 + Math.round(stepProgressValue * iterNum));
            
            if (useRed) {
                algoSepConvolverR.run();
                gaussianBufferR = algoSepConvolverR.getOutputBuffer();
                gradientMagnitude3D(gaussianBufferR, gradientBufferR);

                for (i = 0; i < length; i++) {
                    gradientBuffer[i] = gradientBufferR[i];
                }
            }

            if (useGreen) {
                algoSepConvolverG.run();
                gaussianBufferG = algoSepConvolverG.getOutputBuffer();
                gradientMagnitude3D(gaussianBufferG, gradientBufferG);

                for (i = 0; i < length; i++) {
                    gradientBuffer[i] += gradientBufferG[i];
                }
            }

            if (useBlue) {
                algoSepConvolverB.run();
                gaussianBufferB = algoSepConvolverB.getOutputBuffer();
                gradientMagnitude3D(gaussianBufferB, gradientBufferB);

                for (i = 0; i < length; i++) {
                    gradientBuffer[i] += gradientBufferB[i];
                }
            }

            for (i = 0; i < length; i++) {
                gradientBuffer[i] /= colorsPresent;
            }

            if (useRed) {
                upDateImage3D(resultBufferR, sourceBufferR, gradientBuffer);
            }

            if (useGreen) {
                upDateImage3D(resultBufferG, sourceBufferG, gradientBuffer);
            }

            if (useBlue) {
                upDateImage3D(resultBufferB, sourceBufferB, gradientBuffer);
            }

            // copy resultBuffer to sourceBuffer for the next iteration
            if (iterNum < (numIterations - 1)) {

                if (useRed) {

                    for (i = 0; i < length; i++) {
                        sourceBufferR[i] = resultBufferR[i];
                    }
                } // if (useRed)

                if (useGreen) {

                    for (i = 0; i < length; i++) {
                        sourceBufferG[i] = resultBufferG[i];
                    }
                } // if (useGreen)

                if (useBlue) {

                    for (i = 0; i < length; i++) {
                        sourceBufferB[i] = resultBufferB[i];
                    }
                } // if (useBlue)
            } // if (iterNum < (numIterations - 1))

        } // end for (int iterNum = 0; ...)

        fireProgressStateChanged(100);
        

        if (useRed) {
            algoSepConvolverR.finalize();
            algoSepConvolverR = null;
        }

        if (useGreen) {
            algoSepConvolverG.finalize();
            algoSepConvolverG = null;
        }

        if (useBlue) {
            algoSepConvolverB.finalize();
            algoSepConvolverB = null;
        }

        // OK, the resultBuffer is filled with the results of the algorithm,
        // put this data into the destination image so it will be displayed in
        // in the ViewJFrameWizard
        try {

            if (useRed) {

                for (i = 0; i < length; i++) {

                    if ((resultBufferR[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                        resultBufferR[i] = 255.0f;
                    } else if (resultBufferR[i] < 0.0f) {
                        resultBufferR[i] = 0.0f;
                    }
                }

                destImage.importRGBData(1, 0, resultBufferR, false);
            }

            if (useGreen) {

                for (i = 0; i < length; i++) {

                    if ((resultBufferG[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                        resultBufferG[i] = 255.0f;
                    } else if (resultBufferG[i] < 0.0f) {
                        resultBufferG[i] = 0.0f;
                    }
                }

                destImage.importRGBData(2, 0, resultBufferG, false);
            }

            if (useBlue) {

                for (i = 0; i < length; i++) {

                    if ((resultBufferB[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                        resultBufferB[i] = 255.0f;
                    } else if (resultBufferB[i] < 0.0f) {
                        resultBufferB[i] = 0.0f;
                    }
                }

                destImage.importRGBData(3, 0, resultBufferB, false);
            }
        } catch (IOException error) {
            sourceBufferR = sourceBufferG = sourceBufferB = null;
            resultBufferR = resultBufferG = resultBufferB = null;
            gaussianBufferR = gaussianBufferG = gaussianBufferB = null;
            gradientBuffer = gradientBufferR = gradientBufferG = gradientBufferB = null;
            errorCleanUp("AlgorithmRegularizedIsotropicDiffusion: Could NOT import resultBuffer to the image", true);

            return;
        } // end try{}-catch{}

        destImage.calcMinMax();
    
        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    } // end run3DC


    /**
     * DOCUMENT ME!
     *
     * @param  resltBuffer  float [] The preallocated result buffer
     * @param  srcBuffer    float [] The initialized source buffer containing the input image
     * @param  gradBuffer   float [] The initialized buffer containing the magnitude of the gradient of the Gaussian
     *                      blurred input image
     */
    private void upDateImage(float[] resltBuffer, float[] srcBuffer, float[] gradBuffer) {
        int row, col, cIndex;
        float leftGradVal, rightGradVal, topGradVal, bottomGradVal, centerGradVal;
        float leftImgVal, rightImgVal, topImgVal, bottomImgVal, centerImgVal;
        float leftDiffVal, rightDiffVal, topDiffVal, bottomDiffVal, centerDiffVal;
        float maxGrad = gradBuffer[0];
        int length = yDim * xDim;

        for (int idx = 1; idx < length; idx++) {

            if (gradBuffer[idx] > maxGrad) {
                maxGrad = gradBuffer[idx];
            }
        } // for (int idx = 1; ...)

        lambda = contrast * maxGrad;


        for (row = 0; row < yDim; row++) {
            cIndex = row * xDim;

            for (col = 0, cIndex += col; col < xDim; cIndex++, col++) {

                centerGradVal = gradBuffer[cIndex];
                centerDiffVal = diffusitivity(centerGradVal);

                rightGradVal = getVal(gradBuffer, col + 1, row);
                rightDiffVal = diffusitivity(rightGradVal);

                leftGradVal = getVal(gradBuffer, col - 1, row);
                leftDiffVal = diffusitivity(leftGradVal);

                topGradVal = getVal(gradBuffer, col, row + 1);
                topDiffVal = diffusitivity(topGradVal);

                bottomGradVal = getVal(gradBuffer, col, row - 1);
                bottomDiffVal = diffusitivity(bottomGradVal);


                centerImgVal = srcBuffer[cIndex];
                leftImgVal = getVal(srcBuffer, col - 1, row);
                rightImgVal = getVal(srcBuffer, col + 1, row);
                topImgVal = getVal(srcBuffer, col, row + 1);
                bottomImgVal = getVal(srcBuffer, col, row - 1);

                resltBuffer[cIndex] = centerImgVal +
                                      (0.5f * timeStep *
                                           ((topImgVal * (topDiffVal + centerDiffVal)) +
                                                (leftImgVal * (leftDiffVal + centerDiffVal)) -
                                                (centerImgVal *
                                                     (leftDiffVal + rightDiffVal + bottomDiffVal + topDiffVal +
                                                          (4.0f * centerDiffVal))) +
                                                (rightImgVal * (centerDiffVal + rightDiffVal)) +
                                                (bottomImgVal * (centerDiffVal + bottomDiffVal))));

            } // end for (col = 0; ...)
        } // end for (row = 0; ...)
    } // end upDateImage(...)

    /**
     * DOCUMENT ME!
     *
     * @param  resltBuffer  float [] The preallocated result buffer
     * @param  srcBuffer    float [] The initialized source buffer containing the input image
     * @param  gradBuffer   float [] The initialized buffer containing the magnitude of the gradient of the Gaussian
     *                      blurred input image
     */
    private void upDateImage3D(float[] resltBuffer, float[] srcBuffer, float[] gradBuffer) {
        int slice, row, col, sIndex, cIndex;
        float leftGradVal, rightGradVal, topGradVal, bottomGradVal, centerGradVal;
        float zLowGradVal, zHighGradVal;
        float leftImgVal, rightImgVal, topImgVal, bottomImgVal, centerImgVal;
        float zLowImgVal, zHighImgVal;
        float leftDiffVal, rightDiffVal, topDiffVal, bottomDiffVal, centerDiffVal;
        float zLowDiffVal, zHighDiffVal;
        float maxGrad = gradBuffer[0];
        int sliceSize = yDim * xDim;
        int length = zDim * sliceSize;

        for (int idx = 1; idx < length; idx++) {

            if (gradBuffer[idx] > maxGrad) {
                maxGrad = gradBuffer[idx];
            }
        } // for (int idx = 1; ...)

        lambda = contrast * maxGrad;

        for (slice = 0; slice < zDim; slice++) {
            sIndex = slice * sliceSize;

            for (row = 0; row < yDim; row++) {
                cIndex = sIndex + (row * xDim);

                for (col = 0, cIndex += col; col < xDim; cIndex++, col++) {

                    centerGradVal = gradBuffer[cIndex];
                    centerDiffVal = diffusitivity(centerGradVal);

                    rightGradVal = getVal(gradBuffer, col + 1, row, slice);
                    rightDiffVal = diffusitivity(rightGradVal);

                    leftGradVal = getVal(gradBuffer, col - 1, row, slice);
                    leftDiffVal = diffusitivity(leftGradVal);

                    topGradVal = getVal(gradBuffer, col, row + 1, slice);
                    topDiffVal = diffusitivity(topGradVal);

                    bottomGradVal = getVal(gradBuffer, col, row - 1, slice);
                    bottomDiffVal = diffusitivity(bottomGradVal);

                    zLowGradVal = getVal(gradBuffer, col, row, slice - 1);
                    zLowDiffVal = diffusitivity(zLowGradVal);

                    zHighGradVal = getVal(gradBuffer, col, row, slice + 1);
                    zHighDiffVal = diffusitivity(zHighGradVal);


                    centerImgVal = srcBuffer[cIndex];
                    leftImgVal = getVal(srcBuffer, col - 1, row, slice);
                    rightImgVal = getVal(srcBuffer, col + 1, row, slice);
                    topImgVal = getVal(srcBuffer, col, row + 1, slice);
                    bottomImgVal = getVal(srcBuffer, col, row - 1, slice);
                    zLowImgVal = getVal(srcBuffer, col, row, slice - 1);
                    zHighImgVal = getVal(srcBuffer, col, row, slice + 1);

                    resltBuffer[cIndex] = centerImgVal +
                                          (0.5f * timeStep *
                                               ((topImgVal * (topDiffVal + centerDiffVal)) +
                                                    (leftImgVal * (leftDiffVal + centerDiffVal)) -
                                                    (centerImgVal *
                                                         (leftDiffVal + rightDiffVal + bottomDiffVal + topDiffVal +
                                                              zLowDiffVal + zHighDiffVal + (6.0f * centerDiffVal))) +
                                                    (rightImgVal * (centerDiffVal + rightDiffVal)) +
                                                    (bottomImgVal * (centerDiffVal + bottomDiffVal)) +
                                                    (zLowImgVal * (centerDiffVal + zLowDiffVal)) +
                                                    (zHighImgVal * (centerDiffVal + zHighDiffVal))));

                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; ...)
    } // end upDateImage3D(...)
} // end class AlgorithmRegularizedIsotropicDiffusion
