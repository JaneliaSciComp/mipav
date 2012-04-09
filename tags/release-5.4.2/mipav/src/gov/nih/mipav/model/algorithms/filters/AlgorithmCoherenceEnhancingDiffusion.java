package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import Jama.*;

import gov.nih.mipav.view.*;

import java.io.*;

import WildMagic.LibFoundation.NumericalAnalysis.Eigenf;


/**
 * <p>Algorithm to apply Coherence Enhancing Anisotropic Diffusion</p>
 *
 * <p>For color form a common structure tensor for all colors by averaging the structure tensors for each color.</p>
 *
 * <p>References:</p>
 *
 * <ol>
 *   <li>Joachim Weickert<br>
 *     <I>Chapter 15</I> Nonlinear Diffusion Filtering in Handbook of Computer Vision and Applications, Volume 2 Signal
 *     Processing and Pattern Recognition. The book is published by Academic Press, ISBN 0-12-379772-1</li>
 *   <li>Algorithms for Non-Linear Diffusion Matlab in a Literate Programming Style by Rein van den Boomgaard,
 *     Intelligent Sensory Information Systems, University of Amsterdam, The Netherlands
 *     http://carol.wins.uva.nl/~rein/nldiffusionweb/material.html</li>
 *   <li>Coherence-Enhancing Diffusion of Colour Images by Joachim Weickert, A. Sanfeliu, J.J. Villanueva, J. Vitria,
 *     editors, Proc. VII National Symposium on Pattern Recognition and Image Analysis (VII NSPRIA, Barcelona, April
 *     21-25, 1997), Vol. 1, pp. 239-244, 1997.</li>
 *   <li>Coherence-Enhancing Diffusion of Colour Images by Joachim Weickert, Image and Vision Computing, Vol. 17, 1999,
 *     pp. 201-212.</li>
 *   <li>Coherence-Enhancing Diffusion Filtering by Joachim Weickert, International Journal of Computer Vision, Vol. 31,
 *     No. 2/3, pp. 111-127, April, 1999</li>
 * </ol>
 *
 * @version  1.0; 31, July 2003
 * @author   Paul F. Hemler, Ph.D.
 */
public class AlgorithmCoherenceEnhancingDiffusion extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    // Boomgaard paper uses 0.7f for derivativeScale
    /** Gaussian scale for derivative operations. */
    float derivativeScale = 0.5f;

    /** flag indicating 2.5D processing. */
    boolean do25D = true;

    // Boomgaard paper uses 5 for gaussianScale
    /** Gaussian scale for structure tensor smoothing. */
    float gaussianScale = 2.0f;

    /** diffusitivity denominator. */
    double k = 0.001;

    /** diffusitivity denominator squared. */
    double k2;

    /** The number of iterations. */
    int numIterations = 1;

    /**
     * The time parameter step size, called tau in the algorithm description In 2D for stability the time step should be
     * <= 0.25. In 3D for stability the time step should be <= 1/6.
     */
    float timeStep;

    /** Kernels for the X derivative. */
    private float[] dxKernelX;

    /** DOCUMENT ME! */
    private float[] dxKernelY;

    /** DOCUMENT ME! */
    private float[] dxKernelZ;

    /** Kernels for the Y derivative. */
    private float[] dyKernelX;

    /** DOCUMENT ME! */
    private float[] dyKernelY;

    /** DOCUMENT ME! */
    private float[] dyKernelZ;

    /** Kernels for the Z derivative. */
    private float[] dzKernelX;

    /** DOCUMENT ME! */
    private float[] dzKernelY;

    /** DOCUMENT ME! */
    private float[] dzKernelZ;

    /** Eigensystem solver. */
    private Eigenf eigenSystemAlgo = null;

    /** Whether to process the entire image, or just the portion within the currently selected VOI. */
    private boolean entireImage = true;

    /** Reference to intermediate buffer. */
    private float[] intermediateBuffer;

    /** Width of the derivative buffer. */
    private int kRadius;

    /** Kernels for the Smoothing kernel. */
    private float[] skernelX;

    /** DOCUMENT ME! */
    private float[] skernelY;

    /** DOCUMENT ME! */
    private float[] skernelZ;

    /** Width of the smoothing buffer. */
    private int skRadius;

    /** image dimensions. */
    private int xDim, yDim, zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmCoherenceEnhancingDiffusion object.
     *
     * @param  srcImg           Source image model
     * @param  numI             Number of iterations
     * @param  k                Diffusitivity denominator
     * @param  derivativeScale  Scale of the gaussian when taking derivatives
     * @param  gaussianScale    Scale of the gaussian for smoothing the structure tensor
     * @param  do25D            Flag indicating 2.5D processing
     * @param  entireImage      whether to filter the entire image or just the VOI region
     */
    public AlgorithmCoherenceEnhancingDiffusion(ModelImage srcImg, int numI, float k, float derivativeScale,
                                                float gaussianScale, boolean do25D, boolean entireImage) {
        super(srcImg, srcImg);

        numIterations = numI;
        this.k = k;
        this.derivativeScale = derivativeScale;
        this.gaussianScale = gaussianScale;
        this.do25D = do25D;
        this.entireImage = entireImage;

        k2 = k * k;

        if (entireImage == false) {
            setMask(srcImg.generateVOIMask());
        }

        init();
        eigenSystemAlgo = new Eigenf(2);
    }


    /**
     * Creates a new AlgorithmCoherenceEnhancingDiffusion object.
     *
     * @param  destImg          Image model where result image is to stored
     * @param  srcImg           Source image model
     * @param  numI             Number of iterations
     * @param  k                Diffusitivity denominator
     * @param  derivativeScale  Scale of the gaussian when taking derivatives
     * @param  gaussianScale    Scale of the gaussian for smoothing the structure tensor
     * @param  do25D            Flag indicating 2.5D processing
     * @param  entireImage      whether to filter the entire image or just the VOI region
     */
    public AlgorithmCoherenceEnhancingDiffusion(ModelImage destImg, ModelImage srcImg, int numI, float k,
                                                float derivativeScale, float gaussianScale, boolean do25D,
                                                boolean entireImage) {
        super(destImg, srcImg);

        numIterations = numI;
        this.k = k;
        this.derivativeScale = derivativeScale;
        this.gaussianScale = gaussianScale;
        this.do25D = do25D;
        this.entireImage = entireImage;

        k2 = k * k;

        if (entireImage == false) {
            setMask(srcImg.generateVOIMask());
        }

        init();
        eigenSystemAlgo = new Eigenf(2);

    } // end AlgorithmCoherenceEnhancingDiffusion(...)

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        eigenSystemAlgo = null;
        intermediateBuffer = null;
        dxKernelX = null;
        dxKernelY = null;
        dxKernelZ = null;
        dyKernelX = null;
        dyKernelY = null;
        dyKernelZ = null;
        dzKernelX = null;
        dzKernelY = null;
        dzKernelZ = null;
        skernelX = null;
        skernelY = null;
        skernelZ = null;
        super.finalize();
    } // end finalize()


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            MipavUtil.displayError("AlgorithmCoherenceEnhancingDiffusion.run()  Source Image is null");

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
            } else if ((srcImage.getNDims() == 3) && do25D) {
                run2DC(srcImage.getExtents()[2]);
            } else {
                run3DC();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                run2D(1);
            } else if ((srcImage.getNDims() == 3) && do25D) {
                run2D(srcImage.getExtents()[2]);
            } else {
                run3D();
            }
        }
    } // end run()

    /**
     * Computes the maximum of two floating point numbers passed in.
     *
     * @param    a  float The first value
     * @param    b  float The second value
     *
     * @return  float The maximum of values a and b
     */
    private static float max(float a, float b) {
        return ((a > b) ? a : b);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dx   float [] destination buffer
     * @param  src  float [] source buffer
     *
     *              <P>Member function computes the derivative of the source buffer in the X-direction using the kernel
     *              formulation as specified in the MATLAB paper. The function uses seperable convolution, which means
     *              it first does a 1D convolution with the derivative of a Gaussian kernel in the X-direction and
     *              stores the result in an intermediate buffer. Once the intermediate buffer is completely filled, a
     *              smoothing in the Y-direction is performed using a 1D Gaussian kernel computed as in the MATLAB
     *              paper.</P>
     */
    private void computeDx(float[] dx, float[] src) {

        int idx;
        float sum = 0.0f;

        // convolve in the x direction
        int row, col;

        for (row = 0; row < yDim; row++) {

            for (col = 0; col < xDim; col++) {
                sum = 0.0f;

                for (idx = -kRadius; idx <= kRadius; idx++) {
                    sum += (dxKernelX[idx + kRadius] * getVal(src, col + idx, row));
                }

                setVal(sum, intermediateBuffer, col, row);
            } // end for (col = 0; ...)
        } // end for (row = 0; ...)

        // convolve in the y direction
        for (row = 0; row < yDim; row++) {

            for (col = 0; col < xDim; col++) {
                sum = 0.0f;

                for (idx = -kRadius; idx <= kRadius; idx++) {
                    sum += (dxKernelY[idx + kRadius] * getVal(intermediateBuffer, col, row + idx));
                }

                setVal(sum, dx, col, row);
            } // end for (col = 0; ...)
        } // end for (row = 0; ...)
    } // end computeDx(...)

    /**
     * DOCUMENT ME!
     *
     * @param  dx   float [] destination buffer
     * @param  src  float [] source buffer
     *
     *              <P>Member function computes the derivative of the source buffer in the X-direction using the kernel
     *              formulation as specified in the MATLAB paper. The function uses seperable convolution, which means
     *              it first does a 1D convolution with the derivative of a Gaussian kernel in the X-direction and
     *              stores the result in dx buffer. Once dx is completely filled, a smoothing in the Y-direction is
     *              performed using a 1D Gaussian kernel and the intermediate result is stored in intermediateBuffer.
     *              Once intermediateBuffer is completely filled, a smoothing in the Z-direction is performed and the
     *              final result is placed in dx.</P>
     */
    private void computeDx3D(float[] dx, float[] src) {

        int idx;
        float sum = 0.0f;

        // convolve in the x direction
        int slice, row, col;

        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dxKernelX[idx + kRadius] * getVal(src, col + idx, row, slice));
                    }

                    setVal(sum, dx, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // convolve in the y direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dxKernelY[idx + kRadius] * getVal(dx, col, row + idx, slice));
                    }

                    setVal(sum, intermediateBuffer, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // convolve in the z direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dxKernelZ[idx + kRadius] * getVal(intermediateBuffer, col, row, slice + idx));
                    }

                    setVal(sum, dx, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)
    } // end computeDx(...)

    /**
     * DOCUMENT ME!
     *
     * @param  dy   float [] destination buffer
     * @param  src  float [] source buffer
     *
     *              <P>Member function computes the derivative of the source buffer in the Y-direction using the kernel
     *              formulation as specified in the MATLAB paper. The function uses seperable convolution, which means
     *              it first does a 1D convolution with a Gaussian kernel in the X-direction and stores the result in an
     *              intermediate buffer. Once the intermediate buffer is completely filled, a 1D convolution in the
     *              Y-direction is performed using a 1D Gaussian derivative kernel computed as in the MATLAB paper.</P>
     */
    private void computeDy(float[] dy, float[] src) {

        int idx;
        float sum = 0.0f;

        // convolve in the x direction
        int row, col;

        for (row = 0; row < yDim; row++) {

            for (col = 0; col < xDim; col++) {
                sum = 0.0f;

                for (idx = -kRadius; idx <= kRadius; idx++) {
                    sum += (dyKernelX[idx + kRadius] * getVal(src, col + idx, row));
                }

                setVal(sum, intermediateBuffer, col, row);
            } // end for (col = 0; ...)
        } // end for (row = 0; ...)

        // convolve in the y direction
        for (row = 0; row < yDim; row++) {

            for (col = 0; col < xDim; col++) {
                sum = 0.0f;

                for (idx = -kRadius; idx <= kRadius; idx++) {
                    sum += (dyKernelY[idx + kRadius] * getVal(intermediateBuffer, col, row + idx));
                }

                setVal(sum, dy, col, row);
            } // end for (col = 0; ...)
        } // end for (row = 0; ...)
    } // end computeDy(...)

    /**
     * DOCUMENT ME!
     *
     * @param  dy   float [] destination buffer
     * @param  src  float [] source buffer
     *
     *              <P>Member function computes the derivative of the source buffer in the Y-direction using the kernel
     *              formulation as specified in the MATLAB paper. The function uses seperable convolution, which means
     *              it first does a 1D smoothing convolution with a Gaussian kernel in the X-direction and stores the
     *              result in dy buffer. Once dy is completely filled, a convolution in the Y-direction is performed
     *              using the derivative of a 1D Gaussian kernel and the intermediate result is stored in
     *              intermediateBuffer. Once intermediateBuffer is completely filled, a smoothing in the Z-direction is
     *              performed and the final result is placed in dy.</P>
     */
    private void computeDy3D(float[] dy, float[] src) {


        int idx;
        float sum = 0.0f;

        // convolve in the x direction
        int slice, row, col;

        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dyKernelX[idx + kRadius] * getVal(src, col + idx, row, slice));
                    }

                    setVal(sum, dy, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // convolve in the y direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dyKernelY[idx + kRadius] * getVal(dy, col, row + idx, slice));
                    }

                    setVal(sum, intermediateBuffer, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // convolve in the z direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dyKernelZ[idx + kRadius] * getVal(intermediateBuffer, col, row, slice + idx));
                    }

                    setVal(sum, dy, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)
    } // end computeDy(...)

    /**
     * DOCUMENT ME!
     *
     * @param  dz   float [] destination buffer
     * @param  src  float [] source buffer
     *
     *              <P>Member function computes the derivative of the source buffer in the Z-direction using the kernel
     *              formulation as specified in the MATLAB paper. The function uses seperable convolution, which means
     *              it first does a 1D convolution with a Gaussian kernel in the X-direction and stores the result in dz
     *              buffer. Once dz is completely filled, a smoothing in the Y-direction is performed using a 1D
     *              Gaussian kernel and the intermediate result is stored in intermediateBuffer. Once intermediateBuffer
     *              is completely filled, a convolution with the derivative of a Gaussian kernel in the Z-direction is
     *              performed and the final result is placed in dz.</P>
     */
    private void computeDz(float[] dz, float[] src) {

        int idx;
        float sum = 0.0f;

        // convolve in the x direction
        int slice, row, col;

        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dzKernelX[idx + kRadius] * getVal(src, col + idx, row, slice));
                    }

                    setVal(sum, dz, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // convolve in the y direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dzKernelY[idx + kRadius] * getVal(dz, col, row + idx, slice));
                    }

                    setVal(sum, intermediateBuffer, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // convolve in the z direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -kRadius; idx <= kRadius; idx++) {
                        sum += (dzKernelZ[idx + kRadius] * getVal(intermediateBuffer, col, row, slice + idx));
                    }

                    setVal(sum, dz, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)
    } // end computeDz(...)

  
    /**
     * DOCUMENT ME!
     *
     * @param  gs   float [] destination buffer
     * @param  src  float [] source buffer
     *
     *              <P>Member function gaussian smoothes the source buffer in the x and y directions using the kernel
     *              formulation as specified in the MATLAB paper. The function uses seperable convolution, which means
     *              it first does a 1D convolution with a Gaussian kernel in the X-direction and stores the result in an
     *              intermediate buffer. Once the intermediate buffer is completely filled, a 1D convolution in the
     *              Y-direction is performed using a 1D Gaussian kernel.</P>
     */
    private void gaussianSmooth(float[] gs, float[] src) {

        int idx;
        float sum = 0.0f;

        // convolve in the x direction
        int row, col;

        for (row = 0; row < yDim; row++) {

            for (col = 0; col < xDim; col++) {
                sum = 0.0f;

                for (idx = -skRadius; idx <= skRadius; idx++) {
                    sum += (skernelX[idx + skRadius] * getVal(src, col + idx, row));
                }

                setVal(sum, intermediateBuffer, col, row);
            } // end for (col = 0; ...)
        } // end for (row = 0; ...)

        // convolve in the y direction
        for (row = 0; row < yDim; row++) {

            for (col = 0; col < xDim; col++) {
                sum = 0.0f;

                for (idx = -skRadius; idx <= skRadius; idx++) {
                    sum += (skernelY[idx + skRadius] * getVal(intermediateBuffer, col, row + idx));
                }

                setVal(sum, gs, col, row);
            } // end for (col = 0; ...)
        } // end for (row = 0; ...)
    } // end gaussianSmooth(...)

    /**
     * DOCUMENT ME!
     *
     * @param  gs   float [] destination buffer
     * @param  src  float [] source buffer
     *
     *              <P>Member function gaussian smoothes the source buffer in the x,y, and z directions using the kernel
     *              formulation as specified in the MATLAB paper. The function uses seperable convolution, which means
     *              it first does a 1D convolution with a Gaussian kernel in the X-direction and stores the result in
     *              gs. Once the gs buffer is completely filled, a 1D convolution in the Y-direction is performed using
     *              a 1D Gaussian kernel and the result is stored in intermediateBuffer. Once intermediateBuffer is
     *              completely filled, a 1D convolution in the Z-direction is performed using a 1D Gaussian kernel and
     *              the result is placed in gs.</P>
     */
    private void gaussianSmooth3D(float[] gs, float[] src) {

        // convolve in the x direction
        int row, col, slice;
        int idx;
        float sum;

        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -skRadius; idx <= skRadius; idx++) {
                        sum += (skernelX[idx + skRadius] * getVal(src, col + idx, row, slice));
                    }

                    setVal(sum, gs, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // convolve in the y direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -skRadius; idx <= skRadius; idx++) {
                        sum += (skernelY[idx + skRadius] * getVal(gs, col, row + idx, slice));
                    }

                    setVal(sum, intermediateBuffer, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

        // Convolve in the z direction
        for (slice = 0; slice < zDim; slice++) {

            for (row = 0; row < yDim; row++) {

                for (col = 0; col < xDim; col++) {
                    sum = 0.0f;

                    for (idx = -skRadius; idx <= skRadius; idx++) {
                        sum += (skernelZ[idx + skRadius] * getVal(intermediateBuffer, col, row, slice + idx));
                    }

                    setVal(sum, gs, col, row, slice);
                } // end for (col = 0; ...)
            } // end for (row = 0; ...)
        } // for (slice = 0; slice < zDim; slice++)

    } // end gaussianSmooth(...)

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
     * Initialize buffers and kernels.
     */
    private void init() {

        int[] imgExtents = srcImage.getExtents();
        xDim = imgExtents[0];
        yDim = imgExtents[1];
        zDim = 1;

        if (srcImage.getNDims() == 3) {
            zDim = imgExtents[2];
        }

        int length = xDim * yDim * zDim;

        try {
            intermediateBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            intermediateBuffer = null;

            return;
        } // end try{}-catch{}


        // make the 1D gaussian derivative kernel for the X-direction
        kRadius = (int) Math.ceil(3.0 * derivativeScale);

        int kLength = (2 * kRadius) + 1;
        dxKernelX = new float[kLength];
        dxKernelY = new float[kLength];
        dxKernelZ = new float[kLength];

        dyKernelX = new float[kLength];
        dyKernelY = new float[kLength];
        dyKernelZ = new float[kLength];

        dzKernelX = new float[kLength];
        dzKernelY = new float[kLength];
        dzKernelZ = new float[kLength];

        int idx;
        float sum = 0.0f;
        float scale2 = derivativeScale * derivativeScale;

        for (idx = -kRadius; idx <= kRadius; idx++) {
            dxKernelX[idx + kRadius] = (float) Math.exp(-((idx * idx) / (2.0 * scale2)));
            sum += dxKernelX[idx + kRadius];
        }

        // normalize and make X a derivative kernel and Y and Z smoothing kernels
        for (idx = -kRadius; idx <= kRadius; idx++) {
            dxKernelZ[idx + kRadius] = dxKernelY[idx + kRadius] = dxKernelX[idx + kRadius] /= sum;
            dxKernelX[idx + kRadius] *= (-idx / scale2);
        }

        sum = 0.0f;

        for (idx = -kRadius; idx <= kRadius; idx++) {
            dyKernelY[idx + kRadius] = (float) Math.exp(-((idx * idx) / (2.0 * scale2)));
            sum += dyKernelY[idx + kRadius];
        }

        // normalize and make Y a derivative kernel and X and Z smoothing kernels
        for (idx = -kRadius; idx <= kRadius; idx++) {
            dyKernelZ[idx + kRadius] = dyKernelX[idx + kRadius] = dyKernelY[idx + kRadius] /= sum;
            dyKernelY[idx + kRadius] *= (-idx / scale2);
        }

        sum = 0.0f;

        for (idx = -kRadius; idx <= kRadius; idx++) {
            dzKernelZ[idx + kRadius] = (float) Math.exp(-((idx * idx) / (2.0 * scale2)));
            sum += dzKernelZ[idx + kRadius];
        }

        // normalize and make Z a derivative kernel and X and Y smoothing kernels
        for (idx = -kRadius; idx <= kRadius; idx++) {
            dzKernelX[idx + kRadius] = dzKernelY[idx + kRadius] = dzKernelZ[idx + kRadius] /= sum;
            dzKernelZ[idx + kRadius] *= (-idx / scale2);
        }

        skRadius = (int) Math.ceil(3.0 * gaussianScale);
        kLength = (2 * skRadius) + 1;
        skernelX = new float[kLength];
        skernelY = new float[kLength];
        skernelZ = new float[kLength];

        sum = 0.0f;
        scale2 = gaussianScale * gaussianScale;

        for (idx = -skRadius; idx <= skRadius; idx++) {
            skernelX[idx + skRadius] = (float) Math.exp(-((idx * idx) / (2.0 * scale2)));
            sum += skernelX[idx + skRadius];
        }

        // normalize the smoothing kernels
        for (idx = -skRadius; idx <= skRadius; idx++) {
            skernelZ[idx + skRadius] = skernelY[idx + skRadius] = skernelX[idx + skRadius] /= sum;
        }
    }

    /**
     * Starts the algorithm for 2D images.
     *
     * @param  numImages  int The number of 2D images
     */
    private void run2D(int numImages) {

        int totalComputation = numImages * numIterations;
        int computationCount = 0;

        // OK, here is where the meat of the algorithm goes

        // Grap the basic image stuff
        int length;
        int sliceOffset;
        length = xDim * yDim;

        // buffers for the image data
        float[] resultBuffer;
        float[] iterationBuffer;
        float[] dx2, dy2, dxdy; // derivative squared buffers
        float[] s11, s12, s22; // structure matrix buffers
        float lambdaOne, lambdaTwo; // eigenvalues
        float c1; // conductance (diffusion coefficient) in coherence direction
        float[] d11, d12, d22; // diffusion tensor
        float[] a, b, c; // references to the diffusion tensor

        try {
            resultBuffer = new float[length];
            iterationBuffer = new float[length];
            dx2 = new float[length];
            dy2 = new float[length];
            dxdy = new float[length];
            s11 = new float[length];
            s12 = new float[length];
            s22 = new float[length];
            d11 = new float[length];
            d12 = new float[length];
            d22 = new float[length];
            a = d11;
            b = d12;
            c = d22;
        } catch (OutOfMemoryError e) {
            resultBuffer = iterationBuffer = null;
            dx2 = dy2 = dxdy = s11 = s12 = s22 = null;
            d11 = d12 = d22 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: Out of memory when creating image buffers", true);

            return;
        } // end try{}-catch{}

        
        fireProgressStateChanged(0, srcImage.getImageName(), "Coherence-Enhancing Diffusion ...");
        
        // processing each slice starts here
        for (int imgNum = 0; imgNum < numImages; imgNum++) {
            sliceOffset = imgNum * length;

            // initialize iterationBuffer with data for the appropriate slice
            try {
                srcImage.exportData(sliceOffset, length, iterationBuffer);
            } catch (IOException error) {
                resultBuffer = iterationBuffer = null;
                dx2 = dy2 = dxdy = s11 = s12 = s22 = null;
                d11 = d12 = d22 = null;
                errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT export source image", true);

                return;
            } // end try{}-catch{}

            // copy the iterationBuffer to the resultBuffer to initialize the first iteration update
            System.arraycopy(iterationBuffer, 0, resultBuffer, 0, length);

            // iterate diffusion for each slice
            for (int iterNum = 1; iterNum <= numIterations; iterNum++) {
                
                fireProgressStateChanged(((float)(computationCount++) / (totalComputation - 1)), null, null);
                
                computeDx(dx2, iterationBuffer);
                computeDy(dy2, iterationBuffer);

                // compute all elements of the structure tensor
                for (int idx = 0; idx < dx2.length; idx++) {
                    dxdy[idx] = dx2[idx] * dy2[idx];
                    dx2[idx] *= dx2[idx];
                    dy2[idx] *= dy2[idx];
                } // end for (int idx = 0; ...)

                // gaussian smooth the structure tensor
                gaussianSmooth(s11, dx2);
                gaussianSmooth(s22, dy2);
                gaussianSmooth(s12, dxdy);

                // structure tensor is:
                // +--       --+
                // | s11   s12 |
                // |           |
                // | s12   s22 |
                // +--       --+

                // Compute the eigenvalues of the structure tensor and elements of the
                // diffusion tensor
                float exponent;
                float c2 = 0.01f;
                float epsilon = 0.0000001f;
                float alpha;
                int idx = 0;
                float Lpc, Lpp, Lcp, Lmp, Lmc, Lmm, Lcm, Lpm;
                float amc, apc, bmc, bcm, bpc, bcp, ccp, ccm;
                float r;

                for (int y = 0; y < yDim; y++) {

                    for (int x = 0; x < xDim; x++, idx++) {

                        if (entireImage || mask.get(sliceOffset + idx)) {

                            alpha = (s11[idx] - s22[idx]) * (s11[idx] - s22[idx]);
                            alpha += (4.0 * (s12[idx] * s12[idx]));
                            alpha = (float) Math.sqrt(alpha);

                            lambdaOne = 0.5f * (s11[idx] + s22[idx] + alpha);
                            lambdaTwo = 0.5f * (s11[idx] + s22[idx] - alpha);

                            exponent = ((lambdaOne - lambdaTwo) * (lambdaOne - lambdaTwo) / (float) k2);

                            c1 = max(0.01f, (float) (1.0 - Math.exp(-exponent)));

                            d11[idx] = 0.5f * (c1 + c2 + ((c2 - c1) * (s11[idx] - s22[idx]) / (alpha + epsilon)));
                            d12[idx] = (c2 - c1) * s12[idx] / (alpha + epsilon);
                            d22[idx] = 0.5f * (c1 + c2 - ((c2 - c1) * (s11[idx] - s22[idx]) / (alpha + epsilon)));

                            Lpc = getVal(iterationBuffer, x + 1, y);
                            Lpp = getVal(iterationBuffer, x + 1, y + 1);
                            Lcp = getVal(iterationBuffer, x, y + 1);

                            Lmp = getVal(iterationBuffer, x - 1, y + 1);
                            Lmc = getVal(iterationBuffer, x - 1, y);
                            Lmm = getVal(iterationBuffer, x - 1, y - 1);

                            Lcm = getVal(iterationBuffer, x, y - 1);
                            Lpm = getVal(iterationBuffer, x + 1, y - 1);

                            amc = getVal(a, x - 1, y);
                            apc = getVal(a, x + 1, y);

                            bmc = getVal(b, x - 1, y);
                            bcm = getVal(b, x, y - 1);
                            bpc = getVal(b, x + 1, y);
                            bcp = getVal(b, x, y + 1);

                            ccp = getVal(c, x, y + 1);
                            ccm = getVal(c, x, y - 1);

                            r = (-0.25f * (bmc + bcp) * Lmp) + (0.50f * (ccp + c[idx]) * Lcp) +
                                (0.25f * (bpc + bcp) * Lpp) + (0.50f * (amc + a[idx]) * Lmc) -
                                (0.50f * (amc + (2 * a[idx]) + apc + ccm + (2 * c[idx]) + ccp) * iterationBuffer[idx]) +
                                (0.50f * (apc + a[idx]) * Lpc) + (0.25f * (bmc + bcm) * Lmm) +
                                (0.50f * (ccm + c[idx]) * Lcm) - (0.25f * (bpc + bcm) * Lpm);

                            resultBuffer[idx] += (timeStep * r);
                        }
                    }
                }

                // update the iterationBuffer for the next pass
                System.arraycopy(resultBuffer, 0, iterationBuffer, 0, length);
            } // end for (int iterNum = 1; ...)

            try {
                destImage.importData(sliceOffset, resultBuffer, true);
            } catch (IOException error) {
                resultBuffer = iterationBuffer = null;
                dx2 = dy2 = dxdy = s11 = s12 = s22 = null;
                d11 = d12 = d22 = null;
                errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT import dest image", true);

                return;
            } // end try{}-catch{}

        } // end for (int imgNum = 0; ...)

        fireProgressStateChanged(100, null, null);
        
        destImage.calcMinMax();

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    } // end run2D(...)

    /**
     * Starts the algorithm for 2D images.
     *
     * @param  numImages  int The number of 2D images
     */
    private void run2DC(int numImages) {

        int totalComputation = numImages * numIterations;
        int computationCount = 0;

        // OK, here is where the meat of the algorithm goes

        // Grap the basic image stuff
        int length;
        int i;
        int sliceOffset;
        int[] imgExtents = srcImage.getExtents();

        xDim = imgExtents[0];
        yDim = imgExtents[1];
        length = xDim * yDim;

        // buffers for the image data
        float[] resultBufferR = null;
        float[] iterationBufferR = null;
        float[] resultBufferG = null;
        float[] iterationBufferG = null;
        float[] resultBufferB = null;
        float[] iterationBufferB = null;
        float[] dx2, dy2, dxdy; // derivative squared buffers
        float[] s11R = null;
        float[] s12R = null;
        float[] s22R = null;
        float[] s11G = null;
        float[] s12G = null;
        float[] s22G = null;
        float[] s11B = null;
        float[] s12B = null;
        float[] s22B = null;
        float[] s11, s12, s22; // structure matrix buffers
        float lambdaOne, lambdaTwo; // eigenvalues
        float c1; // conductance (diffusion coefficient) in coherence direction
        float[] d11, d12, d22; // diffusion tensor
        float[] a, b, c; // references to the diffusion tensor

        boolean useRed = true;
        boolean useGreen = true;
        boolean useBlue = true;
        int colorsPresent = 3;

        fireProgressStateChanged(0, srcImage.getImageName(), "Coherence-Enhancing Diffusion ...");
        
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

        try {

            if (useRed) {
                resultBufferR = new float[length];
                iterationBufferR = new float[length];
                s11R = new float[length];
                s12R = new float[length];
                s22R = new float[length];
            }

            if (useGreen) {
                resultBufferG = new float[length];
                iterationBufferG = new float[length];
                s11G = new float[length];
                s12G = new float[length];
                s22G = new float[length];
            }

            if (useBlue) {
                resultBufferB = new float[length];
                iterationBufferB = new float[length];
                s11B = new float[length];
                s12B = new float[length];
                s22B = new float[length];
            }

            dx2 = new float[length];
            dy2 = new float[length];
            dxdy = new float[length];
            s11 = new float[length];
            s12 = new float[length];
            s22 = new float[length];
            d11 = new float[length];
            d12 = new float[length];
            d22 = new float[length];
            a = d11;
            b = d12;
            c = d22;
        } catch (OutOfMemoryError e) {
            resultBufferR = iterationBufferR = null;
            resultBufferG = iterationBufferG = null;
            resultBufferB = iterationBufferB = null;
            dx2 = dy2 = dxdy = s11 = s12 = s22 = null;
            s11R = s12R = s22R = s11G = s12G = s22G = s11B = s12B = s22B = null;
            d11 = d12 = d22 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: Out of memory when creating image buffers", true);

            return;
        } // end try{}-catch{}

        // processing each slice starts here
        for (int imgNum = 0; imgNum < numImages; imgNum++) {
            sliceOffset = imgNum * length;

            // initialize iterationBuffer with data for the appropriate slice
            try {

                if (useRed) {
                    srcImage.exportRGBData(1, 4 * sliceOffset, length, iterationBufferR);
                }

                if (useGreen) {
                    srcImage.exportRGBData(2, 4 * sliceOffset, length, iterationBufferG);
                }

                if (useBlue) {
                    srcImage.exportRGBData(3, 4 * sliceOffset, length, iterationBufferB);
                }
            } catch (IOException error) {
                resultBufferR = iterationBufferR = null;
                resultBufferG = iterationBufferG = null;
                resultBufferB = iterationBufferB = null;
                s11R = s12R = s22R = s11G = s12G = s22G = s11B = s12B = s22B = null;
                dx2 = dy2 = dxdy = s11 = s12 = s22 = null;
                d11 = d12 = d22 = null;
                errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT export source image", true);

                return;
            } // end try{}-catch{}

            // copy the iterationBuffer to the resultBuffer to initialize the first iteration update
            if (useRed) {
                System.arraycopy(iterationBufferR, 0, resultBufferR, 0, length);
            }

            if (useGreen) {
                System.arraycopy(iterationBufferG, 0, resultBufferG, 0, length);
            }

            if (useBlue) {
                System.arraycopy(iterationBufferB, 0, resultBufferB, 0, length);
            }

            // iterate diffusion for each slice
            for (int iterNum = 1; iterNum <= numIterations; iterNum++) {
                fireProgressStateChanged(((float)(computationCount++) / (totalComputation - 1)), null, null);
                
                if (useRed) {
                    computeDx(dx2, iterationBufferR);
                    computeDy(dy2, iterationBufferR);

                    // compute all elements of the structure tensor
                    for (int idx = 0; idx < dx2.length; idx++) {
                        dxdy[idx] = dx2[idx] * dy2[idx];
                        dx2[idx] *= dx2[idx];
                        dy2[idx] *= dy2[idx];
                    } // end for (int idx = 0; ...)

                    // gaussian smooth the structure tensor
                    gaussianSmooth(s11R, dx2);
                    gaussianSmooth(s22R, dy2);
                    gaussianSmooth(s12R, dxdy);
                } // if (useRed)

                if (useGreen) {
                    computeDx(dx2, iterationBufferG);
                    computeDy(dy2, iterationBufferG);

                    // compute all elements of the structure tensor
                    for (int idx = 0; idx < dx2.length; idx++) {
                        dxdy[idx] = dx2[idx] * dy2[idx];
                        dx2[idx] *= dx2[idx];
                        dy2[idx] *= dy2[idx];
                    } // end for (int idx = 0; ...)

                    // gaussian smooth the structure tensor
                    gaussianSmooth(s11G, dx2);
                    gaussianSmooth(s22G, dy2);
                    gaussianSmooth(s12G, dxdy);
                } // if (useGreen)

                if (useBlue) {
                    computeDx(dx2, iterationBufferB);
                    computeDy(dy2, iterationBufferB);

                    // compute all elements of the structure tensor
                    for (int idx = 0; idx < dx2.length; idx++) {
                        dxdy[idx] = dx2[idx] * dy2[idx];
                        dx2[idx] *= dx2[idx];
                        dy2[idx] *= dy2[idx];
                    } // end for (int idx = 0; ...)

                    // gaussian smooth the structure tensor
                    gaussianSmooth(s11B, dx2);
                    gaussianSmooth(s22B, dy2);
                    gaussianSmooth(s12B, dxdy);
                } // if (useBlue)

                for (i = 0; i < length; i++) {
                    s11[i] = 0.0f;
                    s12[i] = 0.0f;
                    s22[i] = 0.0f;

                    if (useRed) {
                        s11[i] = s11R[i];
                        s12[i] = s12R[i];
                        s22[i] = s22R[i];
                    }

                    if (useGreen) {
                        s11[i] += s11G[i];
                        s12[i] += s12G[i];
                        s22[i] += s22G[i];
                    }

                    if (useBlue) {
                        s11[i] += s11B[i];
                        s12[i] += s12B[i];
                        s22[i] += s22B[i];
                    }

                    s11[i] /= colorsPresent;
                    s12[i] /= colorsPresent;
                    s22[i] /= colorsPresent;
                }

                // structure tensor is:
                // +--       --+
                // | s11   s12 |
                // |           |
                // | s12   s22 |
                // +--       --+

                // Compute the eigenvalues of the structure tensor and elements of the
                // diffusion tensor
                float exponent;
                float c2 = 0.01f;
                float epsilon = 0.0000001f;
                float alpha;
                int idx = 0;
                float LpcR = 0.0f;
                float LppR = 0.0f;
                float LcpR = 0.0f;
                float LmpR = 0.0f;
                float LmcR = 0.0f;
                float LmmR = 0.0f;
                float LcmR = 0.0f;
                float LpmR = 0.0f;
                float LpcG = 0.0f;
                float LppG = 0.0f;
                float LcpG = 0.0f;
                float LmpG = 0.0f;
                float LmcG = 0.0f;
                float LmmG = 0.0f;
                float LcmG = 0.0f;
                float LpmG = 0.0f;
                float LpcB = 0.0f;
                float LppB = 0.0f;
                float LcpB = 0.0f;
                float LmpB = 0.0f;
                float LmcB = 0.0f;
                float LmmB = 0.0f;
                float LcmB = 0.0f;
                float LpmB = 0.0f;
                float amc, apc, bmc, bcm, bpc, bcp, ccp, ccm;
                float r;

                for (int y = 0; y < yDim; y++) {

                    for (int x = 0; x < xDim; x++, idx++) {

                        if (entireImage || mask.get(sliceOffset + idx)) {
                            alpha = (s11[idx] - s22[idx]) * (s11[idx] - s22[idx]);
                            alpha += (4.0 * (s12[idx] * s12[idx]));
                            alpha = (float) Math.sqrt(alpha);

                            lambdaOne = 0.5f * (s11[idx] + s22[idx] + alpha);
                            lambdaTwo = 0.5f * (s11[idx] + s22[idx] - alpha);

                            exponent = ((lambdaOne - lambdaTwo) * (lambdaOne - lambdaTwo) / (float) k2);

                            c1 = max(0.01f, (float) (1.0 - Math.exp(-exponent)));

                            d11[idx] = 0.5f * (c1 + c2 + ((c2 - c1) * (s11[idx] - s22[idx]) / (alpha + epsilon)));

                            d12[idx] = (c2 - c1) * s12[idx] / (alpha + epsilon);

                            d22[idx] = 0.5f * (c1 + c2 - ((c2 - c1) * (s11[idx] - s22[idx]) / (alpha + epsilon)));

                            if (useRed) {
                                LpcR = getVal(iterationBufferR, x + 1, y);
                                LppR = getVal(iterationBufferR, x + 1, y + 1);
                                LcpR = getVal(iterationBufferR, x, y + 1);

                                LmpR = getVal(iterationBufferR, x - 1, y + 1);
                                LmcR = getVal(iterationBufferR, x - 1, y);
                                LmmR = getVal(iterationBufferR, x - 1, y - 1);

                                LcmR = getVal(iterationBufferR, x, y - 1);
                                LpmR = getVal(iterationBufferR, x + 1, y - 1);
                            } // if (useRed)

                            if (useGreen) {
                                LpcG = getVal(iterationBufferG, x + 1, y);
                                LppG = getVal(iterationBufferG, x + 1, y + 1);
                                LcpG = getVal(iterationBufferG, x, y + 1);

                                LmpG = getVal(iterationBufferG, x - 1, y + 1);
                                LmcG = getVal(iterationBufferG, x - 1, y);
                                LmmG = getVal(iterationBufferG, x - 1, y - 1);

                                LcmG = getVal(iterationBufferG, x, y - 1);
                                LpmG = getVal(iterationBufferG, x + 1, y - 1);
                            } // if (useGreen)

                            if (useBlue) {
                                LpcB = getVal(iterationBufferB, x + 1, y);
                                LppB = getVal(iterationBufferB, x + 1, y + 1);
                                LcpB = getVal(iterationBufferB, x, y + 1);

                                LmpB = getVal(iterationBufferB, x - 1, y + 1);
                                LmcB = getVal(iterationBufferB, x - 1, y);
                                LmmB = getVal(iterationBufferB, x - 1, y - 1);

                                LcmB = getVal(iterationBufferB, x, y - 1);
                                LpmB = getVal(iterationBufferB, x + 1, y - 1);
                            } // if (useBlue)

                            amc = getVal(a, x - 1, y);
                            apc = getVal(a, x + 1, y);

                            bmc = getVal(b, x - 1, y);
                            bcm = getVal(b, x, y - 1);
                            bpc = getVal(b, x + 1, y);
                            bcp = getVal(b, x, y + 1);

                            ccp = getVal(c, x, y + 1);
                            ccm = getVal(c, x, y - 1);

                            if (useRed) {
                                r = (-0.25f * (bmc + bcp) * LmpR) + (0.50f * (ccp + c[idx]) * LcpR) +
                                    (0.25f * (bpc + bcp) * LppR) + (0.50f * (amc + a[idx]) * LmcR) -
                                    (0.50f * (amc + (2 * a[idx]) + apc + ccm + (2 * c[idx]) + ccp) *
                                         iterationBufferR[idx]) + (0.50f * (apc + a[idx]) * LpcR) +
                                    (0.25f * (bmc + bcm) * LmmR) + (0.50f * (ccm + c[idx]) * LcmR) -
                                    (0.25f * (bpc + bcm) * LpmR);

                                resultBufferR[idx] += (timeStep * r);
                            } // if (useRed)

                            if (useGreen) {
                                r = (-0.25f * (bmc + bcp) * LmpG) + (0.50f * (ccp + c[idx]) * LcpG) +
                                    (0.25f * (bpc + bcp) * LppG) + (0.50f * (amc + a[idx]) * LmcG) -
                                    (0.50f * (amc + (2 * a[idx]) + apc + ccm + (2 * c[idx]) + ccp) *
                                         iterationBufferG[idx]) + (0.50f * (apc + a[idx]) * LpcG) +
                                    (0.25f * (bmc + bcm) * LmmG) + (0.50f * (ccm + c[idx]) * LcmG) -
                                    (0.25f * (bpc + bcm) * LpmG);

                                resultBufferG[idx] += (timeStep * r);
                            } // if (useGreen)

                            if (useBlue) {
                                r = (-0.25f * (bmc + bcp) * LmpB) + (0.50f * (ccp + c[idx]) * LcpB) +
                                    (0.25f * (bpc + bcp) * LppB) + (0.50f * (amc + a[idx]) * LmcB) -
                                    (0.50f * (amc + (2 * a[idx]) + apc + ccm + (2 * c[idx]) + ccp) *
                                         iterationBufferB[idx]) + (0.50f * (apc + a[idx]) * LpcB) +
                                    (0.25f * (bmc + bcm) * LmmB) + (0.50f * (ccm + c[idx]) * LcmB) -
                                    (0.25f * (bpc + bcm) * LpmB);

                                resultBufferB[idx] += (timeStep * r);
                            } // if (useBlue)
                        }
                    }
                }

                // update the iterationBuffer for the next pass
                if (useRed) {
                    System.arraycopy(resultBufferR, 0, iterationBufferR, 0, length);
                }

                if (useGreen) {
                    System.arraycopy(resultBufferG, 0, iterationBufferG, 0, length);
                }

                if (useBlue) {
                    System.arraycopy(resultBufferB, 0, iterationBufferB, 0, length);
                }
            } // end for (int iterNum = 1; ...)

            try {

                if (useRed) {

                    for (i = 0; i < length; i++) {

                        if ((resultBufferR[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                            resultBufferR[i] = 255.0f;
                        } else if (resultBufferR[i] < 0.0f) {
                            resultBufferR[i] = 0.0f;
                        }
                    }

                    destImage.importRGBData(1, 4 * sliceOffset, resultBufferR, true);
                }

                if (useGreen) {

                    for (i = 0; i < length; i++) {

                        if ((resultBufferG[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                            resultBufferG[i] = 255.0f;
                        } else if (resultBufferG[i] < 0.0f) {
                            resultBufferG[i] = 0.0f;
                        }
                    }

                    destImage.importRGBData(2, 4 * sliceOffset, resultBufferG, true);
                }

                if (useBlue) {

                    for (i = 0; i < length; i++) {

                        if ((resultBufferB[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                            resultBufferB[i] = 255.0f;
                        } else if (resultBufferB[i] < 0.0f) {
                            resultBufferB[i] = 0.0f;
                        }
                    }

                    destImage.importRGBData(3, 4 * sliceOffset, resultBufferB, true);
                }
            } catch (IOException error) {
                resultBufferR = iterationBufferR = null;
                resultBufferG = iterationBufferG = null;
                resultBufferB = iterationBufferB = null;
                s11R = s12R = s22R = s11G = s12G = s22G = s11B = s12B = s22B = null;
                dx2 = dy2 = dxdy = s11 = s12 = s22 = null;
                d11 = d12 = d22 = null;
                errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT import dest image", true);

                return;
            } // end try{}-catch{}

        } // end for (int imgNum = 0; ...)

        fireProgressStateChanged(100, null, null);
        
        
        destImage.calcMinMax();

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    } // end run2DC(...)

    /**
     * Starts the algorithm for 3D images.
     */
    private void run3D() {

        // OK, here is where the meat of the algorithm goes
        int length = xDim * yDim * zDim;

        // buffers for the image data
        float[] resultBuffer;
        float[] iterationBuffer;
        float[] dx2, dy2, dxdy; // derivative squared buffers
        float[] dz2, dxdz, dydz;
        float[] s11, s12, s22; // structure matrix buffers
        float[] s13, s23, s33;
        float lambdaOne, lambdaTwo; // eigenvalues
        float lambdaThree;
        float c1; // conductance (diffusion coefficient) in coherence direction
        float[] d11, d12, d22; // diffusion tensor
        float[] d13, d23, d33;
        float[] a, b, c; // references to the diffusion tensor
        float[] d, e, f;

        fireProgressStateChanged(0, srcImage.getImageName(), "Coherence-Enhancing Diffusion ...");
        
        
        try {
            resultBuffer = new float[length];
            iterationBuffer = new float[length];
            dx2 = new float[length];
            dy2 = new float[length];
            dxdy = new float[length];
            dz2 = new float[length];
            dxdz = new float[length];
            dydz = new float[length];
            s11 = new float[length];
            s12 = new float[length];
            s22 = new float[length];
            s13 = new float[length];
            s23 = new float[length];
            s33 = new float[length];
            d11 = new float[length];
            d12 = new float[length];
            d22 = new float[length];
            d13 = new float[length];
            d23 = new float[length];
            d33 = new float[length];
            a = d11;
            b = d12;
            c = d13;
            d = d22;
            e = d23;
            f = d33;
        } catch (OutOfMemoryError er) {
            resultBuffer = iterationBuffer = null;
            dx2 = dy2 = dxdy = dz2 = dxdz = dydz = null;
            s11 = s12 = s22 = s13 = s23 = s33 = null;
            d11 = d12 = d22 = d13 = d23 = d33 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: Out of memory when creating image buffers", true);

            return;
        } // end try{}-catch{}

        // initialize iterationBuffer with data
        try {
            srcImage.exportData(0, length, iterationBuffer);
        } catch (IOException error) {
            resultBuffer = iterationBuffer = null;
            dx2 = dy2 = dxdy = dz2 = dxdz = dydz = null;
            s11 = s12 = s22 = s13 = s23 = s33 = null;
            d11 = d12 = d22 = d13 = d23 = d33 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT export source image", true);

            return;
        } // end try{}-catch{}

        // copy the iterationBuffer to the resultBuffer to initialize the first iteration update
        System.arraycopy(iterationBuffer, 0, resultBuffer, 0, length);

        // iterate diffusion
        for (int iterNum = 1; iterNum <= numIterations; iterNum++) {

            computeDx3D(dx2, iterationBuffer);
            computeDy3D(dy2, iterationBuffer);
            computeDz(dz2, iterationBuffer);

            // compute all elements of the structure tensor
            for (int idx = 0; idx < length; idx++) {
                dxdy[idx] = dx2[idx] * dy2[idx];
                dxdz[idx] = dx2[idx] * dz2[idx];
                dydz[idx] = dy2[idx] * dz2[idx];
                dx2[idx] *= dx2[idx];
                dy2[idx] *= dy2[idx];
                dz2[idx] *= dz2[idx];
            } // end for (int idx = 0; ...)

            // gaussian smooth the structure tensor
            gaussianSmooth3D(s11, dx2);
            gaussianSmooth3D(s22, dy2);
            gaussianSmooth3D(s12, dxdy);
            gaussianSmooth3D(s13, dxdz);
            gaussianSmooth3D(s23, dydz);
            gaussianSmooth3D(s33, dz2);

            // structure tensor is:
            // +--             --+
            // | s11   s12   s13 |
            // |                 |
            // | s12   s22   s23 |
            // |                 |
            // | s13   s23   s33 |
            // +--             --+

            // Compute the eigenvalues of the structure tensor and elements of the
            // diffusion tensor
            float exponent;
            float c2 = 0.01f;
            float c3 = 0.01f;
            int idx = 0;
            float Lcpm, Lmcm, Lccm, Lpcm, Lcmm, Lmpc, Lcpc, Lppc, Lmcc, Lpcc;
            float Lmmc, Lcmc, Lpmc, Lcpp, Lmcp, Lccp, Lpcp, Lcmp;
            float amcc, apcc;
            float bmcc, bcpc, bpcc, bcmc;
            float cmcc, cccm, cpcc, cccp;
            float dcpc, dcmc;
            float ecpc, eccm, ecmc, eccp;
            float fccm, fccp;
            float r;

            // make a 3X3 eigenSolver
            eigenSystemAlgo = new Eigenf(3);

            double[][] evecs = new double[3][3];
            Matrix rot;
            Matrix cMat = new Matrix(3, 3);
            // cArray is a reference to cMat's internal array.
            double[][] cArray = cMat.getArray();

            for (int i = 0; i <= 2; i++) {

                for (int j = 0; j <= 2; j++) {
                    cArray[i][j] = 0.0;
                }
            }

            Matrix dMat;

            for (int z = 0; z < zDim; z++) {
                fireProgressStateChanged(((((iterNum - 1) * zDim) + z) * 100 / (numIterations * zDim)), null, null);
                
                for (int y = 0; y < yDim; y++) {

                    for (int x = 0; x < xDim; x++, idx++) {

                        if (entireImage || mask.get(idx)) {                          
                            eigenSystemAlgo.SetData(0, 0, s11[idx]);
                            eigenSystemAlgo.SetData(0, 1, s12[idx]);
                            eigenSystemAlgo.SetData(0, 2, s13[idx]);

                            eigenSystemAlgo.SetData(1, 0, s12[idx]);
                            eigenSystemAlgo.SetData(1, 1, s22[idx]);
                            eigenSystemAlgo.SetData(1, 2, s23[idx]);

                            eigenSystemAlgo.SetData(2, 0, s13[idx]);
                            eigenSystemAlgo.SetData(2, 1, s23[idx]);
                            eigenSystemAlgo.SetData(2, 2, s33[idx]);

                            // OK, solve the eigen system
                            eigenSystemAlgo.IncrSortEigenStuff();

                            // extract the eigen values from the AlgorithmEigensolver
                            // The eigenvalues are stored in increasing order and may be
                            // accessed by getEigenvalue(i).  The corresponding eigenvectors
                            // are stored as columns of a matrix.  The eigenvector for the
                            // i-th eigenvalue is stored in the i-th column of the matrix.
                            // Eigenvectors are accessed by getEigenvector(iRow,iCol).
                            // The eigenvectors are normalized to unit length.
                            // lambdaOne is the largest eigenvalue.
                            lambdaOne = eigenSystemAlgo.GetEigenvalue(2);
                            lambdaTwo = eigenSystemAlgo.GetEigenvalue(1);
                            lambdaThree = eigenSystemAlgo.GetEigenvalue(0);

                            evecs[0][0] = eigenSystemAlgo.GetEigenvector(0, 2);
                            evecs[1][0] = eigenSystemAlgo.GetEigenvector(1, 2);
                            evecs[2][0] = eigenSystemAlgo.GetEigenvector(2, 2);

                            evecs[0][1] = eigenSystemAlgo.GetEigenvector(0, 1);
                            evecs[1][1] = eigenSystemAlgo.GetEigenvector(1, 1);
                            evecs[2][1] = eigenSystemAlgo.GetEigenvector(2, 1);

                            evecs[0][2] = eigenSystemAlgo.GetEigenvector(0, 0);
                            evecs[1][2] = eigenSystemAlgo.GetEigenvector(1, 0);
                            evecs[2][2] = eigenSystemAlgo.GetEigenvector(2, 0);
                            	

                            // The columns of the rotation matrix are the eigenvectors
                            // of the structure tensor.
                            rot = new Matrix(evecs);

                            exponent = ((((lambdaOne - lambdaTwo) * (lambdaOne - lambdaTwo)) +
                                         ((lambdaOne - lambdaThree) * (lambdaOne - lambdaThree)) +
                                         ((lambdaTwo - lambdaThree) * (lambdaTwo - lambdaThree))) / (float) k2);

                            c1 = max(0.01f, (float) (1.0 - Math.exp(-exponent)));
                            cArray[0][0] = c1;
                            cArray[1][1] = c2;
                            cArray[2][2] = c3;

                            dMat = (rot.transpose().times(cMat)).times(rot);

                            d11[idx] = (float) dMat.get(0, 0);
                            d12[idx] = (float) dMat.get(0, 1);
                            d22[idx] = (float) dMat.get(1, 1);
                            d13[idx] = (float) dMat.get(0, 2);
                            d23[idx] = (float) dMat.get(1, 2);
                            d33[idx] = (float) dMat.get(2, 2);

                            Lcpm = getVal(iterationBuffer, x, y + 1, z - 1);
                            Lmcm = getVal(iterationBuffer, x - 1, y, z - 1);
                            Lccm = getVal(iterationBuffer, x, y, z - 1);
                            Lpcm = getVal(iterationBuffer, x + 1, y, z - 1);
                            Lcmm = getVal(iterationBuffer, x, y - 1, z - 1);

                            Lmpc = getVal(iterationBuffer, x - 1, y + 1, z);
                            Lcpc = getVal(iterationBuffer, x, y + 1, z);
                            Lppc = getVal(iterationBuffer, x + 1, y + 1, z);
                            Lmcc = getVal(iterationBuffer, x - 1, y, z);
                            Lpcc = getVal(iterationBuffer, x + 1, y, z);
                            Lmmc = getVal(iterationBuffer, x - 1, y - 1, z);
                            Lcmc = getVal(iterationBuffer, x, y - 1, z);
                            Lpmc = getVal(iterationBuffer, x + 1, y - 1, z);

                            Lcpp = getVal(iterationBuffer, x, y + 1, z + 1);
                            Lmcp = getVal(iterationBuffer, x - 1, y, z + 1);
                            Lccp = getVal(iterationBuffer, x, y, z + 1);
                            Lpcp = getVal(iterationBuffer, x + 1, y, z + 1);
                            Lcmp = getVal(iterationBuffer, x, y - 1, z + 1);

                            amcc = getVal(a, x - 1, y, z);
                            apcc = getVal(a, x + 1, y, z);

                            bmcc = getVal(b, x - 1, y, z);
                            bcpc = getVal(b, x, y + 1, z);
                            bpcc = getVal(b, x + 1, y, z);
                            bcmc = getVal(b, x, y - 1, z);

                            cmcc = getVal(c, x - 1, y, z);
                            cccm = getVal(c, x, y, z - 1);
                            cpcc = getVal(c, x + 1, y, z);
                            cccp = getVal(c, x, y, z + 1);

                            dcpc = getVal(d, x, y + 1, z);
                            dcmc = getVal(d, x, y - 1, z);

                            ecpc = getVal(e, x, y + 1, z);
                            eccm = getVal(e, x, y, z - 1);
                            ecmc = getVal(e, x, y - 1, z);
                            eccp = getVal(e, x, y, z + 1);

                            fccm = getVal(f, x, y, z - 1);
                            fccp = getVal(f, x, y, z + 1);

                            r = (-0.25f * (ecpc + eccm) * Lcpm) + (0.25f * (cmcc + cccm) * Lmcm) +
                                (0.50f * (fccm + f[idx]) * Lccm) - (0.25f * (cpcc + cccm) * Lpcm) +
                                (0.25f * (ecmc + eccm) * Lcmm) - (0.25f * (bmcc + bcpc) * Lmpc) +
                                (0.50f * (d[idx] + dcpc) * Lcpc) + (0.25f * (bpcc + bcpc) * Lppc) +
                                (0.50f * (amcc + a[idx]) * Lmcc) -
                                (0.50f *
                                     (apcc + amcc + (2 * a[idx]) + dcpc + dcmc + (2 * d[idx]) + fccp + fccm +
                                          (2 * f[idx])) * iterationBuffer[idx]) + (0.50f * (a[idx] + apcc) * Lpcc) +
                                (0.25f * (bmcc + bcmc) * Lmmc) + (0.50f * (d[idx] + dcmc) * Lcmc) -
                                (0.25f * (bpcc + bcmc) * Lpmc) + (0.25f * (ecpc + eccp) * Lcpp) -
                                (0.25f * (cmcc + cccp) * Lmcp) + (0.50f * (f[idx] + fccp) * Lccp) +
                                (0.25f * (cpcc + cccp) * Lpcp) - (0.25f * (ecmc + eccp) * Lcmp);

                            resultBuffer[idx] += (timeStep * r);
                        }
                    }
                }
            }

            // update the iterationBuffer for the next pass
            System.arraycopy(resultBuffer, 0, iterationBuffer, 0, length);
        } // end for (int iterNum = 1; ...)

        try {
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            resultBuffer = iterationBuffer = null;
            dx2 = dy2 = dxdy = dz2 = dxdz = dydz = null;
            s11 = s12 = s22 = s13 = s23 = s33 = null;
            d11 = d12 = d22 = d13 = d23 = d33 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT import dest image", true);

            return;
        } // end try{}-catch{}

        fireProgressStateChanged(100, null, null);
        
        destImage.calcMinMax();

        // The meat is done
        

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    } // end run3D(...)

    /**
     * Starts the algorithm for 3D color images.
     */
    private void run3DC() {

        // OK, here is where the meat of the algorithm goes
        // Grap the basic image stuff
        int length = xDim * yDim * zDim;
        int i, j;

        // buffers for the image data
        float[] resultBufferR = null;
        float[] iterationBufferR = null;
        float[] resultBufferG = null;
        float[] iterationBufferG = null;
        float[] resultBufferB = null;
        float[] iterationBufferB = null;
        float[] dx2, dy2, dxdy; // derivative squared buffers
        float[] dz2, dxdz, dydz;
        float[] s11R = null;
        float[] s12R = null;
        float[] s22R = null;
        float[] s13R = null;
        float[] s23R = null;
        float[] s33R = null;
        float[] s11G = null;
        float[] s12G = null;
        float[] s22G = null;
        float[] s13G = null;
        float[] s23G = null;
        float[] s33G = null;
        float[] s11B = null;
        float[] s12B = null;
        float[] s22B = null;
        float[] s13B = null;
        float[] s23B = null;
        float[] s33B = null;
        float[] s11, s12, s22; // structure matrix buffers
        float[] s13, s23, s33;
        float lambdaOne, lambdaTwo; // eigenvalues
        float lambdaThree;
        float c1; // conductance (diffusion coefficient) in coherence direction
        float[] d11, d12, d22; // diffusion tensor
        float[] d13, d23, d33;
        float[] a, b, c; // references to the diffusion tensor
        float[] d, e, f;

        boolean useRed = true;
        boolean useGreen = true;
        boolean useBlue = true;
        int colorsPresent = 3;

        fireProgressStateChanged(0, srcImage.getImageName(), "Coherence-Enhancing Diffusion ...");
        
        
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

        try {

            if (useRed) {
                resultBufferR = new float[length];
                iterationBufferR = new float[length];
                s11R = new float[length];
                s12R = new float[length];
                s22R = new float[length];
                s13R = new float[length];
                s23R = new float[length];
                s33R = new float[length];
            }

            if (useGreen) {
                resultBufferG = new float[length];
                iterationBufferG = new float[length];
                s11G = new float[length];
                s12G = new float[length];
                s22G = new float[length];
                s13G = new float[length];
                s23G = new float[length];
                s33G = new float[length];
            }

            if (useBlue) {
                resultBufferB = new float[length];
                iterationBufferB = new float[length];
                s11B = new float[length];
                s12B = new float[length];
                s22B = new float[length];
                s13B = new float[length];
                s23B = new float[length];
                s33B = new float[length];
            }

            dx2 = new float[length];
            dy2 = new float[length];
            dxdy = new float[length];
            dz2 = new float[length];
            dxdz = new float[length];
            dydz = new float[length];
            s11 = new float[length];
            s12 = new float[length];
            s22 = new float[length];
            s13 = new float[length];
            s23 = new float[length];
            s33 = new float[length];
            d11 = new float[length];
            d12 = new float[length];
            d22 = new float[length];
            d13 = new float[length];
            d23 = new float[length];
            d33 = new float[length];
            a = d11;
            b = d12;
            c = d13;
            d = d22;
            e = d23;
            f = d33;
        } catch (OutOfMemoryError er) {
            resultBufferR = iterationBufferR = null;
            resultBufferG = iterationBufferG = null;
            resultBufferB = iterationBufferB = null;
            dx2 = dy2 = dxdy = dz2 = dxdz = dydz = null;
            s11 = s12 = s22 = s13 = s23 = s33 = null;
            s11R = s12R = s22R = s13R = s23R = s33R = null;
            s11G = s12G = s22G = s13G = s23G = s33G = null;
            s11B = s12B = s22B = s13B = s23B = s33B = null;
            d11 = d12 = d22 = d13 = d23 = d33 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: Out of memory when creating image buffers", true);

            return;
        } // end try{}-catch{}

        // initialize iterationBuffer with data
        try {

            if (useRed) {
                srcImage.exportRGBData(1, 0, length, iterationBufferR);
            }

            if (useGreen) {
                srcImage.exportRGBData(2, 0, length, iterationBufferG);
            }

            if (useBlue) {
                srcImage.exportRGBData(3, 0, length, iterationBufferB);
            }
        } catch (IOException error) {
            resultBufferR = iterationBufferR = null;
            resultBufferG = iterationBufferG = null;
            resultBufferB = iterationBufferB = null;
            dx2 = dy2 = dxdy = dz2 = dxdz = dydz = null;
            s11 = s12 = s22 = s13 = s23 = s33 = null;
            s11R = s12R = s22R = s13R = s23R = s33R = null;
            s11G = s12G = s22G = s13G = s23G = s33G = null;
            s11B = s12B = s22B = s13B = s23B = s33B = null;
            d11 = d12 = d22 = d13 = d23 = d33 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT export source image", true);

            return;
        } // end try{}-catch{}

        // copy the iterationBuffer to the resultBuffer to initialize the first iteration update
        if (useRed) {
            System.arraycopy(iterationBufferR, 0, resultBufferR, 0, length);
        }

        if (useGreen) {
            System.arraycopy(iterationBufferG, 0, resultBufferG, 0, length);
        }

        if (useBlue) {
            System.arraycopy(iterationBufferB, 0, resultBufferB, 0, length);
        }

        // iterate diffusion
        for (int iterNum = 1; iterNum <= numIterations; iterNum++) {

            if (useRed) {
                computeDx3D(dx2, iterationBufferR);
                computeDy3D(dy2, iterationBufferR);
                computeDz(dz2, iterationBufferR);

                // compute all elements of the structure tensor
                for (int idx = 0; idx < length; idx++) {
                    dxdy[idx] = dx2[idx] * dy2[idx];
                    dxdz[idx] = dx2[idx] * dz2[idx];
                    dydz[idx] = dy2[idx] * dz2[idx];
                    dx2[idx] *= dx2[idx];
                    dy2[idx] *= dy2[idx];
                    dz2[idx] *= dz2[idx];
                } // end for (int idx = 0; ...)

                // gaussian smooth the structure tensor
                gaussianSmooth3D(s11R, dx2);
                gaussianSmooth3D(s22R, dy2);
                gaussianSmooth3D(s12R, dxdy);
                gaussianSmooth3D(s13R, dxdz);
                gaussianSmooth3D(s23R, dydz);
                gaussianSmooth3D(s33R, dz2);
            } // if (useRed)

            if (useGreen) {
                computeDx3D(dx2, iterationBufferG);
                computeDy3D(dy2, iterationBufferG);
                computeDz(dz2, iterationBufferG);

                // compute all elements of the structure tensor
                for (int idx = 0; idx < length; idx++) {
                    dxdy[idx] = dx2[idx] * dy2[idx];
                    dxdz[idx] = dx2[idx] * dz2[idx];
                    dydz[idx] = dy2[idx] * dz2[idx];
                    dx2[idx] *= dx2[idx];
                    dy2[idx] *= dy2[idx];
                    dz2[idx] *= dz2[idx];
                } // end for (int idx = 0; ...)

                // gaussian smooth the structure tensor
                gaussianSmooth3D(s11G, dx2);
                gaussianSmooth3D(s22G, dy2);
                gaussianSmooth3D(s12G, dxdy);
                gaussianSmooth3D(s13G, dxdz);
                gaussianSmooth3D(s23G, dydz);
                gaussianSmooth3D(s33G, dz2);
            } // if (useGreen)

            if (useBlue) {
                computeDx3D(dx2, iterationBufferB);
                computeDy3D(dy2, iterationBufferB);
                computeDz(dz2, iterationBufferB);

                // compute all elements of the structure tensor
                for (int idx = 0; idx < length; idx++) {
                    dxdy[idx] = dx2[idx] * dy2[idx];
                    dxdz[idx] = dx2[idx] * dz2[idx];
                    dydz[idx] = dy2[idx] * dz2[idx];
                    dx2[idx] *= dx2[idx];
                    dy2[idx] *= dy2[idx];
                    dz2[idx] *= dz2[idx];
                } // end for (int idx = 0; ...)

                // gaussian smooth the structure tensor
                gaussianSmooth3D(s11B, dx2);
                gaussianSmooth3D(s22B, dy2);
                gaussianSmooth3D(s12B, dxdy);
                gaussianSmooth3D(s13B, dxdz);
                gaussianSmooth3D(s23B, dydz);
                gaussianSmooth3D(s33B, dz2);
            } // if (useBlue)

            for (i = 0; i < length; i++) {
                s11[i] = 0.0f;
                s12[i] = 0.0f;
                s22[i] = 0.0f;
                s13[i] = 0.0f;
                s23[i] = 0.0f;
                s33[i] = 0.0f;

                if (useRed) {
                    s11[i] = s11R[i];
                    s12[i] = s12R[i];
                    s22[i] = s22R[i];
                    s13[i] = s13R[i];
                    s23[i] = s23R[i];
                    s33[i] = s33R[i];
                }

                if (useGreen) {
                    s11[i] += s11G[i];
                    s12[i] += s12G[i];
                    s22[i] += s22G[i];
                    s13[i] += s13G[i];
                    s23[i] += s23G[i];
                    s33[i] += s33G[i];
                }

                if (useBlue) {
                    s11[i] += s11B[i];
                    s12[i] += s12B[i];
                    s22[i] += s22B[i];
                    s13[i] += s13B[i];
                    s23[i] += s23B[i];
                    s33[i] += s33B[i];
                }

                s11[i] /= colorsPresent;
                s12[i] /= colorsPresent;
                s22[i] /= colorsPresent;
                s13[i] /= colorsPresent;
                s23[i] /= colorsPresent;
                s33[i] /= colorsPresent;
            }

            // structure tensor is:
            // +--             --+
            // | s11   s12   s13 |
            // |                 |
            // | s12   s22   s23 |
            // |                 |
            // | s13   s23   s33 |
            // +--             --+

            // Compute the eigenvalues of the structure tensor and elements of the
            // diffusion tensor
            float exponent;
            float c2 = 0.01f;
            float c3 = 0.01f;
            int idx = 0;
            float LcpmR = 0.0f;
            float LmcmR = 0.0f;
            float LccmR = 0.0f;
            float LpcmR = 0.0f;
            float LcmmR = 0.0f;
            float LmpcR = 0.0f;
            float LcpcR = 0.0f;
            float LppcR = 0.0f;
            float LmccR = 0.0f;
            float LpccR = 0.0f;
            float LmmcR = 0.0f;
            float LcmcR = 0.0f;
            float LpmcR = 0.0f;
            float LcppR = 0.0f;
            float LmcpR = 0.0f;
            float LccpR = 0.0f;
            float LpcpR = 0.0f;
            float LcmpR = 0.0f;
            float LcpmG = 0.0f;
            float LmcmG = 0.0f;
            float LccmG = 0.0f;
            float LpcmG = 0.0f;
            float LcmmG = 0.0f;
            float LmpcG = 0.0f;
            float LcpcG = 0.0f;
            float LppcG = 0.0f;
            float LmccG = 0.0f;
            float LpccG = 0.0f;
            float LmmcG = 0.0f;
            float LcmcG = 0.0f;
            float LpmcG = 0.0f;
            float LcppG = 0.0f;
            float LmcpG = 0.0f;
            float LccpG = 0.0f;
            float LpcpG = 0.0f;
            float LcmpG = 0.0f;
            float LcpmB = 0.0f;
            float LmcmB = 0.0f;
            float LccmB = 0.0f;
            float LpcmB = 0.0f;
            float LcmmB = 0.0f;
            float LmpcB = 0.0f;
            float LcpcB = 0.0f;
            float LppcB = 0.0f;
            float LmccB = 0.0f;
            float LpccB = 0.0f;
            float LmmcB = 0.0f;
            float LcmcB = 0.0f;
            float LpmcB = 0.0f;
            float LcppB = 0.0f;
            float LmcpB = 0.0f;
            float LccpB = 0.0f;
            float LpcpB = 0.0f;
            float LcmpB = 0.0f;
            float amcc, apcc;
            float bmcc, bcpc, bpcc, bcmc;
            float cmcc, cccm, cpcc, cccp;
            float dcpc, dcmc;
            float ecpc, eccm, ecmc, eccp;
            float fccm, fccp;
            float r;

            // make a 3X3 eigenSolver
            eigenSystemAlgo = new Eigenf(3);

            double[][] evecs = new double[3][3];
            Matrix rot;
            Matrix cMat = new Matrix(3, 3);
            // cArray is a reference to cMat's internal array.
            double[][] cArray = cMat.getArray();

            for (i = 0; i <= 2; i++) {

                for (j = 0; j <= 2; j++) {
                    cArray[i][j] = 0.0;
                }
            }

            Matrix dMat;

            for (int z = 0; z < zDim; z++) {
                fireProgressStateChanged(((((iterNum - 1) * zDim) + z) * 100 / (numIterations * zDim)), null, null);

                for (int y = 0; y < yDim; y++) {

                    for (int x = 0; x < xDim; x++, idx++) {

                        if (entireImage || mask.get(idx)) {
                            eigenSystemAlgo.SetData(0, 0, s11[idx]);
                            eigenSystemAlgo.SetData(0, 1, s12[idx]);
                            eigenSystemAlgo.SetData(0, 2, s13[idx]);

                            eigenSystemAlgo.SetData(1, 0, s12[idx]);
                            eigenSystemAlgo.SetData(1, 1, s22[idx]);
                            eigenSystemAlgo.SetData(1, 2, s23[idx]);

                            eigenSystemAlgo.SetData(2, 0, s13[idx]);
                            eigenSystemAlgo.SetData(2, 1, s23[idx]);
                            eigenSystemAlgo.SetData(2, 2, s33[idx]);

                            // OK, solve the eigen system
                            eigenSystemAlgo.IncrSortEigenStuff();

                            // extract the eigen values from the AlgorithmEigensolver
                            // The eigenvalues are stored in increasing order and may be
                            // accessed by getEigenvalue(i).  The corresponding eigenvectors
                            // are stored as columns of a matrix.  The eigenvector for the
                            // i-th eigenvalue is stored in the i-th column of the matrix.
                            // Eigenvectors are accessed by getEigenvector(iRow,iCol).
                            // The eigenvectors are normalized to unit length.
                            // lambdaOne is the largest eigenvalue.
                            lambdaOne = eigenSystemAlgo.GetEigenvalue(2);
                            lambdaTwo = eigenSystemAlgo.GetEigenvalue(1);
                            lambdaThree = eigenSystemAlgo.GetEigenvalue(0);

                            evecs[0][0] = eigenSystemAlgo.GetEigenvector(0, 2);
                            evecs[1][0] = eigenSystemAlgo.GetEigenvector(1, 2);
                            evecs[2][0] = eigenSystemAlgo.GetEigenvector(2, 2);

                            evecs[0][1] = eigenSystemAlgo.GetEigenvector(0, 1);
                            evecs[1][1] = eigenSystemAlgo.GetEigenvector(1, 1);
                            evecs[2][1] = eigenSystemAlgo.GetEigenvector(2, 1);

                            evecs[0][2] = eigenSystemAlgo.GetEigenvector(0, 0);
                            evecs[1][2] = eigenSystemAlgo.GetEigenvector(1, 0);
                            evecs[2][2] = eigenSystemAlgo.GetEigenvector(2, 0);
                            
                            // The columns of the rotation matrix are the eigenvectors
                            // of the structure tensor.
                            rot = new Matrix(evecs);

                            exponent = ((((lambdaOne - lambdaTwo) * (lambdaOne - lambdaTwo)) +
                                         ((lambdaOne - lambdaThree) * (lambdaOne - lambdaThree)) +
                                         ((lambdaTwo - lambdaThree) * (lambdaTwo - lambdaThree))) / (float) k2);

                            c1 = max(0.01f, (float) (1.0 - Math.exp(-exponent)));
                            cArray[0][0] = c1;
                            cArray[1][1] = c2;
                            cArray[2][2] = c3;
                            
                            dMat = (rot.transpose().times(cMat)).times(rot);

                            d11[idx] = (float) dMat.get(0, 0);
                            d12[idx] = (float) dMat.get(0, 1);
                            d22[idx] = (float) dMat.get(1, 1);
                            d13[idx] = (float) dMat.get(0, 2);
                            d23[idx] = (float) dMat.get(1, 2);
                            d33[idx] = (float) dMat.get(2, 2);

                            if (useRed) {
                                LcpmR = getVal(iterationBufferR, x, y + 1, z - 1);
                                LmcmR = getVal(iterationBufferR, x - 1, y, z - 1);
                                LccmR = getVal(iterationBufferR, x, y, z - 1);
                                LpcmR = getVal(iterationBufferR, x + 1, y, z - 1);
                                LcmmR = getVal(iterationBufferR, x, y - 1, z - 1);

                                LmpcR = getVal(iterationBufferR, x - 1, y + 1, z);
                                LcpcR = getVal(iterationBufferR, x, y + 1, z);
                                LppcR = getVal(iterationBufferR, x + 1, y + 1, z);
                                LmccR = getVal(iterationBufferR, x - 1, y, z);
                                LpccR = getVal(iterationBufferR, x + 1, y, z);
                                LmmcR = getVal(iterationBufferR, x - 1, y - 1, z);
                                LcmcR = getVal(iterationBufferR, x, y - 1, z);
                                LpmcR = getVal(iterationBufferR, x + 1, y - 1, z);

                                LcppR = getVal(iterationBufferR, x, y + 1, z + 1);
                                LmcpR = getVal(iterationBufferR, x - 1, y, z + 1);
                                LccpR = getVal(iterationBufferR, x, y, z + 1);
                                LpcpR = getVal(iterationBufferR, x + 1, y, z + 1);
                                LcmpR = getVal(iterationBufferR, x, y - 1, z + 1);
                            } // if (useRed)

                            if (useGreen) {
                                LcpmG = getVal(iterationBufferG, x, y + 1, z - 1);
                                LmcmG = getVal(iterationBufferG, x - 1, y, z - 1);
                                LccmG = getVal(iterationBufferG, x, y, z - 1);
                                LpcmG = getVal(iterationBufferG, x + 1, y, z - 1);
                                LcmmG = getVal(iterationBufferG, x, y - 1, z - 1);

                                LmpcG = getVal(iterationBufferG, x - 1, y + 1, z);
                                LcpcG = getVal(iterationBufferG, x, y + 1, z);
                                LppcG = getVal(iterationBufferG, x + 1, y + 1, z);
                                LmccG = getVal(iterationBufferG, x - 1, y, z);
                                LpccG = getVal(iterationBufferG, x + 1, y, z);
                                LmmcG = getVal(iterationBufferG, x - 1, y - 1, z);
                                LcmcG = getVal(iterationBufferG, x, y - 1, z);
                                LpmcG = getVal(iterationBufferG, x + 1, y - 1, z);

                                LcppG = getVal(iterationBufferG, x, y + 1, z + 1);
                                LmcpG = getVal(iterationBufferG, x - 1, y, z + 1);
                                LccpG = getVal(iterationBufferG, x, y, z + 1);
                                LpcpG = getVal(iterationBufferG, x + 1, y, z + 1);
                                LcmpG = getVal(iterationBufferG, x, y - 1, z + 1);
                            } // if (useGreen)

                            if (useBlue) {
                                LcpmB = getVal(iterationBufferB, x, y + 1, z - 1);
                                LmcmB = getVal(iterationBufferB, x - 1, y, z - 1);
                                LccmB = getVal(iterationBufferB, x, y, z - 1);
                                LpcmB = getVal(iterationBufferB, x + 1, y, z - 1);
                                LcmmB = getVal(iterationBufferB, x, y - 1, z - 1);

                                LmpcB = getVal(iterationBufferB, x - 1, y + 1, z);
                                LcpcB = getVal(iterationBufferB, x, y + 1, z);
                                LppcB = getVal(iterationBufferB, x + 1, y + 1, z);
                                LmccB = getVal(iterationBufferB, x - 1, y, z);
                                LpccB = getVal(iterationBufferB, x + 1, y, z);
                                LmmcB = getVal(iterationBufferB, x - 1, y - 1, z);
                                LcmcB = getVal(iterationBufferB, x, y - 1, z);
                                LpmcB = getVal(iterationBufferB, x + 1, y - 1, z);

                                LcppB = getVal(iterationBufferB, x, y + 1, z + 1);
                                LmcpB = getVal(iterationBufferB, x - 1, y, z + 1);
                                LccpB = getVal(iterationBufferB, x, y, z + 1);
                                LpcpB = getVal(iterationBufferB, x + 1, y, z + 1);
                                LcmpB = getVal(iterationBufferB, x, y - 1, z + 1);
                            } // if (useBlue)

                            amcc = getVal(a, x - 1, y, z);
                            apcc = getVal(a, x + 1, y, z);

                            bmcc = getVal(b, x - 1, y, z);
                            bcpc = getVal(b, x, y + 1, z);
                            bpcc = getVal(b, x + 1, y, z);
                            bcmc = getVal(b, x, y - 1, z);

                            cmcc = getVal(c, x - 1, y, z);
                            cccm = getVal(c, x, y, z - 1);
                            cpcc = getVal(c, x + 1, y, z);
                            cccp = getVal(c, x, y, z + 1);

                            dcpc = getVal(d, x, y + 1, z);
                            dcmc = getVal(d, x, y - 1, z);

                            ecpc = getVal(e, x, y + 1, z);
                            eccm = getVal(e, x, y, z - 1);
                            ecmc = getVal(e, x, y - 1, z);
                            eccp = getVal(e, x, y, z + 1);

                            fccm = getVal(f, x, y, z - 1);
                            fccp = getVal(f, x, y, z + 1);

                            if (useRed) {
                                r = (-0.25f * (ecpc + eccm) * LcpmR) + (0.25f * (cmcc + cccm) * LmcmR) +
                                    (0.50f * (fccm + f[idx]) * LccmR) - (0.25f * (cpcc + cccm) * LpcmR) +
                                    (0.25f * (ecmc + eccm) * LcmmR) - (0.25f * (bmcc + bcpc) * LmpcR) +
                                    (0.50f * (d[idx] + dcpc) * LcpcR) + (0.25f * (bpcc + bcpc) * LppcR) +
                                    (0.50f * (amcc + a[idx]) * LmccR) -
                                    (0.50f *
                                         (apcc + amcc + (2 * a[idx]) + dcpc + dcmc + (2 * d[idx]) + fccp + fccm +
                                              (2 * f[idx])) * iterationBufferR[idx]) +
                                    (0.50f * (a[idx] + apcc) * LpccR) + (0.25f * (bmcc + bcmc) * LmmcR) +
                                    (0.50f * (d[idx] + dcmc) * LcmcR) - (0.25f * (bpcc + bcmc) * LpmcR) +
                                    (0.25f * (ecpc + eccp) * LcppR) - (0.25f * (cmcc + cccp) * LmcpR) +
                                    (0.50f * (f[idx] + fccp) * LccpR) + (0.25f * (cpcc + cccp) * LpcpR) -
                                    (0.25f * (ecmc + eccp) * LcmpR);

                                resultBufferR[idx] += (timeStep * r);
                            } // if (useRed)

                            if (useGreen) {
                                r = (-0.25f * (ecpc + eccm) * LcpmG) + (0.25f * (cmcc + cccm) * LmcmG) +
                                    (0.50f * (fccm + f[idx]) * LccmG) - (0.25f * (cpcc + cccm) * LpcmG) +
                                    (0.25f * (ecmc + eccm) * LcmmG) - (0.25f * (bmcc + bcpc) * LmpcG) +
                                    (0.50f * (d[idx] + dcpc) * LcpcG) + (0.25f * (bpcc + bcpc) * LppcG) +
                                    (0.50f * (amcc + a[idx]) * LmccG) -
                                    (0.50f *
                                         (apcc + amcc + (2 * a[idx]) + dcpc + dcmc + (2 * d[idx]) + fccp + fccm +
                                              (2 * f[idx])) * iterationBufferG[idx]) +
                                    (0.50f * (a[idx] + apcc) * LpccG) + (0.25f * (bmcc + bcmc) * LmmcG) +
                                    (0.50f * (d[idx] + dcmc) * LcmcG) - (0.25f * (bpcc + bcmc) * LpmcG) +
                                    (0.25f * (ecpc + eccp) * LcppG) - (0.25f * (cmcc + cccp) * LmcpG) +
                                    (0.50f * (f[idx] + fccp) * LccpG) + (0.25f * (cpcc + cccp) * LpcpG) -
                                    (0.25f * (ecmc + eccp) * LcmpG);

                                resultBufferG[idx] += (timeStep * r);
                            } // if (useGreen)

                            if (useBlue) {
                                r = (-0.25f * (ecpc + eccm) * LcpmB) + (0.25f * (cmcc + cccm) * LmcmB) +
                                    (0.50f * (fccm + f[idx]) * LccmB) - (0.25f * (cpcc + cccm) * LpcmB) +
                                    (0.25f * (ecmc + eccm) * LcmmB) - (0.25f * (bmcc + bcpc) * LmpcB) +
                                    (0.50f * (d[idx] + dcpc) * LcpcB) + (0.25f * (bpcc + bcpc) * LppcB) +
                                    (0.50f * (amcc + a[idx]) * LmccB) -
                                    (0.50f *
                                         (apcc + amcc + (2 * a[idx]) + dcpc + dcmc + (2 * d[idx]) + fccp + fccm +
                                              (2 * f[idx])) * iterationBufferB[idx]) +
                                    (0.50f * (a[idx] + apcc) * LpccB) + (0.25f * (bmcc + bcmc) * LmmcB) +
                                    (0.50f * (d[idx] + dcmc) * LcmcB) - (0.25f * (bpcc + bcmc) * LpmcB) +
                                    (0.25f * (ecpc + eccp) * LcppB) - (0.25f * (cmcc + cccp) * LmcpB) +
                                    (0.50f * (f[idx] + fccp) * LccpB) + (0.25f * (cpcc + cccp) * LpcpB) -
                                    (0.25f * (ecmc + eccp) * LcmpB);

                                resultBufferB[idx] += (timeStep * r);
                            } // if (useBlue)
                        }
                    }
                }
            }

            // update the iterationBuffer for the next pass
            if (useRed) {
                System.arraycopy(resultBufferR, 0, iterationBufferR, 0, length);
            }

            if (useGreen) {
                System.arraycopy(resultBufferG, 0, iterationBufferG, 0, length);
            }

            if (useBlue) {
                System.arraycopy(resultBufferB, 0, iterationBufferB, 0, length);
            }
        } // end for (int iterNum = 1; ...)

        try {

            if (useRed) {

                for (i = 0; i < length; i++) {

                    if ((resultBufferR[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                        resultBufferR[i] = 255.0f;
                    } else if (resultBufferR[i] < 0.0f) {
                        resultBufferR[i] = 0.0f;
                    }
                }

                destImage.importRGBData(1, 0, resultBufferR, true);
            }

            if (useGreen) {

                for (i = 0; i < length; i++) {

                    if ((resultBufferG[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                        resultBufferG[i] = 255.0f;
                    } else if (resultBufferG[i] < 0.0f) {
                        resultBufferG[i] = 0.0f;
                    }
                }

                destImage.importRGBData(2, 0, resultBufferG, true);
            }

            if (useBlue) {

                for (i = 0; i < length; i++) {

                    if ((resultBufferB[i] > 255.0f) && (destImage.getType() == ModelStorageBase.ARGB)) {
                        resultBufferB[i] = 255.0f;
                    } else if (resultBufferB[i] < 0.0f) {
                        resultBufferB[i] = 0.0f;
                    }
                }

                destImage.importRGBData(3, 0, resultBufferB, true);
            }
        } catch (IOException error) {
            resultBufferR = iterationBufferR = null;
            resultBufferG = iterationBufferG = null;
            resultBufferB = iterationBufferB = null;
            dx2 = dy2 = dxdy = dz2 = dxdz = dydz = null;
            s11 = s12 = s22 = s13 = s23 = s33 = null;
            s11R = s12R = s22R = s13R = s23R = s33R = null;
            s11G = s12G = s22G = s13G = s23G = s33G = null;
            s11B = s12B = s22B = s13B = s23B = s33B = null;
            d11 = d12 = d22 = d13 = d23 = d33 = null;
            errorCleanUp("AlgorithmCoherenceEnhancingDiffusion: could NOT import dest image", true);

            return;
        } // end try{}-catch{}

        fireProgressStateChanged(100, null, null);
        
        destImage.calcMinMax();

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    } // end run3DC(...)

    /**
     * Sets the value in a 2D image buffer at location row, col.
     *
     * @param  val     float The value to set the pixel to
     * @param  buffer  float[] 2D image buffer
     * @param  col     int Column index
     * @param  row     int Row index
     */
    private void setVal(float val, float[] buffer, int col, int row) {

        if ((col >= 0) && (col < xDim) && (row >= 0) && (row < yDim)) {
            buffer[(row * xDim) + col] = val;
        }
    } // end setVal(...)

    /**
     * Sets the value in a 3D image buffer at location row, col.
     *
     * @param  val     float The value to set the pixel to
     * @param  buffer  float[] 3D image buffer
     * @param  col     int Column index
     * @param  row     int Row index
     * @param  slice   int Slice index
     */
    private void setVal(float val, float[] buffer, int col, int row, int slice) {

        if ((col >= 0) && (col < xDim) && (row >= 0) && (row < yDim) && (slice >= 0) && (slice < zDim)) {
            buffer[(slice * yDim * xDim) + (row * xDim) + col] = val;
        }
    } // end setVal(...)

} // end class AlgorithmCoherenceEnhancingDiffusion
