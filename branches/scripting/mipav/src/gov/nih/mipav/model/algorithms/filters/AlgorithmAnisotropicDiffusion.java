package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * This algorithm anisotropically diffuses an image. That is to say it blurs over regions of an image where the gradient
 * mag. is relatively small but diffuses little over areas of the image where the gradient mag is large (i.e. edges).
 * Therefore, objects are blurred but not edges of objects. K is a factor that controls the diffusion rate. A large
 * value causes this algorithm to act like gaussian smoothing where the number of iterations is related to the sigma of
 * the gaussian. Small values of k reduce the diffusion across edges.
 *
 * <p>The basic equation is the I(x,y,z,t+1) = I(x,y,z,t) + lambda*c(x,y,z,t)*Laplacian(I(x,y,z,t)) I(x,y,z,t) is the
 * brightness function. It is required that 0 <= lambda <= 0.25 for numerical stability. In this program lambda = 0.25
 * is used. The conduction coefficient c(x,y,z,t) is a function of the magnitude of the edge. That is, c = g(||E||). The
 * gradient of the brightness function gives an excellent estimate of the edge, E(x,y,z,t) = grad(I(x,y,z,t)). Thus, c =
 * g (||grad(I(x,y,z,t))||). In this program g is selected so that c = 1/(1 + square(||grad(I(x,y,z,t))||/K))</p>
 *
 * <p>This program uses the a constant times the square root of the sum of the squares of the normalized first order
 * derivatives of the Gaussian function as an approximate gradient function. The constant is selected so that the
 * maximium value of this approximation to the gradient function is 100.</p>
 *
 * @version  0.1 March 6, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class AlgorithmAnisotropicDiffusion extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Storage location of the first derivative of the Gaussian in the X direction. */
    private float[] GxData;

    /** Storage location of the first derivative of the Gaussian in the Y direction. */
    private float[] GyData;

    /** Storage location of the first derivative of the Gaussian in the Z direction. */
    private float[] GzData;

    /** Number of iterations of the diffusion. */
    private int iterations;


    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** Dimensionality fo the Laplacian kernel. */
    private int[] klapExtents;

    /**
     * Controls the diffusion rate. A large value causes this algorithms to act like gaussian smoothing and therefore
     * diffuses across edges. K = small reduces blur across edges. Typical K = 5.
     */
    private float konsnt = 5;


    /** Storage location of the Laplacian data. */
    private float[] lapData;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmAnisotropicDiffusion object.
     *
     * @param  srcImg    reference to the source image
     * @param  sigmas    sigmas used to describe the gaussian that is used in the calculation of the gradient magnitude
     * @param  iter      number of iterations (t) of the diffusion equation
     * @param  kValue    K is a factor that controls the diffusion rate. A large value causes this algorithms to act
     *                   like gaussian smoothing and therefore diffuses across edges. K = small reduces blur across
     *                   edges. Typical K = 5;
     * @param  maskFlag  Flag that indicates that the anisotropic diffusion will be performed for the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmAnisotropicDiffusion(ModelImage srcImg, float[] sigmas, int iter, float kValue, boolean maskFlag,
                                         boolean img25D, int minProgressValue, int maxProgressValue) {
        super(null, srcImg, minProgressValue, maxProgressValue);

        this.sigmas = sigmas;
        iterations = iter;
        this.konsnt = kValue;
        entireImage = maskFlag;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Creates a new AlgorithmAnisotropicDiffusion object.
     *
     * @param  destImg   Reference to destination image.
     * @param  srcImg    Reference to the source image.
     * @param  sigmas    Sigmas used to describe the gaussian that is used in the calculation of the gradient magnitude.
     * @param  iter      Number of iterations (t) of the diffusion equation
     * @param  kValue    K is a factor that controls the diffusion rate. A large value causes this algorithms to act
     *                   like gaussian smoothing and therefore diffuses across edges. K = small reduces blur across
     *                   edges. Typical K = 5.
     * @param  maskFlag  Flag that indicates that the anisotropic diffusion will be performed for the whole image if
     *                   equal to true.
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmAnisotropicDiffusion(ModelImage destImg, ModelImage srcImg, float[] sigmas, int iter, float kValue,
                                         boolean maskFlag, boolean img25D, int minProgressValue, int maxProgressValue) {
        super(destImg, srcImg, minProgressValue, maxProgressValue);

        this.sigmas = sigmas; // Sigmas used to calculate the gradient magnitude
        iterations = iter;
        this.konsnt = kValue;
        entireImage = maskFlag;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets class storages arrays to null so that System.gc() can free the memory.
     */
    public void finalize() {
        GxData = null;
        GyData = null;
        GzData = null;
        lapData = null;
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

        constructLog();

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
     * Calculates the diffused image and replaces the source image with the diffused image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace2D(int nImages) {

        int i, n, s;
        int length, totalLength;
        int start;
        float[] imgBuffer;
        float[] tempBuffer;
        float[] resultBuffer;
        float[] finalBuffer;
        float[] gmBuffer;
        float gmMax, gmMin;
        float val;
        float ix, iy, mag;

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            imgBuffer = new float[length];
            resultBuffer = new float[length];
            finalBuffer = new float[totalLength];
            gmBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            imgBuffer = null;
            resultBuffer = null;
            finalBuffer = null;
            gmBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Out of Memory", true);

            return;
        }


        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Diffusing image ...");
        
        for (s = 0; s < nImages; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, imgBuffer); // locks and releases lock
            } catch (IOException error) {
                imgBuffer = null;
                resultBuffer = null;
                finalBuffer = null;
                gmBuffer = null;
                errorCleanUp("Algorithm Anisotropic Diffusion: Image(s) locked", true);

                return;
            }

            for (n = 0; (n < iterations) && !threadStopped; n++) {

                fireProgressStateChanged(getProgressFromFloat((float) ((s * iterations) + n) / ((nImages * iterations) - 1)), 
                        srcImage.getImageName(), "Diffusing image ...");
                

                // Normalize GM !!
                for (i = 0, gmMax = 0, gmMin = Float.MAX_VALUE; i < length; i++) {

                    if ((entireImage == true) || mask.get(i)) {
                        ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GxData);
                        iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GyData);

                        gmBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy));

                        if (gmBuffer[i] > gmMax) {
                            gmMax = gmBuffer[i];
                        } else if (gmBuffer[i] < gmMin) {
                            gmMin = gmBuffer[i];
                        }
                    }
                }

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if ((entireImage == true) || mask.get(i)) {

                        mag = ((gmBuffer[i] - gmMin) / (gmMax - gmMin)) * 100; // normalized between 0 - 100
                        mag = (float) (1 / (1 + (mag / konsnt * mag / konsnt)));
                        val = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, klapExtents,
                                                              lapData);

                        val = val * mag * 0.25f;
                        resultBuffer[i] = val + imgBuffer[i];
                    } else {
                        resultBuffer[i] = imgBuffer[i];
                    }
                }

                tempBuffer = imgBuffer;
                imgBuffer = resultBuffer;
                resultBuffer = tempBuffer;
            }

            for (i = 0; i < length; i++) {
                finalBuffer[start + i] = imgBuffer[i];
            }
        }

        resultBuffer = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, finalBuffer, true);
        } catch (IOException error) {
            imgBuffer = null;
            resultBuffer = null;
            finalBuffer = null;
            gmBuffer = null;
            errorCleanUp(error + "AnisotropicDiffusion: Image(s) locked", true);

            return;
        }

        fireProgressStateChanged(maxProgressValue, srcImage.getImageName(), "Diffusing image ...");
        setCompleted(true);
    }

    /**
     * Calculates the diffused image and replaces the source image with the diffused image.
     */
    private void calcInPlace3D() {

        int i, n;
        int length, offset;
        float[] imgBuffer;
        float[] tempBuffer;
        float[] resultBuffer;
        float[] gmBuffer;
        float gmMax, gmMin;
        float val, edgeFunct;
        float ix, iy, iz, mag;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            offset = srcImage.getSliceSize();
            imgBuffer = new float[length];
            resultBuffer = new float[length];
            gmBuffer = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Out of memory", true);

            return;
        }

       // initProgressBar();

        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Diffusing image ...");
        
        for (n = 0; (n < iterations) && !threadStopped; n++) {

            // Normalize GM !!
            for (i = 0, gmMax = 0, gmMin = Float.MAX_VALUE; i < length; i++) {

                    int s = i / offset;
                    
                    if ((i % offset) == 0) {
                        fireProgressStateChanged(getProgressFromInt(Math.round((float) (50.0 * s) / (iterations * srcImage.getExtents()[2])) +
                                Math.round(100 * ((float) (n)) / iterations)), 
                                srcImage.getImageName(), "Diffusing image ...");
                    }


                if ((entireImage == true) || mask.get(i)) {
                    ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GxData);
                    iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GyData);
                    iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GzData);

                    gmBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));

                    if (gmBuffer[i] > gmMax) {
                        gmMax = gmBuffer[i];
                    } else if (gmBuffer[i] < gmMin) {
                        gmMin = gmBuffer[i];
                    }
                }
            }

            // two speed ups
            // mult by 1/konsnt
            float invK = 1 / konsnt;
            float normMag = 100 / (gmMax - gmMin);

            for (i = 0; (i < length) && !threadStopped; i++) {

                    int s = i / offset;

                    if ((i % offset) == 0) {
                        
                        fireProgressStateChanged(getProgressFromInt(Math.round((float) (50.0 * s) / (iterations * srcImage.getExtents()[2])) +
                                Math.round(100 * ((float) (n)) / iterations) +
                                Math.round((float) (50.0) / (iterations))), 
                                srcImage.getImageName(), "Diffusing image ...");
                        
                    }

               

                if ((entireImage == true) || mask.get(i)) {

                    // mag = ((gmBuffer[i]-gmMin)/(gmMax-gmMin)) * 100;
                    mag = (gmBuffer[i] - gmMin) * normMag;

                    // edgeFunct = (float)( 1/(1 + mag/konsnt * mag/konsnt));
                    edgeFunct = (float) (1 / (1 + (mag * invK * mag * invK)));

                    val = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, klapExtents, lapData);

                    val = val * edgeFunct * 0.25f;
                    resultBuffer[i] = val + imgBuffer[i];
                } else {
                    resultBuffer[i] = imgBuffer[i];
                }
            }

            tempBuffer = imgBuffer;
            imgBuffer = resultBuffer;
            resultBuffer = tempBuffer;
        }

        resultBuffer = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        try { // Add check to see if image is already float then don't reallocate

            // if (srcImage.getType() != ModelImage.FLOAT) {
            // srcImage.reallocate(ModelImage.FLOAT);
            // }
            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            errorCleanUp("AnisotropicDiffusion: Image(s) locked", true);

            return;
        }

        fireProgressStateChanged(maxProgressValue, srcImage.getImageName(), "Diffusing image ...");
        setCompleted(true);
    }

    /**
     * Calculates the diffused image and stores the resultant diffused image in the destination image model.
     */
    private void calcStoreInDest25D() {

        int i, n, z, offset;
        int length;
        float[] imgBuffer;
        float[] img3DBuffer;
        float[] tempBuffer;
        float[] resultBuffer;
        float[] gmBuffer;
        float gmMax, gmMin;
        float val, edgeFunct;
        float ix, iy, mag;

        try {
            length = srcImage.getSliceSize();
            imgBuffer = new float[length];
            gmBuffer = new float[length];
            resultBuffer = new float[length * srcImage.getExtents()[2]];
            img3DBuffer = new float[length * srcImage.getExtents()[2]];
            srcImage.exportData(0, length * srcImage.getExtents()[2], img3DBuffer); // locks and releases lock
        } catch (IOException error) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            img3DBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            img3DBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Out of Memory", true);

            return;
        }
        
        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Diffusing image ...");
        

        for (n = 0; (n < iterations) && !threadStopped; n++) {

            fireProgressStateChanged(getProgressFromFloat((float) n / (iterations - 1)), srcImage.getImageName(), "Diffusing image ...");
            

            for (z = 0; z < srcImage.getExtents()[2]; z++) {
                offset = z * length;
                System.arraycopy(img3DBuffer, offset, imgBuffer, 0, length);

                // Normalize GM !!
                for (i = 0, gmMax = 0, gmMin = Float.MAX_VALUE; i < length; i++) {

                    if ((entireImage == true) || mask.get(i)) {
                        ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GxData);
                        iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GyData);

                        gmBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy));

                        if (gmBuffer[i] > gmMax) {
                            gmMax = gmBuffer[i];
                        } else if (gmBuffer[i] < gmMin) {
                            gmMin = gmBuffer[i];
                        }
                    }
                }

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if ((entireImage == true) || mask.get(offset + i)) {

                        mag = ((gmBuffer[i] - gmMin) / (gmMax - gmMin)) * 100;
                        edgeFunct = (float) (1 / (1 + (mag / konsnt * mag / konsnt)));

                        val = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, klapExtents,
                                                              lapData);
                        val = val * edgeFunct * 0.25f;
                        resultBuffer[offset + i] = val + img3DBuffer[offset + i];
                    } else {
                        resultBuffer[offset + i] = img3DBuffer[offset + i];
                    }
                }
            }

            tempBuffer = img3DBuffer;
            img3DBuffer = resultBuffer;
            resultBuffer = tempBuffer;
        }

        resultBuffer = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            destImage.importData(0, img3DBuffer, true);
        } catch (IOException error) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            img3DBuffer = null;
            errorCleanUp(error + "Anisotropic Diffusion: Image(s) locked", true);

            return;
        }
        
        fireProgressStateChanged(maxProgressValue, srcImage.getImageName(), "Diffusing image ...");
        setCompleted(true);
    }

    /**
     * Calculates the diffused image and stores the resultant diffused image in the destination image model.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcStoreInDest2D(int nImages) {

        int i, n, s;
        int length;
        int start;
        float[] imgBuffer;
        float[] tempBuffer;
        float[] resultBuffer;
        float[] gmBuffer;
        float gmMax, gmMin;
        float val, edgeFunct;
        float ix, iy, mag;

        try {
            length = srcImage.getSliceSize();
            imgBuffer = new float[length];
            resultBuffer = new float[length];
            gmBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Out of Memory", true);

            return;
        }

        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Diffusing image ...");

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, imgBuffer); // locks and releases lock
            } catch (IOException error) {
                imgBuffer = null;
                resultBuffer = null;
                gmBuffer = null;
                errorCleanUp("Algorithm Anisotropic Diffusion: Image(s) locked", true);

                return;
            }

            for (n = 0; (n < iterations) && !threadStopped; n++) {

                fireProgressStateChanged(getProgressFromFloat((float) ((s * iterations) + n) / ((nImages * iterations) - 1)),
                            srcImage.getImageName(), "Diffusing image ...");
                    
                // Normalize GM !!

                for (i = 0, gmMax = 0, gmMin = Float.MAX_VALUE; i < length; i++) {

                    if ((entireImage == true) || mask.get(i)) {
                        ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GxData);
                        iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GyData);

                        gmBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy));

                        if (gmBuffer[i] > gmMax) {
                            gmMax = gmBuffer[i];
                        } else if (gmBuffer[i] < gmMin) {
                            gmMin = gmBuffer[i];
                        }
                    }
                }

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if ((entireImage == true) || mask.get(i)) {

                        mag = ((gmBuffer[i] - gmMin) / (gmMax - gmMin)) * 100;
                        edgeFunct = (float) (1 / (1 + (mag / konsnt * mag / konsnt)));

                        val = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), imgBuffer, klapExtents,
                                                              lapData);

                        val = val * edgeFunct * 0.25f;
                        resultBuffer[i] = val + imgBuffer[i];
                    } else {
                        resultBuffer[i] = imgBuffer[i];
                    }
                }

                tempBuffer = imgBuffer;
                imgBuffer = resultBuffer;
                resultBuffer = tempBuffer;
            }

            try {
                destImage.importData(start, imgBuffer, false);
            } catch (IOException error) {
                imgBuffer = null;
                resultBuffer = null;
                gmBuffer = null;
                errorCleanUp(error + "Anisotropic Diffusion: Image(s) locked", true);

                return;
            }
        }

        resultBuffer = null;
        tempBuffer = null;
        System.gc();

        fireProgressStateChanged(maxProgressValue, srcImage.getImageName(), "Diffusing image ...");
        
        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        setCompleted(true);
    }

    /**
     * Calculates the diffused image and stores the resultant diffused image in the destination image model.
     */
    private void calcStoreInDest3D() {

        int i, n;
        int length, offset;
        float[] imgBuffer;
        float[] tempBuffer;
        float[] resultBuffer;
        float[] gmBuffer;
        float gmMax, gmMin;
        float val, edgeFunct;
        float ix, iy, iz, mag;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            offset = srcImage.getSliceSize();
            imgBuffer = new float[length];
            resultBuffer = new float[length];
            gmBuffer = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            errorCleanUp("Anisotropic Diffusion: Out of Memory", true);

            return;
        }

        fireProgressStateChanged(minProgressValue, srcImage.getImageName(), "Diffusing image ...");

        for (n = 0; (n < iterations) && !threadStopped; n++) {

            // Normalize GM !!
            for (i = 0, gmMax = 0, gmMin = Float.MAX_VALUE; i < length; i++) {

                    int s = i / offset;
                 
                    if ((i % offset) == 0) {
                        fireProgressStateChanged(getProgressFromInt(Math.round((float) (50.0 * s) / (iterations * srcImage.getExtents()[2])) +
                                Math.round(100 * ((float) (n)) / iterations)), 
                                srcImage.getImageName(), "Diffusing image ...");
                    }
                

                if ((entireImage == true) || mask.get(i)) {
                    ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GxData);
                    iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GyData);
                    iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, kExtents, GzData);

                    gmBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));

                    if (gmBuffer[i] > gmMax) {
                        gmMax = gmBuffer[i];
                    } else if (gmBuffer[i] < gmMin) {
                        gmMin = gmBuffer[i];
                    }
                }
            }

            for (i = 0; (i < length) && !threadStopped; i++) {

                int s = i / offset;

                if ((i % offset) == 0) {
                    fireProgressStateChanged(getProgressFromInt(Math.round((float) (50.0 * s) / (iterations * srcImage.getExtents()[2])) +
                            Math.round(100 * ((float) (n)) / iterations) +
                            Math.round((float) (50.0) / (iterations))), 
                            srcImage.getImageName(), "Diffusing image ...");
                        
                    
                }

                if ((entireImage == true) || mask.get(i)) {
                    mag = ((gmBuffer[i] - gmMin) / (gmMax - gmMin)) * 100;
                    edgeFunct = (float) (1 / (1 + (mag / konsnt * mag / konsnt)));

                    val = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), imgBuffer, klapExtents, lapData);

                    val = val * edgeFunct * 0.25f;
                    resultBuffer[i] = val + imgBuffer[i];
                } else {
                    resultBuffer[i] = imgBuffer[i];
                }
            }

            tempBuffer = imgBuffer;
            imgBuffer = resultBuffer;
            resultBuffer = tempBuffer;
            // System.arraycopy(resultBuffer, 0, imgBuffer, 0, length);
        }

        resultBuffer = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            destImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            setCompleted(false);
            imgBuffer = null;
            resultBuffer = null;
            gmBuffer = null;
            errorCleanUp(error + "Anisotropic Diffusion: Image(s) locked", true);

            return;
        }

        fireProgressStateChanged(maxProgressValue, srcImage.getImageName(), "Diffusing image ...");
        
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String sigmaStr = new String();

        for (int i = 0; i < sigmas.length; i++) {
            sigmaStr += (" " + String.valueOf(sigmas[i]) + ", ");
        }

        historyString = new String("AnisotropicDiffusion(" + sigmaStr + String.valueOf(iterations) + ", " +
                                   String.valueOf(konsnt) + ", " + String.valueOf(entireImage) + ", " +
                                   String.valueOf(image25D) + ")\n");
    }

    /**
     * Creates the derivative kernels used to calculate the gradient magnitude and kernel for the diffusion process.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 1;
        derivOrder[1] = 0;

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        GxData = new float[xkDim * ykDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        GyData = new float[xkDim * ykDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);
        Gy.calc(true);

        klapExtents = new int[2];
        klapExtents[0] = 3;
        klapExtents[1] = 3;

        lapData = new float[9];

        lapData[0] = 0.71f;
        lapData[1] = 1.0f;
        lapData[2] = 0.71f;
        lapData[3] = 1.0f;
        lapData[4] = -6.84f;
        lapData[5] = 1.0f;
        lapData[6] = 0.71f;
        lapData[7] = 1.0f;
        lapData[8] = 0.71f;
    }

    /**
     * Creates the 3D derivative kernels used to calculate the gradient magnitude and kernel for the diffusion process.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(5 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        if (zkDim < 3) {
            zkDim = 3;
        }

        kExtents[2] = zkDim;


        GxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);
        Gy.calc(true);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gz = new GenerateGaussian(GzData, kExtents, sigmas, derivOrder);
        Gz.calc(true);

        klapExtents = new int[3];
        klapExtents[0] = 3;
        klapExtents[1] = 3;
        klapExtents[2] = 3;

        lapData = new float[27];

        lapData[0] = 0.58f;
        lapData[1] = 0.71f;
        lapData[2] = 0.58f;
        lapData[3] = 0.71f;
        lapData[4] = 1.0f;
        lapData[5] = 0.71f;
        lapData[6] = 0.58f;
        lapData[7] = 0.71f;
        lapData[8] = 0.58f;

        lapData[9] = 0.71f;
        lapData[10] = 1.0f;
        lapData[11] = 0.71f;
        lapData[12] = 1.0f;
        lapData[13] = -19.16f;
        lapData[14] = 1.0f;
        lapData[15] = 0.71f;
        lapData[16] = 1.0f;
        lapData[17] = 0.71f;

        lapData[18] = 0.58f;
        lapData[19] = 0.71f;
        lapData[20] = 0.58f;
        lapData[21] = 0.71f;
        lapData[22] = 1.0f;
        lapData[23] = 0.71f;
        lapData[24] = 0.58f;
        lapData[25] = 0.71f;
        lapData[26] = 0.58f;
    }
}
