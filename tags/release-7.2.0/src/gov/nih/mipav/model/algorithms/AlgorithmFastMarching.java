package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 *
 * @version  0.1 March 6, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 *
 *           <p>This algorithm iteratively expands a contour to a boundary.</p>
 *
 *           <p>This version of the anisotropic diffusion equation is: dI/dt = div[g(normalized grad magnitude(image))*
 *           grad I], where g() is the edge stopping function. The original edge stopping function used was the Perona
 *           and Malik function: g(x) = 1/(1 + (x**2/k**2)). However, as outlined in "Robust Anisotropic Diffusion" by
 *           Michael J. Black, Guillermo Sapiro, David H. Marimont, and David Heeger, IEEE Transactions on Image
 *           Processing, Vol. 7, No. 3, March 1998, pp. 421-432, the Perona and Malik function gives outliers(large
 *           values of grad(image)) too much influence. Thus, the Perona and Malik function was replaced with the
 *           Tukey's biweight function: g(x) = 0.5 * [1 - (x/k)**2]**2 for abs(x) <= k = 0 otherwise.</p>
 *
 *           <p>The normalized gradient magnitude of the image goes from 0 to 100. In the above equation I is a VOI
 *           intensity, where initially all the VOI area is assigned 100 and the rest of the image is assigned 0. The
 *           equation is discretized as: I(x,y,z,t+1) = I(x,y,z,t) + 0.25*g(normalized grad magnitude(image(x,y,z)))*
 *           (I(x+gradX,y+gradY,z+gradZ,t) - I(x,y,z,t)), where gradX, gradY, and gradZ are the normalized gradient
 *           measurements of I. The value of I is clamped to a maximum value of 100 and the value of I is not changed if
 *           either its value has fallen to be <= 3 or the values of all 4 of its nearest neighbors are <= 3. Note that
 *           the name AlgorithmLevelSet is misleading because the equation used is a version of the anisotropic
 *           diffusion equation and not a version of the level set equation given by Osher and Sethian: d(phi)/dt +
 *           F*|grad(phi)| = 0, given phi(x,y,z,t=0). For more information on the Anisotropic diffusion equation see:
 *           Geometry-Driven Diffusion in Computer Vision edited by Bart M. ter Haar Romeny, Chapter 3 on Anisotropic
 *           Diffusion by Pietro Perona, Takahiro Shiota, and Jitendra Malik, pages 73-92, Kluwer Academic Publishers,
 *           1994.</p>
 */
public class AlgorithmFastMarching extends AlgorithmBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] edgeImage;

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

    /**
     * Controls the diffusion rate. A large value causes this algorithms to act like gaussian smoothing and therefore
     * diffuses across edges. K = small reduces blur across edges. Typical K = 10.
     */
    private float kValue = 10;

    /** DOCUMENT ME! */
    private float[] levelImage;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;
    
    /** Stores result of AlgorithmConvolver */
    private float[] outputBuffer = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AnisotropicDiffusion.
     *
     * @param  srcImg  reference to the source image
     * @param  sigmas  sigmas used to describe the gaussian that is used in the calculation of the gradient magnitude
     * @param  iter    number of iterations (t) of the diffusion equation
     * @param  kValue  K is a factor that controls the diffusion rate. A large value causes this algorithms to act like
     *                 gaussian smoothing and therefore diffuses across edges. K = small reduces blur across edges.
     *                 Typical K = 10;
     */
    public AlgorithmFastMarching(ModelImage srcImg, float[] sigmas, int iter, float kValue) {
        super(null, srcImg);

        this.sigmas = sigmas;
        iterations = iter;
        this.kValue = kValue;

        if (srcImg.getNDims() == 2) {
            makeKernels2D();
        } else if (srcImg.getNDims() > 2) {
            makeKernels3D();
        }

        mask = srcImage.generateVOIMask();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void cleanUp() {
        GxData = null;
        GyData = null;
        GzData = null;
        srcImage = null;
        edgeImage = null;
        levelImage = null;
    }

    /**
     * finalize - sets class storages arrays to null so that System.gc() can free the memory.
     */
    public void finalize() {
        cleanUp();
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
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
    }

    /**
     * calc2D - calculates the diffused image and creates the new VOI for the original image.
     */
    private void calc2D() {

        int i, n;
        int length;
        float[] imgBuffer;
        float[] tempBuffer;
        float[] resultBuffer;
        float mag;
        double grad;
        int nVOI;
        ViewVOIVector VOIs;
        short voiID;
        AlgorithmConvolver convolver;

        try {
            length = srcImage.getSliceSize();
            imgBuffer = new float[length];
            resultBuffer = new float[length];
            edgeImage = new float[length];
            levelImage = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        } catch (IOException error) {
            cleanUp();
            System.gc();
            displayError("Level set: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("Level set: Out of Memory");
            setCompleted(false);

            return;
        }

        for (i = 0; i < length; i++) {

            if (mask.get(i) == true) {
                levelImage[i] = 100;
            } else {
                levelImage[i] = 0;
            }
        }

        boolean entireImage = true;
        boolean sqrtXY = true;
        convolver = new AlgorithmConvolver(srcImage, GxData, GyData, kExtents, entireImage, sqrtXY);
        convolver.setMinProgressValue(0);
        convolver.setMaxProgressValue(10);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);

        convolver.run();
        convolver.finalize();

        float min = Float.MAX_VALUE;
        float max = 0.0f;

        for (i = 0; i < length; i++) { // calculate gradient magnitude
            mag = outputBuffer[i];
            edgeImage[i] = mag;

            if (mag > max) {
                max = mag;
            }

            if (mag < min) {
                min = mag;
            }
        }

        float divisor = max - min;

        if (divisor == 0) {
            divisor = 1;
        }

        for (i = 0; i < length; i++) { // normalize the data between 0 and 100
            mag = ((edgeImage[i] - min) / divisor) * 100;

            if (Math.abs(mag) <= kValue) {
                edgeImage[i] = 1 - (mag / kValue * mag / kValue);
                edgeImage[i] = 0.5f * edgeImage[i] * edgeImage[i];
            } else {
                edgeImage[i] = 0.0f;
            }
        }

        imgBuffer = null;
        System.gc();

        double gradX, gradY;
        float temp;
        float x, y;
        int xPos;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = 1;

        for (n = 0; (n < iterations) && !threadStopped; n++) {
            fireProgressStateChanged(10 + (90 * n) / (iterations - 1));

            for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {
                resultBuffer[i] = 0;
                xPos = i % xDim;

                if (((xPos >= 1) && (xPos < (xDim - 1))) &&
                        ((levelImage[i - xDim] > 3) || (levelImage[i - 1] > 3) || (levelImage[i + 1] > 3) ||
                             (levelImage[i + xDim] > 3))) {

                    if ((levelImage[i] == 0) || (levelImage[i] > 3)) {

                        gradX = levelImage[i + 1] - levelImage[i - 1];
                        gradY = levelImage[i + xDim] - levelImage[i - xDim];
                        grad = Math.sqrt((gradX * gradX) + (gradY * gradY));

                        if (grad > 0) { // normalize gradient measurements
                            gradX = gradX / grad;
                            gradY = gradY / grad;
                        } else {
                            gradX = gradY = 0;
                        }

                        x = (float) (xPos + gradX);
                        y = (float) ((i / xDim) + gradY);

                        // possible speedup remove function
                        temp = getBiLinear(levelImage, xDim, x, y);
                        temp = (temp - levelImage[i]) * 0.25f * edgeImage[i];

                        if ((temp + levelImage[i]) <= 100) {
                            resultBuffer[i] = temp + levelImage[i];
                        } else {
                            resultBuffer[i] = 100;
                        }
                    }
                }
            }

            tempBuffer = levelImage;
            levelImage = resultBuffer;
            resultBuffer = tempBuffer;
        }

        resultBuffer = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        // delete the VOIs
        VOIs = srcImage.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {
            VOIs.removeElementAt(i);
        }

        voiID = 0;
        mask = new BitSet(length);

        for (i = 0; i < length; i++) {

            if (levelImage[i] >= 90.0f) {
                mask.set(i);
            } else {
                mask.clear(i);
            }
        }

        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(srcImage, mask, xDim, yDim, zDim,
                                                                                     voiID);

        algoPaintToVOI.run();

        
        setCompleted(true);
    }

    /**
     * calc3D - calculates the diffused image and creates the new VOI for the original image.
     */
    private void calc3D() {

        int i, n;
        int length;
        float[] imgBuffer;
        float[] tempBuffer;
        float[] resultBuffer;
        float mag;
        double grad;
        int nVOI;
        ViewVOIVector VOIs;
        short voiID;
        AlgorithmConvolver convolver;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            imgBuffer = new float[length];
            resultBuffer = new float[length];
            levelImage = new float[length];
            edgeImage = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        } catch (IOException error) {
            cleanUp();
            System.gc();
            displayError("Anisotropic Diffusion: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("Anisotropic Diffusion: Out of Memory");
            setCompleted(false);

            return;
        }

        int imageSliceSize = srcImage.getSliceSize();
        int end = length - imageSliceSize;

        for (i = imageSliceSize; i < end; i++) {

            if (mask.get(i) == true) {
                levelImage[i - imageSliceSize] = 100;
                levelImage[i] = 100;
                levelImage[i + imageSliceSize] = 100;
            } else {
                levelImage[i] = 0;
            }
        }
        
        convolver = new AlgorithmConvolver(srcImage, GxData, GyData, GzData, kExtents,true);
        convolver.setMinProgressValue(0);
        convolver.setMaxProgressValue(10);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);

        convolver.run();
        convolver.finalize();

        float min = Float.MAX_VALUE;
        float max = 0.0f;

        for (i = 0; i < length; i++) { // calculate gradient magnitude
            mag = outputBuffer[i];
            edgeImage[i] = mag;

            if (mag > max) {
                max = mag;
            }

            if (mag < min) {
                min = mag;
            }
        }

        float divisor = max - min;

        if (divisor == 0) {
            divisor = 1;
        }

        for (i = 0; i < length; i++) { // normalize the data between 0 and 100
            mag = ((edgeImage[i] - min) / divisor) * 100;

            if (Math.abs(mag) <= kValue) {
                edgeImage[i] = 1 - (mag / kValue * mag / kValue);
                edgeImage[i] = 0.5f * edgeImage[i] * edgeImage[i];
            } else {
                edgeImage[i] = 0.0f;
            }
        }

        imgBuffer = null;
        System.gc();

        double gradX, gradY, gradZ;
        float temp;
        float x, y, z;
        int xPos, yPos;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];

        for (n = 0; (n < iterations) && !threadStopped; n++) {
            fireProgressStateChanged(10 + (90 * n) / (iterations - 1));
            
            for (i = imageSliceSize + xDim + 1; (i < (length - imageSliceSize - xDim - 1)) && !threadStopped; i++) {
                resultBuffer[i] = 0;
                xPos = i % xDim;
                yPos = (i % imageSliceSize) / xDim;

                if (((xPos >= 1) && (xPos < (xDim - 1)) && (yPos >= 1) && (yPos < (yDim - 1))) &&
                        ((levelImage[i - xDim] > 3) || (levelImage[i - 1] > 3) || (levelImage[i + 1] > 3) ||
                             (levelImage[i + xDim] > 3) || (levelImage[i - imageSliceSize] > 3) ||
                             (levelImage[i + imageSliceSize] > 3))) {

                    if ((levelImage[i] == 0) || (levelImage[i] > 3)) {

                        gradX = levelImage[i + 1] - levelImage[i - 1];
                        gradY = levelImage[i + xDim] - levelImage[i - xDim];
                        gradZ = levelImage[i + imageSliceSize] - levelImage[i - imageSliceSize]; // /*******
                        grad = Math.sqrt((gradX * gradX) + (gradY * gradY) + (gradZ * gradZ));

                        if (grad > 0) { // normalize gradient measurements
                            gradX = gradX / grad;
                            gradY = gradY / grad;
                            gradZ = gradZ / grad;

                            x = (float) (xPos + gradX);
                            y = (float) (yPos + gradY);
                            z = (float) ((i / imageSliceSize) + gradZ);

                            temp = getTriLinear(levelImage, xDim, imageSliceSize, x, y, z);
                            temp = (temp - levelImage[i]) * 0.25f * edgeImage[i];
                        } else {
                            temp = 0;
                        }

                        if ((temp + levelImage[i]) <= 100) {
                            resultBuffer[i] = temp + levelImage[i];
                        } else {
                            resultBuffer[i] = 100;
                        }
                    }
                }
            }

            tempBuffer = levelImage;
            levelImage = resultBuffer;
            resultBuffer = tempBuffer;
        }

        resultBuffer = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        // delete the VOIs
        VOIs = srcImage.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {
            VOIs.removeElementAt(i);
        }

        voiID = 0;
        mask = new BitSet(length);

        for (i = 0; i < length; i++) {

            if (levelImage[i] >= 90.0f) {
                mask.set(i);
            } else {
                mask.clear(i);
            }
        }

        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(srcImage, mask, xDim, yDim, zDim,
                                                                                     voiID);

        algoPaintToVOI.run();

        
        setCompleted(true);
    }

    /**
     * getBiLinear - version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     *
     * @param   imageBuffer  buffer containing pixel data
     * @param   xDim         x dimension offset
     * @param   x            x coordinate
     * @param   y            y coordinate
     *
     * @return  DOCUMENT ME!
     */
    private float getBiLinear(float[] imageBuffer, int xDim, float x, float y) {

        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;
        float diff;

        intX = (int) (x);
        intY = (int) (y);

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        if (position >= imageBuffer.length) {
            return 0;
        }

        diff = 1 - dx;
        x1 = (diff * imageBuffer[position]) + (dx * imageBuffer[position + 1]);
        x2 = (diff * imageBuffer[position + xDim]) + (dx * imageBuffer[position + xDim + 1]);

        return ((1 - dy) * x1) + (dy * x2);
    }

    /**
     * getTriLinear - version of get that performs trilinear interpoloation.
     *
     * @param   imageBuffer     buffer containing pixel data
     * @param   xDim            x dimension offset
     * @param   imageSliceSize  DOCUMENT ME!
     * @param   x               x coordinate
     * @param   y               y coordinate
     * @param   z               z coordinate
     *
     * @return  DOCUMENT ME!
     */
    private float getTriLinear(float[] imageBuffer, int xDim, int imageSliceSize, float x, float y, float z) {

        int position1, position2;
        float a1, a2;
        float b1, b2;
        int intX, intY, intZ;
        float dx, dy, dz;

        intX = (int) (x);
        intY = (int) (y);
        intZ = (int) (z);

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSliceSize) + (intY * xDim) + intX;
        position2 = position1 + imageSliceSize;

        if (position2 >= imageBuffer.length) {
            return 0;
        }

        float diffx = 1 - dx;

        a1 = (diffx * imageBuffer[position1]) + (dx * imageBuffer[position1 + 1]);
        a2 = (diffx * imageBuffer[position1 + xDim]) + (dx * imageBuffer[position1 + xDim + 1]);
        b1 = ((1 - dy) * a1) + (dy * a2);

        a1 = (diffx * imageBuffer[position2]) + (dx * imageBuffer[position2 + 1]);
        a2 = (diffx * imageBuffer[position2 + xDim]) + (dx * imageBuffer[position2 + xDim + 1]);
        b2 = ((1 - dy) * a1) + (dy * a2);

        return (((1 - dz) * b1) + (dz * b2));
    }

    /**
     * makeKernals2D - creates the derivative kernels used to calculate the gradient magnitude and kernel for the
     * diffusion process.
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
    }

    /**
     * makeKernals3D - creates the derivative kernels used to calculate the gradient magnitude and kernel for the
     * diffusion process.
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
