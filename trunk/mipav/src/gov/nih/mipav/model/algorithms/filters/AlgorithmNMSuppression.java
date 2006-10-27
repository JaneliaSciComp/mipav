package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Calculates the non-maximum suppression of an image at a scale defined by the user Edges are defined as the union of
 * points for which the gradient magnitude assumes a maximum in the gradient direction. Introduce a local orthonormal
 * coordinate system (u,v) at any point P0, where the v-axis is parallel to the gradient direction at P0, and the u-axis
 * is perpindicular. Iu = sin(a)*Ix - cos(a)*Iy Iv = cos(a)*Ix + sin(a)*Iy cos(a) = Ix/(sqrt(Ix*Ix + Iy*Iy)) evaluated
 * at P0 sin(a) = Iy/(sqrt(Ix*Ix + Iy*Iy)) evaluated at P0 Iu = (Ix*Iy - Ix*Iy)/(sqrt(Ix*Ix + Iy*Iy)) = 0 Iv = (Ix*Ix +
 * Iy*Iy)/(sqrt(Ix*Ix + Iy*Iy)) = sqrt(Ix*Ix + Iy*Iy) Iv*Iv = Ix*Ix + Iy*Iy Ivv = (cos(a)*Ix + sin(a)*Iy)(cos(a)*Ix +
 * sin(a)*Iy) = cos(a)*cos(a)*Ixx + 2*cos(a)*sin(a)*Ixy + sin(a)*sin(a)*Iyy = (Ix*Ix*Ixx + 2*Ix*Iy*Ixy +
 * Iy*Iy*Iyy)/(Ix*Ix + Iy*Iy) Iv*Iv*Ivv = Ix*Ix*Ixx + 2*Ix*Iy*Ixy + Iy*Iy*Iyy Ivvv = (cos(a)*cos(a)*Ixx +
 * 2*cos(a)*sin(a)*Ixy + sin(a)*sin(a)*Iyy)(cos(a)*Ix + sin(a)*Iy) = cos(a)*cos(a)*cos(a)*Ixxx +
 * 3*cos(a)*cos(a)*sin(a)*Ixxy + 3*cos(a)*sin(a)*sin(a)*Ixyy + sin(a)*sin(a)*sin(a)*Iyyy = (Ix*Ix*Ix*Ixxx +
 * 3*Ix*Ix*Iy*Ixxy + 3*Ix*Iy*Iy*Ixyy + Iy*Iy*Iy*Iyyy)/((Ix*Ix + Iy*Iy)**3/2) Iv*Iv*Iv*Ivvv = Ix*Ix*Ix*Ixxx +
 * 3*Ix*Ix*Iy*Ixxy + 3*Ix*Iy*Iy*Ixyy + Iy*Iy*Iy*Iyyy Assuming that the second and third-order directional derivatives of
 * I in the v-direction are not simultaneously zero, a necessary and sufficient condition for P0 to be a gradient
 * maximum in the gradient direction may be stated as: Ivv = 0, Ivvv < 0. Since only the sign information is important,
 * this condition can be restated as: Iv*Iv*Ivv = 0, Iv*Iv*Iv*Ivvv < 0. Reference: Geometry-Driven Diffusion in Computer
 * Vision, Bart M. ter Haar Romeny(Ed.), Chapter2:Linear Scale-Space II: Early Visual Operations by Tony Lindeberg and
 * Bart M. ter Haar Romeny, Kluwer Academic Publishers, 1994, page 45.
 *
 * <p>For 3D a similar derivation: Iv = cos(a)*sin(b)*Ix + sin(a)*sin(b)*Iy + cos(b)*Iz where spherical coordinates are
 * being used cos(a) = Ix/(sqrt(Ix*Ix + Iy*Iy)) evaluated at P0 sin(a) = Iy/(sqrt(Ix*Ix + Iy*Iy)) evaluated at P0 cos(b)
 * = Iz/(sqrt(Ix*Ix + Iy*Iy + Iz*Iz)) evaluated at P0 sin(b) = sqrt(Ix*Ix + Iy*Iy)/(sqrt(Ix*Ix + Iy*Iy + Iz*Iz))
 * evaluated at P0 Iv = (Ix*Ix + Iy*Iy + Iz*Iz)/(sqrt(Ix*Ix + Iy*Iy + Iz*Iz)) = sqrt(Ix*Ix + Iy*Iy + Iz*Iz) Iv*Iv =
 * Ix*Ix + Iy*Iy + Iz*Iz Ivv = (cos(a)*sin(b)*Ix + sin(a)*sin(b)*Iy + cos(b)*Iz)(cos(a)*sin(b)*Ix + sin(a)*sin(b)*Iy +
 * cos(b)*Iz) = cos(a)*cos(a)*sin(b)*sin(b)*Ixx + 2*cos(a)*sin(a)*sin(b)*sin(b)*Ixy + 2*cos(a)*cos(b)*sin(b)*Ixz +
 * sin(a)*sin(a)*sin(b)*sin(b)*Iyy + 2*sin(a)*cos(b)*sin(b)*Iyz + cos(b)*cos(b)*Izz = (Ix*Ix*Ixx + 2*Ix*Iy*Ixy +
 * 2*Ix*Iz*Ixz + Iy*Iy*Iyy + 2*Iy*Iz*Iyz + Iz*Iz*Izz)/ (Ix*Ix + Iy*Iy + Iz*Iz) Iv*Iv*Ivv = Ix*Ix*Ixx + 2*Ix*Iy*Ixy +
 * 2*Ix*Iz*Ixz + Iy*Iy*Iyy + 2*Iy*Iz*Iyz + Iz*Iz*Izz Ivvv = (cos(a)*cos(a)*sin(b)*sin(b)*Ixx +
 * 2*cos(a)*sin(a)*sin(b)*sin(b)*Ixy + 2*cos(a)*cos(b)*sin(b)*Ixz + sin(a)*sin(a)*sin(b)*sin(b)*Iyy +
 * 2*sin(a)*cos(b)*sin(b)*Iyz + cos(b)*cos(b)*Izz)(cos(a)*sin(b)*Ix + sin(a)*sin(b)*Iy + cos(b)*Iz) =
 * cos(a)*cos(a)*cos(a)*sin(b)*sin(b)*sin(b)*Ixxx + 3*cos(a)*cos(a)*sin(a)*sin(b)*sin(b)*sin(b)*Ixxy +
 * 3*cos(a)*cos(a)*cos(b)*sin(b)*sin(b)*Ixxz + 3*cos(a)*sin(a)*sin(a)*sin(b)*sin(b)*sin(b)*Ixyy +
 * 6*cos(a)*sin(a)*cos(b)*sin(b)*sin(b)*Ixyz + 3*cos(a)*cos(b)*cos(b)*sin(b)*Ixzz +
 * sin(a)*sin(a)*sin(a)*sin(b)*sin(b)*sin(b)*Iyyy + 3*sin(a)*sin(a)*cos(b)*sin(b)*sin(b)*Iyyz +
 * 3*sin(a)*cos(b)*cos(b)*sin(b)*Iyzz + cos(b)*cos(b)*cos(b)*Izzz = (Ix*Ix*Ix*Ixxx + 3*Ix*Ix*Iy*Ixxy + 3*Ix*Ix*Iz*Ixxz +
 * 3*Ix*Iy*Iy*Ixyy + 6*Ix*Iy*Iz*Ixyz + 3*Ix*Iz*Iz*Ixzz + Iy*Iy*Iy*Iyyy + 3*Iy*Iy*Iz*Iyyz + 3*Iy*Iz*Iz*Iyzz +
 * Iz*Iz*Iz*Izzz)/ ((Ix*Ix + Iy*Iy + Iz*Iz)**1.5) Iv*Iv*Iv*Ivvv = Ix*Ix*Ix*Ixxx + 3*Ix*Ix*Iy*Ixxy + 3*Ix*Ix*Iz*Ixxz +
 * 3*Ix*Iy*Iy*Ixyy + 6*Ix*Iy*Iz*Ixyz + 3*Ix*Iz*Iz*Ixzz + Iy*Iy*Iy*Iyyy + 3*Iy*Iy*Iz*Iyyz + 3*Iy*Iz*Iz*Iyzz +
 * Iz*Iz*Iz*Izzz</p>
 */


public class AlgorithmNMSuppression extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean edgeImage;

    /** DOCUMENT ME! */
    private boolean entireImage;

    /** DOCUMENT ME! */
    private float[] GxData;

    /** DOCUMENT ME! */
    private float[] GxxData;

    /** DOCUMENT ME! */
    private float[] GxxxData;

    /** DOCUMENT ME! */
    private float[] GxxyData;

    /** DOCUMENT ME! */
    private float[] GxxzData;

    /** DOCUMENT ME! */
    private float[] GxyData;

    /** DOCUMENT ME! */
    private float[] GxyyData;

    /** DOCUMENT ME! */
    private float[] GxyzData;

    /** DOCUMENT ME! */
    private float[] GxzData;

    /** DOCUMENT ME! */
    private float[] GxzzData;

    /** DOCUMENT ME! */
    private float[] GyData;

    /** DOCUMENT ME! */
    private float[] GyyData;

    /** DOCUMENT ME! */
    private float[] GyyyData;

    /** DOCUMENT ME! */
    private float[] GyyzData;

    /** DOCUMENT ME! */
    private float[] GyzData;

    /** DOCUMENT ME! */
    private float[] GyzzData;

    /** DOCUMENT ME! */
    private float[] GzData;

    /** DOCUMENT ME! */
    private float[] GzzData;

    /** DOCUMENT ME! */
    private float[] GzzzData;

    /** DOCUMENT ME! */
    private int[] kExtents;

    /** DOCUMENT ME! */
    private float loThres, hiThres;

    /** DOCUMENT ME! */
    private float[] sigmas;

    /** DOCUMENT ME! */
    private ModelImage zXMask;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmNMSuppression - Constructor.
     *
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the non-maximum suppression will be calculated for the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmNMSuppression(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D) {
        super(null, srcImg);

        this.sigmas = sigmas;
        entireImage = maskFlag;
        edgeImage = false;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * AlgorithmNMSuppression - Constructor.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the non-maximum suppression will be calculated for the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmNMSuppression(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
                                  boolean img25D) {
        super(destImg, srcImg);

        this.sigmas = sigmas;
        edgeImage = false;
        entireImage = maskFlag;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Generates a zero crossing mask for a 2D function sets a Bitset object to 1 is a zero crossing is detected.
     *
     * @param  xDim       X dimension length
     * @param  yDim       Y dimension length
     * @param  buffer     array of data in which to find level crossing
     * @param  edgeImage  edge map of level crossings
     * @param  level      level of crossings to find (e.g. zero crossing of the non-maximum suppression)
     */
    public static void genLevelMask(int xDim, int yDim, float[] buffer, BitSet edgeImage, float level) {

        float x0, x1, x2, x3;
        int i, j, index;
        int indexY;

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;

        edgeImage = new BitSet(xDim * yDim);

        x0 = buffer[0];
        x2 = buffer[xDim];

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                index = indexY + i;
                x1 = buffer[index + 1];
                x3 = buffer[index + 1 + xDim];

                if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                    edgeImage.clear(index);
                } else if ((x0 <= level) && (x1 <= level) && (x2 <= level) && (x3 <= level)) {
                    edgeImage.clear(index);
                } else {
                    edgeImage.set(index);
                }

                x0 = x1;
                x2 = x3;
            }
        }
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        kExtents = null;
        GxxData = null;
        GyyData = null;
        GzzData = null;
        GxData = null;
        GyData = null;
        GxyData = null;
        GxzData = null;
        GyzData = null;
        GxxxData = null;
        GxxyData = null;
        GxyyData = null;
        GyyyData = null;
        GxxzData = null;
        GxzzData = null;
        GxyzData = null;
        GyyzData = null;
        GyzzData = null;
        GzzzData = null;
        destImage = null;
        srcImage = null;
        zXMask = null;
        super.finalize();
    }

    /**
     * Generates a zero crossing mask for a 2D function sets a ModelImage to 1 if a zero crossing is detected.
     *
     * @param  buffer   array in which to find zero crossing
     * @param  buffer2  array which ensures that zero crossing is only counted if buffer2 value at that position is less
     *                  than zero
     */
    public void genZeroXMask(float[] buffer, float[] buffer2) {

        float x0, x1, x2, x3;
        int i, j, index;
        int indexY;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;
        int level = 0;

        int[] destExtents = new int[2];
        destExtents[0] = srcImage.getExtents()[0]; // X dim
        destExtents[1] = srcImage.getExtents()[1]; // Y dim

        zXMask = new ModelImage(ModelImage.UBYTE, destExtents, " Edges", srcImage.getUserInterface());

        x0 = buffer[0];
        x2 = buffer[xDim];

        if ((x0 >= loThres) && (x0 <= hiThres)) {
            x0 = 0;
        }

        if ((x2 >= loThres) && (x2 <= hiThres)) {
            x2 = 0;
        }

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                index = indexY + i;
                x1 = buffer[index + 1];
                x3 = buffer[index + 1 + xDim];

                if ((x1 >= loThres) && (x1 <= hiThres)) {
                    x1 = 0;
                }

                if ((x3 >= loThres) && (x3 <= hiThres)) {
                    x3 = 0;
                }

                if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                    zXMask.set(index, 0);
                } else if ((x0 <= level) && (x1 <= level) && (x2 <= level) && (x3 <= level)) {
                    zXMask.set(index, 0);
                } else if (buffer2[index] >= 0.0f) {
                    zXMask.set(index, 0);
                } else {
                    zXMask.set(index, 255);
                }

                x0 = x1;
                x2 = x3;
            }
        }

        zXMask.calcMinMax();

        FileInfoBase[] fileInfo;
        fileInfo = zXMask.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setEndianess(srcImage.getFileInfo()[0].getEndianess());
        fileInfo[0].setUnitsOfMeasure(srcImage.getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getResolutions(0));
        fileInfo[0].setExtents(zXMask.getExtents());
        fileInfo[0].setMax(zXMask.getMax());
        fileInfo[0].setMin(zXMask.getMin());
        fileInfo[0].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
        fileInfo[0].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
    }

    /**
     * Accessor to return mask indicating zero crossings.
     *
     * @return  - ModelImage of zero crossings ( 2D function 1 = indicates zero crossing
     */
    public ModelImage getZeroXMask() {
        return zXMask;
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
     * Call from dialog if you wish to produce edge image.
     *
     * @param  loThres  DOCUMENT ME!
     * @param  hiThres  DOCUMENT ME!
     */
    public void setEdgeOptions(float loThres, float hiThres) {
        this.edgeImage = true;
        this.loThres = loThres;
        this.hiThres = hiThres;
    }

    /**
     * Calculates the non-maximum suppression image and replaces the source image with the new image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace2D(int nImages) {

        int i, s;
        int length, totalLength;
        int start;
        float ix, iy, ixx, ixy, iyy, ixxx, ixxy, ixyy, iyyy;
        float[] buffer;
        float bufferMin, bufferMax;
        float a;
        float[] resultBuffer, resultBuffer2;

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            resultBuffer = new float[length * nImages];
            resultBuffer2 = new float[length];

            for (i = 0; i < length; i++) {
                resultBuffer2[i] = 1.0f;
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Non-maximum suppression ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            errorCleanUp("Algorithm NMSuppression exportData: Out of memory", true);

            return;
        }

        int mod = totalLength / 100; // mod is 1 percent of length
        

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                resultBuffer2 = null;
                System.gc();
                displayError("Algorithm NMSuppression: Image(s) locked");
                setCompleted(false);
                

                return;
            }

            for (i = 0; (i < length) && !threadStopped; i++) {

                if ((((start + i) % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                }

                if ((entireImage == true) || mask.get(i)) {
                    ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                    iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                    ixx = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                    ixy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxyData);
                    iyy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GyyData);
                    resultBuffer[start + i] = (ix * ix * ixx) + (2.0f * ix * iy * ixy) + (iy * iy * iyy);

                    if ((edgeImage == true) && (nImages == 1)) {
                        ixxx = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxxData);
                        ixxy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxyData);
                        ixyy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxyyData);
                        iyyy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GyyyData);
                        resultBuffer2[i] = (ix * ix * ix * ixxx) + (3.0f * ix * ix * iy * ixxy) +
                                           (3.0f * ix * iy * iy * ixyy) + (iy * iy * iy * iyyy);
                    } // if (edgeImage == true && nImages == 1)
                } else {
                    resultBuffer[start + i] = buffer[i];

                    if ((edgeImage == true) && (nImages == 1)) {
                        resultBuffer2[i] = 1.0f; // > 0 so not interpreted as edge
                    }
                    // resultBuffer[i] = 0;
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {

            // resultBuffer has negative values so UBYTE and USHORT will give invalid results
            if (srcImage.getType() != ModelImage.FLOAT) {
                srcImage.reallocate(ModelImage.FLOAT);
            }

            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            errorCleanUp("Algorithm NMSuppression importData: Image(s) locked", true);

            return;
        }

        if ((edgeImage == true) && (nImages == 1)) {

            // Normalize data to have a range of 20000
            // Do not use a linear transformation which will shift negative to positive
            // or positive to negative
            bufferMax = -Float.MAX_VALUE;
            bufferMin = Float.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if (resultBuffer[i] < bufferMin) {
                    bufferMin = resultBuffer[i];
                }

                if (resultBuffer[i] > bufferMax) {
                    bufferMax = resultBuffer[i];
                }
            } // for (i = 0; i < length; i++)

            a = 20000.0f / (bufferMax - bufferMin);

            for (i = 0; i < length; i++) {
                resultBuffer[i] *= a;
            } // for (i = 0; i < length; i++)

            genZeroXMask(resultBuffer, resultBuffer2);
        }

        
        setCompleted(true);
    }

    /**
     * Calculates the non-maximum suppression and replaces the source image with the new image.
     */
    private void calcInPlace3D() {

        int i;
        int length;
        float ix, iy, iz, ixx, ixy, iyy, ixz, iyz, izz;
        float[] buffer;
        float[] resultBuffer;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Non-maximum suppression ...");
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm NMSuppression exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm NMSuppression exportData: Out of memory", true);

            return;
        }

        

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if ((entireImage == true) || mask.get(i)) {
                ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzData);
                ixx = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                ixy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxyData);
                iyy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyyData);
                ixz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxzData);
                iyz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyzData);
                izz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzzData);

                resultBuffer[i] = (ix * ix * ixx) + (2.0f * ix * iy * ixy) + (iy * iy * iyy) + (2.0f * ix * iz * ixz) +
                                  (2.0f * iy * iz * iyz) + (iz * iz * izz);
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

            // resultBuffer has negative values so UBYTE and USHORT will give invalid results
            if (srcImage.getType() != ModelImage.FLOAT) {
                srcImage.reallocate(ModelImage.FLOAT);
            }

            srcImage.importData(0, resultBuffer, true);
            // if (edgeImage == true)
            // genZeroXMask(resultBuffer);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm NMSuppression importData: Image(s) locked", true);

            return;
        }

        setCompleted(true);
        
    }

    /**
     * This function produces the non-maximum suppression of input image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcStoreInDest2D(int nImages) {
        int i, s, idx;
        int length, totalLength;
        int start;
        float ix, iy, ixx, ixy, iyy, ixxx, ixxy, ixyy, iyyy;
        float[] buffer;
        float[] buffer2;
        float bufferMin, bufferMax;
        float a;
        float nms;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Non-maximum suppression: Image(s) locked");
            setCompleted(false);
            destImage.releaseLock();

            return;
        }

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            buffer2 = new float[length];

            for (i = 0; i < length; i++) {
                buffer2[i] = 1.0f;
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Non-maximum suppression ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            buffer2 = null;
            errorCleanUp("Algorithm NMSuppression exportData: Out of memory", true);

            return;
        }

        int mod = totalLength / 100; // mod is 1 percent of length
        

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                buffer2 = null;
                errorCleanUp("Algorithm NMSuppression: Image(s) locked", true);

                return;
            }

            for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                if ((((start + i) % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                }

                if ((entireImage == true) || mask.get(i)) {
                    ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                    iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                    ixx = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                    ixy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxyData);
                    iyy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GyyData);
                    nms = (ix * ix * ixx) + (2.0f * ix * iy * ixy) + (iy * iy * iyy);
                    destImage.set(idx, nms);

                    if ((edgeImage == true) && (nImages == 1)) {
                        ixxx = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxxData);
                        ixxy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxyData);
                        ixyy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxyyData);
                        iyyy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GyyyData);
                        buffer2[i] = (ix * ix * ix * ixxx) + (3.0f * ix * ix * iy * ixxy) +
                                     (3.0f * ix * iy * iy * ixyy) + (iy * iy * iy * iyyy);
                    } // if (edgeImage == true && nImages == 1)
                } // if (entireImage == true || mask.get(i))
                else {
                    destImage.set(idx, buffer[i]);

                    if ((edgeImage == true) && (nImages == 1)) {
                        buffer2[i] = 1.0f; // > 0 so not interpreted as edge
                    }
                }
            }
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        if (image25D == false) { // Image is 2D  and not 2.5D

            try {
                destImage.exportData(0, length, buffer);
            } catch (IOException error) {
                buffer = null;
                buffer2 = null;
                errorCleanUp("Algorithm NMSuppression exportData: " + error, true);

                return;
            }
        }

        if ((edgeImage == true) && (nImages == 1)) {

            // Normalize data in buffer to have a range of 20000
            // Do not use a linear transformation which will shift positive to negative
            // or negative to positive
            bufferMax = -Float.MAX_VALUE;
            bufferMin = Float.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if (buffer[i] < bufferMin) {
                    bufferMin = buffer[i];
                }

                if (buffer[i] > bufferMax) {
                    bufferMax = buffer[i];
                }
            } // for (i = 0; i < length; i++)

            a = 20000.0f / (bufferMax - bufferMin);

            for (i = 0; i < length; i++) {
                buffer[i] *= a;
            } // for (i = 0; i < length; i++)

            genZeroXMask(buffer, buffer2);
        }

        
        setCompleted(true);
    }


    /**
     * This function produces the Non-maximum suppression of input image.
     */
    private void calcStoreInDest3D() {

        int i;
        int length;
        float ix, iy, iz, ixx, ixy, iyy, ixz, iyz, izz;
        float[] buffer;
        float nms;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm NMSuppression: Image(s) locked");
            setCompleted(false);

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Non-maximum suppression ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm NMSuppression exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm NMSuppression exportData: Out of memory", true);

            return;
        }

        

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if ((entireImage == true) || mask.get(i)) {
                ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzData);
                ixx = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                ixy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxyData);
                iyy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyyData);
                ixz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxzData);
                iyz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyzData);
                izz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzzData);
                nms = (ix * ix * ixx) + (2.0f * ix * iy * ixy) + (iy * iy * iyy) + (2.0f * ix * iz * ixz) +
                      (2.0f * iy * iz * iyz) + (iz * iz * izz);
                destImage.set(i, nms);
            } else {
                destImage.set(i, buffer[i]);
            }
        }

        // if (edgeImage == true)
        // genZeroXMask(buffer);
        destImage.calcMinMax();
        destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        
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

        historyString = new String("Non-maximum suppression(" + sigmaStr + String.valueOf(entireImage) +
                                   String.valueOf(image25D) + ")\n");
    }

    /**
     * Creates 2D Gaussian derivative kernels.
     */
    private void makeKernels2D() {
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

        derivOrder[0] = 1;
        derivOrder[1] = 0;
        GxData = new float[xkDim * ykDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        GyData = new float[xkDim * ykDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);
        Gy.calc(false);

        derivOrder[0] = 1;
        derivOrder[1] = 1;
        GxyData = new float[xkDim * ykDim];

        GenerateGaussian Gxy = new GenerateGaussian(GxyData, kExtents, sigmas, derivOrder);
        Gxy.calc(false);

        derivOrder[0] = 3;
        derivOrder[1] = 0;
        GxxxData = new float[xkDim * ykDim];

        GenerateGaussian Gxxx = new GenerateGaussian(GxxxData, kExtents, sigmas, derivOrder);
        Gxxx.calc(false);

        derivOrder[0] = 2;
        derivOrder[1] = 1;
        GxxyData = new float[xkDim * ykDim];

        GenerateGaussian Gxxy = new GenerateGaussian(GxxyData, kExtents, sigmas, derivOrder);
        Gxxy.calc(false);

        derivOrder[0] = 1;
        derivOrder[1] = 2;
        GxyyData = new float[xkDim * ykDim];

        GenerateGaussian Gxyy = new GenerateGaussian(GxyyData, kExtents, sigmas, derivOrder);
        Gxyy.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 3;
        GyyyData = new float[xkDim * ykDim];

        GenerateGaussian Gyyy = new GenerateGaussian(GyyyData, kExtents, sigmas, derivOrder);
        Gyyy.calc(false);

    }


    /**
     * Creates 3D Gaussian derivative kernels.
     */
    private void makeKernels3D() {
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

        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 0;
        GxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);
        Gy.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gz = new GenerateGaussian(GzData, kExtents, sigmas, derivOrder);
        Gz.calc(false);

        derivOrder[0] = 1;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GxyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxy = new GenerateGaussian(GxyData, kExtents, sigmas, derivOrder);
        Gxy.calc(false);

        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GxzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxz = new GenerateGaussian(GxzData, kExtents, sigmas, derivOrder);
        Gxz.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 1;
        GyzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyz = new GenerateGaussian(GyzData, kExtents, sigmas, derivOrder);
        Gyz.calc(false);


    }

}
