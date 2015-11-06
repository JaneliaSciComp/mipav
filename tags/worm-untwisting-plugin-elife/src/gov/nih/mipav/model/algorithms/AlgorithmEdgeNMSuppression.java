package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Calculates the non-maximum suppression of an image at a scale defined by the user. This algorithm produces an edge
 * map of the zero crossings of the non-maximum suppression for 2D images and 2.5D images. Edges are defined as the
 * union of points for which the gradient magnitude assumes a maximum in the gradient direction. Introuduce a local
 * orthonormal coordinate system (u,v) at any point P0, where the v-axis is parallel to the gradient direction at P0,
 * and the u-axis is perpindicular. Iu = sin(a)*Ix - cos(a)*Iy Iv = cos(a)*Ix + sin(a)*Iy cos(a) = Ix/(sqrt(Ix*Ix +
 * Iy*Iy)) evaluated at P0 sin(a) = Iy/(sqrt(Ix*Ix + Iy*Iy)) evaluated at P0 Iu = (Ix*Iy - Ix*Iy)/(sqrt(Ix*Ix + Iy*Iy))
 * = 0 Iv = (Ix*Ix + Iy*Iy)/(sqrt(Ix*Ix + Iy*Iy)) = sqrt(Ix*Ix + Iy*Iy) Iv*Iv = Ix*Ix + Iy*Iy Ivv = (cos(a)*Ix +
 * sin(a)*Iy)(cos(a)*Ix + sin(a)*Iy) = cos(a)*cos(a)*Ixx + 2*cos(a)*sin(a)*Ixy + sin(a)*sin(a)*Iyy = (Ix*Ix*Ixx +
 * 2*Ix*Iy*Ixy + Iy*Iy*Iyy)/(Ix*Ix + Iy*Iy) Iv*Iv*Ivv = Ix*Ix*Ixx + 2*Ix*Iy*Ixy + Iy*Iy*Iyy Ivvv = (cos(a)*cos(a)*Ixx +
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

public class AlgorithmEdgeNMSuppression extends AlgorithmBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Perform zero crossing detection using the marching squares method. */
    public static final int MARCHING_SQUARES = 0;

    /** Perform zero crossing detection using Matt's old method. */
    public static final int OLD_DETECTION = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

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
    private float[] sigmas;

    /** The type of zero crossing detection to use. */
    private int zeroDetectionType = MARCHING_SQUARES;

    /** DOCUMENT ME! */
    private ModelImage zXMask;
    
    // Buffer to receive result of convolution operation
    private float[] outputBuffer;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmEdgeNMSuppression object.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the EdgeNMSup will be calculated for the whole image if equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmEdgeNMSuppression(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
                                      boolean img25D) {
        super(destImg, srcImg);

        this.sigmas = sigmas;
        image25D = img25D;

        entireImage = maskFlag;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Generates a zero crossing mask for a 2D function sets a Bitset object to 1 is a zero crossing is detected.
     *
     * @param   xDim           the buffer's x dimension
     * @param   yDim           the buffer's y dimension
     * @param   buffer         array in which to find zero crossing
     * @param   level          the level to find the crossing of (usually will be 0)
     * @param   detectionType  the type of zero crossing method to use
     *
     * @return  DOCUMENT ME!
     */
    public static BitSet genLevelMask(int xDim, int yDim, float[] buffer, float level, int detectionType) {
        int i0, i1, i2, i3;
        float x0, x1, x2, x3;
        int i, j;
        int indexY;

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;

        BitSet edgeImage = new BitSet(xDim * yDim);

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                i0 = indexY + i;
                i1 = i0 + 1;
                i2 = i0 + xDim;
                i3 = i0 + 1 + xDim;

                x0 = buffer[i0];
                x1 = buffer[i1];
                x2 = buffer[i2];
                x3 = buffer[i3];

                if (detectionType == MARCHING_SQUARES) {

                    if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                        // case 0 - no edge
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 1 - edge in the lower left
                        edgeImage.set(i2);
                    } else if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 2 - edge in the lower right
                        edgeImage.set(i3);
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 3 - edge horizontally
                        edgeImage.set(i2);
                        edgeImage.set(i3);
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 4 - edge in the upper right
                        edgeImage.set(i1);
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 5 - ambiguous case; either edge in upper right and lower left or
                        // edge that goes from the upper right to the lower left
                        edgeImage.set(i1);
                        edgeImage.set(i2);
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 6 - edge going vertically along the right
                        edgeImage.set(i1);
                        edgeImage.set(i3);
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 < level)) {

                        // case 7 - edge in the upper left
                        edgeImage.set(i0);
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {

                        // case 8 - edge in the upper left
                        edgeImage.set(i0);
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 9 - edge going vertically along the left
                        edgeImage.set(i0);
                        edgeImage.set(i2);
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 10 - ambiguous case; either edge in upper left and lower right or
                        // edge that goes from the upper left to the lower right
                        edgeImage.set(i0);
                        edgeImage.set(i3);
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 11 - edge in the upper right
                        edgeImage.set(i1);
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 12 - edge going horizontally along the top
                        edgeImage.set(i0);
                        edgeImage.set(i1);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 13 - edge in the lower right
                        edgeImage.set(i3);
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 14 - edge in the lower left
                        edgeImage.set(i2);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 < level)) {
                        // case 15 - no edge
                    }
                } else if (detectionType == OLD_DETECTION) {

                    if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                        edgeImage.clear(i0);
                    } else if ((x0 <= level) && (x1 <= level) && (x2 <= level) && (x3 <= level)) {
                        edgeImage.clear(i0);
                    } else {
                        edgeImage.set(i0);
                    }
                }
            }
        }

        return edgeImage;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        GxxData = null;
        GyyData = null;
        GxData = null;
        GyData = null;
        GzData = null;
        GxyData = null;
        GxzData = null;
        GyzData = null;
        GzzData = null;
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

        if (destImage != null) {
            destImage.disposeLocal();
            destImage = null;
        }

        srcImage = null;
        kExtents = null;
        sigmas = null;
        super.finalize();
    }

    /**
     * Generates a zero crossing mask for a 2D function sets a ModelImage to 255 if a zero crossing is detected.
     *
     * @param  slice          DOCUMENT ME!
     * @param  buffer         array in which to find zero crossing
     * @param  buffer2        array which ensures that zero crossing is only counted if buffer2 value at that position
     *                        is less than zero
     * @param  detectionType  the type of zero crossing detection to perform
     */
    public void genZeroXMask(int slice, float[] buffer, float[] buffer2, int detectionType) {
        int i0, i1, i2, i3;
        float x0, x1, x2, x3;
        int i, j;
        int indexY;
        int length;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        length = xDim * yDim;

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;
        float level = 0;
        int offset = slice * length;

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                i0 = indexY + i;
                i1 = i0 + 1;
                i2 = i0 + xDim;
                i3 = i0 + 1 + xDim;

                x0 = buffer[i0];
                x1 = buffer[i1];
                x2 = buffer[i2];
                x3 = buffer[i3];

                if (detectionType == MARCHING_SQUARES) {

                    if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                        // case 0 - no edge
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 1 - edge in the lower left
                        if (buffer2[i2] < 0) {
                            zXMask.set(offset + i2, 255);
                        }
                    } else if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 2 - edge in the lower right
                        if (buffer2[i3] < 0) {
                            zXMask.set(offset + i3, 255);
                        }
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 3 - edge horizontally
                        if (buffer2[i2] < 0) {
                            zXMask.set(offset + i2, 255);
                        }

                        if (buffer2[i3] < 0) {
                            zXMask.set(offset + i3, 255);
                        }
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 4 - edge in the upper right
                        if (buffer2[i1] < 0) {
                            zXMask.set(offset + i1, 255);
                        }
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 5 - ambiguous case; either edge in upper right and lower left or
                        // edge that goes from the upper right to the lower left
                        if (buffer2[i1] < 0) {
                            zXMask.set(offset + i1, 255);
                        }

                        if (buffer2[i2] < 0) {
                            zXMask.set(offset + i2, 255);
                        }
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 6 - edge going vertically along the right
                        if (buffer2[i1] < 0) {
                            zXMask.set(offset + i1, 255);
                        }

                        if (buffer2[i3] < 0) {
                            zXMask.set(offset + i3, 255);
                        }
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 < level)) {

                        // case 7 - edge in the upper left
                        if (buffer2[i0] < 0) {
                            zXMask.set(offset + i0, 255);
                        }
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {

                        // case 8 - edge in the upper left
                        zXMask.set(offset + i0, 255);
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 9 - edge going vertially along the left
                        if (buffer2[i0] < 0) {
                            zXMask.set(offset + i0, 255);
                        }

                        if (buffer2[i2] < 0) {
                            zXMask.set(offset + i2, 255);
                        }
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 10 - ambiguous case; either edge in upper left and lower right or
                        // edge that goes from the upper left to the lower right
                        if (buffer2[i0] < 0) {
                            zXMask.set(offset + i0, 255);
                        }

                        if (buffer2[i3] < 0) {
                            zXMask.set(offset + i3, 255);
                        }
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 11 - edge in the upper right
                        if (buffer2[i1] < 0) {
                            zXMask.set(offset + i1, 255);
                        }
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 12 - edge going horizontally along the top
                        if (buffer2[i0] < 0) {
                            zXMask.set(offset + i0, 255);
                        }

                        if (buffer2[i1] < 0) {
                            zXMask.set(offset + i1, 255);
                        }
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 13 - edge in the lower right
                        if (buffer2[i3] < 0) {
                            zXMask.set(offset + i3, 255);
                        }
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 14 - edge in the lower left
                        if (buffer2[i2] < 0) {
                            zXMask.set(offset + i2, 255);
                        }
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 < level)) {
                        // case 15 - no edge
                    }
                } else if (detectionType == OLD_DETECTION) {

                    if ((x0 > level) && (x1 > level) && (x2 > level) && (x3 > level)) {
                        zXMask.set(offset + i0, 0);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 < level)) {
                        zXMask.set(offset + i0, 0);
                    } else if (buffer2[i0] >= 0) {
                        zXMask.set(offset + i0, 0);
                    } else {
                        zXMask.set(offset + i0, 255);
                    }
                }
            }
        }

        FileInfoBase[] fileInfo = zXMask.getFileInfo();
        fileInfo[slice].setModality(srcImage.getFileInfo()[slice].getModality());
        fileInfo[slice].setFileDirectory(srcImage.getFileInfo()[slice].getFileDirectory());
        fileInfo[slice].setEndianess(srcImage.getFileInfo()[slice].getEndianess());
        fileInfo[slice].setUnitsOfMeasure(srcImage.getFileInfo()[slice].getUnitsOfMeasure());
        fileInfo[slice].setResolutions(srcImage.getFileInfo()[slice].getResolutions());
        fileInfo[slice].setExtents(zXMask.getExtents());
        fileInfo[slice].setMax(255);
        fileInfo[slice].setMin(0);
        fileInfo[slice].setPixelPadValue(srcImage.getFileInfo()[slice].getPixelPadValue());
        fileInfo[slice].setPhotometric(srcImage.getFileInfo()[slice].getPhotometric());
    }

    /**
     * Accessor to return mask indicating zero crossings.
     *
     * @return  ModelImage of zero crossings (2D function); 255 = indicates zero crossing
     */
    public ModelImage getZeroXMask() {
        return zXMask;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int[] destExtents = null;

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

        try {

            if (srcImage.getNDims() == 2) {
                destExtents = new int[2];
                destExtents[0] = srcImage.getExtents()[0]; // X dim
                destExtents[1] = srcImage.getExtents()[1]; // Y dim
            } else if (srcImage.getNDims() == 3) {
                destExtents = new int[3];
                destExtents[0] = srcImage.getExtents()[0]; // X dim
                destExtents[1] = srcImage.getExtents()[1]; // Y dim
                destExtents[2] = srcImage.getExtents()[2]; // Z or T dim
            }

            zXMask = new ModelImage(ModelImage.UBYTE, destExtents, srcImage.getImageName() + "_edgeNM");
        } catch (OutOfMemoryError e) {
            destImage = null;
            srcImage = null;
            zXMask.disposeLocal();
            zXMask = null;
            errorCleanUp("Algorithm EdgeNMSup exportData: Out of memory", true);

            return;
        }

        if (destImage != null) { // NEW
            

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D(1, zeroDetectionType);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcStoreInDest3D(zeroDetectionType);
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcStoreInDest2D(srcImage.getExtents()[2], zeroDetectionType);
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }
    }

    /**
     * This function produces the EdgeNMSup of input image.
     *
     * @param  nImages        number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image
     *                        is to processed independently then nImages equals the number of images in the volume.
     * @param  detectionType  the type of zero crossing detection to perform
     */
    private void calcStoreInDest2D(int nImages, int detectionType) {
        int i, s, idx;
        int length, totalLength;
        int start;
        float[] buffer;
        float[] buffer2;
        float bufferMin, bufferMax;
        float a;
        AlgorithmConvolver convolver;

        try {
            destImage.setLock();
        } catch (IOException error) {
            errorCleanUp("Algorithm EdgeNMSup: Image(s) locked", false);

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

            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Edge ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            buffer2 = null;
            errorCleanUp("Algorithm EdgeNMSup exportData: Out of memory", true);

            return;
        }
        
        convolver = new AlgorithmConvolver(srcImage, GxData, GyData, GxxData, GxyData, GyyData,
                GxxxData, GxxyData, GxyyData, GyyyData, kExtents,entireImage);
        convolver.setMinProgressValue(0);
        convolver.setMaxProgressValue(80);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        convolver.run();
        
        int mod = totalLength / 20; // since progress bar is at 80

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                if ((((start + i) % mod) == 0)) {
                    fireProgressStateChanged(80 + (20 * (start + i)) / (totalLength - 1));
                }

                if ((entireImage == true) || mask.get(i)) {
                    destImage.set(idx, outputBuffer[2*idx]);
                    buffer2[i] = outputBuffer[2*idx+1];
                } // if (entireImage == true || mask.get(i))
                else {
                    destImage.set(idx, outputBuffer[2*idx]);
                    buffer2[i] = 1.0f; // > 0 so not interpreted as edge
                }
            }

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImage.exportDataNoLock(start, length, buffer);
            } catch (IOException error) {
                buffer = null;
                buffer2 = null;
                errorCleanUp("Algorithm EdgeNMSup exportData: " + error, true);

                return;
            }

            // Normalize data in buffer to have a range of 20000
            // Do not use a linear transformation which will shift negative to positive
            // or positive to negative
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

            genZeroXMask(s, buffer, buffer2, detectionType);
        }

        zXMask.calcMinMax();
        destImage.calcMinMax();
        destImage.releaseLock();


        setCompleted(true);
    }

    /**
     * This function produces the Non-maximum suppression of input image.
     *
     * @param  detectionType  the type of zero crossing detection to perform
     */
    private void calcStoreInDest3D(int detectionType) {
        int i, nImages, s;
        int length, totalLength;
        int start;
        float[] sliceBuffer;
        float[] sliceBuffer2;
        float bufferMin, bufferMax;
        float a;
        AlgorithmConvolver convolver;

        try {
            destImage.setLock();
        } catch (IOException error) {
            errorCleanUp("Algorithm EdgeNMSup: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            totalLength = srcImage.getSliceSize() * srcImage.getExtents()[2];
            nImages = srcImage.getExtents()[2];
            sliceBuffer = new float[length];
            sliceBuffer2 = new float[length];

            for (i = 0; i < length; i++) {
                sliceBuffer2[i] = 1.0f;
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating Zero X-ings ...");
        } catch (OutOfMemoryError e) {
            sliceBuffer = null;
            sliceBuffer2 = null;
            errorCleanUp("Algorithm EdgeNMSup exportData: Out of memory", true);

            return;
        }


        convolver = new AlgorithmConvolver(srcImage, GxData, GyData, GzData, GxxData, GxyData, GyyData,
                            GxzData, GyzData, GzzData, GxxxData, GxxyData, GxyyData, GyyyData, GxxzData,
                            GxzzData, GxyzData, GyyzData, GyzzData, GzzzData, kExtents,entireImage);
        convolver.setMinProgressValue(0);
        convolver.setMaxProgressValue(80);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        convolver.run();
        
        int mod = totalLength / 20; // since progress bar is at 80

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            for (i = start; (i < (start + length)) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) i / (totalLength - 1) * 100));
                }

                if ((entireImage == true) || mask.get(i)) {
                    destImage.set(i, outputBuffer[2*i]);
                    sliceBuffer2[i - start] = outputBuffer[2*i+1];
                } // if (entireImage == true || mask.get(i))
                else {
                    destImage.set(i, outputBuffer[2*i]);
                    sliceBuffer2[i - start] = 1.0f; // > 0 so not interpreted as edge
                }
            }

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImage.exportDataNoLock(start, length, sliceBuffer);
            } catch (IOException error) {
                displayError("Algorithm EdgeNMSup exportData: " + error);
                setCompleted(false);

                return;
            }

            // Normalize data in sliceBuffer to have a range of 20000
            // Do not use a linear transformation which will shift negative to positive
            // or positive to negative
            bufferMax = -Float.MAX_VALUE;
            bufferMin = Float.MAX_VALUE;

            for (i = 0; i < length; i++) {

                if (sliceBuffer[i] < bufferMin) {
                    bufferMin = sliceBuffer[i];
                }

                if (sliceBuffer[i] > bufferMax) {
                    bufferMax = sliceBuffer[i];
                }
            } // for (i = 0; i < length; i++)

            a = 20000.0f / (bufferMax - bufferMin);

            for (i = 0; i < length; i++) {
                sliceBuffer[i] *= a;
            } // for (i = 0; i < length; i++)

            genZeroXMask(s, sliceBuffer, sliceBuffer2, detectionType);
        }

        zXMask.calcMinMax();
        destImage.calcMinMax();
        destImage.releaseLock();


        setCompleted(true);
    }

    /**
     * Creates Gaussian derivative kernels.
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
     * Creates Gaussian derivative kernels.
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

        derivOrder[0] = 3;
        derivOrder[1] = 0;
        derivOrder[2] = 0;
        GxxxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxxx = new GenerateGaussian(GxxxData, kExtents, sigmas, derivOrder);

        Gxxx.calc(false);

        derivOrder[0] = 2;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GxxyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxxy = new GenerateGaussian(GxxyData, kExtents, sigmas, derivOrder);

        Gxxy.calc(false);

        derivOrder[0] = 1;
        derivOrder[1] = 2;
        derivOrder[2] = 0;
        GxyyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxyy = new GenerateGaussian(GxyyData, kExtents, sigmas, derivOrder);

        Gxyy.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 3;
        derivOrder[2] = 0;
        GyyyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyyy = new GenerateGaussian(GyyyData, kExtents, sigmas, derivOrder);

        Gyyy.calc(false);

        derivOrder[0] = 2;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GxxzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxxz = new GenerateGaussian(GxxzData, kExtents, sigmas, derivOrder);

        Gxxz.calc(false);

        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 2;
        GxzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxzz = new GenerateGaussian(GxzzData, kExtents, sigmas, derivOrder);

        Gxzz.calc(false);

        derivOrder[0] = 1;
        derivOrder[1] = 1;
        derivOrder[2] = 1;
        GxyzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxyz = new GenerateGaussian(GxyzData, kExtents, sigmas, derivOrder);

        Gxyz.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        derivOrder[1] = 1;
        GyyzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyyz = new GenerateGaussian(GyyzData, kExtents, sigmas, derivOrder);

        Gyyz.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 2;
        GyzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyzz = new GenerateGaussian(GyzzData, kExtents, sigmas, derivOrder);

        Gyzz.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 3;
        GzzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gzzz = new GenerateGaussian(GzzzData, kExtents, sigmas, derivOrder);

        Gzzz.calc(false);
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
