package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Reslices 3D image into (isotropic)cubic voxels. This class assumes that the the X and Y dimensions are equal and that
 * the Z dimension is of lower resolution than of X and Y. However, it does not assume equal spacing in the input image
 * between all slices (sometimes common in CT and MR images). Note that since this class only adds images between slices
 * it reduces to a 1D problem. For example, we only need the pixel above and below for linear interp since there isn't
 * any change in X or change in Y.
 *
 * @version  0.1 Sept 14, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmReslice extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Linear interpolation mode. */
    public static final int LINEAR = 0;

    /** Cubic b-spline interpolation mode. */
    public static final int CUBIC_BSPLINE = 1;

    /** Cubic interpolation mode. */
    public static final int CUBIC = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Interpolation mode (Linear, Cubic Bspline, Cubic Convolution). */
    private int interpMode;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new algorithm for reslicing to isotropic voxels.
     *
     * @param  srcImg       source image model
     * @param  _interpMode  interpolation mode supported by the class.
     */
    public AlgorithmReslice(ModelImage srcImg, int _interpMode) {
        super(null, srcImg);
        interpMode = _interpMode;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Accessor to get interpolated image.
     *
     * @return  the interpolated image
     */
    public ModelImage getResultImage() {
        return destImage;
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

        if (srcImage.getNDims() != 3) {
            displayError("Source Image is not 3D");
            finalize();

            return;
        }

        

        if (interpMode == LINEAR) {
            linearReslice();
        } else if ((interpMode == CUBIC_BSPLINE) || (interpMode == CUBIC)) {
            cubicReslice();
        }
    }

    /**
     * Reslice data is Cubic Bspline where.
     *
     * <p>R(u) = 2/3 + 0.5*|u|^3 - u^2 for 0 <= |u| < 1</p>
     *
     * <p>R(u) = 1/6 * (2 - |u|)^3 for 1 <= |u| < 2</p>
     *
     * @param   u  is the position to be interpolated
     *
     * @return  DOCUMENT ME!
     */
    private float calcRBSpline(float u) {
        float uu;
        float uLessTwo;

        u = Math.abs(u);

        if (u < 1) {
            uu = u * u;

            return (0.6666667f + (0.5f * uu * u) - uu);
        } else {
            uLessTwo = 2 - u;

            return (0.1666667f * uLessTwo * uLessTwo * uLessTwo);
        }
    }

    /**
     * Reslice using cubic convolution reslice where.
     *
     * <p>R(u) = 3/2*|u|^3 - 5/2*u^2 + 1 for 0 <= |u| < 1</p>
     *
     * <p>R(u) = -1/2*|u|^3 + 5/2*u^2 - 4*|u| + 2 for 1 <= |u| <= 2</p>
     *
     * @param   u  is the position to be interpolated
     *
     * @return  the interpolated value
     */
    private float calcRCUBIC(float u) {
        float uu;

        u = Math.abs(u);
        uu = u * u;

        if (u < 1) {
            return (((1.5f * uu * u) - (2.5f * uu) + 1.0f));
        } else {
            return (((-0.5f * uu * u) + (2.5f * uu) - (4.0f * u) + 2.0f));
        }
    }

    /**
     * Reslices the data into isotropic voxels using one of two interpolation methods (cubic Bspline or cubic
     * convolution). Positions between the first and second image, and the last two images are interpolated using
     * terminal slices in 2 of the buffers instead of only in 1 buffer because traditional usage of the interpolation
     * function in these positions would exceed the bounds of the input data.
     */
    private void cubicReslice() {

        int i, j, d;
        int length;
        float zRes = 0, xyRes;
        float[] bufferA;
        float[] bufferB;
        float[] bufferC;
        float[] bufferD;
        int[] destExtents = new int[3];
        float[] resolutions = new float[3];
        float pct;
        float incAmt;
        int slicesNeeded = 0;
        float minImage, maxImage;

        if ((srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.ARGB)) {
            minImage = 0.0f;
            maxImage = 255.0f;
        } else if ((srcImage.getType() == ModelStorageBase.USHORT) ||
                       (srcImage.getType() == ModelStorageBase.ARGB_USHORT)) {
            minImage = 0.0f;
            maxImage = 65535.0f;
        } else if (srcImage.getType() == ModelStorageBase.BYTE) {
            minImage = -128.0f;
            maxImage = 127.0f;
        } else if (srcImage.getType() == ModelStorageBase.SHORT) {
            minImage = -32768.0f;
            maxImage = 32767.0f;
        } else if (srcImage.getType() == ModelStorageBase.INTEGER) {
            minImage = (float) Integer.MIN_VALUE;
            maxImage = (float) Integer.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.UINTEGER) {
            minImage = 0.0f;
            maxImage = (float) 4294967295L;
        } else if (srcImage.getType() == ModelStorageBase.LONG) {
            minImage = (float) Long.MIN_VALUE;
            maxImage = (float) Long.MAX_VALUE;
        } else {
            minImage = -Float.MAX_VALUE;
            maxImage = Float.MAX_VALUE;
        }

        destExtents[0] = srcImage.getExtents()[0];
        destExtents[1] = srcImage.getExtents()[1];

        xyRes = srcImage.getFileInfo(0).getResolutions()[0];
        pct = 0.0f;

        for (i = 2; i < (srcImage.getExtents()[2] + 1); i++) {

            if (i <= (srcImage.getExtents()[2] - 1)) {
                zRes = srcImage.getFileInfo(i).getResolutions()[2];
            } else {
                zRes = srcImage.getFileInfo(srcImage.getExtents()[2] - 1).getResolutions()[2];
            }

            incAmt = xyRes / zRes;

            for (; pct < 1.0f; pct += incAmt) {
                slicesNeeded++;
            }

            pct = pct - 1.0f;
        }

        destExtents[2] = slicesNeeded;

        try {
            destImage = new ModelImage(srcImage.getType(), destExtents, "Isotropic");
        } catch (OutOfMemoryError x) {
            errorCleanUp("Reslice: unable to allocate enough memory", false);

            if (destImage != null) {
                destImage.disposeLocal();
                destImage = null;
            }

            return;
        }

        FileInfoBase fileInfo;

        // set destImage resolutions
        xyRes = srcImage.getFileInfo(0).getResolutions()[0];

        for (i = 0; i < destImage.getExtents()[2]; i++) {
            fileInfo = destImage.getFileInfo(i);
            resolutions[0] = resolutions[1] = xyRes;
            resolutions[2] = xyRes;
            fileInfo.setResolutions(resolutions);
            destImage.setFileInfo(fileInfo, i);
        }

        length = srcImage.getSliceSize();

        if (srcImage.isColorImage()) {
            length = 4 * length;
        }

        try {
            bufferA = new float[length];
            bufferB = new float[length];
            bufferC = new float[length];
            bufferD = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Forming isotropic dataset ...");
            // r1      = new float[TABLE_SIZE]; r2      = new float[TABLE_SIZE];
        } catch (OutOfMemoryError e) {
            bufferA = null;
            bufferB = null;
            bufferC = null;
            bufferD = null;
            errorCleanUp("Algorithm Reslice: Out of memory", true);

            return;
        }


        // makeTables();

        d = 0;
        pct = 0.0f;

        float s1, s2, s3, s4, total;
        float[] temp;

        try {
            srcImage.exportData(0, length, bufferA);
            srcImage.exportData(0, length, bufferB);
            srcImage.exportData(length, length, bufferC);
        } catch (IOException error) {

            if (destImage != null) {
                destImage.disposeLocal();
                destImage = null;
            }

            bufferA = null;
            bufferB = null;
            bufferC = null;
            bufferD = null;

            errorCleanUp("Algorithm Reslice: Image(s) locked", true);

            return;
        }

        for (i = 2; (i < (srcImage.getExtents()[2] + 1)) && !threadStopped; i++) {

            try {

                if (i < srcImage.getExtents()[2]) {
                    srcImage.exportData(i * length, length, bufferD);
                } else {
                    srcImage.exportData((srcImage.getExtents()[2] - 1) * length, length, bufferD);
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Reslice: Image(s) locked", false);

                return;
            }

            fireProgressStateChanged(Math.round((float) (i - 2) / (srcImage.getExtents()[2] - 1) * 100));

            if (i < srcImage.getExtents()[2]) {
                zRes = srcImage.getFileInfo(i).getResolutions()[2];
                xyRes = srcImage.getFileInfo(i).getResolutions()[0];
                incAmt = xyRes / zRes;
            } else {
                zRes = srcImage.getFileInfo(srcImage.getExtents()[2] - 1).getResolutions()[2];
                xyRes = srcImage.getFileInfo(srcImage.getExtents()[2] - 1).getResolutions()[0];
                incAmt = xyRes / zRes;
            }

            if (interpMode == CUBIC_BSPLINE) {

                for (; (pct <= 1) && !threadStopped; pct += incAmt) {

                    for (j = 0; (j < length) && !threadStopped; j++) {
                        s1 = bufferA[j] * calcRBSpline((pct + 1));
                        s2 = bufferB[j] * calcRBSpline((pct));
                        s3 = bufferC[j] * calcRBSpline((pct - 1));
                        s4 = bufferD[j] * calcRBSpline((pct - 2));
                        destImage.set(d++, s1 + s2 + s3 + s4);
                    }
                }
            } else {

                for (; (pct <= 1) && !threadStopped; pct += incAmt) {

                    for (j = 0; (j < length) && !threadStopped; j++) {
                        s1 = bufferA[j] * calcRCUBIC((pct + 1));
                        s2 = bufferB[j] * calcRCUBIC((pct));
                        s3 = bufferC[j] * calcRCUBIC((pct - 1)); // -pct + 1
                        s4 = bufferD[j] * calcRCUBIC((pct - 2));
                        total = s1 + s2 + s3 + s4;

                        if (total < minImage) {
                            total = minImage;
                        } else if (total > maxImage) {
                            total = maxImage;
                        }

                        destImage.set(d++, total);
                    }
                }
            }

            temp = bufferA;
            bufferA = bufferB;
            bufferB = bufferC;
            bufferC = bufferD;
            bufferD = temp;
            pct = pct - 1;
        }

        if (threadStopped) {
            bufferA = null;
            bufferB = null;
            bufferC = null;
            bufferD = null;

            if (destImage != null) {
                destImage.disposeLocal();
                destImage = null;
            }

            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        setCompleted(true);
    }

    /**
     * Reslices the data into isotropic voxels using linear interpolation.
     */
    private void linearReslice() {

        int i, j, k, d;
        int length;
        float zRes = 0, xyRes;
        float[] buffer;
        int[] destExtents = new int[3];
        float[] resolutions = new float[3];
        float pct;
        float incAmt;
        int slicesNeeded = 0;

        destExtents[0] = srcImage.getExtents()[0];
        destExtents[1] = srcImage.getExtents()[1];

        xyRes = srcImage.getFileInfo(0).getResolutions()[0];
        pct = 0.0f;

        for (i = 0; i < (srcImage.getExtents()[2] - 1); i++) {
            zRes = srcImage.getFileInfo(i).getResolutions()[2];
            incAmt = xyRes / zRes;

            for (; pct < 1.0f; pct += incAmt) {
                slicesNeeded++;
            }

            pct = pct - 1.0f;
        }

        destExtents[2] = slicesNeeded;

        try {
            destImage = new ModelImage(srcImage.getType(), destExtents, "Isotropic");
        } catch (OutOfMemoryError x) {
            errorCleanUp("Reslice: unable to allocate enough memory", true);

            if (destImage != null) {
                destImage.disposeLocal();
                destImage = null;
            }

            return;
        }

        FileInfoBase fileInfo;

        // set destImage resolutions
        // xyRes = srcImage.getFileInfo(i).getResolutions()[0];
        for (i = 0; i < destImage.getExtents()[2]; i++) {
            fileInfo = destImage.getFileInfo(i);
            resolutions[0] = resolutions[1] = xyRes;
            resolutions[2] = xyRes;
            fileInfo.setResolutions(resolutions);
            destImage.setFileInfo(fileInfo, i);
        }

        length = srcImage.getSliceSize();

        if (srcImage.isColorImage()) {
            length = 4 * length;
        }

        try {
            buffer = new float[2 * length]; // allocate array for 2 images
            fireProgressStateChanged(srcImage.getImageName(), "Forming isotropic dataset ...");
        } catch (OutOfMemoryError e) {
            buffer = null;

            if (destImage != null) {
                destImage.disposeLocal();
                destImage = null;
            }

            errorCleanUp("Algorithm Reslice: Out of memory", false);

            return;
        }

        d = 0;
        pct = 0;

        float diff;

        for (i = 0; (i < (srcImage.getExtents()[2] - 1)) && !threadStopped; i++) {

            try {
                srcImage.exportData(i * length, 2 * length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Reslice: Image(s) locked");
                setCompleted(false);

                return;
            }

            fireProgressStateChanged(Math.round((float) i / (srcImage.getExtents()[2] - 1) * 100));

            zRes = srcImage.getFileInfo(i).getResolutions()[2];
            xyRes = srcImage.getFileInfo(i).getResolutions()[0];
            incAmt = xyRes / zRes;

            for (; (pct < 1) && !threadStopped; pct += incAmt) {
                diff = 1 - pct;

                for (j = 0, k = length; (j < length) && !threadStopped; j++, k++) {
                    destImage.set(d++, (buffer[j] * diff) + (buffer[k] * pct));
                }
            }

            pct = pct - 1;
        }

        if (threadStopped) {
            buffer = null;

            if (destImage != null) {
                destImage.disposeLocal();
                destImage = null;
            }

            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        setCompleted(true);
    }

    /*
     * private final float calcRT(float u) { float uu; int   tableSize = TABLE_SIZE-1;
     *
     * if (u < 1){ return (r1[(int)(u*tableSize)]); } else{ return (r2[(int)((u-1)*tableSize)]); } }
     *
     * private void makeTables(){ int i; float u; float uu; float uLessTwo;
     *
     * for(i = 0; i < TABLE_SIZE; i++){ u = ((float)i)/TABLE_SIZE; uu = u*u; r1[i] = 0.6667f + 0.5f*uu*u - uu;
     *
     * u += 1; uLessTwo = 2-u; r2[i] = 0.16667f * (uLessTwo * uLessTwo * uLessTwo); } }
     */

}
