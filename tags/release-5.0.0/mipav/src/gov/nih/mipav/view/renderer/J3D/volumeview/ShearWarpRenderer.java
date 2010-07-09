package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * The base class renderer to support shear-warp rendering. See the document ShearWarpRendering.pdf for a detailed
 * description of the renderer architecture.
 */

public abstract class ShearWarpRenderer extends Renderer {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** encoding of voxels based on transparent runs. */
    protected short[][][][] m_aaaasEncode;

    /** DOCUMENT ME! */
    protected short[][][] m_aaasVolumeEncode;

    /** DOCUMENT ME! */
    protected float[][] m_aafBox;

    /** DOCUMENT ME! */
    protected float[][] m_aafM;

    /** permutations that just store voxel indices. */
    protected int[][] m_aaiIndex;

    /** DOCUMENT ME! */
    protected short[][] m_aasSliceEncode;

    /** DOCUMENT ME! */
    protected float[] m_afA, m_afB;

    /** for resampling a slice. */
    protected float[] m_afShear, m_afOffset;

    /**
     * Parallel viewing model. Camera direction is always (0,0,1). The volume image and bounding box axes, center at
     * origin, columns are direction, up, right. The matrix m_aafM[][] is for inverse warping.
     */
    protected int m_aiBound[], m_iQuantity;

    /** for clipping. */
    protected int[] m_aiClipMin, m_aiClipMax;

    /** DOCUMENT ME! */
    protected int[] m_aiCurrentI;

    /** intermediate image indicating that the number of times each pixels is processed. */
    protected int[] m_aiInterC = null;

    /** DOCUMENT ME! */
    protected int[] m_aiSliceBound;

    /** DOCUMENT ME! */
    protected int[] m_aiSliceMin, m_aiSliceMax;

    /** support for skipping opaque intermediate pixels. */
    protected short[] m_asSkip;

    /** flag set by derived class that uses encoding and skipping. */
    protected boolean m_bDoEncodeSkip;

    /** DOCUMENT ME! */
    protected float m_fInv255 = 1.0f / 256.0f;

    /**
     * Temporary quantities used in the inner loop of resampleSingle(). These exist since Java does not allow you to
     * pass references to variables to allow more than one to change on return from a function call.
     */
    protected int m_iI0, m_iSIndex, m_iInterIndex, m_iPixel;

    /** intermediate 2D image supporting quantities. */
    protected int m_iInterBound, m_iInterBoundM1, m_iInterQuantity;

    /** DOCUMENT ME! */
    protected int m_iInterOffset;

    /** the current permutation of the voxel indices (0, 1, or 2). */
    protected int m_iPermute;

    /** DOCUMENT ME! */
    protected int m_iSliceQuantity, m_iSlice;

    /**
     * To avoid memory reallocations. These are used in the bilinearly interpolation of a 2x2 block of voxels in the
     * current slice. The Pij are vertex positions. The MatColorij are the material colors of the voxels. The Cij are
     * the final dynamically lit colors of the voxels.
     */
    protected Point3f m_kP00 = new Point3f();

    /** DOCUMENT ME! */
    protected Point3f m_kP01 = new Point3f();

    /** DOCUMENT ME! */
    protected Point3f m_kP10 = new Point3f();

    /** DOCUMENT ME! */
    protected Point3f m_kP11 = new Point3f();

    /** DOCUMENT ME! */
    protected Point3f m_kPosition = new Point3f();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a renderer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format <code>B | (G << 8) | (R << 16)</code>.
     */
    public ShearWarpRenderer(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);

        // orientation of voxel image
        m_aafBox = new float[3][3];
        m_aafBox[0][0] = 1.0f;
        m_aafBox[0][1] = 0.0f;
        m_aafBox[0][2] = 0.0f;
        m_aafBox[1][0] = 0.0f;
        m_aafBox[1][1] = 1.0f;
        m_aafBox[1][2] = 0.0f;
        m_aafBox[2][0] = 0.0f;
        m_aafBox[2][1] = 0.0f;
        m_aafBox[2][2] = 1.0f;
        m_akAxis[0].set(1.0f, 0.0f, 0.0f);
        m_akAxis[1].set(0.0f, 1.0f, 0.0f);
        m_akAxis[2].set(0.0f, 0.0f, 1.0f);

        // input voxel image bounds
        m_aiBound = new int[3];
        m_aiBound[0] = m_iXBound;
        m_aiBound[1] = m_iYBound;
        m_aiBound[2] = m_iZBound;
        m_iQuantity = m_iXBound * m_iYBound * m_iZBound;
        m_aiClipMin = new int[3];
        m_aiClipMin[0] = 0;
        m_aiClipMin[1] = 0;
        m_aiClipMin[2] = 0;
        m_aiClipMax = new int[3];
        m_aiClipMax[0] = m_iXBound - 1;
        m_aiClipMax[1] = m_iYBound - 1;
        m_aiClipMax[2] = m_iZBound - 1;

        // compute the permuted arrays of voxel indices
        m_aaiIndex = new int[3][];
        m_aaiIndex[0] = new int[m_iQuantity];
        m_aaiIndex[1] = new int[m_iQuantity];
        m_aaiIndex[2] = new int[m_iQuantity];

        for (int iZ = 0; iZ < m_iZBound; iZ++) {

            for (int iY = 0; iY < m_iYBound; iY++) {

                for (int iX = 0; iX < m_iXBound; iX++) {

                    // (x,y,z)
                    int iIndex = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
                    m_aaiIndex[2][iIndex] = iIndex;

                    // (y,z,x)
                    m_aaiIndex[0][iY + (m_iYBound * (iZ + (m_iZBound * iX)))] = iIndex;

                    // (z,x,y)
                    m_aaiIndex[1][iZ + (m_iZBound * (iX + (m_iXBound * iY)))] = iIndex;
                }
            }
        }

        // compute bound for largest possible intermediate images
        m_iInterBound = m_aiBound[0];

        if (m_aiBound[1] > m_iInterBound) {
            m_iInterBound = m_aiBound[1];
        }

        if (m_aiBound[2] > m_iInterBound) {
            m_iInterBound = m_aiBound[2];
        }

        m_iInterBound *= 4;
        m_iInterBoundM1 = m_iInterBound - 1;
        m_iInterQuantity = m_iInterBound * m_iInterBound;

        // encoding and skipping
        m_bDoEncodeSkip = false;
        m_aaasVolumeEncode = null;
        m_aasSliceEncode = null;
        m_asSkip = null;
        m_aaaasEncode = null;

        // various arrays used in the code
        m_afShear = new float[2];
        m_afOffset = new float[2];
        m_afA = new float[2];
        m_afB = new float[2];
        m_aiSliceBound = new int[3];
        m_aiSliceMin = new int[3];
        m_aiSliceMax = new int[3];
        m_aafM = new float[2][2];

        // intermediate transparent flag image
        m_aiInterC = new int[m_iInterQuantity];

        // only parallel viewing is supported
        setParallel(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * The top level rendering call. This function calls beforeResampleAll, resampleAll, and mapIntermediateToFinal, all
     * virtual functions that are implemented in derived classes.
     *
     * @param  iDS  The number of slices to increment during the resampling phase. The value should be one or larger. If
     *              one, all slices of the volume data are resampled. If two, only every other slice is resampled. An
     *              input larger than one is used to allow fast rendering during rotation of the volume data. Once the
     *              rotation terminates, a composite with input of one should be called.
     */
    public synchronized void composite(int iDS) {
        long startTime = 0, now = 0;
        double elapsedTime = 0d;

        // compute maximum component of the box direction vector
        float fMax = 0.0f;
        int i, iMax = -1;

        for (i = 0; i < 3; i++) {
            float fAbs = Math.abs(m_aafBox[2][i]);

            if (fAbs > fMax) {
                fMax = fAbs;
                iMax = i;
            }
        }

        startTime = System.currentTimeMillis();
        traceInit();

        // composite in the appropriate direction
        if (iMax == 0) {
            beforeResampleAll(1, 2, 0);
        } else if (iMax == 1) {
            beforeResampleAll(2, 0, 1);
        } else {
            beforeResampleAll(0, 1, 2);
        }

        resampleAll(iDS);
        mapIntermediateToFinal();
        now = System.currentTimeMillis();
        elapsedTime = (double) (now - startTime);

        if (elapsedTime <= 0) {
            elapsedTime = (double) 0.0;
        }

        Preferences.debug("Shear elapse time = " + (double) (elapsedTime / 1000.0) + "\n"); // in seconds
    }

    /**
     * Disposes of image memory and associated objects.
     */
    public void disposeLocal() {
        m_aafBox = null;
        m_aafM = null;

        m_afShear = null;
        m_afOffset = null;
        m_afA = null;
        m_afB = null;
        m_aiSliceBound = null;
        m_aiBound = null;

        m_aiSliceMin = null;
        m_aiSliceMax = null;
        m_aiClipMin = null;
        m_aiClipMax = null;

        m_aaaasEncode = null;
        m_aaasVolumeEncode = null;
        m_aasSliceEncode = null;

        m_aaiIndex = null;
        m_asSkip = null;
        m_aiCurrentI = null;

        m_aiInterC = null;

        m_kP00 = null;
        m_kP01 = null;
        m_kP10 = null;
        m_kP11 = null;
        m_kPosition = null;

        super.disposeLocal();
        System.gc();
    }

    /**
     * Read the current axis at index i.
     *
     * @param   i  the axis index (0, 1, or 2)
     *
     * @return  the current axis vector at index i
     */
    public Vector3f getAxis(int i) {
        Vector3f kAxis = new Vector3f();
        kAxis.x = m_aafBox[0][i];
        kAxis.y = m_aafBox[1][i];
        kAxis.z = m_aafBox[2][i];

        return kAxis;
    }

    /**
     * Read the current axis at index i.
     *
     * @param  i      the axis index (0, 1, or 2)
     * @param  kAxis  the new axis vector at index i
     */
    public void getAxis(int i, Vector3f kAxis) {
        kAxis.x = m_aafBox[0][i];
        kAxis.y = m_aafBox[1][i];
        kAxis.z = m_aafBox[2][i];
    }

    /**
     * Rotate the oriented bounding box of the 3D image about the specified axis with the specified angle.
     *
     * @param  kAxisAngle  the axis and angle formulation for the rotation
     */
    public void rotateBy(AxisAngle4f kAxisAngle) {
        m_kRotate.set(kAxisAngle);

        int i;

        for (i = 0; i < 3; i++) {
            getAxis(i, m_akAxis[i]);
            m_kRotate.transform(m_akAxis[i]);
        }

        orthonormalize(m_akAxis);

        for (i = 0; i < 3; i++) {
            setAxis(i, m_akAxis[i]);
        }
    }

    /**
     * Rotate the oriented bounding box of the 3D image about the specified axis with the specified angle.
     *
     * @param  transform  The transform and its matrix by which to rotate the image.
     */
    public void rotateFrameBy(Transform3D transform) {

        double rY, rX, rZ;
        double sinrX, sinrY, sinrZ, cosrX, cosrY, cosrZ;
        Matrix3d matrix = new Matrix3d();

        transform.get(matrix);

        rY = -Math.asin(matrix.m02);

        if (Math.cos(rY) != 0) {
            rX = -Math.atan2(matrix.m12, matrix.m22);
            rZ = Math.atan2(matrix.m01, matrix.m00);
        } else {
            rX = -Math.atan2(matrix.m10, matrix.m11);
            rZ = 0;
        }

        cosrX = Math.cos(rX);
        sinrX = Math.sin(rX);
        cosrY = Math.cos(rY);
        sinrY = Math.sin(rY);
        cosrZ = Math.cos(rZ);
        sinrZ = Math.sin(rZ);

        matrix.m00 = cosrZ * cosrY;
        matrix.m01 = -sinrZ * cosrY;
        matrix.m02 = sinrY;

        matrix.m10 = (cosrZ * sinrY * sinrX) + (sinrZ * cosrX);
        matrix.m11 = (-sinrZ * sinrY * sinrX) + (cosrZ * cosrX);
        matrix.m12 = -cosrY * sinrX;

        matrix.m20 = (-cosrZ * sinrY * cosrX) + (sinrZ * sinrX);
        matrix.m21 = (sinrZ * sinrY * cosrX) + (cosrZ * sinrX);
        matrix.m22 = cosrY * cosrX;

        m_kRotate.set(matrix);
        m_akAxis[0] = new Vector3f(1.0f, 0.0f, 0.0f);
        m_akAxis[1] = new Vector3f(0.0f, 1.0f, 0.0f);
        m_akAxis[2] = new Vector3f(0.0f, 0.0f, 1.0f);

        for (int i = 0; i < 3; i++) {
            m_kRotate.transform(m_akAxis[i]);
        }

        orthonormalize(m_akAxis);

        for (int i = 0; i < 3; i++) {
            setAxis(i, m_akAxis[i]);
        }
    }

    /**
     * Change an axis of the oriented bounding box of the 3D image.
     *
     * @param  i      the axis index (0, 1, or 2)
     * @param  kAxis  the new axis vector at index i
     */
    public void setAxis(int i, Vector3f kAxis) {
        m_aafBox[0][i] = kAxis.x;
        m_aafBox[1][i] = kAxis.y;
        m_aafBox[2][i] = kAxis.z;
    }

    /**
     * Change the camera model.
     *
     * @param  bParallel  true for a parallel camera, false for a perspective camera
     */
    public void setParallel(boolean bParallel) {

        // no matter what is passed, only parallel viewing is supported
        super.setParallel(bParallel || true);
    }

    /**
     * Setup the X Negative clipping plane position.
     *
     * @param  value  position of the X negative clip slider.
     */
    public void setXBoundNeg(float value) {
        m_aiClipMin[0] = (int) (value * (m_aiBound[0] - 1));
    }

    /**
     * Setup the X positive clipping plane position.
     *
     * @param  value  position of the X positive clip slider.
     */
    public void setXBoundPos(float value) {
        m_aiClipMax[0] = (int) (value * (m_aiBound[0] - 1));
    }

    /**
     * Setup the Y Negative clipping plane position.
     *
     * @param  value  position of the Y negative clip slider.
     */
    public void setYBoundNeg(float value) {
        m_aiClipMin[1] = (int) (value * (m_aiBound[1] - 1));
    }

    /**
     * Setup the Y positive clipping plane position.
     *
     * @param  value  positin of the Y positve clip slider.
     */
    public void setYBoundPos(float value) {
        m_aiClipMax[1] = (int) (value * (m_aiBound[1] - 1));
    }

    /**
     * Setup the Z negative clipping plane position.
     *
     * @param  value  position of the Z negative clip slider.
     */
    public void setZBoundNeg(float value) {
        m_aiClipMin[2] = (int) (value * (m_aiBound[2] - 1));
    }

    /**
     * Setup the Z positive clipping plane position.
     *
     * @param  value  position of the Z positive clip slider.
     */
    public void setZBoundPos(float value) {
        m_aiClipMax[2] = (int) (value * (m_aiBound[2] - 1));
    }

    /**
     * Map the intermediate images to the final color image. This is a stub for derived classes. This function must be
     * implemented in at least one class in a derivation chain in order for that class to be considered non-abstract.
     */
    protected abstract void mapIntermediateToFinal();

    /**
     * Convert a 32-bit float to a 'byte' that represents an unsigned 8-bit integer (uint8). This is provided to allow
     * 'byte' to represent a color in [0,255], the latter range not supported by Java since it has no unsigned integer
     * types.
     *
     * @param   fValue  a float value in [0.0f,256.0f)
     *
     * @return  the corresponding uint8 value
     */
    protected static final byte fromFloat(float fValue) {
        return (byte) ((int) (fValue) & 0x0ff);
    }

    /**
     * Convert a 32-bit integer (int32) to a 'byte' that represents an unsigned 8-bit integer (uint8). This is provided
     * to allow 'byte' to represent a color in [0,255], the latter range not supported by Java since it has no unsigned
     * integer types.
     *
     * @param   iValue  an int32 value
     *
     * @return  the corresponding uint8 value
     */
    protected static final byte fromInt(int iValue) {
        return (byte) (iValue & 0x0ff);
    }

    /**
     * Convert a 'byte' that represents an unsigned 8-bit integer (uint8) to a 32-bit float. This is provided to allow
     * 'byte' to represent a color in [0,255], the latter range not supported by Java since it has no unsigned integer
     * types.
     *
     * @param   cValue  a uint8 value
     *
     * @return  the corresponding float value in [0.0f,255.0f]
     */
    protected static final float toFloat(byte cValue) {
        return (float) (cValue & 0x0ff);
    }

    /**
     * Convert a 'byte' that represents an unsigned 8-bit integer (uint8) to a signed 32-bit integer (int32). This is
     * provided to allow 'byte' to represent a color in [0,255], the latter range not supported by Java since it has no
     * unsigned integer types.
     *
     * @param   cValue  a uint8 value
     *
     * @return  the corresponding int32 value
     */
    protected static final int toInt(byte cValue) {
        return (int) (cValue & 0x0ff);
    }

    /**
     * This is a callback to be executed before resampleAll is executed. The idea is to initialize the necessary
     * parameters needed for the shear warp resampling of the volume data. The actual shear parameters are computed
     * based on the orientation of the bounding box of the volume data. The permuted slice bounds, index set, and
     * encoding table are selected based on the input permutation. The skip array is initialized to zero since before
     * rendering, all intermediate pixels are transparent. Derived classes may provide additional initializations as
     * needed.
     *
     * <p>Each one of the indices is a permutation; either one of (0,1,2), (1,2,0), (2,0,1).</p>
     *
     * @param  k0  Index to <code>m_aiBound</code>, a value of 0, 1 or 2.
     * @param  k1  Index to <code>m_aiBound</code>, a value of 0, 1 or 2.
     * @param  k2  Index to <code>m_aiBound</code>, a value of 0, 1 or 2.
     */
    protected void beforeResampleAll(int k0, int k1, int k2) {

        // save permutation for use in computing positions
        m_iPermute = k2;

        // all intermediate voxels have not been processed yet
        Arrays.fill(m_aiInterC, 0);

        // compute bounding rectangle for sheared slices
        float fInvMax = 1.0f / m_aafBox[2][k2];
        m_afShear[0] = -m_aafBox[2][k0] * fInvMax;
        m_afOffset[0] = 0.5f * (m_iInterBoundM1 - ((m_aiBound[k0] - 1) + (m_afShear[0] * (m_aiBound[k2] - 1))));
        m_afShear[1] = -m_aafBox[2][k1] * fInvMax;
        m_afOffset[1] = 0.5f * (m_iInterBoundM1 - ((m_aiBound[k1] - 1) + (m_afShear[1] * (m_aiBound[k2] - 1))));

        // inverse shear warp
        float fDet = (m_aafBox[0][k0] * m_aafBox[1][k1]) - (m_aafBox[0][k1] * m_aafBox[1][k0]);
        float fInvDet = 1.0f / fDet;
        m_aafM[0][0] = m_aafBox[1][k1] * fInvDet;
        m_aafM[0][1] = -m_aafBox[0][k1] * fInvDet;
        m_aafM[1][0] = -m_aafBox[1][k0] * fInvDet;
        m_aafM[1][1] = m_aafBox[0][k0] * fInvDet;

        // slice bounds and slice selection
        m_aiSliceBound[0] = m_aiBound[k0];
        m_aiSliceBound[1] = m_aiBound[k1];
        m_aiSliceBound[2] = m_aiBound[k2];
        m_iSliceQuantity = m_aiSliceBound[0] * m_aiSliceBound[1];
        m_aiSliceMin[0] = m_aiClipMin[k0];
        m_aiSliceMin[1] = m_aiClipMin[k1];
        m_aiSliceMin[2] = m_aiClipMin[k2];
        m_aiSliceMax[0] = m_aiClipMax[k0];
        m_aiSliceMax[1] = m_aiClipMax[k1];
        m_aiSliceMax[2] = m_aiClipMax[k2];

        // current index array
        m_aiCurrentI = m_aaiIndex[k2];

        if (m_bDoEncodeSkip) {
            m_aaasVolumeEncode = m_aaaasEncode[k2];
            Arrays.fill(m_asSkip, (short) 0);
        }
    }

    /**
     * This is a callback to be executed before resampleSingle is executed. The bilinear interpolation coefficients are
     * computed here and vary with the single, active slice. The offset into the intermediate image is also computed
     * since this also varies with the active slice. If encoding of transparent data has occurred in the renderer
     * constructor, then the current slice of the encoding table is initialized for use by resampleSingle.
     */
    protected void beforeResampleSingle() {

        // compute the 0-direction index ranges and weighting factors
        float fMin = (m_afShear[0] * m_iSlice) + m_afOffset[0];
        m_afA[0] = fMin - (float) Math.floor(fMin);
        m_afB[0] = 1.0f - m_afA[0];

        int iMin0 = (int) Math.ceil(fMin);

        // compute the 0-direction index ranges and weighting factors
        fMin = (m_afShear[1] * m_iSlice) + m_afOffset[1];
        m_afA[1] = fMin - (float) Math.floor(fMin);
        m_afB[1] = 1.0f - m_afA[1];

        int iMin1 = (int) Math.ceil(fMin);

        // offset into intermediate image of rendered voxel data
        m_iInterOffset = iMin0 + (m_iInterBound * iMin1);

        if (m_bDoEncodeSkip) {
            m_aasSliceEncode = m_aaasVolumeEncode[m_iSlice];
        }
    }

    /**
     * Calculate the encoding table for the specified permutation. See the comments for initializeEncodeSkip. Because
     * bilinear interpolation is used, 2x2 blocks of voxels in the current slice must be analyzed for transparency. The
     * 2x2 block is tagged as non-transparent if and only if at least one of the voxels is non-transparent.
     *
     * <p>Each one of the indices is a permutation; either one of (0,1,2), (1,2,0), (2,0,1).</p>
     *
     * @param  k0         Index to <code>m_aiBound</code>, a value of 0, 1 or 2.
     * @param  k1         Index to <code>m_aiBound</code>, a value of 0, 1 or 2.
     * @param  k2         Index to <code>m_aiBound</code>, a value of 0, 1 or 2.
     * @param  acChannel  The blue channel for intensity renderers or the alpha channel for color renderers.
     */
    protected void computeEncode(int k0, int k1, int k2, byte[] acChannel) {
        int iQuantity = m_aiBound[k0] * m_aiBound[k1];
        int[] aiCurrentI = m_aaiIndex[k2];

        for (int i2 = 0; i2 < m_aiBound[k2]; i2++) {

            for (int i1 = 0; i1 < (m_aiBound[k1] - 1); i1++) {
                short[] asEncode = m_aaaasEncode[k2][i2][i1];
                int iSize = 0;

                for (int i0 = 0; i0 < (m_aiBound[k0] - 1); i0++) {
                    int i = i0 + (m_aiBound[k0] * (i1 + (m_aiBound[k1] * i2)));

                    if ((acChannel[aiCurrentI[i]] != 0) || (acChannel[aiCurrentI[i + 1]] != 0) ||
                            (acChannel[aiCurrentI[i + m_aiBound[k0]]] != 0) ||
                            (acChannel[aiCurrentI[i + 1 + m_aiBound[k0]]] != 0)) {
                        asEncode[iSize++] = (short) i0;
                    }
                }

                // end-of-array sentinel
                asEncode[iSize] = -1;
            }
        }
    }

    /**
     * Compute the vertex positions for the four voxels in the current slice to be bilinearly interpolated.
     *
     * @param  i0  the current permuted indices of the upper left voxel in the 2x2 block of voxels in the slice
     * @param  i1  DOCUMENT ME!
     * @param  i2  DOCUMENT ME!
     */
    protected final void computePositions(int i0, int i1, int i2) {

        switch (m_iPermute) {

            case 0:
                m_kP00.x = i0 - (0.5f * m_aiBound[0]);
                m_kP00.y = i1 - (0.5f * m_aiBound[1]);
                m_kP00.z = i2 - (0.5f * m_aiBound[2]);
                m_kP10.x = m_kP00.x + 1.0f;
                m_kP10.y = m_kP00.y;
                m_kP10.z = m_kP00.z;
                m_kP01.x = m_kP00.x;
                m_kP01.y = m_kP00.y + 1.0f;
                m_kP01.z = m_kP00.z;
                m_kP11.x = m_kP10.x;
                m_kP11.y = m_kP01.y;
                m_kP11.z = m_kP00.z;
                break;

            case 1:
                m_kP00.y = i0 - (0.5f * m_aiBound[1]);
                m_kP00.z = i1 - (0.5f * m_aiBound[2]);
                m_kP00.x = i2 - (0.5f * m_aiBound[0]);
                m_kP10.y = m_kP00.y + 1.0f;
                m_kP10.z = m_kP00.z;
                m_kP10.x = m_kP00.x;
                m_kP01.y = m_kP00.y;
                m_kP01.z = m_kP00.z + 1.0f;
                m_kP01.x = m_kP00.x;
                m_kP11.y = m_kP10.y;
                m_kP11.z = m_kP01.z;
                m_kP11.x = m_kP00.x;
                break;

            case 2:
                m_kP00.z = i0 - (0.5f * m_aiBound[2]);
                m_kP00.x = i1 - (0.5f * m_aiBound[0]);
                m_kP00.y = i2 - (0.5f * m_aiBound[1]);
                m_kP10.z = m_kP00.z + 1.0f;
                m_kP10.x = m_kP00.x;
                m_kP10.y = m_kP00.y;
                m_kP01.z = m_kP00.z;
                m_kP01.x = m_kP00.x + 1.0f;
                m_kP01.y = m_kP00.y;
                m_kP11.z = m_kP10.z;
                m_kP11.x = m_kP01.x;
                m_kP11.y = m_kP00.y;
                break;
        }
    }

    /**
     * Calls disposeLocal of this class to ensure this class nulls the references to global class variables so that
     * memory will be recovered.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    /**
     * Calculate the encoding of the specified channel. The tables calculated by this function is used to skip
     * transparent voxels during the resampling phase. This leads to a significant performance improvement for data sets
     * that have a relatively large number of transparent voxels. Each table corresponds to one of the three
     * permutations of the image indices. The active permutation is determined by the composite function based on the
     * orientation of the bounding box of the volume data.
     *
     * @param  acChannel  the blue channel for intensity renderers or the alpha channel for color renderers
     */
    protected void initializeEncodeSkip(byte[] acChannel) {
        m_bDoEncodeSkip = true;
        m_aaasVolumeEncode = null;
        m_aasSliceEncode = null;

        m_aaaasEncode = new short[3][][][];

        for (int k2 = 0; k2 < 3; k2++) {
            int k0 = (k2 + 1) % 3, k1 = (k2 + 2) % 3;

            m_aaaasEncode[k2] = new short[m_aiBound[k2]][][];

            for (int i2 = 0; i2 < m_aiBound[k2]; i2++) {
                m_aaaasEncode[k2][i2] = new short[m_aiBound[k1] - 1][];

                for (int i1 = 0; i1 < (m_aiBound[k1] - 1); i1++) {
                    m_aaaasEncode[k2][i2][i1] = new short[m_aiBound[k0]];
                }
            }

            computeEncode(k0, k1, k2, acChannel);
        }

        m_asSkip = new short[m_iInterQuantity];
    }

    /**
     * Resample all the slices for the current permuted volume data. This is a stub for derived classes. This function
     * must be implemented by at least one class in a derivation chain in order for that class to be considered
     * non-abstract.
     *
     * @param  iDS  The number of slices to increment during the resampling phase. The value should be one or larger. If
     *              one, all slices of the volume data are resampled. If two, only every other slice is resampled. An
     *              input larger than one is used to allow fast rendering during rotation of the volume data. Once the
     *              rotation terminates, a composite with input of one should be called.
     */
    protected void resampleAll(int iDS) { }

    /**
     * Resample a single slice of the permuted volume data. This is a stub for derived classes. This function must be
     * implemented in at least one class in a derivation chain in order for that class to be considered non-abstract.
     */
    protected void resampleSingle() { }

    /**
     * An array is maintained that keeps track of opaque intermediate pixels during the resampling phase. Whenever
     * pixels become opaque, they can be skipped if revisited later in the resampling. As runs of pixels become opaque
     * on a scan line, skipping the run can greatly increase the speed of rendering. This function is called in
     * resampleSingle by all the renderer classes taking advantage of opacity skipping.
     *
     * @param   i1       the current slice index of the current permuted index set
     * @param   asIndex  the skip array for the current scan line of the slice
     *
     * @return  The return value is 'true' if and only if there is another nonopaque pixel to be processed. If 'true',
     *          then m_iI0, m_iInterIndex, and m_iPixel contain values that are needed for later calculations in
     *          resampleSingle. The variable m_iInterIndex stores the offset into the intermediate and skip arrays for
     *          the current nonopaque pixel. The variable m_iPixel stores the offset into the volume data for the
     *          current nonopaque voxel. The variable m_iI0 is one of the indices for the volume data and is only needed
     *          in the MjLightingRenderer class for computing the voxel position from the permuted indices.
     */
    protected boolean skipToNonopaque(int i1, short[] asIndex) {

        while (true) {

            // skip to next nontransparent voxel of scan line
            m_iI0 = asIndex[m_iSIndex];
            m_iInterIndex = m_iInterOffset + m_iI0;

            if (m_asSkip[m_iInterIndex] == 0) {

                // found nontransparent voxel and nonopaque pixel
                m_iPixel = m_iI0 + (m_aiSliceBound[0] * (i1 + (m_aiSliceBound[1] * m_iSlice)));

                return true;
            }

            // skip to next non-opaque pixel of scan line
            m_iI0 += m_asSkip[m_iInterIndex];

            if (m_iI0 >= (m_aiSliceBound[0] - 1)) {
                return false;
            }

            // at this point, m_iI0 > asIndex[m_iSIndex],
            while ((asIndex[m_iSIndex] != -1) && (m_iI0 > asIndex[m_iSIndex])) {
                m_iSIndex++;
            }

            if (asIndex[m_iSIndex] == -1) {
                return false;
            }

            // assert: asIndex[m_iSIndex-1] < m_iI0 <= asIndex[m_iSIndex]
        }
    }

    /**
     * After the compositing step in resampleSingle, the intensity (for gray scale) or alpha value (for color) might
     * become its maximum value. In this case, the intermediate pixel is opaque. The skip array must be updated so that
     * this pixel is skipped if revisited later.
     */
    protected void updateSkip() {

        // Your right neighbor stores how far to skip to the next nonopaque
        // pixel.  You must skip one more than that.
        short sValue = (short) (m_asSkip[m_iInterIndex + 1] + 1);
        m_asSkip[m_iInterIndex] = sValue;

        // Your last skip value was zero.  Your left neighbor's value must
        // be either zero or one.  If one, then your left neighbor used to
        // skip to you.  But now that you skip somewhere further to the
        // right, the left neighbor and any previous skipping neighbors must
        // skip by one more than they used to.
        int i = m_iInterIndex - 1;

        while ((i >= m_iInterOffset) && (m_asSkip[i] > 0)) {
            m_asSkip[i--] = ++sValue;
        }
    }
}
