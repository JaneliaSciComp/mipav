package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Provides functions to convolve an image with the second derivitive of the Gaussian function to obtain the Hessian
 * matrix for a particular point on the image.
 *
 * @author  Evan McCreedy
 */
public class AlgorithmHessian extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Storage location of the second derivative of the Gaussian in the XX direction. */
    private float[] GxxData;

    /** Storage location of the second derivative of the Gaussian in the XY direction. */
    private float[] GxyData;

    /** Storage location of the second derivative of the Gaussian in the XZ direction. */
    private float[] GxzData;

    /** Storage location of the second derivative of the Gaussian in the YY direction. */
    private float[] GyyData;

    /** Storage location of the second derivative of the Gaussian in the YZ direction. */
    private float[] GyzData;

    /** Storage location of the second derivative of the Gaussian in the ZZ direction. */
    private float[] GzzData;

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;

    /** Image to convolve with hessian. */
    private ModelImage srcImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmHessian object.
     *
     * @param  img     DOCUMENT ME!
     * @param  sigmas  DOCUMENT ME!
     */
    public AlgorithmHessian(ModelImage img, float[] sigmas) {
        srcImage = img;
        this.sigmas = sigmas;

        if (srcImage.getNDims() == 2) {
            makeKernels2D();
        } else {

            if (sigmas[2] == 0.0f) {
                makeKernels2D();
            } else {
                makeKernels3D();
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Convolves the source image at the point (x,y) with the hessian of the gaussian function.
     *
     * @param   x  float point index indicating point of interest
     * @param   y  float point index indicating point of interest
     *
     * @return  hessian matrix for the point (x,y)
     */
    public double[][] hessian2D(float x, float y) {
        Vector2f pt = new Vector2f(x, y);
        int[] extents = srcImage.getExtents();
        int length = extents[0] * extents[1];
        float[] imgBuf = new float[length];

        try {
            srcImage.exportData(0, length, imgBuf);
        } catch (IOException ioe) {
            MipavUtil.displayError("Error caught exporting image");
            setCompleted(false);

            return null;
        }

        float ixx, ixy, iyy;

        ixx = AlgorithmConvolver.convolve2DPt(pt, extents, imgBuf, kExtents, GxxData);
        iyy = AlgorithmConvolver.convolve2DPt(pt, extents, imgBuf, kExtents, GyyData);
        ixy = AlgorithmConvolver.convolve2DPt(pt, extents, imgBuf, kExtents, GxyData);

        double[][] matrix = new double[2][2];

        matrix[0][0] = ixx;
        matrix[0][1] = ixy;
        matrix[1][0] = ixy;
        matrix[1][1] = iyy;

        return matrix;
    }

    /**
     * Convolves the source image at the point (x,y,z) with the hessian of the gaussian function.
     *
     * @param   imgBuf   the image
     * @param   extents  the extents of the image
     * @param   x        float point index indicating point of interest
     * @param   y        float point index indicating point of interest
     *
     * @return  hessian matrix for the point (x,y,z)
     */
    public double[][] hessian2D(float[] imgBuf, int[] extents, float x, float y) {
        Vector2f pt = new Vector2f(x, y);

        float ixx, ixy, iyy;

        ixx = AlgorithmConvolver.convolve2DPt(pt, extents, imgBuf, kExtents, GxxData);
        iyy = AlgorithmConvolver.convolve2DPt(pt, extents, imgBuf, kExtents, GyyData);
        ixy = AlgorithmConvolver.convolve2DPt(pt, extents, imgBuf, kExtents, GxyData);

        double[][] matrix = new double[2][2];

        matrix[0][0] = ixx;
        matrix[0][1] = ixy;
        matrix[1][0] = ixy;
        matrix[1][1] = iyy;

        return matrix;
    }

    /**
     * Convolves the source image at the point (x,y,z) with the hessian of the gaussian function.
     *
     * @param   x  float point index indicating point of interest
     * @param   y  float point index indicating point of interest
     * @param   z  float point index indicating point of interest
     *
     * @return  hessian matrix for the point (x,y,z)
     */
    public double[][] hessian3D(float x, float y, float z) {
        float[] pt = new float[] { x, y, z };
        int[] extents = srcImage.getExtents();
        int length = extents[0] * extents[1] * extents[2];
        float[] imgBuf = new float[length];

        try {
            srcImage.exportData(0, length, imgBuf);
        } catch (IOException ioe) {
            MipavUtil.displayError("Error caught exporting image");
            setCompleted(false);

            return null;
        }

        float ixx, iyy, izz, ixy, iyz, ixz;

        ixx = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GxxData);
        iyy = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GyyData);
        izz = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GzzData);
        ixy = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GxyData);
        iyz = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GyzData);
        ixz = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GxzData);

        double[][] matrix = new double[3][3];

        matrix[0][0] = ixx;
        matrix[0][1] = ixy;
        matrix[0][2] = ixz;
        matrix[1][0] = ixy;
        matrix[1][1] = iyy;
        matrix[1][2] = iyz;
        matrix[2][0] = ixz;
        matrix[2][1] = iyz;
        matrix[2][2] = izz;

        return matrix;
    }

    /**
     * Convolves the source image at the point (x,y,z) with the hessian of the gaussian function.
     *
     * @param   imgBuf   DOCUMENT ME!
     * @param   extents  DOCUMENT ME!
     * @param   x        float point index indicating point of interest
     * @param   y        float point index indicating point of interest
     * @param   z        float point index indicating point of interest
     *
     * @return  hessian matrix for the point (x,y,z)
     */
    public double[][] hessian3D(float[] imgBuf, int[] extents, float x, float y, float z) {
        float[] pt = new float[] { x, y, z };

        float ixx, iyy, izz, ixy, iyz, ixz;

        ixx = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GxxData);
        iyy = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GyyData);
        izz = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GzzData);
        ixy = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GxyData);
        iyz = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GyzData);
        ixz = AlgorithmConvolver.convolve3DPt(pt, extents, imgBuf, kExtents, GxzData);

        double[][] matrix = new double[3][3];

        matrix[0][0] = ixx;
        matrix[0][1] = ixy;
        matrix[0][2] = ixz;
        matrix[1][0] = ixy;
        matrix[1][1] = iyy;
        matrix[1][2] = iyz;
        matrix[2][0] = ixz;
        matrix[2][1] = iyz;
        matrix[2][2] = izz;

        return matrix;
    }

    /**
     * Should not be used for this algorithm -- just a placeholder.
     */
    public void runAlgorithm() { }

    /**
     * Makes 2D derivative kernels to be used in the calculation of the hessian.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 2;
        derivOrder[1] = 0;

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
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
        derivOrder[1] = 1;
        GxyData = new float[xkDim * ykDim];

        GenerateGaussian Gxy = new GenerateGaussian(GxyData, kExtents, sigmas, derivOrder);

        Gxy.calc(false);
    }

    /**
     * Makes 3D derivative kernels to be used in the calculation of the hessian.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(5 * sigmas[1]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        kExtents[2] = zkDim;

        // d_xx
        derivOrder[0] = 2;
        derivOrder[1] = 0;
        derivOrder[2] = 0;
        GxxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);

        Gxx.calc(false);

        // d_yy
        derivOrder[0] = 0;
        derivOrder[1] = 2;
        derivOrder[2] = 0;
        GyyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);

        Gyy.calc(false);

        // d_zz
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 2;
        GzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gzz = new GenerateGaussian(GzzData, kExtents, sigmas, derivOrder);

        Gzz.calc(false);

        // d_xy
        derivOrder[0] = 1;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GxyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxy = new GenerateGaussian(GxyData, kExtents, sigmas, derivOrder);

        Gxy.calc(false);

        // d_yz
        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 1;
        GyzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyz = new GenerateGaussian(GyzData, kExtents, sigmas, derivOrder);

        Gyz.calc(false);

        // d_xz
        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GxzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxz = new GenerateGaussian(GxzData, kExtents, sigmas, derivOrder);

        Gxz.calc(false);
    }
}
