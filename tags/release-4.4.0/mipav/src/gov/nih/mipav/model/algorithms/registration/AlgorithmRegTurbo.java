package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * This code is derived from the Turbo Registration code of Phillipe Thevenaz, specifically code from the module
 * TurboReg_.java. The work is based on the paper: P. Thevenaz, U.E. Ruttimann, M. Unser A Pyramid Approach to Subpixel
 * Registration Based on Intensity IEEE Transactions on Image Processing vol. 7, no. 1, pp. 27-41, January, 1998. This
 * paper is available at: http://bigwww.epfl.ch/publications/thevenaz9801.html Dialog to get user input The user selects
 * the target image. The source image is transformed until it is registered to the target image. Algorithms are executed
 * in their own thread.
 *
 * <p>In automatic mode, the landmark points of the source image are automatically refined to minimize the mean-square
 * difference between the target and the warped source image. For automatic mode if apply to only VOI region is not
 * selected, then every pixel within the image is considered relevant and should participate in the mean-square
 * computation. If apply to only VOI region is selected, then only those data inside the VOI region should participate
 * in the mean-square computation.</p>
 *
 * <p>Two output images are returned. The first is the warped source image. The second is a boolean mask image formed
 * from the warped source VOI.</p>
 */

public class AlgorithmRegTurbo extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** A translation is described by a single point. It keeps area, angle, and orientation. */
    private static final int TRANSLATION = 1;

    /**
     * In 2D a pair of points determines the combination of a translation, of a rotation, and of an isotropic scaling.
     * Angles are conserved.
     */
    private static final int SCALED_ROTATION = 2;

    /**
     * In 2D three points generate an affine transformation, which is any combination of translation, rotation,
     * isotropic scaling, anisotropic scaling, shearing, and skewing. An affine transformation maps parallel lines onto
     * parallel lines.
     */
    private static final int AFFINE = 3;

    /**
     * In 2D four points describe a bilinear transformation, where a point of coordinates(x,y) is mapped on a point of
     * coordinates (u,v) such that u = p0 + p1*x + p2*y + p3*x*y and v = q0 + q1*x + q2*y + q3*x*y. Thus, both u and v
     * are both linear in x, and in y as well.
     */
    private static final int BILINEAR_DISTORTION = 4;
    // The numerical value of transformation is used in scaleBottomDownLandmarks private static final int
    // TRILINEAR_DISTORTION = 5;

    /** NEAREST_NEIGHBOR interpolation cannot be used in automatic mode. */
    private static final int NEAREST_NEIGHBOR = 1;

    /** DOCUMENT ME! */
    private static final int CUBIC_SPLINE = 2;

    /**
     * Create an output image that is distorted in such a way that the landmarks of the source are made to coincide with
     * those of the target.
     */
    private static final int MANUAL = 1;

    /**
     * Refine the landmarks of the source image in such a way that the least-squares error between the source image and
     * target is minimized.
     */
    private static final int AUTOMATIC = 2;

    /** DOCUMENT ME! */
    private static final int TARGET_IMAGE = 1;

    /** DOCUMENT ME! */
    private static final int SOURCE_IMAGE = 2;

    /** DOCUMENT ME! */
    private static final int TARGET_MASK = 3;

    /** DOCUMENT ME! */
    private static final int SOURCE_MASK = 4;

    /**
     * Minimal size of an image in the multiresolution pyramid. To use automatic mode all all dimensions of both the
     * source and target images must be >= 2*MIN_SIZE. If any dimension of either image is less than 2*MIN_SIZE,
     * processing will be NEAREST_NEIGHBOR interpolation in MANUAL mode.
     */
    public static final int MIN_SIZE = 8;

    /**
     * Maximal number of registration iterations per level, when speed is requested at the expense of accuracy. This
     * number must be corrected so that there are more iterations at the coarse levels of the pyramid than at the fine
     * levels.
     */
    private static final int FEW_ITERATIONS = 5;

    /**
     * Maximal number of registration iterations per level, when accuracy is requested at the expense of speed. This
     * number must be corrected so that there are more iterations at the coarse levels of the pyramid than at the fine
     * levels.
     */
    private static final int MANY_ITERATIONS = 10;

    /**
     * Minimal update distance of the landmarks, in pixel units, when accuracy is requested at the expense of speed.
     * This distance does not depend on the pyramid level.
     */
    private static final double PIXEL_HIGH_PRECISION = 0.001;

    /**
     * Minimal update distance of the landmarks, in pixel units, when speed is requested at the expense of accuracy.
     * This distance does not depend on the pyramid level.
     */
    private static final double PIXEL_LOW_PRECISION = 0.1;

    /**
     * Multiplicative factor that determines how many more iterations are allowed for a pyramid level one unit coarser.
     */
    private static final int ITERATION_PROGRESSION = 2;

    /**
     * Initial value of the Marquardt-Levenberg fudge factor.
     */
    private static final double FIRST_LAMBDA = 1.0;

    /**
     * Update parameter of the Marquardt-Levenberg fudge factor.
     */
    private static final double LAMBDA_MAGSTEP = 4.0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int automan; // Either MANUAL or AUTOMATIC

    /** DOCUMENT ME! */
    private double c0;

    /** DOCUMENT ME! */
    private double c0u;

    /** DOCUMENT ME! */
    private double c0uv;

    /** DOCUMENT ME! */
    private double c0v;

    /** DOCUMENT ME! */
    private double c1;

    /** DOCUMENT ME! */
    private double c1u;

    /** DOCUMENT ME! */
    private double c1uv;

    /** DOCUMENT ME! */
    private double c1v;

    /** DOCUMENT ME! */
    private double c2;

    /** DOCUMENT ME! */
    private double c2u;

    /** DOCUMENT ME! */
    private double c2uv;

    /** DOCUMENT ME! */
    private double c2v;

    /** DOCUMENT ME! */
    private double c3;

    /** DOCUMENT ME! */
    private double c3u;

    /** DOCUMENT ME! */
    private double c3uv;

    /** DOCUMENT ME! */
    private double c3v;

    /** DOCUMENT ME! */
    private final double[] dxWeight = new double[4];

    /** DOCUMENT ME! */
    private final double[] dyWeight = new double[4];

    /** DOCUMENT ME! */
    private boolean entireSource;

    /** DOCUMENT ME! */
    private boolean entireTarget;

    /** DOCUMENT ME! */
    private int i;

    /** DOCUMENT ME! */
    private float[] inImg;

    /** DOCUMENT ME! */
    private boolean[] inMsk;

    /** DOCUMENT ME! */
    private int inNx;

    /** DOCUMENT ME! */
    private int inNy;

    /** BILINEAR_DISTORTION, or TRILINEAR_DISTORTION. */
    private int interpolation; // Either NEAREST_NEIGHBOR or CUBIC_SPLINE

    /** DOCUMENT ME! */
    private int iterationCost;

    /** DOCUMENT ME! */
    private int iterationPower;

    /** DOCUMENT ME! */
    private int maxIterations;

    /** DOCUMENT ME! */
    private float[] outImg;

    /** DOCUMENT ME! */
    private boolean[] outMsk;

    /** DOCUMENT ME! */
    private int outNx;

    /** DOCUMENT ME! */
    private int outNy;

    /** DOCUMENT ME! */
    private int p;

    /** DOCUMENT ME! */
    private double pixelPrecision;

    /** DOCUMENT ME! */
    private int pyramidDepth;

    /** DOCUMENT ME! */
    private int pyramidType;

    /** DOCUMENT ME! */
    private int q;

    /** DOCUMENT ME! */
    private ModelImage[] resultImage;

    /** DOCUMENT ME! */
    private double s;

    /** DOCUMENT ME! */
    private float[] sourceArray;

    /** DOCUMENT ME! */
    private float[] sourceCoefficient;

    /** DOCUMENT ME! */
    private int sourceHeight;

    /** DOCUMENT ME! */
    private ModelImage sourceImage;

    /** DOCUMENT ME! */
    private final Stack sourceImagePyramid = new Stack();

    /** DOCUMENT ME! */
    private boolean[] sourceMask;

    /** DOCUMENT ME! */
    private final Stack sourceMaskPyramid = new Stack();

    /** DOCUMENT ME! */
    private double[][] sourcePoint;

    /** DOCUMENT ME! */
    private int sourceSliceSize;

    /** DOCUMENT ME! */
    private int sourceWidth;

    /** DOCUMENT ME! */
    private float[] sourceXGradient;

    /** DOCUMENT ME! */
    private float[] sourceYGradient;

    /** DOCUMENT ME! */
    private double t;

    /** DOCUMENT ME! */
    private float[] targetArray;

    /** DOCUMENT ME! */
    private float[] targetCoefficient;

    /** DOCUMENT ME! */
    private int targetHeight;

    /** DOCUMENT ME! */
    private ModelImage targetImage;

    /** DOCUMENT ME! */
    private final Stack targetImagePyramid = new Stack();

    /** DOCUMENT ME! */
    private double targetJacobian;

    /** DOCUMENT ME! */
    private boolean[] targetMask;

    /** DOCUMENT ME! */
    private final Stack targetMaskPyramid = new Stack();

    /** DOCUMENT ME! */
    private double[][] targetPoint;

    /** DOCUMENT ME! */
    private int targetSliceSize;

    /** DOCUMENT ME! */
    private int targetWidth;

    /** DOCUMENT ME! */
    private BitSet tMask = null;

    /** DOCUMENT ME! */
    private int transformation; // Either TRANSLATION, SCALED_ROTATION, AFFINE,

    /** DOCUMENT ME! */
    private int twiceInNx;

    /** DOCUMENT ME! */
    private int twiceInNy;

    /** DOCUMENT ME! */
    private double x;

    /** DOCUMENT ME! */
    private float[] xGradient;

    /** DOCUMENT ME! */
    private final int[] xIndex = new int[4];

    /** DOCUMENT ME! */
    private final double[] xWeight = new double[4];

    /** DOCUMENT ME! */
    private double y;

    /** DOCUMENT ME! */
    private float[] yGradient;

    /** DOCUMENT ME! */
    private final int[] yIndex = new int[4];

    /** DOCUMENT ME! */
    private final double[] yWeight = new double[4];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmRegTurbo - constructor.
     *
     * @param  resultImage     DOCUMENT ME!
     * @param  targetImage     DOCUMENT ME!
     * @param  entireTarget    DOCUMENT ME!
     * @param  sourceImage     DOCUMENT ME!
     * @param  entireSource    DOCUMENT ME!
     * @param  targetPoint     DOCUMENT ME!
     * @param  sourcePoint     DOCUMENT ME!
     * @param  transformation  DOCUMENT ME!
     * @param  interpolation   DOCUMENT ME!
     * @param  automan         DOCUMENT ME!
     */
    public AlgorithmRegTurbo(ModelImage[] resultImage, ModelImage targetImage, boolean entireTarget,
                             ModelImage sourceImage, boolean entireSource, double[][] targetPoint,
                             double[][] sourcePoint, int transformation, int interpolation, int automan) {
        this.resultImage = resultImage;
        this.targetImage = targetImage;
        this.entireTarget = entireTarget;
        this.sourceImage = sourceImage;
        this.entireSource = entireSource;
        this.targetPoint = targetPoint;
        this.sourcePoint = sourcePoint;
        this.transformation = transformation;
        this.interpolation = interpolation;
        this.automan = automan;

        if (entireTarget == false) {
            tMask = targetImage.generateVOIMask();
        }

        if (entireSource == false) {
            mask = sourceImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void finalize() {

        super.finalize();
    }

    /**
     * run - starts the program.
     */
    public void runAlgorithm() {
        fireProgressStateChanged(sourceImage.getImageName(), "Registering source image...");
        

        

        if (targetImage.getNDims() == 2) {
            turbo2D();
        } else {
            turbo3D();
        }
    }

    /**
     * translationTransform ------------------------------------------------------------------
     *
     * @param  matrix  DOCUMENT ME!
     * @param  outMsk  DOCUMENT ME!
     */
    private void affineTransform(final double[][] matrix, final boolean[] outMsk) {
        double yx;
        double yy;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);

            for (int u = 0; (u < outNx); u++) {
                x = yx + (matrix[0][1] * (double) u);
                y = yy + (matrix[1][1] * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (interpolation == NEAREST_NEIGHBOR) {
                        outImg[k] = inImg[xMsk];
                    } else {
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        outImg[k] = (float) interpolate();
                    }

                    outMsk[k++] = inMsk[xMsk];
                } else {
                    outImg[k] = 0.0F;
                    outMsk[k++] = false;
                }
            }
        }
    }

    /**
     * end coefficientToGradient1D ------------------------------------------------------------------
     *
     * @param  h  DOCUMENT ME!
     * @param  c  DOCUMENT ME!
     * @param  s  DOCUMENT ME!
     */
    private void antiSymmetricFirMirrorOffBounds1D(final double[] h, final double[] c, final double[] s) {

        if (2 <= c.length) {
            s[0] = h[1] * (c[1] - c[0]);

            for (int i = 1; (i < (s.length - 1)); i++) {
                s[i] = h[1] * (c[i + 1] - c[i - 1]);
            }

            s[s.length - 1] = h[1] * (c[c.length - 1] - c[c.length - 2]);
        } else {
            s[0] = 0.0;
        }
    }

    /**
     * end getPyramidDepth ------------------------------------------------------------------
     *
     * @param  basic     DOCUMENT ME!
     * @param  cardinal  DOCUMENT ME!
     * @param  width     DOCUMENT ME!
     * @param  height    DOCUMENT ME!
     * @param  degree    DOCUMENT ME!
     */
    private void basicToCardinal2D(final float[] basic, final float[] cardinal, final int width, final int height,
                                   final int degree) {
        double[] hLine = new double[width];
        double[] vLine = new double[height];
        double[] hData = new double[width];
        double[] vData = new double[height];
        double[] h = null;

        switch (degree) {

            case 3:
                h = new double[2];
                h[0] = 2.0 / 3.0;
                h[1] = 1.0 / 6.0;
                break;

            case 7:
                h = new double[4];
                h[0] = 151.0 / 315.0;
                h[1] = 397.0 / 1680.0;
                h[2] = 1.0 / 42.0;
                h[3] = 1.0 / 5040.0;
                break;
        }

        for (int y = 0; (y < height); y++) {
            extractRow(basic, y, hLine);
            symmetricFirMirrorOffBounds1D(h, hLine, hData);
            putRow(cardinal, y, hData);
        }

        for (int x = 0; (x < width); x++) {
            extractColumn(cardinal, width, x, vLine);
            symmetricFirMirrorOffBounds1D(h, vLine, vData);
            putColumn(cardinal, width, x, vData);
        }
    }

    /**
     * affineTransform ------------------------------------------------------------------
     *
     * @param  matrix  DOCUMENT ME!
     * @param  outMsk  DOCUMENT ME!
     */
    private void bilinearTransform(final double[][] matrix, final boolean[] outMsk) {
        double yx;
        double yy;
        double yxy;
        double yyy;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);
            yxy = matrix[0][3] * (double) v;
            yyy = matrix[1][3] * (double) v;

            for (int u = 0; (u < outNx); u++) {
                x = yx + (matrix[0][1] * (double) u) + (yxy * (double) u);
                y = yy + (matrix[1][1] * (double) u) + (yyy * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (interpolation == NEAREST_NEIGHBOR) {
                        outImg[k] = inImg[xMsk];
                    } else {
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        outImg[k] = (float) interpolate();
                    }

                    outMsk[k++] = inMsk[xMsk];
                } else {
                    outImg[k] = 0.0F;
                    outMsk[k++] = false;
                }
            }
        }
    }

    /**
     * end putRow ------------------------------------------------------------------
     *
     * @param  pyramidType  DOCUMENT ME!
     * @param  width        DOCUMENT ME!
     * @param  height       DOCUMENT ME!
     */
    private void buildCoefficientPyramid(int pyramidType, int width, int height) {
        int fullWidth;
        int fullHeight;
        float[] fullDual = new float[width * height];
        int halfWidth = width;
        int halfHeight = height;

        switch (pyramidType) {

            case TARGET_IMAGE:
                basicToCardinal2D(targetCoefficient, fullDual, width, height, 7);
                break;

            case SOURCE_IMAGE:
                basicToCardinal2D(sourceCoefficient, fullDual, width, height, 7);
                break;
        }

        for (int depth = 1; (depth < pyramidDepth); depth++) {
            fullWidth = halfWidth;
            fullHeight = halfHeight;
            halfWidth /= 2;
            halfHeight /= 2;

            float[] halfDual = getHalfDual2D(fullDual, fullWidth, fullHeight);
            float[] halfCoefficient = getBasicFromCardinal2D(halfDual, halfWidth, halfHeight, 7);

            switch (pyramidType) {

                case TARGET_IMAGE:
                    targetImagePyramid.push(halfCoefficient);
                    targetImagePyramid.push(new Integer(halfHeight));
                    targetImagePyramid.push(new Integer(halfWidth));
                    break;

                case SOURCE_IMAGE:
                    sourceImagePyramid.push(halfCoefficient);
                    sourceImagePyramid.push(new Integer(halfHeight));
                    sourceImagePyramid.push(new Integer(halfWidth));
                    break;
            }

            fullDual = halfDual;
        }
    }

    /**
     * end antiSymmetricFirMirrorOffBounds1D ------------------------------------------------------------------
     *
     * @param  pyramidType  DOCUMENT ME!
     * @param  imageArray   DOCUMENT ME!
     * @param  width        DOCUMENT ME!
     * @param  height       DOCUMENT ME!
     */
    private void buildImageAndGradientPyramid(int pyramidType, float[] imageArray, int width, int height) {
        int fullWidth;
        int fullHeight;
        float[] fullDual = new float[width * height];
        int halfWidth = width;
        int halfHeight = height;
        cardinalToDual2D(imageArray, fullDual, width, height, 3);

        for (int depth = 1; (depth < pyramidDepth); depth++) {
            fullWidth = halfWidth;
            fullHeight = halfHeight;
            halfWidth /= 2;
            halfHeight /= 2;

            float[] halfDual = getHalfDual2D(fullDual, fullWidth, fullHeight);
            float[] halfImage = getBasicFromCardinal2D(halfDual, halfWidth, halfHeight, 7);
            float[] halfXGradient = new float[halfWidth * halfHeight];
            float[] halfYGradient = new float[halfWidth * halfHeight];
            coefficientToXYGradient2D(halfImage, halfXGradient, halfYGradient, halfWidth, halfHeight);
            basicToCardinal2D(halfImage, halfImage, halfWidth, halfHeight, 3);

            switch (pyramidType) {

                case TARGET_IMAGE:
                    targetImagePyramid.push(halfYGradient);
                    targetImagePyramid.push(halfXGradient);
                    targetImagePyramid.push(halfImage);
                    targetImagePyramid.push(new Integer(halfHeight));
                    targetImagePyramid.push(new Integer(halfWidth));
                    break;

                case SOURCE_IMAGE:
                    sourceImagePyramid.push(halfYGradient);
                    sourceImagePyramid.push(halfXGradient);
                    sourceImagePyramid.push(halfImage);
                    sourceImagePyramid.push(new Integer(halfHeight));
                    sourceImagePyramid.push(new Integer(halfWidth));
                    break;
            }

            fullDual = halfDual;
        }
    }

    /**
     * end reduceDual1D ------------------------------------------------------------------
     *
     * @param  pyramidType  DOCUMENT ME!
     * @param  imageArray   DOCUMENT ME!
     * @param  width        DOCUMENT ME!
     * @param  height       DOCUMENT ME!
     */
    private void buildImagePyramid(int pyramidType, float[] imageArray, int width, int height) {
        int fullWidth;
        int fullHeight;
        float[] fullDual = new float[width * height];
        int halfWidth = width;
        int halfHeight = height;
        cardinalToDual2D(imageArray, fullDual, width, height, 3);

        for (int depth = 1; (depth < pyramidDepth); depth++) {
            fullWidth = halfWidth;
            fullHeight = halfHeight;
            halfWidth /= 2;
            halfHeight /= 2;

            float[] halfDual = getHalfDual2D(fullDual, fullWidth, fullHeight);
            float[] halfImage = new float[halfWidth * halfHeight];
            dualToCardinal2D(halfDual, halfImage, halfWidth, halfHeight, 3);

            switch (pyramidType) {

                case TARGET_IMAGE:
                    targetImagePyramid.push(halfImage);
                    targetImagePyramid.push(new Integer(halfHeight));
                    targetImagePyramid.push(new Integer(halfWidth));
                    break;

                case SOURCE_IMAGE:
                    sourceImagePyramid.push(halfImage);
                    sourceImagePyramid.push(new Integer(halfHeight));
                    sourceImagePyramid.push(new Integer(halfWidth));
                    break;
            }

            fullDual = halfDual;
        }
    }

    /**
     * end coefficientToSamples1D ------------------------------------------------------------------
     *
     * @param  pyramidType  DOCUMENT ME!
     * @param  mask         DOCUMENT ME!
     * @param  width        DOCUMENT ME!
     * @param  height       DOCUMENT ME!
     */
    private void buildPyramid(int pyramidType, boolean[] mask, int width, int height) {
        int fullWidth;
        int fullHeight;
        boolean[] fullMask = mask;
        int halfWidth = width;
        int halfHeight = height;

        for (int depth = 1; (depth < pyramidDepth); depth++) {
            fullWidth = halfWidth;
            fullHeight = halfHeight;
            halfWidth /= 2;
            halfHeight /= 2;

            boolean[] halfMask = getHalfMask2D(fullMask, fullWidth, fullHeight);

            switch (pyramidType) {

                case TARGET_MASK:
                    targetMaskPyramid.push(halfMask);
                    break;

                case SOURCE_MASK:
                    sourceMaskPyramid.push(halfMask);
                    break;
            }

            fullMask = halfMask;
        }
    }

    /**
     * end buildImagePyramid ------------------------------------------------------------------
     *
     * @param  cardinal  DOCUMENT ME!
     * @param  dual      DOCUMENT ME!
     * @param  width     DOCUMENT ME!
     * @param  height    DOCUMENT ME!
     * @param  degree    DOCUMENT ME!
     */
    private void cardinalToDual2D(final float[] cardinal, final float[] dual, final int width, final int height,
                                  final int degree) {
        basicToCardinal2D(getBasicFromCardinal2D(cardinal, width, height, degree), dual, width, height,
                          (2 * degree) + 1);
    }

    /**
     * end imageToXYGradient2D ------------------------------------------------------------------
     *
     * @param  c  DOCUMENT ME!
     */
    private void coefficientToGradient1D(final double[] c) {
        double[] h = { 0.0, 1.0 / 2.0 };
        double[] s = new double[c.length];
        antiSymmetricFirMirrorOffBounds1D(h, c, s);
        System.arraycopy(s, 0, c, 0, s.length);
    }

    /**
     * end coefficientToXYGradient2D ------------------------------------------------------------------
     *
     * @param  c  DOCUMENT ME!
     */
    private void coefficientToSamples1D(final double[] c) {
        double[] h = { 2.0 / 3.0, 1.0 / 6.0 };
        double[] s = new double[c.length];
        symmetricFirMirrorOffBounds1D(h, c, s);
        System.arraycopy(s, 0, c, 0, s.length);
    }

    /**
     * end buildImageAndGradientPyramid ------------------------------------------------------------------
     *
     * @param  basic      DOCUMENT ME!
     * @param  xGradient  DOCUMENT ME!
     * @param  yGradient  DOCUMENT ME!
     * @param  width      DOCUMENT ME!
     * @param  height     DOCUMENT ME!
     */
    private void coefficientToXYGradient2D(final float[] basic, final float[] xGradient, final float[] yGradient,
                                           final int width, final int height) {
        double[] hLine = new double[width];
        double[] hData = new double[width];
        double[] vLine = new double[height];

        for (int y = 0; (y < height); y++) {
            extractRow(basic, y, hLine);
            System.arraycopy(hLine, 0, hData, 0, width);
            coefficientToGradient1D(hLine);
            coefficientToSamples1D(hData);
            putRow(xGradient, y, hLine);
            putRow(yGradient, y, hData);
        }

        for (int x = 0; (x < width); x++) {
            extractColumn(xGradient, width, x, vLine);
            coefficientToSamples1D(vLine);
            putColumn(xGradient, width, x, vLine);
            extractColumn(yGradient, width, x, vLine);
            coefficientToGradient1D(vLine);
            putColumn(yGradient, width, x, vLine);
        }
    }

    /**
     * end interpolateDy ------------------------------------------------------------------
     */
    private void computeBilinearGradientConstants() {
        double u1 = targetPoint[0][0];
        double u2 = targetPoint[1][0];
        double u3 = targetPoint[2][0];
        double u4 = targetPoint[3][0];
        double v1 = targetPoint[0][1];
        double v2 = targetPoint[1][1];
        double v3 = targetPoint[2][1];
        double v4 = targetPoint[3][1];

        // double u12 = u1 - u2;
        // double u13 = u1 - u3;
        // double u14 = u1 - u4;
        // double u23 = u2 - u3;
        // double u24 = u2 - u4;
        // double u34 = u3 - u4;
        double v12 = v1 - v2;
        double v13 = v1 - v3;
        double v14 = v1 - v4;
        double v23 = v2 - v3;
        double v24 = v2 - v4;
        double v34 = v3 - v4;
        double uv12 = u1 * u2 * v12;
        double uv13 = u1 * u3 * v13;
        double uv14 = u1 * u4 * v14;
        double uv23 = u2 * u3 * v23;
        double uv24 = u2 * u4 * v24;
        double uv34 = u3 * u4 * v34;
        double det = (uv12 * v34) - (uv13 * v24) + (uv14 * v23) + (uv23 * v14) - (uv24 * v13) + (uv34 * v12);
        c0 = ((-uv34 * v2) + (uv24 * v3) - (uv23 * v4)) / det;
        c0u = ((u3 * v3 * v24) - (u2 * v2 * v34) - (u4 * v4 * v23)) / det;
        c0v = (uv23 - uv24 + uv34) / det;
        c0uv = ((u4 * v23) - (u3 * v24) + (u2 * v34)) / det;
        c1 = ((uv34 * v1) - (uv14 * v3) + (uv13 * v4)) / det;
        c1u = ((-u3 * v3 * v14) + (u1 * v1 * v34) + (u4 * v4 * v13)) / det;
        c1v = (-uv13 + uv14 - uv34) / det;
        c1uv = ((-u4 * v13) + (u3 * v14) - (u1 * v34)) / det;
        c2 = ((-uv24 * v1) + (uv14 * v2) - (uv12 * v4)) / det;
        c2u = ((u2 * v2 * v14) - (u1 * v1 * v24) - (u4 * v4 * v12)) / det;
        c2v = (uv12 - uv14 + uv24) / det;
        c2uv = ((u4 * v12) - (u2 * v14) + (u1 * v24)) / det;
        c3 = ((uv23 * v1) - (uv13 * v2) + (uv12 * v3)) / det;
        c3u = ((-u2 * v2 * v13) + (u1 * v1 * v23) + (u3 * v3 * v12)) / det;
        c3v = (-uv12 + uv13 - uv23) / det;
        c3uv = ((-u3 * v1) + (u2 * v13) + (u3 * v2) - (u1 * v23)) / det;
    }
 

    /**
     * Compute the final image.
     */
    private void doFinalTransform() {

        if (interpolation == NEAREST_NEIGHBOR) {
            inImg = sourceArray;
        } else {
            inImg = sourceCoefficient;
        }

        inMsk = sourceMask;
        inNx = sourceWidth;
        inNy = sourceHeight;
        twiceInNx = 2 * inNx;
        twiceInNy = 2 * inNy;

        int width = targetWidth;
        int height = targetHeight;
        outImg = new float[width * height];
        outMsk = new boolean[width * height];
        outNx = width;
        outNy = height;

        double[][] matrix = getTransformationMatrix(targetPoint, sourcePoint);

        switch (transformation) {

            case TRANSLATION:
                translationTransform(matrix, outMsk);
                break;

            case SCALED_ROTATION:
            case AFFINE:
                affineTransform(matrix, outMsk);
                break;

            case BILINEAR_DISTORTION:
                bilinearTransform(matrix, outMsk);
                break;
        }
    }

    /**
     * Refine the landmarks.
     */
    private void doRegistration() {
        int iterationNumber = 0;

        iterationPower = (int) Math.pow((double) ITERATION_PROGRESSION, (double) pyramidDepth);

        iterationCost = 1;
        scaleBottomDownLandmarks();

        while (!targetImagePyramid.isEmpty()) {
            fireProgressStateChanged(iterationNumber * 100 / (pyramidDepth + 1));
            iterationPower /= ITERATION_PROGRESSION;

            if (transformation == BILINEAR_DISTORTION) {
                outNx = ((Integer) targetImagePyramid.pop()).intValue();
                outNy = ((Integer) targetImagePyramid.pop()).intValue();
                outImg = (float[]) targetImagePyramid.pop();
                outMsk = (boolean[]) targetMaskPyramid.pop();
                inNx = ((Integer) sourceImagePyramid.pop()).intValue();
                inNy = ((Integer) sourceImagePyramid.pop()).intValue();
                inImg = (float[]) sourceImagePyramid.pop();
                inMsk = (boolean[]) sourceMaskPyramid.pop();
            } else {
                inNx = ((Integer) targetImagePyramid.pop()).intValue();
                inNy = ((Integer) targetImagePyramid.pop()).intValue();
                inImg = (float[]) targetImagePyramid.pop();
                inMsk = (boolean[]) targetMaskPyramid.pop();
                outNx = ((Integer) sourceImagePyramid.pop()).intValue();
                outNy = ((Integer) sourceImagePyramid.pop()).intValue();
                outImg = (float[]) sourceImagePyramid.pop();
                xGradient = (float[]) sourceImagePyramid.pop();
                yGradient = (float[]) sourceImagePyramid.pop();
                outMsk = (boolean[]) sourceMaskPyramid.pop();
            }

            twiceInNx = 2 * inNx;
            twiceInNy = 2 * inNy;

            switch (transformation) {

                case TRANSLATION:
                    targetJacobian = 1.0;
                    inverseMarquardtLevenbergOptimization();
                    break;

                case SCALED_ROTATION:
                    targetJacobian = ((targetPoint[0][0] - targetPoint[1][0]) *
                                          (targetPoint[0][0] - targetPoint[1][0])) +
                                     ((targetPoint[0][1] - targetPoint[1][1]) * (targetPoint[0][1] - targetPoint[1][1]));
                    inverseMarquardtLevenbergOptimization();
                    break;

                case AFFINE:
                    targetJacobian = ((targetPoint[1][0] - targetPoint[2][0]) * targetPoint[0][1]) +
                                     ((targetPoint[2][0] - targetPoint[0][0]) * targetPoint[1][1]) +
                                     ((targetPoint[0][0] - targetPoint[1][0]) * targetPoint[2][1]);
                    inverseMarquardtLevenbergOptimization();
                    break;

                case BILINEAR_DISTORTION:
                    MarquardtLevenbergOptimization();
                    break;
            }

            scaleUpLandmarks();
            iterationCost *= ITERATION_PROGRESSION;
            iterationNumber++;
        }

        fireProgressStateChanged(pyramidDepth * 100 / (pyramidDepth + 1));
        iterationPower /= ITERATION_PROGRESSION;

        if (transformation == BILINEAR_DISTORTION) {
            outNx = targetWidth;
            outNy = targetHeight;
            outImg = targetArray;
            outMsk = targetMask;
            inNx = sourceWidth;
            inNy = sourceHeight;
            inImg = sourceCoefficient;
            inMsk = sourceMask;
        } else {
            inNx = targetWidth;
            inNy = targetHeight;
            inImg = targetCoefficient;
            inMsk = targetMask;
            outNx = sourceWidth;
            outNy = sourceHeight;
            outImg = sourceArray;
            xGradient = sourceXGradient;
            yGradient = sourceYGradient;
            outMsk = sourceMask;
        }

        twiceInNx = 2 * inNx;
        twiceInNy = 2 * inNy;

        if (interpolation == CUBIC_SPLINE) {

            switch (transformation) {

                case TRANSLATION:
                case SCALED_ROTATION:
                case AFFINE:
                    inverseMarquardtLevenbergOptimization();
                    break;

                case BILINEAR_DISTORTION:
                    MarquardtLevenbergOptimization();
                    break;
            }
        } // if (interpolation == CUBIC_SPLINE)

        iterationPower = (int) Math.pow((double) ITERATION_PROGRESSION, (double) pyramidDepth);
        fireProgressStateChanged(0);

    }

    /**
     * end cardinalToDual2D ------------------------------------------------------------------
     *
     * @param  dual      DOCUMENT ME!
     * @param  cardinal  DOCUMENT ME!
     * @param  width     DOCUMENT ME!
     * @param  height    DOCUMENT ME!
     * @param  degree    DOCUMENT ME!
     */
    private void dualToCardinal2D(final float[] dual, final float[] cardinal, final int width, final int height,
                                  final int degree) {
        basicToCardinal2D(getBasicFromCardinal2D(dual, width, height, (2 * degree) + 1), cardinal, width, height,
                          degree);
    }

    /**
     * end getBasicFromCardinal2D ------------------------------------------------------------------
     *
     * @param  array   DOCUMENT ME!
     * @param  width   DOCUMENT ME!
     * @param  x       DOCUMENT ME!
     * @param  column  DOCUMENT ME!
     */
    private void extractColumn(final float[] array, final int width, int x, final double[] column) {

        for (int i = 0; (i < column.length); i++) {
            column[i] = (double) array[x];
            x += width;
        }
    }

    /**
     * end extractColumn ------------------------------------------------------------------
     *
     * @param  array  DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  row    DOCUMENT ME!
     */
    private void extractRow(final float[] array, int y, final double[] row) {
        y *= row.length;

        for (int i = 0; (i < row.length); i++) {
            row[i] = (double) array[y++];
        }
    }

    /**
     * getScaledRotationMeanSquares ------------------------------------------------------------------
     *
     * @param   sourcePoint  DOCUMENT ME!
     * @param   matrix       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getAffineMeanSquares(final double[][] sourcePoint, final double[][] matrix) {
        final double u1 = sourcePoint[0][0];
        final double u2 = sourcePoint[1][0];
        final double u3 = sourcePoint[2][0];
        final double v1 = sourcePoint[0][1];
        final double v2 = sourcePoint[1][1];
        final double v3 = sourcePoint[2][1];
        final double uv32 = (u3 * v2) - (u2 * v3);
        final double uv21 = (u2 * v1) - (u1 * v2);
        final double uv13 = (u1 * v3) - (u3 * v1);
        final double det = uv32 + uv21 + uv13;
        double yx;
        double yy;
        double difference;
        double meanSquares = 0.0;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u);
                y = yy + (matrix[1][1] * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        area++;
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        difference = (double) outImg[k] - interpolate();
                        meanSquares += difference * difference;
                    }
                }
            }
        }

        return (meanSquares / ((double) area * Math.abs(det / targetJacobian)));
    }

    /**
     * getAffineMeanSquares ------------------------------------------------------------------
     *
     * @param   sourcePoint  DOCUMENT ME!
     * @param   matrix       DOCUMENT ME!
     * @param   gradient     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getAffineMeanSquares(final double[][] sourcePoint, final double[][] matrix,
                                        final double[] gradient) {
        final double u1 = sourcePoint[0][0];
        final double u2 = sourcePoint[1][0];
        final double u3 = sourcePoint[2][0];
        final double v1 = sourcePoint[0][1];
        final double v2 = sourcePoint[1][1];
        final double v3 = sourcePoint[2][1];
        double uv32 = (u3 * v2) - (u2 * v3);
        double uv21 = (u2 * v1) - (u1 * v2);
        double uv13 = (u1 * v3) - (u3 * v1);
        final double det = uv32 + uv21 + uv13;
        final double u12 = (u1 - u2) / det;
        final double u23 = (u2 - u3) / det;
        final double u31 = (u3 - u1) / det;
        final double v12 = (v1 - v2) / det;
        final double v23 = (v2 - v3) / det;
        final double v31 = (v3 - v1) / det;
        double yx;
        double yy;
        double difference;
        double meanSquares = 0.0;
        double g0;
        double g1;
        double g2;
        double dx0;
        double dx1;
        double dx2;
        double dy0;
        double dy1;
        double dy2;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;
        uv32 /= det;
        uv21 /= det;
        uv13 /= det;

        for (int i = 0; (i < (2 * transformation)); i++) {
            gradient[i] = 0.0;
        }

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u);
                y = yy + (matrix[1][1] * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        area++;
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        difference = (double) outImg[k] - interpolate();
                        meanSquares += difference * difference;
                        g0 = (u23 * (double) v) - (v23 * (double) u) + uv32;
                        g1 = (u31 * (double) v) - (v31 * (double) u) + uv13;
                        g2 = (u12 * (double) v) - (v12 * (double) u) + uv21;
                        dx0 = xGradient[k] * g0;
                        dy0 = yGradient[k] * g0;
                        dx1 = xGradient[k] * g1;
                        dy1 = yGradient[k] * g1;
                        dx2 = xGradient[k] * g2;
                        dy2 = yGradient[k] * g2;
                        gradient[0] += difference * dx0;
                        gradient[1] += difference * dy0;
                        gradient[2] += difference * dx1;
                        gradient[3] += difference * dy1;
                        gradient[4] += difference * dx2;
                        gradient[5] += difference * dy2;
                    }
                }
            }
        }

        return (meanSquares / ((double) area * Math.abs(det / targetJacobian)));
    }

    /**
     * getAffineMeanSquares ------------------------------------------------------------------
     *
     * @param   sourcePoint  DOCUMENT ME!
     * @param   matrix       DOCUMENT ME!
     * @param   hessian      DOCUMENT ME!
     * @param   gradient     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getAffineMeanSquares(final double[][] sourcePoint, final double[][] matrix, final double[][] hessian,
                                        final double[] gradient) {
        final double u1 = sourcePoint[0][0];
        final double u2 = sourcePoint[1][0];
        final double u3 = sourcePoint[2][0];
        final double v1 = sourcePoint[0][1];
        final double v2 = sourcePoint[1][1];
        final double v3 = sourcePoint[2][1];
        double uv32 = (u3 * v2) - (u2 * v3);
        double uv21 = (u2 * v1) - (u1 * v2);
        double uv13 = (u1 * v3) - (u3 * v1);
        final double det = uv32 + uv21 + uv13;
        final double u12 = (u1 - u2) / det;
        final double u23 = (u2 - u3) / det;
        final double u31 = (u3 - u1) / det;
        final double v12 = (v1 - v2) / det;
        final double v23 = (v2 - v3) / det;
        final double v31 = (v3 - v1) / det;
        double yx;
        double yy;
        double difference;
        double meanSquares = 0.0;
        double g0;
        double g1;
        double g2;
        double dx0;
        double dx1;
        double dx2;
        double dy0;
        double dy1;
        double dy2;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;
        uv32 /= det;
        uv21 /= det;
        uv13 /= det;

        for (int i = 0; (i < (2 * transformation)); i++) {
            gradient[i] = 0.0;

            for (int j = 0; (j < (2 * transformation)); j++) {
                hessian[i][j] = 0.0;
            }
        }

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u);
                y = yy + (matrix[1][1] * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        area++;
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        difference = (double) outImg[k] - interpolate();
                        meanSquares += difference * difference;
                        g0 = (u23 * (double) v) - (v23 * (double) u) + uv32;
                        g1 = (u31 * (double) v) - (v31 * (double) u) + uv13;
                        g2 = (u12 * (double) v) - (v12 * (double) u) + uv21;
                        dx0 = xGradient[k] * g0;
                        dy0 = yGradient[k] * g0;
                        dx1 = xGradient[k] * g1;
                        dy1 = yGradient[k] * g1;
                        dx2 = xGradient[k] * g2;
                        dy2 = yGradient[k] * g2;
                        gradient[0] += difference * dx0;
                        gradient[1] += difference * dy0;
                        gradient[2] += difference * dx1;
                        gradient[3] += difference * dy1;
                        gradient[4] += difference * dx2;
                        gradient[5] += difference * dy2;
                        hessian[0][0] += dx0 * dx0;
                        hessian[0][1] += dx0 * dy0;
                        hessian[0][2] += dx0 * dx1;
                        hessian[0][3] += dx0 * dy1;
                        hessian[0][4] += dx0 * dx2;
                        hessian[0][5] += dx0 * dy2;
                        hessian[1][1] += dy0 * dy0;
                        hessian[1][2] += dy0 * dx1;
                        hessian[1][3] += dy0 * dy1;
                        hessian[1][4] += dy0 * dx2;
                        hessian[1][5] += dy0 * dy2;
                        hessian[2][2] += dx1 * dx1;
                        hessian[2][3] += dx1 * dy1;
                        hessian[2][4] += dx1 * dx2;
                        hessian[2][5] += dx1 * dy2;
                        hessian[3][3] += dy1 * dy1;
                        hessian[3][4] += dy1 * dx2;
                        hessian[3][5] += dy1 * dy2;
                        hessian[4][4] += dx2 * dx2;
                        hessian[4][5] += dx2 * dy2;
                        hessian[5][5] += dy2 * dy2;
                    }
                }
            }
        }

        for (int i = 1; (i < (2 * transformation)); i++) {

            for (int j = 0; (j < i); j++) {
                hessian[i][j] = hessian[j][i];
            }
        }

        return (meanSquares / ((double) area * Math.abs(det / targetJacobian)));
    }

    /**
     * bilinearTransform.
     *
     * @param   imageArray  DOCUMENT ME!
     * @param   width       DOCUMENT ME!
     * @param   height      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] getBasicFromCardinal2D(float[] imageArray, int width, int height) {
        float[] basic = new float[width * height];
        double[] hLine = new double[width];
        double[] vLine = new double[height];

        for (int y = 0; (y < height); y++) {
            extractRow(imageArray, y, hLine);
            samplesToInterpolationCoefficient1D(hLine, 3, 0.0);
            putRow(basic, y, hLine);
        }

        for (int x = 0; (x < width); x++) {
            extractColumn(basic, width, x, vLine);
            samplesToInterpolationCoefficient1D(vLine, 3, 0.0);
            putColumn(basic, width, x, vLine);
        }

        return (basic);
    }

    /**
     * end getBasicFromCardinal2D ------------------------------------------------------------------
     *
     * @param   cardinal  DOCUMENT ME!
     * @param   width     DOCUMENT ME!
     * @param   height    DOCUMENT ME!
     * @param   degree    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] getBasicFromCardinal2D(final float[] cardinal, final int width, final int height,
                                           final int degree) {
        float[] basic = new float[width * height];
        double[] hLine = new double[width];
        double[] vLine = new double[height];

        for (int y = 0; (y < height); y++) {
            extractRow(cardinal, y, hLine);
            samplesToInterpolationCoefficient1D(hLine, degree, 0.0);
            putRow(basic, y, hLine);
        }

        for (int x = 0; (x < width); x++) {
            extractColumn(basic, width, x, vLine);
            samplesToInterpolationCoefficient1D(vLine, degree, 0.0);
            putColumn(basic, width, x, vLine);
        }

        return (basic);
    }

    /**
     * end matrixMultiply ------------------------------------------------------------------
     *
     * @param   matrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getBilinearMeanSquares(final double[][] matrix) {
        double yx;
        double yy;
        double yxy;
        double yyy;
        double difference;
        double meanSquares = 0.0;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);
            yxy = matrix[0][3] * (double) v;
            yyy = matrix[1][3] * (double) v;

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u) + (yxy * (double) u);
                y = yy + (matrix[1][1] * (double) u) + (yyy * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        xIndexes();
                        yIndexes();
                        area++;
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        difference = interpolate() - (double) outImg[k];
                        meanSquares += difference * difference;
                    }
                }
            }
        }

        return (meanSquares / (double) area);
    }

    /**
     * getBilinearMeanSquares ------------------------------------------------------------------
     *
     * @param   matrix    DOCUMENT ME!
     * @param   hessian   DOCUMENT ME!
     * @param   gradient  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getBilinearMeanSquares(final double[][] matrix, final double[][] hessian, final double[] gradient) {
        double yx;
        double yy;
        double yxy;
        double yyy;
        double uv;
        double xGradient;
        double yGradient;
        double difference;
        double meanSquares = 0.0;
        double g0;
        double g1;
        double g2;
        double g3;
        double dx0;
        double dx1;
        double dx2;
        double dx3;
        double dy0;
        double dy1;
        double dy2;
        double dy3;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;
        computeBilinearGradientConstants();

        for (int i = 0; (i < (2 * transformation)); i++) {
            gradient[i] = 0.0;

            for (int j = 0; (j < (2 * transformation)); j++) {
                hessian[i][j] = 0.0;
            }
        }

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);
            yxy = matrix[0][3] * (double) v;
            yyy = matrix[1][3] * (double) v;

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u) + (yxy * (double) u);
                y = yy + (matrix[1][1] * (double) u) + (yyy * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        area++;
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xDxWeights();
                        yDyWeights();
                        difference = interpolate() - (double) outImg[k];
                        meanSquares += difference * difference;
                        xGradient = interpolateDx();
                        yGradient = interpolateDy();
                        uv = (double) u * (double) v;
                        g0 = (c0uv * uv) + (c0u * (double) u) + (c0v * (double) v) + c0;
                        g1 = (c1uv * uv) + (c1u * (double) u) + (c1v * (double) v) + c1;
                        g2 = (c2uv * uv) + (c2u * (double) u) + (c2v * (double) v) + c2;
                        g3 = (c3uv * uv) + (c3u * (double) u) + (c3v * (double) v) + c3;
                        dx0 = xGradient * g0;
                        dy0 = yGradient * g0;
                        dx1 = xGradient * g1;
                        dy1 = yGradient * g1;
                        dx2 = xGradient * g2;
                        dy2 = yGradient * g2;
                        dx3 = xGradient * g3;
                        dy3 = yGradient * g3;
                        gradient[0] += difference * dx0;
                        gradient[1] += difference * dy0;
                        gradient[2] += difference * dx1;
                        gradient[3] += difference * dy1;
                        gradient[4] += difference * dx2;
                        gradient[5] += difference * dy2;
                        gradient[6] += difference * dx3;
                        gradient[7] += difference * dy3;
                        hessian[0][0] += dx0 * dx0;
                        hessian[0][1] += dx0 * dy0;
                        hessian[0][2] += dx0 * dx1;
                        hessian[0][3] += dx0 * dy1;
                        hessian[0][4] += dx0 * dx2;
                        hessian[0][5] += dx0 * dy2;
                        hessian[0][6] += dx0 * dx3;
                        hessian[0][7] += dx0 * dy3;
                        hessian[1][1] += dy0 * dy0;
                        hessian[1][2] += dy0 * dx1;
                        hessian[1][3] += dy0 * dy1;
                        hessian[1][4] += dy0 * dx2;
                        hessian[1][5] += dy0 * dy2;
                        hessian[1][6] += dy0 * dx3;
                        hessian[1][7] += dy0 * dy3;
                        hessian[2][2] += dx1 * dx1;
                        hessian[2][3] += dx1 * dy1;
                        hessian[2][4] += dx1 * dx2;
                        hessian[2][5] += dx1 * dy2;
                        hessian[2][6] += dx1 * dx3;
                        hessian[2][7] += dx1 * dy3;
                        hessian[3][3] += dy1 * dy1;
                        hessian[3][4] += dy1 * dx2;
                        hessian[3][5] += dy1 * dy2;
                        hessian[3][6] += dy1 * dx3;
                        hessian[3][7] += dy1 * dy3;
                        hessian[4][4] += dx2 * dx2;
                        hessian[4][5] += dx2 * dy2;
                        hessian[4][6] += dx2 * dx3;
                        hessian[4][7] += dx2 * dy3;
                        hessian[5][5] += dy2 * dy2;
                        hessian[5][6] += dy2 * dx3;
                        hessian[5][7] += dy2 * dy3;
                        hessian[6][6] += dx3 * dx3;
                        hessian[6][7] += dx3 * dy3;
                        hessian[7][7] += dy3 * dy3;
                    }
                }
            }
        }

        for (int i = 1; (i < (2 * transformation)); i++) {

            for (int j = 0; (j < i); j++) {
                hessian[i][j] = hessian[j][i];
            }
        }

        return (meanSquares / (double) area);
    }

    /**
     * end symmetricFirMirrorOffBounds1D ------------------------------------------------------------------
     *
     * @param   fullDual    DOCUMENT ME!
     * @param   fullWidth   DOCUMENT ME!
     * @param   fullHeight  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] getHalfDual2D(final float[] fullDual, final int fullWidth, final int fullHeight) {
        int halfWidth = fullWidth / 2;
        int halfHeight = fullHeight / 2;
        double[] hLine = new double[fullWidth];
        double[] hData = new double[halfWidth];
        double[] vLine = new double[fullHeight];
        double[] vData = new double[halfHeight];
        float[] demiDual = new float[halfWidth * fullHeight];
        float[] halfDual = new float[halfWidth * halfHeight];

        for (int y = 0; (y < fullHeight); y++) {
            extractRow(fullDual, y, hLine);
            reduceDual1D(hLine, hData);
            putRow(demiDual, y, hData);
        }

        for (int x = 0; (x < halfWidth); x++) {
            extractColumn(demiDual, halfWidth, x, vLine);
            reduceDual1D(vLine, vData);
            putColumn(halfDual, halfWidth, x, vData);
        }

        return (halfDual);
    }

    /**
     * end buildPyramid ------------------------------------------------------------------
     *
     * @param   fullMask    DOCUMENT ME!
     * @param   fullWidth   DOCUMENT ME!
     * @param   fullHeight  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean[] getHalfMask2D(final boolean[] fullMask, final int fullWidth, final int fullHeight) {
        int halfWidth = fullWidth / 2;
        int halfHeight = fullHeight / 2;
        boolean oddWidth = ((2 * halfWidth) != fullWidth);
        boolean[] halfMask = new boolean[halfWidth * halfHeight];
        int k = 0;

        for (int y = 0; (y < halfHeight); y++) {

            for (int x = 0; (x < halfWidth); x++) {
                halfMask[k++] = false;
            }
        }

        k = 0;

        int n = 0;

        for (int y = 0; (y < (halfHeight - 1)); y++) {

            for (int x = 0; (x < (halfWidth - 1)); x++) {
                halfMask[k] |= fullMask[n++];
                halfMask[k] |= fullMask[n];
                halfMask[++k] |= fullMask[n++];
            }

            halfMask[k] |= fullMask[n++];
            halfMask[k++] |= fullMask[n++];

            if (oddWidth) {
                n++;
            }

            for (int x = 0; (x < (halfWidth - 1)); x++) {
                halfMask[k - halfWidth] |= fullMask[n];
                halfMask[k] |= fullMask[n++];
                halfMask[k - halfWidth] |= fullMask[n];
                halfMask[k - halfWidth + 1] |= fullMask[n];
                halfMask[k] |= fullMask[n];
                halfMask[++k] |= fullMask[n++];
            }

            halfMask[k - halfWidth] |= fullMask[n];
            halfMask[k] |= fullMask[n++];
            halfMask[k - halfWidth] |= fullMask[n];
            halfMask[k++] |= fullMask[n++];

            if (oddWidth) {
                n++;
            }

            k -= halfWidth;
        }

        for (int x = 0; (x < (halfWidth - 1)); x++) {
            halfMask[k] |= fullMask[n++];
            halfMask[k] |= fullMask[n];
            halfMask[++k] |= fullMask[n++];
        }

        halfMask[k] |= fullMask[n++];
        halfMask[k++] |= fullMask[n++];

        if (oddWidth) {
            n++;
        }

        k -= halfWidth;

        for (int x = 0; (x < (halfWidth - 1)); x++) {
            halfMask[k] |= fullMask[n++];
            halfMask[k] |= fullMask[n];
            halfMask[++k] |= fullMask[n++];
        }

        halfMask[k] |= fullMask[n++];
        halfMask[k] |= fullMask[n];

        return (halfMask);
    }

    /* end getHalfMask2D */

    /**
     * end samplesToInterpolationCoefficient1D ------------------------------------------------------------------
     *
     * @param   c          DOCUMENT ME!
     * @param   z          DOCUMENT ME!
     * @param   tolerance  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getInitialAntiCausalCoefficientMirrorOffBounds(final double[] c, final double z,
        final double tolerance) {
        return (z * c[c.length - 1] / (z - 1.0));
    }

    /**
     * end getInitialAntiCausalCoefficientMirrorOffBounds
     * ------------------------------------------------------------------
     *
     * @param   c          DOCUMENT ME!
     * @param   z          DOCUMENT ME!
     * @param   tolerance  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getInitialCausalCoefficientMirrorOffBounds(final double[] c, final double z,
                                                              final double tolerance) {
        double z1 = z, zn = Math.pow(z, c.length);
        double sum = (1.0 + z) * (c[0] + (zn * c[c.length - 1]));
        int horizon = c.length;

        if (0.0 < tolerance) {
            horizon = 2 + (int) (Math.log(tolerance) / Math.log(Math.abs(z)));
            horizon = (horizon < c.length) ? (horizon) : (c.length);
        }

        zn = zn * zn;

        for (int n = 1; (n < (horizon - 1)); n++) {
            z1 = z1 * z;
            zn = zn / z;
            sum = sum + ((z1 + zn) * c[n]);
        }

        return (sum / (1.0 - Math.pow(z, 2 * c.length)));
    }

    /**
     * end buildCoefficientPyramid ------------------------------------------------------------------
     */
    private void getPyramidDepth() {
        int tw = targetWidth;
        int th = targetHeight;
        int sw = sourceWidth;
        int sh = sourceHeight;
        pyramidDepth = 0;

        while (((2 * MIN_SIZE) <= tw) && ((2 * MIN_SIZE) <= sw) && ((2 * MIN_SIZE) <= th) && ((2 * MIN_SIZE) <= sh)) {
            tw /= 2;
            sw /= 2;
            th /= 2;
            sh /= 2;
            pyramidDepth++;
        }
    }

    /**
     * end getTranslationMeanSquares ------------------------------------------------------------------
     *
     * @param   sourcePoint  DOCUMENT ME!
     * @param   matrix       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getScaledRotationMeanSquares(final double[][] sourcePoint, final double[][] matrix) {
        final double u1 = sourcePoint[0][0];
        final double u2 = sourcePoint[1][0];
        final double v1 = sourcePoint[0][1];
        final double v2 = sourcePoint[1][1];
        final double u12 = u1 - u2;
        final double v12 = v1 - v2;
        final double uv2 = (u12 * u12) + (v12 * v12);
        double yx;
        double yy;
        double difference;
        double meanSquares = 0.0;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u);
                y = yy + (matrix[1][1] * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        area++;
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        difference = (double) outImg[k] - interpolate();
                        meanSquares += difference * difference;
                    }
                }
            }
        }

        return (meanSquares / ((double) area * uv2 / targetJacobian));
    }

    /**
     * getScaledRotationMeanSquares ------------------------------------------------------------------
     *
     * @param   sourcePoint  DOCUMENT ME!
     * @param   matrix       DOCUMENT ME!
     * @param   gradient     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getScaledRotationMeanSquares(final double[][] sourcePoint, final double[][] matrix,
                                                final double[] gradient) {
        final double u1 = sourcePoint[0][0];
        final double u2 = sourcePoint[1][0];
        final double v1 = sourcePoint[0][1];
        final double v2 = sourcePoint[1][1];
        final double u12 = u1 - u2;
        final double v12 = v1 - v2;
        final double uv2 = (u12 * u12) + (v12 * v12);
        final double c = 0.5 * ((u2 * v1) - (u1 * v2)) / uv2;
        final double c1 = u12 / uv2;
        final double c2 = v12 / uv2;
        final double c3 = (uv2 - (u12 * v12)) / uv2;
        final double c4 = (uv2 + (u12 * v12)) / uv2;
        final double c5 = c + (u1 * c1) + (u2 * c2);
        final double c6 = c * ((u12 * u12) - (v12 * v12)) / uv2;
        final double c7 = c1 * c4;
        final double c8 = c1 - c2 - (c1 * c2 * v12);
        final double c9 = c1 + c2 - (c1 * c2 * u12);
        final double c0 = c2 * c3;
        final double dgxx0 = (c1 * u2) + (c2 * v2);
        final double dgyx0 = 2.0 * c;
        final double dgxx1 = c5 + c6;
        final double dgyy1 = c5 - c6;
        double yx;
        double yy;
        double difference;
        double meanSquares = 0.0;
        double gxx0;
        double gxx1;
        double gxy0;
        double gxy1;
        double gyx0;
        double gyx1;
        double gyy0;
        double gyy1;
        double dx0;
        double dx1;
        double dy0;
        double dy1;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int i = 0; (i < (2 * transformation)); i++) {
            gradient[i] = 0.0;
        }

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u);
                y = yy + (matrix[1][1] * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        area++;
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        difference = (double) outImg[k] - interpolate();
                        meanSquares += difference * difference;
                        gxx0 = ((double) u * c1) + ((double) v * c2) - dgxx0;
                        gyx0 = ((double) v * c1) - ((double) u * c2) + dgyx0;
                        gxy0 = -gyx0;
                        gyy0 = gxx0;
                        gxx1 = ((double) v * c8) - ((double) u * c7) + dgxx1;
                        gyx1 = -c3 * gyx0;
                        gxy1 = c4 * gyx0;
                        gyy1 = dgyy1 - ((double) u * c9) - ((double) v * c0);
                        dx0 = (xGradient[k] * gxx0) + (yGradient[k] * gyx0);
                        dy0 = (xGradient[k] * gxy0) + (yGradient[k] * gyy0);
                        dx1 = (xGradient[k] * gxx1) + (yGradient[k] * gyx1);
                        dy1 = (xGradient[k] * gxy1) + (yGradient[k] * gyy1);
                        gradient[0] += difference * dx0;
                        gradient[1] += difference * dy0;
                        gradient[2] += difference * dx1;
                        gradient[3] += difference * dy1;
                    }
                }
            }
        }

        return (meanSquares / ((double) area * uv2 / targetJacobian));
    }

    /**
     * getScaledRotationMeanSquares ------------------------------------------------------------------
     *
     * @param   sourcePoint  DOCUMENT ME!
     * @param   matrix       DOCUMENT ME!
     * @param   hessian      DOCUMENT ME!
     * @param   gradient     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getScaledRotationMeanSquares(final double[][] sourcePoint, final double[][] matrix,
                                                final double[][] hessian, final double[] gradient) {
        final double u1 = sourcePoint[0][0];
        final double u2 = sourcePoint[1][0];
        final double v1 = sourcePoint[0][1];
        final double v2 = sourcePoint[1][1];
        final double u12 = u1 - u2;
        final double v12 = v1 - v2;
        final double uv2 = (u12 * u12) + (v12 * v12);
        final double c = 0.5 * ((u2 * v1) - (u1 * v2)) / uv2;
        final double c1 = u12 / uv2;
        final double c2 = v12 / uv2;
        final double c3 = (uv2 - (u12 * v12)) / uv2;
        final double c4 = (uv2 + (u12 * v12)) / uv2;
        final double c5 = c + (u1 * c1) + (u2 * c2);
        final double c6 = c * ((u12 * u12) - (v12 * v12)) / uv2;
        final double c7 = c1 * c4;
        final double c8 = c1 - c2 - (c1 * c2 * v12);
        final double c9 = c1 + c2 - (c1 * c2 * u12);
        final double c0 = c2 * c3;
        final double dgxx0 = (c1 * u2) + (c2 * v2);
        final double dgyx0 = 2.0 * c;
        final double dgxx1 = c5 + c6;
        final double dgyy1 = c5 - c6;
        double yx;
        double yy;
        double difference;
        double meanSquares = 0.0;
        double gxx0;
        double gxx1;
        double gxy0;
        double gxy1;
        double gyx0;
        double gyx1;
        double gyy0;
        double gyy1;
        double dx0;
        double dx1;
        double dy0;
        double dy1;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int i = 0; (i < (2 * transformation)); i++) {
            gradient[i] = 0.0;

            for (int j = 0; (j < (2 * transformation)); j++) {
                hessian[i][j] = 0.0;
            }
        }

        for (int v = 0; (v < outNy); v++) {
            yx = matrix[0][0] + (matrix[0][2] * (double) v);
            yy = matrix[1][0] + (matrix[1][2] * (double) v);

            for (int u = 0; (u < outNx); u++, k++) {
                x = yx + (matrix[0][1] * (double) u);
                y = yy + (matrix[1][1] * (double) u);
                xMsk = (int) Math.round(x);
                yMsk = (int) Math.round(y);

                if ((0 <= xMsk) && (xMsk < inNx) && (0 <= yMsk) && (yMsk < inNy)) {
                    xMsk += yMsk * inNx;

                    if (outMsk[k] && inMsk[xMsk]) {
                        area++;
                        xIndexes();
                        yIndexes();
                        x -= Math.floor(x);
                        y -= Math.floor(y);
                        xWeights();
                        yWeights();
                        difference = (double) outImg[k] - interpolate();
                        meanSquares += difference * difference;
                        gxx0 = ((double) u * c1) + ((double) v * c2) - dgxx0;
                        gyx0 = ((double) v * c1) - ((double) u * c2) + dgyx0;
                        gxy0 = -gyx0;
                        gyy0 = gxx0;
                        gxx1 = ((double) v * c8) - ((double) u * c7) + dgxx1;
                        gyx1 = -c3 * gyx0;
                        gxy1 = c4 * gyx0;
                        gyy1 = dgyy1 - ((double) u * c9) - ((double) v * c0);
                        dx0 = (xGradient[k] * gxx0) + (yGradient[k] * gyx0);
                        dy0 = (xGradient[k] * gxy0) + (yGradient[k] * gyy0);
                        dx1 = (xGradient[k] * gxx1) + (yGradient[k] * gyx1);
                        dy1 = (xGradient[k] * gxy1) + (yGradient[k] * gyy1);
                        gradient[0] += difference * dx0;
                        gradient[1] += difference * dy0;
                        gradient[2] += difference * dx1;
                        gradient[3] += difference * dy1;
                        hessian[0][0] += dx0 * dx0;
                        hessian[0][1] += dx0 * dy0;
                        hessian[0][2] += dx0 * dx1;
                        hessian[0][3] += dx0 * dy1;
                        hessian[1][1] += dy0 * dy0;
                        hessian[1][2] += dy0 * dx1;
                        hessian[1][3] += dy0 * dy1;
                        hessian[2][2] += dx1 * dx1;
                        hessian[2][3] += dx1 * dy1;
                        hessian[3][3] += dy1 * dy1;
                    }
                }
            }
        }

        for (int i = 1; (i < (2 * transformation)); i++) {

            for (int j = 0; (j < i); j++) {
                hessian[i][j] = hessian[j][i];
            }
        }

        return (meanSquares / ((double) area * uv2 / targetJacobian));
    }

    /**
     * end MarquardtLevenbergOptimization ------------------------------------------------------------------
     *
     * @param   targetPoint  DOCUMENT ME!
     * @param   sourcePoint  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[][] getTransformationMatrix(final double[][] targetPoint, final double[][] sourcePoint) {
        double[][] matrix = null;
        double[][] a = null;
        double[] v = null;

        switch (transformation) {

            case TRANSLATION:
                matrix = new double[2][1];
                matrix[0][0] = sourcePoint[0][0] - targetPoint[0][0];
                matrix[1][0] = sourcePoint[0][1] - targetPoint[0][1];
                break;

            case SCALED_ROTATION:
                matrix = new double[2][3];
                a = new double[3][3];
                v = new double[3];
                a[0][0] = 1.0;
                a[0][1] = targetPoint[0][0];
                a[0][2] = targetPoint[0][1];
                a[1][0] = 1.0;
                a[1][1] = targetPoint[1][0];
                a[1][2] = targetPoint[1][1];
                a[2][0] = 1.0;
                a[2][1] = targetPoint[0][1] - targetPoint[1][1] + targetPoint[1][0];
                a[2][2] = targetPoint[1][0] + targetPoint[1][1] - targetPoint[0][0];
                invertGauss(a);
                v[0] = sourcePoint[0][0];
                v[1] = sourcePoint[1][0];
                v[2] = sourcePoint[0][1] - sourcePoint[1][1] + sourcePoint[1][0];
                for (int i = 0; (i < 3); i++) {
                    matrix[0][i] = 0.0;

                    for (int j = 0; (j < 3); j++) {
                        matrix[0][i] += a[i][j] * v[j];
                    }
                }

                v[0] = sourcePoint[0][1];
                v[1] = sourcePoint[1][1];
                v[2] = sourcePoint[1][0] + sourcePoint[1][1] - sourcePoint[0][0];
                for (int i = 0; (i < 3); i++) {
                    matrix[1][i] = 0.0;

                    for (int j = 0; (j < 3); j++) {
                        matrix[1][i] += a[i][j] * v[j];
                    }
                }

                break;

            case AFFINE:
                matrix = new double[2][3];
                a = new double[3][3];
                v = new double[3];
                a[0][0] = 1.0;
                a[0][1] = targetPoint[0][0];
                a[0][2] = targetPoint[0][1];
                a[1][0] = 1.0;
                a[1][1] = targetPoint[1][0];
                a[1][2] = targetPoint[1][1];
                a[2][0] = 1.0;
                a[2][1] = targetPoint[2][0];
                a[2][2] = targetPoint[2][1];
                invertGauss(a);
                v[0] = sourcePoint[0][0];
                v[1] = sourcePoint[1][0];
                v[2] = sourcePoint[2][0];
                for (int i = 0; (i < 3); i++) {
                    matrix[0][i] = 0.0;

                    for (int j = 0; (j < 3); j++) {
                        matrix[0][i] += a[i][j] * v[j];
                    }
                }

                v[0] = sourcePoint[0][1];
                v[1] = sourcePoint[1][1];
                v[2] = sourcePoint[2][1];
                for (int i = 0; (i < 3); i++) {
                    matrix[1][i] = 0.0;

                    for (int j = 0; (j < 3); j++) {
                        matrix[1][i] += a[i][j] * v[j];
                    }
                }

                break;

            case BILINEAR_DISTORTION:
                matrix = new double[2][4];
                a = new double[4][4];
                v = new double[4];
                a[0][0] = 1.0;
                a[0][1] = targetPoint[0][0];
                a[0][2] = targetPoint[0][1];
                a[0][3] = targetPoint[0][0] * targetPoint[0][1];
                a[1][0] = 1.0;
                a[1][1] = targetPoint[1][0];
                a[1][2] = targetPoint[1][1];
                a[1][3] = targetPoint[1][0] * targetPoint[1][1];
                a[2][0] = 1.0;
                a[2][1] = targetPoint[2][0];
                a[2][2] = targetPoint[2][1];
                a[2][3] = targetPoint[2][0] * targetPoint[2][1];
                a[3][0] = 1.0;
                a[3][1] = targetPoint[3][0];
                a[3][2] = targetPoint[3][1];
                a[3][3] = targetPoint[3][0] * targetPoint[3][1];
                invertGauss(a);
                v[0] = sourcePoint[0][0];
                v[1] = sourcePoint[1][0];
                v[2] = sourcePoint[2][0];
                v[3] = sourcePoint[3][0];
                for (int i = 0; (i < 4); i++) {
                    matrix[0][i] = 0.0;

                    for (int j = 0; (j < 4); j++) {
                        matrix[0][i] += a[i][j] * v[j];
                    }
                }

                v[0] = sourcePoint[0][1];
                v[1] = sourcePoint[1][1];
                v[2] = sourcePoint[2][1];
                v[3] = sourcePoint[3][1];
                for (int i = 0; (i < 4); i++) {
                    matrix[1][i] = 0.0;

                    for (int j = 0; (j < 4); j++) {
                        matrix[1][i] += a[i][j] * v[j];
                    }
                }

                break;
        }

        return (matrix);
    }

    /**
     * end getTransformationMatrix ------------------------------------------------------------------
     *
     * @param   matrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getTranslationMeanSquares(final double[][] matrix) {
        final double dx = matrix[0][0];
        final double dy = matrix[1][0];

        double difference;
        double meanSquares = 0.0;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;
        x = dx - Math.floor(dx);
        y = dy - Math.floor(dy);
        xWeights();
        yWeights();

        for (int v = 0; (v < outNy); v++) {
            y = (double) v + dy;
            yMsk = (int) Math.round(y);

            if ((0 <= yMsk) && (yMsk < inNy)) {
                yMsk *= inNx;
                yIndexes();

                for (int u = 0; (u < outNx); u++, k++) {
                    x = (double) u + dx;
                    xMsk = (int) Math.round(x);

                    if ((0 <= xMsk) && (xMsk < inNx)) {
                        xMsk += yMsk;

                        if (outMsk[k] && inMsk[xMsk]) {
                            xIndexes();
                            area++;
                            difference = (double) outImg[k] - interpolate();
                            meanSquares += difference * difference;
                        }
                    }
                }
            } else {
                k += outNx;
            }
        }

        return (meanSquares / (double) area);
    }

    /**
     * end getTranslationMeanSquares ------------------------------------------------------------------
     *
     * @param   matrix    DOCUMENT ME!
     * @param   gradient  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getTranslationMeanSquares(final double[][] matrix, final double[] gradient) {
        final double dx = matrix[0][0];
        final double dy = matrix[1][0];
        double difference;
        double meanSquares = 0.0;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int i = 0; (i < (2 * transformation)); i++) {
            gradient[i] = 0.0;
        }

        x = dx - Math.floor(dx);
        y = dy - Math.floor(dy);
        xWeights();
        yWeights();

        for (int v = 0; (v < outNy); v++) {
            y = (double) v + dy;
            yMsk = (int) Math.round(y);

            if ((0 <= yMsk) && (yMsk < inNy)) {
                yMsk *= inNx;
                yIndexes();

                for (int u = 0; (u < outNx); u++, k++) {
                    x = (double) u + dx;
                    xMsk = (int) Math.round(x);

                    if ((0 <= xMsk) && (xMsk < inNx)) {
                        xMsk += yMsk;

                        if (outMsk[k] && inMsk[xMsk]) {
                            area++;
                            xIndexes();
                            difference = (double) outImg[k] - interpolate();
                            meanSquares += difference * difference;
                            gradient[0] += difference * xGradient[k];
                            gradient[1] += difference * yGradient[k];
                        }
                    }
                }
            } else {
                k += outNx;
            }
        }

        return (meanSquares / (double) area);
    }

    /**
     * end getTranslationMeanSquares ------------------------------------------------------------------
     *
     * @param   matrix    DOCUMENT ME!
     * @param   hessian   DOCUMENT ME!
     * @param   gradient  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double getTranslationMeanSquares(final double[][] matrix, final double[][] hessian,
                                             final double[] gradient) {
        final double dx = matrix[0][0];
        final double dy = matrix[1][0];
        double difference;
        double meanSquares = 0.0;
        long area = 0L;
        int xMsk;
        int yMsk;
        int k = 0;

        for (int i = 0; (i < (2 * transformation)); i++) {
            gradient[i] = 0.0;

            for (int j = 0; (j < (2 * transformation)); j++) {
                hessian[i][j] = 0.0;
            }
        }

        x = dx - Math.floor(dx);
        y = dy - Math.floor(dy);
        xWeights();
        yWeights();

        for (int v = 0; (v < outNy); v++) {
            y = (double) v + dy;
            yMsk = (int) Math.round(y);

            if ((0 <= yMsk) && (yMsk < inNy)) {
                yMsk *= inNx;
                yIndexes();

                for (int u = 0; (u < outNx); u++, k++) {
                    x = (double) u + dx;
                    xMsk = (int) Math.round(x);

                    if ((0 <= xMsk) && (xMsk < inNx)) {

                        xMsk += yMsk;

                        if (outMsk[k] && inMsk[xMsk]) {
                            area++;
                            xIndexes();
                            difference = (double) outImg[k] - interpolate();
                            meanSquares += difference * difference;
                            gradient[0] += difference * xGradient[k];
                            gradient[1] += difference * yGradient[k];
                            hessian[0][0] += xGradient[k] * xGradient[k];
                            hessian[0][1] += xGradient[k] * yGradient[k];
                            hessian[1][1] += yGradient[k] * yGradient[k];
                        }
                    }
                }
            } else {
                k += outNx;
            }
        }

        for (int i = 1; (i < (2 * transformation)); i++) {

            for (int j = 0; (j < i); j++) {
                hessian[i][j] = hessian[j][i];
            }
        }

        return (meanSquares / (double) area);
    }

    /**
     * end dualToCardinal2D ------------------------------------------------------------------
     *
     * @param  imageArray  DOCUMENT ME!
     * @param  width       DOCUMENT ME!
     * @param  height      DOCUMENT ME!
     */
    private void imageToXYGradient2D(float[] imageArray, int width, int height) {
        double[] hLine = new double[width];
        double[] vLine = new double[height];
        xGradient = new float[width * height];
        yGradient = new float[width * height];
        sourceXGradient = new float[width * height];
        sourceYGradient = new float[width * height];

        for (int y = 0; (y < height); y++) {
            extractRow(imageArray, y, hLine);
            samplesToInterpolationCoefficient1D(hLine, 3, 0.0);
            coefficientToGradient1D(hLine);
            putRow(xGradient, y, hLine);
        }

        for (int x = 0; (x < width); x++) {
            extractColumn(imageArray, width, x, vLine);
            samplesToInterpolationCoefficient1D(vLine, 3, 0.0);
            coefficientToGradient1D(vLine);
            putColumn(yGradient, width, x, vLine);
        }
    }

    /**
     * yWeights ------------------------------------------------------------------
     *
     * @return  DOCUMENT ME!
     */
    private double interpolate() {
        t = 0.0;

        for (int j = 0; (j < 4); j++) {
            s = 0.0;
            p = yIndex[j];

            for (int i = 0; (i < 4); i++) {
                s += xWeight[i] * (double) inImg[p + xIndex[i]];
            }

            t += yWeight[j] * s;
        }

        return (t);
    }

    /**
     * end interpolate ------------------------------------------------------------------
     *
     * @return  DOCUMENT ME!
     */
    private double interpolateDx() {
        t = 0.0;

        for (int j = 0; (j < 4); j++) {
            s = 0.0;
            p = yIndex[j];

            for (int i = 0; (i < 4); i++) {
                s += dxWeight[i] * (double) inImg[p + xIndex[i]];
            }

            t += yWeight[j] * s;
        }

        return (t);
    }

    /**
     * end interpolateDx ------------------------------------------------------------------
     *
     * @return  DOCUMENT ME!
     */
    private double interpolateDy() {
        t = 0.0;

        for (int j = 0; (j < 4); j++) {
            s = 0.0;
            p = yIndex[j];

            for (int i = 0; (i < 4); i++) {
                s += xWeight[i] * (double) inImg[p + xIndex[i]];
            }

            t += dyWeight[j] * s;
        }

        return (t);
    }

    /**
     * end scaleUpLandmarks ------------------------------------------------------------------
     */
    private void inverseMarquardtLevenbergOptimization() {
        double[][] attempt = new double[transformation][2];
        double[][] matrix = getTransformationMatrix(sourcePoint, targetPoint);
        double[][] hessian = new double[2 * transformation][2 * transformation];
        double[][] pseudoHessian = new double[2 * transformation][2 * transformation];
        double[] gradient = new double[2 * transformation];
        double[] update = new double[2 * transformation];
        double bestMeanSquares = 0.0;
        double meanSquares = 0.0;
        double lambda = FIRST_LAMBDA;
        double displacement;
        int iteration = 0;

        switch (transformation) {

            case TRANSLATION:
                bestMeanSquares = getTranslationMeanSquares(matrix, hessian, gradient);
                break;

            case SCALED_ROTATION:
                bestMeanSquares = getScaledRotationMeanSquares(sourcePoint, matrix, hessian, gradient);
                break;

            case AFFINE:
                bestMeanSquares = getAffineMeanSquares(sourcePoint, matrix, hessian, gradient);
                break;
        }

        iteration++;

        do {

            for (int k = 0; (k < (2 * transformation)); k++) {
                pseudoHessian[k][k] = (1.0 + lambda) * hessian[k][k];
            }

            invertGauss(pseudoHessian);
            update = matrixMultiply(pseudoHessian, gradient);
            displacement = 0.0;

            for (int k = 0; (k < transformation); k++) {
                attempt[k][0] = sourcePoint[k][0] - update[2 * k];
                attempt[k][1] = sourcePoint[k][1] - update[(2 * k) + 1];
                displacement += Math.sqrt((update[2 * k] * update[2 * k]) +
                                          (update[(2 * k) + 1] * update[(2 * k) + 1]));
            }

            displacement /= (double) transformation;
            matrix = getTransformationMatrix(attempt, targetPoint);

            switch (transformation) {

                case TRANSLATION:
                    if (interpolation == NEAREST_NEIGHBOR) {
                        meanSquares = getTranslationMeanSquares(matrix, gradient);
                    } else {
                        meanSquares = getTranslationMeanSquares(matrix, hessian, gradient);
                    }

                    break;

                case SCALED_ROTATION:
                    if (interpolation == NEAREST_NEIGHBOR) {
                        meanSquares = getScaledRotationMeanSquares(attempt, matrix, gradient);
                    } else {
                        meanSquares = getScaledRotationMeanSquares(attempt, matrix, hessian, gradient);
                    }

                    break;

                case AFFINE:
                    if (interpolation == NEAREST_NEIGHBOR) {
                        meanSquares = getAffineMeanSquares(attempt, matrix, gradient);
                    } else {
                        meanSquares = getAffineMeanSquares(attempt, matrix, hessian, gradient);
                    }

                    break;
            }

            iteration++;

            if (meanSquares < bestMeanSquares) {
                bestMeanSquares = meanSquares;

                for (int k = 0; (k < transformation); k++) {
                    sourcePoint[k][0] = attempt[k][0];
                    sourcePoint[k][1] = attempt[k][1];
                }

                lambda /= LAMBDA_MAGSTEP;
            } else {
                lambda *= LAMBDA_MAGSTEP;
            }

        } while ((iteration < ((maxIterations * iterationPower) - 1)) && (pixelPrecision <= displacement));

        invertGauss(hessian);
        update = matrixMultiply(hessian, gradient);

        for (int k = 0; (k < transformation); k++) {
            attempt[k][0] = sourcePoint[k][0] - update[2 * k];
            attempt[k][1] = sourcePoint[k][1] - update[(2 * k) + 1];
        }

        matrix = getTransformationMatrix(attempt, targetPoint);

        switch (transformation) {

            case TRANSLATION:
                meanSquares = getTranslationMeanSquares(matrix);
                break;

            case SCALED_ROTATION:
                meanSquares = getScaledRotationMeanSquares(attempt, matrix);
                break;

            case AFFINE:
                meanSquares = getAffineMeanSquares(attempt, matrix);
                break;
        }

        iteration++;

        if (meanSquares < bestMeanSquares) {

            for (int k = 0; (k < transformation); k++) {
                sourcePoint[k][0] = attempt[k][0];
                sourcePoint[k][1] = attempt[k][1];
            }
        }
    }

    /**
     * getAffineMeanSquares ------------------------------------------------------------------
     *
     * @param  matrix  DOCUMENT ME!
     */
    private void invertGauss(final double[][] matrix) {
        int n = matrix.length;
        double[][] inverse = new double[n][n];

        for (int i = 0; (i < n); i++) {
            double max = matrix[i][0];
            double absMax = Math.abs(max);

            for (int j = 0; (j < n); j++) {
                inverse[i][j] = 0.0;

                if (absMax < Math.abs(matrix[i][j])) {
                    max = matrix[i][j];
                    absMax = Math.abs(max);
                }
            }

            inverse[i][i] = 1.0 / max;

            for (int j = 0; (j < n); j++) {
                matrix[i][j] /= max;
            }
        }

        for (int j = 0; (j < n); j++) {
            double max = matrix[j][j];
            double absMax = Math.abs(max);
            int k = j;

            for (int i = j + 1; (i < n); i++) {

                if (absMax < Math.abs(matrix[i][j])) {
                    max = matrix[i][j];
                    absMax = Math.abs(max);
                    k = i;
                }
            }

            if (k != j) {
                double[] partialLine = new double[n - j];
                double[] fullLine = new double[n];
                System.arraycopy(matrix[j], j, partialLine, 0, n - j);
                System.arraycopy(matrix[k], j, matrix[j], j, n - j);
                System.arraycopy(partialLine, 0, matrix[k], j, n - j);
                System.arraycopy(inverse[j], 0, fullLine, 0, n);
                System.arraycopy(inverse[k], 0, inverse[j], 0, n);
                System.arraycopy(fullLine, 0, inverse[k], 0, n);
            }

            for (k = 0; (k <= j); k++) {
                inverse[j][k] /= max;
            }

            for (k = j + 1; (k < n); k++) {
                matrix[j][k] /= max;
                inverse[j][k] /= max;
            }

            for (int i = j + 1; (i < n); i++) {

                for (k = 0; (k <= j); k++) {
                    inverse[i][k] -= matrix[i][j] * inverse[j][k];
                }

                for (k = j + 1; (k < n); k++) {
                    matrix[i][k] -= matrix[i][j] * matrix[j][k];
                    inverse[i][k] -= matrix[i][j] * inverse[j][k];
                }
            }
        }

        for (int j = n - 1; (1 <= j); j--) {

            for (int i = j - 1; (0 <= i); i--) {

                for (int k = 0; (k <= j); k++) {
                    inverse[i][k] -= matrix[i][j] * inverse[j][k];
                }

                for (int k = j + 1; (k < n); k++) {
                    matrix[i][k] -= matrix[i][j] * matrix[j][k];
                    inverse[i][k] -= matrix[i][j] * inverse[j][k];
                }
            }
        }

        for (int i = 0; (i < n); i++) {
            System.arraycopy(inverse[i], 0, matrix[i], 0, n);
        }
    }

    /**
     * end inverseMarquardtLevenbergOptimization ------------------------------------------------------------------
     */
    private void MarquardtLevenbergOptimization() {
        double[][] attempt = new double[transformation][2];
        double[][] matrix = getTransformationMatrix(targetPoint, sourcePoint);
        double[][] hessian = new double[2 * transformation][2 * transformation];
        double[][] pseudoHessian = new double[2 * transformation][2 * transformation];
        double[] gradient = new double[2 * transformation];
        double[] update = new double[2 * transformation];
        double bestMeanSquares = 0.0;
        double meanSquares = 0.0;
        double lambda = FIRST_LAMBDA;
        double displacement;
        int iteration = 0;
        bestMeanSquares = getBilinearMeanSquares(matrix, hessian, gradient);
        iteration++;

        do {

            for (int k = 0; (k < (2 * transformation)); k++) {
                pseudoHessian[k][k] = (1.0 + lambda) * hessian[k][k];
            }

            invertGauss(pseudoHessian);
            update = matrixMultiply(pseudoHessian, gradient);
            displacement = 0.0;

            for (int k = 0; (k < transformation); k++) {
                attempt[k][0] = sourcePoint[k][0] - update[2 * k];
                attempt[k][1] = sourcePoint[k][1] - update[(2 * k) + 1];
                displacement += Math.sqrt((update[2 * k] * update[2 * k]) +
                                          (update[(2 * k) + 1] * update[(2 * k) + 1]));
            }

            displacement /= (double) transformation;
            matrix = getTransformationMatrix(targetPoint, attempt);
            meanSquares = getBilinearMeanSquares(matrix, hessian, gradient);
            iteration++;

            if (meanSquares < bestMeanSquares) {
                bestMeanSquares = meanSquares;

                for (int k = 0; (k < transformation); k++) {
                    sourcePoint[k][0] = attempt[k][0];
                    sourcePoint[k][1] = attempt[k][1];
                }

                lambda /= LAMBDA_MAGSTEP;
            } else {
                lambda *= LAMBDA_MAGSTEP;
            }
        } while ((iteration < ((maxIterations * iterationPower) - 1)) && (pixelPrecision <= displacement));

        invertGauss(hessian);
        update = matrixMultiply(hessian, gradient);

        for (int k = 0; (k < transformation); k++) {
            attempt[k][0] = sourcePoint[k][0] - update[2 * k];
            attempt[k][1] = sourcePoint[k][1] - update[(2 * k) + 1];
        }

        matrix = getTransformationMatrix(targetPoint, attempt);
        meanSquares = getBilinearMeanSquares(matrix);
        iteration++;

        if (meanSquares < bestMeanSquares) {

            for (int k = 0; (k < transformation); k++) {
                sourcePoint[k][0] = attempt[k][0];
                sourcePoint[k][1] = attempt[k][1];
            }
        }

    }

    /**
     * end invertGauss ------------------------------------------------------------------
     *
     * @param   matrix  DOCUMENT ME!
     * @param   vector  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] matrixMultiply(final double[][] matrix, final double[] vector) {
        double[] result = new double[matrix.length];

        for (int i = 0; (i < matrix.length); i++) {
            result[i] = 0.0;

            for (int j = 0; (j < vector.length); j++) {
                result[i] += matrix[i][j] * vector[j];
            }
        }

        return (result);
    }

    /**
     * end getInitialCausalCoefficientMirrorOffBounds ------------------------------------------------------------------
     *
     * @param  array   DOCUMENT ME!
     * @param  width   DOCUMENT ME!
     * @param  x       DOCUMENT ME!
     * @param  column  DOCUMENT ME!
     */
    private void putColumn(final float[] array, final int width, int x, final double[] column) {

        for (int i = 0; (i < column.length); i++) {
            array[x] = (float) column[i];
            x += width;
        }
    }

    /**
     * end putColumn ------------------------------------------------------------------
     *
     * @param  array  DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  row    DOCUMENT ME!
     */
    private void putRow(final float[] array, int y, final double[] row) {
        y *= row.length;

        for (int i = 0; (i < row.length); i++) {
            array[y++] = (float) row[i];
        }
    }

    /**
     * end getHalfDual2D ------------------------------------------------------------------
     *
     * @param  c  DOCUMENT ME!
     * @param  s  DOCUMENT ME!
     */
    private void reduceDual1D(final double[] c, final double[] s) {
        double[] h = { 6.0 / 16.0, 4.0 / 16.0, 1.0 / 16.0 };

        if (2 <= s.length) {
            s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1])) + (h[2] * (c[1] + c[2]));

            for (int i = 2, j = 1; (j < (s.length - 1)); i += 2, j++) {
                s[j] = (h[0] * c[i]) + (h[1] * (c[i - 1] + c[i + 1])) + (h[2] * (c[i - 2] + c[i + 2]));
            }

            if (c.length == (2 * s.length)) {
                s[s.length - 1] = (h[0] * c[c.length - 2]) + (h[1] * (c[c.length - 3] + c[c.length - 1])) +
                                  (h[2] * (c[c.length - 4] + c[c.length - 1]));
            } else {
                s[s.length - 1] = (h[0] * c[c.length - 3]) + (h[1] * (c[c.length - 4] + c[c.length - 2])) +
                                  (h[2] * (c[c.length - 5] + c[c.length - 1]));
            }
        } else {

            switch (c.length) {

                case 3:
                    s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1])) + (h[2] * (c[1] + c[2]));
                    break;

                case 2:
                    s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1])) + (2.0 * h[2] * c[1]);
                    break;
            }
        }
    }

    /**
     * end extractRow ------------------------------------------------------------------
     *
     * @param  c          DOCUMENT ME!
     * @param  degree     DOCUMENT ME!
     * @param  tolerance  DOCUMENT ME!
     */
    private void samplesToInterpolationCoefficient1D(final double[] c, final int degree, final double tolerance) {
        double[] z = new double[0];
        double lambda = 1.0;

        switch (degree) {

            case 3:
                z = new double[1];
                z[0] = Math.sqrt(3.0) - 2.0;
                break;

            case 7:
                z = new double[3];
                z[0] = -0.5352804307964381655424037816816460718339231523426924148812;
                z[1] = -0.122554615192326690515272264359357343605486549427295558490763;
                z[2] = -0.0091486948096082769285930216516478534156925639545994482648003;
                break;
        }

        if (c.length == 1) {
            return;
        }

        for (int k = 0; (k < z.length); k++) {
            lambda = lambda * (1.0 - z[k]) * (1.0 - (1.0 / z[k]));
        }

        for (int n = 0; (n < c.length); n++) {
            c[n] = c[n] * lambda;
        }

        for (int k = 0; (k < z.length); k++) {
            c[0] = getInitialCausalCoefficientMirrorOffBounds(c, z[k], tolerance);

            for (int n = 1; (n < c.length); n++) {
                c[n] = c[n] + (z[k] * c[n - 1]);
            }

            c[c.length - 1] = getInitialAntiCausalCoefficientMirrorOffBounds(c, z[k], tolerance);

            for (int n = c.length - 2; (0 <= n); n--) {
                c[n] = z[k] * (c[n + 1] - c[n]);
            }
        }
    }

    /**
     * end doRegistration ------------------------------------------------------------------
     */
    private void scaleBottomDownLandmarks() {

        for (int depth = 1; (depth < pyramidDepth); depth++) {

            for (int n = 0; (n < transformation); n++) {
                targetPoint[n][0] *= 0.5;
                targetPoint[n][1] *= 0.5;
                sourcePoint[n][0] *= 0.5;
                sourcePoint[n][1] *= 0.5;
            }
        }
    }

    /**
     * end scaleBottomDownLandmarks ------------------------------------------------------------------
     */
    private void scaleUpLandmarks() {

        for (int n = 0; (n < transformation); n++) {
            targetPoint[n][0] *= 2.0;
            targetPoint[n][1] *= 2.0;
            sourcePoint[n][0] *= 2.0;
            sourcePoint[n][1] *= 2.0;
        }
    }

    /**
     * end basicToCardinal2D ------------------------------------------------------------------
     *
     * @param  h  DOCUMENT ME!
     * @param  c  DOCUMENT ME!
     * @param  s  DOCUMENT ME!
     */
    private void symmetricFirMirrorOffBounds1D(final double[] h, final double[] c, final double[] s) {

        switch (h.length) {

            case 2:
                if (2 <= c.length) {
                    s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1]));

                    for (int i = 1; (i < (s.length - 1)); i++) {
                        s[i] = (h[0] * c[i]) + (h[1] * (c[i - 1] + c[i + 1]));
                    }

                    s[s.length - 1] = (h[0] * c[c.length - 1]) + (h[1] * (c[c.length - 2] + c[c.length - 1]));
                } else {
                    s[0] = (h[0] + (2.0 * h[1])) * c[0];
                }

                break;

            case 4:
                if (6 <= c.length) {
                    s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1])) + (h[2] * (c[1] + c[2])) + (h[3] * (c[2] + c[3]));
                    s[1] = (h[0] * c[1]) + (h[1] * (c[0] + c[2])) + (h[2] * (c[0] + c[3])) + (h[3] * (c[1] + c[4]));
                    s[2] = (h[0] * c[2]) + (h[1] * (c[1] + c[3])) + (h[2] * (c[0] + c[4])) + (h[3] * (c[0] + c[5]));

                    for (int i = 3; (i < (s.length - 3)); i++) {
                        s[i] = (h[0] * c[i]) + (h[1] * (c[i - 1] + c[i + 1])) + (h[2] * (c[i - 2] + c[i + 2])) +
                               (h[3] * (c[i - 3] + c[i + 3]));
                    }

                    s[s.length - 3] = (h[0] * c[c.length - 3]) + (h[1] * (c[c.length - 4] + c[c.length - 2])) +
                                      (h[2] * (c[c.length - 5] + c[c.length - 1])) +
                                      (h[3] * (c[c.length - 6] + c[c.length - 1]));
                    s[s.length - 2] = (h[0] * c[c.length - 2]) + (h[1] * (c[c.length - 3] + c[c.length - 1])) +
                                      (h[2] * (c[c.length - 4] + c[c.length - 1])) +
                                      (h[3] * (c[c.length - 5] + c[c.length - 2]));
                    s[s.length - 1] = (h[0] * c[c.length - 1]) + (h[1] * (c[c.length - 2] + c[c.length - 1])) +
                                      (h[2] * (c[c.length - 3] + c[c.length - 2])) +
                                      (h[3] * (c[c.length - 4] + c[c.length - 3]));
                } else {

                    switch (c.length) {

                        case 5:
                            s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1])) + (h[2] * (c[1] + c[2])) +
                                   (h[3] * (c[2] + c[3]));
                            s[1] = (h[0] * c[1]) + (h[1] * (c[0] + c[2])) + (h[2] * (c[0] + c[3])) +
                                   (h[3] * (c[1] + c[4]));
                            s[2] = (h[0] * c[2]) + (h[1] * (c[1] + c[3])) + ((h[2] + h[3]) * (c[0] + c[4]));
                            s[3] = (h[0] * c[3]) + (h[1] * (c[2] + c[4])) + (h[2] * (c[1] + c[4])) +
                                   (h[3] * (c[0] + c[3]));
                            s[4] = (h[0] * c[4]) + (h[1] * (c[3] + c[4])) + (h[2] * (c[2] + c[3])) +
                                   (h[3] * (c[1] + c[2]));
                            break;

                        case 4:
                            s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1])) + (h[2] * (c[1] + c[2])) +
                                   (h[3] * (c[2] + c[3]));
                            s[1] = (h[0] * c[1]) + (h[1] * (c[0] + c[2])) + (h[2] * (c[0] + c[3])) +
                                   (h[3] * (c[1] + c[3]));
                            s[2] = (h[0] * c[2]) + (h[1] * (c[1] + c[3])) + (h[2] * (c[0] + c[3])) +
                                   (h[3] * (c[0] + c[2]));
                            s[3] = (h[0] * c[3]) + (h[1] * (c[2] + c[3])) + (h[2] * (c[1] + c[2])) +
                                   (h[3] * (c[0] + c[1]));
                            break;

                        case 3:
                            s[0] = (h[0] * c[0]) + (h[1] * (c[0] + c[1])) + (h[2] * (c[1] + c[2])) +
                                   (2.0 * h[3] * c[2]);
                            s[1] = (h[0] * c[1]) + ((h[1] + h[2]) * (c[0] + c[2])) + (2.0 * h[3] * c[1]);
                            s[2] = (h[0] * c[2]) + (h[1] * (c[1] + c[2])) + (h[2] * (c[0] + c[1])) +
                                   (2.0 * h[3] * c[0]);
                            break;

                        case 2:
                            s[0] = ((h[0] + h[1] + h[3]) * c[0]) + ((h[1] + (2.0 * h[2]) + h[3]) * c[1]);
                            s[1] = ((h[0] + h[1] + h[3]) * c[1]) + ((h[1] + (2.0 * h[2]) + h[3]) * c[0]);
                            break;

                        case 1:
                            s[0] = (h[0] + (2.0 * (h[1] + h[2] + h[3]))) * c[0];
                            break;
                    }
                }

                break;
        }
    }

    /**
     * end doFinalTransform ------------------------------------------------------------------
     *
     * @param  matrix  DOCUMENT ME!
     * @param  outMsk  DOCUMENT ME!
     */
    private void translationTransform(final double[][] matrix, final boolean[] outMsk) {
        double dx = matrix[0][0];
        double dy = matrix[1][0];
        int xMsk;
        int yMsk;
        x = dx - Math.floor(dx);
        y = dy - Math.floor(dy);

        if (interpolation != NEAREST_NEIGHBOR) {
            xWeights();
            yWeights();
        }

        int k = 0;

        for (int v = 0; (v < outNy); v++) {
            y = (double) v + dy;
            yMsk = (int) Math.round(y);

            if ((0 <= yMsk) && (yMsk < inNy)) {
                yMsk *= inNx;

                if (interpolation != NEAREST_NEIGHBOR) {
                    yIndexes();
                }

                for (int u = 0; (u < outNx); u++, k++) {
                    x = (double) u + dx;
                    xMsk = (int) Math.round(x);

                    if ((0 <= xMsk) && (xMsk < inNx)) {
                        xMsk += yMsk;

                        if (interpolation == NEAREST_NEIGHBOR) {
                            outImg[k] = inImg[xMsk];
                        } else {
                            xIndexes();
                            outImg[k] = (float) interpolate();
                        }

                        outMsk[k] = inMsk[xMsk];
                    } else {
                        outImg[k] = 0.0F;
                        outMsk[k] = false;
                    }
                }
            } else {

                for (int u = 0; (u < outNx); u++, k++) {
                    outImg[k] = 0.0F;
                    outMsk[k] = false;
                }
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void turbo2D() {
        sourceWidth = sourceImage.getExtents()[0];
        sourceHeight = sourceImage.getExtents()[1];
        sourceSliceSize = sourceWidth * sourceHeight;
        sourceArray = new float[sourceSliceSize];

        try {
            sourceImage.exportData(0, sourceSliceSize, sourceArray); // locks and releases and lock
        } catch (IOException error) {
            displayError("AlgorithmRegTurbo: Source image is locked");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        } catch (OutOfMemoryError e) {
            sourceArray = null;
            System.gc();
            displayError("AlgorithmRegTurbo: Out of memory");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        targetWidth = targetImage.getExtents()[0];
        targetHeight = targetImage.getExtents()[1];
        targetSliceSize = targetWidth * targetHeight;
        targetArray = new float[targetSliceSize];

        try {
            targetImage.exportData(0, targetSliceSize, targetArray); // locks and releases and lock
        } catch (IOException error) {
            displayError("AlgorithmRegTurbo: Target image is locked");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        } catch (OutOfMemoryError e) {
            targetArray = null;
            System.gc();
            displayError("AlgorithmRegTurbo: Out of memory");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        getPyramidDepth();

        if (interpolation == CUBIC_SPLINE) {

            // Compute spline coefficients, image pyramids, and mask pyramids
            // targetImage
            fireProgressStateChanged("Calculating target image coefficients");
            targetCoefficient = getBasicFromCardinal2D(targetArray, targetWidth, targetHeight);
            pyramidType = TARGET_IMAGE;

            switch (transformation) {

                case TRANSLATION:
                case SCALED_ROTATION:
                case AFFINE:
                    fireProgressStateChanged("Building target image coefficient pyramid");
                    buildCoefficientPyramid(pyramidType, targetWidth, targetHeight);
                    break;

                case BILINEAR_DISTORTION:
                    fireProgressStateChanged("Building target image pyramid");
                    buildImagePyramid(pyramidType, targetArray, targetWidth, targetHeight);
                    break;
            }

            // sourceImage
            fireProgressStateChanged("Calculating source image coefficients");
            sourceCoefficient = getBasicFromCardinal2D(sourceArray, sourceWidth, sourceHeight);
            pyramidType = SOURCE_IMAGE;

            switch (transformation) {

                case TRANSLATION:
                case SCALED_ROTATION:
                case AFFINE:
                    fireProgressStateChanged("Building target image and gradient pyramid");
                    imageToXYGradient2D(sourceArray, sourceWidth, sourceHeight);
                    buildImageAndGradientPyramid(pyramidType, sourceArray, sourceWidth, sourceHeight);
                    break;

                case BILINEAR_DISTORTION:
                    fireProgressStateChanged("Building target image pyramid");
                    buildCoefficientPyramid(pyramidType, sourceWidth, sourceHeight);
                    break;
            }

            // targetMask
            fireProgressStateChanged("Building target mask pyramid");
            pyramidType = TARGET_MASK;
            targetMask = new boolean[targetSliceSize];

            if (entireTarget == false) {

                // mask = targetImage.generateVOIMask();
                for (i = 0; i < targetSliceSize; i++) {

                    if (tMask.get(i)) {
                        targetMask[i] = true;
                    } else {
                        targetMask[i] = false;
                    }
                }

                tMask = null;
            } // if (entireTarget == false)
            else {

                for (i = 0; i < targetSliceSize; i++) {
                    targetMask[i] = true;
                }
            }

            buildPyramid(pyramidType, targetMask, targetWidth, targetHeight);

            // sourceMask
            fireProgressStateChanged("Building source mask pyramid");
            pyramidType = SOURCE_MASK;
            sourceMask = new boolean[sourceSliceSize];

            if (entireSource == false) {

                // mask = sourceImage.generateVOIMask();
                for (i = 0; i < sourceSliceSize; i++) {

                    if (mask.get(i)) {
                        sourceMask[i] = true;
                    } else {
                        sourceMask[i] = false;
                    }
                }

                mask = null;
            } // if (entireSource == false)
            else {

                for (i = 0; i < sourceSliceSize; i++) {
                    sourceMask[i] = true;
                }
            }

            buildPyramid(pyramidType, sourceMask, sourceWidth, sourceHeight);
            pixelPrecision = PIXEL_HIGH_PRECISION;
            maxIterations = MANY_ITERATIONS;
        } // if (interpolation == CUBIC_SPLINE)
        else if (interpolation == NEAREST_NEIGHBOR) {
            pixelPrecision = PIXEL_LOW_PRECISION;
            maxIterations = FEW_ITERATIONS;

            targetMask = new boolean[targetSliceSize];

            if (entireTarget == false) {

                // mask = targetImage.generateVOIMask();
                for (i = 0; i < targetSliceSize; i++) {

                    if (tMask.get(i)) {
                        targetMask[i] = true;
                    } else {
                        targetMask[i] = false;
                    }
                }

                tMask = null;
            } // if (entireTarget == false)
            else {

                for (i = 0; i < targetSliceSize; i++) {
                    targetMask[i] = true;
                }
            }

            sourceMask = new boolean[sourceSliceSize];

            if (entireSource == false) {

                // mask = sourceImage.generateVOIMask();
                for (i = 0; i < sourceSliceSize; i++) {

                    if (mask.get(i)) {
                        sourceMask[i] = true;
                    } else {
                        sourceMask[i] = false;
                    }
                }

                mask = null;
            } // if (entireSource == false)
            else {

                for (i = 0; i < sourceSliceSize; i++) {
                    sourceMask[i] = true;
                }
            }
        } // else if (interpolation == NEAREST_NEIGHBOR)

        if (automan == AUTOMATIC) {
            fireProgressStateChanged("Refining the source landmark positions");
            doRegistration();
        } // if (automan == AUTOMATIC)

        fireProgressStateChanged("Computing the final image");
        doFinalTransform();

        try {
            resultImage[0].importData(0, outImg, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmRegTurbo: IOException on resultImage[0]" + ".importData(0,outImg,true)");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("AlgorithmRegTurbo: Out of memory on resultImage[0]" + ".importData(0,outImg,true)");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        try {
            resultImage[1].importData(0, outMsk, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmRegTurbo: IOException on resultImage[1]" + ".importData(0,outMsk,true)");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("AlgorithmRegTurbo: Out of memory on resultImage[1]" + ".importData(0,outMsk,true)");
            
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        
        setCompleted(true);
        notifyListeners(this);

        return;

    }

    /**
     * end computeBilinearGradientConstants.
     */
    private void turbo3D() { }

    /**
     * getBilinearMeanSquares ------------------------------------------------------------------
     */
    private void xDxWeights() {
        s = 1.0 - x;
        dxWeight[0] = 0.5 * x * x;
        xWeight[0] = x * dxWeight[0] / 3.0;
        dxWeight[3] = -0.5 * s * s;
        xWeight[3] = s * dxWeight[3] / -3.0;
        dxWeight[1] = 1.0 - (2.0 * dxWeight[0]) + dxWeight[3];
        xWeight[1] = (2.0 / 3.0) + ((1.0 + x) * dxWeight[3]);
        dxWeight[2] = 1.5 * x * (x - (4.0 / 3.0));
        xWeight[2] = (2.0 / 3.0) - ((2.0 - x) * dxWeight[0]);
    }

    /**
     * xDxWeights ------------------------------------------------------------------
     */
    private void xIndexes() {
        p = (int) Math.floor(x) + 2;

        for (int k = 0; (k < 4); p--, k++) {
            q = (p < 0) ? (-1 - p) : (p);

            if (twiceInNx <= q) {
                q %= twiceInNx;
            }

            xIndex[k] = (inNx <= q) ? (twiceInNx - 1 - q) : (q);
        }
    }

    /**
     * xIndexes ------------------------------------------------------------------
     */
    private void xWeights() {
        s = 1.0 - x;
        xWeight[3] = s * s * s / 6.0;
        s = x * x;
        xWeight[2] = (2.0 / 3.0) - (0.5 * s * (2.0 - x));
        xWeight[0] = s * x / 6.0;
        xWeight[1] = 1.0 - xWeight[0] - xWeight[2] - xWeight[3];
    }

    /**
     * xWeights ------------------------------------------------------------------
     */
    private void yDyWeights() {
        t = 1.0 - y;
        dyWeight[0] = 0.5 * y * y;
        yWeight[0] = y * dyWeight[0] / 3.0;
        dyWeight[3] = -0.5 * t * t;
        yWeight[3] = t * dyWeight[3] / -3.0;
        dyWeight[1] = 1.0 - (2.0 * dyWeight[0]) + dyWeight[3];
        yWeight[1] = (2.0 / 3.0) + ((1.0 + y) * dyWeight[3]);
        dyWeight[2] = 1.5 * y * (y - (4.0 / 3.0));
        yWeight[2] = (2.0 / 3.0) - ((2.0 - y) * dyWeight[0]);
    }

    /**
     * yDyWeights ------------------------------------------------------------------
     */
    private void yIndexes() {
        p = (int) Math.floor(y) + 2;

        for (int k = 0; (k < 4); p--, k++) {
            q = (p < 0) ? (-1 - p) : (p);

            if (twiceInNy <= q) {
                q %= twiceInNy;
            }

            yIndex[k] = (inNy <= q) ? ((twiceInNy - 1 - q) * inNx) : (q * inNx);
        }
    }

    /**
     * yIndexes ------------------------------------------------------------------
     */
    private void yWeights() {
        t = 1.0 - y;
        yWeight[3] = t * t * t / 6.0;
        t = y * y;
        yWeight[2] = (2.0 / 3.0) - (0.5 * t * (2.0 - y));
        yWeight[0] = t * y / 6.0;
        yWeight[1] = 1.0 - yWeight[0] - yWeight[2] - yWeight[3];
    }

}
