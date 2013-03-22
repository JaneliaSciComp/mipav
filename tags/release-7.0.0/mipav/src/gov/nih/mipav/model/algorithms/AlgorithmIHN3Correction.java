package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.JamaMatrix;

import gov.nih.mipav.view.*;

import java.io.IOException;
import java.util.*;


/**
 * <p>
 * N3 Inhomogeneity correction This is based on code by John G. Sled, McConnell Brain Imaging Centre, Montreal
 * Neurological Institute, McGill University Information may be found at: http://www.bic.mni.mcgill.ca/software/N3/
 * Bibliography:
 * </p>
 * 
 * <p>
 * 1. J.G. Sled, A.P. Zijdenbos, and A.C. Evans, "A non-parametric method for automatic correction of intensity
 * non-uniformity in MRI data," IEEE Transactions on Medical Imaging, vol. 17, pp. 87-97, February, 1998.
 * </p>
 * 
 * <p>
 * 2. J.G. Sled, A.P. Zijdenbos, and A.C. Evans, "A comparison of retrospective intensity non-uniformity correction
 * methods for MRI," in Information Processing in Medical Imaging, pp. 459-464, 1997.
 * </p>
 * 
 * <p>
 * 3. J.G. Sled, "A non-parametric method for automatic correction of intensity non-uniformity in MRI data," Master's
 * thesis, McGill University, Montreal, QC, May, 1997.
 * </p>
 * 
 * <p>
 * The N3 method should work with any MR volume including raw (non-stereotaxic) data.
 * </p>
 * 
 * <p>
 * An artifact often seen in MRI is for the signal intensity to vary smoothly across an image. Variously referred to as
 * RF inhomogeneity, shading artifact, or intensity non-uniformity, it is usually attributed to such factors as poor
 * radio frequency(RF) field uniformity, eddy currents driven by switching of field gradients, and patient anatomy both
 * inside and outside the field of view.
 * </p>
 * 
 * <p>
 * This code corrects intensity non-uniformity in MR data without requiring supervision. This method can be applied
 * without a tissue intensity or geometric model. Described as Non-parametric Non_uniform intensity Normalization (N3),
 * the method is independent of pulse sequence and insensitive to pathological data that might otherwise violate model
 * assumptions. To eliminate the dependence of the field estimate on anatomy, an iterative approach is employed to
 * estimate both the multiplicative bias field and the distribution of true tissue intensities. Preprocessing of MR data
 * using N3 has been shown to substantially improve the accuracy of anatomical analysis techniques such as tissue
 * classification and cortical surface extraction.
 * </p>
 * 
 * <hr>
 * 
 * <p>
 * From John G. Sled's original N3 code:
 * 
 * <blockquote>
 * 
 * <pre>
 * Copyright 1996, John G. Sled
 * McConnell Brain Imaging Centre,
 * Montreal Neurological Institute, McGill University.
 * 
 * Permission to use, copy, modify, and distribute this software and its documentation for any purpose and without fee
 * is hereby granted, provided that the above copyright notice appear in all copies. The author and McGill University
 * make no representations about the suitability of this software for any purpose. It is provided &quot;as is&quot; without
 * express or implied warranty.
 * </pre>
 * 
 * </blockquote>
 * </p>
 */
public class AlgorithmIHN3Correction extends AlgorithmBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** inverse FFT */
    public static final int INVERSE = -1;

    /** forward FFT */
    public static final int FORWARD = 1;

    /** DOCUMENT ME! */
    public static final int spline = 4; // splines are cubic

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[][] AtA;

    /** DOCUMENT ME! */
    private int AtAP, AtFP, UP, DP, FP;

    /** DOCUMENT ME! */
    private double[][] AtF;

    /**
     * If true determines the threshold by histogram analysis. If true a VOI cannot be used and the input threshold is
     * ignored.
     */
    private boolean autoThreshold = false;

    /** DOCUMENT ME! */
    private float[][][][] bendingMatrix;

    /** DOCUMENT ME! */
    private final int binNumber = 200;

    /** Histogram bin width. */
    private float binWidth;

    /** DOCUMENT ME! */
    private int blockInt, currentOffset;

    /** DOCUMENT ME! */
    private float[] blurI;

    /** DOCUMENT ME! */
    private float[] blurR;

    /** Buffer for original source image. */
    private float[] buffer;

    /** DOCUMENT ME! */
    private final float[][] bW = { { -1, 3, -3, 1}, {3, -6, 0, 4}, { -3, 3, 3, 1}, {1, 0, 0, 0}};

    /** DOCUMENT ME! */
    private float[] CArray;

    /** DOCUMENT ME! */
    private float class_max;

    /** DOCUMENT ME! */
    private float class_min;

    /** DOCUMENT ME! */
    private double[][] coef;

    /** DOCUMENT ME! */
    private float[] correctedBuffer;

    /** DOCUMENT ME! */
    private float[][] d1Spline;

    /** DOCUMENT ME! */
    private float[][] DArray;

    /** DOCUMENT ME! */
    private float denom;

    /** DOCUMENT ME! */
    private int[] derivative;

    /** DOCUMENT ME! */
    private int[] dloc_i;

    /** DOCUMENT ME! */
    private int[] dloc_j;

    /** DOCUMENT ME! */
    private float[][] domain;

    /**
     * The measure used to terminate the iterations is the coefficient of variation of change in field estimates between
     * successive iterations.
     */
    private float endTol = 0.001f;

    /**
     * Ideally the Weiner filter noise equals the white noise of the image. The performance of the N3 method is enhanced
     * by supplying a mask for the region of interest. If true, the N3 method is applied to the entire image.
     */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private float[] estimateBuffer;

    /** DOCUMENT ME! */
    private float estimateOffset;

    /** DOCUMENT ME! */
    private float estimateScale;

    /** DOCUMENT ME! */
    private float estimateSlope;

    /** DOCUMENT ME! */
    private float[] fHist;

    /** DOCUMENT ME! */
    private float[] fHistPaddedI;

    /** DOCUMENT ME! */
    private float[] fHistPaddedR;

    /** Lowest allowable field value. */
    private final float field_floor = 0.1f;

    /** DOCUMENT ME! */
    private float[] fieldBuffer;

    /**
     * Characteristic distance over which the field varies. The distance between adjacent knots in bspline fitting with
     * at least 4 knots going in every dimension. The default in the dialog is one third the distance (resolution *
     * extents) of the smallest dimension.
     */
    private float fieldDistance = 200.0f;

    /** DOCUMENT ME! */
    private ModelImage fieldImage;

    /** DOCUMENT ME! */
    private float[] filterI;

    /** DOCUMENT ME! */
    private float[] filterR;

    /** DOCUMENT ME! */
    private float fIndex;

    /** DOCUMENT ME! */
    private double firstMoment0;

    /** DOCUMENT ME! */
    private double firstMoment1;

    /** DOCUMENT ME! */
    private float fOffset;

    /** DOCUMENT ME! */
    private double fwhm;

    /** DOCUMENT ME! */
    private double fwhmFactor;

    /** DOCUMENT ME! */
    private double fwhmScale;

    /** DOCUMENT ME! */
    private double halfBin;

    /** DOCUMENT ME! */
    private int[] hist;

    /** DOCUMENT ME! */
    private float histFactor;

    /** DOCUMENT ME! */
    private double histMean;

    /** DOCUMENT ME! */
    private float histOffset;

    /** Sum of contents of hisotogram bins. */
    private double histSum;

    /** DOCUMENT ME! */
    private int iLoc;

    /** DOCUMENT ME! */
    private float incr, value_k;

    /** DOCUMENT ME! */
    private int index;

    /** DOCUMENT ME! */
    private int index1;

    /** DOCUMENT ME! */
    private float[][] integral;

    /** DOCUMENT ME! */
    private int interval;

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private double[][] JArray;

    /**
     * Width of deconvolution kernel used to sharpen the histogram. Larger values give faster convergence while smaller
     * values give greater accuracy.
     */
    private float kernelfwhm = 0.15f;

    /** DOCUMENT ME! */
    private float[][] knots;

    /** DOCUMENT ME! */
    private float lambda;

    /** DOCUMENT ME! */
    private float loc;

    /** DOCUMENT ME! */
    private int[] locations;

    /** Log transformed data. */
    private float[] logBuffer;

    /** If minimum value of shrunken buffer < 1.0f, amount that must be added to make it 1.0f. Otherwise == 0.0f. */
    private float logOffset;

    /** DOCUMENT ME! */
    private int[] lower;

    /** DOCUMENT ME! */
    private BitSet mask;

    /** Maximum number of iterations. */
    private int maxIters = 50;

    /** DOCUMENT ME! */
    private float[] momentI;

    /** DOCUMENT ME! */
    private float[] momentR;

    /** Number of basis functions in each dimension. */
    private int[] nArray;

    /** Number of histogram bins for shrunken buffer. */
    private int nBins;

    /** DOCUMENT ME! */
    private int nDimensions;

    /** DOCUMENT ME! */
    private int[] newDim;

    /** DOCUMENT ME! */
    private float newRes;

    /** DOCUMENT ME! */
    private float[] newResol;

    /** DOCUMENT ME! */
    private int newSliceSize;

    /** DOCUMENT ME! */
    private int newVolSize;

    /** DOCUMENT ME! */
    private int nMax;

    /** Noise used in Weiner filter. */
    private float noise = 0.01f;

    /** DOCUMENT ME! */
    private int nProduct, four;

    /** DOCUMENT ME! */
    private int offset;

    /** DOCUMENT ME! */
    private int offset1, offset2;

    /** DOCUMENT ME! */
    private int[][] offsetSp;

    /** DOCUMENT ME! */
    private int offsetStep, first, last;

    /** DOCUMENT ME! */
    private int order;

    /** DOCUMENT ME! */
    private int[] orgDim;

    /** Distance per pixel in millimeters. */
    private final float[] orgResol;

    /** DOCUMENT ME! */
    private int padded_size;

    /** DOCUMENT ME! */
    private int pLocation, pLocation2, pValue, pDloc_i, pDloc_j;

    /** DOCUMENT ME! */
    private float product;

    /** DOCUMENT ME! */
    private int pSpline, pOffset;

    /** DOCUMENT ME! */
    private int region;

    /** DOCUMENT ME! */
    private float[] residueBuffer;

    /** Buffer for sampled image. */
    private float[] sBuffer;

    /** DOCUMENT ME! */
    private float scale;

    /**
     * The factor by which the data is subsampled to a lower resolution in estimating the slowly varying non-uniformity
     * field. Reduce sampling in the finest sampling direction by the shrink factor. Reduce other sampling directions
     * only if resolution is less than shrink times original minimum resolution. Uses nearest neighbor resampling.
     */
    private float shrink = 4.0f;

    /** DOCUMENT ME! */
    private int sizeC;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private double slope;

    /** DOCUMENT ME! */
    private int[] smallN;

    /** Mask for shrunken buffer. */
    private BitSet sMask = null;

    /** Maximum value of shrunken buffer. */
    private float sMax;

    /** Minimum value of shrunken buffer. */
    private float sMin;

    /** DOCUMENT ME! */
    private float sourceMin, sourceMax;

    /** DOCUMENT ME! */
    private final float spline_lambda = 1.0f;

    /** DOCUMENT ME! */
    private final float spline_subsample = 1.0f;

    /** DOCUMENT ME! */
    private double sqrtNum;

    /** DOCUMENT ME! */
    private int start, length, mid;

    /** DOCUMENT ME! */
    private float startK;

    /** DOCUMENT ME! */
    private float stddev, mean, numVoxels, sumValue, sum2Value;

    /** DOCUMENT ME! */
    private int[] step;

    /** DOCUMENT ME! */
    private double Sx, Sy, Sz;

    /** DOCUMENT ME! */
    private float temp, temp2;

    /** Values at less than threshold are treated as part of the background. */
    private float threshold = 1.0f;

    /** DOCUMENT ME! */
    private int tiflat;

    /** DOCUMENT ME! */
    private int[] tiindex;

    /** DOCUMENT ME! */
    private int[] tistep;

    /** DOCUMENT ME! */
    private int tjflat;

    /** DOCUMENT ME! */
    private int[] tjindex;

    /** DOCUMENT ME! */
    private int[] tjstep;

    /** Mask that meets threshold and sMask requirements for shrunken volume. */
    private BitSet tMask = null;

    /** FORWARD or INVERSE for FFT. */
    private int transformDir;

    /** DOCUMENT ME! */
    // private TransMatrix transMatrix;
    /** DOCUMENT ME! */
    private int[] upper;

    /** If true, the program is run from a script. */
    private boolean useScript = false;

    /** DOCUMENT ME! */
    private float value;

    /** DOCUMENT ME! */
    private float value1, value2, frac, rfrac;

    /** DOCUMENT ME! */
    private float[] values;

    /** DOCUMENT ME! */
    private double vari;

    /** DOCUMENT ME! */
    private double varMax;

    /** DOCUMENT ME! */
    private int volSize;

    /** Factor by which to shrink volume. */
    private float volumeFactor = 1.0f;

    /** DOCUMENT ME! */
    private float[] workingBuffer;

    /** DOCUMENT ME! */
    private float xf;

    /** DOCUMENT ME! */
    // private float[][] xfrm;
    /** DOCUMENT ME! */
    private int xo, xyo;

    /** DOCUMENT ME! */
    private float xv, xyv;

    /** DOCUMENT ME! */
    private float[] yMat;

    /** DOCUMENT ME! */
    private float[] ys;

    /** DOCUMENT ME! */
    private float[] zero;

    /** DOCUMENT ME! */
    private double zeroMoment0;

    /** DOCUMENT ME! */
    private double zeroMoment1;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmIHN3Correction object.
     * 
     * @param destImg image model where result image is to stored
     * @param fieldImg image model where used field is stored
     * @param srcImg source image model
     * @param _threshold Values at less than _threshold are treated as part of the background
     * @param _maxIters Maximum number of iterations
     * @param _endTol The measure used to terminate the iterations is the coefficient of variation of change in field
     *            estimates between successive iterations.
     * @param _fieldDistance Characteristic distance over which the field varies. The distance between adjacent knots in
     *            bspline fitting with at least 4 knots going in every dimension. The default in the dialog is one third
     *            the distance (resolution * extents) of the smallest dimension.
     * @param _shrink The factor by which the data is subsampled to a lower resolution in estimating the slowly varying
     *            non-uniformity field. Reduce sampling in the finest sampling direction by the shrink factor.
     * @param _kernelfwhm Width of deconvolution kernel used to sharpen the histogram. Larger values give faster
     *            convergence while smaller values give greater accuracy.
     * @param _noise Noise used in Weiner filter
     * @param _entireImage If true, the N3 method is applied to the entire image. If false, the N3 method is applied
     *            only to the region of interest.
     * @param _autoThreshold If true determines the threshold by histogram analysis. If true a VOI cannot be used and
     *            the input threshold is ignored.
     * @param useScript If true, the program is run from a script
     */
    public AlgorithmIHN3Correction(final ModelImage destImg, final ModelImage fieldImg, final ModelImage srcImg,
            final float _threshold, final int _maxIters, final float _endTol, final float _fieldDistance,
            final float _shrink, final float _kernelfwhm, final float _noise, final boolean _entireImage,
            final boolean _autoThreshold, final boolean useScript) {

        super(destImg, srcImg);
        fieldImage = fieldImg;
        threshold = _threshold;
        maxIters = _maxIters;
        endTol = _endTol;
        fieldDistance = _fieldDistance;
        shrink = _shrink;
        kernelfwhm = _kernelfwhm;
        noise = _noise;
        entireImage = _entireImage;
        autoThreshold = _autoThreshold;
        this.useScript = useScript;
        orgResol = new float[srcImage.getNDims()];

        for (int i = 0; i < srcImage.getNDims(); i++) {
            orgResol[i] = srcImage.getFileInfo(0).getResolutions()[i];
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        cleanUp();
        super.finalize();
    }

    /**
     * Start algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("IHN3Correction: Source Image is null");

            return;
        }

        if (srcImage.getNDims() == 2) {
            nDimensions = 2;
            IHN3Correction2();
        } else if (srcImage.getNDims() == 3) {
            nDimensions = 3;
            IHN3Correction3();
        }

        if (threadStopped) {
            finalize();

            return;
        }
    }

    /**
     * Prepares this class for destruction.
     */
    private void cleanUp() {

        correctedBuffer = null;
        workingBuffer = null;
        estimateBuffer = null;
        domain = null;
        lower = null;
        upper = null;
        step = null;
        nArray = null;
        bendingMatrix = null;
        CArray = null;
        DArray = null;
        integral = null;
        tiindex = null;
        tistep = null;
        tjindex = null;
        tjstep = null;
        derivative = null;
        yMat = null;
        ys = null;
        fHist = null;
        fieldBuffer = null;
        buffer = null;
        logBuffer = null;
        sBuffer = null;
        residueBuffer = null;
        correctedBuffer = null;
        workingBuffer = null;

        fieldImage = null;
    }

    /**
     * This is the method that calculates the FFT.
     * 
     * @param rData real data
     * @param iData imaginary data
     */
    private void fft(final float[] rData, final float[] iData) {

        final double TWO_PI = 2 * java.lang.Math.PI;
        double wt1Imag, wt1Real;
        double angle, delta;
        float imag, real, fTemp, fReal, fImag;
        int i, index1, index2, index3;
        int j1, j2, j3;
        int k1, k1Double;
        int iSwap, i1Swap, i2Swap, index, dim;
        int direction;
        final int ndim = 1;

        if (transformDir == AlgorithmIHN3Correction.FORWARD) {
            direction = 1;
        } else {
            direction = -1;
        }

        // fireProgressStateChanged("Running FFT algorithm...");
        j1 = 1;
        dim = 1;

        for (i = 0; i < ndim; i++) {
            j1 *= dim;
            dim = padded_size;
            j2 = j1 * dim;
            j3 = j2 * (padded_size / (dim * j1));

            i1Swap = 0;

            for (index1 = 0; index1 < j2; index1 += j1) {

                for (index2 = index1; (index2 < (index1 + j1)) && (index1 < i1Swap); index2++) {

                    for (index3 = index2; index3 < j3; index3 += j2) {
                        i2Swap = -index1 + index3 + i1Swap;

                        fTemp = iData[index3];
                        iData[index3] = iData[i2Swap];
                        iData[i2Swap] = fTemp;

                        fTemp = rData[index3];
                        rData[index3] = rData[i2Swap];
                        rData[i2Swap] = fTemp;
                    }
                }

                for (iSwap = j2 / 2; (iSwap >= j1) && (iSwap < (i1Swap + 1)); iSwap >>= 1) {
                    i1Swap = i1Swap - iSwap;
                }

                i1Swap = i1Swap + iSwap;
            }

            for (k1 = j1; k1 < j2; k1 <<= 1) {
                delta = TWO_PI / (k1 << 1) * direction * j1;
                angle = 0;

                for (index1 = 0, angle = 0; index1 < k1; index1 += j1) {
                    wt1Imag = java.lang.Math.sin(angle);
                    wt1Real = java.lang.Math.cos(angle);
                    angle += delta;

                    for (index2 = index1; index2 < (index1 + j1); index2++) {
                        k1Double = k1 << 1;

                        for (index3 = index2; index3 < j3; index3 += k1Double) {
                            index = index3 + k1;
                            fReal = rData[index];
                            fImag = iData[index];
                            imag = (float) ( (fImag * wt1Real) + (fReal * wt1Imag));
                            real = (float) ( (fReal * wt1Real) - (fImag * wt1Imag));
                            iData[index] = iData[index3] - imag;
                            rData[index] = rData[index3] - real;
                            iData[index3] = iData[index3] + imag;
                            rData[index3] = rData[index3] + real;
                        }
                    }
                }
            }
        }

        if (transformDir == AlgorithmIHN3Correction.INVERSE) {

            for (i = 0; i < padded_size; i++) {
                rData[i] = rData[i] / padded_size;
                iData[i] = iData[i] / padded_size;
            }

        } // end of if (transformDir == INVERSE)

    } // end of FFT()

    /**
     * DOCUMENT ME!
     */
    private void fitSplinesToVolumeLookup2D() {

        // do least squares fit to data

        int x, y, i, j, k, l, m;
        int fourX, fourY;

        for (x = lower[0]; x <= upper[0]; x += spline_subsample) {
            fourX = 4 * x;

            for (y = lower[1]; y <= upper[1]; y += spline_subsample) {
                fourY = 4 * y;
                index = x + (y * newDim[0]);

                if (tMask.get(index)) {
                    value = workingBuffer[index];

                    // compute 4 by 4 tensor
                    pValue = 0;
                    pLocation = 0;

                    for (i = fourX; i < (fourX + 4); i++) {
                        xv = d1Spline[0][i];
                        xo = offsetSp[0][i];

                        for (j = fourY; j < (fourY + 4); j++) {
                            values[pValue++] = xv * d1Spline[1][j];
                            locations[pLocation++] = xo + offsetSp[1][j];
                        } // for (j = 0; j < 4; j++)
                    } // for (i = 0; i < 4; i++)

                    // create list of differences between locations for fast indexing
                    pDloc_i = 0;
                    pDloc_j = 0;
                    pLocation = 0;
                    pLocation2 = 1;

                    for (i = (four - 2); i >= 0; i--) {
                        dloc_i[pDloc_i] = locations[pLocation2++] - locations[pLocation++];
                        dloc_j[pDloc_j++] = dloc_i[pDloc_i++] * nProduct;
                    }

                    // Add new data to AtA and AtF
                    AtAP = 0;
                    AtFP = 0;

                    // start in bottom right corner of block within AtA
                    DP = AtAP + (locations[four - 1] * nProduct) + locations[four - 1];
                    FP = AtFP + locations[four - 1];

                    for (k = (four - 1); k >= 0;) {
                        UP = DP;
                        value_k = values[k];
                        AtF[FP][0] += value * value_k;
                        l = k - 1;
                        pValue = l;
                        pDloc_i = l;

                        for (; l >= 0; l--) { // only do half in Sled's code since AtA is
                            // symmetric and he has special
                            // code to require only the diagonal and
                            // upper parts. However, with my code I must
                            // explicitly calculate the entire symmetric
                            // matrix
                            incr = value_k * values[pValue--];
                            UP -= dloc_i[pDloc_i--];
                            AtA[UP / nProduct][UP % nProduct] += incr;
                            AtA[UP % nProduct][UP / nProduct] += incr;
                        }

                        // do diagonal elements separately
                        AtA[DP / nProduct][DP % nProduct] += value_k * value_k;

                        --k;

                        if (k >= 0) {
                            DP -= dloc_i[k] + dloc_j[k];
                            FP -= dloc_i[k];
                        } // if (k >= 0)
                    } // for (k = (four-1); k >= 0;)

                } // if (tMask.get(index))
            } // for (y = lower[1]; y <= upper[1]; y += spline_subsample)
        } // for (x = lower[0]; x <= upper[0]; x += spline_subsample)

        // fit splines to the data

        // compute N dimensional bending energy matrix
        // calculate step sizes for indexing an array as a tensor
        for (i = 0; i < nDimensions; i++) {
            step[i] = 1;

            for (j = (i + 1); j < nDimensions; j++) {
                step[i] *= nArray[j];
            }
        }

        // compute 1-D bending energy matrices
        for (i = 0; i < nDimensions; i++) {

            for (order = 0; order < 3; order++) {

                // forms a nArray[i] by nArray[i] matrix with order'th derivatives
                if (nArray[i] < 4) {
                    MipavUtil.displayError("algorithm IHN3Correction bending energy not defined for size < 4");

                    setCompleted(false);

                    return;
                }

                // standardized cubic B-spline defined on [-4 0] with knots at
                // -4 -3 -2 -1 0
                // each unit segment is written as a cubic defined on [0 1]
                // e.g. -x^3 + 3x^2 -3x + 1
                bW[0][0] = -1;
                bW[0][1] = 3;
                bW[0][2] = -3;
                bW[0][3] = 1;
                bW[1][0] = 3;
                bW[1][1] = -6;
                bW[1][2] = 0;
                bW[1][3] = 4;
                bW[2][0] = -3;
                bW[2][1] = 3;
                bW[2][2] = 3;
                bW[2][3] = 1;
                bW[3][0] = 1;
                bW[3][1] = 0;
                bW[3][2] = 0;
                bW[3][3] = 0;

                // take order'th derivative of B
                for (m = 0; m < order; m++) {

                    for (j = 0; j < AlgorithmIHN3Correction.spline; j++) {

                        for (k = 1; k < AlgorithmIHN3Correction.spline; k++) {
                            bW[j][AlgorithmIHN3Correction.spline - k] = k
                                    * bW[j][AlgorithmIHN3Correction.spline - k - 1];
                        }

                        bW[j][0] = 0.0f;
                    }
                }

                // compute product integral for each pair of segments
                // actually we only compute upper triangle since D will be symmetric

                int endK;
                int maxK1;
                int maxK2;

                for (m = 0; m < AlgorithmIHN3Correction.spline; m++) {

                    for (j = m; j < AlgorithmIHN3Correction.spline; j++) {

                        // convolve each pair of polynomials
                        for (k = 0; k < sizeC; k++) {
                            CArray[k] = 0;
                            endK = k + 1;

                            if ( (sizeC - k) < endK) {
                                endK = sizeC - k;
                            }

                            for (l = 0; l < endK; l++) {
                                maxK1 = k - AlgorithmIHN3Correction.spline + 1;

                                if (maxK1 < 0) {
                                    maxK1 = 0;
                                }

                                maxK2 = AlgorithmIHN3Correction.spline - 1 - k;

                                if (maxK2 < 0) {
                                    maxK2 = 0;
                                }

                                CArray[k] += bW[m][maxK1 + l] * bW[j][AlgorithmIHN3Correction.spline - 1 - l - maxK2];
                            }
                        } // for (k = 0; k < sizeC; k++)

                        // evaluate integral on [0,1]
                        DArray[m][j] = 0;

                        for (k = 0; k < sizeC; k++) {
                            DArray[m][j] += CArray[k] / (sizeC - k);
                        } // for (k = 0; k < sizeC; k++)

                    } // for (j = m; j < spline; j++)
                } // for (m = 0; m < spline; m++)

                // define 6 regions: [-1,0] [-2,0] [-3,0] [-4,0] [-2,-1] [-3, -1]
                // splines can be shifted with respect to each by 0 1 2 or 3
                // Compute product interval on each interval for each offset

                // start by computing integrals for first four regions
                for (offset = 0; offset < 4; offset++) {

                    for (region = 0; region < (4 - offset); region++) {
                        integral[region][offset] = 0;

                        for (interval = 0; interval <= region; interval++) {
                            integral[region][offset] += DArray[interval][interval + offset];
                        } // for (interval = 0; interval <= region; interval++)
                    } // for (region = 0; region < (4 - offset); region++)

                    // last few regions are same since large offset is equivalent to a
                    // small region
                    for (region = (4 - offset); region < 4; region++) {
                        integral[region][offset] = integral[region - 1][offset];
                    } // for (region = (4 - offset); region < 4; region++)
                } // for (offset = 0; offset < 4; offset++)

                // compute integrals for regions five and six separately
                integral[4][0] = DArray[1][1];
                integral[4][1] = DArray[1][2];
                integral[4][2] = DArray[1][3];
                integral[4][3] = 0;

                integral[5][0] = DArray[1][1] + DArray[2][2];
                integral[5][1] = DArray[1][2] + DArray[2][3];
                integral[5][2] = DArray[1][3];
                integral[5][3] = 0;

                // form integrals into bending energy matrix

                // set elements common to matrices of all sizes first
                for (m = 0; m < 3; m++) {
                    bendingMatrix[i][order][0][m] = integral[0][m];
                    bendingMatrix[i][order][m][0] = integral[0][m];
                    bendingMatrix[i][order][nArray[i] - 1][nArray[i] - m - 1] = integral[0][m];
                    bendingMatrix[i][order][nArray[i] - m - 1][nArray[i] - 1] = integral[0][m];
                } // for (m = 0 ; m < 3; m++)

                if (nArray[i] == 4) { // deal with special cases
                    bendingMatrix[i][order][1][1] = integral[4][0];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 2] = integral[4][0];
                    bendingMatrix[i][order][1][2] = integral[4][1];
                    bendingMatrix[i][order][2][1] = integral[4][1];
                    bendingMatrix[i][order][3][0] = integral[3][3];
                    bendingMatrix[i][order][0][3] = integral[3][3];
                } // if (nArray[i] == 4)
                else if (nArray[i] == 5) {
                    bendingMatrix[i][order][1][1] = integral[1][0];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 2] = integral[1][0];
                    bendingMatrix[i][order][2][2] = integral[5][0];
                    bendingMatrix[i][order][2][1] = integral[1][1];
                    bendingMatrix[i][order][1][2] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 3][nArray[i] - 2] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 3] = integral[1][1];
                    bendingMatrix[i][order][1][3] = integral[3][2];
                    bendingMatrix[i][order][3][1] = integral[3][2];

                    for (m = 0; m < (nArray[i] - 3); m++) {
                        bendingMatrix[i][order][m][m + 3] = integral[3][3];
                        bendingMatrix[i][order][m + 3][m] = integral[3][3];
                    } // for (m = 0; m < (nArray[i]-3); m++)
                } // else if (nArray[i] == 5)
                else { // n > 5 (general case)

                    // fill in bulk region
                    for (j = 0; j < 4; j++) {

                        for (m = (3 - j); m < (nArray[i] - 3); m++) {
                            bendingMatrix[i][order][m][m + j] = integral[3][j];
                            bendingMatrix[i][order][m + j][m] = integral[3][j];
                        } // for (m = (3-j); m < (nArray[i]-3); m++)
                    } // for (j = 0; j < 4; j++)

                    // fill in boundary region
                    bendingMatrix[i][order][2][2] = integral[2][0];
                    bendingMatrix[i][order][nArray[i] - 3][nArray[i] - 3] = integral[2][0];
                    bendingMatrix[i][order][1][2] = integral[1][1];
                    bendingMatrix[i][order][2][1] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 3] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 3][nArray[i] - 2] = integral[1][1];
                    bendingMatrix[i][order][1][1] = integral[1][0];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 2] = integral[1][0];
                } // else n > 5
            } // for (order = 0; order < 3; order++)
        } // for (i = 0; i < nDimensions; i++)

        // allocate large matrix
        for (i = 0; i < nProduct; i++) {

            for (j = 0; j < nProduct; j++) {
                JArray[i][j] = 0.0;
            } // for (j = 0; j < nProduct; j++)
        } // for (i = 0; i < nProduct; i++)

        // form kronecker products
        // eg. in 3D x"yz + xy"z + xyz" + 2x'y'z + 2xy'z' + 2x'yz'
        //

        tiflat = 0;

        for (i = 0; i < nDimensions; i++) {
            tiindex[i] = 0;
            tistep[i] = 1;

            for (j = (i + 1); j < nDimensions; j++) {
                tistep[i] *= nArray[j];
            }
        }

        tjflat = 0;

        for (i = 0; i < nDimensions; i++) {
            tjindex[i] = 0;
            tjstep[i] = 1;

            for (j = (i + 1); j < nDimensions; j++) {
                tjstep[i] *= nArray[j];
            }
        }

        // do terms with second derivatives
        for (k = 0; k < nDimensions; k++) {

            for (i = 0; i < nDimensions; i++) {
                derivative[i] = 0;
            }

            derivative[k] = 2;

            for (i = 0; i < nProduct; i++) {

                for (j = 0; j < nProduct; j++) {
                    product = 1.0f;

                    for (m = 0; m < nDimensions; m++) {
                        product *= bendingMatrix[m][derivative[m]][tiindex[m]][tjindex[m]];
                    } // for (m = 0; m < nDimensions; m++)

                    JArray[tiflat][tjflat] += product;

                    for (m = (nDimensions - 1); m >= 0; m--) {

                        if (tjindex[m] < (nArray[m] - 1)) {
                            tjindex[m]++;
                            tjflat += tjstep[m];

                            break;
                        } else {
                            tjindex[m] = 0;
                            tjflat -= (nArray[m] - 1) * tjstep[m];
                        }
                    } // for (m = (nDimensions-1); m >= 0; m--)
                } // for (j = 0; j < nProduct; j++)

                for (m = (nDimensions - 1); m >= 0; m--) {

                    if (tiindex[m] < (nArray[m] - 1)) {
                        tiindex[m]++;
                        tiflat += tistep[m];

                        break;
                    } else {
                        tiindex[m] = 0;
                        tiflat -= (nArray[m] - 1) * tistep[m];
                    }
                } // for (m = (nDimensions); m >= 0; m--)
            } // for (i = 0; i < nProduct; i++)
        } // for (k = 0; k < nDimensions; k++)

        for (i = 0; i < nDimensions; i++) {
            tiindex[i] = 0;
            tjindex[i] = 0;
        }

        // do terms with first derivatives
        for (k = 0; k < nDimensions; k++) {

            for (l = k + 1; l < nDimensions; l++) {

                for (m = 0; m < nDimensions; m++) {
                    derivative[m] = 0;
                }

                derivative[k] = 1;
                derivative[l] = 1;

                for (i = 0; i < nProduct; i++) {

                    for (j = 0; j < nProduct; j++) {
                        product = 1.0f;

                        for (m = 0; m < nDimensions; m++) {
                            product *= bendingMatrix[m][derivative[m]][tiindex[m]][tjindex[m]];
                        } // for (m = 0; m < nDimensions; m++)

                        JArray[tiflat][tjflat] += 2.0f * product;

                        for (m = nDimensions - 1; m >= 0; m--) {

                            if (tjindex[m] < (nArray[m] - 1)) {
                                tjindex[m]++;
                                tjflat += tjstep[m];

                                break;
                            } else {
                                tjindex[m] = 0;
                                tjflat -= (nArray[m] - 1) * tjstep[m];
                            }
                        } // for (m = nDimensions-1; m >= 0; m--)
                    } // for (j = 0; j < nProduct; j++)

                    for (m = nDimensions - 1; m >= 0; m--) {

                        if (tiindex[m] < (nArray[m] - 1)) {
                            tiindex[m]++;
                            tiflat += tistep[m];

                            break;
                        } else {
                            tiindex[m] = 0;
                            tiflat -= (nArray[m] - 1) * tistep[m];
                        }
                    } // for (m = nDimensions; m >= 0; m--)
                } // for (i = 0; i < nProduct; i++)
            } // for (l = k+1; l < nDimensions; l++)
        } // for (k = 0; k < nDimensions; k++)

        for (i = 0; i < nProduct; i++) {

            for (j = 0; j < nProduct; j++) {
                JArray[i][j] = (lambda * JArray[i][j]) + AtA[i][j];
            }
        }

        // solve a system of equations (J)(coef) = AtF for coef where J is symmetric
        // The diagonal pivoting method is used to factor J as
        // J = U * D * U**T
        // where U is a product of permutation and unit upper triangular matrices, and
        // D is symmetric and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
        // The factored form of J is then used to solved the system of equations
        // J * coef = AtF

        // Compute the factorization J = U*D*U'
        // A = new Matrix(J);
        // B = new Matrix(AtF);
        final JamaMatrix AMat = new JamaMatrix(JArray);
        final JamaMatrix BMat = new JamaMatrix(AtF);
        final JamaMatrix XMat = AMat.solve(BMat);
        coef = XMat.getArrayCopy();

    }

    /**
     * DOCUMENT ME!
     */
    private void fitSplinesToVolumeLookup3D() {

        // do least squares fit to data

        int fourX, fourY, fourZ;
        int divUP, modUP;
        int x, y, z, i, j, k, l, m;

        for (x = lower[0]; (x <= upper[0]) && !threadStopped; x += spline_subsample) {
            fourX = 4 * x;

            for (y = lower[1]; (y <= upper[1]) && !threadStopped; y += spline_subsample) {
                fourY = 4 * y;
                offset2 = x + (y * newDim[0]);

                for (z = lower[2]; (z <= upper[2]) && !threadStopped; z += spline_subsample) {
                    fourZ = 4 * z;
                    index = offset2 + (z * newSliceSize);

                    if (tMask.get(index)) {
                        value = workingBuffer[index];

                        // compute 4 by 4 by 4 tensor
                        pValue = 0;
                        pLocation = 0;

                        for (i = fourX; i < (fourX + 4); i++) {
                            xv = d1Spline[0][i];
                            xo = offsetSp[0][i];

                            for (j = fourY; j < (fourY + 4); j++) {
                                xyv = xv * d1Spline[1][j];
                                xyo = xo + offsetSp[1][j];

                                for (k = fourZ; k < (fourZ + 4); k++) {
                                    values[pValue++] = xyv * d1Spline[2][k];
                                    locations[pLocation++] = xyo + offsetSp[2][k];
                                } // for (k = 0; k < 4; k++)
                            } // for (j = 0; j < 4; j++)
                        } // for (i = 0; i < 4; i++)

                        // create list of differences between locations for fast indexing
                        pDloc_i = 0;
                        pDloc_j = 0;
                        pLocation = 0;
                        pLocation2 = 1;

                        for (i = (four - 2); i >= 0; i--) {
                            dloc_i[pDloc_i] = locations[pLocation2++] - locations[pLocation++];
                            dloc_j[pDloc_j++] = dloc_i[pDloc_i++] * nProduct;
                        }

                        // Add new data to AtA and AtF
                        AtAP = 0;
                        AtFP = 0;

                        // start in bottom right corner of block within AtA
                        DP = AtAP + (locations[four - 1] * nProduct) + locations[four - 1];
                        FP = AtFP + locations[four - 1];

                        for (k = (four - 1); k >= 0;) {
                            UP = DP;
                            value_k = values[k];
                            AtF[FP][0] += value * value_k;
                            l = k - 1;
                            pValue = l;
                            pDloc_i = l;

                            for (; l >= 0; l--) { // only do half in Sled's code since AtA is
                                // symmetric and he has special
                                // code to require only the diagonal and
                                // upper parts. However, with my code I must
                                // explicitly calculate the entire symmetric
                                // matrix
                                incr = value_k * values[pValue--];
                                UP -= dloc_i[pDloc_i--];
                                modUP = UP % nProduct;
                                divUP = UP / nProduct;
                                AtA[divUP][modUP] += incr;
                                AtA[modUP][divUP] += incr;
                            }

                            // do diagonal elements separately
                            AtA[DP / nProduct][DP % nProduct] += value_k * value_k;

                            --k;

                            if (k >= 0) {
                                DP -= dloc_i[k] + dloc_j[k];
                                FP -= dloc_i[k];
                            } // if (k >= 0)
                        } // for (k = (four-1); k >= 0;)

                    } // if (tMask.get(index))
                } // for (z = lower[2]; z <= upper[2]; z += spline_subsample)
            } // for (y = lower[1]; y <= upper[1]; y += spline_subsample)
        } // for (x = lower[0]; x <= upper[0]; x += spline_subsample)

        if (threadStopped) {
            return;
        }

        // fit splines to the data

        // compute N dimensional bending energy matrix
        // calculate step sizes for indexing an array as a tensor
        // for (i = 0; i < nDimensions; i++) {
        // step[i] = 1;
        // for (j = (i+1); j < nDimensions; j++) {
        // step[i] *= nArray[j];
        // }
        // }

        // compute 1-D bending energy matrices
        for (i = 0; (i < nDimensions) && !threadStopped; i++) {

            for (order = 0; (order < 3) && !threadStopped; order++) {

                // forms a nArray[i] by nArray[i] matrix with order'th derivatives
                if (nArray[i] < 4) {
                    cleanUp();
                    MipavUtil.displayError("algorithm IHN3Correction bending energy not defined for size < 4");

                    setCompleted(false);

                    return;
                }
                // bendingMatrix[i][order] = new float[nArray[i]][nArray[i]];

                // standardized cubic B-spline defined on [-4 0] with knots at
                // -4 -3 -2 -1 0
                // each unit segment is written as a cubic defined on [0 1]
                // e.g. -x^3 + 3x^2 -3x + 1
                bW[0][0] = -1;
                bW[0][1] = 3;
                bW[0][2] = -3;
                bW[0][3] = 1;
                bW[1][0] = 3;
                bW[1][1] = -6;
                bW[1][2] = 0;
                bW[1][3] = 4;
                bW[2][0] = -3;
                bW[2][1] = 3;
                bW[2][2] = 3;
                bW[2][3] = 1;
                bW[3][0] = 1;
                bW[3][1] = 0;
                bW[3][2] = 0;
                bW[3][3] = 0;

                // take order'th derivative of B
                for (m = 0; m < order; m++) {

                    for (j = 0; j < AlgorithmIHN3Correction.spline; j++) {

                        for (k = 1; k < AlgorithmIHN3Correction.spline; k++) {
                            bW[j][AlgorithmIHN3Correction.spline - k] = k
                                    * bW[j][AlgorithmIHN3Correction.spline - k - 1];
                        }

                        bW[j][0] = 0.0f;
                    }
                }

                // compute product integral for each pair of segments
                // actually we only compute upper triangle since D will be symmetric
                int endK;
                int maxK1;
                int maxK2;

                for (m = 0; m < AlgorithmIHN3Correction.spline; m++) {

                    for (j = m; j < AlgorithmIHN3Correction.spline; j++) {

                        // convolve each pair of polynomials
                        for (k = 0; k < sizeC; k++) {
                            CArray[k] = 0;
                            endK = k + 1;

                            if ( (sizeC - k) < endK) {
                                endK = sizeC - k;
                            }

                            for (l = 0; l < endK; l++) {
                                maxK1 = k - AlgorithmIHN3Correction.spline + 1;

                                if (maxK1 < 0) {
                                    maxK1 = 0;
                                }

                                maxK2 = AlgorithmIHN3Correction.spline - 1 - k;

                                if (maxK2 < 0) {
                                    maxK2 = 0;
                                }

                                CArray[k] += bW[m][maxK1 + l] * bW[j][AlgorithmIHN3Correction.spline - 1 - l - maxK2];
                            }
                        } // for (k = 0; k < sizeC; k++)

                        // evaluate integral on [0,1]
                        DArray[m][j] = 0;

                        for (k = 0; k < sizeC; k++) {
                            DArray[m][j] += CArray[k] / (sizeC - k);
                        } // for (k = 0; k < sizeC; k++)

                    } // for (j = m; j < spline; j++)
                } // for (m = 0; m < spline; m++)

                // define 6 regions: [-1,0] [-2,0] [-3,0] [-4,0] [-2,-1] [-3, -1]
                // splines can be shifted with respect to each by 0 1 2 or 3
                // Compute product interval on each interval for each offset

                // start by computing integrals for first four regions
                for (offset = 0; offset < 4; offset++) {

                    for (region = 0; region < (4 - offset); region++) {
                        integral[region][offset] = 0;

                        for (interval = 0; interval <= region; interval++) {
                            integral[region][offset] += DArray[interval][interval + offset];
                        } // for (interval = 0; interval <= region; interval++)
                    } // for (region = 0; region < (4 - offset); region++)

                    // last few regions are same since large offset is equivalent to a
                    // small region
                    for (region = (4 - offset); region < 4; region++) {
                        integral[region][offset] = integral[region - 1][offset];
                    } // for (region = (4 - offset); region < 4; region++)
                } // for (offset = 0; offset < 4; offset++)

                // compute integrals for regions five and six separately
                integral[4][0] = DArray[1][1];
                integral[4][1] = DArray[1][2];
                integral[4][2] = DArray[1][3];
                integral[4][3] = 0;

                integral[5][0] = DArray[1][1] + DArray[2][2];
                integral[5][1] = DArray[1][2] + DArray[2][3];
                integral[5][2] = DArray[1][3];
                integral[5][3] = 0;

                // form integrals into bending energy matrix

                // set elements common to matrices of all sizes first
                for (m = 0; m < 3; m++) {
                    bendingMatrix[i][order][0][m] = integral[0][m];
                    bendingMatrix[i][order][m][0] = integral[0][m];
                    bendingMatrix[i][order][nArray[i] - 1][nArray[i] - m - 1] = integral[0][m];
                    bendingMatrix[i][order][nArray[i] - m - 1][nArray[i] - 1] = integral[0][m];
                } // for (m = 0 ; m < 3; m++)

                if (nArray[i] == 4) { // deal with special cases
                    bendingMatrix[i][order][1][1] = integral[4][0];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 2] = integral[4][0];
                    bendingMatrix[i][order][1][2] = integral[4][1];
                    bendingMatrix[i][order][2][1] = integral[4][1];
                    bendingMatrix[i][order][3][0] = integral[3][3];
                    bendingMatrix[i][order][0][3] = integral[3][3];
                } // if (nArray[i] == 4)
                else if (nArray[i] == 5) {
                    bendingMatrix[i][order][1][1] = integral[1][0];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 2] = integral[1][0];
                    bendingMatrix[i][order][2][2] = integral[5][0];
                    bendingMatrix[i][order][2][1] = integral[1][1];
                    bendingMatrix[i][order][1][2] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 3][nArray[i] - 2] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 3] = integral[1][1];
                    bendingMatrix[i][order][1][3] = integral[3][2];
                    bendingMatrix[i][order][3][1] = integral[3][2];

                    for (m = 0; m < (nArray[i] - 3); m++) {
                        bendingMatrix[i][order][m][m + 3] = integral[3][3];
                        bendingMatrix[i][order][m + 3][m] = integral[3][3];
                    } // for (m = 0; m < (nArray[i]-3); m++)
                } // else if (nArray[i] == 5)
                else { // n > 5 (general case)

                    // fill in bulk region
                    for (j = 0; j < 4; j++) {

                        for (m = (3 - j); m < (nArray[i] - 3); m++) {
                            bendingMatrix[i][order][m][m + j] = integral[3][j];
                            bendingMatrix[i][order][m + j][m] = integral[3][j];
                        } // for (m = (3-j); m < (nArray[i]-3); m++)
                    } // for (j = 0; j < 4; j++)

                    // fill in boundary region
                    bendingMatrix[i][order][2][2] = integral[2][0];
                    bendingMatrix[i][order][nArray[i] - 3][nArray[i] - 3] = integral[2][0];
                    bendingMatrix[i][order][1][2] = integral[1][1];
                    bendingMatrix[i][order][2][1] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 3] = integral[1][1];
                    bendingMatrix[i][order][nArray[i] - 3][nArray[i] - 2] = integral[1][1];
                    bendingMatrix[i][order][1][1] = integral[1][0];
                    bendingMatrix[i][order][nArray[i] - 2][nArray[i] - 2] = integral[1][0];
                } // else n > 5
            } // for (order = 0; order < 3; order++)
        } // for (i = 0; i < nDimensions; i++)

        if (threadStopped) {
            return;
        }

        // allocate large matrix
        for (i = 0; i < nProduct; i++) {

            for (j = 0; j < nProduct; j++) {
                JArray[i][j] = 0.0;
            }
        }

        // form kronecker products
        // eg. in 3D x"yz + xy"z + xyz" + 2x'y'z + 2xy'z' + 2x'yz'
        //

        tiflat = 0;

        for (i = 0; i < nDimensions; i++) {
            tiindex[i] = 0;
            tistep[i] = 1;

            for (j = (i + 1); j < nDimensions; j++) {
                tistep[i] *= nArray[j];
            }
        }

        tjflat = 0;

        for (i = 0; i < nDimensions; i++) {
            tjindex[i] = 0;
            tjstep[i] = 1;

            for (j = (i + 1); j < nDimensions; j++) {
                tjstep[i] *= nArray[j];
            }
        }

        // do terms with second derivatives
        for (k = 0; k < nDimensions; k++) {

            for (i = 0; i < nDimensions; i++) {
                derivative[i] = 0;
            }

            derivative[k] = 2;

            for (i = 0; i < nProduct; i++) {

                for (j = 0; j < nProduct; j++) {
                    product = 1.0f;

                    for (m = 0; m < nDimensions; m++) {
                        product *= bendingMatrix[m][derivative[m]][tiindex[m]][tjindex[m]];
                    } // for (m = 0; m < nDimensions; m++)

                    JArray[tiflat][tjflat] += product;

                    for (m = (nDimensions - 1); m >= 0; m--) {

                        if (tjindex[m] < (nArray[m] - 1)) {
                            tjindex[m]++;
                            tjflat += tjstep[m];

                            break;
                        } else {
                            tjindex[m] = 0;
                            tjflat -= (nArray[m] - 1) * tjstep[m];
                        }
                    } // for (m = (nDimensions-1); m >= 0; m--)
                } // for (j = 0; j < nProduct; j++)

                for (m = (nDimensions - 1); m >= 0; m--) {

                    if (tiindex[m] < (nArray[m] - 1)) {
                        tiindex[m]++;
                        tiflat += tistep[m];

                        break;
                    } else {
                        tiindex[m] = 0;
                        tiflat -= (nArray[m] - 1) * tistep[m];
                    }
                } // for (m = (nDimensions); m >= 0; m--)
            } // for (i = 0; i < nProduct; i++)
        } // for (k = 0; k < nDimensions; k++)

        for (i = 0; i < nDimensions; i++) {
            tiindex[i] = 0;
            tjindex[i] = 0;
        }

        // do terms with first derivatives
        for (k = 0; k < nDimensions; k++) {

            for (l = k + 1; l < nDimensions; l++) {

                for (m = 0; m < nDimensions; m++) {
                    derivative[m] = 0;
                }

                derivative[k] = 1;
                derivative[l] = 1;

                for (i = 0; i < nProduct; i++) {

                    for (j = 0; j < nProduct; j++) {
                        product = 1.0f;

                        for (m = 0; m < nDimensions; m++) {
                            product *= bendingMatrix[m][derivative[m]][tiindex[m]][tjindex[m]];
                        } // for (m = 0; m < nDimensions; m++)

                        JArray[tiflat][tjflat] += 2.0f * product;

                        for (m = nDimensions - 1; m >= 0; m--) {

                            if (tjindex[m] < (nArray[m] - 1)) {
                                tjindex[m]++;
                                tjflat += tjstep[m];

                                break;
                            } else {
                                tjindex[m] = 0;
                                tjflat -= (nArray[m] - 1) * tjstep[m];
                            }
                        } // for (m = nDimensions-1; m >= 0; m--)
                    } // for (j = 0; j < nProduct; j++)

                    for (m = nDimensions - 1; m >= 0; m--) {

                        if (tiindex[m] < (nArray[m] - 1)) {
                            tiindex[m]++;
                            tiflat += tistep[m];

                            break;
                        } else {
                            tiindex[m] = 0;
                            tiflat -= (nArray[m] - 1) * tistep[m];
                        }
                    } // for (m = nDimensions; m >= 0; m--)
                } // for (i = 0; i < nProduct; i++)
            } // for (l = k+1; l < nDimensions; l++)
        } // for (k = 0; k < nDimensions; k++)

        for (i = 0; i < nProduct; i++) {

            for (j = 0; j < nProduct; j++) {
                JArray[i][j] = (lambda * JArray[i][j]) + AtA[i][j];
            }
        }

        // solve a system of equations (J)(coef) = AtF for coef whiere J is symmetric
        // The diagonal pivoting method is used to factor J as
        // J = U * D * U**T
        // where U is a product of permutation and unit upper triangular matrices, and
        // D is symmetric and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
        // The factored form of J is then used to solved the system of equations
        // J * coef = AtF

        // Compute the factorization J = U*D*U'

        // A = new Matrix(J);
        // B = new Matrix(AtF);
        // B = new Matrix(AtF);
        final JamaMatrix AMat = new JamaMatrix(JArray);
        final JamaMatrix BMat = new JamaMatrix(AtF);
        final JamaMatrix XMat = AMat.solve(BMat);
        coef = XMat.getArrayCopy();
    }

    /**
     * IHN3Correction2(). Derived from PERL file nu_estimate_np_and_em.in Iteratively estimates intensity non-uniformity
     * artifacts in MRI areas.
     */
    private void IHN3Correction2() {
        int i, j, k;
        srcImage.calcMinMax();
        orgDim = new int[2];
        orgDim[0] = srcImage.getExtents()[0];
        orgDim[1] = srcImage.getExtents()[1];
        sliceSize = orgDim[0] * orgDim[1];

        fireProgressStateChanged(srcImage.getImageName(), "Importing source image...");

        try {
            buffer = new float[sliceSize];
            srcImage.exportData(0, sliceSize, buffer);
        } catch (final IOException ioe) {
            MipavUtil.displayError("algorithm IHN3Correction reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("AlgorithmIHN3Correction: Out of memory on field image import data");

            setCompleted(false);

            return;
        }

        try {
            mask = null;

            if (entireImage == false) {
                mask = srcImage.generateVOIMask();
            }

            newDim = new int[nDimensions];
            newDim[0] = orgDim[0];
            newDim[1] = orgDim[1];
            newSliceSize = newDim[0] * newDim[1];
            newResol = new float[nDimensions];
            newResol[0] = orgResol[0];
            newResol[1] = orgResol[1];

            if (shrink != 1.0f) {

                // find the smallest resolution
                newRes = Math.min(orgResol[0], orgResol[1]);
                newRes = shrink * newRes;
                volumeFactor = 1.0f;

                if (orgResol[0] < newRes) {
                    newResol[0] = orgResol[0] * shrink;
                    volumeFactor = volumeFactor * shrink;
                    newDim[0] = (int) Math.ceil( (orgDim[0] - 1) / shrink) + 1;
                    Sx = (newDim[0] * newResol[0]) / (orgDim[0] * orgResol[0]);
                } else {
                    Sx = 1.0;
                }

                if (orgResol[1] < newRes) {
                    newResol[1] = orgResol[1] * shrink;
                    volumeFactor = volumeFactor * shrink;
                    newDim[1] = (int) Math.ceil( (orgDim[1] - 1) / shrink) + 1;
                    Sy = (newDim[1] * newResol[1]) / (orgDim[1] * orgResol[1]);
                } else {
                    Sy = 1.0;
                }

                newSliceSize = newDim[0] * newDim[1];
                final TransMatrix xfrm = new TransMatrix(3);
                // xfrm.setZoom(Sx, Sy); xfrm.Inverse();
                xfrm.setZoom(1.0f / Sx, 1.0f / Sy);
                transformNearestNeighbor2D(buffer, xfrm);
            } // if (shrink != 1.0f)
            else {
                sBuffer = new float[sliceSize];

                for (i = 0; i < sliceSize; i++) {
                    sBuffer[i] = buffer[i];
                }

                if (mask != null) {
                    sMask = new BitSet(sliceSize);

                    for (i = 0; i < sliceSize; i++) {

                        if (mask.get(i)) {
                            sMask.set(i);
                        } else {
                            sMask.clear(i);
                        }
                    }
                } // if (mask != null)
            }

            logBuffer = new float[newSliceSize];
            sMin = Float.MAX_VALUE;
            sMax = -Float.MAX_VALUE;

            for (i = 0; i < newSliceSize; i++) {
                logBuffer[i] = sBuffer[i];

                if (logBuffer[i] < sMin) {
                    sMin = logBuffer[i];
                }

                if (logBuffer[i] > sMax) {
                    sMax = logBuffer[i];
                }
            }

            logOffset = 0.0f;

            if (sMin < 1.0f) {
                logOffset = 1.0f - sMin;

                if ( (sMax + logOffset) >= Float.MAX_VALUE) {
                    MipavUtil.displayError("Dynamic range too great for log compression");

                    setCompleted(false);

                    return;
                }

                for (i = 0; i < newSliceSize; i++) {
                    logBuffer[i] += logOffset;
                }
            }

            for (i = 0; i < newSliceSize; i++) {
                logBuffer[i] = (float) Math.log(logBuffer[i]);
            }

            // Create a new mask that meets the sMask requirements and the threshold requirements
            tMask = new BitSet(newSliceSize);

            if (autoThreshold) {

                // automatically determine threshold based on bimodal distribution
                nBins = (int) Math.ceil(sMax - sMin + 1);
                binWidth = (sMax - sMin) / (nBins - 1);
                sourceMin = sMin - (binWidth / 2.0f);
                sourceMax = sMax + (binWidth / 2.0f);
                histFactor = nBins / (sourceMax - sourceMin);
                histOffset = -histFactor * sourceMin;
                hist = new int[nBins];

                for (i = 0; i < nBins; i++) {
                    hist[i] = 0;
                }

                for (i = 0; i < newSliceSize; i++) {
                    index = (int) ( (sBuffer[i] * histFactor) + histOffset);

                    if (index == nBins) {
                        index = nBins - 1;
                    }

                    hist[index]++;
                }

                histSum = 0.0;

                for (i = 0; i < nBins; i++) {
                    histSum += hist[i];
                }

                halfBin = binWidth / 2.0;
                histMean = 0.0f;

                for (i = 0; i < nBins; i++) {
                    histMean += hist[i] * ( ( (i - histOffset) / histFactor) + halfBin);
                }

                histMean = histMean / histSum;
                zeroMoment0 = hist[0] / histSum;
                firstMoment0 = ( ( ( -histOffset) / histFactor) + halfBin) * hist[0] / histSum;

                varMax = 0.0;
                i = 0;

                for (k = 1; k < nBins; k++) {

                    // 0th and 1st cumulative moments of the histogram up to the kth level
                    zeroMoment1 = zeroMoment0 + (hist[k] / histSum);
                    firstMoment1 = firstMoment0 + ( ( ( (k - histOffset) / histFactor) + halfBin) * hist[k] / histSum);

                    if ( (zeroMoment1 > 0.0) && (zeroMoment1 < 1.0)) {
                        sqrtNum = (histMean * zeroMoment1) - firstMoment1;
                        vari = sqrtNum * sqrtNum / (zeroMoment1 * (1.0 - zeroMoment1));

                        if (vari > varMax) {
                            i = k;
                            varMax = vari;
                        }
                    }

                    zeroMoment0 = zeroMoment1;
                    firstMoment0 = firstMoment1;
                } // for (k = 1; k < nBins; k++)

                threshold = (float) ( ( (i - histOffset) / histFactor) + halfBin);
                hist = null;
            } // if (autoThreshold)

            j = 0;

            for (i = 0; i < newSliceSize; i++) {
                tMask.set(i);

                if (sBuffer[i] < threshold) {
                    tMask.clear(i);
                } else {
                    j++;
                }
            }

            if (autoThreshold && ( !useScript)) {
                MipavUtil.displayInfo("auto threshold = " + threshold + " includes " + j + " pixels excludes "
                        + (newSliceSize - j) + " pixels");
            }

            if (sMask != null) {

                for (i = 0; i < newSliceSize; i++) {

                    if ( !sMask.get(i)) {
                        tMask.clear(i);
                    }
                }
            } // if (sMask != null)

            sBuffer = null;
            sMask = null;

            // Set logBuffer to zero where the final tMask is not set
            for (i = 0; i < newSliceSize; i++) {

                if ( !tMask.get(i)) {
                    logBuffer[i] = 0.0f;
                }
            }

            // Create zero initial field estimate
            residueBuffer = new float[newSliceSize];

            for (i = 0; i < newSliceSize; i++) {
                residueBuffer[i] = 0.0f;
            }

            correctedBuffer = new float[newSliceSize];
            workingBuffer = new float[newSliceSize];
            estimateBuffer = new float[newSliceSize];
            domain = new float[nDimensions][2];
            lower = new int[nDimensions];
            upper = new int[nDimensions];
            step = new int[nDimensions];
            nArray = new int[nDimensions];
            bendingMatrix = new float[nDimensions][3][][];
            CArray = new float[ (2 * AlgorithmIHN3Correction.spline) - 1];
            sizeC = (2 * AlgorithmIHN3Correction.spline) - 1;
            DArray = new float[AlgorithmIHN3Correction.spline][AlgorithmIHN3Correction.spline];
            integral = new float[6][4];
            tiindex = new int[nDimensions];
            tistep = new int[nDimensions];
            tjindex = new int[nDimensions];
            tjstep = new int[nDimensions];
            derivative = new int[nDimensions];
            yMat = new float[binNumber];
            ys = new float[binNumber];
            fHist = new float[binNumber];
        } catch (final OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: Out of memory on field image import data");

            setCompleted(false);

            return;
        }

        padded_size = (int) (Math.pow(2, Math.ceil(Math.log(binNumber) / Math.log(2.0)) + 1) + 0.5);

        // Begin main loop: tissue intensity estimation and field estimation
        stddev = 100.0f;
        volumeDomain();

        for (iters = 0; ( (iters < maxIters) && (stddev >= endTol)) && !threadStopped; iters++) {
            fireProgressStateChanged("Starting iteration " + iters);
            fireProgressStateChanged(Math.round((float) iters / (maxIters - 1) * 100));

            // Correct volume using field estimate
            for (i = 0; i < newSliceSize; i++) {
                correctedBuffer[i] = logBuffer[i] - residueBuffer[i];
                workingBuffer[i] = correctedBuffer[i];
            }

            class_min = Float.MAX_VALUE;
            class_max = -Float.MAX_VALUE;

            for (i = 0; i < newSliceSize; i++) {

                if (tMask.get(i)) {
                    value = workingBuffer[i];

                    if (value < class_min) {
                        class_min = value;
                    } else if (value > class_max) {
                        class_max = value;
                    }
                } // if (tMask.get(i)
            } // for (i = 0; i < newSliceSize; i++)

            Arrays.fill(fHist, 0.0f);

            if (class_max <= class_min) {
                class_max = class_min + 1.0f;
            }

            binWidth = (class_max - class_min) / (binNumber - 1);
            sourceMin = class_min - (binWidth / 2.0f);
            sourceMax = class_max + (binWidth / 2.0f);

            for (i = 0; i < newSliceSize; i++) {

                if (tMask.get(i)) {
                    value = workingBuffer[i];

                    if ( (value >= class_min) && (value <= class_max)) {
                        loc = (value - sourceMin) / binWidth;

                        // iLoc = (int)(Math.floor(loc));
                        iLoc = (int) (loc);
                        fOffset = loc - iLoc - 0.5f;

                        if (fOffset == 0.0f) {
                            fHist[iLoc]++;
                        } else if ( (fOffset > 0.0f) && (iLoc <= (binNumber - 2))) {
                            fHist[iLoc] += 1.0f - fOffset;
                            fHist[iLoc + 1] += fOffset;
                        } else if (iLoc >= 1) {
                            fHist[iLoc] += 1.0f + fOffset;
                            fHist[iLoc - 1] -= fOffset;
                        }
                    } // if ((value >= class_min) && (value <= class_max))
                } // if (tMask.get(i))
            } // for (i = 0; i < newSliceSize; i++)

            // determine size of working matrices
            offset = (padded_size - binNumber) / 2;

            // create filters
            slope = (class_max - class_min) / (double) (binNumber - 1);

            Arrays.fill(blurI, 0.0f);

            // Create gaussian function
            fwhm = kernelfwhm / slope;
            fwhmFactor = 4.0 * Math.log(2.0) / (fwhm * fwhm);
            fwhmScale = 2.0 * Math.sqrt(Math.log(2.0) / Math.PI) / fwhm;
            blurR[0] = (float) fwhmScale;

            for (i = 1; i <= ( (padded_size - 1) / 2); i++) {
                blurR[i] = blurR[padded_size - i] = (float) (fwhmScale * Math.exp( -i * i * fwhmFactor));
            }

            if ( ( (padded_size - 1) / 2) != (padded_size / 2)) { // if size is even fill in middle value
                blurR[padded_size / 2] = (float) (fwhmScale * Math.exp( -padded_size * padded_size * fwhmFactor / 4.0));
            }

            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(blurR, blurI);

            // Create Weiner restoration filter in frequency domain assuming white noise model
            for (i = 0; i < padded_size; i++) {
                denom = 1 / ( (blurR[i] * blurR[i]) + (blurI[i] * blurI[i]) + noise);
                filterR[i] = blurR[i] * denom;
                filterI[i] = -blurI[i] * denom;
            }

            Arrays.fill(fHistPaddedR, 0.0f);
            Arrays.fill(fHistPaddedI, 0.0f);

            for (i = offset; i < (offset + binNumber); i++) {
                fHistPaddedR[i] = fHist[i - offset];
            }

            // Compute filtered distribution f
            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(fHistPaddedR, fHistPaddedI);

            for (i = 0; i < padded_size; i++) {
                temp = fHistPaddedR[i];
                fHistPaddedR[i] = (fHistPaddedR[i] * filterR[i]) - (fHistPaddedI[i] * filterI[i]);
                fHistPaddedI[i] = (temp * filterI[i]) + (fHistPaddedI[i] * filterR[i]);
            }

            transformDir = AlgorithmIHN3Correction.INVERSE;
            fft(fHistPaddedR, fHistPaddedI);
            Arrays.fill(fHistPaddedI, 0.0f);

            // Make negative elemnts of the real part of the inverse zero
            for (i = 0; i < padded_size; i++) {

                if (fHistPaddedR[i] < 0.0f) {
                    fHistPaddedR[i] = 0.0f;
                }
            }

            // create moment array
            for (i = 0; i < padded_size; i++) {
                momentR[i] = (float) ( (class_min + ( (i - offset) * slope)) * fHistPaddedR[i]);
                momentI[i] = 0.0f;
            }

            // compute mapping
            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(momentR, momentI);

            for (i = 0; i < padded_size; i++) {
                temp = momentR[i];
                momentR[i] = (momentR[i] * blurR[i]) - (momentI[i] * blurI[i]);
                momentI[i] = (temp * blurI[i]) + (momentI[i] * blurR[i]);
            }

            transformDir = AlgorithmIHN3Correction.INVERSE;
            fft(momentR, momentI);

            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(fHistPaddedR, fHistPaddedI);

            for (i = 0; i < padded_size; i++) {
                temp = fHistPaddedR[i];
                fHistPaddedR[i] = (fHistPaddedR[i] * blurR[i]) - (fHistPaddedI[i] * blurI[i]);
                fHistPaddedI[i] = (temp * blurI[i]) + (fHistPaddedI[i] * blurR[i]);
            }

            transformDir = AlgorithmIHN3Correction.INVERSE;
            fft(fHistPaddedR, fHistPaddedI);

            for (i = offset; i < (offset + binNumber); i++) {
                yMat[i - offset] = momentR[i] / fHistPaddedR[i];
            }

            // remove any NAN and infinities from yMat
            for (i = 0; i < binNumber; i++) {

                if ( (Float.isNaN(yMat[i])) || (yMat[i] == Float.NEGATIVE_INFINITY)
                        || (yMat[i] == Float.POSITIVE_INFINITY)) {
                    yMat[i] = 0.0f;
                }
            }

            estimateSlope = 1.0f / (binNumber - 1);

            for (i = 0; i < binNumber; i++) {
                ys[i] = i * estimateSlope;
            }

            estimateScale = 1.0f / (class_max - class_min);
            estimateOffset = -class_min * estimateScale;

            for (i = 0; i < newSliceSize; i++) {

                if (tMask.get(i)) {
                    fIndex = (workingBuffer[i] * estimateScale) + estimateOffset;

                    /* Search the table for the value */
                    start = 0;
                    length = binNumber;

                    while (length > 1) {
                        mid = start + (length / 2);
                        offset = mid;

                        if (fIndex < ys[offset]) {
                            length = mid - start;
                        } else {
                            length = start + length - mid;
                            start = mid;
                        }
                    }

                    /* Add a special check for the end of the table */
                    offset1 = 1;
                    offset2 = binNumber - 2;

                    if ( (start == 0) && (fIndex == ys[offset1])) {
                        start = 1;
                    } else if ( (start == (binNumber - 1)) && (fIndex == ys[offset2])) {
                        start = binNumber - 2;
                    }

                    /* Save the value */
                    offset = start;
                    offset1 = offset;

                    if (start < (binNumber - 1)) {
                        offset2 = offset + 1;
                    } else {
                        offset2 = offset;
                    }

                    value1 = ys[offset1];
                    value2 = ys[offset2];
                    denom = value2 - value1;

                    if (denom != 0.0f) {
                        frac = (fIndex - value1) / denom;
                    } else {
                        frac = 0.0f;
                    }

                    if (frac < 0.0f) {
                        frac = 0.0f;
                    }

                    if (frac > 1.0f) {
                        frac = 1.0f;
                    }

                    rfrac = 1.0f - frac;
                    estimateBuffer[i] = (rfrac * yMat[offset1]) + (frac * yMat[offset2]);
                } // if (tMask.get(i))
                else {
                    estimateBuffer[i] = 0.0f;
                }
            } // for (i = 0; i < newSliceSize; i++)

            for (i = 0; i < newSliceSize; i++) {
                workingBuffer[i] = logBuffer[i] - estimateBuffer[i];
                estimateBuffer[i] = residueBuffer[i]; // note old residue is saved for checking stopping condition
            }

            // filter residue volume
            lambda = spline_lambda / volumeFactor;

            // compensate for N dependence in lambda
            lambda = lambda / (spline_subsample * spline_subsample);

            TBSplineVolume();
            fitSplinesToVolumeLookup2D();
            smoothVolumeLookup2D();

            for (i = 0; i < newSliceSize; i++) {
                residueBuffer[i] = workingBuffer[i];
                workingBuffer[i] = estimateBuffer[i] - residueBuffer[i];
            }

            // check stopping condition
            numVoxels = 0;
            sumValue = 0.0f;
            sum2Value = 0.0f;

            for (i = 0; i < newSliceSize; i++) {

                if (tMask.get(i)) {
                    numVoxels++;
                    sumValue += workingBuffer[i];
                    sum2Value += workingBuffer[i] * workingBuffer[i];
                } // if (tMask.get(i))
            } // for (i = 0; i < newSliceSize; i++)

            mean = sumValue / numVoxels;
            stddev = (float) Math.sqrt( (sum2Value / numVoxels) - (mean * mean));

        } // for (iters = 0; ((iters < maxIters) && (stddev >= endTol)) && !threadStopped; iters++)

        if (threadStopped) {
            Preferences.debug("Stopped after iteration " + (iters - 1) + " due to cancellation\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        fireProgressStateChanged("Last iteration was = " + (iters - 1));
        logBuffer = null;
        correctedBuffer = null;
        estimateBuffer = null;

        for (i = 0; i < newSliceSize; i++) {
            workingBuffer[i] = (float) Math.exp(residueBuffer[i]);
            // Don't subtract logOffset because we wish to normalize to a mean of 1.0
        }

        residueBuffer = null;

        numVoxels = 0;
        sumValue = 0.0f;

        for (i = 0; i < newSliceSize; i++) {

            if (tMask.get(i)) {
                numVoxels++;
                sumValue += workingBuffer[i];
            }
        }

        mean = sumValue / numVoxels;

        if (mean != 0.0f) {

            for (i = 0; i < newSliceSize; i++) {
                workingBuffer[i] /= mean;
            }
        } else {
            cleanUp();
            System.gc();
            MipavUtil.displayError("algorithm IHN3Correction could not normalize with zero mean");

            setCompleted(false);

            return;
        }

        fieldBuffer = new float[sliceSize];

        if (shrink == 1.0f) {

            for (i = 0; i < sliceSize; i++) {
                fieldBuffer[i] = workingBuffer[i];
            }
        } else {
            Sx = (orgDim[0] * orgResol[0]) / (newDim[0] * newResol[0]);
            Sy = (orgDim[1] * orgResol[1]) / (newDim[1] * newResol[1]);
            final TransMatrix xfrm = new TransMatrix(3);
            // xfrm.setZoom(Sx, Sy); xfrm.Inverse();
            xfrm.setZoom(1.0f / Sx, 1.0f / Sy);
            transformBilinear(workingBuffer, xfrm);
        } // else shrink != 1.0f

        workingBuffer = null;

        // put lower bound on field intensity
        for (i = 0; i < sliceSize; i++) {

            if (fieldBuffer[i] < field_floor) {
                fieldBuffer[i] = field_floor;
            }
        }

        try {

            if (fieldImage != null) {
                fieldImage.importData(0, fieldBuffer, true);
            }

            for (i = 0; i < sliceSize; i++) {

                // Offset to be all nonnegative before field normalization
                if (sMin >= 0.0f) {
                    fieldBuffer[i] = buffer[i] / fieldBuffer[i];
                } else {

                    // put the new minimum at 0 for normalization
                    fieldBuffer[i] = (buffer[i] + (logOffset - 1)) / fieldBuffer[i];
                }
            }

            destImage.importData(0, fieldBuffer, true);
        } catch (final IOException error) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: IOException on field image import data");

            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: Out of memory on field image import data");

            setCompleted(false);

            return;
        }

        setCompleted(true);

        return;
    }

    /**
     * IHN3Correction3. Derived from PERL file nu_estimate_np_and_em.in Iteratively estimates intensity non-uniformity
     * artifacts in MRI volumes.
     */
    private void IHN3Correction3() {
        int i, j, k;
        srcImage.calcMinMax();
        orgDim = new int[3];
        orgDim[0] = srcImage.getExtents()[0];
        orgDim[1] = srcImage.getExtents()[1];
        orgDim[2] = srcImage.getExtents()[2];
        sliceSize = orgDim[0] * orgDim[1];
        volSize = sliceSize * orgDim[2];

        fireProgressStateChanged(srcImage.getImageName(), "Importing source image...");

        try {
            buffer = new float[volSize];
            srcImage.exportData(0, volSize, buffer);
        } catch (final IOException ioe) {
            cleanUp();
            System.gc();
            MipavUtil.displayError("algorithm IHN3Correction reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: Out of memory on field image import data");

            setCompleted(false);

            return;
        }

        try {
            mask = null;

            if (entireImage == false) {
                mask = srcImage.generateVOIMask();
            }

            newDim = new int[nDimensions];
            newDim[0] = orgDim[0];
            newDim[1] = orgDim[1];
            newDim[2] = orgDim[2];
            newSliceSize = newDim[0] * newDim[1];
            newVolSize = newSliceSize * newDim[2];
            newResol = new float[nDimensions];
            newResol[0] = orgResol[0];
            newResol[1] = orgResol[1];
            newResol[2] = orgResol[2];

            if (shrink != 1.0f) {

                // find the smallest resolution
                newRes = Math.min(orgResol[0], orgResol[1]);
                newRes = Math.min(orgResol[2], newRes);
                newRes = shrink * newRes;
                volumeFactor = 1.0f;

                if (orgResol[0] < newRes) {
                    newResol[0] = orgResol[0] * shrink;
                    volumeFactor = volumeFactor * shrink;
                    newDim[0] = (int) Math.ceil( (orgDim[0] - 1) / shrink) + 1;
                    Sx = (newDim[0] * newResol[0]) / (orgDim[0] * orgResol[0]);
                } else {
                    Sx = 1.0;
                }

                if (orgResol[1] < newRes) {
                    newResol[1] = orgResol[1] * shrink;
                    volumeFactor = volumeFactor * shrink;
                    newDim[1] = (int) Math.ceil( (orgDim[1] - 1) / shrink) + 1;
                    Sy = (newDim[1] * newResol[1]) / (orgDim[1] * orgResol[1]);
                } else {
                    Sy = 1.0;
                }

                if (orgResol[2] < newRes) {
                    newResol[2] = orgResol[2] * shrink;
                    volumeFactor = volumeFactor * shrink;
                    newDim[2] = (int) Math.ceil( (orgDim[2] - 1) / shrink) + 1;
                    Sz = (newDim[2] * newResol[2]) / (orgDim[2] * orgResol[2]);
                } else {
                    Sz = 1.0;
                }

                newSliceSize = newDim[0] * newDim[1];
                newVolSize = newSliceSize * newDim[2];
                final TransMatrix xfrm = new TransMatrix(4);
                // xfrm.setZoom(Sx, Sy, Sz); xfrm.Inverse();
                xfrm.setZoom(1.0f / Sx, 1.0f / Sy, 1.0f / Sz);
                transformNearestNeighbor3D(buffer, xfrm);
            } // if (shrink != 1.0f)
            else {
                sBuffer = new float[volSize];

                for (i = 0; i < volSize; i++) {
                    sBuffer[i] = buffer[i];
                }

                if (mask != null) {
                    sMask = new BitSet(volSize);

                    for (i = 0; i < volSize; i++) {

                        if (mask.get(i)) {
                            sMask.set(i);
                        } else {
                            sMask.clear(i);
                        }
                    }
                } // if (mask != null)
            }

            logBuffer = new float[newVolSize];
            sMin = Float.MAX_VALUE;
            sMax = -Float.MAX_VALUE;

            for (i = 0; i < newVolSize; i++) {
                logBuffer[i] = sBuffer[i];

                if (logBuffer[i] < sMin) {
                    sMin = logBuffer[i];
                }

                if (logBuffer[i] > sMax) {
                    sMax = logBuffer[i];
                }
            }

            logOffset = 0.0f;

            if (sMin < 1.0f) {
                logOffset = 1.0f - sMin;

                if ( (sMax + logOffset) >= Float.MAX_VALUE) {
                    MipavUtil.displayError("Dynamic range too great for log compression");

                    setCompleted(false);

                    return;
                }

                for (i = 0; i < newVolSize; i++) {
                    logBuffer[i] += logOffset;
                }
            }

            for (i = 0; i < newVolSize; i++) {
                logBuffer[i] = (float) Math.log(logBuffer[i]);
            }

            // Create a new mask that meets the sMask requirements and the threshold requirements
            tMask = new BitSet(newVolSize);

            if (autoThreshold) {

                // automatically determine threshold based on bimodal distribution
                nBins = (int) Math.ceil(sMax - sMin + 1);
                binWidth = (sMax - sMin) / (nBins - 1);
                sourceMin = sMin - (binWidth / 2.0f);
                sourceMax = sMax + (binWidth / 2.0f);
                histFactor = nBins / (sourceMax - sourceMin);
                histOffset = -histFactor * sourceMin;
                hist = new int[nBins];

                for (i = 0; i < nBins; i++) {
                    hist[i] = 0;
                }

                for (i = 0; i < newVolSize; i++) {
                    index = (int) ( (sBuffer[i] * histFactor) + histOffset);

                    if (index == nBins) {
                        index = nBins - 1;
                    }

                    hist[index]++;
                }

                histSum = 0.0;

                for (i = 0; i < nBins; i++) {
                    histSum += hist[i];
                }

                halfBin = binWidth / 2.0;
                histMean = 0.0f;

                for (i = 0; i < nBins; i++) {
                    histMean += hist[i] * ( ( (i - histOffset) / histFactor) + halfBin);
                }

                histMean = histMean / histSum;
                zeroMoment0 = hist[0] / histSum;
                firstMoment0 = ( ( ( -histOffset) / histFactor) + halfBin) * hist[0] / histSum;

                varMax = 0.0;
                i = 0;

                for (k = 1; k < nBins; k++) {

                    // 0th and 1st cumulative moments of the histogram up to the kth level
                    zeroMoment1 = zeroMoment0 + (hist[k] / histSum);
                    firstMoment1 = firstMoment0 + ( ( ( (k - histOffset) / histFactor) + halfBin) * hist[k] / histSum);

                    if ( (zeroMoment1 > 0.0) && (zeroMoment1 < 1.0)) {
                        sqrtNum = (histMean * zeroMoment1) - firstMoment1;
                        vari = sqrtNum * sqrtNum / (zeroMoment1 * (1.0 - zeroMoment1));

                        if (vari > varMax) {
                            i = k;
                            varMax = vari;
                        }
                    }

                    zeroMoment0 = zeroMoment1;
                    firstMoment0 = firstMoment1;
                } // for (k = 1; k < nBins; k++)

                threshold = (float) ( ( (i - histOffset) / histFactor) + halfBin);
                hist = null;
            } // if (autoThreshold)

            j = 0;

            for (i = 0; i < newVolSize; i++) {
                tMask.set(i);

                if (sBuffer[i] < threshold) {
                    tMask.clear(i);
                } else {
                    j++;
                }
            }

            if (autoThreshold && ( !useScript)) {
                MipavUtil.displayInfo("auto threshold = " + threshold + " includes " + j + " voxels excludes "
                        + (newVolSize - j) + " voxels");
            }

            if (sMask != null) {

                for (i = 0; i < newVolSize; i++) {

                    if ( !sMask.get(i)) {
                        tMask.clear(i);
                    }
                }
            } // if (sMask != null)

            sBuffer = null;
            sMask = null;

            // Set logBuffer to zero where the final tMask is not set
            for (i = 0; i < newVolSize; i++) {

                if ( !tMask.get(i)) {
                    logBuffer[i] = 0.0f;
                }
            }

            // Create zero initial field estimate
            residueBuffer = new float[newVolSize];

            // for (i = 0; i < newVolSize; i++) {
            // residueBuffer[i] = 0.0f;
            // }
            correctedBuffer = new float[newVolSize];
            workingBuffer = new float[newVolSize];
            estimateBuffer = new float[newVolSize];
            domain = new float[nDimensions][2];
            lower = new int[nDimensions];
            upper = new int[nDimensions];
            nArray = new int[nDimensions];
            bendingMatrix = new float[nDimensions][3][][];
            CArray = new float[ (2 * AlgorithmIHN3Correction.spline) - 1];
            sizeC = (2 * AlgorithmIHN3Correction.spline) - 1;
            DArray = new float[AlgorithmIHN3Correction.spline][AlgorithmIHN3Correction.spline];
            integral = new float[6][4];
            tiindex = new int[nDimensions];
            tistep = new int[nDimensions];
            tjindex = new int[nDimensions];
            tjstep = new int[nDimensions];
            derivative = new int[nDimensions];
            fHist = new float[binNumber];
            yMat = new float[binNumber];
            ys = new float[binNumber];

        } catch (final OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: Out of memory on field image import data");

            setCompleted(false);

            return;
        }

        // determine size of working matrices
        padded_size = (int) (Math.pow(2, Math.ceil(Math.log(binNumber) / Math.log(2.0)) + 1) + 0.5);

        // Begin main loop: tissue intensity estimation and field estimation
        stddev = 100.0f;
        volumeDomain();

        for (iters = 0; ( (iters < maxIters) && (stddev >= endTol)) && !threadStopped; iters++) {
            Preferences.debug("Starting iteration " + iters + ": Tol = " + stddev + "\n", Preferences.DEBUG_ALGORITHM);

            fireProgressStateChanged("Starting iteration " + iters + ": Tol = " + stddev);
            fireProgressStateChanged(Math.round((float) iters / (maxIters - 1) * 100));

            // Correct volume using field estimate
            for (i = 0; i < newVolSize; i++) {
                correctedBuffer[i] = logBuffer[i] - residueBuffer[i];
                workingBuffer[i] = correctedBuffer[i];
            }

            class_min = Float.MAX_VALUE;
            class_max = -Float.MAX_VALUE;

            for (i = 0; i < newVolSize; i++) {

                if (tMask.get(i)) {
                    value = workingBuffer[i];

                    if (value < class_min) {
                        class_min = value;
                    } else if (value > class_max) {
                        class_max = value;
                    }
                } // if (tMask.get(i)
            } // for (i = 0; i < newVolSize; i++)

            Arrays.fill(fHist, 0.0f);

            if (class_max <= class_min) {
                class_max = class_min + 1.0f;
            }

            binWidth = (class_max - class_min) / (binNumber - 1);
            sourceMin = class_min - (binWidth / 2.0f);
            sourceMax = class_max + (binWidth / 2.0f);

            for (i = 0; i < newVolSize; i++) {

                if (tMask.get(i)) {
                    value = workingBuffer[i];

                    if ( (value >= class_min) && (value <= class_max)) {
                        loc = (value - sourceMin) / binWidth;

                        // iLoc = (int)(Math.floor(loc));
                        iLoc = (int) (loc);
                        fOffset = loc - iLoc - 0.5f;

                        if (fOffset == 0.0f) {
                            fHist[iLoc]++;
                        } else if ( (fOffset > 0.0f) && (iLoc <= (binNumber - 2))) {
                            fHist[iLoc] += 1.0f - fOffset;
                            fHist[iLoc + 1] += fOffset;
                        } else if (iLoc >= 1) {
                            fHist[iLoc] += 1.0f + fOffset;
                            fHist[iLoc - 1] -= fOffset;
                        }
                    } // if ((value >= class_min) && (value <= class_max))
                } // if (tMask.get(i))
            } // for (i = 0; i < newVolSize; i++)

            offset = (padded_size - binNumber) / 2;

            // create filters
            slope = (class_max - class_min) / (double) (binNumber - 1);
            Arrays.fill(blurI, 0.0f);

            // Create gaussian function
            fwhm = kernelfwhm / slope;
            fwhmFactor = 4.0 * Math.log(2.0) / (fwhm * fwhm);
            fwhmScale = 2.0 * Math.sqrt(Math.log(2.0) / Math.PI) / fwhm;
            blurR[0] = (float) fwhmScale;

            for (i = 1; i <= ( (padded_size - 1) / 2); i++) {
                blurR[i] = blurR[padded_size - i] = (float) (fwhmScale * Math.exp( -i * i * fwhmFactor));
            }

            if ( ( (padded_size - 1) / 2) != (padded_size / 2)) { // if size is even fill in middle value
                blurR[padded_size / 2] = (float) (fwhmScale * Math.exp( -padded_size * padded_size * fwhmFactor / 4.0));
            }

            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(blurR, blurI);

            // Create Weiner restoration filter in frequency domain assuming white noise model
            for (i = 0; i < padded_size; i++) {
                denom = 1 / ( (blurR[i] * blurR[i]) + (blurI[i] * blurI[i]) + noise);
                filterR[i] = blurR[i] * denom;
                filterI[i] = -blurI[i] * denom;
            }

            Arrays.fill(fHistPaddedR, 0.0f);
            Arrays.fill(fHistPaddedI, 0.0f);

            for (i = offset; i < (offset + binNumber); i++) {
                fHistPaddedR[i] = fHist[i - offset];
            }

            // Compute filtered distribution f
            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(fHistPaddedR, fHistPaddedI);

            for (i = 0; i < padded_size; i++) {
                temp = fHistPaddedR[i];
                fHistPaddedR[i] = (fHistPaddedR[i] * filterR[i]) - (fHistPaddedI[i] * filterI[i]);
                fHistPaddedI[i] = (temp * filterI[i]) + (fHistPaddedI[i] * filterR[i]);
            }

            transformDir = AlgorithmIHN3Correction.INVERSE;
            fft(fHistPaddedR, fHistPaddedI);

            for (i = 0; i < padded_size; i++) {
                fHistPaddedI[i] = 0.0f;

                if (fHistPaddedR[i] < 0.0f) { // Make negative elemnts of the real part of the inverse zero
                    fHistPaddedR[i] = 0.0f;
                }
            }

            for (i = 0; i < padded_size; i++) {
                momentR[i] = (float) ( (class_min + ( (i - offset) * slope)) * fHistPaddedR[i]);
                momentI[i] = 0.0f;
            }

            // compute mapping
            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(momentR, momentI);

            for (i = 0; i < padded_size; i++) {
                temp = momentR[i];
                momentR[i] = (momentR[i] * blurR[i]) - (momentI[i] * blurI[i]);
                momentI[i] = (temp * blurI[i]) + (momentI[i] * blurR[i]);
            }

            transformDir = AlgorithmIHN3Correction.INVERSE;
            fft(momentR, momentI);
            transformDir = AlgorithmIHN3Correction.FORWARD;
            fft(fHistPaddedR, fHistPaddedI);

            for (i = 0; i < padded_size; i++) {
                temp = fHistPaddedR[i];
                fHistPaddedR[i] = (fHistPaddedR[i] * blurR[i]) - (fHistPaddedI[i] * blurI[i]);
                fHistPaddedI[i] = (temp * blurI[i]) + (fHistPaddedI[i] * blurR[i]);
            }

            transformDir = AlgorithmIHN3Correction.INVERSE;
            fft(fHistPaddedR, fHistPaddedI);

            for (i = offset; i < (offset + binNumber); i++) {
                yMat[i - offset] = momentR[i] / fHistPaddedR[i];
            }

            // remove any NAN and infinities from yMat
            for (i = 0; i < binNumber; i++) {

                if ( (Float.isNaN(yMat[i])) || (yMat[i] == Float.NEGATIVE_INFINITY)
                        || (yMat[i] == Float.POSITIVE_INFINITY)) {
                    yMat[i] = 0.0f;
                }
            }

            estimateSlope = 1.0f / (binNumber - 1);

            for (i = 0; i < binNumber; i++) {
                ys[i] = i * estimateSlope;
            }

            estimateScale = 1.0f / (class_max - class_min);
            estimateOffset = -class_min * estimateScale;

            for (i = 0; (i < newVolSize) && !threadStopped; i++) {

                if (tMask.get(i)) {
                    fIndex = (workingBuffer[i] * estimateScale) + estimateOffset;

                    /* Search the table for the value */
                    start = 0;
                    length = binNumber;

                    while (length > 1) {
                        mid = start + (length / 2);
                        offset = mid;

                        if (fIndex < ys[offset]) {
                            length = mid - start;
                        } else {
                            length = start + length - mid;
                            start = mid;
                        }
                    }

                    /* Add a special check for the end of the table */
                    offset1 = 1;
                    offset2 = binNumber - 2;

                    if ( (start == 0) && (fIndex == ys[offset1])) {
                        start = 1;
                    } else if ( (start == (binNumber - 1)) && (fIndex == ys[offset2])) {
                        start = binNumber - 2;
                    }

                    /* Save the value */
                    offset = start;
                    offset1 = offset;

                    if (start < (binNumber - 1)) {
                        offset2 = offset + 1;
                    } else {
                        offset2 = offset;
                    }

                    value1 = ys[offset1];
                    value2 = ys[offset2];
                    denom = value2 - value1;

                    if (denom != 0.0f) {
                        frac = (fIndex - value1) / denom;
                    } else {
                        frac = 0.0f;
                    }

                    if (frac < 0.0f) {
                        frac = 0.0f;
                    }

                    if (frac > 1.0f) {
                        frac = 1.0f;
                    }

                    rfrac = 1.0f - frac;
                    estimateBuffer[i] = (rfrac * yMat[offset1]) + (frac * yMat[offset2]);
                } // if (tMask.get(i))
                else {
                    estimateBuffer[i] = 0.0f;
                }
            } // for (i = 0; i < newVolSize; i++)

            if (threadStopped) {
                break;
            }

            for (i = 0; i < newVolSize; i++) {
                workingBuffer[i] = logBuffer[i] - estimateBuffer[i];
                estimateBuffer[i] = residueBuffer[i];
            }

            // filter residue volume
            lambda = spline_lambda / volumeFactor;

            // compensate for N dependence in lambda
            lambda = lambda / (spline_subsample * spline_subsample * spline_subsample);

            // volumeDomain();
            TBSplineVolume();

            fitSplinesToVolumeLookup3D();

            if (threadStopped) {
                break;
            }

            smoothVolumeLookup3D();

            if (threadStopped) {
                break;
            }

            for (i = 0; i < newVolSize; i++) {
                residueBuffer[i] = workingBuffer[i];
                workingBuffer[i] = estimateBuffer[i] - residueBuffer[i];
            }

            // check stopping condition
            numVoxels = 0;
            sumValue = 0.0f;
            sum2Value = 0.0f;

            for (i = 0; i < newVolSize; i++) {

                if (tMask.get(i)) {
                    numVoxels++;
                    sumValue += workingBuffer[i];
                    sum2Value += workingBuffer[i] * workingBuffer[i];
                } // if (tMask.get(i))
            } // for (i = 0; i < newVolSize; i++)

            mean = sumValue / numVoxels;
            stddev = (float) Math.sqrt( (sum2Value / numVoxels) - (mean * mean));
        } // for (iters = 0; ((iters < maxIters) && (stddev >= endTol)) && !threadStopped; iters++)

        if (threadStopped) {
            Preferences.debug("Stopped during iteration " + (iters - 1) + " due to cancellation\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        fireProgressStateChanged("Last iteration was = " + (iters - 1));

        logBuffer = null;
        correctedBuffer = null;
        estimateBuffer = null;
        System.gc();

        for (i = 0; i < newVolSize; i++) {
            workingBuffer[i] = (float) Math.exp(residueBuffer[i]);
            // Don't subtract logOffset because we wish to normalize to a mean of 1.0.
        }

        residueBuffer = null;

        numVoxels = 0;
        sumValue = 0.0f;

        for (i = 0; i < newVolSize; i++) {

            if (tMask.get(i)) {
                numVoxels++;
                sumValue += workingBuffer[i];
            }
        }

        mean = sumValue / numVoxels;

        if (mean != 0.0f) {

            for (i = 0; i < newVolSize; i++) {
                workingBuffer[i] /= mean;
            }
        } else {
            MipavUtil.displayError("Algorithm IHN3Correction could not normalize with zero mean");

            setCompleted(false);

            return;
        }

        fieldBuffer = new float[volSize];

        if (shrink == 1.0f) {

            for (i = 0; i < volSize; i++) {
                fieldBuffer[i] = workingBuffer[i];
            }
        } else {
            Sx = (orgDim[0] * orgResol[0]) / (newDim[0] * newResol[0]);
            Sy = (orgDim[1] * orgResol[1]) / (newDim[1] * newResol[1]);
            Sz = (orgDim[2] * orgResol[2]) / (newDim[2] * newResol[2]);
            final TransMatrix xfrm = new TransMatrix(4);
            // xfrm.setZoom(Sx, Sy, Sz); xfrm.Inverse();
            xfrm.setZoom(1.0f / Sx, 1.0f / Sy, 1.0f / Sz);
            transformTrilinear(workingBuffer, xfrm);
        } // else shrink != 1.0f

        workingBuffer = null;

        // put lower bound on field intensity
        for (i = 0; i < volSize; i++) {

            if (fieldBuffer[i] < field_floor) {
                fieldBuffer[i] = field_floor;
            }
        }

        try {

            if (fieldImage != null) {
                fieldImage.importData(0, fieldBuffer, true);
            }

            for (i = 0; i < volSize; i++) {

                // Offset to be all nonnegative before field normalization
                if (sMin >= 0.0f) {
                    fieldBuffer[i] = buffer[i] / fieldBuffer[i];
                } else {

                    // put the new minimum at 0 for normalization
                    fieldBuffer[i] = (buffer[i] + (logOffset - 1)) / fieldBuffer[i];
                }
            }

            destImage.importData(0, fieldBuffer, true);

        } catch (final IOException error) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: IOException on field image import data");

            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: Out of memory on field image import data");

            setCompleted(false);

            return;
        }

        setCompleted(true);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void smoothVolumeLookup2D() {

        // compute spline function at every point within mask volume,
        // other values are set to zero
        int fourX, fourY;
        int x, y, i, j;

        for (x = 0; x < newDim[0]; x++) {
            fourX = 4 * x;

            for (y = 0; y < newDim[1]; y++) {
                fourY = 4 * y;
                index = x + (y * newDim[0]);

                if (tMask.get(index)) {

                    // compute 4 by 4 tensor and inner product with coefficients
                    value = 0.0f;

                    for (i = fourX; i < (fourX + 4); i++) {
                        xv = d1Spline[0][i];
                        xo = offsetSp[0][i];

                        for (j = fourY; j < (fourY + 4); j++) {
                            value += xv * d1Spline[1][j] * coef[xo + offsetSp[1][j]][0];
                        } // for (j = 0; j < 4; j++)
                    } // for (i = 0; i < 4; i++)

                    workingBuffer[index] = value;
                } // if (tMask.get(index))
                else {
                    workingBuffer[index] = 0.0f;
                }
            } // for (y = 0; y < newDim[1]; y++)
        } // for (x = 0; x < newDim[0]; x++)
    }

    /**
     * DOCUMENT ME!
     */
    private void smoothVolumeLookup3D() {

        // compute spline function at every point within mask volume,
        // other values are set to zero
        int fourX, fourY, fourZ;
        int x, y, z, i, j, k;

        for (x = 0; x < newDim[0]; x++) {
            fourX = 4 * x;

            for (y = 0; y < newDim[1]; y++) {
                fourY = 4 * y;
                index1 = x + (y * newDim[0]);

                for (z = 0; z < newDim[2]; z++) {
                    fourZ = 4 * z;
                    index = index1 + (z * newSliceSize);

                    if (tMask.get(index)) {

                        // compute 4 by 4 by 4 tensor and inner product with coefficients
                        value = 0.0f;

                        for (i = fourX; i < (fourX + 4); i++) {
                            xv = d1Spline[0][i];
                            xo = offsetSp[0][i];

                            for (j = fourY; j < (fourY + 4); j++) {
                                xyv = xv * d1Spline[1][j];
                                xyo = xo + offsetSp[1][j];

                                for (k = fourZ; k < (fourZ + 4); k++) {
                                    value += xyv * d1Spline[2][k] * coef[xyo + offsetSp[2][k]][0];
                                } // for (k = 0; k < 4; k++)
                            } // for (j = 0; j < 4; j++)
                        } // for (i = 0; i < 4; i++)

                        workingBuffer[index] = value;
                    } // if (tMask.get(index))
                    else {
                        workingBuffer[index] = 0.0f;
                    }
                } // for (z = 0; z < newDim[2]; z++)
            } // for (y = 0; y < newDim[1]; y++)
        } // for (x = 0; x < newDim[0]; x++)

    }

    /**
     * DOCUMENT ME!
     */
    private void TBSplineVolume() {
        int i, j, k;

        // remove all data points from fit
        for (i = 0; i < nProduct; i++) {
            AtF[i][0] = 0.0;

            for (j = 0; j < nProduct; j++) {
                AtA[i][j] = 0.0f;
            }
        }

        // evaluate 1D B splines at voxel centers
        for (i = 0; i < nDimensions; i++) {
            pSpline = 0;
            pOffset = 0;
            offsetStep = 1;

            for (j = 0; j < (nDimensions - i - 1); j++) {
                offsetStep *= nArray[nDimensions - j - 1];
            }

            // skip any voxel outside the domain
            first = (domain[i][0] > 0.0f) ? (int) Math.ceil(domain[i][0] / newResol[i]) : 0;

            // compute index of last voxel within domain plus one
            last = (domain[i][1] < ( (newDim[i] - 1) * newResol[i])) ? ((int) Math.floor(domain[i][1] / newResol[i]) + 1)
                    : newDim[i];

            // fill outside domain with zeros
            for (k = 0; k < (first * 4); k++) {
                d1Spline[i][pSpline++] = 0.0f;
                offsetSp[i][pOffset++] = 0;
            }

            xf = first * newResol[i];

            for (j = first; j < last; j++, xf += newResol[i]) {

                // locate nearest knot location greater than or equal to point
                blockInt = (int) Math.ceil( (xf - zero[i]) / fieldDistance) - 1;

                if (blockInt < 0) {
                    blockInt = 0;
                } else if (blockInt > (nArray[i] - 4)) {
                    blockInt = nArray[i] - 4;
                }

                // set offset into coef array
                currentOffset = blockInt * offsetStep;
                offsetSp[i][pOffset++] = currentOffset;

                for (k = 3; k > 0; k--) {
                    currentOffset += offsetStep;
                    offsetSp[i][pOffset++] = currentOffset;
                }

                // compute terms used in tensor product
                temp2 = knots[blockInt + 4][i] - xf;
                temp = scale * temp2 * temp2 * temp2;
                d1Spline[i][pSpline++] = temp;

                temp2 = knots[blockInt + 5][i] - xf;
                d1Spline[i][pSpline++] = (scale * temp2 * temp2 * temp2) - (4.0f * temp);

                temp2 = xf - knots[blockInt + 3][i];
                temp = scale * temp2 * temp2 * temp2;
                temp2 = xf - knots[blockInt + 2][i];
                d1Spline[i][pSpline++] = (scale * temp2 * temp2 * temp2) - (4.0f * temp);
                d1Spline[i][pSpline++] = temp;
            } // for (j = first; j < last; j++, xf += newResol[i])

            // fill outside domain with zeros
            for (k = (last * 4); k < (newDim[i] * 4); k++) {
                d1Spline[i][pSpline++] = 0.0f;
                offsetSp[i][pOffset++] = 0;
            }
        } // for (i = 0; i < nDimensions; i++)
    }

    /**
     * Transforms and resamples volume using bilinear interpolation.
     * 
     * @param imgBuf - image array
     * @param xfrm - TransMatrix to be applied
     */
    private void transformBilinear(final float[] imgBuf, final TransMatrix xfrm) {
        int i, j;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float value;
        float imm, jmm;
        float temp1, temp2;
        int roundX, roundY;

        // int length = orgDim[0]*orgDim[1];
        // int mod = orgDim[0]/50;
        // int counter = 0; //used for progress bar
        float T00, T01, T02, T10, T11, T12;
        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);

        for (i = 0; i < orgDim[0]; i++) {

            // if ( isProgressBarVisible()&& i%mod ==0) {
            // progressBar.setValue((int)((float)i/oXdim * 100+0.5));
            // }
            imm = i * orgResol[0];
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; j < orgDim[1]; j++) {

                // transform i,j
                value = 0; // remains zero if voxel is transformed out of bounds
                jmm = j * orgResol[1];
                X = (temp1 + (jmm * T01)) / newResol[0];
                roundX = (int) (X + 0.5f);

                if ( (X >= 0) && (roundX < newDim[0])) {
                    Y = (temp2 + (jmm * T11)) / newResol[1];
                    roundY = (int) (Y + 0.5f);

                    if ( (Y >= 0) && (roundY < newDim[1])) {

                        if ( (roundX == (newDim[0] - 1)) || (roundY == (newDim[1] - 1))) { // cannot interpolate on
                            // last
                            // X or Y
                            X0pos = roundX;
                            Y0pos = roundY * newDim[0];
                            value = imgBuf[Y0pos + X0pos];
                        } else {

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            x0 = X - (int) X;
                            y0 = Y - (int) Y;
                            x1 = 1 - x0;
                            y1 = 1 - y0;
                            X0pos = (int) X;
                            Y0pos = (int) Y * newDim[0];
                            X1pos = X0pos + 1;
                            Y1pos = Y0pos + newDim[0];
                            value = (x1 * y1 * imgBuf[Y0pos + X0pos]) + (x0 * y1 * imgBuf[Y0pos + X1pos])
                                    + (x1 * y0 * imgBuf[Y1pos + X0pos]) + (x0 * y0 * imgBuf[Y1pos + X1pos]);
                        }
                    } // end if Y in bounds
                } // end if X in bounds

                fieldBuffer[i + (j * orgDim[0])] = value;
                // counter++;
            } // end for j
        } // end for i
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     * 
     * @param imgBuf image array
     * @param xfrm transformation matrix to be applied
     */
    private void transformNearestNeighbor2D(final float[] imgBuf, final TransMatrix xfrm) {
        int i, j;
        float X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        float imm, jmm;

        // int mod = newDim[0]/50;
        // int counter = 0; //used for progress bar
        float T00, T01, T02, T10, T11, T12;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);

        sBuffer = new float[newSliceSize];

        if (mask != null) {
            sMask = new BitSet(newSliceSize);

            for (i = 0; i < newSliceSize; i++) {
                sMask.clear(i);
            }
        }

        for (i = 0; i < newDim[0]; i++) {

            // if (isProgressBarVisible()&& i%mod==0) {
            // progressBar.setValue((int)((float)i/oXdim * 100+0.5));
            // }
            for (j = 0; j < newDim[1]; j++) {

                // transform i,j
                imm = i * newResol[0];
                jmm = j * newResol[1];
                X = (imm * T00) + (jmm * T01) + T02;
                Y = (imm * T10) + (jmm * T11) + T12;

                // set intensity of i,j,k to new transformed coordinate if
                // x,y,z is w/in dimensions of image
                X = X / orgResol[0];
                Y = Y / orgResol[1];

                roundX = (int) (X + 0.5f);
                roundY = (int) (Y + 0.5f);

                if ( (roundX < 0) || (roundX >= (orgDim[0] - 1)) || (roundY < 0) || (roundY >= (orgDim[1] - 1))) {
                    sBuffer[i + (j * newDim[0])] = 0;
                } else {
                    xOffset = roundX;
                    yOffset = roundY * orgDim[0];
                    sBuffer[i + (j * newDim[0])] = imgBuf[xOffset + yOffset];

                    if ( (mask != null) && (mask.get(xOffset + yOffset))) {
                        sMask.set(i + (j * newDim[0]));
                    }
                }
                // counter++;
            }
        }

    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     * 
     * @param imgBuf image array
     * @param xfrm transformation matrix to be applied
     */
    private void transformNearestNeighbor3D(final float[] imgBuf, final TransMatrix xfrm) {
        int i, j, k;
        float X, Y, Z;
        int xOffset, yOffset, zOffset;
        int roundX, roundY, roundZ;
        float imm, jmm, kmm;

        // int mod = newDim[0]/50;
        // int counter = 0; //used for progress bar
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T03 = xfrm.get(0, 3);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);
        T13 = xfrm.get(1, 3);
        T20 = xfrm.get(2, 0);
        T21 = xfrm.get(2, 1);
        T22 = xfrm.get(2, 2);
        T23 = xfrm.get(2, 3);

        sBuffer = new float[newVolSize];

        if (mask != null) {
            sMask = new BitSet(newVolSize);

            for (i = 0; i < newVolSize; i++) {
                sMask.clear(i);
            }
        }

        for (i = 0; i < newDim[0]; i++) {

            // if (isProgressBarVisible()&& i%mod==0) {
            // progressBar.setValue((int)((float)i/newDim[0] * 100+0.5));
            // }
            imm = i * newResol[0];

            for (j = 0; j < newDim[1]; j++) {
                jmm = j * newResol[1];

                for (k = 0; k < newDim[2]; k++) {

                    // transform i,j,k
                    kmm = k * newResol[2];

                    X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                    Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                    Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;

                    // set intensity of i,j,k to new transformed coordinate if
                    // x,y,z is w/in dimensions of image
                    X = X / orgResol[0];
                    Y = Y / orgResol[1];
                    Z = Z / orgResol[2];

                    roundX = (int) (X + 0.5f);
                    roundY = (int) (Y + 0.5f);
                    roundZ = (int) (Z + 0.5f);

                    if ( (roundX < 0) || (roundX > (orgDim[0] - 1)) || (roundY < 0) || (roundY > (orgDim[1] - 1))
                            || (roundZ < 0) || (roundZ > (orgDim[2] - 1))) {
                        sBuffer[i + (j * newDim[0]) + (k * newSliceSize)] = 0;
                    } else {
                        xOffset = roundX;
                        yOffset = roundY * orgDim[0];
                        zOffset = roundZ * sliceSize;
                        sBuffer[i + (j * newDim[0]) + (k * newSliceSize)] = imgBuf[xOffset + yOffset + zOffset];

                        if ( (mask != null) && (mask.get(xOffset + yOffset + zOffset))) {
                            sMask.set(i + (j * newDim[0]) + (k * newSliceSize));
                        }
                    }
                }
            }
        }

    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     * 
     * @param imgBuffer image array
     * @param xfrm transformation matrix to be applied
     */
    private void transformTrilinear(final float[] imgBuffer, final TransMatrix matrix) {

        AlgorithmTransform.transformTrilinear(imgBuffer, fieldBuffer, matrix, newDim[0], newDim[1], newDim[2],
                newResol[0], newResol[1], newResol[2], orgDim[0], orgDim[1], orgDim[2], orgResol[0], orgResol[1],
                orgResol[2], null);
    }

    /**
     * Allocates and initializes important buffers.
     */
    private void volumeDomain() {
        int i, j;

        try {
            scale = 1.0f / (fieldDistance * fieldDistance * fieldDistance);

            for (i = 0; i < nDimensions; i++) {
                domain[i][0] = -0.5f * newResol[i];
                domain[i][1] = (newDim[i] - 0.5f) * newResol[i];
            }

            // fit splines to data points that are non-zero in the mask
            // only look at values within domain
            for (i = 0; i < nDimensions; i++) {
                lower[i] = (int) Math.ceil(domain[i][0] / newResol[i]);
                upper[i] = (int) Math.floor(domain[i][1] / newResol[i]);
            } // for (i = 0; i < nDimensions; i++)

            for (i = 0; i < nDimensions; i++) {
                nArray[i] = (int) Math.ceil( (domain[i][1] - domain[i][0]) / fieldDistance) + 3;
            }

            nMax = nArray[0];

            for (i = 1; i < nDimensions; i++) {

                if (nArray[i] > nMax) {
                    nMax = nArray[i];
                }
            }

            nProduct = 1;

            for (i = 0; i < nDimensions; i++) {
                nProduct *= nArray[i];
            }

            JArray = new double[nProduct][nProduct];
            knots = new float[nMax + 4][nDimensions];

            for (i = 0; i < nDimensions; i++) {
                startK = 0.5f * (domain[i][0] + domain[i][1] - (fieldDistance * (nArray[i] + 3)));

                for (j = 0; j < (nArray[i] + 4); j++) {
                    knots[j][i] = startK + (fieldDistance * j);
                }
            }

            zero = new float[nDimensions];

            for (i = 0; i < nDimensions; i++) {
                zero[i] = knots[3][i];
            }

            four = 1;
            smallN = new int[4];

            for (i = 0; i < nDimensions; i++) {
                four *= 4;
                smallN[i] = 4;
            }

            AtA = new float[nProduct][nProduct]; // changes
            AtF = new double[nProduct][1];
            coef = new double[nProduct][1];
            values = new float[four];
            locations = new int[four];
            dloc_i = new int[four];
            dloc_j = new int[four];

            // allocate space for cached values
            d1Spline = new float[nDimensions][];
            offsetSp = new int[nDimensions][];

            for (i = 0; i < nDimensions; i++) {
                d1Spline[i] = new float[newDim[i] * 4];
                offsetSp[i] = new int[newDim[i] * 4];
            }

            for (i = 0; i < nDimensions; i++) {

                for (int order = 0; order < 3; order++) {
                    bendingMatrix[i][order] = new float[nArray[i]][nArray[i]];
                }
            }

            blurR = new float[padded_size];
            blurI = new float[padded_size];

            filterR = new float[padded_size];
            filterI = new float[padded_size];

            fHistPaddedR = new float[padded_size];
            fHistPaddedI = new float[padded_size];

            momentR = new float[padded_size];
            momentI = new float[padded_size];

        } catch (final OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("AlgorithmIHN3Correction: Out of memory on field image import data in routine volumeDomain");

            setCompleted(false);

            return;
        }
    }

}
