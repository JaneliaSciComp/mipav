package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import Jama.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmDenoisingBLS_GSM extends AlgorithmBase {
    // The image is expanded so that all dimensions are powers of 2 and there is zero padding going past each original
    // boundary by the coefficient number - 2.   The image is transformed into wavelet coefficients.

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * This is a port of MATLAB BLS-GSM Image Denoising software written by Javier Portilla of Universidad de Granada,
     * Spain. References: 1.) "Image Denoising Using Scale Mixtures of Gaussians in the Wavelet Domain", Javier
     * Portilla, Vasily Strela, Martin J. Wainwright, and Eero P. Simoncelli, IEEE Transactions on Image Processing,
     * Vol. 12. No. 11, November, 2003, pp. 1338-1351. 2.) "The Steerable Pyramid: A Flexible Architecture for
     * Multi-Scale Derivative Computation", Eero P. Simoncelli and William T. Freeman, 2nd IEEE International Conference
     * on Image Processing, Washington, D.C., vol. III, pp. 444-447, October, 1995.
     *
     * <p>This program only applies to 2D black and white images. Notes from Javier Portilla's readme.txt: This program
     * uses 1 of the 4 selected repres1 methods to clean a noisy image with image padding with mirror reflection to
     * avoid boundary artifacts, conversion from the image domain to the wavelet domain and back from the wavelet domain
     * to the image domain. In the wavelet domain there is a decomposition into subbands, which are processed
     * sequentially (in couples corresponding to subbands of the same orientation adjacent in the scale when useParent
     * is true, or individually when useParent is false). Then the processed subbands are recomposed back into a single
     * image.</p>
     *
     * <p>The 4 methods are: 1.) Orthogonal wavelet. Very fast, but of relatively poor denoising performance, because of
     * not being translation invariant. 2.) Undecimated version of orthogonal wavelet. In fact, this is a highly
     * redundant version of the orthogonal wavelet that avoids intra-subband aliasing, but still keeps a multiscale
     * structure. (The lowest level of the pyramid are extended 2 times in each dimension, whereas the rest of the
     * scales are extended 4 times in each dimension.) It provides a good trade-off between computational cost and
     * denoising performance (which is very high). 3.) Steerable pyramid. It allows the user to choose an arbitrary
     * number of orientations. The splitting in oriented subbands is not applied to the very high frequencies, which are
     * represented in a single subband (high-pass residual). With a moderate computational cost, for a modest number of
     * orientations (4 or less), its results depend on the type of image, being comparable (or slightly worse) on
     * average than those obtained with the option undecimated orthogonal wavelet. 4.) Full steerable pyramid. Same as
     * steerable pyramid, but now also the very high frequencies are splitted into orientations. It provides very high
     * denoising performance (for some images slightly better than undecimated orthogonal wavelet), especially with a
     * high number of orientations (8, for example), but it is very demanding computationally.</p>
     *
     * <p>Note that the shrink and expand functions shrink and expand an image in the Fourier domain.</p>
     */

    // Possible repres1 values
    private static final int ORTHOGONAL_WAVELET = 1;

    /** DOCUMENT ME! */
    private static final int UNDECIMATED_ORTHOGONAL_WAVELET = 2;

    /** DOCUMENT ME! */
    private static final int STEERABLE_PYRAMID = 3;

    /** DOCUMENT ME! */
    private static final int FULL_STEERABLE_PYRAMID = 4;

    /** Possible repres2 values. */
    private static final int NONE = 0;

    /** DOCUMENT ME! */
    private static final int HAAR = 1; // Haar wavelet

    /** Symmetric quadrature mirror filters. */
    private static final int QMF5 = 2;

    /** DOCUMENT ME! */
    private static final int QMF8 = 3;

    /** DOCUMENT ME! */
    private static final int QMF9 = 4;

    /** DOCUMENT ME! */
    private static final int QMF12 = 5;

    /** DOCUMENT ME! */
    private static final int QMF13 = 6;

    /** DOCUMENT ME! */
    private static final int QMF16 = 7;

    /** Daubechies wavelet. */
    private static final int DAUB1 = 8;

    /** DOCUMENT ME! */
    private static final int DAUB2 = 9;

    /** DOCUMENT ME! */
    private static final int DAUB3 = 10;

    /** DOCUMENT ME! */
    private static final int DAUB4 = 11;

    /** DOCUMENT ME! */
    private static final int GAUSS3 = 12;

    /** DOCUMENT ME! */
    private static final int GAUSS5 = 13;

    /** DOCUMENT ME! */
    private static final int BINOMIAL = 14;

    /** Types of edges. */
    private static final int CIRCULAR = 1;

    /** DOCUMENT ME! */
    private static final int REFLECT1 = 2;

    /** DOCUMENT ME! */
    private static final int EXTEND = 3;

    /** DOCUMENT ME! */
    private static final int REPEAT = 4;

    /** Phase solutions for Daubechies scaling and wavelet filters. */
    private static final int MINIMUM_PHASE = 1;

    /** DOCUMENT ME! */
    private static final int MID_PHASE = 2;

    /** DOCUMENT ME! */
    private static final int MAXIMUM_PHASE = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[] aArray;

    /** DOCUMENT ME! */
    private int arrayLength;

    /** DOCUMENT ME! */
    private int blockSizeX = 3; // Local neighborhood x

    /** DOCUMENT ME! */
    private int blockSizeY = 3; // Local neighborhood y

    /** DOCUMENT ME! */
    private int error = 0; // 0 for no error, 1 for error

    /** DOCUMENT ME! */
    private boolean includeCovar = true; // If true, include covariance in the GSM model
                                         // If false, assume uncorrelated Gaussian vectors
                                         // describe both noise and signal.  The coefficients in this
                                         // representation are strongly correlated, both because of
                                         // inherent spectral features of the image and because of the
                                         // redundancy induced by this overcomplete representation, and
                                         // ignoring this correlation in the model leads to a signficant
                                         // loss in performance.

    /** DOCUMENT ME! */
    private int nOrientations = 8; // Number of orientations.  For separable wavelets this must be 3.

    /** DOCUMENT ME! */
    private int npx; // Integer multiple of 2**(nScales+1) for applying pyramidal representation

    /** DOCUMENT ME! */
    private int npy; // Integer multiple of 2**(nScales+1) for applying pyramidal representation

    /** DOCUMENT ME! */
    private int nScales = 4; // Number of scales

    /** DOCUMENT ME! */
    private int nx;

    /** DOCUMENT ME! */
    private int ny;

    /** DOCUMENT ME! */
    private boolean optimize = true; // If true, use the full Bayesian least squares estimator
                                     // If false, use a two-step estimator (MAP estimator of the local
                                     // multiplier, followed by linear estimation of the coefficient)
                                     // Using the two-step estimator results in a substantial
                                     // reduction in performance

    /**
     * repres1 = ORTHOGONAL_WAVELET Here repres2 can be DAUB2, DAUB3, DAUB4, HAAR, QMF5, QMF8, QMF9, QMF12, QMF13, QMF16
     * QMF9 is default.
     *
     * <p>repres1 = UNDECIMATED_ORTHOGONAL_WAVELET Here repres2 can be DAUB2, DAUB3, DAUB4 DAUB2 is the default.</p>
     *
     * <p>repres1 = STEERABLE_PYRAMID Here repres2 = NONE</p>
     *
     * <p>repres1 = FULL_STEERABLE_PYRAMID Here repres2 = NONE</p>
     */
    private int repres1 = FULL_STEERABLE_PYRAMID;

    /** Particular wavelet used for repres1 = ORTHOGONAL_WAVELET or UNDECIMATED_ORTHOGONAL_WAVELET. */
    private int repres2 = NONE;

    /** DOCUMENT ME! */
    private float sig; // noise standard deviation

    /** DOCUMENT ME! */
    private boolean useBoundary = true; // If true, use mirror reflected boundary padding.

    /** DOCUMENT ME! */
    private boolean useParent = true; // If true, process subbands in couples of the same orientation
                                      // adjacent in scale.  That is, include the coarse scale parent
                                      // in the neighborhood.  If false, process subbands individually.
                                      // Eliminating the coarse-scale parent from the neighborhood decreases
                                      // performance significantly only at high noise levels.  This should
                                      // not be taken to mean that the parent coefficient does not provide
                                      // information about the reference coefficient, but rather that the
                                      // information is somewhat redundant with that provided by the other
                                      // neighbors.

    /** DOCUMENT ME! */
    private boolean usePSD = false; // If true, use power spectral density = fft(autocorrelation)
                                    // If false, use white noise
                                    // Default is use white noise

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     *
     * @param  srcImg         source image model
     * @param  sig            Noise standard deviation
     * @param  usePSD         If true, use power spectral density. If false, use white noise
     * @param  blockSizeX     Local neighborhood x
     * @param  blockSizeY     Local neighborhood y
     * @param  useParent      If true, process subbands in couples of the same orientation adjacent in scale. That is,
     *                        include the coarse scale parent in the neighborhood. If false, process subbands
     *                        individually
     * @param  useBoundary    If true, use mirror reflected boundary padding.
     * @param  nScales        Number of scales
     * @param  nOrientations  Number of orientations. For separable wavelets this must be 3.
     * @param  includeCovar   Include covariance in the GSM model
     * @param  optimize       If true, use the full Bayesian least squares estimator If false, use a two-step estimator.
     * @param  repres1        ORTHOGONAL_WAVELET, UNDECIMATED_ORTHOGONAL_WAVELET, STEERABLE_PYRAMID, or
     *                        FULL_STEERABLE_PYRAMID
     * @param  repres2        Particular wavelet used for repres1 = ORTHOGONAL_WAVELET or UNDECIMATED_ORTHOGONAL_WAVELET
     */
    public AlgorithmDenoisingBLS_GSM(ModelImage srcImg, float sig, boolean usePSD, int blockSizeX, int blockSizeY,
                                     boolean useParent, boolean useBoundary, int nScales, int nOrientations,
                                     boolean includeCovar, boolean optimize, int repres1, int repres2) {
        super(null, srcImg);


        this.sig = sig;
        this.usePSD = usePSD;
        this.blockSizeX = blockSizeX;
        this.blockSizeY = blockSizeY;
        this.useParent = useParent;
        this.useBoundary = useBoundary;
        this.nScales = nScales;
        this.nOrientations = nOrientations;
        this.includeCovar = includeCovar;
        this.optimize = optimize;
        this.repres1 = repres1;
        this.repres2 = repres2;
    }

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     *
     * @param  destImg        image model where result image is to be stored
     * @param  srcImg         source image model
     * @param  sig            Noise standard deviation
     * @param  usePSD         If true, use power spectral density. If false, use white noise.
     * @param  blockSizeX     Local neighborhood x
     * @param  blockSizeY     Local neighborhood y
     * @param  useParent      If true, process subbands in couples of the same orientation adjacent in scale. That is,
     *                        include the coarse scale parent in the neighborhood. If false, process subbands
     *                        individually
     * @param  useBoundary    If true, use mirror reflected boundary padding.
     * @param  nScales        Number of scales
     * @param  nOrientations  Number of orientations. For separable wavelets this must be 3.
     * @param  includeCovar   Include covariance in the GSM model
     * @param  optimize       If true, use the full Bayesian least squares estimator If false, use a two-step estimator.
     * @param  repres1        ORTHOGONAL_WAVELET, UNDECIMATED_ORTHOGONAL_WAVELET, STEERABLE_PYRAMID, or
     *                        FULL_STEERABLE_PYRAMID
     * @param  repres2        Particular wavelet used for repres1 = ORTHOGONAL_WAVELET or UNDECIMATED_ORTHOGONAL_WAVELET
     */
    public AlgorithmDenoisingBLS_GSM(ModelImage destImg, ModelImage srcImg, float sig, boolean usePSD, int blockSizeX,
                                     int blockSizeY, boolean useParent, boolean useBoundary, int nScales,
                                     int nOrientations, boolean includeCovar, boolean optimize, int repres1,
                                     int repres2) {
        super(destImg, srcImg);

        this.sig = sig;
        this.usePSD = usePSD;
        this.blockSizeX = blockSizeX;
        this.blockSizeY = blockSizeY;
        this.useParent = useParent;
        this.useBoundary = useBoundary;
        this.nScales = nScales;
        this.nOrientations = nOrientations;
        this.includeCovar = includeCovar;
        this.optimize = optimize;
        this.repres1 = repres1;
        this.repres2 = repres2;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepare this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        aArray = null;
        super.finalize();
    }

    /**
     * Starts the program. This contains a port of the code contained in denoi_BLS_GSM.m written by Javier Portilla,
     * Univ. de Granada, 11/15/2004
     */
    public void runAlgorithm() {
        int x, y;
        int xs;
        int ys;
        int twoPow;
        int i;
        double[] corArray;
        double[] padArray;
        int bx;
        int by;
        ModelImage corImage;
        AlgorithmAutoCorrelation algoAutoCorrelation;
        double[] imagArray;
        double[] delta;
        int deltax; // x dimension of delta
        int deltay; // y dimension of delta
        FFTUtility fftUtil;
        double[] psArray;
        RandomNumberGen randomGen;
        double[] ranArray;
        double[] im;
        int imx; // x dimension of im
        int imy; // y dimension of im
        double[] imD = null;
        double[] aux;
        double mean;
        long time;
        long time2;
        long elapsedTime;
        int daubOrder = 2;
        int j;
        int index;
        boolean isOddPSx;
        int ndx;
        boolean isOddPSy;
        int ndy;
        double[] corArray2;
        double[] imagArray2;
        int i2, j2;
        double oldMin;
        double oldMax;
        double newMin;
        double newMax;
        double slope;
        double offset;

        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        if (((blockSizeX % 2) == 0) || ((blockSizeY % 2) == 0)) {
            displayError("Spatial dimensions of neighborhood must be odd");
            setCompleted(false);

            return;
        }

        if (((repres1 == ORTHOGONAL_WAVELET) || (repres1 == UNDECIMATED_ORTHOGONAL_WAVELET)) && (nOrientations != 3)) {
            MipavUtil.displayWarning("For X-Y separable orientations nOrientations must be 3");
            nOrientations = 3;
        }

        fireProgressStateChanged(srcImage.getImageName(), "DenoisingBLS_GSM...");

        

        nx = srcImage.getExtents()[0];
        ny = srcImage.getExtents()[1];
        arrayLength = nx * ny;
        imx = nx;
        imy = ny;

        // Ensure that the processed image has dimensions that are integer multiples of 2**(nScales+1),
        // so it will not crash when applying the pyramidal representation.  The idea is padding with
        // mirror reflected pixels.
        twoPow = 2;

        // twoPow = 2**(nScales+1)
        for (i = 0; i < nScales; i++) {
            twoPow = 2 * twoPow;
        }

        npx = (int) (Math.ceil((double) nx / twoPow) * twoPow);
        npy = (int) (Math.ceil((double) ny / twoPow) * twoPow);

        try {
            im = new double[arrayLength];
        } catch (OutOfMemoryError e) {
            im = null;
            System.gc();
            displayError("AlgorithmDenoisingBLS_GSM: Out of memory creating im");
            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, arrayLength, im);
        } catch (IOException error) {
            displayError("AlgorithmDenoisingBLS_GSM: Source image is locked");
            setCompleted(false);

            return;
        }

        if ((nx != npx) || (ny != npy)) {
            padArray = new double[npx * npy];

            // Mirror only down and right
            for (y = 0; y < ny; y++) {

                for (x = 0; x < nx; x++) {
                    padArray[x + (npx * y)] = im[x + (nx * y)];
                }
            } // for (y = 0; y < ny; y++)

            for (y = ny, ys = ny - 1; y < npy; y++, ys--) {

                for (x = 0; x < nx; x++) {
                    padArray[x + (npx * y)] = im[x + (nx * ys)];
                }
            } // for (y = ny, ys = ny-1; y < npy; y++, ys--)

            for (y = 0; y < ny; y++) {

                for (x = nx, xs = nx - 1; x < npx; x++, xs--) {
                    padArray[x + (npx * y)] = im[xs + (nx * y)];
                }
            } // for (y = 0; y < ny; y++)

            for (y = ny, ys = ny - 1; y < npy; y++, ys--) {

                for (x = nx, xs = nx - 1; x < npx; x++, xs--) {
                    padArray[x + (npx * y)] = im[xs + (nx * ys)];
                }
            } // for (y = ny, ys = ny-1; y < npy; y++, ys--)

            im = null;
            im = padArray;
            imx = npx;
            imy = npy;
        } // if ((nx != npx) || (ny != npy))

        // Size of the extension for boundary handling
        if ((repres1 == STEERABLE_PYRAMID) || (repres1 == FULL_STEERABLE_PYRAMID)) {
            twoPow = 1;

            for (i = 0; i < (nScales - 2); i++) {
                twoPow = 2 * twoPow;
            }

            bx = (blockSizeX - 1) * twoPow;
            by = (blockSizeY - 1) * twoPow;
        } // if ((repres1 == STEERABLE_PYRAMID) || (repres1 == FULL_STEERABLE_PYRAMID))
        else {
            twoPow = 1;

            for (i = 0; i < (nScales - 1); i++) {
                twoPow = 2 * twoPow;
            }

            bx = (blockSizeX - 1) * twoPow;
            by = (blockSizeY - 1) * twoPow;
        } // else

        if (usePSD) {

            // Must not use the padded srcImage array with mirroring for FFT
            String name = srcImage.getImageName() + "_autocorrelation";
            corImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name);
            algoAutoCorrelation = new AlgorithmAutoCorrelation(corImage, srcImage);
            algoAutoCorrelation.run();
            algoAutoCorrelation.finalize();
            corArray = new double[arrayLength];

            try {
                corImage.exportData(0, arrayLength, corArray);
            } catch (IOException error) {
                displayError("AlgorithmDenoisingBLS_GSM: corImage is locked");
                setCompleted(false);

                return;
            }

            imagArray = new double[nx * ny];

            // The fft of the autocorrelation gives the power spectral density
            // NOTE: Scale factors do not matter here.
            // forward FFT
            fftUtil = new FFTUtility(corArray, imagArray, ny, nx, 1, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(corArray, imagArray, 1, ny, nx, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            center(corArray, imagArray, nx, ny);

            // Ensure even dimensions
            if ((nx % 2) == 1) {
                isOddPSx = true;
                ndx = nx - 1;
            } else {
                isOddPSx = false;
                ndx = nx;
            }

            if ((ny % 2) == 1) {
                isOddPSy = true;
                ndy = ny - 1;
            } else {
                isOddPSy = false;
                ndy = ny;
            }

            if (isOddPSx || isOddPSy) {
                corArray2 = new double[ndx * ndy];
                imagArray2 = new double[ndx * ndy];

                for (j = 0; j < ndy; j++) {

                    for (i = 0; i < ndx; i++) {
                        corArray2[i + (j * ndx)] = corArray[i + (j * nx)];
                        imagArray2[i + (j * ndx)] = imagArray[i + (j * nx)];
                    }
                }
            } else {
                corArray2 = corArray;
                imagArray2 = imagArray;
            }

            center(corArray2, imagArray2, ndx, ndy);

            // A power spectrum is always real and nonnegative
            for (i = 0; i < corArray2.length; i++) {

                if (corArray2[i] < 0.0) {
                    corArray2[i] = 0.0;
                } else {
                    corArray2[i] = Math.sqrt(corArray2[i]);
                }

                imagArray2[i] = 0.0;
            }
        } // if (usePSD)
        else { // white noise
            ndx = npx;
            ndy = npy;
            corArray2 = new double[npx * npy];
            imagArray2 = new double[npx * npy];

            for (i = 0; i < corArray2.length; i++) {
                corArray2[i] = 1.0;
            }
        } // else white noise

        // Inverse FFT
        fftUtil = new FFTUtility(corArray2, imagArray2, ndy, ndx, 1, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(corArray2, imagArray2, 1, ndy, ndx, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        center(corArray2, imagArray2, ndx, ndy);

        // Make the dimensions of the power spectral density support the same as those
        // of the padded image
        deltax = npx;
        deltay = npy;
        delta = new double[npx * npy];

        if ((ndy <= npy) && (ndx <= npx)) {

            for (j = (npy / 2) - (ndy / 2), j2 = 0; j < ((npy / 2) + (ndy / 2)); j++, j2++) {

                for (i = (npx / 2) - (ndx / 2), i2 = 0; i < ((npx / 2) + (ndx / 2)); i++, i2++) {
                    delta[i + (npx * j)] = corArray2[i2 + (ndx * j2)];
                }
            }
        } // if ((ndy <= npy) && (ndx <= npx))
        else if ((ndy > npy) && (ndx > npx)) {

            for (j = 0, j2 = (ndy / 2) - (npy / 2); j2 < ((ndy / 2) + (npy / 2)); j++, j2++) {

                for (i = 0, i2 = (ndx / 2) - (npx / 2); i2 < ((ndx / 2) + (npx / 2)); i++, i2++) {
                    delta[i + (npx * j)] = corArray2[i2 + (ndx * j2)];
                }
            }
        } // else if ((ndy > npy) && (ndx > npx))
        else if ((ndy <= npy) && (ndx > npx)) {

            for (j = (npy / 2) - (ndy / 2), j2 = 0; j < ((npy / 2) + (ndy / 2)); j++, j2++) {

                for (i = 0, i2 = (ndx / 2) - (npx / 2); i2 < ((ndx / 2) + (npx / 2)); i++, i2++) {
                    delta[i + (npx * j)] = corArray2[i2 + (ndx * j2)];
                }
            }
        } // else if ((ndy <= npy) && (ndx > npx))
        else if ((ndy > npy) && (ndx <= npx)) {

            for (j = 0, j2 = (ndy / 2) - (npy / 2); j2 < ((ndy / 2) + (npy / 2)); j++, j2++) {

                for (i = (npx / 2) - (ndx / 2), i2 = 0; i < ((npx / 2) + (ndx / 2)); i++, i2++) {
                    delta[i + (npx * j)] = corArray2[i2 + (ndx * j2)];
                }
            }
        } // else if ((ndy > npy) && (ndx <= npx))


        if (repres1 == ORTHOGONAL_WAVELET) {
            imagArray = new double[npx * npy];

            // forward FFT
            fftUtil = new FFTUtility(delta, imagArray, npy, npx, 1, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(delta, imagArray, 1, npy, npx, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            psArray = new double[npx * npy];

            for (i = 0; i < psArray.length; i++) {
                psArray[i] = (delta[i] * delta[i]) + (imagArray[i] * imagArray[i]);
            }

            imagArray = new double[npx * npy];
            center(psArray, imagArray, npx, npy);

            // Noise, to be used only with translation variant transforms (such as
            // orthogonal wavelet)
            ranArray = new double[npx * npy];
            randomGen = new RandomNumberGen();

            for (i = 0; i < ranArray.length; i++) {
                ranArray[i] = randomGen.genStandardGaussian();
            }

            // forward FFT
            fftUtil = new FFTUtility(ranArray, imagArray, npy, npx, 1, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(ranArray, imagArray, 1, npy, npx, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();

            for (i = 0; i < ranArray.length; i++) {
                ranArray[i] = Math.atan2(imagArray[i], ranArray[i]);
                delta[i] = Math.sqrt(psArray[i]);
                imagArray[i] = delta[i] * Math.sin(ranArray[i]);
                delta[i] = delta[i] * Math.cos(ranArray[i]);
            }

            // Inverse FFT
            fftUtil = new FFTUtility(delta, imagArray, npy, npx, 1, +1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(delta, imagArray, 1, npy, npx, +1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
        } // if (repres1 == ORTHOGONAL_WAVELET)

        // Boundary handling: it extends im and delta
        if (useBoundary) {
            im = mirrorExtension(im, npx, npy, bx, by);
            imx = npx + (2 * bx);
            imy = npy + (2 * by);

            if (repres1 == ORTHOGONAL_WAVELET) {
                delta = mirrorExtension(delta, npx, npy, bx, by);
            } else {
                aux = new double[npx * npy];

                for (i = 0; i < delta.length; i++) {
                    aux[i] = delta[i];
                }

                delta = new double[(npx + (2 * bx)) * (npy + (2 * by))];

                for (y = 0; y < npy; y++) {

                    for (x = 0; x < npx; x++) {
                        delta[x + bx + ((npx + (2 * bx)) * (y + by))] = aux[x + (npx * y)];
                    }
                }
            }

            deltax = npx + (2 * bx);
            deltay = npy + (2 * by);
        } // if (useBoundary)
        else {
            bx = 0;
            by = 0;
        }

        // Normalize the energy (the noise variance is given by "sig")
        mean = 0.0;

        for (i = 0; i < delta.length; i++) {
            mean += delta[i] * delta[i];
        }

        mean = mean / delta.length;
        mean = Math.sqrt(mean);

        for (i = 0; i < delta.length; i++) {
            delta[i] = delta[i] / mean;
        }

        // Impose the desired variance to the noise
        for (i = 0; i < delta.length; i++) {
            delta[i] = sig * delta[i];
        }

        // main
        time = System.currentTimeMillis();

        // Standard steerable pyramid
        if (repres1 == STEERABLE_PYRAMID) {
            imD = decompReconst(im, imx, imy, delta, deltax, deltay);
        } else if (repres1 == FULL_STEERABLE_PYRAMID) {
            imD = decompReconstFull(im, imx, imy, delta, deltax, deltay);
        } else if (repres1 == ORTHOGONAL_WAVELET) {
            imD = decompReconstW(im, imx, imy, repres2, delta, deltax, deltay);
        } else if (repres1 == UNDECIMATED_ORTHOGONAL_WAVELET) {

            if (repres2 == HAAR) {
                daubOrder = 2;
            } else if (repres2 == DAUB1) {
                daubOrder = 2;
            } else if (repres2 == DAUB2) {
                daubOrder = 4;
            } else if (repres2 == DAUB3) {
                daubOrder = 6;
            } else if (repres2 == DAUB4) {
                daubOrder = 8;
            }

            imD = decompReconstWU(im, imx, imy, daubOrder, delta, deltax, deltay);
        }

        if (error == 1) {
            setCompleted(false);

            return;
        }

        time2 = System.currentTimeMillis();
        elapsedTime = time2 - time;
        Preferences.debug("Elapsed time in seconds = " + Math.round(elapsedTime / 1000.0));

        newMin = Double.MAX_VALUE;
        newMax = -Double.MAX_VALUE;
        aArray = new double[nx * ny];

        for (j = by, index = 0; j < (by + ny); j++) {

            for (i = bx; i < (bx + nx); i++, index++) {
                aArray[index] = imD[i + (imx * j)];

                if (newMin > aArray[index]) {
                    newMin = aArray[index];
                }

                if (newMax < aArray[index]) {
                    newMax = aArray[index];
                }
            }
        }

        // Scale back to old image min and image max
        // This is necessary since a large shift in range can occur
        // for steerable pyramid and full steerable pyramid.
        oldMin = srcImage.getMin();
        oldMax = srcImage.getMax();
        slope = (oldMax - oldMin) / (newMax - newMin);
        offset = oldMax - (slope * newMax);

        for (i = 0; i < aArray.length; i++) {
            aArray[i] = (slope * aArray[i]) + offset;
        }

        if (destImage != null) {

            try {
                destImage.importData(0, aArray, true);
            } catch (IOException error) {
                displayError("AlgorithmDenoisingBLS_GSM: IOException on destination image import data");
                setCompleted(false);

                return;
            }
        } else {

            try {
                srcImage.importData(0, aArray, true);
            } catch (IOException error) {
                displayError("AlgorithmDenoisingBLS_GSM: IOException on source image import data");
                setCompleted(false);

                return;
            }

        }

        fireProgressStateChanged(100, null, null);

        setCompleted(true);

    }

    /**
     * This is a port of binomialFilter.m by Eero Simoncelli, 2/97. returns a vector of binomial coefficients of order
     * (binomialSize-1)
     *
     * @param   binomialSize  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] binomialFilter(int binomialSize) {
        double[] kernel;
        double[] baseK;
        int n;

        if (binomialSize < 2) {
            MipavUtil.displayError("binomialSize must be larger than 1");

            return null;
        }

        kernel = new double[2];
        kernel[0] = 0.5;
        kernel[1] = 0.5;

        baseK = new double[2];
        baseK[0] = 0.5;
        baseK[1] = 0.5;

        for (n = 1; n <= (binomialSize - 2); n++) {
            kernel = conv(baseK, kernel);
        }

        return kernel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  xout  DOCUMENT ME!
     * @param  lx    DOCUMENT ME!
     * @param  g0    DOCUMENT ME!
     * @param  g1    DOCUMENT ME!
     * @param  lh    DOCUMENT ME!
     * @param  xinl  DOCUMENT ME!
     * @param  xinh  DOCUMENT ME!
     */
    private void bpconv(double[] xout, int lx, double[] g0, double[] g1, int lh, double[] xinl, double[] xinh) {
        int i, j;
        double x0;

        for (i = lh - 2; i > -1; i--) {
            xinl[i] = xinl[lx + i];
            xinh[i] = xinh[lx + i];
        }

        for (i = 0; i < lx; i++) {
            x0 = 0;

            for (j = 0; j < lh; j++) {
                x0 = x0 + (xinl[j + i] * g0[lh - 1 - j]) + (xinh[j + i] * g1[lh - 1 - j]);
                xout[i] = x0;
            }
        }
    }

    /**
     * This is a port of buildFullSFpyr2.m Construct a steerable pyramid on matrix im, in the Fourier domain. Unlike the
     * standard transform, subdivides the highpass band into orientations.
     *
     * @param   im      DOCUMENT ME!
     * @param   imx     DOCUMENT ME!
     * @param   imy     DOCUMENT ME!
     * @param   ht      DOCUMENT ME!
     * @param   order   DOCUMENT ME!
     * @param   twidth  DOCUMENT ME!
     * @param   pind    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector buildFullSFpyr2(double[] im, int imx, int imy, int ht, int order, double twidth, Vector pind) {
        Vector pyr = null;
        int max_ht;
        int nbands;
        int ctrx;
        int ctry;
        double[][] xramp;
        double[][] yramp;
        double[][] angle;
        double[][] log_rad;
        int i, j;
        int index;
        double value;
        double logC;
        double[] xrcos = null;
        double[] yrcos = null;
        double[] yircos;
        double[][] lo0mask = null;
        FFTUtility fftUtil;
        double[] imdftr;
        double[] imdfti;
        double[] lo0dftr;
        double[] lo0dfti;
        double[][] hi0mask;
        int lutsize;
        double[] xcosn;
        double var;
        double consta;
        double[] ycosn;
        double[][] bands;
        int[][] bind;
        int b;
        double[][] anglemask;
        double[][] maskr;
        double[][] maski;
        double[] bandfftr = null;
        double[] bandffti = null;

        // log2(x) = loge(x)/loge(2)
        logC = 1.0 / Math.log(2.0);
        max_ht = (int) Math.floor((logC * Math.log(Math.min(imx, imy))) + 1);

        if (ht > max_ht) {
            MipavUtil.displayError("Error! Cannot build pyramid higher than " + max_ht + " levels");
            error = 1;

            return null;
        }

        if ((order > 15) || (order < 0)) {
            MipavUtil.displayWarning("Warning: ORDER must be an integer in the range 0 to 15");

            if (order < 0) {
                MipavUtil.displayWarning("Setting order to 0");
                order = 0;
            } else if (order > 15) {
                order = 15;
                MipavUtil.displayWarning("Setting order to 15");
            }
        } // if ((order > 15) || (order < 0))

        nbands = order + 1;

        if (twidth <= 0) {
            MipavUtil.displayWarning("Warning: TWIDTH must be positive.  Setting to 1");
            twidth = 1;
        }

        ctrx = (int) Math.ceil((imx + 0.5) / 2.0);
        ctry = (int) Math.ceil((imy + 0.5) / 2.0);
        xramp = new double[imy][imx];
        yramp = new double[imy][imx];

        for (i = 0; i < imx; i++) {
            value = ((i + 1.0) - ctrx) / (imx / 2.0);

            for (j = 0; j < imy; j++) {
                xramp[j][i] = value;
            }
        }

        for (j = 0; j < imy; j++) {
            value = ((j + 1.0) - ctry) / (imy / 2.0);

            for (i = 0; i < imx; i++) {
                yramp[j][i] = value;
            }
        }

        angle = new double[imy][imx];
        log_rad = new double[imy][imx];

        for (j = 0; j < imy; j++) {

            for (i = 0; i < imx; i++) {
                angle[j][i] = Math.atan2(yramp[j][i], xramp[j][i]);
                log_rad[j][i] = Math.sqrt((xramp[j][i] * xramp[j][i]) + (yramp[j][i] * yramp[j][i]));
            }
        }

        log_rad[ctry - 1][ctrx - 1] = log_rad[ctry - 1][ctrx - 2];

        // log2(log_rad) = loge(rad)/loge(2)
        for (j = 0; j < imy; j++) {

            for (i = 0; i < imx; i++) {
                log_rad[j][i] = logC * Math.log(log_rad[j][i]);
            }
        }

        // Radial transition function (a raised cosine in log-frequency)
        xrcos = new double[259];
        yrcos = new double[259];
        rcosFn(twidth, (-twidth / 2.0), 0.0, 1.0, xrcos, yrcos);

        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }

        yircos = new double[yrcos.length];

        for (i = 0; i < yrcos.length; i++) {
            yircos[i] = Math.sqrt(1.0 - (yrcos[i] * yrcos[i]));
        }

        lo0mask = pointOp(log_rad, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);

        // forward FFT
        imdftr = new double[imx * imy];

        for (i = 0; i < im.length; i++) {
            imdftr[i] = im[i];
        }

        imdfti = new double[imx * imy];
        fftUtil = new FFTUtility(imdftr, imdfti, imy, imx, 1, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(imdftr, imdfti, 1, imy, imx, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        center(imdftr, imdfti, imx, imy);
        lo0dftr = new double[imx * imy];
        lo0dfti = new double[imx * imy];

        for (j = 0; j < imy; j++) {

            for (i = 0; i < imx; i++) {
                index = i + (j * imx);
                lo0dftr[index] = imdftr[index] * lo0mask[j][i];
                lo0dfti[index] = imdfti[index] * lo0mask[j][i];
            }
        }

        pyr = buildSFpyrLevs(lo0dftr, lo0dfti, imx, imy, log_rad, xrcos, yrcos, angle, ht, nbands, pind);

        // Split the highpass band into orientations
        hi0mask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);

        lutsize = 1024;
        xcosn = new double[3 * (lutsize + 1)];

        // -2*PI to PI
        for (i = 0, j = -((2 * lutsize) + 1); i < xcosn.length; i++, j++) {
            xcosn[i] = j * Math.PI / lutsize;
        }

        order = nbands - 1;
        var = factorial(order);
        consta = Math.pow(2.0, (2.0 * order)) * var * var / (nbands * factorial(2 * order));
        consta = Math.sqrt(consta);
        ycosn = new double[xcosn.length];

        for (i = 0; i < xcosn.length; i++) {
            ycosn[i] = consta * Math.pow(Math.cos(xcosn[i]), order);
        }

        bands = new double[nbands][imx * imy];
        bind = new int[nbands][2];

        var = 1.0 / Math.sqrt(nbands);
        bandfftr = new double[imx * imy];
        bandffti = new double[imx * imy];

        for (b = 1; b <= nbands; b++) {
            anglemask = pointOp(angle, ycosn, (xcosn[0] + (Math.PI * (b - 1) / (double) nbands)), (xcosn[1] - xcosn[0]),
                                true);
            maskr = new double[imy][imx];
            maski = new double[imy][imx];

            if (((nbands - 1) % 4) == 0) {

                for (j = 0; j < imy; j++) {

                    for (i = 0; i < imx; i++) {
                        maskr[j][i] = anglemask[j][i] * hi0mask[j][i];
                    }
                }
            } // if (((nbands-1) % 4) == 0)
            else if (((nbands - 1) % 4) == 1) {

                for (j = 0; j < imy; j++) {

                    for (i = 0; i < imx; i++) {
                        maski[j][i] = -anglemask[j][i] * hi0mask[j][i];
                    }
                }
            } // else if (((nbands-1) %4) == 1)
            else if (((nbands - 1) % 4) == 2) {

                for (j = 0; j < imy; j++) {

                    for (i = 0; i < imx; i++) {
                        maskr[j][i] = -anglemask[j][i] * hi0mask[j][i];
                    }
                }
            } // else if (((nbands-1) %4) == 2)
            else if (((nbands - 1) % 4) == 3) {

                for (j = 0; j < imy; j++) {

                    for (i = 0; i < imx; i++) {
                        maski[j][i] = anglemask[j][i] * hi0mask[j][i];
                    }
                }
            } // else if (((nbands-1) %4) == 3)

            // Make real the contents in the HF cross (to avoid
            // information loss in these frequencies.)
            // It distributes evenly these contents among the nbands orientations

            for (i = 0; i < imx; i++) {
                maskr[0][i] = var;
                maski[0][i] = 0.0;
            }

            for (j = 1; j < imy; j++) {
                maskr[j][0] = var;
                maski[j][0] = 0.0;
            }

            for (j = 0; j < imy; j++) {

                for (i = 0; i < imx; i++) {
                    index = i + (j * imx);
                    bandfftr[index] = (imdftr[index] * maskr[j][i]) - (imdfti[index] * maski[j][i]);
                    bandffti[index] = (imdftr[index] * maski[j][i]) + (imdfti[index] * maskr[j][i]);
                }
            }

            center(bandfftr, bandffti, imx, imy);

            // Inverse FFT
            fftUtil = new FFTUtility(bandfftr, bandffti, imy, imx, 1, +1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(bandfftr, bandffti, 1, imy, imx, +1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();

            for (i = 0; i < bandfftr.length; i++) {
                bands[b - 1][i] = bandfftr[i];
            }

            bind[b - 1][0] = imx;
            bind[b - 1][1] = imy;
        } // for (b = 1; b <= nbands; b++)

        for (i = nbands - 1; i >= 0; i--) {
            pyr.insertElementAt(bands[i], 0);
            pind.insertElementAt(bind[i], 0);
        }

        return pyr;
    }

    /**
     * This is a port of MATLAB routine buildSFpyr.m by Eero Simoncelli, 5/97. Construct a steerable pyramid on array
     * im, in the Fourier domain. This is similar to buildSpyr, except that: + Reconstruction is exact (within floating
     * point errors) + It can produce any number of orientation bands. - Typically slower, especially for
     * non-power-of-two sizes - Boundary handling is circular
     *
     * @param   im      DOCUMENT ME!
     * @param   imx     x dimension of im
     * @param   imy     y dimension of im
     * @param   ht      Specfies the number of pyramid levels to build
     * @param   order   The squared radial functions tile the Fourier plane, with a raised-cosine falloff. Angular
     *                  functions are cos(theta - k/PI/(order +1))^order. order is one less than the number of
     *                  orientation bands. default = 3
     * @param   twidth  The width of the transition region of the radial lowpass function in octaves. (default = 1,
     *                  which gives a raised cosine for the bandpass filters).
     * @param   pind    An N by 2 matrix containing the sizes of each subband.
     *
     * @return  pyr A vector containing the N pyramid subbands, ordered from fine to coarse
     */
    private Vector buildSFpyr(double[] im, int imx, int imy, int ht, int order, double twidth, Vector pind) {
        int max_ht;
        int nbands;
        int ctrx;
        int ctry;
        double[][] xramp;
        double[][] yramp;
        double[][] angle;
        double[][] log_rad;
        int i, j;
        int index;
        double value;
        double logC;
        double[] xrcos = null;
        double[] yrcos = null;
        double[] yircos;
        double[][] lo0mask = null;
        FFTUtility fftUtil;
        double[] imdftr;
        double[] imdfti;
        double[] lo0dftr;
        double[] lo0dfti;
        double[][] hi0mask;
        double[] hi0dftr;
        double[] hi0dfti;
        Vector pyr;
        int[] intMat;

        // log2(x) = loge(x)/loge(2)
        logC = 1.0 / Math.log(2.0);
        max_ht = (int) Math.floor((logC * Math.log(Math.min(imx, imy))) + 2);

        if (ht > max_ht) {
            MipavUtil.displayError("Error! Cannot build pyramid higher than " + max_ht + " levels");
            error = 1;

            return null;
        }

        if ((order > 15) || (order < 0)) {
            MipavUtil.displayWarning("Warning: ORDER must be an integer in the range 0 to 15");

            if (order < 0) {
                MipavUtil.displayWarning("Setting order to 0");
                order = 0;
            } else if (order > 15) {
                MipavUtil.displayWarning("Setting order to 15");
                order = 15;
            }
        } // if ((order > 15) || (order < 0))

        nbands = order + 1;

        if (twidth <= 0) {
            MipavUtil.displayWarning("Warning: TWIDTH must be positive.  Setting to 1");
            twidth = 1;
        }

        ctrx = (int) Math.ceil((imx + 0.5) / 2.0);
        ctry = (int) Math.ceil((imy + 0.5) / 2.0);
        xramp = new double[imy][imx];
        yramp = new double[imy][imx];

        for (i = 0; i < imx; i++) {
            value = 2.0 * ((i + 1) - ctrx) / imx;

            for (j = 0; j < imy; j++) {
                xramp[j][i] = value;
            }
        }

        for (j = 0; j < imy; j++) {
            value = 2.0 * ((j + 1) - ctry) / imy;

            for (i = 0; i < imx; i++) {
                yramp[j][i] = value;
            }
        }

        angle = new double[imy][imx];
        log_rad = new double[imy][imx];

        for (j = 0; j < imy; j++) {

            for (i = 0; i < imx; i++) {
                angle[j][i] = Math.atan2(yramp[j][i], xramp[j][i]);
                log_rad[j][i] = Math.sqrt((xramp[j][i] * xramp[j][i]) + (yramp[j][i] * yramp[j][i]));
            }
        }

        log_rad[ctry - 1][ctrx - 1] = log_rad[ctry - 1][ctrx - 2];

        // log2(log_rad) = loge(rad)/loge(2)
        for (j = 0; j < imy; j++) {

            for (i = 0; i < imx; i++) {
                log_rad[j][i] = logC * Math.log(log_rad[j][i]);
            }
        }

        // Radial transition function (a raised cosine in log-frequency)
        xrcos = new double[259];
        yrcos = new double[259];
        rcosFn(twidth, (-twidth / 2.0), 0.0, 1.0, xrcos, yrcos);

        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }

        yircos = new double[yrcos.length];

        for (i = 0; i < yrcos.length; i++) {
            value = yrcos[i];
            yircos[i] = Math.sqrt(1.0 - (value * value));
        }

        lo0mask = pointOp(log_rad, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);

        // forward FFT
        imdftr = new double[imx * imy];

        for (i = 0; i < im.length; i++) {
            imdftr[i] = im[i];
        }

        imdfti = new double[imx * imy];
        fftUtil = new FFTUtility(imdftr, imdfti, imy, imx, 1, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(imdftr, imdfti, 1, imy, imx, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        center(imdftr, imdfti, imx, imy);
        lo0dftr = new double[imx * imy];
        lo0dfti = new double[imx * imy];

        for (j = 0; j < imy; j++) {

            for (i = 0; i < imx; i++) {
                index = i + (j * imx);
                lo0dftr[index] = imdftr[index] * lo0mask[j][i];
                lo0dfti[index] = imdfti[index] * lo0mask[j][i];
            }
        }

        pyr = buildSFpyrLevs(lo0dftr, lo0dfti, imx, imy, log_rad, xrcos, yrcos, angle, ht, nbands, pind);

        hi0mask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);
        hi0dftr = new double[imx * imy];
        hi0dfti = new double[imx * imy];

        for (j = 0; j < imy; j++) {

            for (i = 0; i < imx; i++) {
                index = i + (j * imx);
                hi0dftr[index] = imdftr[index] * hi0mask[j][i];
                hi0dfti[index] = imdfti[index] * hi0mask[j][i];
            }
        }

        center(hi0dftr, hi0dfti, imx, imy);

        // Inverse FFT
        fftUtil = new FFTUtility(hi0dftr, hi0dfti, imy, imx, 1, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(hi0dftr, hi0dfti, 1, imy, imx, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();

        pyr.insertElementAt(hi0dftr, 0);
        intMat = new int[2];
        intMat[0] = imx;
        intMat[1] = imy;
        pind.insertElementAt(intMat, 0);

        return pyr;
    }

    /**
     * This is a port of MATLAB function buildSFpyrLevs.m written by Eero Simoncelli, 5/97. Recursive function for
     * constructing levels of a steerable pyramid. This is called by buildSFpyr, and is usually not called directly.
     *
     * @param   lodftr_orig  DOCUMENT ME!
     * @param   lodfti_orig  DOCUMENT ME!
     * @param   lodx         DOCUMENT ME!
     * @param   lody         DOCUMENT ME!
     * @param   lograd_orig  DOCUMENT ME!
     * @param   xrcos        DOCUMENT ME!
     * @param   yrcos        DOCUMENT ME!
     * @param   angle_orig   DOCUMENT ME!
     * @param   ht           DOCUMENT ME!
     * @param   nbands       DOCUMENT ME!
     * @param   pind         DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector buildSFpyrLevs(double[] lodftr_orig, double[] lodfti_orig, int lodx, int lody,
                                  double[][] lograd_orig, double[] xrcos, double[] yrcos, double[][] angle_orig, int ht,
                                  int nbands, Vector pind) {
        FFTUtility fftUtil;
        double[][] bands;
        int[][] bind;
        int logx;
        int logy;
        int i, j;
        int lutsize;
        double[] xcosn;
        int order;
        double consta;
        double[] ycosn;
        double[][] himask;
        int b;
        double[][] anglemask;
        double[] banddftr;
        double[] banddfti;
        int index;
        int ctrx;
        int ctry;
        int lodimsx;
        int lodimsy;
        int loctrx;
        int loctry;
        int lostartx;
        int lostarty;
        int loendx;
        int loendy;
        double[][] temp;
        double[] yircos;
        double[][] lomask;
        Vector pyr = new Vector();
        Vector npyr;
        Vector nind = new Vector();
        logy = lograd_orig.length;
        logx = lograd_orig[0].length;

        double[][] lograd = new double[logy][logx];
        int angley = angle_orig.length;
        int anglex = angle_orig[0].length;
        double[][] angle = new double[angley][anglex];
        double[] lodftr = new double[lodftr_orig.length];
        double[] lodfti = new double[lodfti_orig.length];
        double p, q;
        int[] intMat;

        for (j = 0; j < logy; j++) {

            for (i = 0; i < logx; i++) {
                lograd[j][i] = lograd_orig[j][i];
            }
        }

        for (j = 0; j < angley; j++) {

            for (i = 0; i < anglex; i++) {
                angle[j][i] = angle_orig[j][i];
            }
        }

        for (i = 0; i < lodftr.length; i++) {
            lodftr[i] = lodftr_orig[i];
            lodfti[i] = lodfti_orig[i];
        }

        if (ht <= 0) {
            center(lodftr, lodfti, lodx, lody);

            // Inverse FFT
            fftUtil = new FFTUtility(lodftr, lodfti, lody, lodx, 1, +1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(lodftr, lodfti, 1, lody, lodx, +1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            pyr.removeAllElements();
            pyr.add(lodftr);
            pind.removeAllElements();
            intMat = new int[2];
            intMat[0] = lodx;
            intMat[1] = lody;
            pind.add(intMat);
        } // if (ht <= 0)
        else {
            bands = new double[nbands][lodx * lody];
            bind = new int[nbands][2];

            for (j = 0; j < logy; j++) {

                for (i = 0; i < logx; i++) {
                    lograd[j][i] = lograd[j][i] + 1;
                }
            } // for (j = 0; j < logy; j++)

            lutsize = 1024;
            xcosn = new double[(3 * lutsize) + 3];

            for (i = 0, j = -((2 * lutsize) + 1); i < ((3 * lutsize) + 3); i++, j++) {
                xcosn[i] = (Math.PI * j) / lutsize;
            }

            order = nbands - 1;

            // Divide by sqrt(sum_(n=0)^(N-1) cos(pi*n/N)^(2(N-1)) )
            p = factorial(order);
            q = factorial(2 * order);
            consta = Math.pow(2.0, (2.0 * order)) * (p * p) / (nbands * q);
            ycosn = new double[(3 * lutsize) + 3];
            consta = Math.sqrt(consta);

            for (i = 0; i < ycosn.length; i++) {
                ycosn[i] = consta * Math.pow(Math.cos(xcosn[i]), order);
            }

            himask = pointOp(lograd, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);

            banddftr = new double[lodx * lody];
            banddfti = new double[lodx * lody];

            for (b = 1; b <= nbands; b++) {
                anglemask = pointOp(angle, ycosn, (xcosn[0] + (Math.PI * (b - 1) / (double) nbands)),
                                    (xcosn[1] - xcosn[0]), true);

                if (((nbands - 1) % 4) == 0) {

                    for (j = 0; j < lody; j++) {

                        for (i = 0; i < lodx; i++) {
                            index = i + (j * lodx);
                            banddftr[index] = lodftr[index] * anglemask[j][i] * himask[j][i];
                            banddfti[index] = lodfti[index] * anglemask[j][i] * himask[j][i];
                        }
                    }
                } // if (((nbands-1) % 4) == 0)
                else if (((nbands - 1) % 4) == 1) {

                    for (j = 0; j < lody; j++) {

                        for (i = 0; i < lodx; i++) {
                            index = i + (j * lodx);
                            banddfti[index] = -lodftr[index] * anglemask[j][i] * himask[j][i];
                            banddftr[index] = lodfti[index] * anglemask[j][i] * himask[j][i];
                        }
                    }
                } // else if (((nbands-1) % 4) == 1)
                else if (((nbands - 1) % 4) == 2) {

                    for (j = 0; j < lody; j++) {

                        for (i = 0; i < lodx; i++) {
                            index = i + (j * lodx);
                            banddftr[index] = -lodftr[index] * anglemask[j][i] * himask[j][i];
                            banddfti[index] = -lodfti[index] * anglemask[j][i] * himask[j][i];
                        }
                    }
                } // else if (((nbands-1) % 4) == 2)
                else if (((nbands - 1) % 4) == 3) {

                    for (j = 0; j < lody; j++) {

                        for (i = 0; i < lodx; i++) {
                            index = i + (j * lodx);
                            banddfti[index] = lodftr[index] * anglemask[j][i] * himask[j][i];
                            banddftr[index] = -lodfti[index] * anglemask[j][i] * himask[j][i];
                        }
                    }
                } // else if (((nbands-1) % 4) == 3)

                center(banddftr, banddfti, lodx, lody);

                // Inverse FFT
                fftUtil = new FFTUtility(banddftr, banddfti, lody, lodx, 1, +1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(banddftr, banddfti, 1, lody, lodx, +1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();

                for (i = 0; i < banddftr.length; i++) {
                    bands[b - 1][i] = banddftr[i];
                }

                bind[b - 1][0] = lodx;
                bind[b - 1][1] = lody;
            } // for (b = 1; b <= nbands; b++)

            ctrx = (int) Math.ceil((lodx + 0.5) / 2.0);
            ctry = (int) Math.ceil((lody + 0.5) / 2.0);
            lodimsx = (int) Math.ceil((lodx - 0.5) / 2.0);
            lodimsy = (int) Math.ceil((lody - 0.5) / 2.0);
            loctrx = (int) Math.ceil((lodimsx + 0.5) / 2.0);
            loctry = (int) Math.ceil((lodimsy + 0.5) / 2.0);
            lostartx = ctrx - loctrx + 1;
            lostarty = ctry - loctry + 1;
            loendx = lostartx + lodimsx - 1;
            loendy = lostarty + lodimsy - 1;

            temp = new double[loendy - lostarty + 1][loendx - lostartx + 1];

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    temp[j][i] = lograd[lostarty - 1 + j][lostartx - 1 + i];
                }
            }

            lograd = new double[loendy - lostarty + 1][loendx - lostartx + 1];

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    lograd[j][i] = temp[j][i];
                }
            }

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    temp[j][i] = angle[lostarty - 1 + j][lostartx - 1 + i];
                }
            }

            angle = new double[loendy - lostarty + 1][loendx - lostartx + 1];

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    angle[j][i] = temp[j][i];
                }
            }

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    index = (lostartx - 1 + i) + (lodx * (lostarty - 1 + j));
                    temp[j][i] = lodftr[index];
                }
            }

            lodftr = new double[(loendx - lostartx + 1) * (loendy - lostarty + 1)];

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    index = i + (j * (loendx - lostartx + 1));
                    lodftr[index] = temp[j][i];
                }
            }

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    index = (lostartx - 1 + i) + (lodx * (lostarty - 1 + j));
                    temp[j][i] = lodfti[index];
                }
            }

            lodfti = new double[(loendx - lostartx + 1) * (loendy - lostarty + 1)];

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    index = i + (j * (loendx - lostartx + 1));
                    lodfti[index] = temp[j][i];
                }
            }

            yircos = new double[yrcos.length];

            for (i = 0; i < yrcos.length; i++) {
                yircos[i] = Math.abs(Math.sqrt(1.0 - (yrcos[i] * yrcos[i])));
            }

            lomask = pointOp(lograd, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);

            for (j = 0; j < (loendy - lostarty + 1); j++) {

                for (i = 0; i < (loendx - lostartx + 1); i++) {
                    index = i + (j * (loendx - lostartx + 1));
                    lodftr[index] = lomask[j][i] * lodftr[index];
                    lodfti[index] = lomask[j][i] * lodfti[index];
                }
            }

            npyr = buildSFpyrLevs(lodftr, lodfti, (loendx - lostartx + 1), (loendy - lostarty + 1), lograd, xrcos,
                                  yrcos, angle, ht - 1, nbands, nind);

            pyr.removeAllElements();

            for (i = 0; i < nbands; i++) {
                pyr.add(bands[i]);
            }

            pyr.addAll(npyr);
            pind.removeAllElements();

            for (i = 0; i < nbands; i++) {
                pind.add(bind[i]);
            }

            pind.addAll(nind);
        } // else

        return pyr;
    }

    /**
     * This is a port of buildWpyr.m by Eero Simoncelli, 6/96. Construct a separable orthonormal QMF/wavelet pyramid on
     * matrix im.
     *
     * @param   im     DOCUMENT ME!
     * @param   imx    DOCUMENT ME!
     * @param   imy    DOCUMENT ME!
     * @param   ht     Specifies the number of pyramid levels to build. default is maxPyrHt(im, filt).
     * @param   filt   Filter can be of even or odd length, but should be symmetric
     * @param   edges  Specifies edge-handling, and defaults to REFLECT1.
     * @param   pind   N size 2 element int[] arrays, containing the sizes of each subband.
     *
     * @return  DOCUMENT ME!
     */
    private Vector buildWpyr(double[] im, int imx, int imy, int ht, int filt, int edges, Vector pind) {

        // pyr is a vector containing the N pyramid subbands, ordered from fine to coarse.
        Vector pyr = new Vector();
        double[] kernel;
        double[] hkernel;
        int stag;
        int max_ht;
        int x;
        int y;
        int[] intMat;
        int[] intMat2;
        int[] intMat3;
        double[] lolo = null;
        double[] hihi;
        double[] lo;
        double[] hi;
        double[] lohi = null;
        double[] hilo = null;
        int[] lox = new int[1];
        int[] loy = new int[1];
        int[] hix = new int[1];
        int[] hiy = new int[1];
        int[] lolox = new int[1];
        int[] loloy = new int[1];
        int[] hihix = new int[1];
        int[] hihiy = new int[1];
        int[] lohix = new int[1];
        int[] lohiy = new int[1];
        int[] hilox = new int[1];
        int[] hiloy = new int[1];
        Vector nind = new Vector();
        Vector npyr;

        kernel = namedFilter(filt, 0);

        hkernel = modulateFlip(kernel);

        // Stagger sampling if filter is odd-length:
        if ((kernel.length % 2) == 0) {
            stag = 2;
        } else {
            stag = 1;
        }

        // Compute maximum pyramid height for given image and filter size.
        // Specifically: the number of corrDn operations that can be sequentially
        // performed when subsampling by a factor of 2;
        max_ht = 0;
        x = imx;
        y = imy;

        while ((x >= kernel.length) && (y >= kernel.length)) {
            max_ht++;
            x = (int) Math.floor(x / 2.0);
            y = (int) Math.floor(y / 2.0);
        }

        if (ht > max_ht) {
            MipavUtil.displayError("Cannot build pyramid higher than " + max_ht + " levels");
            error = 1;

            return null;
        }

        if (ht <= 0) {

            if (pyr != null) {
                pyr.removeAllElements();
            }

            if (pyr == null) {
                pyr = new Vector();
            }

            pyr.add(im);
            intMat = new int[2];
            intMat[0] = imx;
            intMat[1] = imy;

            if (pind != null) {
                pind.removeAllElements();
            }

            if (pind == null) {
                pind = new Vector();
            }

            pind.add(intMat);
        } // if (ht <= 0)
        else {

            if (imy == 1) {
                lolo = corrDn(im, imx, imy, kernel, kernel.length, 1, edges, 2, 1, stag, 1, lolox, loloy);
                hihi = corrDn(im, imx, imy, hkernel, hkernel.length, 1, edges, 2, 1, 2, 1, hihix, hihiy);
            } else if (imx == 1) {
                lolo = corrDn(im, imx, imy, kernel, 1, kernel.length, edges, 1, 2, 1, stag, lolox, loloy);
                hihi = corrDn(im, imx, imy, hkernel, 1, hkernel.length, edges, 1, 2, 1, 2, hihix, hihiy);
            } else {
                lo = corrDn(im, imx, imy, kernel, kernel.length, 1, edges, 2, 1, stag, 1, lox, loy);
                hi = corrDn(im, imx, imy, hkernel, hkernel.length, 1, edges, 2, 1, 2, 1, hix, hiy);
                lolo = corrDn(lo, lox[0], loy[0], kernel, 1, kernel.length, edges, 1, 2, 1, stag, lolox, loloy);

                // horizontal
                lohi = corrDn(hi, hix[0], hiy[0], kernel, 1, kernel.length, edges, 1, 2, 1, stag, lohix, lohiy);

                // vertical
                hilo = corrDn(lo, lox[0], loy[0], hkernel, 1, hkernel.length, edges, 1, 2, 1, 2, hilox, hiloy);

                // diagonal
                hihi = corrDn(hi, hix[0], hiy[0], hkernel, 1, hkernel.length, edges, 1, 2, 1, 2, hihix, hihiy);
            }

            npyr = buildWpyr(lolo, lolox[0], loloy[0], ht - 1, filt, edges, nind);

            if ((imx == 1) || (imy == 1)) {

                if (pyr != null) {
                    pyr.removeAllElements();
                }

                if (pyr == null) {
                    pyr = new Vector();
                }

                pyr.add(hihi);
                pyr.addAll(npyr);

                if (pind != null) {
                    pind.removeAllElements();
                }

                if (pind == null) {
                    pind = new Vector();
                }

                intMat = new int[2];
                intMat[0] = hihix[0];
                intMat[1] = hihiy[0];
                pind.add(intMat);
                pind.addAll(nind);
            } else {

                if (pyr != null) {
                    pyr.removeAllElements();
                }

                if (pyr == null) {
                    pyr = new Vector();
                }

                pyr.add(lohi);
                pyr.add(hilo);
                pyr.add(hihi);
                pyr.addAll(npyr);

                if (pind != null) {
                    pind.removeAllElements();
                }

                if (pind == null) {
                    pind = new Vector();
                }

                intMat = new int[2];
                intMat[0] = lohix[0];
                intMat[1] = lohiy[0];
                pind.add(intMat);
                intMat2 = new int[2];
                intMat2[0] = hilox[0];
                intMat2[1] = hiloy[0];
                pind.add(intMat2);
                intMat3 = new int[2];
                intMat3[0] = hihix[0];
                intMat3[1] = hihiy[0];
                pind.add(intMat3);

                if (nind != null) {
                    pind.addAll(nind);
                }
            }
        } // else

        return pyr;
    }

    /**
     * This is a port of buildWUpyr.m by JPM, Univ. de Granada, 3/2003 Construct a separable undecimated orthonormal
     * QMF/wavelet pyramid on matrix (or vector) im
     *
     * @param   im         DOCUMENT ME!
     * @param   imx        First dimension of im
     * @param   imy        Second dimension of im
     * @param   nScales    The number of pyramid levels to build
     * @param   daubOrder  The order of the daubechies wavelet filter used
     * @param   pind       N 2-element int[] array containing the sizes of each subband
     *
     * @return  A vector containing the N pyramid subbands, ordered from fine to coarse.
     */
    private Vector buildWUpyr(double[] im, int imx, int imy, int nScales, int daubOrder, Vector pind) {
        Vector pyr = new Vector();
        double[] dummy = new double[1];
        double[] h;
        double[][] lpr = null;
        double[][] yh = null;
        int[][] pArr = null;
        int i;
        int nband;
        int nsc;
        int nor;
        double[] band;
        double[] oldband = null;
        int newx;
        int newy;
        int bandx;
        int bandy;
        int j;
        int index;
        int sh;
        int twoPow;
        int[] intArr = new int[2];
        double[] bandi = null;
        int[] intMat;
        double[] h_1 = new double[daubOrder];

        if (nScales < 1) {
            MipavUtil.displayError("Number of scales must be >= 1");
            error = 1;

            return null;
        }

        // Fixed number of orientations
        nOrientations = 3;
        h = daubcqf(daubOrder, MINIMUM_PHASE, h_1);

        if (error == 1) {
            return null;
        }

        // Performs the decomposition
        lpr = new double[imy][imx];

        if (Math.min(imx, imy) == 1) {
            yh = new double[imy][(nScales + 1) * imx];
        } else {
            yh = new double[imy][3 * (nScales + 1) * imx];
        }

        mrdwt(im, imx, imy, h, 1, h.length, nScales + 1, lpr, yh);

        if (error == 1) {
            return null;
        }

        // Reorganize the output, forcing the same format as with buildFullSFpyr2
        pyr.add(dummy);

        if (pind != null) {
            pind.removeAllElements();
        }

        // Room for a "virtual" high pass residual, for compatibility
        pArr = new int[((nScales + 1) * nOrientations) + 2][2];

        for (i = 0; i < pArr.length; i++) {
            pind.add(pArr[i]);
        }

        nband = 1;

        for (nsc = 1; nsc <= (nScales + 1); nsc++) {

            for (nor = 1; nor <= nOrientations; nor++) {
                nband = nband + 1;
                bandx = imx;
                bandy = yh.length;
                band = new double[yh.length * imx];

                for (j = 0, index = 0; j < yh.length; j++) {

                    for (i = (nband - 2) * imx; i < ((nband - 1) * imx); i++) {
                        band[index++] = yh[j][i];
                    }
                }

                twoPow = 1;

                for (i = 0; i < nsc; i++) {
                    twoPow *= 2;
                }

                // Approximate phase compensation
                sh = ((daubOrder / 2) - 1) * twoPow;

                if (nor == 1) { // horizontal
                    twoPow = 1;

                    for (i = 0; i < (nsc - 1); i++) {
                        twoPow *= 2;
                    }

                    // Use x y order rather than original code y x order
                    intArr[0] = twoPow;
                    intArr[1] = sh;
                    shiftReal(band, imx, yh.length, intArr, band);
                } // if (nor == 1)
                else if (nor == 2) { // vertical
                    twoPow = 1;

                    for (i = 0; i < (nsc - 1); i++) {
                        twoPow *= 2;
                    }

                    intArr[0] = sh;
                    intArr[1] = twoPow;
                    shiftReal(band, imx, yh.length, intArr, band);
                } // else if (nor == 2)
                else { // diagonal
                    intArr[0] = sh;
                    intArr[1] = sh;
                    shiftReal(band, imx, yh.length, intArr, band);
                } // else

                if (nsc > 2) {

                    // The low frequency bands are shrunk in the frequency domain
                    twoPow = 1;

                    for (i = 0; i < (nsc - 2); i++) {
                        twoPow *= 2;
                    }

                    oldband = new double[band.length];

                    for (i = 0; i < band.length; i++) {
                        oldband[i] = band[i];
                    }

                    newx = bandx / twoPow;
                    newy = bandy / twoPow;
                    band = new double[newx * newy];
                    bandi = new double[newx * newy];
                    shrink(oldband, bandx, bandy, twoPow, band, bandi);
                    bandx = newx;
                    bandy = newy;
                } // if (nsc > 2)

                pyr.add(band);
                intMat = new int[2];
                intMat[0] = bandx;
                intMat[1] = bandy;
                pind.setElementAt(intMat, nband - 1);
            } // for (nor = 1; nor <= nOrientations; nor++)
        } // for (nsc = 1; nsc <= nScales+1; nsc++)

        bandy = lpr.length;
        bandx = lpr[0].length;
        band = new double[bandy * bandx];

        for (j = 0, index = 0; j < bandy; j++) {

            for (i = 0; i < bandx; i++) {
                band[index++] = lpr[j][i];
            }
        }

        twoPow = 1;

        for (i = 0; i < nScales; i++) {
            twoPow *= 2;
        }

        oldband = new double[band.length];

        for (i = 0; i < band.length; i++) {
            oldband[i] = band[i];
        }

        newx = bandx / twoPow;
        newy = bandy / twoPow;
        band = new double[newx * newy];
        bandi = new double[newx * newy];
        shrink(oldband, bandx, bandy, twoPow, band, bandi);
        bandx = newx;
        bandy = newy;
        pyr.add(band);
        intMat = new int[2];
        intMat[0] = bandx;
        intMat[1] = bandy;
        pind.setElementAt(intMat, nband);

        return pyr;
    }


    /**
     * Centers the FFT for display purposes.
     *
     * @param  rData  real data buffer
     * @param  iData  imaginary data buffer
     * @param  xDim   DOCUMENT ME!
     * @param  yDim   DOCUMENT ME!
     */
    private void center(double[] rData, double[] iData, int xDim, int yDim) {

        // center() is called after the forward fast fourier transform to enhance the display
        // center() is called before the inverse fast fourier transform to return the data
        // to its original ordering.
        int i, j;
        int xnew, ynew;
        int xdimHalf, ydimHalf;
        double[] centerData;
        int length = xDim * yDim;

        try {
            centerData = new double[length]; // Temp storage for centered
        } catch (OutOfMemoryError e) {
            centerData = null;
            System.gc();
            displayError("AlgorithmDenoisingBLS_GSM: Out of memory creating centerData");
            setCompleted(false);

            return;
        }

        xdimHalf = xDim / 2;
        ydimHalf = yDim / 2;

        for (j = 0; j < ydimHalf; j++) {

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                ynew = j + ydimHalf;
                centerData[(xDim * ynew) + xnew] = rData[(xDim * j) + i];
            }
        }

        for (j = ydimHalf; j < yDim; j++) {

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                ynew = j - ydimHalf;
                centerData[(xDim * ynew) + xnew] = rData[(xDim * j) + i];
            }
        }

        for (j = ydimHalf; j < yDim; j++) {

            for (i = xdimHalf; i < xDim; i++) {

                xnew = i - xdimHalf;
                ynew = j - ydimHalf;
                centerData[(xDim * ynew) + xnew] = rData[(xDim * j) + i];
            }
        }

        for (j = 0; j < ydimHalf; j++) {

            for (i = xdimHalf; i < xDim; i++) {

                xnew = i - xdimHalf;
                ynew = j + ydimHalf;
                centerData[(xDim * ynew) + xnew] = rData[(xDim * j) + i];
            }
        }

        for (i = 0; i < length; i++) {
            rData[i] = centerData[i];
        }

        for (j = 0; j < ydimHalf; j++) {

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                ynew = j + ydimHalf;
                centerData[(xDim * ynew) + xnew] = iData[(xDim * j) + i];
            }
        }

        for (j = ydimHalf; j < yDim; j++) {

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                ynew = j - ydimHalf;
                centerData[(xDim * ynew) + xnew] = iData[(xDim * j) + i];
            }
        }

        for (j = ydimHalf; j < yDim; j++) {

            for (i = xdimHalf; i < xDim; i++) {

                xnew = i - xdimHalf;
                ynew = j - ydimHalf;
                centerData[(xDim * ynew) + xnew] = iData[(xDim * j) + i];
            }
        }

        for (j = 0; j < ydimHalf; j++) {

            for (i = xdimHalf; i < xDim; i++) {

                xnew = i - xdimHalf;
                ynew = j + ydimHalf;
                centerData[(xDim * ynew) + xnew] = iData[(xDim * j) + i];
            }
        }

        for (i = 0; i < length; i++) {
            iData[i] = centerData[i];
        }


    } // center

    /**
     * 1D convolution.
     *
     * @param   s1  DOCUMENT ME!
     * @param   s2  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] conv(double[] s1, double[] s2) {
        int j;
        int k;
        double[] kernel = new double[s1.length + s2.length - 1];

        for (k = 0; k < kernel.length; k++) {

            for (j = Math.max(0, k + 1 - s2.length); j <= Math.min(k, s1.length - 1); j++) {
                kernel[k] += s1[j] * s2[k - j];
            }
        }

        return kernel;
    }

    /**
     * This is a port of corrDn.c by EPS, 7/96 Compute correlation of matrices image with filt, followed by
     * downsampling. These arguments should be 1D or 2D matrices, and image must be larger (in both dimensions) than
     * filt. The origin of filt is assumed to be floor(size(filt)/2) + 1
     *
     * @param   image   DOCUMENT ME!
     * @param   xdim    first dimension of image
     * @param   ydim    second dimension of image
     * @param   filt    DOCUMENT ME!
     * @param   xfdim   first dimension of filt
     * @param   yfdim   second dimension of filt
     * @param   edges   Determines boundary handling CIRCULAR = Circular convolution REFLECT1 = Reflect about the edge
     *                  pixels REFLECT2 = Reflect, doubling the edge pixels REPEAT = Repeat the edge pixels ZERO =
     *                  Assume values of zero outside the image boundary EXTEND = Reflect and invert DONT_COMPUTE = Zero
     *                  output when filter overhangs input boundaries Downsampling factors are determined by xstep and
     *                  ystep
     * @param   xstep   DOCUMENT ME!
     * @param   ystep   start and stop detrmine the window over which convolution occurs
     * @param   xstart  DOCUMENT ME!
     * @param   ystart  DOCUMENT ME!
     * @param   xrdim   first dimension of result
     * @param   yrdim   second dimension of result Note: This operation corresponds to multiplication of a signal vector
     *                  by a matrix whose rows contain copies of the filt shifted by multiples of step. See upconv for
     *                  the operation corresponding to the transpose of this matrix.
     *
     * @return  DOCUMENT ME!
     */
    private double[] corrDn(double[] image, int xdim, int ydim, double[] filt, int xfdim, int yfdim, int edges,
                            int xstep, int ystep, int xstart, int ystart, int[] xrdim, int[] yrdim) {
        double[] result = null;
        int xstop;
        int ystop;
        double[] temp;

        xstart--;
        ystart--;
        xstop = xdim;
        ystop = ydim;
        xrdim[0] = (xstop - xstart + xstep - 1) / xstep;
        yrdim[0] = (ystop - ystart + ystep - 1) / ystep;
        result = new double[xrdim[0] * yrdim[0]];

        if (edges == CIRCULAR) {
            internal_wrap_reduce(image, xdim, ydim, filt, xfdim, yfdim, xstart, xstep, xstop, ystart, ystep, ystop,
                                 result);
        } else {
            temp = new double[xfdim * yfdim];
            internal_reduce(image, xdim, ydim, filt, temp, xfdim, yfdim, xstart, xstep, xstop, ystart, ystep, ystop,
                            result, edges);
        } // else

        return result;
    }

    /**
     * This is a port of daubcqf.m by Ramesh Gopinath % [h_0,h_1] = daubcqf(N,TYPE); % % Function computes the
     * Daubechies' scaling and wavelet filters % (normalized to sqrt(2)). % % Input: % N : Length of filter (must be
     * even) % TYPE : Optional parameter that distinguishes the minimum phase, % maximum phase and mid-phase solutions
     * ('min', 'max', or % 'mid'). If no argument is specified, the minimum phase % solution is used. % % Output: % h_0
     * : Minimal phase Daubechies' scaling filter % h_1 : Minimal phase Daubechies' wavelet filter % % Example: % N = 4;
     * % TYPE = 'min'; % [h_0,h_1] = daubcqf(N,TYPE) % h_0 = 0.4830 0.8365 0.2241 -0.1294 % h_1 = 0.1294 0.2241 -0.8365
     * 0.4830 % % Reference: "Orthonormal Bases of Compactly Supported Wavelets", % CPAM, Oct.89 % %File Name: daubcqf.m
     * %Last Modification Date: 01/02/96 15:12:57 %Current Version: daubcqf.m 2.4 %File Creation Date: 10/10/88 %Author:
     * Ramesh Gopinath <ramesh@dsp.rice.edu> % %Copyright (c) 2000 RICE UNIVERSITY. All rights reserved. %Created by
     * Ramesh Gopinath, Department of ECE, Rice University. % %This software is distributed and licensed to you on a
     * non-exclusive %basis, free-of-charge. Redistribution and use in source and binary forms, %with or without
     * modification, are permitted provided that the following %conditions are met: % %1. Redistribution of source code
     * must retain the above copyright notice, % this list of conditions and the following disclaimer. %2.
     * Redistribution in binary form must reproduce the above copyright notice, % this list of conditions and the
     * following disclaimer in the % documentation and/or other materials provided with the distribution. %3. All
     * advertising materials mentioning features or use of this software % must display the following acknowledgment:
     * This product includes % software developed by Rice University, Houston, Texas and its contributors. %4. Neither
     * the name of the University nor the names of its contributors % may be used to endorse or promote products derived
     * from this software % without specific prior written permission. % %THIS SOFTWARE IS PROVIDED BY WILLIAM MARSH
     * RICE UNIVERSITY, HOUSTON, TEXAS, %AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, %BUT
     * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS %FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
     * IN NO EVENT SHALL RICE UNIVERSITY %OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
     * %EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, %PROCUREMENT OF SUBSTITUTE GOODS OR
     * SERVICES; LOSS OF USE, DATA, OR PROFITS; %OR BUSINESS INTERRUPTIONS) HOWEVER CAUSED AND ON ANY THEORY OF
     * LIABILITY, %WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR %OTHERWISE), PRODUCT
     * LIABILITY, OR OTHERWISE ARISING IN ANY WAY OUT OF THE %USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
     * OF SUCH DAMAGE. % %For information on commercial licenses, contact Rice University's Office of %Technology
     * Transfer at techtran@rice.edu or (713) 348-6173
     *
     * @param   N     DOCUMENT ME!
     * @param   type  DOCUMENT ME!
     * @param   h_1   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] daubcqf(int N, int type, double[] h_1) {
        int k;
        double a;
        double[] p;
        double[] q;
        double[] h_0;
        double[] qtr;
        double[] qti;
        int j;
        double[] temp;
        int i;
        double[][] A;
        int n;
        EigenvalueDecomposition eig;
        double[] eigenvalue;
        double[] imagvalue;
        int qtl;
        int index;
        double[] cr;
        double[] ci;
        double sum;
        double var;
        boolean haveImag;

        if ((N % 2) == 1) {
            MipavUtil.displayError("No Daubechies filter exists for odd length");
            error = 1;

            return null;
        }

        k = N / 2;
        a = 1.0;
        p = new double[1];
        p[0] = 1.0;
        q = new double[1];
        q[0] = 1.0;
        h_0 = new double[2];
        h_0[0] = 1.0;
        h_0[1] = 1.0;

        for (j = 1; j <= (k - 1); j++) {
            a = -a * 0.25 * (j + k - 1.0) / j;
            temp = new double[h_0.length];

            for (i = 0; i < h_0.length; i++) {
                temp[i] = h_0[i];
            }

            h_0 = new double[temp.length + 1];
            h_0[0] = temp[0];

            for (i = 1; i < (h_0.length - 1); i++) {
                h_0[i] = temp[i - 1] + temp[i];
            }

            h_0[h_0.length - 1] = temp[h_0.length - 2];
            temp = new double[p.length];

            for (i = 0; i < p.length; i++) {
                temp[i] = p[i];
            }

            p = new double[temp.length + 1];
            p[0] = temp[0];

            for (i = 1; i < (p.length - 1); i++) {
                p[i] = temp[i] - temp[i - 1];
            }

            p[p.length - 1] = -temp[p.length - 2];
            temp = new double[p.length];

            for (i = 0; i < p.length; i++) {
                temp[i] = p[i];
            }

            p = new double[temp.length + 1];
            p[0] = temp[0];

            for (i = 1; i < (p.length - 1); i++) {
                p[i] = temp[i] - temp[i - 1];
            }

            p[p.length - 1] = -temp[p.length - 2];
            temp = new double[q.length];

            for (i = 0; i < q.length; i++) {
                temp[i] = q[i];
            }

            q = new double[temp.length + 2];
            q[0] = a * p[0];

            for (i = 1; i < (q.length - 1); i++) {
                q[i] = temp[i - 1] + (a * p[i]);
            }

            q[q.length - 1] = a * p[q.length - 1];
        } // for (j = 1; j <= k-1; j++)

        // Obtain the roots of polynomial of q
        n = q.length - 1;
        A = new double[n][n];

        for (i = 0; i < (n - 1); i++) {
            A[i + 1][i] = 1.0;
        }

        for (j = 0; j < n; j++) {
            A[0][j] = -q[j + 1] / q[0];
        }

        eig = new EigenvalueDecomposition(new Matrix(A));
        eigenvalue = eig.getRealEigenvalues();
        imagvalue = eig.getImagEigenvalues();

        haveImag = false;

        for (i = 0; i < imagvalue.length; i++) {

            if (imagvalue[i] != 0.0) {
                haveImag = true;
            }
        }

        if (!haveImag) {
            Arrays.sort(eigenvalue);
        } else {
            sort(eigenvalue, imagvalue);
        }

        qtr = new double[k - 1];
        qti = new double[k - 1];

        for (i = 0; i < (k - 1); i++) {
            qtr[i] = eigenvalue[i];
            qti[i] = imagvalue[i];
        }

        if (type == MID_PHASE) {

            if ((k % 2) == 1) {
                qtl = 0;

                for (i = 0; i <= (N - 1); i += 4) {
                    qtl++;
                }

                for (i = 1; i <= (N - 1); i += 4) {
                    qtl++;
                }

                qtr = new double[qtl];
                qti = new double[qtl];

                for (i = 0, index = 0; i <= (N - 1); i += 4) {
                    qtr[index] = eigenvalue[i];
                    qti[index++] = imagvalue[i];
                }

                for (i = 1; i <= (N - 1); i += 4) {
                    qtr[index] = eigenvalue[i];
                    qti[index++] = imagvalue[i];
                }
            } else {
                qtl = 1;

                for (i = 3; i <= (k - 2); i += 4) {
                    qtl++;
                }

                for (i = 4; i <= (k - 2); i += 4) {
                    qtl++;
                }

                for (i = N - 4; i >= (k - 1); i -= 4) {
                    qtl++;
                }

                for (i = N - 5; i >= (k - 1); i -= 4) {
                    qtl++;
                }

                qtr = new double[qtl];
                qti = new double[qtl];
                index = 0;
                qtr[index] = eigenvalue[0];
                qti[index++] = imagvalue[0];

                for (i = 3; i <= (k - 2); i += 4) {
                    qtr[index] = eigenvalue[i];
                    qti[index++] = imagvalue[i];
                }

                for (i = 4; i <= (k - 2); i += 4) {
                    qtr[index] = eigenvalue[i];
                    qti[index++] = imagvalue[i];
                }

                for (i = N - 4; i >= (k - 1); i -= 4) {
                    qtr[index] = eigenvalue[i];
                    qti[index++] = imagvalue[i];
                }

                for (i = N - 5; i >= (k - 1); i -= 4) {
                    qtr[index] = eigenvalue[i];
                    qti[index++] = imagvalue[i];
                }
            }
        } // if (type == MID_PHASE)

        // Form a polynomial from the roots in qt
        // The poynomial coefficients are ordered in descending powers
        // The first coefficient for the highest power is 1.
        cr = new double[qtr.length + 1];
        ci = new double[qtr.length + 1];
        cr[0] = 1.0;
        ci[0] = 0.0;

        for (j = 0; j <= (cr.length - 2); j++) {

            for (i = j; i >= 0; i--) {
                cr[i + 1] = cr[i + 1] - (qtr[j] * cr[i]) + (qti[j] * ci[i]);
                ci[i + 1] = ci[i + 1] - (qtr[j] * ci[i]) - (qti[j] * cr[i]);
            }
        }

        h_0 = conv(h_0, cr);

        // Normalize to sqrt(2)
        sum = 0.0;

        for (i = 0; i < h_0.length; i++) {
            sum += h_0[i];
        }

        var = Math.sqrt(2.0) / sum;

        for (i = 0; i < h_0.length; i++) {
            h_0[i] *= var;
        }

        if (type == MAXIMUM_PHASE) {
            temp = new double[h_0.length];

            for (i = 0; i < h_0.length; i++) {
                temp[i] = h_0[h_0.length - 1 - i];
            }

            for (i = 0; i < h_0.length; i++) {
                h_0[i] = temp[i];
            }
        } // if (type == MAXIMUM_PHASE)

        sum = 0.0;

        for (i = 0; i < h_0.length; i++) {
            sum += h_0[i] * h_0[i];
        }

        if ((sum - 1.0) > 1.0e-4) {
            MipavUtil.displayError("Daubcqf numerically unstable for this value of N");
            error = 1;

            return null;
        }

        for (i = 0; i < h_0.length; i++) {
            h_1[i] = h_0[h_0.length - 1 - i];
        }

        for (i = 0; i <= (N - 1); i += 2) {
            h_1[i] = -h_1[i];
        }

        return h_0;
    }

    /**
     * Decompose image into subbands, denoise, and recompose again Port of routine written by Javier Portilla, Univ. de
     * Granada, 11/04
     *
     * @param   fn      DOCUMENT ME!
     * @param   fnx     x dimension of fn
     * @param   fny     y dimension of fn
     * @param   noise   DOCUMENT ME!
     * @param   noisex  x dimension of noise
     * @param   noisey  y dimension of noise
     *
     * @return  DOCUMENT ME!
     */
    private double[] decompReconst(double[] fn, int fnx, int fny, double[] noise, int noisex, int noisey) {
        double[] fh = null;
        Vector pind = new Vector();
        Vector pyr = null;
        Vector pyrN = null;
        Vector pyrh = new Vector();
        int bandNum;
        int nband;
        double[] aux = null;
        double[] oldaux = null;
        int newx;
        int newy;
        double[] auxn = null;
        double[] oldauxn = null;
        int[] nsx = new int[1];
        int[] nsy = new int[1];
        int[] nsxn = new int[1];
        int[] nsyn = new int[1];
        boolean prnt;
        double[][][] BL;
        double[][][] BLn;
        double var;
        int i, j;
        int index;
        double[] imagArray = null;
        double sy2;
        double sn2;
        double SNRin;
        double[] BLT;
        int blx;
        int bly;
        int[] intMat;

        pyr = buildSFpyr(fn, fnx, fny, nScales, nOrientations - 1, 1.0, pind);

        if (error == 1) {
            return null;
        }

        pyrN = buildSFpyr(noise, noisex, noisey, nScales, nOrientations - 1, 1.0, pind);

        if (error == 1) {
            return null;
        }

        pyrh.addAll(pyr);
        bandNum = pind.size();

        for (nband = 1; nband <= (bandNum - 1); nband++) {
            fireProgressStateChanged((100 * nband) / (bandNum - 1));
            aux = pyrBand(pyr, pind, nband - 1, nsx, nsy);
            auxn = pyrBand(pyrN, pind, nband - 1, nsxn, nsyn);
            prnt = useParent && (nband < (bandNum - 1 - nOrientations)) && (nband > 1);

            if (prnt) {
                BL = new double[nsy[0]][nsx[0]][2];
                BLn = new double[nsy[0]][nsx[0]][2];
            } // if (prnt)
            else {
                BL = new double[nsy[0]][nsx[0]][1];
                BLn = new double[nsy[0]][nsx[0]][1];
            } // else

            // Because we are discarding 2 coefficients on every dimension
            var = Math.sqrt(((nsx[0] - 2.0) * (nsy[0] - 2.0)) / (nsx[0] * nsy[0]));

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    index = i + (j * nsx[0]);
                    BL[j][i][0] = aux[index];
                    BLn[j][i][0] = auxn[index] * var;
                }
            }

            if (prnt) {
                aux = pyrBand(pyr, pind, nband + nOrientations - 1, nsxn, nsyn);
                oldaux = new double[aux.length];

                for (i = 0; i < aux.length; i++) {
                    oldaux[i] = aux[i];
                }

                newx = (int) Math.round(2.0 * nsxn[0]);
                newy = (int) Math.round(2.0 * nsyn[0]);
                aux = new double[newx * newy];
                imagArray = new double[newx * newy];
                expand(oldaux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                auxn = pyrBand(pyrN, pind, nband + nOrientations - 1, nsxn, nsyn);
                oldauxn = new double[auxn.length];

                for (i = 0; i < auxn.length; i++) {
                    oldauxn[i] = auxn[i];
                }

                newx = (int) Math.round(2.0 * nsxn[0]);
                newy = (int) Math.round(2.0 * nsyn[0]);
                auxn = new double[newx * newy];
                imagArray = new double[newx * newy];
                expand(oldauxn, 2.0, nsxn[0], nsyn[0], auxn, imagArray);

                for (j = 0; j < nsy[0]; j++) {

                    for (i = 0; i < nsx[0]; i++) {
                        index = i + (j * nsx[0]);
                        BL[j][i][1] = aux[index];
                        BLn[j][i][1] = auxn[index] * var;
                    }
                }
            } // if (prnt)

            sy2 = 0.0;
            sn2 = 0.0;

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    sy2 = sy2 + (BL[j][i][0] * BL[j][i][0]);
                    sn2 = sn2 + (BLn[j][i][0] * BLn[j][i][0]);
                }
            }

            sy2 = sy2 / (nsx[0] * nsy[0]);
            sn2 = sn2 / (nsx[0] * nsy[0]);

            if (sy2 > sn2) {
                SNRin = 4.342944819 * Math.log((sy2 - sn2) / sn2);
                Preferences.debug("SNRin = " + SNRin + "\n");
            } else {
                Preferences.debug("decompReconst: Signal is not detectable in noisy subband\n");
            }

            // main
            BL = denoi_BLS_GSM_band(BL, BLn, prnt);
            bly = BL.length;
            blx = BL[0].length;
            BLT = new double[blx * bly];

            for (j = 0; j < bly; j++) {

                for (i = 0; i < blx; i++) {
                    index = i + (j * blx);
                    BLT[index] = BL[j][i][0];
                }
            }

            pyrh.setElementAt(BLT, nband - 1);
            intMat = new int[2];
            intMat[0] = blx;
            intMat[1] = bly;
            pind.setElementAt(intMat, nband - 1);
        } // for (nband = 1; nband <= bandNum-1; nband++)

        fh = reconSFpyr(pyrh, pind, null, null, 1.0);

        if (error == 1) {
            return null;
        }

        return fh;
    }

    /**
     * This is a port of decomp_reconst_full.m, JPM, Univ. de Granada, 11/04 Decompose image into subbands, denoise, and
     * recompose again Version using the full steerable pyramid (High pass residual splitted into orientations)
     *
     * @param   im      DOCUMENT ME!
     * @param   imx     DOCUMENT ME!
     * @param   imy     DOCUMENT ME!
     * @param   noise   DOCUMENT ME!
     * @param   noisex  DOCUMENT ME!
     * @param   noisey  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] decompReconstFull(double[] im, int imx, int imy, double[] noise, int noisex, int noisey) {
        double[] fh = null;
        Vector pind = new Vector();
        Vector pindN = new Vector();
        Vector pyr = null;
        Vector pyrN = null;
        Vector pyrh;
        int bandNum;
        int nband;
        double[] aux = null;
        double[] oldaux = null;
        int newx;
        int newy;
        double[] auxn = null;
        double[] oldauxn = null;
        int[] nsx = new int[1];
        int[] nsy = new int[1];
        int[] nsxn = new int[1];
        int[] nsyn = new int[1];
        int[] nsxnn = new int[1];
        int[] nsynn = new int[1];
        boolean prnt;
        double[][][] BL;
        double[][][] BLn;
        double var;
        int i, j;
        int index;
        double[] imagArray = null;
        double sy2;
        double sn2;
        double SNRin;
        double[] BLT;
        int blx;
        int bly;
        int[] intMat;

        pyr = buildFullSFpyr2(im, imx, imy, nScales, nOrientations - 1, 1.0, pind);

        if (error == 1) {
            return null;
        }

        pyrN = buildFullSFpyr2(noise, noisex, noisey, nScales, nOrientations - 1, 1.0, pindN);

        if (error == 1) {
            return null;
        }

        pyrh = pyr;
        bandNum = pind.size() - 1;

        // Everything except the low pass residual
        for (nband = 2; nband <= bandNum; nband++) {
            fireProgressStateChanged((100 * (nband - 1)) / (bandNum - 1));
            aux = pyrBand(pyr, pind, nband - 1, nsx, nsy);
            auxn = pyrBand(pyrN, pindN, nband - 1, nsxn, nsyn);

            // Has the subband a parent?
            prnt = useParent && (nband < (bandNum - nOrientations));

            if (prnt) {
                BL = new double[nsy[0]][nsx[0]][2];
                BLn = new double[nsy[0]][nsx[0]][2];
            } // if (prnt)
            else {
                BL = new double[nsy[0]][nsx[0]][1];
                BLn = new double[nsy[0]][nsx[0]][1];
            } // else

            // Because we are discarding 2 coefficients on every dimension
            var = Math.sqrt(((nsx[0] - 2.0) * (nsy[0] - 2.0)) / (nsx[0] * nsy[0]));

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    index = i + (j * nsx[0]);
                    BL[j][i][0] = aux[index];
                    BLn[j][i][0] = auxn[index] * var;
                }
            }

            if (prnt) {
                aux = pyrBand(pyr, pind, nband + nOrientations - 1, nsxn, nsyn);
                auxn = pyrBand(pyrN, pindN, nband + nOrientations - 1, nsxnn, nsynn);

                // Resample 2x2 the parent if not in the high-pass oriented subbands.
                // if (nband > nOrientations+1) {
                if (aux.length < (nsx[0] * nsy[0])) {
                    oldaux = new double[aux.length];

                    for (i = 0; i < aux.length; i++) {
                        oldaux[i] = aux[i];
                    }

                    newx = (int) Math.round(2.0 * nsxn[0]);
                    newy = (int) Math.round(2.0 * nsyn[0]);
                    aux = new double[newx * newy];
                    imagArray = new double[newx * newy];
                    expand(oldaux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                    oldauxn = new double[auxn.length];

                    for (i = 0; i < auxn.length; i++) {
                        oldauxn[i] = auxn[i];
                    }

                    newx = (int) Math.round(2.0 * nsxnn[0]);
                    newy = (int) Math.round(2.0 * nsynn[0]);
                    auxn = new double[newx * newy];
                    imagArray = new double[newx * newy];
                    expand(oldauxn, 2.0, nsxnn[0], nsynn[0], auxn, imagArray);
                } // if (nband > nOrientations+1)

                for (j = 0; j < nsy[0]; j++) {

                    for (i = 0; i < nsx[0]; i++) {
                        index = i + (j * nsx[0]);
                        BL[j][i][1] = aux[index];
                        BLn[j][i][1] = auxn[index] * var;
                    }
                }
            } // if (prnt)

            sy2 = 0.0;
            sn2 = 0.0;

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    sy2 = sy2 + (BL[j][i][0] * BL[j][i][0]);
                    sn2 = sn2 + (BLn[j][i][0] * BLn[j][i][0]);
                }
            }

            sy2 = sy2 / (nsx[0] * nsy[0]);
            sn2 = sn2 / (nsx[0] * nsy[0]);

            if (sy2 > sn2) {
                SNRin = 4.342944819 * Math.log((sy2 - sn2) / sn2);
                Preferences.debug("SNRin = " + SNRin + "\n");
            } else {
                Preferences.debug("decompReconstFull: Signal is not detectable in noisy subband\n");
            }

            // main
            BL = denoi_BLS_GSM_band(BL, BLn, prnt);
            bly = BL.length;
            blx = BL[0].length;
            BLT = new double[blx * bly];

            for (j = 0; j < bly; j++) {

                for (i = 0; i < blx; i++) {
                    index = i + (j * blx);
                    BLT[index] = BL[j][i][0];
                }
            }

            pyrh.setElementAt(BLT, nband - 1);
            intMat = new int[2];
            intMat[0] = blx;
            intMat[1] = bly;
            pind.setElementAt(intMat, nband - 1);
        } // for (nband = 2; nband <= bandNum; nband++)

        fh = reconFullSFpyr2(pyrh, pind, null, null, 1.0);

        if (error == 1) {
            return null;
        }

        return fh;
    }

    /**
     * Port of decomp_reconst_W.m by JPM, Univ. de Granada, 3/03 Decompose image into subbands, denoise using BLS-GSM
     * method, and recompose again. Version using a critically sampled pyramid (orthogonal wavelet), as implemented in
     * MatlabPyrTools (Eero)
     *
     * @param   im      DOCUMENT ME!
     * @param   imx     DOCUMENT ME!
     * @param   imy     DOCUMENT ME!
     * @param   filter  DOCUMENT ME!
     * @param   noise   DOCUMENT ME!
     * @param   noisex  DOCUMENT ME!
     * @param   noisey  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] decompReconstW(double[] im, int imx, int imy, int filter, double[] noise, int noisex, int noisey) {
        double[] fh = null;
        Vector pind = new Vector();
        Vector pyr = null;
        Vector pyrN = null;
        Vector pyrh;
        int bandNum;
        int nband;
        double[] aux = null;
        double[] auxn = null;
        double[] oldaux = null;
        double[] oldauxn = null;
        int newx;
        int newy;
        int[] nsx = new int[1];
        int[] nsy = new int[1];
        int[] nsxn = new int[1];
        int[] nsyn = new int[1];
        boolean prnt;
        double[][][] BL;
        double[][][] BLn;
        int i, j;
        int index;
        double[] imagArray = null;
        double sy2;
        double sn2;
        double SNRin;
        double[] BLT;
        int blx;
        int bly;
        int[] intMat;
        int[] fhx = new int[1];
        int[] fhy = new int[1];

        // Number of orientations: vertical, horizontal, and mixed diagonals
        // (for compatibility)
        nOrientations = 3;

        pyr = buildWpyr(im, imx, imy, nScales, filter, CIRCULAR, pind);

        if (error == 1) {
            return null;
        }

        pyrN = buildWpyr(noise, noisex, noisey, nScales, filter, CIRCULAR, pind);

        if (error == 1) {
            return null;
        }

        pyrh = pyr;
        bandNum = pind.size();

        // Everything except the low pass residual
        for (nband = 1; nband <= (bandNum - 1); nband++) {
            fireProgressStateChanged((100 * nband) / (bandNum - 1));
            aux = pyrBand(pyr, pind, nband - 1, nsx, nsy);
            auxn = pyrBand(pyrN, pind, nband - 1, nsxn, nsyn);

            // Has the subband a parent?
            prnt = useParent && (nband < (bandNum - nOrientations));

            if (prnt) {
                BL = new double[nsy[0]][nsx[0]][2];
                BLn = new double[nsy[0]][nsx[0]][2];
            } // if (prnt)
            else {
                BL = new double[nsy[0]][nsx[0]][1];
                BLn = new double[nsy[0]][nsx[0]][1];
            } // else

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    index = i + (j * nsx[0]);
                    BL[j][i][0] = aux[index];
                    BLn[j][i][0] = auxn[index];
                }
            }

            if (prnt) {
                aux = pyrBand(pyr, pind, nband + nOrientations - 1, nsxn, nsyn);
                oldaux = new double[aux.length];

                for (i = 0; i < aux.length; i++) {
                    oldaux[i] = aux[i];
                }

                newx = (int) Math.round(2.0 * nsxn[0]);
                newy = (int) Math.round(2.0 * nsyn[0]);
                aux = new double[newx * newy];
                imagArray = new double[newx * newy];
                expand(oldaux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                auxn = pyrBand(pyrN, pind, nband + nOrientations - 1, nsxn, nsyn);
                oldauxn = new double[auxn.length];

                for (i = 0; i < auxn.length; i++) {
                    oldauxn[i] = auxn[i];
                }

                newx = (int) Math.round(2.0 * nsxn[0]);
                newy = (int) Math.round(2.0 * nsyn[0]);
                auxn = new double[newx * newy];
                imagArray = new double[newx * newy];
                expand(oldauxn, 2.0, nsxn[0], nsyn[0], auxn, imagArray);

                for (j = 0; j < nsy[0]; j++) {

                    for (i = 0; i < nsx[0]; i++) {
                        index = i + (j * nsx[0]);
                        BL[j][i][1] = aux[index];
                        BLn[j][i][1] = auxn[index];
                    }
                }
            } // if (prnt)

            sy2 = 0.0;
            sn2 = 0.0;

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    sy2 = sy2 + (BL[j][i][0] * BL[j][i][0]);
                    sn2 = sn2 + (BLn[j][i][0] * BLn[j][i][0]);
                }
            }

            sy2 = sy2 / (nsx[0] * nsy[0]);
            sn2 = sn2 / (nsx[0] * nsy[0]);

            if (sy2 > sn2) {
                SNRin = 4.342944819 * Math.log((sy2 - sn2) / sn2);
                Preferences.debug("SNRin = " + SNRin + "\n");
            } else {
                Preferences.debug("decompReconst: Signal is not detectable in noisy subband\n");
            }

            // main
            BL = denoi_BLS_GSM_band(BL, BLn, prnt);
            bly = BL.length;
            blx = BL[0].length;
            BLT = new double[blx * bly];

            for (j = 0; j < bly; j++) {

                for (i = 0; i < blx; i++) {
                    index = i + (j * blx);
                    BLT[index] = BL[j][i][0];
                }
            }

            pyrh.setElementAt(BLT, nband - 1);
            intMat = new int[2];
            intMat[0] = blx;
            intMat[1] = bly;
            pind.setElementAt(intMat, nband - 1);
        } // for (nband = 1; nband <= bandNum-1; nband++)

        fh = reconWpyr(pyrh, pind, filter, CIRCULAR, null, null, fhx, fhy);

        if (error == 1) {
            return null;
        }

        return fh;
    }

    /**
     * This is a port of decomp_reconst_WU.m by Javier.Portilla, Univ. de Granada, 11/04 Decompose image into subbands
     * (undecimated wavelet), denoise again, and recompose again.
     *
     * @param   im         image
     * @param   imx        First dimension of image
     * @param   imy        Second dimension of image
     * @param   daubOrder  Order of the daubechie function used (must be even)
     * @param   noise      image having the same autocorrelation as the noise (e.g., a delta for white noise)
     * @param   noisex     First dimension of noise
     * @param   noisey     Second dimension of noise
     *
     * @return  DOCUMENT ME!
     */
    private double[] decompReconstWU(double[] im, int imx, int imy, int daubOrder, double[] noise, int noisex,
                                     int noisey) {
        double[] fh = null;
        Vector pind = new Vector();
        Vector pyr = null;
        Vector pyrN = null;
        Vector pyrh = new Vector();
        int bandNum;
        int nband;
        double[] aux = null;
        double[] oldaux;
        int newx;
        int newy;
        double[] auxn = null;
        double[] oldauxn = null;
        int[] nsx = new int[1];
        int[] nsy = new int[1];
        int[] nsxn = new int[1];
        int[] nsyn = new int[1];
        int[] nsxnn = new int[1];
        int[] nsynn = new int[1];
        boolean prnt;
        double[][][] BL;
        double[][][] BLn;
        double var;
        int i, j;
        int index;
        double[] imagArray = null;
        double sy2;
        double sn2;
        double SNRin;
        double[] BLT;
        int blx;
        int bly;
        int[] intMat;

        // Number of orientations: vertical, horizontal, and mixed diagonals
        nOrientations = 3;

        pyr = buildWUpyr(im, imx, imy, nScales, daubOrder, pind);

        if (error == 1) {
            return null;
        }

        pyrN = buildWUpyr(noise, noisex, noisey, nScales, daubOrder, pind);

        if (error == 1) {
            return null;
        }

        pyrh.addAll(pyr);
        bandNum = pind.size() - 1;

        // Everything except the lowpass residual
        for (nband = 2; nband <= bandNum; nband++) {
            fireProgressStateChanged((100 * (nband - 1)) / (bandNum - 1));
            aux = pyrBand(pyr, pind, nband - 1, nsx, nsy);
            auxn = pyrBand(pyrN, pind, nband - 1, nsxn, nsyn);

            // Has the subband a parent?
            prnt = useParent && (nband < (bandNum - nOrientations));

            if (prnt) {
                BL = new double[nsy[0]][nsx[0]][2];
                BLn = new double[nsy[0]][nsx[0]][2];
            } // if (prnt)
            else {
                BL = new double[nsy[0]][nsx[0]][1];
                BLn = new double[nsy[0]][nsx[0]][1];
            } // else

            // Because we are discarding 2 coefficients on every dimension
            var = Math.sqrt(((nsx[0] - 2.0) * (nsy[0] - 2.0)) / (nsx[0] * nsy[0]));

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    index = i + (j * nsx[0]);
                    BL[j][i][0] = aux[index];
                    BLn[j][i][0] = auxn[index] * var;
                }
            }

            if (prnt) {
                aux = pyrBand(pyr, pind, nband + nOrientations - 1, nsxn, nsyn);
                auxn = pyrBand(pyrN, pind, nband + nOrientations - 1, nsxnn, nsynn);

                // Resample 2x2 the parent if not in the high-pass oriented subbands
                if (nband > (nOrientations + 1)) {
                    oldaux = new double[aux.length];

                    for (i = 0; i < aux.length; i++) {
                        oldaux[i] = aux[i];
                    }

                    newx = (int) Math.round(2.0 * nsxn[0]);
                    newy = (int) Math.round(2.0 * nsyn[0]);
                    aux = new double[newx * newy];
                    imagArray = new double[newx * newy];
                    expand(oldaux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                    oldauxn = new double[auxn.length];

                    for (i = 0; i < auxn.length; i++) {
                        oldauxn[i] = auxn[i];
                    }

                    newx = (int) Math.round(2.0 * nsxnn[0]);
                    newy = (int) Math.round(2.0 * nsynn[0]);
                    auxn = new double[newx * newy];
                    imagArray = new double[newx * newy];
                    expand(oldauxn, 2.0, nsxnn[0], nsynn[0], auxn, imagArray);
                }

                for (j = 0; j < nsy[0]; j++) {

                    for (i = 0; i < nsx[0]; i++) {
                        index = i + (j * nsx[0]);
                        BL[j][i][1] = aux[index];
                        BLn[j][i][1] = auxn[index] * var;
                    }
                }
            } // if (prnt)

            sy2 = 0.0;
            sn2 = 0.0;

            for (j = 0; j < nsy[0]; j++) {

                for (i = 0; i < nsx[0]; i++) {
                    sy2 = sy2 + (BL[j][i][0] * BL[j][i][0]);
                    sn2 = sn2 + (BLn[j][i][0] * BLn[j][i][0]);
                }
            }

            sy2 = sy2 / (nsx[0] * nsy[0]);
            sn2 = sn2 / (nsx[0] * nsy[0]);

            if (sy2 > sn2) {
                SNRin = 4.342944819 * Math.log((sy2 - sn2) / sn2);
                Preferences.debug("SNRin = " + SNRin + "\n");
            } else {
                Preferences.debug("decompReconstWU: Signal is not detectable in noisy subband\n");
            }

            // main
            BL = denoi_BLS_GSM_band(BL, BLn, prnt);
            bly = BL.length;
            blx = BL[0].length;
            BLT = new double[blx * bly];

            for (j = 0; j < bly; j++) {

                for (i = 0; i < blx; i++) {
                    index = i + (j * blx);
                    BLT[index] = BL[j][i][0];
                }
            }

            pyrh.setElementAt(BLT, nband - 1);
            intMat = new int[2];
            intMat[0] = blx;
            intMat[1] = bly;
            pind.setElementAt(intMat, nband - 1);
        } // for (nband = 2; nband <= bandNum; nband++)

        fh = reconWUpyr(pyrh, pind, daubOrder);

        if (error == 1) {
            return null;
        }

        return fh;
    }

    /**
     * This is a port of denoi_BLS_GSM_band by JPM, Univ. de Granada, 4/03 It solves for the BLS global optimum
     * solution, using a flat (pseudo) prior for log(z) includeCovar Include/ not include covariance in the GSM model
     * optimize BLS/ MAP-Wiener (2-step)
     *
     * @param   y      DOCUMENT ME!
     * @param   noise  DOCUMENT ME!
     * @param   prnt   Inlcude/ not include parent
     *
     * @return  DOCUMENT ME!
     */
    private double[][][] denoi_BLS_GSM_band(double[][][] y, double[][][] noise, boolean prnt) {
        double[][][] x_hat = null;
        int nv, nh;
        int nblv, nblh;
        int nexp;
        double[][] zM = null;
        int N;
        int Lx;
        int Ly;
        int cent;
        double[][] Y = null;
        double[][] W = null;
        double[][] foo = null;
        int n;
        int[] offset = new int[2];
        int i, j, kx, ky, index;
        double[] mtxr;
        double[] resr = null;
        int indexW;
        double[][] C_w = null;
        int inum;
        double sig2;
        double var;
        EigenvalueDecomposition eig;
        double[] eigenvalue;
        double[][] S;
        double[][] iS;
        double[][] C_y = null;
        double sy2;
        double[][] C_x = null;
        double[][] Q = null;
        double[][] L = null;
        double sumL;
        double sumPosL;
        double sx2;
        double[] la;
        double[][] V;
        double[][] V2;
        double[][] M;
        double[] m;
        double lzmin;
        double step;
        int nsamp_z;
        double[] lzi;
        double[] zi;
        double[][] laz;
        double[][] p_lz;
        double[][] mu_x = null;
        double[] z_w = null;
        double[] pg1_lz;
        double[][] laz2;
        double[][] aux;
        int[] ind;
        double[] z;
        int uv;
        int lh;
        int dv;
        int rh;
        double[][] p_z;
        double[][] p_lz_y;
        double maxp;
        boolean zeroFound;
        double[][] rmat1;
        double[][] rmat2;
        double[][] rmat3;
        double[][] rmat4;
        int j2;
        int nx, ny;
        double max;
        int SNumNonZeros;

        nv = y.length;
        nh = y[0].length;

        // Discard the outer coefficients for the reference (central) coefficients
        // to avoid boundary effects
        nblv = nv - blockSizeY + 1;
        nblh = nh - blockSizeX + 1;
        nexp = nblv * nblh; // number of coefficients considered
        zM = new double[nv][nh]; // hidden variable z
        x_hat = new double[nv][nh][1]; // coefficient estimation
        N = blockSizeX * blockSizeY; // size of the neighborhood

        if (prnt) {
            N = N + 1;
        }

        // bolckSizeX and blockSizeY must be odd
        Lx = (blockSizeX - 1) / 2;
        Ly = (blockSizeY - 1) / 2;

        // reference coefficient in the neighborhood
        // central coef in the fine band
        cent = ((blockSizeX * blockSizeY) + 1) / 2;

        // It will be the observed signal (rearranged in nexp neighborhoods)
        Y = new double[nexp][N];

        // It will be a signal with the same autocorrelation as the noise
        W = new double[nexp][N];

        foo = new double[nexp][N];

        // Compute covariance of noise from 'noise'
        n = 0;
        mtxr = new double[nv * nh];

        for (ny = -Ly; ny <= Ly; ny++) { // Spatial neighbors

            for (nx = -Lx; nx <= Lx; nx++) {
                n = n + 1;

                for (ky = 0; ky < nv; ky++) {

                    for (kx = 0; kx < nh; kx++) {
                        index = kx + (ky * nh);
                        mtxr[index] = noise[ky][kx][0];
                    }
                }

                offset[0] = nx;
                offset[1] = ny;
                resr = new double[mtxr.length];
                shiftReal(mtxr, nh, nv, offset, resr);
                foo = new double[nblv][nblh];

                for (ky = 0, indexW = 0; ky < nblv; ky++) {

                    for (kx = 0; kx < nblh; kx++) {
                        index = (kx + Lx) + ((ky + Ly) * nh);
                        foo[ky][kx] = resr[index];
                        W[indexW++][n - 1] = foo[ky][kx];
                    }
                }
            } // for (j = -Ly; j <= Ly; j++)
        } // for (i = -Lx; i <= Lx; i++)


        if (prnt) { // parent
            n = n + 1;
            foo = new double[nblv][nblh];

            for (ky = 0, indexW = 0; ky < nblv; ky++) {

                for (kx = 0; kx < nblh; kx++) {
                    foo[ky][kx] = noise[ky + Ly][kx + Lx][1];
                    W[indexW++][n - 1] = foo[ky][kx];
                }
            }
        } // if (prnt)

        C_w = innerProd(W);

        for (j = 0; j < N; j++) {

            for (i = 0; i < N; i++) {
                C_w[j][i] = C_w[j][i] / nexp;
            }
        }

        inum = N;

        if (prnt) {
            inum = inum - 1;
        }

        sig2 = 0.0;

        for (i = 0; i < inum; i++) {
            sig2 += C_w[i][i];
        }

        // Noise variance in the (fine) subband
        sig2 = sig2 / inum;

        W = null;

        if (!includeCovar) {

            if (prnt) {
                var = C_w[N - 1][N - 1];
                C_w = new double[N][N];

                for (i = 0; i < (N - 1); i++) {
                    C_w[i][i] = sig2;
                }

                C_w[N - 1][N - 1] = var;
            } else {
                C_w = new double[N][N];

                for (i = 0; i < N; i++) {
                    C_w[i][i] = sig2;
                }
            }
        } // if (!inlcudeCovar)

        // Rearrange observed samples in 'nexp' neighborhoods
        n = 0;

        for (ny = -Ly; ny <= Ly; ny++) { // Spatial neighbors

            for (nx = -Lx; nx <= Lx; nx++) {
                n = n + 1;

                for (ky = 0; ky < nv; ky++) {

                    for (kx = 0; kx < nh; kx++) {
                        index = kx + (ky * nh);
                        mtxr[index] = y[ky][kx][0];
                    }
                }

                offset[0] = nx;
                offset[1] = ny;
                resr = new double[mtxr.length];
                shiftReal(mtxr, nh, nv, offset, resr);
                foo = new double[nblv][nblh];

                for (ky = 0, indexW = 0; ky < nblv; ky++) {

                    for (kx = 0; kx < nblh; kx++) {
                        index = (kx + Lx) + ((ky + Ly) * nh);
                        foo[ky][kx] = resr[index];
                        Y[indexW++][n - 1] = foo[ky][kx];
                    }
                }
            } // for (nx = -Lx; nx <= Lx; nx++)
        } // for (ny = -Ly; ny <= Ly; ny++)

        if (prnt) { // parent
            n = n + 1;
            foo = new double[nblv][nblh];

            for (ky = 0, indexW = 0; ky < nblv; ky++) {

                for (kx = 0; kx < nblh; kx++) {
                    foo[ky][kx] = y[ky + Ly][kx + Lx][1];
                    Y[indexW++][n - 1] = foo[ky][kx];
                }
            }
        } // if (prnt)

        foo = null;

        // For modulating the local stdv of noise
        // For now leave out code for case with sig a vector

        eig = new EigenvalueDecomposition(new Matrix(C_w));
        eigenvalue = eig.getRealEigenvalues();

        // In EigenvalueDecomposition the columns represent the
        // eigenvectors
        S = eig.getV().getArray();

        // S * S' = C_w
        for (j = 0; j < N; j++) {

            if (eigenvalue[j] > 0) {
                var = Math.sqrt(eigenvalue[j]);
            } else {
                var = 0.0;
            }

            for (i = 0; i < N; i++) {
                S[i][j] = S[i][j] * var;
            }
        }

        SNumNonZeros = 0;

        for (j = 0; j < N; j++) {

            for (i = 0; i < N; i++) {

                if (S[j][i] != 0.0) {
                    SNumNonZeros++;
                }
            }
        }

        if (SNumNonZeros >= 2) {
            iS = (new Matrix(S)).inverse().getArray();
        } else {
            iS = new double[N][N];
        }

        for (i = 0; i < noise.length; i++) {

            for (j = 0; j < noise[i].length; j++) {
                noise[i][j] = null;
                ;
            }

            noise[i] = null;
        }

        noise = null;

        C_y = innerProd(Y);

        for (j = 0; j < N; j++) {

            for (i = 0; i < N; i++) {
                C_y[j][i] = C_y[j][i] / nexp;
            }
        }

        // sy2 = observed (signal + noise) variance in the subband
        sy2 = 0.0;

        for (i = 0; i < (N - 1); i++) {
            sy2 += C_y[i][i];
        }

        if (!prnt) {
            sy2 += C_y[N - 1][N - 1];
            sy2 = sy2 / N;
        } else {
            sy2 = sy2 / (N - 1);
        }

        // C_x = C_y - C_w as signal and noise are assumed to be independent
        C_x = new double[n][n];

        for (j = 0; j < N; j++) {

            for (i = 0; i < N; i++) {
                C_x[j][i] = C_y[j][i] - C_w[j][i];
            }
        }

        eig = new EigenvalueDecomposition(new Matrix(C_x));
        eigenvalue = eig.getRealEigenvalues();

        // In EigenvalueDecomposition the columns represent the
        // eigenvectors
        Q = eig.getV().getArray();

        // Correct possible negative eigenvalues, without changing
        // the overall variance
        L = new double[N][N];
        sumL = 0.0;
        sumPosL = 0.0;

        for (i = 0; i < N; i++) {
            sumL += eigenvalue[i];

            if (eigenvalue[i] > 0.0) {
                sumPosL += eigenvalue[i];
            }
        }

        if (sumPosL == 0) {
            sumPosL = 1;
        }

        for (i = 0; i < N; i++) {

            if (eigenvalue[i] > 0) {
                L[i][i] = eigenvalue[i] * sumL / sumPosL;
            }
        }

        C_x = ((new Matrix(Q)).times(new Matrix(L))).times((new Matrix(Q)).transpose()).getArray();

        // Estimated signal variance in the subband
        sx2 = sy2 - sig2;

        if (sx2 < 0.0) {
            sx2 = 0.0;
        }

        if (!includeCovar) {

            if (prnt) {
                var = C_x[N - 1][N - 1];
                C_x = new double[N][N];

                for (i = 0; i < (N - 1); i++) {
                    C_x[i][i] = sx2;
                }

                C_x[N - 1][N - 1] = var;
            } // if (prnt)
            else {
                C_x = new double[N][N];

                for (i = 0; i < N; i++) {
                    C_x[i][i] = sx2;
                }
            } // else
        } // if (!includeCovar)

        // Double diagonalization of signal and noise eigenvalues:
        // energy in the new representation
        eig = new EigenvalueDecomposition(((new Matrix(iS)).times(new Matrix(C_x))).times((new Matrix(iS)).transpose()));
        la = eig.getRealEigenvalues();

        for (i = 0; i < N; i++) {

            if (la[i] < 0.0) {
                la[i] = 0.0;
            }
        }

        // In EigenvalueDecomposition the columns represent the
        // eigenvectors
        Q = eig.getV().getArray();

        // Linearly transform the observations, and keep the quadratic
        // values (we do not model phase).
        V = (((new Matrix(Q)).transpose()).times(new Matrix(iS))).times((new Matrix(Y)).transpose()).getArray();

        for (i = 0; i < Y.length; i++) {
            Y[i] = null;
        }

        Y = null;
        V2 = new double[nexp][N];

        for (j = 0; j < nexp; j++) {

            for (i = 0; i < N; i++) {
                V2[j][i] = V[i][j] * V[i][j];
            }
        }

        M = (new Matrix(S)).times(new Matrix(Q)).getArray();
        m = new double[N];

        for (i = 0; i < N; i++) {
            m[i] = M[cent - 1][i];
        }

        // Compute p(Y|log(z))
        // Non-informative prior
        lzmin = -20.5;

        // lzmax = 3.5;
        step = 2;
        nsamp_z = 13;

        lzi = new double[nsamp_z];
        zi = new double[nsamp_z];

        for (i = 0; i < nsamp_z; i++) {
            lzi[i] = lzmin + (i * step);
            zi[i] = Math.exp(lzi[i]);
        }

        laz = new double[N][nsamp_z];

        for (i = 0; i < N; i++) {

            for (j = 0; j < nsamp_z; j++) {
                laz[i][j] = la[i] * zi[j];
            }
        }

        p_lz = new double[nexp][nsamp_z];
        // mu_x = new double[nexp][nsamp_z] obtained later in multiplication

        if (z_w == null) { // Spatially invariant noise

            // Normalization term (depends on z, but not on Y)
            pg1_lz = new double[nsamp_z];

            for (j = 0; j < nsamp_z; j++) {
                pg1_lz[j] = laz[0][j] + 1.0;

                for (i = 1; i < N; i++) {
                    pg1_lz[j] *= (laz[i][j] + 1.0);
                }

                pg1_lz[j] = 1.0 / Math.sqrt(pg1_lz[j]);
            }

            laz2 = new double[N][nsamp_z];

            for (j = 0; j < nsamp_z; j++) {

                for (i = 0; i < N; i++) {
                    laz2[i][j] = 1.0 / (1.0 + laz[i][j]);
                }
            }

            aux = (new Matrix(V2)).times(new Matrix(laz2)).getArray();

            for (j = 0; j < nsamp_z; j++) {

                for (i = 0; i < nexp; i++) {
                    aux[i][j] = Math.exp(-0.5 * aux[i][j]);
                }
            }

            // p_lz gives us the conditional Gaussian density values
            // for the observed samples and the considered samples of z
            for (j = 0; j < nsamp_z; j++) {

                for (i = 0; i < nexp; i++) {
                    p_lz[i][j] = aux[i][j] * pg1_lz[j];
                }
            }

            // Compute mu_x(z) = E{x|log(z), Y}
            aux = new double[N][nsamp_z];

            for (j = 0; j < nsamp_z; j++) {

                for (i = 0; i < N; i++) {
                    aux[i][j] = m[i] * laz[i][j] * laz2[i][j];
                }
            }

            for (i = 0; i < laz2.length; i++) {
                laz2[i] = null;
            }

            laz2 = null;

            // Wiener estimation, for each considered sample of z
            mu_x = ((new Matrix(V)).transpose()).times(new Matrix(aux)).getArray();
        } // if (z_w == null)
        else { // Spatially variant noise

            // Do not fill in at this time
        }

        // Use ML estimation of z only for the boundaries
        // ind contains the column index of the maximum
        // value in each row of p_lz
        ind = new int[nexp];

        for (i = 0; i < nexp; i++) {
            ind[i] = 0;
            max = p_lz[i][0];

            for (j = 1; j < nsamp_z; j++) {

                if (p_lz[i][j] > max) {
                    ind[i] = j;
                    max = p_lz[i][j];
                }
            }
        } // for (i = 0; i < nexp; i++)

        z = new double[nexp];

        for (i = 0; i < nexp; i++) {
            z[i] = zi[ind[i]];
        }

        for (i = 0; i < V2.length; i++) {
            V2[i] = null;
        }

        V2 = null;
        aux = null;

        // For boundary handling
        uv = 1 + Ly;
        lh = 1 + Lx;
        dv = nblv + Ly;
        rh = nblh + Lx;

        for (j = uv - 1, index = 0; j < dv; j++) {

            for (i = lh - 1; i < rh; i++) {
                zM[j][i] = z[index++];
            }
        }

        // Propagation of the ML-estimated z to the boundaries

        // a) Corners
        for (j = 0; j < uv; j++) {

            for (i = 0; i < lh; i++) {
                zM[j][i] = zM[uv - 1][lh - 1];
            }
        }

        for (j = 0; j < uv; j++) {

            for (i = rh - 1; i < nh; i++) {
                zM[j][i] = zM[uv - 1][rh - 1];
            }
        }

        for (j = dv - 1; j < nv; j++) {

            for (i = 0; i < lh; i++) {
                zM[j][i] = zM[dv - 1][lh - 1];
            }
        }

        for (j = dv - 1; j < nv; j++) {

            for (i = rh - 1; i < nh; i++) {
                zM[j][i] = zM[dv - 1][rh - 1];
            }
        }

        // b) Bands
        for (j = 0; j < (uv - 1); j++) {

            for (i = lh; i < (rh - 1); i++) {
                zM[j][i] = zM[uv - 1][i];
            }
        }

        for (j = dv; j < nv; j++) {

            for (i = lh; i < (rh - 1); i++) {
                zM[j][i] = zM[dv - 1][i];
            }
        }

        for (j = uv; j < (dv - 1); j++) {

            for (i = 0; i < (lh - 1); i++) {
                zM[j][i] = zM[j][lh - 1];
            }
        }

        for (j = uv; j < (dv - 1); j++) {

            for (i = rh; i < nh; i++) {
                zM[j][i] = zM[j][rh - 1];
            }
        }

        // Do scalar Wiener for the boundary coefficients
        if (z_w != null) {
            // Spatially varying noise
        } else {

            // Spatially invariant noise
            for (j = 0; j < nv; j++) {

                for (i = 0; i < nh; i++) {
                    x_hat[j][i][0] = y[j][i][0] * sx2 * zM[j][i] / ((sx2 * zM[j][i]) + sig2);
                }
            }
        } // else

        // Prior for log(z)
        p_z = new double[nsamp_z][1];

        // Flat log-prior (non-informative for GSM)
        for (i = 0; i < nsamp_z; i++) {
            p_z[i][0] = 1.0 / nsamp_z;
        }

        // Compute p(log(z)|Y) from p(Y|log(z)) and p(log(z)) (Bayes rule)
        p_lz_y = new double[nexp][nsamp_z];

        for (j = 0; j < nsamp_z; j++) {

            for (i = 0; i < nexp; i++) {
                p_lz_y[i][j] = p_lz[i][j] / nsamp_z;
            }
        }

        for (i = 0; i < nexp; i++) {
            p_lz[i] = null;
        }

        p_lz = null;

        if (!optimize) {

            // Maximum likelihood in log(z): p_lz_y becomes a delta function
            for (i = 0; i < nexp; i++) {
                maxp = p_lz_y[i][0];

                for (j = 1; j < nsamp_z; j++) {

                    if (p_lz_y[i][j] > maxp) {
                        maxp = p_lz_y[i][j];
                    }
                }

                for (j = 0; j < nsamp_z; j++) {

                    if (p_lz_y[i][j] == maxp) {
                        p_lz_y[i][j] = 1.0;
                    } else {
                        p_lz_y[i][j] = 0.0;
                    }
                }
            }
        } // if (!optimize)

        aux = new double[nexp][1];

        for (i = 0; i < nexp; i++) {
            aux[i][0] = p_lz_y[i][0];

            for (j = 1; j < nsamp_z; j++) {
                aux[i][0] += p_lz_y[i][j];
            }
        }

        zeroFound = false;

        for (i = 0; i < nexp; i++) {

            if (aux[i][0] == 0.0) {
                zeroFound = true;
            }
        }

        if (zeroFound) {
            foo = new double[nexp][1];

            for (i = 0; i < nexp; i++) {

                if (aux[i][0] == 0.0) {
                    foo[i][0] = 1.0;
                }
            }

            // Normalizing: p(log(z)|Y)
            rmat1 = new double[nexp][nsamp_z];

            for (i = 0; i < nexp; i++) {

                if (foo[i][0] == 0.0) {

                    for (j = 0; j < nsamp_z; j++) {
                        rmat1[i][j] = 1.0;
                    }
                }
            }

            rmat2 = new double[nexp][nsamp_z];

            for (i = 0; i < nexp; i++) {
                rmat2[i][0] = aux[i][0] + foo[i][0];

                for (j = 1; j < nsamp_z; j++) {
                    rmat2[i][j] = rmat2[i][0];
                }
            }

            rmat3 = new double[nexp][nsamp_z];

            for (i = 0; i < nexp; i++) {

                for (j = 0; j < nsamp_z; j++) {
                    rmat3[i][j] = foo[i][0];
                }
            }

            rmat4 = new double[nexp][nsamp_z];

            for (i = 0; i < nexp; i++) {

                for (j = 0; j < nsamp_z; j++) {
                    rmat4[i][j] = p_z[j][0];
                }
            }

            for (i = 0; i < nexp; i++) {

                for (j = 0; j < nsamp_z; j++) {
                    p_lz_y[i][j] = (rmat1[i][j] * p_lz_y[i][j] / rmat2[i][j]) + (rmat3[i][j] * rmat4[i][j]);
                }
            }

            for (i = 0; i < nexp; i++) {
                rmat1[i] = null;
                rmat2[i] = null;
                rmat3[i] = null;
                rmat4[i] = null;
            }

            rmat1 = null;
            rmat2 = null;
            rmat3 = null;
            rmat4 = null;
        } // if (zeroFound)
        else {
            rmat1 = new double[nexp][nsamp_z];

            for (i = 0; i < nexp; i++) {

                for (j = 0; j < nsamp_z; j++) {
                    rmat1[i][j] = aux[i][0];
                }
            }

            for (i = 0; i < nexp; i++) {

                for (j = 0; j < nsamp_z; j++) {
                    p_lz_y[i][j] = p_lz_y[i][j] / rmat1[i][j];
                }
            }

            for (i = 0; i < nexp; i++) {
                rmat1[i] = null;
            }

            rmat1 = null;
        } // else

        for (i = 0; i < aux.length; i++) {
            aux[i] = null;
        }

        aux = null;

        // Compute E{x|Y} = int_log(z){ E{x|log(z), Y} p(log(z)|Y) d(log(z)) }
        for (i = 0; i < nexp; i++) {

            for (j = 0; j < nsamp_z; j++) {
                mu_x[i][j] = mu_x[i][j] * p_lz_y[i][j];
            }
        }

        aux = new double[nexp][1];

        for (i = 0; i < nexp; i++) {
            aux[i][0] = mu_x[i][0];

            for (j = 1; j < nsamp_z; j++) {
                aux[i][0] += mu_x[i][j];
            }
        }

        for (j = Ly, j2 = 0; j < (nblv + Ly); j++) {

            for (i = Lx; i < (nblh + Lx); i++) {
                x_hat[j][i][0] = aux[j2++][0];
            }
        }

        return x_hat;
    }

    /**
     * Port of expand.m, written by JPM, 5/2003 It expands (spatially) an image into a factor f in each dimension. It
     * does it filling in with zeros the expanded Fourier domain.
     *
     * @param  t    DOCUMENT ME!
     * @param  f    DOCUMENT ME!
     * @param  mx   DOCUMENT ME!
     * @param  my   DOCUMENT ME!
     * @param  ter  DOCUMENT ME!
     * @param  tei  DOCUMENT ME!
     */
    private void expand(double[] t, double f, int mx, int my, double[] ter, double[] tei) {
        FFTUtility fftUtil;
        double[] tr = null;
        double[] ti = null;
        int i, j, i2, j2;
        double f2;
        int cx;
        int cy;
        boolean evenmx;
        boolean evenmy;
        int x1, x2, y1, y2;
        int xs;
        int ys;
        double esqr;
        double esqi;
        int[] offset = new int[2];
        int fmx, fmy;

        tr = new double[mx * my];
        ti = new double[mx * my];

        for (i = 0; i < tr.length; i++) {
            tr[i] = t[i];
        }

        // forward FFT
        fftUtil = new FFTUtility(tr, ti, my, mx, 1, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(tr, ti, 1, my, mx, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        center(tr, ti, mx, my);
        f2 = f * f;

        for (i = 0; i < tr.length; i++) {
            tr[i] = f2 * tr[i];
            ti[i] = f2 * ti[i];
        }

        fmx = (int) Math.round(f * mx);
        fmy = (int) Math.round(f * my);

        cx = (int) Math.ceil(fmx / 2.0);
        evenmx = (fmx == ((fmx / 2) * 2));
        cy = (int) Math.ceil(fmy / 2.0);
        evenmy = (fmy == ((fmy / 2) * 2));

        x1 = cx - (int) Math.floor(fmx / (2.0 * f)) - 1;

        if (evenmx) {
            x1 = x1 + 2;
        }

        x2 = cx + (int) Math.floor(fmx / (2.0 * f)) - 1;
        y1 = cy - (int) Math.floor(fmy / (2.0 * f)) - 1;

        if (evenmy) {
            y1 = y1 + 2;
        }

        y2 = cy + (int) Math.floor(fmy / (2.0 * f)) - 1;

        xs = 0;

        if (evenmx) {
            xs = 1;
        }

        ys = 0;

        if (evenmy) {
            ys = 1;
        }

        for (j = y1, j2 = ys; j <= y2; j++, j2++) {

            for (i = x1, i2 = xs; i <= x2; i++, i2++) {
                ter[i + (fmx * j)] = tr[i2 + (mx * j2)];
                tei[i + (fmx * j)] = ti[i2 + (mx * j2)];
            }
        }

        if (evenmx) {

            for (j = y1, j2 = 1; j <= y2; j++, j2++) {
                ter[x1 - 1 + (fmx * j)] = tr[mx * j2] / 2.0;
                tei[x1 - 1 + (fmx * j)] = ti[mx * j2] / 2.0;
            }

            for (j = y1, j2 = (int) Math.round(fmy / f) - 1; j <= y2; j++, j2--) {
                ter[x2 + 1 + (fmx * j)] = tr[mx * j2] / 2.0;
                tei[x2 + 1 + (fmx * j)] = -ti[mx * j2] / 2.0;
            }
        } // if (evenmx)

        if (evenmy) {

            for (i = x1, i2 = 1; i <= x2; i++, i2++) {
                ter[i + (fmx * (y1 - 1))] = tr[i2] / 2.0;
                tei[i + (fmx * (y1 - 1))] = ti[i2] / 2.0;
            }

            for (i = x1, i2 = (int) Math.round(fmx / f) - 1; i <= x2; i++, i2--) {
                ter[i + (fmx * (y2 + 1))] = tr[i2] / 2.0;
                tei[i + (fmx * (y2 + 1))] = -ti[i2] / 2.0f;
            }
        } // if (evenmy)

        if (evenmx && evenmy) {
            esqr = tr[0] / 4.0;
            esqi = ti[0] / 4.0;
            ter[x1 - 1 + (fmx * (y1 - 1))] = esqr;
            tei[x1 - 1 + (fmx * (y1 - 1))] = esqi;
            ter[x2 + 1 + (fmx * (y1 - 1))] = esqr;
            tei[x2 + 1 + (fmx * (y1 - 1))] = esqi;
            ter[x1 - 1 + (fmx * (y2 + 1))] = esqr;
            tei[x1 - 1 + (fmx * (y2 + 1))] = esqi;
            ter[x2 + 1 + (fmx * (y2 + 1))] = esqr;
            tei[x2 + 1 + (fmx * (y2 + 1))] = esqi;
        } // if (evenmx && evenmy)

        center(ter, tei, fmx, fmy);
        offset[0] = 1;

        if (evenmx) {
            offset[0] = 0;
        }

        offset[1] = 1;

        if (evenmy) {
            offset[1] = 0;
        }

        shift(ter, tei, fmx, fmy, offset, ter, tei);

        // Inverse FFT
        fftUtil = new FFTUtility(ter, tei, fmy, fmx, 1, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(ter, tei, 1, fmy, fmx, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();

        return;
    }

    /**
     * Returns the factorial of a nonnegative integer.
     *
     * @param   number  integer whose factorial is being returned
     *
     * @return  number!
     */
    private long factorial(int number) {
        long i, j;

        if (number < 0) {
            MipavUtil.displayError("A factorial cannot be performed on a negative number");

            return -1L;
        }

        if (number == 0) {
            return 1L;
        } else {

            for (i = 1, j = 1; i <= number; i++) {
                j = j * i;
            }

            return j;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  xin    DOCUMENT ME!
     * @param  lx     DOCUMENT ME!
     * @param  h0     DOCUMENT ME!
     * @param  h1     DOCUMENT ME!
     * @param  lh     DOCUMENT ME!
     * @param  xoutl  DOCUMENT ME!
     * @param  xouth  DOCUMENT ME!
     */
    private void fpconv(double[] xin, int lx, double[] h0, double[] h1, int lh, double[] xoutl, double[] xouth) {
        int i, j;
        double x0, x1;

        for (i = lx; i < (lx + lh - 1); i++) {
            xin[i] = xin[i - lx];
        }

        for (i = 0; i < lx; i++) {
            x0 = 0;
            x1 = 0;

            for (j = 0; j < lh; j++) {
                x0 = x0 + (xin[j + i] * h0[lh - 1 - j]);
                x1 = x1 + (xin[j + i] * h1[lh - 1 - j]);
            }

            xoutl[i] = x0;
            xouth[i] = x1;
        } // for (i = 0; i < lx; i++)
    }

    /**
     * Computes mat'*mat.
     *
     * @param   mat  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[][] innerProd(double[][] mat) {
        int ydim = mat.length;
        int xdim = mat[0].length;
        double[][] res = new double[xdim][xdim];
        int i, j;
        double tmp;
        int k;

        for (i = 0; i < xdim; i++) {

            for (j = i; j < xdim; j++) {
                tmp = 0.0;

                for (k = 0; k < ydim; k++) {
                    tmp += mat[k][i] * mat[k][j];
                }

                res[i][j] = tmp;
                res[j][i] = tmp;
            }
        }

        return res;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  image   DOCUMENT ME!
     * @param  filt    DOCUMENT ME!
     * @param  temp    DOCUMENT ME!
     * @param  xfdim   DOCUMENT ME!
     * @param  yfdim   DOCUMENT ME!
     * @param  xstart  DOCUMENT ME!
     * @param  xstep   DOCUMENT ME!
     * @param  xstop   DOCUMENT ME!
     * @param  ystart  DOCUMENT ME!
     * @param  ystep   DOCUMENT ME!
     * @param  ystop   DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xdim    DOCUMENT ME!
     * @param  ydim    DOCUMENT ME!
     * @param  edges   DOCUMENT ME!
     */
    private void internal_expand(double[] image, double[] filt, double[] temp, int xfdim, int yfdim, int xstart,
                                 int xstep, int xstop, int ystart, int ystep, int ystop, double[] result, int xdim,
                                 int ydim, int edges) {
        // dcompReconstW calls recnWpyr with CIRCULAR so internal_expand is not called
    }

    /**
     * DOCUMENT ME!
     *
     * @param  image   DOCUMENT ME!
     * @param  xdim    DOCUMENT ME!
     * @param  ydim    DOCUMENT ME!
     * @param  filt    DOCUMENT ME!
     * @param  temp    DOCUMENT ME!
     * @param  xfdim   DOCUMENT ME!
     * @param  yfdim   DOCUMENT ME!
     * @param  xstart  DOCUMENT ME!
     * @param  xstep   DOCUMENT ME!
     * @param  xstop   DOCUMENT ME!
     * @param  ystart  DOCUMENT ME!
     * @param  ystep   DOCUMENT ME!
     * @param  ystop   DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  edges   DOCUMENT ME!
     */
    private void internal_reduce(double[] image, int xdim, int ydim, double[] filt, double[] temp, int xfdim, int yfdim,
                                 int xstart, int xstep, int xstop, int ystart, int ystep, int ystop, double[] result,
                                 int edges) {
        // Code not needed yet since decomReconstW only calls buildWpyr with CIRCULAR
    }

    /**
     * Performs upsampling (padding with zeroes) followed by convolution of filt with image (a.k.a. expand in
     * Burt&Adelson81). The operations are combined to avoid unnecessary multiplication of the filter samples with zeros
     * in the unsampled image. The convolution is done in 9 sections so that the mod operation is not performed
     * unnecessarily.
     *
     * @param  image   DOCUMENT ME!
     * @param  filt    DOCUMENT ME!
     * @param  xfdim   DOCUMENT ME!
     * @param  yfdim   DOCUMENT ME!
     * @param  xstart  DOCUMENT ME!
     * @param  xstep   DOCUMENT ME!
     * @param  xstop   DOCUMENT ME!
     * @param  ystart  DOCUMENT ME!
     * @param  ystep   DOCUMENT ME!
     * @param  ystop   DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xdim    DOCUMENT ME!
     * @param  ydim    DOCUMENT ME!
     */
    private void internal_wrap_expand(double[] image, double[] filt, int xfdim, int yfdim, int xstart, int xstep,
                                      int xstop, int ystart, int ystep, int ystop, double[] result, int xdim,
                                      int ydim) {
        int filtSize = xfdim * yfdim;
        int xCtrStop = xdim - xfdim + 1;
        int yCtrStop = ydim - yfdim + 1;
        int xCtrStart = 0;
        int yCtrStart = 0;
        int xfmid = xfdim / 2;
        int yfmid = yfdim / 2;
        double[][] imval;
        int xpos;
        int ypos;
        int impos;
        double val;
        int xres;
        int yres;
        int filtPos;
        int xFiltStop;

        // shift start/stop coords to filter upper left hand corner
        xstart -= xfmid;
        ystart -= yfmid;
        xstop -= xfmid;
        ystop -= yfmid;

        if (xstop < xCtrStop) {
            xCtrStop = xstop;
        }

        if (ystop < yCtrStop) {
            yCtrStop = ystop;
        }

        imval = new double[ydim][xdim];

        for (ypos = 0; ypos < ydim; ypos++) {

            for (xpos = 0; xpos < xdim; xpos++) {
                imval[ypos][xpos] = result[xpos + (ypos * xdim)];
            }
        }

        // Top rows
        for (impos = 0, ypos = ystart; ypos < yCtrStart; ypos += ystep) {

            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos + ydim, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize;
                         yres++, xFiltStop += xfdim) {

                    for (xres = xpos + xdim; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres % ydim][xres % xdim] += val * filt[filtPos];
                    }
                }
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++)

            for (; xpos < xCtrStop; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos + ydim, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize;
                         yres++, xFiltStop += xfdim) {

                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres % ydim][xres] += val * filt[filtPos];
                    }
                }
            } // for (; xpos < xCtrStop; xpos += xstep, impos++)

            for (; xpos < xstop; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos + ydim, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize;
                         yres++, xFiltStop += xfdim) {

                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres % ydim][xres % xdim] += val * filt[filtPos];
                    }
                }
            } // for (; xpos < xstop; xpos += xstep, impos++)
        } // for (impos = 0, ypos = ystart; ypos < yCtrStart; ypos += ystep)

        // Mid rows
        for (; ypos < yCtrStop; ypos += ystep) {

            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {

                    for (xres = xpos + xdim; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres][xres % xdim] += val * filt[filtPos];
                    }
                }
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++)

            // Center section
            for (; xpos < xCtrStop; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {

                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres][xres] += val * filt[filtPos];
                    }
                }
            } // for (; xpos < xCtrStop; xpos += xstep, impos++)

            for (; xpos < xstop; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {

                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres][xres % xdim] += val * filt[filtPos];
                    }
                }
            } // for (; xpos < xstop; xpos += xstep, impos++)
        } // for (; ypos < yCtrStop; ypos += ystep)

        // Bottom rows
        for (; ypos < ystop; ypos += ystep) {

            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {

                    for (xres = xpos + xdim; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres % ydim][xres % xdim] += val * filt[filtPos];
                    }
                }
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++)

            for (; xpos < xCtrStop; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {

                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres % ydim][xres] += val * filt[filtPos];
                    }
                }
            } // for (; xpos < xCtrStop; xpos += xstep, impos++)

            for (; xpos < xstop; xpos += xstep, impos++) {
                val = image[impos];

                for (yres = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {

                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres % ydim][xres % xdim] += val * filt[filtPos];
                    }
                }
            } // for (; xpos < xstop; xpos += xstep, impos++)
        } // for (; ypos < ystop; ypos += ystep)

        for (ypos = 0; ypos < ydim; ypos++) {

            for (xpos = 0; xpos < xdim; xpos++) {
                result[xpos + (ypos * xdim)] = imval[ypos][xpos];
            }
        }

        return;
    }

    /**
     * This is ported from wrap.c by Eero Simoncelli, 2/97 Performs correlation (i.e., convolution with filt(-x,-y) of
     * filt with image followed by subsampling (a.k.a. reduce in Burt&Adelson81). The operations are combined to avoid
     * unnecessary computation of the convolution samples that are to be discarded in the subsampling operation. The
     * convolution is done in 9 sections so that mod operations are not performed unnecessarily. The subsampling lattice
     * is specified by the start, step, and stop parameters.
     *
     * @param  image   DOCUMENT ME!
     * @param  xdim    first dimension of image
     * @param  ydim    second dimension of image
     * @param  filt    DOCUMENT ME!
     * @param  xfdim   first dimension of filt
     * @param  yfdim   second dimension of filt
     * @param  xstart  DOCUMENT ME!
     * @param  xstep   DOCUMENT ME!
     * @param  xstop   DOCUMENT ME!
     * @param  ystart  DOCUMENT ME!
     * @param  ystep   DOCUMENT ME!
     * @param  ystop   DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     */
    private void internal_wrap_reduce(double[] image, int xdim, int ydim, double[] filt, int xfdim, int yfdim,
                                      int xstart, int xstep, int xstop, int ystart, int ystep, int ystop,
                                      double[] result) {
        int filtSize = xfdim * yfdim;
        int xCtrStop = xdim - xfdim + 1;
        int yCtrStop = ydim - yfdim + 1;
        int xCtrStart = 0;
        int yCtrStart = 0;
        int xfmid = xfdim / 2;
        int yfmid = yfdim / 2;
        double[][] imval;
        int i;
        int j;
        int index;
        int respos;
        int ypos;
        int xpos;
        double sum;
        int xim;
        int yim;
        int filtPos;
        int xFiltStop;

        /* shift start/stop coords to filter upper left hand corner */
        xstart -= xfmid;
        ystart -= yfmid;
        xstop -= xfmid;
        ystop -= yfmid;

        if (xstop < xCtrStop) {
            xCtrStop = xstop;
        }

        if (ystop < yCtrStop) {
            yCtrStop = ystop;
        }

        imval = new double[ydim][xdim];

        for (j = 0; j < ydim; j++) {

            for (i = 0; i < xdim; i++) {
                index = i + (j * xdim);
                imval[j][i] = image[index];
            }
        }

        // top rows
        for (respos = 0, ypos = ystart; ypos < yCtrStart; ypos += ystep) {

            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos + ydim, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize;
                         yim++, xFiltStop += xfdim) {

                    for (xim = xpos + xdim; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim % xdim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++)

            for (; xpos < xCtrStop; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos + ydim, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize;
                         yim++, xFiltStop += xfdim) {

                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (; xpos < xCtrStop; xpos += xstep, respos++)

            for (; xpos < xstop; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos + ydim, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize;
                         yim++, xFiltStop += xfdim) {

                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim % xdim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (; xpos < xstop; xpos += xstep, respos++)
        } // for (respos = 0, ypos = ystart; ypos < yCtrStart; ypos += ystep)

        // mid rows
        for (; ypos < yCtrStop; ypos += ystep) {

            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {

                    for (xim = xpos + xdim; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim][xim % xdim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++)

            // Center section
            for (; xpos < xCtrStop; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {

                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim][xim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (; xpos < xCtrStop; xpos += xstep, respos++)

            for (; xpos < xstop; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {

                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim][xim % xdim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (; xpos < xstop; xpos += xstep, respos++)
        } // for (; ypos < yCtrStop; ypos += ystep)

        // Bottom rows
        for (; ypos < ystop; ypos += ystep) {

            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {

                    for (xim = xpos + xdim; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim % xdim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++)

            for (; xpos < xCtrStop; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {

                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim] * filt[filtPos];
                    }
                }
            } // for (; xpos < xCtrStop; xpos += xstep, respos++)

            for (; xpos < xstop; xpos += xstep, respos++) {
                sum = 0.0;

                for (yim = ypos, filtPos = 0, xFiltStop = xfdim; xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {

                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim % xdim] * filt[filtPos];
                    }
                }

                result[respos] = sum;
            } // for (; xpos < xstop; xpos += xstep, respos++)
        } // for (; ypos < ystop; ypos += ystep)

        return;
    } // internal_wrap_reduce

    /**
     * This is a port of file mirdwt_r.c File Name: MIRDWT.c Last Modification Date: 06/14/95 16:22:45 Current Version:
     * MIRDWT.c 2.4 File Creation Date: Wed Oct 12 08:44:43 1994 Author: Markus Lang <lang@jazz.rice.edu> Copyright (c)
     * 2000 RICE UNIVERSITY. All rights reserved. Created by Markus Lang, Department of ECE, Rice University. This
     * software is distributed and licensed to you on a non-exclusive basis, free-of-charge. Redistribution and use in
     * source and binary forms, with or without modification, are permitted provided that the following conditions are
     * met: 1. Redistribution of source code must retain the above copyright notice, this list of conditions and the
     * following disclaimer. 2. Redistribution in binary form must reproduce the above copyright notice, this list of
     * conditions and the following disclaimer in the documentation and/or other materials provided with the
     * distribution. 3. All advertising materials mentioning features or use of this software must display the following
     * acknowledgment: This product includes software developed by Rice University, Houston, Texas and its contributors.
     * 4. Neither the name of the University nor the names of its contributors may be used to endorse or promote
     * products derived from this software without specific prior written permission. THIS SOFTWARE IS PROVIDED BY
     * WILLIAM MARSH RICE UNIVERSITY, HOUSTON, TEXAS, AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES,
     * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
     * DISCLAIMED. IN NO EVENT SHALL RICE UNIVERSITY OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
     * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
     * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTIONS) HOWEVER CAUSED AND ON ANY THEORY OF
     * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE), PRODUCT LIABILITY,
     * OR OTHERWISE ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
     * DAMAGE. For information on commercial licenses, contact Rice University's Office of Technology Transfer at
     * techtran@rice.edu or (713) 348-6173 Change History: Fixed the code such that 1D vectors passed to it can be in
     * either passed as a row or column vector. Also took care of the code such that it will compile with both under
     * standard C compilers as well as for ANSI C compilers Jan Erik Odegard <odegard@ece.rice.edu> Wed Jun 14 1995 Fix
     * minor bug to allow maximum number of levels MATLAB description: %function res = mirdwt(yl,yh,h,L); % % function
     * computes the inverse redundant discrete wavelet transform y for a % 1D or 2D input signal. redundant means here
     * that the subsampling after % each stage of the forward transform has been omitted. yl contains the % lowpass and
     * yh the highpass components as computed, e.g., by mrdwt. In % case of a 2D signal the ordering in yh is [lh hl hh
     * lh hl ... ] (first % letter refers to row, second to column filtering). % % Input: % yl : lowpass component % yh
     * : highpass components % h : scaling filter % L : number of levels. in case of a 1D signal length(yl) must be %
     * divisible by 2^L; in case of a 2D signal the row and the % column dimension must be divisible by 2^L. % % Output:
     * % res : finite length 1D or 2D signal
     *
     * @param  res  DOCUMENT ME!
     * @param  m    DOCUMENT ME!
     * @param  n    DOCUMENT ME!
     * @param  h    DOCUMENT ME!
     * @param  lh   DOCUMENT ME!
     * @param  L    DOCUMENT ME!
     * @param  yl   DOCUMENT ME!
     * @param  yh   DOCUMENT ME!
     * @param  nh   columns of yh
     */
    private void MIRDWT(double[] res, int m, int n, double[] h, int lh, int L, double[] yl, double[] yh, int nh) {
        double[] xh = new double[m * n];
        double[] xdummyl = new double[Math.max(m, n)];
        double[] xdummyh = new double[Math.max(m, n)];
        double[] ydummyll = new double[Math.max(m, n) + lh - 1];
        double[] ydummylh = new double[Math.max(m, n) + lh - 1];
        double[] ydummyhl = new double[Math.max(m, n) + lh - 1];
        double[] ydummyhh = new double[Math.max(m, n) + lh - 1];
        double[] g0 = new double[lh];
        double[] g1 = new double[lh];
        int i;
        int actual_L, actual_m, actual_n, c_o_a, ir, n_c, n_cb, lhm1;
        int ic, n_r, n_rb, c_o_a_p2n, sample_f;

        if (n == 1) {
            n = m;
            m = 1;
        } // if (n == 1)

        /* Analysis lowpass and highpass */
        for (i = 0; i < lh; i++) {
            g0[i] = h[i] / 2;
            g1[i] = h[lh - i - 1] / 2;
        }

        for (i = 1; i <= lh; i += 2) {
            g1[i] = -g1[i];
        }

        lhm1 = lh - 1;

        /* 2^L */
        sample_f = 1;

        for (i = 1; i < L; i++) {
            sample_f = sample_f * 2;
        }

        actual_m = m / sample_f;
        actual_n = n / sample_f;

        /* Restore yl in res */
        for (i = 0; i < (m * n); i++) {
            res[i] = yl[i];
        }

        /* Main loop */
        for (actual_L = L; actual_L >= 1; actual_L--) {

            /* Actual (level dependent) column offset */
            if (m == 1) {
                c_o_a = n * (actual_L - 1);
            } else {
                c_o_a = 3 * n * (actual_L - 1);
            }

            c_o_a_p2n = c_o_a + (2 * n);

            /* Go by columns in case of a 2D signal */
            if (m > 1) {
                n_rb = m / actual_m; /* # of row blocks per column */

                for (ic = 0; ic < n; ic++) { /* Loop over column */

                    for (n_r = 0; n_r < n_rb; n_r++) { /* Loop within one column */

                        /* Store in dummy variables */
                        ir = -sample_f + n_r;

                        for (i = 0; i < actual_m; i++) {
                            ir = ir + sample_f;
                            ydummyll[i + lhm1] = res[ic + (n * ir)];
                            ydummylh[i + lhm1] = yh[c_o_a + ic + (nh * ir)];
                            ydummyhl[i + lhm1] = yh[c_o_a + n + ic + (nh * ir)];
                            ydummyhh[i + lhm1] = yh[c_o_a_p2n + ic + (nh * ir)];
                        } // for (i = 0; i < actual_m; i++)

                        /* Perform filtering and adding: first LL/LH, then HL/HH */
                        bpconv(xdummyl, actual_m, g0, g1, lh, ydummyll, ydummylh);
                        bpconv(xdummyh, actual_m, g0, g1, lh, ydummyhl, ydummyhh);

                        /* Store dummy variables in matrices */
                        ir = -sample_f + n_r;

                        for (i = 0; i < actual_m; i++) {
                            ir = ir + sample_f;
                            res[ic + (n * ir)] = xdummyl[i];
                            xh[ic + (n * ir)] = xdummyh[i];
                        } // for (i = 0; i < actual_m; i++)
                    } // for (n_r = 0; n_r < n_rb; n_r++)
                } // for (ic = 0; ic < n; ic++)
            } // if (m > 1)

            /* Go by rows */
            n_cb = n / actual_n; /* # of column blocks per row */

            for (ir = 0; ir < m; ir++) { /* Loop over rows */

                for (n_c = 0; n_c < n_cb; n_c++) { /* Loop within one row */

                    /* Store in dummy variable */
                    ic = -sample_f + n_c;

                    for (i = 0; i < actual_n; i++) {
                        ic = ic + sample_f;
                        ydummyll[i + lhm1] = res[ic + (n * ir)];

                        if (m > 1) {
                            ydummyhh[i + lhm1] = xh[ic + (n * ir)];
                        } else {
                            ydummyhh[i + lhm1] = yh[c_o_a + ic + (nh * ir)];
                        }
                    } // for (i = 0; i < actual_n; i++)

                    /* Perform filtering lowpass/highpass */
                    bpconv(xdummyl, actual_n, g0, g1, lh, ydummyll, ydummyhh);

                    /* Restore dummy variables in matrices */
                    ic = -sample_f + n_c;

                    for (i = 0; i < actual_n; i++) {
                        ic = ic + sample_f;
                        res[ic + (n * ir)] = xdummyl[i];
                    } // for (i = 0; i < actual_n; i++)
                } // for (n_c = 0; n_c < n_cb; n_c++)
            } // for (ir = 0; ir < m; ir++)

            sample_f = sample_f / 2;
            actual_m = actual_m * 2;
            actual_n = actual_n * 2;
        } // for (actual_L = L; actual_L >= 1; actual_L--)

        return;
    }

    /**
     * This is a port of mirdwt.c File Name: mirdwt.c Last Modification Date: %G% %U% Current Version: %M% %I% File
     * Creation Date: Wed Oct 12 08:44:43 1994 Author: Markus Lang <lang@jazz.rice.edu> Copyright: All software,
     * documentation, and related files in this distribution are Copyright (c) 1994 Rice University Permission is
     * granted for use and non-profit distribution providing that this notice be clearly maintained. The right to
     * distribute any portion for profit or as part of any commercial product is specifically reserved for the author.
     * Change History: Fixed code such that the result has the same dimension as the input for 1D problems. Also, added
     * some standard error checking. Jan Erik Odegard <odegard@ece.rice.edu> Wed Jun 14 1995
     *
     * @param   yl    DOCUMENT ME!
     * @param   n     First dimension of yl; number of columns of yl
     * @param   m     Second dimension of yl; number of rows of yl
     * @param   yh    DOCUMENT ME!
     * @param   nh    First dimension of yh; number of columns of yh
     * @param   mh    Second dimension of yh; number of rows of yh
     * @param   h     DOCUMENT ME!
     * @param   hcol  DOCUMENT ME!
     * @param   hrow  DOCUMENT ME!
     * @param   L     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] mirdwt(double[] yl, int n, int m, double[] yh, int nh, int mh, double[] h, int hcol, int hrow,
                            int L) {
        double[] res = new double[n * m];
        int lh;
        int twoPow;
        int i;

        if (hcol > hrow) {
            lh = hcol;
        } else {
            lh = hrow;
        }

        if (L < 0) {
            MipavUtil.displayError("The number of levels, L, must be a non-negative integer");
            error = 1;

            return null;
        }

        /* Check for consistency of rows and columns of yl, yh */
        if (Math.min(m, n) > 1) {

            if ((m != mh) || ((3 * n * L) != nh)) {
                System.out.println("m = " + m + " mh = " + mh + " n = " + n + " L = " + L + " nh = " + nh);
                MipavUtil.displayError("Dimensions of first two input matrices not consistent");
                error = 1;

                return null;
            }
        } else {

            if ((m != mh) || ((n * L) != nh)) {
                MipavUtil.displayError("Dimensions of first two input vectors not consistent");
                error = 1;

                return null;
            }
        }

        /* Check the row dimension of input */
        if (m > 1) {
            twoPow = 1;

            for (i = 0; i < L; i++) {
                twoPow *= 2;
            }

            if ((m % twoPow) != 0) {
                MipavUtil.displayError("The matrix row dimension must be a multiple of 2^L");
                error = 1;

                return null;
            }
        } // if (m > 1)

        /* Check the column dimension of input */
        if (n > 1) {
            twoPow = 1;

            for (i = 0; i < L; i++) {
                twoPow *= 2;
            }

            if ((n % twoPow) != 0) {
                MipavUtil.displayError("The matrix column dimension must be a multiple of 2^L");
                error = 1;

                return null;
            }
        } // if (n > 1)

        MIRDWT(res, m, n, h, lh, L, yl, yh, nh);

        return res;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   im  DOCUMENT ME!
     * @param   nx  DOCUMENT ME!
     * @param   ny  DOCUMENT ME!
     * @param   bx  DOCUMENT ME!
     * @param   by  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] mirrorExtension(double[] im, int nx, int ny, int bx, int by) {
        int x, y, xs, ys;
        int npx = nx + (2 * bx);
        int npy = ny + (2 * by);
        double[] pad = new double[npx * npy];

        for (y = 0; y < ny; y++) {

            for (x = 0; x < nx; x++) {
                pad[x + bx + (npx * (y + by))] = im[x + (nx * y)];
            }
        } // for (y = 0; y < ny; y++)

        for (y = 0, ys = (2 * by) - 1; y <= (by - 1); y++, ys--) {

            for (x = 0; x < npx; x++) {
                pad[x + (npx * y)] = pad[x + (npx * ys)];
            }
        }

        for (y = 0; y < npy; y++) {

            for (x = 0, xs = (2 * bx) - 1; x <= (bx - 1); x++, xs--) {
                pad[x + (npx * y)] = pad[xs + (npx * y)];
            }
        }

        for (y = ny + by, ys = ny + by - 1; y <= (ny + (2 * by) - 1); y++, ys--) {

            for (x = 0; x < npx; x++) {
                pad[x + (npx * y)] = pad[x + (npx * ys)];
            }
        }

        for (y = 0; y < npy; y++) {

            for (x = nx + bx, xs = nx + bx - 1; x <= (nx + (2 * bx) - 1); x++, xs--) {
                pad[x + (npx * y)] = pad[xs + (npx * y)];
            }
        }

        for (y = 0, ys = (2 * by) - 1; y <= (by - 1); y++, ys--) {

            for (x = 0, xs = (2 * bx) - 1; x <= (bx - 1); x++, xs--) {
                pad[x + (npx * y)] = pad[xs + (npx * ys)];
            }
        }

        for (y = ny + by, ys = ny + by - 1; y <= (ny + (2 * by) - 1); y++, ys--) {

            for (x = nx + bx, xs = nx + bx - 1; x <= (nx + (2 * bx) - 1); x++, xs--) {
                pad[x + (npx * y)] = pad[xs + (npx * ys)];
            }
        }

        for (y = 0, ys = (2 * by) - 1; y <= (by - 1); y++, ys--) {

            for (x = nx + bx, xs = nx + bx - 1; x <= (nx + (2 * bx) - 1); x++, xs--) {
                pad[x + (npx * y)] = pad[xs + (npx * ys)];
            }
        }

        for (y = ny + by, ys = ny + by - 1; y <= (ny + (2 * by) - 1); y++, ys--) {

            for (x = 0, xs = (2 * bx) - 1; x <= (bx - 1); x++, xs--) {
                pad[x + (npx * y)] = pad[xs + (npx * ys)];
            }
        }

        return pad;
    }

    /**
     * This is a port of the file modulateFlip.m by Eero Simoncelli, 7/96. QMF/Wavelet highpass filter construction:
     * modulate by (-1)^n, reverse order (and shift by one, which is handled by the convolution routines). This is an
     * extension of the original definition of QMF's (e.g., see Simoncelli90).
     *
     * @param   lfilt  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] modulateFlip(double[] lfilt) {
        double[] hfilt = null;
        int sz;
        int sz2;
        int[] ind;
        int i;

        sz = lfilt.length;
        sz2 = (int) Math.ceil(sz / 2.0);

        ind = new int[sz];

        for (i = 0; i < sz; i++) {
            ind[i] = sz - i;
        }

        hfilt = new double[sz];

        for (i = 0; i < sz; i++) {

            if (((ind[i] - sz2) % 2) == 0) {
                hfilt[i] = lfilt[ind[i] - 1];
            } else {
                hfilt[i] = -lfilt[ind[i] - 1];
            }
        }

        return hfilt;

    }

    /**
     * This is a port of MRDWT.c File Name: MRDWT.c Last Modification Date: 09/21/95 15:42:59 Current Version: MRDWT.c
     * 2.4 File Creation Date: Wed Oct 12 08:44:43 1994 Author: Markus Lang <lang@jazz.rice.edu> Copyright (c) 2000 RICE
     * UNIVERSITY. All rights reserved. Created by Markus Lang, Department of ECE, Rice University. This software is
     * distributed and licensed to you on a non-exclusive basis, free-of-charge. Redistribution and use in source and
     * binary forms, with or without modification, are permitted provided that the following conditions are met: 1.
     * Redistribution of source code must retain the above copyright notice, this list of conditions and the following
     * disclaimer. 2. Redistribution in binary form must reproduce the above copyright notice, this list of conditions
     * and the following disclaimer in the documentation and/or other materials provided with the distribution. 3. All
     * advertising materials mentioning features or use of this software must display the following acknowledgment: This
     * product includes software developed by Rice University, Houston, Texas and its contributors. 4. Neither the name
     * of the University nor the names of its contributors may be used to endorse or promote products derived from this
     * software without specific prior written permission. THIS SOFTWARE IS PROVIDED BY WILLIAM MARSH RICE UNIVERSITY,
     * HOUSTON, TEXAS, AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
     * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL RICE
     * UNIVERSITY OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
     * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
     * PROFITS; OR BUSINESS INTERRUPTIONS) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
     * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE), PRODUCT LIABILITY, OR OTHERWISE ARISING IN ANY WAY OUT OF
     * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. For information on commercial
     * licenses, contact Rice University's Office of Technology Transfer at techtran@rice.edu or (713) 348-6173 Change
     * History: Fixed the code such that 1D vectors passed to it can be in either passed as a row or column vector. Also
     * took care of the code such that it will compile with both under standard C compilers as well as for ANSI C
     * compilers Jan Erik Odegard <odegard@ece.rice.edu> Wed Jun 14 1995 MATLAB description: %[yl,yh] = mrdwt(im,h,L); %
     * % function computes the redundant discrete wavelet transform y for a 1D or % 2D input signal . redundant means
     * here that the subsampling after each % stage is omitted. yl contains the lowpass and yl the highpass %
     * components. In case of a 2D signal the ordering in yh is [lh hl hh lh hl % ... ] (first letter refers to row,
     * second to column filtering). % % Input: % im : finite length 1D or 2D signal (implicitely periodized) % h :
     * scaling filter % L : number of levels. in case of a 1D signal length(im) must be % divisible by 2^L; in case of a
     * 2D signal the row and the % column dimension must be divisible by 2^L. % % Output: % yl : lowpass component % yh
     * : highpass components % % see also: mdwt, midwt, mirdwt
     *
     * @param  im  DOCUMENT ME!
     * @param  m   DOCUMENT ME!
     * @param  n   DOCUMENT ME!
     * @param  h   DOCUMENT ME!
     * @param  lh  DOCUMENT ME!
     * @param  L   DOCUMENT ME!
     * @param  yl  DOCUMENT ME!
     * @param  yh  DOCUMENT ME!
     */
    private void MRDWT(double[] im, int m, int n, double[] h, int lh, int L, double[][] yl, double[][] yh) {
        int i;
        int j;
        double[] h0 = new double[lh];
        double[] h1 = new double[lh];
        double[] xdummyl = new double[Math.max(m, n) + lh - 1];
        double[] xdummyh = new double[Math.max(m, n) + lh - 1];
        double[] ydummyll = new double[Math.max(m, n)];
        double[] ydummylh = new double[Math.max(m, n)];
        double[] ydummyhl = new double[Math.max(m, n)];
        double[] ydummyhh = new double[Math.max(m, n)];
        int actual_m;
        int actual_n;
        int index;
        int sample_f;
        int actual_L;
        int c_o_a;
        int c_o_a_p2n;
        int n_cb;
        int ir;
        int n_c;
        int ic;
        int n_rb;
        int n_r;

        if (n == 1) {
            n = m;
            m = 1;
        }

        // Analysis lowpass and highpass
        for (i = 0; i < lh; i++) {
            h0[i] = h[lh - i - 1];
            h1[i] = h[i];
        }

        for (i = 0; i < lh; i += 2) {
            h1[i] = -h1[i];
        }

        actual_m = 2 * m;
        actual_n = 2 * n;

        for (j = 0, index = 0; j < m; j++) {

            for (i = 0; i < n; i++) {
                yl[j][i] = im[index++];
            }
        }

        // main loop
        sample_f = 1;

        for (actual_L = 1; actual_L <= L; actual_L++) {
            actual_m = actual_m / 2;
            actual_n = actual_n / 2;

            /* actual (level dependent) column offset */
            if (m == 1) {
                c_o_a = n * (actual_L - 1);
            } else {
                c_o_a = 3 * n * (actual_L - 1);
            }

            c_o_a_p2n = c_o_a + (2 * n);

            /* Go by rows */
            n_cb = n / actual_n; /* # of column blocks per row */

            for (ir = 0; ir < m; ir++) { /* Loop over rows */

                for (n_c = 0; n_c < n_cb; n_c++) { /* Loop within one row */

                    /* Store in dummy variable */
                    ic = -sample_f + n_c;

                    for (i = 0; i < actual_n; i++) {
                        ic = ic + sample_f;
                        xdummyl[i] = yl[ir][ic];
                    }

                    /* Perform filtering lowpass/highpass */
                    fpconv(xdummyl, actual_n, h0, h1, lh, ydummyll, ydummyhh);

                    /* Restore dummy variables in matrices */
                    ic = -sample_f + n_c;

                    for (i = 0; i < actual_n; i++) {
                        ic = ic + sample_f;
                        yl[ir][ic] = ydummyll[i];
                        yh[ir][c_o_a + ic] = ydummyhh[i];
                    } // for (i = 0; i < actual_n; i++)
                } // for (n_c = 0; n_c < n_cb; n_c++)
            } // for (ir = 0; ir < m; ir++)

            /* Go by columns in case of a 2D signal */
            if (m > 1) {
                n_rb = m / actual_m; /* # of row blocks per column */

                for (ic = 0; ic < n; ic++) { /* Loop over column */

                    for (n_r = 0; n_r < n_rb; n_r++) { /* Loop within one column */

                        /* Store in dummy variables */
                        ir = -sample_f + n_r;

                        for (i = 0; i < actual_m; i++) {
                            ir = ir + sample_f;
                            xdummyl[i] = yl[ir][ic];
                            xdummyh[i] = yh[ir][c_o_a + ic];
                        } // for (i = 0; i < actual_m; i++)

                        /* Perform filtering, first LL/LH, then HL/HH */
                        fpconv(xdummyl, actual_m, h0, h1, lh, ydummyll, ydummylh);
                        fpconv(xdummyh, actual_m, h0, h1, lh, ydummyhl, ydummyhh);

                        /* Restore dummy variables in matrices */
                        ir = -sample_f + n_r;

                        for (i = 0; i < actual_m; i++) {
                            ir = ir + sample_f;
                            yl[ir][ic] = ydummyll[i];
                            yh[ir][c_o_a + ic] = ydummylh[i];
                            yh[ir][c_o_a + n + ic] = ydummyhl[i];
                            yh[ir][c_o_a_p2n + ic] = ydummyhh[i];
                        } // for (i = 0; i < actual_m; i++)
                    } // for (n_r = 0; n_r < n_rb; n_r++)
                } // for (ic = 0; ic < n; ic++)
            } // if (m > 1)

            sample_f = sample_f * 2;
        } // for (actual_L = 1; actual_L <= L; actual_L++)

        return;
    }

    /**
     * This is a port of mrdwt.c File Name: mrdwt.c Last Modification Date: %G% %U% Current Version: %M% %I% File
     * Creation Date: Wed Oct 12 08:44:43 1994 Author: Markus Lang <lang@jazz.rice.edu>.
     *
     * <p>Copyright: All software, documentation, and related files in this distribution are Copyright (c) 1994 Rice
     * University</p>
     *
     * <p>Permission is granted for use and non-profit distribution providing that this notice be clearly maintained.
     * The right to distribute any portion for profit or as part of any commercial product is specifically reserved for
     * the author.</p>
     *
     * <p>Change History: Fixed code such that the result has the same dimension as the input for 1D problems. Also,
     * added some standard error checking. Jan Erik Odegard <odegard@ece.rice.edu> Wed Jun 14 1995</p>
     *
     * @param  im    DOCUMENT ME!
     * @param  n     columns of im, the x dimension of im
     * @param  m     rows of im, the y dimension of im
     * @param  h     DOCUMENT ME!
     * @param  hcol  DOCUMENT ME!
     * @param  hrow  DOCUMENT ME!
     * @param  L     DOCUMENT ME!
     * @param  yl    DOCUMENT ME!
     * @param  yh    DOCUMENT ME!
     */
    private void mrdwt(double[] im, int n, int m, double[] h, int hcol, int hrow, int L, double[][] yl, double[][] yh) {
        int lh;
        int denom;
        int i;

        if (hcol > hrow) {
            lh = hcol;
        } else {
            lh = hrow;
        }

        if (L < 0) {
            MipavUtil.displayError("The number of levels, L, must be a nonnegative integer");
            error = 1;

            return;
        }

        /* Check the row dimension of the input */
        if (m > 1) {
            denom = 1;

            for (i = 0; i < L; i++) {
                denom *= 2;
            }

            if ((m % denom) != 0) {
                MipavUtil.displayError("mrdwt must have m a multiple of 2^L");
                error = 1;

                return;
            }
        } // if (m > 1)

        /* Check the column dimension of the input */
        if (n > 1) {
            denom = 1;

            for (i = 0; i < L; i++) {
                denom *= 2;
            }

            if ((n % denom) != 0) {
                MipavUtil.displayError("mrdwt must have n a multiple of 2^L");
                error = 1;

                return;
            }
        } // if (n > 1)

        MRDWT(im, m, n, h, lh, L, yl, yh);
    }

    /**
     * Ported from named_filter.m by Eero Simoncelli, 6/96. Some standard 1D filter kernels. These are scaled such that
     * their L2-norm is 1.0. binomN - binomial coefficient filter of order N-1 haar: - Haar wavelet. qmf8, qmf12, qmf16
     * - Symmetric Quadrature Mirror Filters [Johnston80] daub2,daub3,daub4 - Daubechies wavelet [Daubechies88]. qmf5,
     * qmf9, qmf13: - Symmetric Quadrature Mirror Filters [Simoncelli88,Simoncelli90] References: [Johnston80] - J D
     * Johnston, "A filter family designed for use in quadrature mirror filter banks", Proc. ICASSP, pp 291-294, 1980.
     *
     * <p>[Daubechies88] - I Daubechies, "Orthonormal bases of compactly supported wavelets", Commun. Pure Appl. Math,
     * vol. 42, pp 909-996, 1988.</p>
     *
     * <p>[Simoncelli88] - E P Simoncelli, "Orthogonal sub-band image transforms", PhD Thesis, MIT Dept. of Elec. Eng.
     * and Comp. Sci. May 1988. Also available as: MIT Media Laboratory Vision and Modeling Technical Report #100.</p>
     *
     * <p>[Simoncelli90] - E P Simoncelli and E H Adelson, "Subband image coding", Subband Transforms, chapter 4, ed.
     * John W Woods, Kluwer Academic Publishers, Norwell, MA, 1990, pp 143--192.</p>
     *
     * @param   name          DOCUMENT ME!
     * @param   binomialSize  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] namedFilter(int name, int binomialSize) {
        double[] kernel = null;
        double sq2 = Math.sqrt(2.0);
        int i;

        if (name == BINOMIAL) {
            kernel = binomialFilter(binomialSize);

            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];
            }
        } else if (name == QMF5) {
            kernel = new double[] { -0.076103, 0.3535534, 0.8593118, 0.3535534, -0.076103 };
        } else if (name == QMF9) {
            kernel = new double[] {
                         0.02807382, -0.060944743, -0.073386624, 0.41472545, 0.7973934, 0.41472545, -0.073386624,
                         -0.060944743, 0.02807382
                     };
        } else if (name == QMF13) {
            kernel = new double[] {
                         -0.014556438, 0.021651438, 0.039045125, -0.09800052, -0.057827797, 0.42995453, 0.7737113,
                         0.42995453, -0.057827797, -0.09800052, 0.039045125, 0.021651438, -0.014556438
                     };
        } else if (name == QMF8) {
            kernel = new double[] {
                         0.00938715, -0.07065183, 0.06942827, 0.4899808, 0.4899808, 0.06942827, -0.07065183, 0.00938715
                     };

            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];
            }
        } else if (name == QMF12) {
            kernel = new double[] {
                         -0.003809699, 0.01885659, -0.002710326, -0.08469594, 0.08846992, 0.4843894, 0.4843894,
                         0.08846992, -0.08469594, -0.002710326, 0.01885659, -0.003809699
                     };

            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];
            }
        } else if (name == QMF16) {
            kernel = new double[] {
                         0.001050167, -0.005054526, -0.002589756, 0.0276414, -0.009666376, -0.09039223, 0.09779817,
                         0.4810284, 0.4810284, 0.09779817, -0.09039223, -0.009666376, 0.0276414, -0.002589756,
                         -0.005054526, 0.001050167
                     };

            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];
            }
        } else if (name == HAAR) {
            kernel = new double[] { 1.0 / sq2, 1.0 / sq2 };
        } else if (name == DAUB2) {
            kernel = new double[] { 0.482962913145, 0.836516303738, 0.224143868042, -0.129409522551 };
        } else if (name == DAUB3) {
            kernel = new double[] {
                         0.332670552950, 0.806891509311, 0.459877502118, -0.135011020010, -0.085441273882,
                         0.035226291882
                     };

        } else if (name == DAUB4) {
            kernel = new double[] {
                         0.230377813309, 0.714846570553, 0.630880767930, -0.027983769417, -0.187034811719,
                         0.030841381836, 0.032883011667, -0.010597401785
                     };

        } else if (name == GAUSS3) {

            // for backward compatibility
            kernel = new double[] { 0.25, 0.5, 0.25 };

            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];
            }
        } else if (name == GAUSS5) {

            // for backward compatibility
            kernel = new double[] { 0.0625, 0.25, 0.375, 0.25, 0.0625 };

            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];
            }
        } else {
            MipavUtil.displayError("Bad filter name = " + name);
            error = 1;
        }

        return kernel;
    }

    /**
     * This is a port of pointOp.c by Eero Simoncelli, 7/96 Apply a point operation, specified by lookup table LUT to
     * image array IM. LUT must be a row or column vector, and is assumed to contain (equi-spaced) samples of the
     * function. origin specifies the abscissa associated with the first sample, and increment specifies the spacing
     * between samples. Between- sample values are estimated via linear interpolation. If warnings is true, the function
     * outputs a warning whenever the lookup table is extrapolated. The drawbacks are that the lookup table must be
     * equi-spaced, and the interpolation is linear.
     *
     * @param   im         DOCUMENT ME!
     * @param   lut        DOCUMENT ME!
     * @param   origin     DOCUMENT ME!
     * @param   increment  DOCUMENT ME!
     * @param   warnings   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[][] pointOp(double[][] im, double[] lut, double origin, double increment, boolean warnings) {
        int imy = im.length;
        int imx = im[0].length;
        double[][] res = new double[imy][imx];
        int lutsize = lut.length;
        int i, j;
        double pos;
        int index;
        lutsize = lutsize - 2; // Maximum index value

        boolean l_unwarned = warnings;
        boolean r_unwarned = warnings;

        if (increment > 0) {

            for (j = 0; j < imy; j++) {

                for (i = 0; i < imx; i++) {
                    pos = (im[j][i] - origin) / increment;
                    index = (int) pos; // Floor

                    if (index < 0) {
                        index = 0;

                        if (l_unwarned) {
                            MipavUtil.displayWarning("Warning: Extrapolating to left of lookup table...");
                            l_unwarned = false;
                        }
                    } // if (index < 0)
                    else if (index > lutsize) {
                        index = lutsize;

                        if (r_unwarned) {
                            MipavUtil.displayWarning("Warning: Extrapolating to right of lookup table...");
                            r_unwarned = false;
                        }
                    } // else if (index > lutsize)

                    res[j][i] = lut[index] + ((lut[index + 1] - lut[index]) * (pos - index));
                } // for (i = 0; i < imx; i++)
            } // for (j = 0; j < imy; j++)
        } // if (increment > 0)
        else {

            for (j = 0; j < imy; j++) {

                for (i = 0; i < imx; i++) {
                    res[j][i] = lut[0];
                }
            }
        }

        return res;
    }

    /**
     * This duplicates the functionality of pyrBand.m and pyrBandIndices.m.
     *
     * @param    pyr   DOCUMENT ME!
     * @param    pind  DOCUMENT ME!
     * @param    band  DOCUMENT ME!
     * @param    x     DOCUMENT ME!
     * @param    y     DOCUMENT ME!
     *
     * @rows     DOCUMENT ME!
     * @columns  DOCUMENT ME!
     *
     * @return   DOCUMENT ME!
     */
    private double[] pyrBand(Vector pyr, Vector pind, int band, int[] x, int[] y) {
        double[] arr;
        int[] intMat;

        arr = (double[]) pyr.get(band);
        intMat = (int[]) pind.get(band);

        x[0] = intMat[0];
        y[0] = intMat[1];

        return arr;
    }

    /**
     * This is a port of a MATLAB rcosFn.m written by Eero Simoncelli, 7/96. Returns a lookup table containing a "raised
     * cosine" soft threshold function: Y = value1 + (value2 - value1) * cos^2(PI/2 * (X - position + width)/ width)
     *
     * @param  width     width of the region over which the transition occurs (default = 1)
     * @param  position  the location of the center of the threshold (default = 0).
     * @param  value1    Value to the left of the transition
     * @param  value2    Value to the right of the transition
     * @param  X         DOCUMENT ME!
     * @param  Y         DOCUMENT ME!
     */
    private void rcosFn(double width, double position, double value1, double value2, double[] X, double[] Y) {
        int i, j;
        int sz = 256; // arbitrary
        double var;

        for (i = -sz - 1, j = 0; i <= 1; i++, j++) {
            X[j] = Math.PI * i / (2.0 * sz);
        }

        for (i = 0; i < X.length; i++) {
            var = Math.cos(X[i]);
            var = var * var;
            Y[i] = value1 + ((value2 - value1) * var);
        }

        // Make sure end values are repeated, for extrapolation...
        Y[0] = Y[1];
        Y[sz + 2] = Y[sz + 1];

        for (i = 0; i < X.length; i++) {
            X[i] = position + ((2.0 * width / Math.PI) * (X[i] + (Math.PI / 4.0)));
        }

        return;
    }

    /**
     * This is a port of reconFullSFpyr2.m Reconstruct image from its steerable pyramid representation, in the Fourier
     * domain, as created by buildSFpyr. Unlike the standard transform, subdivides the highpass band into orientations
     *
     * @param   pyr     DOCUMENT ME!
     * @param   pind    DOCUMENT ME!
     * @param   levs    DOCUMENT ME!
     * @param   bands   DOCUMENT ME!
     * @param   twidth  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] reconFullSFpyr2(Vector pyr, Vector pind, int[] levs, int[] bands, double twidth) {
        boolean allLevs = false;
        boolean allBands = false;
        int nbands;
        int maxLev;
        int i;
        int j;
        int dimX;
        int dimY;
        int ctrX;
        int ctrY;
        int[][] intArr;
        int[] intMat;
        double[][] xramp;
        double[][] yramp;
        double var;
        double[][] angle;
        double[][] log_rad;
        double[] xrcos = null;
        ;

        double[] yrcos = null;
        double[] yircos;
        boolean haveOne;
        double[][] resdft = new double[2][];
        FFTUtility fftUtil;
        double[][] lo0mask;
        int index;
        boolean haveZero;
        double[][] hi0mask;
        double[][] arr;
        int lutsize;
        double[] xcosn;
        int order;
        double consta;
        double[] ycosn;
        int b;
        boolean haveB;
        double[][] anglemask;
        double[] bandr;
        double[] bandi;
        double[][] maskr;
        double[][] maski;
        int[] x = new int[1];
        int[] y = new int[1];
        int ind;

        if (levs == null) {
            allLevs = true;
        }

        if (bands == null) {
            allBands = true;
        }

        if (twidth <= 0) {
            MipavUtil.displayWarning("Warning.  twidth must be positive.  Setting to 1");
            twidth = 1.0;
        }

        intMat = new int[2];

        // Dummy highpass [ [0 0]; pind] designated at end of buildFullSFpyr2
        pind.insertElementAt(intMat, 0);

        nbands = spyrNumBands(pind) / 2;

        intArr = new int[nbands][];

        for (i = 0; i < nbands; i++) {
            intArr[i] = (int[]) pind.remove(0);
        }

        maxLev = 2 + spyrHt(pind);

        // Don't restore dummy highpass band designated at end of buildFullSFpyr2
        for (i = nbands - 1; i >= 1; i--) {
            pind.insertElementAt(intArr[i], 0);
        }

        if (allLevs) {
            levs = new int[maxLev + 1];

            for (i = 0; i <= maxLev; i++) {
                levs[i] = i;
            }
        } // if (allLevs)
        else {

            for (i = 0; i < levs.length; i++) {

                if ((levs[i] > maxLev) || (levs[i] < 0)) {
                    MipavUtil.displayError("Level numbers must be in the range 0 to " + maxLev);
                    error = 1;

                    return null;
                }
            }
        } // else

        if (allBands) {
            bands = new int[nbands];

            for (i = 1; i <= nbands; i++) {
                bands[i - 1] = i;
            }
        } // if (allBands)
        else {

            for (i = 0; i < bands.length; i++) {

                if ((bands[i] < 1) || (bands[i] > nbands)) {
                    MipavUtil.displayError("Band numbers must be in the range 1 to " + nbands);
                    error = 1;

                    return null;
                }
            }
        } // else

        intMat = (int[]) pind.get(0);
        dimX = intMat[0];
        dimY = intMat[1];
        ctrX = (int) Math.ceil((dimX + 0.5) / 2.0);
        ctrY = (int) Math.ceil((dimY + 0.5) / 2.0);

        xramp = new double[dimY][dimX];
        yramp = new double[dimY][dimX];

        for (i = 0; i < dimX; i++) {
            var = ((i + 1.0) - ctrX) / (dimX / 2.0);

            for (j = 0; j < dimY; j++) {
                xramp[j][i] = var;
            }
        }

        for (j = 0; j < dimY; j++) {
            var = ((j + 1.0) - ctrY) / (dimY / 2.0);

            for (i = 0; i < dimX; i++) {
                yramp[j][i] = var;
            }
        }

        angle = new double[dimY][dimX];
        log_rad = new double[dimY][dimX];

        for (j = 0; j < dimY; j++) {

            for (i = 0; i < dimX; i++) {
                angle[j][i] = Math.atan2(yramp[j][i], xramp[j][i]);
                log_rad[j][i] = Math.sqrt((xramp[j][i] * xramp[j][i]) + (yramp[j][i] * yramp[j][i]));
            }
        }

        log_rad[ctrY - 1][ctrX - 1] = log_rad[ctrY - 1][ctrX - 2];

        // log2(x) = loge(x)/loge(2)
        var = 1.0 / Math.log(2.0);

        for (j = 0; j < dimY; j++) {

            for (i = 0; i < dimX; i++) {
                log_rad[j][i] = var * Math.log(log_rad[j][i]);
            }
        }

        // Radial transition function (a raised cosine in log-frequency)
        xrcos = new double[259];
        yrcos = new double[259];
        rcosFn(twidth, (-twidth / 2.0), 0.0, 1.0, xrcos, yrcos);

        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }

        yircos = new double[yrcos.length];

        for (i = 0; i < yrcos.length; i++) {
            yircos[i] = Math.sqrt(1.0 - (yrcos[i] * yrcos[i]));
        }

        if (pind.size() == 1) {
            haveOne = false;

            for (i = 0; i < levs.length; i++) {

                if (levs[i] == 1) {
                    haveOne = true;
                }
            }

            if (haveOne) {
                resdft[0] = pyrBand(pyr, pind, 0, x, y);
                resdft[1] = new double[x[0] * y[0]];

                // forward FFT
                fftUtil = new FFTUtility(resdft[0], resdft[1], y[0], x[0], 1, -1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(resdft[0], resdft[1], 1, y[0], x[0], -1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();
                center(resdft[0], resdft[1], x[0], y[0]);
            } else {
                intMat = (int[]) pind.get(0);
                x[0] = intMat[0];
                y[0] = intMat[1];
                resdft[0] = new double[x[0] * y[0]];
                resdft[1] = new double[x[0] * y[0]];
            }
        } // if (pind.size() == 1)
        else {
            arr = new double[nbands][];
            intArr = new int[nbands][];

            for (i = 0; i < nbands; i++) {
                arr[i] = (double[]) pyr.remove(0);
                intArr[i] = (int[]) pind.remove(0);
            }

            resdft = reconSFpyrLevs(pyr, pind, log_rad, xrcos, yrcos, angle, nbands, levs, bands);

            for (i = nbands - 1; i >= 0; i--) {
                pyr.insertElementAt(arr[i], 0);
                pind.insertElementAt(intArr[i], 0);
            }

        } // else

        lo0mask = pointOp(log_rad, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);

        for (j = 0; j < dimY; j++) {

            for (i = 0; i < dimX; i++) {
                index = i + (j * dimX);
                resdft[0][index] = resdft[0][index] * lo0mask[j][i];
                resdft[1][index] = resdft[1][index] * lo0mask[j][i];
            }
        }

        haveZero = false;

        for (i = 0; i < levs.length; i++) {

            if (levs[i] == 0) {
                haveZero = true;
            }
        }

        // Oriented highpass bands
        if (haveZero) {
            lutsize = 1024;
            xcosn = new double[3 * (lutsize + 1)];

            // -2*PI to PI
            for (i = 0, j = -((2 * lutsize) + 1); i < xcosn.length; i++, j++) {
                xcosn[i] = j * Math.PI / lutsize;
            }

            order = nbands - 1;

            // Divide by sqrt(sum_(n=0)^(N-1) cos(pi*n/N)^(2(N-1)) )
            var = factorial(order);
            consta = Math.pow(2.0, (2.0 * order)) * var * var / (nbands * factorial(2 * order));
            consta = Math.sqrt(consta);
            ycosn = new double[xcosn.length];

            for (i = 0; i < xcosn.length; i++) {
                ycosn[i] = consta * Math.pow(Math.cos(xcosn[i]), order);
            }

            hi0mask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);

            ind = 0;

            for (b = 1; b <= nbands; b++) {
                haveB = false;

                for (i = 0; i < bands.length; i++) {

                    if (bands[i] == b) {
                        haveB = true;
                    }
                }

                if (haveB) {
                    anglemask = pointOp(angle, ycosn, (xcosn[0] + (Math.PI * (b - 1) / (double) nbands)),
                                        (xcosn[1] - xcosn[0]), true);
                    bandr = (double[]) pyr.get(ind);
                    bandi = new double[dimX * dimY];

                    // forward FFT
                    fftUtil = new FFTUtility(bandr, bandi, dimY, dimX, 1, -1, FFTUtility.FFT);
                    fftUtil.run();
                    fftUtil.finalize();
                    fftUtil = new FFTUtility(bandr, bandi, 1, dimY, dimX, -1, FFTUtility.FFT);
                    fftUtil.run();
                    fftUtil.finalize();
                    center(bandr, bandi, dimX, dimY);

                    // Make real the contents in the HF cross (to avoid information loss in these freqs.)
                    // It distributes evenly these contents among the nbands orientations
                    maskr = new double[dimY][dimX];
                    maski = new double[dimY][dimX];

                    if (((nbands - 1) % 4) == 0) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                maskr[j][i] = anglemask[j][i] * hi0mask[j][i];
                            }
                        }
                    } // if (((nbands-1) % 4) == 0)
                    else if (((nbands - 1) % 4) == 1) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                maski[j][i] = anglemask[j][i] * hi0mask[j][i];
                            }
                        }
                    } // else if (((nbands-1) %4) == 1)
                    else if (((nbands - 1) % 4) == 2) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                maskr[j][i] = -anglemask[j][i] * hi0mask[j][i];
                            }
                        }
                    } // else if (((nbands-1) %4) == 2)
                    else if (((nbands - 1) % 4) == 3) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                maski[j][i] = -anglemask[j][i] * hi0mask[j][i];
                            }
                        }
                    } // else if (((nbands-1) %4) == 3)

                    var = 1.0 / Math.sqrt(nbands);

                    for (i = 0; i < dimX; i++) {
                        maskr[0][i] = var;
                        maski[0][i] = 0.0;
                    }

                    for (j = 1; j < dimY; j++) {
                        maskr[j][0] = var;
                        maski[j][0] = 0.0;
                    }

                    for (j = 0; j < dimY; j++) {

                        for (i = 0; i < dimX; i++) {
                            index = i + (j * dimX);
                            resdft[0][index] = resdft[0][index] + (bandr[index] * maskr[j][i]) -
                                               (bandi[index] * maski[j][i]);
                            resdft[1][index] = resdft[1][index] + (bandr[index] * maski[j][i]) +
                                               (bandi[index] * maskr[j][i]);
                        }
                    }
                } // if (haveB)

                ind++;
            } // for (b = 1; b <= nbands; b++)
        } // if (haveZero)

        x[0] = lo0mask[0].length;
        y[0] = lo0mask.length;
        center(resdft[0], resdft[1], x[0], y[0]);

        // Inverse FFT
        fftUtil = new FFTUtility(resdft[0], resdft[1], y[0], x[0], 1, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(resdft[0], resdft[1], 1, y[0], x[0], +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();

        return resdft[0];
    }

    /**
     * This is a port of reconSFpyr.m by Eero Simoncelli, 5/97. Reconstruct image from its steerable pyramid
     * representation, in the Fourier domain, as created by buildSFpyr.
     *
     * @param   pyr     A vector containing the N pyramid subbands, ordered from fine to coarse
     * @param   pind    A vector with a N 2 element int[] set of the sizes of each subband
     * @param   levs    Optional. A list of the levels to include or null for all levels. 0 corresponds to the residual
     *                  highpass subband. 1 corresponds to the finest oriented scale. The lowpass band corresponds to
     *                  number spyrHt(indices)+1.
     * @param   bands   Optional. Should be a list of the bands to include or null for all bands. 1 = vertical, rest
     *                  proceeding anti- clockwise.
     * @param   twidth  The width of the transition region of the radial lowpass function, in octaves. Default = 1,
     *                  which gives a raised cosine for the bandpass filters.
     *
     * @return  DOCUMENT ME!
     */
    private double[] reconSFpyr(Vector pyr, Vector pind, int[] levs, int[] bands, double twidth) {
        boolean allLevs = false;
        boolean allBands = false;
        int nbands;
        int maxLev;
        int i;
        int j;
        int dimX;
        int dimY;
        int ctrX;
        int ctrY;
        double[][] xramp;
        double[][] yramp;
        double var;
        double[][] angle;
        double[][] log_rad;
        double[] xrcos = null;
        double[] yrcos = null;
        double[] yircos;
        boolean haveOne;
        int[] px = new int[1];
        int[] py = new int[1];
        double[][] resdft = new double[2][];
        FFTUtility fftUtil;
        double[][] lo0mask;
        int index;
        boolean haveZero;
        double[][] hi0mask;
        double[] arr;
        int x;
        int y;
        double[] imag;
        int hix;
        int hiy;
        int[] intMat;

        if (levs == null) {
            allLevs = true;
        }

        if (bands == null) {
            allBands = true;
        }

        if (twidth <= 0) {
            MipavUtil.displayWarning("Warning.  twidth must be positive.  Setting to 1");
            twidth = 1.0;
        }

        nbands = spyrNumBands(pind);

        maxLev = 1 + spyrHt(pind);

        if (allLevs) {
            levs = new int[maxLev + 1];

            for (i = 0; i <= maxLev; i++) {
                levs[i] = i;
            }
        } // if (allLevs)
        else {

            for (i = 0; i < levs.length; i++) {

                if ((levs[i] > maxLev) || (levs[i] < 0)) {
                    MipavUtil.displayError("Level numbers must be in the range 0 to " + maxLev);
                    error = 1;

                    return null;
                }
            }
        } // else

        if (allBands) {
            bands = new int[nbands];

            for (i = 1; i <= nbands; i++) {
                bands[i - 1] = i;
            }
        } // if (allBands)
        else {

            for (i = 0; i < bands.length; i++) {

                if ((bands[i] < 1) || (bands[i] > nbands)) {
                    MipavUtil.displayError("Band numbers must be in the range 1 to 3 " + nbands);
                    error = 1;

                    return null;
                }
            }
        } // else

        intMat = (int[]) pind.get(0);
        dimX = intMat[0];
        dimY = intMat[1];
        ctrX = (int) Math.ceil((dimX + 0.5) / 2.0);
        ctrY = (int) Math.ceil((dimY + 0.5) / 2.0);

        xramp = new double[dimY][dimX];
        yramp = new double[dimY][dimX];

        for (i = 0; i < dimX; i++) {
            var = ((i + 1.0) - ctrX) / (dimX / 2.0);

            for (j = 0; j < dimY; j++) {
                xramp[j][i] = var;
            }
        }

        for (j = 0; j < dimY; j++) {
            var = ((j + 1.0) - ctrY) / (dimY / 2.0);

            for (i = 0; i < dimX; i++) {
                yramp[j][i] = var;
            }
        }

        angle = new double[dimY][dimX];
        log_rad = new double[dimY][dimX];

        for (j = 0; j < dimY; j++) {

            for (i = 0; i < dimX; i++) {
                angle[j][i] = Math.atan2(yramp[j][i], xramp[j][i]);
                log_rad[j][i] = Math.sqrt((xramp[j][i] * xramp[j][i]) + (yramp[j][i] * yramp[j][i]));
            }
        }

        log_rad[ctrY - 1][ctrX - 1] = log_rad[ctrY - 1][ctrX - 2];

        // log2(x) = loge(x)/loge(2)
        var = 1.0 / Math.log(2.0);

        for (j = 0; j < dimY; j++) {

            for (i = 0; i < dimX; i++) {
                log_rad[j][i] = var * Math.log(log_rad[j][i]);
            }
        }

        // Radial transition function (a raised cosine in log-frequency)
        xrcos = new double[259];
        yrcos = new double[259];
        rcosFn(twidth, (-twidth / 2.0), 0.0, 1.0, xrcos, yrcos);

        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }

        yircos = new double[yrcos.length];

        for (i = 0; i < yrcos.length; i++) {
            yircos[i] = Math.sqrt(Math.abs(1.0 - (yrcos[i] * yrcos[i])));
        }

        if (pind.size() == 2) {
            haveOne = false;

            for (i = 0; i < levs.length; i++) {

                if (levs[i] == 1) {
                    haveOne = true;
                }
            }

            if (haveOne) {
                resdft[0] = pyrBand(pyr, pind, 1, px, py);
                resdft[1] = new double[px[0] * py[0]];

                // forward FFT
                fftUtil = new FFTUtility(resdft[0], resdft[1], py[0], px[0], 1, -1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(resdft[0], resdft[1], 1, py[0], px[0], -1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();
                center(resdft[0], resdft[1], px[0], py[0]);
            } else {
                intMat = (int[]) pind.get(1);
                px[0] = intMat[0];
                py[0] = intMat[1];
                resdft[0] = new double[px[0] * py[0]];
                resdft[1] = new double[px[0] * py[0]];
            }
        } // if (pind.size() == 2)
        else {
            arr = (double[]) pyr.remove(0);
            intMat = (int[]) pind.remove(0);
            resdft = reconSFpyrLevs(pyr, pind, log_rad, xrcos, yrcos, angle, nbands, levs, bands);
            pyr.insertElementAt(arr, 0);
            pind.insertElementAt(intMat, 0);
        } // else

        lo0mask = pointOp(log_rad, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);

        for (j = 0; j < lo0mask.length; j++) {

            for (i = 0; i < lo0mask[0].length; i++) {
                index = i + (j * lo0mask[0].length);
                resdft[0][index] = resdft[0][index] * lo0mask[j][i];
                resdft[1][index] = resdft[1][index] * lo0mask[j][i];
            }
        }

        // residual highpass subband
        haveZero = false;

        for (i = 0; i < levs.length; i++) {

            if (levs[i] == 0) {
                haveZero = true;
            }
        }

        if (haveZero) {
            hi0mask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);
            arr = (double[]) pyr.get(0);
            intMat = (int[]) pind.get(0);
            x = intMat[0];
            y = intMat[1];
            imag = new double[x * y];

            // forward FFT
            fftUtil = new FFTUtility(arr, imag, y, x, 1, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(arr, imag, 1, y, x, -1, FFTUtility.FFT);
            fftUtil.run();
            fftUtil.finalize();
            center(arr, imag, x, y);
            hiy = hi0mask.length;
            hix = hi0mask[0].length;

            for (j = 0; j < hiy; j++) {

                for (i = 0; i < hix; i++) {
                    index = i + (j * hix);
                    arr[index] = arr[index] * hi0mask[j][i];
                    imag[index] = imag[index] * hi0mask[j][i];
                    resdft[0][index] = resdft[0][index] + arr[index];
                    resdft[1][index] = resdft[1][index] + imag[index];
                }
            }
        } // if (haveZero)

        px[0] = lo0mask[0].length;
        py[0] = lo0mask.length;
        center(resdft[0], resdft[1], px[0], py[0]);

        // Inverse FFT
        fftUtil = new FFTUtility(resdft[0], resdft[1], py[0], px[0], 1, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(resdft[0], resdft[1], 1, py[0], px[0], +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();

        return resdft[0];
    }

    /**
     * This is a port of reconSFpyrLevs.m written by Eero Simoncelli, 5/97. Recursive function for constructing levels
     * of a steerable pyramid representation. This is called by reconSFpyr, and is not usually called directly.
     *
     * @param   pyr           DOCUMENT ME!
     * @param   pind          DOCUMENT ME!
     * @param   log_rad_orig  DOCUMENT ME!
     * @param   xrcos         DOCUMENT ME!
     * @param   yrcos         DOCUMENT ME!
     * @param   angle         DOCUMENT ME!
     * @param   nbands        DOCUMENT ME!
     * @param   levs          DOCUMENT ME!
     * @param   bands         DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[][] reconSFpyrLevs(Vector pyr, Vector pind, double[][] log_rad_orig, double[] xrcos, double[] yrcos,
                                      double[][] angle, int nbands, int[] levs, int[] bands) {
        int lo_ind;
        int dimX;
        int dimY;
        int ctrX;
        int ctrY;
        int logx;
        int logy;
        int i;
        int j;
        boolean moreOne;
        int lodimX;
        int lodimY;
        int loctrX;
        int loctrY;
        int lostartX;
        int lostartY;
        int loendX;
        int loendY;
        double[][] nlog_rad;
        double[][] nangle;
        double[][] arr;
        double[][] resdft = new double[2][];
        double[][] nresdft = new double[2][];
        int[] levsm1;
        int[] px;
        int[] py;
        FFTUtility fftUtil;
        double[] yircos;
        double[][] lomask;
        int i2;
        int j2;
        int index;
        int index2;
        boolean haveOne;
        int lutsize;
        double[] xcosn;
        int order;
        double consta;
        double var;
        double[] ycosn;
        double[][] himask;
        int b;
        boolean haveB;
        double[][] anglemask;
        double[] bandr;
        double[] bandi;
        int[] intMat;
        int[][] parr;
        int ind;
        double[][] log_rad;

        lo_ind = nbands + 1;

        intMat = (int[]) pind.get(0);
        dimX = intMat[0];
        dimY = intMat[1];
        ctrX = (int) Math.ceil((dimX + 0.5) / 2.0);
        ctrY = (int) Math.ceil((dimY + 0.5) / 2.0);

        logy = log_rad_orig.length;
        logx = log_rad_orig[0].length;
        log_rad = new double[logy][logx];

        for (j = 0; j < logy; j++) {

            for (i = 0; i < logx; i++) {
                log_rad[j][i] = log_rad_orig[j][i] + 1.0;
            }
        }

        moreOne = false;

        for (i = 0; i < levs.length; i++) {

            if (levs[i] > 1) {
                moreOne = true;
            }
        }

        if (moreOne) {
            lodimX = (int) Math.ceil((dimX - 0.5) / 2.0);
            lodimY = (int) Math.ceil((dimY - 0.5) / 2.0);
            loctrX = (int) Math.ceil((lodimX + 0.5) / 2.0);
            loctrY = (int) Math.ceil((lodimY + 0.5) / 2.0);
            lostartX = ctrX - loctrX + 1;
            lostartY = ctrY - loctrY + 1;
            loendX = lostartX + lodimX - 1;
            loendY = lostartY + lodimY - 1;
            nlog_rad = new double[loendY - lostartY + 1][loendX - lostartX + 1];
            nangle = new double[loendY - lostartY + 1][loendX - lostartX + 1];

            for (j = 0; j < (loendY - lostartY + 1); j++) {

                for (i = 0; i < (loendX - lostartX + 1); i++) {
                    nlog_rad[j][i] = log_rad[j + lostartY - 1][i + lostartX - 1];
                    nangle[j][i] = angle[j + lostartY - 1][i + lostartX - 1];
                }
            }

            if (pind.size() > lo_ind) {
                arr = new double[lo_ind - 1][];
                parr = new int[(lo_ind - 1)][2];

                for (i = 0; i < (lo_ind - 1); i++) {
                    arr[i] = (double[]) pyr.remove(0);
                }

                for (i = 0; i < (lo_ind - 1); i++) {
                    parr[i] = (int[]) pind.remove(0);
                }

                levsm1 = new int[levs.length];

                for (i = 0; i < levs.length; i++) {
                    levsm1[i] = levs[i] - 1;
                }

                nresdft = reconSFpyrLevs(pyr, pind, nlog_rad, xrcos, yrcos, nangle, nbands, levsm1, bands);

                for (i = lo_ind - 2; i >= 0; i--) {
                    pyr.insertElementAt(arr[i], 0);
                    pind.insertElementAt(parr[i], 0);
                }
            } // if (pind.size() > lo_ind)
            else {
                px = new int[1];
                py = new int[1];
                nresdft[0] = pyrBand(pyr, pind, lo_ind - 1, px, py);
                nresdft[1] = new double[px[0] * py[0]];

                // forward FFT
                fftUtil = new FFTUtility(nresdft[0], nresdft[1], py[0], px[0], 1, -1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(nresdft[0], nresdft[1], 1, py[0], px[0], -1, FFTUtility.FFT);
                fftUtil.run();
                fftUtil.finalize();
                center(nresdft[0], nresdft[1], px[0], py[0]);
            } // else

            yircos = new double[yrcos.length];

            for (i = 0; i < yrcos.length; i++) {
                yircos[i] = Math.sqrt(Math.abs(1.0 - (yrcos[i] * yrcos[i])));
            }

            lomask = pointOp(nlog_rad, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);

            resdft[0] = new double[dimX * dimY];
            resdft[1] = new double[dimX * dimY];

            for (j = lostartY - 1, j2 = 0; j < loendY; j++, j2++) {

                for (i = lostartX - 1, i2 = 0; i < loendX; i++, i2++) {
                    index = i + (j * dimX);
                    index2 = i2 + (j2 * (loendX - lostartX + 1));
                    resdft[0][index] = nresdft[0][index2] * lomask[j2][i2];
                    resdft[1][index] = nresdft[1][index2] * lomask[j2][i2];
                }
            }
        } // if (moreOne)
        else {
            resdft[0] = new double[dimX * dimY];
            resdft[1] = new double[dimX * dimY];
        } // else

        haveOne = false;

        for (i = 0; i < levs.length; i++) {

            if (levs[i] == 1) {
                haveOne = true;
            }
        }

        if (haveOne) {
            lutsize = 1024;
            xcosn = new double[3 * (lutsize + 1)];

            // -2*PI to PI
            for (i = 0, j = -((2 * lutsize) + 1); i < xcosn.length; i++, j++) {
                xcosn[i] = j * Math.PI / lutsize;
            }

            order = nbands - 1;

            // Divide by sqrt(sum_(n=0)^(N-1) cos(pi*n/N)^(2(N-1)) )
            var = factorial(order);
            consta = Math.pow(2.0, (2.0 * order)) * var * var / (nbands * factorial(2 * order));
            consta = Math.sqrt(consta);
            ycosn = new double[xcosn.length];

            for (i = 0; i < xcosn.length; i++) {
                ycosn[i] = consta * Math.pow(Math.cos(xcosn[i]), order);
            }

            himask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);

            ind = 0;

            for (b = 1; b <= nbands; b++) {
                haveB = false;

                for (i = 0; i < bands.length; i++) {

                    if (bands[i] == b) {
                        haveB = true;
                    }
                }

                if (haveB) {
                    anglemask = pointOp(angle, ycosn, (xcosn[0] + (Math.PI * (b - 1) / (double) nbands)),
                                        (xcosn[1] - xcosn[0]), true);
                    bandr = (double[]) pyr.get(ind);
                    bandi = new double[dimX * dimY];

                    // forward FFT
                    fftUtil = new FFTUtility(bandr, bandi, dimY, dimX, 1, -1, FFTUtility.FFT);
                    fftUtil.run();
                    fftUtil.finalize();
                    fftUtil = new FFTUtility(bandr, bandi, 1, dimY, dimX, -1, FFTUtility.FFT);
                    fftUtil.run();
                    fftUtil.finalize();
                    center(bandr, bandi, dimX, dimY);

                    if (((nbands - 1) % 4) == 0) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                index = i + (j * dimX);
                                resdft[0][index] = resdft[0][index] + (bandr[index] * anglemask[j][i] * himask[j][i]);
                                resdft[1][index] = resdft[1][index] + (bandi[index] * anglemask[j][i] * himask[j][i]);
                            }
                        }
                    } // if (((nbands -1) % 4) == 0)
                    else if (((nbands - 1) % 4) == 1) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                index = i + (j * dimX);
                                resdft[0][index] = resdft[0][index] - (bandi[index] * anglemask[j][i] * himask[j][i]);
                                resdft[1][index] = resdft[1][index] + (bandr[index] * anglemask[j][i] * himask[j][i]);
                            }
                        }
                    } // else if (((nbands-1) % 4) == 1)
                    else if (((nbands - 1) % 4) == 2) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                index = i + (j * dimX);
                                resdft[0][index] = resdft[0][index] - (bandr[index] * anglemask[j][i] * himask[j][i]);
                                resdft[1][index] = resdft[1][index] - (bandi[index] * anglemask[j][i] * himask[j][i]);
                            }
                        }
                    } // else if (((nbands-1) % 4) == 2)
                    else if (((nbands - 1) % 4) == 3) {

                        for (j = 0; j < dimY; j++) {

                            for (i = 0; i < dimX; i++) {
                                index = i + (j * dimX);
                                resdft[0][index] = resdft[0][index] + (bandi[index] * anglemask[j][i] * himask[j][i]);
                                resdft[1][index] = resdft[1][index] - (bandr[index] * anglemask[j][i] * himask[j][i]);
                            }
                        }
                    } // else if (((nbands-1) % 4) == 3)
                } // if (haveB)

                ind++;
            } // for (b = 1; b <= nbands; b++)
        } // if (haveOne)

        return resdft;
    }

    /**
     * This is a port of reconWpyr.m by Eero Simoncelli, 6/96 Reconstruct image from its separable orthonormal
     * QMF/wavelet pyramid representation, as created by buildWpyr.
     *
     * @param   pyr    A Vector containing the N pyramid subbands, ordered from fine to coarse.
     * @param   ind    A Vector with N 2 element int[] containing the sizes of each subband
     * @param   filt   A standard filter
     * @param   edges  Specifies edge handling
     * @param   levs   Levels to include or all for default. 1 corresponds to the finest scale. The lowpass band
     *                 corresponds to wpyrHt(indices) + 1.
     * @param   bands  Bands to include or all for default. 1 = horizontal, 2 = vertical, 3 = diagonal.
     * @param   resX   First dimension of res
     * @param   resY   Second dimension of res
     *
     * @return  DOCUMENT ME!
     */
    private double[] reconWpyr(Vector pyr, Vector ind, int filt, int edges, int[] levs, int[] bands, int[] resX,
                               int[] resY) {
        double[] res = null;
        boolean allLevs = false;
        boolean allBands = false;
        int maxLev;
        int i;
        double[] kernel;
        double[] hkernel;
        int stag;
        int[] res_sz;
        int loind;
        int[] intMat;
        int[] intMat2;
        int[] hres_sz = null;
        int[] lres_sz = null;
        boolean moreOne;
        double[] nres;
        int[] nresx = new int[1];
        int[] nresy = new int[1];
        double[][] arr;
        int[][] parr;
        int[] levsm1;
        double[] ires = null;
        boolean anyOne;
        int[] x = new int[1];
        int[] y = new int[1];
        boolean anyB1;
        boolean anyB2;
        boolean anyB3;
        int[] res_p;

        if (levs == null) {
            allLevs = true;
        }

        if (bands == null) {
            allBands = true;
        }

        maxLev = 1 + wpyrHt(ind);

        if (allLevs) {
            levs = new int[maxLev];

            for (i = 0; i < maxLev; i++) {
                levs[i] = i + 1;
            }
        } else {

            for (i = 0; i < levs.length; i++) {

                if (levs[i] > maxLev) {
                    MipavUtil.displayError("Level numbers must be in the range 1 to " + maxLev);
                    error = 1;

                    return null;
                }
            }
        } // else

        if (allBands) {
            bands = new int[3];
            bands[0] = 1;
            bands[1] = 2;
            bands[2] = 3;
        } else {

            for (i = 0; i < bands.length; i++) {

                if ((bands[i] < 1) || (bands[i] > 3)) {
                    MipavUtil.displayError("Band numbers must be in the range 1 to 3");
                    error = 1;

                    return null;
                }
            }
        } // else

        kernel = namedFilter(filt, 0);
        hkernel = modulateFlip(kernel);

        // For odd-length filters, stagger the sampling lattices:
        if (((kernel.length) % 2) == 0) {
            stag = 2;
        } else {
            stag = 1;
        }

        // Compute size of result image: assumes critical sampling (boundaries correct)
        // Use res_p since if res_sz was set directly the value in the ind vector would
        // change when res_sz was changed.
        res_p = (int[]) ind.get(0);
        res_sz = new int[2];
        res_sz[0] = res_p[0];
        res_sz[1] = res_p[1];

        if (res_sz[0] == 1) {
            loind = 2;
            res_sz[1] = 0;

            for (i = 0; i < ind.size(); i++) {
                intMat = (int[]) ind.get(i);
                res_sz[1] += intMat[1];
            }
        } // if (res_sz[0] == 1)
        else if (res_sz[1] == 1) {
            loind = 2;
            res_sz[0] = 0;

            for (i = 0; i < ind.size(); i++) {
                intMat = (int[]) ind.get(i);
                res_sz[0] += intMat[0];
            }
        } // else if (res_sz[1] == 1)
        else {
            loind = 4;

            // Horizontal + vertical bands
            intMat = (int[]) ind.get(0);
            intMat2 = (int[]) ind.get(1);
            res_sz[0] = intMat[0] + intMat2[0];
            res_sz[1] = intMat[1] + intMat2[1];
            hres_sz = new int[2];
            hres_sz[0] = intMat[0];
            hres_sz[1] = res_sz[1];
            lres_sz = new int[2];
            lres_sz[0] = intMat2[0];
            lres_sz[1] = res_sz[1];
        } // else

        moreOne = false;

        for (i = 0; i < levs.length; i++) {

            if (levs[i] > 1) {
                moreOne = true;
            }
        }

        // First, recursively collapse coarser scales:
        if (moreOne) {

            if (ind.size() > loind) {
                arr = new double[loind - 1][];
                parr = new int[(loind - 1)][2];

                for (i = 0; i < (loind - 1); i++) {
                    arr[i] = (double[]) pyr.remove(0);
                }

                for (i = 0; i < (loind - 1); i++) {
                    parr[i] = (int[]) ind.remove(0);
                }

                levsm1 = new int[levs.length];

                for (i = 0; i < levs.length; i++) {
                    levsm1[i] = levs[i] - 1;
                }

                nres = reconWpyr(pyr, ind, filt, edges, levsm1, bands, nresx, nresy);

                for (i = loind - 2; i >= 0; i--) {
                    pyr.insertElementAt(arr[i], 0);
                    ind.insertElementAt(parr[i], 0);
                }
            } // if (ind.size() > loind)
            else {

                // lowpass subband
                nres = pyrBand(pyr, ind, loind - 1, nresx, nresy);
            }

            if (res_sz[0] == 1) {
                res = new double[res_sz[0] * res_sz[1]];
                upConv(nres, nresx[0], nresy[0], kernel, 1, kernel.length, edges, 1, 2, 1, stag, res_sz[0], res_sz[1],
                       res);
            } else if (res_sz[1] == 1) {
                res = new double[res_sz[0] * res_sz[1]];
                upConv(nres, nresx[0], nresy[0], kernel, kernel.length, 1, edges, 2, 1, stag, 1, res_sz[0], res_sz[1],
                       res);
            } else {
                ires = new double[lres_sz[0] * lres_sz[1]];
                upConv(nres, nresx[0], nresy[0], kernel, 1, kernel.length, edges, 1, 2, 1, stag, lres_sz[0], lres_sz[1],
                       ires);
                res = new double[res_sz[0] * res_sz[1]];
                upConv(ires, lres_sz[0], lres_sz[1], kernel, kernel.length, 1, edges, 2, 1, stag, 1, res_sz[0],
                       res_sz[1], res);
            }
        } // if (moreOne)
        else {
            res = new double[res_sz[0] * res_sz[1]];
        }

        // Add in reconstructed bands from this level
        anyOne = false;

        for (i = 0; i < levs.length; i++) {

            if (levs[i] == 1) {
                anyOne = true;
            }
        }

        if (anyOne) {
            anyB1 = false;
            anyB2 = false;
            anyB3 = false;

            for (i = 0; i < bands.length; i++) {

                if (bands[i] == 1) {
                    anyB1 = true;
                } else if (bands[i] == 2) {
                    anyB2 = true;
                } else if (bands[i] == 3) {
                    anyB3 = true;
                }
            }

            if (res_sz[0] == 1) {
                upConv(pyrBand(pyr, ind, 0, x, y), x[0], y[0], hkernel, 1, hkernel.length, edges, 1, 2, 1, 2, res_sz[0],
                       res_sz[1], res);
            } else if (res_sz[1] == 1) {
                upConv(pyrBand(pyr, ind, 0, x, y), x[0], y[0], hkernel, hkernel.length, 1, edges, 2, 1, 2, 1, res_sz[0],
                       res_sz[1], res);
            } else {

                if (anyB1) { // horizontal
                    ires = new double[hres_sz[0] * hres_sz[1]];
                    upConv(pyrBand(pyr, ind, 0, x, y), x[0], y[0], kernel, 1, kernel.length, edges, 1, 2, 1, stag,
                           hres_sz[0], hres_sz[1], ires);

                    // Destructively modify res
                    upConv(ires, hres_sz[0], hres_sz[1], hkernel, hkernel.length, 1, edges, 2, 1, 2, 1, res_sz[0],
                           res_sz[1], res);
                } //

                if (anyB2) { // vertical
                    ires = new double[lres_sz[0] * lres_sz[1]];
                    upConv(pyrBand(pyr, ind, 1, x, y), x[0], y[0], hkernel, 1, hkernel.length, edges, 1, 2, 1, 2,
                           lres_sz[0], lres_sz[1], ires);

                    // Destructively modify res
                    upConv(ires, lres_sz[0], lres_sz[1], kernel, kernel.length, 1, edges, 2, 1, stag, 1, res_sz[0],
                           res_sz[1], res);
                } // if (anyB2)

                if (anyB3) { // diagonal
                    ires = new double[hres_sz[0] * hres_sz[1]];
                    upConv(pyrBand(pyr, ind, 2, x, y), x[0], y[0], hkernel, 1, hkernel.length, edges, 1, 2, 1, 2,
                           hres_sz[0], hres_sz[1], ires);

                    // Destructively modify res
                    upConv(ires, hres_sz[0], hres_sz[1], hkernel, hkernel.length, 1, edges, 2, 1, 2, 1, res_sz[0],
                           res_sz[1], res);
                } // if (anyB3)
            } // else
        } // if (anyOne)

        return res;
    }

    /**
     * This is a port of reconWpyr.m by JPM, Univ. de Granada, 3/2003 Reconstruct image from its separable undecimated
     * orthonormal QMF/wavelet pyramid representation, as created by buildWUpyr
     *
     * @param   pyr        A vector cotaining the N pyramid subbands, ordered from fine to coarse.
     * @param   pind       Contains the N 2 element int[] arrays with the sizes of each subband
     * @param   daubOrder  Specifies the order of the daubechies wavelet filter used
     *
     * @return  DOCUMENT ME!
     */
    private double[] reconWUpyr(Vector pyr, Vector pind, int daubOrder) {
        double[] res = null;
        double[] h;
        double[] yh = null;
        int nband;
        int nsc;
        int nor;
        double[] band;
        double[] oldband = null;
        int newx;
        int newy;
        int sh;
        double[] lpr = null;
        int[] bandx = new int[1];
        int[] bandy = new int[1];
        int i, j;
        int twoPow;
        double[] bandi = null;
        int[] offset = new int[2];
        double[] temp;
        int yhx = 0;
        int yhy = 0;
        int lprx;
        int lpry;
        double[] h_1 = new double[daubOrder];

        nOrientations = 3;
        nScales = ((pind.size() - 2) / nOrientations) - 1;
        h = daubcqf(daubOrder, MINIMUM_PHASE, h_1);

        nband = 1;

        // Empty "high pass residual band" for compatibility with full steerpyr 2
        // The number of scales corresponds to the number of pyramid levels
        // (also for compatibility)
        for (nsc = 1; nsc <= (nScales + 1); nsc++) {

            for (nor = 1; nor <= nOrientations; nor++) {
                nband = nband + 1;
                band = pyrBand(pyr, pind, nband - 1, bandx, bandy);

                // Approximate phase compensation
                twoPow = 1;

                for (i = 0; i < nsc; i++) {
                    twoPow *= 2;
                }

                sh = ((daubOrder / 2) - 1) * twoPow;

                if (nsc > 2) {
                    twoPow = 1;

                    for (i = 0; i < (nsc - 2); i++) {
                        twoPow *= 2;
                    }

                    oldband = new double[band.length];

                    for (i = 0; i < band.length; i++) {
                        oldband[i] = band[i];
                    }

                    newx = (int) Math.round(twoPow * bandx[0]);
                    newy = (int) Math.round(twoPow * bandy[0]);
                    band = new double[newx * newy];
                    bandi = new double[newx * newy];
                    expand(oldband, twoPow, bandx[0], bandy[0], band, bandi);
                    bandx[0] = newx;
                    bandy[0] = newy;
                } // if (nsc > 2)

                if (nor == 1) { // horizontal
                    twoPow = 1;

                    for (i = 0; i < (nsc - 1); i++) {
                        twoPow *= 2;
                    }

                    offset[0] = -twoPow;
                    offset[1] = -sh;
                    shiftReal(band, bandx[0], bandy[0], offset, band);
                } // if (nor = 1)
                else if (nor == 2) { // vertical
                    twoPow = 1;

                    for (i = 0; i < (nsc - 1); i++) {
                        twoPow *= 2;
                    }

                    offset[0] = -sh;
                    offset[1] = -twoPow;
                    shiftReal(band, bandx[0], bandy[0], offset, band);
                } // else if (nor == 2)
                else { // diagonal
                    offset[0] = -sh;
                    offset[1] = -sh;
                    shiftReal(band, bandx[0], bandy[0], offset, band);
                } // else

                if (yh == null) {
                    yh = new double[band.length];

                    for (i = 0; i < band.length; i++) {
                        yh[i] = band[i];
                    }

                    yhy = bandy[0];
                    yhx = bandx[0];
                } else {

                    // For horizontal concatenation the number of rows of yh and band is required to be
                    // the same.  Since band is always expanded up to the same image size, this is
                    // not a problem.
                    temp = new double[yhy * (yhx + bandx[0])];

                    for (j = 0; j < yhy; j++) {

                        for (i = 0; i < yhx; i++) {
                            temp[i + (j * (yhx + bandx[0]))] = yh[i + (j * yhx)];
                        }
                    }

                    for (j = 0; j < yhy; j++) {

                        for (i = 0; i < bandx[0]; i++) {
                            temp[i + yhx + (j * (yhx + bandx[0]))] = band[i + (j * bandx[0])];
                        }
                    }

                    yh = new double[yhy * (yhx + bandx[0])];

                    for (i = 0; i < yh.length; i++) {
                        yh[i] = temp[i];
                    }

                    yhx = yhx + bandx[0];
                } // else
            } // for (nor = 1; nor <= nOrientations; nor++)
        } // for (nsc = 1; nsc <= nScales+1; nsc++)

        nband = nband + 1;
        band = pyrBand(pyr, pind, nband - 1, bandx, bandy);
        twoPow = 1;

        for (i = 0; i < nScales; i++) {
            twoPow *= 2;
        }

        lprx = (int) Math.round(twoPow * bandx[0]);
        lpry = (int) Math.round(twoPow * bandy[0]);
        lpr = new double[lprx * lpry];
        bandi = new double[lprx * lpry];
        expand(band, twoPow, bandx[0], bandy[0], lpr, bandi);
        res = mirdwt(lpr, lprx, lpry, yh, yhx, yhy, h, 1, h.length, nScales + 1);

        return res;
    }

    


    /**
     * Port of shift.m Circular shift 2D matrix samples by offset (a [X, Y] 2 vector), such that res(pos) =
     * mtx(pos-offset).
     *
     * @param  mtxr    DOCUMENT ME!
     * @param  mtxi    DOCUMENT ME!
     * @param  dimx    DOCUMENT ME!
     * @param  dimy    DOCUMENT ME!
     * @param  offset  DOCUMENT ME!
     * @param  resr    DOCUMENT ME!
     * @param  resi    DOCUMENT ME!
     */
    private void shift(double[] mtxr, double[] mtxi, int dimx, int dimy, int[] offset, double[] resr, double[] resi) {
        int n;
        int offsetx;
        int offsety;
        int i, j, i2, j2;
        double[] mtr = new double[mtxr.length];
        double[] mti = new double[mtxi.length];

        for (i = 0; i < mtxr.length; i++) {
            mtr[i] = mtxr[i];
            mti[i] = mtxi[i];
        }

        n = (int) Math.floor(-offset[0] / (double) dimx);
        offsetx = -offset[0] - (n * dimx);

        if (offsetx < 0) {
            offsetx = offsetx + dimx;
        }

        n = (int) Math.floor(-offset[1] / (double) dimy);
        offsety = -offset[1] - (n * dimy);

        if (offsety < 0) {
            offsety = offsety + dimy;
        }

        for (j = 0, j2 = offsety; j2 <= (dimy - 1); j++, j2++) {

            for (i = 0, i2 = offsetx; i2 <= (dimx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mtr[i2 + (dimx * j2)];
                resi[i + (dimx * j)] = mti[i2 + (dimx * j2)];
            }

            for (i = dimx - offsetx, i2 = 0; i2 <= (offsetx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mtr[i2 + (dimx * j2)];
                resi[i + (dimx * j)] = mti[i2 + (dimx * j2)];
            }
        }

        for (j = dimy - offsety, j2 = 0; j2 <= (offsety - 1); j++, j2++) {

            for (i = 0, i2 = offsetx; i2 <= (dimx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mtr[i2 + (dimx * j2)];
                resi[i + (dimx * j)] = mti[i2 + (dimx * j2)];
            }

            for (i = dimx - offsetx, i2 = 0; i2 <= (offsetx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mtr[i2 + (dimx * j2)];
                resi[i + (dimx * j)] = mti[i2 + (dimx * j2)];
            }
        }

        return;
    }

    /**
     * Circular shift 2D matrix samples by offset (a [X, Y] 2 vector), such that res(pos) = mtx(pos-offset).
     *
     * @param  mtxr    DOCUMENT ME!
     * @param  dimx    DOCUMENT ME!
     * @param  dimy    DOCUMENT ME!
     * @param  offset  DOCUMENT ME!
     * @param  resr    DOCUMENT ME!
     */
    private void shiftReal(double[] mtxr, int dimx, int dimy, int[] offset, double[] resr) {
        int n;
        int offsetx;
        int offsety;
        int i, j, i2, j2;
        double[] mt = new double[mtxr.length];

        for (i = 0; i < mtxr.length; i++) {
            mt[i] = mtxr[i];
        }

        n = (int) Math.floor(-offset[0] / (double) dimx);
        offsetx = -offset[0] - (n * dimx);

        if (offsetx < 0) {
            offsetx = offsetx + dimx;
        }

        n = (int) Math.floor(-offset[1] / (double) dimy);
        offsety = -offset[1] - (n * dimy);

        if (offsety < 0) {
            offsety = offsety + dimy;
        }

        for (j = 0, j2 = offsety; j2 <= (dimy - 1); j++, j2++) {

            for (i = 0, i2 = offsetx; i2 <= (dimx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mt[i2 + (dimx * j2)];
            }

            for (i = dimx - offsetx, i2 = 0; i2 <= (offsetx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mt[i2 + (dimx * j2)];
            }
        }

        for (j = dimy - offsety, j2 = 0; j2 <= (offsety - 1); j++, j2++) {

            for (i = 0, i2 = offsetx; i2 <= (dimx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mt[i2 + (dimx * j2)];
            }

            for (i = dimx - offsetx, i2 = 0; i2 <= (offsetx - 1); i++, i2++) {
                resr[i + (dimx * j)] = mt[i2 + (dimx * j2)];
            }
        }

        return;
    }

    /**
     * This is a port of shrink.m by JPM, 5/1/95. It shrinks (spatially) an image by a factor f in each dimension. It
     * does it by cropping the Fourier transform of the image.
     *
     * @param  t    DOCUMENT ME!
     * @param  tx   First dimension of t
     * @param  ty   Second dimension of t
     * @param  f    Shrink by this factor in each dimension
     * @param  tsr  Real part of shrunk result
     * @param  tsi  Imaginary part of shrunk result
     */
    private void shrink(double[] t, int tx, int ty, int f, double[] tsr, double[] tsi) {
        FFTUtility fftUtil;
        int i, j, i2, j2;
        int cx;
        int cy;
        boolean evenmx;
        boolean evenmy;
        int evenx;
        int eveny;
        int y1, y2, x1, x2;
        int[] intArr = new int[2];
        double[] tr = new double[t.length];
        double[] ti = new double[t.length];
        int tsx;
        int tsy;

        for (i = 0; i < t.length; i++) {
            tr[i] = t[i];
        }

        // forward FFT
        fftUtil = new FFTUtility(tr, ti, ty, tx, 1, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(tr, ti, 1, ty, tx, -1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        center(tr, ti, tx, ty);

        for (i = 0; i < tr.length; i++) {
            tr[i] = tr[i] / (f * f);
            ti[i] = ti[i] / (f * f);
        }

        tsx = tx / f;
        tsy = ty / f;

        cx = (int) Math.ceil(tx / 2.0);
        cy = (int) Math.ceil(ty / 2.0);
        evenmx = ((2 * (tx / 2)) == tx);

        if (evenmx) {
            evenx = 1;
        } else {
            evenx = 0;
        }

        evenmy = ((2 * (ty / 2)) == ty);

        if (evenmy) {
            eveny = 1;
        } else {
            eveny = 0;
        }

        y1 = cy + (2 * eveny) - (int) Math.floor(ty / (2.0 * f));
        y2 = cy + (int) Math.floor(ty / (2.0 * f));
        x1 = cx + (2 * evenx) - (int) Math.floor(tx / (2.0 * f));
        x2 = cx + (int) Math.floor(tx / (2.0 * f));

        for (j = eveny, j2 = y1 - 1; j < tsy; j++, j2++) {

            for (i = evenx, i2 = x1 - 1; i < tsx; i++, i2++) {
                tsr[i + (tsx * j)] = tr[i2 + (tx * j2)];
                tsi[i + (tsx * j)] = ti[i2 + (tx * j2)];
            }
        } // for (j = eveny, j2 = y1-1; j < tsy[0]; j++, j2++)

        if (evenmy) {

            for (j = eveny, j2 = y1 - 1; j < tsy; j++, j2++) {
                tsr[tsx * j] = (tr[x1 - 2 + (tx * j2)] + tr[x2 + (tx * j2)]) / 2.0;
                tsi[tsx * j] = (ti[x1 - 2 + (tx * j2)] + ti[x2 + (tx * j2)]) / 2.0;
            }
        } // if (evenmy)

        if (evenmx) {

            for (i = evenx, i2 = x1 - 1; i < tsx; i++, i2++) {
                tsr[i] = (tr[i2 + (tx * (y1 - 2))] + tr[i2 + (tx * y2)]) / 2.0;
                tsi[i] = (ti[i2 + (tx * (y1 - 2))] + ti[i2 + (tx * y2)]) / 2.0;
            }
        } // if (evenmx)

        if (evenmx && evenmy) {
            tsr[0] = (tr[x1 - 2 + (tx * (y1 - 2))] + tr[x2 + (tx * (y1 - 2))] + tr[x1 - 2 + (tx * y2)] +
                      tr[x2 + (tx * y2)]) / 4.0;
            tsi[0] = (ti[x1 - 2 + (tx * (y1 - 2))] + ti[x2 + (tx * (y1 - 2))] + ti[x1 - 2 + (tx * y2)] +
                      ti[x2 + (tx * y2)]) / 4.0;
        } // if (evenmx && evenmy)

        center(tsr, tsi, tsx, tsy);
        intArr[0] = 1 - evenx;
        intArr[1] = 1 - eveny;
        shift(tsr, tsi, tsx, tsy, intArr, tsr, tsi);

        // Inverse FFT
        fftUtil = new FFTUtility(tsr, tsi, tsy, tsx, 1, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(tsr, tsi, 1, tsy, tsx, +1, FFTUtility.FFT);
        fftUtil.run();
        fftUtil.finalize();

        return;
    }

    /**
     * Complex numbers are sorted by absolute values and when absolute values are equal are further sorted by angle on
     * the interval from -PI to PI.
     *
     * @param  realBuffer  DOCUMENT ME!
     * @param  imagBuffer  DOCUMENT ME!
     */
    private void sort(double[] realBuffer, double[] imagBuffer) {
        int i, j, inc;
        double vr;
        double vi;
        inc = 1;

        double sq;
        double vsq;

        int end = realBuffer.length;

        do {
            inc *= 3;
            inc++;
        } while (inc <= end);

        do {
            inc /= 3;

            for (i = inc + 1; i <= end; i++) {
                vr = realBuffer[i - 1];
                vi = imagBuffer[i - 1];
                j = i;

                while (((sq = ((realBuffer[j - inc - 1] * realBuffer[j - inc - 1]) +
                                   (imagBuffer[j - inc - 1] * imagBuffer[j - inc - 1]))) >
                            (vsq = (vr * vr) + (vi * vi))) ||
                           ((sq == vsq) &&
                                (Math.atan2(imagBuffer[j - inc - 1], realBuffer[j - inc - 1]) > Math.atan2(vi, vr)))) {
                    realBuffer[j - 1] = realBuffer[j - inc - 1];
                    imagBuffer[j - 1] = imagBuffer[j - inc - 1];
                    j -= inc;

                    if (j <= inc) {
                        break;
                    }
                }

                realBuffer[j - 1] = vr;
                imagBuffer[j - 1] = vi;
            }
        } while (inc > 1);

    }

    /**
     * This is a port of spyrHt.m by Eero Simoncelli, 6/96. Compute height of steerable pyramid with given index matrix.
     *
     * @param   pind  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int spyrHt(Vector pind) {
        int ht = 0;
        int nbands;

        nbands = spyrNumBands(pind);

        // Don't count lowpass or highpass residual bands
        if (pind.size() > 2) {
            ht = (pind.size() - 2) / nbands;
        } else {
            ht = 0;
        }

        return ht;
    }

    /**
     * This is a port of spyrNumbands.m by Eero Simoncelli, 2/97. Compute number of orientation bands in a steerable
     * pyrmaid with given index matrix. If the pryamid contains only highpass and lowpass bands (i.e., zero levels),
     * returns 0.
     *
     * @param   pind  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int spyrNumBands(Vector pind) {
        int nbands = 0;
        int b;
        int x;
        int y;
        int[] intMat;

        if (pind.size() == 2) {
            nbands = 0;
        } else {

            // Count number of orientation bands
            b = 3;
            intMat = (int[]) pind.get(1);
            x = intMat[0];
            y = intMat[1];

            while ((b <= pind.size()) && ((intMat = (int[]) pind.get(b - 1)) != null) && (intMat[0] == x) &&
                       (intMat[1] == y)) {
                b = b + 1;
            }

            nbands = b - 2;
        }

        return nbands;
    }

    /**
     * This is ported from upConv.c by Eero Simoncelli, 7/96. Upsample matrix image, followed by convolution with matrix
     * filt. These arguments should be 1D or 2D matrices, and image must be larger (in both dimensions) than filt. The
     * origin of filt is assumed to be floor(size(filt)/2)+1.
     *
     * @param  image   DOCUMENT ME!
     * @param  xdim    DOCUMENT ME!
     * @param  ydim    DOCUMENT ME!
     * @param  filt    DOCUMENT ME!
     * @param  xfdim   DOCUMENT ME!
     * @param  yfdim   DOCUMENT ME!
     * @param  edges   Determines boundary handling CIRCULAR - Circular convolution REFLECT1 - Reflect about the edge
     *                 pixels REFLECT2 - Reflect, doubling the edge pixels REPEAT - Repeat the edge pixels ZERO - Assume
     *                 values of zero outside image boundary EXTEND - Reflect and invert DONT_COMPUTE - Zero output when
     *                 filter overhangs output boundaries Upsampling factors are determined by xstep and ystep
     * @param  xstep   DOCUMENT ME!
     * @param  ystep   The window over which the convolution occurs is specified by xstart, ystart, xstop, and ystop.
     * @param  xstart  DOCUMENT ME!
     * @param  ystart  DOCUMENT ME!
     * @param  xstop   DOCUMENT ME!
     * @param  ystop   This operation corresponds to multiplication of a signal vector by a matrix whose columns
     *                 contains copies of the time-reversed (or space- reversed) filt shifted by multiple copies of
     *                 step. See corrDn for the operation corresponding to the transpose of this matrix.
     * @param  result  DOCUMENT ME!
     */
    private void upConv(double[] image, int xdim, int ydim, double[] filt, int xfdim, int yfdim, int edges, int xstep,
                        int ystep, int xstart, int ystart, int xstop, int ystop, double[] result) {
        double[] origFilt;
        int origx;
        int origy;
        int x;
        int y;
        double[] temp;
        int xrdim = xstop;
        int yrdim = ystop;

        xstart--;
        ystart--;

        // upConv has a bug for even length kernels when using the REFLECT1,
        // EXTEND, or REPEAT edge handlers
        if (((edges == REFLECT1) || (edges == EXTEND) || (edges == REPEAT)) &&
                (((xfdim % 2) == 0) || ((yfdim % 2) == 0))) {
            origFilt = filt;
            origx = xfdim;
            origy = yfdim;
            xfdim = (2 * (origx / 2)) + 1;
            yfdim = (2 * (origy / 2)) + 1;
            filt = new double[xfdim * yfdim];

            for (y = 0; y < origy; y++) {

                for (x = 0; x < origx; x++) {
                    filt[(y * xfdim) + x] = origFilt[(y * origx) + x];
                }
            }
        }

        if (edges == CIRCULAR) {
            internal_wrap_expand(image, filt, xfdim, yfdim, xstart, xstep, xstop, ystart, ystep, ystop, result, xrdim,
                                 yrdim);
        } else {
            temp = new double[xfdim * yfdim];
            internal_expand(image, filt, temp, xfdim, yfdim, xstart, xstep, xstop, ystart, ystep, ystop, result, xrdim,
                            yrdim, edges);
        }

        return;
    }

    /**
     * This is a port of wpyrHt.m by Eero Simoncelli, 6/96. Compute height of separable QMF/wavelet pyramid with given
     * index matrix
     *
     * @param   pind  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int wpyrHt(Vector pind) {
        int[] intMat;
        int nbands;
        int ht;

        intMat = (int[]) pind.get(0);

        if ((intMat[0] == 1) || (intMat[1] == 1)) {
            nbands = 1;
        } else {
            nbands = 3;
        }

        ht = (pind.size() - 1) / nbands;

        return ht;
    }


}
