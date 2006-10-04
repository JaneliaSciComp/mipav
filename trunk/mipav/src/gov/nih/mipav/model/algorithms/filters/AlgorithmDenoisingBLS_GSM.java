package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import java.util.*;

import java.io.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmDenoisingBLS_GSM extends AlgorithmBase {
    // The image is expanded so that all dimensions are powers of 2 and there is zero padding going past each original
    // boundary by the coefficient number - 2.   The image is transformed into wavelet coefficients.
    /**
     * This is a port of MATLAB BLS-GSM Image Denoising software written by Javier Portilla of Universidad de
     * Granada, Spain.
     * References: 1.) "Image Denoising Using Scale Mixtures of Gaussians in the Wavelet Domain", Javier Portilla,
     * Vasily Strela, Martin J. Wainwright, and Eero P. Simoncelli, IEEE Transactions on Image Processing,
     * Vol. 12. No. 11, November, 2003, pp. 1338-1351.
     * 2.) "The Steerable Pyramid: A Flexible Architecture for Multi-Scale Derivative Computation", Eero P.
     * Simoncelli and William T. Freeman, 2nd IEEE International Conference on Image Processing, Washington,
     * D.C., vol. III, pp. 444-447, October, 1995.
     */
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    
    private static final int INVERSE = -1;
    
    private static final int FILTER = 0;

    /** DOCUMENT ME! */
    private static final int FORWARD = 1;
    
    // Possible repres1 values
    private static final int ORTHOGONAL_WAVELET = 1;
    
    private static final int UNDECIMATED_ORTHOGONAL_WAVELET = 2;
    
    private static final int STEERABLE_PYRAMID = 3;
    
    private static final int FULL_STEERABLE_PYRAMID = 4;
    
    // Possible repres2 values
    private static final int NONE = 0;
    
    private static final int HAAR = 1; // Haar wavelet
    
    // Symmetric quadrature mirror filters
    private static final int QMF5 = 2;
    
    private static final int QMF8 = 3;
    
    private static final int QMF9 = 4;
    
    private static final int QMF12 = 5;
    
    private static final int QMF13 = 6;
    
    private static final int QMF16 = 7;
    
    // Daubechies wavelet
    private static final int DAUB2 = 8;
    
    private static final int DAUB3 = 9;
    
    private static final int DAUB4 = 10;
    
    
    /**
     * underlying wavelet filter For 4 coefficients the routine daub4 is considerably faster than pwt. DAUB4 implements
     * a Daubechies 4-coefficient wavelet filter. PWT implements Daubechies filters with 4, 12, and 20 coefficients.
     * DAUB4 and PWT use different default centerings. In spite of the faster speed of DAUB4, the dialog is designed
     * only to call PWT so that the user only sees changes due to the number of coefficients and is not confused by
     * changes in centering.
     * Reference on NONNEGATIVE_GARROTE and SCAD thresholding:
     * "Wavelet Estimators in Nonparametric Regression: A Comparative Simulation Study" by Anestis Antoniadis,
     * Jeremie Bigot, and Theofanis Sapatinas, Journal of Statistical Software, Vol. 6, Issue 6, pp. 1-83, 2001.
     */

    /** DOCUMENT ME! */
    public static final int PWT = 2;

    /** coefficents for DAUB4 wavelet filter. */
    public static final double C0 = 0.4829629131445341;

    /** DOCUMENT ME! */
    public static final double C1 = 0.8365163037378079;

    /** DOCUMENT ME! */
    public static final double C2 = 0.2241438680420134;

    /** DOCUMENT ME! */
    public static final double C3 = -0.1294095225512604;

    /** coefficients for the PWT filter. */
    public static final double[] c4 = {
        0.0, 0.4829629131445341, 0.8365163037378079, 0.2241438680420134, -0.1294095225512604
    };

    /** DOCUMENT ME! */
    public static final double[] c12 = {
        0.0, 0.111540743350, 0.494623890398, 0.751133908021, 0.315250351709, -0.226264693965, -0.129766867567,
        0.097501605587, 0.027522865530, -0.031582039318, 0.000553842201, 0.004777257511, -0.001077301085
    };

    /** DOCUMENT ME! */
    public static final double[] c20 = {
        0.0, 0.026670057901, 0.188176800078, 0.527201188932, 0.688459039454, 0.281172343661, -0.249846424327,
        -0.195946274377, 0.127369340336, 0.093057364604, -0.071394147166, -0.029457536822, 0.033212674059,
        0.003606553567, -0.010733175483, 0.001395351747, 0.001992405295, -0.000685856695, -0.000116466855,
        0.000093588670, -0.000013264203
    };

    /** thresholding estimator. */
    public static final int HARD = 1;

    /** DOCUMENT ME! */
    public static final int SOFT = 2;
    
    public static final int NONNEGATIVE_GARROTE = 3;
    
    public static final int SCAD = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] aArray;

    /** DOCUMENT ME! */
    private float aCutoff;

    /** DOCUMENT ME! */
    private float[] aExp;

    /** DOCUMENT ME! */
    private float[] aLog;

    /** DOCUMENT ME! */
    private float aMax;

    /** DOCUMENT ME! */
    private int arrayLength;

    /** DOCUMENT ME! */
    private int belowThreshold;

    /** DOCUMENT ME! */
    private double[] cc;

    /** DOCUMENT ME! */
    private int cm2; // cNum - 2

    /** DOCUMENT ME! */
    private int cNum;

    /** DOCUMENT ME! */
    private double[] cr;

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private boolean doWaveletImage; // display wavelet transform in log magnitude if true

    /** DOCUMENT ME! */
    private int filterType;

    /** DOCUMENT ME! */
    private boolean foundSize;

    /** DOCUMENT ME! */
    private int ioff;

    /** DOCUMENT ME! */
    private int joff;

    /** DOCUMENT ME! */
    private int minSize;

    /** DOCUMENT ME! */
    private int nDims;

    /** DOCUMENT ME! */
    private int[] newExtents;

    /** DOCUMENT ME! */
    private int newXDim, newYDim, newZDim;

    /** DOCUMENT ME! */
    private int offsetX;

    /** DOCUMENT ME! */
    private int offsetY;

    /** DOCUMENT ME! */
    private int offsetZ;

    /** DOCUMENT ME! */
    private float threshold; // fraction of maximum magnitude below which coefficients
                             // are zeroed

    /** DOCUMENT ME! */
    private int thresholdType; // HARD or SOFT

    /** DOCUMENT ME! */
    private ModelImage waveletImage = null;

    /** DOCUMENT ME! */
    private int xDim, yDim, zDim;

    /** DOCUMENT ME! */
    private int xy, newXY;
    
    private float sig;  // noise standard deviation
    
    private int blockSizeX = 3; // Local neighborhood x
    
    private int blockSizeY = 3; // Local neighborhood y
    
    private boolean usePSD = true; // If true, use power spectral density = fft(autocorrelation)
                                   // If false, use white noise
    
    private boolean useParent = true;
    
    private boolean useBoundary = true;
    
    private int nScales = 4; // Number of scales
    
    private int nOrientations = 8; // Number of orientations.  For separable wavelets this must be 3.
    
    private boolean includeCovar = true; // Include covariance in the GSM model
    
    private boolean optimize = true; // BLS / MAP-Wiener (2-step)
    
    /**
     * repres1 = ORTHOGONAL_WAVELET
     *           Here repres2 can be NONE, QMF5, QMF8, QMF9, QMF12, QMF13, QMF16, DAUB2, DAUB3, DAUB4
     *           
     * repres1 = UNDECIMATED_ORTHOGONAL_WAVELET
     *           Here repres2 can be NONE, DAUB2, DAUB3, DAUB4
     *           
     * repres1 = STEERABLE_PYRAMID
     *           Here repres2 = NONE
     *           
     * repres1 = FULLY_STEERABLE_PYRAMID
     *           Here repres2 = NONE
     */
    private int repres1 = FULL_STEERABLE_PYRAMID;
    
    private int repres2 = NONE;
    
    private float seed = 0.0f; // Seed used for generating the Gaussian noise
    
    private int nx;
    
    private int ny;
    
    private int dimLengths[];
    
    private int ndim;
    
    private int npx; // Integer multiple of 2**(nScales+1) for applying pyramidal representation
    
    private int npy; // Integer multiple of 2**(nScales+1) for applying pyramidal representation
    
    private int ndx; // Padded power of 2 for fft
    
    private int ndy; // Padded power of 2 for fft
    
    private int newDimLengths[];
    
    private int newArrayLength;
    
    private float finalData[];
    
    private int transformDir;
    
    private boolean doCenter;
    
    private boolean doStrip;
    
    private int error = 0; // 0 for no error, 1 for error

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     *
     * @param  srcImg          source image model
     * @param  filterType      wavelet filter
     * @param  cNum            number of coefficients in the pwt filter
     * @param  thresholdType   HARD or SOFT thresholding
     * @param  threshold       fraction of maximum magnitude below which coefficients are zeroed
     * @param  doWaveletImage  display wavelet transform image in log magnitude if true
     * @param  sig             Noise standard deviation
     * @param  usePSD          If true, use power spectral density.  If false, use white noise
     * @param  blockSizeX      Local neighborhood x
     * @param  blockSizeY      Local neighborhood y
     * @param  useParent
     * @param  useBoundary
     * @param  nScales         Number of scales
     * @param  nOrientations   Number of orientations.  For separable wavelets this must be 3.
     * @param  includeCovar    Include covariance in the GSM model
     * @param  optimize        BLS / MAP-Wiener (2-step)
     * @param  repres1         
     * @param  repres2
     * @param  seed            Seed used for generating the Gaussian noise
     */
    public AlgorithmDenoisingBLS_GSM(ModelImage srcImg, int filterType, int cNum, int thresholdType, float threshold,
                                     boolean doWaveletImage, float sig, boolean usePSD, 
                                     int blockSizeX, int blockSizeY, boolean useParent, boolean useBoundary,
                                     int nScales, int nOrientations, boolean includeCovar,
                                     boolean optimize, int repres1, int repres2, float seed) {
        super(null, srcImg);

        this.filterType = filterType;
        this.cNum = cNum;

        if (filterType == DAUB4) {
            cNum = 4;
        }

        cm2 = cNum - 2;
        this.thresholdType = thresholdType;
        this.threshold = threshold;
        this.doWaveletImage = doWaveletImage;
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
        this.seed = seed;
    }

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     *
     * @param  destImg         image model where result image is to be stored
     * @param  srcImg          source image model
     * @param  filterType      wavelet filter
     * @param  cNum            number of coefficients in the pwt filter
     * @param  thresholdType   HARD or SOFT thresholding
     * @param  threshold       fraction of maximum magnitude below which coefficients are zeroed
     * @param  doWaveletImage  display log maagnitude wavelet transform image if true
     * @param  sig             Noise standard deviation
     * @param  usePSD          If true, use power spectral density.  If false, use white noise.
     * @param  blockSizeX      Local neighborhood x
     * @param  blockSizeY      Local neighborhood y
     * @param  useParent
     * @param  useBoundary
     * @param  nScales         Number of scales
     * @param  nOrientations   Number of orientations.  For separable wavelets this must be 3.
     * @param  includeCovar    Include covariance in the GSM model
     * @param  optimize        BLS / MAP-Wiener (2-step)
     * @param  repres1
     * @param  repres2
     * @param  seed            Seed used for generating the Gaussian noise
     */
    public AlgorithmDenoisingBLS_GSM(ModelImage destImg, ModelImage srcImg, int filterType, int cNum, int thresholdType,
                                     float threshold, boolean doWaveletImage, float sig, boolean usePSD,
                                     int blockSizeX, int blockSizeY, boolean useParent, boolean useBoundary,
                                     int nScales, int nOrientations, boolean includeCovar,
                                     boolean optimize, int repres1, int repres2, float seed) {
        super(destImg, srcImg);

        this.filterType = filterType;
        this.cNum = cNum;

        if (filterType == DAUB4) {
            cNum = 4;
        }

        cm2 = cNum - 2;
        this.thresholdType = thresholdType;
        this.threshold = threshold;
        this.doWaveletImage = doWaveletImage;
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
        this.seed = seed;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepare this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        waveletImage = null;
        cc = null;
        cr = null;
        aArray = null;
        aExp = null;
        aLog = null;
        super.finalize();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the wavelet image
     */
    public ModelImage getWaveletImage() {
        return waveletImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int k;
        int x, y, z;
        int xs;
        int ys;
        float aCutSquared;
        int twoPow;
        int i;
        int bpx;
        int bpy;
        float corArray[];
        float padArray[];
        int bx;
        int by;
        int extents[] = new int[2];
        ModelImage padImage;
        ModelImage corImage;
        AlgorithmAutoCorrelation algoAutoCorrelation;
        double imagArray[];
        double delta[];
        FFTUtility fftUtil;
        double psArray[];
        RandomNumberGen randomGen;
        double ranArray[];
        float im[];
        int imx; // x dimension of im
        int imy; // y dimesnion of im
        float imD[];
        double aux[];
        double mean;
        long time;

        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);
            return;
        }
        
        if (((blockSizeX % 2) == 0) || ((blockSizeY %2) == 0)) {
            displayError("Spatial dimensions of neighborhood must be odd");
            setCompleted(false);
            return;
        }
        
        if (((repres1 == ORTHOGONAL_WAVELET) || (repres1 == UNDECIMATED_ORTHOGONAL_WAVELET)) &&
            (nOrientations != 3)) {
            MipavUtil.displayWarning("For X-Y separable orientations nOrientations must be 3");
            nOrientations = 3;
        }

        fireProgressStateChanged(srcImage.getImageName(), "DenoisingBLS_GSM...");

        constructLog();
        
        nx = srcImage.getExtents()[0];
        ny = srcImage.getExtents()[1];
        arrayLength = nx*ny;
        imx = nx;
        imy = ny;
        
        ndim = srcImage.getNDims();
        
        dimLengths = srcImage.getExtents();
        
        // Ensure that the processed image has dimensions that are integer multiples of 2**(nScales+1),
        // so it will not crash when applying the pyramidal representation.  The idea is padding with 
        // mirror reflected pixels.  Note that an integer multiple of 2**(nScales+1) is not the same
        // as pure power of 2 as required in the fft.
        twoPow = 2;
        // twoPow = 2**(nScales+1)
        for (i = 0; i < nScales; i++) {
            twoPow = 2 * twoPow;
        }
        npx = (int)(Math.ceil(nx/twoPow)*twoPow);
        npy = (int)(Math.ceil(ny/twoPow)*twoPow);
        newDimLengths = new int[2];
        newDimLengths[0] = npx;
        newDimLengths[1] = npy;
        newArrayLength = npx * npy;
        
        

        try {
            im = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            im = null;
            System.gc();
            displayError("AlgorithmDenoisingBLS_GSM: Out of memory creating a");
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
            padArray = new float[npx*npy];
            // Mirror only down and right
            for (y = 0; y < ny; y++) {
                for (x = 0; x < nx; x++) {
                    padArray[x + npx*y] = im[x + nx*y];
                }
            } // for (y = 0; y < ny; y++)
            for (y = ny, ys = ny-1; y < npy; y++, ys--) {
                for (x = 0; x < nx; x++) {
                    padArray[x + npx*y] = im[x + nx*ys];
                }
            } // for (y = ny, ys = ny-1; y < npy; y++, ys--)
            for (y = 0; y < ny; y++) {
                for (x = nx, xs = nx - 1; x < npx; x++, xs--) {
                    padArray[x + npx*y] = im[xs + nx*y];
                }
            } // for (y = 0; y < ny; y++)
            for (y = ny, ys = ny-1; y < npy; y++, ys--) {
                for (x = nx, xs = nx-1; x < npx; x++, xs--) {
                    padArray[x + npx*y] = im[xs + nx*ys];
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
            for (i = 0; i < nScales - 2; i++) {
                twoPow = 2 * twoPow;
            }
            bx = (blockSizeX - 1)*twoPow;
            by = (blockSizeY - 1)*twoPow;
        } // if ((repres1 == STEERABLE_PYRAMID) || (repres1 == FULL_STEERABLE_PYRAMID))
        else {
            twoPow = 1;
            for (i = 0; i < nScales - 1; i++) {
                twoPow = 2 * twoPow;
            }
            bx = (blockSizeX - 1)*twoPow;
            by = (blockSizeY - 1)*twoPow;    
        } // else
        
        if (usePSD) {
            // Must not use the padded srcImage array with mirroring for FFT 
            String name = srcImage.getImageName()+"_autocorrelation";
            corImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), name,
                                         srcImage.getUserInterface());
            algoAutoCorrelation = new AlgorithmAutoCorrelation(corImage, srcImage);
            algoAutoCorrelation.run();
            corArray = new float[arrayLength];
            try {
                corImage.exportData(0, arrayLength, corArray);
            } catch (IOException error) {
                displayError("AlgorithmDenoisingBLS_GSM: corImage is locked");
                setCompleted(false);
                return;
            }
            
            delta = new double[npx*npy];
            for (y = 0; y < ny; y++) {
                for (x = 0; x < nx; x++) {
                    delta[x + npx*y] = corArray[x + nx*y];
                }
            } // for (y = 0; y < ny; y++)
            
            imagArray = new double[npx*npy];
            // The fft of the autocorrelation gives the power spectral density
            // NOTE: Scale factors do not matter here.
            // forward FFT
            fftUtil = new FFTUtility(delta, imagArray, npy, npx, 1, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(delta, imagArray, 1, npy, npx, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            // A power spectrum is always real and nonnegative
            for (i = 0; i < delta.length; i++) {
                if (delta[i] < 0.0) {
                    delta[i] = 0.0f;
                }
                else {
                    delta[i] = Math.sqrt(delta[i]);
                }
            }
        } // if (usePSD)
        else { // white noise
            delta = new double[npx*npy];
            for (i = 0; i < delta.length; i++) {
                delta[i] = 1.0;
            }
        } // else white noise
        
        imagArray = new double[npx*npy];
        // Inverse FFT
        fftUtil = new FFTUtility(delta, imagArray, npy, npx, 1, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(delta, imagArray, 1, npy, npx, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        center(delta, imagArray);
        
        if (repres1 == ORTHOGONAL_WAVELET) {
            imagArray = new double[npx*npy];
            // forward FFT
            fftUtil = new FFTUtility(delta, imagArray, npy, npx, 1, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(delta, imagArray, 1, npy, npx, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            psArray = new double[npx*npy];
            for (i = 0; i < psArray.length; i++) {
                psArray[i] = delta[i]*delta[i] + imagArray[i]*imagArray[i];
            }
            imagArray = new double[npx*npy];
            center(delta, imagArray);
            // Noise, to be used only with translation variant transforms (such as
            // orthogonal wavelet)
            ranArray = new double[npx*npy];
            randomGen = new RandomNumberGen();
            for (i = 0; i < ranArray.length; i++) {
                ranArray[i] = randomGen.genStandardGaussian();    
            }
            // forward FFT
            fftUtil = new FFTUtility(ranArray, imagArray, npy, npx, 1, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(ranArray, imagArray, 1, npy, npx, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            for (i = 0; i < ranArray.length; i++) {
                ranArray[i] = Math.atan2(imagArray[i], ranArray[i]);
                delta[i] = Math.sqrt(delta[i]);
                imagArray[i] = delta[i] * Math.sin(ranArray[i]);
                delta[i] = delta[i] * Math.cos(ranArray[i]);
            }
            // Inverse FFT
            fftUtil = new FFTUtility(delta, imagArray, npy, npx, 1, +1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(delta, imagArray, 1, npy, npx, +1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
        } // if (repres1 == ORTHOGONAL_WAVELET)
        
        if (useBoundary) {
            im = mirrorExtension(im, npx, npy, bx, by);
            imx = npx + 2*bx;
            imy = npy + 2*by;
            if (repres1 == ORTHOGONAL_WAVELET) {
                delta = mirrorExtension(delta, npx, npy, bx, by);
            }
            else {
                aux = delta;
                delta = new double[(npx + 2*bx)*(npy + 2*by)];
                for (y = 0; y < npy; y++) {
                    for (x = 0; x < npx; x++) {
                        delta[x + bx + (npx + 2*bx)*(y + by)] = aux[x + npx*by];
                    }
                }
            }
        } // if (useBoundary)
        else {
            bx = 0;
            by = 0;
        }
        
        // Normalize the energy (the noise variance is given by "sig")
        mean = 0.0;
        for (i = 0; i < delta.length; i++) {
            mean += delta[i]*delta[i];
        }
        mean = mean/delta.length;
        mean = Math.sqrt(mean);
        for (i = 0; i < delta.length; i++) {
            delta[i] = delta[i]/mean;
        }
        // Impose the desired variance to the noise
        for (i = 0; i < delta.length; i++) {
            delta[i] = sig * delta[i];
        }
        
        // main
        time = System.currentTimeMillis();
        // Standard steerable pyramid
        if (repres1 == STEERABLE_PYRAMID) {
            imD = decompReconst(im, imx, imy, delta);  
            if (error == 1) {
                setCompleted(false);
                return;   
            }
        }

        if (filterType == PWT) {

            switch (cNum) {

                case 4:
                    cc = c4;
                    cr = new double[5];
                    break;

                case 12:
                    cc = c12;
                    cr = new double[13];
                    break;

                case 20:
                    cc = c20;
                    cr = new double[21];
                    break;
            } // switch(cNum)

            for (k = 1; k <= cNum; k++) {
                cr[cNum + 1 - k] = sig * cc[k];
                sig = -sig;
            }

            ioff = joff = -(cNum >> 1);
            // These values of ioff and joff center the "support" of the
            // wavelets at each level.  Alternatively, the "peaks" of the
            // wavelets can be approximately centered by the choices
            // ioff = -2 and joff = -cNum + 2.  Note that
            // daub4 and pwt use different default centerings.
        } // if (filterType == PWT)

        nDims = srcImage.getNDims();
        extents = srcImage.getExtents();
        xDim = extents[0];
        yDim = extents[1];

        if (nDims > 2) {
            zDim = extents[2];
        }

        arrayLength = 1;

        for (i = 0; i < nDims; i++) {
            arrayLength *= extents[i];
        }

        try {
            aArray = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            aArray = null;
            System.gc();
            displayError("AlgorithmWaveletThreshold: Out of memory creating a");
            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, arrayLength, aArray);
        } catch (IOException error) {
            displayError("AlgorithmWaveletThreshold: Source image is locked");
            setCompleted(false);

            return;
        }


        // Create aExp with all dimensions powers of 2 and with zero padding
        // going in cNum - 2 past each boundary
        newExtents = new int[nDims];

        for (i = 0; i < nDims; i++) {
            minSize = extents[i] + (2 * cm2);
            foundSize = false;

            for (int j = 2; (j < 30) && (!foundSize); j++) {

                if ((int) Math.pow(2, j) >= minSize) {
                    newExtents[i] = (int) Math.pow(2, j);
                    foundSize = true;
                } // if ((int)Math.pow(2,j) >= minSize)
            } // for (j = 2; (j < 30) && (!foundSize); j++)
        } // for (i = 0; i < nDims; i++)

        newXDim = newExtents[0];
        newYDim = newExtents[1];

        if (nDims > 2) {
            newZDim = newExtents[2];
        }

        newArrayLength = 1;

        for (i = 0; i < nDims; i++) {
            newArrayLength *= newExtents[i];
        }

        try {
            aExp = new float[newArrayLength];
        } catch (OutOfMemoryError e) {
            aExp = null;
            System.gc();
            displayError("AlgorithmWaveletThreshold: Out of memory creating aExp");
            setCompleted(false);

            return;
        }

        for (i = 0; i < newArrayLength; i++) {
            aExp[i] = 0.0f;
        }

        if (nDims == 2) {
            offsetX = (newXDim - xDim) / 2;
            offsetY = (newYDim - yDim) / 2;

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {
                    aExp[x + offsetX + (newXDim * (y + offsetY))] = aArray[x + (xDim * y)];
                }
            }

            // Mirror the rows
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[x + newXDim*(offsetY - j - 1)] =
            // aExp[x + newXDim*(offsetY + j)];
            // aExp[x + newXDim*(offsetY + yDim + j)] =
            // aExp[x + newXDim*(offsetY + yDim - j - 1)];
            // }
            // }

            // Mirror the columns
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[offsetX - j - 1 + newXDim*y] =
            // aExp[offsetX + j + newXDim*y];
            // aExp[offsetX + xDim + j + newXDim*y] =
            // aExp[offsetX + xDim - j - 1 + newXDim*y];
            // }
            // }

            // Have (cNum-2) squared in each corner to mirror
            // upper left corner
            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // aExp[x + newXDim*y] =
            // aExp[x + delX + newXDim*(y + delY)];
            // }
            // }

            // upper right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // aExp[x + newXDim*y] =
            // aExp[x - delX + newXDim*(y + delY)];
            // }
            // }

            // lower left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // aExp[x + newXDim*y] =
            // aExp[x + delX + newXDim*(y - delY)];
            // }
            // }

            // lower right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // aExp[x + newXDim*y] =
            // aExp[x - delX + newXDim*(y - delY)];
            // }
            // }
        } // if (nDims == 2)
        else if (nDims == 3) {
            offsetX = (newXDim - xDim) / 2;
            offsetY = (newYDim - yDim) / 2;
            offsetZ = (newZDim - zDim) / 2;
            xy = xDim * yDim;
            newXY = newXDim * newYDim;

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {

                    for (z = 0; z < zDim; z++) {
                        aExp[x + offsetX + (newXDim * (y + offsetY)) + (newXY * (z + offsetZ))] = aArray[x +
                                                                                                         (xDim * y) +
                                                                                                         (xy * z)];
                    }
                }
            }

            // Mirror the rows
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[x + newXDim*(offsetY - j - 1) + newXY*z] =
            // aExp[x + newXDim*(offsetY + j) + newXY*z];
            // aExp[x + newXDim*(offsetY + yDim + j) + newXY*z] =
            // aExp[x + newXDim*(offsetY + yDim - j - 1) + newXY*z];
            // }
            // }
            // }

            // Mirror the columns
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[offsetX - j - 1 + newXDim*y + newXY*z] =
            // aExp[offsetX + j + newXDim*y + newXY*z];
            // aExp[offsetX + xDim + j + newXDim*y + newXY*z] =
            // aExp[offsetX + xDim - j - 1 + newXDim*y + newXY*z];
            // }
            // }
            // }

            // Mirror the planes
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (j = 0; j < cm2; j++) {
            // aExp[x + newXDim*y + newXY*(offsetZ - j - 1)] =
            // aExp[x + newXDim*y + newXY*(offsetZ + j)];
            // aExp[x + newXDim*y + newXY*(offsetZ + zDim + j)] =
            // aExp[x + newXDim*y + newXY*(offsetZ + zDim - j - 1)];
            // }
            // }
            // }

            // Mirror 4 xys, 4 xzs, 4 yzs, and 8 xyzs
            // Mirror 4 xys
            // upper left corner
            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y + delY) + newXY*z];
            // }
            // }
            // }

            // upper right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y + delY) + newXY*z];
            // }
            // }
            // }

            // lower left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y - delY) + newXY*z];
            // }
            // }
            // }

            // lower right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ; z < offsetZ + zDim; z++) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y - delY) + newXY*z];
            // }
            // }
            // }

            // Mirror 4 xzs
            // upper left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*y + newXY*(z + delZ)];
            // }
            // }
            // }

            // upper right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*y + newXY*(z + delZ)];
            // }
            // }
            // }

            // lower left corner
            // for (x = offsetX - cm2, delX = 2*cm2 - 1; x < offsetX; x++, delX -=2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*y + newXY*(z - delZ)];
            // }
            // }
            // }

            // lower right corner
            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY; y < offsetY + yDim; y++) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*y + newXY*(z - delZ)];
            // }
            // }
            // }

            // Mirror 4 yzs
            // upper left corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + newXDim*(y + delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // upper right corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + newXDim*(y - delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // lower left corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x+ newXDim*(y + delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // lower right corner
            // for (x = offsetX; x < offsetX + xDim; x++) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + newXDim*(y - delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // 8 xyzs
            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y + delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y + delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y - delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX - cm2,delX = 2*cm2 - 1; x < offsetX; x++, delX -= 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x + delX + newXDim*(y - delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y + delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY - cm2, delY = 2*cm2 - 1; y < offsetY; y++, delY -= 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y + delY) + newXY*(z - delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ - cm2, delZ = 2*cm2 - 1; z < offsetZ; z++, delZ -= 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y - delY) + newXY*(z + delZ)];
            // }
            // }
            // }

            // for (x = offsetX + xDim, delX = 1; x < offsetX + xDim + cm2; x++, delX += 2) {
            // for (y = offsetY + yDim, delY = 1; y < offsetY + yDim + cm2; y++, delY += 2) {
            // for (z = offsetZ + zDim, delZ = 1; z < offsetZ + zDim + cm2; z++, delZ += 2) {
            // aExp[x + newXDim*y + newXY*z] =
            // aExp[x - delX + newXDim*(y - delY) + newXY*(z - delZ)];
            // }
            // }
            // }
        } // else if (nDims == 3)

        transformDir = FORWARD;
        wtn(aExp);

        if (doWaveletImage) {
            waveletImage = new ModelImage(ModelImage.FLOAT, newExtents, "wavelet transform");
            waveletImage.setOriginalExtents(extents);

            try {
                aLog = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                aLog = null;
                System.gc();
                displayError("AlgorithmWaveletThreshold: Out of memory creating aLog");
                setCompleted(false);

                return;
            }

            for (i = 0; i < newArrayLength; i++) {
                aLog[i] = (float) (0.4342944819 * Math.log(1 + Math.abs(aExp[i])));
            }

            try {
                waveletImage.importData(0, aLog, true);
            } catch (IOException error) {
                displayError("AlgorithmWaveletThreshold: IOException on wavelet image import data");
                setCompleted(false);

                return;
            }
        } // if (doWaveletImage)

        aMax = -Float.MAX_VALUE;

        for (i = 0; i < aExp.length; i++) {

            if (Math.abs(aExp[i]) > aMax) {
                aMax = Math.abs(aExp[i]);
            }
        }

        belowThreshold = 0;
        aCutoff = aMax * threshold;

        switch (thresholdType) {

            case HARD:
                for (i = 0; i < aExp.length; i++) {

                    if (Math.abs(aExp[i]) < aCutoff) {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    }
                }

                break;

            case SOFT:
                for (i = 0; i < aExp.length; i++) {

                    if (aExp[i] >= aCutoff) {
                        aExp[i] -= aCutoff;
                    } else if (aExp[i] <= -aCutoff) {
                        aExp[i] += aCutoff;
                    } else {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    }
                }

                break;
                
            case NONNEGATIVE_GARROTE:
                aCutSquared = aCutoff * aCutoff;
                for (i = 0; i < aExp.length; i++) {
                    if (Math.abs(aExp[i]) < aCutoff) {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    }
                    else {
                        aExp[i] -= aCutSquared/aExp[i];
                    }
                }
                break;
                
            case SCAD:
                for (i = 0; i < aExp.length; i++) {
                    if (Math.abs(aExp[i]) < aCutoff) {
                        aExp[i] = 0.0f;
                        belowThreshold++;
                    } else if ((aExp[i] >= aCutoff) && (aExp[i] <= 2*aCutoff)) {
                        aExp[i] -= aCutoff;
                    } else if ((aExp[i] <= -aCutoff) && (aExp[i] >= -2*aCutoff)) {
                        aExp[i] += aCutoff;
                    } else if ((aExp[i] > 2*aCutoff) && (aExp[i] <= 3.7*aCutoff)) {
                        aExp[i] = (float)((2.7*aExp[i] - 3.7*aCutoff)/1.7);
                    } else if ((aExp[i] < -2*aCutoff) && (aExp[i] >= -3.7*aCutoff)) {
                        aExp[i] = (float)((2.7*aExp[i] + 3.7*aCutoff)/1.7);
                    }
                }
                break;
        } // switch(thresholdType)
        System.out.println(belowThreshold + " of the " + aExp.length + " coefficents are below threshold");

        transformDir = INVERSE;
        wtn(aExp);

        // Strip the outer pixels from aExp to return to the originally
        // sized a
        if (nDims == 2) {

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {
                    aArray[x + (xDim * y)] = aExp[x + offsetX + (newXDim * (y + offsetY))];
                }
            }
        } // if (nDims == 2)
        else if (nDims == 3) {

            for (x = 0; x < xDim; x++) {

                for (y = 0; y < yDim; y++) {

                    for (z = 0; z < zDim; z++) {
                        aArray[x + (xDim * y) + (xy * z)] = aExp[x + offsetX + (newXDim * (y + offsetY)) +
                                                                 (newXY * (z + offsetZ))];
                    }
                }
            }
        } // else if (nDims == 3)

        aExp = null;

        // clamp to mins and maxs allowed by data type
        if (destImage != null) {
            dataType = destImage.getType();
        } else {
            dataType = srcImage.getType();
        }

        if (dataType == ModelImage.UBYTE) {

            for (i = 0; i < aArray.length; i++) {

                if (aArray[i] > 255.0f) {
                    aArray[i] = 255.0f;
                } else if (aArray[i] < 0.0f) {
                    aArray[i] = 0.0f;
                }
            }
        } // if (dataType == ModelImage.UBYTE)
        else if (dataType == ModelImage.BYTE) {

            for (i = 0; i < aArray.length; i++) {

                if (aArray[i] > 127.0f) {
                    aArray[i] = 127.0f;
                } else if (aArray[i] < -128.0f) {
                    aArray[i] = -128.0f;
                }
            }
        } // else if (dataType == ModelImage.BYTE)
        else if (dataType == ModelImage.USHORT) {

            for (i = 0; i < aArray.length; i++) {

                if (aArray[i] > 65535.0f) {
                    aArray[i] = 65535.0f;
                } else if (aArray[i] < 0.0f) {
                    aArray[i] = 0.0f;
                }
            }
        } // else if (dataType == ModelImage.USHORT)
        else if (dataType == ModelImage.SHORT) {

            for (i = 0; i < aArray.length; i++) {

                if (aArray[i] > 32767.0f) {
                    aArray[i] = 32767.0f;
                } else if (aArray[i] < -32768.0f) {
                    aArray[i] = -32768.0f;
                }
            }
        } // else if (dataType == ModelImage.SHORT)
        else if (dataType == ModelImage.UINTEGER) {

            for (i = 0; i < aArray.length; i++) {

                if (aArray[i] > 4294967295L) {
                    aArray[i] = 4294967295L;
                } else if (aArray[i] < 0) {
                    aArray[i] = 0;
                }
            }
        } else if (dataType == ModelImage.INTEGER) {

            for (i = 0; i < aArray.length; i++) {

                if (aArray[i] > Integer.MAX_VALUE) {
                    aArray[i] = Integer.MAX_VALUE;
                }

                if (aArray[i] < Integer.MIN_VALUE) {
                    aArray[i] = Integer.MIN_VALUE;
                }
            }
        }

        if (destImage != null) {

            try {
                destImage.importData(0, aArray, true);
            } catch (IOException error) {
                displayError("AlgorithmWaveletThreshold: IOException on destination image import data");
                setCompleted(false);

                return;
            }
        } else {

            try {
                srcImage.importData(0, aArray, true);
            } catch (IOException error) {
                displayError("AlgorithmWaveletThreshold: IOException on source image import data");
                setCompleted(false);

                return;
            }

        }

        setCompleted(true);

    }
    
    /**
     * Decompose image into subbands, denoise, and recompose again
     * Port of routine written by Javier Portilla
     *
     * @param fn
     * @param fnx x dimension of fn
     * @param fny y dimension of fn
     * @param noise
     * @return
     */
    private float[] decompReconst(float fn[], int fnx, int fny, double noise[]) {
        float fh[] = null;
        int pind[][] = null;
        Vector pyr = null;
        pyr = buildSFpyr(fn, fnx, fny, nScales, nOrientations-1, 1, pind);
        if (error == 1) {
            return null;
        }
        return fh;
    }
    
    /**
     * This is a port of a MATLAB routine by Eero Simoncelli.
     * Construct a steerable pyramid on array im, in the Fourier domain.  This is 
     * similar to buildSpyr, except that:
     * + Reconstruction is exact (within floating point errors)
     * + It can produce any number of orientation bands.
     * - Typically slower, especially for non-power-of-two sizes
     * - Boundary handling is circular
     * @param im
     * @param imx x dimension of im
     * @param imy y dimension of im
     * @param ht  Specfies the number of pyramid levels to build
     * @param order   The squared radial functions tile the Fourier plane, with a
     *                raised-cosine falloff.  Angular functions are
     *                cos(theta - k/PI/(order +1))^order.  order is one less than
     *                the number of orientation bands.  default = 3
     * @param twidth  The width of the transition region of the radial lowpass function
     *                in octaves.  (default = 1, which gives a raised cosine for the
     *                bandpass filters).
     * @param indices An N by 2 matrix containing the sizes of each subband.
     * @param error[] 0 = no error, 1 = error
     * @return pyr    A vector containing the N pyramid subbands, ordered from fine
     *                to coarse 
     */
    private Vector buildSFpyr(float[] im, int imx, int imy, int ht, int order, int twidth,
                              int indices[][]) {
        int max_ht;
        Vector pyr = new Vector();
        // log2(x) = loge(x)/loge(2)
        max_ht = (int)Math.floor((1.0/Math.log(2.0))*Math.log(Math.min(imx,imy)) + 2);
        
        if (ht > max_ht) {
            MipavUtil.displayError("Error! Cannot build pyramid higher than " + 
                                   max_ht + " levels");
            error = 1;
            return null;
        }
        return pyr;
    }
    
    private float[] mirrorExtension(float im[], int nx, int ny, int bx, int by) {
        int x, y, xs, ys;
        int npx = nx + 2*bx;
        float pad[] = new float[(nx+2*bx)*(ny+2*by)];
        for (y = 0; y < ny; y++) {
            for (x = 0; x < nx; x++) {
                pad[x + bx + npx*(y+by)] = im[x + nx*y];
            }
        } // for (y = 0; y < ny; y++)
        for (y = 0, ys = 2*by - 1; y <= by - 1; y++, ys--) {
            for (x = bx; x < nx + bx; x++) {
                pad[x + npx*y] = pad[x + npx*ys];
            }
        }
        for (y = by; y < ny + by; y++) {
            for (x = 0, xs = 2*bx - 1; x <= bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*y];
            }
        }
        for (y = ny + by, ys = ny + by - 1; y <= ny + 2*by - 1; y++, ys--) {
            for (x = bx; x < nx + bx; x++) {
                pad[x + npx*y] = pad[x + npx*ys];
            }
        }
        for (y = by; y < ny + by; y++) {
            for (x = nx + bx, xs = nx + bx - 1; x <= nx + 2*bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*y];
            }
        }
        for (y = 0, ys = 2*by-1; y <= by-1; y++, ys--) {
            for (x = 0, xs = 2*bx-1; x <= bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        for (y = ny + by, ys = ny + by - 1; y <= ny + 2*by - 1; y++, ys--) {
            for (x = nx + bx, xs = nx + bx - 1; x <= nx + 2*bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        for (y = 0, ys = 2*by - 1; y <= by - 1; y++, ys--) {
            for (x = nx + bx, xs = nx + bx - 1; x <= nx + 2*bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        for (y = ny + by, ys = ny + by - 1; y <= ny + 2*by - 1; y++, ys--) {
            for (x = 0, xs = 2*bx-1; x <= bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        return pad;
    } 
    
    private double[] mirrorExtension(double im[], int nx, int ny, int bx, int by) {
        int x, y, xs, ys;
        int npx = nx + 2*bx;
        double pad[] = new double[(nx+2*bx)*(ny+2*by)];
        for (y = 0; y < ny; y++) {
            for (x = 0; x < nx; x++) {
                pad[x + bx + npx*(y+by)] = im[x + nx*y];
            }
        } // for (y = 0; y < ny; y++)
        for (y = 0, ys = 2*by - 1; y <= by - 1; y++, ys--) {
            for (x = bx; x < nx + bx; x++) {
                pad[x + npx*y] = pad[x + npx*ys];
            }
        }
        for (y = by; y < ny + by; y++) {
            for (x = 0, xs = 2*bx - 1; x <= bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*y];
            }
        }
        for (y = ny + by, ys = ny + by - 1; y <= ny + 2*by - 1; y++, ys--) {
            for (x = bx; x < nx + bx; x++) {
                pad[x + npx*y] = pad[x + npx*ys];
            }
        }
        for (y = by; y < ny + by; y++) {
            for (x = nx + bx, xs = nx + bx - 1; x <= nx + 2*bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*y];
            }
        }
        for (y = 0, ys = 2*by-1; y <= by-1; y++, ys--) {
            for (x = 0, xs = 2*bx-1; x <= bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        for (y = ny + by, ys = ny + by - 1; y <= ny + 2*by - 1; y++, ys--) {
            for (x = nx + bx, xs = nx + bx - 1; x <= nx + 2*bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        for (y = 0, ys = 2*by - 1; y <= by - 1; y++, ys--) {
            for (x = nx + bx, xs = nx + bx - 1; x <= nx + 2*bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        for (y = ny + by, ys = ny + by - 1; y <= ny + 2*by - 1; y++, ys--) {
            for (x = 0, xs = 2*bx-1; x <= bx - 1; x++, xs--) {
                pad[x + npx*y] = pad[xs + npx*ys];
            }
        }
        return pad;
    } 
    
    
    
    
    
    /**
     * Centers the FFT for display purposes.
     *
     * @param  rData  real data buffer
     * @param  iData  imaginary data buffer
     */
    private void center(float[] rData, float[] iData) {

        // center() is called after the forward fast fourier transform to enhance the display
        // center() is called before the inverse fast fourier transform to return the data
        // to its original ordering.
        int i, j, k;
        int xdim, ydim, zdim;
        int xnew, ynew, znew;
        int xdimHalf, ydimHalf, zdimHalf;
        float[] centerData;
        int newLength;

        if (image25D) {
            newLength = newDimLengths[0] * newDimLengths[1];
        } else {
            newLength = newArrayLength;
        }

        // Perform a data centering operation
        // Center 1D data
        if (ndim == 1) {

            try {
                centerData = new float[newDimLengths[0]];
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating centerData");
                setCompleted(false);
                return;
            }

            xdimHalf = newDimLengths[0] / 2;

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                centerData[xnew] = rData[i];
            }

            for (i = xdimHalf; i < newDimLengths[0]; i++) {

                xnew = i - xdimHalf;
                centerData[xnew] = rData[i];
            }

            for (i = 0; i < newArrayLength; i++) {
                rData[i] = centerData[i];
            }

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                centerData[xnew] = iData[i];
            }

            for (i = xdimHalf; i < newDimLengths[0]; i++) {

                xnew = i - xdimHalf;
                centerData[xnew] = iData[i];
            }

            for (i = 0; i < newArrayLength; i++) {
                iData[i] = centerData[i];
            }

        } // end of for ndim == 1

        // Center 2D data
        else if ((ndim == 2) || (image25D)) {

            try {
                centerData = new float[newDimLengths[0] * newDimLengths[1]]; // Temp storage for centered
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating centerData");
                setCompleted(false);

                return;
            }

            xdimHalf = newDimLengths[0] / 2;
            ydimHalf = newDimLengths[1] / 2;
            xdim = newDimLengths[0];
            ydim = newDimLengths[1];

            for (j = 0; j < ydimHalf; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = 0; j < ydimHalf; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (i = 0; i < newLength; i++) {
                rData[i] = centerData[i];
            }

            for (j = 0; j < ydimHalf; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = 0; j < ydimHalf; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (i = 0; i < newLength; i++) {
                iData[i] = centerData[i];
            }

        } // end of else if ((ndim == 2) || (image25D))

        // 3D center
        // center data is buffer to hold 1/8 of data as we center the FFT
        else if (ndim == 3) {

            try {
                centerData = new float[newDimLengths[0] / 2 * newDimLengths[1] / 2 * newDimLengths[2] / 2];
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating centerData");
                setCompleted(false);

                return;
            }

            xdimHalf = newDimLengths[0] / 2;
            ydimHalf = newDimLengths[1] / 2;
            zdimHalf = newDimLengths[2] / 2;
            xdim = newDimLengths[0];
            ydim = newDimLengths[1];
            zdim = newDimLengths[2];

            int sliceSize = xdim * ydim;
            int centerSliceSize = xdimHalf * ydimHalf;

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        centerData[(centerSliceSize * k) + (xdimHalf * j) + i] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 2
            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = rData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 3
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = rData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 4
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = rData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********** start sector 1

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        centerData[(centerSliceSize * k) + (xdimHalf * j) + i] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 2
            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = iData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 3
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = iData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 4
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = iData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

        } // end of else if (ndim == 3)
    } // center
    
    /**
     * Centers the FFT for display purposes.
     *
     * @param  rData  real data buffer
     * @param  iData  imaginary data buffer
     */
    private void center(double[] rData, double[] iData) {

        // center() is called after the forward fast fourier transform to enhance the display
        // center() is called before the inverse fast fourier transform to return the data
        // to its original ordering.
        int i, j, k;
        int xdim, ydim, zdim;
        int xnew, ynew, znew;
        int xdimHalf, ydimHalf, zdimHalf;
        double[] centerData;
        int newLength;

        if (image25D) {
            newLength = newDimLengths[0] * newDimLengths[1];
        } else {
            newLength = newArrayLength;
        }

        // Perform a data centering operation
        // Center 1D data
        if (ndim == 1) {

            try {
                centerData = new double[newDimLengths[0]];
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating centerData");
                setCompleted(false);

                return;
            }

            xdimHalf = newDimLengths[0] / 2;

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                centerData[xnew] = rData[i];
            }

            for (i = xdimHalf; i < newDimLengths[0]; i++) {

                xnew = i - xdimHalf;
                centerData[xnew] = rData[i];
            }

            for (i = 0; i < newArrayLength; i++) {
                rData[i] = centerData[i];
            }

            for (i = 0; i < xdimHalf; i++) {

                xnew = i + xdimHalf;
                centerData[xnew] = iData[i];
            }

            for (i = xdimHalf; i < newDimLengths[0]; i++) {

                xnew = i - xdimHalf;
                centerData[xnew] = iData[i];
            }

            for (i = 0; i < newArrayLength; i++) {
                iData[i] = centerData[i];
            }

        } // end of for ndim == 1

        // Center 2D data
        else if ((ndim == 2) || (image25D)) {

            try {
                centerData = new double[newDimLengths[0] * newDimLengths[1]]; // Temp storage for centered
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating centerData");
                setCompleted(false);

                return;
            }

            xdimHalf = newDimLengths[0] / 2;
            ydimHalf = newDimLengths[1] / 2;
            xdim = newDimLengths[0];
            ydim = newDimLengths[1];

            for (j = 0; j < ydimHalf; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = 0; j < ydimHalf; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = rData[(newDimLengths[0] * j) + i];
                }
            }

            for (i = 0; i < newLength; i++) {
                rData[i] = centerData[i];
            }

            for (j = 0; j < ydimHalf; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = 0; i < xdimHalf; i++) {

                    xnew = i + xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = ydimHalf; j < newDimLengths[1]; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j - ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (j = 0; j < ydimHalf; j++) {

                for (i = xdimHalf; i < newDimLengths[0]; i++) {

                    xnew = i - xdimHalf;
                    ynew = j + ydimHalf;
                    centerData[(newDimLengths[0] * ynew) + xnew] = iData[(newDimLengths[0] * j) + i];
                }
            }

            for (i = 0; i < newLength; i++) {
                iData[i] = centerData[i];
            }

        } // end of else if ((ndim == 2) || (image25D))

        // 3D center
        // center data is buffer to hold 1/8 of data as we center the FFT
        else if (ndim == 3) {

            try {
                centerData = new double[newDimLengths[0] / 2 * newDimLengths[1] / 2 * newDimLengths[2] / 2];
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating centerData");
                setCompleted(false);

                return;
            }

            xdimHalf = newDimLengths[0] / 2;
            ydimHalf = newDimLengths[1] / 2;
            zdimHalf = newDimLengths[2] / 2;
            xdim = newDimLengths[0];
            ydim = newDimLengths[1];
            zdim = newDimLengths[2];

            int sliceSize = xdim * ydim;
            int centerSliceSize = xdimHalf * ydimHalf;

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        centerData[(centerSliceSize * k) + (xdimHalf * j) + i] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 2
            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = rData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 3
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = rData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 4
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = rData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = rData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j;
                        znew = k + zdimHalf;
                        rData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********** start sector 1

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        centerData[(centerSliceSize * k) + (xdimHalf * j) + i] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 2
            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = iData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j + ydimHalf;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 3
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = iData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

            // ********* start sector 4
            for (k = 0; k < zdimHalf; k++) {

                for (j = ydimHalf; j < ydim; j++) {

                    for (i = xdimHalf; i < xdim; i++) {
                        xnew = i - xdimHalf;
                        ynew = j - ydimHalf;
                        znew = k;
                        centerData[(centerSliceSize * znew) + (xdimHalf * ynew) + xnew] = iData[(sliceSize * k) +
                                                                                                (xdim * j) + i];
                    }
                }
            }

            for (k = zdimHalf; k < zdim; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i + xdimHalf;
                        ynew = j + ydimHalf;
                        znew = k - zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = iData[(sliceSize * k) + (xdim * j) + i];
                    }
                }
            }

            for (k = 0; k < zdimHalf; k++) {

                for (j = 0; j < ydimHalf; j++) {

                    for (i = 0; i < xdimHalf; i++) {
                        xnew = i;
                        ynew = j;
                        znew = k + zdimHalf;
                        iData[(sliceSize * znew) + (xdim * ynew) + xnew] = centerData[(centerSliceSize * k) +
                                                                                      (xdimHalf * j) + i];
                    }
                }
            }

        } // end of else if (ndim == 3)
    } // center


    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        historyString = new String("WaveletThreshold(" + String.valueOf(cNum) + String.valueOf(thresholdType) +
                                   String.valueOf(threshold) + String.valueOf(doWaveletImage) + ")\n");
    }

    /**
     * DOCUMENT ME!
     *
     * @param  a  data vector to which Daubechies 4-coefficient wavelet filter or its transpose (for transformDir ==
     *            INVERSE) is applied
     * @param  n  DOCUMENT ME!
     */
    private void daub4(float[] a, int n) {
        double[] wksp;
        int nh, nh1, i, j;

        if (n < 4) {
            MipavUtil.displayError("Length of daub4 array must be at least 4");
            setCompleted(false);

            return;
        }

        wksp = new double[n];
        nh1 = (nh = n >> 1) + 1;

        if (transformDir == FORWARD) { // Apply filter

            for (i = 0, j = 0; j < (n - 3); j += 2, i++) {
                wksp[i] = (C0 * a[j]) + (C1 * a[j + 1]) + (C2 * a[j + 2]) + (C3 * a[j + 3]);
                wksp[i + nh] = (C3 * a[j]) - (C2 * a[j + 1]) + (C1 * a[j + 2]) - (C0 * a[j + 3]);
            }

            wksp[i] = (C0 * a[n - 2]) + (C1 * a[n - 1]) + (C2 * a[0]) + (C3 * a[1]);
            wksp[i + nh] = (C3 * a[n - 2]) - (C2 * a[n - 1]) + (C1 * a[0]) - (C0 * a[1]);
        } // if (transformDir == FORWARD)
        else { // Apply transpose filter
            wksp[0] = (C2 * a[nh - 1]) + (C1 * a[n - 1]) + (C0 * a[0]) + (C3 * a[nh1 - 1]);
            wksp[1] = (C3 * a[nh - 1]) - (C0 * a[n - 1]) + (C1 * a[0]) - (C2 * a[nh1 - 1]);

            for (i = 0, j = 2; i < (nh - 1); i++) {
                wksp[j++] = (C2 * a[i]) + (C1 * a[i + nh]) + (C0 * a[i + 1]) + (C3 * a[i + nh1]);
                wksp[j++] = (C3 * a[i]) - (C0 * a[i + nh]) + (C1 * a[i + 1]) - (C2 * a[i + nh1]);
            }
        } // else apply transpose filter

        for (i = 0; i < n; i++) {
            a[i] = (float) wksp[i];
        }

        wksp = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  a  data to which wavelet filter (for transformDir == FORWARD) or to which transpose wavelet filter is
     *            applied (for transformDir == INVERSE)
     * @param  n  Used hierarchically by routines wt1 and wtn
     */
    private void pwt(float[] a, int n) {
        double ai, ai1;
        double[] wksp;
        int i, ii, j, jf, jr, k, n1, ni, nj, nh, nmod;

        if (n < 4) {
            MipavUtil.displayError("Length of pwt array must be at least 4");
            setCompleted(false);

            return;
        }

        wksp = new double[n];
        nmod = cNum * n;
        n1 = n - 1; // Mask of all bits, since n a power of 2
        nh = n >> 1;

        for (j = 0; j < n; j++) {
            wksp[j] = 0.0;
        }

        if (transformDir == FORWARD) { // Apply filter

            for (ii = 1, i = 1; i <= n; i += 2, ii++) {
                ni = i + nmod + ioff; // Pointer to be incremented and wrapped around
                nj = i + nmod + joff;

                for (k = 1; k <= cNum; k++) {
                    jf = n1 & (ni + k); // use bitwise and to wrap around the pointers
                    jr = n1 & (nj + k);
                    wksp[ii - 1] += cc[k] * a[jf];
                    wksp[ii + nh - 1] += cr[k] * a[jr];
                }
            }
        } // if (transformDir == FORWARD)
        else { // Apply transpose filter

            for (ii = 1, i = 1; i <= n; i += 2, ii++) {
                ai = a[ii - 1];
                ai1 = a[ii + nh - 1];
                ni = i + nmod + ioff;
                nj = i + nmod + joff;

                for (k = 1; k <= cNum; k++) {
                    jf = (n1 & (ni + k)) + 1;
                    jr = (n1 & (nj + k)) + 1;
                    wksp[jf - 1] += cc[k] * ai;
                    wksp[jr - 1] += cr[k] * ai1;
                }
            }
        } // else apply transpose filter

        for (j = 0; j < n; j++) {
            a[j] = (float) wksp[j];
        }

        wksp = null;
    }


    /**
     * DOCUMENT ME!
     *
     * @param  a  data replaced by its wavelet transform for transformDir == FORWARD or by its inverse wavelet transform
     *            for transformDir == INVERSE One dimensional discrete wavelet transform. Note that the length of a must
     *            be an integer power of 2
     */
    private void wt1(float[] a) {
        int nn;
        int len;
        len = a.length;

        if (len < 4) {
            MipavUtil.displayError("Length of wt1 array must be at least 4");
            setCompleted(false);

            return;
        }

        if (transformDir == FORWARD) { // wavelet transform

            // Start at larget hierarchy, and work towards smallest
            switch (filterType) {

                case DAUB4:
                    for (nn = len; nn >= 4; nn >>= 1) {
                        daub4(a, nn);
                    }

                    break;

                case PWT:
                    for (nn = len; nn >= 4; nn >>= 1) {
                        pwt(a, nn);
                    }

                    break;
            } // switch(filterType)
        } // if (transformDir == FORWARD)
        else { // inverse wavelet transform

            // Start at smallest hierarchy, and work towards largest
            switch (filterType) {

                case DAUB4:
                    for (nn = 4; nn <= len; nn <<= 1) {
                        daub4(a, nn);
                    }

                    break;

                case PWT:
                    for (nn = 4; nn <= len; nn <<= 1) {
                        pwt(a, nn);
                    }

                    break;
            } // switch(filterType)
        } // else inverse wavelet transform
    }

    /**
     * DOCUMENT ME!
     *
     * @param  a  data replaced by its wavelet transform for transformDir == FORWARD or by its inverse wavelet transform
     *            for transformDir == INVERSE n-dimensional discrete wavelet transform. Note that the length of each
     *            dimension of a must be an integer power of 2
     */
    private void wtn(float[] a) {
        int i1, i2, i3, k, n, nnew, nprev = 1, nt, ntot = 1, idim, ndim;
        float[] wksp;

        ndim = newExtents.length;

        for (idim = 0; idim < ndim; idim++) {
            ntot *= newExtents[idim];
        }

        wksp = new float[ntot];

        for (idim = 1; idim <= ndim; idim++) { // Main loop over dimensions
            n = newExtents[idim - 1];
            nnew = n * nprev;

            if (n > 4) {

                for (i2 = 0; i2 < ntot; i2 += nnew) {

                    for (i1 = 1; i1 <= nprev; i1++) {

                        for (i3 = i1 + i2, k = 1; k <= n; k++, i3 += nprev) {
                            wksp[k - 1] = a[i3 - 1];
                        }

                        // Copy the relevant row or column or etc. into workspace
                        if (transformDir == FORWARD) { // one-dimensional wavelet transform

                            switch (filterType) {

                                case DAUB4:
                                    for (nt = n; nt >= 4; nt >>= 1) {
                                        daub4(wksp, nt);
                                    }

                                    break;

                                case PWT:
                                    for (nt = n; nt >= 4; nt >>= 1) {
                                        pwt(wksp, nt);
                                    }

                                    break;
                            } // switch(filterType)
                        } // if (transformDir == FORWARD)
                        else { // or inverse transform

                            switch (filterType) {

                                case DAUB4:
                                    for (nt = 4; nt <= n; nt <<= 1) {
                                        daub4(wksp, nt);
                                    }

                                    break;

                                case PWT:
                                    for (nt = 4; nt <= n; nt <<= 1) {
                                        pwt(wksp, nt);
                                    }

                                    break;
                            } // switch(filterType)
                        } // else inverse transform

                        for (i3 = i1 + i2, k = 1; k <= n; k++, i3 += nprev) {
                            a[i3 - 1] = (float) wksp[k - 1];
                        }
                        // Copy back from workspace
                    }
                }
            }

            nprev = nnew;
        }

        wksp = null;
    }
}
