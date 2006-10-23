package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

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
    
    // Possible repres1 values
    private static final int ORTHOGONAL_WAVELET = 1;
    
    private static final int UNDECIMATED_DAUBECHIES_WAVELET = 2;
    
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
    
    private static final int GAUSS3 = 11;
    
    private static final int GAUSS5 = 12;
    
    private static final int BINOMIAL = 13;
    
    // Types of edges
    private static final int CIRCULAR = 1;
    
    private static final int REFLECT1 = 2;
    
    private static final int EXTEND = 3;
    
    private static final int REPEAT = 4;
    
    // Phase solutions for Daubechies scaling and wavelet filters
    private static final int MINIMUM_PHASE = 1;
    
    private static final int MID_PHASE = 2;
    
    private static final int MAXIMUM_PHASE = 3;
    
    
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

    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] aArray;

    

    /** DOCUMENT ME! */
    private int arrayLength;

    
    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private ModelImage waveletImage = null;
    
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
     *           QMF9 is default.
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
    
    private int npx; // Integer multiple of 2**(nScales+1) for applying pyramidal representation
    
    private int npy; // Integer multiple of 2**(nScales+1) for applying pyramidal representation
    
    private int error = 0; // 0 for no error, 1 for error

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     *
     * @param  srcImg          source image model
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
    public AlgorithmDenoisingBLS_GSM(ModelImage srcImg, float sig, boolean usePSD, 
                                     int blockSizeX, int blockSizeY, boolean useParent, boolean useBoundary,
                                     int nScales, int nOrientations, boolean includeCovar,
                                     boolean optimize, int repres1, int repres2, float seed) {
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
        this.seed = seed;
    }

    /**
     * Creates a new AlgorithmWaveletThreshold object.
     *
     * @param  destImg         image model where result image is to be stored
     * @param  srcImg          source image model
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
    public AlgorithmDenoisingBLS_GSM(ModelImage destImg, ModelImage srcImg, float sig, boolean usePSD,
                                     int blockSizeX, int blockSizeY, boolean useParent, boolean useBoundary,
                                     int nScales, int nOrientations, boolean includeCovar,
                                     boolean optimize, int repres1, int repres2, float seed) {
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
        aArray = null;
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
     * This contains a port of the code contained in denoi_BLS_GSM.m written by
     * Javier Portilla, Univ. de Granada, 11/15/2004
     */
    public void runAlgorithm() {
        int k;
        int x, y, z;
        int xs;
        int ys;
        int twoPow;
        int i;
        int bpx;
        int bpy;
        double corArray[];
        double padArray[];
        int bx;
        int by;
        int extents[] = new int[2];
        ModelImage padImage;
        ModelImage corImage;
        AlgorithmAutoCorrelation algoAutoCorrelation;
        double imagArray[];
        double delta[];
        int deltax; // x dimension of delta
        int deltay; // y dimension of delta
        FFTUtility fftUtil;
        double psArray[];
        RandomNumberGen randomGen;
        double ranArray[];
        double im[];
        int imx; // x dimension of im
        int imy; // y dimesnion of im
        double imD[];
        double aux[];
        double mean;
        long time;
        int filter;
        int daubOrder = 2;

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
        
        if (((repres1 == ORTHOGONAL_WAVELET) || (repres1 == UNDECIMATED_DAUBECHIES_WAVELET)) &&
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
        
        try {
            im = new double[arrayLength];
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
            padArray = new double[npx*npy];
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
            corArray = new double[arrayLength];
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
        deltax = npx;
        deltay = npy;
        
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
        center(delta, imagArray, npx, npy);
        
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
            center(delta, imagArray, npx, npy);
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
            deltax = npx + 2*bx;
            deltay = npy + 2*by;
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
            imD = decompReconst(im, imx, imy, delta, deltax, deltay);  
            if (error == 1) {
                setCompleted(false);
                return;   
            }
        }
        else if (repres1 == FULL_STEERABLE_PYRAMID) {
            imD = decompReconstFull(im, imx, imy, delta, deltax, deltay);
        }
        else if (repres1 == ORTHOGONAL_WAVELET) {
            filter = repres2;
            imD = decompReconstW(im, imx, imy, filter, delta, deltax, deltay);
        }
        else if (repres1 == UNDECIMATED_DAUBECHIES_WAVELET) {
            if (repres2 == HAAR) {
                daubOrder = 2;
            }
            else if (repres2 == DAUB2) {
                daubOrder = 4;
            }
            else if (repres2 == DAUB3) {
                daubOrder = 6;
            }
            else if (repres2 == DAUB4) {
                daubOrder = 8;
            }
            imD = decompReconstWU(im, imx, imy, daubOrder, delta, deltax, deltay);
        }

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

        setCompleted(true);

    }
    
    /**
     * This is a port of decomp_reconst_full.m, JPM, Univ. de Granada, 11/04
     * Decompose image into subbands, denoise, and recompose again
     * @param im
     * @param imx
     * @param imy
     * @param noise
     * @param noisex
     * @param noisey
     * @return
     */
    private double[] decompReconstFull(double im[], int imx, int imy, double noise[],
                                       int noisex, int noisey) {
        double fh[] = null;
        Vector pind = null;
        Vector pyr = null;
        Vector pyrN = null;
        Vector pyrh;
        int bandNum;
        int nband;
        double aux[] = null;
        double auxn[] = null;
        int nsx[] = new int[1];
        int nsy[] = new int[1];
        int nsxn[] = new int[1];
        int nsyn[] = new int[1];
        int nsxnn[] = new int[1];
        int nsynn[] = new int[1];
        boolean prnt;
        double BL[][][];
        double BLn[][][];
        double var;
        int i, j;
        int index;
        double imagArray[] = null;
        double sy2;
        double sn2;
        double SNRin;
        double BLT[];
        int blx;
        int bly;
        int intMat[];
        
        pyr = buildFullSFpyr2(im, imx, imy, nScales, nOrientations-1, 1.0, pind);
        if (error == 1) {
            return null;
        }
        pyrN = buildFullSFpyr2(noise, noisex, noisey, nScales, nOrientations-1, 1.0, pind);
        if (error == 1) {
            return null;
        }
        pyrh = pyr;
        bandNum = pind.size()-1;
        // Everything except the low pass residual
        for (nband = 2; nband <= bandNum; nband++) {
            fireProgressStateChanged((100 * (nband-1))/ (bandNum-1));
            aux = pyrBand(pyr, pind, nband-1,nsx, nsy);
            auxn = pyrBand(pyrN, pind, nband-1, nsxn, nsyn);
            prnt = useParent && (nband < bandNum - nOrientations);
            if (prnt) {
                BL = new double[nsx[0]][nsy[0]][2];
                BLn = new double[nsx[0]][nsy[0]][2];
            } // if (prnt)
            else {
                BL = new double[nsx[0]][nsy[0]][1];
                BLn = new double[nsx[0]][nsy[0]][1];
            } // else 
            // Because we are discarding 2 coefficients on every dimension
            var = Math.sqrt(((nsx[0]-2)*(nsy[0]-2))/(nsx[0]*nsy[0]));
            for (j = 0; j < nsy[0]; j++) {
                for (i = 0; i < nsx[0]; i++) {
                    index = i + j * nsx[0];
                    BL[i][j][0] = aux[index];
                    BLn[i][j][0] = auxn[index]*var;
                }
            }
            if (prnt) {
                aux = pyrBand(pyr, pind, nband+nOrientations-1, nsxn, nsyn);
                auxn = pyrBand(pyrN, pind, nband+nOrientations-1, nsxnn, nsynn);
                // Resample 2x2 the parent if not in the high-pass oriented subbands.
                if (nband > nOrientations+1) {
                    expand(aux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                    expand(auxn, 2.0, nsxnn[0], nsynn[0], auxn, imagArray);    
                } // if (nband > nOrientations+1)
                for (j = 0; j < nsy[0]; j++) {
                    for (i = 0; i < nsx[0]; i++) {
                        index = i + j * nsx[0];
                        BL[i][j][1] = aux[index];
                        BLn[i][j][1] = auxn[index]*var;
                    }
                }
            } // if (prnt)
            
            sy2 = 0.0;
            sn2 = 0.0;
            for (j = 0; j < nsy[0]; j++) {
                for (i = 0; i < nsx[0]; i++) {
                    sy2 = sy2 + BL[i][j][0]*BL[i][j][0];
                    sn2 = sn2 + BLn[i][j][0]*BLn[i][j][0];
                }
            }
            sy2 = sy2/(nsx[0]*nsy[0]);
            sn2 = sn2/(nsx[0]*nsy[0]);
            if (sy2 > sn2) {
                SNRin = 4.342944819*Math.log((sy2-sn2)/sn2);
            }
            else {
                Preferences.debug(
                "decompReconst: Signal is not detectable in noisy subband");
            }
            
            // main
            BL = denoi_BLS_GSM_band(BL, BLn, prnt);
            blx = BL.length;
            bly = BL[0].length;
            // Create BL transpose
            BLT = new double[bly * blx];
            for (j = 0; j < bly; j++) {
                for (i = 0; i < blx; i++) {
                    index = j + i*bly;
                    BLT[index] = BL[i][j][0];
                }
            }
            pyrh.setElementAt(BLT, nband-1);
            intMat = new int[2];
            intMat[0] = blx;
            intMat[1] = bly;
            pind.setElementAt(intMat, nband-1);
        } // for (nband = 2; nband <= bandNum; nband++)
        fh = reconFullSFpyr2(pyrh, pind, null, null, 1.0);
        if (error == 1) {
            return null;
        }
        return fh;
    }
    
    /**
     * Port of decomp_reconst_W.m by JPM, Univ. de Granada, 3/03
     * Decompose image into subbands, denoise using BLS-GSM method, and recompose again.
     * Version using a critically sampled pyramid (orthogonal wavelet), as 
     * implemented in MatlabPyrTools (Eero)
     * @param im
     * @param imx
     * @param imy
     * @param filter
     * @param noise
     * @param noisex
     * @param noisey
     * @return
     */
    private double[] decompReconstW(double im[], int imx, int imy, int filter, double noise[],
            int noisex, int noisey) {
            double fh[] = null;
            Vector pind = null;
            Vector pyr = null;
            Vector pyrN = null;
            Vector pyrh;
            int bandNum;
            int nband;
            double aux[] = null;
            double auxn[] = null;
            int nsx[] = new int[1];
            int nsy[] = new int[1];
            int nsxn[] = new int[1];
            int nsyn[] = new int[1];
            boolean prnt;
            double BL[][][];
            double BLn[][][];
            int i, j;
            int index;
            double imagArray[] = null;
            double sy2;
            double sn2;
            double SNRin;
            double BLT[];
            int blx;
            int bly;
            int intMat[];
            int fhx[] = null;
            int fhy[] = null;
            
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
            for (nband = 1; nband <= bandNum-1; nband++) {
                fireProgressStateChanged((100 * nband)/ (bandNum-1));
                aux = pyrBand(pyr, pind, nband-1,nsx, nsy);
                auxn = pyrBand(pyrN, pind, nband-1, nsxn, nsyn);
                // Has the subband a parent?
                prnt = useParent && (nband < bandNum - nOrientations);
                if (prnt) {
                    BL = new double[nsx[0]][nsy[0]][2];
                    BLn = new double[nsx[0]][nsy[0]][2];
                } // if (prnt)
                else {
                    BL = new double[nsx[0]][nsy[0]][1];
                    BLn = new double[nsx[0]][nsy[0]][1];
                } // else 
                for (j = 0; j < nsy[0]; j++) {
                    for (i = 0; i < nsx[0]; i++) {
                        index = i + j * nsx[0];
                        BL[i][j][0] = aux[index];
                        BLn[i][j][0] = auxn[index];
                    }
                }
                if (prnt) {
                    aux = pyrBand(pyr, pind, nband+nOrientations-1, nsxn, nsyn);
                    expand(aux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                    auxn = pyrBand(pyrN, pind, nband+nOrientations-1, nsxn, nsyn);
                    expand(auxn, 2.0, nsxn[0], nsyn[0], auxn, imagArray);
                    for (j = 0; j < nsy[0]; j++) {
                        for (i = 0; i < nsx[0]; i++) {
                            index = i + j * nsx[0];
                            BL[i][j][1] = aux[index];
                            BLn[i][j][1] = auxn[index];
                        }
                    }
                } // if (prnt)
                
                sy2 = 0.0;
                sn2 = 0.0;
                for (j = 0; j < nsy[0]; j++) {
                    for (i = 0; i < nsx[0]; i++) {
                        sy2 = sy2 + BL[i][j][0]*BL[i][j][0];
                        sn2 = sn2 + BLn[i][j][0]*BLn[i][j][0];
                    }
                }
                sy2 = sy2/(nsx[0]*nsy[0]);
                sn2 = sn2/(nsx[0]*nsy[0]);
                if (sy2 > sn2) {
                    SNRin = 4.342944819*Math.log((sy2-sn2)/sn2);
                }
                else {
                    Preferences.debug(
                    "decompReconst: Signal is not detectable in noisy subband");
                }
                
                // main
                BL = denoi_BLS_GSM_band(BL, BLn, prnt);
                blx = BL.length;
                bly = BL[0].length;
                // Create BL transpose
                BLT = new double[bly * blx];
                for (j = 0; j < bly; j++) {
                    for (i = 0; i < blx; i++) {
                        index = j + i*bly;
                        BLT[index] = BL[i][j][0];
                    }
                }
                pyrh.setElementAt(BLT, nband-1);
                intMat = new int[2];
                intMat[0] = blx;
                intMat[1] = bly;
                pind.setElementAt(intMat, nband-1);
            } // for (nband = 1; nband <= bandNum-1; nband++)
            fh = reconWpyr(pyrh, pind, filter, CIRCULAR, null, null, fhx, fhy);
            if (error == 1) {
                return null;
            }
            return fh;
    }
    
    /** 
     * This is a port of decomp_reconst_WU.m by Javier.Portilla, Univ. de Granada, 11/04
     * Decompose image into subbands (undecimated wavelet), denoise again, and
     * recompose again.
     * @param im  image
     * @param imx  First dimension of image
     * @param imy  Second dimension of image
     * @param daubOrder  Order of the daubechie function used (must be even)
     * @param noise  image having the same autocorrelation as the noise
     *               (e.g., a delta for white noise)
     * @param noisex  First dimension of noise
     * @param noisey  Second dimension of noise
     * @return
     */
    private double[] decompReconstWU(double im[], int imx, int imy, int daubOrder,
                                     double noise[], int noisex, int noisey) {
        double fh[] = null;
        Vector pind = null;
        Vector pyr = null;
        Vector pyrN = null;
        Vector pyrh;
        int bandNum;
        int nband;
        double aux[] = null;
        double auxn[] = null;
        int nsx[] = new int[1];
        int nsy[] = new int[1];
        int nsxn[] = new int[1];
        int nsyn[] = new int[1];
        int nsxnn[] = new int[1];
        int nsynn[] = new int[1];
        boolean prnt;
        double BL[][][];
        double BLn[][][];
        double var;
        int i, j;
        int index;
        double imagArray[] = null;
        double sy2;
        double sn2;
        double SNRin;
        double BLT[];
        int blx;
        int bly;
        int intMat[];
        
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
        pyrh = pyr;
        bandNum = pind.size() - 1;
        
        // Everything except the lowpass residual
        for (nband = 2; nband <= bandNum; nband++) {
            fireProgressStateChanged((100 * (nband-1))/ (bandNum-1));
            aux = pyrBand(pyr, pind, nband-1,nsx, nsy);
            auxn = pyrBand(pyrN, pind, nband-1, nsxn, nsyn);
            // Has the subband a parent?
            prnt = useParent && (nband < bandNum - nOrientations);
            if (prnt) {
                BL = new double[nsx[0]][nsy[0]][2];
                BLn = new double[nsx[0]][nsy[0]][2];
            } // if (prnt)
            else {
                BL = new double[nsx[0]][nsy[0]][1];
                BLn = new double[nsx[0]][nsy[0]][1];
            } // else 
            // Because we are discarding 2 coefficients on every dimension
            var = Math.sqrt(((nsx[0]-2)*(nsy[0]-2))/(nsx[0]*nsy[0]));
            for (j = 0; j < nsy[0]; j++) {
                for (i = 0; i < nsx[0]; i++) {
                    index = i + j * nsx[0];
                    BL[i][j][0] = aux[index];
                    BLn[i][j][0] = auxn[index]*var;
                }
            }
            if (prnt) {
                aux = pyrBand(pyr, pind, nband+nOrientations-1, nsxn, nsyn);
                auxn = pyrBand(pyrN, pind, nband+nOrientations-1, nsxnn, nsynn);
                // Resample 2x2 the parent if not in the high-pass oriented subbands
                if (nband > nOrientations + 1) {
                    expand(aux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                    expand(auxn, 2.0, nsxnn[0], nsynn[0], auxn, imagArray);    
                }
                for (j = 0; j < nsy[0]; j++) {
                    for (i = 0; i < nsx[0]; i++) {
                        index = i + j * nsx[0];
                        BL[i][j][1] = aux[index];
                        BLn[i][j][1] = auxn[index]*var;
                    }
                }
            } // if (prnt)
            
            sy2 = 0.0;
            sn2 = 0.0;
            for (j = 0; j < nsy[0]; j++) {
                for (i = 0; i < nsx[0]; i++) {
                    sy2 = sy2 + BL[i][j][0]*BL[i][j][0];
                    sn2 = sn2 + BLn[i][j][0]*BLn[i][j][0];
                }
            }
            sy2 = sy2/(nsx[0]*nsy[0]);
            sn2 = sn2/(nsx[0]*nsy[0]);
            if (sy2 > sn2) {
                SNRin = 4.342944819*Math.log((sy2-sn2)/sn2);
            }
            else {
                Preferences.debug(
                "decompReconst: Signal is not detectable in noisy subband");
            }
            
            // main
            BL = denoi_BLS_GSM_band(BL, BLn, prnt);
            blx = BL.length;
            bly = BL[0].length;
            // Create BL transpose
            BLT = new double[bly * blx];
            for (j = 0; j < bly; j++) {
                for (i = 0; i < blx; i++) {
                    index = j + i*bly;
                    BLT[index] = BL[i][j][0];
                }
            }
            pyrh.setElementAt(BLT, nband-1);
            intMat = new int[2];
            intMat[0] = blx;
            intMat[1] = bly;
            pind.setElementAt(intMat, nband-1);
        } // for (nband = 2; nband <= bandNum; nband++)
        fh = reconWUpyr(pyrh, pind, daubOrder);
        if (error == 1) {
            return null;
        }
        return fh;
    }
    
    /**
     * Decompose image into subbands, denoise, and recompose again
     * Port of routine written by Javier Portilla
     *
     * @param fn
     * @param fnx x dimension of fn
     * @param fny y dimension of fn
     * @param noise
     * @param noisex x dimension of noise
     * @param noisey y dimension of noise
     * @return
     */
    private double[] decompReconst(double fn[], int fnx, int fny, double noise[],
                                  int noisex, int noisey) {
        double fh[] = null;
        Vector pind = null;
        Vector pyr = null;
        Vector pyrN = null;
        Vector pyrh;
        int bandNum;
        int nband;
        double aux[] = null;
        double auxn[] = null;
        int nsx[] = new int[1];
        int nsy[] = new int[1];
        int nsxn[] = new int[1];
        int nsyn[] = new int[1];
        boolean prnt;
        double BL[][][];
        double BLn[][][];
        double var;
        int i, j;
        int index;
        double imagArray[] = null;
        double sy2;
        double sn2;
        double SNRin;
        double BLT[];
        int blx;
        int bly;
        int intMat[];
        
        pyr = buildSFpyr(fn, fnx, fny, nScales, nOrientations-1, 1.0, pind);
        if (error == 1) {
            return null;
        }
        pyrN = buildSFpyr(noise, noisex, noisey, nScales, nOrientations-1, 1.0, pind);
        if (error == 1) {
            return null;
        }
        pyrh = pyr;
        bandNum = pind.size();
        for (nband = 1; nband <= bandNum-1; nband++) {
            fireProgressStateChanged((100 * nband)/ (bandNum-1));
            aux = pyrBand(pyr, pind, nband-1,nsx, nsy);
            auxn = pyrBand(pyrN, pind, nband-1, nsxn, nsyn);
            prnt = useParent && (nband < bandNum - 1 - nOrientations) &&
                   (nband > 1);
            if (prnt) {
                BL = new double[nsx[0]][nsy[0]][2];
                BLn = new double[nsx[0]][nsy[0]][2];
            } // if (prnt)
            else {
                BL = new double[nsx[0]][nsy[0]][1];
                BLn = new double[nsx[0]][nsy[0]][1];
            } // else 
            // Because we are discarding 2 coefficients on every dimension
            var = Math.sqrt(((nsx[0]-2)*(nsy[0]-2))/(nsx[0]*nsy[0]));
            for (j = 0; j < nsy[0]; j++) {
                for (i = 0; i < nsx[0]; i++) {
                    index = i + j * nsx[0];
                    BL[i][j][0] = aux[index];
                    BLn[i][j][0] = auxn[index]*var;
                }
            }
            if (prnt) {
                aux = pyrBand(pyr, pind, nband+nOrientations-1, nsxn, nsyn);
                expand(aux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
                auxn = pyrBand(pyrN, pind, nband+nOrientations-1, nsxn, nsyn);
                expand(auxn, 2.0, nsxn[0], nsyn[0], auxn, imagArray);
                for (j = 0; j < nsy[0]; j++) {
                    for (i = 0; i < nsx[0]; i++) {
                        index = i + j * nsx[0];
                        BL[i][j][1] = aux[index];
                        BLn[i][j][1] = auxn[index]*var;
                    }
                }
            } // if (prnt)
            
            sy2 = 0.0;
            sn2 = 0.0;
            for (j = 0; j < nsy[0]; j++) {
                for (i = 0; i < nsx[0]; i++) {
                    sy2 = sy2 + BL[i][j][0]*BL[i][j][0];
                    sn2 = sn2 + BLn[i][j][0]*BLn[i][j][0];
                }
            }
            sy2 = sy2/(nsx[0]*nsy[0]);
            sn2 = sn2/(nsx[0]*nsy[0]);
            if (sy2 > sn2) {
                SNRin = 4.342944819*Math.log((sy2-sn2)/sn2);
            }
            else {
                Preferences.debug(
                "decompReconst: Signal is not detectable in noisy subband");
            }
            
            // main
            BL = denoi_BLS_GSM_band(BL, BLn, prnt);
            blx = BL.length;
            bly = BL[0].length;
            // Create BL transpose
            BLT = new double[bly * blx];
            for (j = 0; j < bly; j++) {
                for (i = 0; i < blx; i++) {
                    index = j + i*bly;
                    BLT[index] = BL[i][j][0];
                }
            }
            pyrh.setElementAt(BLT, nband-1);
            intMat = new int[2];
            intMat[0] = blx;
            intMat[1] = bly;
            pind.setElementAt(intMat, nband-1);
        } // for (nband = 1; nband <= bandNum-1; nband++)
        fh = reconSFpyr(pyrh, pind, null, null, 1.0);
        if (error == 1) {
            return null;
        }
        return fh;
    }
    
    /**
     * This is a port of reconFullSFpyr2.m
     * Reconstruct image from its steerable pyramid representation, in the
     * Fourier domain, as created by buildSFpyr.
     * Unlike the standard transform, subdivides the highpass band into
     * orientations
     * @param pyr
     * @param pind
     * @param levs
     * @param bands
     * @param twidth
     * @return
     */
    private double[] reconFullSFpyr2(Vector pyr, Vector pind, int levs[],
            int bands[], double twidth) {
        double res[] = null;
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
        int intArr[][];
        int intMat[];
        double xramp[][];
        double yramp[][];
        double var;
        double angle[][];
        double log_rad[][];
        double xrcos[] = null;;
        double yrcos[] = null;
        double yircos[];
        boolean haveOne;
        double resdftr[] = null;;
        double resdfti[] = null;
        FFTUtility fftUtil;
        double lo0mask[][];
        int index;
        boolean haveZero;
        double hi0mask[][];
        double arr[][];
        int lutsize;
        double xcosn[];
        int order;
        double consta;
        double ycosn[];
        int b;
        boolean haveB;
        double anglemask[][];
        double bandr[];
        double bandi[];
        double maskr[][];
        double maski[][];
        int x[] = null;
        int y[] = null;
        
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
         
        nbands = spyrNumBands(pind)/2;
        
        intArr = new int[nbands][];
        for (i = 0; i < nbands; i++) {
            intArr[i] = (int[])pind.remove(0);
        }
        maxLev = 1 + spyrHt(pind);
        for (i = nbands-1; i >= 0; i--) {
            pind.insertElementAt(intArr[i], 0);
        }
        
        if (allLevs) {
            levs = new int[maxLev+1];
            for (i = 0; i <= maxLev; i++) {
                levs[i] = i;
            }
        } // if (allLevs)
        else {
            for (i = 0; i < levs.length; i++) {
                if ((levs[i] > maxLev) || (levs[i] < 0)) {
                    MipavUtil.displayError(
                    "Level numbers must be in the range 0 to " + maxLev);
                    error = 1;
                    return null;
                }
            }
        } //  else 
         
        if (allBands) {
            bands = new int[nbands];
            for (i = 1; i <= nbands; i++) {
                bands[i] = i;
            }
        } // if (allBands)
        else {
            for (i = 0; i < bands.length; i++) {
                if ((levs[i] < 1) || (levs[i] > nbands)) {
                    MipavUtil.displayError(
                    "Band numbers must be in the range 1 to " + nbands);
                    error = 1;
                    return null;
                }
            }
        } // else
         
        intMat = (int[])pind.get(1);
        dimX = intMat[0];
        dimY = intMat[1];
        ctrX = (int)Math.ceil((dimX + 0.5)/2);
        ctrY = (int)Math.ceil((dimY + 0.5)/2);
        
        xramp = new double[dimX][dimY];
        yramp = new double[dimX][dimY];
        for (j = 0; j < dimY; j++) {
            var = ((j+1.0)-ctrY)/(dimY/2.0);
            for (i = 0; i < dimX; i++) {
                xramp[i][j] = var;
            }
        }
        for (i = 0; i < dimX; i++) {
            var = ((i+1.0)-ctrX)/(dimX/2.0);
            for (j = 0; j < dimY; j++) {
                yramp[i][j]= var;
            }
        }
        angle = new double[dimX][dimY];
        log_rad = new double[dimX][dimY];
        for (j = 0; j < dimY; j++) {
            for (i = 0; i < dimX; i++) {
                angle[i][j] = Math.atan2(yramp[i][j],xramp[i][j]);
                log_rad[i][j] = Math.sqrt(xramp[i][j]*xramp[i][j] + yramp[i][j]*yramp[i][j]);
            }
        }
        log_rad[ctrX-1][ctrY-1] = log_rad[ctrX-1][ctrY-2];
        // log2(x) = loge(x)/loge(2)
        var = 1.0/Math.log(2.0);
        for (j = 0; j < dimY; j++) {
            for (i = 0; i < dimX; i++) {
                log_rad[i][j] = var * Math.log(log_rad[i][j]);
            }
        }
        
        // Radial transition function (a raised cosine in log-frequency)
        rcosFn(twidth, (-twidth/2.0), 0.0, 1.0, xrcos, yrcos);
        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }
        yircos = new double[yrcos.length];
        for (i = 0; i < yrcos.length; i++) {
            yircos[i] = Math.sqrt(1.0 - yrcos[i]*yrcos[i]);
        }
        
        if (pind.size() == 2) {
            haveOne = false;
            for (i = 0; i < levs.length; i++) {
                if (levs[i] == 1) {
                    haveOne = true;
                }
            }
            if (haveOne) {
                resdftr = pyrBand(pyr, pind, 1, x, y);
                resdfti = new double[x[0]*y[0]];
                // forward FFT
                fftUtil = new FFTUtility(resdftr, resdfti, y[0], x[0], 1, -1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(resdftr, resdfti, 1, y[0], x[0], -1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                center(resdftr, resdfti, x[0], y[0]);
            }
            else {
                intMat = (int[])pind.get(1);
                x[0] = intMat[0];
                y[0] = intMat[1];
                resdftr = new double[x[0]*y[0]];
                resdfti = new double[x[0]*y[0]];
            }
        } // if (pind.size() == 2)
        else {
            arr = new double[nbands+1][];
            intArr = new int[nbands+1][];
            for (i = 0; i < nbands+1; i++) {
                arr[i] = (double[])pyr.remove(0);
                intArr[i] = (int[])pind.remove(0);
            }
            intMat = (int [])pind.get(0);
            x[0] = intMat[0];
            y[0] = intMat[1];
            reconSFpyrLevs(pyr, pind, log_rad, xrcos, yrcos, angle, nbands, 
                           levs, bands, resdftr, resdfti);
            for (i = nbands; i >= 0; i--) {
                pyr.insertElementAt(arr[i], 0);
                pind.insertElementAt(intArr[i], 0);
            }
        } // else
        
        lo0mask = pointOp(log_rad, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);
        for (j = 0; j < dimY; j++) {
            for (i = 0; i < dimX; i++) {
                index = i + j*dimX;
                resdftr[index] = resdftr[index] * lo0mask[i][j];
                resdfti[index] = resdfti[index] * lo0mask[i][j];
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
            xcosn = new double[3*(lutsize+1)];
            // -2*PI to PI
            for (i = 0, j = -(2*lutsize+1); i < xcosn.length; i++, j++) {
                xcosn[i] = j * Math.PI/lutsize;
            }
            order = nbands-1;
            // Divide by sqrt(sum_(n=0)^(N-1) cos(pi*n/N)^(2(N-1)) )
            var = factorial(order);
            consta = Math.pow(2.0,(2.0*order))*var*var/(nbands*factorial(2*order));
            consta = Math.sqrt(consta);
            ycosn = new double[xcosn.length];
            for (i = 0; i < xcosn.length; i++) {
                ycosn[i] = consta * Math.pow(xcosn[i], order);
            }
            hi0mask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1]-xrcos[0]), false);
            
            index = 0;
            for (b = 1; b <= nbands; b++) {
                haveB = false;
                for (i = 0; i < bands.length; b++) {
                    if (bands[i] == b) {
                        haveB = true;
                    }
                }
                if (haveB) {
                    anglemask = pointOp(angle, ycosn, (xcosn[0] + Math.PI*(b-1)/nbands),
                                        (xcosn[1] - xcosn[0]), true);
                    bandr = (double[])pyr.get(index);
                    bandi = new double[dimX * dimY];
                    // forward FFT
                    fftUtil = new FFTUtility(bandr, bandi, dimY, dimX, 1, -1, FFTUtility.FFT);
                    fftUtil.setProgressBarVisible(false);
                    fftUtil.run();
                    fftUtil.finalize();
                    fftUtil = new FFTUtility(bandr, bandi, 1, dimY, dimX, -1, FFTUtility.FFT);
                    fftUtil.setProgressBarVisible(false);
                    fftUtil.run();
                    fftUtil.finalize();
                    center(bandr, bandi, dimX, dimY);
                    // Make real the contents in the HF cross (to avoid information loss in these freqs.)
                    // It distributes evenly these contents among the nbands orientations
                    maskr = new double[dimX][dimY];
                    maski = new double[dimX][dimY];
                    if (((nbands-1) % 4) == 0) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                maskr[i][j] = anglemask[i][j]*hi0mask[i][j];
                            }
                        }
                    } // if (((nbands-1) % 4) == 0)
                    else if (((nbands-1) %4) == 1) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                maski[i][j] = anglemask[i][j]*hi0mask[i][j];
                            }
                        }    
                    } // else if (((nbands-1) %4) == 1)
                    else if (((nbands-1) %4) == 2) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                maskr[i][j] = -anglemask[i][j]*hi0mask[i][j];
                            }
                        }    
                    } // else if (((nbands-1) %4) == 2)
                    else if (((nbands-1) %4) == 3) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                maski[i][j] = -anglemask[i][j]*hi0mask[i][j];
                            }
                        }    
                    } // else if (((nbands-1) %4) == 3)
                    
                    var = 1.0/Math.sqrt(nbands);
                    for (j = 0; j < dimY; j++) {
                        maskr[0][j] = var;
                    }
                    for (i = 1; i < dimX; i++) {
                        maskr[i][0] = var;
                    }
                    
                    for (j = 0; j < dimY; j++) {
                        for (i = 0; i < dimX; i++) {
                            index = i + j * dimX;
                            resdftr[index] = resdftr[index] + bandr[index] * maskr[i][j] - bandi[index] * maski[i][j];
                            resdfti[index] = resdfti[index] + bandr[index] * maski[i][j] + bandi[index] * maskr[i][j];
                        }
                    }
                } // if (haveB)
                index++;
            } // for (b = 1; b <= nbands; b++)    
        } // if (haveZero)
        
        center(resdftr, resdfti, x[0], y[0]);
        // Inverse FFT
        fftUtil = new FFTUtility(resdftr, resdfti, y[0], x[0], 1, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(resdftr, resdfti, 1, y[0], x[0], +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        return resdftr;
    }
    
    /**
     * This is a port of wpyrHt.m  by Eero Simoncelli, 6/96.
     * Compute height of separable QMF/wavelet pyramid with given index matrix
     * @param pind
     * @return
     */
    private int wpyrHt(Vector pind) {
        int intMat[];
        int nbands;
        int ht;
        
        intMat = (int[])pind.get(0);
        if ((intMat[0] == 1) || (intMat[1] == 1)) {
            nbands = 1;
        }
        else {
            nbands = 3;
        }
        ht = (pind.size()-1)/nbands;
        return ht;
    }
    
    /**
     * This is ported from upConv.c by Eero Simoncelli, 7/96.
     * Upsample matrix image, followed by convolution with matrix filt.  These
     * arguments should be 1D or 2D matrices, and image must be larger (in
     * both dimensions) than filt.  The origin of filt is assumed to be
     * floor(size(filt)/2)+1.
     * @param image
     * @param xdim
     * @param ydim
     * @param filt
     * @param xfdim
     * @param yfdim
     * @param edges Determines boundary handling
     *              CIRCULAR - Circular convolution
     *              REFLECT1 - Reflect about the edge pixels
     *              REFLECT2 - Reflect, doubling the edge pixels
     *              REPEAT - Repeat the edge pixels
     *              ZERO - Assume values of zero outside image boundary
     *              EXTEND - Reflect and invert
     *              DONT_COMPUTE - Zero output when filter overhangs output boundaries
     * Upsampliing factors are determined by xstep and ystep
     * @param xstep
     * @param ystep
     * The window over which the convolution occurs is specified by
     * xstart, ystart, xstop, and ystop.
     * @param xstart
     * @param ystart
     * @param xstop
     * @param ystop
     * This operation corresponds to multiplication of a signal vector by a 
     * matrix whose columns contains copies of the time-reversed (or space-
     * reversed) filt shifted by multiple copies of step.  See corrDn for
     * the operation corresponding to the transpose of this matrix.
     * @param result
     * @param xrdim First dimension of result
     * @param yrdim Second dimension of result
     */
    private void upConv(double image[], int xdim, int ydim, double filt[],
                            int xfdim, int yfdim, int edges, int xstep, int ystep,
                            int xstart, int ystart, int xstop, int ystop,
                            double result[], int xrdim, int yrdim) {
        double origFilt[];
        int origx;
        int origy;
        int x;
        int y;
        double temp[];
        
        if (result == null) {
            result = new double[xrdim * yrdim];
        }
        
        xstart--;
        ystart--;
        
        // upConv has a bug for even length kernels when using the REFLECT1,
        // EXTEND, or REPEAT edge handlers
        if (((edges == REFLECT1) || (edges == EXTEND) || (edges == REPEAT)) &&
            ((xfdim%2 == 0) || (yfdim %2 == 0))) {
            origFilt = filt;
            origx = xfdim;
            origy = yfdim;
            xfdim = 2*(origx/2) + 1;
            yfdim = 2*(origy/2) + 1;
            filt = new double[xfdim * yfdim];
            for (y = 0; y < origy; y++) {
                for (x = 0; x < origx; x++) {
                    filt[y*xfdim + x] = origFilt[y*origx + x];
                }
            }
        }
        
        if (edges == CIRCULAR) {
            internal_wrap_expand(image, filt, xfdim, yfdim,
                    xstart, xstep, xstop, ystart, ystep, ystop,
                    result, xrdim, yrdim);    
        }
        else {
            temp = new double[xfdim * yfdim]; 
            internal_expand(image, filt, temp, xfdim, yfdim,
                            xstart, xstep, xstop, ystart, ystep, ystop,
                            result, xrdim, yrdim, edges);
        }
        
        return;
    }
    
    /**
     * Performs upsampling (padding with zeroes) followed by convolution of
     * filt with image (a.k.a. expand in Burt&Adelson81).  The operations
     * are combined to avoid unnecessary multiplication of the filter samples
     * with zeros in the unsampled image.  The convolution is done in 9
     * sections so that the mod operation is not performed unnecessarily.
     * @param image
     * @param filt
     * @param xfdim
     * @param yfdim
     * @param xstart
     * @param xstep
     * @param xstop
     * @param ystart
     * @param ystep
     * @param ystop
     * @param result
     * @param xdim
     * @param ydim
     */
    private void internal_wrap_expand(double image[], double filt[], int xfdim, int yfdim,
                                      int xstart, int xstep, int xstop, int ystart, int ystep, int ystop,
                                      double result[], int xdim, int ydim) {
        int filtSize = xfdim * yfdim;
        int xCtrStop = xdim - xfdim + 1;
        int yCtrStop = ydim - yfdim + 1;
        int xCtrStart = 0;
        int yCtrStart = 0;
        int xfmid = xfdim/2;
        int yfmid = yfdim/2;
        double imval[][];
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
        
        // Top rows
        for (impos = 0, ypos = ystart; ypos < yCtrStart; ypos += ystep) {
            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos + ydim, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos + xdim; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres%ydim][xres%xdim] += val * filt[filtPos];
                    }
                }
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++)
            
            for (; xpos < xCtrStop; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos + ydim, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres%ydim][xres] += val * filt[filtPos];
                    }
                }    
            } // for (; xpos < xCtrStop; xpos += xstep, impos++)
            
            for (; xpos < xstop; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos + ydim, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres%ydim][xres%xdim] += val * filt[filtPos];
                    }
                }    
            } // for (; xpos < xstop; xpos += xstep, impos++)
        } // for (impos = 0, ypos = ystart; ypos < yCtrStart; ypos += ystep)
        
        // Mid rows
        for (; ypos < yCtrStop; ypos += ystep) {
            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos + xdim; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres][xres%xdim] += val * filt[filtPos];
                    }
                }        
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++)
            
            // Center section
            for (; xpos < xCtrStop; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres][xres] += val * filt[filtPos];
                    }
                }    
            } // for (; xpos < xCtrStop; xpos += xstep, impos++)
            
            for (; xpos < xstop; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres][xres%xdim] += val * filt[filtPos];
                    }
                }    
            } // for (; xpos < xstop; xpos += xstep, impos++)
        } // for (; ypos < yCtrStop; ypos += ystep)
        
        // Bottom rows
        for (; ypos < ystop; ypos += ystep) {
            for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos + xdim; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres%ydim][xres%xdim] += val * filt[filtPos];
                    }
                }    
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, impos++)
            
            for (; xpos < xCtrStop; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres%ydim][xres] += val * filt[filtPos];
                    }
                }    
            } // for (; xpos < xCtrStop; xpos += xstep, impos++)
            
            for (; xpos < xstop; xpos += xstep, impos++) {
                val = image[impos]; 
                for (yres = ypos, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yres++, xFiltStop += xfdim) {
                    for (xres = xpos; filtPos < xFiltStop; filtPos++, xres++) {
                        imval[yres%ydim][xres%xdim] += val * filt[filtPos];
                    }
                }    
            } // for (; xpos < xstop; xpos += xstep, impos++)
        } // for (; ypos < ystop; ypos += ystep)
        
        return;
    }
    
    private void internal_expand(double image[], double filt[], double temp[], int xfdim, int yfdim,
                                 int xstart, int xstep, int xstop, int ystart, int ystep, int ystop,
                                 double result[], int xdim, int ydim, int edges) {
        // dcompReconstW calls recnWpyr with CIRCULAR so internal_expand is not called    
    }
    
    /**
     * This is a port of reconWpyr.m by Eero Simoncelli, 6/96
     * Reconstruct image from its separable orthonormal QMF/wavelet pyramid
     * representation, as created by buildWpyr.
     * @param pyr  A Vector containing the N pyramid subbands, ordered from fine
     *             to coarse.
     * @param ind A Vector with N 2 element int[] containing the sizes of each
     *            subband
     * @param filt A standard filter
     * @param edges Specifies edge handling
     * @param levs Levels to include or all for default.  1 corresponds to the
     *             finest scale.  The lowpass band corresponds to wpyrHt(indices) + 1.
     * @param bands Bands to include or all for default. 1 = horizontal, 
     *              2 = vertical, 3 = diagonal.
     * @param resX First dimension of res
     * @param resY Second dimension of res
     * @return
     */
    private double[] reconWpyr(Vector pyr, Vector ind, int filt,
                               int edges, int levs[], int bands[],
                               int resX[], int resY[]) {
        double res[] = null;
        boolean allLevs = false;
        boolean allBands = false;
        int maxLev;
        int i;
        double kernel[];
        double hkernel[];
        int stag;
        int res_sz[];
        int loind;
        int intMat[];
        int intMat2[];
        int hres_sz[] = null;
        int lres_sz[] = null;
        boolean moreOne;
        double nres[];
        int nresx[] = new int[1];
        int nresy[] = new int[1];
        double arr[][];
        int parr[][];
        int levsm1[];
        double ires[] = null;
        boolean anyOne;
        int x[] = null;
        int y[] = null;
        boolean anyB1;
        boolean anyB2;
        boolean anyB3;
        
        if (resX == null) {
            resX = new int[1];
        }
        
        if (resY == null) {
            resY = new int[1];
        }
        
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
                levs[i] = i+1;
            }
        }
        else {
            for (i = 0; i < levs.length; i++) {
                if (levs[i] > maxLev) {
                    MipavUtil.displayError("Level numbers must be in the range 1 to "
                                            + maxLev);
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
        }
        else {
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
        }
        else {
            stag = 1;
        }
        
        // Compute size of result image: assumes critical sampling (boundaries correct)
        res_sz = (int[])ind.get(0);
        if (res_sz[0] == 1) {
            loind = 2;
            res_sz[1] = 0;
            for (i = 0; i < ind.size(); i++) {
                intMat = (int[])ind.get(i);
                res_sz[1] += intMat[1];
            }
        } // if (res_sz[0] == 1)
        else if (res_sz[1] == 1) {
            loind = 2;
            res_sz[0] = 0;
            for (i = 0; i < ind.size(); i++) {
                intMat = (int[])ind.get(i);
                res_sz[0] += intMat[0];
            }
        } // else if (res_sz[1] == 1)
        else {
            loind = 4;
            // Horizontal + vertical bands
            intMat = (int[])ind.get(0);
            intMat2 = (int[])ind.get(1);
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
                arr = new double[loind-1][];
                parr = new int[(loind-1)][2];
                for (i = 0; i < loind-1; i++) {
                    arr[i] = (double[])pyr.remove(0);
                }
                for (i = 0; i < loind-1; i++) {
                    parr[i] = (int[])ind.remove(0);
                }
                levsm1 = new int[levs.length];
                for (i = 0; i < levs.length; i++) {
                    levsm1[i] = levs[i] - 1;
                }
                nres = reconWpyr(pyr, ind, filt, edges, levsm1, bands, nresx, nresy);
                for (i = loind-2; i >= 0; i--) {
                    pyr.insertElementAt(arr[i], 0);
                    ind.insertElementAt(parr[i], 0);
                }
            } // if (ind.size() > loind)
            else {
                // lowpass subband
                nres = pyrBand(pyr, ind, loind-1, nresx, nresy);
            }
            
            if (res_sz[0] == 1) {
                res = null;
                upConv(nres, nresx[0], nresy[0], kernel, 1, kernel.length,
                             edges, 1, 2, 1, stag, res_sz[0], res_sz[1],
                             res, res_sz[0], res_sz[1]);    
            }
            else if (res_sz[1] == 1) {
                res = null;
                upConv(nres, nresx[0], nresy[0], kernel, kernel.length, 1,
                             edges, 2, 1, stag, 1, res_sz[0], res_sz[1],
                             res, res_sz[0], res_sz[1]);    
            }
            else {
                ires = null;
                upConv(nres, nresx[0], nresy[0], kernel, 1, kernel.length,
                              edges, 1, 2, 1, stag, lres_sz[0], lres_sz[1],
                              ires, lres_sz[0], lres_sz[1]); 
                res = null;
                upConv(ires, lres_sz[0], lres_sz[1], kernel, kernel.length, 1,
                             edges, 2, 1, stag, 1, res_sz[0], res_sz[1],
                             res, res_sz[0], res_sz[1]);
            }
        } // if (moreOne)
        else {
            res = new double[res_sz[0] * res_sz[1]];   
        }
        
        // Add in reconsructed bands from this level
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
                }
                else if (bands[i] == 2) {
                    anyB2 = true;
                }
                else if (bands[i] == 3) {
                    anyB3 = true;
                }
            }
            if (res_sz[0] == 1) {
                upConv(pyrBand(pyr, ind, 0, x, y), x[0], y[0], 
                       hkernel, 1, hkernel.length, edges, 1, 2, 1, 2, 
                       res_sz[0], res_sz[1], res, res_sz[0], res_sz[1]);
            }
            else if (res_sz[1] == 1) {
                upConv(pyrBand(pyr, ind, 0, x, y), x[0], y[0],
                       hkernel, hkernel.length, 1, edges, 2, 1, 2, 1,
                       res_sz[0], res_sz[1], res, res_sz[0], res_sz[1]);    
            }
            else {
                if (anyB1) { // horizontal
                    ires = null;
                    upConv(pyrBand(pyr, ind, 0, x, y), x[0], y[0],
                           kernel, 1, kernel.length, edges, 1, 2, 1, stag,
                           hres_sz[0], hres_sz[1], ires, hres_sz[0], hres_sz[1]);
                    // Destructively modify res
                    upConv(ires, hres_sz[0], hres_sz[1], hkernel, hkernel.length, 1,
                           edges, 2, 1, 2, 1, res_sz[0], res_sz[1], res, res_sz[0], res_sz[1]);
                } // 
                if (anyB2) { // vertical
                    ires = null;
                    upConv(pyrBand(pyr, ind, 1, x, y), x[0], y[0],
                           hkernel, 1, hkernel.length, edges, 1, 2, 1, 2,
                           lres_sz[0], lres_sz[1], ires, lres_sz[0], lres_sz[1]);
                    // Destructively modify res
                    upConv(ires, lres_sz[0], lres_sz[1], kernel, kernel.length, 1,
                           edges, 2, 1, stag, 1, res_sz[0], res_sz[1], res, res_sz[0], res_sz[1]);    
                } // if (anyB2) 
                if (anyB3) { // diagonal
                    ires = null;
                    upConv(pyrBand(pyr, ind, 2, x, y), x[0], y[0],
                           hkernel, 1, hkernel.length, edges, 1, 2, 1, 2,
                           hres_sz[0], hres_sz[1], ires, hres_sz[0], hres_sz[1]);
                    // Destructively modify res
                    upConv(ires, hres_sz[0], hres_sz[1], hkernel, hkernel.length, 1,
                           edges, 2, 1, 2, 1, res_sz[0], res_sz[1], res, res_sz[0], res_sz[1]);    
                } // if (anyB3) 
            } // else
        } // if (anyOne)
        return res;
    }
    
    /**
     * 
     * @param pyr
     * @param pind
     * @param daubOrder
     * @return
     */
    private double[] reconWUpyr(Vector pyr, Vector pind, int daubOrder) {
        double res[] = null;
        return res;
    }
    
    /**
     * This is a port of reconSFpyr.m by Eero Simoncelli, 5/97.
     * Reconstruct image from its steerable pyramid representation, in the
     * Fourier domain, as created by buildSFpyr.
     * @param pyr  A vector containing the N pyramid subbands, ordered from
     *             fine to coarse
     * @param pind     A vector with a NX2 Integer set of the sizes of each
     *                 subband
     * @param levs   Optional.  A list of the levels to include or null
     *               for all levels.  0 corresponds to the residual highpass
     *               subband.  1 corresponds to the finest oriented scale.
     *               The lowpass band corresponds to number spyrHt(indices+1)
     * @param band  Optional.  Should be a list of the bands to include or
     *              null for all bands.  1 = vertical, rest proceeding anti-
     *              clockwise.
     * @param twidth  The width of the transition region of the radial lowpass
     *                function, in octaves.  Default = 1, which gives a raised
     *                cosine for the bandpass filters.
     * @return
     */
    private double[] reconSFpyr(Vector pyr, Vector pind, int levs[],
                                int bands[], double twidth) {
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
        double xramp[][];
        double yramp[][];
        double var;
        double angle[][];
        double log_rad[][];
        double xrcos[] = null;;
        double yrcos[] = null;
        double yircos[];
        boolean haveOne;
        int px[] = new int[1];
        int py[] = new int[1];
        double resdftr[] = null;;
        double resdfti[] = null;
        FFTUtility fftUtil;
        double lo0mask[][];
        int index;
        boolean haveZero;
        double hi0mask[][];
        double arr[];
        int x;
        int y;
        double imag[];
        int hix;
        int hiy;
        int intMat[];
        int intMat2[];
        
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
            levs = new int[maxLev+1];
            for (i = 0; i <= maxLev; i++) {
                levs[i] = i;
            }
        } // if (allLevs)
        else {
            for (i = 0; i < levs.length; i++) {
                if ((levs[i] > maxLev) || (levs[i] < 0)) {
                    MipavUtil.displayError(
                    "Level numbers must be in the range 0 to " + maxLev);
                    error = 1;
                    return null;
                }
            }
        } //  else 
         
        if (allBands) {
            bands = new int[nbands];
            for (i = 1; i <= nbands; i++) {
                bands[i] = i;
            }
        } // if (allBands)
        else {
            for (i = 0; i < bands.length; i++) {
                if ((levs[i] < 1) || (levs[i] > nbands)) {
                    MipavUtil.displayError(
                    "Band numbers must be in the range 1 to " + nbands);
                    error = 1;
                    return null;
                }
            }
        } // else
         
        intMat = (int[])pind.get(0);
        dimX = intMat[0];
        dimY = intMat[1];
        ctrX = (int)Math.ceil((dimX + 0.5)/2);
        ctrY = (int)Math.ceil((dimY + 0.5)/2);
        
        xramp = new double[dimX][dimY];
        yramp = new double[dimX][dimY];
        for (j = 0; j < dimY; j++) {
            var = ((j+1.0)-ctrY)/(dimY/2.0);
            for (i = 0; i < dimX; i++) {
                xramp[i][j] = var;
            }
        }
        for (i = 0; i < dimX; i++) {
            var = ((i+1.0)-ctrX)/(dimX/2.0);
            for (j = 0; j < dimY; j++) {
                yramp[i][j]= var;
            }
        }
        angle = new double[dimX][dimY];
        log_rad = new double[dimX][dimY];
        for (j = 0; j < dimY; j++) {
            for (i = 0; i < dimX; i++) {
                angle[i][j] = Math.atan2(yramp[i][j],xramp[i][j]);
                log_rad[i][j] = Math.sqrt(xramp[i][j]*xramp[i][j] + yramp[i][j]*yramp[i][j]);
            }
        }
        log_rad[ctrX-1][ctrY-1] = log_rad[ctrX-1][ctrY-2];
        // log2(x) = loge(x)/loge(2)
        var = 1.0/Math.log(2.0);
        for (j = 0; j < dimY; j++) {
            for (i = 0; i < dimX; i++) {
                log_rad[i][j] = var * Math.log(log_rad[i][j]);
            }
        }
        
        // Radial transition function (a raised cosine in log-frequency)
        rcosFn(twidth, (-twidth/2.0), 0.0, 1.0, xrcos, yrcos);
        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }
        yircos = new double[yrcos.length];
        for (i = 0; i < yrcos.length; i++) {
            yircos[i] = Math.sqrt(Math.abs(1.0 - yrcos[i]*yrcos[i]));
        }
        
        if (pind.size() == 2) {
            haveOne = false;
            for (i = 0; i < levs.length; i++) {
                if (levs[i] == 1) {
                    haveOne = true;
                }
            }
            if (haveOne) {
                resdftr = pyrBand(pyr, pind, 1, px, py);
                resdfti = new double[px[0]*py[0]];
                // forward FFT
                fftUtil = new FFTUtility(resdftr, resdfti, py[0], px[0], 1, -1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(resdftr, resdfti, 1, py[0], px[0], -1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                center(resdftr, resdfti, px[0], py[0]);
            }
            else {
                intMat = (int[])pind.get(1);
                px[0] = intMat[0];
                py[0] = intMat[1];
                resdftr = new double[px[0]*py[0]];
                resdfti = new double[px[0]*py[0]];
            }
        } // if (pind.size() == 2)
        else {
            arr = (double[])pyr.remove(0);
            intMat = (int[])pind.remove(0);
            intMat2 = (int[])pind.get(0);
            px[0] = intMat2[0];
            py[0] = intMat2[1];
            reconSFpyrLevs(pyr, pind, log_rad, xrcos, yrcos, angle, nbands, 
                           levs, bands, resdftr, resdfti);
            pyr.insertElementAt(arr, 0);
            pind.insertElementAt(intMat, 0);
        } // else
        
        lo0mask = pointOp(log_rad, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);
        for (j = 0; j < dimY; j++) {
            for (i = 0; i < dimX; i++) {
                index = i + j*dimX;
                resdftr[index] = resdftr[index] * lo0mask[i][j];
                resdfti[index] = resdfti[index] * lo0mask[i][j];
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
            arr = (double [])pyr.get(0);
            intMat = (int[])pind.get(0);
            x = intMat[0];
            y = intMat[1];
            imag = new double[x * y];
            // forward FFT
            fftUtil = new FFTUtility(arr, imag, y, x, 1, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(arr, imag, 1, y, x, -1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            center(arr, imag, x, y);
            hix = hi0mask.length;
            hiy = hi0mask[0].length;
            for (j = 0; j < hiy; j++) {
                for (i = 0; i < hix; i++) {
                    index = i + j * hix;
                    arr[index] = arr[index] * hi0mask[i][j];
                    imag[index] = imag[index] * hi0mask[i][j];
                    resdftr[index] = resdftr[index] + arr[index];
                    resdfti[index] = resdfti[index] + imag[index];
                }
            }
        } // if (haveZero)
        
        center(resdftr, resdfti, px[0], py[0]);
        // Inverse FFT
        fftUtil = new FFTUtility(resdftr, resdfti, py[0], px[0], 1, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(resdftr, resdfti, 1, py[0], px[0], +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        return resdftr;
    }
    
    /**
     * This is a port of reconSFpyrLevs.m written by Eero Simoncelli, 5/97.
     * Recursive function for constructing levels of a steerable pyramid representation.
     * This is called by reconSFpyr, and is not usually called directly.
     * @param pyr
     * @param pind
     * @param log_rad
     * @param xrcos
     * @param yrcos
     * @param angle
     * @param nbands
     * @param levs
     * @param bands
     * @param resdftr
     * @param resdfti
     */
    private void reconSFpyrLevs(Vector pyr, Vector pind, double log_rad[][], double xrcos[],
                                    double yrcos[], double angle[][], int nbands, int levs[],
                                    int bands[], double resdftr[], double resdfti[]) {
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
        double nlog_rad[][];
        double nangle[][];
        double arr[][];
        double nresdftr[] = null;
        double nresdfti[] = null;
        int levsm1[];
        int px[];
        int py[];
        FFTUtility fftUtil;
        double yircos[];
        double lomask[][];
        int i2;
        int j2;
        int index;
        int index2;
        boolean haveOne;
        int lutsize;
        double xcosn[];
        int order;
        double consta;
        double var;
        double ycosn[];
        double himask[][];
        int b;
        boolean haveB;
        double anglemask[][];
        double bandr[];
        double bandi[];
        int intMat[];
        int parr[][];
        
        lo_ind = nbands + 1;
        intMat = (int[])pind.get(0);
        dimX = intMat[0];
        dimY = intMat[1];
        ctrX = (int)Math.ceil((dimX + 0.5)/2);
        ctrY = (int)Math.ceil((dimY + 0.5)/2);
        
        logx = log_rad.length;
        logy = log_rad[0].length;
        for (j = 0; j < logy; j++) {
            for (i = 0; i < logx; i++) {
                log_rad[i][j] = log_rad[i][j] + 1.0;
            }
        }
        
        moreOne = false;
        for (i = 0; i < levs.length; i++) {
            if (levs[i] > 1) {
                moreOne = true;
            }
        }
        
        if (moreOne) {
            lodimX = (int)Math.ceil((dimX - 0.5)/2);
            lodimY = (int)Math.ceil((dimY - 0.5)/2);
            loctrX = (int)Math.ceil((lodimX + 0.5)/2);
            loctrY = (int)Math.ceil((lodimY + 0.5)/2);
            lostartX = ctrX - loctrX + 1;
            lostartY = ctrY - loctrY + 1;
            loendX = lostartX + lodimX - 1;
            loendY = lostartY + lodimY - 1;
            nlog_rad = new double[loendX-lostartX][loendY-lostartY];
            nangle = new double[loendX-lostartX][loendY-lostartY];
            for (j = 0; j < loendY-lostartY; j++) {
                for (i = 0; i < loendX-lostartX; i++) {
                    nlog_rad[i][j] = log_rad[i + lostartX - 1][j + lostartY - 1];
                    nangle[i][j] = angle[i + lostartX - 1][j + lostartY - 1];
                }
            }
            if (pind.size() > lo_ind) {
                arr = new double[lo_ind-1][];
                parr = new int[(lo_ind-1)][2];
                for (i = 0; i < lo_ind-1; i++) {
                    arr[i] = (double[])pyr.remove(0);
                }
                for (i = 0; i < lo_ind-1; i++) {
                    parr[i] = (int[])pind.remove(0);
                }
                levsm1 = new int[levs.length];
                for (i = 0; i < levs.length; i++) {
                    levsm1[i] = levs[i] - 1;
                }
                reconSFpyrLevs(pyr, pind, nlog_rad, xrcos, yrcos, nangle, nbands,
                               levsm1, bands, nresdftr, nresdfti);
                for (i = lo_ind-2; i >= 0; i--) {
                    pyr.insertElementAt(arr[i], 0);
                    pind.insertElementAt(parr[i], 0);
                }
            } // if (pind.size() > lo_ind)
            else {
                px = new int[1];
                py = new int[1];
                nresdftr = pyrBand(pyr, pind, lo_ind-1, px, py);
                nresdfti = new double[px[0] * py[0]];
                // forward FFT
                fftUtil = new FFTUtility(nresdftr, nresdfti, py[0], px[0], 1, -1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(nresdftr, nresdfti, 1, py[0], px[0], -1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                center(nresdftr, nresdfti, px[0], py[0]);
            } // else
            
            yircos = new double[yrcos.length];
            for (i = 0; i < yrcos.length; i++) {
                yircos[i] = Math.sqrt(Math.abs(1.0 - yrcos[i]*yrcos[i]));
            }
            lomask = pointOp(nlog_rad, yircos, xrcos[0], (xrcos[1]-xrcos[0]), false);
            
            resdftr = new double[dimX * dimY];
            resdfti = new double[dimX * dimY];
            for (j = lostartY-1, j2 = 0; j < loendY; j++, j2++) {
                for (i = lostartX-1, i2 = 0; i < loendX; i++, i2++) {
                    index = i + j * dimX;
                    index2 = i2 + j2 * (loendX-lostartX);
                    resdftr[index] = nresdftr[index2] * lomask[i2][j2];
                    resdfti[index] = nresdfti[index2] * lomask[i2][j2];
                }
            }
        } // if (moreOne)
        else {
            resdftr = new double[dimX * dimY];
            resdfti = new double[dimX * dimY];
        } // else
        
        haveOne = false;
        for (i = 0; i < levs.length; i++) {
            if (levs[i] == 1) {
                haveOne = true;
            }
        }
        
        if (haveOne) {
            lutsize = 1024;
            xcosn = new double[3*(lutsize+1)];
            // -2*PI to PI
            for (i = 0, j = -(2*lutsize+1); i < xcosn.length; i++, j++) {
                xcosn[i] = j * Math.PI/lutsize;
            }
            order = nbands-1;
            // Divide by sqrt(sum_(n=0)^(N-1) cos(pi*n/N)^(2(N-1)) )
            var = factorial(order);
            consta = Math.pow(2.0,(2.0*order))*var*var/(nbands*factorial(2*order));
            consta = Math.sqrt(consta);
            ycosn = new double[xcosn.length];
            for (i = 0; i < xcosn.length; i++) {
                ycosn[i] = consta * Math.pow(xcosn[i], order);
            }
            himask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1]-xrcos[0]), false);
            
            index = 0;
            for (b = 1; b <= nbands; b++) {
                haveB = false;
                for (i = 0; i < bands.length; b++) {
                    if (bands[i] == b) {
                        haveB = true;
                    }
                }
                if (haveB) {
                    anglemask = pointOp(angle, ycosn, (xcosn[0] + Math.PI*(b-1)/nbands),
                                        (xcosn[1] - xcosn[0]), true);
                    bandr = (double[])pyr.get(index);
                    bandi = new double[dimX * dimY];
                    // forward FFT
                    fftUtil = new FFTUtility(bandr, bandi, dimY, dimX, 1, -1, FFTUtility.FFT);
                    fftUtil.setProgressBarVisible(false);
                    fftUtil.run();
                    fftUtil.finalize();
                    fftUtil = new FFTUtility(bandr, bandi, 1, dimY, dimX, -1, FFTUtility.FFT);
                    fftUtil.setProgressBarVisible(false);
                    fftUtil.run();
                    fftUtil.finalize();
                    center(bandr, bandi, dimX, dimY);
                    
                    if (((nbands -1) % 4) == 0) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                index = i + j * dimX;
                                resdftr[index] = resdftr[index] + bandr[index]*anglemask[i][j]*himask[i][j];
                                resdfti[index] = resdfti[index] + bandi[index]*anglemask[i][j]*himask[i][j];
                            }
                        }
                    } // if (((nbands -1) % 4) == 0)
                    else if (((nbands-1) % 4) == 1) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                index = i + j * dimX;
                                resdftr[index] = resdftr[index] - bandi[index]*anglemask[i][j]*himask[i][j];
                                resdfti[index] = resdfti[index] + bandr[index]*anglemask[i][j]*himask[i][j];
                            }
                        }    
                    } // else if (((nbands-1) % 4) == 1)
                    else if (((nbands-1) % 4) == 2) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                index = i + j * dimX;
                                resdftr[index] = resdftr[index] - bandr[index]*anglemask[i][j]*himask[i][j];
                                resdfti[index] = resdfti[index] - bandi[index]*anglemask[i][j]*himask[i][j];
                            }
                        }    
                    } // else if (((nbands-1) % 4) == 2)
                    else if (((nbands-1) % 4) == 3) {
                        for (j = 0; j < dimY; j++) {
                            for (i = 0; i < dimX; i++) {
                                index = i + j * dimX;
                                resdftr[index] = resdftr[index] + bandi[index]*anglemask[i][j]*himask[i][j];
                                resdfti[index] = resdfti[index] - bandr[index]*anglemask[i][j]*himask[i][j];
                            }
                        }    
                    } // else if (((nbands-1) % 4) == 3)
                } // if (haveB)
                index++;
            } // for (b = 1; b <= nbands; b++)
        } // if (haveOne)
    }
    
    /**
     * This is a port of spyrHt.m by Eero Simoncelli, 6/96.
     * Compute height of steerable pyramid with given index matrix.
     * @param pind
     * @return
     */
    private int spyrHt(Vector pind) {
        int ht = 0;
        int nbands;
        
        nbands = spyrNumBands(pind);
        
        // Don't count lowpass or highpass residual bands
        if (pind.size() > 2) {
            ht = (pind.size() - 2)/nbands;
        }
        else {
            ht = 0;
        }
        return ht;
    }
    
    /**
     * This is a port of spyrNumbands.m by Eero Simoncelli, 2/97.
     * Compute number of orientation bands in a steerable pyrmaid with
     * given index matrix.  If the pryamid contains only highpass and
     * lowpass bands (i.e., zero levels), returns 0.
     * @param pind
     * @return
     */
    private int spyrNumBands(Vector pind) {
        int nbands = 0;
        int b;
        int x;
        int y;
        int intMat[];
        if (pind.size() == 2) {
            nbands = 0;
        }
        else {
            // Count number of orientation bands
            b = 3;
            intMat = (int[])pind.get(1);
            x = intMat[0];
            y = intMat[1];
            while ((b <= pind.size()) && 
                    ((intMat = (int[])pind.get(b-1)) != null) &&
                    (intMat[0] == x) && (intMat[1] == y)) {
                     b = b+1;   
            }
            nbands = b - 2;
        }
        return nbands;
    }
    
    /**
     * This is a port of denoi_BLS_GSM_band by JPM, Univ. de Granada, 4/03
     * It solves for the BLS global optimum solution, using a flat (pseudo)
     * prior for log(z)
     * includeCovar Include/ not include covariance in the GSM model
     * optimize BLS/ MAP-Wiener (2-step)
     * @param y
     * @param noise
     * @param prnt Inlcude/ not include parent
     * @return
     */
    private double[][][] denoi_BLS_GSM_band(double y[][][], double noise[][][],
            boolean prnt) {
        double x_hat[][][] = null;
        int nx, ny;
        int nblx, nbly;
        int nexp;
        double zM[][] = null;
        int N;
        int Lx;
        int Ly;
        int cent;
        double Y[][] = null;
        double W[][] = null;
        double foo[][] = null;
        int n;
        int offset[] = new int[2];
        int i,j,kx,ky,index;
        double mtxr[];
        double mtxi[];
        double resr[] = null;
        double resi[] = null;
        int indexW;
        double C_w[][] = null;
        int inum;
        double sig2;
        double var;
        EigenvalueDecomposition eig;
        double[] eigenvalue;
        double[][] S;
        double[][] iS;
        double C_y[][] = null;
        double sy2;
        double C_x[][] = null;
        double Q[][] = null;
        double L[][] = null;
        double sumL;
        double sumPosL;
        double sx2;
        double la[];
        double V[][];
        double V2[][];
        double M[][];
        double m[];
        double lzmin;
        double lzmax;
        double step;
        int nsamp_z;
        double lzi[];
        double zi[];
        double laz[][];
        double p_lz[][];
        double mu_x[][] = null;
        double z_w[] = null;
        double pg1_lz[];
        double laz2[][];
        double aux[][];
        int ind[];
        double z[];
        int uv;
        int lh;
        int dv;
        int rh;
        double p_z[][];
        double p_lz_y[][];
        double maxp;
        boolean zeroFound;
        double rmat1[][];
        double rmat2[][];
        double rmat3[][];
        double rmat4[][];
        int i2, j2;
        
        nx = y.length;
        ny = y[0].length;
        
        // Discard the outer coefficients for the reference (central) coefficients
        // to avoid boundary effects
        nblx = nx - blockSizeX + 1;
        nbly = ny - blockSizeY + 1;
        nexp = nblx * nbly; // number of coefficients considered
        zM = new double[nx][ny]; // hidden variable z
        x_hat = new double[nx][ny][1]; // coefficient estimation
        N = blockSizeX * blockSizeY; // size of the neighborhood
        if (prnt) {
            N = N + 1; 
        }
        
        Lx = (blockSizeX - 1)/2;
        Ly = (blockSizeY - 1)/2;
        // reference coefficient in the neighborhood
        // central coef in the fine band
        cent = (blockSizeX*blockSizeY + 1)/2;
        
        // It will be the observed signal (rearranged in nexp neighborhoods)
        Y = new double[nexp][N];
        // It will be a signal with the same autocorrelation as the noise
        W = new double[nexp][N];
        
        foo = new double[nexp][N];
        
        // Compute covariance of noise from 'noise'
        n = 0;
        for (i = -Lx; i <= Lx; i++) {
            for (j = -Ly; j <= Ly; j++) {
                n = n + 1;
                mtxr = new double[nx*ny];
                mtxi = new double[nx*ny];
                for (ky = 0; ky < ny; ky++) {
                    for (kx = 0; kx < nx; kx++) {
                        index = kx + ky * nx;
                        mtxr[index] = noise[kx][ky][0];
                    }
                }
                offset[0] = i;
                offset[1] = j;
                shift(mtxr, mtxi, nx, ny, offset, resr, resi);
                foo = new double[nblx][nbly];
                for (ky = 0, indexW = 0; ky < nbly; ky++) {
                    for (kx = 0; kx < nblx; kx++) {
                        index = (kx + Lx)+ (ky + Ly) * nblx;
                        foo[kx][ky] = resr[index];
                        W[indexW++][n-1] = foo[kx][ky];
                    }
                }
            } // for (j = -Ly; j <= Ly; j++)
        } // for (i = -Lx; i <= Lx; i++)
        
        if (prnt) { // parent
            n = n +1;
            foo = new double[nblx][nbly];
            for (ky = 0, indexW = 0; ky < nbly; ky++) {
                for (kx = 0; kx < nblx; kx++) {
                    foo[kx][ky] = noise[kx+Lx][ky+Ly][1];
                    W[indexW++][n-1] = foo[kx][ky];
                }
            }
        } // if (prnt)
        
        C_w = innerProd(W);
        for (j = 0; j < N; j++) {
            for (i = 0; i < N; i++) {
                C_w[i][j] = C_w[i][j]/nexp;
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
        sig2 = sig2/inum;
        
        W = null;
        if (!includeCovar) {
            if (prnt) {
                var = C_w[N-1][N-1];
                C_w = new double[N][N];
                for (i = 0; i < N-1; i++) {
                    C_w[i][i] = sig2;
                }
                C_w[N-1][N-1] = var;
            }
            else {
                C_w = new double[N][N];
                for (i = 0; i < N; i++) {
                    C_w[i][i] = sig2;
                }
            }
        } // if (!inlcudeCovar)
        
        // Rearrange observed samples in 'nexp' neighborhoods
        n = 0;
        for (i = -Lx; i <= Lx; i++) {
            for (j = -Ly; j <= Ly; j++) {
                n = n + 1;
                mtxr = new double[nx*ny];
                mtxi = new double[nx*ny];
                for (ky = 0; ky < ny; ky++) {
                    for (kx = 0; kx < nx; kx++) {
                        index = kx + ky * nx;
                        mtxr[index] = y[kx][ky][0];
                    }
                }
                offset[0] = i;
                offset[1] = j;
                shift(mtxr, mtxi, nx, ny, offset, resr, resi);
                foo = new double[nblx][nbly];
                for (ky = 0, indexW = 0; ky < nbly; ky++) {
                    for (kx = 0; kx < nblx; kx++) {
                        index = (kx + Lx)+ (ky + Ly) * nblx;
                        foo[kx][ky] = resr[index];
                        Y[indexW++][n-1] = foo[kx][ky];
                    }
                }
            } // for (j = -Ly; j <= Ly; j++)
        } // for (i = -Lx; i <= Lx; i++)
        
        if (prnt) { // parent
            n = n +1;
            foo = new double[nblx][nbly];
            for (ky = 0, indexW = 0; ky < nbly; ky++) {
                for (kx = 0; kx < nblx; kx++) {
                    foo[kx][ky] = y[kx+Lx][ky+Ly][1];
                    Y[indexW++][n-1] = foo[kx][ky];
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
        for (j = 0; j < N; j++) {
            var = Math.sqrt(eigenvalue[j]);
            for (i = 0; i < N; i++) {
                if (eigenvalue[j] > 0) {
                    S[i][j] = S[i][j] * var;
                }
                else {
                    S[i][j] = 0;
                }
            }
        }
        iS = (new Matrix(S)).inverse().getArray();
        noise = null;
        
        C_y = innerProd(Y);
        for (j = 0; j < N; j++) {
            for (i = 0; i < N; i++) {
                C_y[i][j] = C_y[i][j]/nexp;
            }
        }
        // sy2 = observed (signal + noise) variance in the subband
        sy2 = 0.0;
        for (i = 0; i < N-1; i++) {
            sy2 += C_y[i][i];
        }
        if (!prnt) {
            sy2 += C_y[N-1][N-1];
            sy2 = sy2/N;
        }
        else {
            sy2 = sy2/(N-1);
        }
        
        // C_x = C_y - C_w as signal and noise are assumed to be independent
        for (j = 0; j < N; j++) {
            for (i = 0; i < N; i++) {
                C_x[i][j] = C_y[i][j] - C_w[i][j];
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
                L[i][i] = eigenvalue[i]*sumL/sumPosL;
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
                var = C_x[N-1][N-1];
                C_x = new double[N][N];
                for (i = 0; i < N-1; i++) {
                    C_x[i][i] = sx2;
                }
                C_x[N-1][N-1] = var;
            } // if (prnt)
            else {
                C_x = new double[N][N];
                for (i = 0; i < N; i++) {
                    C_x[i][i] = sx2;
                }
            } // else
        } // if (!includeCovar)
        
        // Double diagonalization of signal and noise eigenvlaues:
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
        Y = null;
        V2 = new double[nexp][N];
        for (j = 0; j < nexp; j++) {
            for (i = 0; i < N; i++) {
                V2[j][i] = V[i][j]*V[i][j];
            }
        }
        M = (new Matrix(S)).times(new Matrix(Q)).getArray();
        m = new double[N];
        for (i = 0; i < N; i++) {
            m[i] = M[cent-1][i];
        }
        
        // Compute p(Y|log(z))
        // Non-informative prior
        lzmin = -20.5;
        lzmax = 3.5;
        step = 2;
        nsamp_z = 13;
        
        lzi = new double[nsamp_z];
        zi = new double[nsamp_z];
        for (i = 0; i < nsamp_z; i++) {
            lzi[i] = lzmin + i*step;
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
                pg1_lz[j] = 1.0/Math.sqrt(pg1_lz[j]);
            }
            laz2 = new double[N][nsamp_z];
            for (j = 0; j < nsamp_z; j++) {
                for (i = 0; i < N; i++) {
                    laz2[i][j] = 1.0/(1.0 + laz[i][j]);
                }
            }
            aux = (new Matrix(V2)).times(new Matrix(laz2)).getArray();
            laz2 = null;
            for (j = 0; j < nsamp_z; j++) {
                for (i = 0; i < nexp; i++) {
                    aux[i][j] = Math.exp(-0.5*aux[i][j]);
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
                    aux[i][j] = m[i]*laz[i][j]/(1.0 + laz[i][j]);
                }
            }
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
            for (j = 1; j < nsamp_z; j++) {
                if (p_lz[i][j] > ind[i]) {
                    ind[i] = j;
                }
            }
        } // for (i = 0; i < nexp; i++)
        z = new double[nexp];
        for (i = 0; i < nexp; i++) {
            z[i] = zi[ind[i]];
        }
        V2 = null;
        aux = null;
        
        // For boundary handling
        uv = 1 + Lx;
        lh = 1 + Ly;
        dv = nblx + Lx;
        rh = nbly + Ly;
        
        for (j = lh-1, index = 0; j < rh; j++) {
            for (i = uv-1; i < dv; i++) {
                zM[i][j] = z[index++];
            }
        }
        
        // Propagation of the ML-estimated z to the boundaries
        
        // a) Corners
        for (j = 0; j < lh; j++) {
            for (i = 0; i < uv; i++) {
                zM[i][j] = zM[uv-1][lh-1];
            }
        }
        for (j = rh-1; j < ny; j++) {
            for (i = 0; i < uv; i++) {
                zM[i][j] = zM[uv-1][rh-1];
            }
        }
        for (j = 0; j < lh; j++) {
            for (i = dv-1; i < nx; i++) {
                zM[i][j] = zM[dv-1][lh-1];
            }
        }
        for (j = rh-1; j < ny; j++) {
            for (i = dv-1; i < nx; i++) {
                zM[i][j] = zM[dv-1][rh-1];
            }
        }
        // b) Bands
        for (j = lh; j < rh-1; j++) {
            for (i = 0; i < uv-1; i++) {
                zM[i][j] = zM[uv-1][j];
            }
        }
        for (j = lh; j < rh-1; j++) {
            for (i = dv; i < nx; i++) {
                zM[i][j] = zM[dv-1][j];
            }
        }
        for (j = 0; j < lh-1; j++) {
            for (i = uv; i < dv-1; i++) {
                zM[i][j] = zM[i][lh-1];
            }
        }
        for (j = rh; j < ny; j++) {
            for (i = uv; i < dv-1; i++) {
                zM[i][j] = zM[i][rh-1];
            }
        }
        
        // Do scalar Wiener for the boundary coefficients
        if (z_w != null) {
            // Spatially varying noise
        }
        else {
            // Spatially invariant noise
            for (j = 0; j < ny; j++) {
                for(i = 0; i < nx; i++) {
                    x_hat[i][j][0] = y[i][j][0]*sx2*zM[i][j]/(sx2*zM[i][j] + sig2);  
                }
            }
        } // else
        
        // Prior for log(z)
        p_z = new double[nsamp_z][1];
        // Flat log-prior (non-informative for GSM)
        for (i = 0; i < nsamp_z; i++) {
            p_z[i][0] = 1.0/nsamp_z;
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
                   }
                   else {
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
            for (i = 0; i <nexp; i++) {
                for (j = 0; j < nsamp_z; j++) {
                    rmat3[i][j] = foo[i][0];
                }
            }
            rmat4 = new double[nexp][nsamp_z];
            for (i = 0; i < nexp; i++) {
                for (j = 0; j <nsamp_z; j++) {
                    rmat4[i][j] = p_z[j][0];
                }
            }
            for (i = 0; i < nexp; i++) {
                for (j = 0; j < nsamp_z; j++) {
                    p_lz_y[i][j] = rmat1[i][j]*p_lz_y[i][j]/rmat2[i][j] +
                    rmat3[i][j]*rmat4[i][j];
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
                    p_lz_y[i][j] = p_lz_y[i][j]/rmat1[i][j];
                }
            }
            for (i = 0; i < nexp; i++) {
                rmat1[i] = null;
            }
            rmat1 = null;
        } // else
        aux[0] = null;
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
        
        for (j = Ly, j2 = 0; j < nbly + Ly; j++, j2++) {
           for (i = Lx, i2 = 0; i < nblx + Lx; i++, i2++) {
               x_hat[i][j][0] = aux[i2][j2];
           }
        }
        return x_hat;
    }
    
    /**
     * Computes mat'*mat
     * @param mat
     * @return
     */
    private double[][] innerProd(double mat[][]) {
        int xdim = mat.length;
        int ydim = mat[0].length;
        double res[][] = new double[ydim][ydim];
        int i,j;
        double tmp;
        int k;
        
        for (i = 0; i < ydim; i++) {
            for (j = i; j < ydim; j++) {
                tmp = 0.0;
                for (k = 0; k < xdim; k++) {
                    tmp += mat[i][k] * mat[k][j];
                }
                res[i][j] = tmp;
                res[j][i] = tmp;
            }
        }
        return res;
    }
    
    /**
     * Port of expand.m, written by JPM, 5/1//95
     * It expands (spatially) an image into a factor f in each dimension.
     * It does it filling in with zeros the expanded Fourier domain.
     * @param t
     * @param f
     * @param mx
     * @param my
     * @param ter
     * @param tei
     */
    private void expand(double[] t, double f, int mx, int my,
                        double [] ter, double [] tei) {
        FFTUtility fftUtil;
        double tr[] = null;
        double ti[] = null;
        int i, j, i2, j2;
        double f2;
        int cx;
        int cy;
        boolean evenmx;
        boolean evenmy;
        int x1, x2, y1, y2;
        int xs;
        int ys;
        int xf;
        int yf;
        double esqr;
        double esqi;
        int offset[] = new int[2];
        
        mx = (int)Math.round(f*mx);
        my = (int)Math.round(f*my);
        
        tr = new double[mx*my];
        ti = new double[mx*my];
        for (i = 0; i < tr.length; i++) {
            tr[i] = t[i];
        }
        ter = new double[mx*my];
        tei = new double[mx*my];
        // forward FFT
        fftUtil = new FFTUtility(tr, ti, my, mx, 1, -1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(tr, ti, 1, my, mx, -1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        center(tr, ti, mx, my);
        f2 = f * f;
        for (i = 0; i < tr.length; i++) {
            tr[i] = f2 * tr[i];
            ti[i] = f2 * ti[i];
        }
        
        cx = (int)Math.ceil(mx/2.0);
        evenmx = (mx == ((mx/2)*2));
        cy = (int)Math.ceil(my/2.0);
        evenmy = (my == ((my/2)*2));
        
        x1 = cx - (int)Math.floor(mx/(2.0*f)) - 1;
        if (evenmx) {
            x1 = x1 + 2;
        }
        x2 = cx + (int)Math.floor(mx/(2.0*f)) - 1;
        y1 = cy - (int)Math.floor(my/(2.0*f)) - 1;
        if (evenmy) {
            y1 = y1 + 2;
        }
        y2 = cy + (int)Math.floor(my/(2.0*f)) - 1;
        
        xs = 0;
        if (evenmx) {
            xs = 1;
        }
        xf = (int)Math.round(mx/f) - 1;
        ys = 0;
        if (evenmy) {
            ys = 1;
        }
        yf = (int)Math.round(my/f) - 1;
        
        for (j = y1, j2 = ys; j <= y2; j++, j2++) {
            for (i = x1, i2 = xs; i <= x2; i++, i2++) {
                ter[i + mx*j] = tr[i2 + mx*j2];
                tei[i + mx*j] = ti[i2 + mx*j2];
            }
        }
        
        if (evenmx) {
            for (j = y1, j2 = 1; j <= y2; j++, j2++) {
                ter[x1-1 + mx*j] = tr[mx*j2]/2.0;
                tei[x1-1 + mx*j] = ti[mx*j2]/2.0;
            }
            
            for (j = y1, j2 = (int)Math.round(my/f)-1; j <= y2; j++, j2--) {
                ter[x2+1 + mx*j] = tr[mx*j2]/2.0;
                tei[x2+1 + mx*j] = -ti[mx*j2]/2.0;
            }
        } // if (evenmx)
        
        if (evenmy) {
            for (i = x1, i2 = 1; i <= x2; i++, i2++) {
                ter[i + mx*(y1-1)] = tr[i2]/2.0;
                tei[i + mx*(y1-1)] = ti[i2]/2.0;
            }
            
            for (i = x1, i2 = (int)Math.round(mx/f)-1; i <= x2; i++, i2--) {
                ter[i + mx*(y2+1)] = tr[i2]/2.0;
                tei[i + mx*(y2+1)] = -ti[i2]/2.0f;
            }
        } // if (evenmy)
        
        if (evenmx && evenmy) {
            esqr = tr[0]/4.0;
            esqi = ti[0]/4.0;
            ter[x1-1 + mx*(y1-1)] = esqr;
            tei[x1-1 + mx*(y1-1)] = esqi;
            ter[x2+1 + mx*(y1-1)] = esqr;
            tei[x2+1 + mx*(y1-1)] = esqi;
            ter[x1-1 + mx*(y2+1)] = esqr;
            tei[x1-1 + mx*(y2+1)] = esqi;
            ter[x2+1 + mx*(y2+1)] = esqr;
            tei[x2+1 + mx*(y2+1)] = esqi;
        } // if (evenmx && evenmy)
        
        center(ter, tei, mx, my);
        offset[0] = 1;
        if (evenmx) {
            offset[0] = 0;
        }
        offset[1] = 1;
        if (evenmy) {
            offset[1] = 0;
        }
        shift(ter, tei, mx, my, offset, ter, tei);
        // Inverse FFT
        fftUtil = new FFTUtility(ter, tei, my, mx, 1, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(ter, tei, 1, my, mx, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        return;
    }
    
    
    /**
     * Port of shift.m
     * Circular shift 2D matrix samples by offset (a [X, Y} 2 vector),
     * such that res(pos) = mtx(pos-offset)
     * @param mtxr
     * @param mtxi
     * @param dimx
     * @param dimy
     * @param offset
     * @param resr
     * @param resi
     */
    private void shift(double mtxr[], double mtxi[], int dimx, int dimy, int offset[],
                       double resr[], double resi[]) {
        resr = new double[mtxr.length];
        resi = new double[mtxr.length];
        int n;
        int offsetx;
        int offsety;
        int i, j, i2, j2;
        
        n = (int)Math.floor(-offset[0]/(double)dimx);
        offsetx = -offset[0] - n*dimx;
        if (offsetx < 0) {
            offsetx = offsetx + dimx;
        }
        
        n = (int)Math.floor(-offset[1]/(double)dimy);
        offsety = -offset[1] - n*dimy;
        if (offsety < 0) {
            offsety = offsety + dimy;
        }
        
        for (j = 0, j2 = offsety; j2 <= dimy-1; j++, j2++) {
            for (i = 0, i2 = offsetx; i2 <= dimx - 1; i++, i2++) {
                resr[i + dimx*j] = mtxr[i2 + dimx*j2];
                resi[i + dimx*j] = mtxi[i2 + dimx*j2];
            }
            
            for (i = dimx - offsetx, i2 = 0; i2 <= offsetx-1; i++, i2++) {
                resr[i + dimx*j] = mtxr[i2 + dimx*j2];
                resi[i + dimx*j] = mtxi[i2 + dimx*j2];
            }
        }
        
        for (j = dimy - offsety, j2 = 0; j2 <= offsety - 1; j++, j2++) {
            for (i = 0, i2 = offsetx; i2 <= dimx - 1; i++, i2++) {
                resr[i + dimx*j] = mtxr[i2 + dimx*j2];
                resi[i + dimx*j] = mtxi[i2 + dimx*j2];
            }
            
            for (i = dimx - offsetx, i2 = 0; i2 <= offsetx-1; i++, i2++) {
                resr[i + dimx*j] = mtxr[i2 + dimx*j2];
                resi[i + dimx*j] = mtxi[i2 + dimx*j2];
            }
        }
        
        return;
    }
    
    /**
     * This duplicates the functionality of pyrBand.m
     * @param pyr
     * @param pind
     * @param band
     * @rows
     * @columns
     * @return
     */
    private double[] pyrBand(Vector pyr, Vector pind, int band, int x[], int y[]) {
        double arr[];
        int intMat[];
        
        arr = (double[])pyr.get(band); 
        intMat = (int[])pind.get(band);
        if (x == null) {
            x = new int[1];
        }
        if (y == null) {
            y = new int[1];
        }
        x[0] = intMat[0];
        y[0] = intMat[1];
        return arr;
    }
    
    /**
     * This is a port of buildFullSFpyr2.m
     * Construct a steerable pyramid on matrix im, in the Fourier domain.
     * Unlike the standard transform, subdivides the highpass band into
     * orientations.
     * @param im
     * @param imx
     * @param imy
     * @param ht
     * @param order
     * @param twidth
     * @param pind
     * @return
     */
    private Vector buildFullSFpyr2(double[] im, int imx, int imy, int ht, int order, double twidth,
            Vector pind) {
        Vector pyr = null;
        int max_ht;
        int nbands;
        int ctrx;
        int ctry;
        double xramp[][];
        double yramp[][];
        double angle[][];
        double log_rad[][];
        int i, j;
        int index;
        double value;
        double logC;
        double xrcos[] = null;
        double yrcos[] = null;
        double yircos[];
        double lo0mask[][] = null;
        FFTUtility fftUtil;
        double imdftr[];
        double imdfti[];
        double lo0dftr[];
        double lo0dfti[];
        double hi0mask[][];
        int lutsize;
        double xcosn[];
        double var;
        double consta;
        double ycosn[];
        double bands[][];
        int bind[][];
        int b;
        double anglemask[][];
        double maskr[][];
        double maski[][];
        double bandfftr[] = null;
        double bandffti[] = null;
        int intMat[];
        
        // log2(x) = loge(x)/loge(2)
        logC = 1.0/Math.log(2.0);
        max_ht = (int)Math.floor(logC*Math.log(Math.min(imx,imy)) + 2);
        
        if (ht > max_ht) {
            MipavUtil.displayError("Error! Cannot build pyramid higher than " + 
                                   max_ht + " levels");
            error = 1;
            return null;
        }
        
        if ((order > 15) || (order < 0)) {
            MipavUtil.displayWarning("Warning: ORDER must be an integer in the range 0 to 15");
            if (order < 0) {
                order = 0;
            }
            else if (order > 15) {
                order = 15;
            } 
        } // if ((order > 15) || (order < 0))
        nbands = order + 1;
        
        if (twidth <= 0) {
            MipavUtil.displayWarning("Warning: TWIDTH must be positive.  Setting to 1");
            twidth = 1;
        }
        ctrx = (int)Math.ceil((imx + 0.5)/2);
        ctry = (int)Math.ceil((imy + 0.5)/2);
        xramp = new double[imx][imy];
        yramp = new double[imx][imy];
        for (j = 0; j < imy; j++) {
            value = 2.0*((j+1) - ctry)/imy;
            for (i = 0; i < imx; i++) {
                xramp[i][j] = value;
            }
        }
        for (i = 0; i < imx; i++) {
            value = 2.0*((i + 1) - ctrx)/imx;
            for (j = 0; j < imy; j++) {
                yramp[i][j] = value;
            }
        }
        angle = new double[imx][imy];
        log_rad = new double[imx][imy];
        for (j = 0; j < imy; j++) {
            for (i = 0; i < imx; i++) {
                angle[i][j] = Math.atan2(yramp[i][j], xramp[i][j]);
                log_rad[i][j] = Math.sqrt(xramp[i][j]*xramp[i][j] + yramp[i][j]*yramp[i][j]);
            }
        }
        log_rad[ctrx-1][ctry-1] = log_rad[ctrx-1][ctry-2];
        // log2(log_rad) = loge(rad)/loge(2)
        for (j = 0; j < imy; j++) {
            for (i = 0; i < imx; i++) {
                log_rad[i][j] = logC*Math.log(log_rad[i][j]);
            }
        }
        
        // Radial transition function (a raised cosine in log-frequency)
        rcosFn(twidth, (-twidth/2.0), 0.0, 1.0, xrcos, yrcos);
        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }
        
        yircos = new double[yrcos.length];
        for (i = 0; i < yrcos.length; i++) {
            value = yrcos[i];
            yircos[i] = Math.sqrt(1.0 - value*value);
        }
        lo0mask = pointOp(log_rad, yircos, xrcos[0],
                          (xrcos[1] - xrcos[0]), false);
        // forward FFT
        imdftr = new double[imx*imy];
        for (i = 0; i < im.length; i++) {
            imdftr[i] = im[i];    
        }
        imdfti = new double[imx*imy];
        fftUtil = new FFTUtility(imdftr, imdfti, imy, imx, 1, -1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(imdftr, imdfti, 1, imy, imx, -1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        center(imdftr, imdfti, imx, imy);
        lo0dftr = new double[imx*imy];
        lo0dfti = new double[imx*imy];
        for (j = 0; j < imy; j++) {
            for (i = 0; i < imx; i++) {
                index = i + j*imx;
                lo0dftr[index] = imdftr[index] * lo0mask[imx][imy];
                lo0dfti[index] = imdfti[index] * lo0mask[imx][imy];
            }
        }
        
        pyr = buildSFpyrLevs(lo0dftr, lo0dfti, imx, imy, log_rad, xrcos,
                             yrcos, angle, ht, nbands, pind);
        
        // Split the highpass band into orientations
        hi0mask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);
        
        lutsize = 1024;
        xcosn = new double[3*(lutsize+1)];
        // -2*PI to PI
        for (i = 0, j = -(2*lutsize+1); i < xcosn.length; i++, j++) {
            xcosn[i] = j * Math.PI/lutsize;
        }
        order = nbands-1;
        // Divide by sqrt(sum_(n=0)^(N-1) cos(pi*n/N)^(2(N-1)) )
        var = factorial(order);
        consta = Math.pow(2.0,(2.0*order))*var*var/(nbands*factorial(2*order));
        consta = Math.sqrt(consta);
        ycosn = new double[xcosn.length];
        for (i = 0; i < xcosn.length; i++) {
            ycosn[i] = consta * Math.pow(xcosn[i], order);
        }
        
        bands = new double[nbands][imx * imy];
        bind = new int[nbands][2];
        
        for (b = 1; b <= nbands; b++) {
            anglemask = pointOp(angle, ycosn, (xcosn[0] + Math.PI*(b-1)/nbands),
                                (xcosn[1] - xcosn[0]), true);  
            maskr = new double[imx][imy];
            maski = new double[imx][imy];
            if (((nbands-1) % 4) == 0) {
                for (j = 0; j < imy; j++) {
                    for (i = 0; i < imx; i++) {
                        maskr[i][j] = anglemask[i][j]*hi0mask[i][j];
                    }
                }
            } // if (((nbands-1) % 4) == 0)
            else if (((nbands-1) %4) == 1) {
                for (j = 0; j < imy; j++) {
                    for (i = 0; i < imx; i++) {
                        maski[i][j] = -anglemask[i][j]*hi0mask[i][j];
                    }
                }    
            } // else if (((nbands-1) %4) == 1)
            else if (((nbands-1) %4) == 2) {
                for (j = 0; j < imy; j++) {
                    for (i = 0; i < imx; i++) {
                        maskr[i][j] = -anglemask[i][j]*hi0mask[i][j];
                    }
                }    
            } // else if (((nbands-1) %4) == 2)
            else if (((nbands-1) %4) == 3) {
                for (j = 0; j < imy; j++) {
                    for (i = 0; i < imx; i++) {
                        maski[i][j] = anglemask[i][j]*hi0mask[i][j];
                    }
                }    
            } // else if (((nbands-1) %4) == 3)
            // Make real the contents in the HF cross (to avoid
            // information loss in these frequencies.)
            // It distributes evenly these contents among the nbands orientations
            var = 1.0/Math.sqrt(nbands);
            for (j = 0; j < imy; j++) {
                maskr[0][j] = var;
            }
            for (i = 1; i < imx; i++) {
                maskr[i][0] = var;
            }
            
            if (bandfftr == null) {
                bandfftr = new double[imx * imy];    
            }
            if (bandffti == null) {
                bandffti = new double[imx * imy];
            }
            for (j = 0; j < imy; j++) {
                for (i = 0; i < imx; i++) {
                    index = i + j * imx;
                    bandfftr[index] = imdftr[index] * maskr[i][j] - imdfti[index] * maski[i][j];
                    bandffti[index] = imdftr[index] * maski[i][j] + imdfti[index] * maskr[i][j];
                }
            }
            center(bandfftr, bandffti, imx, imy);
            // Inverse FFT
            fftUtil = new FFTUtility(bandfftr, bandffti, imy, imx, 1, +1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(bandfftr, bandffti, 1, imy, imx, +1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            for (i = 0; i < bandfftr.length; i++) {
                bands[b-1][i] = bandfftr[i];
            }
            bind[b-1][0] = imx;
            bind[b-1][1] = imy;
        } // for (b = 1; b <= nbands; b++)
        
        for (i = nbands-1; i >= 0; i--) {
            pyr.insertElementAt(bands[i], 0);
            pind.insertElementAt(bind[i], 0);
        }
        
        intMat = new int[2];
        // Dummy highpass [ [0 0]; pind]
        pind.insertElementAt(intMat, 0);
        return pyr;
    }
    
    /**
     * Ported from named_filter.m by Eero Simoncelli, 6/96.
     * Some standard 1D filter kernels.  These are scaled such that their 
     * L2-norm is 1.0.
     * binomN             - binomial coefficient filter of order N-1
       haar:              - Haar wavelet.
       qmf8, qmf12, qmf16 - Symmetric Quadrature Mirror Filters [Johnston80]
       daub2,daub3,daub4  - Daubechies wavelet [Daubechies88].
       qmf5, qmf9, qmf13: - Symmetric Quadrature Mirror Filters [Simoncelli88,Simoncelli90]

     * References:
     * [Johnston80] - J D Johnston, "A filter family designed for use in quadrature 
       mirror filter banks", Proc. ICASSP, pp 291-294, 1980.
      
      [Daubechies88] - I Daubechies, "Orthonormal bases of compactly supported wavelets",
      Commun. Pure Appl. Math, vol. 42, pp 909-996, 1988.
     
      [Simoncelli88] - E P Simoncelli,  "Orthogonal sub-band image transforms",
      PhD Thesis, MIT Dept. of Elec. Eng. and Comp. Sci. May 1988.
      Also available as: MIT Media Laboratory Vision and Modeling Technical 
      Report #100. 
      
      [Simoncelli90] -  E P Simoncelli and E H Adelson, "Subband image coding",
      Subband Transforms, chapter 4, ed. John W Woods, Kluwer Academic 
      Publishers,  Norwell, MA, 1990, pp 143--192.
     * @param name
     * @return
     */
    private double[] namedFilter(int name, int binomialSize) {
        double kernel[]= null;
        double sq2 = Math.sqrt(2.0);
        int i;
        if (name == BINOMIAL) {
            kernel = binomialFilter(binomialSize);
            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];
            }
        }
        else if (name == QMF5) {
            kernel = new double[]{-0.076103, 0.3535534, 0.8593118,
                                   0.3535534, -0.076103};
        }
        else if (name == QMF9) {
            kernel = new double[]{0.02807382, -0.060944743, -0.073386624,
                                  0.41472545, 0.7973934, 0.41472545,
                                  -0.073386624, -0.060944743, 0.02807382};    
        }
        else if (name == QMF13) {
            kernel = new double[]{-0.014556438, 0.021651438, 0.039045125,
                                  -0.09800052, -0.057827797, 0.42995453,
                                  0.7737113, 0.42995453, -0.057827797,
                                  -0.09800052, 0.039045125, 0.021651438,
                                  -0.014556438};
        }
        else if (name == QMF8) {
            kernel = new double[]{0.00938715, -0.07065183, 0.06942827,
                                  0.4899808, 0.4899808, 0.06942827,
                                  -0.07065183, 0.00938715};
            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];   
            }
        }
        else if (name == QMF12) {
            kernel = new double[]{-0.003809699, 0.01885659, -0.002710326,
                                  -0.08469594, 0.08846992, 0.4843894,
                                  0.4843894, 0.08846992, -0.08469594,
                                  -0.002710326, 0.01885659, -0.003809699}; 
            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];   
            }
        }
        else if (name == QMF16) {
            kernel = new double[]{0.001050167, -0.005054526, -0.002589756,
                                  0.0276414, -0.009666376, -0.09039223,
                                  0.09779817, 0.4810284, 0.4810284,
                                  0.09779817, -0.09039223, -0.009666376,
                                  0.0276414, -0.002589756, -0.005054526,
                                  0.001050167};
            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];   
            }
        }
        else if (name == HAAR) {
            kernel = new double[]{1.0/sq2, 1.0/sq2};
        }
        else if (name == DAUB2) {
            kernel = new double[]{0.482962913145, 0.836516303738, 0.224143868042,
                                  -0.129409522551};
        }
        else if (name == DAUB3) {
            kernel = new double[]{0.332670552950, 0.806891509311, 0.459877502118,
                                  -0.135011020010, -0.085441273882,  0.035226291882};

        }
        else if (name == DAUB4) {
            kernel = new double[]{0.230377813309, 0.714846570553, 0.630880767930,
                                  -0.027983769417, -0.187034811719, 0.030841381836,
                                  0.032883011667, -0.010597401785};

        }
        else if (name == GAUSS3) {
            // for backward compatibility
            kernel = new double[]{0.25, 0.5, 0.25};
            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];   
            }
        }
        else if (name == GAUSS5) {
            // for backward compatibility
            kernel = new double[]{0.0625, 0.25, 0.375, 0.25, 0.0625};
            for (i = 0; i < kernel.length; i++) {
                kernel[i] = sq2 * kernel[i];   
            }
        }
        else {
            MipavUtil.displayError("Bad filter name = " + name);
            error = 1;
        }
        return kernel;
    }
    
    /**
     * This is a port of binomialFilter.m by Eero Simoncelli, 2/97.
     * returns a vector of binomial coefficients of order (binomialSize-1)
     * @param binomialSize
     * @return
     */
    private double[] binomialFilter(int binomialSize) {
        double kernel[];
        double baseK[];
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
        
        for (n = 1; n <= binomialSize-2; n++) {
            kernel = conv(baseK, kernel);
        }
        
        return kernel;
    }
    
    /**
     * 1D convolution
     * @param s1
     * @param s2
     * @return
     */
    private double[] conv(double s1[], double s2[]) {
        int j;
        int k;
        double kernel[] = new double[s1.length + s2.length-1];
        
        for (k = 0; k < kernel.length; k++) {
            for (j = Math.max(0,k+1-s2.length); j <= Math.min(k,s1.length-1); j++) {
                kernel[k] += s1[j]*s2[k-j];
            }
        }
        return kernel;
    }
    
    /**
     * This is a port of the file modulateFlip.m by Eero Simoncelli, 7/96.
     * QMF/Wavelet highpass filter construction: modulate by (-1)^n,
     * reverse order (and shift by one, which is handled by the convolution
     * routines).  This is an extension of the original definition of QMF's
     * (e.g., see Simoncelli90).
     * @param lfilt
     * @return
     */
    private double[] modulateFlip(double lfilt[]) {
        double hfilt[] = null;
        int sz;
        int sz2;
        int ind[];
        int i;
        
        sz = lfilt.length;
        sz2 = (int)Math.ceil(sz/2.0);
        
        ind = new int[sz];
        for (i = 0; i < sz; i++) {
            ind[i] = sz - i;
        }
        
        hfilt = new double[sz];
        for (i = 0; i < sz; i++) {
            if (((ind[i] - sz2) % 2) == 0) {
                hfilt[i] = lfilt[ind[i]];
            }
            else {
                hfilt[i] = -lfilt[ind[i]];
            }
        }
        return hfilt;
        
    }
    
    /**
     * This is a port of corrDn.c by EPS, 7/96
     * Compute correlation of matrices image with filt, followed by downsampling.
     * These arguments should be 1D or 2D matrices, and image must be larger
     * (in both dimensions) than filt.  The origin of filt is assumed to be
     * floor(size(filt)/2) + 1
     * @param image
     * @param xdim first dimension of image
     * @param ydim second dimension of image
     * @param filt
     * @param xfdim first dimension of filt
     * @param yfdim second dimension of filt
     * @param edges Determines boundary handling
     *        CIRCULAR = Circular convolution
     *        REFLECT1 = Reflect about the edge pixels
     *        REFLECT2 = Reflect, doubling the edge pixels
     *        REPEAT = Repeat the edge pixels
     *        ZERO = Assume values of zero outside the image boundary
     *        EXTEND = Reflect and invert
     *        DONT_COMPUTE = Zero output when filter overhangs input boundaries
     * Downsampling factors are determined by xstep and ystep
     * @param xstep
     * @param ystep
     * start and stop detrmine the window overwhich convolution occurs
     * @param xstart
     * @param ystart
     * @param xrdim first dimension of result
     * @param yrdim second dimension of result
     * Note: This operation corresponds to multiplication of a signal
     * vector by a matrix whose rows contain copies of the filt shifted by
     * multiples of step.  See upconv for the operation corresponding to
     * the transpose of this matrix.
     * @return
     */
    private double[] corrDn(double image[], int xdim, int ydim, double filt[], 
                            int xfdim, int yfdim, int edges,
                            int xstep, int ystep, int xstart, int ystart,
                            int xrdim[], int yrdim[]) {
        double result[] = null;
        int xstop;
        int ystop;
        double temp[];
        
        xstart--;
        ystart--;
        xstop = xdim;
        ystop = ydim;
        if (xrdim == null) {
            xrdim = new int[1];
        }
        if (yrdim == null) {
            yrdim = new int[1];
        }
        xrdim[0] = (xstop-xstart+xstep-1)/xstep;
        yrdim[0] = (ystop-ystart+ystep-1)/ystep;
        result = new double[xrdim[0] * yrdim[0]];
        if (edges == CIRCULAR) {
            internal_wrap_reduce(image, xdim, ydim, filt, xfdim, yfdim,
                                 xstart, xstep, xstop, ystart, ystep, ystop,
                                 result);        
        }
        else {
            temp = new double[xfdim * yfdim];
            internal_reduce(image, xdim, ydim, filt, temp, xfdim, yfdim,
                            xstart, xstep, xstop, ystart, ystep, ystop,
                            result, edges);
        } // else
        return result;
    }
    
    /**
     * This is ported from wrap.c by Eero Simoncelli, 2/97
     * Performs correlation (i.e., convolution with filt(-x,-y) of filt
     * with image followed by subsampling (a.k.a. reduce in Burt&Adelson81).
     * The operations are combined to avoid unnecessary computation of the
     * convolution samples that are to be discarded in the subsampling
     * operation.  The convolution is done in 9 sections so that mod
     * operations are not performed unnecessarily.  The subsampling lattice
     * is specified by the start, step, and stop parameters.
     * @param image
     * @param xdim first dimension of image
     * @param ydim second dimension of image
     * @param filt
     * @param xfdim first dimension of filt
     * @param yfdim second dimension of filt
     * @param xstart
     * @param xstep
     * @param xstop
     * @param ystart
     * @param ystep
     * @param ystop
     * @param result
     */
    private void internal_wrap_reduce(double image[], int xdim, int ydim, double filt[],
                                      int xfdim, int yfdim, int xstart, int xstep, int xstop,
                                      int ystart, int ystep, int ystop, double result[]) {
        int filtSize = xfdim * yfdim;
        int xCtrStop = xdim - xfdim + 1;
        int yCtrStop = ydim - yfdim + 1;
        int xCtrStart = 0;
        int yCtrStart = 0;
        int xfmid = xfdim/2;
        int yfmid = yfdim/2;
        double imval[][];
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
                index = i + j*xdim;
                imval[j][i] = image[index];
            }
        }
        
        // top rows
        for (respos = 0, ypos = ystart; ypos < yCtrStart; ypos += ystep) {
           for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++) {
               sum = 0.0;
               for (yim = ypos+ydim, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
                   for (xim = xpos+xdim; filtPos < xFiltStop; filtPos++, xim++) {
                       sum += imval[yim % ydim][xim % xdim] * filt[filtPos];
                   }
               }
               result[respos] = sum;
           } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++)
           
           for (; xpos < xCtrStop; xpos += xstep, respos++) {
               sum = 0.0;
               for (yim = ypos+ydim, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
                   for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                       sum += imval[yim % ydim][xim] * filt[filtPos];
                   }
               }
               result[respos] = sum;    
           } // for (; xpos < xCtrStop; xpos += xstep, respos++)
           
           for (; xpos < xstop; xpos += xstep, respos++) {
               sum = 0.0;
               for (yim = ypos+ydim, filtPos = 0, xFiltStop = xfdim;
                    xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
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
                for (yim = ypos, filtPos = 0, xFiltStop = xfdim;
                     xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
                    for (xim = xpos+xdim; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim][xim % xdim] * filt[filtPos];
                    }
                }
                result[respos] = sum;    
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++)
            
            // Center section
            for (; xpos < xCtrStop; xpos += xstep, respos++) {
                sum = 0.0;
                for (yim = ypos, filtPos = 0, xFiltStop = xfdim;
                     xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim][xim] * filt[filtPos];
                    }
                }
                result[respos] = sum;    
            } // for (; xpos < xCtrStop; xpos += xstep, respos++)
            
            for (; xpos < xstop; xpos += xstep, respos++) {
                sum = 0.0;
                for (yim = ypos, filtPos = 0, xFiltStop = xfdim;
                     xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
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
                for (yim = ypos, filtPos = 0, xFiltStop = xfdim;
                     xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
                    for (xim = xpos+xdim; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim % xdim] * filt[filtPos];
                    }
                }
                result[respos] = sum;    
            } // for (xpos = xstart; xpos < xCtrStart; xpos += xstep, respos++)
            
            for (; xpos < xCtrStop; xpos += xstep, respos++) {
                sum = 0.0;
                for (yim = ypos, filtPos = 0, xFiltStop = xfdim;
                     xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim] * filt[filtPos];
                    }
                }    
            } // for (; xpos < xCtrStop; xpos += xstep, respos++)
            
            for (; xpos < xstop; xpos += xstep, respos++) {
                sum = 0.0;
                for (yim = ypos, filtPos = 0, xFiltStop = xfdim;
                     xFiltStop <= filtSize; yim++, xFiltStop += xfdim) {
                    for (xim = xpos; filtPos < xFiltStop; filtPos++, xim++) {
                        sum += imval[yim % ydim][xim % xdim] * filt[filtPos];
                    }
                }
                result[respos] = sum;    
            } // for (; xpos < xstop; xpos += xstep, respos++)
        } // for (; ypos < ystop; ypos += ystep)
        
        return;
    } // internal_wrap_reduce
    
    private void internal_reduce(double image[], int xdim, int ydim, double filt[], double temp[],
                                 int xfdim, int yfdim, int xstart, int xstep, int xstop,
                                 int ystart, int ystep, int ystop, double result[], int edges) {
        // Code not needed yet since decomReconstW only calls buildWpyr with CIRCULAR    
    }
    
    /**
     * This is a port of buildWpyr.m by Eero Sinoncelli, 6/96.
     * Construct a separable orthonormal QMF/wavelet pyramid on matrix im.
     * @param im
     * @param imx
     * @param imy
     * @param ht  Specifies the number of pyramid levels to build.
     *            default is maxPyrHt(im, filt).
     * @param filt Filter can be of even or odd length, but should be symmetric
     * @param edges Specifies edge-handling, and defaults to REFLECT1.
     * @param pind N size 2 int[] arrays, containing the sizes of each subband.
     * @return
     */
    private Vector buildWpyr(double[] im, int imx, int imy, int ht, int filt, int edges,
            Vector pind) {
        // pyr is a vector containing the N pyramid subbands, ordered from fine to coarse.
        Vector pyr = null;
        double kernel[];
        double hkernel[];
        int stag;
        int max_ht;
        int x;
        int y;
        int intMat[];
        int intMat2[];
        int intMat3[];
        double lolo[] = null;
        double hihi[];
        double lo[];
        double hi[];
        double lohi[] = null;
        double hilo[] = null;
        int lox[] = null;
        int loy[] = null;
        int hix[] = null;
        int hiy[] = null;
        int lolox[] = null;
        int loloy[] = null;
        int hihix[] = null;
        int hihiy[] = null;
        int lohix[] = null;
        int lohiy[] = null;
        int hilox[] = null;
        int hiloy[] = null;
        Vector nind = null;
        Vector npyr;
        
        kernel = namedFilter(filt, 0);
        
        hkernel = modulateFlip(kernel);
        
        // Stagger sampling if filter is odd-length:
        if ((kernel.length % 2) == 0) {
            stag = 2;
        }
        else {
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
            x = (int)Math.floor(x/2.0);
            y = (int)Math.floor(y/2.0);
        }
        if (ht > max_ht) {
            MipavUtil.displayError("Cannot build pyramid higher than " +
                                   max_ht + " levels");
            error = 1;
            return null;
        }
        
        if (ht <= 0) {
            pyr.removeAllElements();
            pyr.add(im);
            intMat = new int[2];
            intMat[0] = imx;
            intMat[1] = imy;
            pind.removeAllElements();
            pind.add(intMat);
        } // if (ht <= 0)
        else {
            if (imy == 1) {
              lolo = corrDn(im, imx, imy, kernel, kernel.length, 1, edges, 2, 1, stag, 1, lolox, loloy);
              hihi = corrDn(im, imx, imy, hkernel, hkernel.length, 1, edges, 2, 1, 2, 1, hihix, hihiy);
            }
            else if (imx == 1) {
                lolo = corrDn(im, imx, imy, kernel, 1, kernel.length, edges, 1, 2, 1, stag, lolox, loloy);
                hihi = corrDn(im, imx, imy, hkernel, 1, hkernel.length, edges, 1, 2, 1, 2, hihix, hihiy);
            }
            else {
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
            
            npyr = buildWpyr(lolo, lolox[0], loloy[0], ht-1, filt, edges, nind);
            
            if ((imx == 1) || (imy == 1)) {
                pyr.removeAllElements();
                pyr.add(hihi);
                pyr.addAll(npyr);
                pind.removeAllElements();
                intMat = new int[2];
                intMat[0] = hihix[0];
                intMat[1] = hihiy[0];
                pind.add(intMat);
                pind.addAll(nind);
            }
            else {
                pyr.removeAllElements();
                pyr.add(lohi);
                pyr.add(hilo);
                pyr.add(hihi);
                pyr.addAll(npyr);
                pind.removeAllElements();
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
                pind.addAll(nind);
            }
        } // else
        
        return pyr;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  buffer  float [] input buffer to be sorted Sort an array into ascending numerical order by Shell's method
     *                 Reference: Numerical Recipes in C The Art of Scientific Computing Second Edition by William H.
     *                 Press, Saul A. Teukolsky, William T. Vetterling, Brian P. Flannery, pp. 331- 332.
     */
    private void shellSort(double[] buffer) {
        int i, j, inc;
        double v;
        inc = 1;

        int end = buffer.length;

        do {
            inc *= 3;
            inc++;
        } while (inc <= end);

        do {
            inc /= 3;

            for (i = inc + 1; i <= end; i++) {
                v = buffer[i - 1];
                j = i;

                while (buffer[j - inc - 1] > v) {
                    buffer[j - 1] = buffer[j - inc - 1];
                    j -= inc;

                    if (j <= inc) {
                        break;
                    }
                }

                buffer[j - 1] = v;
            }
        } while (inc > 1);

    }
    
    /**
     * This is a port of daubcqf.m by Ramesh Gopinath
     * %    [h_0,h_1] = daubcqf(N,TYPE); 
    %
    %    Function computes the Daubechies' scaling and wavelet filters
    %    (normalized to sqrt(2)).
    %
    %    Input: 
    %       N    : Length of filter (must be even)
    %       TYPE : Optional parameter that distinguishes the minimum phase,
    %              maximum phase and mid-phase solutions ('min', 'max', or
    %              'mid'). If no argument is specified, the minimum phase
    %              solution is used.
    %
    %    Output: 
    %       h_0 : Minimal phase Daubechies' scaling filter 
    %       h_1 : Minimal phase Daubechies' wavelet filter 
    %
    %    Example:
    %       N = 4;
    %       TYPE = 'min';
    %       [h_0,h_1] = daubcqf(N,TYPE)
    %       h_0 = 0.4830 0.8365 0.2241 -0.1294
    %       h_1 = 0.1294 0.2241 -0.8365 0.4830
    %
    %    Reference: "Orthonormal Bases of Compactly Supported Wavelets",
    %                CPAM, Oct.89 
    %
    
    %File Name: daubcqf.m
    %Last Modification Date: 01/02/96   15:12:57
    %Current Version: daubcqf.m 2.4
    %File Creation Date: 10/10/88
    %Author: Ramesh Gopinath  <ramesh@dsp.rice.edu>
    %
    %Copyright (c) 2000 RICE UNIVERSITY. All rights reserved.
    %Created by Ramesh Gopinath, Department of ECE, Rice University. 
    %
    %This software is distributed and licensed to you on a non-exclusive 
    %basis, free-of-charge. Redistribution and use in source and binary forms, 
    %with or without modification, are permitted provided that the following 
    %conditions are met:
    %
    %1. Redistribution of source code must retain the above copyright notice, 
    %   this list of conditions and the following disclaimer.
    %2. Redistribution in binary form must reproduce the above copyright notice, 
    %   this list of conditions and the following disclaimer in the 
    %   documentation and/or other materials provided with the distribution.
    %3. All advertising materials mentioning features or use of this software 
    %   must display the following acknowledgment: This product includes 
    %   software developed by Rice University, Houston, Texas and its contributors.
    %4. Neither the name of the University nor the names of its contributors 
    %   may be used to endorse or promote products derived from this software 
    %   without specific prior written permission.
    %
    %THIS SOFTWARE IS PROVIDED BY WILLIAM MARSH RICE UNIVERSITY, HOUSTON, TEXAS, 
    %AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, 
    %BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
    %FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL RICE UNIVERSITY 
    %OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
    %EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
    %PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
    %OR BUSINESS INTERRUPTIONS) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
    %WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
    %OTHERWISE), PRODUCT LIABILITY, OR OTHERWISE ARISING IN ANY WAY OUT OF THE 
    %USE OF THIS SOFTWARE,  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    %
    %For information on commercial licenses, contact Rice University's Office of 
    %Technology Transfer at techtran@rice.edu or (713) 348-6173

     * @param N
     * @param type
     * @return
     */
    private double[] daubcqf(int N, int type) {
        double h[] = null;
        int k;
        double a;
        double p[];
        double q[];
        double h_0[];
        double qt[];
        double h_1[];
        int j;
        double temp[];
        int i;
        double A[][];
        int n;
        EigenvalueDecomposition eig;
        double eigenvalue[];
        int qtl;
        int index;
        
        if ((N % 2) == 1) {
            MipavUtil.displayError("No Daubechies filter exists for odd length");
            error = 1;
            return null;
        }
        
        k = N/2;
        a = 1.0;
        p = new double[1];
        p[0] = 1.0;
        q = new double[1];
        q[0] = 1.0;
        h_0 = new double[2];
        h_0[0] = 1.0;
        h_0[1] = 1.0;
        for (j = 1; j <= k-1; j++) {
            a = -a * 0.25 * (j + k - 1.0)/j;
            temp = h_0;
            h_0 = new double[temp.length+1];
            h_0[0] = temp[0];
            for (i = 1; i < h_0.length-1; i++) {
                h_0[i] = temp[i-1] + temp[i];
            }
            h_0[h_0.length-1] = temp[h_0.length-2];
            temp = p;
            p = new double[temp.length+1];
            p[0] = temp[0];
            for (i = 1; i < p.length-1; i++) {
                p[i] = temp[i] - temp[i-1];
            }
            p[p.length-1] = -temp[p.length-2];
            temp = p;
            p = new double[temp.length+1];
            p[0] = temp[0];
            for (i = 1; i < p.length-1; i++) {
                p[i] = temp[i] - temp[i-1];
            }
            p[p.length-1] = -temp[p.length-2];
            temp = q;
            q = new double[temp.length+2];
            q[0] = a*p[0];
            for (i = 1; i < q.length-1; i++) {
                q[i] = temp[i-1] + a*p[i];
            }
            q[q.length-1] = a*p[q.length-1];
        } // for (j = 1; j <= k-1; j++)
        // Obtain the roots of polynomial of q
        n = q.length-1;
        A = new double[n][n];
        for (i = 0; i < n-1; i++) {
            A[i+1][i] = 1.0;
        }
        for (j = 0; j < n; j++) {
            A[0][j] = -q[j+1]/q[0];
        }
        eig = new EigenvalueDecomposition(new Matrix(A));
        eigenvalue = eig.getRealEigenvalues();
        shellSort(eigenvalue);
        qt = new double[k-1];
        for (i = 0; i < k-1; i++) {
            qt[i] = q[i];   
        }
        if (type == MID_PHASE) {
            if ((k %2) == 1) {
               qtl = 0;
               for (i = 0; i <= N-1; i+=4) {
                   qtl++;
               }
               for (i = 1; i <= N-1; i+=4) {
                   qtl++;
               }
               qt = new double[qtl];
               for (i = 0, index = 0; i < N-1; i+=4) {
                   qt[index++] = q[i];
               }
               for (i = 1; i <= N-1; i+=4) {
                   qt[index++] = q[i];
               }
            }
            else {
                qtl = 1;
                for (i = 3; i <= k-2; i+= 4) {
                    qtl++;
                }
                for (i = 4; i <= k-2; i+= 4) {
                    qtl++;
                }
                for (i = N-2; i >= k-1; i-= 4) {
                    qtl++;
                }
                for (i = N-3; i >= k-1; i-= 4) {
                    qtl++;
                }
                qt = new double[qtl];
                index = 0;
                qt[index++] = q[0];
                for (i = 3; i <= k-2; i+= 4) {
                    qt[index++] = q[i];
                }
                for (i = 4; i <= k-2; i+= 4) {
                    qt[index++] = q[i];
                }
                for (i = N-2; i >= k-1; i-= 4) {
                    qt[index++] = q[i];
                }
                for (i = N-3; i >= k-1; i-= 4) {
                    qt[index++] = q[i];
                }
            }
        } // if (type == MID_PHASE)
        return h;
        
    }
    
    /**
     * This is a port of buildWUpyr.m by JPM, Univ. de Granada
     * Construct a separable undecimated orthonormal QMF/wavelet pyramid
     * on matrix (or vector) im
     * @param im
     * @param imx First dimension of im
     * @param imy Second dimension of im
     * @param nScales  The number of pyramid levels to build
     * @param daubOrder  The order of the daubechies wavelet filter used
     * @param pind  N 2-element int[] containing the sizes of each subband
     * @return  A vector containing the N pyramid subbands, ordered from
     *          fine to coarse.
     */
    private Vector buildWUpyr(double im[], int imx, int imy, int nScales,
                              int daubOrder, Vector pind) {
        Vector pyr = null;
        double h[];
        
        if (nScales < 1) {
            MipavUtil.displayError("Number of scales must be >= 1");
            error = 1;
            return null;
        }
        
        // Fixed number of orientations
        nOrientations = 3;
        h = daubcqf(daubOrder, MINIMUM_PHASE);
        if (error == 1) {
            return null;
        }
        return pyr;
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
     * @param pind An N by 2 matrix containing the sizes of each subband.
     * @return pyr    A vector containing the N pyramid subbands, ordered from fine
     *                to coarse 
     */
    private Vector buildSFpyr(double[] im, int imx, int imy, int ht, int order, double twidth,
                              Vector pind) {
        int max_ht;
        int nbands;
        int ctrx;
        int ctry;
        double xramp[][];
        double yramp[][];
        double angle[][];
        double log_rad[][];
        int i, j;
        int index;
        double value;
        double logC;
        double xrcos[] = null;
        double yrcos[] = null;
        double yircos[];
        double lo0mask[][] = null;
        FFTUtility fftUtil;
        double imdftr[];
        double imdfti[];
        double lo0dftr[];
        double lo0dfti[];
        double hi0mask[][];
        double hi0dftr[];
        double hi0dfti[];
        Vector pyr;
        int intMat[];
        // log2(x) = loge(x)/loge(2)
        logC = 1.0/Math.log(2.0);
        max_ht = (int)Math.floor(logC*Math.log(Math.min(imx,imy)) + 2);
        
        if (ht > max_ht) {
            MipavUtil.displayError("Error! Cannot build pyramid higher than " + 
                                   max_ht + " levels");
            error = 1;
            return null;
        }
        
        if ((order > 15) || (order < 0)) {
            MipavUtil.displayWarning("Warning: ORDER must be an integer in the range 0 to 15");
            if (order < 0) {
                order = 0;
            }
            else if (order > 15) {
                order = 15;
            } 
        } // if ((order > 15) || (order < 0))
        nbands = order + 1;
        
        if (twidth <= 0) {
            MipavUtil.displayWarning("Warning: TWIDTH must be positive.  Setting to 1");
            twidth = 1;
        }
        ctrx = (int)Math.ceil((imx + 0.5)/2);
        ctry = (int)Math.ceil((imy + 0.5)/2);
        xramp = new double[imx][imy];
        yramp = new double[imx][imy];
        for (j = 0; j < imy; j++) {
            value = 2.0*((j+1) - ctry)/imy;
            for (i = 0; i < imx; i++) {
                xramp[i][j] = value;
            }
        }
        for (i = 0; i < imx; i++) {
            value = 2.0*((i + 1) - ctrx)/imx;
            for (j = 0; j < imy; j++) {
                yramp[i][j] = value;
            }
        }
        angle = new double[imx][imy];
        log_rad = new double[imx][imy];
        for (j = 0; j < imy; j++) {
            for (i = 0; i < imx; i++) {
                angle[i][j] = Math.atan2(yramp[i][j], xramp[i][j]);
                log_rad[i][j] = Math.sqrt(xramp[i][j]*xramp[i][j] + yramp[i][j]*yramp[i][j]);
            }
        }
        log_rad[ctrx-1][ctry-1] = log_rad[ctrx-1][ctry-2];
        // log2(log_rad) = loge(rad)/loge(2)
        for (j = 0; j < imy; j++) {
            for (i = 0; i < imx; i++) {
                log_rad[i][j] = logC*Math.log(log_rad[i][j]);
            }
        }
        
        // Radial transition function (a raised cosine in log-frequency)
        rcosFn(twidth, (-twidth/2.0), 0.0, 1.0, xrcos, yrcos);
        for (i = 0; i < yrcos.length; i++) {
            yrcos[i] = Math.sqrt(yrcos[i]);
        }
        
        yircos = new double[yrcos.length];
        for (i = 0; i < yrcos.length; i++) {
            value = yrcos[i];
            yircos[i] = Math.sqrt(1.0 - value*value);
        }
        lo0mask = pointOp(log_rad, yircos, xrcos[0],
                          (xrcos[1] - xrcos[0]), false);
        // forward FFT
        imdftr = new double[imx*imy];
        for (i = 0; i < im.length; i++) {
            imdftr[i] = im[i];    
        }
        imdfti = new double[imx*imy];
        fftUtil = new FFTUtility(imdftr, imdfti, imy, imx, 1, -1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(imdftr, imdfti, 1, imy, imx, -1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        center(imdftr, imdfti, imx, imy);
        lo0dftr = new double[imx*imy];
        lo0dfti = new double[imx*imy];
        for (j = 0; j < imy; j++) {
            for (i = 0; i < imx; i++) {
                index = i + j*imx;
                lo0dftr[index] = imdftr[index] * lo0mask[imx][imy];
                lo0dfti[index] = imdfti[index] * lo0mask[imx][imy];
            }
        }
        
        pyr = buildSFpyrLevs(lo0dftr, lo0dfti, imx, imy, log_rad, xrcos,
                             yrcos, angle, ht, nbands, pind);
        
        hi0mask = pointOp(log_rad, yrcos, xrcos[0], (xrcos[1] - xrcos[0]), false);
        hi0dftr = new double[imx*imy];
        hi0dfti = new double[imx*imy];
        for (j = 0; j < imy; j++) {
            for (i = 0; i < imx; i++) {
                index = i + j*imx;
                hi0dftr[index] = imdftr[index] * hi0mask[i][j];
                hi0dfti[index] = imdfti[index] * hi0mask[i][j];
            }
        }
        center(hi0dftr, hi0dfti, imx, imy);
        // Inverse FFT
        fftUtil = new FFTUtility(hi0dftr, hi0dfti, imy, imx, 1, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();
        fftUtil = new FFTUtility(hi0dftr, hi0dfti, 1, imy, imx, +1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
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
     * This is a port of a MATLAB function written by Eero Simoncelli, 5/97.
     * Recursive function for constructing levels of a steerable pyramid.
     * This is called by buildSFpyr, and is usually not called directly.
     * @param lodftr
     * @param lodfti
     * @param lodx
     * @param lody
     * @param lograd
     * @param xrcos
     * @param yrcos
     * @param angle
     * @param ht
     * @param nbands
     * @return
     */
    private Vector buildSFpyrLevs(double lodftr[], double lodfti[], int lodx, int lody, 
                                  double lograd[][], double xrcos[], double yrcos[],
                                  double angle[][], int ht, int nbands, Vector pind) {
        FFTUtility fftUtil;
        double bands[][];
        int bind[][];
        int logx;
        int logy;
        int i, j;
        int lutsize;
        double xcosn[];
        int order;
        double constant;
        double ycosn[];
        double himask[][];
        int b;
        double anglemask[][];
        double banddftr[];
        double banddfti[];
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
        double temp[][];
        double temp2[];
        double yircos[];
        double lomask[][];
        Vector pyr = new Vector();
        Vector npyr;
        Vector nind = null;
        pind = new Vector();
        logx = lograd.length;
        logy = lograd[0].length;
        long p, q;
        int intMat[];
        if (ht <= 0) {
            center(lodftr,lodfti, lodx, lody);
            // Inverse FFT
            fftUtil = new FFTUtility(lodftr, lodfti, lody, lodx, 1, +1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            fftUtil = new FFTUtility(lodftr, lodfti, 1, lody, lodx, +1, FFTUtility.FFT);
            fftUtil.setProgressBarVisible(false);
            fftUtil.run();
            fftUtil.finalize();
            pyr.add(lodftr);
            intMat = new int[2];
            intMat[0] = lodx;
            intMat[1] = lody;
            pind.add(intMat);
        } // if (ht <= 0)
        else {
            bands = new double[nbands][lodx*lody];
            bind = new int[nbands][2]; 
            
            for (j = 0; j < logy; j++) {
                for (i = 0; i < logx; i++) {
                    lograd[i][j] = lograd[i][j] + 1;
                }
            } // for (j = 0; j < logy; j++)
            
            lutsize = 1024;
            xcosn = new double[3*lutsize + 3];
            for (i = 0, j = -(2*lutsize+1); i < 3*lutsize + 3; i++, j++) {
                xcosn[i] = (Math.PI * j) / lutsize;
            }
            order = nbands - 1;
            // Divide by sqrt(sum_(n=0)^(N-1) cos(pi*n/N)^(2(N-1)) )
            p = factorial(order);
            q = factorial(2*order);
            constant = Math.pow(2.0,(2.0*order)) * (p*p) / (nbands * q);
            ycosn = new double[3*lutsize + 3];
            for (i = 0; i < ycosn.length; i++) {
                ycosn[i] = Math.sqrt(constant) * Math.pow(Math.cos(xcosn[i]), order);
            }
            himask = pointOp(lograd, yrcos, xrcos[0], (xrcos[1]-xrcos[0]), false);
            
            banddftr = new double[lodx*lody];
            banddfti = new double[lodx*lody];
            for (b = 1; b <= nbands; b++) {
                anglemask = pointOp(angle, ycosn, xcosn[0] + Math.PI*(b-1)/nbands,
                                    xcosn[1] - xcosn[0], true);
                if (((nbands-1) % 4) == 0) {
                    for (j = 0; j < lody; j++) {
                        for (i = 0; i < lodx; i++) {
                            index = i + j*lodx;
                            banddftr[index] = lodftr[index] * anglemask[i][j] * himask[i][j];
                            banddfti[index] = lodfti[index] * anglemask[i][j] * himask[i][j];
                        }
                    }
                } // if (((nbands-1) % 4) == 0)
                else if (((nbands-1) % 4) == 1) {
                    for (j = 0; j < lody; j++) {
                        for (i = 0; i < lodx; i++) {
                            index = i + j*lodx;
                            banddfti[index] = -lodftr[index] * anglemask[i][j] * himask[i][j];
                            banddftr[index] = lodfti[index] * anglemask[i][j] * himask[i][j];
                        }
                    }    
                } // else if (((nbands-1) % 4) == 1)
                else if (((nbands-1) % 4) == 2) {
                    for (j = 0; j < lody; j++) {
                        for (i = 0; i < lodx; i++) {
                            index = i + j*lodx;
                            banddftr[index] = -lodftr[index] * anglemask[i][j] * himask[i][j];
                            banddfti[index] = -lodfti[index] * anglemask[i][j] * himask[i][j];
                        }
                    }    
                } // else if (((nbands-1) % 4) == 2)
                else if (((nbands-1) % 4) == 3) {
                    for (j = 0; j < lody; j++) {
                        for (i = 0; i < lodx; i++) {
                            index = i + j*lodx;
                            banddfti[index] = lodftr[index] * anglemask[i][j] * himask[i][j];
                            banddftr[index] = -lodfti[index] * anglemask[i][j] * himask[i][j];
                        }
                    }    
                } // else if (((nbands-1) % 4) == 3)
                center(banddftr, banddfti, lodx, lody);
                // Inverse FFT
                fftUtil = new FFTUtility(banddftr, banddfti, lody, lodx, 1, +1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                fftUtil = new FFTUtility(banddftr, banddfti, 1, lody, lodx, +1, FFTUtility.FFT);
                fftUtil.setProgressBarVisible(false);
                fftUtil.run();
                fftUtil.finalize();
                
                for (i = 0; i < banddftr.length; i++) {
                    bands[b-1][i] = banddftr[i];
                }
                bind[b-1][0] = lodx;
                bind[b-1][1] = lody;
            } // for (b = 1; b <= nbands; b++)
            
            ctrx = (int)Math.ceil((lodx + 0.5)/2.0);
            ctry = (int)Math.ceil((lody + 0.5)/2.0);
            lodimsx = (int)Math.ceil((lodx - 0.5)/2.0);
            lodimsy = (int)Math.ceil((lody - 0.5)/2.0);
            loctrx = (int)Math.ceil((lodimsx + 0.5)/2.0);
            loctry = (int)Math.ceil((lodimsy + 0.5)/2.0);
            lostartx = ctrx - loctrx + 1;
            lostarty = ctry - loctry + 1;
            loendx = lostartx + lodimsx - 1;
            loendy = lostarty + lodimsy - 1;
            
            temp = new double[loendx - lostartx][loendy - lostarty];
            for (j = 0; j < loendy - lostarty; j++) {
                for (i = 0; i < loendx - lostartx; i++) {
                    temp[i][j] = lograd[lostartx - 1 + i][lostarty - 1 + j];
                }
            }
            lograd = null;
            lograd = temp;
            temp = new double[loendx - lostartx][loendy - lostarty];
            for (j = 0; j < loendy - lostarty; j++) {
                for (i = 0; i < loendx - lostartx; i++) {
                    temp[i][j] = angle[lostartx - 1 + i][lostarty - 1 + j];
                }
            }
            angle = null;
            angle = temp;
            temp2 = new double[(loendx - lostartx) * (loendy - lostarty)];
            for (j = 0; j < loendy - lostarty; j++) {
                for (i = 0; i < loendx - lostartx; i++) {
                    index = (lostartx - 1 + i) + lodx*(lostarty - 1 + j);
                    temp2[i + j*(loendx-lostartx)] = lodftr[index];
                }
            }
            lodftr = null;
            lodftr = temp2;
            temp2 = new double[(loendx - lostartx) * (loendy - lostarty)];
            for (j = 0; j < loendy - lostarty; j++) {
                for (i = 0; i < loendx - lostartx; i++) {
                    index = (lostartx - 1 + i) + lodx*(lostarty - 1 + j);
                    temp2[i + j*(loendx-lostartx)] = lodfti[index];
                }
            }
            lodfti = null;
            lodfti = temp2;
            yircos = new double[yrcos.length];
            for (i = 0; i < yrcos.length; i++) {
                yircos[i] = Math.abs(Math.sqrt(1.0 - yrcos[i]*yrcos[i]));        
            }
            lomask = pointOp(lograd, yircos, xrcos[0], (xrcos[1] - xrcos[0]), false);
            
            for (j = 0; j < loendy - lostarty; j++) {
                for (i = 0; i < loendx - lostartx; i++) {
                    index = i + j*(loendx-lostartx);
                    lodftr[index] = lomask[i][j] * lodftr[index];
                    lodfti[index] = lomask[i][j] * lodfti[index];
                }
            }
            
            npyr = buildSFpyrLevs(lodftr, lodfti, (loendx - lostartx), (loendy - lostarty),
                                  lograd, xrcos, yrcos, angle, ht-1, nbands, nind);
            
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
     * This is a port of pointOp.c by Eero Simoncelli, 7/96
     * Apply a point operation, specified by lookup table LUT to image array IM.
     * LUT must be a row or column vector, and is assumed to contain (equi-spaced)
     * samples of the function.  origin specifies the abscissa associated with the
     * first sample, and increment specifies the spacing between samples. Between-
     * sample values are estimated via linear interpolation.  If warnings is true,
     * the function outputs a warning whenever the lookup table is extrapolated.
     * The drawbacks are that the lookup table must be equi-spaced, and the 
     * interpolation is linear. 
     * @param im
     * @param lut
     * @param origin
     * @param increment
     * @param warnings
     * @return
     */
    private double[][] pointOp(double im[][], double lut[],
                               double origin, double increment, boolean warnings) {
        int imx = im.length;
        int imy = im[0].length;
        double res[][] = new double[imx][imy];
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
                    pos = (im[i][j] - origin) / increment;
                    index = (int)pos; // Floor
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
                    res[i][j] = lut[index] + (lut[index+1] - lut[index]) * (pos - index);
                } // for (i = 0; i < imx; i++)
            } // for (j = 0; j < imy; j++)
        } // if (increment > 0)
        else {
            for (j = 0; j < imy; j++) {
                for (i = 0; i < imx; i++) {
                    res[i][j] = lut[0];
                }
            }
        }
        return res;
    }
    
    /**
     * This is a port of a MATLAB function written by Eero Simoncelli, 7/96.
     * Returns a lookup table containing a "raised cosine" soft threshold function:
     * Y = value1 + (value2 - value1) * cos^2(PI/2 * (X - position + width)/ width)
     * 
     * @param width  width of the region over which the transition occurs (default = 1)
     * @param position the location of the center of the threshold (default = 0).
     * @param value1 Value to the left of the transition
     * @param value2 Value to the right of the transition
     * @param X
     * @param Y
     */
    private void rcosFn(double width, double position, double value1, double value2,
                        double X[], double Y[]) {
        int i, j;
        int sz = 256; // arbitrary
        double var;
        X = new double[sz+3];
        Y = new double[sz+3];
        for (i = -sz-1, j = 0; i <= 1; i++, j++) {
            X[j] = Math.PI * i / (2.0 * sz);
        }
        for (i = 0; i < X.length; i++) {
            var = Math.cos(X[i]);
            var = var*var;
            Y[i] = value1 + (value2 - value1) * var;
        }
        
        // Make sure end values are repeated, for extrapolation...
        Y[0] = Y[1];
        Y[sz+2] = Y[sz+1];
        
        for (i = 0; i < X.length; i++) {
            X[i] = position + (2.0*width/Math.PI) * (X[i] + Math.PI/4.0);
        }
        
        return;
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
     * @param  xDim
     * @param  yDim
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
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        historyString = new String("DenoisingBLS_GSM(" + ")\n");
    }

    
    
}
