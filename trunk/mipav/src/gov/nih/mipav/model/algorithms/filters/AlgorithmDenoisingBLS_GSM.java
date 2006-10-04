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
    
    private int newDimLengths[];
    
    private int newArrayLength;
    
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
        int pind[][] = null;
        Vector pyr = null;
        Vector pyrN = null;
        pyr = buildSFpyr(fn, fnx, fny, nScales, nOrientations-1, 1.0, pind);
        if (error == 1) {
            return null;
        }
        pyrN = buildSFpyr(noise, noisex, noisey, nScales, nOrientations-1, 1.0, pind);
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
    private Vector buildSFpyr(double[] im, int imx, int imy, int ht, int order, double twidth,
                              int indices[][]) {
        int max_ht;
        int nbands;
        int ctrx;
        int ctry;
        double xramp[][];
        double yramp[][];
        double angle[][];
        double log_rad[][];
        int i, j;
        double value;
        double logC;
        double xrcos[] = null;
        double yrcos[] = null;
        double yircos[];
        Vector pyr = new Vector();
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
        return pyr;
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
     */
    private void center(double[] rData, double[] iData) {

        // center() is called after the forward fast fourier transform to enhance the display
        // center() is called before the inverse fast fourier transform to return the data
        // to its original ordering.
        int i, j;
        int xnew, ynew;
        int xdimHalf, ydimHalf;
        double[] centerData;
        int newLength;

        if (image25D) {
            newLength = newDimLengths[0] * newDimLengths[1];
        } else {
            newLength = newArrayLength;
        }

        

            try {
                centerData = new double[newDimLengths[0] * newDimLengths[1]]; // Temp storage for centered
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmDenoisingBLS_GSM: Out of memory creating centerData");
                setCompleted(false);

                return;
            }

            xdimHalf = newDimLengths[0] / 2;
            ydimHalf = newDimLengths[1] / 2;

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

        
    } // center


    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        historyString = new String("DenoisingBLS_GSM(" + ")\n");
    }

    
    
}
