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
        double imagArray[];
        pyr = buildSFpyr(fn, fnx, fny, nScales, nOrientations-1, 1.0, pind);
        if (error == 1) {
            return null;
        }
        pyrN = buildSFpyr(noise, noisex, noisey, nScales, nOrientations-1, 1.0, pind);
        if (error == 1) {
            return null;
        }
        pyrh = pyr;
        bandNum = pind.size()/2;
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
                imagArray = new double[aux.length];
                expand(aux, 2.0, nsxn[0], nsyn[0], aux, imagArray);
            } // if (prnt)
        } // for (nband = 1; nband <= bandNum-1; nband++)
        return fh;
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
        ter = new double[mx*my];
        tei = new double[mx*my];
        
        //forward FFT
        tr = new double[mx*my];
        ti = new double[mx*my];
        for (i = 0; i < tr.length; i++) {
            tr[i] = t[i];
        }
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
    private double[] pyrBand(Vector pyr, Vector pind, int band, int rows[], int columns[]) {
        double arr[];
        
        arr = (double[])pyr.get(band); 
        rows[0] = ((Integer)pind.get(2*band)).intValue();
        columns[0] = ((Integer)pind.get(2*band+1)).intValue();
        return arr;
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
        pind.insertElementAt(new Integer(imy), 0);
        pind.insertElementAt(new Integer(imx), 0);
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
        Integer bind[][];
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
            pind.add(new Integer(lodx));
            pind.add(new Integer(lody));
        } // if (ht <= 0)
        else {
            bands = new double[nbands][lodx*lody];
            bind = new Integer[nbands][2]; 
            
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
                bind[b-1][0] = new Integer(lodx);
                bind[b-1][1] = new Integer(lody);
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
                for (j = 0; j < 2; j++) {
                    pind.add(bind[i][j]);
                }
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
