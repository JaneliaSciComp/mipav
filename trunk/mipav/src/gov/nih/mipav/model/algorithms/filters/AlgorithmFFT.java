package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;

import java.io.*;

import java.util.*;


/**
 * AlgorithmFFT.java.
 *
 * @author  William Gandler and Matthew J. McAuliffe Processing images by filtering in the frequency domain is a 3 step
 *          process: 1.) Performing a forward fast fourier transform to convert a spatial image into its frequency
 *          image. 2.) Enhancing some frequency components of the image and attenuating other frequency components of
 *          the image by mulitplying by a lowpass, highpass, bandpass, or bandstop filter. Frequency filters may be
 *          constructed with 1 of 3 methods - finite impulse response filters constructed with Hamming windows, Gaussian
 *          filters, and Butterworth filters. However, for the Gaussian filters only lowpass and highpass filters are
 *          available. 3.) Performing an inverse fast fourier transform to convert from the frequency domain back into
 *          the spatial domain. This software module performs all 3 steps of this process, but only performs one step
 *          after a dialog OK. The AlgorithmFrequencyFilter module performs all 3 steps after a dialog OK.
 *
 *          <p>The module also creates a Gabor filter, which is essentially a tilted Gaussian with 2 unequal axes at an
 *          offset (freqU, freqV) from the origin. A Gabor filter only responds to a texture having both a particular
 *          frequency and a particular orientation. Note that a filter and its mirror image reflected across the u and v
 *          frequency axes produce identical frequency responses.</p>
 *
 *          <p>The core algorithm of this module, the fast fourier transform algorithm found in exec(), requires that
 *          all the dimensions of an N-dimensional dataset be powers of 2. To be able to use this algorithm on datasets
 *          with arbitrary dimensions, the data is zero padded to powers of 2 before applying the forward fast fourier
 *          transform and stripped down to the original dimensions after applying the inverse fast fourier transform. If
 *          unequalDim is equal to true, the fourier pictures are allowed to have unequal dimensions so as to save
 *          memory. If unequalDim is equal to false, the pictures have equal dimensions so as to maximize symmetry.</p>
 *
 *          <p>The typical full sequence is as follows: 1.) Data from a real spatial image is exported into a float
 *          array realData. 2.) An equally sized float array called imagData is created and filled with zeros. 3.)
 *          realData and imagData are enlarged to the same length in every dimension. If finite impulse repsonse filters
 *          are constructed with Hamming windows and no cropping, the new dimension size is equal to the minimum power
 *          of two number that equals or exceeds the maximum original dimension size + kDim - 1, where kDim is the
 *          diameter of a circular or spherical convolution kernel. If finite impulse response filters with Hamming
 *          windows and cropping or infinite impulse response Gaussian or Butterworth filters are used, the new
 *          dimension size is equal to the minimum power of two number that equals or exceeds the maximum original
 *          dimension size. The data is padded with zeros at the end of each dimension. 4.) exec() is invoked to run the
 *          fast fourier transform algorithm. 5.) The center() algorithm is invoked to reorder the data for display. The
 *          Fourier transform of real data is conjugate symmetric; the real parts are even and the imaginary parts are
 *          odd. In other words, the magnitude is even and the phase is odd. Since images display the magnitude or
 *          log10(1 + magnitude) a symmetry reflected across the center of the image is observed. 6.) The complex data
 *          is imported to the display image with the image minimum and maximum being calculated. If logMagDisplay is
 *          set equal to false, the minimum and maximum of sqrt(realData(i)*realData(i) + imagData(i)*imagData(i)) will
 *          be used as the image minimum and maximum in other modules. If logMagDisplay is set equal to true, the
 *          log10(1 + sqrt(realData(i)*realData(i) + imagData(i)*imagData(i))) will be used as the image minimum and
 *          maximum in other modules. Note that while this module stores the complex data in the source or destination
 *          image, the display oriented modules in the MIPAV package will use the magnitude or log magnitude as
 *          indicated above in the screen displays since complex numbers cannot be directly displayed. 7.) The command
 *          setOriginalExtents is invoked so that the original dimensions of the source image are available along with
 *          the display image. The original dimensions are needed so that the original source dimensions can be restored
 *          after the inverse fast fourier transform. If finite impulse response filters with windows are used: 8a.) An
 *          ideal filter kernel is constructed in the spatial domain. 9a.) A Hamming window kernel is constructed in the
 *          spatial domain. 10a.) The ideal kernel and the Hamming window kernel are multiplied together. 11a.) The
 *          kernel is zero padded up to the same dimensions that the image data was. 12a.) exec() is run to obtain the
 *          FFT of the kernel. 13a.) The center() algorithm is run to reorder the data. If Gaussian or Butterworth
 *          filters are used: For Gaussian and Butterworth filters the transfer functions affect the real and imaginary
 *          parts of the FFT of the image in exactly the same manner. These filters are zero-phase- shift filters
 *          because they do not alter the phase of the transform. Since these filters are conjugate symmetric, the
 *          inverse FFTs of these filters are purely real. Since this filtering is equivalent to convolving to real 2D
 *          data sets, the result must be purely real. 8b.) The real part of the FFT is set equal to the appropriate
 *          filter magnitude and the imaginary part is set equal to zero. This filter has the same dimensions as the
 *          padded image data. 14) The data FFT is set equal to the product of the data FFT and the filter FFT. 15.) The
 *          inverse FFT process is invoked. The complex data is exported into the 2 float arrays realData and imagData.
 *          There should be no need for zero padding at this point since the dimensions should already be all powers of
 *          2 from before. 16.) The center() algorithm is invoked to restore the data to its original ordering. 17.)
 *          exec() is invoked to run the inverse fast fourier transform algorithm. 18.) The realData now holds the
 *          correct response if Gaussian or Butterworth filtering was used. If FIR filtering with windows was used then
 *          realData holds a version of the correct response shifted by (kDim - 1)/2 toward the end of each dimension.
 *          The imagData should contain only roundoff error. 19.) For FIR filters shift the data back by (kDim - 1)/2
 *          toward the start of each dimension. 20.) Stripping is performed to return the image to its original
 *          dimensions. 21.) The realData() is imported into the the new spatial image.</p>
 *
 *          <p>Methods included calculate the magnitude and phase as shown below:</p>
 *
 *          <p>magnitude = ( (realData)^2 + (imagData)^2 )^(1/2); phase = arctan(imagData/realData);</p>
 */

public class AlgorithmFFT extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int INVERSE = -1;

    /** DOCUMENT ME! */
    public static final int FILTER = 0;

    /** DOCUMENT ME! */
    public static final int FORWARD = 1;

    /** DOCUMENT ME! */
    public static final int LOWPASS = 1;

    /** DOCUMENT ME! */
    public static final int HIGHPASS = 2;

    /** DOCUMENT ME! */
    public static final int BANDPASS = 3;

    /** DOCUMENT ME! */
    public static final int BANDSTOP = 4;

    /** DOCUMENT ME! */
    public static final int WINDOW = 1;

    /** DOCUMENT ME! */
    public static final int GAUSSIAN = 2;

    /** DOCUMENT ME! */
    public static final int BUTTERWORTH = 3;

    /** DOCUMENT ME! */
    public static final int GABOR = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Size of buffers (realData and imagData). */
    private int arrayLength;

    /** GAUSSIAN = 2, BUTTERWORTH = 3. */
    private int butterworthOrder; // order of the Butterworth filter

    // User inputs f1 and f2 from 0.0 to 1.0.  Program multiplies
    // these numbers by PI for FIR filters and by (newLength - 1)/2
    // for Gaussian and Butterworth filters.
    /** WINDOW = 1 for windowed finite impulse response. */
    private int constructionMethod;

    /** Dimension sizes. */
    private int[] dimLengths;

    /** Exceeds the smallest power of 2 number >= the largest dimension. */
    private boolean doCrop; // true if cropping is actually performed

    /** if image cropping the ending index in a dimension. */
    private int[] end;

    /** Cutoff frequency in LOWPASS and HIGHPASS. */
    private float f1;

    /** Lower frequency in BANDPASS and BANDSTOP. */
    private float f2; // higher frequency in BANDPASS and BANDSTOP

    /** LOWPASS, HIGHPASS, BANDPASS, or BANDSTOP. */
    private int filterType;

    /** Final data. */
    private float[] finalData;

    /** Variables used in Gabor filter. */
    private float freqU; // Frequency along U axis before rotation by theta range -1 to 1

    /** DOCUMENT ME! */
    private float freqV; // Frequency along V axis before rotation by theta range -1 to 1

    /** (kernel diameter - 1)/2. */
    private int halfKDim;

    /** Ideal kernel data. */
    private float[] iKernel;


    /** Imaginary data. */
    private float[] imagData;

    /** If true process each slice one at a time in 3D image. */
    private boolean image25D;


    /** If true crop image if largest image dimension + kDim - 1. */
    private boolean imageCrop;


    /** Imaginary kernel data. */
    private float[] imagKernelData;

    /** Kernel diameter. */
    private int kDim;

    /** If true display log10 of 1 + magnitude. */
    private boolean logMagDisplay;

    /** Magnitude data. */
    private float[] magData;

    /** DOCUMENT ME! */
    private float minimum, maximum;

    /** Number of dimensions. */
    private int ndim; //

    /** Size of zero padded buffers. */
    private int newArrayLength;

    /** Zero padded dimension sizes. */
    private int[] newDimLengths;

    /** Length of every new array dimension. */
    private int newLength;

    /** DOCUMENT ME! */
    private int newSliceSize;

    /** Size of original buffers (realData and imagData). */
    private int originalArrayLength; //

    /** Original dimension sizes. */
    private int[] originalDimLengths;

    /** Phase data. */
    private float[] phaseData;


    /** Real data. */
    private float[] realData;

    /** Real kernel data. */
    private float[] realKernelData;

    /** DOCUMENT ME! */
    private float sigmaU; // Standard deviation along prerotated U axis

    /** DOCUMENT ME! */
    private float sigmaV; // Standard deviation along prerotated V axis

    /** If image cropping the starting index in a dimension. */
    private int[] start;

    /** DOCUMENT ME! */
    private float theta; // Rotation in radians;

    /** Transform direction. */
    private int transformDir;

    /** If true allow unequal FFT dimensions. */
    private boolean unequalDim;

    /** Window kernel data. */
    private float[] wKernel;

    /** True if zero padding actually performed. */
    private boolean zeroPad;
    
    private boolean doSelfTest = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Used for GABOR filters.
     *
     * @param  srcImg         source image model
     * @param  transformDir   indicates transform direction; 1 = forward FFT, 0 = frequency filter -1 = inverse FFT
     * @param  logMagDisplay  if true display log10 of 1 + magnitude
     * @param  unequalDim     if true allow unequal FFT dimensions
     * @param  freqU          Frequency along U axis before rotation by theta range -1 to 1
     * @param  freqV          Frequency along V axis before rotation by theta range -1 to 1
     * @param  sigmaU         Destination image model
     * @param  sigmaV         Standard deviation along prerotated V axis
     * @param  theta          Rotation in radians
     */
    public AlgorithmFFT(ModelImage srcImg, int transformDir, boolean logMagDisplay, boolean unequalDim, float freqU,
                        float freqV, float sigmaU, float sigmaV, float theta) {

        super(null, srcImg);

        this.transformDir = transformDir;
        this.logMagDisplay = logMagDisplay;
        srcImage.setLogMagDisplay(logMagDisplay);

        if (transformDir == FORWARD) {
            this.unequalDim = unequalDim;
            srcImage.setUnequalDim(unequalDim);
        } else if (transformDir == FILTER) {
            this.unequalDim = srcImage.getUnequalDim();
        } else if (transformDir == INVERSE) {
            this.unequalDim = srcImage.getUnequalDim();
        }

        if (transformDir == FORWARD) {

            if (srcImage.getNDims() > 2) {
                this.image25D = true;
            } else {
                this.image25D = false;
            }

            srcImage.setImage25D(image25D);
        } else if (transformDir == FILTER) {
            this.image25D = srcImage.getImage25D();
        } else if (transformDir == INVERSE) {
            this.image25D = srcImage.getImage25D();
        }

        this.imageCrop = false;
        this.freqU = freqU;
        this.freqV = freqV;
        this.sigmaU = sigmaU;
        this.sigmaV = sigmaV;
        this.theta = theta;

        if (transformDir == FORWARD) {
            this.constructionMethod = GABOR;
            srcImage.setOriginalFilterConstruction(GABOR);
        } else if (transformDir == FILTER) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
        } else if (transformDir == INVERSE) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
        }

        if (transformDir == FILTER) {
            srcImage.setFreqU(freqU);
            srcImage.setFreqV(freqV);
            srcImage.setSigmaU(sigmaU);
            srcImage.setSigmaV(sigmaV);
            srcImage.setTheta(theta);
        } // if (transformDir == FILTER)
    }

    /**
     * Used for GABOR filters.
     *
     * @param  destImg        image model where result image is to be stored
     * @param  srcImg         source image model
     * @param  transformDir   indicates transform direction; 1 = forward FFT, 0 = frequency filter, -1 = inverse filter
     * @param  logMagDisplay  if true display log10 of 1 + magnitude
     * @param  unequalDim     if true allow unequal FFT dimensions
     * @param  freqU          Frequency along U axis before rotation by theta range -1 to 1
     * @param  freqV          Frequency along V axis before rotation by theta range -1 to 1
     * @param  sigmaU         Standard deviation along prerotated U axis
     * @param  sigmaV         Standard deviation along prerotated V axis
     * @param  theta          Rotation in radians
     */
    public AlgorithmFFT(ModelImage destImg, ModelImage srcImg, int transformDir, boolean logMagDisplay,
                        boolean unequalDim, float freqU, float freqV, float sigmaU, float sigmaV, float theta) {

        super(destImg, srcImg);

        this.transformDir = transformDir;
        this.logMagDisplay = logMagDisplay;
        destImage.setLogMagDisplay(logMagDisplay);

        if (transformDir == FORWARD) {
            this.unequalDim = unequalDim;
            destImage.setUnequalDim(unequalDim);
        } else if (transformDir == FILTER) {
            this.unequalDim = srcImage.getUnequalDim();
            destImage.setUnequalDim(unequalDim);
        } else if (transformDir == INVERSE) {
            this.unequalDim = srcImage.getUnequalDim();
        }

        if (transformDir == FORWARD) {

            if (srcImg.getNDims() > 2) {
                this.image25D = true;
            } else {
                this.image25D = false;
            }

            destImage.setImage25D(image25D);
        } else if (transformDir == FILTER) {
            this.image25D = srcImage.getImage25D();
            destImage.setImage25D(image25D);
        } else if (transformDir == INVERSE) {
            this.image25D = srcImage.getImage25D();
        }

        this.imageCrop = false;
        this.freqU = freqU;
        this.freqV = freqV;
        this.sigmaU = sigmaU;
        this.sigmaV = sigmaV;
        this.theta = theta;

        if (transformDir == FORWARD) {
            this.constructionMethod = GABOR;
            destImage.setOriginalFilterConstruction(constructionMethod);
        } else if (transformDir == FILTER) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
            destImage.setOriginalFilterConstruction(this.constructionMethod);
        } else if (transformDir == INVERSE) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
        }

        if (transformDir == FILTER) {
            destImage.setFreqU(freqU);
            destImage.setFreqV(freqV);
            destImage.setSigmaU(sigmaU);
            destImage.setSigmaV(sigmaV);
            destImage.setTheta(theta);
        } // if (transformDir == FILTER)
    }

    /**
     * Creates a new AlgorithmFFT object.
     *
     * @param  srcImg              source image model
     * @param  transformDir        indicates transform direction; 1 = forward FFT, 0 = frequency filter -1 = inverse FFT
     * @param  logMagDisplay       if true display log10 of 1 + magnitude
     * @param  unequalDim          if true allow unequal FFT dimensions
     * @param  image25D            if true process one slice at a time in 3D image
     * @param  imageCrop           if true crop image if largest image dimension + kDim - 1 exceeds the smallest power
     *                             of 2 number >= the largest dimension
     * @param  kernelDiameter      convolution kernel diameter - must be an odd integer >= 3
     * @param  filterType          LOWPASS, HIGHPASS, BANDPASS, or BANDSTOP
     * @param  freq1               cutoff frequency in LOWPASS and HIGHPASS lower frequency in BANDPASS and BANDSTOP
     * @param  freq2               higher frequency in BANDPASS and BANDSTOP User inputs f1 and f2 going from 0.0 to
     *                             1.0. Program multiplies these numbers by PI for FIR filters and by (newLength - 1)/2
     *                             for Gaussian and Butterworth filters.
     * @param  constructionMethod  WINDOW for window finite impulse response, GAUSSIAN, or BUTTERWORTH
     * @param  butterworthOrder    order of a Butterworth filter
     */
    public AlgorithmFFT(ModelImage srcImg, int transformDir, boolean logMagDisplay, boolean unequalDim,
                        boolean image25D, boolean imageCrop, int kernelDiameter, int filterType, float freq1,
                        float freq2, int constructionMethod, int butterworthOrder) {

        super(null, srcImg);

        this.transformDir = transformDir;
        this.logMagDisplay = logMagDisplay;
        srcImage.setLogMagDisplay(logMagDisplay);

        if (transformDir == FORWARD) {
            this.unequalDim = unequalDim;
            srcImage.setUnequalDim(unequalDim);
        } else if (transformDir == FILTER) {
            this.unequalDim = srcImage.getUnequalDim();
        } else if (transformDir == INVERSE) {
            this.unequalDim = srcImage.getUnequalDim();
        }

        if (transformDir == FORWARD) {
            this.image25D = image25D;
            srcImage.setImage25D(image25D);
        } else if (transformDir == FILTER) {
            this.image25D = srcImage.getImage25D();
        } else if (transformDir == INVERSE) {
            this.image25D = srcImage.getImage25D();
        }

        this.imageCrop = imageCrop;

        if (transformDir == FORWARD) {
            kDim = kernelDiameter;
            srcImage.setOriginalKernelDimension(kDim);
        } else if (transformDir == FILTER) {
            kDim = srcImage.getOriginalKernelDimension();
        } else if (transformDir == INVERSE) {
            kDim = srcImage.getOriginalKernelDimension();
        }

        this.filterType = filterType;
        f1 = freq1;
        f2 = freq2;

        if (transformDir == FORWARD) {
            this.constructionMethod = constructionMethod;
            srcImage.setOriginalFilterConstruction(constructionMethod);
            this.butterworthOrder = butterworthOrder;
            srcImage.setOriginalButterworthOrder(butterworthOrder);
        } else if (transformDir == FILTER) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
            this.butterworthOrder = srcImage.getOriginalButterworthOrder();
        } else if (transformDir == INVERSE) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
        }

        if (transformDir == FILTER) {
            destImage.setFilterType(filterType);

            if (constructionMethod == WINDOW) {
                srcImage.setFreq1((float) (f1 / Math.PI));
            } else {
                srcImage.setFreq1(f1);
            }

            if ((filterType == BANDPASS) || (filterType == BANDSTOP)) {

                if (constructionMethod == WINDOW) {
                    srcImage.setFreq2((float) (f2 / Math.PI));
                } else {
                    srcImage.setFreq2(f2);
                }
            }
        } // if (transformDir == FILTER)
    }

    /**
     * Creates a new AlgorithmFFT object.
     *
     * @param  destImg             image model where result image is to be stored
     * @param  srcImg              source image model
     * @param  transformDir        indicates transform direction; 1 = forward FFT, 0 = frequency filter, -1 = inverse
     *                             filter
     * @param  logMagDisplay       if true display log10 of 1 + magnitude
     * @param  unequalDim          if true allow unequal FFT dimensions
     * @param  image25D            if true process each slice one at a time in 3D image
     * @param  imageCrop           if true crop image if largest image dimension + kDim - 1 exceeds the smallest power
     *                             of 2 number >= the largest dimension
     * @param  kernelDiameter      convolution kernel diameter - must be an odd integer >= 3
     * @param  filterType          LOWPASS, HIGHPASS, BANDPASS, or BANDSTOP
     * @param  freq1               cutoff frequency in LOWPASS and HIGHPASS lower frequency in BANDPASS and BANDSTOP
     * @param  freq2               higher frequency in BANDPASS and BANDSTOP User inputs f1 and f2 going from 0.0 to
     *                             1.0. Program multiplies these numbers by PI for FIR filters and by (newLength - 1)/2
     *                             for Gaussian and Butterworth filters.
     * @param  constructionMethod  WINDOW for window finite impulse response, GAUSSIAN, or BUTTERWORTH
     * @param  butterworthOrder    order of the Butterworth filter
     */
    public AlgorithmFFT(ModelImage destImg, ModelImage srcImg, int transformDir, boolean logMagDisplay,
                        boolean unequalDim, boolean image25D, boolean imageCrop, int kernelDiameter, int filterType,
                        float freq1, float freq2, int constructionMethod, int butterworthOrder ) {

        super(destImg, srcImg);

        this.transformDir = transformDir;
        this.logMagDisplay = logMagDisplay;
        destImage.setLogMagDisplay(logMagDisplay);

        if (transformDir == FORWARD) {
            this.unequalDim = unequalDim;
            destImage.setUnequalDim(unequalDim);
        } else if (transformDir == FILTER) {
            this.unequalDim = srcImage.getUnequalDim();
            destImage.setUnequalDim(unequalDim);
        } else if (transformDir == INVERSE) {
            this.unequalDim = srcImage.getUnequalDim();
        }

        if (transformDir == FORWARD) {
            this.image25D = image25D;
            destImage.setImage25D(image25D);
        } else if (transformDir == FILTER) {
            this.image25D = srcImage.getImage25D();
            destImage.setImage25D(image25D);
        } else if (transformDir == INVERSE) {
            this.image25D = srcImage.getImage25D();
        }

        this.imageCrop = imageCrop;

        if (transformDir == FORWARD) {
            kDim = kernelDiameter;
            destImage.setOriginalKernelDimension(kDim);
        } else if (transformDir == FILTER) {
            kDim = srcImage.getOriginalKernelDimension();
            destImage.setOriginalKernelDimension(kDim);
        } else if (transformDir == INVERSE) {
            kDim = srcImage.getOriginalKernelDimension();
        }

        this.filterType = filterType;
        f1 = freq1;
        f2 = freq2;

        if (transformDir == FORWARD) {
            this.constructionMethod = constructionMethod;
            destImage.setOriginalFilterConstruction(constructionMethod);
            this.butterworthOrder = butterworthOrder;
            destImage.setOriginalButterworthOrder(butterworthOrder);
        } else if (transformDir == FILTER) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
            destImage.setOriginalFilterConstruction(this.constructionMethod);
            this.butterworthOrder = srcImage.getOriginalButterworthOrder();
        } else if (transformDir == INVERSE) {
            this.constructionMethod = srcImage.getOriginalFilterConstruction();
        }

        if (transformDir == FILTER) {
            destImage.setFilterType(filterType);

            if (constructionMethod == WINDOW) {
                destImage.setFreq1((float) (f1 / Math.PI));
            } else {
                destImage.setFreq1(f1);
            }

            if ((filterType == BANDPASS) || (filterType == BANDSTOP)) {

                if (constructionMethod == WINDOW) {
                    destImage.setFreq2((float) (f2 / Math.PI));
                } else {
                    destImage.setFreq2(f2);
                }
            }
        } // if (transformDir == FILTER)
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepare this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Returns reference to imaginary data array.
     *
     * @return  the reference the the imaginary datat array
     */
    public float[] getImaginaryData() {
        return imagData;
    }

    /**
     * Returns reference to magnitude data array.
     *
     * @return  the reference to the magnitude data array
     */
    public float[] getMagData() {
        return magData;
    }

    /**
     * Returns reference to phase data array.
     *
     * @return  the reference to the phase data array
     */
    public float[] getPhaseData() {
        return phaseData;
    }

    /**
     * Returns reference to real data array.
     *
     * @return  the reference the the real datat array
     */
    public float[] getRealData() {
        return realData;
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        
        if (doSelfTest) {
            selfTest();
        }
        else if (destImage != null) {
            calcStoreInDest();
        } else {
            calcInPlace();
        }
    }
    
    private void selfTest() {
        float a[] = null;
        float b[] = null;
        float c[] = null;
        float d[] = null;
        float newA[] = null;
        float newC[] = null;
        int FORWARD_INVERSE = 1;
        int SPATIAL_SHIFT = 2;
        int FREQUENCY_SHIFT = 3;
        int x, y, z;
        int nDims = 2;
        int extents[] = null;
        int sliceSize = 0;
        int newExtents[] = null;
        int nTests = 128;
        int i, j;
        int arrayLength;
        int imageType = ModelStorageBase.FLOAT;
        ModelImage forwardImage;
        boolean createNewImage = true;
        ModelImage resultImage = null;
        ModelImage inverseImage = null;
        int transformDir;
        boolean logMagDisplay = true;
        boolean unequalDim = true;;
        boolean image25D = false;
        boolean imageCrop = false;
        int kernelDiameter = 15;
        int constructionMethod = WINDOW;
        int butterworthOrder = 1;
        float freq1 = 0.4f;
        float freq2 = 0.7f;
        AlgorithmFFT FFTAlgo;
        double error[];
        boolean  doCrop = false;
        int dimTest;
        int newLength;
        int start[] = null;
        int end[] = null;
        int newArrayLength;
        int u;
        int v;
        int w;
        int testType;
        RandomNumberGen randomGen = new RandomNumberGen();
        ViewUserInterface UI = ViewUserInterface.getReference();
        boolean foundError[] = new boolean[nTests];
        int errorsFound = 0;
        
        // The first 120 perform a forward transform, then the inverse, and verify that the result is
        // the same as the original data
        // The first 60 tests create a destImage
        // Tests 61 thru 120 only use the srcImage
        // Tests 121 thru 124 verify the spatial translation property
        // f(x - extents[0]/2, y - extents[1]/2) <=> F(u, v) * ((-1)**(u + v))
        // Tests 125 thru 128 verify the frequency translation property
        // f(x,y) * ((-1)**(x+y)) <=> F(u - extents[0]/2, v - extents[1]/2)
        createNewImage = true;
        testType = FORWARD_INVERSE;
        for (i = 0; i < nTests; i++) {
            if (i == 60) {
                createNewImage = false;
            }
            UI.setDataText("Running test = " + i + "\n");
            if ((i == 0) || (i == 60)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                imageCrop = true;
            } // if ((i == 0) || ( i == 60))
            else if ((i == 1) || (i == 61)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                imageCrop = false;    
            }
            else if ((i == 2) || (i == 62)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                image25D = false;
                imageCrop = true;
            }
            else if ((i == 3) || (i == 63)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                image25D = false; 
                imageCrop = false;
            }
            else if ((i == 4) || (i == 64)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = GAUSSIAN;  
            }
            else if ((i == 5) || (i == 65)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = GAUSSIAN;
                image25D = false;
            }
            else if ((i == 6) || (i == 66)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = BUTTERWORTH;
                butterworthOrder = 1;
            }
            else if ((i == 7) || (i == 67)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                image25D = false;
            }
            else if ((i == 8) || (i == 68)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                imageCrop = true;
            } // else if ((i == 8) || (i == 68))
            else if ((i == 9) || (i == 69)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                imageCrop = false;    
            }
            else if ((i == 10) || (i == 70)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = false;
                imageCrop = true;
            }
            else if ((i == 11) || (i == 71)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = false;
                imageCrop = false;    
            }
            else if ((i == 12) || (i == 72)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = GAUSSIAN; 
                unequalDim = true;
            }
            else if ((i == 13) || (i == 73)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = GAUSSIAN;
                unequalDim = true;
                image25D = false;
            }
            else if ((i == 14) || (i == 74)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = BUTTERWORTH;
                butterworthOrder = 1;
                unequalDim = true;
            }
            else if ((i == 15) || (i == 75)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = true;
                image25D = false;
            }
            else if ((i == 16) || (i == 76)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                imageCrop = true;
            } // if ((i == 16) || (i == 76))
            else if ((i == 17) || (i == 77)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                imageCrop = false;    
            }
            else if ((i == 18) || (i == 78)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = false;
                imageCrop = true;
            }
            else if ((i == 19) || (i == 79)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 32;
                extents[1] = 64;
                extents[2] = 128;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = false;
                imageCrop = false;    
            }
            else if ((i == 20) || (i == 80)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = GAUSSIAN; 
                unequalDim = false;
            }
            else if ((i == 21) || (i == 81)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = GAUSSIAN;
                unequalDim = false;
                image25D = false;
            }
            else if ((i == 22) || (i == 82)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 256;
                constructionMethod = BUTTERWORTH;
                butterworthOrder = 1;
                unequalDim = false;
            }
            else if ((i == 23) || (i == 83)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = false;
                image25D = false;
            }
            else if ((i == 24) || (i == 84)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                imageCrop = true;
            } // else if ((i == 24) || (i == 84))
            else if ((i == 25) || (i == 85)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                imageCrop = false;    
            }
            else if ((i == 26) || (i == 86)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = false;
                imageCrop = true;
            }
            else if ((i == 27) || (i == 87)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = false;
                imageCrop = false;    
            }
            else if ((i == 28) || (i == 88)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = GAUSSIAN; 
                unequalDim = true;
            }
            else if ((i == 29) || (i == 89)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = GAUSSIAN;
                unequalDim = true;
                image25D = false;
            }
            else if ((i == 30) || (i == 90)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = BUTTERWORTH;
                butterworthOrder = 1;
                unequalDim = true;
            }
            else if ((i == 31) || (i == 91)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = true;
                image25D = false;
            }
            else if ((i == 32) || (i == 92)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                imageCrop = true;
            } // else if ((i == 32) || (i == 92))
            else if ((i == 33) || (i == 93)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                imageCrop = false;    
            }
            else if ((i == 34) || (i == 94)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = false;
                imageCrop = true;
            }
            else if ((i == 35) || (i == 95)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = false;
                imageCrop = false;    
            }
            else if ((i == 36) || (i == 96)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = GAUSSIAN; 
                unequalDim = false;
            }
            else if ((i == 37) || (i == 97)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = GAUSSIAN;
                unequalDim = false;
                image25D = false;
            }
            else if ((i == 38) || (i == 98)) {
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 111;
                extents[1] = 239;
                constructionMethod = BUTTERWORTH;
                butterworthOrder = 1;
                unequalDim = false;
            }
            else if ((i == 39) || (i == 99)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = false;
                image25D = false;
            }
            else if ((i == 40) || (i == 100)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                image25D = true;
                imageCrop = true;
            }
            else if ((i == 41) || (i == 101)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                image25D = true;
                imageCrop = false;
            }
            else if ((i == 42) || (i == 102)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = GAUSSIAN;
                image25D = true;
            }
            else if ((i == 43) || (i == 103)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                image25D = true;
            }
            else if ((i == 44) || (i == 104)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = true;
                imageCrop = true;
            }
            else if ((i == 45) || (i == 105)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = true;
                imageCrop = false;
            }
            else if ((i == 46) || (i == 106)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = GAUSSIAN;
                unequalDim = true;
                image25D = true;
            }
            else if ((i == 47) || (i == 107)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = true;
                image25D = true;
            }
            else if ((i == 48) || (i == 108)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = true;
                imageCrop = true;
            }
            else if ((i == 49) || (i == 109)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = true;
                imageCrop = false;
            }
            else if ((i == 50) || (i == 110)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = GAUSSIAN;
                unequalDim = false;
                image25D = true;
            }
            else if ((i == 51) || (i == 111)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 64;
                extents[1] = 128;
                extents[2] = 256;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = false;
                image25D = true;
            }
            else if ((i == 52) || (i == 112)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = true;
                imageCrop = true;
            }
            else if ((i == 53) || (i == 113)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = true;
                image25D = true;
                imageCrop = false;
            }
            else if ((i == 54) || (i == 114)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = GAUSSIAN;
                unequalDim = true;
                image25D = true;
            }
            else if ((i == 55) || (i == 115)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = true;
                image25D = true;
            }
            else if ((i == 56) || (i == 116)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = true;
                imageCrop = true;
            }
            else if ((i == 57) || (i == 117)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = WINDOW;
                kernelDiameter = 15;
                unequalDim = false;
                image25D = true;
                imageCrop = false;
            }
            else if ((i == 58) || (i == 118)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = GAUSSIAN;
                unequalDim = false;
                image25D = true;
            }
            else if ((i == 59) || (i == 119)) {
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 59;
                extents[1] = 111;
                extents[2] = 239;
                constructionMethod = BUTTERWORTH; 
                butterworthOrder = 1;
                unequalDim = false;
                image25D = true;
            }
            else if (i == 120) {
                testType = SPATIAL_SHIFT;
                createNewImage = true;
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = GAUSSIAN;  
            }
            else if (i == 121) {
                testType = SPATIAL_SHIFT;
                createNewImage = false;
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = GAUSSIAN;      
            }
            else if (i == 122) {
                testType = SPATIAL_SHIFT;
                createNewImage = true;
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = GAUSSIAN;
                image25D = false;    
            }
            else if (i == 123) {
                testType = SPATIAL_SHIFT;
                createNewImage = false;
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = GAUSSIAN;
                image25D = false;     
            }
            else if (i == 124) {
                testType = FREQUENCY_SHIFT;
                createNewImage = true;
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = GAUSSIAN;  
            }
            else if (i == 125) {
                testType = FREQUENCY_SHIFT;
                createNewImage = false;
                nDims = 2;
                extents = new int[nDims];
                extents[0] = 256;
                extents[1] = 256;
                constructionMethod = GAUSSIAN;      
            }
            else if (i == 126) {
                testType = FREQUENCY_SHIFT;
                createNewImage = true;
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = GAUSSIAN;
                image25D = false;    
            }
            else if (i == 127) {
                testType = FREQUENCY_SHIFT;
                createNewImage = false;
                nDims = 3;
                extents = new int[nDims];
                extents[0] = 128;
                extents[1] = 128;
                extents[2] = 128;
                constructionMethod = GAUSSIAN;
                image25D = false;     
            }
            if (nDims == 3) {
                sliceSize = extents[0] * extents[1];
            }
            
            
            doCrop = false;
            if (imageCrop && (constructionMethod == WINDOW)) {
                newExtents = new int[nDims];
    
    
                // If imageCrop is false:
                // Find the lowest power of 2 number not less than kdim + (dimLengths[i]) - 1.
                // If imageCrop is true:
                // Find the lowest power of 2 number not less than (dimLengths[i])
                // This must be done to prevent aliasing in using a frequency filter,
                // and to have a power of 2 for the FFT.
                // Make all dimensions equal to this the largest of the above dimensions if
                // unequalDim is false
    
                arrayLength = 1;
    
                for (j = 0; j < nDims; j++) {
                    arrayLength *= extents[j];
                    newExtents[j] = extents[j];
                }
    
                if ((nDims == 3)&& (image25D)) {
    
                    for (j = 0; j < 2; j++) {
    
                        for (dimTest = newExtents[j]; dimTest > 1; dimTest = dimTest / 2) {
    
                            if ((dimTest % 2) != 0) {
                                dimTest = 1;
    
                                // log2x = logex/loge2
                                newExtents[j] = 1 + (int) (Math.log((double) newExtents[j]) / Math.log(2.0));
                                newExtents[j] = Math.round((float) (Math.pow(2.0, (double) newExtents[j])));
                            }
                        }
                    } // for (j = 0; j < 2; j++)
                } // if (nDIms == 3) && (image25D))
                else { // not image25D
    
                    for (j = 0; j < nDims; j++) {
    
                        for (dimTest = newExtents[j]; dimTest > 1; dimTest = dimTest / 2) {
    
                            if ((dimTest % 2) != 0) {
                                dimTest = 1;
    
                                // log2x = logex/loge2
                                newExtents[j] = 1 + (int) (Math.log((double) newExtents[j]) / Math.log(2.0));
                                newExtents[j] = Math.round((float) (Math.pow(2.0, (double) newExtents[j])));
                            }
                        }
                    } // for (j = 0; j < nDims; j++)
                } // else not image25D
    
                if (!unequalDim) { // if dimensions are to be made equal
                    newLength = 0;
    
                    if (image25D) {
    
                        for (j = 0; j < 2; j++) {
    
                            if (newExtents[j] > newLength) {
                                newLength = newExtents[j];
                            }
                        }
    
                        for (j = 0; j < 2; j++) {
                            newExtents[j] = newLength;
                        }
                    } else { // not image25D
    
                        for (j = 0; j < nDims; j++) {
    
                            if (newExtents[j] > newLength) {
                                newLength = newExtents[j];
                            }
                        }
    
                        for (j = 0; j < nDims; j++) {
                            newExtents[j] = newLength;
                        }
                    } // else not image25D
                } // end of if (!unequalDim)
    
    
                if (image25D) {

                    for (j = 0; j < 2; j++) {

                        if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
                            doCrop = true;
                        }
                    }
                } // if (image25D)
                else { // not image25D

                    for (j = 0; j < nDims; j++) {

                        if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
                            doCrop = true;
                        }
                    }
                } // else not image25D
    
                
                if (doCrop) {
                    start = new int[nDims];
                    end = new int[nDims];
    
                    if ((nDims == 3) && (image25D)) {
    
                        for (j = 0; j < 2; j++) {
    
                            if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
                                start[j] = ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2) +
                                           ((extents[j] + kernelDiameter - 1 - newExtents[j]) % 2);
                                end[j] = extents[j] - 1 - ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2);
                            } else {
                                start[j] = 0;
                                end[j] = extents[j] - 1;
                            }
                        }
    
                        start[2] = 0;
                        end[2] = extents[2] - 1;
                    } // if ((nDims == 3) && (image25D))
                    else { // not image25D
    
                        for (j = 0; j < nDims; j++) {
    
                            if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
                                start[j] = ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2) +
                                           ((extents[j] + kernelDiameter - 1 - newExtents[j]) % 2);
                                end[j] = extents[j] - 1 - ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2);
                            } else {
                                start[j] = 0;
                                end[j] = extents[j] - 1;
                            }
                        }
                    } // else not image25D
                } // if (doCrop)
            } // if (imageCrop && (constructionMethod == WINDOW))
            
            arrayLength = extents[0];
            for (j = 1; j < nDims; j++) {
                arrayLength *= extents[j];    
            }
            
            a = new float[arrayLength];
            c = new float[arrayLength];
            
            for (j = 0; j < arrayLength; j++) {
                c[j] = randomGen.genUniformRandomNum(0.0f, 1.0f);
                a[j] = c[j];
            } // for (j = 0; j < arrayLength; j++)
            
            forwardImage = new ModelImage(imageType, extents, "forwardImage");
            
            try {
                forwardImage.importData(0, a, true);
            }
            catch (IOException e) {
                displayError("IOException on forwardImage.importData(0, a, true)");
            }
            
            forwardImage.setOriginalCropCheckbox(imageCrop);

            String name = forwardImage.getImageName() +  "_FFT";
            transformDir = FORWARD;

            if (createNewImage)  {

                try {
                    resultImage = (ModelImage) forwardImage.clone();
                    resultImage.setImageName(name);
                    resultImage.resetVOIs();

                    // Make algorithm
                    FFTAlgo = new AlgorithmFFT(resultImage, forwardImage, transformDir, logMagDisplay, unequalDim, image25D,
                                               imageCrop, kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                               butterworthOrder);
                    FFTAlgo.calcStoreInDest();
                    
                } catch (OutOfMemoryError e) {
                    displayError("AlgorithmFFT: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    FFTAlgo = new AlgorithmFFT(forwardImage, transformDir, logMagDisplay, unequalDim, image25D, imageCrop,
                                               kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                               butterworthOrder);

                    FFTAlgo.calcInPlace();
                } catch (OutOfMemoryError e) {
                    displayError("AlgorithmFFT: unable to allocate enough memory");

                    return;
                }
            }
            
            FFTAlgo.finalize();
            if (testType == SPATIAL_SHIFT) {
                // Test f(x - extents[0]/2, y - extents[1]/2) <=> F(u,v) * ((-1)**(u + v))
                b = new float[arrayLength];
                d = new float[arrayLength];
                newC = new float[arrayLength];
                if (createNewImage) {
                    try {
                        resultImage.exportComplexData(0, arrayLength, a, b);
                    }
                    catch(IOException e) {
                        displayError("IOException error on resultImage.exportComplexData(0, arrayLength, a, b)");
                        return;
                    }
                } // if (createNewImage)
                else { // not createNewImage
                    try {
                        forwardImage.exportComplexData(0, arrayLength, a, b);
                    }
                    catch(IOException e) {
                        displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, a, b)");
                        return;
                    }    
                } // else not createNewImage
                if (nDims == 2) {
                    for (v = 0; v < extents[1]; v++) {
                        for (u = 0; u < extents[0]; u++) {
                            if (((u + v) % 2) == 1) {
                                a[u + v*extents[0]] *= -1;
                                b[u + v*extents[0]] *= -1;
                            }
                        }
                    }
                }
                else {
                    for (w = 0; w < extents[2]; w++) {
                        for (v = 0; v < extents[1]; v++) {
                            for (u = 0; u < extents[0]; u++) {
                                if (((u + v + w) % 2) == 1) {
                                    a[u + v*extents[0] + w*sliceSize] *= -1;
                                    b[u + v*extents[0] + w*sliceSize] *= -1;
                                }
                            }
                        }    
                    }
                }
                
                if (createNewImage) {
                    try {
                        resultImage.importComplexData(0, a, b, true, true);
                    }
                    catch(IOException e) {
                        displayError("IOException error on resultImage.importComplexData(0, a, b, true, true)");
                        return;
                    }
                } // if (createNewImage)
                else { // not createNewImage
                    try {
                        forwardImage.importComplexData(0, a, b, true, true);
                    }
                    catch(IOException e) {
                        displayError("IOException error on forwardImage.importComplexData(0, a, b, true, true)");
                        return;
                    }    
                } // else not createNewImage
                transformDir = INVERSE;
                
                if (createNewImage)  {
                    
                    try {
                        inverseImage = (ModelImage) resultImage.clone();
                        inverseImage.setImageName(name);
                        inverseImage.resetVOIs();
    
                        // Make algorithm
                        FFTAlgo = new AlgorithmFFT(inverseImage, resultImage, transformDir, logMagDisplay, unequalDim, image25D,
                                                   imageCrop, kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                                   butterworthOrder);
                        FFTAlgo.calcStoreInDest();
                        
                    } catch (OutOfMemoryError e) {
                        displayError("AlgorithmFFT: unable to allocate enough memory");
    
                        if (inverseImage != null) {
                            inverseImage.disposeLocal(); // Clean up memory of result image
                            inverseImage = null;
                        }
    
                        return;
                    }
                    
                    try {
                        inverseImage.exportData(0, arrayLength, a);
                    }
                    catch(IOException e) {
                        displayError("IOException error on inverseImage.exportData(0, arrayLength, a)");
                        return;
                    }
                } else {
    
                    try {
    
                        // No need to make new image space because the user has choosen to replace the source image
                        // Make the algorithm class
                        FFTAlgo = new AlgorithmFFT(forwardImage, transformDir, logMagDisplay, unequalDim, image25D, imageCrop,
                                                   kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                                   butterworthOrder);
    
                        FFTAlgo.calcInPlace();
                    } catch (OutOfMemoryError e) {
                        displayError("AlgorithmFFT: unable to allocate enough memory");
    
                        return;
                    }
                    
                    try {
                        forwardImage.exportData(0, arrayLength, a);
                    }
                    catch(IOException e) {
                        displayError("IOException error on forwardImage.exportData(0, arrayLength, a)");
                        return;
                    }
                }
                
                FFTAlgo.finalize();
                if (forwardImage !=  null) {
                    forwardImage.disposeLocal();
                    forwardImage = null;
                }
                if (resultImage != null) {
                    resultImage.disposeLocal();
                    resultImage = null;
                }
                if (inverseImage != null) {
                    inverseImage.disposeLocal();
                    inverseImage = null;
                }
                
                testCenter(a, extents);
                
                error = rms(a, c, arrayLength);
                if (error[0] >= 2.0E-7) {
                    foundError[i] = true;
                }
                
                for (j = 0; j < arrayLength; j++) {
                    a[j] = Math.abs(a[j] - c[j]);
                } // for (j = 0; j < n; j++)
    
                shellSort(a);
                if (a[arrayLength-1] >= 1.0E-6) {
                    foundError[i] = true;
                }
                
                if (foundError[i]) {
                    errorsFound++;
                    UI.setDataText("Test = " + i + " rms error = " + error[0] + "\n");
                    UI.setDataText("Test = " + i + " the 10 largest error differences\n");
                    for (j = 0; j < 10; j++) {
                        UI.setDataText("Diff[" + j + "] = " + a[arrayLength-1-j] + "\n");
                    }
                }
            } // if (testType == SPATIAL_SHIFT)
            else if (testType == FREQUENCY_SHIFT) {
                // Test f(x,y) * ((-1)**(x+y)) <=> F(u - extents[0]/2, v - extents[1]/2)
                b = new float[arrayLength];
                d = new float[arrayLength];
                if (createNewImage) {
                    try {
                        resultImage.exportComplexData(0, arrayLength, a, b);
                    }
                    catch(IOException e) {
                        displayError("IOException error on resultImage.exportComplexData(0, arrayLength, a, b)");
                        return;
                    }
                } // if (createNewImage)
                else { // not createNewImage
                    try {
                        forwardImage.exportComplexData(0, arrayLength, a, b);
                    }
                    catch(IOException e) {
                        displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, a, b)");
                        return;
                    } 
                    forwardImage.reallocate(ModelStorageBase.FLOAT);
                } // else not createNewImage
                testCenter(a, b, extents);
                if (nDims == 2) {
                    for (y = 0; y < extents[1]; y++) {
                        for (x = 0; x < extents[0]; x++) {
                            if (((x + y) % 2) == 1) {
                                c[x + y*extents[0]] *= -1;
                            }
                        }
                    }
                }
                else {
                    for (z = 0; z < extents[2]; z++) {
                        for (y = 0; y < extents[1]; y++) {
                            for (x = 0; x < extents[0]; x++) {
                                if (((x + y + z) % 2) == 1) {
                                    c[x + y*extents[0] + z*sliceSize] *= -1;
                                }
                            }
                        }
                    }
                }
                
                try {
                    forwardImage.importData(0, c, true);
                }
                catch (IOException e) {
                    displayError("IOException on forwardImage.importData(0, c, true)");
                }
                
                forwardImage.setOriginalCropCheckbox(imageCrop);

                transformDir = FORWARD;

                if (createNewImage)  {

                    try {

                        // Make algorithm
                        FFTAlgo = new AlgorithmFFT(resultImage, forwardImage, transformDir, logMagDisplay, unequalDim, image25D,
                                                   imageCrop, kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                                   butterworthOrder);
                        FFTAlgo.calcStoreInDest();
                        
                    } catch (OutOfMemoryError e) {
                        displayError("AlgorithmFFT: unable to allocate enough memory");

                        if (resultImage != null) {
                            resultImage.disposeLocal(); // Clean up memory of result image
                            resultImage = null;
                        }

                        return;
                    }
                } else {

                    try {

                        // No need to make new image space because the user has choosen to replace the source image
                        // Make the algorithm class
                        FFTAlgo = new AlgorithmFFT(forwardImage, transformDir, logMagDisplay, unequalDim, image25D, imageCrop,
                                                   kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                                   butterworthOrder);

                        FFTAlgo.calcInPlace();
                    } catch (OutOfMemoryError e) {
                        displayError("AlgorithmFFT: unable to allocate enough memory");

                        return;
                    }
                }
                
                FFTAlgo.finalize();
                
                if (createNewImage) {
                    try {
                        resultImage.exportComplexData(0, arrayLength, c, d);
                    }
                    catch(IOException e) {
                        displayError("IOException error on resultImage.exportComplexData(0, arrayLength, c, d)");
                        return;
                    }
                } // if (createNewImage)
                else { // not createNewImage
                    try {
                        forwardImage.exportComplexData(0, arrayLength, c, d);
                    }
                    catch(IOException e) {
                        displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, c, d)");
                        return;
                    }    
                } // else not createNewImage
                
                if (forwardImage != null) {
                    forwardImage.disposeLocal();
                }
                if (resultImage != null) {
                    resultImage.disposeLocal();
                }
                
                error = rms(a, b, c, d, arrayLength);
                if ((error[0] >= 2.0E-7) || (error[1] > 2.0E-7)) {
                    foundError[i] = true;
                }
                
                for (j = 0; j < arrayLength; j++) {
                    a[j] = (float)Math.sqrt((a[j] - c[j])*(a[j] - c[j]) + (b[j] - d[j])*(b[j] - d[j]));
                } // for (j = 0; j < n; j++)
    
                shellSort(a);
                if (a[arrayLength-1] >= 1.0E-6) {
                    foundError[i] = true;
                }
                
                if (foundError[i]) {
                    errorsFound++;
                    UI.setDataText("Test = " + i + " rms errors = " + error[0] + " , " + error[1] + "\n");
                    UI.setDataText("Test = " + i + " the 10 largest error differences\n");
                    for (j = 0; j < 10; j++) {
                        UI.setDataText("Diff[" + j + "] = " + a[arrayLength-1-j] + "\n");
                    }
                }
            } // else if (testType == FREQUENCY_SHIFT)
            else { // testType == FORWARD_INVERSE
                transformDir = INVERSE;
                
                if (createNewImage)  {
    
                    try {
                        inverseImage = (ModelImage) resultImage.clone();
                        inverseImage.setImageName(name);
                        inverseImage.resetVOIs();
    
                        // Make algorithm
                        FFTAlgo = new AlgorithmFFT(inverseImage, resultImage, transformDir, logMagDisplay, unequalDim, image25D,
                                                   imageCrop, kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                                   butterworthOrder);
                        FFTAlgo.calcStoreInDest();
                        
                    } catch (OutOfMemoryError e) {
                        displayError("AlgorithmFFT: unable to allocate enough memory");
    
                        if (inverseImage != null) {
                            inverseImage.disposeLocal(); // Clean up memory of result image
                            inverseImage = null;
                        }
    
                        return;
                    }
                    
                    try {
                        inverseImage.exportData(0, arrayLength, a);
                    }
                    catch(IOException e) {
                        displayError("IOException error on inverseImage.exportData(0, arrayLength, a)");
                        return;
                    }
                } else {
    
                    try {
    
                        // No need to make new image space because the user has choosen to replace the source image
                        // Make the algorithm class
                        FFTAlgo = new AlgorithmFFT(forwardImage, transformDir, logMagDisplay, unequalDim, image25D, imageCrop,
                                                   kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                                   butterworthOrder);
    
                        FFTAlgo.calcInPlace();
                    } catch (OutOfMemoryError e) {
                        displayError("Dialog FFT: unable to allocate enough memory");
    
                        return;
                    }
                    
                    try {
                        forwardImage.exportData(0, arrayLength, a);
                    }
                    catch(IOException e) {
                        displayError("IOException error on forwardImage.exportData(0, arrayLength, a)");
                        return;
                    }
                }
                
                FFTAlgo.finalize();
                if (forwardImage !=  null) {
                    forwardImage.disposeLocal();
                    forwardImage = null;
                }
                if (resultImage != null) {
                    resultImage.disposeLocal();
                    resultImage = null;
                }
                if (inverseImage != null) {
                    inverseImage.disposeLocal();
                    inverseImage = null;
                }
                
                if (doCrop) {
                  j = 0;
                  if (nDims == 2) {
                      newArrayLength = (end[0] - start[0] + 1)* (end[1] - start[1] + 1);
                      newA = new float[newArrayLength];
                      newC = new float[newArrayLength];
                      for (y = start[1]; y <= end[1]; y++) {
                          for (x = start[0]; x <= end[0]; x++) {
                              newA[j] = a[x + y*extents[0]];
                              newC[j++] = c[x + y*extents[0]];
                          }
                      }
                  } // if (nDims == 2)
                  else {
                      newArrayLength = (end[0] - start[0] + 1)* (end[1] - start[1] + 1) * (end[2] - start[2] + 1);
                      newA = new float[newArrayLength];
                      newC = new float[newArrayLength];
                      for (z = start[2]; z <= end[2]; z++) {
                          for (y = start[1]; y <= end[1]; y++) {
                              for (x = start[0]; x <= end[0]; x++) {
                                  newA[j] = a[x + y*extents[0] + z*sliceSize];
                                  newC[j++] = c[x + y*extents[0] + z*sliceSize];
                              }
                          }  
                      }
                  }
                  a = new float[newArrayLength];
                  c = new float[newArrayLength];
                  for (j = 0; j < newArrayLength; j++) {
                      a[j] = newA[j];
                      c[j] = newC[j];
                  }
                  arrayLength = newArrayLength;
                } // if (doCrop)
                
                
                error = rms(a, c, arrayLength);
                if (error[0] >= 2.0E-7) {
                    foundError[i] = true;
                }
                
                for (j = 0; j < arrayLength; j++) {
                    a[j] = Math.abs(a[j] - c[j]);
                } // for (j = 0; j < n; j++)
    
                shellSort(a);
                if (a[arrayLength-1] >= 1.0E-6) {
                    foundError[i] = true;
                }
                
                if (foundError[i]) {
                    errorsFound++;
                    UI.setDataText("Test = " + i + " rms error = " + error[0] + "\n");
                    UI.setDataText("Test = " + i + " the 10 largest error differences\n");
                    for (j = 0; j < 10; j++) {
                        UI.setDataText("Diff[" + j + "] = " + a[arrayLength-1-j] + "\n");
                    }
                }
            } // else testType == FORWARD_INVERSE
            
        } // for (i = 0; i < nTests; i++)
        UI.setDataText("Errors were found in " + errorsFound + " of " + nTests + " tests\n");
        setCompleted(true);
        return;
    }
    
    private void testCenter(float[] a, int[] extents) {
        int x, y, z;
        int newX, newY, newZ;
        float newA[] = new float[a.length];
        int sliceSize;
        int nDims = extents.length;
       
        if (nDims == 2) {
            for (y = 0; y < extents[1]; y++) {
                if ((y - extents[1]/2) >= 0) {
                    newY = y - extents[1]/2;
                }
                else {
                    newY = y + extents[1]/2;
                }
                for (x = 0; x < extents[0]; x++) {
                    if ((x - extents[0]/2) >= 0) {
                        newX = x - extents[0]/2;
                    }
                    else {
                        newX = x + extents[0]/2;
                    }
                    newA[newX + extents[0]*newY] = a[x + extents[0]*y];
                }
            }
        } // if (nDims == 2)
        else { // nDims == 3
            sliceSize = extents[0] * extents[1];
            for (z = 0; z < extents[2]; z++) {
                if ((z - extents[2]/2) >= 0) {
                    newZ = z - extents[2]/2;
                }
                else {
                    newZ = z + extents[2]/2;
                }
                for (y = 0; y < extents[1]; y++) {
                    if ((y - extents[1]/2) >= 0) {
                        newY = y - extents[1]/2;
                    }
                    else {
                        newY = y + extents[1]/2;
                    }
                    for (x = 0; x < extents[0]; x++) {
                        if ((x - extents[0]/2) >= 0) {
                            newX = x - extents[0]/2;
                        }
                        else {
                            newX = x + extents[0]/2;
                        }
                        newA[newX + extents[0]*newY + sliceSize*newZ] = a[x + extents[0]*y + sliceSize*z];
                    }
                }
            }
        } // else nDims == 3 
        for (x = 0; x < a.length; x++) {
            a[x] = newA[x];
        }
    }
    
    private void testCenter(float[] a, float [] b, int[] extents) {
        int x, y, z;
        int newX, newY, newZ;
        float newA[] = new float[a.length];
        float newB[] = new float[b.length];
        int sliceSize;
        int nDims = extents.length;
       
        if (nDims == 2) {
            for (y = 0; y < extents[1]; y++) {
                if ((y - extents[1]/2) >= 0) {
                    newY = y - extents[1]/2;
                }
                else {
                    newY = y + extents[1]/2;
                }
                for (x = 0; x < extents[0]; x++) {
                    if ((x - extents[0]/2) >= 0) {
                        newX = x - extents[0]/2;
                    }
                    else {
                        newX = x + extents[0]/2;
                    }
                    newA[newX + extents[0]*newY] = a[x + extents[0]*y];
                    newB[newX + extents[0]*newY] = b[x + extents[0]*y];
                }
            }
        } // if (nDims == 2)
        else { // nDims == 3
            sliceSize = extents[0] * extents[1];
            for (z = 0; z < extents[2]; z++) {
                if ((z - extents[2]/2) >= 0) {
                    newZ = z - extents[2]/2;
                }
                else {
                    newZ = z + extents[2]/2;
                }
                for (y = 0; y < extents[1]; y++) {
                    if ((y - extents[1]/2) >= 0) {
                        newY = y - extents[1]/2;
                    }
                    else {
                        newY = y + extents[1]/2;
                    }
                    for (x = 0; x < extents[0]; x++) {
                        if ((x - extents[0]/2) >= 0) {
                            newX = x - extents[0]/2;
                        }
                        else {
                            newX = x + extents[0]/2;
                        }
                        newA[newX + extents[0]*newY + sliceSize*newZ] = a[x + extents[0]*y + sliceSize*z];
                        newB[newX + extents[0]*newY + sliceSize*newZ] = b[x + extents[0]*y + sliceSize*z];
                    }
                }
            }
        } // else nDims == 3 
        for (x = 0; x < a.length; x++) {
            a[x] = newA[x];
            b[x] = newB[x];
        }
    }
    
    private double[] rms(float[] a, float[] c, int n) {
        // Computes rms error for transform-inverse pair arrays a = transform, inverse results arrays c = original
        // data n = dimension of arrays a, and c

        // rms errors for a and b arrays
        double[] error = new double[1];
        double ssa = 0.0;
        int j;
        double amc;

        for (j = 0; j < n; j++) {
            amc = a[j] - c[j];
            ssa = (amc * amc) + ssa;
        }

        error[0] = Math.sqrt(ssa / (double) n);

        return error;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     * @param   c  DOCUMENT ME!
     * @param   d  DOCUMENT ME!
     * @param   n  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    double[] rms(float[] a, float[] b, float[] c, float[] d, int n) {
        // Computes rms error for transform-inverse pair arrays a,b = transform, inverse results arrays c,d = original
        // data n = dimension of arrays a, b, c, and d

        // rms errors for a and b arrays
        double[] error = new double[2];
        double ssa = 0.0;
        double ssb = 0.0;
        int j;
        double amc;
        double bmd;

        for (j = 0; j < n; j++) {
            amc = a[j] - c[j];
            ssa = (amc * amc) + ssa;
            bmd = b[j] - d[j];
            ssb = (bmd * bmd) + ssb;
        }

        error[0] = Math.sqrt(ssa / (double) n);
        error[1] = Math.sqrt(ssb / (double) n);

        return error;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  buffer  float [] input buffer to be sorted Sort an array into ascending numerical order by Shell's method
     *                 Reference: Numerical Recipes in C The Art of Scientific Computing Second Edition by William H.
     *                 Press, Saul A. Teukolsky, William T. Vetterling, Brian P. Flannery, pp. 331- 332.
     */
    private void shellSort(float[] buffer) {
        int i, j, inc;
        float v;
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
     * Returns the Bessel function J1(x) for any real x. From Numerical Recipes in C Second Edition Section 6.5 p. 233
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float bessj1(float x) {

        float ax, z;
        double xx, y, ans, ans1, ans2;

        if ((ax = Math.abs(x)) < 8.0) {
            y = x * x;
            ans1 = x *
                       (72362614232.0 +
                            (y *
                                 (-7895059235.0 +
                                      (y *
                                           (242396853.1 +
                                                (y * (-2972611.439 + (y * (15704.48260 + (y * (-30.16036606)))))))))));
            ans2 = 144725228442.0 +
                   (y * (2300535178.0 + (y * (18583304.74 + (y * (99447.43394 + (y * (376.9991397 + (y * 1.0)))))))));
            ans = ans1 / ans2;
        } else {
            z = 8.0f / ax;
            y = z * z;
            xx = ax - 2.356194491;
            ans1 = 1.0 +
                   (y * (0.183105e-2 + (y * (-0.3516396496e-4 + (y * (0.2457520174e-5 + (y * (-0.240337019e-6))))))));
            ans2 = 0.04687499995 +
                   (y * (-0.2002690873e-3 + (y * (0.8449199096e-5 + (y * (-0.88228987e-6 + (y * 0.105787412e-6)))))));
            ans = Math.sqrt(0.636619772 / ax) * ((Math.cos(xx) * ans1) - (z * Math.sin(xx) * ans2));

            if (x < 0.0) {
                ans = -ans;
            }
        }

        return (float) ans;
    }

    /**
     * This function replaces the original image with a new image that is either the FFT, the filtered FFT, or the
     * inverse FFT of the original or filtered image.
     */
    private void calcInPlace() {

        int i;
        float tempR;
        float[] realSubsetData;
        float[] imagSubsetData;
        int z;

        makeComplexData();

        if ((transformDir == FILTER) && (constructionMethod == WINDOW)) {

            // The filter kernel is constructed.
            makeKernelData();

            // The filter FFT is created
            exec(realKernelData, imagKernelData, 0);

            if (threadStopped) {
                finalize();

                return;
            }

        } // end of if ((transformDir == FILTER) && (constructionMethod == WINDOW))

        if ((transformDir == FORWARD) || (transformDir == INVERSE)) {

            if (image25D && (ndim >= 3)) {
                realSubsetData = new float[newSliceSize];
                imagSubsetData = new float[newSliceSize];

                if (transformDir == FORWARD) {
                    fireProgressStateChanged(0, srcImage.getImageName(), "Running forward FFTs ...");
                } else {
                    fireProgressStateChanged(0, srcImage.getImageName(), "Running inverse FFTs ...");
                }

                for (z = 0; z < newDimLengths[2]; z++) {

                    for (i = 0; i < newSliceSize; i++) {
                        realSubsetData[i] = realData[(z * newSliceSize) + i];
                        imagSubsetData[i] = imagData[(z * newSliceSize) + i];
                    }

                    exec(realSubsetData, imagSubsetData, z);

                    fireProgressStateChanged((Math.round(10 + ((float) (z + 1) / newDimLengths[2] * 80))), null, null);
                   

                    if (transformDir == FORWARD) {

                        for (i = 0; i < newSliceSize; i++) {
                            realData[(z * newSliceSize) + i] = realSubsetData[i];
                            imagData[(z * newSliceSize) + i] = imagSubsetData[i];
                        }
                    } // if (tranformDir == FORWARD)
                } // for (z = 0; z < newDimLengths[2]; z++)

                realSubsetData = null;
                imagSubsetData = null;
            } // if (image25D)
            else { // not image25D
                exec(realData, imagData, 0);
            }

            if (threadStopped) {
                finalize();

                return;
            }
        } // if ((transformDir == FORWARD) || (transformDir == INVERSE))

        if ((transformDir == FILTER) && (constructionMethod == GAUSSIAN)) {
            makeGaussianFilter(f1);
        } // end of if ((transformDir == FILTER) && (constructionMethod == GAUSSIAN))

        if ((transformDir == FILTER) && (constructionMethod == GABOR)) {
            makeGaborFilter(freqU, freqV, sigmaU, sigmaV, theta);
        }

        if ((transformDir == FILTER) && (constructionMethod == BUTTERWORTH)) {
            makeButterworthFilter(f1, f2);
        } // end of if ((transformDir == FILTER)&& (constructionMethod == BUTTERWORTH))

        if ((transformDir == FILTER) && (constructionMethod == WINDOW)) {

            // The image FFT is multiplied by the filter FFT.
            if (image25D && (ndim >= 3)) {

                for (z = 0; z < newDimLengths[2]; z++) {

                    for (i = 0; i < newSliceSize; i++) {
                        tempR = (realData[(z * newSliceSize) + i] * realKernelData[i]) -
                                (imagData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagData[(z * newSliceSize) + i] = (imagData[(z * newSliceSize) + i] * realKernelData[i]) +
                                                           (realData[(z * newSliceSize) + i] * imagKernelData[i]);
                        realData[(z * newSliceSize) + i] = tempR;
                    }
                }
            } // if (image25D)
            else { // not image25D)

                for (i = 0; i < newArrayLength; i++) {
                    tempR = (realData[i] * realKernelData[i]) - (imagData[i] * imagKernelData[i]);
                    imagData[i] = (imagData[i] * realKernelData[i]) + (realData[i] * imagKernelData[i]);
                    realData[i] = tempR;
                }
            } // else not image25D
        } // end of if ((transformDir == FILTER) && (constructionMethod == WINDOW))

        if ((transformDir == FILTER)) {
            fireProgressStateChanged(-1, null, "Storing filtered FFT in source image ...");
        }

        if (transformDir == FORWARD) {
            srcImage.setOriginalExtents(dimLengths);
            srcImage.setOriginalMinimum(minimum);
            srcImage.setOriginalMaximum(maximum);
            srcImage.setOriginalDoCrop(doCrop);

            if (doCrop) {
                srcImage.setOriginalStart(start);
                srcImage.setOriginalEnd(end);
            }

            srcImage.setHaveWindowed(false);

            fireProgressStateChanged(-1, null, "Storing FFT in source image ...");
            
        } // end of if (transformDir == FORWARD)

        if (transformDir == FILTER) {

            if (constructionMethod == WINDOW) {
                srcImage.setHaveWindowed(true);
            } else {
                srcImage.setHaveWindowed(false);
            }
        } // end of if (transformDir == FILTER)

        if ((transformDir == FORWARD) || (transformDir == FILTER)) {

            // In the frequency domain so complex data is needed
            try {
                srcImage.reallocate(ModelStorageBase.COMPLEX, newDimLengths);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on srcImage.reallocate");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on srcImage.reallocate");

                setCompleted(false);

                return;
            }

            try {

                // Calculate image minimum and maximum based on magnitude
                // if logMagDisplay is false or the log10(1 + magnitude) if logMagDisplay
                // is true
                srcImage.importComplexData(0, realData, imagData, true, logMagDisplay);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on source image import complex data");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on source image import complex data");

                setCompleted(false);

                return;
            }
        } // end of if ((transformDir == FORWARD)|| (transformDir == FILTER))
        else if (transformDir == INVERSE) {
            fireProgressStateChanged(-1, null, "Storing inverse FFT in source image ...");
            
            // back in the spatial domain so only realData is now present
            try {
                srcImage.reallocate(ModelStorageBase.FLOAT, originalDimLengths);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on srcImage.reallocate");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on srcImage.reallocate");

                setCompleted(false);

                return;
            }

            try {
                srcImage.importData(0, finalData, true);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on source image import data");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on source image import data");

                setCompleted(false);

                return;
            }
        } // end of else if (transformDir == INVERSE)


        setCompleted(true);
    }

    /**
     * This function produces a new image that is either the FFT, the filtered FFT, or the inverse FFT of the original
     * or filtered image.
     */
    private void calcStoreInDest() {

        int i;
        float tempR;
        float[] realSubsetData;
        float[] imagSubsetData;
        int z;

        makeComplexData();

        if ((transformDir == FILTER) && (constructionMethod == WINDOW)) {

            // The filter kernel is constructed.
            makeKernelData();

            // The filter FFT is created
            exec(realKernelData, imagKernelData, 0);

            if (threadStopped) {
                finalize();

                return;
            }
        } // end of if ((transformDir == FILTER) && (constructionMethod == WINDOW))

        if ((transformDir == FORWARD) || (transformDir == INVERSE)) {

            if ((image25D && (ndim >= 3))) {
                realSubsetData = new float[newSliceSize];
                imagSubsetData = new float[newSliceSize];
                
                fireProgressStateChanged(0, srcImage.getImageName(), "Running forward FFTs ...");

                for (z = 0; z < newDimLengths[2]; z++) {

                    for (i = 0; i < newSliceSize; i++) {
                        realSubsetData[i] = realData[(z * newSliceSize) + i];
                        imagSubsetData[i] = imagData[(z * newSliceSize) + i];
                    }

                    exec(realSubsetData, imagSubsetData, z);

                    fireProgressStateChanged((Math.round(10 + ((float) (z + 1) / newDimLengths[2] * 80))), srcImage.getImageName(), "Running forward FFTs ...");
                 
                    if (transformDir == FORWARD) {

                        for (i = 0; i < newSliceSize; i++) {
                            realData[(z * newSliceSize) + i] = realSubsetData[i];
                            imagData[(z * newSliceSize) + i] = imagSubsetData[i];
                        }
                    } // if (tranformDir == FORWARD)
                } // for (z = 0; z < newDimLengths[2]; z++)

                realSubsetData = null;
                imagSubsetData = null;
            } // if (image25D)
            else { // not image25D
                exec(realData, imagData, 0);
            } // else not image25D

            if (threadStopped) {
                finalize();

                return;
            }
        } // if ((transformDir == FORWARD) || (transformDir == INVERSE))

        if ((transformDir == FILTER) && (constructionMethod == GAUSSIAN)) {
            makeGaussianFilter(f1);
        } // end of if ((transformDir == FILTER) && (constructionMethod == GAUSSIAN))

        if ((transformDir == FILTER) && (constructionMethod == GABOR)) {
            makeGaborFilter(freqU, freqV, sigmaU, sigmaV, theta);
        }

        if ((transformDir == FILTER) && (constructionMethod == BUTTERWORTH)) {
            makeButterworthFilter(f1, f2);
        } // end of if ((transformDir == FILTER)&& (constructionMethod == BUTTERWORTH))

        if ((transformDir == FILTER) && (constructionMethod == WINDOW)) {

            // The image FFT is multiplied by the filter FFT.
            if (image25D && (ndim >= 3)) {

                for (z = 0; z < newDimLengths[2]; z++) {

                    for (i = 0; i < newSliceSize; i++) {
                        tempR = (realData[(z * newSliceSize) + i] * realKernelData[i]) -
                                (imagData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagData[(z * newSliceSize) + i] = (imagData[(z * newSliceSize) + i] * realKernelData[i]) +
                                                           (realData[(z * newSliceSize) + i] * imagKernelData[i]);
                        realData[(z * newSliceSize) + i] = tempR;
                    }
                }
            } // if (image25D)
            else { // not image25D)

                for (i = 0; i < newArrayLength; i++) {
                    tempR = (realData[i] * realKernelData[i]) - (imagData[i] * imagKernelData[i]);
                    imagData[i] = (imagData[i] * realKernelData[i]) + (realData[i] * imagKernelData[i]);
                    realData[i] = tempR;
                }
            } // else not image25D
        } // end of if ((transformDir == FILTER) && (constructionMethod == WINDOW))

        if (transformDir == FILTER) {
            fireProgressStateChanged(-1, null, "Storing filtered FFT in destination image ...");
        }

        if (transformDir == FORWARD) {
            fireProgressStateChanged(-1, null, "Storing FFT in destination image ...");
        }

        if ((transformDir == FORWARD) || (transformDir == FILTER)) {

            // In the frequency domain so complex data is needed
            try {
                destImage.reallocate(ModelStorageBase.COMPLEX, newDimLengths);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on destImage.reallocate");
                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on destImage.reallocate");

                setCompleted(false);

                return;
            }

            try {

                // Calculate image minimum and maximum based on magnitude
                // if logMagDisplay is false or the log10(1 + magnitude) if logMagDisplay
                // is true
                destImage.importComplexData(0, realData, imagData, true, logMagDisplay);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on destination image import complex data");


                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on destination image import complex data");

                setCompleted(false);

                return;
            }

            if (transformDir == FORWARD) {
                destImage.setOriginalExtents(dimLengths);
                destImage.setOriginalMinimum(minimum);
                destImage.setOriginalMaximum(maximum);
                destImage.setOriginalDoCrop(doCrop);

                if (doCrop) {
                    destImage.setOriginalStart(start);
                    destImage.setOriginalEnd(end);
                }

                destImage.setHaveWindowed(false);
            } else if (transformDir == FILTER) {
                destImage.setOriginalExtents(srcImage.getOriginalExtents());
                destImage.setOriginalMinimum(srcImage.getOriginalMinimum());
                destImage.setOriginalMaximum(srcImage.getOriginalMaximum());
                doCrop = srcImage.getOriginalDoCrop();
                destImage.setOriginalDoCrop(doCrop);

                if (doCrop) {
                    destImage.setOriginalStart(srcImage.getOriginalStart());
                    destImage.setOriginalEnd(srcImage.getOriginalEnd());
                }

                if (constructionMethod == WINDOW) {
                    destImage.setHaveWindowed(true);
                } else {
                    destImage.setHaveWindowed(false);
                }
            }

        } // end of if ((transformDir == FORWARD) || (transformDir == FILTER))
        else if (transformDir == INVERSE) {
            fireProgressStateChanged(-1, null, "Storing inverse FFT in destination image ...");
          
            // back in the spatial domain so only realData is now present
            try {
                destImage.reallocate(ModelStorageBase.FLOAT, originalDimLengths);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on destImage.reallocate");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on destImage.reallocate");

                setCompleted(false);

                return;
            }

            try {
                destImage.importData(0, finalData, true);
            } catch (IOException error) {
                displayError("AlgorithmFFT: IOException on destination image import data");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT: Out of memory on destination image import data");

                setCompleted(false);

                return;
            }

        } // end of else if (transformDir == INVERSE)

        fireProgressStateChanged(100, null, null);
        setCompleted(true);
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
    }

    /**
     * This function edge strips the realData after the inverse FFT to return to the original dimensions that were
     * present before the forward FFT was performed.
     *
     * @param  rData  DOCUMENT ME!
     * @param  z      DOCUMENT ME!
     */
    private void edgeStrip(float[] rData, int z) {
        int i, j, k, m, n;
        float[] tempData;
        int aLength;

        // This function edge strips the realData after the inverse FFT to
        // return to the original dimensions that were present before the
        // forward FFT was performed.
        originalArrayLength = 1;

        for (i = 0; i < ndim; i++) {
            originalArrayLength *= originalDimLengths[i];
        }

        if (image25D) {
            aLength = originalDimLengths[0] * originalDimLengths[1];
        } else {
            aLength = originalArrayLength;
        }

        try {
            tempData = new float[aLength];
        } catch (OutOfMemoryError e) {
            tempData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating tempData in edgeStrip routine");

            setCompleted(false);

            return;
        }

        if (ndim == 1) {

            for (i = 0; i < originalDimLengths[0]; i++) {
                tempData[i] = rData[i];
            }
        } else if ((ndim == 2) || (image25D)) {

            for (j = 0; j < originalDimLengths[1]; j++) {

                for (i = 0; i < originalDimLengths[0]; i++) {
                    tempData[i + (originalDimLengths[0] * j)] = rData[i + (newDimLengths[0] * j)];
                }
            }
        } else if (ndim == 3) {

            for (k = 0; k < originalDimLengths[2]; k++) {

                for (j = 0; j < originalDimLengths[1]; j++) {

                    for (i = 0; i < originalDimLengths[0]; i++) {
                        tempData[i + (originalDimLengths[0] * j) + (originalDimLengths[0] * originalDimLengths[1] * k)] = rData[i +
                                                                                                                                (newDimLengths[0] *
                                                                                                                                     j) +
                                                                                                                                (newDimLengths[0] *
                                                                                                                                     newDimLengths[1] *
                                                                                                                                     k)];
                    }
                }
            }
        } else if (ndim == 4) {

            for (m = 0; m < originalDimLengths[3]; m++) {

                for (k = 0; k < originalDimLengths[2]; k++) {

                    for (j = 0; j < originalDimLengths[1]; j++) {

                        for (i = 0; i < originalDimLengths[0]; i++) {
                            tempData[i + (originalDimLengths[0] * j) +
                                     (originalDimLengths[0] * originalDimLengths[1] * k) +
                                     (originalDimLengths[0] * originalDimLengths[1] * originalDimLengths[2] * m)] = rData[i +
                                                                                                                          (newDimLengths[0] *
                                                                                                                               j) +
                                                                                                                          (newDimLengths[0] *
                                                                                                                               newDimLengths[1] *
                                                                                                                               k) +
                                                                                                                          (newDimLengths[0] *
                                                                                                                               newDimLengths[1] *
                                                                                                                               newDimLengths[2] *
                                                                                                                               m)];
                        }
                    }
                }
            }
        } else if (ndim == 5) {

            for (n = 0; n < originalDimLengths[4]; n++) {

                for (m = 0; m < originalDimLengths[3]; m++) {

                    for (k = 0; k < originalDimLengths[2]; k++) {

                        for (j = 0; j < originalDimLengths[1]; j++) {

                            for (i = 0; i < originalDimLengths[0]; i++) {
                                tempData[i + (originalDimLengths[0] * j) +
                                         (originalDimLengths[0] * originalDimLengths[1] * k) +
                                         (originalDimLengths[0] * originalDimLengths[1] * originalDimLengths[2] * m) +
                                         (originalDimLengths[0] * originalDimLengths[1] * originalDimLengths[2] *
                                              originalDimLengths[3] * n)] = rData[i + (newDimLengths[0] * j) +
                                                                                  (newDimLengths[0] * newDimLengths[1] *
                                                                                       k) +
                                                                                  (newDimLengths[0] * newDimLengths[1] *
                                                                                       newDimLengths[2] * m) +
                                                                                  (newDimLengths[0] * newDimLengths[1] *
                                                                                       newDimLengths[2] *
                                                                                       newDimLengths[3] * n)];
                            }
                        }
                    }
                }
            }
        }

        try {

            if ((!image25D) || (z == 0)) {
                finalData = new float[originalArrayLength];
            }
        } catch (OutOfMemoryError e) {
            realData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating realData in edgeStrip routine");

            setCompleted(false);

            return;
        }

        for (i = 0; i < aLength; i++) {
            finalData[(z * aLength) + i] = tempData[i];
        }
    }

    /**
     * This is the method that calculates the FFT Perform a data centering operation after the forward FFT Perform a
     * data centering operation before the inverse FFT Note that a frequency filter operation performs a forward FFT.
     *
     * @param  rData  real data buffer
     * @param  iData  imaginary data buffer
     * @param  z      slice number in image25D processing, 0 otherwise
     */
    private void exec(float[] rData, float[] iData, int z) {

        double TWO_PI = 2 * java.lang.Math.PI;
        double wt1Imag, wt1Real;
        double angle, delta;
        float imag, real, fTemp, fReal, fImag;
        int i, index1, index2, index3;
        int j1, j2, j3;
        int k1, k1Double;
        int iSwap, i1Swap, i2Swap, index, dim;
        int direction;
        boolean haveWindowed;
        int dimNumber;
        int newLength;

        if ((transformDir == FORWARD) || (transformDir == FILTER)) {
            direction = 1;
        } else {
            direction = -1;
        }

        if (image25D) {
            dimNumber = 2;
            newLength = newDimLengths[0] * newDimLengths[1];
        } else {
            dimNumber = ndim;
            newLength = newArrayLength;
        }

        if (transformDir == INVERSE) {

            if (!(image25D && (ndim >= 3))) {
                fireProgressStateChanged(-1, null, "Centering data before inverse FFT ...");
            }

            center(rData, iData);
        }

        if (!(image25D && (ndim >= 3))) {
            fireProgressStateChanged(-1, null, "Running FFT algorithm ...");
        }

        j1 = 1;
        dim = 1;

        for (i = 0; (i < dimNumber) && !threadStopped; i++) {
            j1 *= dim;
            dim = newDimLengths[i];
            j2 = j1 * dim;
            j3 = j2 * (newLength / (dim * j1));

            i1Swap = 0;

            for (index1 = 0; (index1 < j2) && !threadStopped; index1 += j1) {

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

            for (k1 = j1; (k1 < j2) && !threadStopped; k1 <<= 1) {
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
                            imag = (float) ((fImag * wt1Real) + (fReal * wt1Imag));
                            real = (float) ((fReal * wt1Real) - (fImag * wt1Imag));
                            iData[index] = iData[index3] - imag;
                            rData[index] = rData[index3] - real;
                            iData[index3] = iData[index3] + imag;
                            rData[index3] = rData[index3] + real;
                        }
                    }
                }
            }

            if (threadStopped) {
                return;
            }

            if (!(image25D && (ndim >= 3))) {
                fireProgressStateChanged((Math.round(10 + ((float) (i + 1) / ndim * 80))), null, null);
            }
        }

        if (threadStopped) {
            return;
        }

        if ((transformDir == FORWARD) || (transformDir == FILTER)) {

            if (!(image25D && (ndim >= 3))) {
                fireProgressStateChanged(-1, null, "Centering data after FFT algorithm ...");
            }

            center(rData, iData);
        }

        if (transformDir == INVERSE) {
            /* When the srcImage = fft(imageA) fft(imageB), then on inverse,
             * recenter: */
            if ( srcImage.getConvolve() == true )
            {
                center(rData, iData);
            }
            for (i = 0; i < newLength; i++) {
                rData[i] = rData[i] / newLength;
                iData[i] = iData[i] / newLength;
            }

            haveWindowed = srcImage.getHaveWindowed();

            if (haveWindowed == true) {

                // shift each dimension back to the start by (kDim - 1)/2
                shiftBack(rData);
            }

            originalDimLengths = srcImage.getOriginalExtents();

            // Do edge stripping to restore the original dimensions the source image had
            // before the forward FFT
            if (!(image25D && (ndim >= 3))) {
                fireProgressStateChanged(-1, null, "Zero stripping data after inverse FFT ...");
            }

            edgeStrip(rData, z);

            if ((!(image25D && (ndim >= 3))) || (z == (dimLengths[2] - 1))) {
                doCrop = srcImage.getOriginalDoCrop();

                if (doCrop) {
                    zeroAround();
                    /* When the srcImage = fft(imageA) fft(imageB), then on inverse,
                     * recenter: */
                    if ( srcImage.getConvolve() == true )
                    {
                        zeroAround();
                    }
                }

                minimum = srcImage.getOriginalMinimum();
                maximum = srcImage.getOriginalMaximum();

                for (i = 0; i < originalArrayLength; i++) {

                    if (finalData[i] > maximum) {
                        finalData[i] = maximum;
                    }

                    if (finalData[i] < minimum) {
                        finalData[i] = minimum;
                    }
                }
            } // if ((!image25D) || (z == dimLengths[2] - 1))

        } // end of if (transformDir == INVERSE)

    } // end of exec()

    /**
     * Builds a hamming kernel in 2 dimensions.
     */
    private void hammingKernel2D() {
        int x, y, halfKDim, pos;
        double distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (double) (halfKDim + 1);

        try {
            wKernel = new float[kDim * kDim];
        } catch (OutOfMemoryError e) {
            wKernel = null;
            System.gc();
            displayError("hammingKernel2D: Out of memory creating wKernel");
            setCompleted(false);

            return;
        }

        for (y = 0; y <= (kDim - 1); y++) {

            for (x = 0; x <= (kDim - 1); x++) {
                pos = (y * kDim) + x;
                distance = Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)));

                if (distance < tau) {
                    wKernel[pos] = (float) (0.54 + (0.46 * Math.cos(Math.PI * distance / tau)));
                } else {
                    wKernel[pos] = 0.0f;
                }
            }
        }
    }

    /**
     * Builds a hamming kernel in 3 dimensions.
     */
    private void hammingKernel3D() {
        int x, y, z, halfKDim, pos;
        double distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (double) (halfKDim + 1);

        try {
            wKernel = new float[kDim * kDim * kDim];
        } catch (OutOfMemoryError e) {
            wKernel = null;
            System.gc();
            displayError("hammingKernel3D: Out of memory creating wKernel");
            setCompleted(false);

            return;
        }

        for (z = 0; z <= (kDim - 1); z++) {

            for (y = 0; y <= (kDim - 1); y++) {

                for (x = 0; x <= (kDim - 1); x++) {
                    pos = (z * kDim * kDim) + (y * kDim) + x;
                    distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                                 ((z - halfKDim) * (z - halfKDim)));

                    if (distance < tau) {
                        wKernel[pos] = (float) (0.54 + (0.46 * Math.cos(Math.PI * distance / tau)));
                    } else {
                        wKernel[pos] = 0.0f;
                    }
                }
            }
        }
    }

    /**
     * Builds the idealized bandpass kernel in 2 dimensions.
     *
     * @param  FLow   defines the cutoff frequecy at low freqs.
     * @param  FHigh  defines the cutoff frequecy at high freqs.
     */
    private void idealBPKernel2D(float FLow, float FHigh) {
        int x, y, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealBPKernel2D: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (y = 0; y <= (kDim - 1); y++) {

            for (x = 0; x <= (kDim - 1); x++) {
                pos = (y * kDim) + x;
                distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)));

                if ((x == halfKDim) && (y == halfKDim)) {
                    iKernel[pos] = ((FHigh * FHigh) - (FLow * FLow)) / ((float) (4.0 * Math.PI));
                } else if (distance < tau) {
                    iKernel[pos] = (FHigh / ((float) (2.0 * Math.PI * distance)) * bessj1(FHigh * distance)) -
                                   (FLow / ((float) (2.0 * Math.PI * distance)) * bessj1(FLow * distance));
                } else {
                    iKernel[pos] = 0.0f;
                }

            }
        }
    }

    /**
     * Builds the idealized bandpass kernel in 2 dimensions.
     *
     * @param  FLow   defines the cutoff frequecy at low freqs.
     * @param  FHigh  defines the cutoff frequecy at high freqs.
     */
    private void idealBPKernel3D(float FLow, float FHigh) {
        int x, y, z, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealBPKernel3D: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (z = 0; z <= (kDim - 1); z++) {

            for (y = 0; y <= (kDim - 1); y++) {

                for (x = 0; x <= (kDim - 1); x++) {
                    pos = (z * kDim * kDim) + (y * kDim) + x;
                    distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                                 ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = ((FHigh * FHigh) - (FLow * FLow)) / ((float) (4.0 * Math.PI));
                    } else if (distance < tau) {
                        iKernel[pos] = (FHigh / ((float) (2.0 * Math.PI * distance)) * bessj1(FHigh * distance)) -
                                       (FLow / ((float) (2.0 * Math.PI * distance)) * bessj1(FLow * distance));
                    } else {
                        iKernel[pos] = 0.0f;
                    }
                }
            }
        }
    }

    /**
     * Builds the idealized bandstop kernel in 2 dimensions.
     *
     * @param  FLow   defines the cutoff frequecy at low freqs.
     * @param  FHigh  defines the cutoff frequecy at high freqs.
     */
    private void idealBSKernel2D(float FLow, float FHigh) {
        int x, y, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealBSKernel2D: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (y = 0; y <= (kDim - 1); y++) {

            for (x = 0; x <= (kDim - 1); x++) {
                pos = (y * kDim) + x;
                distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)));

                if ((x == halfKDim) && (y == halfKDim)) {
                    iKernel[pos] = 1.0f - (((FHigh * FHigh) - (FLow * FLow)) / ((float) (4.0 * Math.PI)));
                } else if (distance < tau) {
                    iKernel[pos] = (-FHigh / ((float) (2.0 * Math.PI * distance)) * bessj1(FHigh * distance)) +
                                   (FLow / ((float) (2.0 * Math.PI * distance)) * bessj1(FLow * distance));
                } else {
                    iKernel[pos] = 0.0f;
                }

            }
        }
    }

    /**
     * Builds the idealized bandstop kernel in 3 dimensions.
     *
     * @param  FLow   defines the cutoff frequecy at low freqs.
     * @param  FHigh  defines the cutoff frequecy at high freqs.
     */
    private void idealBSKernel3D(float FLow, float FHigh) {
        int x, y, z, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealBSKernel2D: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (z = 0; z <= (kDim - 1); z++) {

            for (y = 0; y <= (kDim - 1); y++) {

                for (x = 0; x <= (kDim - 1); x++) {
                    pos = (z * kDim * kDim) + (y * kDim) + x;
                    distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                                 ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = 1.0f - (((FHigh * FHigh) - (FLow * FLow)) / ((float) (4.0 * Math.PI)));
                    } else if (distance < tau) {
                        iKernel[pos] = (-FHigh / ((float) (2.0 * Math.PI * distance)) * bessj1(FHigh * distance)) +
                                       (FLow / ((float) (2.0 * Math.PI * distance)) * bessj1(FLow * distance));
                    } else {
                        iKernel[pos] = 0.0f;
                    }
                }
            }
        }
    }

    /**
     * Builds the idealized highpass kernel in 2 dimensions.
     *
     * @param  cutoffFreq  defines the cutoff frequecy
     */
    private void idealHPKernel2D(float cutoffFreq) {
        int x, y, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealHP2DKernel: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (y = 0; y <= (kDim - 1); y++) {

            for (x = 0; x <= (kDim - 1); x++) {
                pos = (y * kDim) + x;
                distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)));

                if ((x == halfKDim) && (y == halfKDim)) {
                    iKernel[pos] = 1.0f - ((cutoffFreq * cutoffFreq) / ((float) (4.0 * Math.PI)));
                } else if (distance < tau) {
                    iKernel[pos] = -cutoffFreq / ((float) (2.0 * Math.PI * distance)) * bessj1(cutoffFreq * distance);
                } else {
                    iKernel[pos] = 0.0f;
                }

            }
        }
    }

    /**
     * Builds the idealized highpass kernel in 3 dimensions.
     *
     * @param  cutoffFreq  defines the cutoff frequecy
     */
    private void idealHPKernel3D(float cutoffFreq) {
        int x, y, z, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealHPKernel3D: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (z = 0; z <= (kDim - 1); z++) {

            for (y = 0; y <= (kDim - 1); y++) {

                for (x = 0; x <= (kDim - 1); x++) {
                    pos = (z * kDim * kDim) + (y * kDim) + x;
                    distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                                 ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = 1.0f - ((cutoffFreq * cutoffFreq) / ((float) (4.0 * Math.PI)));
                    } else if (distance < tau) {
                        iKernel[pos] = -cutoffFreq / ((float) (2.0 * Math.PI * distance)) *
                                           bessj1(cutoffFreq * distance);
                    } else {
                        iKernel[pos] = 0.0f;
                    }
                }
            }
        }
    }

    /**
     * Builds the idealized lowpass kernel in 2 dimensions.
     *
     * @param  cutoffFreq  defines the cutoff frequecy
     */
    private void idealLPKernel2D(float cutoffFreq) {
        int x, y, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealLPKernel: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (y = 0; y <= (kDim - 1); y++) {

            for (x = 0; x <= (kDim - 1); x++) {
                pos = (y * kDim) + x;
                distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)));

                if ((x == halfKDim) && (y == halfKDim)) {
                    iKernel[pos] = (cutoffFreq * cutoffFreq) / ((float) (4.0 * Math.PI));
                } else if (distance < tau) {
                    iKernel[pos] = cutoffFreq / ((float) (2.0 * Math.PI * distance)) * bessj1(cutoffFreq * distance);
                } else {
                    iKernel[pos] = 0.0f;
                }

            }
        }
    }

    /**
     * Builds the idealized lowpass kernel in 3 dimensions.
     *
     * @param  cutoffFreq  defines the cutoff frequecy
     */
    private void idealLPKernel3D(float cutoffFreq) {
        int x, y, z, halfKDim, pos;
        float distance, tau;

        halfKDim = (kDim - 1) / 2;
        tau = (float) (halfKDim + 1);

        try {
            iKernel = new float[kDim * kDim * kDim];
        } catch (OutOfMemoryError e) {
            iKernel = null;
            System.gc();
            displayError("idealLPKernel3D: Out of memory creating iKernel");
            setCompleted(false);

            return;
        }

        for (z = 0; z <= (kDim - 1); z++) {

            for (y = 0; y <= (kDim - 1); y++) {

                for (x = 0; x <= (kDim - 1); x++) {
                    pos = (z * kDim * kDim) + (y * kDim) + x;
                    distance = (float) Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                                 ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = (cutoffFreq * cutoffFreq) / ((float) (4.0 * Math.PI));
                    } else if (distance < tau) {
                        iKernel[pos] = cutoffFreq / ((float) (2.0 * Math.PI * distance)) *
                                           bessj1(cutoffFreq * distance);
                    } else {
                        iKernel[pos] = 0.0f;
                    }
                }
            }
        }
    }

    /**
     * Calculates magnitude from real and imaginary parts magnitude = ( (realData)^2 + (imagData)^2 )^(1/2);
     */
    private void magnitude() {

        int i;

        try {
            magData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            magData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating magData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < arrayLength; i++) {
            magData[i] = (float) java.lang.Math.sqrt((realData[i] * realData[i]) + (imagData[i] * imagData[i]));
        }
    }

    /**
     * Builds a Butterworth filter.
     *
     * @param  fr1  DOCUMENT ME!
     * @param  fr2  DOCUMENT ME!
     */
    private void makeButterworthFilter(float fr1, float fr2) {
        int x, y, z, pos;
        float distsq, width, centersq, coeff, num, xnorm, ynorm, znorm, xcenter, ycenter, zcenter;
        int upperZ;
        xcenter = (newDimLengths[0] - 1.0f) / 2.0f;
        ycenter = (newDimLengths[1] - 1.0f) / 2.0f;
        xnorm = xcenter * xcenter;
        ynorm = ycenter * ycenter;

        if ((ndim == 2) || (image25D)) {

            if (image25D) {
                upperZ = newDimLengths[2] - 1;
            } else {
                upperZ = 0;
            }

            if (filterType == LOWPASS) {

                for (z = 0; z <= upperZ; z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm);
                            coeff = (float) (1.0 / (1.0 + Math.pow(distsq / (fr1 * fr1), butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == LOWPASS)
            else if (filterType == HIGHPASS) {

                for (z = 0; z <= upperZ; z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm);
                            coeff = (float) (1.0 / (1.0 + Math.pow((fr1 * fr1) / distsq, butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == HIGHPASS)
            else if (filterType == BANDPASS) {
                width = fr2 - fr1;
                centersq = fr1 * fr2;

                for (z = 0; z <= upperZ; z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm);
                            num = (float) Math.pow(Math.sqrt(distsq) * width, 2.0 * butterworthOrder);
                            coeff = (float) (num / (num + Math.pow((distsq - centersq), 2.0 * butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == BANDPASS)
            else if (filterType == BANDSTOP) {
                width = fr2 - fr1;
                centersq = fr1 * fr2;

                for (z = 0; z <= upperZ; z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm);
                            num = (float) Math.pow((distsq - centersq), 2.0 * butterworthOrder);
                            coeff = (float) (num / (num + Math.pow(Math.sqrt(distsq) * width, 2.0 * butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // else if (filterType == BANDSTOP)
        } // if ((ndim == 2) || (image25D))
        else if (ndim == 3) {
            zcenter = (newDimLengths[2] - 1.0f) / 2.0f;
            znorm = zcenter * zcenter;

            if (filterType == LOWPASS) {

                for (z = 0; z <= (newDimLengths[2] - 1); z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newDimLengths[0] * newDimLengths[1]) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm) +
                                     ((z - zcenter) * (z - zcenter) / znorm);
                            coeff = (float) (1.0 / (1.0 + Math.pow(distsq / (fr1 * fr1), butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == LOWPASS)
            else if (filterType == HIGHPASS) {

                for (z = 0; z <= (newDimLengths[2] - 1); z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newDimLengths[0] * newDimLengths[1]) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm) +
                                     ((z - zcenter) * (z - zcenter) / znorm);
                            coeff = (float) (1.0 / (1.0 + Math.pow((fr1 * fr1) / distsq, butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == HIGHPASS)
            else if (filterType == BANDPASS) {
                width = fr2 - fr1;
                centersq = fr1 * fr2;

                for (z = 0; z <= (newDimLengths[2] - 1); z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newDimLengths[0] * newDimLengths[1]) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm) +
                                     ((z - zcenter) * (z - zcenter) / znorm);
                            num = (float) Math.pow(Math.sqrt(distsq) * width, 2.0 * butterworthOrder);
                            coeff = (float) (num / (num + Math.pow((distsq - centersq), 2.0 * butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == BANDPASS)
            else if (filterType == BANDSTOP) {
                width = fr2 - fr1;
                centersq = fr1 * fr2;

                for (z = 0; z <= (newDimLengths[2] - 1); z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newDimLengths[0] * newDimLengths[1]) + (y * newDimLengths[0]) + x;
                            distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm) +
                                     ((z - zcenter) * (z - zcenter) / znorm);
                            num = (float) Math.pow((distsq - centersq), 2.0 * butterworthOrder);
                            coeff = (float) (num / (num + Math.pow(Math.sqrt(distsq) * width, 2.0 * butterworthOrder)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of else if (filterType == BANDSTOP)
        } // end of else if (ndim == 3)
    }

    /* l'Hopitals rule if f(a) = g(a) = 0 and if the limit of the ratio f'(t)/g'(t) as t approaches a exists,
     * then lim t->a f(t)/g(t) = lim t->a f'(t)/g'(t).  The hlp(n1,n2)low pass impulse response may be regarded as
     * ((R*R)/(2*PI))*J(R*sqrt(n1*n1 + n2*n2))/(R*sqrt(n1*n1 + n2*n2)).  The taylor series for J1(x) = (x/2) -
     * ((x**3)/16) + ...  Therefore, the derivative of J1(x) at x = 0 equals 1/2.  Hence, thehlp(n1,n2)low pass impulse
     * response is equal to (R**2)/(4*PI) at n1=n2=0. */

    /**
     * makeComplexData.
     */
    private void makeComplexData() {

        int i, j, k, m, n, dimTest;
        float[] tempData;

        ndim = srcImage.getNDims();
        dimLengths = srcImage.getExtents();
        newDimLengths = new int[dimLengths.length];

        if (transformDir == FORWARD) {
            doCrop = false;
        }

        zeroPad = false;

        // If imageCrop is false:
        // Find the lowest power of 2 number not less than kdim + (dimLengths[i]) - 1.
        // If imageCrop is true:
        // Find the lowest power of 2 number not less than (dimLengths[i])
        // This must be done to prevent aliasing in using a frequency filter,
        // and to have a power of 2 for the FFT.
        // Make all dimensions equal to this the largest of the above dimensions if
        // unequalDim is false

        arrayLength = 1;

        for (i = 0; i < ndim; i++) {
            arrayLength *= dimLengths[i];
            newDimLengths[i] = dimLengths[i];
        }

        if ((transformDir == FORWARD) && (imageCrop == false) && (constructionMethod == WINDOW)) {

            if (image25D) {
                newDimLengths[0] = newDimLengths[0] + kDim - 1;
                newDimLengths[1] = newDimLengths[1] + kDim - 1;
            } // if (image25D
            else { // not image25D

                for (i = 0; i < ndim; i++) {
                    newDimLengths[i] = newDimLengths[i] + kDim - 1;
                }
            } // else not image25D
        }

        if (image25D) {

            for (i = 0; i < 2; i++) {

                for (dimTest = newDimLengths[i]; dimTest > 1; dimTest = dimTest / 2) {

                    if ((dimTest % 2) != 0) {
                        dimTest = 1;

                        // log2x = logex/loge2
                        newDimLengths[i] = 1 + (int) (Math.log((double) newDimLengths[i]) / Math.log(2.0));
                        newDimLengths[i] = Math.round((float) (Math.pow(2.0, (double) newDimLengths[i])));
                    }
                }
            } // for (i = 0; i < 2; i++)
        } // if (image25D)
        else { // not image25D

            for (i = 0; i < ndim; i++) {

                for (dimTest = newDimLengths[i]; dimTest > 1; dimTest = dimTest / 2) {

                    if ((dimTest % 2) != 0) {
                        dimTest = 1;

                        // log2x = logex/loge2
                        newDimLengths[i] = 1 + (int) (Math.log((double) newDimLengths[i]) / Math.log(2.0));
                        newDimLengths[i] = Math.round((float) (Math.pow(2.0, (double) newDimLengths[i])));
                    }
                }
            } // for (i = 0; i < ndim; i++)
        } // else not image25D

        if (!unequalDim) { // if dimensions are to be made equal
            newLength = 0;

            if (image25D) {

                for (i = 0; i < 2; i++) {

                    if (newDimLengths[i] > newLength) {
                        newLength = newDimLengths[i];
                    }
                }

                for (i = 0; i < 2; i++) {
                    newDimLengths[i] = newLength;
                }
            } else { // not image25D

                for (i = 0; i < ndim; i++) {

                    if (newDimLengths[i] > newLength) {
                        newLength = newDimLengths[i];
                    }
                }

                for (i = 0; i < ndim; i++) {
                    newDimLengths[i] = newLength;
                }
            } // else not image25D
        } // end of if (!unequalDim)

        newArrayLength = 1;

        for (i = 0; i < ndim; i++) {
            newArrayLength *= newDimLengths[i];
        }

        newSliceSize = newDimLengths[0] * newDimLengths[1];

        if (newArrayLength > arrayLength) {
            zeroPad = true;
        }

        if ((transformDir == FORWARD) && (imageCrop == true) && (constructionMethod == WINDOW)) {

            if (image25D && (ndim >= 3)) {

                for (i = 0; i < 2; i++) {

                    if ((dimLengths[i] + kDim - 1) > newDimLengths[i]) {
                        doCrop = true;
                    }
                }
            } // if ((image25D && (ndim >= 3)))
            else { // not image25D

                for (i = 0; i < ndim; i++) {

                    if ((dimLengths[i] + kDim - 1) > newDimLengths[i]) {
                        doCrop = true;
                    }
                }
            } // else not image25D
        } // end of if ((transformDir == FORWARD) && (imageCrop == true) && (constructionMethod == WINDOW))

        if ((transformDir == FORWARD) && (doCrop)) {
            start = new int[ndim];
            end = new int[ndim];

            if (image25D && (ndim >= 3)) {

                for (i = 0; i < 2; i++) {

                    if ((dimLengths[i] + kDim - 1) > newDimLengths[i]) {
                        start[i] = ((dimLengths[i] + kDim - 1 - newDimLengths[i]) / 2) +
                                   ((dimLengths[i] + kDim - 1 - newDimLengths[i]) % 2);
                        end[i] = dimLengths[i] - 1 - ((dimLengths[i] + kDim - 1 - newDimLengths[i]) / 2);
                    } else {
                        start[i] = 0;
                        end[i] = dimLengths[i] - 1;
                    }
                }

                start[2] = 0;
                end[2] = dimLengths[2] - 1;
            } // if (image25D)
            else { // not image25D

                for (i = 0; i < ndim; i++) {

                    if ((dimLengths[i] + kDim - 1) > newDimLengths[i]) {
                        start[i] = ((dimLengths[i] + kDim - 1 - newDimLengths[i]) / 2) +
                                   ((dimLengths[i] + kDim - 1 - newDimLengths[i]) % 2);
                        end[i] = dimLengths[i] - 1 - ((dimLengths[i] + kDim - 1 - newDimLengths[i]) / 2);
                    } else {
                        start[i] = 0;
                        end[i] = dimLengths[i] - 1;
                    }
                }
            } // else not image25D
        } // end of if ((transformDir == FORWARD) && (doCrop))

        try {
            realData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            realData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating realData");

            setCompleted(false);

            return;
        }

        try {
            imagData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            imagData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating imagData");

            setCompleted(false);

            return;
        }

        try {

            if (srcImage.getType() != ModelStorageBase.COMPLEX) {
                srcImage.exportData(0, arrayLength, realData); // locks and releases and lock

                // If the data is all real, then create an equal size imagData array and
                // fill it with zeros.
                Arrays.fill(imagData, 0.0f);
            } else {
                srcImage.exportComplexData(0, arrayLength, realData, imagData);
            }
        } catch (IOException error) {
            displayError("AlgorithmFFT: Source image is locked");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            realData = null;
            imagData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory");

            setCompleted(false);

            return;
        }

        if (transformDir == FORWARD) {
            minimum = Float.MAX_VALUE;
            maximum = -Float.MAX_VALUE;

            for (i = 0; i < arrayLength; i++) {

                if (realData[i] > maximum) {
                    maximum = realData[i];
                }

                if (realData[i] < minimum) {
                    minimum = realData[i];
                }
            }
        } // end of if (transformDir == FORWARD)

        if ((transformDir == FORWARD) && (doCrop)) {

            try {
                tempData = new float[arrayLength];
            } catch (OutOfMemoryError e) {
                tempData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating tempData for cropping");

                setCompleted(false);

                return;
            }

            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = start[0]; i <= end[0]; i++) {
                    tempData[i - start[0]] = realData[i];
                }
            } else if (ndim == 2) {

                for (j = start[1]; j <= end[1]; j++) {

                    for (i = start[0]; i <= end[0]; i++) {
                        tempData[i - start[0] + (dimLengths[0] * (j - start[1]))] = realData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = start[2]; k <= end[2]; k++) {

                    for (j = start[1]; j <= end[1]; j++) {

                        for (i = start[0]; i <= end[0]; i++) {
                            tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                     (dimLengths[0] * dimLengths[1] * (k - start[2]))] = realData[i +
                                                                                                  (dimLengths[0] * j) +
                                                                                                  (dimLengths[0] *
                                                                                                       dimLengths[1] *
                                                                                                       k)];
                        }
                    }
                }
            } else if (ndim == 4) {

                for (m = start[3]; m <= end[3]; m++) {

                    for (k = start[2]; k <= end[2]; k++) {

                        for (j = start[1]; j <= end[1]; j++) {

                            for (i = start[0]; i <= end[0]; i++) {
                                tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                         (dimLengths[0] * dimLengths[1] * (k - start[2])) +
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m - start[3]))] = realData[i +
                                                                                                                      (dimLengths[0] *
                                                                                                                           j) +
                                                                                                                      (dimLengths[0] *
                                                                                                                           dimLengths[1] *
                                                                                                                           k) +
                                                                                                                      (dimLengths[0] *
                                                                                                                           dimLengths[1] *
                                                                                                                           dimLengths[2] *
                                                                                                                           m)];
                            }
                        }
                    }
                }
            } else if (ndim == 5) {

                for (n = start[4]; n <= end[4]; n++) {

                    for (m = start[3]; m <= end[3]; m++) {

                        for (k = start[2]; k <= end[2]; k++) {

                            for (j = start[1]; j <= end[1]; j++) {

                                for (i = start[0]; i <= end[0]; i++) {
                                    tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                             (dimLengths[0] * dimLengths[1] * (k - start[2])) +
                                             (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m - start[3])) +
                                             (dimLengths[0] * dimLengths[1] * dimLengths[2] * dimLengths[3] *
                                                  (n - start[4]))] = realData[i + (dimLengths[0] * j) +
                                                                              (dimLengths[0] * dimLengths[1] * k) +
                                                                              (dimLengths[0] * dimLengths[1] *
                                                                                   dimLengths[2] * m) +
                                                                              (dimLengths[0] * dimLengths[1] *
                                                                                   dimLengths[2] * dimLengths[3] * n)];
                                }
                            }
                        }
                    }
                }
            }

            for (i = 0; i < arrayLength; i++) {
                realData[i] = tempData[i];
            }

            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = start[0]; i <= end[0]; i++) {
                    tempData[i - start[0]] = imagData[i];
                }
            } else if (ndim == 2) {

                for (j = start[1]; j <= end[1]; j++) {

                    for (i = start[0]; i <= end[0]; i++) {
                        tempData[i - start[0] + (dimLengths[0] * (j - start[1]))] = imagData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = start[2]; k <= end[2]; k++) {

                    for (j = start[1]; j <= end[1]; j++) {

                        for (i = start[0]; i <= end[0]; i++) {
                            tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                     (dimLengths[0] * dimLengths[1] * (k - start[2]))] = imagData[i +
                                                                                                  (dimLengths[0] * j) +
                                                                                                  (dimLengths[0] *
                                                                                                       dimLengths[1] *
                                                                                                       k)];
                        }
                    }
                }
            } else if (ndim == 4) {

                for (m = start[3]; m <= end[3]; m++) {

                    for (k = start[2]; k <= end[2]; k++) {

                        for (j = start[1]; j <= end[1]; j++) {

                            for (i = start[0]; i <= end[0]; i++) {
                                tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                         (dimLengths[0] * dimLengths[1] * (k - start[2])) +
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m - start[3]))] = imagData[i +
                                                                                                                      (dimLengths[0] *
                                                                                                                           j) +
                                                                                                                      (dimLengths[0] *
                                                                                                                           dimLengths[1] *
                                                                                                                           k) +
                                                                                                                      (dimLengths[0] *
                                                                                                                           dimLengths[1] *
                                                                                                                           dimLengths[2] *
                                                                                                                           m)];
                            }
                        }
                    }
                }
            } else if (ndim == 5) {

                for (n = start[4]; n <= end[4]; n++) {

                    for (m = start[3]; m <= end[3]; m++) {

                        for (k = start[2]; k <= end[2]; k++) {

                            for (j = start[1]; j <= end[1]; j++) {

                                for (i = start[0]; i <= end[0]; i++) {
                                    tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                             (dimLengths[0] * dimLengths[1] * (k - start[2])) +
                                             (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m - start[3])) +
                                             (dimLengths[0] * dimLengths[1] * dimLengths[2] * dimLengths[3] *
                                                  (n - start[4]))] = imagData[i + (dimLengths[0] * j) +
                                                                              (dimLengths[0] * dimLengths[1] * k) +
                                                                              (dimLengths[0] * dimLengths[1] *
                                                                                   dimLengths[2] * m) +
                                                                              (dimLengths[0] * dimLengths[1] *
                                                                                   dimLengths[2] * dimLengths[3] * n)];
                                }
                            }
                        }
                    }
                }
            }

            for (i = 0; i < arrayLength; i++) {
                imagData[i] = tempData[i];
            }

        } // end of if ((transformDir == FORWARD) && (doCrop))

        if (zeroPad) {

            // zero pad the data so that all dimensions are powers of 2
            fireProgressStateChanged(-1, null, "Zero padding source data ...");
         
            try {
                tempData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                tempData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating tempData for zero padding");

                setCompleted(false);

                return;
            }


            if (ndim == 1) {

                for (i = 0; i < dimLengths[0]; i++) {
                    tempData[i] = realData[i];
                }
            } else if (ndim == 2) {

                for (j = 0; j < dimLengths[1]; j++) {

                    for (i = 0; i < dimLengths[0]; i++) {
                        tempData[i + (newDimLengths[0] * j)] = realData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = 0; k < dimLengths[2]; k++) {

                    for (j = 0; j < dimLengths[1]; j++) {

                        for (i = 0; i < dimLengths[0]; i++) {
                            tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k)] = realData[i +
                                                                                                                        (dimLengths[0] *
                                                                                                                             j) +
                                                                                                                        (dimLengths[0] *
                                                                                                                             dimLengths[1] *
                                                                                                                             k)];
                        }
                    }
                }
            } else if (ndim == 4) {

                for (m = 0; m < dimLengths[3]; m++) {

                    for (k = 0; k < dimLengths[2]; k++) {

                        for (j = 0; j < dimLengths[1]; j++) {

                            for (i = 0; i < dimLengths[0]; i++) {
                                tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k) +
                                         (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] * m)] = realData[i +
                                                                                                                  (dimLengths[0] *
                                                                                                                       j) +
                                                                                                                  (dimLengths[0] *
                                                                                                                       dimLengths[1] *
                                                                                                                       k) +
                                                                                                                  (dimLengths[0] *
                                                                                                                       dimLengths[1] *
                                                                                                                       dimLengths[2] *
                                                                                                                       m)];
                            }
                        }
                    }
                }
            } else if (ndim == 5) {

                for (n = 0; n < dimLengths[4]; n++) {

                    for (m = 0; m < dimLengths[3]; m++) {

                        for (k = 0; k < dimLengths[2]; k++) {

                            for (j = 0; j < dimLengths[1]; j++) {

                                for (i = 0; i < dimLengths[0]; i++) {
                                    tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k) +
                                             (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] * m) +
                                             (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] *
                                                  newDimLengths[3] * n)] = realData[i + (dimLengths[0] * j) +
                                                                                    (dimLengths[0] * dimLengths[1] *
                                                                                         k) +
                                                                                    (dimLengths[0] * dimLengths[1] *
                                                                                         dimLengths[2] * m) +
                                                                                    (dimLengths[0] * dimLengths[1] *
                                                                                         dimLengths[2] * dimLengths[3] *
                                                                                         n)];
                                }
                            }
                        }
                    }
                }
            }

            try {
                realData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                realData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating realData in zero padding routine");

                setCompleted(false);

                return;
            }

            for (i = 0; i < newArrayLength; i++) {
                realData[i] = tempData[i];
            }

            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = 0; i < dimLengths[0]; i++) {
                    tempData[i] = imagData[i];
                }
            } else if (ndim == 2) {

                for (j = 0; j < dimLengths[1]; j++) {

                    for (i = 0; i < dimLengths[0]; i++) {
                        tempData[i + (newDimLengths[0] * j)] = imagData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = 0; k < dimLengths[2]; k++) {

                    for (j = 0; j < dimLengths[1]; j++) {

                        for (i = 0; i < dimLengths[0]; i++) {
                            tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k)] = imagData[i +
                                                                                                                        (dimLengths[0] *
                                                                                                                             j) +
                                                                                                                        (dimLengths[0] *
                                                                                                                             dimLengths[1] *
                                                                                                                             k)];
                        }
                    }
                }
            } else if (ndim == 4) {

                for (m = 0; m < dimLengths[3]; m++) {

                    for (k = 0; k < dimLengths[2]; k++) {

                        for (j = 0; j < dimLengths[1]; j++) {

                            for (i = 0; i < dimLengths[0]; i++) {
                                tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k) +
                                         (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] * m)] = imagData[i +
                                                                                                                  (dimLengths[0] *
                                                                                                                       j) +
                                                                                                                  (dimLengths[0] *
                                                                                                                       dimLengths[1] *
                                                                                                                       k) +
                                                                                                                  (dimLengths[0] *
                                                                                                                       dimLengths[1] *
                                                                                                                       dimLengths[2] *
                                                                                                                       m)];
                            }
                        }
                    }
                }
            } else if (ndim == 5) {

                for (n = 0; n < dimLengths[4]; n++) {

                    for (m = 0; m < dimLengths[3]; m++) {

                        for (k = 0; k < dimLengths[2]; k++) {

                            for (j = 0; j < dimLengths[1]; j++) {

                                for (i = 0; i < dimLengths[0]; i++) {
                                    tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k) +
                                             (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] * m) +
                                             (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] *
                                                  newDimLengths[3] * n)] = imagData[i + (dimLengths[0] * j) +
                                                                                    (dimLengths[0] * dimLengths[1] *
                                                                                         k) +
                                                                                    (dimLengths[0] * dimLengths[1] *
                                                                                         dimLengths[2] * m) +
                                                                                    (dimLengths[0] * dimLengths[1] *
                                                                                         dimLengths[2] * dimLengths[3] *
                                                                                         n)];
                                }
                            }
                        }
                    }
                }
            }

            try {
                imagData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                imagData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating imagData in zero padding routine");

                setCompleted(false);

                return;
            }

            for (i = 0; i < newArrayLength; i++) {
                imagData[i] = tempData[i];
            }
        } // end of if (zeroPad)
    } // end of makeComplexData()

    /**
     * DOCUMENT ME!
     *
     * @param  freqU   DOCUMENT ME!
     * @param  freqV   DOCUMENT ME!
     * @param  sigmaU  DOCUMENT ME!
     * @param  sigmaV  DOCUMENT ME!
     * @param  theta   DOCUMENT ME!
     */
    private void makeGaborFilter(float freqU, float freqV, float sigmaU, float sigmaV, float theta) {
        int x, y, z, pos;
        int upperZ;
        float xcenter, ycenter;
        float xScale, yScale;
        float u, v;
        float cosTheta;
        float sinTheta;
        double xDenom, yDenom;
        float coeff;

        xcenter = (newDimLengths[0] - 1.0f) / 2.0f;
        ycenter = (newDimLengths[1] - 1.0f) / 2.0f;
        cosTheta = (float) Math.cos((double) theta);
        sinTheta = (float) Math.sin((double) theta);
        xDenom = 2.0 * sigmaU * sigmaU;
        yDenom = 2.0 * sigmaV * sigmaV;

        if (image25D) {
            upperZ = newDimLengths[2] - 1;
        } else {
            upperZ = 0;
        }

        for (z = 0; z <= upperZ; z++) {

            for (y = 0; y <= (newDimLengths[1] - 1); y++) {
                yScale = (y - ycenter) / ycenter;

                for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                    xScale = (x - xcenter) / xcenter;
                    pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                    u = ((xScale - freqU) * cosTheta) + ((yScale - freqV) * sinTheta);
                    v = ((yScale - freqV) * cosTheta) - ((xScale - freqU) * sinTheta);
                    coeff = (float) Math.exp(-((u * u / xDenom) + (v * v / yDenom)));
                    realData[pos] *= coeff;
                    imagData[pos] *= coeff;
                }
            }
        }
    } // private void makeGaborFilter

    /**
     * Builds a Gaussian filter with the root mean square frequency.
     *
     * @param  rmsFreq  root mean square frequency
     */
    private void makeGaussianFilter(float rmsFreq) {
        double xexpDenom, yexpDenom, zexpDenom;
        int x, y, z, pos;
        int upperZ;
        float coeff, xcenter, ycenter, zcenter;

        xcenter = (newDimLengths[0] - 1.0f) / 2.0f;
        ycenter = (newDimLengths[1] - 1.0f) / 2.0f;

        xexpDenom = 2.0 * rmsFreq * rmsFreq * xcenter * xcenter;
        yexpDenom = 2.0 * rmsFreq * rmsFreq * ycenter * ycenter;

        if ((ndim == 2) || (image25D)) {

            if (image25D) {
                upperZ = newDimLengths[2] - 1;
            } else {
                upperZ = 0;
            }

            if (filterType == LOWPASS) {

                for (z = 0; z <= upperZ; z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                            coeff = (float) (Math.exp(-(x - xcenter) * (x - xcenter) / xexpDenom) *
                                                 Math.exp(-(y - ycenter) * (y - ycenter) / yexpDenom));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == LOWPASS)
            else if (filterType == HIGHPASS) {

                for (z = 0; z <= upperZ; z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                            coeff = (float) (1.0 -
                                             (Math.exp(-(x - xcenter) * (x - xcenter) / xexpDenom) *
                                                  Math.exp(-(y - ycenter) * (y - ycenter) / yexpDenom)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == HIGHPASS)
        } // end of if ((ndim == 2) || (image25D))
        else if (ndim == 3) {
            zcenter = (newDimLengths[2] - 1.0f) / 2.0f;
            zexpDenom = 2.0 * rmsFreq * rmsFreq * zcenter * zcenter;

            if (filterType == LOWPASS) {

                for (z = 0; z <= (newDimLengths[2] - 1); z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newDimLengths[0] * newDimLengths[1]) + (y * newDimLengths[0]) + x;
                            coeff = (float) (Math.exp(-(x - xcenter) * (x - xcenter) / xexpDenom) *
                                                 Math.exp(-(y - ycenter) * (y - ycenter) / yexpDenom) *
                                                 Math.exp(-(z - zcenter) * (z - zcenter) / zexpDenom));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == LOWPASS)
            else if (filterType == HIGHPASS) {

                for (z = 0; z <= (newDimLengths[2] - 1); z++) {

                    for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                        for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                            pos = (z * newDimLengths[0] * newDimLengths[1]) + (y * newDimLengths[0]) + x;
                            coeff = (float) (1.0 -
                                             (Math.exp(-(x - xcenter) * (x - xcenter) / xexpDenom) *
                                                  Math.exp(-(y - ycenter) * (y - ycenter) / yexpDenom) *
                                                  Math.exp(-(z - zcenter) * (z - zcenter) / zexpDenom)));
                            realData[pos] *= coeff;
                            imagData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == HIGHPASS)
        } // end of else if (ndim == 3)
    }

    /**
     * makeKernelData -
     */
    private void makeKernelData() {

        int i;
        int x, y, z, halfKDim, pos;

        halfKDim = (kDim - 1) / 2;

        if ((ndim == 2) || (image25D)) {

            switch (filterType) {

                case LOWPASS:
                    idealLPKernel2D(f1);
                    break;

                case HIGHPASS:
                    idealHPKernel2D(f1);
                    break;

                case BANDPASS:
                    idealBPKernel2D(f1, f2);
                    break;

                case BANDSTOP:
                    idealBSKernel2D(f1, f2);
                    break;

                default:
                    displayError("makeKernelData: no such filter type");
                 
                    setCompleted(false);

                    return;
            } // end of switch(filterType)

            hammingKernel2D();

            // Multiply the window kernel by the ideal filter kernel to smooth the filter edges
            // An ideal filter with a sudden cutoff cannot be used because severe ringing
            // will result.
            try {
                realKernelData = new float[newSliceSize];
            } catch (OutOfMemoryError e) {
                realKernelData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating realKernelData");

                setCompleted(false);

                return;
            }

            for (y = 0; y <= (kDim - 1); y++) {

                for (x = 0; x <= (kDim - 1); x++) {
                    pos = (y * kDim) + x;
                    realKernelData[x + (newDimLengths[0] * y)] = iKernel[pos] * wKernel[pos];
                }
            }

            try {
                imagKernelData = new float[newSliceSize];
            } catch (OutOfMemoryError e) {
                imagKernelData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating imagKernelData");


                setCompleted(false);

                return;
            }

        } // end of if ((ndim == 2) || (image25D))
        else if (ndim == 3) {

            switch (filterType) {

                case LOWPASS:
                    idealLPKernel3D(f1);
                    break;

                case HIGHPASS:
                    idealHPKernel3D(f1);
                    break;

                case BANDPASS:
                    idealBPKernel3D(f1, f2);
                    break;

                case BANDSTOP:
                    idealBSKernel3D(f1, f2);
                    break;

                default:
                    displayError("makeKernelData: no such filter type");
                 
                    setCompleted(false);

                    return;
            } // end of switch(filterType)

            hammingKernel3D();

            try {
                realKernelData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                realKernelData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating realKernelData");

                setCompleted(false);

                return;
            }

            for (z = 0; z <= (kDim - 1); z++) {

                for (y = 0; y <= (kDim - 1); y++) {

                    for (x = 0; x <= (kDim - 1); x++) {
                        pos = (z * kDim * kDim) + (y * kDim) + x;
                        realKernelData[(newDimLengths[0] * newDimLengths[1] * z) + (newDimLengths[0] * y) + x] = iKernel[pos] *
                                                                                                                     wKernel[pos];
                    }
                }
            }

            try {
                imagKernelData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                imagKernelData = null;
                System.gc();
                displayError("AlgorithmFFT: Out of memory creating imagKernelData");

                setCompleted(false);

                return;
            }
        } // end of if (ndim = 3)
    }

    /**
     * Calculates phase from real and imaginary parts.
     *
     * @param  phase  = arctan(imagData/realData);
     */
    private void phase(float[] phase) {

        int i;

        try {
            phaseData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            phaseData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating phaseData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < arrayLength; i++) {
            phase[i] = (float) java.lang.Math.atan2(imagData[i], realData[i]);
        }
    }

    /**
     * Shifts the FFT data back from the center for processing purposes and inverse FFT.
     *
     * @param  rData  DOCUMENT ME!
     */
    private void shiftBack(float[] rData) {
        int i, j, k;
        float[] tempData;
        int halfKDim;

        halfKDim = (kDim - 1) / 2;

        if (image25D) {
            newLength = newDimLengths[0] * newDimLengths[1];
        } else {
            newLength = newArrayLength;
        }

        // For FIR filters created with windowing a shift in every dimension of (kDim - 1)/2 toward
        // the end has occurred.  This shifts each dimension back toward the start by (kDim - 1)/2.
        try {
            tempData = new float[newLength];
        } catch (OutOfMemoryError e) {
            tempData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating tempData in shiftBack routine");

            setCompleted(false);

            return;
        }

        if (ndim == 1) {

            for (i = 0; i < halfKDim; i++) {
                tempData[i + newDimLengths[0] - halfKDim] = rData[i];
            }

            for (i = halfKDim; i < newDimLengths[0]; i++) {
                tempData[i - halfKDim] = rData[i];
            }
        } // end of if (ndim == 1)
        else if ((ndim == 2) || (image25D)) {

            for (j = 0; j < halfKDim; j++) {

                for (i = 0; i < halfKDim; i++) {
                    tempData[i + newDimLengths[0] - halfKDim + (newDimLengths[0] * (j + newDimLengths[1] - halfKDim))] = rData[i +
                                                                                                                               (newDimLengths[0] *
                                                                                                                                    j)];
                }

                for (i = halfKDim; i < newDimLengths[0]; i++) {
                    tempData[i - halfKDim + (newDimLengths[0] * (j + newDimLengths[1] - halfKDim))] = rData[i +
                                                                                                            (newDimLengths[0] *
                                                                                                                 j)];
                }
            }

            for (j = halfKDim; j < newDimLengths[1]; j++) {

                for (i = 0; i < halfKDim; i++) {
                    tempData[i + newDimLengths[0] - halfKDim + (newDimLengths[0] * (j - halfKDim))] = rData[i +
                                                                                                            (newDimLengths[0] *
                                                                                                                 j)];
                }

                for (i = halfKDim; i < newDimLengths[0]; i++) {
                    tempData[i - halfKDim + (newDimLengths[0] * (j - halfKDim))] = rData[i + (newDimLengths[0] * j)];
                }
            }
        } // else if ((ndim == 2) || (image25D))
        else if (ndim == 3) {

            for (k = 0; k < halfKDim; k++) {

                for (j = 0; j < halfKDim; j++) {

                    for (i = 0; i < halfKDim; i++) {
                        tempData[i + newDimLengths[0] - halfKDim +
                                 (newDimLengths[0] * (j + newDimLengths[1] - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k + newDimLengths[2] - halfKDim))] = rData[i +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         j) +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         newDimLengths[1] *
                                                                                                                         k)];
                    }

                    for (i = halfKDim; i < newDimLengths[0]; i++) {
                        tempData[i - halfKDim + (newDimLengths[0] * (j + newDimLengths[1] - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k + newDimLengths[2] - halfKDim))] = rData[i +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         j) +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         newDimLengths[1] *
                                                                                                                         k)];
                    }
                }

                for (j = halfKDim; j < newDimLengths[1]; j++) {

                    for (i = 0; i < halfKDim; i++) {
                        tempData[i + newDimLengths[0] - halfKDim + (newDimLengths[0] * (j - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k + newDimLengths[2] - halfKDim))] = rData[i +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         j) +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         newDimLengths[1] *
                                                                                                                         k)];
                    }

                    for (i = halfKDim; i < newDimLengths[0]; i++) {
                        tempData[i - halfKDim + (newDimLengths[0] * (j - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k + newDimLengths[2] - halfKDim))] = rData[i +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         j) +
                                                                                                                    (newDimLengths[0] *
                                                                                                                         newDimLengths[1] *
                                                                                                                         k)];
                    }
                }
            } // end of for (k = 0; k < halfKDim; k++)

            for (k = halfKDim; k < newDimLengths[2]; k++) {

                for (j = 0; j < halfKDim; j++) {

                    for (i = 0; i < halfKDim; i++) {
                        tempData[i + newDimLengths[0] - halfKDim +
                                 (newDimLengths[0] * (j + newDimLengths[1] - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k - halfKDim))] = rData[i +
                                                                                                 (newDimLengths[0] *
                                                                                                      j) +
                                                                                                 (newDimLengths[0] *
                                                                                                      newDimLengths[1] *
                                                                                                      k)];
                    }

                    for (i = halfKDim; i < newDimLengths[0]; i++) {
                        tempData[i - halfKDim + (newDimLengths[0] * (j + newDimLengths[1] - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k - halfKDim))] = rData[i +
                                                                                                 (newDimLengths[0] *
                                                                                                      j) +
                                                                                                 (newDimLengths[0] *
                                                                                                      newDimLengths[1] *
                                                                                                      k)];
                    }
                }

                for (j = halfKDim; j < newDimLengths[1]; j++) {

                    for (i = 0; i < halfKDim; i++) {
                        tempData[i + newDimLengths[0] - halfKDim + (newDimLengths[0] * (j - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k - halfKDim))] = rData[i +
                                                                                                 (newDimLengths[0] *
                                                                                                      j) +
                                                                                                 (newDimLengths[0] *
                                                                                                      newDimLengths[1] *
                                                                                                      k)];
                    }

                    for (i = halfKDim; i < newDimLengths[0]; i++) {
                        tempData[i - halfKDim + (newDimLengths[0] * (j - halfKDim)) +
                                 (newDimLengths[0] * newDimLengths[1] * (k - halfKDim))] = rData[i +
                                                                                                 (newDimLengths[0] *
                                                                                                      j) +
                                                                                                 (newDimLengths[0] *
                                                                                                      newDimLengths[1] *
                                                                                                      k)];
                    }
                }
            } // for (k = halfKDim; k < newDimLengths[2]; k++)
        } // else if (ndim == 3)

        for (i = 0; i < newLength; i++) {
            rData[i] = tempData[i];
        }
    } // end of private void shiftBack(float[] rData)

    /**
     * If a cropping operation was performed, shift the data back to the center and put zeros around the edges.
     */
    private void zeroAround() {
        int i, j, k, m, n;
        float[] tempData;

        try {
            tempData = new float[originalArrayLength];
        } catch (OutOfMemoryError e) {
            tempData = null;
            System.gc();
            displayError("AlgorithmFFT: Out of memory creating tempData in zeroAround routine");

            setCompleted(false);

            return;
        }

        start = srcImage.getOriginalStart();
        end = srcImage.getOriginalEnd();

        if (ndim == 1) {

            for (i = 0; i <= (end[0] - start[0]); i++) {
                tempData[i + start[0]] = finalData[i];
            }
        } else if (ndim == 2) {

            for (j = 0; j <= (end[1] - start[1]); j++) {

                for (i = 0; i <= (end[0] - start[0]); i++) {
                    tempData[i + start[0] + (originalDimLengths[0] * (j + start[1]))] = finalData[i +
                                                                                                  (originalDimLengths[0] *
                                                                                                       j)];
                }
            }
        } else if (ndim == 3) {

            for (k = 0; k <= (end[2] - start[2]); k++) {

                for (j = 0; j <= (end[1] - start[1]); j++) {

                    for (i = 0; i <= (end[0] - start[0]); i++) {
                        tempData[i + start[0] + (originalDimLengths[0] * (j + start[1])) +
                                 (originalDimLengths[0] * originalDimLengths[1] * (k + start[2]))] = finalData[i +
                                                                                                               (originalDimLengths[0] *
                                                                                                                    j) +
                                                                                                               (originalDimLengths[0] *
                                                                                                                    originalDimLengths[1] *
                                                                                                                    k)];
                    }
                }
            }
        } else if (ndim == 4) {

            for (m = 0; m <= (end[3] - start[3]); m++) {

                for (k = 0; k <= (end[2] - start[2]); k++) {

                    for (j = 0; j <= (end[1] - start[1]); j++) {

                        for (i = 0; i <= (end[0] - start[0]); i++) {
                            tempData[i + start[0] + (originalDimLengths[0] * (j + start[1])) +
                                     (originalDimLengths[0] * originalDimLengths[1] * (k + start[2])) +
                                     (originalDimLengths[0] * originalDimLengths[1] * originalDimLengths[2] *
                                          (m + start[3]))] = finalData[i + (originalDimLengths[0] * j) +
                                                                       (originalDimLengths[0] * originalDimLengths[1] *
                                                                            k) +
                                                                       (originalDimLengths[0] * originalDimLengths[1] *
                                                                            originalDimLengths[2] * m)];
                        }
                    }
                }
            }
        } else if (ndim == 5) {

            for (n = 0; n <= (end[4] - start[4]); n++) {

                for (m = 0; m <= (end[3] - start[3]); m++) {

                    for (k = 0; k <= (end[2] - start[2]); k++) {

                        for (j = 0; j <= (end[1] - start[1]); j++) {

                            for (i = 0; i <= (end[0] - start[0]); i++) {
                                tempData[i + start[0] + (originalDimLengths[0] * (j + start[1])) +
                                         (originalDimLengths[0] * originalDimLengths[1] * (k + start[2])) +
                                         (originalDimLengths[0] * originalDimLengths[1] * originalDimLengths[2] *
                                              (m + start[3])) +
                                         (originalDimLengths[0] * originalDimLengths[1] * originalDimLengths[2] *
                                              originalDimLengths[3] * (n + start[4]))] = finalData[i +
                                                                                                   (originalDimLengths[0] *
                                                                                                        j) +
                                                                                                   (originalDimLengths[0] *
                                                                                                        originalDimLengths[1] *
                                                                                                        k) +
                                                                                                   (originalDimLengths[0] *
                                                                                                        originalDimLengths[1] *
                                                                                                        originalDimLengths[2] *
                                                                                                        m) +
                                                                                                   (originalDimLengths[0] *
                                                                                                        originalDimLengths[1] *
                                                                                                        originalDimLengths[2] *
                                                                                                        originalDimLengths[3] *
                                                                                                        n)];
                            }
                        }
                    }
                }
            }
        }

        for (i = 0; i < originalArrayLength; i++) {
            finalData[i] = tempData[i];
        }
    }
}
