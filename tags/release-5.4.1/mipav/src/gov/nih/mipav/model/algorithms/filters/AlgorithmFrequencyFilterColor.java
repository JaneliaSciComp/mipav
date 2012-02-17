package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.Bessel;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Dimension;
import java.io.IOException;
import java.util.Arrays;


/**
 * AlgorithmFrequencyFilterColor.java.
 * 
 *          AlgorithmFrequencyFilterColor only works for 2D and 25D for color.  Quaternion Fourier transforms are
 *          used. 
 *          imagiData[i] = (float)((red + green + blue)/sqrt(3));
        	imagjData[i] = (float)((green - blue)/sqrt(2));
        	imagkData[i] = (float)((-2.0*red + green + blue)/sqrt(6));
        	imagi corresponds corresponds to luminance information and imagj and imagk correspond to
        	chrominance information.
        	u1 = (i + j + k)/sqrt(3)
        	u2 = (j - k)/sqrt(2)
        	where u1 and u2 are two unit pure quaternions that are orthogonal to each other.
        	u3 = u1 * u2 = (-2*i + j + k)/sqrt(6)
        	The zero valued real array and the imagiData are used for one complex FFT.  The imagjData and 
        	imagkData are used for a second complex FFT.  In the Butterworth and Gaussian filters the same
        	coefficient is multiplied by realData, imagiData, imagjData, and imagkData.  This works
        	because when h(x,y) = h(-x,-y), the quaternion convolution operation in the space domain
        	corresponds to the product operation in the frequency domain.  This is the same as the
        	case of the conventional convolution.  After inverse filtering red, green, and blue are formed
            form the processed luminance and chrominance data:
        	finalRData[i] = (lum/sqrt(3) - 2.0*chr2/sqrt(6));
            finalGData[i] = (lum/sqrt(3) + chr1/sqrt(2) + chr2/sqrt(6));
            finalBData[i] = (float)(lum/sqrt(3) - chr1/sqrt(2) + chr2/sqrt(6));
 *
 * @author  William Gandler and Matthew J. McAuliffe Processing images by filtering in the frequency domain is a 3 step
 *          process: 1.) Performing a forward fast fourier transform to convert a spatial image into its frequency
 *          image. 2.) Enhancing some frequency components of the image and attenuating other frequency components of
 *          the image by mulitplying by a lowpass, highpass, bandpass, or bandstop filter. Frequency filters may be
 *          constructed with 1 of 3 methods - finite impulse response filters constructed with Hamming windows, Gaussian
 *          filters, and Butterworth filters. However, for the Gaussian filters only lowpass and highpass filters are
 *          available. 3.) Performing an inverse fast fourier transform to convert from the frequency domain back into
 *          the spatial domain. This software module performs all 3 steps in one combined process. AlgorithmFFT is used
 *          to perform the steps one at a time and examine the Frequency space images.
 *
 *          <p>The module also creates a Gabor filter, which is essentially a tilted Gaussian with 2 unequal axes at an
 *          offset (freqU, freqV) from the origin. A Gabor filter only responds to a texture having both a particular
 *          frequency and a particular orientation. Note that a filter and its mirror image reflected across the u and v
 *          frequency axes produce identical frequency responses.</p>
 *
 *          <p>This module also performs homomorphic fitering in which if any data is zero or negative a positive
 *          constant is added to all pixels to give a minimum value of 1. Then the log of the data is taken, the fast
 *          fourier transform is performed, the high frequencies are multiplied by a gain greater than 1 while the low
 *          frequencies are multiplied by a gain less than 1, the inverse fast fourier transform is performed, and the
 *          exponential of the the data is taken. Optionally, a specified percentage of the data can be clamped at the
 *          low end and/or at the high end. Finally, the data is rescaled so that the original minimum and maximum
 *          values are achieved. Only Butterworth filters are used with homomorphic filters in this module. References
 *          on homomorphic filters: 1.) Digital Image Processing Second Edition by Rafael C. Gonzalez and Richard E.
 *          Woods, Prentice-Hall, Inc., 2002, Chapter 4.5, pp. 191-194. 2.) "Butterworth equations for homomorphic
 *          filtering of images" by Holger. G. Adelmann, Computers in Medicine and Biology, Vol. 28, 1998, pp. 169-181.
 *          </p>
 *          3.) "Efficient Implementation of Quanternion Fourier Transform, Convolution, and Correlation by 2-D
 *          Complex FFT" by Soo-Chang Pei, Jian-Jiun Ding, and Ja-Han Chang, IEEE Transactions on Signal Processing,
 *          Vol. 49, No. 11, November, 2001, pp. 2783-2797.
 *          4.) "Frequency Domain Filtering of Colour Images using Quaternion Fourier Transforms" by
 *          B.D. Venkatramana Redd and Dr. T. Jaychandra Prasad, IJSCT, VOl. 1, Issue 2, December, 2010,
 *          pp. 46-52.
 *
 *          <p>The core algorithm of this module, the fast fourier transform algorithm found in exec(), requires that
 *          all the dimensions of an N-dimensional dataset be powers of 2. To be able to use this algorithm on datasets
 *          with arbitrary dimensions, the data is zero padded to powers of 2 before applying the forward fast fourier
 *          transform and stripped down to the original dimensions after applying the inverse fast fourier transform.
 *          The powers of 2 are not kept identical because symmetrical Fourier pictures are not required.</p>
 *
 *          <p>The typical full sequence is as follows: 1.) Data from a real spatial image is exported into a float
 *          array realData. 2.) An equally sized float array called imagData is created and filled with zeros. 3.)
 *          realData and imagData are enlarged to the powers of two. If finite impulse repsonse filters are constructed
 *          with Hamming windows and no cropping, the new dimension size is equal to the minimum power of two number
 *          that equals or exceeds the original dimension size + kDim - 1, where kDim is the diameter of a circular or
 *          spherical convolution kernel. If finite impulse response filters with Hamming windows and cropping or
 *          infinite impulse response Gaussian or Butterworth filters are used, the new dimension size is equal to the
 *          minimum power of two number that equals or exceeds the original dimension size. The data is padded with
 *          zeros at the end of each dimension. 4.) exec() is invoked to run the fast fourier transform algorithm. 5.)
 *          If Butterworth or Gaussian or Gabor filters are used, the center algorithm is invoked to reorder the data so
 *          that frequencies with the lowest magnitudes are at the center. If finite impulse response filters with
 *          windows are used: 6a.) An ideal filter kernel is constructed in the spatial domain. 7a.) A Hamming window
 *          kernel is constructed in the spatial domain. 8a.) The ideal kernel and the Hamming window kernel are
 *          multiplied together. 9a.) The kernel is zero padded up to the same dimensions that the image data was. 10a.)
 *          exec() is run to obtain the FFT of the kernel. If Gaussian or Butterworth filters are used: For Gaussian and
 *          Butterworth filters the transfer functions affect the real and imaginary parts of the FFT of the image in
 *          exactly the same manner. These filters are zero-phase- shift filters because they do not alter the phase of
 *          the transform. Since these filters are conjugate symmetric, the inverse FFTs of these filters are purely
 *          real. Since this filtering is equivalent to convolving to real 2D data sets, the result must be purely real.
 *          6b.) The real part of the FFT is set equal to the appropriate filter magnitude and the imaginary part is set
 *          equal to zero. This filter has the same dimensions as the padded image data. 11.) The data FFT is set equal
 *          to the product of the data FFT and the filter FFT. 12.) The inverse FFT process is invoked. The complex data
 *          is exported into the 2 float arrays realData and imagData. There should be no need for zero padding at this
 *          point since the dimensions should already be all powers of 2 from before. 13.) If Butterworth or Gaussian
 *          filters are used, the center() routine is invoked to restore the data to its original ordering. 14.) exec()
 *          is invoked to run the inverse fast fourier transform algorithm. 15.) The realData now holds the correct
 *          response if Gaussian or Butterworth filtering was used. If FIR filtering with windows was used then realData
 *          holds a version of the correct response shifted by (kDim - 1)/2 toward the end of each dimension. The
 *          imagData should contain only roundoff error. 16.) For FIR filters shift the data back by (kDim - 1)/2 toward
 *          the start of each dimension. 17.) Stripping is performed to return the image to its original dimensions.
 *          18.) The realData() is imported into the the new spatial image.</p>
 *
 *          <p>Methods included calculate the magnitude and phase as shown below:</p>
 *
 *          <p>magnitude = ( (realData)^2 + (imagData)^2 )^(1/2); phase = arctan(imagData/realData);</p>
 */


public class AlgorithmFrequencyFilterColor extends AlgorithmBase {

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
    public static final int HOMOMORPHIC = 5;

    /** DOCUMENT ME! */
    public static final int LAPLACIAN = 5;

    /** DOCUMENT ME! */
    public static final int WINDOW = 1;

    /** DOCUMENT ME! */
    public static final int GAUSSIAN = 2;

    /** DOCUMENT ME! */
    public static final int BUTTERWORTH = 3;

    /** DOCUMENT ME! */
    public static final int GABOR = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int arrayLength; // size of buffers (realData and imagData)

    /** DOCUMENT ME! */
    private int butterworthOrder; // order of the Butterworth filter

    /** DOCUMENT ME! */
    private int constructionMethod; // WINDOW = 1 for windowed finite impulse response
                                    // GAUSSIAN = 2, BUTTERWORTH = 3

    /** DOCUMENT ME! */
    private boolean createGabor = true;

    /** DOCUMENT ME! */
    private int[] dimLengths; // dimension sizes

    /** DOCUMENT ME! */
    private boolean doCrop; // true if cropping is actually performed

    /** DOCUMENT ME! */
    private int[] end; // if image cropping the ending index in a dimension

    /** DOCUMENT ME! */
    private float f1; // cutoff frequency in LOWPASS and HIGHPASS
                      // lower frequency in BANDPASS and BANDSTOP

    /** DOCUMENT ME! */
    private float f2; // higher frequency in BANDPASS and BANDSTOP
                      // User inputs f1 and f2 from 0.0 to 1.0.  Program multiplies
                      // these numbers by PI for FIR filters.

    /** DOCUMENT ME! */
    private int filterType; // LOWPASS, HIGHPASS, BANDPASS, or BANDSTOP

    /** DOCUMENT ME! */
    private float[] finalRData; // final data
    private float[] finalGData;
    private float[] finalBData;

    /** Variables used in Gabor filter. */
    private float freqU; // Frequency along U axis before rotation by theta range -1 to 1

    /** DOCUMENT ME! */
    private float freqV; // Frequency along V axis before rotation by theta range -1 to 1

    /** DOCUMENT ME! */
    private float highGain; // HOMOMORPHIC variable > 1 for multiplication of high frequencies

    /** DOCUMENT ME! */
    private float highTruncated; // HOMOMORPHIC variable for part of high histogram end truncated

    /** DOCUMENT ME! */
    private float[] iKernel; // ideal kernel data

    /** DOCUMENT ME! */
    private float[] imagiData; // imaginary data
    private float[] imagjData;
    private float[] imagkData;

    /** DOCUMENT ME! */
    private boolean image25D; // if true processes each slice of a 3D image independently

    /** DOCUMENT ME! */
    private boolean imageCrop; // if true crop image if largest image dimension + kDim - 1
                               // exceeds the smallest power of 2 number >= the largest
                               // dimension

    /** DOCUMENT ME! */
    private float[] imagKernelData; // imaginary kernel data

    /** DOCUMENT ME! */
    private int kDim; // kernel diameter

    /** DOCUMENT ME! */
    private float lowGain; // HOMOMORPHIC variable < 1 for multiplication of low frequencies

    /** DOCUMENT ME! */
    private float lowTruncated; // HOMOMORPHIC variable for part of low histogram end trucated

    /** DOCUMENT ME! */
    private float[] magData; // magnitude data

    /** DOCUMENT ME! */
    private float minimum, maximum;

    /** DOCUMENT ME! */
    private int ndim; // number of dimensions

    /** DOCUMENT ME! */
    private int newArrayLength; // size of zero padded buffers

    /** DOCUMENT ME! */
    private int[] newDimLengths; // zero padded dimension sizes

    /** DOCUMENT ME! */
    private int newSliceSize;

    /** DOCUMENT ME! */
    private float[] phaseData; // phase data

    /** DOCUMENT ME! */
    private float[] realData; // real data

    /** DOCUMENT ME! */
    private float[] realKernelData; // real kernel data

    /** DOCUMENT ME! */
    private byte[] sData;

    /** DOCUMENT ME! */
    private float sigmaU; // Standard deviation along prerotation U axis

    /** DOCUMENT ME! */
    private float sigmaV; // Standard deviation along prerotation V axis

    /** DOCUMENT ME! */
    private int[] start; // if image cropping the starting index in a dimension

    /** DOCUMENT ME! */
    private float theta; // Rotation in radians;

    /** DOCUMENT ME! */
    private int transformDir; // transform direction

    /** DOCUMENT ME! */
    private float[] wKernel; // window kernel data

    /** DOCUMENT ME! */
    private boolean zeroPad; // true if zero padding actually performed
    
    private ModelImage gaborImage = null ;
    
    private int originalDataType;
    
    private double sqrt2 = Math.sqrt(2.0);
    private double sqrt3 = Math.sqrt(3.0);
    private double sqrt6 = Math.sqrt(6.0);
 
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used in Laplacian medialness option of live wire cost function.
     *
     * @param  srcImg     source image model
     * @param  image25D   if true processes each slice of a 3D image independently
     * @param  imageCrop  if true crop image if largest image dimension + kDim - 1 exceeds the smallest power of 2
     *                    number >= the largest dimension
     */
    public AlgorithmFrequencyFilterColor(ModelImage srcImg, boolean image25D, boolean imageCrop) {
        super(null, srcImg);

        this.imageCrop = imageCrop;
        this.image25D = image25D;
        this.constructionMethod = WINDOW;
        this.kDim = 65; // Use the value of the biggest kernel in the set
    }

    /**
     * Constructor used for Gabor transform.
     *
     * @param  srcImg       Source image model
     * @param  freqU        Frequency along U axis before rotation by theta range -1 to 1
     * @param  freqV        Frequency along V axis before rotation by theta range -1 to 1
     * @param  sigmaU       Standard deviation along prerotated U axis
     * @param  sigmaV       Standard deviation along prerotated V axis
     * @param  theta        Rotation in radians
     * @param  createGabor  If true, produce an image of the Gabor filter
     */
    public AlgorithmFrequencyFilterColor(ModelImage srcImg, float freqU, float freqV, float sigmaU, float sigmaV,
                                    float theta, boolean createGabor) {
        super(null, srcImg);

        this.freqU = freqU;
        this.freqV = freqV;
        this.sigmaU = sigmaU;
        this.sigmaV = sigmaV;
        this.theta = theta;
        this.createGabor = createGabor;
        this.constructionMethod = GABOR;
        this.imageCrop = false;

        if (srcImg.getNDims() > 2) {
            this.image25D = true;
        } else {
            this.image25D = false;
        }
    }

    /**
     * This constructor is only used for HOMOMORPHIC filters.
     *
     * @param  srcImg            source image model
     * @param  image25D          If true, processes each slice of a 3D image independently
     * @param  freq1             cutoff frequency, transition frequency from low to high gain
     * @param  butterworthOrder  order of the Butterworth filter
     * @param  lowGain           < 1, gain at low frequencies
     * @param  highGain          > 1, gain at high frequencies
     * @param  lowTruncated      part of low histogram end truncated
     * @param  highTruncated     part of high histogram end truncated
     */
    public AlgorithmFrequencyFilterColor(ModelImage srcImg, boolean image25D, float freq1, int butterworthOrder,
                                    float lowGain, float highGain, float lowTruncated, float highTruncated) {
        super(null, srcImg);

        this.imageCrop = false;
        this.image25D = image25D;
        this.filterType = HOMOMORPHIC;
        f1 = freq1;
        this.constructionMethod = BUTTERWORTH;
        this.butterworthOrder = butterworthOrder;
        this.lowGain = lowGain;
        this.highGain = highGain;
        this.lowTruncated = lowTruncated;
        this.highTruncated = highTruncated;
    }

    /**
     * Constructor used for Gabor transform.
     *
     * @param  destImg      Destination image model
     * @param  srcImg       Source image model
     * @param  freqU        Frequency along U axis before rotation by theta range -1 to 1
     * @param  freqV        Frequency along V axis before rotation by theta range -1 to 1
     * @param  sigmaU       Standard deviation along prerotated U axis
     * @param  sigmaV       Standard deviation along prerotated V axis
     * @param  theta        Rotation in radians
     * @param  createGabor  If true, produce an image of the Gabor filter
     */
    public AlgorithmFrequencyFilterColor(ModelImage destImg, ModelImage srcImg, float freqU, float freqV, float sigmaU,
                                    float sigmaV, float theta, boolean createGabor) {
        super(destImg, srcImg);

        this.freqU = freqU;
        this.freqV = freqV;
        this.sigmaU = sigmaU;
        this.sigmaV = sigmaV;
        this.theta = theta;
        this.createGabor = createGabor;
        this.constructionMethod = GABOR;
        this.imageCrop = false;

        if (srcImg.getNDims() > 2) {
            this.image25D = true;
        } else {
            this.image25D = false;
        }
    }

    /**
     * This constructor is only used for HOMOMORPHIC filters.
     *
     * @param  destImg           image model where result image is to be stored
     * @param  srcImg            source image model
     * @param  image25D          If true, processes each slice of a 3D image independently
     * @param  freq1             cutoff frequency, transition frequency from low to high gain
     * @param  butterworthOrder  order of the Butterworth filter
     * @param  lowGain           < 1, gain at low frequencies
     * @param  highGain          > 1, gain at high frequencies
     * @param  lowTruncated      part of low histogram end truncated
     * @param  highTruncated     part of high histogram end truncated
     */
    public AlgorithmFrequencyFilterColor(ModelImage destImg, ModelImage srcImg, boolean image25D, float freq1,
                                    int butterworthOrder, float lowGain, float highGain, float lowTruncated,
                                    float highTruncated) {
        super(destImg, srcImg);

        this.imageCrop = false;
        this.image25D = image25D;
        this.filterType = HOMOMORPHIC;
        f1 = freq1;
        this.constructionMethod = BUTTERWORTH;
        this.butterworthOrder = butterworthOrder;
        this.lowGain = lowGain;
        this.highGain = highGain;
        this.lowTruncated = lowTruncated;
        this.highTruncated = highTruncated;
    }

    /**
     * Creates a new AlgorithmFrequencyFilter object.
     *
     * @param  srcImg              source image model
     * @param  image25D            if true processes each slice of a 3D image independently
     * @param  imageCrop           if true crop image if largest image dimension + kDim - 1 exceeds the smallest power
     *                             of 2 number >= the largest dimension
     * @param  kernelDiameter      convolution kernel diameter - must be an odd integer >= 3
     * @param  filterType          LOWPASS, HIGHPASS, BANDPASS, or BANDSTOP
     * @param  freq1               cutoff frequency in LOWPASS and HIGHPASS lower frequency in BANDPASS and BANDSTOP
     * @param  freq2               higher frequency in BANDPASS and BANDSTOP User inputs f1 and f2 going from 0.0 to
     *                             1.0. Program multiplies these numbers by PI for FIR filters.
     * @param  constructionMethod  WINDOW for window finite impulse response, GAUSSIAN, or BUTTERWORTH
     * @param  butterworthOrder    order of a Butterworth filter
     */
    public AlgorithmFrequencyFilterColor(ModelImage srcImg, boolean image25D, boolean imageCrop, int kernelDiameter,
                                    int filterType, float freq1, float freq2, int constructionMethod,
                                    int butterworthOrder) {
        super(null, srcImg);

        this.image25D = image25D;
        this.imageCrop = imageCrop;
        kDim = kernelDiameter;
        this.filterType = filterType;
        f1 = freq1;
        f2 = freq2;
        this.constructionMethod = constructionMethod;
        this.butterworthOrder = butterworthOrder;
    }

    /**
     * Creates a new AlgorithmFrequencyFilter object.
     *
     * @param  destImg             image model where result image is to be stored
     * @param  srcImg              source image model
     * @param  image25D            if true processes each slice of a 3D image independently
     * @param  imageCrop           if true crop image if largest image dimension + kDim - 1 exceeds the smallest power
     *                             of 2 number >= the largest dimension
     * @param  kernelDiameter      convolution kernel diameter - must be an odd integer >= 3
     * @param  filterType          LOWPASS, HIGHPASS, BANDPASS, or BANDSTOP
     * @param  freq1               cutoff frequency in LOWPASS and HIGHPASS lower frequency in BANDPASS and BANDSTOP
     * @param  freq2               higher frequency in BANDPASS and BANDSTOP User inputs f1 and f2 going from 0.0 to
     *                             1.0. Program multiplies these numbers by PI for FIR filters.
     * @param  constructionMethod  WINDOW for window finite impulse response, GAUSSIAN, or BUTTERWORTH
     * @param  butterworthOrder    order of the Butterworth filter
     */
    public AlgorithmFrequencyFilterColor(ModelImage destImg, ModelImage srcImg, boolean image25D, boolean imageCrop,
                                    int kernelDiameter, int filterType, float freq1, float freq2,
                                    int constructionMethod, int butterworthOrder) {
        super(destImg, srcImg);

        this.imageCrop = imageCrop;
        this.image25D = image25D;
        kDim = kernelDiameter;
        this.filterType = filterType;
        f1 = freq1;
        f2 = freq2;
        this.constructionMethod = constructionMethod;
        this.butterworthOrder = butterworthOrder;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        realData = null; // real data
        imagiData = null; // imaginary data
        imagjData = null;
        imagkData = null;
        realKernelData = null; // real kernel data
        imagKernelData = null; // imaginary kernel data
        magData = null; // magnitude data
        phaseData = null; // phase data
        iKernel = null; // ideal kernel data
        wKernel = null; // window kernel data
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Returns reference to imaginaryi data array.
     *
     * @return  the reference the the imaginaryi data array
     */
    public float[] getImaginaryiData() {
        return imagiData;
    }
    
    /**
     * Returns reference to imaginaryj data array.
     *
     * @return  the reference the the imaginaryj data array
     */
    public float[] getImaginaryjData() {
        return imagjData;
    }
    
    /**
     * Returns reference to imaginaryk data array.
     *
     * @return  the reference the the imaginaryk data array
     */
    public float[] getImaginarykData() {
        return imagkData;
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

    /*
     * We tried this used in the Laplacian medialness option of live wire cost function @return resultData
     */
    /*private float[] calc2DMedialness() {
     *  int s; int i; int x,y,pos; int kernDim; int kExtents[] = new int[2]; int derivOrder[] = new int[2]; float
     * sigmas[] = new float[2]; float GxxData[]; float GyyData[]; float realProduct[]; float imagProduct[]; int
     * sliceSize; float resultData[];
     *
     * fireProgressStateChanged(srcImage.getImageName(), "Importing source image...");
     *
     * int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width; int yScreen =
     * Toolkit.getDefaultToolkit().getScreenSize().height;
     *
     *
     * makeComplexData(); sliceSize = dimLengths[0]*dimLengths[1]; // Perform forward FFT on data transformDir = FORWARD;
     * exec(realData,imagData,0); try {     realProduct     = new float[newSliceSize];     imagProduct     = new
     * float[newSliceSize];     realKernelData  = new float[newSliceSize];     imagKernelData  = new
     * float[newSliceSize];     resultData      = new float[sliceSize];     sData           = new byte[sliceSize]; }
     * catch (OutOfMemoryError e) {     realProduct = null;     System.gc();     displayError("AlgorithmFrequencyFilter:
     * Out of memory creating realProduct");          setCompleted(false);     return null; }
     *
     * for (s = 1; s <= 8 && !threadStopped; s++) {     // make Laplacian kernel     sigmas[0] = (float)s;     sigmas[1] =
     * (float)s;     derivOrder[0] = 2;     derivOrder[1] = 0;     kernDim = kExtents[0] = kExtents[1] = 8*s + 1;
     * GxxData = new float[kernDim*kernDim];     GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas,
     * derivOrder);     Gxx.calc(false);     derivOrder[0] = 0;     derivOrder[1] = 2;     GyyData = new
     * float[kernDim*kernDim];     GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);
     * Gyy.calc(false);     for (i = 0; i < GyyData.length; i++) {         GxxData[i] = -(GxxData[i] + GyyData[i]); }
     *
     * for (i = 0; i < newSliceSize; i++)       realKernelData[i] = 0.0f;
     *
     * for (y = 0; y <= kernDim - 1; y++) {       for (x = 0; x <= kernDim - 1; x++) {         pos = y*kernDim + x;
     * realKernelData[x + newDimLengths[0]*y] = GxxData[pos];         }     }
     *
     * // The filter FFT is created     transformDir = FILTER;     exec(realKernelData,imagKernelData,0);     if
     * (threadStopped) {         finalize();         return null;     }
     *
     * for (i = 0; i < newSliceSize; i++) {       realProduct[i] = realData[i]*realKernelData[i] -
     * imagData[i]*imagKernelData[i];       imagProduct[i] = imagData[i]*realKernelData[i] +
     * realData[i]*imagKernelData[i];     }
     *
     * // Perform inverse FFT     transformDir = INVERSE;     exec(realProduct,imagProduct,0);
     *
     * for (i = 0; i < sliceSize; i++) {         if (finalData[i] > resultData[i]) {             resultData[i] =
     * finalData[i];             sData[i] = (byte)s;         }     }
     *
     * } // for (s = 1; s <= 8 && !threadStopped; s++)  setCompleted(true); return resultData;}*/

    /**
     * DOCUMENT ME!
     *
     * @return  sData
     */
    public byte[] getSData() {
        return sData;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @return  sData
     */
    public ModelImage getGabor() {
        return gaborImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

         // log beginning information

        if (destImage != null) {
            calcStoreInDest();
        } else {
            calcInPlace();
        }
    }

    

    /**
     * This function replaces the original image with a new image that is either the FFT, the filtered FFT, or the
     * inverse FFT of the original or filtered image.
     */
    private void calcInPlace() {

        int i;
        float tempR;
        float[] realSubsetData;
        float[] imagiSubsetData;
        float[] imagjSubsetData;
        float[] imagkSubsetData;
        int z;
        FileInfoBase[] fileInfo;
        float lum;
        float chr1;
        float chr2;

        fireProgressStateChanged(0, null, "Running frequency filter ...");
        
        originalDataType = srcImage.getType();

        makeHyperComplexData();

        if (constructionMethod == WINDOW) {

            // The filter kernel is constructed.
            makeKernelData();

            // The filter FFT is created
            transformDir = FILTER;
            exec(realKernelData, imagKernelData, 0, 1);

            if (threadStopped) {
                finalize();

                return;
            }
        } // end of if (constructionMethod == WINDOW)

        // Perform forward FFT on data
        transformDir = FORWARD;

        if (image25D) {
            realSubsetData = new float[newSliceSize];
            imagiSubsetData = new float[newSliceSize];
            imagjSubsetData = new float[newSliceSize];
            imagkSubsetData = new float[newSliceSize];

            for (z = 0; z < newDimLengths[2]; z++) {

                for (i = 0; i < newSliceSize; i++) {
                    realSubsetData[i] = realData[(z * newSliceSize) + i];
                    imagiSubsetData[i] = imagiData[(z * newSliceSize) + i];
                    imagjSubsetData[i] = imagjData[(z * newSliceSize) + i];
                    imagkSubsetData[i] = imagkData[(z * newSliceSize) + i];
                }

                exec(realSubsetData, imagiSubsetData, z, 1);
                exec(imagjSubsetData, imagkSubsetData, z, 2);

                fireProgressStateChanged((Math.round(10 + ((float) (z + 1) / newDimLengths[2] * 40))), null,
                                         "Running forward FFTs ...");
                // fireProgressStateChanged(Math.round(10 + ((float) (z + 1) / newDimLengths[2] * 40)));

                for (i = 0; i < newSliceSize; i++) {
                    realData[(z * newSliceSize) + i] = realSubsetData[i];
                    imagiData[(z * newSliceSize) + i] = imagiSubsetData[i];
                    imagjData[(z * newSliceSize) + i] = imagjSubsetData[i];
                    imagkData[(z * newSliceSize) + i] = imagkSubsetData[i];
                }
            } // for (z = 0; z < newDimLengths[2]; z++)
        } // if (image25D)
        else { // not image25D
            exec(realData, imagiData, 0, 1);
            exec(imagjData, imagkData, 0, 2);
        } // else not image25D

        if (threadStopped) {
            finalize();

            return;
        }


        if (constructionMethod == GAUSSIAN) {
            makeGaussianFilter(f1);
        } // end of if (constructionMethod == GAUSSIAN)

        if (constructionMethod == GABOR) {
            makeGaborFilter(freqU, freqV, sigmaU, sigmaV, theta, createGabor);
        }

        if ((filterType != HOMOMORPHIC) && (constructionMethod == BUTTERWORTH)) {
            makeButterworthFilter(f1, f2);
        } // end of if ((filterType != HOMOMORPHIC) && (constructionMethod == BUTTERWORTH))

        if (filterType == HOMOMORPHIC) {
            makeHomomorphicFilter(f1);
        } // if (filterType == HOMOMORPHIC)


        if (constructionMethod == WINDOW) {

            // The image FFT is multiplied by the filter FFT.
            if (image25D) {

                for (z = 0; z < newDimLengths[2]; z++) {

                    for (i = 0; i < newSliceSize; i++) {
                        tempR = (realData[(z * newSliceSize) + i] * realKernelData[i]) -
                                (imagiData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagiData[(z * newSliceSize) + i] = (imagiData[(z * newSliceSize) + i] * realKernelData[i]) +
                                                           (realData[(z * newSliceSize) + i] * imagKernelData[i]);
                        realData[(z * newSliceSize) + i] = tempR;
                        tempR = (imagjData[(z * newSliceSize) + i] * realKernelData[i]) -
                        (imagkData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagkData[(z * newSliceSize) + i] = (imagkData[(z * newSliceSize) + i] * realKernelData[i]) +
                                                   (imagjData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagjData[(z * newSliceSize) + i] = tempR;
                    }
                }
            } // if (image25D)
            else { // not image25D)

                for (i = 0; i < newArrayLength; i++) {
                    tempR = (realData[i] * realKernelData[i]) - (imagiData[i] * imagKernelData[i]);
                    imagiData[i] = (imagiData[i] * realKernelData[i]) + (realData[i] * imagKernelData[i]);
                    realData[i] = tempR;
                    tempR = (imagjData[i] * realKernelData[i]) - (imagkData[i] * imagKernelData[i]);
                    imagkData[i] = (imagkData[i] * realKernelData[i]) + (imagjData[i] * imagKernelData[i]);
                    imagjData[i] = tempR;
                }
            } // else not image25D
        } // end of if (constructionMethod == WINDOW)

        // Perform inverse FFT
        transformDir = INVERSE;

        if (image25D) {
            realSubsetData = new float[newSliceSize];
            imagiSubsetData = new float[newSliceSize];
            imagjSubsetData = new float[newSliceSize];
            imagkSubsetData = new float[newSliceSize];
            // fireProgressStateChanged("Running inverse FFTs");

            for (z = 0; z < newDimLengths[2]; z++) {

                for (i = 0; i < newSliceSize; i++) {
                    realSubsetData[i] = realData[(z * newSliceSize) + i];
                    imagiSubsetData[i] = imagiData[(z * newSliceSize) + i];
                    imagjSubsetData[i] = imagjData[(z * newSliceSize) + i];
                    imagkSubsetData[i] = imagkData[(z * newSliceSize) + i];
                }

                exec(realSubsetData, imagiSubsetData, z, 1);
                exec(imagjSubsetData, imagkSubsetData, z, 2);

                fireProgressStateChanged((Math.round(50 + ((float) (z + 1) / newDimLengths[2] * 40))), null,
                                         "Running inverse FFTs ...");

                // fireProgressStateChanged(Math.round(50 + ((float) (z + 1) / newDimLengths[2] * 40)));
            } // for (z = 0; z < newDimLengths[2]; z++)

            realSubsetData = null;
            imagiSubsetData = null;
            imagjSubsetData = null;
            imagkSubsetData = null;
        } // if (image25D)
        else { // not image25D
            exec(realData, imagiData, 0, 1);
            exec(imagjData, imagkData, 0, 2);
        } // else not image25D

        if (filterType == HOMOMORPHIC) {
            restoreFinalData(finalRData);
            restoreFinalData(finalGData);
            restoreFinalData(finalBData);
        } // if (filterType == HOMOMORPHIC)

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged(-1, null, "Storing inverse FFT in source image ...");

        //        fireProgressStateChanged("Storing inverse FFT in source image...");
        // back in the spatial domain so only realData is now present
        try {
            srcImage.reallocate(originalDataType, dimLengths);
        } catch (IOException error) {
            displayError("AlgorithmFrequencyFilterColor: IOException on srcImage.reallocate");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory on srcImage.reallocate");

            setCompleted(false);

            return;
        }
        
        for (i = 0; i < finalRData.length; i++) {
            lum = finalRData[i];
            chr1 = finalGData[i];
            chr2 = finalBData[i];
            finalRData[i] = (float)(lum/sqrt3 - 2.0*chr2/sqrt6);
            finalGData[i] = (float)(lum/sqrt3 + chr1/sqrt2 + chr2/sqrt6);
            finalBData[i] = (float)(lum/sqrt3 - chr1/sqrt2 + chr2/sqrt6);
        }
        
        if (filterType != HOMOMORPHIC) {

            
        	for (i = 0; i < arrayLength; i++) {

                if (finalRData[i] > maximum) {
                    finalRData[i] = maximum;
                }

                if (finalRData[i] < minimum) {
                    finalRData[i] = minimum;
                }
           
        		
                if (finalGData[i] > maximum) {
                    finalGData[i] = maximum;
                }

                if (finalGData[i] < minimum) {
                    finalGData[i] = minimum;
                }
                
                if (finalBData[i] > maximum) {
                    finalBData[i] = maximum;
                }

                if (finalBData[i] < minimum) {
                    finalBData[i] = minimum;
                }	
           }
        } // if (filterType != HOMOMORPHIC)

        try {
            srcImage.importRGBData(1, 0, finalRData, true);
            srcImage.importRGBData(2, 0, finalGData, true);
            srcImage.importRGBData(3, 0, finalBData, true);
        } catch (IOException error) {
            displayError("AlgorithmFrequencyFilterColor: IOException on source image import data");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory on source image import data");

            setCompleted(false);

            return;
        }
        
        fileInfo = srcImage.getFileInfo();
        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setDataType(ModelStorageBase.FLOAT);
        }

        fireProgressStateChanged(100, null, null);

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
        float[] imagiSubsetData;
        float[] imagjSubsetData;
        float[] imagkSubsetData;
        int z;
        float lum;
        float chr1;
        float chr2;

        fireProgressStateChanged(0, null, "Running frequency filter ...");
        
        originalDataType = srcImage.getType();

        makeHyperComplexData();

        if (constructionMethod == WINDOW) {

            // The filter kernel is constructed.
            makeKernelData();

            // The filter FFT is created
            transformDir = FILTER;
            exec(realKernelData, imagKernelData, 0, 1);

            if (threadStopped) {
                finalize();

                return;
            }
        } // end of if (constructionMethod == WINDOW)

        // Perform forward FFT on image
        transformDir = FORWARD;

        if (image25D) {
            realSubsetData = new float[newSliceSize];
            imagiSubsetData = new float[newSliceSize];
            imagjSubsetData = new float[newSliceSize];
            imagkSubsetData = new float[newSliceSize];

            for (z = 0; z < newDimLengths[2]; z++) {

                for (i = 0; i < newSliceSize; i++) {
                    realSubsetData[i] = realData[(z * newSliceSize) + i];
                    imagiSubsetData[i] = imagiData[(z * newSliceSize) + i];
                    imagjSubsetData[i] = imagjData[(z * newSliceSize) + i];
                    imagkSubsetData[i] = imagkData[(z * newSliceSize) + i];
                }

                exec(realSubsetData, imagiSubsetData, z, 1);
                exec(imagjSubsetData, imagkSubsetData, z, 2);
                fireProgressStateChanged((Math.round(10 + ((float) (z + 1) / newDimLengths[2] * 40))), null,
                                         "Running forward FFTs ...");
                // fireProgressStateChanged(Math.round(10 + ((float) (z + 1) / newDimLengths[2] * 40)));

                for (i = 0; i < newSliceSize; i++) {
                    realData[(z * newSliceSize) + i] = realSubsetData[i];
                    imagiData[(z * newSliceSize) + i] = imagiSubsetData[i];
                    imagjData[(z * newSliceSize) + i] = imagjSubsetData[i];
                    imagkData[(z * newSliceSize) + i] = imagkSubsetData[i];
                }
            } // for (z = 0; z < newDimLengths[2]; z++)
        } // if (image25D)
        else { // not image25D
            exec(realData, imagiData, 0, 1);
            exec(imagjData, imagkData, 0, 2);
        } // else not image25D

        if (threadStopped) {
            finalize();

            return;
        }


        if (constructionMethod == GAUSSIAN) {
            makeGaussianFilter(f1);
        } // end of if (constructionMethod == GAUSSIAN)

        if (constructionMethod == GABOR) {
            makeGaborFilter(freqU, freqV, sigmaU, sigmaV, theta, createGabor);
        }

        if ((filterType != HOMOMORPHIC) && (constructionMethod == BUTTERWORTH)) {
            makeButterworthFilter(f1, f2);
        } // end of if ((filterType != HOMOMORPHIC) && (constructionMethod == BUTTERWORTH))

        if (filterType == HOMOMORPHIC) {
            makeHomomorphicFilter(f1);
        } // if (filterType == HOMOMORPHIC)

        if (constructionMethod == WINDOW) {
            // The image FFT is multiplied by the filter FFT.

            if (image25D) {

                for (z = 0; z < newDimLengths[2]; z++) {

                    for (i = 0; i < newSliceSize; i++) {
                        tempR = (realData[(z * newSliceSize) + i] * realKernelData[i]) -
                                (imagiData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagiData[(z * newSliceSize) + i] = (imagiData[(z * newSliceSize) + i] * realKernelData[i]) +
                                                           (realData[(z * newSliceSize) + i] * imagKernelData[i]);
                        realData[(z * newSliceSize) + i] = tempR;
                        tempR = (imagjData[(z * newSliceSize) + i] * realKernelData[i]) -
                        (imagkData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagkData[(z * newSliceSize) + i] = (imagkData[(z * newSliceSize) + i] * realKernelData[i]) +
                                                   (imagjData[(z * newSliceSize) + i] * imagKernelData[i]);
                        imagjData[(z * newSliceSize) + i] = tempR;

                    }
                }
            } // if (image25D)
            else { // not image25D)

                for (i = 0; i < newArrayLength; i++) {
                    tempR = (realData[i] * realKernelData[i]) - (imagiData[i] * imagKernelData[i]);
                    imagiData[i] = (imagiData[i] * realKernelData[i]) + (realData[i] * imagKernelData[i]);
                    realData[i] = tempR;
                    tempR = (imagjData[i] * realKernelData[i]) - (imagkData[i] * imagKernelData[i]);
                    imagkData[i] = (imagkData[i] * realKernelData[i]) + (imagjData[i] * imagKernelData[i]);
                    imagjData[i] = tempR;
                }
            } // else not image25D
        } // if (constructionMethod == WINDOW)

        // Perform inverse FFT
        transformDir = INVERSE;

        if (image25D) {
            realSubsetData = new float[newSliceSize];
            imagiSubsetData = new float[newSliceSize];
            imagjSubsetData = new float[newSliceSize];
            imagkSubsetData = new float[newSliceSize];
            // fireProgressStateChanged("Running inverse FFTs");

            for (z = 0; z < newDimLengths[2]; z++) {

                for (i = 0; i < newSliceSize; i++) {
                    realSubsetData[i] = realData[(z * newSliceSize) + i];
                    imagiSubsetData[i] = imagiData[(z * newSliceSize) + i];
                    imagjSubsetData[i] = imagjData[(z * newSliceSize) + i];
                    imagkSubsetData[i] = imagkData[(z * newSliceSize) + i];
                }
                exec(realSubsetData, imagiSubsetData, z, 1);
                exec(imagjSubsetData, imagkSubsetData, z, 2);
                fireProgressStateChanged((Math.round(50 + ((float) (z + 1) / newDimLengths[2] * 40))), null,
                                         "Running inverse FFTs ...");
                // fireProgressStateChanged(Math.round(50 + ((float) (z + 1) / newDimLengths[2] * 40)));
            } // for (z = 0; z < newDimLengths[2]; z++)

            realSubsetData = null;
            imagiSubsetData = null;
            imagjSubsetData = null;
            imagkSubsetData = null;
        } // if (image25D)
        else { // not image25D
            exec(realData, imagiData, 0, 1);
            exec(imagjData, imagkData, 0, 2);
        } // else not image25D

        if (filterType == HOMOMORPHIC) {
            restoreFinalData(finalRData);
            restoreFinalData(finalGData);
            restoreFinalData(finalBData);
        } // if (filterType == HOMOMORPHIC)

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged(-1, null, "Storing inverse FFT in destination image ...");
        // fireProgressStateChanged("Storing inverse FFT in destination image...");

        // back in the spatial domain so only realData is now present
        try {
            destImage.reallocate(originalDataType, dimLengths);
        } catch (IOException error) {
            displayError("AlgorithmFrequencyFilterColor: IOException on destImage.reallocate");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory on destImage.reallocate");

            setCompleted(false);

            return;
        }
        
        for (i = 0; i < finalRData.length; i++) {
            lum = finalRData[i];
            chr1 = finalGData[i];
            chr2 = finalBData[i];
            finalRData[i] = (float)(lum/sqrt3 - 2.0*chr2/sqrt6);
            finalGData[i] = (float)(lum/sqrt3 + chr1/sqrt2 + chr2/sqrt6);
            finalBData[i] = (float)(lum/sqrt3 - chr1/sqrt2 + chr2/sqrt6);
        }
        
        if (filterType != HOMOMORPHIC) {

            
        	for (i = 0; i < arrayLength; i++) {

                if (finalRData[i] > maximum) {
                    finalRData[i] = maximum;
                }

                if (finalRData[i] < minimum) {
                    finalRData[i] = minimum;
                }
           
        		
                if (finalGData[i] > maximum) {
                    finalGData[i] = maximum;
                }

                if (finalGData[i] < minimum) {
                    finalGData[i] = minimum;
                }
                
                if (finalBData[i] > maximum) {
                    finalBData[i] = maximum;
                }

                if (finalBData[i] < minimum) {
                    finalBData[i] = minimum;
                }	
           }
        } // if (filterType != HOMOMORPHIC)

        try {
            destImage.importRGBData(1, 0, finalRData, true);
            destImage.importRGBData(2, 0, finalGData, true);
            destImage.importRGBData(3, 0, finalBData, true);
        } catch (IOException error) {
            displayError("AlgorithmFrequencyFilterColor: IOException on destination image import data");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory on destination image import data");

            setCompleted(false);

            return;
        }

        // destImage.calcMinMax();

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
                displayError("AlgorithmFrequencyFilterColor: Out of memory creating centerData");

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
                displayError("AlgorithmFrequencyFilterColor: Out of memory creating centerData");

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

        } // else if ((ndim == 2) || (image25D))

        // 3D center
        // center data is buffer to hold 1/8 of data as we center the FFT
        else if (ndim == 3) {

            try {
                centerData = new float[newDimLengths[0] / 2 * newDimLengths[1] / 2 * newDimLengths[2] / 2];
            } catch (OutOfMemoryError e) {
                centerData = null;
                System.gc();
                displayError("AlgorithmFrequencyFilterColor: Out of memory creating centerData");

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
    private void edgeStrip(float[] rData, int z, int colorIndex) {
        int i, j, k, m, n;
        float[] tempData;
        int sliceSize = dimLengths[0] * dimLengths[1];
        int aLength;
        // This function edge strips the realData after the inverse FFT to
        // return to the original dimensions that were present before the
        // forward FFT was performed.

        if (image25D) {
            aLength = sliceSize;
        } else {
            aLength = arrayLength;
        }

        try {
            tempData = new float[aLength];
        } catch (OutOfMemoryError e) {
            tempData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory creating tempData in edgeStrip routine");

            setCompleted(false);

            return;
        }

        if (ndim == 1) {

            for (i = 0; i < dimLengths[0]; i++) {
                tempData[i] = rData[i];
            }
        } else if ((ndim == 2) || (image25D)) {

            for (j = 0; j < dimLengths[1]; j++) {

                for (i = 0; i < dimLengths[0]; i++) {
                    tempData[i + (dimLengths[0] * j)] = rData[i + (newDimLengths[0] * j)];
                }
            }
        } else if (ndim == 3) {

            for (k = 0; k < dimLengths[2]; k++) {

                for (j = 0; j < dimLengths[1]; j++) {

                    for (i = 0; i < dimLengths[0]; i++) {
                        tempData[i + (dimLengths[0] * j) + (dimLengths[0] * dimLengths[1] * k)] = rData[i +
                                                                                                        (newDimLengths[0] *
                                                                                                             j) +
                                                                                                        (newDimLengths[0] *
                                                                                                             newDimLengths[1] *
                                                                                                             k)];
                    }
                }
            }
        } else if (ndim == 4) {

            for (m = 0; m < dimLengths[3]; m++) {

                for (k = 0; k < dimLengths[2]; k++) {

                    for (j = 0; j < dimLengths[1]; j++) {

                        for (i = 0; i < dimLengths[0]; i++) {
                            tempData[i + (dimLengths[0] * j) + (dimLengths[0] * dimLengths[1] * k) +
                                     (dimLengths[0] * dimLengths[1] * dimLengths[2] * m)] = rData[i +
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

            for (n = 0; n < dimLengths[4]; n++) {

                for (m = 0; m < dimLengths[3]; m++) {

                    for (k = 0; k < dimLengths[2]; k++) {

                        for (j = 0; j < dimLengths[1]; j++) {

                            for (i = 0; i < dimLengths[0]; i++) {
                                tempData[i + (dimLengths[0] * j) + (dimLengths[0] * dimLengths[1] * k) +
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * m) +
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * dimLengths[3] * n)] = rData[i +
                                                                                                                      (newDimLengths[0] *
                                                                                                                           j) +
                                                                                                                      (newDimLengths[0] *
                                                                                                                           newDimLengths[1] *
                                                                                                                           k) +
                                                                                                                      (newDimLengths[0] *
                                                                                                                           newDimLengths[1] *
                                                                                                                           newDimLengths[2] *
                                                                                                                           m) +
                                                                                                                      (newDimLengths[0] *
                                                                                                                           newDimLengths[1] *
                                                                                                                           newDimLengths[2] *
                                                                                                                           newDimLengths[3] *
                                                                                                                           n)];
                            }
                        }
                    }
                }
            }
        }
        
        if (colorIndex == 1) {
	        try {
	
	            if ((!image25D) || (z == 0)) {
	                finalRData = new float[arrayLength];
	            }
	        } catch (OutOfMemoryError e) {
	            finalRData = null;
	            System.gc();
	            displayError("AlgorithmFrequencyFilter: Out of memory creating finalData in edgeStrip routine");
	
	            setCompleted(false);
	
	            return;
	        }
	
	        for (i = 0; i < aLength; i++) {
	            finalRData[(z * sliceSize) + i] = tempData[i];
	        }
        }
        else if (colorIndex == 2) {
        	try {
        		
	            if ((!image25D) || (z == 0)) {
	                finalGData = new float[arrayLength];
	            }
	        } catch (OutOfMemoryError e) {
	            finalGData = null;
	            System.gc();
	            displayError("AlgorithmFrequencyFilterColor: Out of memory creating finalData in edgeStrip routine");
	
	            setCompleted(false);
	
	            return;
	        }
	
	        for (i = 0; i < aLength; i++) {
	            finalGData[(z * sliceSize) + i] = tempData[i];
	        }	
        }
        else {
        	try {
        		
	            if ((!image25D) || (z == 0)) {
	                finalBData = new float[arrayLength];
	            }
	        } catch (OutOfMemoryError e) {
	            finalBData = null;
	            System.gc();
	            displayError("AlgorithmFrequencyFilterColor: Out of memory creating finalData in edgeStrip routine");
	
	            setCompleted(false);
	
	            return;
	        }
	
	        for (i = 0; i < aLength; i++) {
	            finalBData[(z * sliceSize) + i] = tempData[i];
	        }
        }
    }


    /**
     * This is the method that calculates the FFT Perform a data centering operation after the forward FFT Perform a
     * data centering operation before the inverse FFT Note that a frequency filter operation performs a forward FFT.
     *
     * @param  rData  real data buffer
     * @param  iData  imaginary data buffer
     * @param  z      slice number in image25D processing, 0 otherwise
     * @param complexHalf = 1 for first half, 2 for second half
     */
    private void exec(float[] rData, float[] iData, int z, int complexHalf) {

        double TWO_PI = 2 * java.lang.Math.PI;
        double wt1Imag, wt1Real;
        double angle, delta;
        float imag, real, fTemp, fReal, fImag;
        int i, index1, index2, index3;
        int j1, j2, j3;
        int k1, k1Double;
        int iSwap, i1Swap, i2Swap, index, dim;
        int direction;
        int dimNumber;
        int newLength;
        // boolean haveWindowed;

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

        if ((transformDir == INVERSE) &&
                ((constructionMethod == GAUSSIAN) || (constructionMethod == BUTTERWORTH) ||
                     (constructionMethod == GABOR))) {

            if (!image25D) {

                // fireProgressStateChanged("Centering data before inverse FFT...");
                fireProgressStateChanged(-1, null, "Centering data before inverse FFT ...");
            }

            center(rData, iData);
        }

        if (!image25D) {

            // fireProgressStateChanged("Running FFT algorithm...");
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

            if (threadStopped) {
                finalize();

                return;
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

            if (!image25D) {

                if (transformDir == FORWARD) {
                    fireProgressStateChanged((Math.round(10 + ((float) (i + 1) / ndim * 40))), null, null);
                    // fireProgressStateChanged(Math.round(10 + ((float) (i + 1) / ndim * 40)));
                }

                if (transformDir == INVERSE) {
                    fireProgressStateChanged((Math.round(50 + ((float) (i + 1) / ndim * 40))), null, null);
                    // fireProgressStateChanged(Math.round(50 + ((float) (i + 1) / ndim * 40)));
                }
            } // if (!image25D)
        }

        if (threadStopped) {
            return;
        }

        if ((transformDir == FORWARD) &&
                ((constructionMethod == GAUSSIAN) || (constructionMethod == BUTTERWORTH) ||
                     (constructionMethod == GABOR))) {

            if (!image25D) {
                fireProgressStateChanged(-1, null, "Centering data after FFT algorithm ...");
                //                fireProgressStateChanged("Centering data after FFT algorithm...");
            }

            center(rData, iData);
        }

        if (transformDir == INVERSE) {
        
            for (i = 0; i < newLength; i++) {
                rData[i] = rData[i] / newLength;
                iData[i] = iData[i] / newLength;
            }
            

            if (constructionMethod == WINDOW) {

                // shift each dimension back to the start by (kDim - 1)/2
                shiftBack(rData);
                shiftBack(iData);
            }

            // Do edge stripping to restore the original dimensions the source image had
            // before the forward FFT
            if (!image25D) {

                // fireProgressStateChanged("Zero stripping data after inverse FFT...");
                fireProgressStateChanged(-1, null, "Zero stripping data after inverse FFT ...");
            }

            if (complexHalf == 1) {
               edgeStrip(iData, z, 1);
            }
            else {
            	edgeStrip(rData, z, 2);
            	edgeStrip(iData, z, 3);
            }

            if ((!image25D) || (z == (dimLengths[2] - 1))) {

                if (doCrop) {
                	if (complexHalf == 1) {
                        zeroAround(finalRData);
                	}
                	else {
                		zeroAround(finalGData);
                		zeroAround(finalBData);
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
                    distance = (float)
                                   Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
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
                	double realArg = FHigh * distance;
                	double imaginaryArg = 0.0;
                	double initialOrder = 1.0;
                	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                	double highRealResult[] = new double[1];
                	double imagResult[] = new double[1];
                	int[] nz = new int[1]; // number of components set to zero due to underflow
                    int[] errorFlag = new int[1]; // zero if no error
                    
                	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                            sequenceNumber, highRealResult, imagResult, nz, errorFlag);
                	bes.run();
                	if (errorFlag[0] != 0) {
                	    displayError("Bessel_J error for realArg = " + realArg);
                	    setCompleted(false);
                	    return;
                	}
                	
                	realArg = FLow * distance;
                	double lowRealResult[] = new double[1];
                	bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                            sequenceNumber, lowRealResult, imagResult, nz, errorFlag);
                	bes.run();
                	if (errorFlag[0] != 0) {
                	    displayError("Bessel_J error for realArg = " + realArg);
                	    setCompleted(false);
                	    return;
                	}                	
                    iKernel[pos] = (float)((FHigh / (2.0 * Math.PI * distance) * highRealResult[0]) -
                                   (FLow / (2.0 * Math.PI * distance) * lowRealResult[0]));
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
                    distance = (float)
                                   Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                             ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = ((FHigh * FHigh) - (FLow * FLow)) / ((float) (4.0 * Math.PI));
                    } else if (distance < tau) {
                    	double realArg = FHigh * distance;
                    	double imaginaryArg = 0.0;
                    	double initialOrder = 1.0;
                    	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                    	double highRealResult[] = new double[1];
                    	double imagResult[] = new double[1];
                    	int[] nz = new int[1]; // number of components set to zero due to underflow
                        int[] errorFlag = new int[1]; // zero if no error
                        
                    	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                                sequenceNumber, highRealResult, imagResult, nz, errorFlag);
                    	bes.run();
                    	if (errorFlag[0] != 0) {
                    	    displayError("Bessel_J error for realArg = " + realArg);
                    	    setCompleted(false);
                    	    return;
                    	}
                    	
                    	realArg = FLow * distance;
                    	double lowRealResult[] = new double[1];
                    	bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                                sequenceNumber, lowRealResult, imagResult, nz, errorFlag);
                    	bes.run();
                    	if (errorFlag[0] != 0) {
                    	    displayError("Bessel_J error for realArg = " + realArg);
                    	    setCompleted(false);
                    	    return;
                    	}                	
                        iKernel[pos] = (float)((FHigh / (2.0 * Math.PI * distance) * highRealResult[0]) -
                                       (FLow / (2.0 * Math.PI * distance) * lowRealResult[0]));
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
                	double realArg = FHigh * distance;
                	double imaginaryArg = 0.0;
                	double initialOrder = 1.0;
                	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                	double highRealResult[] = new double[1];
                	double imagResult[] = new double[1];
                	int[] nz = new int[1]; // number of components set to zero due to underflow
                    int[] errorFlag = new int[1]; // zero if no error
                    
                	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                            sequenceNumber, highRealResult, imagResult, nz, errorFlag);
                	bes.run();
                	if (errorFlag[0] != 0) {
                	    displayError("Bessel_J error for realArg = " + realArg);
                	    setCompleted(false);
                	    return;
                	}
                	
                	realArg = FLow * distance;
                	double lowRealResult[] = new double[1];
                	bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                            sequenceNumber, lowRealResult, imagResult, nz, errorFlag);
                	bes.run();
                	if (errorFlag[0] != 0) {
                	    displayError("Bessel_J error for realArg = " + realArg);
                	    setCompleted(false);
                	    return;
                	}                	
                    iKernel[pos] = (float)((-FHigh / (2.0 * Math.PI * distance) * highRealResult[0]) +
                                   (FLow / (2.0 * Math.PI * distance) * lowRealResult[0]));
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
                    distance = (float)
                                   Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                             ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = 1.0f - (((FHigh * FHigh) - (FLow * FLow)) / ((float) (4.0 * Math.PI)));
                    } else if (distance < tau) {
                    	double realArg = FHigh * distance;
                    	double imaginaryArg = 0.0;
                    	double initialOrder = 1.0;
                    	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                    	double highRealResult[] = new double[1];
                    	double imagResult[] = new double[1];
                    	int[] nz = new int[1]; // number of components set to zero due to underflow
                        int[] errorFlag = new int[1]; // zero if no error
                        
                    	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                                sequenceNumber, highRealResult, imagResult, nz, errorFlag);
                    	bes.run();
                    	if (errorFlag[0] != 0) {
                    	    displayError("Bessel_J error for realArg = " + realArg);
                    	    setCompleted(false);
                    	    return;
                    	}
                    	
                    	realArg = FLow * distance;
                    	double lowRealResult[] = new double[1];
                    	bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                                sequenceNumber, lowRealResult, imagResult, nz, errorFlag);
                    	bes.run();
                    	if (errorFlag[0] != 0) {
                    	    displayError("Bessel_J error for realArg = " + realArg);
                    	    setCompleted(false);
                    	    return;
                    	}                	
                        iKernel[pos] = (float)((-FHigh / (2.0 * Math.PI * distance) * highRealResult[0]) +
                                       (FLow / (2.0 * Math.PI * distance) * lowRealResult[0]));
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
                	double realArg = cutoffFreq * distance;
                	double imaginaryArg = 0.0;
                	double initialOrder = 1.0;
                	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                	double realResult[] = new double[1];
                	double imagResult[] = new double[1];
                	int[] nz = new int[1]; // number of components set to zero due to underflow
                    int[] errorFlag = new int[1]; // zero if no error
                    
                	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                            sequenceNumber, realResult, imagResult, nz, errorFlag);
                	bes.run();
                	if (errorFlag[0] != 0) {
                	    displayError("Bessel_J error for realArg = " + realArg);
                	    setCompleted(false);
                	    return;
                	}
                	        	
                    iKernel[pos] = (float)(-cutoffFreq / (2.0 * Math.PI * distance) * realResult[0]) ;
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
                    distance = (float)
                                   Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                             ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = 1.0f - ((cutoffFreq * cutoffFreq) / ((float) (4.0 * Math.PI)));
                    } else if (distance < tau) {
                    	double realArg = cutoffFreq * distance;
                    	double imaginaryArg = 0.0;
                    	double initialOrder = 1.0;
                    	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                    	double realResult[] = new double[1];
                    	double imagResult[] = new double[1];
                    	int[] nz = new int[1]; // number of components set to zero due to underflow
                        int[] errorFlag = new int[1]; // zero if no error
                        
                    	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                                sequenceNumber, realResult, imagResult, nz, errorFlag);
                    	bes.run();
                    	if (errorFlag[0] != 0) {
                    	    displayError("Bessel_J error for realArg = " + realArg);
                    	    setCompleted(false);
                    	    return;
                    	}
                        iKernel[pos] = (float)(-cutoffFreq / (2.0 * Math.PI * distance) *
                                           realResult[0]);
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
                	double realArg = cutoffFreq * distance;
                	double imaginaryArg = 0.0;
                	double initialOrder = 1.0;
                	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                	double realResult[] = new double[1];
                	double imagResult[] = new double[1];
                	int[] nz = new int[1]; // number of components set to zero due to underflow
                    int[] errorFlag = new int[1]; // zero if no error
                    
                	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                            sequenceNumber, realResult, imagResult, nz, errorFlag);
                	bes.run();
                	if (errorFlag[0] != 0) {
                	    displayError("Bessel_J error for realArg = " + realArg);
                	    setCompleted(false);
                	    return;
                	}
                    iKernel[pos] = (float)(cutoffFreq / (2.0 * Math.PI * distance) * realResult[0]);
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
                    distance = (float)
                                   Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                             ((z - halfKDim) * (z - halfKDim)));

                    if ((x == halfKDim) && (y == halfKDim) && (z == halfKDim)) {
                        iKernel[pos] = (cutoffFreq * cutoffFreq) / ((float) (4.0 * Math.PI));
                    } else if (distance < tau) {
                    	double realArg = cutoffFreq * distance;
                    	double imaginaryArg = 0.0;
                    	double initialOrder = 1.0;
                    	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
                    	double realResult[] = new double[1];
                    	double imagResult[] = new double[1];
                    	int[] nz = new int[1]; // number of components set to zero due to underflow
                        int[] errorFlag = new int[1]; // zero if no error
                        
                    	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                                sequenceNumber, realResult, imagResult, nz, errorFlag);
                    	bes.run();
                    	if (errorFlag[0] != 0) {
                    	    displayError("Bessel_J error for realArg = " + realArg);
                    	    setCompleted(false);
                    	    return;
                    	}
                        iKernel[pos] = (float)(cutoffFreq / (2.0 * Math.PI * distance) *
                                           realResult[0]);
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
    @SuppressWarnings("unused")
    private void magnitude() {

        int i;

        try {
            magData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            magData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilter: Out of memory creating magData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < arrayLength; i++) {
            magData[i] = (float) java.lang.Math.sqrt((realData[i] * realData[i]) + (imagiData[i] * imagiData[i]));
        }
    }


    /**
     * DOCUMENT ME!
     *
     * @param  fr1  DOCUMENT ME!
     * @param  fr2  DOCUMENT ME!
     */
    private void makeButterworthFilter(float fr1, float fr2) {
        int x, y, z, pos;
        float distsq, width, centersq, coeff, num, xnorm, ynorm, xcenter, ycenter;
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
                            imagiData[pos] *= coeff;
                            imagjData[pos] *= coeff;
                            imagkData[pos] *= coeff;
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
                            imagiData[pos] *= coeff;
                            imagjData[pos] *= coeff;
                            imagkData[pos] *= coeff;
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
                            imagiData[pos] *= coeff;
                            imagjData[pos] *= coeff;
                            imagkData[pos] *= coeff;
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
                            imagiData[pos] *= coeff;
                            imagjData[pos] *= coeff;
                            imagkData[pos] *= coeff;
                        }
                    }
                }
            } // else if (filterType == BANDSTOP)
        } // if ((ndim == 2) || (image25D))
        
    }


    /**
     * makeHyperComplexData -
     */
    private void makeHyperComplexData() {

        int i, j, k, m, n, dimTest;
        float[] tempData;
        float red, green, blue;

        ndim = srcImage.getNDims();
        dimLengths = srcImage.getExtents();
        newDimLengths = new int[dimLengths.length];

        arrayLength = 1;
        newArrayLength = 1;
        doCrop = false;
        zeroPad = false;

        // If imageCrop is false:
        // Find the lowest power of 2 number not less than kdim + dimLengths[i] - 1.
        // If imageCrop is true:
        // Find the lowest power of 2 number not less than dimLengths[i]
        // This must be done to prevent aliasing in using a frequency filter
        // and to have a power of 2 for the FFT.
        // Make dimensions equal to these sizes in a zero padded array

        for (i = 0; i < ndim; i++) {
            arrayLength *= dimLengths[i];
            newDimLengths[i] = dimLengths[i];
        }

        if ((imageCrop == false) && (constructionMethod == WINDOW)) {
            zeroPad = true;

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
                        zeroPad = true;
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
                        zeroPad = true;
                    }
                }
            } // for (i = 0; i < ndim; i++)
        } // else not image25D

        if ((imageCrop == true) && (constructionMethod == WINDOW)) {

            if (image25D) {

                for (i = 0; i < 2; i++) {

                    if ((dimLengths[i] + kDim - 1) > newDimLengths[i]) {
                        doCrop = true;
                    }
                }
            } // if (image25D)
            else { // not image25D

                for (i = 0; i < ndim; i++) {

                    if ((dimLengths[i] + kDim - 1) > newDimLengths[i]) {
                        doCrop = true;
                    }
                }
            } // else not image25D
        } // end of if ((imageCrop == true) && (constructionMethod == WINDOW))

        if (doCrop) {
            start = new int[ndim];
            end = new int[ndim];

            if (image25D) {

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
        } // end of if (doCrop)

        for (i = 0; i < ndim; i++) {
            newArrayLength *= newDimLengths[i];
        }

        newSliceSize = newDimLengths[0] * newDimLengths[1];

        try {
            realData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            realData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory creating realData");

            setCompleted(false);

            return;
        }

        try {
            imagiData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            imagiData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory creating imagiData");

            setCompleted(false);

            return;
        }
        
        try {
            imagjData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            imagjData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory creating imagjData");

            setCompleted(false);

            return;
        }
        
        try {
            imagkData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            imagkData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilterColor: Out of memory creating imagkData");

            setCompleted(false);

            return;
        }

        try {
                srcImage.exportRGBData(1, 0, arrayLength, imagiData); // locks and releases and lock
                srcImage.exportRGBData(2, 0, arrayLength, imagjData); // locks and releases and lock
                srcImage.exportRGBData(3, 0, arrayLength, imagkData); // locks and releases and lock
                Arrays.fill(realData, 0.0f);
            
        } catch (IOException error) {
            displayError("AlgorithmFrequencyFilter: Source image is locked");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            realData = null;
            imagiData = null;
            imagjData = null;
            imagkData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilter: Out of memory");

            setCompleted(false);

            return;
        }
        
        minimum = Float.MAX_VALUE;
        maximum = -Float.MAX_VALUE;

        for (i = 0; i < arrayLength; i++) {

            if (imagiData[i] > maximum) {
                maximum = imagiData[i];
            }

            if (imagiData[i] < minimum) {
                minimum = imagiData[i];
            }
            
            if (imagjData[i] > maximum) {
                maximum = imagjData[i];
            }

            if (imagjData[i] < minimum) {
                minimum = imagjData[i];
            }
            
            if (imagkData[i] > maximum) {
                maximum = imagkData[i];
            }

            if (imagkData[i] < minimum) {
                minimum = imagkData[i];
            }
        }
        
        for (i = 0; i < arrayLength; i++) {
        	red = imagiData[i];
        	green = imagjData[i];
        	blue = imagkData[i];
        	imagiData[i] = (float)((red + green + blue)/sqrt3);
        	imagjData[i] = (float)((green - blue)/sqrt2);
        	imagkData[i] = (float)((-2.0*red + green + blue)/sqrt6);
        }

        if (filterType == HOMOMORPHIC) {

            // fireProgressStateChanged("Taking log of data");
            fireProgressStateChanged(-1, null, "Taking log of data ...");

            if (minimum < 1.0f) {
                float makePos = 1.0f - minimum;

                for (i = 0; i < arrayLength; i++) {
                    realData[i] += makePos;
                }
            } // if (minimum < 1.0f)

            for (i = 0; i < arrayLength; i++) {
                realData[i] = (float) Math.log(realData[i]);
            }
        } // if (filterType == HOMOMORPHIC)

        if (doCrop) {

            try {
                tempData = new float[arrayLength];
            } catch (OutOfMemoryError e) {
                tempData = null;
                System.gc();
                displayError("AlgorithmFrequencyFilter: Out of memory creating tempData for cropping");

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
                    tempData[i - start[0]] = imagiData[i];
                }
            } else if (ndim == 2) {

                for (j = start[1]; j <= end[1]; j++) {

                    for (i = start[0]; i <= end[0]; i++) {
                        tempData[i - start[0] + (dimLengths[0] * (j - start[1]))] = imagiData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = start[2]; k <= end[2]; k++) {

                    for (j = start[1]; j <= end[1]; j++) {

                        for (i = start[0]; i <= end[0]; i++) {
                            tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                     (dimLengths[0] * dimLengths[1] * (k - start[2]))] = imagiData[i +
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
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m - start[3]))] = imagiData[i +
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
                                                  (n - start[4]))] = imagiData[i + (dimLengths[0] * j) +
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
                imagiData[i] = tempData[i];
            }
            
            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = start[0]; i <= end[0]; i++) {
                    tempData[i - start[0]] = imagjData[i];
                }
            } else if (ndim == 2) {

                for (j = start[1]; j <= end[1]; j++) {

                    for (i = start[0]; i <= end[0]; i++) {
                        tempData[i - start[0] + (dimLengths[0] * (j - start[1]))] = imagjData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = start[2]; k <= end[2]; k++) {

                    for (j = start[1]; j <= end[1]; j++) {

                        for (i = start[0]; i <= end[0]; i++) {
                            tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                     (dimLengths[0] * dimLengths[1] * (k - start[2]))] = imagjData[i +
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
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m - start[3]))] = imagjData[i +
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
                                                  (n - start[4]))] = imagjData[i + (dimLengths[0] * j) +
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
                imagjData[i] = tempData[i];
            }
            
            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = start[0]; i <= end[0]; i++) {
                    tempData[i - start[0]] = imagkData[i];
                }
            } else if (ndim == 2) {

                for (j = start[1]; j <= end[1]; j++) {

                    for (i = start[0]; i <= end[0]; i++) {
                        tempData[i - start[0] + (dimLengths[0] * (j - start[1]))] = imagkData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = start[2]; k <= end[2]; k++) {

                    for (j = start[1]; j <= end[1]; j++) {

                        for (i = start[0]; i <= end[0]; i++) {
                            tempData[i - start[0] + (dimLengths[0] * (j - start[1])) +
                                     (dimLengths[0] * dimLengths[1] * (k - start[2]))] = imagkData[i +
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
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m - start[3]))] = imagkData[i +
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
                                                  (n - start[4]))] = imagkData[i + (dimLengths[0] * j) +
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
                imagkData[i] = tempData[i];
            }

        } // end of if (doCrop)

        if (zeroPad) {

            // zero pad the data so that all dimensions are powers of 2
            // fireProgressStateChanged("Zero padding source data...");
            fireProgressStateChanged(-1, null, "Zero padding source data ...");

            try {
                tempData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                tempData = null;
                System.gc();
                displayError("AlgorithmFrequencyFilter: Out of memory creating tempData for zero padding");

                setCompleted(false);

                return;
            }

            Arrays.fill(tempData, 0.0f);

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
                displayError("AlgorithmFrequencyFilter: Out of memory creating realData in zero padding routine");

                setCompleted(false);

                return;
            }

            for (i = 0; i < newArrayLength; i++) {
                realData[i] = tempData[i];
            }

            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = 0; i < dimLengths[0]; i++) {
                    tempData[i] = imagiData[i];
                }
            } else if (ndim == 2) {

                for (j = 0; j < dimLengths[1]; j++) {

                    for (i = 0; i < dimLengths[0]; i++) {
                        tempData[i + (newDimLengths[0] * j)] = imagiData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = 0; k < dimLengths[2]; k++) {

                    for (j = 0; j < dimLengths[1]; j++) {

                        for (i = 0; i < dimLengths[0]; i++) {
                            tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k)] = imagiData[i +
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
                                         (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] * m)] = imagiData[i +
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
                                                  newDimLengths[3] * n)] = imagiData[i + (dimLengths[0] * j) +
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
                imagiData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                imagiData = null;
                System.gc();
                displayError("AlgorithmFrequencyFilter: Out of memory creating imagData in zero padding routine");

                setCompleted(false);

                return;
            }

            for (i = 0; i < newArrayLength; i++) {
                imagiData[i] = tempData[i];
            }
            
            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = 0; i < dimLengths[0]; i++) {
                    tempData[i] = imagjData[i];
                }
            } else if (ndim == 2) {

                for (j = 0; j < dimLengths[1]; j++) {

                    for (i = 0; i < dimLengths[0]; i++) {
                        tempData[i + (newDimLengths[0] * j)] = imagjData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = 0; k < dimLengths[2]; k++) {

                    for (j = 0; j < dimLengths[1]; j++) {

                        for (i = 0; i < dimLengths[0]; i++) {
                            tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k)] = imagjData[i +
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
                                         (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] * m)] = imagjData[i +
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
                                                  newDimLengths[3] * n)] = imagjData[i + (dimLengths[0] * j) +
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
                imagjData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                imagjData = null;
                System.gc();
                displayError("AlgorithmFrequencyFilter: Out of memory creating imagjData in zero padding routine");

                setCompleted(false);

                return;
            }

            for (i = 0; i < newArrayLength; i++) {
                imagjData[i] = tempData[i];
            }

            Arrays.fill(tempData, 0.0f);

            if (ndim == 1) {

                for (i = 0; i < dimLengths[0]; i++) {
                    tempData[i] = imagkData[i];
                }
            } else if (ndim == 2) {

                for (j = 0; j < dimLengths[1]; j++) {

                    for (i = 0; i < dimLengths[0]; i++) {
                        tempData[i + (newDimLengths[0] * j)] = imagkData[i + (dimLengths[0] * j)];
                    }
                }
            } else if (ndim == 3) {

                for (k = 0; k < dimLengths[2]; k++) {

                    for (j = 0; j < dimLengths[1]; j++) {

                        for (i = 0; i < dimLengths[0]; i++) {
                            tempData[i + (newDimLengths[0] * j) + (newDimLengths[0] * newDimLengths[1] * k)] = imagkData[i +
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
                                         (newDimLengths[0] * newDimLengths[1] * newDimLengths[2] * m)] = imagkData[i +
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
                                                  newDimLengths[3] * n)] = imagkData[i + (dimLengths[0] * j) +
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
                imagkData = new float[newArrayLength];
            } catch (OutOfMemoryError e) {
                imagkData = null;
                System.gc();
                displayError("AlgorithmFrequencyFilter: Out of memory creating imagkData in zero padding routine");

                setCompleted(false);

                return;
            }

            for (i = 0; i < newArrayLength; i++) {
                imagkData[i] = tempData[i];
            }

            fireProgressStateChanged((.1f), null, null);
            // fireProgressStateChanged(10);
        } // end of if (zeroPad)
    } // end of makeHyperComplexData()

    /**
     * DOCUMENT ME!
     *
     * @param  freqU        DOCUMENT ME!
     * @param  freqV        DOCUMENT ME!
     * @param  sigmaU       DOCUMENT ME!
     * @param  sigmaV       DOCUMENT ME!
     * @param  theta        DOCUMENT ME!
     * @param  createGabor  DOCUMENT ME!
     */
    private void makeGaborFilter(float freqU, float freqV, float sigmaU, float sigmaV, float theta,
                                 boolean createGabor) {
        int x, y, z, pos;
        int upperZ;
        float xcenter, ycenter;
        float xScale, yScale;
        float u, v;
        float cosTheta;
        float sinTheta;
        double xDenom, yDenom;
        float coeff;
        float[] realData2 = null;
        int[] gaborExtents;

        if (createGabor) {
            realData2 = new float[newDimLengths[0] * newDimLengths[1]];
        }

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
                    imagiData[pos] *= coeff;
                    imagjData[pos] *= coeff;
                    imagkData[pos] *= coeff;

                    if (createGabor) {

                        if (z == 0) {
                            realData2[pos] = coeff;
                        }
                    }
                }
            }
        }

        if (createGabor) {
            gaborExtents = new int[2];
            gaborExtents[0] = newDimLengths[0];
            gaborExtents[1] = newDimLengths[1];
            gaborImage = new ModelImage(ModelStorageBase.FLOAT, gaborExtents, "gaborFilter");

            try {
                gaborImage.importData(0, realData2, true);
            } catch (IOException e) {
                displayError("Error on gaborImage.importData");
            }

            new ViewJFrameImage(gaborImage, null, new Dimension(610, 220));
        }
    } // private void makeGaborFilter

    /**
     * DOCUMENT ME!
     *
     * @param  rmsFreq  DOCUMENT ME!
     */
    private void makeGaussianFilter(float rmsFreq) {
        double xexpDenom, yexpDenom;
        int x, y, z, pos;
        int upperZ;
        float coeff, xcenter, ycenter;

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
                            imagiData[pos] *= coeff;
                            imagjData[pos] *= coeff;
                            imagkData[pos] *= coeff;
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
                            imagiData[pos] *= coeff;
                            imagjData[pos] *= coeff;
                            imagkData[pos] *= coeff;
                        }
                    }
                }
            } // end of if (filterType == HIGHPASS)
        } // end of if ((ndim == 2) || (image25D))
        
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fr1  DOCUMENT ME!
     */
    private void makeHomomorphicFilter(float fr1) {
        int x, y, z, pos;
        float distsq, coeff, xnorm, ynorm, xcenter, ycenter;
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

            for (z = 0; z <= upperZ; z++) {

                for (y = 0; y <= (newDimLengths[1] - 1); y++) {

                    for (x = 0; x <= (newDimLengths[0] - 1); x++) {
                        pos = (z * newSliceSize) + (y * newDimLengths[0]) + x;
                        distsq = ((x - xcenter) * (x - xcenter) / xnorm) + ((y - ycenter) * (y - ycenter) / ynorm);
                        coeff = (float) (1.0 / (1.0 + Math.pow((fr1 * fr1) / distsq, butterworthOrder)));
                        realData[pos] *= (((highGain - lowGain) * coeff) + lowGain);
                        imagiData[pos] *= (((highGain - lowGain) * coeff) + lowGain);
                        imagjData[pos] *= (((highGain - lowGain) * coeff) + lowGain);
                        imagkData[pos] *= (((highGain - lowGain) * coeff) + lowGain);
                    }
                }
            }
        } // if ((ndim == 2) || (image25D))
        
    }

    /* l'Hopitals rule if f(a) = g(a) = 0 and if the limit of the ratio f'(t)/g'(t) as t approaches a exists,
     * then lim t->a f(t)/g(t) = lim t->a f'(t)/g'(t).  The hlp(n1,n2)low pass impulse response may be regarded as
     * ((R*R)/(2*PI))*J(R*sqrt(n1*n1 + n2*n2))/(R*sqrt(n1*n1 + n2*n2)).  The taylor series for J1(x) = (x/2) -
     * ((x**3)/16) + ...  Therefore, the derivative of J1(x) at x = 0 equals 1/2.  Hence, thehlp(n1,n2)low pass impulse
     * response is equal to (R**2)/(4*PI) at n1=n2=0. */

    /**
     * sets the kernel variables.
     */
    private void makeKernelData() {

        int i;
        int x, y, z, pos;

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
                displayError("AlgorithmFrequencyFilter: Out of memory creating realKernelData");

                setCompleted(false);

                return;
            }

            Arrays.fill(realKernelData, 0.0f);

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
                displayError("AlgorithmFrequencyFilter: Out of memory creating imagKernelData");

                setCompleted(false);

                return;
            }

            for (i = 0; i < newSliceSize; i++) {
                imagKernelData[i] = 0.0f;
            }

        } // if ((ndim == 2) || (image25D))
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
                displayError("AlgorithmFrequencyFilter: Out of memory creating realKernelData");

                setCompleted(false);

                return;
            }

            Arrays.fill(realKernelData, 0.0f);

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
                displayError("AlgorithmFrequencyFilter: Out of memory creating imagKernelData");

                setCompleted(false);

                return;
            }

            Arrays.fill(imagKernelData, 0.0f);
        } // end of if (ndim = 3)
    }

    /**
     * Calculates phase from real and imaginary parts.
     *
     * @param  phase  = arctan(imagiData/realData);
     */
    @SuppressWarnings("unused")
    private void phase(float[] phase) {

        int i;

        try {
            phaseData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            phaseData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilter: Out of memory creating phaseData");
            setCompleted(false);

            return;
        }

        for (i = 0; i < arrayLength; i++) {
            phase[i] = (float) java.lang.Math.atan2(imagiData[i], realData[i]);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void restoreFinalData(float finalData[]) {
        int i;
        float[] finalData2;

        fireProgressStateChanged(-1, null, "Taking exponentials ...");
        // fireProgressStateChanged("Taking exponentials");

        for (i = 0; i < finalData.length; i++) {
            finalData[i] = (float) Math.exp(finalData[i]);
        }

        if ((lowTruncated > 0.0) || (highTruncated > 0.0)) {

            // fireProgressStateChanged("Sorting data");
            fireProgressStateChanged(-1, null, "Sorting data ...");
            finalData2 = new float[finalData.length];

            for (i = 0; i < finalData.length; i++) {
                finalData2[i] = finalData[i];
            }

            Arrays.sort(finalData2);

            if (lowTruncated > 0.0) {
                fireProgressStateChanged(-1, null, "Clamping low data ...");
                // fireProgressStateChanged("Clamping low data");

                int lowIndex = Math.round(lowTruncated * (finalData.length - 1));
                float lowClamp = finalData2[lowIndex];

                for (i = 0; i < finalData.length; i++) {

                    if (finalData[i] < lowClamp) {
                        finalData[i] = lowClamp;
                    }
                }
            } // if (lowTruncated > 0.0)

            if (highTruncated > 0.0) {
                fireProgressStateChanged(-1, null, "Clamping high data ...");
                // fireProgressStateChanged("Clamping high data");

                int highIndex = Math.round((1 - highTruncated) * (finalData.length - 1));
                float highClamp = finalData2[highIndex];

                for (i = 0; i < finalData.length; i++) {

                    if (finalData[i] > highClamp) {
                        finalData[i] = highClamp;
                    }
                }
            } // if (highTruncated > 0.0)
        } // if (lowTruncated > 0.0) || (highTruncated > 0.0))

        float newMin = Float.MAX_VALUE;
        float newMax = -Float.MAX_VALUE;

        for (i = 0; i < finalData.length; i++) {

            if (finalData[i] < newMin) {
                newMin = finalData[i];
            }

            if (finalData[i] > newMax) {
                newMax = finalData[i];
            }
        } // for (i = 0; i < finalData.length; i++)

        // Rescale to original range
        // a*newMax + b = maximum
        // a*newMin + b = mimimum
        float a = (maximum - minimum) / (newMax - newMin);
        float b = maximum - (a * newMax);

        for (i = 0; i < finalData.length; i++) {
            finalData[i] = (a * finalData[i]) + b;
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
        int newLength;

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
            displayError("AlgorithmFrequencyFilter: Out of memory creating tempData in shiftBack routine");

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
    private void zeroAround(float finalData[]) {
        int i, j, k, m, n;
        float[] tempData;

        try {
            tempData = new float[arrayLength];
        } catch (OutOfMemoryError e) {
            tempData = null;
            System.gc();
            displayError("AlgorithmFrequencyFilter: Out of memory creating tempData in zeroAround routine");

            setCompleted(false);

            return;
        }

        Arrays.fill(tempData, 0.0f);

        if (ndim == 1) {

            for (i = 0; i <= (end[0] - start[0]); i++) {
                tempData[i + start[0]] = finalData[i];
            }
        } else if (ndim == 2) {

            for (j = 0; j <= (end[1] - start[1]); j++) {

                for (i = 0; i <= (end[0] - start[0]); i++) {
                    tempData[i + start[0] + (dimLengths[0] * (j + start[1]))] = finalData[i + (dimLengths[0] * j)];
                }
            }
        } else if (ndim == 3) {

            for (k = 0; k <= (end[2] - start[2]); k++) {

                for (j = 0; j <= (end[1] - start[1]); j++) {

                    for (i = 0; i <= (end[0] - start[0]); i++) {
                        tempData[i + start[0] + (dimLengths[0] * (j + start[1])) +
                                 (dimLengths[0] * dimLengths[1] * (k + start[2]))] = finalData[i + (dimLengths[0] * j) +
                                                                                               (dimLengths[0] *
                                                                                                    dimLengths[1] * k)];
                    }
                }
            }
        } else if (ndim == 4) {

            for (m = 0; m <= (end[3] - start[3]); m++) {

                for (k = 0; k <= (end[2] - start[2]); k++) {

                    for (j = 0; j <= (end[1] - start[1]); j++) {

                        for (i = 0; i <= (end[0] - start[0]); i++) {
                            tempData[i + start[0] + (dimLengths[0] * (j + start[1])) +
                                     (dimLengths[0] * dimLengths[1] * (k + start[2])) +
                                     (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m + start[3]))] = finalData[i +
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

            for (n = 0; n <= (end[4] - start[4]); n++) {

                for (m = 0; m <= (end[3] - start[3]); m++) {

                    for (k = 0; k <= (end[2] - start[2]); k++) {

                        for (j = 0; j <= (end[1] - start[1]); j++) {

                            for (i = 0; i <= (end[0] - start[0]); i++) {
                                tempData[i + start[0] + (dimLengths[0] * (j + start[1])) +
                                         (dimLengths[0] * dimLengths[1] * (k + start[2])) +
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * (m + start[3])) +
                                         (dimLengths[0] * dimLengths[1] * dimLengths[2] * dimLengths[3] *
                                              (n + start[4]))] = finalData[i + (dimLengths[0] * j) +
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
            finalData[i] = tempData[i];
        }
    }

}
