package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;


/**
 * An implementation of Maximum Likelihood Iterated Blind Deconvolution based on the following papers:
 *
 * <p>Holmes, T.J., Maximum Likelihood Image Restoration Adapted for Noncoherent Optical Imaging, JOSA-A, 5(5): 666-673,
 * 1988.</p>
 *
 * <p>Holmes, T., Bhattacharyya, S., Cooper, J., Hanzel, D., Krishnamurthi, V., Lin, W., Roysam, B., Szarowski, D.,
 * Turner, J., Light Microscopic Images Reconstructed by Maximum Likelihood Deconvolution, Ch. 24, Handbook of
 * Biological Confocal Microscopy, J. Pawley, Plenum, 1995.</p>
 *
 * <p>Holmes, T., Liu, Y., Image Restoration for 2D and 3D Fluorescence Microscopy, Visualization in Biomedical
 * Microscopies, A. Kriete, VCH, 1992.</p>
 *
 * <p>Holmes, T., Blind Deconvolution of Quantum-Limited Noncoherent Imagery, JOSA-A, 9: 1052 - 1061, 1992.</p>
 */
public class AlgorithmMaximumLikelihoodIteratedBlindDeconvolution extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] m_afResolutions = new float[] { 1f, 1f, 1f };

    /** DOCUMENT ME! */
    private int[] m_aiUnits;

    /** DOCUMENT ME! */
    private boolean m_bConstraintsInit = false;

    /** Flag indicating whether or not to update the progres bar:. */
    private boolean m_bUpdatePBar = true;

    /** Boolean to use constraints:. */
    private boolean m_bUseMicroscopeConstraints = true;

    /** DOCUMENT ME! */
    private float m_fAxialBandLimit;

    /** Physically-based parameters:. */
    /** numerical aperature of the imaging lens:. */
    private float m_fObjectiveNumericalAperature;

    /** 3). Bandlimit and missing cone constraint: */
    private float m_fRadialBandLimit;

    /** DOCUMENT ME! */
    private float m_fRadius;

    /** sample index of refraction:. */
    private float m_fRefractiveIndex;

    /** DOCUMENT ME! */
    private float m_fTanTheta;

    /** wavelength of the reflected or fluorescing light:. */
    private float m_fWavelength;

    /** Original data size:. */
    private int m_iArrayLength = 1;

    /** Constraint parameters:. */
    private int m_iDimX;

    /** DOCUMENT ME! */
    private int m_iDimY;

    /** DOCUMENT ME! */
    private int m_iDimZ;

    /** Number of iterations for the deconvolution:. */
    private int m_iNumberIterations;

    /** Show the deconvolved image in progress every m_iNumberProgress steps:. */
    private int m_iNumberProgress;
    
    /** The new resolutions in each dimension if resample is true. Used only if resample is true. */
    private float[] m_fNewRes;
    
    /** DOCUMENT ME! */
    private ModelImage m_kCalcResult1;

    /** DOCUMENT ME! */
    private ModelImage m_kCalcResult2;

    /** DOCUMENT ME! */
    private ModelImage m_kCalcResult3;

    /** estimated of the reconstructed image:. */
    private ModelImage m_kEstimatedImage;

    /** Images to store FFT and Calc results. */
    private ModelImage m_kImageSpectrum1;

    /** DOCUMENT ME! */
    private ModelImage m_kImageSpectrum2;

    /** DOCUMENT ME! */
    private ModelImage m_kImageSpectrum3;

    /** DOCUMENT ME! */
    private ModelImage m_kMirrorImage;

    /** source image to be reconstructed! */
    private ModelImage m_kOriginalSourceImage;

    /** point spread function image:. */
    private ModelImage m_kPSFImage;

    /** Used for color image. */
    private ModelImage m_kPSFImageCopy;

    /** DOCUMENT ME! */
    private ModelImage m_kSourceBlue = null;

    /** DOCUMENT ME! */
    private ModelImage m_kSourceGreen = null;

    /** source image to be reconstructed! */
    private ModelImage m_kSourceImage;

    /**
     * For color images: the source is converted to gray for the deconvolution. Once the estimate and psf are found,
     * each component is reconstructed.
     */
    private ModelImage m_kSourceRed = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMaximumLikelihoodIteratedBlindDeconvolution object.
     *
     * @param  kSrcImg                       the input image to be reconstructed
     * @param  iIterations                   the number of times to iterate in the deconvolution
     * @param  iProgress                     Display deconvolved image every iProgress images
     * @param  fObjectiveNumericalAperature  the numerical aperature of the imaging lense
     * @param  fWavelength                   the reflected or fluorescing light
     * @param  fRefractiveIndex              the index of refraction for the sample
     * @param  bUseConstraints               When true, the lens NA, wavelength, and refractive index are used in the
     *                                       deconvolution process
     */
    public AlgorithmMaximumLikelihoodIteratedBlindDeconvolution(ModelImage kSrcImg, int iIterations, int iProgress,
                                                                float fObjectiveNumericalAperature, float fWavelength,
                                                                float fRefractiveIndex, boolean bUseConstraints, int[] newExtents, boolean doResample) {
    	if (doResample) {
    		m_kOriginalSourceImage = resample(kSrcImg, newExtents);
    		m_kSourceImage = (ModelImage) m_kOriginalSourceImage.clone();
    		new ViewJFrameImage((ModelImage)(m_kOriginalSourceImage.clone()), null, new Dimension(610, 200));
    		
    	} else {
    		m_kOriginalSourceImage = kSrcImg;
            m_kSourceImage = (ModelImage) kSrcImg.clone();
    		
    	}
        m_iNumberIterations = iIterations;
        m_iNumberProgress = iProgress;
        m_fObjectiveNumericalAperature = fObjectiveNumericalAperature;
        m_fWavelength = fWavelength;
        m_fRefractiveIndex = fRefractiveIndex;
        m_bUseMicroscopeConstraints = bUseConstraints;

        /* Determine the array length: */
        m_iArrayLength = 1;

        for (int i = 0; i < m_kSourceImage.getNDims(); i++) {
            m_iArrayLength *= m_kSourceImage.getExtents()[i];
        }

        /* convert color images: */
        if (m_kSourceImage.isColorImage() && (m_iNumberIterations != 0)) {
            m_kSourceImage.disposeLocal();
            m_kSourceImage = convertToGray(m_kOriginalSourceImage);
        }

        /* Convert to float: */
        if (!m_kSourceImage.isColorImage()) {

            try {
                m_kSourceImage.convertToFloat();
            } catch (java.io.IOException e) { }
        }

        /* The initial guess at the estimated image is the original image: */
        m_kEstimatedImage = (ModelImage) (m_kSourceImage.clone());
           m_kEstimatedImage.setImageName("estimate" + 0);

        /* The initial psf is a gaussian of size 3x3: */
        // m_kPSFImage = initPSF(m_kSourceImage.getNDims(), m_kSourceImage.getExtents());
        initPSF(m_kSourceImage.getNDims(), m_kSourceImage.getExtents());

        /*
         * new ViewJFrameImage((ModelImage)(m_kPSFImage.clone()), null, new Dimension(610, 200));
         */
        /* Initialize progress bar: */
        //fireProgressStateChanged(0,"Blind Deconvolution", "Computing iterative blind deconvolution...");
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {

        if (m_kImageSpectrum1 != null) {
            m_kImageSpectrum1.disposeLocal();
            m_kImageSpectrum1 = null;
        }

        if (m_kImageSpectrum2 != null) {
            m_kImageSpectrum2.disposeLocal();
            m_kImageSpectrum2 = null;
        }

        if (m_kImageSpectrum3 != null) {
            m_kImageSpectrum3.disposeLocal();
            m_kImageSpectrum3 = null;
        }

        if (m_kMirrorImage != null) {
            m_kMirrorImage.disposeLocal();
            m_kMirrorImage = null;
        }

        if (m_kCalcResult1 != null) {
            m_kCalcResult1.disposeLocal();
            m_kCalcResult1 = null;
        }

        if (m_kCalcResult3 != null) {
            m_kCalcResult3.disposeLocal();
            m_kCalcResult3 = null;
        }

        System.gc();
    }


    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Returns the point spread function image.
     *
     * @return  m_kPSFImage
     */
    public ModelImage getPSFImage() {
        return m_kPSFImageCopy;
    }

    /**
     * Returns the reconstructed image:
     *
     * @return  m_kEstimatedImage
     */
    public ModelImage getReconstructedImage() {
        return m_kEstimatedImage;
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (m_kSourceImage == null) {
            MipavUtil.displayError("AlgorithmMaximumLikelihoodIteratedBlindDeconvolution  Source Image is null");

            return;
        }

        if (m_iNumberIterations != 0) {

            runDeconvolution(m_kSourceImage, m_kEstimatedImage, m_kPSFImage, true);

            if (threadStopped) {
                finalize();

                return;
            }

            if (m_kOriginalSourceImage.isColorImage()) {
                m_iNumberIterations = 1;

                /* doesn't matter, just has to be higher than m_iNumberIterations */
                m_iNumberProgress = 2;
                m_bUpdatePBar = false;

                /* get new estimates for each color channel by runing the
                 * deconvolution with the estimates from the grayscale image --
                 * deconvolution does not iterated again: */
                m_kSourceBlue = runDeconvolution(m_kSourceBlue, (ModelImage) m_kEstimatedImage.clone(),
                                                 (ModelImage) m_kPSFImageCopy.clone(), false);
                m_kSourceRed = runDeconvolution(m_kSourceRed, (ModelImage) m_kEstimatedImage.clone(),
                                                (ModelImage) m_kPSFImageCopy.clone(), false);
                m_kSourceGreen = runDeconvolution(m_kSourceGreen, (ModelImage) m_kEstimatedImage.clone(),
                                                  (ModelImage) m_kPSFImageCopy.clone(), false);

                /* Use the reconstructed color channels to generate new color image: */
                m_kEstimatedImage.disposeLocal();
                m_kEstimatedImage = convertFromGray(m_kSourceRed, m_kSourceGreen, m_kSourceBlue);
                
            }
        }

        fireProgressStateChanged(100);

        cleanUp();
        setCompleted(true);

    } // end runAlgorithm

    /**
     * Performs a AlgorithmImageCalculator calculation on the two input images. Used here to either multiply or divide
     * two images.
     *
     * @param   kReturn  DOCUMENT ME!
     * @param   kImage1  the first input image
     * @param   kImage2  the second input image
     * @param   iType    the type of calculation (multiply or divide)
     *
     * @return  kReturn, the result of the image calculation
     */
    private ModelImage calc(ModelImage kReturn, ModelImage kImage1, ModelImage kImage2, int iType) {

        AlgorithmImageCalculator algImageCalc = new AlgorithmImageCalculator(kReturn, kImage1, kImage2, iType,
                                                                             AlgorithmImageMath.CLIP, true, null);

        /* Must not run in separate thread, since we need the results before
         * proceeding to the next step: */
        algImageCalc.setRunningInSeparateThread(false);
        algImageCalc.run();
        algImageCalc.finalize();
        algImageCalc = null;
        System.gc();

        return kReturn;

    }

    /**
     * Dispose of temporary images after the deconvolution process is completed:
     */
    private void cleanUp() {
        m_kSourceImage.disposeLocal();
        m_kSourceImage = null;

        m_kOriginalSourceImage = null;

        if (m_kPSFImage != null) {
            m_kPSFImage.disposeLocal();
            m_kPSFImage = null;
        }

        if (m_kSourceRed != null) {
            m_kSourceRed.disposeLocal();
            m_kSourceRed = null;
        }

        if (m_kSourceGreen != null) {
            m_kSourceGreen.disposeLocal();
            m_kSourceGreen = null;
        }

        if (m_kSourceBlue != null) {
            m_kSourceBlue.disposeLocal();
            m_kSourceBlue = null;
        }

    }

    /**
     * Applies constraints to the input image kImage and writes the constrained values into the m_kPSFImage. Constraints
     * are the following: 2). Hourglass constraint 3). Bandlimit and missing cone constraint 4). Nonnegativity
     * constraint 1). Unit summation constraint
     *
     * @param   kImagePSF  the unconstrained PSFImage
     *
     * @return  DOCUMENT ME!
     */
    private ModelImage constraints(ModelImage kImagePSF) {

        if (m_bConstraintsInit == false) {
            initConstraints();
            m_bConstraintsInit = true;
        }

        ModelImage kImageInvFFT = null;

        /* 2). Hourglass constraint: */
        if (m_bUseMicroscopeConstraints) {
            ModelImage kWorking = (ModelImage) kImagePSF.clone();
            float fXHalf = (m_afResolutions[0] * (m_iDimX + 1)) / 2f;
            float fYHalf = (m_afResolutions[1] * (m_iDimY + 1)) / 2f;
            float fZHalf = (m_afResolutions[2] * (m_iDimZ + 1)) / 2f;
            float fX, fY, fZ;

            float fRadiusZ = 0.0f;

            for (int z = 0; z < m_iDimZ; z++) {

                for (int y = 0; y < m_iDimY; y++) {

                    for (int x = 0; x < m_iDimX; x++) {
                        fX = m_afResolutions[0] * x;
                        fY = m_afResolutions[1] * y;
                        fZ = m_afResolutions[2] * z;

                        fRadiusZ = (float) (Math.abs(fZ - fZHalf) * m_fTanTheta);

                        if ((((fX - fXHalf) * (fX - fXHalf)) + ((fY - fYHalf) * (fY - fYHalf))) >
                                ((m_fRadius + fRadiusZ) * (m_fRadius + fRadiusZ))) {
                            kWorking.set((z * m_iDimY * m_iDimX) + (y * m_iDimX) + x, 0f);
                        }
                    }
                }
            }

            /* 3). Bandlimit and missing cone constraint: */
            if (m_kImageSpectrum1 == null) {
                m_kImageSpectrum1 = (ModelImage) kWorking.clone();
            }

            ModelImage kImageFFT = fft(m_kImageSpectrum1, kWorking, 1);
            // kWorking.disposeLocal();

            for (int z = 0; z < m_iDimZ; z++) {

                for (int y = 0; y < m_iDimY; y++) {

                    for (int x = 0; x < m_iDimX; x++) {
                        fX = m_afResolutions[0] * x;
                        fY = m_afResolutions[1] * y;
                        fZ = m_afResolutions[2] * z;

                        fRadiusZ = (float) (Math.abs(fZ - fZHalf) * m_fTanTheta);

                        if (((((fX - fXHalf) * (fX - fXHalf)) + ((fY - fYHalf) * (fY - fYHalf))) >
                                 (m_fRadialBandLimit * m_fRadialBandLimit)) ||
                                ((float) (Math.abs(fZ - fZHalf)) > m_fAxialBandLimit) ||
                                ((((fX - fXHalf) * (fX - fXHalf)) + ((fY - fYHalf) * (fY - fYHalf))) <
                                     (fRadiusZ * fRadiusZ))) {

                            /* Set real value to 0: */
                            kImageFFT.set((z * m_iDimY * m_iDimX) + (y * m_iDimX) + (x) + 0, 0f);

                            /* Set imaginary value to 0: */
                            kImageFFT.set((z * m_iDimY * m_iDimX) + (y * m_iDimX) + (x) + 1, 0f);
                        }
                    }
                }
            }

            if (m_kImageSpectrum2 == null) {
                m_kImageSpectrum2 = (ModelImage) kWorking.clone();
            }

            kWorking.disposeLocal();
            kImageInvFFT = fft(m_kImageSpectrum2, m_kImageSpectrum1, -1);
        } else {
            kImageInvFFT = kImagePSF;
        }

        /* 4). Nonnegativity constraint: */
        float fSum = 0;

        for (int i = 0; i < m_iArrayLength; i++) {

            if (kImageInvFFT.getFloat(i) < 0) {
                kImageInvFFT.set(i, 0f);
            }

            fSum += kImageInvFFT.getFloat(i);
        }

        if (fSum == 0) {
            MipavUtil.displayInfo("Microscope settings inappropriate for the image resolutions, deconvolving without microscope settings.");

            /* Don't use microscope constraints: */
            m_bUseMicroscopeConstraints = false;

            /* Recalculate sum based on no constraints: */
            kImageInvFFT = kImagePSF;

            for (int i = 0; i < m_iArrayLength; i++) {

                if (kImageInvFFT.getFloat(i) < 0) {
                    kImageInvFFT.set(i, 0f);
                }

                fSum += kImageInvFFT.getFloat(i);
            }
        }

        /* 1). Unit summation constraint: */
        for (int i = 0; i < m_iArrayLength; i++) {
            kImagePSF.set(i, kImageInvFFT.getFloat(i) / fSum);
        }

        kImageInvFFT.calcMinMax();

        return kImagePSF;

    }

    /**
     * Called after the deconvolution algorithm is run. The deconvolution processes the grayscale image. Once the
     * estimated and psf images are calculated, they are used to reconstruct the red, green, and blue channels
     * separately, which are then recombined into a color image.
     *
     * @param   kRed    the reconstructed red image
     * @param   kGreen  the reconstructed green image
     * @param   kBlue   the reconstructed blue image
     *
     * @return  the RGB image
     */
    private ModelImage convertFromGray(ModelImage kRed, ModelImage kGreen, ModelImage kBlue) {
        ModelImage kResult = (ModelImage) m_kOriginalSourceImage.clone();
        
        // Make algorithm
        AlgorithmRGBConcat kMathAlgo = new AlgorithmRGBConcat(kRed, kGreen, kBlue, kResult, true, false, 255.0f, true);
    	
        /* Must not run in separate thread, since we need the results before
         * proceeding to the next step: */
        kMathAlgo.setRunningInSeparateThread(false);
        kMathAlgo.run();
        kMathAlgo.finalize();
        kMathAlgo = null;
        System.gc();

        return kResult;
    }

    /**
     * Convert the input image, kImage, to a grayscale image using the AlgorithmRGBtoGray. The deconvolution is done on
     * the grayscale image, however after deconvolution, the original color values are reconstructed from the psf and
     * grayscale estimate.
     *
     * @param   kImage  the color image to conver to grayscale:
     *
     * @return  the converted grayscale image
     */
    private ModelImage convertToGray(ModelImage kImage) {

        /* First save each of the red, green, and blue channels in a
         * separate ModelImage for reconstruction. */
        /* Determine the type of the color image: */
        int iType = kImage.getType();

        if (iType == ModelStorageBase.ARGB) {
            iType = ModelStorageBase.UBYTE;
        }

        if (iType == ModelStorageBase.ARGB_FLOAT) {
            iType = ModelStorageBase.FLOAT;
        }

        if (iType == ModelStorageBase.ARGB_USHORT) {
            iType = ModelStorageBase.USHORT;
        }

        /* Create three separate ModelImages of the same type: */
        m_kSourceRed = new ModelImage(iType, kImage.getExtents(), "GrayRed");
        m_kSourceGreen = new ModelImage(iType, kImage.getExtents(), "GrayGreen");
        m_kSourceBlue = new ModelImage(iType, kImage.getExtents(), "GrayBlue");

        AlgorithmRGBtoGrays kRGBAlgoMulti = new AlgorithmRGBtoGrays(m_kSourceRed, m_kSourceGreen, m_kSourceBlue,
                                                                    kImage);

        /* Must not run in separate thread, since we need the results before
         * proceeding to the next step: */
        kRGBAlgoMulti.setRunningInSeparateThread(false);
        kRGBAlgoMulti.run();
        kRGBAlgoMulti.finalize();
        kRGBAlgoMulti = null;

        JDialogRGBtoGrays kDialogTemp = new JDialogRGBtoGrays(kImage.getParentFrame(), kImage);

        JDialogBase.updateFileInfo(kImage, m_kSourceRed);

        JDialogBase.updateFileInfo(kImage, m_kSourceGreen);

        JDialogBase.updateFileInfo(kImage, m_kSourceBlue);
        kDialogTemp.dispose();
        kDialogTemp = null;

        try {

            /* Convert to float: */
            m_kSourceBlue.convertToFloat();
            m_kSourceRed.convertToFloat();
            m_kSourceGreen.convertToFloat();
        } catch (java.io.IOException e) { }

        m_kSourceRed.calcMinMax();
        m_kSourceGreen.calcMinMax();
        m_kSourceBlue.calcMinMax();
        
        
        /* Convert the input image kImage to gray: */
        ModelImage kResult = new ModelImage(iType, kImage.getExtents(), null);
        AlgorithmRGBtoGray kRGBAlgo = new AlgorithmRGBtoGray(kResult, kImage);

        /* Must not run in separate thread, since we need the results before
         * proceeding to the next step: */
        kRGBAlgo.setRunningInSeparateThread(false);
        kRGBAlgo.run();
        kRGBAlgo.finalize();
        kRGBAlgo = null;
        System.gc();

        return kResult;
    }

    /**
     * Convolves two image by computing the fast fourier transforms of each image, multiplying the ffts and then
     * computing the inverse fft of the result:
     *
     * @param   kImage1  the first input image
     * @param   kImage2  the second input image
     * @param   fNoise   amount of Noise to add when the result of the convolution is going to be the denominator in the
     *                   next equation to avoid divide by zero errors.
     *
     * @return  kResult, the result of convolving the two images
     */
    private ModelImage convolve(ModelImage kImage1, ModelImage kImage2, float fNoise) {

        /* Calculate the forward fft of the two input images: */
        if (m_kImageSpectrum1 == null) {
            m_kImageSpectrum1 = (ModelImage) kImage1.clone();
        }

        ModelImage kImageSpectrum1 = fft(m_kImageSpectrum1, kImage1, 1);

        if (m_kImageSpectrum2 == null) {
            m_kImageSpectrum2 = (ModelImage) kImage2.clone();
        }

        ModelImage kImageSpectrum2 = fft(m_kImageSpectrum2, kImage2, 1);

        /* multiply the resulting spectrums: */
        if (m_kCalcResult1 == null) {
            m_kCalcResult1 = (ModelImage) (kImageSpectrum1.clone());
        }

        ModelImage kConvolve = calc(m_kCalcResult1, kImageSpectrum1, kImageSpectrum2,
                                    AlgorithmImageCalculator.MULTIPLY);

        /* Set the image Convolve flag to be true: */
        kConvolve.setConvolve(true);

        /* Calculate the inverse FFT: */
        if (m_kImageSpectrum3 == null) {
            m_kImageSpectrum3 = (ModelImage) kConvolve.clone();
        }

        ModelImage kResult = fft(m_kImageSpectrum3, kConvolve, -1);

        /* Add noise factor: */
        for (int i = 0; i < m_iArrayLength; i++) {
            kResult.set(i, kResult.getFloat(i) + fNoise);
        }

        /* Return result: */
        return kResult;
    }


    /**
     * Computes the fft of the input image, returns the fft. Calls AlgorithmFFT.
     *
     * @param   kImageFFT  DOCUMENT ME!
     * @param   kImage     the input image
     * @param   iDir       forward fft when iDir = 1, inverse fft when iDir = -1
     *
     * @return  kResult, the fft of kImage
     */
    private ModelImage fft(ModelImage kImageFFT, ModelImage kImage, int iDir) {

        // FFT parameters
        boolean logMagDisplay = false;
        boolean unequalDim = true;
        boolean image25D = false;
        boolean complexInverse = false;

        // take the FFT of the first input image:
        AlgorithmFFT kFFT = new AlgorithmFFT(kImageFFT, kImage, iDir, logMagDisplay, unequalDim, image25D, complexInverse);

        /* Must not run in separate thread, since we need the results before
         * proceeding to the next step: */
        kFFT.setRunningInSeparateThread(false);
        kFFT.run();
        kFFT.finalize();
        kFFT = null;
        System.gc();

        return kImageFFT;
    }

    /**
     * Initializes the constraint variables for the PSF deconvolution, based on the microscope parameters (numerical
     * aperature, wavelength, and refractive index). The pixel resolutions are also converted to nanometers -- the units
     * for wavelength.
     */
    private void initConstraints() {
        m_iDimX = m_kOriginalSourceImage.getExtents()[0];
        m_iDimY = m_kOriginalSourceImage.getExtents()[1];
        m_iDimZ = 1;

        if (m_kOriginalSourceImage.getNDims() > 2) {
            m_iDimZ = m_kOriginalSourceImage.getExtents()[2];
        }

        /* convert pixel resolutions to nanometers: */
        m_aiUnits = m_kOriginalSourceImage.getFileInfo()[0].getUnitsOfMeasure();

        for (int i = 0; i < m_kOriginalSourceImage.getNDims(); i++) {
            m_afResolutions[i] = m_kOriginalSourceImage.getFileInfo()[0].getResolutions()[i];

            if (m_aiUnits[i] == Unit.INCHES.getLegacyNum()) {
                m_afResolutions[i] *= 2.54e7f; // 25400000f;
            } else if (m_aiUnits[i] == Unit.MILS.getLegacyNum()) {
                m_afResolutions[i] *= 2.54e4f;
            } else if (m_aiUnits[i] == Unit.CENTIMETERS.getLegacyNum()) {
                m_afResolutions[i] *= 1e7f; // 10000000f;
            } else if (m_aiUnits[i] == Unit.ANGSTROMS.getLegacyNum()) {
                m_afResolutions[i] *= 0.1f;
            } else if (m_aiUnits[i] == Unit.NANOMETERS.getLegacyNum()) {
                m_afResolutions[i] *= 1f;
            } else if (m_aiUnits[i] == Unit.MICROMETERS.getLegacyNum()) {
                m_afResolutions[i] *= 1e3f; // 1000f;
            } else if (m_aiUnits[i] == Unit.MILLIMETERS.getLegacyNum()) {
                m_afResolutions[i] *= 1e6f; // 1000000f;
            } else if (m_aiUnits[i] == Unit.METERS.getLegacyNum()) {
                m_afResolutions[i] *= 1e9f; // 1000000000f;
            } else if (m_aiUnits[i] == Unit.KILOMETERS.getLegacyNum()) {
                m_afResolutions[i] *= 1e12f; // 1000000000000f;
            } else if (m_aiUnits[i] == Unit.MILES.getLegacyNum()) {
                m_afResolutions[i] *= 1.609344e12f; // 1609344000000f;
            }

        }

        /* 2). Hourglass constraint: */
        m_fRadius = (3 * 0.61f * m_fWavelength) / m_fObjectiveNumericalAperature;
        m_fTanTheta = (float) Math.tan(Math.asin(m_fObjectiveNumericalAperature / m_fRefractiveIndex));

        /* 3). Bandlimit and missing cone constraint: */
        m_fRadialBandLimit = (float) (2.0f * m_fObjectiveNumericalAperature / m_fWavelength);
        m_fAxialBandLimit = (float) ((m_fObjectiveNumericalAperature * m_fObjectiveNumericalAperature) /
                                         (2.0 * m_fRefractiveIndex * m_fWavelength));
    }

    /**
     * Initialize the first PSF guess as a 3x3 Gaussian.
     *
     * @param  iNumberDimensions  iNumberDimensions, the dimensions of the data
     * @param  aiExtents          aiExtents, the ranges in each dimension
     */
    private void initPSF(int iNumberDimensions, int[] aiExtents) {

        /* Create a new ModelImage to contain the Gaussian: */
        m_kPSFImage = new ModelImage(ModelStorageBase.FLOAT, aiExtents, "psf" + 0);

        /* Set up the data for the GenerateGaussian class: */
        float[] fGaussData = new float[m_iArrayLength];
        float[] fSigmas = new float[iNumberDimensions];
        int[] iDerivOrder = new int[iNumberDimensions];

        for (int i = 0; i < iNumberDimensions; i++) {
            fSigmas[i] = 3f;
            iDerivOrder[i] = 0;
        }

        /* Generate the Gaussian: */
        GenerateGaussian kGauss = new GenerateGaussian(fGaussData, aiExtents, fSigmas, iDerivOrder);
        kGauss.calc(false);
        kGauss.finalize();
        kGauss = null;

        /* Copy the Gaussian data into the ModelImage: */
        float fMax = Float.MIN_VALUE;
        float fMin = Float.MAX_VALUE;

        for (int i = 0; i < m_iArrayLength; i++) {

            if (fGaussData[i] > fMax) {
                fMax = fGaussData[i];
            }

            if (fGaussData[i] < fMin) {
                fMin = fGaussData[i];
            }
        }

        for (int i = 0; i < m_iArrayLength; i++) {
            m_kPSFImage.set(i, (float) (255 * (fGaussData[i] - fMin) / (fMax - fMin)));
        }

        fGaussData = null;
        fSigmas = null;
        iDerivOrder = null;

        /* Apply constraints to the image and return: */
        constraints(m_kPSFImage);
    }

    /**
     * Does a mirror operation on an image, so that x, y, and z values are mirrored Calls JDialogFlip.
     *
     * @param   kImage  the input image
     *
     * @return  DOCUMENT ME!
     */
    private ModelImage mirror(ModelImage kImage) {

        if (m_kMirrorImage == null) {
            m_kMirrorImage = (ModelImage) (kImage.clone());
        }

        if (kImage.getNDims() == 3) {
            AlgorithmFlip flipz = new AlgorithmFlip(m_kMirrorImage, AlgorithmFlip.Z_AXIS, AlgorithmFlip.IMAGE, true);
            flipz.run();
            flipz.finalize();
            flipz = null;
        }

        AlgorithmFlip flipy = new AlgorithmFlip(m_kMirrorImage, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE, true);
        flipy.run();
        flipy.finalize();
        flipy = null;

        AlgorithmFlip flipx = new AlgorithmFlip(m_kMirrorImage, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
        flipx.run();
        flipx.finalize();
        flipx = null;
        System.gc();

        return m_kMirrorImage;
    }
    
    private ModelImage resample(ModelImage kImage, int[] newExtents) {
    	
    	ModelImage kTransformedImage = (ModelImage) (kImage.clone());
    	int[] extents = kImage.getExtents();
    	float[] res = kImage.getFileInfo(0).getResolutions();
    	m_fNewRes = new float[extents.length];
    	AlgorithmTransform algoTransform = null;
    	
    	for (int i = 0; i < extents.length; i++) {
    		m_fNewRes[i] = (res[i] * (extents[i])) / (newExtents[i]);
    	}
    	
    	if (kImage.getNDims() == 3) {
            algoTransform = new AlgorithmTransform(kImage, new TransMatrix(4), AlgorithmTransform.TRILINEAR,
            														 m_fNewRes[0], m_fNewRes[1], m_fNewRes[2],
            														 newExtents[0], newExtents[1], newExtents[2],
            														 false, true, false);
        } else{
        	algoTransform = new AlgorithmTransform(kImage, new TransMatrix(3), AlgorithmTransform.BILINEAR,
					 m_fNewRes[0], m_fNewRes[1],
					 newExtents[0], newExtents[1],
                     new int[] { kImage.getUnitsOfMeasure(0), kImage.getUnitsOfMeasure(1) },
					 false, true, false);
        	
        }
        	
    	algoTransform.run();
    	kTransformedImage = algoTransform.getTransformedImage();
        algoTransform.finalize();
        
        return kTransformedImage;
        
    	
    }

    /**
     * Runs the deconvolution algorithm.
     *
     * @param   kSource    the original image data
     * @param   kEstimate  the initial estimated guess at the reconstructed image
     * @param   kPSF       the initial point spread function guess
     * @param   bCopy      when true the new estimates after interating are copied back into the data members.
     *
     * @return  , the resulting estimated image
     */
    private ModelImage runDeconvolution(ModelImage kSource, ModelImage kEstimate, ModelImage kPSF, boolean bCopy) {
        ModelImage tempImageConvolve;
        ModelImage tempImageCalc;
        ModelImage originalDividedByEstimateCPSF;
        ModelImage kReturn = null;

        /* Loop over the number of iterations: */
        for (int i = 1; i <= m_iNumberIterations; i++) {

            /* Convolve the current estimate with the current PSF: */
            tempImageConvolve = convolve(kEstimate, kPSF, 1f);

            /* Uncomment to check results of convolution:
             * new ViewJFrameImage((ModelImage)(tempImageConvolve.clone()),null, new Dimension(610,* 200));
             */

            /* Divide the original image by the result and store: */
            if (m_kCalcResult3 == null) {
                m_kCalcResult3 = (ModelImage) (kSource.clone());
            }

            originalDividedByEstimateCPSF = calc(m_kCalcResult3, kSource, tempImageConvolve,
                                                 AlgorithmImageCalculator.DIVIDE);

            /* Create new kEstimate based on original image and psf: */
            /* Convolve the divided image with the psf mirror: */
            ModelImage kPSFMirror = mirror(kPSF);

            tempImageConvolve = convolve(originalDividedByEstimateCPSF, kPSFMirror, 0);

            if (m_kCalcResult2 == null) {
                m_kCalcResult2 = (ModelImage) (tempImageConvolve.clone());
            }

            /* multiply the result by the current estimated image: */
            tempImageCalc = calc(m_kCalcResult2, tempImageConvolve, kEstimate, AlgorithmImageCalculator.MULTIPLY);
            kEstimate = tempImageCalc;
            kEstimate.setImageName("estimated" + i);


            if ((i == 1) && (bCopy == true)) {
                m_kEstimatedImage.disposeLocal();
            }


            /* Create new psf based on original image and psf: */
            /* convolve the divided image with the estimated mirror: */
            ModelImage kEstimateMirror = mirror(kEstimate);

            tempImageConvolve = convolve(originalDividedByEstimateCPSF, kEstimateMirror, 0);

            /* multiply the result by the current psf image: */
            /* Apply constraints to new PSF: */
            tempImageCalc = calc(m_kPSFImage, tempImageConvolve, kPSF, AlgorithmImageCalculator.MULTIPLY);
            kPSF = constraints(tempImageCalc);
            kPSF.setImageName("psf" + i);


            /* Display progression: */
            if (((i % m_iNumberProgress) == 0) && (i != m_iNumberIterations)) {
                new ViewJFrameImage((ModelImage) (kEstimate.clone()), null, new Dimension(610, 200));
            }

            if (m_bUpdatePBar) {
                fireProgressStateChanged((int) (100 * ((float) i / (float) (m_iNumberIterations + 1))));
            }
        }

        if (bCopy) {

            if (!m_kOriginalSourceImage.isColorImage()) {
                m_kEstimatedImage.disposeLocal();
                m_kEstimatedImage = kEstimate;
                m_kPSFImageCopy = (ModelImage) kPSF.clone();
                m_kPSFImage.disposeLocal();
                kPSF.disposeLocal();
            } else {
                m_kEstimatedImage = (ModelImage) kEstimate.clone();
                m_kPSFImageCopy = (ModelImage) kPSF.clone();

            }

        } else {
            kReturn = (ModelImage) (kEstimate.clone());
        }

        return kReturn;
    }
}
