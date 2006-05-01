package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.awt.*;

/**
 * An implementation of Maximum Likelihood Iterated Blind Deconvolution based
 * on the following papers:
 *
 * Holmes, T.J., Maximum Likelihood Image Restoration Adapted for Noncoherent
 * Optical Imaging, JOSA-A, 5(5): 666-673, 1988.
 *
 * Holmes, T., Bhattacharyya, S., Cooper, J., Hanzel, D., Krishnamurthi, V.,
 * Lin, W., Roysam, B., Szarowski, D., Turner, J., Light Microscopic Images
 * Reconstructed by Maximum Likelihood Deconvolution, Ch. 24, Handbook of
 * Biological Confocal Microscopy, J. Pawley, Plenum, 1995.
 *
 * Holmes, T., Liu, Y., Image Restoration for 2D and 3D Fluorescence
 * Microscopy, Visualization in Biomedical Microscopies, A. Kriete, VCH, 1992.
 *
 * Holmes, T., Blind Deconvolution of Quantum-Limited Noncoherent Imagery,
 * JOSA-A, 9: 1052 - 1061, 1992.
 *
 */
public class AlgorithmMaximumLikelihoodIteratedBlindDeconvolution extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Number of iterations for the deconvolution: */
    private int m_iNumberIterations;

    /** Show the deconvolved image in progress every m_iNumberProgress steps: */
    private int m_iNumberProgress;

    /** Physically-based parameters: */
    /** numerical aperature of the imaging lense: */
    private float m_fObjectiveNumericalAperature;
    /** wavelength of the reflected or fluorescencing light: */
    private float m_fWavelength;
    /** sample index of refraction: */
    private float m_fRefractiveIndex;

    /** source image to be reconstructed! */
    private ModelImage m_kSourceImage;

    /** point spread function image, and mirror of psf: */
    private ModelImage m_kPSFImage;
    private ModelImage m_kPSFMirrorImage;

    /** estimated image, and mirror */
    private ModelImage m_kEstimatedImage;
    private ModelImage m_kEstimatedMirrorImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMaximumLikelihoodIteratedBlindDeconvolution object.
     *
     * @param  kSrcImg, the input image to be reconstructed
     * @param iIterations, the number of times to iterate in the deconvolution
     * @param fObjectiveNumericalAperature, the numerical aperature of the imaging lense
     * @param fWavelength, the reflected or fluorescening light
     * @param fRefractiveIndex, the index of refraction for the sample
     */
    public AlgorithmMaximumLikelihoodIteratedBlindDeconvolution( ModelImage kSrcImg,
                                                                 int iIterations,
                                                                 int iProgress,
                                                                 float fObjectiveNumericalAperature,
                                                                 float fWavelength,
                                                                 float fRefractiveIndex
                                                                 )
    {
        m_kSourceImage = kSrcImg;
        m_iNumberIterations =  iIterations;
        m_iNumberProgress = iProgress;
        m_fObjectiveNumericalAperature = fObjectiveNumericalAperature;
        m_fWavelength = fWavelength;
        m_fRefractiveIndex = fRefractiveIndex;
        
        /* The initial guess at the estimated image is the original image: */
        m_kEstimatedImage = (ModelImage)(m_kSourceImage.clone());
        m_kEstimatedImage.setImageName( "estimate" + 0 );

        /* The initial PSF is set to a uniform gray with a constant pixel 
         * value = 1/number of voxels */
        m_kPSFImage = new ModelImage(m_kSourceImage.getType(), m_kSourceImage.getExtents(), "psf" + 0);

        // get the dimensions and length of the arrays
        int numDims = m_kSourceImage.getNDims();
        int[] exts = m_kSourceImage.getExtents();
        int arrayLength = 1;
        for (int i = 0; i < numDims; i++) {
            arrayLength *= exts[i];
        }
        if ( m_kSourceImage.isColorImage() )
        {
            arrayLength *= 4;
        }
        for ( int i = 0; i < arrayLength; i++ )
        {
            m_kPSFImage.set(i, 1.0f/arrayLength );
        }

        /* Create the mirror images: */
        m_kEstimatedMirrorImage = mirror( m_kEstimatedImage );
        m_kPSFMirrorImage = mirror( m_kPSFImage );
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {
        m_kSourceImage = null;
        m_kPSFImage = null;
        m_kPSFMirrorImage = null;
        m_kEstimatedImage = null;
        m_kEstimatedMirrorImage = null;
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
     * Runs the deconvolution algorithm.
     */
    private void runDeconvolution()
    {
        ModelImage tempImage;
        ModelImage originalDividedByEstimateCPSF;
        for ( int i = 1; i <= m_iNumberIterations; i++ )
        {
            /* Convolve the current estimate with the current PSF: */
            tempImage = convolve( m_kEstimatedImage, m_kPSFImage, 
                                  (i%m_iNumberProgress) == 0 );
            /* Divide the original image by the result and store: */
            originalDividedByEstimateCPSF = calc( m_kSourceImage, tempImage,
                                                  AlgorithmImageCalculator.DIVIDE );

            /* Create new m_kEstimatedImage based on original image and psf: */
            /* Convolve the divided image with the psf mirror: */
            tempImage = convolve( originalDividedByEstimateCPSF, m_kPSFMirrorImage, false );
            /* multiply the result by the current estimated image: */
            m_kEstimatedImage = calc( m_kEstimatedImage, tempImage,
                                      AlgorithmImageCalculator.MULTIPLY );
            m_kEstimatedImage.setImageName( "estimated" + i );

            /* Create new psf based on original image and psf: */
            /* convolve the divided image with the estimated mirror: */
            tempImage =
                convolve( originalDividedByEstimateCPSF, m_kEstimatedMirrorImage, false );
            /* multiply the result by the current psf image: */
            m_kPSFImage = calc( m_kPSFImage, tempImage,
                                AlgorithmImageCalculator.MULTIPLY );
            /* Apply constraints to new PSF: */
            constraints( );
            m_kPSFImage.setImageName( "psf" + i );

            m_kEstimatedMirrorImage = null;
            m_kEstimatedMirrorImage = mirror( m_kEstimatedImage );
            m_kPSFMirrorImage.setImageName( "estimatedmirror" + i );
            m_kPSFMirrorImage = null;
            m_kPSFMirrorImage = mirror( m_kPSFImage );
            m_kPSFMirrorImage.setImageName( "psfmirror" + i );

            tempImage = null;
            originalDividedByEstimateCPSF = null;
            System.gc();
        }
    } // end run()


    /**
     * Convolves two image by computing the fast fourier transforms of each
     * image, multiplying the ffts and then computing the inverse fft of the
     * result:
     * @param kImage1, the first input image
     * @param kImage2, the second input image
     * @param bDisplay, when true, the result of the convolution is displayed
     * in a separate window
     * @return kResult, the result of convolving the two images
     */
    private ModelImage convolve( ModelImage kImage1, ModelImage kImage2,
                                 boolean bDisplay )
    {
        /* Calculate the fft of the two input images: */
        ModelImage kImageSpectrum1 = fft( kImage1 );
        ModelImage kImageSpectrum2 = fft( kImage2 );

        /* multiply the resulting spectrums: */
        ModelImage kConvolve =
            calc( kImageSpectrum1, kImageSpectrum2, AlgorithmImageCalculator.MULTIPLY );

        /* inverse FFT the result: */
        ModelImage kResult = inverse_fft( kConvolve, kImage1 );

        if ( bDisplay )
        {
            //            new ViewJFrameImage((ModelImage)(kImageSpectrum1.clone()), null, new Dimension(610, 200));
//             new ViewJFrameImage((ModelImage)(kImage2.clone()), null, new Dimension(610, 200));
//             new ViewJFrameImage((ModelImage)(kImageSpectrum2.clone()), null, new Dimension(610, 200));
            new ViewJFrameImage((ModelImage)(kResult.clone()), null, new Dimension(610, 200));
        }

        kImageSpectrum1 = null;
        kImageSpectrum2 = null;
        kConvolve = null;
        System.gc();
        return kResult;
    }

    /**
     * Computes the fft of the input image, returns the fft. Calls AlgorithmFFT.
     * @param kImage, the input image
     * @return kResult, the fft of kImage
     */
    private ModelImage fft( ModelImage kImage )
    {
        ModelImage kImageSpectrum =
            new ModelImage(ModelStorageBase.COMPLEX, kImage.getExtents(),
                           null, null);

        // FFT parameters
        int forwardTransformDir = 1;
        int inverseTransformDir = -1;
        boolean logMagDisplay = true;
        boolean unequalDim = false;
        boolean image25D = false;
        boolean imageCrop = true;
        int kernelDiameter = 15;
        int filterType = 2;
        float freq1 = 0.0f;
        float freq2 = 1.0f;
        int constructionMethod = 1;
        int butterworthOrder = 0;

        // take the FFT of the first input image: 
        AlgorithmFFT kFFT = new AlgorithmFFT( kImageSpectrum, kImage,
                                              forwardTransformDir, logMagDisplay,
                                              unequalDim, image25D, imageCrop,
                                              kernelDiameter, filterType, freq1, freq2,
                                              constructionMethod, butterworthOrder);
        kFFT.setActiveImage(false);
        kFFT.setProgressBarVisible( false );
        kFFT.run();
        kFFT.finalize();
        kFFT = null;
        System.gc();
        return kImageSpectrum;
    }

    /**
     * Computes the inverse fft of the input image kImage, returns the
     * fft. Calls AlgorithmFFT.
     * @param kImage, the input image
     * @param kResultInput, the image template for the result image 
     * @return kResult, the fft of kImage
     */
    private ModelImage inverse_fft( ModelImage kImage, ModelImage kResultInput )
    {
        ModelImage kResult = (ModelImage)(kResultInput.clone());

        // FFT parameters
        int forwardTransformDir = 1;
        int inverseTransformDir = -1;
        boolean logMagDisplay = true;
        boolean unequalDim = false;
        boolean image25D = false;
        boolean imageCrop = true;
        int kernelDiameter = 15;
        int filterType = 2;
        float freq1 = 0.0f;
        float freq2 = 1.0f;
        int constructionMethod = 1;
        int butterworthOrder = 0;

        // take the FFT of the first input image: 
        AlgorithmFFT kIFFT = new AlgorithmFFT(kResult, kImage,
                                              inverseTransformDir, logMagDisplay,
                                              unequalDim, image25D, imageCrop,
                                              kernelDiameter, filterType, freq1, freq2,
                                              constructionMethod, butterworthOrder);
        kIFFT.setActiveImage(false);
        kIFFT.setProgressBarVisible( false );
        kIFFT.run();
        kIFFT.finalize();
        kIFFT = null;
        System.gc();
        return kResult;

    }

    /**
     * Performs a AlgorithmImageCalculator calculation on the two input
     * images. Used here to either multiply or divide two images. 
     * @param kImage1, the first input image
     * @param kImage2, the second input image
     * @param iType, the type of calculation (multiply or divide)
     * @return kReturn, the result of the image calculation
     */
    private ModelImage calc( ModelImage kImage1, ModelImage kImage2, int iType )
    {
        ModelImage kReturn = new ModelImage(kImage1.getType(),
                                            kImage1.getExtents(), "" );
        AlgorithmImageCalculator algImageCalc =
            new AlgorithmImageCalculator( kReturn, kImage1, kImage2,
                                          iType,
                                          AlgorithmImageMath.PROMOTE, true,
                                          null);

        algImageCalc.setActiveImage(false);
        algImageCalc.setProgressBarVisible( false );
        algImageCalc.run();

        kReturn.setFileInfo(kImage1.getFileInfo());
        algImageCalc.finalize();
        algImageCalc = null;
        System.gc();
        return kReturn;
    }

    /** Applies constraints to the input image kImage and writes the
     * constrained values into the m_kPSFImage.
     * Constraints are the following:
     * 1). Unit summation constraint
     * 2). Hourglass constraint
     * 3). Bandlimit and missing cone constraint
     * 4). Nonnegativity constraint
     *
     * @param kImage, the unconstrained PSFImage
     * MEMBER MODIFIED: m_kPSFImage is modified by this funcion.
     */
    private void constraints(  )
    {
        int iDimX = m_kPSFImage.getExtents()[0];
        int iDimY = m_kPSFImage.getExtents()[1];
        int iDimZ = 1;
        if ( m_kPSFImage.getNDims() > 2 )
        {
            iDimZ = m_kPSFImage.getExtents()[2];
        }
        int arrayLength = iDimX * iDimY * iDimZ;
        if ( m_kPSFImage.isColorImage() )
        {
            arrayLength *= 4;
        }

        /* 1). Unit summation constraint: */
        float fSum = 0;
        for ( int i = 0; i < arrayLength; i++ )
        {
            fSum += m_kPSFImage.getFloat( i );
        }
        for ( int i = 0; i < arrayLength; i++ )
        {
            m_kPSFImage.set( i, m_kPSFImage.getFloat( i ) / fSum );
        }

        /* 2). Hourglass constraint: */
        float fRadius = (3 * 0.61f * m_fWavelength) / m_fObjectiveNumericalAperature;
        float fTheta = (float)Math.asin( m_fObjectiveNumericalAperature / m_fRefractiveIndex );
        float fRadiusZ = 0.0f;
        for ( int z = 1; z <= iDimZ; z++ )
        {
            for ( int x = 1; x <= iDimX; x++ )
            {
                for ( int y = 1; y <= iDimY; y++ )
                {
                    fRadiusZ = (float)(Math.abs( z - (iDimZ+1)/2 ) * Math.tan( fTheta ));
                    if ( ( ((x - (iDimX+1)/2) * (x - (iDimX+1)/2)) + 
                           ((y - (iDimY+1)/2) * (y - (iDimY+1)/2))   ) > 
                         ( ( fRadius + fRadiusZ ) * ( fRadius + fRadiusZ ) ) )
                    {
                        if ( m_kPSFImage.isColorImage() )
                        {
                            for ( int c = 0; c < 4; c++ )
                            {
                                m_kPSFImage.set( (z - 1) * (iDimY * iDimX * 4) + 
                                            (y - 1) * iDimX * 4 +
                                            (x - 1) * 4 + 
                                            c,
                                            0f );
                            }
                        }
                        else
                        {
                            m_kPSFImage.set( (z - 1) * (iDimY * iDimX ) + 
                                        (y - 1) * iDimX +
                                        (x - 1),
                                        0f );
                        }
                    }
                }
            }
        }

        /* 3). Bandlimit and missing cone constraint: */
        float fRadialBandLimit = (float)( 2.0f * m_fObjectiveNumericalAperature / m_fWavelength );
        float fAxialBandLimit = (float)(( m_fObjectiveNumericalAperature *
                                          m_fObjectiveNumericalAperature )  /
                                        (2.0 * m_fRefractiveIndex * m_fWavelength ));
        ModelImage kImageFFT = fft( m_kPSFImage );
        for ( int z = 1; z <= iDimZ; z++ )
        {
            for ( int x = 1; x <= iDimX; x++ )
            {
                for ( int y = 1; y <= iDimY; y++ )
                {
                    fRadiusZ = (float)(Math.abs( z - (iDimZ+1)/2 ) * Math.tan( fTheta ));
                    if ( ( ( ((x - (iDimX+1)/2) * (x - (iDimX+1)/2)) + 
                             ((y - (iDimY+1)/2) * (y - (iDimY+1)/2))   ) > 
                           fRadialBandLimit ) ||
                         ( (float)(Math.abs( z - (iDimZ+1)/2 )) > fAxialBandLimit ) ||
                         ( ( ((x - (iDimX+1)/2) * (x - (iDimX+1)/2)) + 
                                ((y - (iDimY+1)/2) * (y - (iDimY+1)/2))   ) <  
                              fRadius )
                         )
                    {
                        if ( kImageFFT.isColorImage() )
                        {
                            for ( int c = 0; c < 4; c++ )
                            {
                                kImageFFT.set( (z - 1) * (iDimY * iDimX * 4) + 
                                               (y - 1) * iDimX * 4 +
                                               (x - 1) * 4 + 
                                               c,
                                               0f );
                            }
                        }
                        else
                        {
                            kImageFFT.set( (z - 1) * (iDimY * iDimX ) + 
                                           (y - 1) * iDimX +
                                           (x - 1),
                                            0f );
                        }
                    }
                }
            }
        }
        /* 4). Nonnegativity constraint: */
        m_kPSFImage = inverse_fft( kImageFFT, m_kPSFImage );
        for ( int i = 0; i < arrayLength; i++ )
        {
            if ( m_kPSFImage.getFloat(i) < 0 )
            {
                m_kPSFImage.set( i, 0.0000000001f );
            }
        }
    }

    /**
     * Does a mirror operation on an image, so that x, y, and z values are mirrored
     * Calls JDialogFlip
     * @param kImage, the input image
     * @param kReturn, the mirror of kImage
     */
    private ModelImage mirror( ModelImage kImage )
    {
        ModelImage kReturn = (ModelImage)(kImage.clone());
        JDialogFlip flipy =
            new JDialogFlip( ViewUserInterface.getReference(),
                             kReturn, AlgorithmFlip.Y_AXIS);
        flipy.setActiveImage(false);
        flipy.setSeparateThread( false );
        flipy.callAlgorithm();
        flipy = null;
        JDialogFlip flipx =
            new JDialogFlip( ViewUserInterface.getReference(),
                             kReturn, AlgorithmFlip.X_AXIS);
        flipx.setActiveImage(false);
        flipx.setSeparateThread( false );
        flipx.callAlgorithm();
        flipx = null;
        System.gc();
        return kReturn;
    }



    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (m_kSourceImage == null) {
            MipavUtil.displayError("AlgorithmMaximumLikelihoodIteratedBlindDeconvolution  Source Image is null");

            return;
        }

        runDeconvolution();

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);

    } // end runAlgorithm

    /**
     * Returns the reconstructed image:
     * @return m_kEstimatedImage
     */
    public ModelImage getReconstructedImage()
    {
        return m_kEstimatedImage;
    }
    /** 
     * Returns the reconstructed PSF image:
     * @return m_kPSFImage
     */
    public ModelImage getPSFImage()
    {
        return m_kPSFImage;
    }
}
