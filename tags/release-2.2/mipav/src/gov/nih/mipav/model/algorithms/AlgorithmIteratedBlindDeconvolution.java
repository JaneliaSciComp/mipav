package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmFFT;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import java.lang.Object;
import java.io.IOException;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.structures.ModelStorageBase;

public class AlgorithmIteratedBlindDeconvolution extends AlgorithmBase {

    private ModelImage inImage;
    private ModelImage psfImage;
    private ModelImage outImage;

    float[] blurredReals;
    float[] blurredSpectrumReals;
    float[] blurredSpectrumImags;

    float[] estimatedReals;
    float[] estimatedSpectrumReals;
    float[] estimatedSpectrumImags;

    float[] psfReals;
    float[] psfSpectrumReals;
    float[] psfSpectrumImags;

    float[] tmpArray;

    int arrayLength;

    ModelImage blurredImageSpectrum;
    ModelImage psfImageSpectrum;

    ModelImage estimatedImage;
    ModelImage estimatedImageSpectrum;

    AlgorithmFFT estimatedFFT, estimatedIFFT, psfIFFT, psfFFT;

    public AlgorithmIteratedBlindDeconvolution(ModelImage destImg, ModelImage srcImg, ModelImage psfImg) {
        inImage  = srcImg;
        psfImage = psfImg;
        outImage = destImg;

        // get the dimensions and length of the arrays
        int numDims = outImage.getNDims();
        int[] exts  = outImage.getExtents();
        arrayLength = 1;
        for (int i = 0; i < numDims; i++) {
            arrayLength *= exts[i];
        }

        // allocate the arrays
        try {
            blurredReals         = new float[arrayLength];
            blurredSpectrumReals = new float[arrayLength];
            blurredSpectrumImags = new float[arrayLength];

            estimatedReals = new float[arrayLength];
            estimatedSpectrumReals = new float[arrayLength];
            estimatedSpectrumImags = new float[arrayLength];

            psfReals = new float[arrayLength];
            psfSpectrumReals = new float[arrayLength];
            psfSpectrumImags = new float[arrayLength];

            tmpArray = new float[arrayLength];
        }
        catch (OutOfMemoryError e) {
            blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
            estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
            psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
            System.gc();
            displayError("AlgorithmIteratedBlindDeconvolution: Out of memory creating cBlurArray");
            setCompleted(false);
            return;
        }

        // make images for the arrays
        blurredImageSpectrum =
            new ModelImage(ModelStorageBase.COMPLEX, inImage.getExtents(), null, null );

        // FFT parameters
        int forwardTransformDir = 1;
        int inverseTransformDir = -1;
        boolean logMagDisplay   = true;
        boolean unequalDim      = false;
        boolean image25D        = false;
        boolean imageCrop       = true;
        int kernelDiameter      = 15;
        int filterType          = 2;
        float freq1             = 0.4f;
        float freq2             = 0.7f;
        int constructionMethod  = 1;
        int butterworthOrder    = 0;


        // take the FFT of the inImage
        AlgorithmFFT blurredFFT =
            new AlgorithmFFT(blurredImageSpectrum, inImage, forwardTransformDir, logMagDisplay,
                             unequalDim, image25D, imageCrop, kernelDiameter,
                             filterType, freq1, freq2, constructionMethod,
                             butterworthOrder);
        blurredFFT.setProgressBarVisible(false);
        blurredFFT.run();

        // fill the blurred arrays with values from the blurredImage
        try {
            blurredImageSpectrum.exportComplexData(0, arrayLength,
                                                   blurredSpectrumReals,
                                                   blurredSpectrumImags);
        }
        catch (IOException error) {
            displayError("AlgorithmIteratedBlindDeconvolution: Source image is locked");
            setCompleted(false);
            return;
        }
        catch (OutOfMemoryError e) {
            blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
            estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
            psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
            System.gc();
            displayError("AlgorithmIteratedBlindDeconvolution: Out of memory");
            setCompleted(false);
            return;
        }


        // make an image containing random values corresponding to the restored image
        RandomNumberGen rndNum = new RandomNumberGen();
        for (int i = 0; i < arrayLength; i++) {
            estimatedReals[i] = rndNum.genUniformRandomNum(0.00001f, 1.0f);
        }

        estimatedImage =
            new ModelImage(inImage.getType(), inImage.getExtents(), null, null );

        try {
            estimatedImage.importData(0, estimatedReals, true);
        }
        catch (IOException error) {
            displayError("AlgorithmIteratedBlindDeconvolution: IOException on fOldImage image import data");
            progressBar.dispose();
            setCompleted(false);
            return;
        }
        catch (OutOfMemoryError e) {
            blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
            estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
            psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
            System.gc();
            displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on fOldImage image import data");
            progressBar.dispose();
            setCompleted(false);
            return;
        }


        estimatedImageSpectrum =
            new ModelImage(ModelStorageBase.COMPLEX, inImage.getExtents(), null, null);

        // take the FFT of the estimatedImage
        estimatedFFT =
            new AlgorithmFFT(estimatedImageSpectrum, estimatedImage, forwardTransformDir, logMagDisplay,
                             unequalDim, image25D, imageCrop, kernelDiameter,
                             filterType, freq1, freq2, constructionMethod,
                             butterworthOrder);

        estimatedIFFT =
            new AlgorithmFFT(estimatedImage, estimatedImageSpectrum, inverseTransformDir, logMagDisplay,
                             unequalDim, image25D, imageCrop, kernelDiameter,
                             filterType, freq1, freq2, constructionMethod,
                             butterworthOrder);

        psfImageSpectrum =
            new ModelImage(ModelStorageBase.COMPLEX, inImage.getExtents(), null, null);
        psfImageSpectrum.setOriginalExtents(inImage.getExtents());


        psfIFFT =
            new AlgorithmFFT(psfImage, psfImageSpectrum, inverseTransformDir, logMagDisplay,
                            unequalDim, image25D, imageCrop, kernelDiameter,
                            filterType, freq1, freq2, constructionMethod,
                            butterworthOrder);

       psfFFT =
           new AlgorithmFFT(psfImageSpectrum, psfImage, forwardTransformDir, logMagDisplay,
                           unequalDim, image25D, imageCrop, kernelDiameter,
                           filterType, freq1, freq2, constructionMethod,
                           butterworthOrder);
   }


    /**
     *   Starts the program
     */
    public void runAlgorithm() {

        if (inImage == null) {
            MipavUtil.displayError(
                "AlgorithmIteratedBlindDeconvolution  Source Image is null");
            return;
        }

        run2D();

        if (threadStopped) {
            finalize();
            return;
        }
        setCompleted(true);

    } // end runAlgorithm



    public void run2D() {

        float a, b, c, d, denom, blurredMagVal, psfMagVal;
        Complex blurredVal    = new Complex();
        Complex estimatedVal  = new Complex();
        Complex psfVal        = new Complex();
        Complex tmpComplexVal = new Complex();
        Complex tmpComplexVal2 = new Complex();
        Complex tmpComplexVal3 = new Complex();
        Complex tmpComplexVal4 = new Complex();
        Complex tmpComplexVal5 = new Complex();
        float psfRealVal, psfImagVal;
        float blurredRealVal, blurredImagVal;
        float estimatedRealVal, estimatedImagVal;
        float beta = 0.9f;
        Complex BETA = new Complex(beta);
        Complex OneMinusBETA = new Complex(1.0f - beta);
        Complex ONE = new Complex(1.0f);
        int iteration = 0;
        while (iteration < 200) {

            // execute the FFT on the estimated image
            estimatedFFT.setProgressBarVisible(false);
            estimatedFFT.run();

            // fill the estimated arrays with values from the estimatedImageSpectrum
            try {
                estimatedImageSpectrum.exportComplexData(0, arrayLength,
                                                         estimatedSpectrumReals,
                                                         estimatedSpectrumImags);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: Source image is locked");
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory");
                setCompleted(false);
                return;
            }

            // estimate the psf (H = blurredImageSpectrum / estimatedImageSpectrum)
            for (int i = 0; i < arrayLength; i++) {
                blurredVal.setReal(blurredSpectrumReals[i]);
                blurredVal.setImag(blurredSpectrumImags[i]);
                estimatedVal.setReal(estimatedSpectrumReals[i]);
                estimatedVal.setImag(estimatedSpectrumImags[i]);

                tmpComplexVal = blurredVal.divide(estimatedVal);

                psfSpectrumReals[i] = tmpComplexVal.getReal();
                psfSpectrumImags[i] = tmpComplexVal.getImag();
            } // end for(int i = 0; ...)


            // inverse transform of H (h)
            try {
                psfImageSpectrum.importComplexData(0, psfSpectrumReals, psfSpectrumImags, true, true);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: IOException on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            psfImageSpectrum.calcMinMax();
            psfImageSpectrum.setOriginalMinimum((float)psfImageSpectrum.getMin());
            psfImageSpectrum.setOriginalMaximum((float)psfImageSpectrum.getMax());
            psfIFFT.setProgressBarVisible(false);
            psfIFFT.run();

            // fill the psf arrays with values from the psfImage
            try {
                psfImage.exportData(0, arrayLength, psfReals);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: psfImage image is locked");
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory");
                setCompleted(false);
                return;
            }



            // add non-negativity constraint for the psf
            for (int i = 0; i < arrayLength; i++) {
                if (psfReals[i] < 0) psfReals[i] = 0.0f;
            }

            // FFT the impulse response h to get H
            try {
                psfImage.importData(0, psfReals, true);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: IOException on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            psfImage.calcMinMax();
            psfImage.setOriginalMinimum((float)psfImage.getMin());
            psfImage.setOriginalMaximum((float)psfImage.getMax());
            psfFFT.setProgressBarVisible(false);
            psfFFT.run();

            try {
                psfImageSpectrum.exportComplexData(0, arrayLength, psfSpectrumReals, psfSpectrumImags);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: IOException on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }


            // form a new estimated image spectrum from the blurred image spectrum and H
            for (int i = 0; i < arrayLength; i++) {
                blurredVal.setReal(blurredSpectrumReals[i]);
                blurredVal.setImag(blurredSpectrumImags[i]);
                blurredMagVal = blurredVal.magnitude();

                psfVal.setReal(psfSpectrumReals[i]);
                psfVal.setImag(psfSpectrumImags[i]);
                psfMagVal = psfVal.magnitude();

                estimatedVal.setReal(estimatedSpectrumReals[i]);
                estimatedVal.setImag(estimatedSpectrumImags[i]);

                if (psfMagVal >= blurredMagVal) {
                    tmpComplexVal  = OneMinusBETA.multiply(estimatedVal);
                    tmpComplexVal2 = blurredVal.divide(psfVal);
                    tmpComplexVal3 = BETA.multiply(tmpComplexVal2);
                    tmpComplexVal4 = tmpComplexVal.add(tmpComplexVal3);

                    estimatedSpectrumReals[i] = tmpComplexVal4.getReal();
                    estimatedSpectrumImags[i] = tmpComplexVal4.getImag();

                } else {
                    tmpComplexVal = OneMinusBETA.divide(estimatedVal);
                    tmpComplexVal2 = psfVal.divide(blurredVal);
                    tmpComplexVal3 = BETA.multiply(tmpComplexVal2);
                    tmpComplexVal4 = tmpComplexVal.add(tmpComplexVal3);
                    tmpComplexVal5 = ONE.divide(tmpComplexVal4);

                    estimatedSpectrumReals[i] = tmpComplexVal5.getReal();
                    estimatedSpectrumImags[i] = tmpComplexVal5.getImag();

                }
            } // end for (int i = 0; ...)

            try {
                estimatedImageSpectrum.importComplexData(0, estimatedSpectrumReals,
                        estimatedSpectrumImags, true, true);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: IOException on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            estimatedImageSpectrum.calcMinMax();
            estimatedImageSpectrum.setOriginalMinimum((float)estimatedImageSpectrum.getMin());
            estimatedImageSpectrum.setOriginalMaximum((float)estimatedImageSpectrum.getMax());

            estimatedIFFT.setProgressBarVisible(false);
            estimatedIFFT.run();

            // fill the estimated arrays with values from the estimatedImageSpectrum
            try {
                estimatedImage.exportData(0, arrayLength, estimatedReals);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: Source image is locked");
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory");
                setCompleted(false);
                return;
            }

            // add non-negativity constraint for the estimated image
            for (int i = 0; i < arrayLength; i++) {
                if (estimatedReals[i] < 0) estimatedReals[i] = 0.0f;
            }

            try {
                estimatedImage.importData(0, estimatedReals, true);
            }
            catch (IOException error) {
                displayError("AlgorithmIteratedBlindDeconvolution: IOException on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            catch (OutOfMemoryError e) {
                blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
                estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
                psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
                System.gc();
                displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on destination image import data");
                progressBar.dispose();
                setCompleted(false);
                return;
            }
            estimatedImage.calcMinMax();
            estimatedImage.setOriginalMinimum((float)estimatedImage.getMin());
            estimatedImage.setOriginalMaximum((float)estimatedImage.getMax());

            iteration++;
        } // end while(...)

        // view the PSF image
        new ViewJFrameImage(psfImage);

        for(int i = 0; i < arrayLength; i++) {
            tmpArray[i] = estimatedReals[i];
        }
        try {
            outImage.importData(0, tmpArray, true);
        }
        catch (IOException error) {
            displayError("AlgorithmIteratedBlindDeconvolution: IOException on destination image import data");
            progressBar.dispose();
            setCompleted(false);
            return;
        }
        catch (OutOfMemoryError e) {
            blurredReals = blurredSpectrumReals = blurredSpectrumImags = null;
            estimatedReals = estimatedSpectrumReals = estimatedSpectrumImags = null;
            psfReals = psfSpectrumReals = psfSpectrumImags = tmpArray = null;
            System.gc();
            displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on destination image import data");
            progressBar.dispose();
            setCompleted(false);
            return;
        }

     } // end run2D()



    /**
     *    Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }



    /**
     *	Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {
        inImage  = null;
        psfImage = null;
        outImage = null;
        System.gc();
    }



    public void runIt() {
        ModelImage cBlurImage =
            new ModelImage(inImage.getType(), inImage.getExtents(), null, null );


        // make an array containing the input data
        int numDims = outImage.getNDims();
        int[] exts = outImage.getExtents();
        int arrayLength = 1;
        for (int i = 0; i < numDims; i++) {
            arrayLength *= exts[i];
        }
        double[] realData;
        double[] imagData;
        double[] magData;
        try {
            realData = new double[arrayLength];
            imagData = new double[arrayLength];
            magData  = new double[arrayLength];
        }
        catch (OutOfMemoryError e) {
            realData = imagData = null;
            System.gc();
            displayError("AlgorithmIteratedBlindDeconvolution: Out of memory creating inData");
            setCompleted(false);
            return;
        }
        try {
            inImage.exportData(0, arrayLength, realData); // locks and releases and lock
            // fill it with zeros.
            for (int i = 0; i < arrayLength; i++) {
                imagData[i] = 0.0f;
            }
        }
        catch (IOException error) {
            displayError("AlgorithmIteratedBlindDeconvolution: Source image is locked");
            setCompleted(false);
            return;
        }
        catch (OutOfMemoryError e) {
            realData = imagData = null;
            System.gc();
            displayError("AlgorithmIteratedBlindDeconvolution: Out of memory");
            setCompleted(false);
            return;
        }

        FFTUtility fftUtil;
        fftUtil = new FFTUtility(realData, imagData, 1, exts[0], 1, -1, FFTUtility.FFT);
        fftUtil.setProgressBarVisible(false);
        fftUtil.run();
        fftUtil.finalize();

        for (int i = 0; i < arrayLength; i++) {
            magData[i] = Math.sqrt(realData[i]*realData[i] + imagData[i]*imagData[i]);
        }
        try {
            outImage.importData(0, magData, true);
        }
        catch (IOException error) {
            displayError("AlgorithmIteratedBlindDeconvolution: IOException on fOldImage image import data");
            progressBar.dispose();
            setCompleted(false);
            return;
        }
        catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmIteratedBlindDeconvolution: Out of memory on fOldImage image import data");
            progressBar.dispose();
            setCompleted(false);
            return;
        }

    } // runIt


}
