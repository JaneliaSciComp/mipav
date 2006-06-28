package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;


/**
  This program uses a mandatory signal 1 VOI, an optional signal 2 VOI, a mandatory noise background
  VOI, and the number of NMR receivers to calculate the signal to noise ratio for the signal VOI(s).
  The contrast to noise ratio is simply the signal to noise ratio for signal VOI 1 minus the signal to 
  noise ratio for signal VOI 2.  This program requires a single MRI magnitude image.  The program 
  mathematics assumes a Rician probability distribution characteristic of a MRI magnitude image.
  The noise standard deviation equals the 
  square root((sum over i for background VOI of pixel(i) * pixel(i))/
              (2 * number of background pixels * number of NMR receivers))
  For each signal VOI the mean is calculated.
  The the mean is divided by the noise standard deviation
  The signal to noise ratio is calculated from the VOI mean to noise ratio using an equation
  (mean/standard deviation) = 1 * 3 * 5 * ... (2n - 1) * (1/((2**(n-1))*((n - 1)!))) *
                              sqrt(PI/2) * 1F1(-1/2, n, -SNR*SNR/2)
  where n = number of NMR receivers in the phase array
  1F1 is the confluent hypergeometric function
  For the case of 1 NMR receiver the confluent hypergometric function can be replaced by modified
  Bessel functions as follows:
  (mean/standard deviation) = sqrt(PI/2) * exp(-SNR*SNR/4)((1 + (SNR*SNR/2))*I0(SNR*SNR/4) +
                                                            (SNR*SNR/2)*I1(SNR*SNR/4))
                                                            
  References:
  1.) PhD. Thesis Signal and Noise Estimation From Magnetic Resonance Images by Jan Sijbers,
  Universiteit Antwerpen, Department Natuurkunde.
  2.) Signal-to-noise Measurements in Magnitude Images from NMR Phased Arrays by Chris D.
  Constantinides, Ergin Atalar, Elliot R. McVeigh, Magnetic Resonance in Medicine,
  November, 1997, 38(5), pp. 852-857.
  Erratum in Magnetic Resonance in Medicine, July, 2004, 52(1), p. 219.
 */
public class AlgorithmSingleMRIImageSNR extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The index of a rerquired noise background VOI */
    private int backgroundIndex;
  
    /** DOCUMENT ME! */
    private ViewUserInterface UI;
    
    /** The index of a required signal VOI */
    private int signalIndex;
    
    /** The index of a second optional signal VOI.  >= 0 if present */
    private int signal2Index;
    
    /** The number of NMR receivers */
    private int numReceivers;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmSingleMRIImageSNR object.
     *
     * @param  srcImg              source image
     * @param  signalIndex         the index of the signal VOI
     * @param  signal2Index        the index of the signal2 VOI if >= 0
     * @param  backgroundIndex     the index of the background VOI
     * @param  numReceivers        the number of NMR receivers
     */
    public AlgorithmSingleMRIImageSNR(ModelImage srcImg, int signalIndex, int signal2Index,
                                      int backgroundIndex, int numReceivers) {

        super(null, srcImg);
        
        this.signalIndex = signalIndex;
        this.signal2Index = signal2Index;
        this.backgroundIndex = backgroundIndex;
        this.numReceivers = numReceivers;
        UI = srcImage.getUserInterface();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }
    
    /**
     * To create the standard progressBar. Stores in the class-global, progressBar
     */
    private void buildProgressBar() {

        try {

            if (pBarVisible == true) {
                progressBar = new ViewJProgressBar(srcImage.getImageName(), "Calculating MRI SNR ...", 0, 100, true, this,
                                                   this);

                int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
                int yScreen = Toolkit.getDefaultToolkit().getScreenSize().height;
                progressBar.setLocation(xScreen / 2, yScreen / 2);
                progressBar.setVisible(true);
            }
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("SingleMRIImageSNR(" + signalIndex + ", " + signal2Index + ", " + backgroundIndex + ", "
                                   + numReceivers + "\n");
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        
        int xDim, yDim, zDim, tDim, sliceSize;
        int i;
        int imageLength;
        float floatBuffer[];
        short mask[];
        float backgroundVariance = 0.0f;
        double backgroundStdDev;
        int backgroundCount = 0;
        float mean = 0.0f;
        int meanCount = 0;
        double meanDivStdDev;
        float mean2 = 0.0f;
        int mean2Count = 0;
        double mean2DivStdDev;
        double snr = 0.0;
        double snr2;
        double cnr;
        
        DecimalFormat nf;

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        buildProgressBar();

        constructLog();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imageLength  = sliceSize;
        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
            imageLength *= zDim;
        }
        if (srcImage.getNDims() > 3) {
            tDim = srcImage.getExtents()[3];
            imageLength *= tDim;
        }
        
        nf = new DecimalFormat("0.00E0");
        
        floatBuffer = new float[imageLength];
        try {
            srcImage.exportData(0, imageLength, floatBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e +
                                   " on srcImage.export(0, imageLength, floatBuffer)");
            disposeProgressBar();
            setCompleted(false);

            return;
        }
        
        mask = new short[imageLength];
        for (i = 0; i < mask.length; i++) {
            mask[i] = -1;
        }
        
        mask = srcImage.generateVOIMask(mask, signalIndex);
        if (signal2Index >= 0) {
            mask = srcImage.generateVOIMask(mask, signal2Index);
        }
        mask = srcImage.generateVOIMask(mask, backgroundIndex);
        
        for (i = 0; i < imageLength; i++) {

            if (mask[i] == backgroundIndex) {
                backgroundVariance += floatBuffer[i]*floatBuffer[i];
                backgroundCount++;
            }
            
            if (mask[i] == signalIndex) {
                mean += floatBuffer[i];
                meanCount++;
            }
            
            if (signal2Index >= 0) {
                if (mask[i] == signal2Index) {
                    mean2 += floatBuffer[i];
                    mean2Count++;
                }
            } // if (signal2Index >= 0)
        } // for (i = 0; i < imageLength; i++)
        
        backgroundVariance = backgroundVariance/(2.0f * backgroundCount * numReceivers);
        backgroundStdDev = Math.sqrt(backgroundVariance);
        Preferences.debug("Noise standard deviation = " + nf.format(backgroundStdDev) + "\n");
        mean = mean/meanCount;
        Preferences.debug("Mean for signal 1 VOI = " + nf.format(mean) + "\n");
        meanDivStdDev = mean/backgroundStdDev;
        
        if (numReceivers == 1) {
            snr = func(meanDivStdDev, true); 
            Preferences.debug("SNR for signal 1 VOI = " + nf.format(snr) + "\n");
            UI.setDataText("SNR for signal 1 VOI = " + nf.format(snr) + "\n");
        } // if (numReceivers == 1)
        if (signal2Index >= 0) {
            mean2 = mean2/mean2Count;
            Preferences.debug("Mean for signal 2 VOI = " + nf.format(mean2) + "\n");
            mean2DivStdDev = mean2/backgroundStdDev;
            if (numReceivers == 1) {
                snr2 = func(mean2DivStdDev, false);
                Preferences.debug("SNR for signal 2 VOI = " + nf.format(snr2) + "\n");
                UI.setDataText("SNR for signal 2 VOI = " + nf.format(snr2) + "\n");
                cnr = snr - snr2;
                Preferences.debug("Constrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n");
                UI.setDataText("Constrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n");
            }
        } // if (signal2Index >= 0)

        disposeProgressBar();
        setCompleted(true);
        return;
    }
    
    private double func(double meanDivStdDev, boolean signal) {
        double snr;
        double lowerBound;
        double upperBound;
        int i;
        int maxIters = 1000;
        snr = meanDivStdDev/2.0;
        lowerBound = 0.0;
        upperBound = meanDivStdDev;
        double calculatedMeanDivStdDev;
        double square;
        double sqdiv2;
        double constant = Math.sqrt(Math.PI/2.0);
        double[] cyr = new double[1];
        double[] cyi = new double[1];
        int[] nz = new int[1];
        int[] errorFlag = new int[1];
        double i0;
        double i1;
        for (i = 0; i < maxIters; i++) {
            square = snr * snr / 2.0;
            sqdiv2 = square/2.0;
            Bessel i0Bessel =
                new Bessel(Bessel.BESSEL_I,sqdiv2,0.0,0.0,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag);
            i0Bessel.run();
            i0 = cyr[0];
            Bessel i1Bessel =
                new Bessel(Bessel.BESSEL_I,sqdiv2,0.0,1.0,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag);
            i1Bessel.run();
            i1 = cyr[0];
            calculatedMeanDivStdDev = constant * Math.exp(-sqdiv2) *((1.0 + square)*i0 + square*i1);
            if (Math.abs(calculatedMeanDivStdDev - meanDivStdDev)/meanDivStdDev < 0.001) {
                break;
            }
            if (calculatedMeanDivStdDev > meanDivStdDev) {
                upperBound = snr;
                snr = (snr + lowerBound)/2.0;
            }
            else {
                lowerBound = snr;
                snr = (snr + upperBound)/2.0;
            }
        } // for (i = 0; i < maxIters; i++)
        if (signal) {
            if (i == maxIters) {
                Preferences.debug("Failure to converge for signal 1 VOI SNR after " + maxIters + 
                                  " iterations\n");
                UI.setDataText("Failure to converge for signal 1 VOI SNR after " + maxIters + 
                        " iterations\n");
            }
            else {
                Preferences.debug("Signal 1 VOI SNR converged after " + i + " iterations\n");
            }
        }
        else {
            if (i == maxIters) {
                Preferences.debug("Failure to converge for signal 2 VOI SNR after " + maxIters + 
                                  " iterations\n");
                UI.setDataText("Failure to converge for signal 2 VOI SNR after " + maxIters + 
                " iterations\n");
            }
            else {
                Preferences.debug("Signal 2 VOI SNR converged after " + i + " iterations\n");
            }   
        }
        return snr;
        
    }


}
