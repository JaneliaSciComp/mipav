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
                                                            
  For 1F1(-1/2, 1, x) tested with Shanjie Zhang and Jianming Jin Computation of Special
  Functions CHGM routine and ACM Algorithm 707 conhyp routine by Mark Nardin, W. F. Perger,
  and Atul Bhalla the results were:
  From x = -706 to x = +184 the results of the 2 routines matched to at least 1 part in 1E5.
  
  For x = -3055 to x = +184 the ACM routine seemed to give valid results.
  For x <= -3056 the ACM routine results oscillated wildly and at x = -3200 the
  result was frozen at 1.0E75.
  For x >= +185 the ACM result was frozen at 1.0E75.
  The log result was also seen to oscillate for x <= -3056.
  
  For x = -706 to x = +781 the CHGM routine seemed to give valid results.
  For x = -707 to x = -745 the CHGM routine gave infinity and for x <= -746 the
  CHGM routine gave NaN.
  For x >= +783 the CHGM routine gave a result of -infinity.
  
  So for x = -(signal/noise)*(signal/noise)/2, the CHGM routine can handle a maximum
  signal/noise value of 37.5 and in standard output the ACM routine can handle a 
  maximum signal/noise value of 78.166.
  
  For 1F1(-1/2, 2, x) the results were very similar:
  From x = -706 to x = +189 the results of the 2 routines matched to at least 1 part in 1E5.
  
  From x = -3058 to x = +189 the ACM routine seemed to give valid results.
  At x <= -3059 the ACM results oscillated wildly.
  For x > +189 the ACM result was frozen at 1.0E75.
  
  For x = -706 to x = +791 the CHGM routine seemed to give valid results.
  For x = -707 to x = -745 the CHGM routine gave infinity and for x <= -746 the 
  CHGM routine gave NaN.
  For x >= +792 the CHGM routine gave a result of -infinity.
  
  In this module use the ACM routine for x >= -3000.
  For large negative x, use the formula:
  As |x| approaches infinity,
  1F1(a, b, x) approaches (gamma(b)/gamma(b-a))*((-x)**(-a))*[1 + Order(|x|**-1)} for the
                 real part of x < 0.
  More specifically from the Sato reference, the asymptotic behavior of the confluent
  hypergeometric function is:
  1F1(a, b, x) approaches (gamma(a)/gamma(b-a)*((-x)**(-a))*G(a, a-b+1, -x)
  where G is an asymptotic series G(a, b, x) = 1 + (a*b)/(1!*x) + (a*(a+1)*b*(b+1))/(2!*x**2) + ...
                                                            
  References:
  1.) PhD. Thesis Signal and Noise Estimation From Magnetic Resonance Images by Jan Sijbers,
  Universiteit Antwerpen, Department Natuurkunde.
  2.) Signal-to-noise Measurements in Magnitude Images from NMR Phased Arrays by Chris D.
  Constantinides, Ergin Atalar, Elliot R. McVeigh, Magnetic Resonance in Medicine,
  November, 1997, 38(5), pp. 852-857.
  Erratum in Magnetic Resonance in Medicine, July, 2004, 52(1), p. 219.
  3.) I. S. Gradshteyn and I. M. Rhyzik, Tables of integrals, series, and products, 
  Sixth edition.
  4.) Tohru Sato, Liviu F. Chibotaru, and Arnout Ceulemans, The Exe dynamic Jahn-Teller problem:
  A new insight from the strong coupling limit, The Journal of Chemical Physics, Vol. 122, 054104, 
  2005, Appendix C.
  
  Derivation of Bessel function formula when number of receivers == 1
  E[M**v] = (2*sigma**2)**(v/2) * gamma(1 + v/2) * 1F1(-v/2, 1, -A**2/(2*sigma**2))
  E[M] = sigma * sqrt(2) * gamma(3/2) * 1F1(-1/2, 1, -A**2/(2*sigma**2))
  gamma(1/2) = sqrt(PI)
  gamma(n+1) = n*gamma(n)
  gamma(3/2) = sqrt(pi)/2
  E[M] = sigma * sqrt(PI/2) * 1F1(-1/2, 1, -A**2/(2*sigma**2))
  1F1(alpha, gamma, z) = exp(z) * 1F1(gamma - alpha, gamma, -z)
  Let pow = A**2/(2*sigma**2)
  1F1(-1/2, 1, -pow) = exp(-pow) * 1F1(3/2, 1, pow) =
  exp(-pow) * [1/2 * 1F1(3/2, 1, pow) + 1/2 * 1F1(3/2, 1, pow)]
  
  alpha*1F1(alpha+1, gamma, z) = (z + 2*alpha - gamma)*1F1(alpha, gamma, z) 
                               + (gamma - alpha) * 1F1(alpha - 1, gamma, z)
 1F1(-1/2, 1, -pow) = exp(-pow) * [0.5 * 1F1(3/2, 1, pow) + pow * 1F1(1/2, 1, pow)
                                    + 0.5 * 1F1(-1/2, 1, pow)] =
    exp(-pow) * [(1 + pow) * 1F1(1/2, 1, pow) 
                 + 0.5*(1F1(3/2, 1, pow) - 1F1(1/2, 1, pow))
                 - 0.5*(1F1(1/2, 1, pow) - 1F1(-1/2, 1, pow))]
                 
  (z/gamma)*1F1(alpha+1, gamma+1, z) = 1F1(alpha+1, gamma, z) - 1F1(alpha, gamma, z)
  
  1F1(-1/2, 1, -pow) = exp(-pow) * [(1 + pow)* 1F1(1/2, 1, pow) + (pow/2) * 1F1(3/2, 2, pow)
                                                               - (pow/2) * 1F1(1/2, 2, pow)] =
     exp(-pow) * [(1 + pow) * 1F1(1/2, 1, pow) + ((pow/2)**2)*1F1(3/2, 3, pow)] =
     exp(-pow/2) * [(1 + pow) * exp(-pow/2) * 1F1(1/2, 1, pow) + ((pow/2)**2)* exp(-pow/2) * 1F1(3/2, 3, pow)]
     
  Iv(x) = ((2**-v)/gamma(v+1)) * (x**v)  * exp(-x) * 1F1(1/2 + v, 1 + 2*v, 2*x)
  I0(x) = exp(-x) * 1F1(1/2, 1, 2*x)
  I1(x) = (x * exp(-x)/2) * 1F1(3/2, 3, 2*x)
  
  1F1(-1/2, 1, -pow) = exp(-pow/2) * [(1 + pow) * I0(pow/2) + pow * I1(pow/2)]
 */
public class AlgorithmSingleMRIImageSNR extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    /** Confluent Hypergeometric Function of the First Kind. */
    private static final int CONFLUENT_HYPERGEOMETRIC_FIRST_KIND = 1;
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
    
    private DecimalFormat nf;

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
        boolean test = false;
        
        if (test) {
            ConfluentHypergeometric cf;
            int Lnchf = 0;
            int ip = 776;
            double result[] = new double[1];
            double realResult[] = new double[1];
            double imagResult[] = new double[1];
            double x = -30.0;
                for (x = -3100.0; x <= -3000.0; x++) {
                    //cf = new ConfluentHypergeometric(CONFLUENT_HYPERGEOMETRIC_FIRST_KIND,
                                                                 //-0.5, 2.0, x, result);
                    //cf.run();
                    cf = new ConfluentHypergeometric(-0.5, 0.0, 2.0, 0.0, x, 0.0,
                                                       Lnchf, ip, realResult, imagResult);
                    cf.run();
                    Preferences.debug("x = " + x + /*" result[0] = " + result[0] +*/ 
                            " realResult[0] = " + realResult[0] + "\n");
                    //if (Math.abs((result[0] - realResult[0])/result[0]) > 1.0E-5) {
                        //Preferences.debug("Mismatch at x = " + x + "\n");
                    //}
                }
                setCompleted(true);
                return;
        } // if (test)

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
        
        snr = funcC(meanDivStdDev, true);
        Preferences.debug("SNR for signal 1 VOI = " + nf.format(snr) + "\n");
        UI.setDataText("SNR for signal 1 VOI = " + nf.format(snr) + "\n");
        
        if (signal2Index >= 0) {
            mean2 = mean2/mean2Count;
            Preferences.debug("Mean for signal 2 VOI = " + nf.format(mean2) + "\n");
            mean2DivStdDev = mean2/backgroundStdDev;
            snr2 = funcC(mean2DivStdDev, false);
            Preferences.debug("SNR for signal 2 VOI = " + nf.format(snr2) + "\n");
            UI.setDataText("SNR for signal 2 VOI = " + nf.format(snr2) + "\n");
            cnr = snr - snr2;
            Preferences.debug("Constrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n");
            UI.setDataText("Constrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n");
        } // if (signal2Index >= 0)

        disposeProgressBar();
        setCompleted(true);
        return;
    }
    
    private double funcC(double meanDivStdDev, boolean signal) {
        /** For 1F1(-1/2, 1, x) for the ACM code called the result is only
         *  valid for x >= -3055 and for 1F1(-1/2, 2, x) the result is only
         *  valid for x >= -3058.  For large negative x, use the formula:
         *  As |x| approaches infinity,
         *  1F1(a, b, x) approaches (gamma(b)/gamma(b-a))*((-x)**(-a))*[1 + Order(|x|**-1)} for the
         *  real part of x < 0.
         *  More specifically from the Sato reference, the asymptotic behavior of the confluent
         *  hypergeometric function is:
         *  1F1(a, b, x) approaches (gamma(a)/gamma(b-a)*((-x)**(-a))*G(a, a-b+1, -x)
         *  where G is an asymptotic series G(a, b, x) = 1 + (a*b)/(1!*x) + (a*(a+1)*b*(b+1))/(2!*x**2)
         *  + ...
         */
        double snr;
        double lowerBound;
        double upperBound;
        int i;
        int maxIters = 100;
        snr = meanDivStdDev/2.0;
        lowerBound = 0.0;
        upperBound = meanDivStdDev;
        double calculatedMeanDivStdDev;
        double error = 0.0;
        double square;
        double constant = Math.sqrt(Math.PI/2.0);
        //double result[] = new double[1];
        //int kind = CONFLUENT_HYPERGEOMETRIC_FIRST_KIND;
        ConfluentHypergeometric cf;
        int Lnchf = 0; // 0 for standard output; 1 for log of result
        int ip = 776; // Number of desired array positions; 776 is the maximum possible value.
        double realResult[] = new double[1];
        double imagResult[] = new double[1];
        int n;
        Gamma gam;
        double resultB[] = new double[1];
        double resultBMinusA[] = new double[1];
        double gamConstant;
        for (n = 1; n <= numReceivers; n++) {
            constant = constant * (2*n - 1);
        }
        constant = constant/Math.pow(2.0,numReceivers-1);
        for (n = numReceivers - 1; n >= 2; n--) {
            constant = constant/n;
        }
        gam = new Gamma((double)numReceivers, resultB);
        gam.run();
        gam = new Gamma((numReceivers + 0.5), resultBMinusA);
        gam.run();
        gamConstant = resultB[0]/(resultBMinusA[0]*Math.sqrt(2.0));
        for (i = 0; i < maxIters; i++) {
            square = snr * snr / 2.0;
            //cf = new ConfluentHypergeometric(kind, -0.5, numReceivers, -square, result);
            //cf.run();
            if (square <= 3000) {
                cf = new ConfluentHypergeometric(-0.5, 0.0, (double)numReceivers, 0.0, -square, 0.0,
                    Lnchf, ip, realResult, imagResult);
                cf.run();
                //Preferences.debug("realResult cf run = " + realResult[0] + "\n");
            }
            else {
                realResult[0] = gamConstant * snr * (1.0 + (-0.5)*(0.5-numReceivers)/square +
                                (-0.25)*(0.5-numReceivers)*(1.5-numReceivers)/(2.0*square*square));
                //Preferences.debug("realResult snr = " + realResult[0] + "\n");
            }
            calculatedMeanDivStdDev = constant * realResult[0];
            error = Math.abs(calculatedMeanDivStdDev - meanDivStdDev)/meanDivStdDev;
            //Preferences.debug("error = " + error + "\n");
            if (error < 0.001) {
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
            Preferences.debug("Error for signal 1 VOI SNR after " + i + 
                                  " iterations  = " + nf.format(100 * error) + "%\n");
        }
        else {
            Preferences.debug("Error for signal 2 VOI SNR after " + i + 
                    " iterations  = " + nf.format(100 * error) + "%\n");
        }
        return snr;
        
    }
    
    /** This func is commented out because it only works for the case when the 
     *  number of receivers equals one.
     */
    /**private double func(double meanDivStdDev, boolean signal) {
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
        
    }*/


}
