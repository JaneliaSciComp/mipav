package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;

import java.io.IOException;
import java.text.DecimalFormat;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfOneVariable;
import de.jtem.numericalMethods.calculus.minimizing.Brent;


/**
 * This program uses a mandatory signal 1 VOI, an optional signal 2 VOI, a mandatory noise background VOI, and the
 * number of NMR receivers to calculate the signal to noise ratio for the signal VOI(s). The contrast to noise ratio is
 * simply the signal to noise ratio for signal VOI 1 minus the signal to noise ratio for signal VOI 2. This program
 * requires a single MRI magnitude image. The program mathematics assumes a General Rician or noncentral chi probability
 * distribution characteristic of a MRI magnitude image. For the case of a single receiver system, the General Rician
 * distribution reduces to the Rician distribution.
 * 
 * <p>
 * The program calculates the SNR by 2 different methods. First, the signal to noise ratio is found using a equation for
 * the first moment of Generalized Rice distribution. The first method follows the general 3 step approach outlined in
 * the Constantinides reference: 1.) The noise standard deviation equals the square root((sum over i for background VOI
 * of pixel(i) * pixel(i))/ (2 * number of background pixels * number of NMR receivers)) 2.) For each signal VOI the
 * mean is calculated. 3.) The the mean is divided by the noise standard deviation The signal to noise ratio is
 * calculated from the VOI mean to noise ratio using an equation (mean/standard deviation) = 1 * 3 * 5 * ... (2n - 1) *
 * (1/((2**(n-1))*((n - 1)!))) * sqrt(PI/2) * 1F1(-1/2, n, -SNR*SNR/2) where n = number of NMR receivers in the phase
 * array 1F1 is the confluent hypergeometric function of the first kind For the case of 1 NMR receiver the confluent
 * hypergometric function can be replaced by modified Bessel functions as follows: (mean/standard deviation) =
 * sqrt(PI/2) * exp(-SNR*SNR/4)((1 + (SNR*SNR/2))*I0(SNR*SNR/4) + (SNR*SNR/2)*I1(SNR*SNR/4)) However, since this Bessel
 * function equivalence can only be used in the case of 1 receiver, a port of the ACM Algorithm 707 conhyp routine is
 * used to find the confluent hypergeometric function for values of SNR * SNR/2 <= 3000. The port of the conhyp routine
 * failed for values of SNR*SNR/2 > 3055, so for SNR*SNR/2 > 3000 the asymptotic limiting formula of the confluent
 * hypergeometric function used in Appendix C of the Sato reference was used.
 * </p>
 * 
 * <p>
 * The second method uses the maximum likelihood approach outlined in the Sijbers thesis. This approach finds the signal
 * value which maximizes the value of the log of the maximum likelihood function (Equation 5.38): Let pixelNumber =
 * number of pixels in the signal VOI log(L) equals approximately -pixelNumber*(n - 1)*log(signal)
 * -pixelNumber*signal**2/(2*backgroundVariance) + sum from i = 1 to pixelNumber of (log(BESSEL_I of order (n - 1) of
 * (pixel[i]*signal/backgroundVariance)) Note that this method does not work for large values of signal to noise ratio
 * because the BESSEL_I function cannot handle large arguments. For example, the UNSCALED BESSEL_I of order 0 of x can
 * only handle values of x up to about 699. For x >= 700, the Bessel UNSCALED function overflows. Since we require the
 * log of the Bessel function, the range is extended by using the SCALED Bessel function which returns the BESSEL
 * function * exp(-realArg), so we obtain the log(Bessel) from the log(returned function) + realArg. For the scaled
 * Bessel function errors only start for x >= 32768. In finding the signal value that minimizes the negative of the
 * maximum likelihood equation, a one dimensional search method called Brent's method that uses inverse parabolic
 * interpolation calls the log of the maximum likelihood function. Brent's method is started with 3 points, 0.75 *
 * first moment calculated signal, the first moment calculated signal, and 1.25 * first moment calculated signal. This
 * assumes that the first moment result differs from the maximum likelihood moment by less than 25%.
 * </p>
 * 
 * <p>
 * Testing the validity of this program is easy enough. Each receiver in the phase array has 2 standard deviations - one
 * associated with the real part of the complex image and one associated with the imaginary part of the complex image.
 * signal with noise = square root((signal without noise)**2 + sum from k = 1 to n of (sigmak real)**2 + (simgak
 * imaginary)**2) So to simulate noisy data at each pixel we simply use 2*n Gaussian distributions in the above fashion.
 * Running testAlgorithm with 1000 counts of a 100.0 signal and 1000 counts of a varying background for the 1 receiver
 * case gave: background Calculated SNR Ideal SNR from first moment 0.5 198.0 200.0 1.0 101.0 100.0 2.0 48.9 50.0 5.0
 * 19.7 20.0 20.0 5.10 5.00 50.0 2.28 2.00 100.0 1.09 1.00
 * </p>
 * 
 * <p>
 * A second run of testAlgorithm showed good agreement between the 2 methods of finding the SNR: background First moment
 * SNR Maximum likelihood SNR 100.0 1.21 1.16 50.0 2.20 2.21 20.0 5.09 5.09 10.0 10.0 10.0 5.0 20.0 20.0
 * </p>
 * 
 * <p>
 * For 1F1(-1/2, 1, x) tested with Shanjie Zhang and Jianming Jin Computation of Special Functions CHGM routine and ACM
 * Algorithm 707 conhyp routine by Mark Nardin, W. F. Perger, and Atul Bhalla the results were: From x = -706 to x =
 * +184 the results of the 2 routines matched to at least 1 part in 1E5.
 * </p>
 * 
 * <p>
 * For x = -3055 to x = +184 the ACM routine seemed to give valid results. For x <= -3056 the ACM routine results
 * oscillated wildly and at x = -3200 the result was frozen at 1.0E75. For x >= +185 the ACM result was frozen at
 * 1.0E75. The log result was also seen to oscillate for x <= -3056.
 * </p>
 * 
 * <p>
 * For x = -706 to x = +781 the CHGM routine seemed to give valid results. For x = -707 to x = -745 the CHGM routine
 * gave infinity and for x <= -746 the CHGM routine gave NaN. For x >= +783 the CHGM routine gave a result of -infinity.
 * </p>
 * 
 * <p>
 * So for x = -(signal/noise)*(signal/noise)/2, the CHGM routine can handle a maximum signal/noise value of 37.5 and in
 * standard output the ACM routine can handle a maximum signal/noise value of 78.166.
 * </p>
 * 
 * <p>
 * For 1F1(-1/2, 2, x) the results were very similar: From x = -706 to x = +189 the results of the 2 routines matched to
 * at least 1 part in 1E5.
 * </p>
 * 
 * <p>
 * From x = -3058 to x = +189 the ACM routine seemed to give valid results. At x <= -3059 the ACM results oscillated
 * wildly. For x > +189 the ACM result was frozen at 1.0E75.
 * </p>
 * 
 * <p>
 * For x = -706 to x = +791 the CHGM routine seemed to give valid results. For x = -707 to x = -745 the CHGM routine
 * gave infinity and for x <= -746 the CHGM routine gave NaN. For x >= +792 the CHGM routine gave a result of -infinity.
 * </p>
 * 
 * <p>
 * In this module use the ACM routine for x >= -3000. For large negative x, use the formula: As |x| approaches infinity,
 * 1F1(a, b, x) approaches (gamma(b)/gamma(b-a))*((-x)**(-a))*[1 + Order(|x|**-1)} for the real part of x < 0. More
 * specifically from the Sato reference, the asymptotic behavior of the confluent hypergeometric function is: 1F1(a, b,
 * x) approaches (gamma(a)/gamma(b-a)*((-x)**(-a))*G(a, a-b+1, -x) where G is an asymptotic series G(a, b, x) = 1 +
 * (a*b)/(1!*x) + (a*(a+1)*b*(b+1))/(2!*x**2) + ...
 * </p>
 * 
 * <p>
 * References: 1.) PhD. Thesis Signal and Noise Estimation From Magnetic Resonance Images by Jan Sijbers, Universiteit
 * Antwerpen, Department Natuurkunde. 2.) Signal-to-noise Measurements in Magnitude Images from NMR Phased Arrays by
 * Chris D. Constantinides, Ergin Atalar, Elliot R. McVeigh, Magnetic Resonance in Medicine, November, 1997, 38(5), pp.
 * 852-857. Erratum in Magnetic Resonance in Medicine, July, 2004, 52(1), p. 219. 3.) I. S. Gradshteyn and I. M. Rhyzik,
 * Tables of integrals, series, and products, Sixth edition. 4.) Tohru Sato, Liviu F. Chibotaru, and Arnout Ceulemans,
 * The Exe dynamic Jahn-Teller problem: A new insight from the strong coupling limit, The Journal of Chemical Physics,
 * Vol. 122, 054104, 2005, Appendix C.
 * </p>
 * 
 * <p>
 * Derivation of Bessel function formula when number of receivers == 1 E[M**v] = (2*sigma**2)**(v/2) * gamma(1 + v/2) *
 * 1F1(-v/2, 1, -A**2/(2*sigma**2)) E[M] = sigma * sqrt(2) * gamma(3/2) * 1F1(-1/2, 1, -A**2/(2*sigma**2)) gamma(1/2) =
 * sqrt(PI) gamma(n+1) = n*gamma(n) gamma(3/2) = sqrt(pi)/2 E[M] = sigma * sqrt(PI/2) * 1F1(-1/2, 1, -A**2/(2*sigma**2))
 * 1F1(alpha, gamma, z) = exp(z) * 1F1(gamma - alpha, gamma, -z) Let pow = A**2/(2*sigma**2) 1F1(-1/2, 1, -pow) =
 * exp(-pow) * 1F1(3/2, 1, pow) = exp(-pow) * [1/2 * 1F1(3/2, 1, pow) + 1/2 * 1F1(3/2, 1, pow)]
 * </p>
 * 
 * <p>
 * alpha*1F1(alpha+1, gamma, z) = (z + 2*alpha - gamma)*1F1(alpha, gamma, z) + (gamma - alpha) * 1F1(alpha - 1, gamma,
 * z) 1F1(-1/2, 1, -pow) = exp(-pow) * [0.5 * 1F1(3/2, 1, pow) + pow * 1F1(1/2, 1, pow) + 0.5 * 1F1(-1/2, 1, pow)] =
 * exp(-pow) * [(1 + pow) * 1F1(1/2, 1, pow) + 0.5*(1F1(3/2, 1, pow) - 1F1(1/2, 1, pow)) - 0.5*(1F1(1/2, 1, pow) -
 * 1F1(-1/2, 1, pow))]
 * </p>
 * 
 * <p>
 * (z/gamma)*1F1(alpha+1, gamma+1, z) = 1F1(alpha+1, gamma, z) - 1F1(alpha, gamma, z)
 * </p>
 * 
 * <p>
 * 1F1(-1/2, 1, -pow) = exp(-pow) * [(1 + pow)* 1F1(1/2, 1, pow) + (pow/2) * 1F1(3/2, 2, pow) - (pow/2) * 1F1(1/2, 2,
 * pow)] = exp(-pow) * [(1 + pow) * 1F1(1/2, 1, pow) + ((pow/2)**2)*1F1(3/2, 3, pow)] = exp(-pow/2) * [(1 + pow) *
 * exp(-pow/2) * 1F1(1/2, 1, pow) + ((pow/2)**2)* exp(-pow/2) * 1F1(3/2, 3, pow)]
 * </p>
 * 
 * <p>
 * Iv(x) = ((2**-v)/gamma(v+1)) * (x**v) * exp(-x) * 1F1(1/2 + v, 1 + 2*v, 2*x) I0(x) = exp(-x) * 1F1(1/2, 1, 2*x) I1(x) =
 * (x * exp(-x)/2) * 1F1(3/2, 3, 2*x)
 * </p>
 * 
 * <p>
 * 1F1(-1/2, 1, -pow) = exp(-pow/2) * [(1 + pow) * I0(pow/2) + pow * I1(pow/2)]
 * </p>
 */
public class AlgorithmSingleMRIImageSNR extends AlgorithmBase implements RealFunctionOfOneVariable {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The index of a rerquired noise background VOI. */
    private int backgroundIndex;

    /** DOCUMENT ME! */
    private double backgroundVariance;

    /** DOCUMENT ME! */
    private DecimalFormat nf;

    /** The number of NMR receivers. */
    private int numReceivers;

    /** The index of a second optional signal VOI. >= 0 if present */
    private int signal2Index;

    /** DOCUMENT ME! */
    private float[] signalBuffer;

    /** The index of a required signal VOI. */
    private int signalIndex;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useMaxLikelihood = true;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmSingleMRIImageSNR object.
     * 
     * @param srcImg source image
     * @param signalIndex the index of the signal VOI
     * @param signal2Index the index of the signal2 VOI if >= 0
     * @param backgroundIndex the index of the background VOI
     * @param numReceivers the number of NMR receivers
     */
    public AlgorithmSingleMRIImageSNR(ModelImage srcImg, int signalIndex, int signal2Index, int backgroundIndex,
            int numReceivers) {

        super(null, srcImg);

        this.signalIndex = signalIndex;
        this.signal2Index = signal2Index;
        this.backgroundIndex = backgroundIndex;
        this.numReceivers = numReceivers;
        UI = ViewUserInterface.getReference();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {

        int xDim, yDim, zDim, tDim, sliceSize;
        int i, j;
        int imageLength;
        float[] floatBuffer;
        double minimumSignal;
        double centralSignal;
        double maximumSignal;
        double[] maxLikelihoodSignal = new double[2];
        short[] mask;
        double backgroundStdDev;
        int backgroundCount = 0;
        float mean = 0.0f;
        int meanCount = 0;
        double meanDivStdDev;
        float mean2 = 0.0f;
        int mean2Count = 0;
        double mean2DivStdDev;
        double snr = 0.0;
        double snrML = 0.0;
        double snr2;
        double snrML2 = 0.0;
        double cnr;
        double cnrML;
        double tol = 1.0E-3;
        boolean test = false;
        boolean validityTest = false;
        
        boolean testme= true;
//        if (testme) {
//            AlgorithmMultiExponentialFitting mef = new AlgorithmMultiExponentialFitting();
//            mef.selfTest();
//            setCompleted(true);
//            return;
//        }

        if (validityTest) {
            testAlgorithm();
            setCompleted(true);
            return;
        }

        if (test) {
            //ConfluentHypergeometric cf;
            //int Lnchf = 0;
            //int ip = 776;
            //double[] result = new double[1];
            double[] realResult = new double[1];
            double[] imagResult = new double[1];
            //double[] realResult2 = new double[1];
            //double[] imagResult2 = new double[1];
            //double mag;
            //double mag2;
            double x = -30.0;
            //double realZ;
            //double imagZ;
            Gamma gam;
            double[] resultB = new double[1];
            double[] resultBMinusA = new double[1];
            //double gamConstant;
            gam = new Gamma((double) numReceivers, resultB);
            gam.run();
            gam = new Gamma( (numReceivers + 0.5), resultBMinusA);
            gam.run();
            //gamConstant = resultB[0] / resultBMinusA[0];

            double imaginaryArg = 0.0;
            double initialOrder = (double) (numReceivers - 1);
            int sequenceNumber = 1; // Number of sequential Bessel function orders calculated

            int[] nz = new int[1]; // number of components set to zero due to underflow
            int[] errorFlag = new int[1]; // zero if no error

            for (x = 12000.00; x <= 40000.0; x++) {
                Bessel bes = new Bessel(Bessel.BESSEL_I, x, imaginaryArg, initialOrder, Bessel.SCALED_FUNCTION,
                        sequenceNumber, realResult, imagResult, nz, errorFlag);
                bes.run();
                Preferences.debug("x = " + x + " realResult[0] = " + realResult[0] + " nz = " + nz[0] + " errorFlag = "
                        + errorFlag[0] + "\n", Preferences.DEBUG_ALGORITHM);
            }

            /*
             * for (realZ = -10.0; realZ <= 10.0; realZ++) { for (imagZ = -10.0; imagZ <= 10.0; imagZ++) { cf = new
             * ConfluentHypergeometric(-0.5, 1.0, realZ, imagZ, realResult, imagResult); cf.run(); cf = new
             * ConfluentHypergeometric(-0.5, 0.0, 1.0, 0.0, realZ, imagZ, Lnchf, ip, realResult2, imagResult2);
             * cf.run(); //result[0] = gamConstant * Math.sqrt(-x) * (1.0 + (-0.5)*(0.5-numReceivers)/(-x) +
             * //(-0.25)*(0.5-numReceivers)*(1.5-numReceivers)/(2.0*x*x)); Preferences.debug("realZ = " + realZ + "
             * imagZ = " + imagZ + " realResult[0] = " + realResult[0] + " imagResult[0] = " + imagResult[0] + "
             * realResult2[0] = " + realResult2[0] + " imagResult2[0] = " + imagResult2[0] + "\n",
             *  Preferences.DEBUG_ALGORITHM); if
             * ((Math.abs((realResult[0] - realResult2[0])/realResult[0]) > 1.0E-5) || (Math.abs((imagResult[0] -
             * imagResult2[0])/imagResult[0]) > 1.0E-5)){ Preferences.debug("Mismatch at realZ = " + realZ + " imagZ = " +
             * imagZ + "\n", Preferences.DEBUG_ALGORITHM); } } }
             */
            setCompleted(true);

            return;
        } // if (test)

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Calculating MRI SNR ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imageLength = sliceSize;

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
            MipavUtil.displayError("IOException " + e + " on srcImage.export(0, imageLength, floatBuffer)");

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

        backgroundVariance = 0.0;

        for (i = 0; i < imageLength; i++) {

            if (mask[i] == backgroundIndex) {
                backgroundVariance += floatBuffer[i] * floatBuffer[i];
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

        backgroundVariance = backgroundVariance / (2.0f * backgroundCount * numReceivers);
        backgroundStdDev = Math.sqrt(backgroundVariance);
        Preferences.debug("Noise standard deviation = " + nf.format(backgroundStdDev) + "\n", Preferences.DEBUG_ALGORITHM);
        mean = mean / meanCount;
        Preferences.debug("Mean for signal 1 VOI = " + nf.format(mean) + "\n", Preferences.DEBUG_ALGORITHM);
        meanDivStdDev = mean / backgroundStdDev;

        snr = funcC(meanDivStdDev, true);
        Preferences.debug("First moment SNR for signal 1 VOI = " + nf.format(snr) + "\n", Preferences.DEBUG_ALGORITHM);
        UI.setDataText("First moment SNR for signal 1 VOI = " + nf.format(snr) + "\n");

        signalBuffer = new float[meanCount];

        for (i = 0, j = 0; i < imageLength; i++) {

            if (mask[i] == signalIndex) {
                signalBuffer[j++] = floatBuffer[i];
            }
        }

        centralSignal = snr * backgroundStdDev;
        minimumSignal = 0.75 * centralSignal;
        maximumSignal = 1.25 * centralSignal;
        Brent.search( minimumSignal, centralSignal, maximumSignal, maxLikelihoodSignal, this, tol );

        if (useMaxLikelihood) {
            snrML = maxLikelihoodSignal[0] / backgroundStdDev;
            Preferences.debug("Maximum likelihood SNR for signal 1 VOI = " + nf.format(snrML) + "\n", 
            		Preferences.DEBUG_ALGORITHM);
            UI.setDataText("Maximum likelihood SNR for signal 1 VOI = " + nf.format(snrML) + "\n");
        }

        if (signal2Index >= 0) {
            mean2 = mean2 / mean2Count;
            Preferences.debug("Mean for signal 2 VOI = " + nf.format(mean2) + "\n", Preferences.DEBUG_ALGORITHM);
            mean2DivStdDev = mean2 / backgroundStdDev;
            snr2 = funcC(mean2DivStdDev, false);
            Preferences.debug("First moment SNR for signal 2 VOI = " + nf.format(snr2) + "\n", Preferences.DEBUG_ALGORITHM);
            UI.setDataText("First moment SNR for signal 2 VOI = " + nf.format(snr2) + "\n");

            if (useMaxLikelihood) {
                signalBuffer = new float[mean2Count];

                for (i = 0, j = 0; i < imageLength; i++) {

                    if (mask[i] == signal2Index) {
                        signalBuffer[j++] = floatBuffer[i];
                    }
                }

                centralSignal = snr2 * backgroundStdDev;
                minimumSignal = 0.75 * centralSignal;
                maximumSignal = 1.25 * centralSignal;
                Brent.search( minimumSignal, centralSignal, maximumSignal, maxLikelihoodSignal, this, tol );

                if (useMaxLikelihood) {
                    snrML2 = maxLikelihoodSignal[0] / backgroundStdDev;
                    Preferences.debug("Maximum likelihood SNR for signal 2 VOI = " + nf.format(snrML2) + "\n", 
                    		Preferences.DEBUG_ALGORITHM);
                    UI.setDataText("Maximum likelihood SNR for signal 2 VOI = " + nf.format(snrML2) + "\n");
                } // if (useMaxLikelihood)
            } // if (useMaxLikelihood)

            cnr = snr - snr2;
            Preferences.debug("First moment contrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n", 
            		Preferences.DEBUG_ALGORITHM);
            UI.setDataText("First moment contrast to noise ratio for 1 - 2 = " + nf.format(cnr) + "\n");

            if (useMaxLikelihood) {
                cnrML = snrML - snrML2;
                Preferences.debug("Maximum likelihood contrast to noise ratio for 1 - 2 = " + nf.format(cnrML) + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                UI.setDataText("Maximum likelihood contrast to noise ratio for 1 - 2 = " + nf.format(cnrML) + "\n");
            } // if (useMaxLikelihood)
        } // if (signal2Index >= 0)

        setCompleted(true);

        return;
    }

    /**
     * This routine iterates to find the solution to the first moment equation for the Generalized Rice distribution.
     * 
     * @param meanDivStdDev DOCUMENT ME!
     * @param signal DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private double funcC(double meanDivStdDev, boolean signal) {

        /**
         * For 1F1(-1/2, 1, x) for the ACM code called the result is only valid for x >= -3055 and for 1F1(-1/2, 2, x)
         * the result is only valid for x >= -3058. For large negative x, use the formula: As |x| approaches infinity,
         * 1F1(a, b, x) approaches (gamma(b)/gamma(b-a))*((-x)**(-a))*[1 + Order(|x|**-1)} for the real part of x < 0.
         * More specifically from the Sato reference, the asymptotic behavior of the confluent hypergeometric function
         * is: 1F1(a, b, x) approaches (gamma(a)/gamma(b-a)*((-x)**(-a))*G(a, a-b+1, -x) where G is an asymptotic series
         * G(a, b, x) = 1 + (a*b)/(1!*x) + (a*(a+1)*b*(b+1))/(2!*x**2) + ...
         */
        double snr;
        double lowerBound;
        double upperBound;
        int i;
        int maxIters = 100;
        snr = meanDivStdDev / 2.0;
        lowerBound = 0.0;
        upperBound = meanDivStdDev;

        double calculatedMeanDivStdDev;
        double error = 0.0;
        double square;
        double constant = Math.sqrt(Math.PI / 2.0);

        // double result[] = new double[1];
        ConfluentHypergeometric cf;
        int Lnchf = 0; // 0 for standard output; 1 for log of result
        int ip = 776; // Number of desired array positions; 776 is the maximum possible value.
        double[] realResult = new double[1];
        double[] imagResult = new double[1];
        int n;
        Gamma gam;
        double[] resultB = new double[1];
        double[] resultBMinusA = new double[1];
        double gamConstant;

        for (n = 1; n <= numReceivers; n++) {
            constant = constant * ( (2 * n) - 1);
        }

        constant = constant / Math.pow(2.0, numReceivers - 1);

        for (n = numReceivers - 1; n >= 2; n--) {
            constant = constant / n;
        }

        gam = new Gamma((double) numReceivers, resultB);
        gam.run();
        gam = new Gamma( (numReceivers + 0.5), resultBMinusA);
        gam.run();
        gamConstant = resultB[0] / (resultBMinusA[0] * Math.sqrt(2.0));

        for (i = 0; i < maxIters; i++) {
            square = snr * snr / 2.0;

            // cf = new ConfluentHypergeometric(-0.5, numReceivers, -square, result);
            // cf.run();
            if (square <= 3000) {
                cf = new ConfluentHypergeometric( -0.5, 0.0, (double) numReceivers, 0.0, -square, 0.0, Lnchf, ip,
                        realResult, imagResult);
                cf.run();
                // Preferences.debug("realResult cf run = " + realResult[0] + "\n"), Preferences.DEBUG_ALGORITHM;
            } else {
                realResult[0] = gamConstant
                        * snr
                        * (1.0 + ( ( -0.5) * (0.5 - numReceivers) / square) + ( ( -0.25) * (0.5 - numReceivers)
                                * (1.5 - numReceivers) / (2.0 * square * square)));
                // Preferences.debug("realResult snr = " + realResult[0] + "\n", Preferences.DEBUG_ALGORITHM);
            }

            calculatedMeanDivStdDev = constant * realResult[0];
            error = Math.abs(calculatedMeanDivStdDev - meanDivStdDev) / meanDivStdDev;

            // Preferences.debug("error = " + error + "\n", Preferences.DEBUG_ALGORITHM);
            if (error < 0.001) {
                break;
            }

            if (calculatedMeanDivStdDev > meanDivStdDev) {
                upperBound = snr;
                snr = (snr + lowerBound) / 2.0;
            } else {
                lowerBound = snr;
                snr = (snr + upperBound) / 2.0;
            }
        } // for (i = 0; i < maxIters; i++)

        if (signal) {
            Preferences.debug("Error for signal 1 VOI SNR after " + i + " iterations  = " + nf.format(100 * error)
                    + "%\n", Preferences.DEBUG_ALGORITHM);
        } else {
            Preferences.debug("Error for signal 2 VOI SNR after " + i + " iterations  = " + nf.format(100 * error)
                    + "%\n", Preferences.DEBUG_ALGORITHM);
        }

        return snr;

    }

    /**
     * For an input signal value this routine returns the log of the maximimum likelihood function.
     * 
     * @param signal DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private double receiverMaximumLikelihood(double signal) {
        int n;
        double likelihood = 0.0;
        n = signalBuffer.length;

        Bessel bes;
        int i;
        double imaginaryArg = 0.0;
        double initialOrder = (double) (numReceivers - 1);
        int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
        double signalDivVar = signal / backgroundVariance;
        double[] realResult = new double[1];
        double[] imagResult = new double[1];
        int[] nz = new int[1]; // number of components set to zero due to underflow
        int[] errorFlag = new int[1]; // zero if no error
        double realArg;

        for (i = 0; i < n; i++) {
            realArg = signalBuffer[i] * signalDivVar;
            bes = new Bessel(Bessel.BESSEL_I, realArg, imaginaryArg, initialOrder, Bessel.SCALED_FUNCTION,
                    sequenceNumber, realResult, imagResult, nz, errorFlag);
            bes.run();
            likelihood += (Math.log(realResult[0]) + realArg);

            if (errorFlag[0] != 0) {
                Preferences.debug("Bessel_I error for realArg = " + realArg + "\n", Preferences.DEBUG_ALGORITHM);
                useMaxLikelihood = false;

                return Double.NEGATIVE_INFINITY;
            }
        } // for (i = 0; i < n; i++)

        likelihood -= ( (n * (numReceivers - 1) * Math.log(signal)) + (n * signalDivVar * signal / 2.0));
        Preferences.debug("signal = " + signal + " likelihood = " + likelihood + "\n", Preferences.DEBUG_ALGORITHM);

        return likelihood;
    }

    /**
     * DOCUMENT ME!
     */
    private void testAlgorithm() {
        double signal = 100.0;
        double stdDev = 1.0;
        int backgroundCount = 1000;
        int signalCount = 1000;
        int numReceivers = 1;
        double backgroundStdDev;
        int i;
        int n;
        double noise;
        RandomNumberGen randomGen;
        double pixelSquare;
        double mean = 0.0;
        double meanDivStdDev;
        double snr;
        double minimumSignal;
        double centralSignal;
        double maximumSignal;
        //double[] maxLikelihoodSignal = new double[1];
        double tol = 1.0E-3;

        nf = new DecimalFormat("0.00E0");

        randomGen = new RandomNumberGen();

        backgroundVariance = 0.0;

        for (i = 0; i < backgroundCount; i++) {

            for (n = 0; n < (2 * numReceivers); n++) {
                noise = stdDev * randomGen.genStandardGaussian();
                backgroundVariance += noise * noise;
            }
        }

        backgroundVariance = backgroundVariance / (2.0f * backgroundCount * numReceivers);
        backgroundStdDev = Math.sqrt(backgroundVariance);
        Preferences.debug("Noise standard deviation = " + nf.format(backgroundStdDev) + "\n", Preferences.DEBUG_ALGORITHM);

        signalBuffer = new float[signalCount];

        for (i = 0; i < signalCount; i++) {
            pixelSquare = signal * signal;

            for (n = 0; n < (2 * numReceivers); n++) {
                noise = stdDev * randomGen.genStandardGaussian();
                pixelSquare += noise * noise;
            }

            signalBuffer[i] = (float) Math.sqrt(pixelSquare);
            mean += Math.sqrt(pixelSquare);
        }

        mean = mean / signalCount;
        Preferences.debug("Mean = " + nf.format(mean) + "\n", Preferences.DEBUG_ALGORITHM);
        meanDivStdDev = mean / backgroundStdDev;

        snr = funcC(meanDivStdDev, true);
        Preferences.debug("SNR = " + nf.format(snr) + "\n", Preferences.DEBUG_ALGORITHM);

        centralSignal = snr * backgroundStdDev;
        minimumSignal = 0.5 * centralSignal;
        maximumSignal = 1.5 * centralSignal;
        double[] maxLikelihoodSignal = new double[2];
        Brent.search(minimumSignal, centralSignal, maximumSignal, maxLikelihoodSignal, this, tol );
        snr = maxLikelihoodSignal[0] / backgroundStdDev;
        Preferences.debug("Maximum likelihood snr for signal 1 VOI = " + nf.format(snr) + "\n", Preferences.DEBUG_ALGORITHM);

        return;
    } // testAlgorithm

    @Override
    public double eval(double x) {
        return -receiverMaximumLikelihood(x);
    }

    /**
     * This func is commented out because it only works for the case when the number of receivers equals one.
     */
    /**
     * private double func(double meanDivStdDev, boolean signal) { double snr; double lowerBound; double upperBound; int
     * i; int maxIters = 1000; snr = meanDivStdDev/2.0; lowerBound = 0.0; upperBound = meanDivStdDev; double
     * calculatedMeanDivStdDev; double square; double sqdiv2; double constant = Math.sqrt(Math.PI/2.0); double[] cyr =
     * new double[1]; double[] cyi = new double[1]; int[] nz = new int[1]; int[] errorFlag = new int[1]; double i0;
     * double i1; for (i = 0; i < maxIters; i++) { square = snr snr / 2.0; sqdiv2 = square/2.0; Bessel i0Bessel = new
     * Bessel(Bessel.BESSEL_I,sqdiv2,0.0,0.0,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag); i0Bessel.run(); i0 =
     * cyr[0]; Bessel i1Bessel = new
     * Bessel(Bessel.BESSEL_I,sqdiv2,0.0,1.0,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag); i1Bessel.run(); i1 =
     * cyr[0]; calculatedMeanDivStdDev = constant * Math.exp(-sqdiv2) *((1.0 + square)*i0 + square*i1); if
     * (Math.abs(calculatedMeanDivStdDev - meanDivStdDev)/meanDivStdDev < 0.001) { break; } if (calculatedMeanDivStdDev >
     * meanDivStdDev) { upperBound = snr; snr = (snr + lowerBound)/2.0; } else { lowerBound = snr; snr = (snr +
     * upperBound)/2.0; } } // for (i = 0; i < maxIters; i++) if (signal) { if (i == maxIters) {
     * Preferences.debug("Failure to converge for signal 1 VOI SNR after " + maxIters + " iterations\n", 
     * Preferences.DEBUG_ALGORITHM);
     * UI.setDataText("Failure to converge for signal 1 VOI SNR after " + maxIters + " iterations\n"); } else {
     * Preferences.debug("Signal 1 VOI SNR converged after " + i + " iterations\n", 
     * Preferences.DEBUG_ALGORITHM); } } else { if (i == maxIters) {
     * Preferences.debug("Failure to converge for signal 2 VOI SNR after " + maxIters + " iterations\n", 
     * Preferences.DEBUG_ALGORITHM);
     * UI.setDataText("Failure to converge for signal 2 VOI SNR after " + maxIters + " iterations\n"); } else {
     * Preferences.debug("Signal 2 VOI SNR converged after " + i + " iterations\n", 
     * Preferences.DEBUG_ALGORITHM); } } return snr; }
     */
}
