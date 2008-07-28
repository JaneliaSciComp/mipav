package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.*;

import gov.nih.mipav.view.*;

import java.util.*;


/**
 * Algorithm that produces a random number (Gaussian or uniform) in a specific user/programmer defined range.
 *
 * @version  0.1 May 4, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public class RandomNumberGen extends Random {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4394735859119506749L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * RandomNumberGen - constructor.
     */
    public RandomNumberGen() {
        super(System.currentTimeMillis());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the factorial of a nonnegative integer.
     *
     * @param   number  integer whose factorial is being returned
     *
     * @return  number!
     */
    public static final long factorial(int number) {
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
     * Generates a Gaussian random number greater or equal to the stRange and less than or equal to the endRange.
     *
     * @param   stRange   the random number will be greater than or equal to this number
     * @param   endRange  the random number will be less than or equal to this number
     *
     * @return  integer number in the range
     */
    public final int genGaussianRandomNum(int stRange, int endRange) {
        float rNum;
        // Float.MIN_VALUE equals 2**(-149) equals approximately 1.4E-45
        // Consider the case of stRange equals 0
        // The additon of -0.5f + (float)(3.0 * Float.MIN_VALUE) gives
        // -0.5f.  In fact, the addition of -0.5 + (float)(1.0E37 * Float.MIN_VALUE)
        // still gives - 0.5f.  If you use -0.5f + (float)(1.1E37 * Float_MIN_VALUE),
        // a result of -0.49999997f is achieved.
        // If stRange or endRange are different than zero, then you must add
        // in an even larger multiple of Float.MIN_VALUE
        float newSt;
        float newEnd;
        float newSta;
        float newEnda;
        float eps = (float)(1.1E37 * Float.MIN_VALUE);
        newSta = (float)stRange - 0.5f;
        newSt = newSta + eps;
        while (newSt == newSta) {
            eps = 2.0f * eps;
            newSt = newSta + eps;
        }
        newEnda = (float)endRange + 0.5f;
        newEnd = newEnda - eps;
        while (newEnd == newEnda) {
            eps = 2.0f * eps;
            newEnd = newEnda - eps;
        }
        // This modification ensures that the Gaussian encompasses
        // bins on both sides of the end numbers the same way that
        // it does to the intermediate numbers.

        rNum = (float) (nextGaussian());

        if (rNum > 4) {
            rNum = 4;
        } else if (rNum < -4) {
            rNum = -4;
        }

        rNum = (rNum / 8) + 0.5f; // range  0 to 1

        return (int) (Math.round((rNum * (newEnd - newSt)) + newSt));

    }

    /**
     * Generates a Gaussian random number greater or equal to the stRange and less than or equal to the endRange.
     *
     * @param   stRange   the random number will be greater than or equal to this number
     * @param   endRange  the random number will be less than or equal to this number
     *
     * @return  float number in the range
     */
    public final float genGaussianRandomNum(float stRange, float endRange) {
        float rNum;

        rNum = (float) (nextGaussian());

        if (rNum > 4) {
            rNum = 4;
        } else if (rNum < -4) {
            rNum = -4;
        }

        rNum = (rNum / 8) + 0.5f; // range  0 to 1

        return ((rNum * (endRange - stRange)) + stRange);
    }

    /**
     * Generates a Gaussian random number greater or equal to the stRange and less than or equal to the endRange.
     *
     * @param   stRange   the random number will be greater than or equal to this number
     * @param   endRange  the random number will be less than or equal to this number
     *
     * @return  float number in the range
     */
    public final double genGaussianRandomNum(double stRange, double endRange) {
        double rNum;

        rNum = nextGaussian();

        if (rNum > 4) {
            rNum = 4;
        } else if (rNum < -4) {
            rNum = -4;
        }

        rNum = (rNum / 8) + 0.5; // range  0 to 1

        return ((rNum * (endRange - stRange)) + stRange);
    }
    
    public final double genStandardGaussian() {
        double rNum;
        
        rNum = nextGaussian();
        return rNum;
    }

    /**
     * Generates a uniform random number greater or equal to the stRange and less than or equal to the endRange.
     *
     * @param   stRange   the random number will be greater than or equal to this number
     * @param   endRange  the random number will be less than or equal to this number
     *
     * @return  integer number in the range
     */
    public final int genUniformRandomNum(int stRange, int endRange) {
        // Float.MIN_VALUE equals 2**(-149) equals approximately 1.4E-45
        // Consider the case of stRange equals 0
        // The additon of -0.5f + (float)(3.0 * Float.MIN_VALUE) gives
        // -0.5f.  In fact, the addition of -0.5 + (float)(1.0E37 * Float.MIN_VALUE)
        // still gives - 0.5f.  If you use -0.5f + (float)(1.1E37 * Float_MIN_VALUE),
        // a result of -0.49999997f is achieved.
        // If stRange or endRange are different than zero, then you must add
        // in an even larger multiple of Float.MIN_VALUE
        float newSt;
        float newEnd;
        float newSta;
        float newEnda;
        float eps = (float)(1.1E37 * Float.MIN_VALUE);
        newSta = (float)stRange - 0.5f;
        newSt = newSta + eps;
        while (newSt == newSta) {
            eps = 2.0f * eps;
            newSt = newSta + eps;
        }
        newEnda = (float)endRange + 0.5f;
        newEnd = newEnda - eps;
        while (newEnd == newEnda) {
            eps = 2.0f * eps;
            newEnd = newEnda - eps;
        }

        // If the newSt and newEnd modifications are not used, the stRange
        // and endRange numbers will only occur with half the frequency of
        // numbers in between.  For example, if stRange is set equal to 1
        // and endRange is set equal to 3, 2 would occur twice as often as
        // 1 and 3, since 1 is generated by a bin of width 0.5 going from
        // 1.0 to 1.5, 2 is generated by a bin of width 1.0 going from 1.5
        // to 2.5, and 3 is generated by a bin of width 0.5 going from
        // 2.5 to 3.0
        // 3.0*Float_MIN_VALUE is used to compensate for possibility of
        // the float of the integer and 0.5f each being off by
        // Float.MIN_VALUE in the opposite direction.
        return (Math.round((nextFloat() * (newEnd - newSt)) + newSt));
    }

    /**
     * Generates a uniform random number greater or equal to the stRange and less than or equal to the endRange.
     *
     * @param   stRange   the random number will be greater than or equal to this number
     * @param   endRange  the random number will be less than or equal to this number
     *
     * @return  float number in the range
     */
    public final float genUniformRandomNum(float stRange, float endRange) {
        return ((nextFloat() * (endRange - stRange)) + stRange);
    }

    /**
     * Generates a uniform random number greater or equal to the stRange and less than or equal to the endRange.
     *
     * @param   stRange   the random number will be greater than or equal to this number
     * @param   endRange  the random number will be less than or equal to this number
     *
     * @return  float number in the range
     */
    public final double genUniformRandomNum(double stRange, double endRange) {
        return ((nextDouble() * (endRange - stRange)) + stRange);
    }
    
    /**
     * Derived from STSDAS Help Pages Space Telescope Science Institute
     * From 2 lines in procedure mkpnoise in t_mknoise
     * Seen at http://stsdas.stsci.edu/cgi-bin/gethelp.cgi?mknoise.src
     * @param inputValue
     * @param background
     * @param gain
     * @return
     */
    public final double genPoissonRandomNum(double inputValue, double background, double gain) {
        double v;
        double outputValue;
        // Random number seed
        long seed = System.currentTimeMillis();
        
        v = gain * (inputValue + background);
        outputValue = inputValue + (poidev(v, seed) - v)/ gain;
        return outputValue;
    }
    
    /** From Numerical Recipes in C The Art of Scientific Computing Second Edition by William H. Press,
     *  Saul A. Teukolsky, William T.Vetterling, and Brian P. Flannery, pp. 293-295 Poisson Deviates
     *  routine poidev adapted with minor changes
     *  Returns as a double an integer value that is a random deviate drawn from a Poisson distribution
     *  of mean xm
     * @param xm
     * @param seed
     * @return
     */
    public final double poidev(double xm, long seed) {
        double g;
        double em;
        double t;
        double sq;
        double alxm;
        double result[] = new double[1];
        //  0 for ln(gamma(x)), 1 for gamma(x)
        int functionCode = 0;
        Gamma gammaln;
        double y;
        // smallest positive nonzero value 2**-1074
        double smallestUniformValue = Double.MIN_VALUE;
        double largestUniformValue = 1.0;
        double eps = Double.MIN_VALUE;
        while (largestUniformValue == 1.0) {
            largestUniformValue = largestUniformValue - eps;
            eps = 2.0 * eps;
        }
        if (xm < 12.0) {
            // Use direct method
            g = Math.exp(-xm);
            em = -1.0;
            t = 1.0;
            do {
                // Instead of adding exponential deviates it is equivalent to multiply uniform deviates.
                // We never actually have to take the log, merely compare to the pre-computed exponential.
                ++em;
                t *= genUniformRandomNum(smallestUniformValue, largestUniformValue);
            } while (t > g);
        } // if (xm < 12.0)
        else {
            // Use rejection method
            sq = Math.sqrt(2.0 * xm);
            alxm = Math.log(xm);
            gammaln = new Gamma(xm+1.0, functionCode, result);
            gammaln.run();
            try {
                gammaln.finalize();
            }
            catch (Throwable ex) {
                MipavUtil.displayError("Exception on Gamma.finalize()");
                return 0;
            }
            g = xm * alxm - result[0];
            do {
                do {
                    // y is a deviate from a Lorentzian comparison function
                    y = Math.tan(Math.PI * genUniformRandomNum(smallestUniformValue, largestUniformValue)); 
                    // em is y, shifted and scaled.
                    em = sq*y + xm;
                    // Reject if in regime of zero probability
                } while (em < 0.0);
                // The trick for integer-valued distributions.
                em = Math.floor(em);
                gammaln = new Gamma(em+1.0, functionCode, result);
                gammaln.run();
                try {
                    gammaln.finalize();
                }
                catch (Throwable ex) {
                    MipavUtil.displayError("Exception on Gamma.finalize()");
                    return 0;
                }
                t = 0.9*(1.0 + y*y)*Math.exp(em*alxm - result[0] - g);
                // The ratio of the desired distribution to the comparison function;  we accept or 
                // reject by comparing it to another uniform deviate.  The factor of 0.9 is chosen so
                // that t never exceeds 1.
            } while (genUniformRandomNum(smallestUniformValue, largestUniformValue) > t);
        } // else
        return em;
    }

    /**
     * Calculates the Poisson probability function.
     *
     * @param   numberObserved  number of items observed
     * @param   average         mean of the Poisson distribution
     *
     * @return  probability of numberObserved items occurring Note that if the average rate lambda is given instead of
     *          the mean, then the mean is found from mean = lambda * time interval The Poisson may be used to
     *          approximate a binomial distribution if the number n is large and the probability p is small. In this
     *          case the mean is given by n*p. Since the number of counts in a unit time interval cannot be negative,
     *          the Poisson is undefined for numberObserved less than zero. Likewise, the average of the number of
     *          observed items cannot be negative. The variance of a Poisson distribution is equal to its mean.
     */
    public final float poissonProbability(int numberObserved, float average) {

        if (numberObserved < 0) {
            MipavUtil.displayError("The number observed cannot be less than zero");

            return -1.0f;
        }

        if (average < 0.0f) {
            MipavUtil.displayError("The average number of observed items cannot be negative");

            return -1.0f;
        }

        return (float) (Math.pow(average, numberObserved) / factorial(numberObserved) * Math.exp(-average));
    }

}
