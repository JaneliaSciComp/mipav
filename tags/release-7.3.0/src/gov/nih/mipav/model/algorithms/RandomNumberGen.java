package gov.nih.mipav.model.algorithms;


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
    
    /** This code is derived from PoissDecay.cpp, PoissonDeviate, PoissonRecur, and RandomDeviate,
     * code accompanying Data Reduction and Error Analysis for the
     * Physical Sciences, Third Edition, by Philip R. Bevington and D. Keith Robinson
     * p(x, u)is the probability of observing x events where u is the average number of events observed.
     * x must be zero or a positive integer.
     * p(x, u) = (u**x)* exp(-u)/x!
     * The sum of x from 0 to infinity of p(x, u) = 1.
     * mean value of x = u
     * variance of x = u
     * Return gain * p(x, u) + offset
     * @param nEvents
     * @param mu mean of Poisson distribtuion
     * @param gain
     * @param offset
     * @return
     */
    public final double[] poissDecay(int nEvents, double mu, double gain, double offset) {
        int maxBins = 100;
        double poissEvents[] = new double[nEvents];
        long seed1;
        long seed2;
        long seed3;
        int i;
        int n;
        double poiss;
        double ptable[];
        int jx;
        double temp;
        double r;
        
        // Initialize the 3 seeds to integers uniformly distributed between 1 and 30,000
        seed1 = genUniformRandomNum(1, 30000);
        seed2 = genUniformRandomNum(1, 30000);
        seed3 = genUniformRandomNum(1, 30000);
        // mean + 8 * sigma
        n = (int)(mu + 8 * Math.sqrt(mu));
        if (n > maxBins) {
            MipavUtil.displayError("Overflow error in poissDecay mean mu too large");
            return null;
        }
        ptable = new double[n+1];
        poiss = Math.exp(-mu);
        ptable[0] = poiss;
        for (i = 1; i <= n-1; i++) {
            // poiss = (mu**i)*exp(-mu)/(i!)
            poiss = poiss * mu/i;
            ptable[i] = ptable[i-1]+poiss;
        } // for (i = 1; i <= n-1; i++)
        // Assure unit probability
        ptable[n] = 1.0;
        for (i = 0; i < nEvents; i++) {
            jx = -1;
            seed1 = 171 * (seed1 % 177) - 2 * (seed1 / 177);
            if (seed1 < 0) {
                seed1 = seed1 + 30269;
            }
            seed2 = 172*(seed2 % 176) - 35 * (seed2/ 176);
            if (seed2 < 0) {
                seed2 = seed2 + 30307;
            }
            seed3 = 170 * (seed3 % 178) - 63 * (seed3 / 178);
            if (seed3 < 0) {
                seed3 = seed3 + 30323;
            }
            temp = seed1/30269.0 + seed2/30307.0 + seed3/30323.0;
            r = temp - (int)temp;
            do {
                jx++;
            } while (ptable[jx] <= r);
            poissEvents[i] = gain * jx + offset;
        } // for (i = 1; i <= nEvents; i++)
        return poissEvents;
    }

}
