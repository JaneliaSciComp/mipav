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