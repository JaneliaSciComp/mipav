package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.*;

/**
 * This module draws uniformly randomly positioned circles with a specified radius.
 See http://www.indiana.edu/~statmath for skewness, kurtosis, and Jarque-Bera Test that uses skewness
 and kurtosis to test for normality.
 
 The nearest neighbor distribution of circles generated from a uniform distribution is taken from 
 "The mosaic of nerve cells in the mammalian retina" by H. Wassle and H. J. Riemann, Proc. R. Soc. Lond.,
 B. 200, 441-461, 1978. 
 */
public class AlgorithmRandomCircleGeneration extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Circle radius
    private int radius;
    
    // number of circles to be drawn
    private int numCircles;
    
    private ModelImage testImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmRandomCircleGeneration - default constructor.
     */
    public AlgorithmRandomCircleGeneration() { }

    /**
     * AlgorithmRandomCircleGeneration.
     *
     * @param  srcImg   Blank source image in which circles will be drawn
     * @param  radius   Circle radius
     * @param  numCircles Number of circles to be drawn
     */
    public AlgorithmRandomCircleGeneration(ModelImage srcImage, int radius, int numCircles) {
        super(null, srcImage);
        this.radius = radius;
        this.numCircles = numCircles;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int xDim;
        int yDim;
        byte mask[];
        int x;
        int y;
        int circlesGenerated;
        int yDistSquared;
        int xDistSquared;
        int radiusSquared;
        int xMaskDim;
        int yMaskDim;
        int distSquared;
        int lowestDistSquared;
        int i;
        int j;
        int attempts;
        boolean found;
        byte buffer[];
        int length;
        int xCenter = radius;
        int yCenter = radius;
        /** Reference to the random number generator. */
        RandomNumberGen randomGen;
        int circlesDrawn;
        int circleXCenter[] = new int[numCircles];
        int circleYCenter[] = new int[numCircles];
        double nearestNeighborDistance[];
        double total;
        double mean;
        double variance;
        double stdDev;
        double median;
        double deviate;
        double deviateSquared;
        double deviateCubed;
        double deviateFourth;
        double totalDeviateSquared;
        double totalDeviateCubed;
        double totalDeviateFourth;
        double skewness;
        double kurtosis;
        double chiSquaredOfTwo;
        double density;
        double observedFrequency[] = new double[7];
        double theoreticalFrequency[] = new double[7];
        double chiSquaredOfFour;
        double z;
        int index;
        double normFactor;
        double smallerDistance;
        double largerDistance;
        double oneSeventhRange;
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Random circle generation ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new byte[length];
        // Create a mask for setting circles
        radiusSquared = radius * radius;
        xMaskDim = 2 * radius + 1;
        yMaskDim = xMaskDim;
        mask = new byte[xMaskDim * yMaskDim];
        for (y = 0; y <= 2*radius; y++) {
            yDistSquared = (y - radius);
            yDistSquared = yDistSquared * yDistSquared;
            for (x = 0; x <= 2*radius; x++) {
                xDistSquared = (x - radius);
                xDistSquared = xDistSquared * xDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared <= radiusSquared) {
                    mask[x + y * xMaskDim] = 1;
                }
            }
        } // for (y = 0; y <= 2*radius; y++)
        
        randomGen = new RandomNumberGen();
        circlesGenerated = 0;
        for (i = 1; i <= numCircles; i++) {
        found = false;
        attempts = 0;
            while ((!found) && (attempts <= 100)) {
                circlesGenerated++;
                found = true;
                xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                yloop:
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] == 1) {
                                found = false;
                                attempts++;
                                break yloop;
                            }
                        }
                    }
                } // for (y = 0; y <= 2*radius; y++)
            } // while ((!found) && (attempts <= 100)
            if (!found) {
                break;
            }
            circleXCenter[i-1] = xCenter;
            circleYCenter[i-1] = yCenter;
            for (y = 0; y <= 2*radius; y++) {
                for (x = 0; x <= 2*radius; x++) {
                    if (mask[x + y * xMaskDim] == 1) {
                        buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] = 1;
                    }
                }
            }
        } // for (i = 1; i <= numCircles; i++)
        circlesDrawn = i-1;
        Preferences.debug(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.\n");
        System.out.println(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.");
        
        nearestNeighborDistance = new double[circlesDrawn];
        for (i = 0; i < circlesDrawn; i++) {
            lowestDistSquared = Integer.MAX_VALUE;
            for (j = 0; j < circlesDrawn; j++) {
                if (i != j) {
                    xDistSquared = circleXCenter[i] - circleXCenter[j];
                    xDistSquared = xDistSquared * xDistSquared;
                    yDistSquared = circleYCenter[i] - circleYCenter[j];
                    yDistSquared = yDistSquared * yDistSquared;
                    distSquared = xDistSquared + yDistSquared;
                    if (distSquared < lowestDistSquared) {
                        lowestDistSquared = distSquared;
                        nearestNeighborDistance[i] = Math.sqrt(distSquared);
                    }
                }
            }
        } // for (i = 0; i < circlesDrawn; i++)
        
       Arrays.sort(nearestNeighborDistance);
       total = 0.0;
       for (i = 0; i < circlesDrawn; i++) {
           total += nearestNeighborDistance[i];
       }
       mean = total/circlesDrawn;
       totalDeviateSquared = 0.0;
       totalDeviateCubed = 0.0;
       totalDeviateFourth = 0.0;
       for (i = 0; i < circlesDrawn; i++) {
           deviate = nearestNeighborDistance[i] - mean;
           deviateSquared = deviate * deviate;
           totalDeviateSquared += deviateSquared;
           deviateCubed = deviateSquared * deviate;
           totalDeviateCubed += deviateCubed;
           deviateFourth = deviateCubed * deviate;
           totalDeviateFourth += deviateFourth;
       }
       variance = totalDeviateSquared/(circlesDrawn - 1);
       stdDev = Math.sqrt(variance);
       // Skewness is a third standardized moment that measures the degree of symmetry of a probablility
       // distribution.  A distribution that is symmetrical has a skewness of zero.  If the skewness is 
       // positive, that means the right tail is heavier than the left tail.  If the skewness is negative,
       // then the left tail of the distribution is dominant.
       // skewness = E[(x - mean)**3]/(stdDev**3)
       // skewness = totalDeviateCubed/((stdDev**3)*(sample number - 1))
       // skewness = (sqrt(sample number - 1) * totalDeviateCubed)/(totalDeviateSquared**1.5)
       skewness = totalDeviateCubed/(Math.pow(stdDev, 3)* (circlesDrawn - 1));
       // Kurtosis, based on the fourth central moment, measures the thinness of tails or peakedness
       // of a probability distribution.  If kurtosis of a random variable is less than 3, the distribution
       // has thicker tails and a lower peak compared to a normal distribution.  Kurtosis larger than 3
       // indicates a higher peak than a Gaussian and thinner tails.
       // kurtosis = [(x - mean)**4]/(stdDev**4)
       // kurtosis = totalDeviateFourth/((stdDev**4) * (sample number - 1))
       // kurtosis = ((sample number - 1) * totalDeviateFourth)/(totalDeviateSquared**2)
       kurtosis = totalDeviateFourth/(Math.pow(stdDev, 4) * (circlesDrawn - 1));
       if ((circlesDrawn % 2) == 0) {
           // even number
           median = (nearestNeighborDistance[circlesDrawn/2 - 1] + nearestNeighborDistance[circlesDrawn/2])/2.0;
       }
       else {
           // odd number
           median = nearestNeighborDistance[(circlesDrawn - 1)/2];
       }
       Preferences.debug("Nearest neighbor statistics:\n ");
       System.out.println("Nearest neighbor statistics: ");
       Preferences.debug("Smallest distance = " + nearestNeighborDistance[0] + "\n");
       System.out.println("Smallest distance = " + nearestNeighborDistance[0]);
       Preferences.debug("Mean distance = " + mean + "\n");
       System.out.println("Mean distance = " + mean);
       Preferences.debug("Median distance = " + median + "\n");
       System.out.println("Median distance = " + median);
       Preferences.debug("Largest distance = " + nearestNeighborDistance[circlesDrawn-1] + "\n");
       System.out.println("Largest distance = " + nearestNeighborDistance[circlesDrawn-1]);
       Preferences.debug("Standard deviation = " + stdDev + "\n");
       System.out.println("Standard deviation = " + stdDev);
       Preferences.debug("Skewness = " + skewness + "\n");
       System.out.println("Skewness = " + skewness);
       Preferences.debug("Kurtosis = " + kurtosis + "\n");
       System.out.println("Kurtosis = " + kurtosis);
       
       // Test chi squared goodness of fit for a Gaussian with the calculated mean and standard deviation
       // The chi squared statistic has a number of degrees of freedom equal to the number of categories
       // minus 3.  Let's make 7 categories, so degrees of freedom = 4.
       // The 7 categories have lowest values of (nearestNeighborDistance - mean)/stdDev =
       // -infinity, -1.40, -0.80, -0.20, 0.40, 1.00, and 1.60.
       for (i = 0; i < circlesDrawn; i++) {
           z = (nearestNeighborDistance[i] - mean)/stdDev;
           if (z >= 1.60) {
               observedFrequency[6]++;
           }
           else if (z >= 1.00) {
               observedFrequency[5]++;
           }
           else if (z >= 0.40) {
               observedFrequency[4]++;
           }
           else if (z >= -0.20) {
               observedFrequency[3]++;
           }
           else if (z >= -0.80) {
               observedFrequency[2]++;
           }
           else if (z >= -1.40) {
               observedFrequency[1]++;
           }
           else {
               observedFrequency[0]++;
           }
       }
       
       theoreticalFrequency[0] = 0.0808 * circlesDrawn;
       theoreticalFrequency[1] = 0.1311 * circlesDrawn;
       theoreticalFrequency[2] = 0.2088 * circlesDrawn;
       theoreticalFrequency[3] = 0.2347 * circlesDrawn;
       theoreticalFrequency[4] = 0.1859 * circlesDrawn;
       theoreticalFrequency[5] = 0.1039 * circlesDrawn;
       theoreticalFrequency[6] = 0.0548 * circlesDrawn;
       chiSquaredOfFour = 0.0;
       for (i = 0; i < 7; i++) {
           deviate = observedFrequency[i] - theoreticalFrequency[i];
           chiSquaredOfFour += deviate * deviate / theoreticalFrequency[i];    
       }
       Preferences.debug("Chi squared fit for a gaussian for 4 df = " + chiSquaredOfFour + "\n");
       System.out.println("Chi squared fit for a gaussian for 4 df = " + chiSquaredOfFour);
       if (chiSquaredOfFour >= 14.86) {
           Preferences.debug("Gaussian rejected at the 0.5 percent level of signficance\n");
           System.out.println("Gaussian rejected at the 0.5 percent level of significance");
       }
       else if (chiSquaredOfFour >= 13.28) {
           Preferences.debug("Gaussian rejected at the 1.0 percent level of signficance\n");
           System.out.println("Gaussian rejected at the 1.0 percent level of significance");    
       }
       else if (chiSquaredOfFour >= 11.14) {
           Preferences.debug("Gaussian rejected at the 2.5 percent level of signficance\n");
           System.out.println("Gaussian rejected at the 2.5 percent level of significance");    
       }
       else if (chiSquaredOfFour >= 9.49) {
           Preferences.debug("Gaussian rejected at the 5.0 percent level of signficance\n");
           System.out.println("Gaussian rejected at the 5.0 percent level of significance");
       }
       else {
           Preferences.debug("Gaussian not rejected\n");
           System.out.println("Gaussian not rejected");
       }
       
       // Given a large number of observations, the Jarque-bera test can be used as a normality test.
       // The Jarque-Bera test, a type of Lagrange multiplier test, was developed to test normality,
       // heteroscedasticy, and serial correlation (autocorrelation) of regression residuals.  The
       // Jarque-Bera statistic is computed from skewness and kurtosis and asymptotically follows the
       // chi-squared distribution with 2 degress of freedom.
       // (sample number)*[skewness**2/6 + (kurtosis - 3)**2/24] follows a chi squared of 2 degrees of freedom
       // distribution.
       chiSquaredOfTwo = circlesDrawn * (skewness * skewness/6.0 + (kurtosis - 3.0) * (kurtosis - 3.0)/24.0);
       Preferences.debug("Jarque-Bera test using skewness and kurtosis yields a chi squared of 2 df = " 
                          + chiSquaredOfTwo + "\n");
       System.out.println("Jarque-Bera test using skewness and kurtosis yields a chi squared of 2 df = " 
                          + chiSquaredOfTwo);
       if (chiSquaredOfTwo >= 10.60) {
           Preferences.debug("Normality rejected at the 0.5 percent level of signficance\n");
           System.out.println("Normality rejected at the 0.5 percent level of significance");
       }
       else if (chiSquaredOfTwo >= 9.21) {
           Preferences.debug("Normality rejected at the 1.0 percent level of signficance\n");
           System.out.println("Normality rejected at the 1.0 percent level of significance");    
       }
       else if (chiSquaredOfTwo >= 7.38) {
           Preferences.debug("Normality rejected at the 2.5 percent level of signficance\n");
           System.out.println("Normality rejected at the 2.5 percent level of significance");    
       }
       else if (chiSquaredOfTwo >= 5.99) {
           Preferences.debug("Normality rejected at the 5.0 percent level of signficance\n");
           System.out.println("Normality rejected at the 5.0 percent level of significance");
       }
       else {
           Preferences.debug("Normality not rejected\n");
           System.out.println("Normality not rejected");
       }
       
       // The probability density function for the nearest neighbor distance when circles of a 
       // fixed radius are generated from a uniform random distribution is
       // p(r) = 0 for r < 2 * radius
       // p(r) = normFactor*2*PI*density*r*exp(-density*PI*r**2) for r >= 2*radius
       // Here the density is the density of all the generated circles whether they were drawn or not.
       // normFactor is the normalizing factor needed to make the integeral of p(r)dr from r = 0 to 
       // r = infinity be 1.  Here normFactor = exp(4*(radius**2)*density*PI)
       // theoretical Frequency = numCircles * normFactor * (exp(-density*PI*(smaller distance ** 2)) -
       //                                       exp(-density*PI*(larger distance ** 2)))
       density = circlesGenerated * Math.PI * radius * radius / length;
       normFactor = Math.exp(4.0*radius*radius*density*Math.PI);
       for (i = 0; i < 7; i++) {
           observedFrequency[i] = 0;
       }
       for (i = 0; i < circlesDrawn; i++) {
          // Generate an observed index from 0 to 6
           index = (int)((7*(nearestNeighborDistance[i] - nearestNeighborDistance[0]))/
                   (nearestNeighborDistance[circlesDrawn-1] - nearestNeighborDistance[0]));
           if (index > 6) {
               index = 6;
           }
           observedFrequency[index]++;
       } // for (i = 0; i < circlesDrawn; i++)
       oneSeventhRange = (nearestNeighborDistance[circlesDrawn-1] - nearestNeighborDistance[0])/7.0;
       chiSquaredOfFour = 0.0;
       for (i = 0; i < 7; i++) {
           smallerDistance = nearestNeighborDistance[0] + i * oneSeventhRange;
           largerDistance = nearestNeighborDistance[0] + (i+1) * oneSeventhRange;
           theoreticalFrequency[i] = numCircles * normFactor * (Math.exp(-density*Math.PI*smallerDistance*smallerDistance) -
                                     (Math.exp(-density*Math.PI*largerDistance*largerDistance)));
           deviate = observedFrequency[i] - theoreticalFrequency[i];
           chiSquaredOfFour += deviate * deviate / theoreticalFrequency[i];    
       }
       Preferences.debug("Chi squared fit for a uniform random distribution for 4 df = " + chiSquaredOfFour + "\n");
       System.out.println("Chi squared fit for a uniform random distribution for 4 df = " + chiSquaredOfFour);
       if (chiSquaredOfFour >= 14.86) {
           Preferences.debug("Uniform random distribution rejected at the 0.5 percent level of signficance\n");
           System.out.println("Uniform random distribution rejected at the 0.5 percent level of significance");
       }
       else if (chiSquaredOfFour >= 13.28) {
           Preferences.debug("Uniform random distribution rejected at the 1.0 percent level of signficance\n");
           System.out.println("Uniform random distribution rejected at the 1.0 percent level of significance");    
       }
       else if (chiSquaredOfFour >= 11.14) {
           Preferences.debug("Uniform random distribution rejected at the 2.5 percent level of signficance\n");
           System.out.println("Uniform random distribution rejected at the 2.5 percent level of significance");    
       }
       else if (chiSquaredOfFour >= 9.49) {
           Preferences.debug("Uniform random distribution rejected at the 5.0 percent level of signficance\n");
           System.out.println("Uniform random distribution rejected at the 5.0 percent level of significance");
       }
       else {
           Preferences.debug("Uniform random distribution not rejected\n");
           System.out.println("Uniform random distribution not rejected");
       }
       
        try {
            srcImage.importData(0, buffer, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on srcImage.importData(0, buffer, true");
            setCompleted(false);
            return;  
        }
        
        setCompleted(true);
        return;
    }
}
