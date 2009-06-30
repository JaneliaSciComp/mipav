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
 B. 200, 441-461, 1978.  Some of the math does not seem correct, but the general conclusion is that generation
 of circles from a uniform random distribution will result in a Rayleigh or Weibull distribution for the
 nearest neighbor distances.
 
  From Computational Methods in Biophysics, Biomaterials, BioTechnology, and Medical Systems
  Algorithm Development, Mathematical Analysis, and Diagnostics Volume 2 Computational Methods edited by Cornelius
  T. Leondes, Chapter 2 Computer techniques for spatial analysis of objects in biomedical images by
  G. Cevenini, M. R. Massia, and P. Barbini, Kluwer Academic Publishers, 2003, pp. 39-90, information about
  randomly distributed circles.
  "Analysis of the distribution W**2 of squared NN distances between n randomly distributed circles shows that
  2*lambda*PI*(W**2 - n*diameter**2)
  has a chi-squared distribution with 2*n degrees of freedom.  diameter is the average diameter of the circular objects,
  lambda is the mean number per unit area.
  X is a point to NN object distance.
  W is an inter-object NN distance.  Consider W as going from the center of one object to the center of
  the nearest object.
  The point-object squared NN distance distribution, X**2, is not affected by the reperesentation of objects
  as circles, thus 2*lambda*X**2 also has a chi-squared distribution with 2m degrees of freedom, where m is the
  number of selected points.  It means that, under the assumption of circular objecs of equal size, all tests 
  based on squared NN distances, like those of Hopkins, Brown, or others, can still be used.  If m = n, and these
  results are combined, an estimate of the mean radius of the objects as circles can be obtained as 
  rest = sqrt((W**2 - X**2)/n)
  Again, a Monte Carlo approach is sugested.  If rest is significantly different from the value measured directly
  on the image by automatic procedures of digital image processing or a value nevertheless compatible with the
  average physical size of the objects, we can sustain that the statistical pattern is not random.  If rest is high,
  it indicates regularity; if low, aggregation.  Moreover, since the formula for rest is formally a function of NN
  distances, we can again inspect classes of distances to obtain scale-related indications about the statistical
  pattern.
  
  Best reference for nearest neighbor distribution for equally sized circles is: "Nearest Neighbor Assessments of
  Spatial Configurations of Circles rather Than Points" by Daniel Simberloff, Ecology, Vol. 60, No. 4, Aug. 1979,
  pp. 679-685.
  d = diameter
  lambda = mean number per unit area
  mean nearest neighbor value = d + (exp(lambda * PI * d**2)/sqrt(lambda)*integral from t = d*sqrt(2*lambda*PI) to
  t = infinity of (1/sqrt(2*PI))*exp(-t**2/2)dt.  The integral is simply the 1 - Gaussian probability integral.
  E(nearest neighbor distance squared) = d**2 + 1/(lambda * PI)
  variance = E(r**2) - [E(r)]**2 = d**2 + 1/(lambda * PI) - [E(r)]**2
  standard error = sqrt(variance)/sqrt(N)
  Get percentile from Gaussian probability integral.
  If the measured mean is significantly greater than the analytical mean, the distribution is uniform or regular.
  If the measured mean is signficantly less than than the analytical mean, the distribution is clumped or 
  aggregated.
  
  Let x2 = chi squared and v = degrees of freedom
  The probability density function p(x2,v) = (1/((2**(v/2))* gamma(v/2)))*(x2**((v-2)/2))*exp(-x2/2)
  The probability of observing a value of chi square that is larger than a particular value for a random
  sample of N observations with v degrees of freedom is the integral of this probability from chi square = x2
  to chi square = infinity.
 */
public class AlgorithmCircleGeneration extends AlgorithmBase {
    
    public static final int RANDOM = 1;
    
    public static final int AGGREGATED = 2;
    
    public static final int REGULAR = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Circle radius
    private int radius;
    
    // number of circles to be drawn
    private int numCircles;
    
    // RANDOM, AGGREGATED, or REGULAR.
    private int pattern;
    
    // Used in AGGREGATED.  initialRandomCircles are drawn randomly.  The rest are drawn with nearestNeighborDistance
    // less than or equal to maximumNearestNeighborDistance
    private int initialRandomCircles;
    
    // Used in REGULAR
    private double minimumNearestNeighborDistance;
    
    // Used in AGGREGATED and REGULAR
    private double maximumNearestNeighborDistance;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmCircleGeneration - default constructor.
     */
    public AlgorithmCircleGeneration() { }

    /**
     * AlgorithmRandomCircleGeneration.
     *
     * @param  srcImg   Blank source image in which circles will be drawn
     * @param  radius   Circle radius
     * @param  numCircles Number of circles to be drawn
     * @param  pattern RANDOM, AGGREGATED, or REGULAR
     * @param  initialRandomCircles Used in AGGREGATED.  initialRandomCircles are drawn randomly.  The rest
     *         are drawn with nearestNeighborDistance less than or equal ot maximumNearestNeighborDistance.
     * @param  minimumNearestNeighborDistance Used in REGULAR
     * @param  maximumNearestNeighborDistance Used in AGGREGATED and REGULAR
     */
    public AlgorithmCircleGeneration(ModelImage srcImage, int radius, int numCircles, int pattern,
            int initialRandomCircles, double minimumNearestNeighborDistance, double maximumNearestNeighborDistance) {
        super(null, srcImage);
        this.radius = radius;
        this.numCircles = numCircles;
        this.pattern = pattern;
        this.initialRandomCircles = initialRandomCircles;
        this.minimumNearestNeighborDistance = minimumNearestNeighborDistance;
        this.maximumNearestNeighborDistance = maximumNearestNeighborDistance;
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
        int buffer[];
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
        double smallerDistance;
        double largerDistance;
        double oneSeventhRange;
        int boundaryDistance;
        int circlesLeft;
        int maskBytesSet;
        int circleBytesSet;
        double circlesAwayFromBoundary;
        double thirdSmallToMean;
        double a;
        double b;
        double theoreticalDistance[] = new double[8];
        double nearestNeighborDistanceSumOfSquares;
        double chiSquared;
        Statistics stat;
        double degreesOfFreedom;
        double chiSquaredPercentile[] = new double[1];
        double diameter;
        double integral[] = new double[1];
        double analyticalMean;
        double analyticalMeanSquared;
        double analyticalVariance;
        double analyticalStandardError;
        double percentile[] = new double[1];
        int numRandomCircles;
        double minimumNNDistanceSquared;
        double maximumNNDistanceSquared;
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Circle generation ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new int[length];
        // Create a mask for setting circles
        radiusSquared = radius * radius;
        xMaskDim = 2 * radius + 1;
        yMaskDim = xMaskDim;
        mask = new byte[xMaskDim * yMaskDim];
        maskBytesSet = 0;
        for (y = 0; y <= 2*radius; y++) {
            yDistSquared = (y - radius);
            yDistSquared = yDistSquared * yDistSquared;
            for (x = 0; x <= 2*radius; x++) {
                xDistSquared = (x - radius);
                xDistSquared = xDistSquared * xDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared <= radiusSquared) {
                    mask[x + y * xMaskDim] = 1;
                    maskBytesSet++;
                }
            }
        } // for (y = 0; y <= 2*radius; y++)
        
        minimumNNDistanceSquared = minimumNearestNeighborDistance * minimumNearestNeighborDistance;
        maximumNNDistanceSquared = maximumNearestNeighborDistance * maximumNearestNeighborDistance;
        
        randomGen = new RandomNumberGen();
        switch(pattern) {
            case RANDOM:
                numRandomCircles = numCircles;
                break;
            case AGGREGATED:
                numRandomCircles = initialRandomCircles;
                break;
            case REGULAR:
                numRandomCircles = 1;
                break;
            default:
                numRandomCircles = numCircles;
        }
        for (i = 1; i <= numRandomCircles; i++) {
        found = false;
        attempts = 0;
            while ((!found) && (attempts <= 100)) {
                found = true;
                xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                yloop:
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
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
                        buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  i;
                    }
                }
            }
        } // for (i = 1; i <= numRandomCircles; i++)
        circlesDrawn = i-1;
        if (circlesDrawn == 1) {
            Preferences.debug("1 random circle drawn.  1 random circle requested.\n");
            System.out.println("1 random circle drawn.  1 random circle requested.");    
        }
        else {
            Preferences.debug(circlesDrawn + " random circles drawn.  " + numCircles + " random circles requested.\n");
            System.out.println(circlesDrawn + " random circles drawn.  " + numCircles + " random circles requested.");
        }
        
        if ((pattern == AGGREGATED) && (circlesDrawn == initialRandomCircles)) {
            for (i = initialRandomCircles+1; i <= numCircles; i++) {
                found = false;
                attempts = 0;
                    while ((!found) && (attempts <= 1000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                        yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                        attemptloop:
                        {
                            for (y = 0; y <= 2*radius; y++) {
                                for (x = 0; x <= 2*radius; x++) {
                                    if (mask[x + y * xMaskDim] == 1) {
                                        if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                            found = false;
                                            attempts++;
                                            break attemptloop;
                                        }
                                    }
                                }
                            } // for (y = 0; y <= 2*radius; y++)
                            for (j = 0; j < i-1; j++) {         
                                xDistSquared = circleXCenter[j] - xCenter;
                                xDistSquared = xDistSquared * xDistSquared;
                                yDistSquared = circleYCenter[j] - yCenter;
                                yDistSquared = yDistSquared * yDistSquared;
                                distSquared = xDistSquared + yDistSquared;
                                if (distSquared <= maximumNNDistanceSquared) {
                                    break attemptloop;
                                }  
                            }
                            found = false;
                            attempts++;
                        } // attemptloop
                    } // while ((!found) && (attempts <= 1000)
                    if (!found) {
                        break;
                    }
                    circleXCenter[i-1] = xCenter;
                    circleYCenter[i-1] = yCenter;
                    for (y = 0; y <= 2*radius; y++) {
                        for (x = 0; x <= 2*radius; x++) {
                            if (mask[x + y * xMaskDim] == 1) {
                                buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  i;
                            }
                        }
                    }
                } // for (i = initialRandomCircles+1; i <= numCircles; i++)
                circlesDrawn = i-1; 
                Preferences.debug(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.\n");
                System.out.println(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.");
        } // if ((pattern == AGGREGATED) && (circlesDrawn == initialRandomCircles))
        
        if (pattern == REGULAR) {
            for (i = 2; i <= numCircles; i++) {
                found = false;
                attempts = 0;
                    while ((!found) && (attempts <= 1000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                        yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                        yyloop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break yyloop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                        lowestDistSquared = Integer.MAX_VALUE;
                        for (j = 0; j < i-1; j++) {         
                            xDistSquared = circleXCenter[j] - xCenter;
                            xDistSquared = xDistSquared * xDistSquared;
                            yDistSquared = circleYCenter[j] - yCenter;
                            yDistSquared = yDistSquared * yDistSquared;
                            distSquared = xDistSquared + yDistSquared;
                            if (distSquared < lowestDistSquared) {
                                lowestDistSquared = distSquared;
                            }  
                        }
                        if ((lowestDistSquared < minimumNNDistanceSquared) || 
                            (lowestDistSquared > maximumNNDistanceSquared)) {
                            found = false;
                            attempts++;
                        }  
                    } // while ((!found) && (attempts <= 1000)
                    if (!found) {
                        break;
                    }
                    circleXCenter[i-1] = xCenter;
                    circleYCenter[i-1] = yCenter;
                    for (y = 0; y <= 2*radius; y++) {
                        for (x = 0; x <= 2*radius; x++) {
                            if (mask[x + y * xMaskDim] == 1) {
                                buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  i;
                            }
                        }
                    }
                } // for (i = 2; i <= numCircles; i++)
                circlesDrawn = i-1; 
                Preferences.debug(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.\n");
                System.out.println(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.");    
        } // if (pattern == REGULAR)
        
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
        
       // Remember that nearest neighbor statistics will not hold near a boundary, so to be safe only consider those
       // circles at least the maximum nearestNeighborDistance aways from the boundary.  Otherswise, the maximum
       // nearest neighbor distance is artificially inflated by boundary effects.
       Preferences.debug("Before removing boundary influenced circles maximum nearest neighbor distance = " + 
                         nearestNeighborDistance[circlesDrawn - 1] + "\n");
       System.out.println("Before removing boundary influenced circles maximum nearest neighbor distance = " + 
               nearestNeighborDistance[circlesDrawn - 1]);
       boundaryDistance = (int)Math.ceil(nearestNeighborDistance[circlesDrawn - 1]);
       circlesLeft = 0;
       for (i = 0; i < circlesDrawn; i++) {
           if ((circleXCenter[i] >= boundaryDistance) && (circleXCenter[i] <= xDim - 1 - boundaryDistance) &&
               (circleYCenter[i] >= boundaryDistance) && (circleYCenter[i] <= yDim - 1 - boundaryDistance)) {
               circleXCenter[circlesLeft] = circleXCenter[i];
               circleYCenter[circlesLeft++] = circleYCenter[i];
           }
       }
       Preferences.debug("To avoid boundary effects only " + circlesLeft + " of the " + circlesDrawn + 
            " circles drawn will be analyzed\n");
       System.out.println("To avoid boundary effects only " + circlesLeft + " of the " + circlesDrawn + 
       " circles drawn will be analyzed\n");
       nearestNeighborDistance = new double[circlesLeft];
       for (i = 0; i < circlesLeft; i++) {
           lowestDistSquared = Integer.MAX_VALUE;
           for (j = 0; j < circlesLeft; j++) {
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
       } // for (i = 0; i < circlesLeft; i++)
       Arrays.sort(nearestNeighborDistance);
       total = 0.0;
       for (i = 0; i < circlesLeft; i++) {
           total += nearestNeighborDistance[i];
       }
       mean = total/circlesLeft;
       totalDeviateSquared = 0.0;
       totalDeviateCubed = 0.0;
       totalDeviateFourth = 0.0;
       for (i = 0; i < circlesLeft; i++) {
           deviate = nearestNeighborDistance[i] - mean;
           deviateSquared = deviate * deviate;
           totalDeviateSquared += deviateSquared;
           deviateCubed = deviateSquared * deviate;
           totalDeviateCubed += deviateCubed;
           deviateFourth = deviateCubed * deviate;
           totalDeviateFourth += deviateFourth;
       }
       variance = totalDeviateSquared/(circlesLeft - 1);
       stdDev = Math.sqrt(variance);
       // Skewness is a third standardized moment that measures the degree of symmetry of a probablility
       // distribution.  A distribution that is symmetrical has a skewness of zero.  If the skewness is 
       // positive, that means the right tail is heavier than the left tail.  If the skewness is negative,
       // then the left tail of the distribution is dominant.
       // skewness = E[(x - mean)**3]/(stdDev**3)
       // skewness = totalDeviateCubed/((stdDev**3)*(sample number - 1))
       // skewness = (sqrt(sample number - 1) * totalDeviateCubed)/(totalDeviateSquared**1.5)
       skewness = totalDeviateCubed/(Math.pow(stdDev, 3)* (circlesLeft - 1));
       // Kurtosis, based on the fourth central moment, measures the thinness of tails or peakedness
       // of a probability distribution.  If kurtosis of a random variable is less than 3, the distribution
       // has thicker tails and a lower peak compared to a normal distribution.  Kurtosis larger than 3
       // indicates a higher peak than a Gaussian and thinner tails.
       // kurtosis = [(x - mean)**4]/(stdDev**4)
       // kurtosis = totalDeviateFourth/((stdDev**4) * (sample number - 1))
       // kurtosis = ((sample number - 1) * totalDeviateFourth)/(totalDeviateSquared**2)
       kurtosis = totalDeviateFourth/(Math.pow(stdDev, 4) * (circlesLeft - 1));
       if ((circlesLeft % 2) == 0) {
           // even number
           median = (nearestNeighborDistance[circlesLeft/2 - 1] + nearestNeighborDistance[circlesLeft/2])/2.0;
       }
       else {
           // odd number
           median = nearestNeighborDistance[(circlesLeft - 1)/2];
       }
       Preferences.debug("Nearest neighbor statistics:\n ");
       System.out.println("Nearest neighbor statistics: ");
       Preferences.debug("Smallest distance = " + nearestNeighborDistance[0] + "\n");
       System.out.println("Smallest distance = " + nearestNeighborDistance[0]);
       Preferences.debug("Mean distance = " + mean + "\n");
       System.out.println("Mean distance = " + mean);
       Preferences.debug("Median distance = " + median + "\n");
       System.out.println("Median distance = " + median);
       Preferences.debug("Largest distance = " + nearestNeighborDistance[circlesLeft-1] + "\n");
       System.out.println("Largest distance = " + nearestNeighborDistance[circlesLeft-1]);
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
       for (i = 0; i < circlesLeft; i++) {
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
       
       theoreticalFrequency[0] = 0.0808 * circlesLeft;
       theoreticalFrequency[1] = 0.1311 * circlesLeft;
       theoreticalFrequency[2] = 0.2088 * circlesLeft;
       theoreticalFrequency[3] = 0.2347 * circlesLeft;
       theoreticalFrequency[4] = 0.1859 * circlesLeft;
       theoreticalFrequency[5] = 0.1039 * circlesLeft;
       theoreticalFrequency[6] = 0.0548 * circlesLeft;
       chiSquaredOfFour = 0.0;
       for (i = 0; i < 7; i++) {
           deviate = observedFrequency[i] - theoreticalFrequency[i];
           chiSquaredOfFour += deviate * deviate / theoreticalFrequency[i];    
       }
       Preferences.debug("Chi squared for a gaussian fit on mean and standard deviation for 4 df = "
                          + chiSquaredOfFour + "\n");
       System.out.println("Chi squared for a gaussian fit on mean and standard deviation for 4 df = "
                          + chiSquaredOfFour);
       degreesOfFreedom = 4;
       stat = new Statistics(Statistics.CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION,
               chiSquaredOfFour, degreesOfFreedom, chiSquaredPercentile);
       stat.run();
       
       Preferences.debug("ChiSquared percentile for Gaussian fit on mean and standard deviation = " +
                         chiSquaredPercentile[0]*100.0 + "\n");
       System.out.println("chiSquared percentile for Gaussian fit on mean and standard deviation = " +
                          chiSquaredPercentile[0]*100.0);
       if (chiSquaredPercentile[0] >= 0.95) {
           Preferences.debug("chiSquared test rejects Gaussian fit on mean and standard deviation at a " +
                   (100.0 - chiSquaredPercentile[0]*100.0) + " level of signficance\n");
           System.out.println("chiSquared test rejects Gaussian fit on mean and standard deviation at a " +
                   (100.0 - chiSquaredPercentile[0]*100.0) + " level of signficance"); 
       }
       else {
           Preferences.debug("chiSquared test does not reject Gaussian fit on mean and standard deviation\n");
           System.out.println("chiSquared test does not reject Gaussian fit on mean and standard deviation");
       }
       
       // Given a large number of observations, the Jarque-bera test can be used as a normality test.
       // The Jarque-Bera test, a type of Lagrange multiplier test, was developed to test normality,
       // heteroscedasticy, and serial correlation (autocorrelation) of regression residuals.  The
       // Jarque-Bera statistic is computed from skewness and kurtosis and asymptotically follows the
       // chi-squared distribution with 2 degress of freedom.
       // (sample number)*[skewness**2/6 + (kurtosis - 3)**2/24] follows a chi squared of 2 degrees of freedom
       // distribution.
       chiSquaredOfTwo = circlesLeft * (skewness * skewness/6.0 + (kurtosis - 3.0) * (kurtosis - 3.0)/24.0);
       Preferences.debug("Jarque-Bera test using skewness and kurtosis yields a chi squared for 2 df = " 
                          + chiSquaredOfTwo + "\n");
       System.out.println("Jarque-Bera test using skewness and kurtosis yields a chi squared for 2 df = " 
                          + chiSquaredOfTwo);
       degreesOfFreedom = 2;
       stat = new Statistics(Statistics.CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION,
               chiSquaredOfTwo, degreesOfFreedom, chiSquaredPercentile);
       stat.run();
       Preferences.debug("chiSquared percentile for Jarque-Bera test using skewness and kurtosis = " +
                         chiSquaredPercentile[0]*100.0 + "\n");
       System.out.println("chiSquared percentile for Jarque-Bera test using skewness and kurtosis = " +
                          chiSquaredPercentile[0]*100.0);
       if (chiSquaredPercentile[0] >= 0.95) {
           Preferences.debug("chiSquared test rejects Gaussian fit based on skewness and kurtosis at a " +
                   (100.0 - chiSquaredPercentile[0]*100.0) + " level of signficance\n");
           System.out.println("chiSquared test rejects Gaussian fit based on skewness and kurtosis at a  " +
                   (100.0 - chiSquaredPercentile[0]*100.0) + " level of signficance"); 
       }
       else {
           Preferences.debug("chiSquared test does not reject Gaussian fit based on skewness and kurtosis\n");
           System.out.println("chiSquared test does not reject Gaussian fit based on skewness and kurtosis");
       }
       
       // The probability density function for the nearest neighbor distance when circles of a 
       // fixed radius are generated from a uniform random distribution is a Rayleigh or
       // Weibull distribution.
       // The probability density function is:
       // p(r) = (2/b)*(r - a)*exp(-(r - a)*(r - a)/b) for r >= a
       // p(r) = 0 for r < a
       // The cumulative function is:
       // P(r) = 1 - exp(-(r - a)*(r - a)/b) for r >= a
       // P(r) = 0 for r < a
       // mean = a + sqrt(PI* b/4)
       // variance = b*(4 - PI)/4
       // Expect a = 2 * radius or a = nearestNeighborDistance[0], b = 1/(PI * density)
       // Take a = nearestNeighborDistance[0].
       nearestNeighborDistanceSumOfSquares = 0.0;
       for (i = 0; i < circlesLeft; i++) {
           nearestNeighborDistanceSumOfSquares += nearestNeighborDistance[i]*nearestNeighborDistance[i];
       }
       density = (double)circlesLeft/(double)((xDim - 2 * boundaryDistance) * (yDim - 2 * boundaryDistance));
       diameter = 2.0 * radius;
       
       // Calculate analytical mean
       z = -diameter*Math.sqrt(2.0 * density * Math.PI);
       stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z, circlesLeft-1, integral);
       stat.run();
       analyticalMean = diameter + Math.exp(density*Math.PI*diameter*diameter)*integral[0]/Math.sqrt(density);
       Preferences.debug("Analytical mean = " + analyticalMean + "\n");
       System.out.println("Analytical mean = " + analyticalMean);
       analyticalMeanSquared = diameter*diameter + 1.0/(density*Math.PI);
       analyticalVariance = analyticalMeanSquared - analyticalMean*analyticalMean;
       analyticalStandardError = Math.sqrt(analyticalVariance/circlesLeft);
       z = (mean - analyticalMean)/analyticalStandardError;
       stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z, circlesLeft-1, percentile);
       stat.run();
       Preferences.debug("Percentile in Gaussian probability integral for measured mean around analytical mean = "
                         + percentile[0]*100.0 + "\n");
       System.out.println("Percentile in Gaussian probability integral for measured mean around analytical mean = " +
                           percentile[0]*100.0);
       if (percentile[0] < 0.025) {
           // Measured mean signficantly less than analytical mean of random distribution
           Preferences.debug("Clumping or aggregation found in nearest neighbor distances\n");
           System.out.println("Clumping or arrgrgation found in nearest neighbor distances");
       }
       else if (percentile[0] > 0.975) {
           // Measured mean significantly greater than analytical mean of random distribution
           Preferences.debug("Uniform or regular distribution found in nearest neighbor distances\n");
           System.out.println("Uniform or regular distribution found in nearest neighbor distances");
       }
       else {
         // Measured mean not significantly different from analytical mean of random distribution
           Preferences.debug("Measured mean consistent with random distribution\n");
           System.out.println("Measured mean consistent with random distribution");
       }
       chiSquared = 2.0 * density * Math.PI * (nearestNeighborDistanceSumOfSquares - circlesLeft * diameter * diameter);
       Preferences.debug("chiSquared for sum of squared NN distances of " + circlesLeft + " circles = " +
                         chiSquared + "\n");
       System.out.println("chiSquared for sum of squared NN distances of " + circlesLeft + " circles = " +
                         chiSquared);
       degreesOfFreedom = 2 * circlesLeft;
       
       stat = new Statistics(Statistics.CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION,
               chiSquared, degreesOfFreedom, chiSquaredPercentile);
       stat.run();
       Preferences.debug("chiSquared percentile for sum of squared NN distances = " + chiSquaredPercentile[0]*100.0 + "\n");
       System.out.println("chiSquared percentile for sum of squared NN distances = " + chiSquaredPercentile[0]*100.0);
       if (chiSquaredPercentile[0] < 0.025) {
           Preferences.debug("chiSquared test consistent with aggregated nearest neighbor distribution\n");
           System.out.println("chiSquared test consistent with aggregated nearest neighbor distribution");
       }
       else if (chiSquaredPercentile[0] >= 0.975) {
           Preferences.debug("chiSquared test consistent with uniform nearest neighbor distribution\n");
           System.out.println("chiSquared tests consistent with uniform nearest neighbor distribution"); 
       }
       else {
           Preferences.debug("chiSquared test does not reject random circle distribution\n");
           System.out.println("chiSquared test does not reject random circle distribution");
       }
       
       for (i = 0; i < buffer.length; i++) {
           if (buffer[i] > 0) {
               buffer[i] = 1;
           }
       }
       try {
           srcImage.importData(0, buffer, true);
       }
       catch(IOException e) {
           MipavUtil.displayError("IO exception on srcImage.importData(0, buffer, true)");
           setCompleted(false);
           return;
       }
       
       setCompleted(true);
       return;
    }
}
