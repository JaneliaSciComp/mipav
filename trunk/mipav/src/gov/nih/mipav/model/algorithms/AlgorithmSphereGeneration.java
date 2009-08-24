package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

/**
 * This module draws uniformly randomly positioned spheres with a specified radius.
  
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
  
  I have derived similar mathematics for Poisson distributed spheres.  
  Consider a Poisson distribution of points in spheres of radius r.  The probability that a sphere of radius r
  about a point contains no points, given a density of points = lambda is exp(-(4/3)*lambda*PI*(r**3))
  Thus, 1 - exp(-(4/3)*lambda*PI*(r**3)) is the proportion of nearest neighbor distances <= r.
  Differentiate this expression with respect to r to obtain as the probability density function of r,
  4*lambda*PI*(r**2)*exp(-(4/3)*lambda*PI*(r**3)).  Now find the probability density function for spheres
  of diameter dm.
  For 0 < r <= dm, 0
  For r > dm: 4*lambda*PI*(r**2)*exp(-(4/3)*lambda*PI*(r**3)) * 
             (1 / ( 1 - 4*lambda*PI*integral from r = 0 to r = dm of (r**2)*exp(-(4/3)*lambda*PI*(r**3))dr)) =
             
             4*lambda*PI*(r**2)*exp(-(4/3)*lambda*PI*(r**3)) *
             (1/ ( 1 + integral from r = 0 to r = dm of exp(-(4/3)*lambda*PI*(r**3))d(-(4/3)*lambda*PI*(r**3)))) =
             
             4*lambda*PI*(r**2)*exp(-(4/3)*lambda*PI*(r**3)) *
             (1 /(1 + exp(-(4/3)*lambda*PI*(dm**3)) - 1)) =
             
              4*lambda*PI*exp((4/3)*lambda*PI*(dm**3))*(r**2)*exp(-(4/3)*lambda*PI*(r**3))
              
 E(r) = 4*lambda*PI*exp((4/3)*lambda*PI*(dm**3)) *
        integral from r = dm to r = infinity of (r**3)*exp(-(4/3)*lambda*PI*(r**3))dr =
        
        -exp((4/3)*lambda*PI*(dm**3)) *
        integral from r = dm to r = infinity of rd(exp(-(4/3)*lambda*PI*(r**3)))
        
        -exp((4/3)*lambda*PI*(dm**3)) *
        (-dm*exp(-(4/3)*lambda*PI*(dm**3)) - intergal from r = dm to r = infinity of exp(-(4/3)*lambda*PI*(r**3))dr =
        
        dm + exp((4/3)*lambda*PI*(dm**3))*integral from r = dm to r = infinity of exp(-(4/3)*lambda*PI*(r**3))dr
        
 E(r**2) = 4*lambda*PI*exp((4/3)*lambda*PI*(dm**3)) * 
           integral from r = dm to r = infinity of (r**4)*exp(-(4/3)*lambda*PI*(r**3))dr =
           
           -exp((4/3)*lambda*PI*(dm**3)) *
           integral from r = dm to r = infinity of (r**2)*d(exp(-(4/3)*lambda*PI*(r**3))) =
           
           -exp((4/3)*lambda*PI*(dm**3) *
           (-(dm**2)*exp(-(4/3)*lambda*PI*(dm**3)) 
           - integral from r = dm to r = infinity of exp(-(4/3)*lambda*PI*(r**3))d(r**2) =
           
           dm**2 + exp((4/3)*lambda*PI*(dm**3)) * 
           integral from r = dm to r = infinity of 2*r*exp(-(4/3)*lambda*PI*(r**3))dr
           
           
             
 */
public class AlgorithmSphereGeneration extends AlgorithmBase {
    
    public static final int RANDOM = 1;
    
    // The first initialRandomSpheres are generated at random.  The remaining spheres are only accepted if the 
    // nearest neighbor distance is <= maximumNearestNeighborDistance.
    public static final int AGGREGATED = 2;
    
    // Regular patterns can arise from inhibition or repulsion mechanisms which constrain objects to remain a
    // certain distance from each other.  The first sphere is generated at random.  All other spheres are only
    // accepted if the nearest neighbor distance is >= minimumNearestNeighborDistance and 
    // <= maximumNearestNeighborDistance.
    public static final int REGULAR = 3;
    
    // Very small and large distances between neighboring objects are allowed, but not intermediate distances.
    // Such constrained patterns are found in nature due to growth patterns.
    // The first sphere is generated at random.  The remaining spheres are only accepted if the nearest neighbor
    // distance is less than the lowestForbiddenNNDistance or greater than the highestForbiddenNNDistance.  For
    // each objected rejected from being in the forbidden intermediate range, the new replacement generated sphere
    // must have a value greater than the highestForbiddenNNDistance and less than or equal to the 
    // highestRegenerationNNDistance.  The pattern obtained is not distinguishable from randomness for NN distances
    // less than the lowestForbiddenNNDistance and greater than the highestRegenerationNNDistance, but is regular
    // in the range from the lowestForbiddenNNDistance to the highestRegenerationNNDistnace with a peak of 
    // significance at an NN Distance just above the hgihestForbiddenNNDistance.
    public static final int CONSTRAINED = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Sphere radius
    private int radius;
    
    // number of sphere to be drawn
    private int numSpheres;
    
    // RANDOM, AGGREGATED, or REGULAR.
    private int pattern;
    
    // Used in AGGREGATED.  initialRandomSpheres are drawn randomly.  The rest are drawn with nearestNeighborDistance
    // less than or equal to maximumNearestNeighborDistance
    private int initialRandomSpheres;
    
    // Used in REGULAR
    private double minimumNearestNeighborDistance;
    
    // Used in AGGREGATED and REGULAR
    private double maximumNearestNeighborDistance;
    
    private double lowestForbiddenNNDistance;
    
    private double highestForbiddenNNDistance;
    
    private double highestRegenerationNNDistance;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmSphereGeneration - default constructor.
     */
    public AlgorithmSphereGeneration() { }

    /**
     * AlgorithmRandomSphereGeneration.
     *
     * @param  srcImg   Blank source image in which spheres will be drawn
     * @param  radius   Sphere radius
     * @param  numSpheres Number of spheres to be drawn
     * @param  pattern RANDOM, AGGREGATED, or REGULAR
     * @param  initialRandomSpheres Used in AGGREGATED.  initialRandomSpheres are drawn randomly.  The rest
     *         are drawn with nearestNeighborDistance less than or equal ot maximumNearestNeighborDistance.
     * @param  minimumNearestNeighborDistance Used in REGULAR
     * @param  maximumNearestNeighborDistance Used in AGGREGATED and REGULAR
     * @param  lowestForbiddenNNDistance Used in CONSTRAINED
     * @param  highestForbiddenNNDistance Used in CONSTRAINED
     * @param  highestRegeneerationNNDistance Used in CONSTRAINED
     */
    public AlgorithmSphereGeneration(ModelImage srcImage, int radius, int numSpheres, int pattern,
            int initialRandomSpheres, double minimumNearestNeighborDistance, double maximumNearestNeighborDistance,
            double lowestForbiddenNNDistance, double highestForbiddenNNDistance, double highestRegenerationNNDistance) {
        super(null, srcImage);
        this.radius = radius;
        this.numSpheres = numSpheres;
        this.pattern = pattern;
        this.initialRandomSpheres = initialRandomSpheres;
        this.minimumNearestNeighborDistance = minimumNearestNeighborDistance;
        this.maximumNearestNeighborDistance = maximumNearestNeighborDistance;
        this.lowestForbiddenNNDistance = lowestForbiddenNNDistance;
        this.highestForbiddenNNDistance = highestForbiddenNNDistance;
        this.highestRegenerationNNDistance = highestRegenerationNNDistance;
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
        int zDim;
        int xySize;
        byte mask[];
        int x;
        int y;
        int zint;
        int yDistSquared;
        int xDistSquared;
        int zDistSquared;
        int radiusSquared;
        int xMaskDim;
        int yMaskDim;
        int zMaskDim;
        int xyMask;
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
        int zCenter = radius;
        /** Reference to the random number generator. */
        RandomNumberGen randomGen;
        int spheresDrawn;
        int sphereXCenter[] = new int[numSpheres];
        int sphereYCenter[] = new int[numSpheres];
        int sphereZCenter[] = new int[numSpheres];
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
        double density;
        double z;
        int boundaryDistance;
        int spheresLeft;
        int maskBytesSet;
        double nearestNeighborDistanceSumOfSquares;
        Statistics stat;
        double diameter;
        double analyticalMean;
        double analyticalMeanSquared;
        double analyticalVariance;
        double analyticalStandardError;
        double percentile[] = new double[1];
        int numRandomSpheres;
        double minimumNNDistanceSquared;
        double maximumNNDistanceSquared;
        double lowestForbiddenSquared;
        double highestForbiddenSquared;
        double highestRegenerationSquared;
        boolean intermediateRejected;
        IntModelMean meanModel;
        IntModelMeanSquared meanSquaredModel;
        int steps;
        double numInt;
        double eps = 1.0e-12;
        IntModelMean2 meanModel2;
        IntModelMeanSquared2 meanSquaredModel2;
        /** finite bound of integration range used in dqagie (has no meaning if interval is doubly-infinite). */
        double bound;
        /** Integral over (bound, +infinity), (-infinity, bound), or (-infinity, +infinity). */
        int routine = Integration2.DQAGIE;
        /**
         * In dqagie indicates the kind of integration range involved inf = 1 corresponds to (bound, +infinity) inf = -1
         * corresponds to (-infinity, bound) inf = 2 corresponds to (-infinity, +infinity).
         */
        int inf = 1;
        double epsabs = 0.0;
        double epsrel = 1.0E-3;
        /**
         * Gives an upper bound on the number of subintervals in the partition of lower, upper.
         */
        int limit = 100;
        double numInt2;
        int errorStatus;
        double absError;
        int neval;
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Sphere generation ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        xySize = xDim * yDim;
        length = xySize * zDim;
        buffer = new int[length];
        // Create a mask for setting spheres
        radiusSquared = radius * radius;
        xMaskDim = 2 * radius + 1;
        yMaskDim = xMaskDim;
        zMaskDim = xMaskDim;
        xyMask = xMaskDim * yMaskDim;
        mask = new byte[xMaskDim * yMaskDim * zMaskDim];
        maskBytesSet = 0;
        for (zint = 0; zint <= 2*radius; zint++) {
            zDistSquared = zint - radius;
            zDistSquared = zDistSquared * zDistSquared;
            for (y = 0; y <= 2*radius; y++) {
                yDistSquared = (y - radius);
                yDistSquared = yDistSquared * yDistSquared;
                for (x = 0; x <= 2*radius; x++) {
                    xDistSquared = (x - radius);
                    xDistSquared = xDistSquared * xDistSquared;
                    distSquared = xDistSquared + yDistSquared + zDistSquared;
                    if (distSquared <= radiusSquared) {
                        mask[x + y * xMaskDim + zint * xyMask] = 1;
                        maskBytesSet++;
                    }
                }
            } // for (y = 0; y <= 2*radius; y++)
        } // for (zint = 0; zint <= 2 * radius; zint++)
        
        minimumNNDistanceSquared = minimumNearestNeighborDistance * minimumNearestNeighborDistance;
        maximumNNDistanceSquared = maximumNearestNeighborDistance * maximumNearestNeighborDistance;
        lowestForbiddenSquared = lowestForbiddenNNDistance * lowestForbiddenNNDistance;
        highestForbiddenSquared = highestForbiddenNNDistance * highestForbiddenNNDistance;
        highestRegenerationSquared = highestRegenerationNNDistance * highestRegenerationNNDistance;
        
        randomGen = new RandomNumberGen();
        switch(pattern) {
            case RANDOM:
                numRandomSpheres = numSpheres;
                break;
            case AGGREGATED:
                numRandomSpheres = initialRandomSpheres;
                break;
            case REGULAR:
            case CONSTRAINED:
                numRandomSpheres = 1;
                break;
            default:
                numRandomSpheres = numSpheres;
        }
        for (i = 1; i <= numRandomSpheres; i++) {
        found = false;
        attempts = 0;
            while ((!found) && (attempts <= 1000)) {
                found = true;
                xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                zCenter = randomGen.genUniformRandomNum(radius, zDim - radius - 1);
                zloop:
                for (zint = 0; zint <= 2*radius; zint++) {
                    for (y = 0; y <= 2*radius; y++) {
                        for (x = 0; x <= 2*radius; x++) {
                            if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                                if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)
                                           + xySize * (zCenter + zint - radius)] != 0) {
                                    found = false;
                                    attempts++;
                                    break zloop;
                                }
                            }
                        }
                    } // for (y = 0; y <= 2*radius; y++)
                } // for (zint = 0; zint <= 2*radius; zint++)
            } // while ((!found) && (attempts <= 1000)
            if (!found) {
                break;
            }
            sphereXCenter[i-1] = xCenter;
            sphereYCenter[i-1] = yCenter;
            sphereZCenter[i-1] = zCenter;
            for (zint = 0; zint <= 2*radius; zint++) {
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius) +
                                   xySize * (zCenter + zint - radius)] =  i;
                        }
                    }
                }
            }
        } // for (i = 1; i <= numRandomSpheres; i++)
        spheresDrawn = i-1;
        if (spheresDrawn == 1) {
            Preferences.debug("1 random sphere drawn.  1 random sphere requested.\n");
            System.out.println("1 random sphere drawn.  1 random sphere requested.");    
        }
        else {
            Preferences.debug(spheresDrawn + " random spheres drawn.  " + numSpheres + " random spheres requested.\n");
            System.out.println(spheresDrawn + " random spheres drawn.  " + numSpheres + " random spheres requested.");
        }
        
        if ((pattern == AGGREGATED) && (spheresDrawn == initialRandomSpheres)) {
            for (i = initialRandomSpheres+1; i <= numSpheres; i++) {
                found = false;
                attempts = 0;
                    while ((!found) && (attempts <= 10000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                        yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                        zCenter = randomGen.genUniformRandomNum(radius, zDim - radius - 1);
                        attemptloop:
                        {
                            for (zint = 0; zint <= 2*radius; zint++) {
                                for (y = 0; y <= 2*radius; y++) {
                                    for (x = 0; x <= 2*radius; x++) {
                                        if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                                            if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius) +
                                                       xySize * (zCenter + zint - radius)] != 0) {
                                                found = false;
                                                attempts++;
                                                break attemptloop;
                                            }
                                        }
                                    }
                                } // for (y = 0; y <= 2*radius; y++)
                            } // for(zint = 0; zint <= 2*radius; zint++)
                            for (j = 0; j < i-1; j++) {         
                                xDistSquared = sphereXCenter[j] - xCenter;
                                xDistSquared = xDistSquared * xDistSquared;
                                yDistSquared = sphereYCenter[j] - yCenter;
                                yDistSquared = yDistSquared * yDistSquared;
                                zDistSquared = sphereZCenter[j] - zCenter;
                                zDistSquared = zDistSquared * zDistSquared;
                                distSquared = xDistSquared + yDistSquared + zDistSquared;
                                if (distSquared <= maximumNNDistanceSquared) {
                                    break attemptloop;
                                }  
                            }
                            found = false;
                            attempts++;
                        } // attemptloop
                    } // while ((!found) && (attempts <= 10000)
                    if (!found) {
                        break;
                    }
                    sphereXCenter[i-1] = xCenter;
                    sphereYCenter[i-1] = yCenter;
                    sphereZCenter[i-1] = zCenter;
                    for (zint = 0; zint <= 2*radius; zint++) {
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                                    buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)+
                                           xySize *(zCenter + zint - radius)] =  i;
                                }
                            }
                        }
                    }
                } // for (i = initialRandomSpheres+1; i <= numSpheres; i++)
                spheresDrawn = i-1; 
                Preferences.debug(spheresDrawn + " spheres drawn.  " + numSpheres + " spheres requested.\n");
                System.out.println(spheresDrawn + " spheres drawn.  " + numSpheres + " spheres requested.");
        } // if ((pattern == AGGREGATED) && (spheresDrawn == initialRandomSpheres))
        
        if (pattern == REGULAR) {
            for (i = 2; i <= numSpheres; i++) {
                found = false;
                attempts = 0;
                wloop:
                    while ((!found) && (attempts <= 10000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                        yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                        zCenter = randomGen.genUniformRandomNum(radius, zDim - radius - 1);
                        for (zint = 0; zint <= 2*radius; zint++) {
                            for (y = 0; y <= 2*radius; y++) {
                                for (x = 0; x <= 2*radius; x++) {
                                    if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                                        if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)+
                                                   xySize * (zCenter + zint - radius)] != 0) {
                                            found = false;
                                            attempts++;
                                            continue wloop;
                                        }
                                    }
                                }
                            } // for (y = 0; y <= 2*radius; y++)
                        } // for (zint = 0; zint <= 2*radius; zint++)
                        lowestDistSquared = Integer.MAX_VALUE;
                        for (j = 0; j < i-1; j++) {         
                            xDistSquared = sphereXCenter[j] - xCenter;
                            xDistSquared = xDistSquared * xDistSquared;
                            yDistSquared = sphereYCenter[j] - yCenter;
                            yDistSquared = yDistSquared * yDistSquared;
                            zDistSquared = sphereZCenter[j] - zCenter;
                            zDistSquared = zDistSquared * zDistSquared;
                            distSquared = xDistSquared + yDistSquared + zDistSquared;
                            if (distSquared < lowestDistSquared) {
                                lowestDistSquared = distSquared;
                            }  
                        }
                        if ((lowestDistSquared < minimumNNDistanceSquared) || 
                            (lowestDistSquared > maximumNNDistanceSquared)) {
                            found = false;
                            attempts++;
                        }  
                    } // while ((!found) && (attempts <= 10000)
                    if (!found) {
                        break;
                    }
                    sphereXCenter[i-1] = xCenter;
                    sphereYCenter[i-1] = yCenter;
                    sphereZCenter[i-1] = zCenter;
                    for (zint = 0; zint <= 2*radius; zint++) {
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                                    buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius) +
                                           xySize * (zCenter + zint - radius)] =  i;
                                }
                            }
                        }
                    }
                } // for (i = 2; i <= numSpheres; i++)
                spheresDrawn = i-1; 
                Preferences.debug(spheresDrawn + " spheres drawn.  " + numSpheres + " spheres requested.\n");
                System.out.println(spheresDrawn + " spheres drawn.  " + numSpheres + " spheres requested.");    
        } // if (pattern == REGULAR)
        
        if (pattern == CONSTRAINED) {
            for (i = 2; i <= numSpheres; i++) {
                found = false;
                attempts = 0;
                intermediateRejected = false;
                wl:
                    while ((!found) && (attempts <= 10000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                        yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                        zCenter = randomGen.genUniformRandomNum(radius, zDim - radius - 1);
                        for (zint = 0; zint <= 2*radius; zint++) {
                            for (y = 0; y <= 2*radius; y++) {
                                for (x = 0; x <= 2*radius; x++) {
                                    if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                                        if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius) +
                                                   xySize * (zCenter + zint - radius)] != 0) {
                                            found = false;
                                            attempts++;
                                            continue wl;
                                        }
                                    }
                                }
                            } // for (y = 0; y <= 2*radius; y++)
                        } // for (zint = 0; zint <= 2*radius; zint++)
                        lowestDistSquared = Integer.MAX_VALUE;
                        for (j = 0; j < i-1; j++) {         
                            xDistSquared = sphereXCenter[j] - xCenter;
                            xDistSquared = xDistSquared * xDistSquared;
                            yDistSquared = sphereYCenter[j] - yCenter;
                            yDistSquared = yDistSquared * yDistSquared;
                            zDistSquared = sphereZCenter[j] - zCenter;
                            zDistSquared = zDistSquared * zDistSquared;
                            distSquared = xDistSquared + yDistSquared + zDistSquared;
                            if (distSquared < lowestDistSquared) {
                                lowestDistSquared = distSquared;
                            }  
                        }
                        if ((!intermediateRejected) && (lowestDistSquared >= lowestForbiddenSquared) && 
                            (lowestDistSquared <= highestForbiddenSquared)) {
                            found = false;
                            intermediateRejected = true;
                            attempts++;
                        } 
                        else if (intermediateRejected && ((lowestDistSquared <= highestForbiddenSquared) ||
                                (lowestDistSquared > highestRegenerationSquared))) {
                            found = false;
                            attempts++;
                        }
                    } // while ((!found) && (attempts <= 10000)
                    if (!found) {
                        break;
                    }
                    sphereXCenter[i-1] = xCenter;
                    sphereYCenter[i-1] = yCenter;
                    sphereZCenter[i-1] = zCenter;
                    for (zint = 0; zint <= 2*radius; zint++) {
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim + zint * xyMask] == 1) {
                                    buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius) +
                                           xySize * (zCenter + zint - radius)] =  i;
                                }
                            }
                        }
                    }
                } // for (i = 2; i <= numSpheres; i++)
                spheresDrawn = i-1; 
                Preferences.debug(spheresDrawn + " spheres drawn.  " + numSpheres + " spheres requested.\n");
                System.out.println(spheresDrawn + " spheres drawn.  " + numSpheres + " spheres requested.");     
        } // if (pattern == CONSTRAINED)
        
        nearestNeighborDistance = new double[spheresDrawn];
        for (i = 0; i < spheresDrawn; i++) {
            lowestDistSquared = Integer.MAX_VALUE;
            for (j = 0; j < spheresDrawn; j++) {
                if (i != j) {          
                    xDistSquared = sphereXCenter[i] - sphereXCenter[j];
                    xDistSquared = xDistSquared * xDistSquared;
                    yDistSquared = sphereYCenter[i] - sphereYCenter[j];
                    yDistSquared = yDistSquared * yDistSquared;
                    zDistSquared = sphereZCenter[i] - sphereZCenter[j];
                    zDistSquared = zDistSquared * zDistSquared;
                    distSquared = xDistSquared + yDistSquared + zDistSquared;
                    if (distSquared < lowestDistSquared) {
                        lowestDistSquared = distSquared;
                        nearestNeighborDistance[i] = Math.sqrt(distSquared);
                    }  
                }
            }
        } // for (i = 0; i < spheresDrawn; i++)
        
       // Remember that nearest neighbor statistics will not hold near a boundary, so to be safe only consider those
       // spheres at least the maximum nearestNeighborDistance aways from the boundary.  Otherswise, the maximum
       // nearest neighbor distance is artificially inflated by boundary effects.
       Preferences.debug("Before removing boundary influenced spheres maximum nearest neighbor distance = " + 
                         nearestNeighborDistance[spheresDrawn - 1] + "\n");
       System.out.println("Before removing boundary influenced spheres maximum nearest neighbor distance = " + 
               nearestNeighborDistance[spheresDrawn - 1]);
       boundaryDistance = (int)Math.ceil(nearestNeighborDistance[spheresDrawn - 1]);
       spheresLeft = 0;
       for (i = 0; i < spheresDrawn; i++) {
           if ((sphereXCenter[i] >= boundaryDistance) && (sphereXCenter[i] <= xDim - 1 - boundaryDistance) &&
               (sphereYCenter[i] >= boundaryDistance) && (sphereYCenter[i] <= yDim - 1 - boundaryDistance) &&
               (sphereZCenter[i] >= boundaryDistance) && (sphereZCenter[i] <= zDim - 1 - boundaryDistance)) {
               sphereXCenter[spheresLeft] = sphereXCenter[i];
               sphereYCenter[spheresLeft] = sphereYCenter[i];
               sphereZCenter[spheresLeft++] = sphereZCenter[i];
           }
       }
       Preferences.debug("To avoid boundary effects only " + spheresLeft + " of the " + spheresDrawn + 
            " spheres drawn will be analyzed\n");
       System.out.println("To avoid boundary effects only " + spheresLeft + " of the " + spheresDrawn + 
       " spheres drawn will be analyzed\n");
       nearestNeighborDistance = new double[spheresLeft];
       for (i = 0; i < spheresLeft; i++) {
           lowestDistSquared = Integer.MAX_VALUE;
           for (j = 0; j < spheresLeft; j++) {
               if (i != j) {
                   xDistSquared = sphereXCenter[i] - sphereXCenter[j];
                   xDistSquared = xDistSquared * xDistSquared;
                   yDistSquared = sphereYCenter[i] - sphereYCenter[j];
                   yDistSquared = yDistSquared * yDistSquared;
                   zDistSquared = sphereZCenter[i] - sphereZCenter[j];
                   zDistSquared = zDistSquared * zDistSquared;
                   distSquared = xDistSquared + yDistSquared + zDistSquared;
                   if (distSquared < lowestDistSquared) {
                       lowestDistSquared = distSquared;
                       nearestNeighborDistance[i] = Math.sqrt(distSquared);
                   }       
               }
           }
       } // for (i = 0; i < spheresLeft; i++)
       Arrays.sort(nearestNeighborDistance);
       total = 0.0;
       for (i = 0; i < spheresLeft; i++) {
           total += nearestNeighborDistance[i];
       }
       mean = total/spheresLeft;
       totalDeviateSquared = 0.0;
       totalDeviateCubed = 0.0;
       totalDeviateFourth = 0.0;
       for (i = 0; i < spheresLeft; i++) {
           deviate = nearestNeighborDistance[i] - mean;
           deviateSquared = deviate * deviate;
           totalDeviateSquared += deviateSquared;
           deviateCubed = deviateSquared * deviate;
           totalDeviateCubed += deviateCubed;
           deviateFourth = deviateCubed * deviate;
           totalDeviateFourth += deviateFourth;
       }
       variance = totalDeviateSquared/(spheresLeft - 1);
       stdDev = Math.sqrt(variance);
       // Skewness is a third standardized moment that measures the degree of symmetry of a probablility
       // distribution.  A distribution that is symmetrical has a skewness of zero.  If the skewness is 
       // positive, that means the right tail is heavier than the left tail.  If the skewness is negative,
       // then the left tail of the distribution is dominant.
       // skewness = E[(x - mean)**3]/(stdDev**3)
       // skewness = totalDeviateCubed/((stdDev**3)*(sample number - 1))
       // skewness = (sqrt(sample number - 1) * totalDeviateCubed)/(totalDeviateSquared**1.5)
       skewness = totalDeviateCubed/(Math.pow(stdDev, 3)* (spheresLeft - 1));
       // Kurtosis, based on the fourth central moment, measures the thinness of tails or peakedness
       // of a probability distribution.  If kurtosis of a random variable is less than 3, the distribution
       // has thicker tails and a lower peak compared to a normal distribution.  Kurtosis larger than 3
       // indicates a higher peak than a Gaussian and thinner tails.
       // kurtosis = [(x - mean)**4]/(stdDev**4)
       // kurtosis = totalDeviateFourth/((stdDev**4) * (sample number - 1))
       // kurtosis = ((sample number - 1) * totalDeviateFourth)/(totalDeviateSquared**2)
       kurtosis = totalDeviateFourth/(Math.pow(stdDev, 4) * (spheresLeft - 1));
       if ((spheresLeft % 2) == 0) {
           // even number
           median = (nearestNeighborDistance[spheresLeft/2 - 1] + nearestNeighborDistance[spheresLeft/2])/2.0;
       }
       else {
           // odd number
           median = nearestNeighborDistance[(spheresLeft - 1)/2];
       }
       Preferences.debug("Nearest neighbor statistics:\n ");
       System.out.println("Nearest neighbor statistics: ");
       Preferences.debug("Smallest distance = " + nearestNeighborDistance[0] + "\n");
       System.out.println("Smallest distance = " + nearestNeighborDistance[0]);
       Preferences.debug("Mean distance = " + mean + "\n");
       System.out.println("Mean distance = " + mean);
       Preferences.debug("Median distance = " + median + "\n");
       System.out.println("Median distance = " + median);
       Preferences.debug("Largest distance = " + nearestNeighborDistance[spheresLeft-1] + "\n");
       System.out.println("Largest distance = " + nearestNeighborDistance[spheresLeft-1]);
       Preferences.debug("Standard deviation = " + stdDev + "\n");
       System.out.println("Standard deviation = " + stdDev);
       Preferences.debug("Skewness = " + skewness + "\n");
       System.out.println("Skewness = " + skewness);
       Preferences.debug("Kurtosis = " + kurtosis + "\n");
       System.out.println("Kurtosis = " + kurtosis);
       
       // The probability density function for the nearest neighbor distance when spheres of a 
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
       for (i = 0; i < spheresLeft; i++) {
           nearestNeighborDistanceSumOfSquares += nearestNeighborDistance[i]*nearestNeighborDistance[i];
       }
       density = (double)spheresLeft/(double)((xDim - 2 * boundaryDistance) * (yDim - 2 * boundaryDistance) *
                                              (zDim - 2 * boundaryDistance));
       diameter = 2.0 * radius;
       
       // Calculate analytical mean
       meanModel = new IntModelMean(diameter, 1.0E30, Integration.MIDINF, eps, density);
       meanModel.driver();
       steps = meanModel.getStepsUsed();
       numInt = meanModel.getIntegral();
       Preferences.debug("In Integration.MIDINF numerical Integral for mean = " + 
               numInt + " after " + steps + " steps used\n");
       bound = diameter;
       meanModel2 = new IntModelMean2(bound, routine, inf, epsabs, epsrel, limit, density);
       meanModel2.driver();
       numInt2 = meanModel2.getIntegral();
       errorStatus = meanModel2.getErrorStatus();
       absError = meanModel2.getAbserr();
       neval = meanModel2.getNeval();
       Preferences.debug("In Integration2.DQAGIE numerical Integral for mean = " + numInt2 + " after " + neval +
                         " integrand evaluations used\n");
       Preferences.debug("Error status = " + errorStatus +
                         " with absolute error = " + absError + "\n");
       analyticalMean = diameter + Math.exp(density*Math.PI*(4.0/3.0)*diameter*diameter*diameter)*numInt;
       Preferences.debug("Analytical mean = " + analyticalMean + "\n");
       System.out.println("Analytical mean = " + analyticalMean);
       // Calculate analytical mean squared
       meanSquaredModel = new IntModelMeanSquared(diameter, 1.0E30, Integration.MIDINF, eps, density);
       meanSquaredModel.driver();
       steps = meanSquaredModel.getStepsUsed();
       numInt = meanSquaredModel.getIntegral();
       Preferences.debug("In Integration.MIDINF numerical Integral for mean squared = " + 
               numInt + " after " + steps + " steps used\n");
       bound = diameter;
       meanSquaredModel2 = new IntModelMeanSquared2(bound, routine, inf, epsabs, epsrel, limit, density);
       meanSquaredModel2.driver();
       numInt2 = meanSquaredModel2.getIntegral();
       errorStatus = meanSquaredModel2.getErrorStatus();
       absError = meanSquaredModel2.getAbserr();
       neval = meanSquaredModel2.getNeval();
       Preferences.debug("In Integration2.DQAGIE numerical Integral for mean squared = " + numInt2 + " after " + neval +
                         " integrand evaluations used\n");
       Preferences.debug("Error status = " + errorStatus +
                         " with absolute error = " + absError + "\n");
       analyticalMeanSquared = diameter*diameter + Math.exp(density*Math.PI*(4.0/3.0)*diameter*diameter*diameter)*numInt;
       Preferences.debug("Analytical mean squared = " + analyticalMeanSquared + "\n");
       System.out.println("Analytical mean squared = " + analyticalMeanSquared);
       analyticalVariance = spheresLeft*(analyticalMeanSquared - analyticalMean*analyticalMean)/(spheresLeft - 1);
       analyticalStandardError = Math.sqrt(analyticalVariance);
       z = (mean - analyticalMean)/analyticalStandardError;
       stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z, spheresLeft-1, percentile);
       stat.run();
       Preferences.debug("Percentile in Gaussian probability integral for measured mean around analytical mean = "
                         + percentile[0]*100.0 + "\n");
       System.out.println("Percentile in Gaussian probability integral for measured mean around analytical mean = " +
                           percentile[0]*100.0);
       if (percentile[0] < 0.025) {
           // Measured mean signficantly less than analytical mean of random distribution
           Preferences.debug("Clumping or aggregation found in nearest neighbor distances\n");
           System.out.println("Clumping or aggregation found in nearest neighbor distances");
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
    
    class IntModelMean extends Integration {
        double density;
        /**
         * Creates a new IntModel object.
         *
         * @param  lower    DOCUMENT ME!
         * @param  upper    DOCUMENT ME!
         * @param  routine  DOCUMENT ME!
         * @param  eps      DOCUMENT ME!
         */
        public IntModelMean(double lower, double upper, int routine, double eps, double density) {
            super(lower, upper, routine, eps);
            this.density = density;
        }


        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double function;
            function = Math.exp(-density * (4.0/3.0) * Math.PI * x * x * x);

            return function;
        }
    }
    
    class IntModelMeanSquared extends Integration {
        double density;
        /**
         * Creates a new IntModel object.
         *
         * @param  lower    DOCUMENT ME!
         * @param  upper    DOCUMENT ME!
         * @param  routine  DOCUMENT ME!
         * @param  eps      DOCUMENT ME!
         */
        public IntModelMeanSquared(double lower, double upper, int routine, double eps, double density) {
            super(lower, upper, routine, eps);
            this.density = density;
        }


        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double function;
            function = 2.0 * x * Math.exp(-density * (4.0/3.0) * Math.PI * x * x * x);

            return function;
        }
    }
    
    
    class IntModelMean2 extends Integration2 {
        double density;
        public IntModelMean2(double bound, int routine, int inf,
                double epsabs, double epsrel, int limit, double density) {
        super(bound, routine, inf, epsabs, epsrel, limit);
        this.density = density;
        }
       

        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double function;
            function = Math.exp(-density * (4.0/3.0) * Math.PI * x * x * x);

            return function;
        }
    }
    
    class IntModelMeanSquared2 extends Integration2 {
        double density;
        public IntModelMeanSquared2(double bound, int routine, int inf,
                double epsabs, double epsrel, int limit, double density) {
        super(bound, routine, inf, epsabs, epsrel, limit);
        this.density = density;
        }
       

        /**
         * DOCUMENT ME!
         */
        public void driver() {
            super.driver();
        }

        /**
         * DOCUMENT ME!
         *
         * @param   x  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double intFunc(double x) {
            double function;
            function = 2.0 * x * Math.exp(-density * (4.0/3.0) * Math.PI * x * x * x);

            return function;
        }
    }
}
