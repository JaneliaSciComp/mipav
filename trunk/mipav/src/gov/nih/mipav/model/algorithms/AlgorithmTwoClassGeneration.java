package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.*;

/**
 Reference is "Overall and pairwise segregation tests based on nearest neighbor contigency tables" by Elvan
 Ceyhan, Computational Statistics and Data Analysis, 53, 2009, pp. 2786-2808.
 */
public class AlgorithmTwoClassGeneration extends AlgorithmBase {
    
    public static final int FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS = 1;
    
    public static final int FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS = 2;
    
    public static final int RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS = 3;
    
    public static final int RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS = 4;
    
    public static final int MATERN_SAME_PARENTS = 5;
    
    public static final int MATERN_DIFFERENT_PARENTS = 6;
    
    public static final int INHOMOGENEOUS_POISSON = 7;
    
    public static final int SQRT_X_PLUS_Y = 1;
    
    public static final int SQRT_X_TIMES_Y = 2;
    
    public static final int ABS_X_MINUS_Y = 3;
    

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Circle radius
    private int radius;
    
    private int process;
    
    // number of parents
    private int numParents;
    
    private int numOffspring1;
    
    private int numOffspring2;
    
    private double normalizedStdDev;
    
    private double parentPoissonNormalizedMean;
    
    private double normalizedDiscRadius;
    
    private int numPoints1;
    
    private int numPoints2;
    
    private int inhomogeneous;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmTwoClassGeneration - default constructor.
     */
    public AlgorithmTwoClassGeneration() { }

    /**
     * AlgorithmTwoClassGeneration.
     *
     * @param  srcImg   Blank source image in which circles will be drawn
     * @param  radius   Circle radius
     * @param  process 
     * @param  numParents Number of parents
     * @param  numOffspring1
     * @param  numOffspring2
     * @param  normalizedStdDev
     * @param  parentPoissonNormalizedMean
     * @param  normalizedDiscRadius
     * @param  numPoints1
     * @param  numPoints2
     * @param  inhomogeneous
     */
    public AlgorithmTwoClassGeneration(ModelImage srcImage, int radius, int process, int numParents, 
            int numOffspring1, int numOffspring2, double normalizedStdDev,
            double parentPoissonNormalizedMean, double normalizedDiscRadius,
            int numPoints1, int numPoints2,
            int inhomogeneous) {
        super(null, srcImage);
        this.radius = radius;
        this.process = process;
        this.numParents = numParents;
        this.numOffspring1 = numOffspring1;
        this.numOffspring2 = numOffspring2;
        this.normalizedStdDev = normalizedStdDev;
        this.parentPoissonNormalizedMean = parentPoissonNormalizedMean;
        this.normalizedDiscRadius = normalizedDiscRadius;
        this.numPoints1 = numPoints1;
        this.numPoints2 = numPoints2;
        this.inhomogeneous = inhomogeneous;
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
        final int SAME = 1;
        final int DIFFERENT = 2;
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
        //int circleXCenter[] = new int[numCircles];
        //int circleYCenter[] = new int[numCircles];
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
        int boundaryDistance;
        int circlesLeft;
        int maskBytesSet;
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
        double lowestForbiddenSquared;
        double highestForbiddenSquared;
        double highestRegenerationSquared;
        boolean intermediateRejected;
        byte parentX[] = null;
        int xParentXLocation[];
        int xParentYLocation[];
        int xParentsPlaced;
        byte parentY[] = null;
        int yParentXLocation[] = null;
        int yParentYLocation[] = null;
        int yParentsPlaced = 0;
        int parentXLocation;
        int parentYLocation;
        int parentNumber;
        double angle;
        double distance;
        int xCircleXCenter[] = null;
        int xCircleYCenter[] = null;
        int yCircleXCenter[] = null;
        int yCircleYCenter[] = null;
        int offspring1Drawn = 0;
        int offspring2Drawn = 0;
        int offspring1PerParent;
        int offspring2PerParent;
        int discRadius;
        int expandedXDim;
        int expandedYDim;
        int expandedLength;
        int discRadiusSquared;
        int xDiscMaskDim;
        int yDiscMaskDim;
        byte discMask[];
        int xDiscMask[];
        int yDiscMask[];
        int discMaskBytesSet;
        int paddedBuffer[];
        double offspring1PoissonMean;
        double offspring2PoissonMean;
        int pointsInCluster;
        double poissonValues[];
        int events;
        double gain;
        double offset;
        int discIndex;
        int parentsPlaced;
        double offspringPoissonMean;
        int originalCirclesCreated1;
        int originalCirclesCreated2;
        double intensity;
        double maxIntensity;
        int xCircleOrigXCenter[];
        int xCircleOrigYCenter[];
        int yCircleOrigXCenter[];
        int yCircleOrigYCenter[];
        double retentionProbability;
        double cumulativeProb;
        boolean retainCircle[];
        double NN1Distance[];
        byte NN1Type[];
        double NN2Distance[];
        byte NN2Type[];
        int NN1Neighbor[];
        int NN2Neighbor[];
        int N11;
        int N12;
        int N21;
        int N22;
        int C1;
        int C2;
        int n1;
        int n2;
        int n;
        double EN11;
        double EN12;
        double EN21;
        double EN22;
        // A (base, NN) pair (X,Y) is reflexive if (Y,X) is also a (base, NN) pair.
        // R is twice the number of reflexive pairs
        int R;
        // Q is the number of points with shared NNs, which occurs when two or more points share a NN.
        // Then Q = 2*(Q2 + 3*Q3 + 6*Q4 + 10*Q5 * 15*Q6), where Qk is the number of points that serve
        // as a NN to other points k times.
        int Q;
        int Q2;
        int Q3;
        int Q4;
        int Q5;
        int Q6;
        int Q1Array[];
        int Q2Array[];
        double p11;
        double p111;
        double p1111;
        double p12;
        double p112;
        double p1122;
        double p21;
        double p221;
        double p2211;
        double p22;
        double p222;
        double p2222;
        double varN11;
        double varN12;
        double varN21;
        double varN22;
        // Under complete spatial randomness independence, zijD asymptotically has a N(0,1) distribution 
        // conditional on Q and R;
        double z11D;
        double z12D;
        double z21D;
        double z22D;
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Two class generation ...");

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
        
        randomGen = new RandomNumberGen();
        if ((process == FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) || 
            (process == FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS) ||
            (process == RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) || 
            (process == RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS)) {
            stdDev = (xDim - 1)*normalizedStdDev;
            parentX = new byte[length];
            xParentXLocation = new int[numParents];
            xParentYLocation = new int[numParents];
            xCircleXCenter = new int[numOffspring1];
            xCircleYCenter = new int[numOffspring1];
            yCircleXCenter = new int[numOffspring2];
            yCircleYCenter = new int[numOffspring2];
            offspring1PerParent = numOffspring1/numParents;
            offspring2PerParent = numOffspring2/numParents;
            for (i = 0; i < numParents; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum(0, xDim - 1);
                    yCenter = randomGen.genUniformRandomNum(0, yDim - 1);
                    if (parentX[xCenter + xDim * yCenter] != 0) {
                        found = false;
                        attempts++;
                    }
                    else {
                        xParentXLocation[i] = xCenter;
                        xParentYLocation[i] = yCenter;
                        parentX[xCenter + xDim * yCenter] = 1;
                    }
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
            } // for (i = 0; i < numParents; i++)
            xParentsPlaced = i;
            if (xParentsPlaced == 1) {
                if (numParents != 1) {
                    Preferences.debug("1 X parent point placed.  " + numParents + " parent points requested.\n");
                    System.out.println("1 X parent point placed. " + numParents + " parent points requested.");
                    setCompleted(false);
                    return;
                    
                }
                else {
                    Preferences.debug("1 X parent point placed.  1 parent point requested\n");
                    System.out.println("1 X parent point placed.  1 parent point requested");    
                }
            }
            else if (xParentsPlaced != numParents) {
                Preferences.debug(xParentsPlaced + " X parent points placed.  " +
                                  numParents + " parent points requested.\n");
                System.out.println(xParentsPlaced + " X parent points placed.  " +
                        numParents + " parent points requested.");
                setCompleted(false);
                return;
            }   
            else { // xParentsPlaced == numParents
                Preferences.debug(xParentsPlaced + " X parent points placed.  " +
                        numParents + " parent points requested.\n");
                System.out.println(xParentsPlaced + " X parent points placed.  " +
                        numParents + " parent points requested.");
            }
            
            if ((process == FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS) ||
                    (process == RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS)) {
                    parentY = new byte[length];
                    yParentXLocation = new int[numParents];
                    yParentYLocation = new int[numParents];
                    for (i = 0; i < numParents; i++) {
                        found = false;
                        attempts = 0;
                        while ((!found) && (attempts <= 100)) {
                            found = true;
                            xCenter = randomGen.genUniformRandomNum(0, xDim - 1);
                            yCenter = randomGen.genUniformRandomNum(0, yDim - 1);
                            if ((parentX[xCenter + xDim * yCenter] != 0) || (parentY[xCenter + xDim * yCenter] != 0)) {
                                found = false;
                                attempts++;
                            }
                            else {
                                yParentXLocation[i] = xCenter;
                                yParentYLocation[i] = yCenter;
                                parentY[xCenter + xDim * yCenter] = 1;
                            }
                        } // while ((!found) && (attempts <= 100))
                        if (!found) {
                            break;
                        }
                    } // for (i = 0; i < numParents; i++)
                    yParentsPlaced = i;
                    if (yParentsPlaced == 1) {
                        if (numParents != 1) {
                            Preferences.debug("1 Y parent point placed.  " + numParents + " parent points requested.\n");
                            System.out.println("1 Y parent point placed. " + numParents + " parent points requested.");
                            setCompleted(false);
                            return;
                            
                        }
                        else {
                            Preferences.debug("1 Y parent point placed.  1 parent point requested\n");
                            System.out.println("1 Y parent point placed.  1 parent point requested");    
                        }
                    }
                    else if (yParentsPlaced != numParents) {
                        Preferences.debug(yParentsPlaced + " Y parent points placed.  " +
                                          numParents + " parent points requested.\n");
                        System.out.println(yParentsPlaced + " Y parent points placed.  " +
                                numParents + " parent points requested.");
                        setCompleted(false);
                        return;
                    }   
                    else { // yParentsPlaced == numParents
                        Preferences.debug(yParentsPlaced + " Y parent points placed.  " +
                                numParents + " parent points requested.\n");
                        System.out.println(yParentsPlaced + " Y parent points placed.  " +
                                numParents + " parent points requested.");
                    }
                } // if ((process == FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS) ||
            for (i = 0; i < numOffspring1; i++) {
                if ((process == FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) ||
                   (process == FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS)) {
                    parentXLocation = xParentXLocation[i/offspring1PerParent];
                    parentYLocation = xParentYLocation[i/offspring1PerParent];
                }
                else {
                    parentNumber =  randomGen.genUniformRandomNum(0, numParents - 1);
                    parentXLocation = xParentXLocation[parentNumber];
                    parentYLocation = xParentYLocation[parentNumber];
                }
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    // radially symmetric
                    angle = randomGen.genUniformRandomNum(0.0, Math.PI);
                    distance = stdDev * randomGen.genStandardGaussian();
                    xCenter = (int)Math.round(parentXLocation + distance * Math.cos(angle));
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = (int)Math.round(parentYLocation + distance * Math.sin(angle));
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    rloop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break rloop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
                xCircleXCenter[i] = xCenter;
                xCircleYCenter[i] = yCenter;
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  1;
                        }
                    }
                }
            } // for (i = 0; i < numOffspring1; i++)
            offspring1Drawn = i;
            Preferences.debug(offspring1Drawn + " offspring 1 drawn.  " + numOffspring1 + " offspring 1 requested.\n");
            System.out.println(offspring1Drawn + " offspring 1 drawn.  " + numOffspring1 + " offspring 1 requested.");
            
            for (i = 0; i < numOffspring2; i++) {
                if (process == FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) {
                    parentXLocation = xParentXLocation[i/offspring2PerParent];
                    parentYLocation = xParentYLocation[i/offspring2PerParent];
                }
                else if (process == FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS) {
                    parentXLocation = yParentXLocation[i/offspring2PerParent];
                    parentYLocation = yParentYLocation[i/offspring2PerParent];    
                }
                else if (process == RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) {
                    parentNumber =  randomGen.genUniformRandomNum(0, numParents - 1);
                    parentXLocation = xParentXLocation[parentNumber];
                    parentYLocation = xParentYLocation[parentNumber];
                }
                else {
                    parentNumber =  randomGen.genUniformRandomNum(0, numParents - 1);
                    parentXLocation = yParentXLocation[parentNumber];
                    parentYLocation = yParentYLocation[parentNumber];    
                }
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    // radially symmetric
                    angle = randomGen.genUniformRandomNum(0.0, Math.PI);
                    distance = stdDev * randomGen.genStandardGaussian();
                    xCenter = (int)Math.round(parentXLocation + distance * Math.cos(angle));
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = (int)Math.round(parentYLocation + distance * Math.sin(angle));
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    r2loop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r2loop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
                yCircleXCenter[i] = xCenter;
                yCircleYCenter[i] = yCenter;
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  2;
                        }
                    }
                }
            } // for (i = 0; i < numOffspring2; i++)
            offspring2Drawn = i;
            Preferences.debug(offspring2Drawn + " offspring 2 drawn.  " + numOffspring2 + " offspring 2 requested.\n");
            System.out.println(offspring2Drawn + " offspring 2 drawn.  " + numOffspring2 + " offspring 2 requested.");
        } // if ((process == FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) || 
        
        
        if ((process == MATERN_SAME_PARENTS) || (process == MATERN_DIFFERENT_PARENTS)) {
            // Put 100 * parentPoissonNormalizedMean points into an area 100 times as large
            // as the resulting area.
            discRadius = (int)Math.round(normalizedDiscRadius * (xDim - 1));
            expandedXDim = 10 * xDim;
            expandedYDim = 10 * yDim;
            expandedLength = expandedXDim * expandedYDim;
            // Create a mask for the disc around the Poisson parent points
            discRadiusSquared = discRadius * discRadius;
            xDiscMaskDim = 2 * discRadius + 1;
            yDiscMaskDim = xDiscMaskDim;
            discMask = new byte[xDiscMaskDim * yDiscMaskDim];
            discMaskBytesSet = 0;
            for (y = 0; y <= 2*discRadius; y++) {
                yDistSquared = y - discRadius;
                yDistSquared = yDistSquared * yDistSquared;
                for (x = 0; x <= 2 * discRadius; x++) {
                    xDistSquared = x - discRadius;
                    xDistSquared = xDistSquared * xDistSquared;
                    distSquared = xDistSquared + yDistSquared;
                    if (distSquared <= discRadiusSquared) {
                        discMask[x + y * xDiscMaskDim] = 1;
                        discMaskBytesSet++;
                    }
                }
            } // for (y = 0; y <= 2*radius; y++)
            xDiscMask = new int[discMaskBytesSet];
            yDiscMask = new int[discMaskBytesSet];
            i = 0;
            for (y = 0; y <= 2*discRadius; y++) {
                for (x = 0; x <= 2*discRadius; x++) {
                    if (discMask[x + y * xDiscMaskDim] == 1) {
                        xDiscMask[i] = x;
                        yDiscMask[i++] = y;
                    }
                }
            }
            paddedBuffer = new int[(xDim + 4 * discRadius)*(yDim + 4 * discRadius)];
            numParents = (int)Math.round(100 * parentPoissonNormalizedMean);
            parentX = new byte[expandedLength];
            xParentXLocation = new int[numParents];
            xParentYLocation = new int[numParents];
            for (i = 0, j = 0; i < numParents; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum(0, expandedXDim - 1);
                    yCenter = randomGen.genUniformRandomNum(0, expandedYDim - 1);
                    if (parentX[xCenter + expandedXDim * yCenter] != 0) {
                        found = false;
                        attempts++;
                    }
                    else {
                        parentX[xCenter + expandedXDim * yCenter] = 1;
                        if ((xCenter >= 4*xDim - discRadius) && (xCenter <= 5*xDim - 1 + discRadius) &&
                            (yCenter >= 4*yDim - discRadius) && (yCenter <= 5*yDim - 1 + discRadius)) {
                            // Go from discRadius to dim + 3 * discRadius -1 in paddedBuffer
                            xParentXLocation[j] = xCenter - (4 * xDim - 2*discRadius);
                            xParentYLocation[j++] = yCenter - (4 * yDim - 2*discRadius);
                        }
                    }
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }    
            } // for (i = 0; i < numParents; i++)
            xParentsPlaced = j;
            Preferences.debug(xParentsPlaced + " X parents placed in padded buffer.  Mean of " + 
                              parentPoissonNormalizedMean + " X parents requested for unpadded buffer.\n");
            System.out.println(xParentsPlaced + " X parents placed in padded buffer.  Mean of " + 
                              parentPoissonNormalizedMean + " X parents requested for unpadded buffer.");
            if (xParentsPlaced == 0) {
                setCompleted(false);
                return;
            }
            
            if (process == MATERN_DIFFERENT_PARENTS) {
                parentY = new byte[expandedLength];
                yParentXLocation = new int[numParents];
                yParentYLocation = new int[numParents];
                for (i = 0, j = 0; i < numParents; i++) {
                    found = false;
                    attempts = 0;
                    while ((!found) && (attempts <= 100)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(0, expandedXDim - 1);
                        yCenter = randomGen.genUniformRandomNum(0, expandedYDim - 1);
                        if ((parentX[xCenter + expandedXDim * yCenter] != 0) || 
                            (parentY[xCenter + expandedXDim * yCenter] != 0)){
                            found = false;
                            attempts++;
                        }
                        else {
                            parentY[xCenter + expandedXDim * yCenter] = 1;
                            if ((xCenter >= 4*xDim - discRadius) && (xCenter <= 5*xDim - 1 + discRadius) &&
                                (yCenter >= 4*yDim - discRadius) && (yCenter <= 5*yDim - 1 + discRadius)) {
                                // Go from discRadius to dim + 3 * discRadius -1 in paddedBuffer
                                yParentXLocation[j] = xCenter - (4 * xDim - 2*discRadius);
                                yParentYLocation[j++] = yCenter - (4 * yDim - 2*discRadius);
                            }
                        }
                    } // while ((!found) && (attempts <= 100))
                    if (!found) {
                        break;
                    }    
                } // for (i = 0; i < numParents; i++)
                yParentsPlaced = j;
                Preferences.debug(yParentsPlaced + " Y parents placed in padded buffer.  Mean of " + 
                                  parentPoissonNormalizedMean + " Y parents requested for unpadded buffer.\n");
                System.out.println(yParentsPlaced + " Y parents placed in padded buffer.  Mean of " + 
                                  parentPoissonNormalizedMean + " Y parents requested for unpadded buffer."); 
                if (yParentsPlaced == 0) {
                    setCompleted(false);
                    return;
                }
            } // if (process == MATERN_DIFFERENT_PARENTS)
            
            offspring1PoissonMean = numOffspring1/parentPoissonNormalizedMean;
            xCircleXCenter = new int[(int)Math.round(2 * xParentsPlaced * offspring1PoissonMean)];
            xCircleYCenter = new int[xCircleXCenter.length];
            offspring1Drawn = 0;
            for (i = 0; i < xParentsPlaced; i++) {
                 events = 1;
                 gain = 1.0;
                 offset = 0;
                 poissonValues = randomGen.poissDecay(events, offspring1PoissonMean, gain, offset);
                 pointsInCluster = (int)Math.round(poissonValues[0]);
                 for (j = 0; j < pointsInCluster; j++) {
                     found = false;
                     attempts = 0;
                     while ((!found) && (attempts <= 100)) {
                         found = true;
                         discIndex = randomGen.genUniformRandomNum(0, discMaskBytesSet - 1);
                         // center goes from 0 to dim + 4 * discRadius - 1 in paddedBuffer
                         xCenter = xParentXLocation[i] + xDiscMask[discIndex] - discRadius;
                         if ((xCenter - radius < 0) || (xCenter + radius > xDim + 4*discRadius - 1)) {
                             found = false;
                             attempts++;
                             continue;
                         }
                         yCenter = xParentYLocation[i] + yDiscMask[discIndex] - discRadius;
                         if ((yCenter - radius < 0) || (yCenter + radius > yDim + 4*discRadius - 1)) {
                             found = false;
                             attempts++;
                             continue;
                         }
                         r3loop:
                             for (y = 0; y <= 2*radius; y++) {
                                 for (x = 0; x <= 2*radius; x++) {
                                     if (mask[x + y * xMaskDim] == 1) {
                                         if (paddedBuffer[(xCenter + x - radius) + 
                                                          (xDim + 4 * discRadius)*(yCenter + y - radius)] != 0) {
                                             found = false;
                                             attempts++;
                                             break r3loop;
                                         }
                                     }
                                 }
                             } // for (y = 0; y <= 2*radius; y++)
                     } // while ((!found) && (attempts <= 100))
                     if (!found) {
                         break;
                     }
                     
                     for (y = 0; y <= 2*radius; y++) {
                         for (x = 0; x <= 2*radius; x++) {
                             if (mask[x + y * xMaskDim] == 1) {
                                 paddedBuffer[(xCenter + x - radius) + (xDim + 4 * discRadius) *(yCenter + y - radius)] =  1;
                             }
                         }
                     }  
                     
                     if ((xCenter - radius >= 2 * discRadius) && (xCenter + radius < xDim + 2 * discRadius) &&
                             (yCenter - radius >= 2 * discRadius) && (yCenter + radius < yDim + 2 * discRadius)) {
                         // Subtract 2*discRadius to change offset from paddedBuffer to buffer
                         xCircleXCenter[offspring1Drawn] = xCenter - 2 * discRadius;
                         xCircleYCenter[offspring1Drawn++] = yCenter - 2 * discRadius;
                     }
                 } // for (j = 0; j < pointsInCluster; j++)
            } // for (i = 0; i < xParentsPlaced; i++)
            Preferences.debug(offspring1Drawn + " offspring 1 drawn\n");
            System.out.println(offspring1Drawn + " offspring 1 drawn");
            
            offspring2PoissonMean = numOffspring2/parentPoissonNormalizedMean;
            if (process == MATERN_SAME_PARENTS) {
                parentsPlaced = xParentsPlaced;
            }
            else {
                parentsPlaced = yParentsPlaced;   
            }
            yCircleXCenter = new int[(int)Math.round(2 * parentsPlaced * offspring2PoissonMean)];
            yCircleYCenter = new int[yCircleXCenter.length];
            offspring2Drawn = 0;
            for (i = 0; i < parentsPlaced; i++) {
                 events = 1;
                 gain = 1.0;
                 offset = 0;
                 poissonValues = randomGen.poissDecay(events, offspring2PoissonMean, gain, offset);
                 pointsInCluster = (int)Math.round(poissonValues[0]);
                 for (j = 0; j < pointsInCluster; j++) {
                     found = false;
                     attempts = 0;
                     while ((!found) && (attempts <= 100)) {
                         found = true;
                         discIndex = randomGen.genUniformRandomNum(0, discMaskBytesSet - 1);
                         // center goes from 0 to dim + 4 * discRadius - 1 in paddedBuffer
                         if (process == MATERN_SAME_PARENTS) {
                             xCenter = xParentXLocation[i] + xDiscMask[discIndex] - discRadius;
                         }
                         else {
                             xCenter = yParentXLocation[i] + xDiscMask[discIndex] - discRadius;
                         }
                         if ((xCenter - radius < 0) || (xCenter + radius > xDim + 4*discRadius - 1)) {
                             found = false;
                             attempts++;
                             continue;
                         }
                         if (process == MATERN_SAME_PARENTS) {
                             yCenter = xParentYLocation[i] + yDiscMask[discIndex] - discRadius;
                         }
                         else {
                             yCenter = yParentYLocation[i] + yDiscMask[discIndex] - discRadius;    
                         }
                         if ((yCenter - radius < 0) || (yCenter + radius > yDim + 4*discRadius - 1)) {
                             found = false;
                             attempts++;
                             continue;
                         }
                         r3loop:
                             for (y = 0; y <= 2*radius; y++) {
                                 for (x = 0; x <= 2*radius; x++) {
                                     if (mask[x + y * xMaskDim] == 1) {
                                         if (paddedBuffer[(xCenter + x - radius) + 
                                                          (xDim + 4 * discRadius)*(yCenter + y - radius)] != 0) {
                                             found = false;
                                             attempts++;
                                             break r3loop;
                                         }
                                     }
                                 }
                             } // for (y = 0; y <= 2*radius; y++)
                     } // while ((!found) && (attempts <= 100))
                     if (!found) {
                         break;
                     }
                     
                     for (y = 0; y <= 2*radius; y++) {
                         for (x = 0; x <= 2*radius; x++) {
                             if (mask[x + y * xMaskDim] == 1) {
                                 paddedBuffer[(xCenter + x - radius) + (xDim + 4 * discRadius) *(yCenter + y - radius)] =  2;
                             }
                         }
                     }  
                     
                     if ((xCenter - radius >= 2 * discRadius) && (xCenter + radius < xDim + 2 * discRadius) &&
                             (yCenter - radius >= 2 * discRadius) && (yCenter + radius < yDim + 2 * discRadius)) {
                         // Subtract 2*discRadius to change offset from paddedBuffer to buffer
                         yCircleXCenter[offspring2Drawn] = xCenter - 2 * discRadius;
                         yCircleYCenter[offspring2Drawn++] = yCenter - 2 * discRadius;
                     }
                 } // for (j = 0; j < pointsInCluster; j++)
            } // for (i = 0; i < parentsPlaced; i++)
            Preferences.debug(offspring2Drawn + " offspring 2 drawn\n");
            System.out.println(offspring2Drawn + " offspring 2 drawn");
            
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    buffer[x + xDim * y] = paddedBuffer[(x + 2 * discRadius) + (xDim + 4 * discRadius) * (y + 2 * discRadius)];
                }
            }
        } // if ((process == MATERN_SAME_PARENTS) || (process == MATERN_DIFFERENT_PARENTS))
        
        if (process == INHOMOGENEOUS_POISSON) {
            xCircleOrigXCenter = new int[numPoints1];
            xCircleOrigYCenter = new int[numPoints1];
            yCircleOrigXCenter = new int[numPoints2];
            yCircleOrigYCenter = new int[numPoints2];
            for (i = 0; i < numPoints1; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum(radius, xDim - 1 - radius);
                    yCenter = randomGen.genUniformRandomNum(radius, yDim - 1 - radius);
                    rloop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break rloop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
                xCircleOrigXCenter[i] = xCenter;
                xCircleOrigYCenter[i] = yCenter;
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  1;
                        }
                    }
                }
            } // for (i = 0; i < numPoints1; i++)
            originalCirclesCreated1 = i;
            Preferences.debug(originalCirclesCreated1 + " type 1 circles orignally created.  " + 
                    numPoints1 + " type 1 circles requested.\n");
            System.out.println(originalCirclesCreated1 + " type 1 circles orignally created.  " + 
                    numPoints1 + " type 1 circles requested.\n"); 
            
            for (i = 0; i < numPoints2; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum(radius, xDim - 1 - radius);
                    yCenter = randomGen.genUniformRandomNum(radius, yDim - 1 - radius);
                    rloop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break rloop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
                yCircleOrigXCenter[i] = xCenter;
                yCircleOrigYCenter[i] = yCenter;
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  2;
                        }
                    }
                }
            } // for (i = 0; i < numPoints2; i++)
            originalCirclesCreated2 = i;
            Preferences.debug(originalCirclesCreated2 + " type 2 circles orignally created.  " + 
                    numPoints2 + " type 2 circles requested.\n");
            System.out.println(originalCirclesCreated2 + " type 2 circles orignally created.  " + 
                    numPoints2 + " type 2 circles requested.\n");
            
            maxIntensity = -Double.MAX_VALUE;
            for (i = 0; i < originalCirclesCreated1; i++) {
                intensity = Math.sqrt((double)xCircleOrigXCenter[i]/(xDim - 1) + (double)xCircleOrigYCenter[i]/(yDim - 1));
                if (intensity > maxIntensity) {
                    maxIntensity = intensity;
                }
            }
            
            retainCircle = new boolean[originalCirclesCreated1];
            offspring1Drawn = 0;
            for (i = 0; i < originalCirclesCreated1; i++) {
                intensity = Math.sqrt((double)xCircleOrigXCenter[i]/(xDim - 1) + (double)xCircleOrigYCenter[i]/(yDim - 1));
                retentionProbability = intensity/maxIntensity;
                cumulativeProb = randomGen.genUniformRandomNum(0.0, 1.0);
                if (cumulativeProb <= retentionProbability) {
                    retainCircle[i] = true;
                    offspring1Drawn++;
                }
            }
            
            Preferences.debug(offspring1Drawn + " type 1 offspring retained.\n");
            System.out.println(offspring1Drawn + " type 1 offspring retained.");
            
            xCircleXCenter = new int[offspring1Drawn];
            xCircleYCenter = new int[offspring1Drawn];
            for (i = 0, j = 0; i < originalCirclesCreated1; i++) {
                if (retainCircle[i]) {
                    xCircleXCenter[j] = xCircleOrigXCenter[i];
                    xCircleYCenter[j++] = xCircleOrigYCenter[i]; 
                }
            }
            
            maxIntensity = -Double.MAX_VALUE;
            for (i = 0; i < originalCirclesCreated2; i++) {
                if (inhomogeneous == SQRT_X_PLUS_Y) {
                    intensity = Math.sqrt((double)yCircleOrigXCenter[i]/(xDim - 1) + (double)yCircleOrigYCenter[i]/(yDim - 1));
                }
                else if (inhomogeneous == SQRT_X_TIMES_Y) {
                    intensity = Math.sqrt((double)yCircleOrigXCenter[i] * yCircleOrigYCenter[i]/((xDim-1)*(yDim-1)));
                }
                else {
                    intensity = Math.abs((double)yCircleOrigXCenter[i]/(xDim-1) - (double)yCircleOrigYCenter[i]/(yDim-1));
                }
                if (intensity > maxIntensity) {
                    maxIntensity = intensity;
                }
            }
            
            retainCircle = new boolean[originalCirclesCreated2];
            offspring2Drawn = 0;
            for (i = 0; i < originalCirclesCreated2; i++) {
                if (inhomogeneous == SQRT_X_PLUS_Y) {
                    intensity = Math.sqrt((double)yCircleOrigXCenter[i]/(xDim - 1) + (double)yCircleOrigYCenter[i]/(yDim - 1));
                }
                else if (inhomogeneous == SQRT_X_TIMES_Y) {
                    intensity = Math.sqrt((double)yCircleOrigXCenter[i] * yCircleOrigYCenter[i]/((xDim-1)*(yDim-1)));
                }
                else {
                    intensity = Math.abs((double)yCircleOrigXCenter[i]/(xDim-1) - (double)yCircleOrigYCenter[i]/(yDim-1));
                }
                retentionProbability = intensity/maxIntensity;
                cumulativeProb = randomGen.genUniformRandomNum(0.0, 1.0);
                if (cumulativeProb <= retentionProbability) {
                    retainCircle[i] = true;
                    offspring2Drawn++;
                }
            }
            
            Preferences.debug(offspring2Drawn + " type 2 offspring retained.\n");
            System.out.println(offspring2Drawn + " type 2 offspring retained.");
            
            yCircleXCenter = new int[offspring2Drawn];
            yCircleYCenter = new int[offspring2Drawn];
            for (i = 0, j = 0; i < originalCirclesCreated2; i++) {
                if (retainCircle[i]) {
                    yCircleXCenter[j] = yCircleOrigXCenter[i];
                    yCircleYCenter[j++] = yCircleOrigYCenter[i]; 
                }
            }
            
            for (i = 0; i < buffer.length; i++) {
                buffer[i] = 0;
            }
            
            for (i = 0; i < offspring1Drawn; i++) {
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCircleXCenter[i] + x - radius) + xDim*(xCircleYCenter[i] + y - radius)] =  1;
                        }
                    }
                }    
            }
            
            for (i = 0; i < offspring2Drawn; i++) {
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(yCircleXCenter[i] + x - radius) + xDim*(yCircleYCenter[i] + y - radius)] =  2;
                        }
                    }
                }    
            }
        } // if (process == INHOMOGENEOUS_POISSON)
        
        N11 = 0;
        N12 = 0;
        N21 = 0;
        N22 = 0;
        NN1Distance = new double[offspring1Drawn];
        NN1Type = new byte[offspring1Drawn];
        NN1Neighbor = new int[offspring1Drawn];
        for (i = 0; i < offspring1Drawn; i++) {
            lowestDistSquared = Integer.MAX_VALUE;
            for (j = 0; j < offspring1Drawn; j++) {
                if (i != j) {          
                    xDistSquared = xCircleXCenter[i] - xCircleXCenter[j];
                    xDistSquared = xDistSquared * xDistSquared;
                    yDistSquared = xCircleYCenter[i] - xCircleYCenter[j];
                    yDistSquared = yDistSquared * yDistSquared;
                    distSquared = xDistSquared + yDistSquared;
                    if (distSquared < lowestDistSquared) {
                        lowestDistSquared = distSquared;
                        NN1Distance[i] = Math.sqrt(distSquared);
                        NN1Type[i] = SAME;
                        NN1Neighbor[i] = j;
                    }  
                }
            }
            
            for (j = 0; j < offspring2Drawn; j++) {          
                xDistSquared = xCircleXCenter[i] - yCircleXCenter[j];
                xDistSquared = xDistSquared * xDistSquared;
                yDistSquared = xCircleYCenter[i] - yCircleYCenter[j];
                yDistSquared = yDistSquared * yDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared < lowestDistSquared) {
                    lowestDistSquared = distSquared;
                    NN1Distance[i] = Math.sqrt(distSquared);
                    NN1Type[i] = DIFFERENT;
                    NN1Neighbor[i] = j;
                }  
            }
        } // for (i = 0; i < offspring1Drawn; i++)
        
        for (i = 0; i < offspring1Drawn; i++) {
            if (NN1Type[i] == SAME) {
                N11++;
            }
            else {
                N12++;
            }
        }
        
        NN2Distance = new double[offspring2Drawn];
        NN2Type = new byte[offspring2Drawn];
        NN2Neighbor = new int[offspring2Drawn];
        for (i = 0; i < offspring2Drawn; i++) {
            lowestDistSquared = Integer.MAX_VALUE;
            for (j = 0; j < offspring1Drawn; j++) {         
                xDistSquared = yCircleXCenter[i] - xCircleXCenter[j];
                xDistSquared = xDistSquared * xDistSquared;
                yDistSquared = yCircleYCenter[i] - xCircleYCenter[j];
                yDistSquared = yDistSquared * yDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared < lowestDistSquared) {
                    lowestDistSquared = distSquared;
                    NN2Distance[i] = Math.sqrt(distSquared);
                    NN2Type[i] = DIFFERENT;
                    NN2Neighbor[i] = j;
                }  
            }
            
            for (j = 0; j < offspring2Drawn; j++) { 
                if (i != j) {
                    xDistSquared = yCircleXCenter[i] - yCircleXCenter[j];
                    xDistSquared = xDistSquared * xDistSquared;
                    yDistSquared = yCircleYCenter[i] - yCircleYCenter[j];
                    yDistSquared = yDistSquared * yDistSquared;
                    distSquared = xDistSquared + yDistSquared;
                    if (distSquared < lowestDistSquared) {
                        lowestDistSquared = distSquared;
                        NN2Distance[i] = Math.sqrt(distSquared);
                        NN2Type[i] = SAME;
                        NN2Neighbor[i] = j;
                    } 
                }
            }
        } // for (i = 0; i < offspring2Drawn; i++)
        
        for (i = 0; i < offspring2Drawn; i++) {
            if (NN2Type[i] == DIFFERENT) {
                N21++;
            }
            else {
                N22++;
            }
        }
        
        C1 = N11 + N21;
        C2 = N12 + N22;
        n1 = N11 + N12;
        n2 = N21 + N22;
        n = n1 + n2;
        
        // Dixon's cell-specific test of segregation
        EN11 = (double)n1*(n1 - 1)/(n - 1);
        EN12 = (double)n1 * n2/(n - 1);
        EN21 = EN12;
        EN22 = (double)n2*(n2 - 1)/(n - 1);
        
        // Find R, twice the number of reflexive pairs.
        R = 0;
        for (i = 0; i < offspring1Drawn; i++) {
            if (NN1Type[i] == SAME && NN1Neighbor[NN1Neighbor[i]] == i) {
                R++;
            }
            else if (NN1Type[i] == DIFFERENT && NN2Neighbor[NN1Neighbor[i]] == i) {
                R++;
            }
        }
        
        for (i = 0; i < offspring2Drawn; i++) {
            if (NN2Type[i] == DIFFERENT && NN1Neighbor[NN2Neighbor[i]] == i) {
                R++;
            }
            else if (NN2Type[i] == SAME && NN2Neighbor[NN2Neighbor[i]] == i) {
                R++;
            }
        }
        
        Q1Array = new int[offspring1Drawn];
        Q2Array = new int[offspring2Drawn];
        for (i = 0; i < offspring1Drawn; i++) {
            if (NN1Type[i] == SAME) {
                Q1Array[NN1Neighbor[i]]++;
            }
            else {
                Q2Array[NN1Neighbor[i]]++;
            }
        }
        
        for (i = 0; i < offspring2Drawn; i++) {
            if (NN2Type[i] == DIFFERENT) {
                Q1Array[NN2Neighbor[i]]++;    
            }
            else {
                Q2Array[NN2Neighbor[i]]++;
            }
        }
        
        Q2 = 0;
        Q3 = 0;
        Q4 = 0;
        Q5 = 0;
        Q6 = 0;
        for (i = 0; i < offspring1Drawn; i++) {
            if (Q1Array[i] == 2) {
                Q2++;
            }
            else if (Q1Array[i] == 3) {
                Q3++;
            }
            else if (Q1Array[i] == 4) {
                Q4++;
            }
            else if (Q1Array[i] == 5) {
                Q5++;
            }
            else if (Q1Array[i] == 6) {
                Q6++;
            }
        }
        
        for (i = 0; i < offspring2Drawn; i++) {
            if (Q2Array[i] == 2) {
                Q2++;
            }
            else if (Q2Array[i] == 3) {
                Q3++;
            }
            else if (Q2Array[i] == 4) {
                Q4++;
            }
            else if (Q2Array[i] == 5) {
                Q5++;
            }
            else if (Q2Array[i] == 6) {
                Q6++;
            }
        }
        
        Q = 2 * (Q2 + 3*Q3 + 6*Q4 + 10*Q5 + 15*Q6);
        
        p11 = (double)n1*(n1 - 1)/(n*(n - 1));
        p111 = (double)n1*(n1 - 1)*(n1 - 2)/(n*(n - 1)*(n - 2));
        p1111 = (double)n1*(n1 - 1)*(n1 - 2)*(n1 - 3)/(n*(n - 1)*(n - 2)*(n - 3));
        p12 = (double)n1*n2/(n*(n - 1));
        p112 = (double)n1*(n1 - 1)*n2/(n*(n - 1)*(n - 2));
        p1122 = (double)n1*(n1 - 1)*n2*(n2 - 1)/(n*(n - 1)*(n - 2)*(n - 3));
        p21 = p12;
        p221 = (double)n2*(n2 - 1)*n1/(n*(n - 1)*(n - 2));
        p2211 = p1122;
        p22 = (double)n2*(n2 - 1)/(n*(n-1));
        p222 = (double)n2*(n2 - 1)*(n2 - 2)/(n*(n - 1)*(n - 2));
        p2222 = (double)n2*(n2 - 1)*(n2 - 2)*(n2 - 3)/(n*(n - 1)*(n - 2)*(n - 3));
        
        varN11 = (n + R)*p11 + (2*n - 2*R + Q)*p111 + (n*n - 3*n - Q + R)*p1111 -n*n*p11*p11;
        varN12 = n*p12 + Q*p112 + (n*n - 3*n - Q + R)*p1122 - n*n*p12*p12;
        varN21 = n*p21 + Q*p221 + (n*n - 3*n - Q + R)*p2211 - n*n*p21*p21;
        varN22 = (n + R)*p22 + (2*n - 2*R + Q)*p222 + (n*n - 3*n - Q + R)*p2222 - n*n*p22*p22;
        
        z11D = (N11 - EN11)/Math.sqrt(varN11);
        z12D = (N12 - EN12)/Math.sqrt(varN12);
        z21D = (N21 - EN21)/Math.sqrt(varN21);
        z22D = (N22 - EN22)/Math.sqrt(varN22);
        
        Preferences.debug("Dixon's cell-specific test of segregation\n");
        System.out.println("Dixon's cell-specific test of segregation");
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z11D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N11 around expected mean N11 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N11 around expected mean N11 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N11 indicates association\n");
            System.out.println("Low value of N11 indicates association");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N11 indicates segregation\n");
            System.out.println("High value of N11 indicates segregation");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N11 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N11 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z12D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N12 around expected mean N12 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N12 around expected mean N12 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N12 indicates segregation\n");
            System.out.println("Low value of N12 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N12 indicates association\n");
            System.out.println("High value of N12 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N12 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N12 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z21D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N21 around expected mean N21 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N21 around expected mean N21 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N21 indicates segregation\n");
            System.out.println("Low value of N21 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N21 indicates association\n");
            System.out.println("High value of N21 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N21 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N21 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z22D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N22 around expected mean N22 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N22 around expected mean N22 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N22 indicates association\n");
            System.out.println("Low value of N22 indicates association");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N22 indicates segregation\n");
            System.out.println("High value of N22 indicates segregation");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N22 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N22 value");
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
