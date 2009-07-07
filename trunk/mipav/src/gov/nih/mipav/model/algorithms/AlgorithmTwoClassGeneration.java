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
    
    private double offspringPoissonNormalizedMean;
    
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
     * @param  offspringPoissonNormalizedMean
     * @param  inhomogeneous
     */
    public AlgorithmTwoClassGeneration(ModelImage srcImage, int radius, int process, int numParents, 
            int numOffspring1, int numOffspring2, double normalizedStdDev,
            double parentPoissonNormalizedMean, double normalizedDiscRadius, double offspringPoissonNormalizedMean,
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
        this.offspringPoissonNormalizedMean = offspringPoissonNormalizedMean;
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
