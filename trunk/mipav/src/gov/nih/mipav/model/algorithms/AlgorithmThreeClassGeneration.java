package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import Jama.*;

import gov.nih.mipav.view.*;

import java.io.*;

/**
 *      This program generates type 1, type 2, and type 3 objects in different spatial patterns and tests to see if
 *      randomness if rejected for segregation or association.  The objects may be points or circles of
 *      a specified radius. 
 *
 *      Segregation occurs when members of a given class have nearest neighbors that are more frequently of the
 *      same class and less frequently of the other class than would be expected if there were randomness in the
 *      nearest neighbor structure.  Association occurs when members of a given class have nearest neighbors that
 *      are less frequently of the same class and more frequently of the other class than would be expected if 
 *      there were randomness in the nearest neighbor structure.
 *      
 *      Nearest neighbor contingency tables are constructed using the nearest neighbor frequencies of classes.
 *      Below is the nearest neighbor contingency table used for the 3 class case in this program:
 *                                                NN class                                                   Sum
 *                                                Class 1                Class 2          Class 3
 *      Base class            Class 1             N11                    N12              N13                n1
 *                            Class 2             N21                    N22              N23                n2
 *                            Class 3             N31                    N32              N33                n3
 *                            
 *                            Sum                 C1                     C2               C3                 n
 *                            
 *      This program performs the following tests: Dixon's overall test of segregation, Dixon's cell-specific
 *      tests of segregation, Ceyhan's overall test of segregation, and Ceyhan's cell-specific tests of 
 *      segregation.  Dixon's overall test of segregation generates CD which follows a chi squared distribution
 *      with 6 degrees of freedom.  Spatial randomness is rejected if CD has a value greater than the chi squared
 *      cumulative frequency value of 0.95.  In Dixon's cell-specific tests ZijD for i = 1,2,3 j = 1,2,3 are calculated
 *      from (Nij - expected value(Nij))/sqrt(variance Nij).  ZijD has a normal distribution with 0 mean and a 
 *      standard deviation of 1.  2 sided tests are used with one tail end of the curve indicating segregation and
 *      the other tail end of the curve indicating association.  Cutoff is at 0.025 at one end of the tail and at
 *      0.975 at the other end of the tail.  Ceyhan's overall test of segregation generates CN
 *      which follows a chi squared distribution with 4 degrees of freedom.  Spatial randomness if rejected if CN
 *      has a value greater than the chi squared cumulative frequency value of 0.95. In Ceyhan's cell-specific
 *      tests ZijN with i = 1,2,3 j = 1,2,3 are calculated from Tij/sqrt(variance Tij).  ZijN has a normal distribution
 *      with 0 mean and a standard deviation of 1.
 *      
 *      Note that there is a contradiction between the equations given for case 2 for Cov[Tii, Tkl] and case 4 for
 *      Cov[Tij, Tkl] between the 2 Ceyhan papers.  Reference 1 has:
 *      Case 2:
 *      Cov[Tii, Tkl] = Cov[Nii, Nkl] - nk*Cov[Nii, Cl]/(n - 1) - (ni - 1)*Cov[Nkl, Ci]/(n - 1)
 *                      + (ni - 1)*nk*Cov[Ci, Cl]/(n-1)**2
 *      case 4:
 *      Cov[Tij, Tkl] = Cov[Nij, Nkl] - nk*Cov[Nij, Cl]/(n - 1) - ni*Cov[Nkl, Cj]/(n - 1)
 *                      + ni*nk*Cov[Cj, Cl]/(n-1)**2
 *                      
 *      Reference 2 has:
 *      Case 2:
 *      Cov[Tii, Tkl] = Cov[Nii, Nkl] - nl*Cov[Nii, Cl]/(n - 1) - (ni - 1)*Cov[Nkl, Ci]/(n - 1)
 *                      + (ni - 1)*nl*Cov[Ci, Cl]/(n-1)**2
 *      case 4:
 *      Cov[Tij, Tkl] = Cov[Nij, Nkl] - nl*Cov[Nij, Cl]/(n - 1) - ni*Cov[Nkl, Cj]/(n - 1)
 *                      + ni*nl*Cov[Cj, Cl]/(n-1)**2
 *                      
 *      nk in the second and fourth terms in both cases in reference 1 becomes nl in reference 2.
 *      I am using the reference 2 version of nl since this yields a CN value = 13.26, close to the
 *      CN = 13.11 calculated by Ceyhan for the Pielou data in reference 1. 
 * 
 References :
 1.) "Overall and pairwise segregation tests based on nearest neighbor contigency tables" by Elvan
 Ceyhan, Computational Statistics and Data Analysis, 53, 2009, pp. 2786-2808.
 2.) Technical Report #KU-EC-08-6: New Tests of Spatial Segregation Based on Nearest Neighbor
 Contingency Tables by Elvan Ceyhan, September 18, 2008
 3.) "Nearest-neighbor contingency table analysis of spatial segregation for several species" 
 by Philip M. Dixon, Ecoscience, Vol. 9, No. 2, 2002, pp. 142-151.
 */
public class AlgorithmThreeClassGeneration extends AlgorithmBase {
    
    // numParents are generated with a uniform random distribution over the square.
    // Type 1 offspring are generated with (numOffspring1/numParents) offspring from
    // each parent in the set with a radially symmetric Gaussian distribution whose
    // standard deviation = normalized standard deviation * (xDim - 1).  Type 2
    // offspring are generated with (numOffspring2/numParents) offspring from each
    // parent in the same set used for the type 1 offspring with the same radially 
    // symmetric Gaussian distribution.  Type 3 offspring are generated with 
    // (numOffspring3/numParents) offspring from each parent in the same set used
    // for type 1 offspring with the same radially symmetric Gaussian distribution.
    // For complete spatial randomness a rejection rate in the test for complete spatial
    // randomness of 0.05 would be expected.
    // In this case the rejection rates are slightly (but significantly) higher than
    // 0.05, so the 3 classes are slightly segregated.
    // Segregation increases as normalizedStdDev decreases.
    public static final int FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS = 1;
    
    // A first set of numParents are generated with a uniform random distribution over the square.
    // Type 1 offspring are generated with (numOffspring1/numParents) offspring from
    // each parent in the first set with a radially symmetric Gaussian distribution whose
    // standard deviation = normalized standard deviation * (xDim - 1).  A second
    // set of numParents are generated with a uniform distribution over the square.
    // Type 2 offspring are generated with (numOffspring2/numParents) offspring from each
    // parent in the second set with the same radially symmetric Gaussian distribution.
    // Type 3 offspring are generated with (numOffspring3/numParents) offspring from each
    // parent in the third set with the same radially symmetric Gaussian distribution.
    // The 3 classes are strongly segregated.
    // Segregation increases as normalizedStdDev decreases.
    public static final int FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS = 2;
    
    // numParents are generated with a uniform random distribution over the square.
    // Type 1 offspring are generated from a randomly chosen parent
    // in the set with a radially symmetric Gaussian distribution whose
    // standard deviation = normalized standard deviation * (xDim - 1).  Type 2
    // offspring are generated from a randomly chosen
    // parent in the same set used for the type 1 offspring with the same radially 
    // symmetric Gaussian distribution.
    // Type 3 offspring are generated from a randomly chosen
    // parent in the same set used for the type 1 offspring with the same radially 
    // symmetric Gaussian distribution.
    // The 3 classes satisfy randomness in the nearest neighbor structure.
    public static final int RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS = 3;
    
    // A first set of numParents are generated with a uniform random distribution over the square.
    // Type 1 offspring are generated from a randomly chosen
    // parent in the first set with a radially symmetric Gaussian distribution whose
    // standard deviation = normalized standard deviation * (xDim - 1).  A second
    // set of numParents are generated with a uniform distribution over the square.
    // Type 2 offspring are generated from a randomly chosen
    // parent in the second set with the same radially symmetric Gaussian distribution.
    // Type 3 offspring are generated from a randomly chosen
    // parent in the third set with the same radially symmetric Gaussian distribution.
    // The 3 classes are strongly segregated.
    // Segregation increases as normalizedStdDev decreases.
    public static final int RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS = 4;
    
    // Put 100 * parentPoissonNormalizedMean points into an area 100 times as large
    // as the actual image area.  Then each parent point is replaced with a random
    // cluster of offspring.  The number of objects inside each cluster are random with
    // a Poisson distribution whose mean = numOffspring1/parentPoissonNormalizedMean for
    // type 1 objects, whose mean = numOffspring2/parentPoissonNormalizedMean for type 2 objects,
    // and whose mean = nunOffspring2/parentPoissonNormalizedMean for type 3 objects.
    // The points are placed independently and uniformly inside a disc with a 
    // disc radius = normalized disc radius * (xDim - 1).  Only consider those objects
    // falling inside the actual image area.  One set of parent points is used for
    // both offspring.
    // The 3 classes satisfy randomness in the nearest neighbor structure.
    public static final int MATERN_SAME_PARENTS = 5;
    
    // Put 100 * parentPoissonNormalizedMean points into an area 100 times as large
    // as the actual image area.  Then each parent point is replaced with a random
    // cluster of type 1 offspring.  The number of type 1 objects inside each cluster are random with
    // a Poisson distribution whose mean = numOffspring1/parentPoissonNormalizedMean.
    // The type 1 offspring are placed independently and uniformly inside a disc with a 
    // disc radius = normalized disc radius * (xDim - 1).  Only consider those points
    // falling inside the actual image area.  A second set of 100 * parentPoissonNormalizedMean
    // parent points is generated for type 2 objects.  Each second set parent point is replaced with
    // a cluster of type 2 offspring.  The number of type 2 objects inside each cluster are random with
    // a Poisson distribution whose mean = numOffspring2/parentPoissonNormalizedMean.  The type 2 
    // offsprihg are placed independently and uniformly inside a disc with a disc radius =
    // normalized disc radius * (xDim - 1).  Only consider those points falling inside the
    // actual image area.  A third set of 100 * parentPoissonNormalizedMean
    // parent points is generated for type 3 objects.  Each third set parent point is replaced with
    // a cluster of type 3 offspring.  The number of type 3 objects inside each cluster are random with
    // a Poisson distribution whose mean = numOffspring3/parentPoissonNormalizedMean.  The type 3 
    // offsprihg are placed independently and uniformly inside a disc with a disc radius =
    // normalized disc radius * (xDim - 1).  Only consider those points falling inside the
    // actual image area.
    // The classes are strongly segregated.
    // As the normalizedDiscRadius increases, the level of segregation decreases.
    public static final int MATERN_DIFFERENT_PARENTS = 6;
    
    // Type 1 objects are generated uniformly over the square (0, (1 - 2*segregation)*(xDim - 1)) by
    // (0, (1 - 2*segregation)*(yDim - 1)).  Type 2 objects are generated uniformly over the square
    // (2*segregation*(xDim - 1), xDim - 1) by (2*segregation*(yDim-1), yDim - 1).  Type 3 objects
    // are generated uniformly over the square (segregation*(xDim-1), (1 - segregation)*(xDim-1)) by
    // (segregation*(yDim - 1), (1 - segregation)*(yDim-1)).
    public static final int SEGREGATION_ALTERNATIVE = 7;
    
    // Type 1 objects are generated with a uniform random distribution over the square.
    // For each type 2 object a type 1 object is randomly selected, a distance ry is generated from a 
    // uniform random distribution of numbers from 0 to discRadius, and an angle is generated from a
    // uniform random distribution of angles from 0 to 2*PI.  The x center of the type 2 object is placed
    // at the x location of the parent type 1 object + ry * cosine(angle).  The y center of the type 2
    // object is placed at the y location of the parent type 1 object + ry * sine(angle).
    // For each type 3 object a type 1 object is randomly selected, a distance rz is generated from a 
    // uniform random distribution of numbers from 0 to discRadius2, and an angle is generated from a
    // uniform random distribution of angles from 0 to 2*PI.  The x center of the type 3 object is placed
    // at the x location of the parent type 1 object + rz * cosine(angle).  The y center of the type 3
    // object is placed at the y location of the parent type 1 object + rz * sine(angle).
    public static final int ASSOCIATION_ALTERNATIVE = 8;
    

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Circle radius
    private int radius;
    
    private int process;
    
    // number of parents
    private int numParents;
    
    private int numOffspring1;
    
    private int numOffspring2;
    
    private int numOffspring3;
    
    private double normalizedStdDev;
    
    private double parentPoissonNormalizedMean;
    
    private double normalizedDiscRadius;
    
    private double normalizedDiscRadius2;
    
    private double segregation;
    
    // Test with NNCT for Pielou's data, shown in Table 3 of "Overall and pairwise segregation tests based on nearest
    // neighbor contingency tables"
    private boolean selfTest1 = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmThreeClassGeneration - default constructor.
     */
    public AlgorithmThreeClassGeneration() { }

    /**
     * AlgorithmThreeClassGeneration.
     *
     * @param  srcImg   Blank source image in which circles will be drawn
     * @param  radius   Circle radius
     * @param  process 
     * @param  numParents Number of parents
     * @param  numOffspring1
     * @param  numOffspring2
     * @param  numOffspring3
     * @param  normalizedStdDev
     * @param  parentPoissonNormalizedMean
     * @param  normalizedDiscRadius
     * @param  normalizedDiscRadius2
     * @param  segregation
     */
    public AlgorithmThreeClassGeneration(ModelImage srcImage, int radius, int process, int numParents, 
            int numOffspring1, int numOffspring2, int numOffspring3, double normalizedStdDev,
            double parentPoissonNormalizedMean, double normalizedDiscRadius, double normalizedDiscRadius2, 
            double segregation) {
        super(null, srcImage);
        this.radius = radius;
        this.process = process;
        this.numParents = numParents;
        this.numOffspring1 = numOffspring1;
        this.numOffspring2 = numOffspring2;
        this.numOffspring3 = numOffspring3;
        this.normalizedStdDev = normalizedStdDev;
        this.parentPoissonNormalizedMean = parentPoissonNormalizedMean;
        this.normalizedDiscRadius = normalizedDiscRadius;
        this.normalizedDiscRadius2 = normalizedDiscRadius2;
        this.segregation = segregation;
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
        final int ONE = 1;
        final int TWO = 2;
        final int THREE = 3;
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
        double stdDev;
        int maskBytesSet;
        Statistics stat;
        double degreesOfFreedom;
        double chiSquaredPercentile[] = new double[1];
        double percentile[] = new double[1];
        byte parentX[] = null;
        int xParentXLocation[];
        int xParentYLocation[];
        int xParentsPlaced;
        byte parentY[] = null;
        int yParentXLocation[] = null;
        int yParentYLocation[] = null;
        int yParentsPlaced = 0;
        byte parentZ[] = null;
        int zParentXLocation[] = null;
        int zParentYLocation[] = null;
        int zParentsPlaced = 0;
        int parentXLocation;
        int parentYLocation;
        int parentZLocation;
        int parentNumber;
        double angle;
        double distance;
        int xCircleXCenter[] = null;
        int xCircleYCenter[] = null;
        int yCircleXCenter[] = null;
        int yCircleYCenter[] = null;
        int zCircleXCenter[] = null;
        int zCircleYCenter[] = null;
        int offspring1Drawn = 0;
        int offspring2Drawn = 0;
        int offspring3Drawn = 0;
        int offspring1PerParent;
        int offspring2PerParent;
        int offspring3PerParent;
        int discRadius;
        int discRadius2;
        int expandedXDim;
        int expandedYDim;
        int expandedLength;
        int discRadiusSquared;
        int xDiscMaskDim;
        int yDiscMaskDim;
        byte discMask[];
        int xDiscMask[];
        int yDiscMask[];
        int zDiscMask[];
        int discMaskBytesSet;
        int paddedBuffer[];
        double offspring1PoissonMean;
        double offspring2PoissonMean;
        double offspring3PoissonMean;
        int pointsInCluster;
        double poissonValues[];
        int events;
        double gain;
        double offset;
        int discIndex;
        int parentsPlaced;
        int xNumber;
        double ry;
        double rz;
        double NN1Distance[];
        byte NN1Type[];
        double NN2Distance[];
        byte NN2Type[];
        double NN3Distance[];
        byte NN3Type[];
        int NN1Neighbor[];
        int NN2Neighbor[];
        int NN3Neighbor[];
        int N11;
        int N12;
        int N13;
        int N21;
        int N22;
        int N23;
        int N31;
        int N32;
        int N33;
        int C1;
        int C2;
        int C3;
        long n1;
        long n2;
        long n3;
        long n;
        double EN11;
        double EN12;
        double EN13;
        double EN21;
        double EN22;
        double EN23;
        double EN31;
        double EN32;
        double EN33;
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
        int Q3Array[];
        double p11;
        double p111;
        double p1111;
        double p12;
        double p112;
        double p1122;
        double p13;
        double p113;
        double p1133;
        double p21;
        double p221;
        double p2211;
        double p22;
        double p222;
        double p2222;
        double p23;
        double p223;
        double p2233;
        double p31;
        double p32;
        double p331;
        double p3311;
        double p332;
        double p3322;
        double p33;
        double p333;
        double p3333;
        double varN11;
        double varN12;
        double varN13;
        double varN21;
        double varN22;
        double varN23;
        double varN31;
        double varN32;
        double varN33;
        double covN11N22;
        double r;
        double CD;
        // Under complete spatial randomness independence, zijD asymptotically has a N(0,1) distribution 
        // conditional on Q and R;
        double z11D;
        double z12D;
        double z13D;
        double z21D;
        double z22D;
        double z23D;
        double z31D;
        double z32D;
        double z33D;
        double T11;
        double T12;
        double T21;
        double T22;
        double p122;
        double p1112;
        double p2221;
        double covN11N12;
        double covN11N21;
        double covN21N11;
        double covN12N21;
        double covN12N22;
        double varC1;
        double varC2;
        double covN11C1;
        double covN12C2;
        double covN21C1;
        double covN22C2;
        double varT11;
        double varT12;
        double varT21;
        double varT22;
        double z11N;
        double z12N;
        double z21N;
        double z22N;
        double covN21N12;
        double covN12N11;
        double covN21N22;
        double covN22N11;
        double covN22N21;
        double covN22N12;
        double covN11C2;
        double covN12C1;
        double covN22C1;
        double covN21C2;
        double covC1C1;
        double covC1C2;
        double covC2C1;
        double covC2C2;
        double covT11T12;
        double covT11T21;
        double covT11T22;
        double covT12T11;
        double covT12T21;
        double covT12T22;
        double covT21T11;
        double covT21T12;
        double covT21T22;
        double covT22T11;
        double covT22T12;
        double covT22T21;
        double sigma[][];
        Matrix sigmaN;
        double Tp[][];
        Matrix TpM;
        double T[][];
        Matrix TM;
        double CN[][];
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Three class generation ...");

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
        
        switch(process) {
            case FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS:
                Preferences.debug("FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS\n");
                System.out.println("FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS");
                break;
            case FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS:
                Preferences.debug("FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS\n");
                System.out.println("FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS");
                break;
            case RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS:
                Preferences.debug("RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS\n");
                System.out.println("RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS");
                break;
            case RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS:
                Preferences.debug("RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS\n");
                System.out.println("RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS");
                break;
            case MATERN_SAME_PARENTS:
                Preferences.debug("MATERN_SAME_PARENTS\n");
                System.out.println("MATERN_SAME_PARENTS");
                break;
            case MATERN_DIFFERENT_PARENTS:
                Preferences.debug("MATERN_DIFFERENT_PARENTS\n");
                System.out.println("MATERN_DIFFERENT_PARENTS");
                break;
            case SEGREGATION_ALTERNATIVE:
                Preferences.debug("SEGREGATION_ALTERNATIVE\n");
                System.out.println("SEGREGATION_ALTERNATIVE");
                break;
            case ASSOCIATION_ALTERNATIVE:
                Preferences.debug("ASSOCIATION_ALTERNATIVE\n");
                System.out.println("ASSOCIATION_ALTERNATIVE");
                break;
        }
        
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
            zCircleXCenter = new int[numOffspring3];
            zCircleYCenter = new int[numOffspring3];
            offspring1PerParent = numOffspring1/numParents;
            offspring2PerParent = numOffspring2/numParents;
            offspring3PerParent = numOffspring3/numParents;
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
                    
                    parentZ = new byte[length];
                    zParentXLocation = new int[numParents];
                    zParentYLocation = new int[numParents];
                    for (i = 0; i < numParents; i++) {
                        found = false;
                        attempts = 0;
                        while ((!found) && (attempts <= 100)) {
                            found = true;
                            xCenter = randomGen.genUniformRandomNum(0, xDim - 1);
                            yCenter = randomGen.genUniformRandomNum(0, yDim - 1);
                            if ((parentX[xCenter + xDim * yCenter] != 0) || (parentY[xCenter + xDim * yCenter] != 0) ||
                                (parentZ[xCenter + xDim * yCenter] != 0)) {
                                found = false;
                                attempts++;
                            }
                            else {
                                zParentXLocation[i] = xCenter;
                                zParentYLocation[i] = yCenter;
                                parentZ[xCenter + xDim * yCenter] = 1;
                            }
                        } // while ((!found) && (attempts <= 100))
                        if (!found) {
                            break;
                        }
                    } // for (i = 0; i < numParents; i++)
                    zParentsPlaced = i;
                    if (zParentsPlaced == 1) {
                        if (numParents != 1) {
                            Preferences.debug("1 Z parent point placed.  " + numParents + " parent points requested.\n");
                            System.out.println("1 Z parent point placed. " + numParents + " parent points requested.");
                            setCompleted(false);
                            return;
                            
                        }
                        else {
                            Preferences.debug("1 Z parent point placed.  1 parent point requested\n");
                            System.out.println("1 Z parent point placed.  1 parent point requested");    
                        }
                    }
                    else if (zParentsPlaced != numParents) {
                        Preferences.debug(zParentsPlaced + " Z parent points placed.  " +
                                          numParents + " parent points requested.\n");
                        System.out.println(zParentsPlaced + " Z parent points placed.  " +
                                numParents + " parent points requested.");
                        setCompleted(false);
                        return;
                    }   
                    else { // zParentsPlaced == numParents
                        Preferences.debug(zParentsPlaced + " Z parent points placed.  " +
                                numParents + " parent points requested.\n");
                        System.out.println(zParentsPlaced + " Z parent points placed.  " +
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
            
            for (i = 0; i < numOffspring3; i++) {
                if (process == FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) {
                    parentXLocation = xParentXLocation[i/offspring3PerParent];
                    parentYLocation = xParentYLocation[i/offspring3PerParent];
                }
                else if (process == FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS) {
                    parentXLocation = zParentXLocation[i/offspring3PerParent];
                    parentYLocation = zParentYLocation[i/offspring3PerParent];    
                }
                else if (process == RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS) {
                    parentNumber =  randomGen.genUniformRandomNum(0, numParents - 1);
                    parentXLocation = xParentXLocation[parentNumber];
                    parentYLocation = xParentYLocation[parentNumber];
                }
                else {
                    parentNumber =  randomGen.genUniformRandomNum(0, numParents - 1);
                    parentXLocation = zParentXLocation[parentNumber];
                    parentYLocation = zParentYLocation[parentNumber];    
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
                    r2zloop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r2zloop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
                zCircleXCenter[i] = xCenter;
                zCircleYCenter[i] = yCenter;
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  3;
                        }
                    }
                }
            } // for (i = 0; i < numOffspring3; i++)
            offspring3Drawn = i;
            Preferences.debug(offspring3Drawn + " offspring 3 drawn.  " + numOffspring3 + " offspring 3 requested.\n");
            System.out.println(offspring3Drawn + " offspring 3 drawn.  " + numOffspring3 + " offspring 3 requested.");
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
                
                parentZ = new byte[expandedLength];
                zParentXLocation = new int[numParents];
                zParentYLocation = new int[numParents];
                for (i = 0, j = 0; i < numParents; i++) {
                    found = false;
                    attempts = 0;
                    while ((!found) && (attempts <= 100)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(0, expandedXDim - 1);
                        yCenter = randomGen.genUniformRandomNum(0, expandedYDim - 1);
                        if ((parentX[xCenter + expandedXDim * yCenter] != 0) || 
                            (parentY[xCenter + expandedXDim * yCenter] != 0) || 
                            (parentZ[xCenter + expandedXDim * yCenter] != 0)){
                            found = false;
                            attempts++;
                        }
                        else {
                            parentZ[xCenter + expandedXDim * yCenter] = 1;
                            if ((xCenter >= 4*xDim - discRadius) && (xCenter <= 5*xDim - 1 + discRadius) &&
                                (yCenter >= 4*yDim - discRadius) && (yCenter <= 5*yDim - 1 + discRadius)) {
                                // Go from discRadius to dim + 3 * discRadius -1 in paddedBuffer
                                zParentXLocation[j] = xCenter - (4 * xDim - 2*discRadius);
                                zParentYLocation[j++] = yCenter - (4 * yDim - 2*discRadius);
                            }
                        }
                    } // while ((!found) && (attempts <= 100))
                    if (!found) {
                        break;
                    }    
                } // for (i = 0; i < numParents; i++)
                zParentsPlaced = j;
                Preferences.debug(zParentsPlaced + " Z parents placed in padded buffer.  Mean of " + 
                                  parentPoissonNormalizedMean + " Z parents requested for unpadded buffer.\n");
                System.out.println(zParentsPlaced + " Z parents placed in padded buffer.  Mean of " + 
                                  parentPoissonNormalizedMean + " Z parents requested for unpadded buffer."); 
                if (zParentsPlaced == 0) {
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
                         r4loop:
                             for (y = 0; y <= 2*radius; y++) {
                                 for (x = 0; x <= 2*radius; x++) {
                                     if (mask[x + y * xMaskDim] == 1) {
                                         if (paddedBuffer[(xCenter + x - radius) + 
                                                          (xDim + 4 * discRadius)*(yCenter + y - radius)] != 0) {
                                             found = false;
                                             attempts++;
                                             break r4loop;
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
            
            offspring3PoissonMean = numOffspring3/parentPoissonNormalizedMean;
            if (process == MATERN_SAME_PARENTS) {
                parentsPlaced = xParentsPlaced;
            }
            else {
                parentsPlaced = zParentsPlaced;   
            }
            zCircleXCenter = new int[(int)Math.round(2 * parentsPlaced * offspring3PoissonMean)];
            zCircleYCenter = new int[yCircleXCenter.length];
            offspring3Drawn = 0;
            for (i = 0; i < parentsPlaced; i++) {
                 events = 1;
                 gain = 1.0;
                 offset = 0;
                 poissonValues = randomGen.poissDecay(events, offspring3PoissonMean, gain, offset);
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
                             xCenter = zParentXLocation[i] + xDiscMask[discIndex] - discRadius;
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
                             yCenter = zParentYLocation[i] + yDiscMask[discIndex] - discRadius;    
                         }
                         if ((yCenter - radius < 0) || (yCenter + radius > yDim + 4*discRadius - 1)) {
                             found = false;
                             attempts++;
                             continue;
                         }
                         r4zloop:
                             for (y = 0; y <= 2*radius; y++) {
                                 for (x = 0; x <= 2*radius; x++) {
                                     if (mask[x + y * xMaskDim] == 1) {
                                         if (paddedBuffer[(xCenter + x - radius) + 
                                                          (xDim + 4 * discRadius)*(yCenter + y - radius)] != 0) {
                                             found = false;
                                             attempts++;
                                             break r4zloop;
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
                                 paddedBuffer[(xCenter + x - radius) + (xDim + 4 * discRadius) *(yCenter + y - radius)] =  3;
                             }
                         }
                     }  
                     
                     if ((xCenter - radius >= 2 * discRadius) && (xCenter + radius < xDim + 2 * discRadius) &&
                             (yCenter - radius >= 2 * discRadius) && (yCenter + radius < yDim + 2 * discRadius)) {
                         // Subtract 2*discRadius to change offset from paddedBuffer to buffer
                         zCircleXCenter[offspring3Drawn] = xCenter - 2 * discRadius;
                         zCircleYCenter[offspring3Drawn++] = yCenter - 2 * discRadius;
                     }
                 } // for (j = 0; j < pointsInCluster; j++)
            } // for (i = 0; i < parentsPlaced; i++)
            Preferences.debug(offspring3Drawn + " offspring 3 drawn\n");
            System.out.println(offspring3Drawn + " offspring 3 drawn");
            
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    buffer[x + xDim * y] = paddedBuffer[(x + 2 * discRadius) + (xDim + 4 * discRadius) * (y + 2 * discRadius)];
                }
            }
        } // if ((process == MATERN_SAME_PARENTS) || (process == MATERN_DIFFERENT_PARENTS))
        
        if (process == SEGREGATION_ALTERNATIVE) {
            xCircleXCenter = new int[numOffspring1];
            xCircleYCenter = new int[numOffspring1];
            yCircleXCenter = new int[numOffspring2];
            yCircleYCenter = new int[numOffspring2];
            zCircleXCenter = new int[numOffspring3];
            zCircleYCenter = new int[numOffspring3];
            for (i = 0; i < numOffspring1; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum(0, (int)Math.round((xDim - 1)*(1 - 2*segregation)));
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = randomGen.genUniformRandomNum(0, (int)Math.round((yDim - 1)*(1 - 2*segregation)));
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    r7loop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r7loop;
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
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum((int)Math.round(2*segregation*(xDim-1)), xDim-1);
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = randomGen.genUniformRandomNum((int)Math.round(2*segregation*(yDim-1)), yDim-1);
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    r8loop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r8loop;
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
            
            for (i = 0; i < numOffspring3; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum((int)Math.round(segregation*(xDim-1)), (int)Math.round((1 - segregation)*(xDim-1)));
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = randomGen.genUniformRandomNum((int)Math.round(segregation*(yDim-1)), (int)Math.round((1 - segregation)*(yDim-1)));
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    r8zloop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r8zloop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
                zCircleXCenter[i] = xCenter;
                zCircleYCenter[i] = yCenter;
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  3;
                        }
                    }
                }
            } // for (i = 0; i < numOffspring3; i++)
            offspring3Drawn = i;
            Preferences.debug(offspring3Drawn + " offspring 3 drawn.  " + numOffspring3 + " offspring 3 requested.\n");
            System.out.println(offspring2Drawn + " offspring 3 drawn.  " + numOffspring2 + " offspring 3 requested.");
            
        } // if (process == SEGREGATION_ALTERNATIVE)
        
        if (process == ASSOCIATION_ALTERNATIVE) {
            discRadius = (int)Math.round(normalizedDiscRadius * (xDim - 1));
            discRadius2 = (int)Math.round(normalizedDiscRadius2 * (xDim - 1));
            xCircleXCenter = new int[numOffspring1];
            xCircleYCenter = new int[numOffspring1];
            yCircleXCenter = new int[numOffspring2];
            yCircleYCenter = new int[numOffspring2];
            zCircleXCenter = new int[numOffspring3];
            zCircleYCenter = new int[numOffspring3];
            for (i = 0; i < numOffspring1; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xCenter = randomGen.genUniformRandomNum(0, xDim);
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = randomGen.genUniformRandomNum(0, yDim);
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    r9loop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r9loop;
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
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xNumber =  randomGen.genUniformRandomNum(0, offspring1Drawn - 1);
                    parentXLocation = xCircleXCenter[xNumber];
                    parentYLocation = xCircleYCenter[xNumber];
                    ry = randomGen.genUniformRandomNum(0.0, discRadius);
                    angle = randomGen.genUniformRandomNum(0.0, 2.0*Math.PI);
                    xCenter = (int)Math.round(parentXLocation + ry * Math.cos(angle));
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = (int)Math.round(parentYLocation + ry * Math.sin(angle));
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    r10loop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r10loop;
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
            
            for (i = 0; i < numOffspring3; i++) {
                found = false;
                attempts = 0;
                while ((!found) && (attempts <= 100)) {
                    found = true;
                    xNumber =  randomGen.genUniformRandomNum(0, offspring1Drawn - 1);
                    parentXLocation = xCircleXCenter[xNumber];
                    parentYLocation = xCircleYCenter[xNumber];
                    rz = randomGen.genUniformRandomNum(0.0, discRadius2);
                    angle = randomGen.genUniformRandomNum(0.0, 2.0*Math.PI);
                    xCenter = (int)Math.round(parentXLocation + rz * Math.cos(angle));
                    if ((xCenter - radius < 0) || (xCenter + radius > xDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    yCenter = (int)Math.round(parentYLocation + rz * Math.sin(angle));
                    if ((yCenter - radius < 0) || (yCenter + radius > yDim - 1)) {
                        found = false;
                        attempts++;
                        continue;
                    }
                    r10zloop:
                        for (y = 0; y <= 2*radius; y++) {
                            for (x = 0; x <= 2*radius; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] != 0) {
                                        found = false;
                                        attempts++;
                                        break r10zloop;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                } // while ((!found) && (attempts <= 100))
                if (!found) {
                    break;
                }
                zCircleXCenter[i] = xCenter;
                zCircleYCenter[i] = yCenter;
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] =  3;
                        }
                    }
                }
            } // for (i = 0; i < numOffspring3; i++)
            offspring3Drawn = i;
            Preferences.debug(offspring3Drawn + " offspring 3 drawn.  " + numOffspring2 + " offspring 3 requested.\n");
            System.out.println(offspring3Drawn + " offspring 3 drawn.  " + numOffspring3 + " offspring 3 requested.");
        } // if (process == ASSOCIATION_ALTERNATIVE)
        
        N11 = 0;
        N12 = 0;
        N13 = 0;
        N21 = 0;
        N22 = 0;
        N23 = 0;
        N31 = 0;
        N32 = 0;
        N33 = 0;
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
                        NN1Type[i] = ONE;
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
                    NN1Type[i] = TWO;
                    NN1Neighbor[i] = j;
                }  
            }
            
            for (j = 0; j < offspring3Drawn; j++) {          
                xDistSquared = xCircleXCenter[i] - zCircleXCenter[j];
                xDistSquared = xDistSquared * xDistSquared;
                yDistSquared = xCircleYCenter[i] - zCircleYCenter[j];
                yDistSquared = yDistSquared * yDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared < lowestDistSquared) {
                    lowestDistSquared = distSquared;
                    NN1Distance[i] = Math.sqrt(distSquared);
                    NN1Type[i] = THREE;
                    NN1Neighbor[i] = j;
                }  
            }
        } // for (i = 0; i < offspring1Drawn; i++)
        
        for (i = 0; i < offspring1Drawn; i++) {
            if (NN1Type[i] == ONE) {
                N11++;
            }
            else if (NN1Type[i] == TWO) {
                N12++;
            }
            else {
                N13++;
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
                    NN2Type[i] = ONE;
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
                        NN2Type[i] = TWO;
                        NN2Neighbor[i] = j;
                    } 
                }
            }
            
            for (j = 0; j < offspring3Drawn; j++) {         
                xDistSquared = yCircleXCenter[i] - zCircleXCenter[j];
                xDistSquared = xDistSquared * xDistSquared;
                yDistSquared = yCircleYCenter[i] - zCircleYCenter[j];
                yDistSquared = yDistSquared * yDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared < lowestDistSquared) {
                    lowestDistSquared = distSquared;
                    NN2Distance[i] = Math.sqrt(distSquared);
                    NN2Type[i] = THREE;
                    NN2Neighbor[i] = j;
                }  
            }
        } // for (i = 0; i < offspring2Drawn; i++)
        
        for (i = 0; i < offspring2Drawn; i++) {
            if (NN2Type[i] == ONE) {
                N21++;
            }
            else if (NN2Type[i] == TWO){
                N22++;
            }
            else {
                N23++;
            }
        }
        
        NN3Distance = new double[offspring3Drawn];
        NN3Type = new byte[offspring3Drawn];
        NN3Neighbor = new int[offspring3Drawn];
        for (i = 0; i < offspring3Drawn; i++) {
            lowestDistSquared = Integer.MAX_VALUE;
            for (j = 0; j < offspring1Drawn; j++) {         
                xDistSquared = zCircleXCenter[i] - xCircleXCenter[j];
                xDistSquared = xDistSquared * xDistSquared;
                yDistSquared = zCircleYCenter[i] - xCircleYCenter[j];
                yDistSquared = yDistSquared * yDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared < lowestDistSquared) {
                    lowestDistSquared = distSquared;
                    NN2Distance[i] = Math.sqrt(distSquared);
                    NN2Type[i] = ONE;
                    NN2Neighbor[i] = j;
                }  
            }
            
            for (j = 0; j < offspring2Drawn; j++) { 
                xDistSquared = zCircleXCenter[i] - yCircleXCenter[j];
                xDistSquared = xDistSquared * xDistSquared;
                yDistSquared = zCircleYCenter[i] - yCircleYCenter[j];
                yDistSquared = yDistSquared * yDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared < lowestDistSquared) {
                    lowestDistSquared = distSquared;
                    NN2Distance[i] = Math.sqrt(distSquared);
                    NN2Type[i] = TWO;
                    NN2Neighbor[i] = j;
                } 
            }
            
            for (j = 0; j < offspring3Drawn; j++) { 
                if (i != j) {
                    xDistSquared = zCircleXCenter[i] - zCircleXCenter[j];
                    xDistSquared = xDistSquared * xDistSquared;
                    yDistSquared = zCircleYCenter[i] - zCircleYCenter[j];
                    yDistSquared = yDistSquared * yDistSquared;
                    distSquared = xDistSquared + yDistSquared;
                    if (distSquared < lowestDistSquared) {
                        lowestDistSquared = distSquared;
                        NN2Distance[i] = Math.sqrt(distSquared);
                        NN2Type[i] = THREE;
                        NN2Neighbor[i] = j;
                    } 
                }
            }
        } // for (i = 0; i < offspring3Drawn; i++)
        
        for (i = 0; i < offspring3Drawn; i++) {
            if (NN3Type[i] == ONE) {
                N31++;
            }
            else if (NN3Type[i] == TWO){
                N32++;
            }
            else {
                N33++;
            }
        }
        
        C1 = N11 + N21 + N31;
        C2 = N12 + N22 + N32;
        C3 = N13 + N23 + N33;
        n1 = N11 + N12 + N13;
        n2 = N21 + N22 + N23;
        n3 = N31 + N32 + N33;
        n = n1 + n2 + n3;
        
        // Dixon's cell-specific tests of segregation
        EN11 = ((double)n1*(n1 - 1))/(n - 1);
        EN12 = ((double)n1 * n2)/(n - 1);
        EN13 = ((double)n1 * n3)/(n - 1);
        EN21 = EN12;
        EN22 = ((double)n2*(n2 - 1))/(n - 1);
        EN23 = ((double)n2 * n3)/(n - 1);
        EN31 = EN13;
        EN32 = EN23;
        EN33 = ((double)n3*(n3 - 1))/(n - 1);
        
        // Find R, twice the number of reflexive pairs.
        R = 0;
        for (i = 0; i < offspring1Drawn; i++) {
            if (NN1Type[i] == ONE && NN1Type[NN1Neighbor[i]] == ONE && NN1Neighbor[NN1Neighbor[i]] == i) {
                R++;
            }
            else if (NN1Type[i] == TWO && NN2Type[NN1Neighbor[i]] == ONE && NN2Neighbor[NN1Neighbor[i]] == i) {
                R++;
            }
            else if (NN1Type[i] == THREE && NN3Type[NN1Neighbor[i]] == ONE && NN3Neighbor[NN1Neighbor[i]] == i) {
                R++;
            }
        }
        
        for (i = 0; i < offspring2Drawn; i++) {
            if (NN2Type[i] == ONE && NN1Type[NN2Neighbor[i]] == TWO && NN1Neighbor[NN2Neighbor[i]] == i) {
                R++;
            }
            else if (NN2Type[i] == TWO && NN2Type[NN2Neighbor[i]] == TWO && NN2Neighbor[NN2Neighbor[i]] == i) {
                R++;
            }
            else if (NN2Type[i] == THREE && NN3Type[NN2Neighbor[i]] == TWO && NN3Neighbor[NN2Neighbor[i]] == i) {
                R++;
            }
        }
        
        for (i = 0; i < offspring3Drawn; i++) {
            if (NN3Type[i] == ONE && NN1Type[NN3Neighbor[i]] == THREE && NN1Neighbor[NN3Neighbor[i]] == i) {
                R++;
            }
            else if (NN3Type[i] == TWO && NN2Type[NN3Neighbor[i]] == THREE && NN2Neighbor[NN3Neighbor[i]] == i) {
                R++;
            }
            else if (NN3Type[i] == THREE && NN3Type[NN3Neighbor[i]] == THREE && NN3Neighbor[NN3Neighbor[i]] == i) {
                R++;
            }
        }
        
        Q1Array = new int[offspring1Drawn];
        Q2Array = new int[offspring2Drawn];
        Q3Array = new int[offspring3Drawn];
        for (i = 0; i < offspring1Drawn; i++) {
            if (NN1Type[i] == ONE) {
                Q1Array[NN1Neighbor[i]]++;
            }
            else if (NN1Type[i] == TWO){
                Q2Array[NN1Neighbor[i]]++;
            }
            else {
                Q3Array[NN1Neighbor[i]]++;
            }
        }
        
        for (i = 0; i < offspring2Drawn; i++) {
            if (NN2Type[i] == ONE) {
                Q1Array[NN2Neighbor[i]]++;    
            }
            else if (NN2Type[i] == TWO){
                Q2Array[NN2Neighbor[i]]++;
            }
            else {
                Q3Array[NN2Neighbor[i]]++;
            }
        }
        
        for (i = 0; i < offspring3Drawn; i++) {
            if (NN3Type[i] == ONE) {
                Q1Array[NN3Neighbor[i]]++;    
            }
            else if (NN3Type[i] == TWO){
                Q2Array[NN3Neighbor[i]]++;
            }
            else {
                Q3Array[NN3Neighbor[i]]++;
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
        
        for (i = 0; i < offspring3Drawn; i++) {
            if (Q3Array[i] == 2) {
                Q2++;
            }
            else if (Q3Array[i] == 3) {
                Q3++;
            }
            else if (Q3Array[i] == 4) {
                Q4++;
            }
            else if (Q3Array[i] == 5) {
                Q5++;
            }
            else if (Q3Array[i] == 6) {
                Q6++;
            }
        }
        
        Q = 2 * (Q2 + 3*Q3 + 6*Q4 + 10*Q5 + 15*Q6);
        
        p11 = ((double)n1*(n1 - 1))/(n*(n - 1));
        p111 = ((double)n1*(n1 - 1)*(n1 - 2))/(n*(n - 1)*(n - 2));
        p1111 = ((double)n1*(n1 - 1)*(n1 - 2)*(n1 - 3))/(n*(n - 1)*(n - 2)*(n - 3));
        p12 = ((double)n1*n2)/(n*(n - 1));
        p13 = ((double)n1*n3)/(n*(n-1));
        p112 = ((double)n1*(n1 - 1)*n2)/(n*(n - 1)*(n - 2));
        p113 = ((double)n1*(n1 - 1)*n3)/(n*(n - 1)*(n - 2));
        p1122 = ((double)n1*(n1 - 1)*n2*(n2 - 1))/(n*(n - 1)*(n - 2)*(n - 3));
        p1133 = ((double)n1*(n1 - 1)*n3*(n3 - 1))/(n*(n - 1)*(n - 2)*(n - 3));
        p21 = p12;
        p221 = ((double)n2*(n2 - 1)*n1)/(n*(n - 1)*(n - 2));
        p2211 = p1122;
        p22 = ((double)n2*(n2 - 1))/(n*(n-1));
        p222 = ((double)n2*(n2 - 1)*(n2 - 2))/(n*(n - 1)*(n - 2));
        p2222 = ((double)n2*(n2 - 1)*(n2 - 2)*(n2 - 3))/(n*(n - 1)*(n - 2)*(n - 3));
        p23 = ((double)n2*n3)/(n*(n-1));
        p223 = ((double)n2*(n2 - 1)*n3)/(n*(n - 1)*(n - 2));
        p2233 = ((double)n2*(n2 - 1)*n3*(n3 - 1))/(n*(n - 1)*(n - 2)*(n - 3));
        p31 = p13;
        p331 = ((double)n3*(n3 - 1)*n1)/(n*(n - 1)*(n - 2));
        p3311 = p1133;
        p32 = p23;
        p332 = ((double)n3*(n3 - 1)*n2)/(n*(n - 1)*(n - 2));
        p3322 = p2233;
        p33 = ((double)n3*(n3 - 1))/(n*(n - 1));
        p333 = ((double)n3*(n3 - 1)*(n3 - 2))/(n*(n - 1)*(n - 2));
        p3333 = ((double)n3*(n3 - 1)*(n3 - 2)*(n3 - 3))/(n*(n - 1)*(n - 2)*(n - 3));
        
        
        varN11 = (n + R)*p11 + (2*n - 2*R + Q)*p111 + (n*n - 3*n - Q + R)*p1111 -n*n*p11*p11;
        varN12 = n*p12 + Q*p112 + (n*n - 3*n - Q + R)*p1122 - n*n*p12*p12;
        varN13 = n*p13 + Q*p113 + (n*n - 3*n - Q + R)*p1133 - n*n*p13*p13;
        varN21 = n*p21 + Q*p221 + (n*n - 3*n - Q + R)*p2211 - n*n*p21*p21;
        varN22 = (n + R)*p22 + (2*n - 2*R + Q)*p222 + (n*n - 3*n - Q + R)*p2222 - n*n*p22*p22;
        varN23 = n*p23 + Q*p223 + (n*n - 3*n - Q + R)*p2233 - n*n*p23*p23;
        varN31 = n*p31 + Q*p331 + (n*n - 3*n - Q + R)*p3311 - n*n*p31*p31;
        varN32 = n*p32 + Q*p332 + (n*n - 3*n - Q + R)*p3322 - n*n*p32*p32;
        varN33 = (n + R)*p33 + (2*n - 2*R + Q)*p333 + (n*n - 3*n - Q + R)*p3333 - n*n*p33*p33;
        
        z11D = (N11 - EN11)/Math.sqrt(varN11);
        z12D = (N12 - EN12)/Math.sqrt(varN12);
        z13D = (N13 - EN13)/Math.sqrt(varN13);
        Preferences.debug("z11D = " + z11D + "\n"); 
        Preferences.debug("z12D = " + z12D + "\n");
        Preferences.debug("z13D = " + z13D + "\n");
        
        z21D = (N21 - EN21)/Math.sqrt(varN21);
        z22D = (N22 - EN22)/Math.sqrt(varN22);
        z23D = (N23 - EN23)/Math.sqrt(varN23);
        Preferences.debug("z21D = " + z21D + "\n");
        Preferences.debug("z22D = " + z22D + "\n");
        Preferences.debug("z23D = " + z23D + "\n");
        
        z31D = (N31 - EN31)/Math.sqrt(varN31);
        z32D = (N32 - EN32)/Math.sqrt(varN32);
        z33D = (N33 - EN33)/Math.sqrt(varN33);
        Preferences.debug("z31D = " + z31D + "\n");
        Preferences.debug("z32D = " + z32D + "\n");
        Preferences.debug("z33D = " + z33D + "\n");
        
        covN11N22 = (n*n - 3*n - Q + R)*p1122 - n*n*p11*p22;
        p122 = ((double)n1*n2*(n2 - 1))/(n*(n - 1)*(n - 2));
        p1112 = ((double)n1*(n1 - 1)*(n1 - 2)*n2)/(n*(n - 1)*(n - 2)*(n - 3));
        p2221 = ((double)n2*(n2 - 1)*(n2 - 2)*n1)/(n*(n - 1)*(n - 2)*(n - 3));
        covN11N12 = (n - R)*p112 + (n*n - 3*n - Q + R)*p1112 - n*n*p11*p12;
        covN11N21 = (n - R + Q)*p112 + (n*n - 3*n - Q + R)*p1112 - n*n*p11*p12;
        covN12N21 = R*p12 + (n - R)*(p112 + p122) + (n*n - 3*n - Q + R)*p1122 - n*n*p12*p21;
        covN12N22 = (n - R + Q)*p221 + (n*n - 3*n - Q + R)*p2221 - n*n*p22*p21;
        covN12N11 = covN11N12;
        covN21N12 = covN12N21;
        covN21N22 = (n - R)*p221 + (n*n - 3*n - Q + R)*p2221 - n*n*p22*p21;
        covN21N11 = covN11N21;
        covN22N11 = covN11N22;
        covN22N21 = covN21N22;
        covN22N12 = covN12N22;
        //Preferences.debug("CD = " + CD + "\n");
        
        // Under random labelling the chi squared statistic has degrees of freedom = 6;
        /*degreesOfFreedom = 6;
        stat = new Statistics(Statistics.CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION,
                CD, degreesOfFreedom, chiSquaredPercentile);
        stat.run();
        Preferences.debug("chiSquared percentile for Dixon's overall test of segregation = " + chiSquaredPercentile[0]*100.0 + "\n");
        System.out.println("chiSquared percentile for Dixon's overall test of segregation = " + chiSquaredPercentile[0]*100.0);
        
        if (chiSquaredPercentile[0] > 0.950) {
            Preferences.debug("chiSquared test rejects random object distribution\n");
            System.out.println("chiSquared test rejects random object distribution"); 
        }
        else {
            Preferences.debug("chiSquared test does not reject random object distribution\n");
            System.out.println("chiSquared test does not reject random object distribution");
        }*/
        
        Preferences.debug("Dixon's cell-specific tests of segregation\n");
        System.out.println("Dixon's cell-specific tests of segregation");
        
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
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z13D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N13 around expected mean N13 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N13 around expected mean N13 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N13 indicates segregation\n");
            System.out.println("Low value of N13 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N13 indicates association\n");
            System.out.println("High value of N13 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N13 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N13 value");
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
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z23D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N23 around expected mean N23 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N23 around expected mean N23 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N23 indicates segregation\n");
            System.out.println("Low value of N23 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N23 indicates association\n");
            System.out.println("High value of N23 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N23 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N23 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z31D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N31 around expected mean N31 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N31 around expected mean N31 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N31 indicates segregation\n");
            System.out.println("Low value of N31 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N31 indicates association\n");
            System.out.println("High value of N31 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N31 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N31 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z32D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N32 around expected mean N32 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N32 around expected mean N32 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N32 indicates segregation\n");
            System.out.println("Low value of N32 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N32 indicates association\n");
            System.out.println("High value of N32 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N32 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N32 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z33D, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean N33 around expected mean N33 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean N33 around expected mean N33 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of N33 indicates association\n");
            System.out.println("Low value of N33 indicates association");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of N33 indicates segregation\n");
            System.out.println("High value of N33 indicates segregation");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on N33 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on N33 value");
        }
        
        T11 = N11 - ((double)(n1 - 1)*C1)/(n - 1);
        T12 = N12 - ((double)n1*C2)/(n - 1);
        T21 = N21 - ((double)n2*C1)/(n - 1);
        T22 = N22 - ((double)(n2 - 1)*C2)/(n - 1);
        
        varC1 = varN11 + varN21 + 2 * covN11N21;
        varC2 = varN12 + varN22 + 2 * covN12N22;
        covN11C1 = varN11 + covN11N21;
        covN12C2 =  varN12 + covN12N22;
        covN21C1 = varN21 + covN11N21;
        covN22C2 =  varN22 + covN12N22;
        varT11 = varN11 + (n1 - 1)*(n1 - 1)*varC1/((n - 1)*(n - 1))
                  - 2 * (n1 - 1)*covN11C1/(n - 1);
        varT12 = varN12 + n1*n1*varC2/((n - 1) * (n - 1))
                 - 2 * n1*covN12C2/(n - 1);
        varT21 = varN21 + n2*n2*varC1/((n - 1)*(n - 1))
                 - 2 * n2*covN21C1/(n - 1);
        varT22 = varN22 + (n2 - 1)*(n2 - 1)*varC2/((n - 1)*(n - 1))
                 - 2 * (n2 - 1)*covN22C2/(n - 1);
        
        z11N = T11/Math.sqrt(varT11);
        z12N = T12/Math.sqrt(varT12);
        z21N = T21/Math.sqrt(varT21);
        z22N = T22/Math.sqrt(varT22);
        Preferences.debug("z11N = " + z11N + "\n");
        Preferences.debug("z21N = " + z21N + "\n");
        Preferences.debug("z12N = " + z12N + "\n");
        Preferences.debug("z22N = " + z22N + "\n");
        
        // CN = T'SigmaNInverseT
        // CN has a chiSquared distribution with 1 degree of freedom
        // T' = [T11 T12 T21 T22]
        // T = [T11]
        //     [T12]
        //     [T21]
        //     [T22]
        // SigmaN = [varT11    covT11T12  covT11T21  covT11T22]
        //          [covT12T11 varT12     covT12T21  covT12T22]
        //          [covT21T11 covT21T12  varT21     covT21T22]
        //          [covT22T11 covT22T12  covT22T21  varT22]
        
        covN11C2 = covN11N12 + covN11N22;
        covN12C1 = covN12N11 + covN12N21;
        covC1C2 = covN11N12 + covN11N22 + covN21N12 + covN21N22;
        //covT11T12 = covN11N12 - n1*covN11C2/(n - 1) - (n1 - 1)*covN12C1/(n - 1)
                    //+ (n1 - 1)*n1*covC1C2/((n - 1)*(n - 1));
        covT11T12 = covN11N12 - n2*covN11C2/(n - 1) - (n1 - 1)*covN12C1/(n - 1)
        + (n1 - 1)*n2*covC1C2/((n - 1)*(n - 1));
        covC1C1 = varN11 + covN11N21 + covN21N11 + varN21;
        //covT11T21 = covN11N21 - n2*covN11C1/(n - 1) - (n1 - 1)*covN21C1/(n - 1)
                    //+ (n1 - 1)*n2*covC1C1/((n - 1)*(n - 1));
        covT11T21 = covN11N21 - n1*covN11C1/(n - 1) - (n1 - 1)*covN21C1/(n - 1)
        + (n1 - 1)*n1*covC1C1/((n - 1)*(n - 1));
        covN22C1 = covN22N11 + covN22N21;
        covT11T22 = covN11N22 - (n2 - 1)*covN11C2/(n - 1) - (n1 - 1)*covN22C1/(n - 1)
                    + (n1 - 1)*(n2 - 1)*covC1C2/((n - 1)*(n - 1));
        covT12T11 = covT11T12;
        covC2C1 = covC1C2;
        covN21C2 = covN21N12 + covN21N22;
        //covT12T21 = covN12N21 - n2*covN12C1/(n - 1) - n1*covN21C2/(n - 1)
                    //+ n1*n2*covC2C1/((n - 1)*(n - 1));
        covT12T21 = covN12N21 - n1*covN12C1/(n - 1) - n1*covN21C2/(n - 1)
        + n1*n1*covC2C1/((n - 1)*(n - 1));
        covC2C2 = varN12 + covN12N22 + covN22N12 + varN22;
        covT12T22 = covN22N12 - n1*covN22C2/(n - 1) - (n2 - 1)*covN12C2/(n - 1)
                    + (n2 - 1)*n1*covC2C2/((n - 1)*(n - 1));
        covT21T11 = covT11T21;
        covT21T12 = covT12T21;
        covT21T22 = covN22N21 - n2*covN22C1/(n - 1) - (n2 - 1)*covN21C2/(n - 1)
                    + (n2 - 1)*n2*covC2C1/((n - 1)*(n - 1));
        covT22T11 = covT11T22;
        covT22T12 = covT12T22;
        covT22T21 = covT21T22;
        sigma = new double[4][4];
        sigma[0][0] =  varT11;
        sigma[0][1] = covT11T12;
        sigma[0][2] = covT11T21;
        sigma[0][3] = covT11T22;
        sigma[1][0] = covT12T11;
        sigma[1][1] =  varT12;
        sigma[1][2] = covT12T21;
        sigma[1][3] = covT12T22;
        sigma[2][0] = covT21T11;
        sigma[2][1] = covT21T12;
        sigma[2][2] = varT21;
        sigma[2][3] = covT21T22;
        sigma[3][0] = covT22T11;
        sigma[3][1] = covT22T12;
        sigma[3][2] = covT22T21;
        sigma[3][3] = varT22;
        sigmaN = new Matrix(sigma);
        Tp = new double[1][4];
        Tp[0][0] = T11;
        Tp[0][1] = T12;
        Tp[0][2] = T21;
        Tp[0][3] = T22;
        TpM = new Matrix(Tp);
        T = new double[4][1];
        T[0][0] = T11;
        T[1][0] = T12;
        T[2][0] = T21;
        T[3][0] = T22;
        TM = new Matrix(T);
        CN = ((TpM.times(sigmaN.inverse())).times(TM)).getArray();
        Preferences.debug("CN = " + CN[0][0] + "\n");
        degreesOfFreedom = 4;
        stat = new Statistics(Statistics.CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION,
                CN[0][0], degreesOfFreedom, chiSquaredPercentile);
        stat.run();
        Preferences.debug("chiSquared percentile for Ceyhan's overall test of segregation = " + chiSquaredPercentile[0]*100.0 + "\n");
        System.out.println("chiSquared percentile for Ceyhan's overall test of segregation = " + chiSquaredPercentile[0]*100.0);
        
        if (chiSquaredPercentile[0] > 0.950) {
            Preferences.debug("chiSquared test rejects random object distribution\n");
            System.out.println("chiSquared test rejects random object distribution"); 
        }
        else {
            Preferences.debug("chiSquared test does not reject random object distribution\n");
            System.out.println("chiSquared test does not reject random object distribution");
        }
        
        Preferences.debug("Ceyhan's cell-specific tests of segregation\n");
        System.out.println("Ceyhan's cell-specific tests of segregation");
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z11N, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean T11 around expected mean T11 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean T11 around expected mean T11 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of T11 indicates association\n");
            System.out.println("Low value of T11 indicates association");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of T11 indicates segregation\n");
            System.out.println("High value of T11 indicates segregation");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on T11 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on T11 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z12N, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean T12 around expected mean T12 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean T12 around expected mean T12 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of T12 indicates segregation\n");
            System.out.println("Low value of T12 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of T12 indicates association\n");
            System.out.println("High value of T12 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on T12 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on T12 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z21N, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean T21 around expected mean T21 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean T21 around expected mean T21 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of T21 indicates segregation\n");
            System.out.println("Low value of T21 indicates segregation");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of T21 indicates association\n");
            System.out.println("High value of T21 indicates association");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on T21 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on T21 value");
        }
        
        stat = new Statistics(Statistics.GAUSSIAN_PROBABILITY_INTEGRAL, z22N, 0, percentile);
        stat.run();
        Preferences.debug("Percentile in Gaussian probability integral for measured mean T22 around expected mean T22 = "
                + percentile[0]*100.0 + "\n");
        System.out.println("Percentile in Gaussian probability integral for measured mean T22 around expected mean T22 = " +
                  percentile[0]*100.0);
        if (percentile[0] < 0.025) {
            Preferences.debug("Low value of T22 indicates association\n");
            System.out.println("Low value of T22 indicates association");
        }
        else if (percentile[0] > 0.975) {
            Preferences.debug("High value of T22 indicates segregation\n");
            System.out.println("High value of T22 indicates segregation");
        }
        else {
            Preferences.debug("Complete spatial randomness cannot be rejected based on T22 value\n");
            System.out.println("Complete spatial randomness cannot be rejected based on T22 value");
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
