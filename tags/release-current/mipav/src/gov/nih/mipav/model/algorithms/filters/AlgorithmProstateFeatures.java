package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;


/**
 * DOCUMENT ME!
 *
 * @version  0.5 September 10, 2009
 * @author   William Gandler This code is based on the material in the GLCM Texture Tutorial by Myrka Hall-Beyer at
 *           http://www.fp.ucalgary.ca/mhallbey/tutorial.htm.
 *           <p> This code can be used for 2D or 2.5D processing.  If optional concatenation is used, then only 1 result
 *           image is formed by concatenating the calculated features after the original source.  For 3D images the features
 *           are selected with the t dimension slider.  Note that optional concatenation cannot be used with color
 *           images.</p>
 *
 *           <p>The Haralick features are based on the frequency of occurrence of the pixel intensity values of 2 pixels
 *           an offset distance apart. The offset distance will almost always be 1. The frequency of occurrence is
 *           counted over a square window which is placed over each pixel interior to a band of width (window size -
 *           1)/2 at the edge of the image, since results are only calculated for pixels over which the entire square
 *           window can be placed. The values in this outer band have been left at zero.</p>
 *
 *           <p>The user selects 1 or more of the 5 direction possibilities and 1 or more of the 14 operator
 *           possibilities. The number of floating point Haralick texture images created equals the number of directions
 *           selected times the number of operators selected. The result image dimensions are the same as the source
 *           window dimensions.</p>
 *
 *           <p>The five direction possibilities are north-south, northeast-southwest, east-west, southeast-northwest,
 *           and spatially invariant. Spatially invariant is created by counting in the four directions and summing the
 *           counts.</p>
 *
 *           <p>The calculations start with the creation of a gray level co-occurrence matrix for each pixel position
 *           interior to the outer band for each direction. This matrix gives the probability for each pair of 2 pixels
 *           within the square window.</p>
 *           
 *           Data should be rescaled so that the Grey-Level Co-occurrence Matrix is of the appropriate size.  Eight bit data
 *           has 256 possible values, so the GLCM would be a 256 x 256 matrix, with 65,536 cells.  16 bit data would give a
 *           matrix of size 65536 x 65536 = 4,294,967,296 cells, so 16 bit data is too much to handle.</p>
 *           
 *           There is another reason for compressing the data.  If all 256 x 256 (or more) cells were used, there would be many
 *           cells filled with zeros (because that combination of grey levels simply does not occur).  The GLCM approximates
 *           the joint probability distribution of the two pixels.  Having many zeros in cells makes this a very bad
 *           approximation.  If the number of grey levels is reduced, the number of zeros is reduced, and the statistical
 *           validity is greatly improved.
 *           
 *          In "An analysis of co-occurrence texture statistics as a function of grey level quantization" David A. Clausi
 *          concludes: "... any value of G greater than 24 is advocated.  However, large values of G (greater than 64) are
 *          deemed unnecessary since they do not improve the classification accuracy and are computationally costly.
 *          Setting G to a value under twenty-four can produce unreliable results."
 *          
 *          Cluster shade is defined in "Urban and Non Urban Area Classification by Texture Characteristics and
 *          Data Fusion" by D. I. Morales, M. Moctezuma, and F. Parmiggiani.  Shade and promenance are defined in
 *          "Improving Co-occurrence Matrix Feature Discrimination" by Ross F. Walker, Paul Jackway, and
 *          I. D. Longstaff.
 */
public class AlgorithmProstateFeatures extends AlgorithmBase implements AlgorithmInterface {
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true calculate angular second moment Sum over i,j of Probability(i,j) * Probability(i,j). */
    private boolean asm;

    /**
     * If true calculate contrast, also known as sum of squares variance Sum over i,j of Probability(i,j) * (i-j) *
     * (i-j).
     */
    private boolean contrast;

    /**
     * If true calculate glcm correlation Sum over i,j of Probability(i,j) * (i - glcm mean) * (j - glcm mean)/ glcm
     * variance.
     */
    private boolean correlation;

    /** DOCUMENT ME! */
    private ModelImage[] destImage = null;
    
    private ModelImage[] classificationImage = null;

    /** If true calculate dissimilarity Sum over i,j of Probability(i,j) * Absolute value(i-j). */
    private boolean dissimilarity;

    /** If true calculate energy Energy = square root(asm). */
    private boolean energy;

    /** If true calcuate entropy Sum over i,j of -Probability(i,j) * ln(Probability(i,j)). */
    private boolean entropy;

    /** If true east-west direction calculated. */
    private boolean ew;

    /**
     * If true calculate homogeneity, also called the Inverse Difference Moment of Order 2 Sum over i,j of
     * Probability(i,j)/(1 + (i-j)*(i-j)).
     */
    private boolean homogeneity;

    /**
     * If true spatially invariant direction calculated. This is created by counting in the 4 directions and summing the
     * counts.
     */
    private boolean invariantDir;

    /** If true calculate inverse difference moment of order 1 Sum over i,j of Probability(i,j)/(1 + abs(i-j)). */
    private boolean inverseOrder1;

    /** If true calculate maximum probability maximum probability is the largest probability value within the window. */
    private boolean maxProbability;

    /**
     * If true calculate gray level co-occurrence matrix mean Sum over i,j of i * Probability(i,j) = Sum over i,j of j *
     * Probability(i,j).
     */
    private boolean mean;

    /** If true northeast-southwest direction calculated. */
    private boolean nesw;

    /** If true north-south direction calculated. */
    private boolean ns;

    /** Distance between the 2 pixels in the pair - almost always 1. */
    private int offsetDistance = 1;

    /** If true southeast-northwest direction calculated. */
    private boolean senw;

    /** If true calculate glcm standard deviation glcm standard deviation = square root(glcm variance). */
    private boolean standardDeviation;

    /**
     * If true calculate glcm variance Sum over i,j of Probability(i,j) * (i - glcm mean) * (i - glcm mean) = Sum over
     * i,j of Probability(i,j) * (j - glcm mean) * (j - glcm mean).
     */
    private boolean variance;
    
    /**
     * If true calculate the cluster shade Sum over i,j of ((i + j - x mean - y mean)**3) * Probability(i,j)
     * For the symmetrical glcm x mean = y mean = glcm mean
     * Sum over i,j over ((i + j - 2 * glcm mean)**3) * Probability(i,j)
     * It is a measure of the degree to which the outliers in the histogram favor one side or another
     * of the statistical mean.
     */
    private boolean shade;
    
    /**
     * If true calculate the cluster promenance Sum over i,j of ((i + j - x mean - y mean)**4) * Probability(i,j)
     * For the symmetrical glcm x mean = y mean = glcm mean
     * Sum over i,j of ((i + j - 2 * glcm mean)**4) * Probability(i,j)
     */
    private boolean promenance;
    
    /** If true, form only 1 result image by concatenating the original source to each of the 
     * calculated features.
     */
    private boolean concatenate;

    /** Size of square window used in calculating each glcm matrix - must be an odd number. */
    private int windowSize = 7;
    
    /** Number of grey levels used if rescaling performed. */
    private int greyLevels = 32;
    
    private int RGBOffset = RED_OFFSET;
    
    private int numberFiltersAdditional = 0;
    
    private boolean gaborFilter;
    
    /** DOCUMENT ME! */
    private float freqU;

    /** DOCUMENT ME! */
    private float freqV;
    /** DOCUMENT ME! */
    private float sigmaU;

    /** DOCUMENT ME! */
    private float sigmaV;
    
    /** DOCUMENT ME! */
    private float theta;
    
    private AlgorithmFrequencyFilter FrequencyFilterAlgo = null;
    
	/** DOCUMENT ME! */
	private ModelImage gaborImage;
	
	private ModelImage gmImage;
	
	private ModelImage image; // source image
	
	private VOIVector VOIs;
	
	private int haralickImagesNumber = 0;

	private boolean distanceFilter;
	
	private ModelImage distanceImage;
	
	private boolean imageOriginFilter;
	
	 private AlgorithmDistanceFilter distanceFilterAlgo = null;
	 
	private AlgorithmGradientMagnitude gradientMagAlgo;
	
	
   // fuzzy c mean
	/** possible values for segmentation. */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    /** DOCUMENT ME! */
    public static final float MAX_FLOAT = 1e30f;

    /** DOCUMENT ME! */
    public static final float ALPHA = 0.3f;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] centroids;

    @SuppressWarnings("unused")
    private int[] classNumber;

    /** DOCUMENT ME! */
    private boolean cropBackground = false; // When cropBackground is true,

    /** 1/(qValue - 1). */
    private double exponent;

    /** jacobiIters1 and jacobiIter2 are only used with gain correction. */
    private int jacobiIters1 = 1;

    /** DOCUMENT ME! */
    private int jacobiIters2 = 2;

    /** DOCUMENT ME! */
    private int maxIter = 100;

    /** DOCUMENT ME! */
    private int nClass = 3;

    @SuppressWarnings("unused")
    private float[] oldMember;

    /** DOCUMENT ME! */
    private boolean outputGainField = false;

    @SuppressWarnings("unused")
    private boolean powEIntFlag = true;

    /** pyramidLevels is only used with gain correction. */
    private int pyramidLevels = 4;

    /** DOCUMENT ME! */
    private float qValue = 2.0f;

    @SuppressWarnings("unused")
    private float[] sCentroids;

    /** DOCUMENT ME! */
    private int segmentation = FUZZY_ONLY;

    /** smooth1 and smooth2 are only used with gain field correction. */
    private float smooth1 = 20000;

    /** DOCUMENT ME! */
    private float smooth2 = 200000;

    /** DOCUMENT ME! */
    private float threshold = 1.0f;

    /** DOCUMENT ME! */
    private float tolerance = 0.01f;

    @SuppressWarnings("unused")
    private boolean[] unusedCentroids;
    
    private boolean wholeImage;

    private boolean fuzzyCMeanFilter;
    
    private ModelImage[] fuzzyCMeanImages;
    
    private AlgorithmFuzzyCMeans fcmAlgo;
	 
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmHaralickTexture object for black and white image.
     *
     * @param              destImg            image model where result image is to stored
     * @param              classificationImg  for each pixel, store which class this pixel belongs to
     * @param              srcImg             source image model
     * @param              windowSize         the size of the square window
     * @param              offsetDistance     distance between 2 pixels of pair
     * @param              greyLevels         Number of grey levels used if rescaling performed
     * @param              ns                 true if north south offset direction calculated
     * @param              nesw               true if northeast-southwest offset direction calculated
     * @param              ew                 true if east west offset direction calculated
     * @param              senw               true if southeast-northwest offset direction calculated
     * @param              invariantDir       true if spatially invariant calculated
     * @param              contrast           true if contrast calculated
     * @param              dissimilarity      true if dissimilarity calculated
     * @param              homogeneity        true if homogeneity calculated
     * @param              inverseOrder1      true if inverse difference moment of order 1 calculated
     * @param              asm                true if angular second moment calculated
     * @param              energy             true if energy calculated
     * @param              maxProbability     true if maximum probability calculated
     * @param              entropy            true if entropy calculated
     * @param              mean               true if gray level coordinate matrix mean calculated
     * @param              variance           true if gray level coordinate matrix variance calculated
     * @param              standardDeviation  true if gray level coordinate matrix standard deviation calculated
     * @param              correlation        true if gray level coordinate matrix correlation calculated
     * @param              shade              true if cluster shade calculated
     * @param              promenance         true if cluster promenance calculated
     * @param              concatenate        If true, only 1 result image formed by concatenating the original 
     *                                        source to each of the calculated features
     */
    public AlgorithmProstateFeatures(ModelImage[] destImg, ModelImage[] classImage,  ModelImage srcImg, int windowSize, int offsetDistance, int greyLevels,
                                    boolean ns, boolean nesw, boolean ew, boolean senw, boolean invariantDir,
                                    boolean contrast, boolean dissimilarity, boolean homogeneity, boolean inverseOrder1,
                                    boolean asm, boolean energy, boolean maxProbability, boolean entropy, boolean mean,
                                    boolean variance, boolean standardDeviation, boolean correlation,
                                    boolean shade, boolean promenance, boolean concatenate, 
                                    boolean gaborFilter, float freqU, float freqV, float sigmaU, float sigmaV, float theta,
                                    boolean distanceFilter, int numberFiltersAdditional, boolean imageOriginFilter,
                                    boolean fuzzyCMeanFilter,
                                    int _nClass, int _pyramidLevels,
                                    int _jacobiIters1, int _jacobiIters2, float _q, float _smooth1, float _smooth2,
                                    boolean _outputGainField, int _segmentation, boolean _cropBackground, float _threshold,
                                    int _max_iter, float _tolerance, boolean _wholeImage,
                                    float[] centroids) {
        super(null, srcImg);
        image = srcImage;
        
        destImage = destImg;
        classificationImage = classImage;
        this.windowSize = windowSize;
        this.offsetDistance = offsetDistance;
        this.greyLevels = greyLevels;
        this.ns = ns;
        this.nesw = nesw;
        this.ew = ew;
        this.senw = senw;
        this.invariantDir = invariantDir;
        this.contrast = contrast;
        this.dissimilarity = dissimilarity;
        this.homogeneity = homogeneity;
        this.inverseOrder1 = inverseOrder1;
        this.asm = asm;
        this.energy = energy;
        this.maxProbability = maxProbability;
        this.entropy = entropy;
        this.mean = mean;
        this.variance = variance;
        this.standardDeviation = standardDeviation;
        this.correlation = correlation;
        this.shade = shade;
        this.promenance = promenance;
        this.concatenate = concatenate;
        this.gaborFilter = gaborFilter;
        this.freqU = freqU;
        this.freqV = freqV;
        this.sigmaU = sigmaU;
        this.sigmaV = sigmaV;
        this.theta = theta;
        this.distanceFilter = distanceFilter;
        this.imageOriginFilter = imageOriginFilter;
        if ( srcImage.getVOIs() != null ) {
        	this.VOIs = srcImage.getVOIs();
        }
        this.numberFiltersAdditional = numberFiltersAdditional;
        
        // fuzzy c mean
        this.fuzzyCMeanFilter = fuzzyCMeanFilter;
        
        if (fuzzyCMeanFilter) {
			this.nClass = _nClass;
			this.pyramidLevels = _pyramidLevels;
			this.jacobiIters1 = _jacobiIters1;
			this.jacobiIters2 = _jacobiIters2;
			this.qValue = _q;
			this.exponent = 1.0f / (qValue - 1.0f);
			this.smooth1 = _smooth1;
			this.smooth2 = _smooth2;
			this.outputGainField = _outputGainField;
			this.segmentation = _segmentation;
			this.cropBackground = _cropBackground;
			this.threshold = _threshold;
			this.maxIter = _max_iter;
			this.tolerance = _tolerance;
			this.wholeImage = _wholeImage;
			this.oldMember = new float[nClass];
			this.sCentroids = new float[nClass];
			this.unusedCentroids = new boolean[nClass];
			this.classNumber = new int[nClass];

			if ((exponent - Math.floor(exponent)) < 0.01) {
				this.powEIntFlag = true;
			} else {
				this.powEIntFlag = false;
			}

			this.centroids = new float[centroids.length];

			for (int i = 0; i < centroids.length; i++) {
				this.centroids[i] = centroids[i];
			}
		}  
        
    }
    
    /**
	 * Creates a new AlgorithmHaralickTexture object for color image.
	 * 
	 * @param destImg
	 *            image model where result image is to stored
	 * @param classificationImg
	 *            for each pixel store which class this pixel belongs to
	 * @param srcImg
	 *            source image model
	 * @param RGBOffset
	 *            selects red, green, or blue channel
	 * @param windowSize
	 *            the size of the square window
	 * @param offsetDistance
	 *            distance between 2 pixels of pair
	 * @param greyLevels
	 *            Number of grey levels used if rescaling performed
	 * @param ns
	 *            true if north south offset direction calculated
	 * @param nesw
	 *            true if northeast-southwest offset direction calculated
	 * @param ew
	 *            true if east west offset direction calculated
	 * @param senw
	 *            true if southeast-northwest offset direction calculated
	 * @param invariantDir
	 *            true if spatially invariant calculated
	 * @param contrast
	 *            true if contrast calculated
	 * @param dissimilarity
	 *            true if dissimilarity calculated
	 * @param homogeneity
	 *            true if homogeneity calculated
	 * @param inverseOrder1
	 *            true if inverse difference moment of order 1 calculated
	 * @param asm
	 *            true if angular second moment calculated
	 * @param energy
	 *            true if energy calculated
	 * @param maxProbability
	 *            true if maximum probability calculated
	 * @param entropy
	 *            true if entropy calculated
	 * @param mean
	 *            true if gray level coordinate matrix mean calculated
	 * @param variance
	 *            true if gray level coordinate matrix variance calculated
	 * @param standardDeviation
	 *            true if gray level coordinate matrix standard deviation
	 *            calculated
	 * @param correlation
	 *            true if gray level coordinate matrix correlation calculated
	 * @param shade
	 *            true if cluster shade calculated
	 * @param promenance
	 *            true if cluster promenance calculated
	 * @param concatenate
	 *            If true, only 1 result image formed by concatenating the
	 *            original source to each of the calculated features
	 */
    public AlgorithmProstateFeatures(ModelImage[] destImg, ModelImage[] classImage,  ModelImage srcImg, int RGBOffset, int windowSize, int offsetDistance, int greyLevels,
                                    boolean ns, boolean nesw, boolean ew, boolean senw, boolean invariantDir,
                                    boolean contrast, boolean dissimilarity, boolean homogeneity, boolean inverseOrder1,
                                    boolean asm, boolean energy, boolean maxProbability, boolean entropy, boolean mean,
                                    boolean variance, boolean standardDeviation, boolean correlation,
                                    boolean shade, boolean promenance, boolean concatenate, 
                                    boolean gaborFilter, float freqU, float freqV, float sigmaU, float sigmaV, float theta,
                                    boolean distanceFilter,
                                    int numberFiltersAdditional,
                                    boolean imageOriginFilter,
                                    boolean fuzzyCMeanFilter,
                                    int _nClass, int _pyramidLevels,
                                    int _jacobiIters1, int _jacobiIters2, float _q, float _smooth1, float _smooth2,
                                    boolean _outputGainField, int _segmentation, boolean _cropBackground, float _threshold,
                                    int _max_iter, float _tolerance, boolean _wholeImage,
                                    float[] centroids) {
        super(null, srcImg);
        image = srcImg;
        destImage = destImg;
        classificationImage = classImage;
        this.RGBOffset = RGBOffset;
        this.windowSize = windowSize;
        this.offsetDistance = offsetDistance;
        this.greyLevels = greyLevels;
        this.ns = ns;
        this.nesw = nesw;
        this.ew = ew;
        this.senw = senw;
        this.invariantDir = invariantDir;
        this.contrast = contrast;
        this.dissimilarity = dissimilarity;
        this.homogeneity = homogeneity;
        this.inverseOrder1 = inverseOrder1;
        this.asm = asm;
        this.energy = energy;
        this.maxProbability = maxProbability;
        this.entropy = entropy;
        this.mean = mean;
        this.variance = variance;
        this.standardDeviation = standardDeviation;
        this.correlation = correlation;
        this.shade = shade;
        this.promenance = promenance;
        this.concatenate = concatenate;
        this.gaborFilter = gaborFilter;
        this.freqU = freqU;
        this.freqV = freqV;
        this.sigmaU = sigmaU;
        this.sigmaV = sigmaV;
        this.theta = theta;
        this.distanceFilter = distanceFilter;
        this.imageOriginFilter = imageOriginFilter;
        if ( srcImage.getVOIs() != null ) {
        	this.VOIs = srcImage.getVOIs();
        }
        this.numberFiltersAdditional = numberFiltersAdditional;
        
        // fuzzy c mean
        this.fuzzyCMeanFilter = fuzzyCMeanFilter;
        
        if (fuzzyCMeanFilter) {
			this.nClass = _nClass;
			this.pyramidLevels = _pyramidLevels;
			this.jacobiIters1 = _jacobiIters1;
			this.jacobiIters2 = _jacobiIters2;
			this.qValue = _q;
			this.exponent = 1.0f / (qValue - 1.0f);
			this.smooth1 = _smooth1;
			this.smooth2 = _smooth2;
			this.outputGainField = _outputGainField;
			this.segmentation = _segmentation;
			this.cropBackground = _cropBackground;
			this.threshold = _threshold;
			this.maxIter = _max_iter;
			this.tolerance = _tolerance;
			this.wholeImage = _wholeImage;
			this.oldMember = new float[nClass];
			this.sCentroids = new float[nClass];
			this.unusedCentroids = new boolean[nClass];
			this.classNumber = new int[nClass];

			if ((exponent - Math.floor(exponent)) < 0.01) {
				this.powEIntFlag = true;
			} else {
				this.powEIntFlag = false;
			}

			this.centroids = new float[centroids.length];

			for (int i = 0; i < centroids.length; i++) {
				this.centroids[i] = centroids[i];
			}
		}
    }

    // ~ Methods
	// --------------------------------------------------------------------------------------------------------

    /**
	 * Prepares this class for destruction.
	 */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        

        fireProgressStateChanged(0, null, "Running Haralick textures ...");
        if ( gaborFilter ) {
        	System.err.println("gabor = true;");
        	calculateGaborFilter();
        	// calculateGM();
        }
        
        if ( distanceFilter ) {
        	calculateDistance();
        }
        
        if ( fuzzyCMeanFilter ) {
        	calculateFuzzyCMean();
        }
        
        calculateHaralick();
    }

    private void calculateFuzzyCMean() {
    	int resultNumber = 0;
    	int i;
    	int[] destExtents = null;
        int presentNumber = 0;


        if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {
            resultNumber++; // segmented image
        } // if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD))

        if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {

            // if (outputGainField) {
            // resultNumber++;
            // }
            resultNumber += nClass;
        } // if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD))

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
        }
        
    	   try {
    		   fuzzyCMeanImages = new ModelImage[resultNumber];
               presentNumber = 0;

               if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {

                   for (i = 0; i < nClass; i++) {
                       fuzzyCMeanImages[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
                                                                     image.getImageName() + 
                                                                                   "_class" + (i + 1));
                   }
                   /* if (outputGainField) {
                    *  resultImage[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
                    * image.getImageName() + "mult", userInterface);              } */
               }

               if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {
            	   fuzzyCMeanImages[presentNumber++] = new ModelImage(ModelStorageBase.UBYTE, destExtents,
                                                                 image.getImageName() + "_seg");
               }

               // Make algorithm
               fcmAlgo = new AlgorithmFuzzyCMeans(fuzzyCMeanImages, image, nClass, pyramidLevels,
                       jacobiIters1, jacobiIters2, qValue, smooth1, smooth2,
                       outputGainField, segmentation, cropBackground, threshold,
                       maxIter, tolerance, wholeImage);

               // This is very important. Adding this object as a listener allows the algorithm to
               // notify this object when it has completed of failed. See algorithm performed event.
               // This is made possible by implementing AlgorithmedPerformed interface
               fcmAlgo.addListener(this);


               if (wholeImage == false) {
                   fcmAlgo.setMask(image.generateVOIMask());
                   // if non null, were set by script file
               }
             
               fcmAlgo.setCentroids(centroids);
            
               fcmAlgo.run();
               
           } catch (OutOfMemoryError x) {

               if (fuzzyCMeanImages != null) {

                   for (i = 0; i < resultNumber; i++) {

                       if (fuzzyCMeanImages[i] != null) {
                    	   fuzzyCMeanImages[i].disposeLocal(); // Clean up memory of result image
                    	   fuzzyCMeanImages[i] = null;
                       }
                   }

                   fuzzyCMeanImages = null;
               }

               System.gc();
               MipavUtil.displayError("Dialog FuzzyCMeans: unable to allocate enough memory");

               return;
           }

    	
    }
    
    
    private void calculateGaborFilter() {

    	 try {
    		 gaborImage = (ModelImage) image.clone();
             // resultImage.setImageName(name);
    		 gaborImage.setType(ModelStorageBase.FLOAT);
    		 gaborImage.reallocate(ModelStorageBase.FLOAT);
    		 // gaborImage.resetVOIs();
    		 
             // No need to make new image space because the user has choosen to replace the source image
             // Make the algorithm class
             FrequencyFilterAlgo = new AlgorithmFrequencyFilter(gaborImage, image, freqU, freqV, sigmaU, sigmaV, theta,
                                                                false);

             // This is very important. Adding this object as a listener allows the algorithm to
             // notify this object when it has completed or failed. See algorithm performed event.
             // This is made possible by implementing AlgorithmedPerformed interface
             FrequencyFilterAlgo.addListener(this);

             FrequencyFilterAlgo.run();
             
             
         } catch (OutOfMemoryError x) {
             MipavUtil.displayError("GaborFilter: unable to allocate enough memory");

             return;
         }
    }
    
    @SuppressWarnings("unused")
    private void calculateGM() {

		float sigmas[] = new float[2];
		sigmas[0] = 1.0f;
		sigmas[1] = 1.0f;

		gmImage = (ModelImage) gaborImage.clone();
		// resultImage.setImageName(name);
		gmImage.setType(ModelStorageBase.FLOAT);
		gmImage.reallocate(ModelStorageBase.FLOAT);

		try {
			gradientMagAlgo = new AlgorithmGradientMagnitude(gmImage, gaborImage, sigmas, true, false);
			gradientMagAlgo.addListener(this);
			gradientMagAlgo.run();

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("GM Filter: unable to allocate enough memory");

			return;
		}
	}
    
    private void calculateDistance() {
    	 try {
    		 distanceImage = (ModelImage) image.clone();
    		 distanceImage.setType(ModelStorageBase.FLOAT);
    		 distanceImage.reallocate(ModelStorageBase.FLOAT);
             // resultImage.setImageName(name);
    		 // distanceImage.resetVOIs();
    		 
             // No need to make new image space because the user has choosen to replace the source image
             // Make the algorithm class
             distanceFilterAlgo = new AlgorithmDistanceFilter(distanceImage, image, distanceFilter);

             // This is very important. Adding this object as a listener allows the algorithm to
             // notify this object when it has completed or failed. See algorithm performed event.
             // This is made possible by implementing AlgorithmedPerformed interface
             distanceFilterAlgo.addListener(this);

             distanceFilterAlgo.run();
             
         } catch (OutOfMemoryError x) {
             MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");

             return;
         }
    }
    
    
    /**
     * DOCUMENT ME!
     */
    private void calculateHaralick() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim;
        boolean doneNS = false;
        boolean doneNESW = false;
        boolean doneEW = false;
        boolean doneSENW = false;
        boolean doneInvariant = false;
        int iDir;
        double imageMin;
        double imageMax;
        srcImage.calcMinMax();
        imageMin = srcImage.getMin();
        imageMax = srcImage.getMax();

        int numDirections = 0;
        int numOperators = 0;
        double[] sourceBuffer = new double[sliceSize];
        float[] gaborBuffer = new float[sliceSize];
        float[] distanceBuffer = new float[sliceSize];
        float[] fuzzyCMeanBuffer;
        byte[]  byteBuffer = new byte[sliceSize];
        float[] floatBuffer;
        int[][] nsBuffer = null;
        int[][] neswBuffer = null;
        int[][] ewBuffer = null;
        int[][] senwBuffer = null;
        int[][] invariantDirBuffer = null;

        // gray level co-occurrence matrix
        float[][] glcm = null;
        int matrixSize;
        int halfWin = (windowSize - 1) / 2;
        int xStart = halfWin;
        int xEnd = xDim - 1 - halfWin;
        int yStart = halfWin;
        int yEnd = yDim - 1 - halfWin;
        int x, y;
        int i, j;
        int index;
        int totalFilters;
        float matrixSum;
        float[][] resultBuffer = null;
        float[][] resultBufferClass = null;
        int currentResult = 0;
        int pos;
        float glcmMean = 0.0f;
        float glcmVariance = 0.0f;
        float glcmASM = 0.0f;
        boolean skip;
        boolean rescale = false;
        double range = imageMax - imageMin;
        double factor = (greyLevels - 1)/range;
        float sum;
        float product;
        int z;
        
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }
        
        if ((srcImage.getType() == ModelStorageBase.FLOAT) || (srcImage.getType() == ModelStorageBase.DOUBLE) ||
            (srcImage.getType() == ModelStorageBase.ARGB_FLOAT)) {
            rescale = true;
        } else if (range >= 64) {
            rescale = true;
        }
        
        if (ns) {
            numDirections++;
        }

        if (nesw) {
            numDirections++;
        }

        if (ew) {
            numDirections++;
        }

        if (senw) {
            numDirections++;
        }

        if (invariantDir) {
            numDirections++;
        }

        if (contrast) {
            numOperators++;
        }

        if (dissimilarity) {
            numOperators++;
        }

        if (homogeneity) {
            numOperators++;
        }

        if (inverseOrder1) {
            numOperators++;
        }

        if (asm) {
            numOperators++;
        }

        if (energy) {
            numOperators++;
        }

        if (maxProbability) {
            numOperators++;
        }

        if (entropy) {
            numOperators++;
        }

        if (mean) {
            numOperators++;
        }

        if (variance) {
            numOperators++;
        }

        if (standardDeviation) {
            numOperators++;
        }

        if (correlation) {
            numOperators++;
        }
        
        if (shade) {
            numOperators++;
        }
        
        if (promenance) {
            numOperators++;
        }

        totalFilters = numDirections * numOperators + numberFiltersAdditional;
        System.err.println("totalFilters = " + totalFilters);
        if (rescale) {
            matrixSize = greyLevels;
        }
        else {
            matrixSize = (int)Math.round(range) + 1;
        }

        if (ns || invariantDir) {
            nsBuffer = new int[matrixSize][matrixSize];
        }

        if (nesw || invariantDir) {
            neswBuffer = new int[matrixSize][matrixSize];
        }

        if (ew || invariantDir) {
            ewBuffer = new int[matrixSize][matrixSize];
        }

        if (senw || invariantDir) {
            senwBuffer = new int[matrixSize][matrixSize];
        }

        if (invariantDir) {
            invariantDirBuffer = new int[matrixSize][matrixSize];
        }

        glcm = new float[matrixSize][matrixSize];
        resultBuffer = new float[totalFilters][sliceSize];
        resultBufferClass = new float[totalFilters][sliceSize];


        for (z = 0; z < zDim; z++) {
			try {
				if (srcImage.isColorImage()) {
					floatBuffer = new float[sliceSize];
					srcImage.exportRGBData(RGBOffset, 4 * z * sliceSize,
							sliceSize, floatBuffer);
					for (i = 0; i < sliceSize; i++) {
						sourceBuffer[i] = (double) floatBuffer[i];
					}
					floatBuffer = null;
				} else {
					srcImage.exportData(z * sliceSize, sliceSize, sourceBuffer);
					if ( gaborFilter ) {
						gaborImage.exportData(z * sliceSize, sliceSize, gaborBuffer);
						// gmImage.exportData(z * sliceSize, sliceSize, gaborBuffer);
					}
					if ( distanceFilter ) {
						distanceImage.exportData(z * sliceSize, sliceSize, distanceBuffer);
					}
				}
			} catch (IOException error) {
				MipavUtil
						.displayError("AlgorithmHaralickTexture: IOException on srcImage.exportData(0,sliceSize,sourceBuffer)");
				setCompleted(false);

				return;
			}

			for (i = 0; i < sliceSize; i++) {
				sourceBuffer[i] -= imageMin;
			}

			if (rescale) {
				for (i = 0; i < sliceSize; i++) {
					byteBuffer[i] = (byte) ((sourceBuffer[i] * factor) + 0.5f);
				}
			} else {
				for (i = 0; i < sliceSize; i++) {
					byteBuffer[i] = (byte) Math.round(sourceBuffer[i]);
				}
			}

			if ((!concatenate) && (z == zDim - 1)) {
				sourceBuffer = null;
			}

			for (i = 0; i < totalFilters; i++) {
				for (j = 0; j < sliceSize; j++) {
					resultBuffer[i][j] = 0;
					resultBufferClass[i][j] = -1;
				}
			}

			for (y = yStart; (y <= yEnd) && !threadStopped; y++) {

				fireProgressStateChanged(
						((int) ((z * 100.0f / zDim) + ((y - yStart) * (100.0f / (zDim * (yEnd - yStart)))))),
						null, null);

				for (x = xStart; x <= xEnd; x++) {
					pos = x + (y * xDim);
					doneNS = false;
					doneNESW = false;
					doneEW = false;
					doneSENW = false;
					doneInvariant = false;
					currentResult = 0;

					for (iDir = 0; iDir < numDirections; iDir++) {
						skip = false;

						if ((ns || invariantDir) && (!doneNS)) {
							doneNS = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									nsBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

								for (i = x - halfWin; i <= (x + halfWin); i++) {
									index = i + (j * xDim);
									nsBuffer[byteBuffer[index]][byteBuffer[index
											+ xDim]]++;
									nsBuffer[byteBuffer[index + xDim]][byteBuffer[index]]++;
								}
							}

							if (ns) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* windowSize;

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = nsBuffer[i][j] / matrixSum;
									}
								}

								skip = true;
							} // if (ns)
						}

						if ((nesw || invariantDir) && (!doneNESW) && (!skip)) {
							doneNESW = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									neswBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

								for (i = x - halfWin + offsetDistance; i <= (x + halfWin); i++) {
									index = i + (j * xDim);
									neswBuffer[byteBuffer[index]][byteBuffer[index
											+ xDim - 1]]++;
									neswBuffer[byteBuffer[index + xDim - 1]][byteBuffer[index]]++;
								}
							}

							if (nesw) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* (windowSize - offsetDistance);

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = neswBuffer[i][j]
												/ matrixSum;
									}
								}

								skip = true;
							} // if (nesw)
						}

						if ((ew || invariantDir) && (!doneEW) && (!skip)) {
							doneEW = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									ewBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin); j++) {

								for (i = x - halfWin; i <= (x + halfWin - offsetDistance); i++) {
									index = i + (j * xDim);
									ewBuffer[byteBuffer[index]][byteBuffer[index + 1]]++;
									ewBuffer[byteBuffer[index + 1]][byteBuffer[index]]++;
								}
							}

							if (ew) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* windowSize;

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = ewBuffer[i][j] / matrixSum;
									}
								}

								skip = true;
							} // if (ew)
						}

						if ((senw || invariantDir) && (!doneSENW) && (!skip)) {
							doneSENW = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									senwBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

								for (i = x - halfWin; i <= (x + halfWin - offsetDistance); i++) {
									index = i + (j * xDim);
									senwBuffer[byteBuffer[index]][byteBuffer[index
											+ xDim + 1]]++;
									senwBuffer[byteBuffer[index + xDim + 1]][byteBuffer[index]]++;
								}
							}

							if (senw) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* (windowSize - offsetDistance);

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = senwBuffer[i][j]
												/ matrixSum;
									}
								}

								skip = true;
							} // if (senw)
						}

						if (invariantDir && (!doneInvariant) && (!skip)) {
							doneInvariant = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									invariantDirBuffer[i][j] = nsBuffer[i][j]
											+ neswBuffer[i][j] + ewBuffer[i][j]
											+ senwBuffer[i][j];
								}
							}

							matrixSum = (4.0f * (windowSize - offsetDistance) * (windowSize - offsetDistance))
									+ (4.0f * (windowSize - offsetDistance) * windowSize);

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcm[i][j] = invariantDirBuffer[i][j]
											/ matrixSum;
								}
							}
						} // else if (invariantDir && (!doneInvariant)

						if (contrast) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									resultBuffer[currentResult][pos] += (i - j) * (i - j) * glcm[i][j];
								}
							}

							currentResult++;
						} // if (contrast)

						if (dissimilarity) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									resultBuffer[currentResult][pos] += Math
											.abs(i - j)
											* glcm[i][j];
								}
							}

							currentResult++;
						} // if (dissimilarity)

						if (homogeneity) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									resultBuffer[currentResult][pos] += glcm[i][j]
											/ (1.0f + ((i - j) * (i - j)));
								}
							}

							currentResult++;
						} // if (homogeneity)

						if (inverseOrder1) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									resultBuffer[currentResult][pos] += glcm[i][j]
											/ (1.0f + Math.abs(i - j));
								}
							}

							currentResult++;
						} // if (inverseOrder1)

						if (asm || energy) {
							glcmASM = 0.0f;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcmASM += glcm[i][j] * glcm[i][j];
								}
							}

							if (!energy) {
								resultBuffer[currentResult][pos] = glcmASM;
								currentResult++;
							} else if (!asm) {
								resultBuffer[currentResult][pos] = (float) Math
										.sqrt(glcmASM);
								currentResult++;
							} else {
								resultBuffer[currentResult][pos] = glcmASM;
								currentResult++;
								resultBuffer[currentResult][pos] = (float) Math
										.sqrt(glcmASM);
								currentResult++;
							}
						} // if (asm || energy)

						if (maxProbability) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {

									if (glcm[i][j] > resultBuffer[currentResult][pos]) {
										resultBuffer[currentResult][pos] = glcm[i][j];
									}
								}
							}

							currentResult++;
						} // if (maxProbability)

						if (entropy) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {

									if (glcm[i][j] > 0.0f) {
										resultBuffer[currentResult][pos] -= glcm[i][j]
												* Math.log(glcm[i][j]);
									}
								}
							}

							currentResult++;
						} // if (entropy)

						if (mean || variance || standardDeviation
								|| correlation || shade || promenance) {
							glcmMean = 0.0f;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcmMean += i * glcm[i][j];
								}
							}

							if (mean) {
								resultBuffer[currentResult][pos] = glcmMean;
								currentResult++;
							}
						} // if (mean || variance || standardDeviation || correlation)

						if (variance || standardDeviation || correlation) {
							glcmVariance = 0.0f;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcmVariance += glcm[i][j] * (i - glcmMean)
											* (i - glcmMean);
								}
							}

							if ((!standardDeviation) && variance) {
								resultBuffer[currentResult][pos] = glcmVariance;
								currentResult++;
							} else if ((!variance) && standardDeviation) {
								resultBuffer[currentResult][pos] = (float) Math
										.sqrt(glcmVariance);
								currentResult++;
							} else if (variance && standardDeviation) {
								resultBuffer[currentResult][pos] = glcmVariance;
								currentResult++;
								resultBuffer[currentResult][pos] = (float) Math
										.sqrt(glcmVariance);
								currentResult++;
							}
						} // if (variance || standardDeviation || correlation)

						if (correlation) {

							if (glcmVariance != 0) {
								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										resultBuffer[currentResult][pos] += glcm[i][j]
												* (i - glcmMean)
												* (j - glcmMean) / glcmVariance;
									}
								}
							} // if (glcmVariance != 0)
							else {
								// When an image area is completely uniform, the GLCM variance is zero, just as the first-order
								// image variance is zero.  As a result, the denominator of the correlation equation becomes 0,
								// and the correlation becomes undefined.  The undefined value is set to 1, as the correlation
								// among the original pixel values is perfect
								resultBuffer[currentResult][pos] = 1.0f;
							}

							currentResult++;
						} // if (correlation)

						if (shade) {
							for (i = 0; i < matrixSize; i++) {
								for (j = 0; j < matrixSize; j++) {
									sum = (i + j - 2 * glcmMean);
									resultBuffer[currentResult][pos] += glcm[i][j]
											* sum * sum * sum;
								}
							}
							currentResult++;
						} // if (shade)

						if (promenance) {
							for (i = 0; i < matrixSize; i++) {
								for (j = 0; j < matrixSize; j++) {
									sum = (i + j - 2 * glcmMean);
									product = sum * sum;
									resultBuffer[currentResult][pos] += glcm[i][j]
											* product * product;
								}
							}
							currentResult++;
						} // if (promenance)
					} // for (iDir = 0; iDir < numDirections; iDir++)
				} // for (x = xStart; x <= xEnd; x++)
			} // for (y = yStart; y <= yEnd && !threadStopped; y++)

			
			// add the classification to the haralick filters
			for (y = yStart; (y <= yEnd) && !threadStopped; y++) {

				for (x = xStart; x <= xEnd; x++) {
					pos = x + (y * xDim);

					if (VOIs.size() > 0) {
						// resultBufferClass[currentResult-1][pos] = -1;
						
						Vector<VOIBase>[] vArray = VOIs.VOIAt(0)
								.getSortedCurves(VOIBase.ZPLANE, zDim);

						if (vArray != null) {
							if (vArray[z] != null && vArray[z].size() > 0) {
								VOIBase v = vArray[z].get(0);
								if (v instanceof VOIContour) {
									if (((VOIContour) v).contains(x, y)) {
										for (int q = 0; q < currentResult; q++) {
											
											resultBufferClass[q][pos] = 1;
										}

									} else {
										for (int q = 0; q < currentResult; q++) {
											
											resultBufferClass[q][pos] = -1;
										}
									}
								}
							}

						}
					}

				}

			}

			
			
			if (gaborFilter) {

				// adding the additional filter operation.
				for (y = yStart; (y <= yEnd) && !threadStopped; y++) {

					for (x = xStart; x <= xEnd; x++) {
						pos = x + (y * xDim);

						if (gaborFilter) {

							resultBuffer[currentResult][pos] = gaborBuffer[pos];

							// System.err.println("resultBuffer[currentResult][pos]
							// = " + gaborBuffer[pos]);
						}

						if (VOIs.size() > 0) {
							if (gaborFilter) {
								resultBufferClass[currentResult][pos] = -1;
							}
							Vector<VOIBase>[] vArray = VOIs.VOIAt(0)
                            .getSortedCurves(VOIBase.ZPLANE, zDim);

							if (vArray != null) {
								if (vArray[z] != null && vArray[z].size() > 0) {
									VOIBase v = vArray[z].get(0);
									if (v instanceof VOIContour) {
										if (((VOIContour) v).contains(x, y)) {
											/*
											for (int q = 0; q < currentResult; q++) {
												// resultBuffer[q][pos] = 255;
												resultBufferClass[q][pos] = 1;
											}
                                            */
											if (gaborFilter) {
												resultBufferClass[currentResult][pos] = 1;
											}
											// resultBuffer[currentResult][pos]
											// = 255;
											// sourceBuffer[pos] = 3400;
										}
									}
								}

							}
						}

					}

				}

				currentResult++;
			}
			
			
			if (distanceFilter) {
				// adding the additional filter operation.
				for (y = yStart; (y <= yEnd) && !threadStopped; y++) {

					for (x = xStart; x <= xEnd; x++) {
						pos = x + (y * xDim);

						if (distanceFilter) {
							resultBuffer[currentResult][pos] = distanceBuffer[pos];

						}

						if (VOIs.size() > 0) {
							if (distanceFilter) {
								resultBufferClass[currentResult][pos] = -1;
							}
							Vector<VOIBase>[] vArray = VOIs.VOIAt(0)
                            .getSortedCurves(VOIBase.ZPLANE, zDim);

							if (vArray != null) {
								if (vArray[z] != null && vArray[z].size() > 0) {
									VOIBase v = vArray[z].get(0);
									if (v instanceof VOIContour) {
										if (((VOIContour) v).contains(x, y)) {
											/*
											for (int q = 0; q < currentResult; q++) {
												// resultBuffer[q][pos] = 255;
												resultBufferClass[q][pos] = 1;
											}
											*/

											if (distanceFilter) {
												resultBufferClass[currentResult][pos] = 1;
											}
											// resultBuffer[currentResult][pos]
											// = 255;
											// sourceBuffer[pos] = 3400;
										}
									}
								}

							}
						}

					}

				}

				currentResult++;
			}
			
			
			// ruida
			if (fuzzyCMeanFilter) {
				for (int fIndex = 0; fIndex < fuzzyCMeanImages.length; fIndex++) {
					try {
						fuzzyCMeanBuffer = new float[sliceSize];
						fuzzyCMeanImages[fIndex].exportData(z * sliceSize, sliceSize, fuzzyCMeanBuffer);		
					} catch (IOException error) {
						MipavUtil
								.displayError("AlgorithmProstateFeatures: IOException on fuzzyCMeanImages.exportData(0,sliceSize,sourceBuffer)");
						setCompleted(false);

						return;
					}
					
					
					for (y = yStart; (y <= yEnd) && !threadStopped; y++) {

						for (x = xStart; x <= xEnd; x++) {
							pos = x + (y * xDim);

							resultBuffer[currentResult][pos] = fuzzyCMeanBuffer[pos];

							if (VOIs.size() > 0) {
								if (fuzzyCMeanFilter) {
									resultBufferClass[currentResult][pos] = -1;
								}
								Vector<VOIBase>[] vArray = VOIs.VOIAt(0)
                                .getSortedCurves(VOIBase.ZPLANE, zDim);

								if (vArray != null) {
									if (vArray[z] != null
											&& vArray[z].size() > 0) {
										VOIBase v = vArray[z].get(0);
										if (v instanceof VOIContour) {
											if (((VOIContour) v).contains(x, y)) {
												/*
												for (int q = 0; q < currentResult; q++) {
													// resultBuffer[q][pos] =
													// 255;
													resultBufferClass[q][pos] = 1;
												}
												*/

												if (fuzzyCMeanFilter) {
													resultBufferClass[currentResult][pos] = 1;
												}
												// resultBuffer[currentResult][pos]
												// = 255;
												// sourceBuffer[pos] = 3400;
											}
										}
									}

								}
							}

						}

					}

					currentResult++;
				}
			}
			
			
			
						
			if (threadStopped) {
				finalize();
				return;
			}

			int imageOriginNumber = 0;
			if (concatenate) {
				if ( imageOriginFilter ) {
					try {
						destImage[0].importData(z * sliceSize, sourceBuffer, false);
						classificationImage[0].importData(z * sliceSize, sourceBuffer, false);
						imageOriginNumber = 1;
					} catch (IOException error) {
						MipavUtil
								.displayError(""
										+ "AlgorithmHaralickTexture: IOException on destImage[0].importData(0, sourceBuffer, true)");
						setCompleted(false);
	
						return;
					}
				}
				
				for (i = 0; i < totalFilters; i++) {

					try {
						
						 destImage[0].importData((i + imageOriginNumber) * zDim * sliceSize + z
								* sliceSize, resultBuffer[i], false);
							
						if ( VOIs.size() > 0  ) {	
								classificationImage[0].importData((i + imageOriginNumber) * zDim * sliceSize + z
								* sliceSize, resultBufferClass[i], false);
						}
					} catch (IOException error) {
						MipavUtil
								.displayError("image, AlgorithmProstateFeature: IOException on destImage["
										+ i
										+ "].importData(0,resultBuffer["
										+ i + "],false)");
						setCompleted(false);

						return;
					}
				} // for (i = 0; i < resultNumber; i++)
			} // if (concatenate)
			/*
			else { // !concatenate
				for (i = 0; i < totalFilters; i++) {

					try {
						destImage[i].importData(z * sliceSize, resultBuffer[i],
								false);
						if ( VOIs.size() > 0 ) {	
							destImage[i].importDataClass(z * sliceSize, resultBufferClass[i],
								false);
						}
					} catch (IOException error) {
						MipavUtil
								.displayError("AlgorithmHaralickTexture: IOException on destImage["
										+ i
										+ "].importData(0,resultBuffer["
										+ i + "],false)");
						setCompleted(false);

						return;
					}
				} // for (i = 0; i < resultNumber; i++)
			} // else !concatenate
			*/
		} // for (z = 0; z < zDim; z++)

        haralickImagesNumber = currentResult;
       
        
		if (concatenate) {
			destImage[0].calcMinMax();
			classificationImage[0].calcMinMax();
		} else {
			for (i = 0; i < totalFilters; i++) {
				destImage[i].calcMinMax();
				classificationImage[i].calcMinMax();
			}
		}

		setCompleted(true);

		return;
	}

    
    public void algorithmPerformed(AlgorithmBase algorithm) {

    	if (algorithm instanceof AlgorithmFrequencyFilter) {

            if ((algorithm.isCompleted() == true) && (gaborImage != null) && ( gaborFilter )) {
               
                try {
                	gaborImage = FrequencyFilterAlgo.getDestImage();
                 
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
         
            } else if (gaborImage != null) {

                // algorithm failed but result image still has garbage
            	gaborImage.disposeLocal(); // clean up memory
            	gaborImage = null;
            }
            FrequencyFilterAlgo.finalize();
            FrequencyFilterAlgo = null;
        }
    	
    	
    	if (gradientMagAlgo instanceof AlgorithmGradientMagnitude) {

            if ((algorithm.isCompleted() == true) && (gmImage != null) && ( gaborFilter )) {
               
                try {
                	gmImage = gradientMagAlgo.getDestImage();
                 
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (gmImage != null) {

                // algorithm failed but result image still has garbage
            	gmImage.disposeLocal(); // clean up memory
            	gmImage = null;
            }
            gradientMagAlgo.finalize();
            gradientMagAlgo = null;
        }
    	
    	if (algorithm instanceof AlgorithmDistanceFilter) {

            if ((algorithm.isCompleted() == true) && (distanceImage != null) && ( distanceFilter )) {
               
                try {
                	distanceImage = distanceFilterAlgo.getDestImage();
                 
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (distanceImage != null) {

                // algorithm failed but result image still has garbage
            	distanceImage.disposeLocal(); // clean up memory
            	distanceImage = null;
            }
            distanceFilterAlgo.finalize();
            distanceFilterAlgo = null;
        }

    	
    	if (algorithm instanceof AlgorithmFuzzyCMeans) {

            if ((fcmAlgo.isCompleted() == true) && (fuzzyCMeanImages != null) && ( fuzzyCMeanFilter )) {
               
            	 for (int i = 0; i < fuzzyCMeanImages.length; i++) {
                     //  updateFileInfo(image, fuzzyCMeanImages[i]);
            		 fuzzyCMeanImages[i].clearMask();
                     /*
                     try {
                         new ViewJFrameImage(fuzzyCMeanImages[i], null, new Dimension(610, 200 + (i * 20)));
                     } catch (OutOfMemoryError error) {
                         System.gc();
                         JOptionPane.showMessageDialog(null, "Out of memory: unable to open new frame", "Error",
                                                       JOptionPane.ERROR_MESSAGE);
                     }
                     */
                 }
            } else if (fuzzyCMeanImages != null) {

                // algorithm failed but result image still has garbage
            	// fuzzyCMeanImages.disposeLocal(); // clean up memory
            	fuzzyCMeanImages = null;
            }
            fcmAlgo.finalize();
            fcmAlgo = null;
        }
       
		
	}

    public int getHaralickImagesNumber() {
    	return haralickImagesNumber;
    }
    
}
