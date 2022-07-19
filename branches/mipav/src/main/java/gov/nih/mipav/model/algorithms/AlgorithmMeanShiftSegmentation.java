package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;
import java.io.IOException;

/**
 * The java code is ported from C++ code downloaded from http://coewww.rutgers.edu/riul/research/code.html.
 * The relevant web page section says:
 * Edge Detection and Image SegmentatiON (EDISON) System

C++ code, can be used through a graphical interface or command line.
The system is described in Synergism in low level vision.
For comments, please contact Bogdan Georgescu or Chris M. Christoudias.

The EDISON system contains the image segmentation/edge preserving filtering algorithm described in the
paper Mean shift: A robust approach toward feature space analysis and the edge detection algorithm
 described in the paper Edge detection with embedded confidence.
 
 Relevant points from reference 1:
 This code works on 2 dimensional gray level and color images.  RGB is converted to LUV for processing.
 Location and range vectors are concatenated
 in a joint spatial-range domain of dimension d = p + 2, where p = 1 in the gray level case, 3 for color
 images, and p > 3 in the multispectral case.  There is a spatial kernel bandwidth hs and a range kernel
 bandwidth hr.   For the 256 by 256 gray level cameraman image mean shift filtering a uniform kernel
 having hs = 8, hr = 4, M = 10 was used.  For the 512 by 512 color image baboon normal filters with hs going from 
 8 to 32 and hr going from 4 to 16 were used.  Only features with large spatial support are represented in
 the filtered image when hs increases.  On the other hand, only features with high color contrast survive
 when hr is large.	Mean shift filtering is run on the image before mean shift segmentation is performed.
 In mean shift segmentation clusters are formed by grouping together pixels which are
 closer than hs in the spatial domain and hr in the range domain.  Assign a label Li to each of the pixels ]
 depending on which cluster it belongs to.  Optionally eliminate spatial regions containing less than M pixels.
 The following were segmented with uniform kernels:
 256 by 256 gray level MIT building with hs = 8, hr = 7, M = 20 into 225 homogeneous regions.
 256 by 256 color room image with hs = 8, hr = 5, M = 20.
 512 by 512 color lake image with hs = 16, hr = 7, M = 40.
 512 by 512 color image hand with hs = 16, hr = 19, M = 40.
 All 256 by 256 images used the same hs = 8 corresponding to a 17 by 17 spatial window,
 while all 512 by 512 images used hs = 16 corresponding to a 31 by 31 window.
 The range parameter hr and the smallest significant feature size M control the number
 of regions in the segmented image.  The more an image deviates from the assumed piecewise
 constant model, larger values have to be used for hr and M to discard the effect of small
 local variations in feature space.
 4 color landscape images were segmented with hs = 8, hr = 7, M = 100.
 4 other color examples used hs = 8, hr = 7, M = 20.
 
 * This code was ported by William Gandler.
 * 
 * References

-------------------------------------------------------------------------------------------------

[1] D. Comanicu, P. Meer: "Mean shift: A robust approach toward feature space analysis".
    IEEE Trans. Pattern Anal. Machine Intell., May 2002.

[2] P. Meer, B. Georgescu: "Edge detection with embedded confidence". IEEE Trans. Pattern Anal.
    Machine Intell., 28, 2001.

[3] C. Christoudias, B. Georgescu, P. Meer: "Synergism in low level vision". 16th International
    Conference of Pattern Recognition, Track 1 - Computer Vision and Robotics, Quebec City,
    Canada, August 2001.
 */

public class AlgorithmMeanShiftSegmentation extends AlgorithmBase {
	// Input data parameters
	private int	L; // length (width * height)
	private int N; // input data dimension or range dimension
	private int kp; // subspace number
	int P[]; // subspace number, and subspace dimensions
	//data space conversion...
	private final double Yn			= 1.00000;
	//const double Un_prime	= 0.19780;
	//const double Vn_prime	= 0.46830;
	private final double Un_prime	= 0.19784977571475;
	private final double Vn_prime	= 0.46834507665248;
	private final double Lt			= 0.008856;
	
	//RGB to LUV conversion
    private final double XYZ[][] = new double[][]{	{  0.4125,  0.3576,  0.1804 },
							{  0.2125,  0.7154,  0.0721 },
							{  0.0193,  0.1192,  0.9502 }	};
							
	//LUV to RGB conversion
	private final double RGB[][] = new double[][]{	{  3.2405, -1.5371, -0.4985 },
														{ -0.9693,  1.8760,  0.0416 },
														{  0.0556, -0.2040,  1.0573 }	};

							
    // Gaussian Lookup Table
	private final int		GAUSS_NUM_ELS   = 16;		// take 16 samples of exp(-u/2)
	private final double	GAUSS_LIMIT     = 2.9;		// GAUSS_LIMIT     = c
	// GAUSS_INCREMENT = (c^2)/(# of samples)
	private final double	GAUSS_INCREMENT = GAUSS_LIMIT*GAUSS_LIMIT/GAUSS_NUM_ELS;
	
	 // Threshold
	private final double	EPSILON	= 0.01;	// define threshold (approx. Value of Mh at a peak or plateau)
	private final double	TC_DIST_FACTOR	= 0.5;		// cluster search windows near convergence that are a distance

	private final int		LIMIT  = 100; // define max. # of iterations to find mode

    private final int NODE_MULTIPLE = 10;

							
	//Linear Storage (used by lattice and bst)////////
    private double data[];								
    // memory allocated for data points stored by tree nodes
	// when used by the lattice data structure data does not store
    // the lattice information; format of data:
	// data = <x11, x12, ..., x1N,...,xL1, xL2, ..., xLN>
    // in the case of the lattice the i in data(i,j) corresponds
    
    // Lattice Data Structure////////
  	private int	height, width;	// Height and width of lattice
  	
    //Range Searching on General Input Data Set////////
  	//private tree root[]; // root of kdBST used to store input

  	//private tree forest[]; // memory allocated for tree nodes

  	//private float range[]; // range vector used to perform range search on kd tree, indexed
  	
    // KERNEL DATA STRUCTURE 
 	private float h[]; // bandwidth vector

 	private float offset[];	// defines bandwidth offset caused by the use of a Gaussian kernel (for example)
 	
 	//Kernel
 	public enum kernelType		{Uniform, Gaussian, UserDefined};
 	
 	// KERNEL DATA STRUCTURE

 	private kernelType	kernel[];  // kernel types for each subspace S[i]

 	private double	w[][];  // weight function lookup table

 	private double	increment[];	 // increment used by weight hashing function

 	//private boolean	uniformKernel; // flag used to indicate if the kernel is uniform or not
 	
 	private userWeightFunct	head, cur; // user defined weight function linked list
    
 	// CLASS STATE 
 	private ClassStateStruct	class_state; //specifies the state of the class(i.e if data has been loaded into 
 											 //the class, if a kernel has been defined, etc.)

 	// MEAN SHIFT PROCESSING DATA STRUCTURES 
    
    // Speed Up Level
    public enum SpeedUpLevel	{NO_SPEEDUP, MED_SPEEDUP, HIGH_SPEEDUP};
    
    // OUTPUT DATA STORAGE
    // Raw Data (1 to 1 correlation with input)////////
 	private float msRawData[];	// Raw data output of mean shift algorithm
 					    // to the location of the data point on the lattice

    //Data Modes
    private int	labels[]; // assigns a label to each data point associating it to
						  // a mode in modes (e.g. a data point having label l has
						  // mode modes[l])

    private float modes[]; // stores the mode data of the input data set, indexed by labels

    private int	modePointCounts[]; // stores for each mode the number of point mapped to that mode,
								   // indexed by labels

    // Index Table
    private int	indexTable[]; //used during fill algorithm

    // LUV_data
    private float LUV_data[]; //stores modes in float format on lattice

    // Image Regions
    private int regionCount; // stores the number of connected regions contained by the image
    
    // BASIN OF ATTRACTION 
 	private byte modeTable[]; // Assigns a marking to each data point specifying whether
 							  // or not it has been assigned a mode. These labels are:
 							  // modeTable[i] = 0 - data point i is not associated with a mode
 						      // modeTable[i] = 1 - data point i is associated with a mode
 							  // modeTable[i] = 2 - data point i is associated with a mode
 							  //                    however its mode is yet to be determined

 	private int	pointList[];  // a list of data points that due to basin of attraction will
	                          // converge to the same mode as the mode that mean shift is
	                          // currently being applied to
 	
 	private int	pointCount;	// the number of points stored by the point list
    
  // IMAGE PRUNING 
    //##########################################

 	// Transitive Closure
 	float			rR2;					//defines square range radius used when clustering pixels
 											//together, thus defining image regions
 	
 	//Image Boundaries
 	RegionList	regionList;			// stores the boundary locations for each region
 	
 	//8 Connected Neighbors/////////
 	private int	neigh[] = new int[8];
 	
 	private double LUV_threshold = 0.1;        //in float mode this determines what "close" means between modes
 	
 	//  REGION ADJACENCY MATRIX

 	// Region Adjacency List
 	private RAList	raList[];				// an array of RAList objects containing an entry for each
 									// region of the image

 	// RAMatrix Free List
 	private RAList	freeRAList[];			// a pointer to the head of a region adjacency list object
 											// free list

 	private RAList raPool[];				// a pool of RAList objects used in the construction of the
 									// RAM

    // COMPUTATION OF EDGE STRENGTHS
 	private double			epsilon = 1.0;				//Epsilon used for transitive closure
 	
 	private int z;
 	private int zDim;
	private int t;
	private int cf;
	
	private byte segmentationBoundaries[] = null;
 	
 	private byte outputBuffer[];
    
    private kernelType spatialKernelType = kernelType.Uniform;
    
    private kernelType rangeKernelType = kernelType.Uniform;
    
    private float spatialBandwidth = 8.0f;
    
    private float rangeBandwidth = 7.0f;
    
    private int minRegion;
    
    private SpeedUpLevel speedUpLevel;

	private boolean measureTime;
	
	private double speedThreshold; // the fraction of window radius used in new optimized filter 2.
	
	// WEIGHT MAP USED WHEN COMPUTING MEAN SHIFT ON A LATTICE
	private double  weightMap[]; // weight map that may be used to weight the kernel
	 					         // upon performing mean shift on a lattice

    private boolean	weightMapDefined = false; // used to indicate if a lattice weight map has been defined
    
    private ModelImage filteredImage = null;
    
    private ModelImage boundariesImage = null;
	
	public AlgorithmMeanShiftSegmentation(ModelImage destImage, ModelImage srcImage, kernelType spatialKernelType,
			kernelType rangeKernelType, float spatialBandwidth, float rangeBandwidth, int minRegion,
			SpeedUpLevel speedUpLevel, boolean measureTime, double speedThreshold, double weightMap[],
			boolean weightMapDefined, ModelImage filteredImage, ModelImage boundariesImage) {
		super(destImage, srcImage);
		this.spatialKernelType = spatialKernelType;
		this.rangeKernelType = rangeKernelType;
		this.spatialBandwidth = spatialBandwidth;
		this.rangeBandwidth = rangeBandwidth;
		this.minRegion = minRegion;
		this.speedUpLevel = speedUpLevel;
		this.measureTime = measureTime;
		this.speedThreshold = speedThreshold;
		this.weightMap = weightMap;
		this.weightMapDefined = weightMapDefined;
		this.filteredImage = filteredImage;
		this.boundariesImage = boundariesImage;
	}
	
	public void runAlgorithm() {
		int nDims;
		int tDim;
		float rgb[] = null;
		int i;
		
		//int kN;
		// Start porting from msImageProcessor::DefineImage
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(),"Mean Shift Segmentation ...");
        
        nDims = srcImage.getNDims();
        width = srcImage.getExtents()[0];
        height = srcImage.getExtents()[1];
        if (nDims > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        L = width * height;
        if (srcImage.isColorImage()) {
        	data = new double[3*L];
        	rgb = new float[4*L];
        	N = 3;
        	outputBuffer = new byte[4*L];
        	cf = 4;
        }
        else {
		    data = new double[L];
		    N = 1;
		    outputBuffer = new byte[L];
		    cf = 1;
        }
        if (boundariesImage != null) {
        	segmentationBoundaries = new byte[L];
        }
        class_state = new ClassStateStruct();
        //allocate memory for weight map
        
        //define default kernel parameters...
        kernel	= new kernelType[]{spatialKernelType, rangeKernelType};
      	P = new int[]{2, N};
      	h	= new float[]{spatialBandwidth , rangeBandwidth};
      	kp = 2;
      	offset = new float[kp];
		increment = new double[kp];
		//kN = 0;
		//for (i = 0; i < kp; i++) {
			//kN += P[i];
		//}
		//range = new float[2*kN];
		//uv = new double[kN];
		// Generate weight function lookup table
		// using above information and user
		// defined weight function list
		generateLookupTable();
		
		for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
        		meanShift();
        		if (srcImage.isColorImage()) {
        			try {
        		        srcImage.exportData(4*(z + t*zDim)*L, 4*L, rgb);	
        			}
        			catch(IOException e) {
	        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        			setCompleted(false);
	        			return;
        			}
        			
        			
        			for(i = 0; i < L; i++)
        			{
        					RGBtoLUV(rgb, 4*i, data, 3*i);
        			}

        		}
        		else {
	        		try {
	        			srcImage.exportData((z + t*zDim)*L, L, data);
	        		}
	        		catch(IOException e) {
	        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        			setCompleted(false);
	        			return;
	        		}
        		}
        		
        		//define input defined on a lattice using mean shift base class
        		defineLInput();
                segment();
                if (boundariesImage != null) {
                	defineBoundaries();
                }
                
                if (destImage != null) {
                	try {
                		destImage.importData(cf*(t*zDim+z)*L, outputBuffer, false);
                	}
                	catch (IOException e) {
                		MipavUtil.displayError("IOException " + e + " on destImage.importData");
                		setCompleted(false);
                		return;
                	}
                }
                else {
                	try {
                		srcImage.importData(cf*(t*zDim+z)*L, outputBuffer, false);
                	}
                	catch (IOException e) {
                		MipavUtil.displayError("IOException " + e + " on srcImage.importData");
                		setCompleted(false);
                		return;
                	}	
                }
        	} // for (z = 0; z < zDim; z++)
		} // for (t = 0; t < tDim; t++)
		meanShift();
		if (destImage != null) {
			destImage.calcMinMax();
		}
		else {
			srcImage.calcMinMax();
		}
		
		if (filteredImage != null) {
			filteredImage.calcMinMax();
		}
		
		if (boundariesImage != null) {
			boundariesImage.calcMinMax();
			segmentationBoundaries = null;
		}
		setCompleted(true);
		return;
		
	}
	
	/*******************************************************/
	/*Define Boundaries                                    */
	/*******************************************************/
	/*Defines the boundaries for each region of the segm-  */
	/*ented image storing the result into a region list    */
	/*object.                                              */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - the image has been segmented and a classifi- */
	/*        cation structure has been created for this   */
	/*        image                                        */
	/*Post:                                                */
	/*      - the boundaries of the segmented image have   */
	/*        been defined and the boundaries of each reg- */
	/*        ion has been stored into a region list obj-  */
	/*        ect.                                         */
	/*******************************************************/

	private void defineBoundaries()
	{

		//declare and allocate memory for boundary map and count
		int	boundaryMap[] = new int[L];
		int boundaryCount[] = new int[regionCount];

		//initialize boundary map and count
		int i;
		for(i = 0; i < L; i++)
			boundaryMap[i]		= -1;
		for(i = 0; i < regionCount; i++)
			boundaryCount[i]	=  0;

		//initialize and declare total boundary count -
		//the total number of boundary pixels present in
		//the segmented image
		int	totalBoundaryCount	= 0;

		//traverse the image checking the right and bottom
		//four connected neighbors of each pixel marking
		//boundary map with the boundaries of each region and
		//incrementing boundaryCount using the label information

		//***********************************************************************
		//***********************************************************************

		int		j, label, dataPoint;

		//first row (every pixel is a boundary pixel)
		for(i = 0; i < width; i++)
		{
				boundaryMap[i]		= label	= labels[i];
				boundaryCount[label]++;
				totalBoundaryCount++;
		}

		//define boundaries for all rows except for the first
		//and last one...
		for(i = 1; i < height - 1; i++)
		{
			//mark the first pixel in an image row as an image boundary...
			dataPoint				= i*width;
			boundaryMap[dataPoint]	= label	= labels[dataPoint];
			boundaryCount[label]++;
			totalBoundaryCount++;

			for(j = 1; j < width - 1; j++)
			{
				//define datapoint and its right and bottom
				//four connected neighbors
				dataPoint		= i*width+j;

				//check four connected neighbors if they are
				//different this pixel is a boundary pixel
				label	= labels[dataPoint];
				if((label != labels[dataPoint-1])    ||(label != labels[dataPoint+1])||
				   (label != labels[dataPoint-width])||(label != labels[dataPoint+width]))
				{
					boundaryMap[dataPoint]		= label	= labels[dataPoint];
					boundaryCount[label]++;
					totalBoundaryCount++;
				}
			}

			//mark the last pixel in an image row as an image boundary...
			dataPoint				= (i+1)*width-1;
			boundaryMap[dataPoint]	= label	= labels[dataPoint];
			boundaryCount[label]++;
			totalBoundaryCount++;

		}

		//last row (every pixel is a boundary pixel) (i = height-1)
		int	start	= (height-1)*width, stop = L;
		for(i = start; i < stop; i++)
		{
			boundaryMap[i]		= label	= labels[i];
			boundaryCount[label]++;
			totalBoundaryCount++;
		}

		//***********************************************************************
		//***********************************************************************

		//store boundary locations into a boundary buffer using
		//boundary map and count

		//***********************************************************************
		//***********************************************************************

		int	boundaryBuffer[]	= new int [totalBoundaryCount];
		int boundaryIndex[]	= new int [regionCount];

		//use boundary count to initialize boundary index...
		int counter = 0;
		for(i = 0; i < regionCount; i++)
		{
			boundaryIndex[i]	= counter;
			counter			   += boundaryCount[i];
		}

		//traverse boundary map placing the boundary pixel
		//locations into the boundaryBuffer
		for(i = 0; i < L; i++)
		{
			//if its a boundary pixel store it into
			//the boundary buffer
			if((label = boundaryMap[i]) >= 0)
			{
				segmentationBoundaries[i] = 1;
				boundaryBuffer[boundaryIndex[label]] = i;
				boundaryIndex[label]++;
			}
			else {
				segmentationBoundaries[i] = 0;
			}
		}
		
		try {
    		boundariesImage.importData((t*zDim+z)*L, segmentationBoundaries, false);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException " + e + " on boundariesImage.importData");
    		setCompleted(false);
    		return;
    	}

		//***********************************************************************
		//***********************************************************************

		//store the boundary locations stored by boundaryBuffer into
		//the region list for each region

		//***********************************************************************
		//***********************************************************************

		//destroy the old region list
		regionList = null;

		//create a new region list
		regionList	= new RegionList(regionCount, totalBoundaryCount, N);

		//add boundary locations for each region using the boundary
		//buffer and boundary counts
		counter	= 0;
		for(i = 0; i < regionCount; i++)
		{
			regionList.addRegion(i, boundaryCount[i],boundaryBuffer, counter);
			counter += boundaryCount[i];
		}

		//***********************************************************************
		//***********************************************************************

	   // deallocate local used memory
	  boundaryMap = null;
	  boundaryCount = null;
	  boundaryBuffer = null;
	  boundaryIndex = null;

		//done.
		return;

	}

	
	/*******************************************************/
	/*Generate Lookup Table                                */
	/*******************************************************/
	/*A weight function look up table is generated.        */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - kernel is an array of kernelTypes specifying */
	/*        the type of kernel to be used on each sub-   */
	/*        space of the input data set x                */
	/*      - kp is the total number of subspaces used to  */
	/*        the input data set x                         */
	/*      - the above information has been pre-loaded    */
	/*        into the MeanShift class private members     */
	/*Post:                                                */
	/*      - a lookup table is generated for the weight   */
	/*        function of the resulting kernel             */
	/*      - uniformKernel is set to true if the kernel   */
	/*        to be used is uniform, false is returned     */
	/*        otherwise                                    */
	/*      - if a user defined weight function is requred */
	/*        for a given subspace but not defined in the  */
	/*        user defined weight function list, an error  */
	/*        is flagged and the program is halted         */
	/*******************************************************/

	private void generateLookupTable()
	{
		
		// Declare Variables
		int i,j;
		
		// Allocate memory for lookup table w
		w = new double[kp][];
		
		// Traverse through kernel generating weight function
		// lookup table w
		
		for(i = 0; i < kp; i++)
	    {
			switch(kernel[i])
			{
				// *Uniform Kernel* has weight funciton w(u) = 1
				// therefore, a weight funciton lookup table is
				// not needed for this kernel --> w[i] = NULL indicates
				// this
			case Uniform:
				
				w        [i] = null;  //weight function not needed for this kernel
				offset   [i] =    1;  //uniform kernel has u < 1.0
				increment[i] =    1;  //has no meaning
				break;
				
				// *Gaussian Kernel* has weight function w(u) = constant*exp(-u^2/[2h[i]^2])
			case Gaussian:
				
				// generate weight function using expression,
				// exp(-u/2), where u = norm(xi - x)^2/h^2
				
				// Allocate memory for weight table
				w[i] = new double [GAUSS_NUM_ELS+1];
				
				for(j = 0; j <= GAUSS_NUM_ELS; j++)
					w[i][j] = Math.exp(-j*GAUSS_INCREMENT/2);
				
				// Set offset = offset^2, and set increment
				offset   [i] = (float)(GAUSS_LIMIT*GAUSS_LIMIT);
				increment[i] = GAUSS_INCREMENT;
				
				// done
				break;
				
				// *User Define Kernel* uses the weight function wf(u)
			case UserDefined:
				
				// Search for user defined weight function
				// defined for subspace (i+1)
				cur = head;
				while((cur != null)&&(cur.subspace != (i+1)))
					cur = cur.next;
				
				// If a user defined subspace has not been found
				// for this subspace, flag an error
				if(cur == null)
				{
					Preferences.debug("\ngenerateLookupTable Fatal Error: User defined kernel for subspace " + (i+1) + " undefined.\n\nAborting Program.\n\n",
							Preferences.DEBUG_ALGORITHM);
					MipavUtil.displayError("Lookup table fatal error for user defined kernel for subspace " + (i+1));
					System.exit(1);
				}
				
				// Otherwise, copy weight function lookup table to w[i]
				w[i] = new double [cur.sampleNumber+1];
				for(j = 0; j <= cur.sampleNumber; j++)
					w[i][j] = cur.w[j];
				
				// Set offset and increment accordingly
				offset   [i] = (float)(cur.halfWindow);
				increment[i] = cur.halfWindow/(float)(cur.sampleNumber);
				
				// done
				break;
				
			default:
				
				MipavUtil.displayError("MeanShift generateLookupTable Unknown kernel type.");
				
			}
			
	    }
	}


	
	private void meanShift()
	{
		
		//intialize kernel strucuture...
		w							= null;
		
		//initialize weight function linked list...
		head						= cur	= null;
		
		//Initialize class state...
		class_state.OUTPUT_DEFINED	= false;
		
		//intialize basin of attraction structure
		//used by the filtering algorithm
		modeTable			= null;
		pointList			= null;
		pointCount			= 0;
		
		//initialize region list
		regionList			= null;

		//initialize output structures...
		msRawData			= null;
		labels				= null;
		modes				= null;
		modePointCounts		= null;
		regionCount			= 0;

		//intialize temporary buffers used for
		//performing connected components
		indexTable			= null;
		LUV_data			= null;

		//initialize region adjacency matrix
		raList				= null;
		freeRAList			= null;
		raPool				= null;

		//initialize epsilon such that transitive closure
		//does not take edge strength into consideration when
		//fusing regions of similar color
		epsilon				= 1.0;

	//Changed by Sushil from 1.0 to 0.1, 11/11/2008
	   LUV_threshold = 0.1;

		
	}

	
	private void defineLInput()
	{
		class_state.OUTPUT_DEFINED	= false;
		
		//done.
		return;
		
	}
	
	private void RGBtoLUV(float rgb[], int rgbOffset, double luv[], int luvOffset)
	{

		//declare variables
		double	x, y, z, L0, u_prime, v_prime, constant;

		//convert RGB to XYZ...
		x		= XYZ[0][0]*rgb[rgbOffset+1] + XYZ[0][1]*rgb[rgbOffset+2] + XYZ[0][2]*rgb[rgbOffset+3];
		y		= XYZ[1][0]*rgb[rgbOffset+1] + XYZ[1][1]*rgb[rgbOffset+2] + XYZ[1][2]*rgb[rgbOffset+3];
		z		= XYZ[2][0]*rgb[rgbOffset+1] + XYZ[2][1]*rgb[rgbOffset+2] + XYZ[2][2]*rgb[rgbOffset+3];

		//convert XYZ to LUV...

		//compute L*
	    L0		= y / (255.0 * Yn);
		if(L0 > Lt)
			luv[luvOffset]	= (116.0 * (Math.pow(L0, 1.0/3.0)) - 16.0);
		else
			luv[luvOffset]	= (903.3 * L0);

		//compute u_prime and v_prime
		constant	= x + 15 * y + 3 * z;
		if(constant != 0)
		{
			u_prime	= (4 * x) / constant;
			v_prime = (9 * y) / constant;
		}
		else
		{
			u_prime	= 4.0;
			v_prime	= 9.0/15.0;
		}

		//compute u* and v*
	    luv[luvOffset+1] = (13 * luv[luvOffset] * (u_prime - Un_prime));
	    luv[luvOffset+2] = (13 * luv[luvOffset] * (v_prime - Vn_prime));

		//done.
		return;

	}
	
	/*/\/\/\/\/\/\/\/\/\/\*/
	  /* Image Segmentation */
	  /*\/\/\/\/\/\/\/\/\/\/*/

	/*******************************************************/
	/*Segment                                              */
	/*******************************************************/
	/*Segments the defined image.                          */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - sigmaS and sigmaR are the spatial and range  */
	/*        radii of the search window respectively      */
	/*      - minRegion is the minimum point density that  */
	/*        a region may have in the resulting segment-  */
	/*        ed image                                     */
	/*      - speedUpLevel determines whether or not the   */
	/*        filtering should be optimized for faster     */
	/*        execution: a value of NO_SPEEDUP turns this  */
	/*        optimization off and a value SPEEDUP turns   */
	/*        this optimization on                         */
	/*Post:                                                */
	/*      - the defined image is segmented and the       */
	/*        resulting segmented image is stored in the   */
	/*        private data members of the image segmenter  */
	/*        class.                                       */
	/*      - any regions whose point densities are less   */
	/*        than or equal to minRegion have been pruned  */
	/*        from the segmented image.                    */
	/*******************************************************/

	private void segment()
	{
        long time1 = 0;
        long time2;
        long elapsedTime;
        long time3 = 0;
        long time4;
        int pxValue;
        int i;
		//Apply mean shift to data set using sigmaS and sigmaR...
		filter();
		
		if (filteredImage != null) {
			if (N  == 1) {
			    for (i = 0; i < L; i++) {
			    	pxValue = (int)(msRawData[i] + 0.5);
			    	if (pxValue < 0) {
			    		outputBuffer[i] = (byte)0;
			    	}
			    	else if (pxValue > 255) {
			    	    outputBuffer[i] = (byte)255;	
			    	}
			    	else {
			    		outputBuffer[i] = (byte)pxValue;
			    	}
			    }
			} // if (N == 1)
			else if (N == 3) {
				for (i = 0; i < L; i++) {
					LUVtoRGB(msRawData, 3*i, outputBuffer, 4*i);
				}
			}
			
			try {
        		filteredImage.importData(cf*(t*zDim+z)*L, outputBuffer, false);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on filteredImage.importData");
        		setCompleted(false);
        		return;
        	}
		} // if (filteredImage != null)
		
        Preferences.debug("Applying transitive closure...\n", Preferences.DEBUG_ALGORITHM);
	    if (measureTime) {
	    	time1 = System.currentTimeMillis();
	    }

		//Apply transitive closure iteratively to the regions classified
		//by the RAM updating labels and modes until the color of each neighboring
		//region is within sqrt(rR2) of one another.
		rR2 = (float)(h[1]*h[1]*0.25);
		transitiveClosure();
		int oldRC = regionCount;
		int deltaRC, counter = 0;
		do {
			transitiveClosure();
			deltaRC = oldRC-regionCount;
			oldRC = regionCount;
			counter++;
		} while ((deltaRC <= 0)&&(counter < 10));

		if (measureTime) {
			time2 = System.currentTimeMillis();
			elapsedTime = time2 - time1;
			Preferences.debug(elapsedTime + "milliseconds, numRegions = " + regionCount + "\n", 
					Preferences.DEBUG_ALGORITHM);
			Preferences.debug("About to prune spurious regions\n", Preferences.DEBUG_ALGORITHM);
			time3 = System.currentTimeMillis();
		}

		//Prune spurious regions (regions whose area is under
		//minRegion) using RAM
		prune(minRegion);

		if (measureTime) {
			time4 = System.currentTimeMillis();
			elapsedTime = time4 - time3;
			Preferences.debug("done. (" + elapsedTime +  "milliseconds, numRegions = " + regionCount + "\n",
					Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Pruned spurious regions\n", Preferences.DEBUG_ALGORITHM);
		}

		//de-allocate memory for region adjacency matrix
		destroyRAM();

		//output to msRawData
		int j, label;
		for(i = 0; i < L; i++)
		{
			label	= labels[i];
			for(j = 0; j < N; j++)
				{
					msRawData[N*i+j] = modes[N*label+j];
				}
		}
		
		if (N  == 1) {
		    for (i = 0; i < L; i++) {
		    	pxValue = (int)(msRawData[i] + 0.5);
		    	if (pxValue < 0) {
		    		outputBuffer[i] = (byte)0;
		    	}
		    	else if (pxValue > 255) {
		    	    outputBuffer[i] = (byte)255;	
		    	}
		    	else {
		    		outputBuffer[i] = (byte)pxValue;
		    	}
		    }
		} // if (N == 1)
		else if (N == 3) {
			for (i = 0; i < L; i++) {
				LUVtoRGB(msRawData, 3*i, outputBuffer, 4*i);
			}
		}

		//done.
		return;

	}
	
	private void LUVtoRGB(float luvVal[], int luvOffset, byte rgbVal[], int rgbOffset)
	{

		//declare variables...
		int		r, g, b;
		double	x, y, z, u_prime, v_prime;

		//perform conversion
		if(luvVal[luvOffset] < 0.1)
			r = g = b = 0;
		else
		{
			//convert luv to xyz...
			if(luvVal[luvOffset] < 8.0)
				y	= Yn * luvVal[luvOffset] / 903.3;
			else
			{
				y	= (luvVal[luvOffset] + 16.0) / 116.0;
				y  *= Yn * y * y;
			}

			u_prime	= luvVal[luvOffset+1] / (13 * luvVal[luvOffset]) + Un_prime;
			v_prime	= luvVal[luvOffset+2] / (13 * luvVal[luvOffset]) + Vn_prime;

			x		= 9 * u_prime * y / (4 * v_prime);
			z		= (12 - 3 * u_prime - 20 * v_prime) * y / (4 * v_prime);

			//convert xyz to rgb...
			//[r, g, b] = RGB*[x, y, z]*255.0
			r		= my_round((RGB[0][0]*x + RGB[0][1]*y + RGB[0][2]*z)*255.0);
			g		= my_round((RGB[1][0]*x + RGB[1][1]*y + RGB[1][2]*z)*255.0);
			b		= my_round((RGB[2][0]*x + RGB[2][1]*y + RGB[2][2]*z)*255.0);

			//check bounds...
			if(r < 0)	r = 0; if(r > 255)	r = 255;
			if(g < 0)	g = 0; if(g > 255)	g = 255;
			if(b < 0)	b = 0; if(b > 255)	b = 255;

		}

		//assign rgb values to rgb vector rgbVal
		rgbVal[rgbOffset+1]	= (byte)r;
		rgbVal[rgbOffset+2]	= (byte)g;
		rgbVal[rgbOffset+3]	= (byte)b;

		//done.
		return;

	}
	
	private int my_round(double in_x)
	{
		if (in_x < 0)
			return (int)(in_x - 0.5);
		else
			return (int)(in_x + 0.5);
	}



	
	/*******************************************************/
	/*Destroy Region Adjacency Matrix                      */
	/*******************************************************/
	/*Destroy a region adjacency matrix.                   */
	/*******************************************************/
	/*Post:                                                */
	/*      - the region adjacency matrix has been destr-  */
	/*        oyed: (1) its memory has been de-allocated,  */
	/*        (2) the RAM structure has been initialize    */
	/*        for re-use.                                  */
	/*******************************************************/

	private void destroyRAM()
	{
		//initialize region adjacency matrix
		raList				= null;
		freeRAList			= null;
		raPool				= null;

		//done.
		return;

	}

	
	/*******************************************************/
	/*Prune                                                */
	/*******************************************************/
	/*Prunes regions from the image whose pixel density    */
	/*is less than a specified threshold.                  */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - minRegion is the minimum allowable pixel de- */
	/*        nsity a region may have without being pruned */
	/*        from the image                               */
	/*Post:                                                */
	/*      - regions whose pixel density is less than     */
	/*        or equal to minRegion have been pruned from  */
	/*        the image.                                   */
	/*******************************************************/

	private void prune(int minRegion)
	{
		
		//Allocate Memory for temporary buffers...
		
		//allocate memory for mode and point count temporary buffers...
		float	modes_buffer[]	= new float	[N*regionCount];
		int		MPC_buffer[]		= new int	[regionCount];
		
		//allocate memory for label buffer
		int	label_buffer[]		= new int	[regionCount];
		
		//Declare variables
		int		i, k, candidate, iCanEl, neighCanEl, iMPC, label, minRegionCount;
		double	minSqDistance, neighborDistance;
		RAList	neighbor[];
		
		//Apply pruning algorithm to classification structure, removing all regions whose area
		//is under the threshold area minRegion (pixels)
		do
		{
			//Assume that no region has area under threshold area  of 
			minRegionCount	= 0;		

			//Step (1):
			
			// Build RAM using classifiction structure originally
			// generated by the method GridTable::Connect()
			buildRAM();
			
			// Step (2):
			
			// Traverse the RAM joining regions whose area is less than minRegion (pixels)
			// with its respective candidate region.
			
			// A candidate region is a region that displays the following properties:
			
			//	- it is adjacent to the region being pruned
			
			//  - the distance of its mode is a minimum to that of the region being pruned
			//    such that or it is the only adjacent region having an area greater than
			//    minRegion
			
			for(i = 0; i < regionCount; i++)
			{
				//if the area of the ith region is less than minRegion
				//join it with its candidate region...

				//*******************************************************************************

				//Note: Adjust this if statement if a more sophisticated pruning criterion
				//      is desired. Basically in this step a region whose area is less than
				//      minRegion is pruned by joining it with its "closest" neighbor (in color).
				//      Therefore, by placing a different criterion for fusing a region the
				//      pruning method may be altered to implement a more sophisticated algorithm.

				//*******************************************************************************

				if(modePointCounts[i] < minRegion)
				{
					//update minRegionCount to indicate that a region
					//having area less than minRegion was found
					minRegionCount++;

					//obtain a pointer to the first region in the
					//region adjacency list of the ith region...
					neighbor	= raList[i].next;
					
					//calculate the distance between the mode of the ith
					//region and that of the neighboring region...
					candidate		= neighbor[0].label;
					minSqDistance	= sqDistance(i, candidate);
					
					//traverse region adjacency list of region i and select
					//a candidate region
					neighbor	= neighbor[0].next;
					while(neighbor != null)
					{

						//calculate the square distance between region i
						//and current neighbor...
						neighborDistance = sqDistance(i, neighbor[0].label);

						//if this neighbors square distance to region i is less
						//than minSqDistance, then select this neighbor as the
						//candidate region for region i
						if(neighborDistance < minSqDistance)
						{
							minSqDistance	= neighborDistance;
							candidate		= neighbor[0].label;
						}

						//traverse region list of region i
						neighbor	= neighbor[0].next;

					}

					//join region i with its candidate region:

					// (1) find the canonical element of region i
					iCanEl		= i;
					while(raList[iCanEl].label != iCanEl)
						iCanEl		= raList[iCanEl].label;

					// (2) find the canonical element of neighboring region
					neighCanEl	= candidate;
					while(raList[neighCanEl].label != neighCanEl)
						neighCanEl	= raList[neighCanEl].label;

					// if the canonical elements of are not the same then assign
					// the canonical element having the smaller label to be the parent
					// of the other region...
					if(iCanEl < neighCanEl)
						raList[neighCanEl].label	= iCanEl;
					else
					{
						//must replace the canonical element of previous
						//parent as well
						raList[raList[iCanEl].label].label	= neighCanEl;

						//re-assign canonical element
						raList[iCanEl].label				= neighCanEl;
					}
				}
			}

			// Step (3):
			
			// Level binary trees formed by canonical elements
			for(i = 0; i < regionCount; i++)
			{
				iCanEl	= i;
				while(raList[iCanEl].label != iCanEl)
					iCanEl	= raList[iCanEl].label;
				raList[i].label	= iCanEl;
			}
			
			// Step (4):
			
			//Traverse joint sets, relabeling image.
			
			// Accumulate modes and re-compute point counts using canonical
			// elements generated by step 2.
			
			//initialize buffers to zero
			for(i = 0; i < regionCount; i++)
				MPC_buffer[i]	= 0;
			for(i = 0; i < N*regionCount; i++)
				modes_buffer[i]	= 0;
			
			//traverse raList accumulating modes and point counts
			//using canoncial element information...
			for(i = 0; i < regionCount; i++)
			{
				
				//obtain canonical element of region i
				iCanEl	= raList[i].label;
				
				//obtain mode point count of region i
				iMPC	= modePointCounts[i];
				
				//accumulate modes_buffer[iCanEl]
				for(k = 0; k < N; k++)
					modes_buffer[(N*iCanEl)+k] += iMPC*modes[(N*i)+k];
				
				//accumulate MPC_buffer[iCanEl]
				MPC_buffer[iCanEl] += iMPC;
				
			}
			
			// (b)
			
			// Re-label new regions of the image using the canonical
			// element information generated by step (2)
			
			// Also use this information to compute the modes of the newly
			// defined regions, and to assign new region point counts in
			// a consecute manner to the modePointCounts array
			
			//initialize label buffer to -1
			for(i = 0; i < regionCount; i++)
				label_buffer[i]	= -1;
			
			//traverse raList re-labeling the regions
			label = -1;
			for(i = 0; i < regionCount; i++)
			{
				//obtain canonical element of region i
				iCanEl	= raList[i].label;
				if(label_buffer[iCanEl] < 0)
				{
					//assign a label to the new region indicated by canonical
					//element of i
					label_buffer[iCanEl]	= ++label;
					
					//recompute mode storing the result in modes[label]...
					iMPC	= MPC_buffer[iCanEl];
					for(k = 0; k < N; k++)
						modes[(N*label)+k]	= (modes_buffer[(N*iCanEl)+k])/(iMPC);
					
					//assign a corresponding mode point count for this region into
					//the mode point counts array using the MPC buffer...
					modePointCounts[label]	= MPC_buffer[iCanEl];
				}
			}
			
			//re-assign region count using label counter
			regionCount		= label+1;
			
			// (c)
			
			// Use the label buffer to reconstruct the label map, which specified
			// the new image given its new regions calculated above
			
			for(i = 0; i < L; i++)
				labels[i]	= label_buffer[raList[labels[i]].label];

			
		}	while(minRegionCount > 0);

		//de-allocate memory
		modes_buffer = null;
		MPC_buffer = null;
		label_buffer = null;
		
		//done.
		return;
		
	}
	
	/*******************************************************/
	/*Square Distance                                      */
	/*******************************************************/
	/*Computs the normalized square distance between two   */
	/*modes.                                               */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - mode1 and mode2 are indeces into the modes   */
	/*        array specifying two modes of the image      */
	/*Post:                                                */
	/*      - the normalized square distance between modes */
	/*        indexed by mode1 and mode2 has been calc-    */
	/*        ulated and the result has been returned.     */
	/*******************************************************/

	private float sqDistance(int mode1, int mode2)
	{

		int		k		= 1, s	= 0, p;
		float	dist	= 0, el;
		for(k = 1; k < kp; k++)
		{
			//Calculate distance squared of sub-space s	
			for(p = 0; p < P[k]; p++)
			{
				el    = (modes[mode1*N+p+s]-modes[mode2*N+p+s])/(h[k]*offset[k]);
				dist += el*el;
			}
			
			//next subspace
			s += P[k];
			k++;
		}

		//return normalized square distance between modes
		//1 and 2
		return dist;

	}


	
	/*******************************************************/
	/*transitive Closure                                   */
	/*******************************************************/
	/*Applies transitive closure to the RAM updating       */
	/*labels, modes and modePointCounts to reflect the new */
	/*set of merged regions resulting from transitive clo- */
	/*sure.                                                */
	/*******************************************************/
	/*Post:                                                */
	/*      - transitive closure has been applied to the   */
	/*        regions classified by the RAM and labels,    */
	/*        modes and modePointCounts have been updated  */
	/*        to reflect the new set of mergd regions res- */
	/*        ulting from transitive closure.              */
	/*******************************************************/

	private void transitiveClosure()
	{

		//Step (1):

		// Build RAM using classifiction structure originally
		// generated by the method GridTable::Connect()
		buildRAM();

		//Step (1a):
		//Compute weights of weight graph using confidence map
		//(if defined)
		if(weightMapDefined)	computeEdgeStrengths();

		//Step (2):

		//Treat each region Ri as a disjoint set:

		// - attempt to join Ri and Rj for all i != j that are neighbors and
		//   whose associated modes are a normalized distance of < 0.5 from one
		//   another

		// - the label of each region in the raList is treated as a pointer to the
		//   canonical element of that region (e.g. raList[i], initially has raList[i].label = i,
		//   namely each region is initialized to have itself as its canonical element).

		//Traverse RAM attempting to join raList[i] with its neighbors...
		int		i, iCanEl, neighCanEl;
		//double	threshold;
		RAList	neighbor[];
		for(i = 0; i < regionCount; i++)
		{
			//acquire first neighbor in region adjacency list pointed to
			//by raList[i]
			neighbor	= raList[i].next;

			//compute edge strenght threshold using global and local
			//epsilon
			//if(epsilon > raList[i].edgeStrength)
				//threshold   = epsilon;
			//else
				//threshold   = raList[i].edgeStrength;

			//traverse region adjacency list of region i, attempting to join
			//it with regions whose mode is a normalized distance < 0.5 from
			//that of region i...
			while(neighbor != null)
			{
				//attempt to join region and neighbor...
				if((inWindow(i, neighbor[0].label))&&(neighbor[0].edgeStrength < epsilon))
				{
					//region i and neighbor belong together so join them
					//by:

					// (1) find the canonical element of region i
					iCanEl		= i;
					while(raList[iCanEl].label != iCanEl)
						iCanEl		= raList[iCanEl].label;

					// (2) find the canonical element of neighboring region
					neighCanEl	= neighbor[0].label;
					while(raList[neighCanEl].label != neighCanEl)
						neighCanEl	= raList[neighCanEl].label;

					// if the canonical elements of are not the same then assign
					// the canonical element having the smaller label to be the parent
					// of the other region...
					if(iCanEl < neighCanEl)
						raList[neighCanEl].label	= iCanEl;
					else
					{
						//must replace the canonical element of previous
						//parent as well
						raList[raList[iCanEl].label].label	= neighCanEl;

						//re-assign canonical element
						raList[iCanEl].label				= neighCanEl;
					}
				}

				//check the next neighbor...
				neighbor	= neighbor[0].next;

			}
		}

		// Step (3):

		// Level binary trees formed by canonical elements
		for(i = 0; i < regionCount; i++)
		{
			iCanEl	= i;
			while(raList[iCanEl].label != iCanEl)
				iCanEl	= raList[iCanEl].label;
			raList[i].label	= iCanEl;
		}

		// Step (4):

		//Traverse joint sets, relabeling image.

		// (a)

		// Accumulate modes and re-compute point counts using canonical
		// elements generated by step 2.

		//allocate memory for mode and point count temporary buffers...
		float	modes_buffer[]	= new float	[N*regionCount];
		int		MPC_buffer[]		= new int	[regionCount];

		//initialize buffers to zero
		for(i = 0; i < regionCount; i++)
			MPC_buffer[i]	= 0;
		for(i = 0; i < N*regionCount; i++)
			modes_buffer[i]	= 0;

		//traverse raList accumulating modes and point counts
		//using canoncial element information...
		int k, iMPC;
		for(i = 0; i < regionCount; i++)
		{

			//obtain canonical element of region i
			iCanEl	= raList[i].label;

			//obtain mode point count of region i
			iMPC	= modePointCounts[i];

			//accumulate modes_buffer[iCanEl]
			for(k = 0; k < N; k++)
				modes_buffer[(N*iCanEl)+k] += iMPC*modes[(N*i)+k];

			//accumulate MPC_buffer[iCanEl]
			MPC_buffer[iCanEl] += iMPC;

		}

		// (b)

		// Re-label new regions of the image using the canonical
		// element information generated by step (2)

		// Also use this information to compute the modes of the newly
		// defined regions, and to assign new region point counts in
		// a consecute manner to the modePointCounts array

		//allocate memory for label buffer
		int	label_buffer[]	= new int [regionCount];

		//initialize label buffer to -1
		for(i = 0; i < regionCount; i++)
			label_buffer[i]	= -1;

		//traverse raList re-labeling the regions
		int	label = -1;
		for(i = 0; i < regionCount; i++)
		{
			//obtain canonical element of region i
			iCanEl	= raList[i].label;
			if(label_buffer[iCanEl] < 0)
			{
				//assign a label to the new region indicated by canonical
				//element of i
				label_buffer[iCanEl]	= ++label;

				//recompute mode storing the result in modes[label]...
				iMPC	= MPC_buffer[iCanEl];
				for(k = 0; k < N; k++)
					modes[(N*label)+k]	= (modes_buffer[(N*iCanEl)+k])/(iMPC);

				//assign a corresponding mode point count for this region into
				//the mode point counts array using the MPC buffer...
				modePointCounts[label]	= MPC_buffer[iCanEl];
			}
		}

		//re-assign region count using label counter
		regionCount	= label+1;

		// (c)

		// Use the label buffer to reconstruct the label map, which specified
		// the new image given its new regions calculated above

		for(i = 0; i < L; i++)
			labels[i]	= label_buffer[raList[labels[i]].label];

		//de-allocate memory
		modes_buffer = null;
		MPC_buffer = null;
		label_buffer = null;

		//done.
		return;

	}
	
	/*******************************************************/
	/*in Window                                            */
	/*******************************************************/
	/*Returns true if the two specified data points are    */
	/*within rR of each other.                             */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - mode1 and mode2 are indeces into msRawData   */
	/*        specifying the modes of the pixels having    */
	/*        these indeces.                               */
	/*Post:                                                */
	/*      - true is returned if mode1 and mode2 are wi-  */
	/*        thin rR of one another, false is returned    */
	/*        otherwise.                                   */
	/*******************************************************/

	private boolean inWindow(int mode1, int mode2)
	{
		int		k		= 1, s	= 0, p;
		double	diff	= 0, el;
		while((diff < 0.25)&&(k != kp)) // Partial Distortion Search
		{
			//Calculate distance squared of sub-space s	
			diff = 0;
			for(p = 0; p < P[k]; p++)
			{
				el    = (modes[mode1*N+p+s]-modes[mode2*N+p+s])/(h[k]*offset[k]);
				if((p == 0)&&(k == 1)&&(modes[mode1*N] > 80))
					diff += 4*el*el;
				else
					diff += el*el;
			}
			
			//next subspace
			s += P[k];
			k++;
		}
		return (diff < 0.25);
	}

	
	/*******************************************************/
	/*Compute Edge Strengths                               */
	/*******************************************************/
	/*Computes the a weight for each link in the region    */
	/*graph maintined by the RAM, resulting in a weighted  */
	/*graph in which the weights consist of a confidence   */
	/*between zero and one indicating if the regions are   */
	/*separated by a strong or weak edge.                  */
	/*******************************************************/
	/*Post:                                                */
	/*      - an edge strength has been computed between   */
	/*        each region of the image and placed as a     */
	/*        weight in the RAM to be used during transi-  */
	/*        tive closure.                                */
	/*******************************************************/

	private void computeEdgeStrengths()
	{
		//traverse labeled image computing edge strengths
		//(excluding image boundary)...
		int    x, y, dp, curLabel, rightLabel, bottomLabel;
		RAList curRegion[] = null;
		for(y = 1; y < height-1; y++)
		{
			for(x = 1; x < width-1; x++)
			{
				//compute data point location using x and y
				dp = y*width + x;

				//obtain labels at different pixel locations
				curLabel	= labels[dp      ];	//current pixel
				rightLabel	= labels[dp+1    ];	//right   pixel
				bottomLabel	= labels[dp+width];	//bottom  pixel

				//check right and bottom neighbor to see if there is a
				//change in label then we are at an edge therefore record
				//the edge strength at this edge accumulating its value
				//in the RAM...
				if(curLabel != rightLabel)
				{
					//traverse into RAM...
					curRegion[0] = raList[curLabel];
					while((curRegion != null)&&(curRegion[0].label != rightLabel))
						curRegion = curRegion[0].next;

					//this should not occur...
					assert(curRegion != null);

					//accumulate edge strength
					curRegion[0].edgeStrength   += weightMap[dp] + weightMap[dp+1];
					curRegion[0].edgePixelCount += 2;
				}

				if(curLabel != bottomLabel)
				{
					//traverse into RAM...
					curRegion[0] = raList[curLabel];
					while((curRegion != null)&&(curRegion[0].label != bottomLabel))
						curRegion = curRegion[0].next;

					//this should not occur...
					assert(curRegion != null);

					//accumulate edge strength
					if(curLabel == rightLabel)
					{
						curRegion[0].edgeStrength   += weightMap[dp] + weightMap[dp+width];
						curRegion[0].edgePixelCount += 2;
					} 
					else
					{
						curRegion[0].edgeStrength	  += weightMap[dp+width];
						curRegion[0].edgePixelCount += 1;
					}

				}
			}
		}

		//compute strengths using accumulated strengths obtained above...
		RAList neighborRegion[] = null;
		float	edgeStrength;
		int		edgePixelCount;
		for(x = 0; x < regionCount; x++)
		{
			//traverse the region list of the current region
			curRegion[0]	= raList[x];
			curRegion	= curRegion[0].next;
			while(curRegion != null)
			{
				//with the assumption that regions having a smaller
				//label in the current region list have already
				//had their edge strengths computed, only compute
				//edge strengths for the regions whose label is greater
				//than x, the current region (region list) under
				//consideration...
				curLabel = curRegion[0].label;
				if(curLabel > x)
				{
					//obtain pointer to the element identifying the
					//current region in the neighbors region list...
					neighborRegion[0] = raList[curLabel];
					while((neighborRegion != null)&&(neighborRegion[0].label != x))
						neighborRegion = neighborRegion[0].next;
					
					//this should not occur...
					assert(neighborRegion != null);
					
					//compute edge strengths using accumulated confidence
					//value and pixel count
					if((edgePixelCount = curRegion[0].edgePixelCount + neighborRegion[0].edgePixelCount) != 0)
					{
						//compute edge strength
						edgeStrength	= curRegion[0].edgeStrength + neighborRegion[0].edgeStrength;
						edgeStrength	/= edgePixelCount;
						
						//store edge strength and pixel count for corresponding regions
						curRegion[0].edgeStrength		= neighborRegion[0].edgeStrength		= edgeStrength;
						curRegion[0].edgePixelCount	= neighborRegion[0].edgePixelCount	= edgePixelCount;
					}
				}

				//traverse to the next region in the region adjacency list
				//of the current region x
				curRegion = curRegion[0].next;

			}
		}

		//compute average edge strength amongst the edges connecting
		//it to each of its neighbors
		int numNeighbors;
		for(x = 0; x < regionCount; x++)
		{
			//traverse the region list of the current region
			//accumulating weights
			curRegion[0]		= raList[x];
			curRegion		= curRegion[0].next;
			edgeStrength	= 0;
			numNeighbors	= 0;
			while(curRegion != null)
			{
				numNeighbors++;
				edgeStrength   += curRegion[0].edgeStrength;
				curRegion		= curRegion[0].next;
			}

			//divide by the number of regions connected
			//to the current region
			if(numNeighbors > 0) edgeStrength /= numNeighbors;

			//store the result in the raList for region
			//x
			raList[x].edgeStrength = edgeStrength;
		}

		//traverse raList and output the resulting list
		//to a file

		//done.
		return;

	}
	

/*******************************************************/
/*Build Region Adjacency Matrix                        */
/*******************************************************/
/*Constructs a region adjacency matrix.                */
/*******************************************************/
/*Pre:                                                 */
/*      - the classification data structure has been   */
/*        constructed.                                 */
/*Post:                                                */
/*      - a region adjacency matrix has been built     */
/*        using the classification data structure.     */
/*******************************************************/

private void buildRAM()
{
     int i;
	// Allocate memory for region adjacency matrix if it hasn't already been allocated
	raList = new RAList[regionCount];
	for (i = 0; i < regionCount; i++) {
		raList[i] = new RAList();
	}
	raPool = new RAList[NODE_MULTIPLE * regionCount];
	for (i = 0; i < NODE_MULTIPLE * regionCount; i++) {
		raPool[i] = new RAList();
	}

	//initialize the region adjacency list
	for(i = 0; i < regionCount; i++)
	{
		raList[i].edgeStrength		= 0;
		raList[i].edgePixelCount	= 0;
		raList[i].label				= i;
		raList[i].next				= null;
	}

	//initialize RAM free list
	freeRAList	= raPool;
	for(i = 0; i < NODE_MULTIPLE*regionCount-1; i++)
	{
		raPool[i].edgeStrength		= 0;
		raPool[i].edgePixelCount	= 0;
		raPool[i].next = new RAList[1];
		raPool[i].next[0] = raPool[i+1];
	}
	raPool[NODE_MULTIPLE*regionCount-1].next	= null;

	//traverse the labeled image building
	//the RAM by looking to the right of
	//and below the current pixel location thus
	//determining if a given region is adjacent
	//to another
	int		j, curLabel, rightLabel, bottomLabel, exists;
	RAList	raNode1[], raNode2[], oldRAFreeList[];
	for(i = 0; i < height - 1; i++)
	{
		//check the right and below neighbors
		//for pixel locations whose x < width - 1
		for(j = 0; j < width - 1; j++)
		{
			//calculate pixel labels
			curLabel	= labels[i*width+j    ];	//current pixel
			rightLabel	= labels[i*width+j+1  ];	//right   pixel
			bottomLabel	= labels[(i+1)*width+j];	//bottom  pixel

			//check to the right, if the label of
			//the right pixel is not the same as that
			//of the current one then region[j] and region[j+1]
			//are adjacent to one another - update the RAM
			if(curLabel != rightLabel)
			{
				//obtain RAList object from region adjacency free
				//list
				raNode1			= freeRAList;
				raNode2			= freeRAList[0].next;

				//keep a pointer to the old region adj. free
				//list just in case nodes already exist in respective
				//region lists
				oldRAFreeList	= freeRAList;

				//update region adjacency free list
				freeRAList		= freeRAList[0].next[0].next;

				//populate RAList nodes
				raNode1[0].label	= curLabel;
				raNode2[0].label	= rightLabel;

				//insert nodes into the RAM
				exists			= 0;
				insert(raList[curLabel], raNode2);
				exists			= insert(raList[rightLabel],raNode1);

				//if the node already exists then place
				//nodes back onto the region adjacency
				//free list
				if(exists > 0)
					freeRAList = oldRAFreeList;

			}

			//check below, if the label of
			//the bottom pixel is not the same as that
			//of the current one then region[j] and region[j+width]
			//are adjacent to one another - update the RAM
			if(curLabel != bottomLabel)
			{
				//obtain RAList object from region adjacency free
				//list
				raNode1			= freeRAList;
				raNode2			= freeRAList[0].next;

				//keep a pointer to the old region adj. free
				//list just in case nodes already exist in respective
				//region lists
				oldRAFreeList	= freeRAList;

				//update region adjacency free list
				freeRAList		= freeRAList[0].next[0].next;

				//populate RAList nodes
				raNode1[0].label	= curLabel;
				raNode2[0].label	= bottomLabel;

				//insert nodes into the RAM
				exists			= 0;
				insert(raList[curLabel],raNode2);
				exists			= insert(raList[bottomLabel],raNode1);

				//if the node already exists then place
				//nodes back onto the region adjacency
				//free list
				if(exists > 0)
					freeRAList = oldRAFreeList;

			}

		}

		//check only to the bottom neighbors of the right boundary
		//pixels...

		//calculate pixel locations (j = width-1)
		curLabel	= labels[i*width+j    ];	//current pixel
		bottomLabel = labels[(i+1)*width+j];	//bottom  pixel

		//check below, if the label of
		//the bottom pixel is not the same as that
		//of the current one then region[j] and region[j+width]
		//are adjacent to one another - update the RAM
		if(curLabel != bottomLabel)
		{
			//obtain RAList object from region adjacency free
			//list
			raNode1			= freeRAList;
			raNode2			= freeRAList[0].next;
			
			//keep a pointer to the old region adj. free
			//list just in case nodes already exist in respective
			//region lists
			oldRAFreeList	= freeRAList;
			
			//update region adjacency free list
			freeRAList		= freeRAList[0].next[0].next;
			
			//populate RAList nodes
			raNode1[0].label	= curLabel;
			raNode2[0].label	= bottomLabel;
			
			//insert nodes into the RAM
			exists			= 0;
			insert(raList[curLabel],raNode2);
			exists			= insert(raList[bottomLabel],raNode1);
			
			//if the node already exists then place
			//nodes back onto the region adjacency
			//free list
			if(exists > 0)
				freeRAList = oldRAFreeList;

		}
	}

	//check only to the right neighbors of the bottom boundary
	//pixels...

	//check the right for pixel locations whose x < width - 1
	for(j = 0; j < width - 1; j++)
	{
		//calculate pixel labels (i = height-1)
		curLabel	= labels[i*width+j    ];	//current pixel
		rightLabel	= labels[i*width+j+1  ];	//right   pixel
		
		//check to the right, if the label of
		//the right pixel is not the same as that
		//of the current one then region[j] and region[j+1]
		//are adjacent to one another - update the RAM
		if(curLabel != rightLabel)
		{
			//obtain RAList object from region adjacency free
			//list
			raNode1			= freeRAList;
			raNode2			= freeRAList[0].next;

			//keep a pointer to the old region adj. free
			//list just in case nodes already exist in respective
			//region lists
			oldRAFreeList	= freeRAList;
			
			//update region adjacency free list
			freeRAList		= freeRAList[0].next[0].next;
			
			//populate RAList nodes
			raNode1[0].label	= curLabel;
			raNode2[0].label	= rightLabel;
			
			//insert nodes into the RAM
			exists			= 0;
			insert(raList[curLabel],raNode2);
			exists			= insert(raList[rightLabel],raNode1);
			
			//if the node already exists then place
			//nodes back onto the region adjacency
			//free list
			if(exists > 0)
				freeRAList = oldRAFreeList;

		}

	}

	//done.
	return;

}

/*******************************************************/
/*Insert                                               */
/*******************************************************/
/*Insert a region node into the region adjacency list. */
/*******************************************************/
/*Pre:                                                 */
/*      - entry is a node representing a connected re- */
/*        gion                                         */
/*Post:                                                */
/*      - entry has been inserted into the region adj- */
/*        acency list if it does not already exist     */
/*        there.                                       */
/*      - if the entry already exists in the region    */
/*        adjacency list 1 is returned otherwise 0 is  */
/*        returned.                                    */
/*******************************************************/

private int insert(RAList source, RAList entry[])
{

	//if the list contains only one element
	//then insert this element into next
	if(source.next == null)
	{
		//insert entry
		source.next		= entry;
		entry[0].next = null;

		//done
		return 0;
	}

	//traverse the list until either:

	//(a) entry's label already exists - do nothing
	//(b) the list ends or the current label is
	//    greater than entry's label, thus insert the entry
	//    at this location

	//check first entry
	if(source.next[0].label > entry[0].label)
	{
		//insert entry into the list at this location
		entry[0].next	= source.next;
		source.next		= entry;

		//done
		return 0;
	}

	//check the rest of the list...
	source.exists	= 0;
	source.cur		= source.next;
	while (source.cur != null)
	{
		if(entry[0].label == source.cur[0].label)
		{
			//node already exists
			source.exists = 1;
			break;
		}
		else if(((source.cur[0].next == null))||(source.cur[0].next[0].label > entry[0].label))
		{
			//insert entry into the list at this location
			entry[0].next	= source.cur[0].next;
			source.cur[0].next	= entry;
			break;
		}

		//traverse the region adjacency list
		source.cur = source.cur[0].next;
	}

	//done. Return exists indicating whether or not a new node was
	//      actually inserted into the region adjacency list.
	return (int)(source.exists);

}




	
	/*******************************************************/
	/*Filter                                               */
	/*******************************************************/
	/*Performs mean shift filtering on the specified input */
	/*image using a user defined kernel.                   */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - the user defined kernel used to apply mean   */
	/*        shift filtering to the defined input image   */
	/*        has spatial bandwidth sigmaS and range band- */
	/*        width sigmaR                                 */
	/*      - speedUpLevel determines whether or not the   */
	/*        filtering should be optimized for faster     */
	/*        execution: a value of NO_SPEEDUP turns this  */
	/*        optimization off and a value SPEEDUP turns   */
	/*        this optimization on                         */
	/*      - a data set has been defined                  */
	/*      - the height and width of the lattice has been */
	/*        specified using method DefineLattice()       */
	/*Post:                                                */
	/*      - mean shift filtering has been applied to the */
	/*        input image using a user defined kernel      */
	/*      - the filtered image is stored in the private  */
	/*        data members of the msImageProcessor class.  */
	/*******************************************************/

	private void filter()
	{
		long time1 = 0;
		long time2;
		long time3 = 0;
		long time4;
		long elapsedTime;

		//Check Class consistency...

		//check:
		// (1) if this operation is consistent
		// (2) if kernel was created
		// (3) if data set is defined
		// (4) if the dimension of the kernel agrees with that
		//     of the defined data set
		// if not ... flag an error!
		classConsistencyCheck(N+2, true);
		
		//If the image has just been read then allocate memory
		//for and initialize output data structure used to store
		//image modes and their corresponding regions...
		if(class_state.OUTPUT_DEFINED == false)
		{
			initializeOutput();

		}

		//****************** Allocate Memory ******************

		//Allocate memory for basin of attraction mode structure...
		modeTable = new byte[L];
		pointList = new int[L];
		
		//start timer
	    if (measureTime) {
		    time1 = System.currentTimeMillis();
	    }

		//*****************************************************

		//filter image according to speedup level...
		switch(speedUpLevel)
		{
		//no speedup...
		case NO_SPEEDUP:	
	      //NonOptimizedFilter((float)(spatialBandwidth), rangeBandwidth);	break;
	      newNonOptimizedFilter(spatialBandwidth, rangeBandwidth);	
		  break;
		//medium speedup
		case MED_SPEEDUP:	
	      //OptimizedFilter1((float)(spatialBandwidth), rangeBandwidth);		break;
	      newOptimizedFilter1(spatialBandwidth, rangeBandwidth);		
		  break;
		//high speedup
		case HIGH_SPEEDUP: 
	      //OptimizedFilter2((float)(spatialBandwidth), rangeBandwidth);		break;
	      newOptimizedFilter2(spatialBandwidth, rangeBandwidth);		
		  break;
	   // new speedup
		}

		//****************** Deallocate Memory ******************

		//de-allocate memory used by basin of attraction mode structure
		modeTable = null;
		pointList = null;

		//re-initialize structure
		pointCount	= 0;

		//*******************************************************

		//Label image regions, also if segmentation is not to be
		//performed use the resulting classification structure to
		//calculate the image boundaries...

	  
	   int i;
	   for (i=0; i<L*N; i++)
	   {
	      LUV_data[i] = msRawData[i];
	   }


	if (measureTime) {
		time2 = System.currentTimeMillis();
		elapsedTime = time2 - time1;
		Preferences.debug("Connecting regions time in msec = " + elapsedTime + "\n");
		time3 = System.currentTimeMillis();
	}
		
		//Perform connecting (label image regions) using LUV_data
		connect();
		
	if (measureTime) {
        time4 = System.currentTimeMillis();
        elapsedTime = time4 - time3;
		Preferences.debug("done. (" + elapsedTime +  " milliseconds, numRegions = " + regionCount+ "\n",
		Preferences.DEBUG_ALGORITHM);
	}

		//done.
		return;

	}
	
	/*******************************************************/
	/*connect                                              */
	/*******************************************************/
	/*Classifies the regions of the mean shift filtered    */
	/*image.                                               */
	/*******************************************************/
	/*Post:                                                */
	/*      - the regions of the mean shift image have been*/
	/*        classified using the private classification  */
	/*        structure of the msImageProcessor Class.     */
	/*        Namely, each region uniquely identified by   */
	/*        its LUV color  (stored by LUV_data) and loc- */
	/*        ation has been labeled and its area computed */
	/*        via an eight-connected fill.                 */
	/*******************************************************/

	private void connect()
	{

		//define eight connected neighbors
		neigh[0]	= 1;
		neigh[1]	= 1-width;
		neigh[2]	= -width;
		neigh[3]	= -(1+width);
		neigh[4]	= -1;
		neigh[5]	= width-1;
		neigh[6]	= width;
		neigh[7]	= width+1;

		//initialize labels and modePointCounts
		int i;
		for(i = 0; i < L; i++)
		{
			labels[i]			= -1;
			modePointCounts[i]	=  0;
		}

		//Traverse the image labeling each new region encountered
		int k, label = -1;
		for(i = 0; i < L; i++)
		{
			//if this region has not yet been labeled - label it
			if(labels[i] < 0)
			{
				//assign new label to this region
				labels[i] = ++label;

				//copy region color into modes
				for(k = 0; k < N; k++)
	            modes[(N*label)+k] = LUV_data[(N*i)+k];
//					modes[(N*label)+k]	= (float)(LUV_data[(N*i)+k]);

				//populate labels with label for this specified region
				//calculating modePointCounts[label]...
				fill(i, label);
			}
		}

		//calculate region count using label
		regionCount	= label+1;

		//done.
		return;
	}
	
	/*******************************************************/
	/*Fill                                                 */
	/*******************************************************/
	/*Given a region seed and a region label, Fill uses    */
	/*the region seed to perform an eight-connected fill   */
	/*for the specified region, labeling all pixels con-   */
	/*tained by the region with the specified label:       */
	/*label.                                               */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - regionLoc is a region seed - a pixel that is */
	/*        identified as being part of the region       */
	/*        labled using the label, label.               */
	/*Post:                                                */
	/*      - all pixels belonging to the region specified */
	/*        by regionLoc (having the same integer LUV    */
	/*        value specified by LUV_data) are classified  */
	/*        as one region by labeling each pixel in the  */
	/*        image clasification structure using label    */
	/*        via an eight-connected fill.                 */
	/*******************************************************/

	private void fill(int regionLoc, int label)
	{

		//declare variables
		int	i, k, neighLoc, neighborsFound, imageSize	= L;

		//Fill region starting at region location
		//using labels...

		//initialzie indexTable
		int	index		= 0;
		indexTable[0]	= regionLoc;

		//increment mode point counts for this region to
		//indicate that one pixel belongs to this region
		modePointCounts[label]++;

		while(true)
		{

			//assume no neighbors will be found
			neighborsFound	= 0;

			//check the eight connected neighbors at regionLoc -
			//if a pixel has similar color to that located at 
			//regionLoc then declare it as part of this region
			for(i = 0; i < 8; i++)
			{
	         // no need
	         /*
				//if at boundary do not check certain neighbors because
				//they do not exist...
				if((regionLoc%width == 0)&&((i == 3)||(i == 4)||(i == 5)))
					continue;
				if((regionLoc%(width-1) == 0)&&((i == 0)||(i == 1)||(i == 7)))
					continue;
	         */   

				//check bounds and if neighbor has been already labeled
				neighLoc			= regionLoc + neigh[i];
				if((neighLoc >= 0)&&(neighLoc < imageSize)&&(labels[neighLoc] < 0))
				{
					for(k = 0; k < N; k++)
					{
//						if(LUV_data[(regionLoc*N)+k] != LUV_data[(neighLoc*N)+k])
	               if (Math.abs(LUV_data[(regionLoc*N)+k]-LUV_data[(neighLoc*N)+k])>=LUV_threshold)
							break;
					}
					
					//neighbor i belongs to this region so label it and
					//place it onto the index table buffer for further
					//processing
					if(k == N)
					{
						//assign label to neighbor i
						labels[neighLoc]	= label;
						
						//increment region point count
						modePointCounts[label]++;
						
						//place index of neighbor i onto the index tabel buffer
						indexTable[++index]	= neighLoc;
						
						//indicate that a neighboring region pixel was
						//identified
						neighborsFound	= 1;
					}
				}
			}

			//check the indexTable to see if there are any more
			//entries to be explored - if so explore them, otherwise
			//exit the loop - we are finished
			if(neighborsFound > 0)
				regionLoc	= indexTable[index];
			else if (index > 1)
				regionLoc	= indexTable[--index];
			else
				break; //fill complete
		}

		//done.
		return;

	}

	
	private void newNonOptimizedFilter(float sigmaS, float sigmaR)
	{

		// Declare Variables
		int   iterationCount, i, j, k;
		double mvAbs, diff, el;

		//re-assign bandwidths to sigmaS and sigmaR
		if(((h[0] = sigmaS) <= 0)||((h[1] = sigmaR) <= 0))
		{
			MipavUtil.displayError("msImageProcessor Segment sigmaS and/or sigmaR is zero or negative.");
			return;
		}
		
		//define input data dimension with lattice
		int lN	= N + 2;
		
		// Traverse each data point applying mean shift
		// to each data point
		
		// Allcocate memory for yk
		double	yk[]		= new double [lN];
		
		// Allocate memory for Mh
		double	Mh[]		= new double [lN];

	   // let's use some temporary data
	   double sdata[];
	   sdata = new double[lN*L];

	   // copy the scaled data
	   int idxs, idxd;
	   idxs = idxd = 0;
	   if (N==3)
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   } else if (N==1)
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   } else
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         // sdata[idxs++] = (i%width)/sigmaS; This is a mistake in the source code
	         sdata[idxs++] = (i/width)/sigmaS;
	         for (j=0; j<N; j++)
	            sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   }
	   // index the data in the 3d buckets (x, y, L)
	   int buckets[];
	   int slist[];
	   slist = new int[L];
	   int bucNeigh[] = new int[27];

	   double sMins; // just for L
	   double sMaxs[] = new double[3]; // for all
	   sMaxs[0] = width/sigmaS;
	   sMaxs[1] = height/sigmaS;
	   sMins = sMaxs[2] = sdata[2];
	   idxs = 2;
	   double cval;
	   for(i=0; i<L; i++)
	   {
	      cval = sdata[idxs];
	      if (cval < sMins)
	         sMins = cval;
	      else if (cval > sMaxs[2])
	         sMaxs[2] = cval;

	      idxs += lN;
	   }

	   int nBuck1, nBuck2, nBuck3;
	   int cBuck1, cBuck2, cBuck3, cBuck;
	   nBuck1 = (int) (sMaxs[0] + 3);
	   nBuck2 = (int) (sMaxs[1] + 3);
	   nBuck3 = (int) (sMaxs[2] - sMins + 3);
	   buckets = new int[nBuck1*nBuck2*nBuck3];
	   for(i=0; i<(nBuck1*nBuck2*nBuck3); i++)
	      buckets[i] = -1;

	   idxs = 0;
	   for(i=0; i<L; i++)
	   {
	      // find bucket for current data and add it to the list
	      cBuck1 = (int) sdata[idxs] + 1;
	      cBuck2 = (int) sdata[idxs+1] + 1;
	      cBuck3 = (int) (sdata[idxs+2] - sMins) + 1;
	      cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);

	      slist[i] = buckets[cBuck];
	      buckets[cBuck] = i;

	      idxs += lN;
	   }
	   // init bucNeigh
	   idxd = 0;
	   for (cBuck1=-1; cBuck1<=1; cBuck1++)
	   {
	      for (cBuck2=-1; cBuck2<=1; cBuck2++)
	      {
	         for (cBuck3=-1; cBuck3<=1; cBuck3++)
	         {
	            bucNeigh[idxd++] = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	         }
	      }
	   }
	   double wsuml, weight;
	   double hiLTr = 80.0/sigmaR;
	   // done indexing/hashing
		
		// proceed ...
	   Preferences.debug("done.\nApplying mean shift (Using Lattice)... \n", Preferences.DEBUG_ALGORITHM);

		for(i = 0; i < L; i++)
		{

			// Assign window center (window centers are
			// initialized by createLattice to be the point
			// data[i])
	      idxs = i*lN;
	      for (j=0; j<lN; j++)
	         yk[j] = sdata[idxs+j];
			
			// Calculate the mean shift vector using the lattice
			// LatticeMSVector(Mh, yk);
	      /*****************************************************/
	   	// Initialize mean shift vector
		   for(j = 0; j < lN; j++)
	   		Mh[j] = 0;
	   	wsuml = 0;
	      // uniformLSearch(Mh, yk_ptr); // modify to new
	      // find bucket of yk
	      cBuck1 = (int) yk[0] + 1;
	      cBuck2 = (int) yk[1] + 1;
	      cBuck3 = (int) (yk[2] - sMins) + 1;
	      cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	      for (j=0; j<27; j++)
	      {
	         idxd = buckets[cBuck+bucNeigh[j]];
	         // list parse, crt point is cHeadList
	         while (idxd>=0)
	         {
	            idxs = lN*idxd;
	            // determine if inside search window
	            el = sdata[idxs+0]-yk[0];
	            diff = el*el;
	            el = sdata[idxs+1]-yk[1];
	            diff += el*el;

	            if (diff < 1.0)
	            {
	               el = sdata[idxs+2]-yk[2];
	               if (yk[2] > hiLTr)
	                  diff = 4*el*el;
	               else
	                  diff = el*el;

	               if (N>1)
	               {
	                  el = sdata[idxs+3]-yk[3];
	                  diff += el*el;
	                  el = sdata[idxs+4]-yk[4];
	                  diff += el*el;
	               }

	               if (diff < 1.0)
	               {
	                  weight = 1-weightMap[idxd];
	                  for (k=0; k<lN; k++)
	                     Mh[k] += weight*sdata[idxs+k];
	                  wsuml += weight;
	               }
	            }
	            idxd = slist[idxd];
	         }
	      }
	   	if (wsuml > 0)
	   	{
			   for(j = 0; j < lN; j++)
	   			Mh[j] = Mh[j]/wsuml - yk[j];
	   	}
	   	else
	   	{
			   for(j = 0; j < lN; j++)
	   			Mh[j] = 0;
	   	}
	      /*****************************************************/
			
			// Calculate its magnitude squared
			mvAbs = 0;
			for(j = 0; j < lN; j++)
				mvAbs += Mh[j]*Mh[j];
			
			// Keep shifting window center until the magnitude squared of the
			// mean shift vector calculated at the window center location is
			// under a specified threshold (Epsilon)
			
			// NOTE: iteration count is for speed up purposes only - it
			//       does not have any theoretical importance
			iterationCount = 1;
			while((mvAbs >= EPSILON)&&(iterationCount < LIMIT))
			{
				
				// Shift window location
				for(j = 0; j < lN; j++)
					yk[j] += Mh[j];
				
				// Calculate the mean shift vector at the new
				// window location using lattice
				// LatticeMSVector(Mh, yk);
	         /*****************************************************/
	         // Initialize mean shift vector
	         for(j = 0; j < lN; j++)
	            Mh[j] = 0;
	         wsuml = 0;
	         // uniformLSearch(Mh, yk_ptr); // modify to new
	         // find bucket of yk
	         cBuck1 = (int) yk[0] + 1;
	         cBuck2 = (int) yk[1] + 1;
	         cBuck3 = (int) (yk[2] - sMins) + 1;
	         cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	         for (j=0; j<27; j++)
	         {
	            idxd = buckets[cBuck+bucNeigh[j]];
	            // list parse, crt point is cHeadList
	            while (idxd>=0)
	            {
	               idxs = lN*idxd;
	               // determine if inside search window
	               el = sdata[idxs+0]-yk[0];
	               diff = el*el;
	               el = sdata[idxs+1]-yk[1];
	               diff += el*el;
	               
	               if (diff < 1.0)
	               {
	                  el = sdata[idxs+2]-yk[2];
	                  if (yk[2] > hiLTr)
	                     diff = 4*el*el;
	                  else
	                     diff = el*el;
	                  
	                  if (N>1)
	                  {
	                     el = sdata[idxs+3]-yk[3];
	                     diff += el*el;
	                     el = sdata[idxs+4]-yk[4];
	                     diff += el*el;
	                  }
	                  
	                  if (diff < 1.0)
	                  {
	                     weight = 1-weightMap[idxd];
	                     for (k=0; k<lN; k++)
	                        Mh[k] += weight*sdata[idxs+k];
	                     wsuml += weight;
	                  }
	               }
	               idxd = slist[idxd];
	            }
	         }
	         if (wsuml > 0)
	         {
	            for(j = 0; j < lN; j++)
	               Mh[j] = Mh[j]/wsuml - yk[j];
	         }
	         else
	         {
	            for(j = 0; j < lN; j++)
	               Mh[j] = 0;
	         }
	         /*****************************************************/
				
				// Calculate its magnitude squared
				//mvAbs = 0;
				//for(j = 0; j < lN; j++)
				//	mvAbs += Mh[j]*Mh[j];
	         mvAbs = (Mh[0]*Mh[0]+Mh[1]*Mh[1])*sigmaS*sigmaS;
	         if (N==3)
	            mvAbs += (Mh[2]*Mh[2]+Mh[3]*Mh[3]+Mh[4]*Mh[4])*sigmaR*sigmaR;
	         else
	            mvAbs += Mh[2]*Mh[2]*sigmaR*sigmaR;

				// Increment interation count
				iterationCount++;
			}

			// Shift window location
			for(j = 0; j < lN; j++)
				yk[j] += Mh[j];
			
			//store result into msRawData...
			for(j = 0; j < N; j++)
				msRawData[N*i+j] = (float)(yk[j+2]*sigmaR);

			// Prompt user on progress
			float percent_complete = (float)(i/(float)(L))*100;
		    fireProgressStateChanged((int)(percent_complete + 0.5));
		}
		
		// de-allocate memory
	   buckets = null;
	   slist = null;
	   sdata = null;

	   yk = null;
	   Mh = null;

		// done.
		return;

	}
	
	private void newOptimizedFilter1(float sigmaS, float sigmaR)
	{
		// Declare Variables
		int		iterationCount, i, j, k, modeCandidateX, modeCandidateY, modeCandidate_i;
		double	mvAbs, diff, el;

		//re-assign bandwidths to sigmaS and sigmaR
		if(((h[0] = sigmaS) <= 0)||((h[1] = sigmaR) <= 0))
		{
			MipavUtil.displayError("msImageProcessor Segment sigmaS and/or sigmaR is zero or negative.");
			return;
		}
		
		//define input data dimension with lattice
		int lN	= N + 2;
		
		// Traverse each data point applying mean shift
		// to each data point
		
		// Allcocate memory for yk
		double	yk[]		= new double [lN];
		
		// Allocate memory for Mh
		double	Mh[]		= new double [lN];

	   // let's use some temporary data
	   double sdata[];
	   sdata = new double[lN*L];

	   // copy the scaled data
	   int idxs, idxd;
	   idxs = idxd = 0;
	   if (N==3)
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   } else if (N==1)
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   } else
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         for (j=0; j<N; j++)
	            sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   }
	   // index the data in the 3d buckets (x, y, L)
	   int buckets[];
	   int slist[];
	   slist = new int[L];
	   int bucNeigh[] = new int[27];

	   double sMins; // just for L
	   double sMaxs[] = new double[3]; // for all
	   sMaxs[0] = width/sigmaS;
	   sMaxs[1] = height/sigmaS;
	   sMins = sMaxs[2] = sdata[2];
	   idxs = 2;
	   double cval;
	   for(i=0; i<L; i++)
	   {
	      cval = sdata[idxs];
	      if (cval < sMins)
	         sMins = cval;
	      else if (cval > sMaxs[2])
	         sMaxs[2] = cval;

	      idxs += lN;
	   }

	   int nBuck1, nBuck2, nBuck3;
	   int cBuck1, cBuck2, cBuck3, cBuck;
	   nBuck1 = (int) (sMaxs[0] + 3);
	   nBuck2 = (int) (sMaxs[1] + 3);
	   nBuck3 = (int) (sMaxs[2] - sMins + 3);
	   buckets = new int[nBuck1*nBuck2*nBuck3];
	   for(i=0; i<(nBuck1*nBuck2*nBuck3); i++)
	      buckets[i] = -1;

	   idxs = 0;
	   for(i=0; i<L; i++)
	   {
	      // find bucket for current data and add it to the list
	      cBuck1 = (int) sdata[idxs] + 1;
	      cBuck2 = (int) sdata[idxs+1] + 1;
	      cBuck3 = (int) (sdata[idxs+2] - sMins) + 1;
	      cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);

	      slist[i] = buckets[cBuck];
	      buckets[cBuck] = i;

	      idxs += lN;
	   }
	   // init bucNeigh
	   idxd = 0;
	   for (cBuck1=-1; cBuck1<=1; cBuck1++)
	   {
	      for (cBuck2=-1; cBuck2<=1; cBuck2++)
	      {
	         for (cBuck3=-1; cBuck3<=1; cBuck3++)
	         {
	            bucNeigh[idxd++] = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	         }
	      }
	   }
	   double wsuml, weight;
	   double hiLTr = 80.0/sigmaR;
	   // done indexing/hashing

		
		// Initialize mode table used for basin of attraction
	   for (i = 0; i < L; i++) {
		   modeTable[i] = 0;
	   }
		
		// proceed 
	   Preferences.debug("done.\nApplying mean shift (Using Lattice) ... \n", Preferences.DEBUG_ALGORITHM);


		for(i = 0; i < L; i++)
		{
			// if a mode was already assigned to this data point
			// then skip this point, otherwise proceed to
			// find its mode by applying mean shift...
			if (modeTable[i] == 1)
				continue;

			// initialize point list...
			pointCount = 0;

			// Assign window center (window centers are
			// initialized by createLattice to be the point
			// data[i])
	      idxs = i*lN;
	      for (j=0; j<lN; j++)
	         yk[j] = sdata[idxs+j];
			
			// Calculate the mean shift vector using the lattice
			// LatticeMSVector(Mh, yk); // modify to new
	      /*****************************************************/
	   	// Initialize mean shift vector
		   for(j = 0; j < lN; j++)
	   		Mh[j] = 0;
	   	wsuml = 0;
	      // uniformLSearch(Mh, yk_ptr); // modify to new
	      // find bucket of yk
	      cBuck1 = (int) yk[0] + 1;
	      cBuck2 = (int) yk[1] + 1;
	      cBuck3 = (int) (yk[2] - sMins) + 1;
	      cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	      for (j=0; j<27; j++)
	      {
	         idxd = buckets[cBuck+bucNeigh[j]];
	         // list parse, crt point is cHeadList
	         while (idxd>=0)
	         {
	            idxs = lN*idxd;
	            // determine if inside search window
	            el = sdata[idxs+0]-yk[0];
	            diff = el*el;
	            el = sdata[idxs+1]-yk[1];
	            diff += el*el;

	            if (diff < 1.0)
	            {
	               el = sdata[idxs+2]-yk[2];
	               if (yk[2] > hiLTr)
	                  diff = 4*el*el;
	               else
	                  diff = el*el;

	               if (N>1)
	               {
	                  el = sdata[idxs+3]-yk[3];
	                  diff += el*el;
	                  el = sdata[idxs+4]-yk[4];
	                  diff += el*el;
	               }

	               if (diff < 1.0)
	               {
	                  weight = 1-weightMap[idxd];
	                  for (k=0; k<lN; k++)
	                     Mh[k] += weight*sdata[idxs+k];
	                  wsuml += weight;
	               }
	            }
	            idxd = slist[idxd];
	         }
	      }
	   	if (wsuml > 0)
	   	{
			   for(j = 0; j < lN; j++)
	   			Mh[j] = Mh[j]/wsuml - yk[j];
	   	}
	   	else
	   	{
			   for(j = 0; j < lN; j++)
	   			Mh[j] = 0;
	   	}
	      /*****************************************************/
	   	// Calculate its magnitude squared
			//mvAbs = 0;
			//for(j = 0; j < lN; j++)
			//	mvAbs += Mh[j]*Mh[j];
	      mvAbs = (Mh[0]*Mh[0]+Mh[1]*Mh[1])*sigmaS*sigmaS;
	      if (N==3)
	         mvAbs += (Mh[2]*Mh[2]+Mh[3]*Mh[3]+Mh[4]*Mh[4])*sigmaR*sigmaR;
	      else
	         mvAbs += Mh[2]*Mh[2]*sigmaR*sigmaR;

			
			// Keep shifting window center until the magnitude squared of the
			// mean shift vector calculated at the window center location is
			// under a specified threshold (Epsilon)
			
			// NOTE: iteration count is for speed up purposes only - it
			//       does not have any theoretical importance
			iterationCount = 1;
			while((mvAbs >= EPSILON)&&(iterationCount < LIMIT))
			{
				
				// Shift window location
				for(j = 0; j < lN; j++)
					yk[j] += Mh[j];
				
				// check to see if the current mode location is in the
				// basin of attraction...

				// calculate the location of yk on the lattice
				modeCandidateX	= (int) (sigmaS*yk[0]+0.5);
				modeCandidateY	= (int) (sigmaS*yk[1]+0.5);
				modeCandidate_i	= modeCandidateY*width + modeCandidateX;

				// if mvAbs != 0 (yk did indeed move) then check
				// location basin_i in the mode table to see if
				// this data point either:
				
				// (1) has not been associated with a mode yet
				//     (modeTable[basin_i] = 0), so associate
				//     it with this one
				//
				// (2) it has been associated with a mode other
				//     than the one that this data point is converging
				//     to (modeTable[basin_i] = 1), so assign to
				//     this data point the same mode as that of basin_i

				if ((modeTable[modeCandidate_i] != 2) && (modeCandidate_i != i))
				{
					// obtain the data point at basin_i to
					// see if it is within h*TC_DIST_FACTOR of
					// of yk
	            diff = 0;
	            idxs = lN*modeCandidate_i;
	            for (k=2; k<lN; k++)
	            {
	               el = sdata[idxs+k] - yk[k];
	               diff += el*el;
	            }

					// if the data point at basin_i is within
					// a distance of h*TC_DIST_FACTOR of yk
					// then depending on modeTable[basin_i] perform
					// either (1) or (2)
					if (diff < TC_DIST_FACTOR)
					{
						// if the data point at basin_i has not
						// been associated to a mode then associate
						// it with the mode that this one will converge
						// to
						if (modeTable[modeCandidate_i] == 0)
						{
							// no mode associated yet so associate
							// it with this one...
							pointList[pointCount++]		= modeCandidate_i;
							modeTable[modeCandidate_i]	= 2;

						} else
						{

							// the mode has already been associated with
							// another mode, thererfore associate this one
							// mode and the modes in the point list with
							// the mode associated with data[basin_i]...

							// store the mode info into yk using msRawData...
							for (j = 0; j < N; j++)
								yk[j+2] = msRawData[modeCandidate_i*N+j]/sigmaR;

							// update mode table for this data point
							// indicating that a mode has been associated
							// with it
							modeTable[i] = 1;

							// indicate that a mode has been associated
							// to this data point (data[i])
							mvAbs = -1;

							// stop mean shift calculation...
							break;
						}
					}
				}
				
	         // Calculate the mean shift vector at the new
	         // window location using lattice
	         // Calculate the mean shift vector using the lattice
	         // LatticeMSVector(Mh, yk); // modify to new
	         /*****************************************************/
	         // Initialize mean shift vector
	         for(j = 0; j < lN; j++)
	            Mh[j] = 0;
	         wsuml = 0;
	         // uniformLSearch(Mh, yk_ptr); // modify to new
	         // find bucket of yk
	         cBuck1 = (int) yk[0] + 1;
	         cBuck2 = (int) yk[1] + 1;
	         cBuck3 = (int) (yk[2] - sMins) + 1;
	         cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	         for (j=0; j<27; j++)
	         {
	            idxd = buckets[cBuck+bucNeigh[j]];
	            // list parse, crt point is cHeadList
	            while (idxd>=0)
	            {
	               idxs = lN*idxd;
	               // determine if inside search window
	               el = sdata[idxs+0]-yk[0];
	               diff = el*el;
	               el = sdata[idxs+1]-yk[1];
	               diff += el*el;
	               
	               if (diff < 1.0)
	               {
	                  el = sdata[idxs+2]-yk[2];
	                  if (yk[2] > hiLTr)
	                     diff = 4*el*el;
	                  else
	                     diff = el*el;
	                  
	                  if (N>1)
	                  {
	                     el = sdata[idxs+3]-yk[3];
	                     diff += el*el;
	                     el = sdata[idxs+4]-yk[4];
	                     diff += el*el;
	                  }
	                  
	                  if (diff < 1.0)
	                  {
	                     weight = 1-weightMap[idxd];
	                     for (k=0; k<lN; k++)
	                        Mh[k] += weight*sdata[idxs+k];
	                     wsuml += weight;
	                  }
	               }
	               idxd = slist[idxd];
	            }
	         }
	         if (wsuml > 0)
	         {
	            for(j = 0; j < lN; j++)
	               Mh[j] = Mh[j]/wsuml - yk[j];
	         }
	         else
	         {
	            for(j = 0; j < lN; j++)
	               Mh[j] = 0;
	         }
	         /*****************************************************/
				
				// Calculate its magnitude squared
				//mvAbs = 0;
				//for(j = 0; j < lN; j++)
				//	mvAbs += Mh[j]*Mh[j];
	         mvAbs = (Mh[0]*Mh[0]+Mh[1]*Mh[1])*sigmaS*sigmaS;
	         if (N==3)
	            mvAbs += (Mh[2]*Mh[2]+Mh[3]*Mh[3]+Mh[4]*Mh[4])*sigmaR*sigmaR;
	         else
	            mvAbs += Mh[2]*Mh[2]*sigmaR*sigmaR;

				// Increment iteration count
				iterationCount++;
				
			}

			// if a mode was not associated with this data point
			// yet associate it with yk...
			if (mvAbs >= 0)
			{
				// Shift window location
				for(j = 0; j < lN; j++)
					yk[j] += Mh[j];
				
				// update mode table for this data point
				// indicating that a mode has been associated
				// with it
				modeTable[i] = 1;

			}
			
	      for (k=0; k<N; k++)
	         yk[k+2] *= sigmaR;

			// associate the data point indexed by
			// the point list with the mode stored
			// by yk
			for (j = 0; j < pointCount; j++)
			{
				// obtain the point location from the
				// point list
				modeCandidate_i = pointList[j];

				// update the mode table for this point
				modeTable[modeCandidate_i] = 1;

				//store result into msRawData...
				for(k = 0; k < N; k++)
					msRawData[N*modeCandidate_i+k] = (float)(yk[k+2]);
			}

			//store result into msRawData...
			for(j = 0; j < N; j++)
				msRawData[N*i+j] = (float)(yk[j+2]);

			// Prompt user on progress
			float percent_complete = (float)(i/(float)(L))*100;
		    fireProgressStateChanged((int)(percent_complete + 0.5));
		
		}
		
		// de-allocate memory
	   buckets = null;
	   slist = null;
	   sdata = null;

	   yk = null;
	   Mh = null;
		
		// done.
		return;

	}
	
	private void newOptimizedFilter2(float sigmaS, float sigmaR)
	{
		// Declare Variables
		int		iterationCount, i, j, k, modeCandidateX, modeCandidateY, modeCandidate_i;
		double	mvAbs, diff, el;

		//re-assign bandwidths to sigmaS and sigmaR
		if(((h[0] = sigmaS) <= 0)||((h[1] = sigmaR) <= 0))
		{
			MipavUtil.displayError("msImageProcessor Segment sigmaS and/or sigmaR is zero or negative.");
			return;
		}
		
		//define input data dimension with lattice
		int lN	= N + 2;
		
		// Traverse each data point applying mean shift
		// to each data point
		
		// Allcocate memory for yk
		double	yk[]		= new double [lN];
		
		// Allocate memory for Mh
		double	Mh[]		= new double [lN];

	   // let's use some temporary data
	   double sdata[];
	   sdata = new double[lN*L];

	   // copy the scaled data
	   int idxs, idxd;
	   idxs = idxd = 0;
	   if (N==3)
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   } else if (N==1)
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   } else
	   {
	      for(i=0; i<L; i++)
	      {
	         sdata[idxs++] = (i%width)/sigmaS;
	         sdata[idxs++] = (i/width)/sigmaS;
	         for (j=0; j<N; j++)
	            sdata[idxs++] = data[idxd++]/sigmaR;
	      }
	   }
	   // index the data in the 3d buckets (x, y, L)
	   int buckets[];
	   int slist[];
	   slist = new int[L];
	   int bucNeigh[] = new int[27];

	   double sMins; // just for L
	   double sMaxs[] = new double[3]; // for all
	   sMaxs[0] = width/sigmaS;
	   sMaxs[1] = height/sigmaS;
	   sMins = sMaxs[2] = sdata[2];
	   idxs = 2;
	   double cval;
	   for(i=0; i<L; i++)
	   {
	      cval = sdata[idxs];
	      if (cval < sMins)
	         sMins = cval;
	      else if (cval > sMaxs[2])
	         sMaxs[2] = cval;

	      idxs += lN;
	   }

	   int nBuck1, nBuck2, nBuck3;
	   int cBuck1, cBuck2, cBuck3, cBuck;
	   nBuck1 = (int) (sMaxs[0] + 3);
	   nBuck2 = (int) (sMaxs[1] + 3);
	   nBuck3 = (int) (sMaxs[2] - sMins + 3);
	   buckets = new int[nBuck1*nBuck2*nBuck3];
	   for(i=0; i<(nBuck1*nBuck2*nBuck3); i++)
	      buckets[i] = -1;

	   idxs = 0;
	   for(i=0; i<L; i++)
	   {
	      // find bucket for current data and add it to the list
	      cBuck1 = (int) sdata[idxs] + 1;
	      cBuck2 = (int) sdata[idxs+1] + 1;
	      cBuck3 = (int) (sdata[idxs+2] - sMins) + 1;
	      cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);

	      slist[i] = buckets[cBuck];
	      buckets[cBuck] = i;

	      idxs += lN;
	   }
	   // init bucNeigh
	   idxd = 0;
	   for (cBuck1=-1; cBuck1<=1; cBuck1++)
	   {
	      for (cBuck2=-1; cBuck2<=1; cBuck2++)
	      {
	         for (cBuck3=-1; cBuck3<=1; cBuck3++)
	         {
	            bucNeigh[idxd++] = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	         }
	      }
	   }
	   double wsuml, weight;
	   double hiLTr = 80.0/sigmaR;
	   // done indexing/hashing

		
		// Initialize mode table used for basin of attraction
	   for (i = 0; i < L; i++) {
		   modeTable[i] = 0;
	   }
		
		// proceed ...
	   Preferences.debug("done.\nApplying mean shift (Using Lattice) ... \n", Preferences.DEBUG_ALGORITHM);


		for(i = 0; i < L; i++)
		{
			// if a mode was already assigned to this data point
			// then skip this point, otherwise proceed to
			// find its mode by applying mean shift...
			if (modeTable[i] == 1)
				continue;

			// initialize point list...
			pointCount = 0;

			// Assign window center (window centers are
			// initialized by createLattice to be the point
			// data[i])
	      idxs = i*lN;
	      for (j=0; j<lN; j++)
	         yk[j] = sdata[idxs+j];
			
			// Calculate the mean shift vector using the lattice
			// LatticeMSVector(Mh, yk); // modify to new
	      /*****************************************************/
	   	// Initialize mean shift vector
		   for(j = 0; j < lN; j++)
	   		Mh[j] = 0;
	   	wsuml = 0;
	      // uniformLSearch(Mh, yk_ptr); // modify to new
	      // find bucket of yk
	      cBuck1 = (int) yk[0] + 1;
	      cBuck2 = (int) yk[1] + 1;
	      cBuck3 = (int) (yk[2] - sMins) + 1;
	      cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	      for (j=0; j<27; j++)
	      {
	         idxd = buckets[cBuck+bucNeigh[j]];
	         // list parse, crt point is cHeadList
	         while (idxd>=0)
	         {
	            idxs = lN*idxd;
	            // determine if inside search window
	            el = sdata[idxs+0]-yk[0];
	            diff = el*el;
	            el = sdata[idxs+1]-yk[1];
	            diff += el*el;

	            if (diff < 1.0)
	            {
	               el = sdata[idxs+2]-yk[2];
	               if (yk[2] > hiLTr)
	                  diff = 4*el*el;
	               else
	                  diff = el*el;

	               if (N>1)
	               {
	                  el = sdata[idxs+3]-yk[3];
	                  diff += el*el;
	                  el = sdata[idxs+4]-yk[4];
	                  diff += el*el;
	               }

	               if (diff < 1.0)
	               {
	                  weight = 1-weightMap[idxd];
	                  for (k=0; k<lN; k++)
	                     Mh[k] += weight*sdata[idxs+k];
	                  wsuml += weight;

	      				//set basin of attraction mode table
	                  if (diff < speedThreshold)
	                  {
					         if(modeTable[idxd] == 0)
					         {
	         					pointList[pointCount++]	= idxd;
						         modeTable[idxd]	= 2;
	      				   }
	                  }
	               }
	            }
	            idxd = slist[idxd];
	         }
	      }
	   	if (wsuml > 0)
	   	{
			   for(j = 0; j < lN; j++)
	   			Mh[j] = Mh[j]/wsuml - yk[j];
	   	}
	   	else
	   	{
			   for(j = 0; j < lN; j++)
	   			Mh[j] = 0;
	   	}
	      /*****************************************************/
	   	// Calculate its magnitude squared
			//mvAbs = 0;
			//for(j = 0; j < lN; j++)
			//	mvAbs += Mh[j]*Mh[j];
	      mvAbs = (Mh[0]*Mh[0]+Mh[1]*Mh[1])*sigmaS*sigmaS;
	      if (N==3)
	         mvAbs += (Mh[2]*Mh[2]+Mh[3]*Mh[3]+Mh[4]*Mh[4])*sigmaR*sigmaR;
	      else
	         mvAbs += Mh[2]*Mh[2]*sigmaR*sigmaR;

			
			// Keep shifting window center until the magnitude squared of the
			// mean shift vector calculated at the window center location is
			// under a specified threshold (Epsilon)
			
			// NOTE: iteration count is for speed up purposes only - it
			//       does not have any theoretical importance
			iterationCount = 1;
			while((mvAbs >= EPSILON)&&(iterationCount < LIMIT))
			{
				
				// Shift window location
				for(j = 0; j < lN; j++)
					yk[j] += Mh[j];
				
				// check to see if the current mode location is in the
				// basin of attraction...

				// calculate the location of yk on the lattice
				modeCandidateX	= (int) (sigmaS*yk[0]+0.5);
				modeCandidateY	= (int) (sigmaS*yk[1]+0.5);
				modeCandidate_i	= modeCandidateY*width + modeCandidateX;

				// if mvAbs != 0 (yk did indeed move) then check
				// location basin_i in the mode table to see if
				// this data point either:
				
				// (1) has not been associated with a mode yet
				//     (modeTable[basin_i] = 0), so associate
				//     it with this one
				//
				// (2) it has been associated with a mode other
				//     than the one that this data point is converging
				//     to (modeTable[basin_i] = 1), so assign to
				//     this data point the same mode as that of basin_i

				if ((modeTable[modeCandidate_i] != 2) && (modeCandidate_i != i))
				{
					// obtain the data point at basin_i to
					// see if it is within h*TC_DIST_FACTOR of
					// of yk
	            diff = 0;
	            idxs = lN*modeCandidate_i;
	            for (k=2; k<lN; k++)
	            {
	               el = sdata[idxs+k] - yk[k];
	               diff += el*el;
	            }

					// if the data point at basin_i is within
					// a distance of h*TC_DIST_FACTOR of yk
					// then depending on modeTable[basin_i] perform
					// either (1) or (2)
					if (diff < speedThreshold)
					{
						// if the data point at basin_i has not
						// been associated to a mode then associate
						// it with the mode that this one will converge
						// to
						if (modeTable[modeCandidate_i] == 0)
						{
							// no mode associated yet so associate
							// it with this one...
							pointList[pointCount++]		= modeCandidate_i;
							modeTable[modeCandidate_i]	= 2;

						} else
						{

							// the mode has already been associated with
							// another mode, thererfore associate this one
							// mode and the modes in the point list with
							// the mode associated with data[basin_i]...

							// store the mode info into yk using msRawData...
							for (j = 0; j < N; j++)
								yk[j+2] = msRawData[modeCandidate_i*N+j]/sigmaR;

							// update mode table for this data point
							// indicating that a mode has been associated
							// with it
							modeTable[i] = 1;

							// indicate that a mode has been associated
							// to this data point (data[i])
							mvAbs = -1;

							// stop mean shift calculation...
							break;
						}
					}
				}
				
	         // Calculate the mean shift vector at the new
	         // window location using lattice
	         // Calculate the mean shift vector using the lattice
	         // LatticeMSVector(Mh, yk); // modify to new
	         /*****************************************************/
	         // Initialize mean shift vector
	         for(j = 0; j < lN; j++)
	            Mh[j] = 0;
	         wsuml = 0;
	         // uniformLSearch(Mh, yk_ptr); // modify to new
	         // find bucket of yk
	         cBuck1 = (int) yk[0] + 1;
	         cBuck2 = (int) yk[1] + 1;
	         cBuck3 = (int) (yk[2] - sMins) + 1;
	         cBuck = cBuck1 + nBuck1*(cBuck2 + nBuck2*cBuck3);
	         for (j=0; j<27; j++)
	         {
	            idxd = buckets[cBuck+bucNeigh[j]];
	            // list parse, crt point is cHeadList
	            while (idxd>=0)
	            {
	               idxs = lN*idxd;
	               // determine if inside search window
	               el = sdata[idxs+0]-yk[0];
	               diff = el*el;
	               el = sdata[idxs+1]-yk[1];
	               diff += el*el;
	               
	               if (diff < 1.0)
	               {
	                  el = sdata[idxs+2]-yk[2];
	                  if (yk[2] > hiLTr)
	                     diff = 4*el*el;
	                  else
	                     diff = el*el;
	                  
	                  if (N>1)
	                  {
	                     el = sdata[idxs+3]-yk[3];
	                     diff += el*el;
	                     el = sdata[idxs+4]-yk[4];
	                     diff += el*el;
	                  }
	                  
	                  if (diff < 1.0)
	                  {
	                     weight = 1-weightMap[idxd];
	                     for (k=0; k<lN; k++)
	                        Mh[k] += weight*sdata[idxs+k];
	                     wsuml += weight;

	         				//set basin of attraction mode table
	                     if (diff < speedThreshold)
	                     {
	   				         if(modeTable[idxd] == 0)
					            {
	            					pointList[pointCount++]	= idxd;
						            modeTable[idxd]	= 2;
	      				      }
	                     }

	                  }
	               }
	               idxd = slist[idxd];
	            }
	         }
	         if (wsuml > 0)
	         {
	            for(j = 0; j < lN; j++)
	               Mh[j] = Mh[j]/wsuml - yk[j];
	         }
	         else
	         {
	            for(j = 0; j < lN; j++)
	               Mh[j] = 0;
	         }
	         /*****************************************************/
				
				// Calculate its magnitude squared
				//mvAbs = 0;
				//for(j = 0; j < lN; j++)
				//	mvAbs += Mh[j]*Mh[j];
	         mvAbs = (Mh[0]*Mh[0]+Mh[1]*Mh[1])*sigmaS*sigmaS;
	         if (N==3)
	            mvAbs += (Mh[2]*Mh[2]+Mh[3]*Mh[3]+Mh[4]*Mh[4])*sigmaR*sigmaR;
	         else
	            mvAbs += Mh[2]*Mh[2]*sigmaR*sigmaR;

				// Increment iteration count
				iterationCount++;
				
			}

			// if a mode was not associated with this data point
			// yet associate it with yk...
			if (mvAbs >= 0)
			{
				// Shift window location
				for(j = 0; j < lN; j++)
					yk[j] += Mh[j];
				
				// update mode table for this data point
				// indicating that a mode has been associated
				// with it
				modeTable[i] = 1;

			}
			
	      for (k=0; k<N; k++)
	         yk[k+2] *= sigmaR;

			// associate the data point indexed by
			// the point list with the mode stored
			// by yk
			for (j = 0; j < pointCount; j++)
			{
				// obtain the point location from the
				// point list
				modeCandidate_i = pointList[j];

				// update the mode table for this point
				modeTable[modeCandidate_i] = 1;

				//store result into msRawData...
				for(k = 0; k < N; k++)
					msRawData[N*modeCandidate_i+k] = (float)(yk[k+2]);
			}

			//store result into msRawData...
			for(j = 0; j < N; j++)
				msRawData[N*i+j] = (float)(yk[j+2]);

			// Prompt user on progress
			float percent_complete = (float)(i/(float)(L))*100;
		    fireProgressStateChanged((int)(percent_complete + 0.5));
		}
		
		
		// de-allocate memory
	   buckets = null;
	   slist = null;
	   sdata = null;

	   yk = null;
	   Mh = null;
		
		// done.
		return;

	}


	/*******************************************************/
	/*Initialize Output                                    */
	/*******************************************************/
	/*Allocates memory needed by the mean shift image pro- */
	/*cessor class output storage data structure.          */
	/*******************************************************/
	/*Post:                                                */
	/*      - the memory needed by the output storage      */
	/*        structure of this class has been (re-)allo-  */
	/*        cated.                                       */
	/*******************************************************/

	private void initializeOutput()
	{

		//De-allocate memory if output was defined for previous image
		destroyOutput();

		//Allocate memory for msRawData (filtered image output)
		msRawData = new float[L*N];
		

		//Allocate memory used to store image modes and their corresponding regions...
		modes = new float[L*(N+2)];
		labels = new int[L];
		modePointCounts = new int[L];
		indexTable= new int[L];

		//Allocate memory for integer modes used to perform connected components
		//(image labeling)...
		LUV_data = new float[N*L];

		//indicate that the class output storage structure has been defined
		class_state.OUTPUT_DEFINED	= true;

	}
	
	/*******************************************************/
	/*Destroy Output                                       */
	/*******************************************************/
	/*De-allocates memory needed by the mean shift image   */
	/*processor class output storage data structure.       */
	/*******************************************************/
	/*Post:                                                */
	/*      - the memory needed by the output storage      */
	/*        structure of this class has been de-alloc-   */
	/*        ated.                                        */
	/*      - the output storage structure has been init-  */
	/*        ialized for re-use.                          */
	/*******************************************************/

	private void destroyOutput()
	{

		//de-allocate memory for msRawData (filtered image output)
		msRawData = null;;

		//de-allocate memory used by output storage and image
		//classification structure
		modes = null;
		labels = null;
		modePointCounts = null;
		indexTable = null;
		
		//de-allocate memory for LUV_data
		LUV_data = null;

		//re-initialize classification structure
		regionCount					= 0;

		//indicate that the output has been destroyed
		class_state.OUTPUT_DEFINED	= false;

		//done.
		return;

	}


	
	/*******************************************************/
	/*Class Consistency Check                              */
	/*******************************************************/
	/*Checks the state of the class prior to the applicat- */
	/*ion of mean shift.                                   */
	/*******************************************************/
	/*Pre:                                                 */
	/*      - iN is the specified dimension of the input,  */
	/*        iN = N for a general input data set, iN = N  */
	/*        + 2 for a input set defined using a lattice  */
	/*Post:                                                */
	/*      - if the kernel has not been created, an input */
	/*        has not been defined and/or the specified    */
	/*        input dimension (iN) does not match that of  */
	/*        the kernel a fatal error is flagged.         */
	/*******************************************************/

	private void classConsistencyCheck(int iN, boolean usingLattice)
	{
		
		//make sure that dimension of the kernel and the input data set
		//agree
		
		//calculate dimension of kernel (kN)
		int i, kN	= 0;
		for(i = 0; i < kp; i++)
			kN	+= P[i];
		
		//perform comparison...
		if(iN != kN)
		{
			MipavUtil.displayError("MeanShift classConsitencyCheck Kernel dimension does not match defined input data dimension.");
			return;
		}
		
		//done.
		return;
		
	}

	// User Defined Weight Function
	private class userWeightFunct {
	   
	  double			w[];
	  double			halfWindow;
	  int				sampleNumber;
	  int				subspace;
	  userWeightFunct	next;

	};
	
	//Define class state structure
	private class ClassStateStruct {
		boolean	OUTPUT_DEFINED;
	};

	
	//define Region Adjacency List class prototype
	private class RAList {

		// RAM Label
		public int		label;

		// RAM Weight
		public float	edgeStrength;
		public int		edgePixelCount;

		// RAM Link
		public RAList	next[];

		
		// current and previous pointer
		private RAList cur[]; 

		// flag
		private byte exists;

	};
	
	//define region structure
	private class REGION {
		int			label;
		int			pointCount;
		int			region;

	};
	
	//region class prototype...
	private class RegionList {
		
		/*******************************************************/
		/*Pre:                                                 */
		/*      - modesPtr is a pointer to an array of modes   */
		/*      - maxRegions_ is the maximum number of regions */
		/*        that can be defined                          */
		/*      - L_ is the number of data points being class- */
		/*        ified by the region list class               */
		/*      - N is the dimension of the data set being cl- */
		/*        assified by the region list class            */
		/*Post:                                                */
		/*      - a region list object has been properly init- */
		/*        ialized.                                     */
		/*******************************************************/

		public RegionList(int maxRegions_, int L_, int N_)
		{
			int i;

			//Obtain maximum number of regions that can be
			//defined by user
			if((maxRegions = maxRegions_) <= 0) {
				MipavUtil.displayError("RegionList Maximum number of regions is zero or negative.");
			    return;
			}

			//Obtain dimension of data set being classified by
			//region list class
			if((N = N_) <= 0) {
				MipavUtil.displayError("RegionList Dimension is zero or negative.");
				return;
			}

			//Obtain length of input data set...
			if((L = L_) <= 0) {
				MipavUtil.displayError("RegionList Length of data set is zero or negative.");
				return;
			}

			//Allocate memory for index table
			indexTable = new int [L];

			//Allocate memory for region list array
			regionList = new REGION [maxRegions];
			for (i = 0; i < maxRegions; i++) {
				regionList[i] = new REGION();
			}

			//Initialize region list...
			numRegions		= freeRegion = 0;

			//Initialize indexTable
			freeBlockLoc	= 0;

			//done.
			return;
		}
		
		/*******************************************************/
		/*Add Region                                           */
		/*******************************************************/
		/*Adds a region to the region list.                    */
		/*******************************************************/
		/*Pre:                                                 */
		/*      - label is a positive integer used to uniquely */
		/*        identify a region                            */
		/*      - pointCount is the number of N-dimensional    */
		/*        data points that exist in the region being   */
		/*        classified.                                  */
		/*      - indeces is a set of indeces specifying the   */
		/*        data points contained within this region     */
		/*      - pointCount must be > 0                       */
		/*Post:                                                */
		/*      - a new region labeled using label and contai- */
		/*        ning pointCount number of points has been    */
		/*        added to the region list.                    */
		/*******************************************************/

		public void addRegion(int label, int pointCount, int indeces[], int indecesOffset)
		{

			//make sure that there is enough room for this new region 
			//in the region list array...
			if(numRegions >= maxRegions) {
				MipavUtil.displayError("addRegion Not enough memory allocated.");
				return;
			}

			//make sure that label is positive and point Count > 0...
			if((label < 0)||(pointCount <= 0)) {
				MipavUtil.displayError("addRegion Label is negative or number of points in region is invalid.");
				return;
			}

			//make sure that there is enough memory in the indexTable
			//for this region...
			if((freeBlockLoc + pointCount) > L) {
				MipavUtil.displayError("addRegion Adding more points than what is contained in data set.");
				return;
			}

			//place new region into region list array using
			//freeRegion index
			regionList[freeRegion].label		= label;
			regionList[freeRegion].pointCount	= pointCount;
			regionList[freeRegion].region		= freeBlockLoc;

			//copy indeces into indexTable using freeBlock...
			int i;
			for(i = 0; i < pointCount; i++)
				indexTable[freeBlockLoc+i] = indeces[i + indecesOffset];

			//increment freeBlock to point to the next free
			//block
			freeBlockLoc	+= pointCount;

			//increment freeRegion to point to the next free region
			//also, increment numRegions to indicate that another
			//region has been added to the region list
			freeRegion++;
			numRegions++;

			//done.
			return;

		}



		//#####################################
		//### REGION LIST PARTITIONED ARRAY ###
		//#####################################

		REGION		regionList[];			//array of maxRegions regions
		//int			minRegion;

		int			maxRegions;				//defines the number maximum number of regions
											//allowed (determined by user during class construction)
		int			numRegions;				//the number of regions currently stored by the
											//region list
		int			freeRegion;				//an index into the regionList pointing to the next
											//available region in the regionList

		//#####################################
		//###         INDEX TABLE           ###
		//#####################################

		int			indexTable[];			//an array of indexes that point into an external structure
											//specifying which points belong to a region
		int			freeBlockLoc;			//points to the next free block of memory in the indexTable

		//#####################################
		//###     INPUT DATA PARAMETERS     ###
		//#####################################

		//Dimension of data set
		//int			N;				     //dimension of data set being classified by region list
											//class

		//Length of the data set
		int			L;						//number of points contained by the data set being classified by
											//region list class
	}

}