package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Random;

import com.sun.media.ui.ProgressBar;

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
	private final double Xn			= 0.95050;
	private final double Yn			= 1.00000;
	private final double Zn			= 1.08870;
	//const double Un_prime	= 0.19780;
	//const double Vn_prime	= 0.46830;
	private final double Un_prime	= 0.19784977571475;
	private final double Vn_prime	= 0.46834507665248;
	private final double Lt			= 0.008856;
	
	//RGB to LUV conversion
    private final double XYZ[][] = new double[][]{	{  0.4125,  0.3576,  0.1804 },
							{  0.2125,  0.7154,  0.0721 },
							{  0.0193,  0.1192,  0.9502 }	};
							
    // Gaussian Lookup Table
	private final int		GAUSS_NUM_ELS   = 16;		// take 16 samples of exp(-u/2)
	private final double	GAUSS_LIMIT     = 2.9;		// GAUSS_LIMIT     = c
	// GAUSS_INCREMENT = (c^2)/(# of samples)
	private final double	GAUSS_INCREMENT = GAUSS_LIMIT*GAUSS_LIMIT/GAUSS_NUM_ELS;
	
	 // Threshold
	private final double	EPSILON	= 0.01;	// define threshold (approx. Value of Mh at a peak or plateau)
	private final double	TC_DIST_FACTOR	= 0.5;		// cluster search windows near convergence that are a distance

	private final int		LIMIT  = 100; // define max. # of iterations to find mode



							
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
  	private tree root[]; // root of kdBST used to store input

  	private tree forest[]; // memory allocated for tree nodes

  	private float range[]; // range vector used to perform range search on kd tree, indexed
  	
    // KERNEL DATA STRUCTURE 
 	private float h[]; // bandwidth vector

 	private float offset[];	// defines bandwidth offset caused by the use of a Gaussian kernel (for example)
 	
 	// WEIGHT MAP USED WHEN COMPUTING MEAN SHIFT ON A LATTICE
 	private float weightMap[]; // weight map that may be used to weight the kernel
 					           // upon performing mean shift on a lattice

 	private boolean	weightMapDefined = false; // used to indicate if a lattice weight map has been defined
 	
 	//Kernel
 	private enum kernelType		{Uniform, Gaussian, UserDefined};
 	
 	// KERNEL DATA STRUCTURE

 	private kernelType	kernel[];  // kernel types for each subspace S[i]

 	private double	w[][];  // weight function lookup table

 	private double	increment[];	 // increment used by weight hashing function

 	private boolean	uniformKernel; // flag used to indicate if the kernel is uniform or not
 	
 	private userWeightFunct	head, cur; // user defined weight function linked list
    
 	// CLASS STATE 
 	private ClassStateStruct	class_state; //specifies the state of the class(i.e if data has been loaded into 
 											 //the class, if a kernel has been defined, etc.)

 	// MEAN SHIFT PROCESSING DATA STRUCTURES 

 	private double	uv[]; // stores normalized distance vector between yk and xi

    // Error Handler
 	private enum ErrorLevel		{EL_OKAY, EL_ERROR, EL_HALT};
 	private enum ErrorType		{NONFATAL, FATAL};
 	
 	// ErrorMessage is an error message that is set by a mean shift library class when an error occurs.
    private String ErrorMessage;
    
    // ErrorStatus indicates if an error has occurred as a result of improper use of a mean shift library
    // class method or because of insufficient resources. ErrorStatus is set to EL_ERROR (ErrorStatus
    // = 1) if an error has occurred. If no error occurred when calling a particular method ErrorStatus
    // is set to EL_OKAY (ErrorStatus = 0). 

    private ErrorLevel	ErrorStatus;
    
    // Speed Up Level
    private enum SpeedUpLevel	{NO_SPEEDUP, MED_SPEEDUP, HIGH_SPEEDUP};
    
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


    private long time5;
    private kernelType spatialKernelType = kernelType.Uniform;
    
    private kernelType rangeKernelType = kernelType.Uniform;
    
    private float spatialBandwidth = 1.0f;
    
    private float rangeBandwidth = 1.0f;
    
    private int minRegion;
    
    private SpeedUpLevel speedUpLevel;

	private boolean measureTime;
	
	public AlgorithmMeanShiftSegmentation(ModelImage destImage, ModelImage srcImage, kernelType spatialKernelType,
			kernelType rangeKernelType, float spatialBandwidth, float rangeBandwidth, int minRegion,
			SpeedUpLevel speedUpLevel, boolean measureTime) {
		super(destImage, srcImage);
		this.spatialKernelType = spatialKernelType;
		this.rangeKernelType = rangeKernelType;
		this.spatialBandwidth = spatialBandwidth;
		this.rangeBandwidth = rangeBandwidth;
		this.minRegion = minRegion;
		this.speedUpLevel = speedUpLevel;
		this.measureTime = measureTime;
	}
	
	public void runAlgorithm() {
		int nDims;
		int zDim;
		int tDim;
		float rgb[] = null;
		int z;
		int t;
		int i;
		int kN;
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
        }
        else {
		    data = new double[L];
		    N = 1;
        }
        class_state = new ClassStateStruct();
        //allocate memory for weight map
        weightMap = new float [L];
        /*      - wm is a floating point array of size         */
        /*        (height x width) specifying for each pixel   */
        /*        edge strength.                               */
        /*      - eps is a threshold used to fuse similar      */
        /*        regions during transitive closure.           */
        /*Post:                                                */
        /*      - wm has been used to populate the weight      */
        /*        map.                                         */
        /*      - the threshold used during transitive closure */
        /*        is taken as eps.                             */
        /*******************************************************/

        /* void msImageProcessor::SetWeightMap(float *wm, float eps)
        /{

        	//initlaize confmap using wm
        	SetLatticeWeightMap(wm);

        	//set threshold value
        	if((epsilon = eps) < 0)
        		ErrorHandler("msImageProcessor", "SetWeightMap", "Threshold is negative.");

        	//done.
        	return;

        }*/
        /*******************************************************/
        /*Set Lattice Weight Map                               */
        /*******************************************************/
        /*Populates the lattice weight map with specified      */
        /*weight values.                                       */
        /*******************************************************/
        /*Pre:                                                 */
        /*      - wm is a floating point array of size L       */
        /*        specifying for each data point a weight      */
        /*        value                                        */
        /*Post:                                                */
        /*      - wm has been used to populate the lattice     */
        /*        weight map.                                  */
        /*******************************************************/

        /*void MeanShift::SetLatticeWeightMap(float *wm)
        {
        	//make sure wm is not NULL
        	if(!wm)
        	{
        		ErrorHandler("MeanShift", "SetWeightMap", "Specified weight map is NULL.");
        		return;
        	}

        	//populate weightMap using wm
        	int i;
        	for(i = 0; i < L; i++)
        		weightMap[i] = wm[i];

        	//indicate that a lattice weight map has been specified
        	weightMapDefined	= true;

        	//done.
        	return;

        }*/


        //define default kernel parameters...
        kernel	= new kernelType[]{spatialKernelType, rangeKernelType};
      	P = new int[]{2, N};
      	h	= new float[]{spatialBandwidth , rangeBandwidth};
      	kp = 2;
      	offset = new float[kp];
		increment = new double[kp];
		kN = 0;
		for (i = 0; i < kp; i++) {
			kN += P[i];
		}
		range = new float[2*kN];
		uv = new double[kN];
		// Generate weight function lookup table
		// using above information and user
		// defined weight function list
		generateLookupTable();
		
		//check for errors
		if(ErrorStatus == ErrorLevel.EL_ERROR) {
		    setCompleted(false);
		    return;
		}
		
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

        	} // for (z = 0; z < zDim; z++)
		} // for (t = 0; t < tDim; t++)
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
		
		// Assume kernel is uniform
		uniformKernel = true;
		
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
				
				// Set uniformKernel to false
				uniformKernel = false;
				
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
				
				// Set uniformKernel to false
				uniformKernel = false;
				
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
		
		//initialize input data set kd-tree
		root						= null;
		forest						= null;
		range						= null;
		
		//intialize kernel strucuture...
		w							= null;
		
		//initialize weight function linked list...
		head						= cur	= null;
		
		//intialize mean shift processing data structures...
		uv							= null;

		//indicate that the lattice weight map is undefined
		weightMapDefined			= false;
		
		ErrorMessage				=  null;
		
		//initialize error status to OKAY
		ErrorStatus					= ErrorLevel.EL_OKAY;
		
		//Initialize class state...
		class_state.INPUT_DEFINED	= false;
		class_state.LATTICE_DEFINED	= false;
		class_state.OUTPUT_DEFINED	= false;
		
	}

	
	private void defineLInput()
	{
		int i;
		
		//if input data is defined de-allocate memory, and
		//re-initialize the input data structure
		if((class_state.INPUT_DEFINED)||(class_state.LATTICE_DEFINED))
			resetInput();
		
		
		//check for errors
		if(ErrorStatus == ErrorLevel.EL_ERROR)
			return;

		//initialize weightMap to an array of zeros
		for (i = 0; i < L; i++) {
			weightMap[i] = 0.0f;
		}
		
		//Indicate that a lattice input has recently been
		//defined
		class_state.LATTICE_DEFINED	= true;
		class_state.INPUT_DEFINED	= false;
		class_state.OUTPUT_DEFINED	= false;
		
		//done.
		return;
		
	}

	
	/*******************************************************/
	/*Reset Input                                          */
	/*******************************************************/
	/*De-allocates memory for and re-intializes input data */
	/*structure.                                           */
	/*******************************************************/
	/*Post:                                                */
	/*      - the memory of the input data structure has   */
	/*        been de-allocated and this strucuture has    */
	/*        been initialized for re-use.                 */
	/*******************************************************/

	private void resetInput()
	{
		
		//initialize input data structure for re-use
		forest	= null;
		root	= null;
		
		//re-set class input to indicate that
		//an input is not longer stored by
		//the private data members of this class
		class_state.INPUT_DEFINED	= class_state.LATTICE_DEFINED = false;
		
	}


	
	private void RGBtoLUV(float rgb[], int rgbOffset, double luv[], int luvOffset)
	{

		//declare variables
		double	x, y, z, L0, u_prime, v_prime, constant;

		//convert RGB to XYZ...
		x		= XYZ[0][0]*rgb[rgbOffset] + XYZ[0][1]*rgb[rgbOffset+1] + XYZ[0][2]*rgb[rgbOffset+2];
		y		= XYZ[1][0]*rgb[rgbOffset] + XYZ[1][1]*rgb[rgbOffset+1] + XYZ[1][2]*rgb[rgbOffset+2];
		z		= XYZ[2][0]*rgb[rgbOffset] + XYZ[2][1]*rgb[rgbOffset+1] + XYZ[2][2]*rgb[rgbOffset+2];

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

		//Apply mean shift to data set using sigmaS and sigmaR...
		filter();

		//check for errors
		/*if(ErrorStatus == EL_ERROR)
			return;

		//check to see if the system has been halted, if so exit
		if(ErrorStatus == EL_HALT)
			return;

		//Check to see if the algorithm is to be halted, if so then
		//destroy output and exit
		if((ErrorStatus = msSys.Progress((float)(0.85))) == EL_HALT)
		{
			DestroyOutput();
			return;
		}

	#ifdef PROMPT
		msSys.Prompt("Applying transitive closure...");
		msSys.StartTimer();
	#endif

		//allocate memory visit table
		visitTable = new unsigned char [L];

		//Apply transitive closure iteratively to the regions classified
		//by the RAM updating labels and modes until the color of each neighboring
		//region is within sqrt(rR2) of one another.
		rR2 = (float)(h[1]*h[1]*0.25);
		TransitiveClosure();
		int oldRC = regionCount;
		int deltaRC, counter = 0;
		do {
			TransitiveClosure();
			deltaRC = oldRC-regionCount;
			oldRC = regionCount;
			counter++;
		} while ((deltaRC <= 0)&&(counter < 10));

		//de-allocate memory for visit table
		delete [] visitTable;
		visitTable	= NULL;

		//Check to see if the algorithm is to be halted, if so then
		//destroy output and regions adjacency matrix and exit
		if((ErrorStatus = msSys.Progress((float)(0.95))) == EL_HALT)
		{
			DestroyRAM();
			DestroyOutput();
			return;
		}

	#ifdef PROMPT
		double timer	= msSys.ElapsedTime();
		msSys.Prompt("done. (%6.2f seconds, numRegions = %6d).\nPruning spurious regions\t... ", timer, regionCount);
		msSys.StartTimer();
	#endif

		//Prune spurious regions (regions whose area is under
		//minRegion) using RAM
		Prune(minRegion);

	#ifdef PROMPT
		timer	= msSys.ElapsedTime();
		msSys.Prompt("done. (%6.2f seconds, numRegions = %6d)\nPruning spurious regions    ...", timer, regionCount);
		msSys.StartTimer();
	#endif

		//Check to see if the algorithm is to be halted, if so then
		//destroy output and regions adjacency matrix and exit
		if((ErrorStatus = msSys.Progress(1.0)) == EL_HALT)
		{
			DestroyRAM();
			DestroyOutput();
			return;
		}

		//de-allocate memory for region adjacency matrix
		DestroyRAM();

		//output to msRawData
		int j, i, label;
		for(i = 0; i < L; i++)
		{
			label	= labels[i];
			for(j = 0; j < N; j++)
				{
					msRawData[N*i+j] = modes[N*label+j];
				}
		}

		//done.*/
		return;

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
		long time1;
		long time2;
		long time3;
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
		if(ErrorStatus == ErrorLevel.EL_ERROR)
			return;
		
		//If the image has just been read then allocate memory
		//for and initialize output data structure used to store
		//image modes and their corresponding regions...
		if(class_state.OUTPUT_DEFINED == false)
		{
			initializeOutput();

			//check for errors...
			if(ErrorStatus == ErrorLevel.EL_ERROR)
				return;
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
		/*switch(speedUpLevel)
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
	      NewOptimizedFilter2(spatialBandwidth, rangeBandwidth);		
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
		Connect();
		
	if (measureTime) {
        time4 = System.currentTimeMillis();
        elapsedTime = time4 - time3;
		Preferences.debug("done. (" + elapsedTime +  " milliseconds, numRegions = " + regionCount+ "\n",
		Preferences.DEBUG_ALGORITHM);
		time5 = System.currentTimeMillis();
	}*/

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
	         sdata[idxs++] = (i%width)/sigmaS;
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
	   Preferences.debug("done.\nApplying mean shift (Using Lattice)... ", Preferences.DEBUG_ALGORITHM);

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
	   Preferences.debug("done.\nApplying mean shift (Using Lattice) ... ", Preferences.DEBUG_ALGORITHM);


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
		
		//make sure input data set has been loaded into mean shift object...
		if((class_state.INPUT_DEFINED == false)&&(!usingLattice))
		{
			MipavUtil.displayError("MeanShift classConsistencyCheck No input data specified.");
			return;
		}
		
		//make sure that the lattice is defined if it is being used
		if((class_state.LATTICE_DEFINED == false)&&(usingLattice))
		{
			MipavUtil.displayError("MeanShift classConsistencyCheck Latice not created.");
			return;
		}
		
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


	
	//k-Dimensional Binary Search Tree
	private class tree {
	  float x[];
	  tree  right;
	  tree  left;
	  tree  parent;
	};

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
		boolean	INPUT_DEFINED;
		boolean	LATTICE_DEFINED;
		boolean	OUTPUT_DEFINED;
	};

 

}