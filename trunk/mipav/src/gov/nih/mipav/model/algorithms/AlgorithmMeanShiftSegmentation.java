package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Random;

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
	int				L, N, kp, P[];						// length, dimension, subspace number, and subspace dimensions
	//data space conversion...
	final double Xn			= 0.95050;
	final double Yn			= 1.00000;
	final double Zn			= 1.08870;
	//const double Un_prime	= 0.19780;
	//const double Vn_prime	= 0.46830;
	final double Un_prime	= 0.19784977571475;
	final double Vn_prime	= 0.46834507665248;
	final double Lt			= 0.008856;
	
	//RGB to LUV conversion
    final double XYZ[][] = new double[][]{	{  0.4125,  0.3576,  0.1804 },
							{  0.2125,  0.7154,  0.0721 },
							{  0.0193,  0.1192,  0.9502 }	};
							
	//Linear Storage (used by lattice and bst)////////
    float data[];								
    // memory allocated for data points stored by tree nodes
	// when used by the lattice data structure data does not store
    // the lattice information; format of data:
	// data = <x11, x12, ..., x1N,...,xL1, xL2, ..., xLN>
    // in the case of the lattice the i in data(i,j) corresponds
    
    // Lattice Data Structure////////
  	int	height, width;	// Height and width of lattice
  	
    //Range Searching on General Input Data Set////////
  	tree			root[];								// root of kdBST used to store input

  	tree			forest[];							// memory allocated for tree nodes

  	float			range[];								// range vector used to perform range search on kd tree, indexed

	
	public AlgorithmMeanShiftSegmentation(ModelImage destImage, ModelImage srcImage) {
		super(destImage, srcImage);
	}
	
	public void runAlgorithm() {
		int nDims;
		int zDim;
		int tDim;
		double luv[];
		float rgb[] = null;
		int z;
		int t;
		int i;
		int rangeDim;
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(),"Mean Shift Segmentation ...");
        meanShift();
        
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
        	luv = new double[3*L];
        	rgb = new float[4*L];
        	rangeDim = 3;
        }
        else {
		    luv = new double[L];
		    rangeDim = 1;
        }
		
		for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
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
        					RGBtoLUV(rgb, 4*i, luv, 3*i);
        			}

        		}
        		else {
	        		try {
	        			srcImage.exportData((z + t*zDim)*L, L, luv);
	        		}
	        		catch(IOException e) {
	        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        			setCompleted(false);
	        			return;
	        		}
        		}
        		
        		//define input defined on a lattice using mean shift base class
        		defineLInput(luv, height, width, rangeDim);

        	} // for (z = 0; z < zDim; z++)
		} // for (t = 0; t < tDim; t++)
	}
	
	private void meanShift()
	{
		
		//intialize input data set parameters...
		P							= null;
		L							= 0;
		N							= 0;
		kp							= 0;
		
		//initialize input data set storage structures...
		data						= null;
		
		/*//initialize input data set kd-tree
		root						= NULL;
		forest						= NULL;
		range						= NULL;
		
		//intialize lattice structure...
		height						= 0;
		width						= 0;
		
		//intialize kernel strucuture...
		h							= NULL;
		kernel						= NULL;
		w							= NULL;
		offset						= NULL;
		increment					= NULL;
		uniformKernel				= false;
		
		//initialize weight function linked list...
		head						= cur	= NULL;
		
		//intialize mean shift processing data structures...
		uv							= NULL;

		//set lattice weight map to null
		weightMap					= NULL;

		//indicate that the lattice weight map is undefined
		weightMapDefined			= false;
		
		//allocate memory for error message buffer...
		ErrorMessage				= new char [256];
		
		//initialize error status to OKAY
		ErrorStatus					= EL_OKAY;
		
		//Initialize class state...
		class_state.INPUT_DEFINED	= false;
		class_state.KERNEL_DEFINED	= false;
		class_state.LATTICE_DEFINED	= false;
		class_state.OUTPUT_DEFINED	= false;*/
		
	}

	
	private void defineLInput(double x[], int ht, int wt, int N_)
	{
		
		//if input data is defined de-allocate memory, and
		//re-initialize the input data structure
		/*if((class_state.INPUT_DEFINED)||(class_state.LATTICE_DEFINED))
			ResetInput();
		
		//Obtain lattice height and width
		if(((height	= ht) <= 0)||((width	= wt) <= 0))
		{
			ErrorHandler("MeanShift", "DefineLInput", "Lattice defined using zero or negative height and/or width.");
			return;
		}
		
		//Obtain input data dimension
		if((N = N_) <= 0)
		{
			ErrorHandler("MeanShift", "DefineInput", "Input defined using zero or negative dimension.");
			return;
		}
		
		//compute the data length, L, of input data set
		//using height and width
		L		= height*width;
		
		//Allocate memory for input data set, and copy
		//x into the private data members of the mean
		//shift class
		InitializeInput(x);
		
		//check for errors
		if(ErrorStatus == EL_ERROR)
			return;

		//allocate memory for weight map
		if(!(weightMap = new float [L]))
		{
			ErrorHandler("MeanShift", "InitializeInput", "Not enough memory.");
			return;
		}

		//initialize weightMap to an array of zeros
		memset(weightMap, 0, L*(sizeof(float)));
		
		//Indicate that a lattice input has recently been
		//defined
		class_state.LATTICE_DEFINED	= true;
		class_state.INPUT_DEFINED	= false;
		class_state.OUTPUT_DEFINED	= false;*/
		
		//done.
		return;
		
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
	
	//k-Dimensional Binary Search Tree
	private class tree {
	  float x[];
	  tree  right;
	  tree  left;
	  tree  parent;
	};

	 

}