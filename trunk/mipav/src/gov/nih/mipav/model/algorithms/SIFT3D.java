package gov.nih.mipav.model.algorithms;


    /* This is a port of SIFT3D is an analogue of the scale-invariant feature transform (SIFT) for three-dimensional images.
     *  It leverages volumetric data and real-world units to detect keypoints and extract a robust description of their content.
     *  It can also perform 3D image registration by matching SIFT3D features and fitting geometric transformations with the RANSAC algorithm.
     *  
     *  The MIT License (MIT)

	Copyright (c) 2015-2016 Blaine Rister et al.
	
	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:
	
	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.
	
	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
     */

public class SIFT3D extends AlgorithmBase {
	// Return codes
	private final int SIFT3D_SINGULAR = 1;
	private final int SIFT3D_SUCCESS  = 0;
	private final int SIFT3D_FAILURE = -1;
	private final int SIFT3D_HELP = 1;
	private final int SIFT3D_VERSION = 2;

	// Truth values
	private final int SIFT3D_TRUE = 1;
	private final int SIFT3D_FALSE = 0;
	
	/* Parameters */
	private int NBINS_AZ = 8;		// Number of bins for azimuthal angles
	private int NBINS_PO = 4;		// Number of bins for polar angles
	private int NHIST_PER_DIM = 4; // Number of SIFT descriptor histograms per dimension 
	//#define ICOS_HIST			// Icosahedral gradient histogram

	/* Constants */
	private int IM_NDIMS = 3; // Number of dimensions in an Image
	private int ICOS_NFACES = 20; // Number of faces in an icosahedron
	private int ICOS_NVERT = 12; // Number of vertices in an icosahedron
	
	// The number of elements in a gradient histogram
	//#ifdef ICOS_HIST
	private int HIST_NUMEL = (ICOS_NVERT);
	//#else
	// private inmt HIST_NUMEL = (NBINS_AZ * NBINS_PO);
	//#endif

	/* Derived constants */
	private int DESC_NUM_TOTAL_HIST = (NHIST_PER_DIM * NHIST_PER_DIM * NHIST_PER_DIM);
	private int DESC_NUMEL = (DESC_NUM_TOTAL_HIST * HIST_NUMEL);
	
	/* Supported image file formats */
	private enum im_format {
	        ANALYZE, /* Analyze */
	        DICOM, /* DICOM */
	        DIRECTORY, /* Directory */
	        NIFTI, /* NIFTI-1 */ 
	        UNKNOWN, /* Not one of the known extensions */
	        FILE_ERROR /* Error occurred in determining the format */
	};
	
	/* Possible data types for matrix elements */ 
	private enum Mat_rm_type {
		SIFT3D_DOUBLE,
		SIFT3D_FLOAT,
		SIFT3D_INT
	};
	
	/* Struct to hold OpenCL programs for this library */
	class Kernels {
	  int downsample_2x_3d;
	};
	
	/* Struct to hold OpenCL data about the user system */
	class CL_data {
		int devices[];	  // num_devices elements
		int queues[]; // One per device
		int platform;
		int context;
		int num_devices;
		int image_format;
		int mem_flags;
		//Kernels kernels;
		int valid;		// Is this struct valid?
	};
	
	/* Struct to hold a dense matrix in row-major order */
	class Mat_rm {

		class u {
			double data_double[];
			float  data_float[];
			int data_int[];
		};
		int size;		// Size of the buffer, in bytes
		int num_cols;           // Number of columns 
		int num_rows;           // Number of rows	
	    int static_mem;         // Flag for statically-allocated memory
		Mat_rm_type type;       // DOUBLE, FLOAT, or INT

	};
	
	/* Struct to hold image data. The image is a rectangular prism, 
	 * where the bottom-left corner is [0 0 0], the x-stride is 1,
	 * the y-stride is the width in x, and the z-stride is the
	 * size of an xy plane. For convenience use the macros IM_GET_IDX, 
	 * IM_GET_VOX, and IM_SET_VOX to manipulate this struct. */
	class Image {

		float data[];		// Raster of voxel values ~16MB
		int cl_image;	// Same-sized OpenCL image object
		double s;		// scale-space location
		int size;		// Total size in pixels
		int nx, ny, nz;		// Dimensions in x, y, and z
		double ux, uy, uz;	// Real world dimensions in x, y, and z
	    int xs, ys, zs;      // Stride in x, y, and z
	    int nc;                 // The number of channels
		int cl_valid;		// If TRUE, cl_image is valid

	};
	
	/* Holds separable FIR filters and programs to apply them */
	class Sep_FIR_filter {

		//int cl_apply_unrolled;	// unrolled OpenCL program to apply filter
		float kernel[];	// filter weights
		int dim;	// dimensionality, e.g. 3 for MRI
		int width;	// number of weights				
		int symmetric;	// enable symmetric optimizations: FALSE or TRUE

	};
	
	/* Holds Gaussian filters */
	class Gauss_filter {

		double sigma;
		Sep_FIR_filter f = new Sep_FIR_filter();

	};
	
	/* Holds Gaussian Scale-Space filters */
	class GSS_filters {

		Gauss_filter first_gauss = new Gauss_filter();	// Used on the very first blur
		Gauss_filter gauss_octave[];	// Array of kernels for one octave
		int num_filters;		// Number of filters for one octave
		int first_level;                // Index of the first scale level

	};
	
	/* Struct to hold miscellaneous SIFT detector OpenCL kernels */
	class SIFT_cl_kernels {

		int downsample_2;

	};
	
	/* Struct to hold a scale-space image pyramid */
	class Pyramid {
		
		// Levels in all octaves
		Image levels[];	

		// Scale-space parameters
		double sigma_n;
		double sigma0;
		int num_kp_levels;

		// Indexing information -- see immacros.h
		int first_octave;
		int num_octaves;
		int first_level;
		int num_levels;

	};
	
	/* Struct defining a vector in spherical coordinates */
	class Svec {

		float mag;	// Magnitude
		float po;	// Polar angle, [0, pi)
		float az;	// Azimuth angle, [0, 2pi)

	};
	
	/* Struct defining a vector in Cartesian coordinates */
	class Cvec {

		float x;
		float y;
		float z;

	};
	
	/* Slab allocation struct */
	class Slab {

		//void *buf;			// Buffer
		int num;			// Number of elements currently in buffer
		int buf_size;	        // Buffer capacity, in bytes

	};

	
	/**
     * SIFT3D - default constructor.
     */
    public SIFT3D() { }
    
    public void runAlgorithm() {
    	
    }
}
