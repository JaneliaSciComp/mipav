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
		
		public CL_data() {
			
		}
	};
	
	/* Struct to hold a dense matrix in row-major order */
	class Mat_rm {

		class u {
			double data_double[];
			float  data_float[];
			int data_int[];
			
			public u() {
				
			}
		};
		int size;		// Size of the buffer, in bytes
		int num_cols;           // Number of columns 
		int num_rows;           // Number of rows	
	    int static_mem;         // Flag for statically-allocated memory
		Mat_rm_type type;       // DOUBLE, FLOAT, or INT
		
		public Mat_rm() {
			
		}

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
		
		public Image() {
			
		}

	};
	
	/* Holds separable FIR filters and programs to apply them */
	class Sep_FIR_filter {

		//int cl_apply_unrolled;	// unrolled OpenCL program to apply filter
		float kernel[];	// filter weights
		int dim;	// dimensionality, e.g. 3 for MRI
		int width;	// number of weights				
		int symmetric;	// enable symmetric optimizations: FALSE or TRUE
		
		public Sep_FIR_filter() {
			
		}

	};
	
	/* Holds Gaussian filters */
	class Gauss_filter {

		double sigma;
		Sep_FIR_filter f = new Sep_FIR_filter();
		public Gauss_filter() {
			
		}

	};
	
	/* Holds Gaussian Scale-Space filters */
	class GSS_filters {

		Gauss_filter first_gauss = new Gauss_filter();	// Used on the very first blur
		Gauss_filter gauss_octave[];	// Array of kernels for one octave
		int num_filters;		// Number of filters for one octave
		int first_level;                // Index of the first scale level
		public GSS_filters() {
			
		}

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
		public Pyramid() {
			
		}

	};
	
	/* Struct defining a vector in spherical coordinates */
	class Svec {

		float mag;	// Magnitude
		float po;	// Polar angle, [0, pi)
		float az;	// Azimuth angle, [0, 2pi)
		
		public Svec() {
			
		}

	};
	
	/* Struct defining a vector in Cartesian coordinates */
	class Cvec {

		float x;
		float y;
		float z;
		
		public Cvec() {
			
		}

	};
	
	/* Slab allocation struct */
	class Slab {

		//void *buf;			// Buffer
		int num;			// Number of elements currently in buffer
		int buf_size;	        // Buffer capacity, in bytes
		
		public Slab() {
			
		}

	};
	
	/* Struct defining a keypoint in 3D space. */
    class Keypoint {

		float r_data[] = new float[IM_NDIMS * IM_NDIMS];	// Memory for matrix R, do not use this
		Mat_rm R = new Mat_rm();				// Rotation matrix into Keypoint space
		double xd, yd, zd;			// sub-pixel x, y, z
		double  sd;				// absolute scale
		int o, s;			        // pyramid indices 
		
		public Keypoint() {
			
		}

	};
	
	/* Struct to hold keypoints */
	class Keypoint_store {
		
		Keypoint buf[];
		Slab slab = new Slab();
		int nx, ny, nz;		// dimensions of first octave
		
		public Keypoint_store() {
			
		}

	};
	
	/* Struct defining an orientation histogram in
	 * spherical coordinates. */
	class Hist {
		float bins[] = new float[HIST_NUMEL];
		
		public Hist() {
			
		}
	};
	
	/* Triangle */
	class Tri {
		Cvec v[] = new Cvec[] {new Cvec(), new Cvec(), new Cvec()}; // Vertices
		int idx[] = new int[3]; // Index of each vertex in the solid
		
		public Tri() {
			
		}
	};

	/* Triangle mesh */
	class Mesh {
		Tri tri[]; 	// Triangles
		int num;	// Number of triangles
		
		public Mesh() {
			
		}
	};
	
	/* Struct defining a 3D SIFT descriptor */
	class SIFT3D_Descriptor {

		Hist hists[] = new Hist[DESC_NUM_TOTAL_HIST]; // Array of orientation histograms
		double xd, yd, zd, sd;	// sub-pixel [x, y, z], absolute scale
		
		public SIFT3D_Descriptor() {
			for (int i = 0; i < DESC_NUM_TOTAL_HIST; i++) {
				hists[i] = new Hist();
			}
		}

	};
	
	/* Struct to hold SIFT3D descriptors */
	class SIFT3D_Descriptor_store {

		SIFT3D_Descriptor buf[];
		int num;
		int nx, ny, nz;			// Image dimensions
		
		public SIFT3D_Descriptor_store() {
			
		}

	};
	
	/* Struct to hold all parameters and internal data of the 
	 * SIFT3D algorithms */
	class SIFT3DC {

	        // Triange mesh
		Mesh mesh = new Mesh();

	        // Filters for computing the GSS pyramid
		GSS_filters gss = new GSS_filters();

		// Other OpenCL kernels
		//SIFT_cl_kernels kernels;

		// Gaussian pyramid
		Pyramid gpyr = new Pyramid();

		// DoG pyramid
		Pyramid dog = new Pyramid();

		// Image to process
		Image im = new Image();

		// Parameters
		double peak_thresh; // Keypoint peak threshold
		double corner_thresh; // Keypoint corner threshold
	    int dense_rotate; // If true, dense descriptors are rotation-invariant
	    
	    public SIFT3DC() {
	    	
	    }

	};
	
	/* Geometric transformations that can be applied by this library. */
	enum tform_type {
		AFFINE,         // Affine (linear + constant)
		TPS             // Thin-plate spline	
	};

	/* Interpolation algorithms that can be used by this library. */
	enum interp_type {
	        LINEAR,         // N-linear interpolation
	        LANCZOS2        // Lanczos kernel, a = 2
	};
	
	/* Virtual function table for Tform class */
	/*typedef struct _Tform_vtable {

	        int (*copy)(const void *const, void *const);

	        void (*apply_xyz)(const void *const, const double, const double, 
	                const double, double *const, double *const, double *const);

	        int (*apply_Mat_rm)(const void *const, const Mat_rm *const, 
	                Mat_rm *const);

	        size_t (*get_size)(void);

	        int (*write)(const char *, const void *const);
	       
	        void (*cleanup)(void *const);

	} Tform_vtable;*/
	
	/* "Abstract class" of transformations */
	class Tform {
	        tform_type type; // The specific type, e.g. Affine, TPS
	        //const Tform_vtable *vtable; // Table of virtual functions
	        
	        public Tform() {
	        	
	        }
	};
	
	/* Struct to hold an affine transformation */
	class Affine {
	    Tform tform = new Tform();    // Abstract parent class
		Mat_rm A = new Mat_rm();	// Transformation matrix, x' = Ax
		
		public Affine() {
			
		}
	};
	
	/* Struct to hold a thin-plate spline */
	class Tps {
	    Tform tform = new Tform();       // Abstract parent class
		Mat_rm params = new Mat_rm();	// Transformation matrix, dim * number of control point + dim +1
		Mat_rm kp_src = new Mat_rm();	// Control point matrix, number of control point * dim
		int dim; 	// Dimensionality, e.g. 3
		
		public Tps() {
			
		}
	};
	
	/* Struct to hold RANSAC parameters */
	class Ransac {
	 	double err_thresh; //error threshold for RANSAC inliers
		int num_iter; //number of RANSAC iterations
		
		public Ransac() {
			
		}
	};


	
	/**
     * SIFT3D - default constructor.
     */
    public SIFT3D() { }
    
    public void runAlgorithm() {
    	
    }
}
