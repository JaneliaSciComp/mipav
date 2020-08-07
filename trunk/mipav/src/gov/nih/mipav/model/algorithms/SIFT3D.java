package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.Preferences;

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
	
	private double SIFT3D_nn_thresh_default; // Default matching threshold
	private double SIFT3D_err_thresh_default;
	private int SIFT3D_num_iter_default;
	private boolean useOCL = false;

	// Return codes
	private final int SIFT3D_SINGULAR = 1;
	private final int SIFT3D_SUCCESS  = 0;
	private final int SIFT3D_FAILURE = -1;
	private final int SIFT3D_HELP = 1;
	private final int SIFT3D_VERSION = 2;
	
	private final int SIFT3D_FILE_DOES_NOT_EXIST = 1; /* The file does not exist */
	private final int SIFT3D_UNSUPPORTED_FILE_TYPE = 2; /* The file type is not supported */
	private final int SIFT3D_WRAPPER_NOT_COMPILED = 3; /* The file type is supported, but the 
	                                     * wrapper library was not compiled. */
	private final int SIFT3D_UNEVEN_SPACING = 4; /* The image slices are not evenly spaced. */
	private final int SIFT3D_INCONSISTENT_AXES = 5; /* The image slices have inconsistent 
	                                    * axes. */
	private final int SIFT3D_DUPLICATE_SLICES = 6; /* Multiple slices in the same location. */

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
	
	/* Implementation options */
	//#define SIFT3D_ORI_SOLID_ANGLE_WEIGHT // Weight bins by solid angle
	//#define SIFT3D_MATCH_MAX_DIST 0.3 // Maximum distance between matching features 
	//#define CUBOID_EXTREMA // Search for extrema in a cuboid region

	/* Internal return codes */
	private final int REJECT = 1;

	/* Default SIFT3D parameters. These may be overriden by 
	 * the calling appropriate functions. */
	private final double peak_thresh_default = 0.1; // DoG peak threshold
	private final int num_kp_levels_default = 3; // Number of levels per octave in which keypoints are found
	private final double corner_thresh_default = 0.4; // Minimum corner score
	private final double sigma_n_default = 1.15; // Nominal scale of input data
	private final double sigma0_default = 1.6; // Scale of the base octave

	/* SIFT3D option names */
	private final String opt_peak_thresh = "peak_thresh";
	private final String opt_corner_thresh = "corner_thresh";
	private final String opt_num_kp_levels = "num_kp_levels";
	private final String opt_sigma_n = "sigma_n";
	private final String opt_sigma0 = "sigma0";

	/* Internal parameters */
	private final double max_eig_ratio =  0.90;	// Maximum ratio of eigenvalue magnitudes
	private final double ori_grad_thresh = 1E-10;   // Minimum norm of average gradient
	private final double FLT_EPSILON = 1.192092896E-7; // 2**(-23)
	private final double DBL_EPSILON = 2.2204460e-16;
	private final double bary_eps = FLT_EPSILON * 1E1;	// Error tolerance for barycentric coordinates
	private final double ori_sig_fctr = 1.5;        // Ratio of window parameter to keypoint scale
	private final double ori_rad_fctr =  3.0; // Ratio of window radius to parameter
	private final double desc_sig_fctr = 7.071067812; // See ori_sig_fctr, 5 * sqrt(2)
	private final double desc_rad_fctr = 2.0;  // See ori_rad_fctr
	private final double trunc_thresh = 0.2f * 128.0f / DESC_NUMEL; // Descriptor truncation threshold

	/* Internal math constants */
	private final double gr = 1.6180339887; // Golden ratio
	
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
	
	/*#pragma OPENCL EXTENSION cl_khr_3d_image_writes : enable

	sampler_t sampler_downsample_2x = CLK_NORMALIZED_COORDS_FALSE |
					  CLK_ADDRESS_CLAMP_TO_EDGE |
					  CLK_FILTER_NEAEREST;

	kernel void downsample_2x_3d(__read_only image3d_t src,
							 	 __write_only image3d_t dst) {

		int x, y, z;
		float4 out;

		x = get_global_id(0);
		y = get_global_id(1);
		z = get_global_id(2);
		out = read_imagef(src, sampler_downsample_2x, (int4) (x, y, z, 0) * 2);
		write_imagef(dst, (int4) (x, y, z, 0), out);
	}*/
	
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
		double data_double[][];
		float  data_float[][];
		int data_int[][];
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

		double data[];		// Raster of voxel values ~16MB
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
		double kernel[];	// filter weights
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

		double mag;	// Magnitude
		double po;	// Polar angle, [0, pi)
		double az;	// Azimuth angle, [0, 2pi)
		
		public Svec() {
			
		}

	};
	
	/* Struct defining a vector in Cartesian coordinates */
	class Cvec {

		double x;
		double y;
		double z;
		
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

		double r_data[] = new double[IM_NDIMS * IM_NDIMS];	// Memory for matrix R, do not use this
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
		double bins[] = new double[HIST_NUMEL];
		
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
	
	/* Virtual function tables */
	/*const Tform_vtable Affine_vtable = {
		copy_Affine,
		apply_Affine_xyz,
		apply_Affine_Mat_rm,
		Affine_get_size,
		write_Affine,
		cleanup_Affine
	};

	const Tform_vtable Tps_vtable = {
		copy_Tps,
		apply_Tps_xyz,
		apply_Tps_Mat_rm,
		Tps_get_size,
		write_Tps,
		cleanup_Tps
	};*/
	
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

	/* Internal data for the SIFT3D + RANSAC registration process */
	class Reg_SIFT3D {

	        double src_units[] = new double[IM_NDIMS];
	        double ref_units[] = new double[IM_NDIMS];
	        SIFT3DC sift3d;
	        Ransac ran;
	        SIFT3D_Descriptor_store desc_src, desc_ref;
	        Mat_rm match_src, match_ref;
	        double nn_thresh;
	        int verbose;

	};

	
	/**
     * SIFT3D - default constructor.
     */
    public SIFT3D() { }
    
    public SIFT3D(double SIFT3D_nn_thresh_default, double SIFT3D_err_thresh_default,
    		int SIFT3D_num_iter_default, boolean useOCL) {
    	this.SIFT3D_nn_thresh_default = SIFT3D_nn_thresh_default;
    	this.SIFT3D_err_thresh_default = SIFT3D_err_thresh_default;
    	this.SIFT3D_num_iter_default = SIFT3D_num_iter_default;
    	this.useOCL = useOCL;
    }
    
    public void runAlgorithm() {
    	// Example of registering two images.
    	/* This illustrates how to use Reg_SIFT3D within a function, freeing all memory
    	 * afterwards. */
    	int status;
    	
    	Image src = new Image();
    	Image ref = new Image();
    	Image warped = new Image();
        Reg_SIFT3D reg = new Reg_SIFT3D();
        Affine affine = new Affine();
        
        // Initialize the intermediates
        init_im(src);
        init_im(ref);
        init_im(warped);
        status = init_Affine(affine, IM_NDIMS);
	    if (status == SIFT3D_FAILURE) {
	    	setCompleted(false);
	    	return;
	    }
	    
	    status = init_Reg_SIFT3D(reg);
	    if (status == SIFT3D_FAILURE) {
            cleanup_Affine(affine);
            setCompleted(false);
            return;
	    }
    	
    }
    
    /* Initialize the values of im so that it can be used by the
     * resize function. Does not allocate memory. */
    private void init_im(Image im)
    {
    	im.data = null;
    	im.cl_valid = SIFT3D_FALSE;

    	im.ux = 1;
    	im.uy = 1;
    	im.uz = 1;

    	im.size = 0;
    	im.s = -1.0;
    	im.nx = 0;
    	im.ny = 0;
    	im.nz = 0;
    	im.xs = 0;
    	im.ys = 0;
    	im.zs = 0;
    }
    
    /* Initialize an Affine struct. This initializes
     * all fields, and allocates memory for the inner
     * matrix, initializing it to zero. */
    private int init_Affine(Affine affine, int dim)
    {
    	int status;

    	// Verify inputs
    	if (dim < 2)
    		return SIFT3D_FAILURE;

    	// Initialize the type
    	affine.tform.type = tform_type.AFFINE;

    	// Initialize the vtable
    	//affine.tform.vtable = Affine_vtable;

    	// Initialize the matrix
    	status = init_Mat_rm(affine.A, dim, dim + 1, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_TRUE);

    	return status;
    } 
    
    /* Shortcut function to initalize a matrix.
     * 
     * Parameters:
     *      mat - The matrix to be initialized
     *      num_rows - The number of rows
     *      num_cols - The number of columns
     *      type - The data type 
     *      set_zero - If true, initializes the elements to zero.
     *
     * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
    private int init_Mat_rm(Mat_rm mat, int num_rows, int num_cols,
                    Mat_rm_type type, int set_zero) {
            int status;
            mat.type = type;
            mat.num_rows = num_rows;
            mat.num_cols = num_cols;
            mat.data_double = null;
            mat.size = 0;
            mat.static_mem = SIFT3D_FALSE;

            status = resize_Mat_rm(mat);
            if (status == SIFT3D_FAILURE) {
                    return SIFT3D_FAILURE;
            }
            
            if (set_zero == SIFT3D_TRUE) {
            	status = zero_Mat_rm(mat);
                if (status == SIFT3D_FAILURE) { 
                    return SIFT3D_FAILURE;
                }
            }
    
            return SIFT3D_SUCCESS;
    }
    
    /* Re-sizes a matrix. The following fields
     * must already be initialized:
     * -num_rows
     * -num_cols
     * -type
     * -u.data_* (NULL for first use, non-null for resize)
     *
     * The following fields will be modified:
     * -size
     * -u.data_* (Change is not guaranteed)
     * 
     * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise.
     */
    private int resize_Mat_rm(Mat_rm mat) {

        int type_size, total_size;

        final int num_rows = mat.num_rows;
        final int num_cols = mat.num_cols;
        double data[][] = mat.data_double;
        final int numel = num_rows * num_cols;
        final Mat_rm_type type = mat.type;

        // Get the size of the underyling datatype
        switch (type) {
            case SIFT3D_DOUBLE:
                type_size = 8;
                break;
            case SIFT3D_FLOAT:
                type_size = 4;
                break;
            case SIFT3D_INT:
                type_size = 4;
                break;
            default:
                System.err.println("resize_Mat_rm: unknown type! \n");
    		    return SIFT3D_FAILURE;
    	}

        // Calculate the new size in bytes
        total_size = type_size * numel;

        // Do nothing if the size has not changed
        if (total_size == mat.size)
            return SIFT3D_SUCCESS;
        mat.size = total_size;

        // Check for static reallocation
        if (mat.static_mem != 0) {
            System.err.println("resize_Mat_rm: illegal re-allocation of static matrix \n");
            return SIFT3D_FAILURE;
        }

        // Reset if the new size is 0 
        if (total_size == 0) {
    	cleanup_Mat_rm(mat);
    	return init_Mat_rm(mat, num_rows, num_cols, type, SIFT3D_FALSE);
        }

        // Re-allocate the memory
        try {
        	data = new double[num_rows][num_cols];
        }
        catch (OutOfMemoryError e) {
            mat.size = 0;
            return SIFT3D_FAILURE;
        }

        return SIFT3D_SUCCESS;
    }
    
    /* De-allocate the memory for a Mat_rm struct, unless it was initialized in
     * static mode. */
    private void cleanup_Mat_rm(Mat_rm mat) {

        if (mat.data_double == null)
            return;

        if (mat.static_mem == 0) {
        	for (int r = 0; r < mat.data_double.length; r++) {
        		mat.data_double[r] = null;
        	}
        	mat.data_double = null;
        }
    }
    
    private int zero_Mat_rm(Mat_rm mat) {
    	final Mat_rm_type type = mat.type;
    	final int num_rows = mat.num_rows;
        final int num_cols = mat.num_cols;
        int r,c;

        // Get the size of the underyling datatype
        switch (type) {
            case SIFT3D_DOUBLE:
                if (mat.data_double != null) {
                	for (r = 0; r < num_rows; r++) {
                		for (c = 0; c < num_cols; c++) {
                			mat.data_double[r][c] = 0.0;
                		}
                	}
                }
                return SIFT3D_SUCCESS;
            case SIFT3D_FLOAT:
            	if (mat.data_float != null) {
                	for (r = 0; r < num_rows; r++) {
                		for (c = 0; c < num_cols; c++) {
                			mat.data_float[r][c] = 0.0f;
                		}
                	}
                }
                return SIFT3D_SUCCESS;
            case SIFT3D_INT:
            	if (mat.data_int != null) {
                	for (r = 0; r < num_rows; r++) {
                		for (c = 0; c < num_cols; c++) {
                			mat.data_int[r][c] = 0;
                		}
                	}
                }
                return SIFT3D_SUCCESS;
            default:
                System.err.println("zero_Mat_rm: unknown type! \n");
    		    return SIFT3D_FAILURE;
    	}	
    }
    
    /* Free the memory associated with an Affine transformation. */
    private void cleanup_Affine(Affine aff)
    {

    	cleanup_Mat_rm(aff.A);
    }
    
    /* Initialize a Reg_SIFT3D struct with the default parameters. This must be
     * called before the struct can be used. */
    private int init_Reg_SIFT3D(Reg_SIFT3D reg) {
        int status;
        reg.nn_thresh = SIFT3D_nn_thresh_default;
    	init_SIFT3D_Descriptor_store(reg.desc_src);
    	init_SIFT3D_Descriptor_store(reg.desc_ref);
    	init_Ransac(reg.ran);
    	status = init_SIFT3D(reg.sift3d);
    	if (status == SIFT3D_FAILURE) {
    		System.err.println("init_Reg_SIFT3D: Unexpected error");
    	    return SIFT3D_FAILURE;	
    	}
    	status = init_Mat_rm(reg.match_src, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
    	if (status == SIFT3D_FAILURE) {
    		System.err.println("init_Reg_SIFT3D: Unexpected error");
    	    return SIFT3D_FAILURE;	
    	}
        status = init_Mat_rm(reg.match_ref, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
        if (status == SIFT3D_FAILURE) {
    		System.err.println("init_Reg_SIFT3D: Unexpected error");
    	    return SIFT3D_FAILURE;	
    	}
        return SIFT3D_SUCCESS;
    }
    
    /* Initialize a SIFT_Descriptor_store for first use.
     * This does not need to be called to reuse the store
     * for a new image. */
    private void init_SIFT3D_Descriptor_store(SIFT3D_Descriptor_store desc) {
    	desc.buf = null;
    }
    
    /* Initialize a RANSAC struct with the default parameters */
    private void init_Ransac(Ransac ran)
    {
    	ran.err_thresh = SIFT3D_err_thresh_default;
    	ran.num_iter = SIFT3D_num_iter_default;
    }
    
    private int init_SIFT3D(SIFT3DC sift3d) {
    	int status;

        Pyramid dog = sift3d.dog;
        Pyramid gpyr = sift3d.gpyr;
        GSS_filters gss = sift3d.gss;

	    // Initialize to defaults
	    final double peak_thresh = peak_thresh_default;
	    final double corner_thresh = corner_thresh_default;
	    final int num_kp_levels = num_kp_levels_default;
	    final double sigma_n = sigma_n_default;
	    final double sigma0 = sigma0_default;
        final int dense_rotate = SIFT3D_FALSE;

	    // First-time pyramid initialization
        init_Pyramid(dog);
        init_Pyramid(gpyr);

        // First-time filter initialization
        init_GSS_filters(gss);

        // Intialize the geometry tables
	    status = init_geometry(sift3d);
	    if (status == SIFT3D_FAILURE) {
		    return SIFT3D_FAILURE;
	    }

	    // init static OpenCL programs and contexts, if support is enabled
	    status = init_cl_SIFT3D(sift3d);
	    if (status == SIFT3D_FAILURE) {
		    return SIFT3D_FAILURE;
	    }

	    // Initialize the image data
	    init_im(sift3d.im);

	    // Save data
	    dog.first_level = gpyr.first_level = -1;
        sift3d.dense_rotate = dense_rotate;
        status = set_sigma_n_SIFT3D(sift3d, sigma_n);
        if (status == SIFT3D_FAILURE) {
        	return SIFT3D_FAILURE;
        }
                /*set_sigma0_SIFT3D(sift3d, sigma0) ||
                set_peak_thresh_SIFT3D(sift3d, peak_thresh) ||
                set_corner_thresh_SIFT3D(sift3d, corner_thresh) ||
                set_num_kp_levels_SIFT3D(sift3d, num_kp_levels))
                return SIFT3D_FAILURE;*/

	return SIFT3D_SUCCESS;
    }
    
    /* Initialize a Pyramid for use. Must be called before a Pyramid can be used
     * in any other functions. */
    private void init_Pyramid(Pyramid pyr)
    {
    	pyr.levels = null;
        pyr.first_level = 0;
    	pyr.num_levels = pyr.num_kp_levels = 0;
    	pyr.first_octave = 0;
    	pyr.num_octaves = 0;
        pyr.sigma0 = pyr.sigma_n = 0.0;
    }
    
    /* Initialize a GSS filters stuct. This must be called before gss can be
     * used in any other functions. */
    private void init_GSS_filters(GSS_filters gss)
    {
    	gss.num_filters = -1;
    	gss.gauss_octave = null;
    }
    
    /* Initialize geometry tables. */
    private int init_geometry(SIFT3DC sift3d) {

    	Mat_rm V = new Mat_rm();
    	Mat_rm F = new Mat_rm();
    	Cvec temp1 = new Cvec();
    	Cvec temp2 = new Cvec();
    	Cvec temp3 = new Cvec();
    	Cvec n = new Cvec();
    	double mag;
    	int i, j;
    	int status;

    	Mesh mesh = sift3d.mesh;

    	/* Verices of a regular icosahedron inscribed in the unit sphere. */
    	final double vert[][] = {  {0,  1,  gr},
    			        {0, -1,  gr},
    			        {0,  1, -gr},
    			        {0, -1, -gr},
    			        {1,  gr,  0},
    			       {-1,  gr,  0},
    			        {1, -gr,  0},
    			       {-1, -gr,  0},
    			       {gr,   0,  1},
    			      {-gr,   0,  1},
    			       {gr,   0, -1}, 
    			      {-gr,   0, -1} }; 

    	/* Vertex triplets forming the faces of the icosahedron. */
    	final double faces[][] = {{0, 1, 8},
        			       {0, 8, 4},
        			       {0, 4, 5},
        			       {0, 5, 9},
        			       {0, 9, 1},
        			       {1, 6, 8},
    			       {8, 6, 10},
    			       {8, 10, 4},
    			       {4, 10, 2},
    			       {4, 2, 5},
    			       {5, 2, 11},
    			       {5, 11, 9},
    			       {9, 11, 7},
    			       {9, 7, 1},
    			       {1, 7, 6},
    			       {3, 6, 7},
    			       {3, 7, 11},
    			       {3, 11, 2},
    			       {3, 2, 10},
    			       {3, 10, 6}};

    	// Initialize matrices
    	status = init_Mat_rm_p(V, vert, ICOS_NVERT, 3, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
    	if (status == SIFT3D_FAILURE) {
    		return SIFT3D_FAILURE;
    	}
    	status = init_Mat_rm_p(F, faces, ICOS_NFACES, 3, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
    	if (status == SIFT3D_FAILURE) {
    		return SIFT3D_FAILURE;
    	}
    			    
    	// Initialize triangle memory
            init_Mesh(mesh);
    	try {
            mesh.tri = new Tri[ICOS_NFACES];
    	}
    	catch (OutOfMemoryError e) {
    		return SIFT3D_FAILURE;
    	}
   
     
    	// Populate the triangle struct for each face
    	for (i = 0; i < ICOS_NFACES; i++) {
            mesh.tri[i] = new Tri();
    		Tri tri = mesh.tri[i];	
    		Cvec v[] = tri.v;

    		// Initialize the vertices
    		for (j = 0; j < 3; j++) {

    			double mag_expected = Math.sqrt(1 + gr * gr);

    			tri.idx[j] = (int)(F.data_double[i][j]);

    			// Initialize the vector
    			v[j].x = V.data_double[tri.idx[j]][0];
    			v[j].y = V.data_double[tri.idx[j]][1];
    			v[j].z = V.data_double[tri.idx[j]][2];

    			// Normalize to unit length
    			mag = SIFT3D_CVEC_L2_NORM(v[j]);
    			if (Math.abs(mag - mag_expected) >= 1.0E-10) {
    				System.err.println("Math.abs(mag - mag_expected) >= 1.0E-10 in init_geometry");
    				return SIFT3D_FAILURE;
    			}
    			SIFT3D_CVEC_SCALE(v[j], 1.0f / mag);
    		}

    		// Compute the normal vector at v[0] as  (V2 - V1) X (V1 - V0)
    		SIFT3D_CVEC_MINUS(v[2], v[1], temp1);
    		SIFT3D_CVEC_MINUS(v[1], v[0], temp2);
    		SIFT3D_CVEC_CROSS(temp1, temp2, n);

    		// Ensure this vector is facing outward from the origin
    		if (SIFT3D_CVEC_DOT(n, v[0]) < 0) {
    			// Swap two vertices
    			temp1 = v[0];
    			v[0] = v[1];
    			v[1] = temp1;

    			// Compute the normal again
    			SIFT3D_CVEC_MINUS(v[2], v[1], temp1);
    			SIFT3D_CVEC_MINUS(v[1], v[0], temp2);
    			SIFT3D_CVEC_CROSS(temp1, temp2, n);
    		}
    		if (SIFT3D_CVEC_DOT(n, v[0]) < 0) {
    			System.err.println("SIFT3D_CVEC_DOT(n, v[0]) < 0 in init_geometry");
				return SIFT3D_FAILURE;	
    		}

    		// Ensure the triangle is equilateral
    		SIFT3D_CVEC_MINUS(v[2], v[0], temp3);
    		if (Math.abs(SIFT3D_CVEC_L2_NORM(temp1) - SIFT3D_CVEC_L2_NORM(temp2)) >= 1E-10) {
    			System.err.println("Math.abs(SIFT3D_CVEC_L2_NORM(temp1) - SIFT3D_CVEC_L2_NORM(temp2)) >= 1E-10 in init_geometry");
				return SIFT3D_FAILURE;		
    		}
    		if (Math.abs(SIFT3D_CVEC_L2_NORM(temp1) - SIFT3D_CVEC_L2_NORM(temp3)) >= 1E-10) {
    			System.err.println("Math.abs(SIFT3D_CVEC_L2_NORM(temp1) - SIFT3D_CVEC_L2_NORM(temp3)) >= 1E-10 in init_geometry");
				return SIFT3D_FAILURE;		
    		}
    	}
    	
    	return SIFT3D_SUCCESS;
    }

    /* As init_Mat_rm, but aliases data memory with pointer p. The flag 
     * mat->static_mem is set, and the matrix does not need to be freed with 
     * cleanup_Mat_rm. But, an error will be thrown if the user attempts to resize
     * the memory. That is, resize_Mat_rm will only return success if the size of 
     * the matrix does not change. */ 
    private int init_Mat_rm_p(Mat_rm mat, double p[][], int num_rows, 
                      int num_cols, Mat_rm_type type, 
                      int set_zero) {
    	    int status;

            // Perform normal initialization
            status = init_Mat_rm(mat, num_rows, num_cols, type, set_zero);
            if (status == SIFT3D_FAILURE) {
                    return SIFT3D_FAILURE;
            }

            // Clean up any existing memory
            cleanup_Mat_rm(mat);

            // Alias with provided memory and set the static flag
            mat.data_double = p;
            mat.static_mem = SIFT3D_TRUE;

            // Optionally set to zero 
            if (set_zero != 0) {
            	status = zero_Mat_rm(mat);
            	if (status == SIFT3D_FAILURE) {
            		return SIFT3D_FAILURE;
            	}
            }

            return SIFT3D_SUCCESS;
    }
    
    /* Initialize a triangle mesh for first use. This must be called before mesh
     * can be used in any other functions. */
    private void init_Mesh(Mesh mesh)
    {
    	mesh.tri = null;
    	mesh.num = -1;
    }
    
    // Return the L2 norm of a Cartesian coordinate vector
    private double SIFT3D_CVEC_L2_NORM(Cvec cvec) {
    	return Math.sqrt(cvec.x * cvec.x + cvec.y * cvec.y +
    	cvec.z * cvec.z);
    }
    
    // Scale a Cartesian coordinate vector by a constant factor
    private void SIFT3D_CVEC_SCALE(Cvec cvec, double a) {
        cvec.x = cvec.x * a;
        cvec.y = cvec.y * a;
        cvec.z = cvec.z * a;
    }
    
    // Operate element-wise on two Cartesian coordinate vectors, cc = ca - cb
    private void SIFT3D_CVEC_MINUS(Cvec ca, Cvec cb, Cvec cc) {
        cc.x = ca.x - cb.x;
        cc.y = ca.y - cb.y;
        cc.z = ca.z - cb.z;
    }
    
	 // Take the cross product of two Cartesian coordinate
	 // vectors, as out = in1 X in2
	 private void SIFT3D_CVEC_CROSS(Cvec in1, Cvec in2, Cvec out) { 
	 	out.x = in1.y * in2.z - in1.z * in2.y; 
	 	out.y = in1.z * in2.x - in1.x * in2.z; 
	 	out.z = in1.x * in2.y - in1.y * in2.x; 
	 } 
	 
	// Return the dot product of two Cartesian coordinate 
	// vectors
	private double SIFT3D_CVEC_DOT(Cvec in1, Cvec in2) {
		return (in1.x * in2.x + in1.y * in2.y + in1.z * in2.z);
	}
	
	/* Initializes the OpenCL data for this SIFT3D struct. This
	 * increments the reference counts for shared data. */
	private int init_cl_SIFT3D(SIFT3DC sift3d) {
		if (useOCL && (Preferences.isGpuCompEnabled() && OpenCLAlgorithmBase.isOCLAvailable()) ) {
		/*cl_image_format image_format;

		// Initialize basic OpenCL platform and context info
		image_format.image_channel_order = CL_R;
		image_format.image_channel_data_type = CL_FLOAT;
		if (init_cl(&cl_data, PLATFORM_NAME_NVIDIA, CL_DEVICE_TYPE_GPU,
	 		    CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR, 
	                    image_format))
			return SIFT3D_FAILURE;

		// Load and compile the downsampling kernel
        */
		}
		return SIFT3D_SUCCESS;
	}

	/* Sets the nominal scale parameter of the input data, checking that it is 
	 * nonnegative. */
	private int set_sigma_n_SIFT3D(SIFT3DC sift3d, double sigma_n) {

	        final double sigma0 = sift3d.gpyr.sigma0;

	        if (sigma_n < 0.0) {
	                System.err.println("SIFT3D sigma_n must be nonnegative. Provided sigma_n : " + sigma_n);
	                return SIFT3D_FAILURE;
	        }

	        return set_scales_SIFT3D(sift3d, sigma0, sigma_n);
	}
	
	/* Helper function to set the scale parameters for a SIFT3D struct. */
	private int set_scales_SIFT3D(SIFT3DC sift3d, double sigma0, double sigma_n) {
            int status;
	        Pyramid gpyr = sift3d.gpyr;
	        Pyramid dog = sift3d.dog;
	        GSS_filters gss = sift3d.gss;

	        // Set the scales for the GSS and DOG pyramids
	        status = set_scales_Pyramid(sigma0, sigma_n, gpyr);
	        if (status == SIFT3D_FAILURE) {
	        	return SIFT3D_FAILURE;
	        }
	        status = set_scales_Pyramid(sigma0, sigma_n, dog);
	        if (status == SIFT3D_FAILURE) {
	            return SIFT3D_FAILURE;
	        }

	        // Do nothing more if we have no image
	        if (sift3d.im.data == null) {
	            return SIFT3D_SUCCESS;
	        }

	        // Recompute the filters
		    return make_gss(gss, gpyr);
	}
	
	/* Set the scale-space parameters on a Pyramid struct. Operates on all levels
	 * of the pyramid. This function is called automatically by resize_Pyramid.
	 *
	 * Parameters:
	 *  -sigma0: The scale parameter of level 0, octave 0
	 *  -sigma_n: The nominal scale parameter of images being transfomed into
	 *      this pyramid struct. 
	 *  -Pyr: The Pyramid to be modified. */
	private int set_scales_Pyramid(double sigma0, double sigma_n, Pyramid pyr) {

	        int o, s;

	        final int num_kp_levels = pyr.num_kp_levels;
	        final Image first_level = 
	                SIFT3D_PYR_IM_GET(pyr, pyr.first_octave, pyr.first_level);

	        // Compute the scales of each level
	        // Loop through all levels of a given pyramid
        	for (o = pyr.first_octave; o <= SIFT3D_PYR_LAST_OCTAVE(pyr); 
                        o++) { 
	        	for (s = pyr.first_level; s <= SIFT3D_PYR_LAST_LEVEL(pyr); 
	                        s++) {

	                // Compute the scale 
	                Image level = SIFT3D_PYR_IM_GET(pyr, o, s);
	                final double scale = 
	                        sigma0 * Math.pow(2.0, o + (double) s / num_kp_levels);

	                // Verify that sigma_n is not too large
	                if (o == pyr.first_octave && s == pyr.first_level && 
	                        scale < sigma_n) {
	                        System.err.println("set_scales_Pyramid: sigma_n too large\n"+
	                                "for these settings. Max allowed: " + (scale - DBL_EPSILON));
	                        return SIFT3D_FAILURE;
	                }

	                // Save the scale
	                level.s = scale;
	        	}
        	}

	        // Store the parameters
	        pyr.sigma0 = sigma0;
	        pyr.sigma_n = sigma_n;

	        return SIFT3D_SUCCESS;
	}
	
	// Get a pointer to an image struct at pyramid level [o, s]
	private Image SIFT3D_PYR_IM_GET(Pyramid pyr, int o, int s) {
		 return pyr.levels[(o - pyr.first_octave) *
							pyr.num_levels + (s - pyr.first_level)];
	}
	
	// Get the index of the last octave of a Pyramid struct
	private int SIFT3D_PYR_LAST_OCTAVE(Pyramid pyr) {
        return (pyr.first_octave + pyr.num_octaves - 1);
	}

	// Get the index of the last level of a Pyramid struct
	private int SIFT3D_PYR_LAST_LEVEL(Pyramid pyr) {
	    return (pyr.first_level + pyr.num_levels - 1);
	}
	
	/* Create GSS filters to create the given scale-space 
	 * pyramid. */
	int make_gss(GSS_filters gss, Pyramid pyr)
	{

		Image cur, next;
		int o, s, i;

		final int dim = 3;

		final int num_filters = pyr.num_levels - 1;
		final int first_level = pyr.first_level;
		final int last_level = SIFT3D_PYR_LAST_LEVEL(pyr);

		// Verify inputs
		if (num_filters < 1) {
			System.err.println("make_gss: pyr has only " + pyr.num_levels + " levels, must have at least 2");
			return SIFT3D_FAILURE;
		}

		// Free all previous data, if any
		cleanup_GSS_filters(gss);
		init_GSS_filters(gss);

		// Copy pyramid parameters
		gss.num_filters = num_filters;
		gss.first_level = first_level;

		// Allocate the filter array (num_filters cannot be zero)
		gss.gauss_octave = new Gauss_filter[num_filters];
		for (i = 0; i < num_filters; i++) {
			gss.gauss_octave[i] = new Gauss_filter();
		}

		// Make the filter for the very first blur
		next = SIFT3D_PYR_IM_GET(pyr, pyr.first_octave, first_level);
		/*if (init_Gauss_incremental_filter(&gss->first_gauss, pyr->sigma_n,
						  next->s, dim))
			return SIFT3D_FAILURE;

		// Make one octave of filters (num_levels - 1)
		o = pyr->first_octave;
		for (s = first_level; s < last_level; s++) {
			cur = SIFT3D_PYR_IM_GET(pyr, o, s);
			next = SIFT3D_PYR_IM_GET(pyr, o, s + 1);
			if (init_Gauss_incremental_filter(SIFT3D_GAUSS_GET(gss, s),
							  cur->s, next->s, dim))
				return SIFT3D_FAILURE;
		}*/

		return SIFT3D_SUCCESS;
	}
	
	/* Free all memory associated with the GSS filters. gss cannot be reused
	 * unless it is reinitialized. */
	private void cleanup_GSS_filters(GSS_filters gss)
	{

		int i;

		final int num_filters = gss.num_filters;

		// We are done if gss has no filters
		if (num_filters < 1)
			return;

		// Free the first filter
		cleanup_Gauss_filter(gss.first_gauss);

		// Free the octave filters
		for (i = 0; i < num_filters; i++) {
			Gauss_filter g = gss.gauss_octave[i];
			cleanup_Gauss_filter(g);
		}

		// Free the octave filter buffer
		gss.gauss_octave = null;
	}
	
	/* Free a Gauss_filter */
	private void cleanup_Gauss_filter(Gauss_filter gauss)
	{
		cleanup_Sep_FIR_filter(gauss.f);
	}
	
	/* Free a Sep_FIR_Filter. */
	private void cleanup_Sep_FIR_filter(Sep_FIR_filter f)
	{

		if (f.kernel != null) {
			f.kernel = null;
		}
	}

}
