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
	
	private double SIFT3D_nn_thresh_default; // Default matching threshold
	private double SIFT3D_err_thresh_default;
	private int SIFT3D_num_iter_default;

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
    		int SIFT3D_num_iter_default) {
    	this.SIFT3D_nn_thresh_default = SIFT3D_nn_thresh_default;
    	this.SIFT3D_err_thresh_default = SIFT3D_err_thresh_default;
    	this.SIFT3D_num_iter_default = SIFT3D_num_iter_default;
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
    void init_im(Image im)
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
    int init_Affine(Affine affine, int dim)
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
    int init_Mat_rm(Mat_rm mat, int num_rows, int num_cols,
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
    int resize_Mat_rm(Mat_rm mat) {

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
    void cleanup_Mat_rm(Mat_rm mat) {

        if (mat.data_double == null)
            return;

        if (mat.static_mem == 0) {
        	for (int r = 0; r < mat.data_double.length; r++) {
        		mat.data_double[r] = null;
        	}
        	mat.data_double = null;
        }
    }
    
    int zero_Mat_rm(Mat_rm mat) {
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
    void cleanup_Affine(Affine aff)
    {

    	cleanup_Mat_rm(aff.A);
    }
    
    /* Initialize a Reg_SIFT3D struct with the default parameters. This must be
     * called before the struct can be used. */
    int init_Reg_SIFT3D(Reg_SIFT3D reg) {
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
    void init_SIFT3D_Descriptor_store(SIFT3D_Descriptor_store desc) {
    	desc.buf = null;
    }
    
    /* Initialize a RANSAC struct with the default parameters */
    void init_Ransac(Ransac ran)
    {
    	ran.err_thresh = SIFT3D_err_thresh_default;
    	ran.num_iter = SIFT3D_num_iter_default;
    }
    
    int init_SIFT3D(SIFT3DC sift3d) {
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
	/*if (init_cl_SIFT3D(sift3d))
		return SIFT3D_FAILURE;

	// Initialize the image data
	init_im(&sift3d->im);

	// Save data
	dog->first_level = gpyr->first_level = -1;
        sift3d->dense_rotate = dense_rotate;
        if (set_sigma_n_SIFT3D(sift3d, sigma_n) ||
                set_sigma0_SIFT3D(sift3d, sigma0) ||
                set_peak_thresh_SIFT3D(sift3d, peak_thresh) ||
                set_corner_thresh_SIFT3D(sift3d, corner_thresh) ||
                set_num_kp_levels_SIFT3D(sift3d, num_kp_levels))
                return SIFT3D_FAILURE;*/

	return SIFT3D_SUCCESS;
    }
    
    /* Initialize a Pyramid for use. Must be called before a Pyramid can be used
     * in any other functions. */
    void init_Pyramid(Pyramid pyr)
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
    void init_GSS_filters(GSS_filters gss)
    {
    	gss.num_filters = -1;
    	gss.gauss_octave = null;
    }
    
    /* Initialize geometry tables. */
    private int init_geometry(SIFT3DC sift3d) {

    	Mat_rm V = new Mat_rm();
    	Mat_rm F = new Mat_rm();
    	Cvec temp1, temp2, temp3, n;
    	float mag;
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
    	/*for (i = 0; i < ICOS_NFACES; i++) {
            mesh.tri[i] = new Tri();
    		Tri tri = mesh.tri[i];	
    		Cvec v[] = tri.v;

    		// Initialize the vertices
    		for (j = 0; j < 3; j++) {

    			double mag_expected = Math.sqrt(1 + gr * gr);

    			tri.idx[j] = (int)(F.data_double[i][j]);

    			// Initialize the vector
    			v[j].x = (int)(V.data_double[tri.idx[j]][0]);
    			v[j].y = (int)(V.data_double[tri.idx[j]][1]);
    			v[j].z = (int)(V.data_double[tri.idx[j]][2]);;

    			// Normalize to unit length
    			mag = SIFT3D_CVEC_L2_NORM(v + j);
    			assert(fabsf(mag - mag_expected) < 1E-10);
    			SIFT3D_CVEC_SCALE(v + j, 1.0f / mag);
    		}

    		// Compute the normal vector at v[0] as  (V2 - V1) X (V1 - V0)
    		SIFT3D_CVEC_OP(v + 2, v + 1, -, &temp1);
    		SIFT3D_CVEC_OP(v + 1, v, -, &temp2);
    		SIFT3D_CVEC_CROSS(&temp1, &temp2, &n);

    		// Ensure this vector is facing outward from the origin
    		if (SIFT3D_CVEC_DOT(&n, v) < 0) {
    			// Swap two vertices
    			temp1 = v[0];
    			v[0] = v[1];
    			v[1] = temp1;

    			// Compute the normal again
    			SIFT3D_CVEC_OP(v + 2, v + 1, -, &temp1);
    			SIFT3D_CVEC_OP(v + 1, v, -, &temp2);
    			SIFT3D_CVEC_CROSS(&temp1, &temp2, &n);
    		}
    		assert(SIFT3D_CVEC_DOT(&n, v) >= 0);

    		// Ensure the triangle is equilateral
    		SIFT3D_CVEC_OP(v + 2, v, -, &temp3);
    		assert(fabsf(SIFT3D_CVEC_L2_NORM(&temp1) - 
                            SIFT3D_CVEC_L2_NORM(&temp2)) < 1E-10);
    		assert(fabsf(SIFT3D_CVEC_L2_NORM(&temp1) - 
                            SIFT3D_CVEC_L2_NORM(&temp3)) < 1E-10);
    	}*/	
    	
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

}
