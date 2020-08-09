package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
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
	/** This image is to registered to the reference image. */
    private ModelImage inputImage;
    /** The inputImage will be registered to this reference image. */
    private ModelImage refImage;
	private double SIFT3D_nn_thresh_default; // Default matching threshold
	private double SIFT3D_err_thresh_default;
	private int SIFT3D_num_iter_default;
	private boolean useOCL = false;
	private double SIFT3D_GAUSS_WIDTH_FCTR = 3.0;
	// Set SIFT3D_MATCH_MAX_DIST <= 0.0 to avoid using in int match_desc()
	private double SIFT3D_MATCH_MAX_DIST = 0.0;
	//private double SIFT3D_MATCH_MAX_DIST = 0.3; // Maximum distance between matching features 

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
	private boolean ICOS_HIST = true;  // Icosahedral gradient histogram

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
    
    /**
     * @param  imageA        Reference image (register input image to reference image).
     * @param  imageB        Input image (register input image to reference image).
     * @param SIFT3D_nn_thresh_default
     * @param SIFT3D_err_thresh_default
     * @param SIFT3D_num_iter_default
     * @param useOCL
     * @param SIFT3D_GAUSS_WIDTH_FCTR
     */
    public SIFT3D(ModelImage imageA, ModelImage imageB,
    		double SIFT3D_nn_thresh_default, double SIFT3D_err_thresh_default,
    		int SIFT3D_num_iter_default, boolean useOCL, double SIFT3D_GAUSS_WIDTH_FCTR,
    		double SIFT3D_MATCH_MAX_DIST, boolean ICOS_HIST) {
    	super(null, imageB);
        refImage = imageA;
        inputImage = imageB;
    	this.SIFT3D_nn_thresh_default = SIFT3D_nn_thresh_default;
    	this.SIFT3D_err_thresh_default = SIFT3D_err_thresh_default;
    	this.SIFT3D_num_iter_default = SIFT3D_num_iter_default;
    	this.useOCL = useOCL;
    	this.SIFT3D_GAUSS_WIDTH_FCTR = SIFT3D_GAUSS_WIDTH_FCTR;
    	this.SIFT3D_MATCH_MAX_DIST = SIFT3D_MATCH_MAX_DIST;
    	this.ICOS_HIST = ICOS_HIST;
    	
    	// The number of elements in a gradient histogram
    	if (ICOS_HIST) {
    	    HIST_NUMEL = ICOS_NVERT;
    	}
    	else {
    	    HIST_NUMEL = (NBINS_AZ * NBINS_PO);
    	}
    	
    	DESC_NUMEL = DESC_NUM_TOTAL_HIST * HIST_NUMEL;
    }
    
    public void runAlgorithm() {
    	// Example of registering two images.
    	/* This illustrates how to use Reg_SIFT3D within a function, freeing all memory
    	 * afterwards. */
    	int status;
    	int t;
    	int i;
    	
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
	    
	    // Read the images
	    // nifti.c and dicom.cpp does not support color reading
	    // Check the dimensionality. 4D is interpreted as a 3D array with
        // multiple channels.
	    src.ux = (double)inputImage.getFileInfo()[0].getResolutions()[0];
	    src.uy = (double)inputImage.getFileInfo()[0].getResolutions()[1];
	    src.uz = (double)inputImage.getFileInfo()[0].getResolutions()[2];
	    src.nx = inputImage.getExtents()[0];
	    src.ny = inputImage.getExtents()[1];
	    src.nz = inputImage.getExtents()[2];
	    src.nc = 1;
	    if (inputImage.getNDims() == 4) {
	    	src.nc = inputImage.getExtents()[3];
	    }
	    int srcVolume = src.nx * src.ny * src.nz;
	    im_default_stride(src);
		im_resize(src);
		if (src.nc == 1) {
			try {
				inputImage.exportData(0, srcVolume, src.data);
			}
			catch(IOException e) {
				// Clean up
		        im_free(src);
		        im_free(ref);
		        im_free(warped);
		        cleanup_Reg_SIFT3D(reg);
		        cleanup_Affine(affine);
				setCompleted(false);
				return;
			}
		}
		else {
			double data2[] = new double[srcVolume];
			for (t = 0; t < src.nc; t++) {
				try {
				    inputImage.exportData(t*srcVolume, srcVolume, data2);
				}
				catch(IOException e) {
					// Clean up
			        im_free(src);
			        im_free(ref);
			        im_free(warped);
			        cleanup_Reg_SIFT3D(reg);
			        cleanup_Affine(affine);
			        data2 = null;
					setCompleted(false);
					return;
				}
				for (i = 0; i < srcVolume; i++) {
					src.data[i*src.nc + t] = data2[i];
				}
			}
			data2 = null;
		}
		
		ref.ux = (double)refImage.getFileInfo()[0].getResolutions()[0];
	    ref.uy = (double)refImage.getFileInfo()[0].getResolutions()[1];
	    ref.uz = (double)refImage.getFileInfo()[0].getResolutions()[2];
	    ref.nx = refImage.getExtents()[0];
	    ref.ny = refImage.getExtents()[1];
	    ref.nz = refImage.getExtents()[2];
	    ref.nc = 1;
	    if (refImage.getNDims() == 4) {
	    	ref.nc = refImage.getExtents()[3];
	    }
	    int refVolume = ref.nx * ref.ny * ref.nz;
	    im_default_stride(ref);
		im_resize(ref);
		if (ref.nc == 1) {
			try {
				refImage.exportData(0, refVolume, ref.data);
			}
			catch(IOException e) {
				// Clean up
		        im_free(src);
		        im_free(ref);
		        im_free(warped);
		        cleanup_Reg_SIFT3D(reg);
		        cleanup_Affine(affine);
				setCompleted(false);
				return;
			}
		}
		else {
			double data2[] = new double[refVolume];
			for (t = 0; t < ref.nc; t++) {
				try {
				    refImage.exportData(t*refVolume, refVolume, data2);
				}
				catch(IOException e) {
					// Clean up
			        im_free(src);
			        im_free(ref);
			        im_free(warped);
			        cleanup_Reg_SIFT3D(reg);
			        cleanup_Affine(affine);
			        data2 = null;
					setCompleted(false);
					return;
				}
				for (i = 0; i < refVolume; i++) {
					ref.data[i*ref.nc + t] = data2[i];
				}
			}
			data2 = null;
		}
		
		// Match features and solve for an affine transformation
        status = register_SIFT3D(reg, affine);
        if (status == SIFT3D_FAILURE) {
        	// Clean up
	        im_free(src);
	        im_free(ref);
	        im_free(warped);
	        cleanup_Reg_SIFT3D(reg);
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
        status = set_sigma0_SIFT3D(sift3d, sigma0);
        if (status == SIFT3D_FAILURE) {
        	return SIFT3D_FAILURE;
        }
        status = set_peak_thresh_SIFT3D(sift3d, peak_thresh);
        if (status == SIFT3D_FAILURE) {
        	return SIFT3D_FAILURE;
        }
        status = set_corner_thresh_SIFT3D(sift3d, corner_thresh);
        if (status == SIFT3D_FAILURE) {
        	return SIFT3D_FAILURE;
        }
        status = set_num_kp_levels_SIFT3D(sift3d, num_kp_levels);
        if (status == SIFT3D_FAILURE) {
        	return SIFT3D_FAILURE;
        }

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
		int status;

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
		status = init_Gauss_incremental_filter(gss.first_gauss, pyr.sigma_n,next.s, dim);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Make one octave of filters (num_levels - 1)
		o = pyr.first_octave;
		for (s = first_level; s < last_level; s++) {
			cur = SIFT3D_PYR_IM_GET(pyr, o, s);
			next = SIFT3D_PYR_IM_GET(pyr, o, s + 1);
			status = init_Gauss_incremental_filter(SIFT3D_GAUSS_GET(gss, s),cur.s, next.s, dim);
			if (status == SIFT3D_FAILURE) {
				return SIFT3D_FAILURE;
			}
		}

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
	
	// Get a pointer to the incremental Gaussian filter for level s
	private Gauss_filter SIFT3D_GAUSS_GET(GSS_filters gss, int s) {
		return gss.gauss_octave[s - gss.first_level];
	}
	
	/* Initialize a Gaussian filter to go from scale s_cur to s_next. */
	int init_Gauss_incremental_filter(Gauss_filter gauss,
					  double s_cur, double s_next,
					  int dim)
	{
		double sigma;
		int status;

		if (s_cur > s_next) {
	                System.err.println("init_Gauss_incremental_filter: s_cur = " + s_cur + " > s_next = " + s_next);
	                return SIFT3D_FAILURE;
	        }
		if (dim <= 0) {
			System.err.println("dim = " + dim + " but must be > 0 in init_Gauss_incremental filter");
			return SIFT3D_FAILURE;
		}

		// Compute filter width parameter (sigma)
		sigma = Math.sqrt(s_next * s_next - s_cur * s_cur);

		// Initialize filter kernel
		status = init_Gauss_filter(gauss, sigma, dim);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		return SIFT3D_SUCCESS;
	}
	
	/* Initialize a normalized Gaussian filter, of the given sigma.
	 * If SIFT3D_GAUSS_WIDTH_FCTR is defined, use that value for
	 * the ratio between the width of the filter and sigma. Otherwise,
	 * use the default value 3.0 
	 */
	private int init_Gauss_filter(Gauss_filter gauss, double sigma, int dim)
	{

		double kernel[];
		double x;
		double acc;
		int i;
		int status;

		final int half_width = sigma > 0 ? 
	                Math.max((int)Math.ceil(sigma * SIFT3D_GAUSS_WIDTH_FCTR), 1) :1;
		final int width = 2 * half_width + 1;

		// Initialize intermediates 
		try {
			kernel = new double[width];
		}
		catch (OutOfMemoryError e) {
			System.err.println("Out of memory error on kernel = new double[width] init_Gauss_filter");
			return SIFT3D_FAILURE;
		}

		// Calculate coefficients
		acc = 0;
		for (i = 0; i < width; i++) {
			// distance away from center of filter
			x = (double)i - half_width;

			// (x / sigma)^2 = x*x / (sigma*sigma)
			x /= sigma + DBL_EPSILON;

			// exponentiate result
			kernel[i] = Math.exp(-0.5 * x * x);

			// sum of all kernel elements
			acc += kernel[i];
		}

		// normalize kernel to sum to 1
		for (i = 0; i < width; i++) {
			kernel[i] /= acc;
		}

		// Save the filter data 
		gauss.sigma = sigma;
		status = init_Sep_FIR_filter(gauss.f, dim, width, kernel,SIFT3D_TRUE);
		if (status == SIFT3D_FAILURE) {
			kernel = null;
			return SIFT3D_FAILURE;
		}          

	    return SIFT3D_SUCCESS;
	}
	
	/* Initialize a separable FIR filter struct with the given parameters. If OpenCL
	 * support is enabled and initialized, this creates a program to apply it with
	 * separable filters.  
	 *
	 * Note that the kernel data will be copied, so the user can free it without 
	 * affecting f. */
	private int init_Sep_FIR_filter(Sep_FIR_filter f, int dim, int width,
				double kernel[], int symmetric)
	{

	    int i;    
		final int kernel_size = width;

	        // Save the data
		f.dim = dim;
		f.width = width;
		f.symmetric = symmetric;

	        // Allocate the kernel memory
		try {
			f.kernel = new double[kernel_size];
		}
		catch (OutOfMemoryError e) {
			System.err.println("init_Sep_FIT_filter: out of memory");
			return SIFT3D_FAILURE;
		}
	       

	        // Copy the kernel data
	   for (i = 0; i < kernel_size; i++) {
		   f.kernel[i] = kernel[i];
	   }

	/*#ifdef SIFT3D_USE_OPENCL
		{
			char src[1 << 15];
			char *template;
			cl_program program;
			cl_int err;
			float k;
			int i;

			const char *path = SEP_FIR_3D_PATH;
			const int half_width = f->half_width;

			// Load the template
			if ((template = read_file(path)) == NULL) {
				printf("init_Sep_FIR_Filter: error reading path %s \n",
				       path);
				return SIFT3D_FAILURE;
			}
			sprintf(src, "%s\n", template);

			// Write the unrolled kernel
			for (i = -half_width; i < half_width; i++) {
				k = f->kernel[i];
				sprintf(src, "acc += %.16f * "
					"read_imagef(src, sampler, center + d_xyz * %d); \n",
					k, i);
			}

			// Write the ending
			sprintf(src,
				"write_imagef(dst, sampler, (float4) center); \n } \n");

			// Compile the program  
			if (compile_cl_program_from_source(&program, cl_data.context,
							   cl_data.devices,
							   cl_data.num_devices,
							   (char **)&src, 1))
				return SIFT3D_FAILURE;
			f->cl_apply_unrolled =
			    clCreateKernel(program, "sep_fir_3d", &err);
			check_cl_error(err, "init_Sep_FIR_Filter: create kernel");
			clReleaseProgram(program);
		}
	#endif*/
		return SIFT3D_SUCCESS;
	}
	
	/* Sets the scale parameter of the first level of octave 0, checking that it
	 * is nonnegative. */
	private int set_sigma0_SIFT3D(SIFT3DC sift3d, double sigma0) {

	        final double sigma_n = sift3d.gpyr.sigma_n;

	        if (sigma0 < 0.0) {
	                System.err.println("SIFT3D sigma0 must be nonnegative. Provided sigm0 : " + sigma0);
	                return SIFT3D_FAILURE; 
	        } 

	        return set_scales_SIFT3D(sift3d, sigma0, sigma_n);
	}
	
	/* Sets the peak threshold, checking that it is in the interval (0, inf) */
	private int set_peak_thresh_SIFT3D(SIFT3DC sift3d, double peak_thresh) {
	        if (peak_thresh <= 0.0 || peak_thresh > 1) {
	                System.err.println("SIFT3D peak_thresh must be in the interval (0, 1]. Provided peak_thresh: " + peak_thresh);
	                return SIFT3D_FAILURE;
	        }

	        sift3d.peak_thresh = peak_thresh;
	        return SIFT3D_SUCCESS;
	}

	/* Sets the corner threshold, checking that it is in the interval [0, 1]. */
	private int set_corner_thresh_SIFT3D(SIFT3DC sift3d, double corner_thresh) {

	        if (corner_thresh < 0.0 || corner_thresh > 1.0) {
	                System.err.println("SIFT3D corner_thresh must be in the interval [0, 1]. Provided corner_thresh: " + corner_thresh);
	                return SIFT3D_FAILURE;
	        }

	        sift3d.corner_thresh = corner_thresh;
	        return SIFT3D_SUCCESS;
	}

	/* Sets the number of levels per octave. This function will resize the
	 * internal data. */
	private int set_num_kp_levels_SIFT3D(SIFT3DC sift3d, int num_kp_levels) {

	        final Pyramid gpyr = sift3d.gpyr;

	        return resize_SIFT3D(sift3d, num_kp_levels);
	}
	
	/* Resize a SIFT3D struct, allocating temporary storage and recompiling the 
	 * filters. Does nothing unless set_im_SIFT3D was previously called. */
	private int resize_SIFT3D(SIFT3DC sift3d, int num_kp_levels) {
            int status;
	        int num_octaves; 

	        final Image im = sift3d.im;
	        Pyramid gpyr = sift3d.gpyr;
	        Pyramid dog = sift3d.dog;
		    final int num_dog_levels = num_kp_levels + 2;
		    final int num_gpyr_levels = num_dog_levels + 1;
	        final int first_octave = 0;
	        final int first_level = -1;

		// Compute the meximum allowed number of octaves
		if (im.data != null) {
	        // The minimum size of a pyramid level is 8 in any dimension
			final int last_octave = 
	                        (int) log2((double) Math.min(Math.min(im.nx, im.ny), 
	                        im.nz)) - 3 - first_octave;

	                // Verify octave parameters
	                if (last_octave < first_octave) {
	                        System.err.println("resize_SIFT3D: input image is too small:");
	                        System.err.println("Must have at least 8 voxels in each dimension");
	                        return SIFT3D_FAILURE;
	                }

	                num_octaves = last_octave - first_octave + 1;
		} else {
	                num_octaves = 0;
	        }

		// Resize the pyramid
		status = resize_Pyramid(im, first_level, num_kp_levels,
	                num_gpyr_levels, first_octave, num_octaves, gpyr);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}
		status = resize_Pyramid(im, first_level, num_kp_levels, 
	                num_dog_levels, first_octave, num_octaves, dog);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

	        // Do nothing more if we have no image
	        if (im.data == null) {
	                return SIFT3D_SUCCESS;
	        }

		// Compute the Gaussian filters
		status = make_gss(sift3d.gss, sift3d.gpyr);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		return SIFT3D_SUCCESS;
	}
	
	private double log2(double x) {
    	return (Math.log(x)/Math.log(2));
    }

	/* Resize a scale-space pyramid according to the size of base image im.
	 *
	 * Parameters:
	 *  -im: An image with the desired dimensions and units at octave 0
	 *  -first_level: The index of the first pyramid level per octave
	 *  -num_kp_levels: The number of levels per octave in which keypoints are 
	 *      detected
	 *  -num_levels: The total number of levels. Must be greater than or equal to
	 *      num_kp_levels.
	 *  -first_octave: The index of the first octave (0 is the base)
	 *  -num_octaves: The total number of octaves 
	 *  -sigma0: The scale parameter of level 0, octave 0
	 *  -sigma_n: The nominal scale of the image im.
	 *  -pyr: The Pyramid to be resized.
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
	int resize_Pyramid(Image im, int first_level, 
	        int num_kp_levels, int num_levels,
	        int first_octave, int num_octaves, 
	        Pyramid pyr) {

	    int status;    
		double units[] = new double[IM_NDIMS];
	        int dims[] = new int[IM_NDIMS];
		double factor;
		int i, o, s;

		final double sigma0 = pyr.sigma0;
		final double sigma_n = pyr.sigma_n;
	    final int old_num_total_levels = pyr.num_levels * pyr.num_octaves;
		final int num_total_levels = num_levels * num_octaves;

	        // Verify inputs
	        if (num_levels < num_kp_levels) {
	                System.err.println("resize_Pyramid: num_levels = " + num_levels + 
	                		" < num_kp_levels = " + num_kp_levels);
	                return SIFT3D_FAILURE;
	        }

	        // Store the new parameters
	        pyr.first_level = first_level;
	        pyr.num_kp_levels = num_kp_levels;
	        pyr.first_octave = first_octave;
	        pyr.num_octaves = num_octaves;
	        pyr.num_levels = num_levels;

	        // Clean up old levels which are no longer needed 
	        for (i = num_total_levels; i < old_num_total_levels; i++) {
	                Image level = pyr.levels[i];
	                im_free(level);
	        }

		// Resize the outer array
	        if (num_total_levels != 0) {
	        	pyr.levels = new Image[num_total_levels];
	        	for (i = 0; i < num_total_levels; i++) {
	        		try {
	        		    pyr.levels[i] = new Image();
	        		}
	        		catch (OutOfMemoryError e) {
	        			return SIFT3D_FAILURE;
	        		}
	        	}
	        }

		// We have nothing more to do if there are no levels
		if (num_total_levels == 0) {
			return SIFT3D_SUCCESS;
		}

	        // Initalize new levels
	        for (i = old_num_total_levels; i < num_total_levels; i++) {
	                Image level = pyr.levels[i];
	                init_im(level);
	        }

	        // We have nothing more to do if the image is empty
	        if (im.data == null) {
	                return SIFT3D_SUCCESS;
	        }

		// Calculate base image dimensions and units
		factor = Math.pow(2.0, -first_octave);
	        for (i = 0; i < IM_NDIMS; i++) {
	        	    if (i == 0) {
	                    dims[i] = (int) ((double) im.nx * factor);
	                    units[i] = im.ux * factor;
	        	    }
	        	    else if (i == 1) {
	        	    	dims[i] = (int) ((double) im.ny * factor);
	                    units[i] = im.uy * factor;	
	        	    }
	        	    else {
	        	    	dims[i] = (int) ((double) im.nz * factor);
	                    units[i] = im.uz * factor;	
	        	    }    
	        }

		// Initialize each level separately
	     // Loop through all levels of a given pyramid
        	for (o = pyr.first_octave; o <= SIFT3D_PYR_LAST_OCTAVE(pyr); 
                        o++) { 
	        	for (s = pyr.first_level; s <= SIFT3D_PYR_LAST_LEVEL(pyr); 
	                        s++) {
	                        // Initialize Image fields
	                        Image level = SIFT3D_PYR_IM_GET(pyr, o, s);
	                        im.nx = dims[0];
	                        im.ny = dims[1];
	                        im.nz = dims[2];
	                        im.ux = units[0];
	                        im.uy = units[1];
	                        im.uz = units[2];
		                level.nc = im.nc;
		                im_default_stride(level);

	                        // Re-size data memory
	                        status = im_resize(level);
	                        if (status == SIFT3D_FAILURE) {
	                                return SIFT3D_FAILURE;
	                        }

	        	}

		        // Adjust dimensions and recalculate image size
	                for (i = 0; i < IM_NDIMS; i++) {
	                        dims[i] /= 2;
	                        units[i] *= 2;
	                }

        	} 

	        // Set the scales for the new levels
	        return set_scales_Pyramid(pyr.sigma0, pyr.sigma_n, pyr);
	}
	
	/* Clean up memory for an Image */
	private void im_free(Image im)
	{
		if (im.data != null)
			im.data = null;
	}
	
	/* Calculate the strides of an image object in the default
	 * manner. The following parameters must be initialized:
	 * -nx
	 * -ny
	 * -nz
	 * -nc
	 * If a dimension is not used, its size should be set
	 * to 1. */
	private void im_default_stride(Image im)
	{

	        int prod;
		int i;

		prod = im.nc;
		im.xs = prod;

		for (i = 1; i < IM_NDIMS; i++) {
			if (i == 1) {
			    prod *= im.nx;
			    im.ys = prod;
			}
			else {
				prod *= im.ny;
				im.zs = prod;
			}
		}
	}
	
	/* Resize an image according to the current nx, ny,
	 * and nz. Does not modify scale space information or
	 * strides. Prior to calling this function, use init_im(im)
	 * and initialize the following fields:
	 * -nx
	 * -ny
	 * -nz
	 * -nc
	 * -xs (can be set by im_default_stride(im)) 
	 * -ys (can be set by im_default_stride(im)) 
	 * -zs (can be set by im_default_stride(im)) 
	 *
	 * All of this initialization can also be done with
	 * init_im_with_dims(), which calls this function.
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise.
	 */
	private int im_resize(Image im)
	{

		int i;
		int dim;

		//FIXME: This will not work for strange strides
		final int size = im.nx * im.ny * im.nz * im.nc;

		// Verify inputs
		for (i = 0; i < IM_NDIMS; i++) {

			if (i == 0) {
				dim = im.nx;
				if (im.nx == 0) {
					System.err.println("im_resize: invalid im.nx = 0");
					return SIFT3D_FAILURE;
				}
			}
			else if (i == 1) {
				dim = im.ny;
				if (im.ny == 0) {
					System.err.println("im_resize: invalid im.ny = 0");
					return SIFT3D_FAILURE;
				}
			}
			else {
				dim = im.nz;
				if (im.nz == 0) {
					System.err.println("im_resize: invalid im.nz = 0");
					return SIFT3D_FAILURE;
				}
			}
		}
		if (im.nc < 1) {
			System.err.println("im_resize: invalid number of channels im.nc: " + im.nc);
			return SIFT3D_FAILURE;
		}

	        // Do nothing if the size has not changed
	        if (im.size == size) {
	            return SIFT3D_SUCCESS;
	        }
		im.size = size;

		// Allocate new memory
		im.data = null;
		try {
		    im.data = new double[size];
		}
		catch (OutOfMemoryError e) {
			System.err.println("Out of memory error on im.data = new double[size] in im_resize");
			return SIFT3D_FAILURE;
		}

	/*#ifdef SIFT3D_USE_OPENCL
		{
			cl_int err;
			int initialized;

			if (cl_data.valid) {
				initialized = (im->data != NULL);

				// Destroy the old image
				if (initialized && im->cl_valid)
					clReleaseMemObject(im->cl_image);

				// Init an OpenCL image
				if (im->nz > 0) {
					im->cl_image = clCreateImage2D(cl_data.context,
								       cl_data.
								       mem_flags,
								       &cl_data.
								       image_format,
								       im->nx, im->ny,
								       im->ys,
								       im->data, &err);
				} else {
					im->cl_image = clCreateImage3D(cl_data.context,
								       cl_data.
								       mem_flags,
								       &cl_data.
								       image_format,
								       im->nx, im->ny,
								       im->nz,
								       im->ys,
								       im->zs,
								       im->data, &err);
				}

				if (err != CL_SUCCESS) {
					im->cl_valid = SIFT3D_FALSE;
					return SIFT3D_FAILURE;
				}

				im->cl_valid = SIFT3D_TRUE;
			}
		}
	#endif*/
		return size != 0 && im.data == null ? SIFT3D_FAILURE : SIFT3D_SUCCESS;
	}
	
	/* Free all memory associated with a Reg_SIFT3D struct. reg cannot be reused
	 * unless it is reinitialized. */
	private void cleanup_Reg_SIFT3D(Reg_SIFT3D reg) {

	        cleanup_SIFT3D_Descriptor_store(reg.desc_src);
	        cleanup_SIFT3D_Descriptor_store(reg.desc_ref);
	        cleanup_SIFT3D(reg.sift3d); 
	        cleanup_Mat_rm(reg.match_src);
	        cleanup_Mat_rm(reg.match_ref);
	}
	
	/* Free all memory associated with a SIFT3D_Descriptor_store. desc
	 * cannot be used after calling this function, unless re-initialized. */
	private void cleanup_SIFT3D_Descriptor_store(SIFT3D_Descriptor_store desc) {
	        desc.buf = null;
	}
	
	/* Free all memory associated with a SIFT3D struct. sift3d cannot be reused
	 * unless it is reinitialized. */
	private void cleanup_SIFT3D(SIFT3DC sift3d) {

		// Clean up the image copy
		im_free(sift3d.im);

	        // Clean up the pyramids
	        cleanup_Pyramid(sift3d.gpyr);
	        cleanup_Pyramid(sift3d.dog);

	        // Clean up the GSS filters
	        cleanup_GSS_filters(sift3d.gss);

	        // Clean up the triangle mesh 
	        cleanup_Mesh(sift3d.mesh);

	/*#ifdef USE_OPENCL
	        // Clean up the OpenCL kernels
	        cleanup_SIFT3D_cl_kernels(&sift3d->kernels);
	#endif*/
	}
	
	/* Release all memory associated with a Pyramid. pyr cannot be used again,
	 * unless it is reinitialized. */
	private void cleanup_Pyramid(Pyramid pyr)
	{

		int o, s;

		// We are done if there are no levels
		if (pyr.levels == null)
			return;

		// Free the levels
		// Loop through all levels of a given pyramid
    	for (o = pyr.first_octave; o <= SIFT3D_PYR_LAST_OCTAVE(pyr); 
                    o++) { 
        	for (s = pyr.first_level; s <= SIFT3D_PYR_LAST_LEVEL(pyr); 
                        s++) {
				Image level = SIFT3D_PYR_IM_GET(pyr, o, s);
				im_free(level);
        	}
    	}

		// Free the pyramid level buffer
		pyr.levels = null;
	}
	
	/* Release all memory associated with a triangle mesh. mesh cannot be reused
	 * before it is reinitialized. */
	private void cleanup_Mesh(Mesh mesh)
	{
		mesh.tri = null;
	}
	
	/* Run the registration procedure. 
	 *
	 * Parameters: 
	 *   reg: The struct holding registration state.
	 *   tform: The output transformation. If NULL, this function only performs
	 *     feature matching.
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
	private int register_SIFT3D(Reg_SIFT3D reg, Affine tform) {

	    int status;    
		Mat_rm match_src_mm = new Mat_rm();
		Mat_rm match_ref_mm = new Mat_rm();
	        int matches[];
	        int i, j;

	        Ransac ran = reg.ran;
	        Mat_rm match_src = reg.match_src;
	        Mat_rm match_ref = reg.match_ref;
	        final double nn_thresh = reg.nn_thresh;
	        SIFT3D_Descriptor_store desc_src = reg.desc_src;
	        SIFT3D_Descriptor_store desc_ref = reg.desc_ref;

		// Verify inputs
		if (desc_src.num <= 0) {
			System.err.println("register_SIFT3D: no source image descriptors are available");
			return SIFT3D_FAILURE;
		}
		if (desc_ref.num <= 0) {
			System.err.println("register_SIFT3D: no reference image descriptors are available");
			return SIFT3D_FAILURE;
		}

	        // Initialize intermediates
	        status = init_Mat_rm(match_src_mm, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
	        if (status == SIFT3D_FAILURE) {
	        	 System.err.println("register_SIFT3D: match_src_mm failed initialization");
	             return SIFT3D_FAILURE;	
	        }
		    status = init_Mat_rm(match_ref_mm, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		    if (status == SIFT3D_FAILURE) {
	            System.err.println("register_SIFT3D: match_ref_mm failed initialization");
	            return SIFT3D_FAILURE;
	        }

		// Match features
	    matches = new int[desc_src.num];
		status = SIFT3D_nn_match(desc_src, desc_ref, nn_thresh, matches);
		if (status == SIFT3D_FAILURE) {
			System.err.println("register_SIFT3D: failed to match descriptors");
			matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_FAILURE;
	    }

	        // Convert matches to coordinate matrices
		/*if (SIFT3D_matches_to_Mat_rm(desc_src, desc_ref, matches,
					     match_src, match_ref)) {
			SIFT3D_ERR("register_SIFT3D: failed to extract "
	                        "coordinate matrices \n");
	                goto register_SIFT3D_quit;
	        }

	        // Quit if no tform was provided
	        if (tform == NULL)
	                goto register_SIFT3D_success;

	        // Convert the coordinate matrices to real-world units
	        if (im2mm(match_src, reg->src_units, &match_src_mm) ||
	            im2mm(match_ref, reg->ref_units, &match_ref_mm))
	                goto register_SIFT3D_quit;

		// Find the transformation in real-world units
		if (find_tform_ransac(ran, &match_src_mm, &match_ref_mm, tform))
	                goto register_SIFT3D_quit;

	        // Convert the transformation back to image space
	        if (mm2im(reg->src_units, reg->ref_units, tform))
	                goto register_SIFT3D_quit;

	register_SIFT3D_success:
	        // Clean up
	        free(matches);
	        cleanup_Mat_rm(&match_src_mm);
	        cleanup_Mat_rm(&match_ref_mm);*/

		return SIFT3D_SUCCESS;

	/*register_SIFT3D_quit:
	        free(matches);
	        cleanup_Mat_rm(&match_src_mm); 
	        cleanup_Mat_rm(&match_ref_mm); 
	        return SIFT3D_FAILURE;*/
	}

	/* Perform nearest neighbor matching on two sets of 
	 * SIFT descriptors.
	 *
	 * This function will reallocate *matches. As such, *matches must be either
	 * NULL or a pointer to previously-allocated array. Upon successful exit,
	 * *matches is an array of size d1->num.
	 * 
	 * On return, the ith element of matches contains the index in d2 of the match
	 * corresponding to the ith descriptor in d1, or -1 if no match was found.
	 *
	 * You might consider using SIFT3D_matches_to_Mat_rm to convert the matches to
	 * coordinate matrices. */
	private int SIFT3D_nn_match(SIFT3D_Descriptor_store d1,
			    SIFT3D_Descriptor_store d2,
			    double nn_thresh, int matches[]) {
	
		int i;
	
		final int num = d1.num;
	
	        // Verify inputs
		if (num < 1) {
			System.err.println("SIFT3D_nn_match: invalid number of descriptors in d1: " + num);
			return SIFT3D_FAILURE;
		}
	
		// Must allocate matches before routine entry
		// Resize the matches array (num cannot be zero)
		/*if ((*matches = (int *) SIFT3D_safe_realloc(*matches, 
			num * sizeof(int))) == NULL) {
		    SIFT3D_ERR("_SIFT3D_nn_match: out of memory! \n");
		    return SIFT3D_FAILURE;
		}*/
	
		for (i = 0; i < d1.num; i++) {
		    // Mark -1 to signal there is no match
		    matches[i] = -1;
		}
		
		// Exhaustive search for matches
	//#pragma omp parallel for
		for (i = 0; i < num; i++) {
	
	                SIFT3D_Descriptor desc1 = d1.buf[i];
	
	                // Forward matching pass
	                matches[i] = match_desc(desc1, d2, nn_thresh);
	
	                // We are done if there was no match
	                if (matches[i] < 0)
	                        continue;
	
	                // Check for forward-backward consistency
	                if (match_desc(d2.buf[matches[i]], d1, nn_thresh) != i) {
	                        matches[i] = -1;
	                }
	        }
	
		return SIFT3D_SUCCESS;
	}
	
	/* Helper function to match desc against the descriptors in store. Returns the
	 * index of the match, or -1 if none was found. */
	private int match_desc(SIFT3D_Descriptor desc,
	        SIFT3D_Descriptor_store store, double nn_thresh) {

		final SIFT3D_Descriptor desc_best;
	        double ssd_best, ssd_nearest;
	        int i;

	if (SIFT3D_MATCH_MAX_DIST > 0.0) {
	        Cvec dims = new Cvec();
	        Cvec dmatch = new Cvec();
	        double dist_match;
					
	        // Compute spatial distance rejection threshold
	        dims.x = (double) store.nx;	
	        dims.y = (double) store.ny;	
	        dims.z = (double) store.nz;	
	        final double diag = SIFT3D_CVEC_L2_NORM(dims);	
	        final double dist_thresh = diag * SIFT3D_MATCH_MAX_DIST;
	} // if (SIFT3D_MATCH_MAX_DIST > 0.0)

	        // Linear search for the best and second-best SSD matches 
	        ssd_best = ssd_nearest = Double.MAX_VALUE;
	        desc_best = null;
	      /*  for (i = 0; i < store.num; i++) { 

	                double ssd;
	                int j;

	                SIFT3D_Descriptor desc2 = store.buf[i];

	                // Compute the SSD of the two descriptors
	                ssd = 0.0;
	                for (j = 0; j < DESC_NUM_TOTAL_HIST; j++) {

	                        int a, p;

	                        Hist hist1 = desc.hists[j];
	                        Hist hist2 = desc2.hists[j];

	                        HIST_LOOP_START(a, p)
	                                const double diff = 
	                                        (double) HIST_GET(hist1, a, p) -
	                                        (double) HIST_GET(hist2, a, p);
	                                        ssd += diff * diff;
	                        HIST_LOOP_END

	                        // Early termination
	                        if (ssd > ssd_nearest)
	                                break;
	                }

	                // Compare to the best matches
	                if (ssd < ssd_best) {
	                        desc_best = desc2; 
	                        ssd_nearest = ssd_best;
	                        ssd_best = ssd;
	                } else  {
	                        ssd_nearest = SIFT3D_MIN(ssd_nearest, ssd);
	                }
	        }

	        // Reject a match if the nearest neighbor is too close
	        if (ssd_best / ssd_nearest > nn_thresh * nn_thresh)
	                        return -1;

	    if (SIFT3D_MATCH_MAX_DIST > 0.0) {
	        // Compute the spatial distance of the match
	        dmatch.x = (float) desc_best->xd - desc1->xd; 
	        dmatch.y = (float) desc_best->yd - desc1->yd; 
	        dmatch.z = (float) desc_best->zd - desc1->zd; 
	        dist_match = (double) SIFT3D_CVEC_L2_NORM(&dmatch);

	        // Reject matches of great distance
	        if (dist_match > dist_thresh)
	                return -1;
	    } // if (SIFT3D_MATCH_MAX_DIST > 0.0)
	        // The match was a success
	        return desc_best - store->buf;*/
	        return 0;
	}


}
