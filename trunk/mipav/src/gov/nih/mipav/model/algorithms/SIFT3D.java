package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Random;
import java.util.Vector;
import java.util.zip.GZIPOutputStream;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.GeneralizedInverse2;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

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
    private ModelImage warpedImage = null;
	private double SIFT3D_nn_thresh_default = 0.8; // Default matching threshold
	private double SIFT3D_err_thresh_default = 5.0;
	private int SIFT3D_num_iter_default = 500;
	private boolean useOCL = false;
	private double SIFT3D_GAUSS_WIDTH_FCTR = 3.0;
	// Set SIFT3D_MATCH_MAX_DIST <= 0.0 to avoid using in int match_desc()
	private double SIFT3D_MATCH_MAX_DIST = 0.0;
	//private double SIFT3D_MATCH_MAX_DIST = 0.3; // Maximum distance between matching features
	private boolean ICOS_HIST = true;  // Icosahedral gradient histogram
	private boolean SIFT3D_RANSAC_REFINE = true;	// Use least-squares refinement in RANSAC
	private boolean CUBOID_EXTREMA = false; // Search for extrema in a cuboid region
	private boolean SIFT3D_ORI_SOLID_ANGLE_WEIGHT = false; // Weight bins by solid angle
	                                                       // Can only be used if ICOS_HIST = false
	private String fileDir = null;
	private String ext_gz = "gz";


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

	/* Constants */
	private int IM_NDIMS = 3; // Number of dimensions in an Image
	private int ICOS_NFACES = 20; // Number of faces in an icosahedron
	private int ICOS_NVERT = 12; // Number of vertices in an icosahedron
	
    private double SIFT3D_AZ_MAX_F = 2.0 * Math.PI; // Maximum azimuth
	private double SIFT3D_PO_MAX_F = Math.PI; // Maximum polar angle
	
	// The number of elements in a gradient histogram
	//#ifdef ICOS_HIST
	private int HIST_NUMEL = (ICOS_NVERT);
	//#else
	// private inmt HIST_NUMEL = (NBINS_AZ * NBINS_PO);
	//#endif

	/* Derived constants */
	private int DESC_NUM_TOTAL_HIST = (NHIST_PER_DIM * NHIST_PER_DIM * NHIST_PER_DIM);
	private int DESC_NUMEL = (DESC_NUM_TOTAL_HIST * HIST_NUMEL);

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
	//private final double gr = 1.6180339887; // Golden ratio
	private final double gr = (1.0 + Math.sqrt(5.0))/2.0;
	
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

		Keypoint buf[];			// Buffer
		int num;			// Number of elements currently in buffer
		int buf_size;	        // Buffer capacity, in bytes
		
		public Slab() {
			
		}

	};
	
	/* Struct defining a keypoint in 3D space. */
    class Keypoint {

		double r_data[][] = new double[IM_NDIMS][IM_NDIMS];	// Memory for matrix R, do not use this
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
	        SIFT3DC sift3d = new SIFT3DC();
	        Ransac ran = new Ransac();
	        SIFT3D_Descriptor_store desc_src = new SIFT3D_Descriptor_store();
	        SIFT3D_Descriptor_store desc_ref = new SIFT3D_Descriptor_store();
	        Mat_rm match_src = new Mat_rm();
	        Mat_rm match_ref = new Mat_rm();
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
    		double SIFT3D_MATCH_MAX_DIST, boolean ICOS_HIST, boolean SIFT3D_RANSAC_REFINE,
    		boolean CUBOID_EXTREMA, boolean SIFT3D_ORI_SOLID_ANGLE_WEIGHT) {
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
    	this.SIFT3D_RANSAC_REFINE = SIFT3D_RANSAC_REFINE;
    	this.CUBOID_EXTREMA = CUBOID_EXTREMA;
    	this.SIFT3D_ORI_SOLID_ANGLE_WEIGHT = SIFT3D_ORI_SOLID_ANGLE_WEIGHT;
    	
    	// The number of elements in a gradient histogram
    	if (ICOS_HIST) {
    	    HIST_NUMEL = ICOS_NVERT;
    	}
    	else {
    	    HIST_NUMEL = (NBINS_AZ * NBINS_PO);
    	}
    	
    	DESC_NUMEL = DESC_NUM_TOTAL_HIST * HIST_NUMEL;
    	fileDir = inputImage.getFileInfo(0).getFileDirectory();
    }
    
    public void runAlgorithm() {
    	// Example of registering two images.
    	/* This illustrates how to use Reg_SIFT3D within a function, freeing all memory
    	 * afterwards. */
    	
    	/* Example file paths */
        //String ref_path = "1.nii.gz";
    	//String src_path = "2.nii.gz";
    	//String match_path = "1_2_matches.nii.gz";
    	//String warped_path = "2_warped.nii.gz";
    	String affine_path = "1_2_affine.csv.gz";

    	int status;
    	int t;
    	int i;
    	double data2[] = null;
    	
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
	    // But SIFT3D_detect_keypoints only accepts single channels
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
			data2 = new double[srcVolume];
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
			data2 = new double[refVolume];
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
		
		// Set the images
        status = set_src_Reg_SIFT3D(reg, src);
        if (status == SIFT3D_FAILURE) {
        	// Clean up
        	System.err.println("Failed in set_src_Reg_SIFT3D(reg, src)");
	        im_free(src);
	        im_free(ref);
	        im_free(warped);
	        cleanup_Reg_SIFT3D(reg);
	        cleanup_Affine(affine);
			setCompleted(false);
			return;	
        }
        
        status = set_ref_Reg_SIFT3D(reg, ref);
        if (status == SIFT3D_FAILURE) {
        	// Clean up
        	System.err.println("Failed in set_ref_Reg_SIFT3D(reg, ref)");
	        im_free(src);
	        im_free(ref);
	        im_free(warped);
	        cleanup_Reg_SIFT3D(reg);
	        cleanup_Affine(affine);
			setCompleted(false);
			return;	
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
        
        // Write the transformation to a file 
        status = write_Affine(affine_path, affine);
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

        // Warp the source image
        status = im_inv_transform(affine, src, interp_type.LINEAR, SIFT3D_TRUE, warped);
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

        int warpedExtents[] = new int[inputImage.getNDims()];
        for (i = 0; i < inputImage.getNDims(); i++) {
        	warpedExtents[i] = inputImage.getExtents()[i];
        }
        warpedImage = new ModelImage(ModelStorageBase.DOUBLE, warpedExtents, inputImage.getImageName() + "_warped");
        if (warpedImage.getNDims() == 3) {
        	try {
        		warpedImage.importData(0, warped.data, true);
        	}
        	catch(IOException e) {
        		System.err.println("IOException on warpedImage.importData(0, warped.data, true");
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
        }
        else if (warpedImage.getNDims() == 4) {
        	data2 = new double[srcVolume];
        	for (t = 0; t < warpedExtents[3]; t++) {
        		for (i = 0; i < srcVolume; i++) {
				    data2[i] = warped.data[i*warped.nc + t];
			    }
				try {
				    warpedImage.importData(t*srcVolume, data2, false);
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
				data2 = null;
				warpedImage.calcMinMax();
        	}
        }
        FileInfoBase fileInfo[] = warpedImage.getFileInfo();
        for (i = 0; i < fileInfo.length; i++) {
        	fileInfo[i].setExtents(warpedExtents);
        	fileInfo[i].setResolutions(inputImage.getFileInfo()[i].getResolutions());
        	fileInfo[i].setUnitsOfMeasure(inputImage.getFileInfo()[i].getUnitsOfMeasure());
        }
        new ViewJFrameImage(warpedImage);
        
        // Clean up
        im_free(src);
        im_free(ref);
        im_free(warped);
        cleanup_Reg_SIFT3D(reg);
        cleanup_Affine(affine);
		setCompleted(true);
		return;	
    	
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
        	mat.data_double = new double[num_rows][num_cols];
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

    	/* Vertices of a regular icosahedron inscribed in the unit sphere. */
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
    			SIFT3D_CVEC_SCALE(v[j], 1.0 / mag);
    		}

    		// Compute the normal vector at v[0] as  (V2 - V1) X (V1 - V0)
    		SIFT3D_CVEC_MINUS(v[2], v[1], temp1);
    		SIFT3D_CVEC_MINUS(v[1], v[0], temp2);
    		SIFT3D_CVEC_CROSS(temp1, temp2, n);

    		// Ensure this vector is facing outward from the origin
    		if (SIFT3D_CVEC_DOT(n, v[0]) < 0) {
    			// Swap two vertices
    			temp1.x = v[0].x;
    			temp1.y = v[0].y;
    			temp1.z = v[0].z;
    			v[0].x = v[1].x;
    			v[0].y = v[1].y;
    			v[0].z = v[1].z;
    			v[1].x = temp1.x;
                v[1].y = temp1.y;
                v[1].z = temp1.z;
    			// Compute the normal again
    			SIFT3D_CVEC_MINUS(v[2], v[1], temp1);
    			SIFT3D_CVEC_MINUS(v[1], v[0], temp2);
    			SIFT3D_CVEC_CROSS(temp1, temp2, n);
    		}
    		if (SIFT3D_CVEC_DOT(n, v[0]) < 0) {
    	    //if (SIFT3D_CVEC_DOT(n, v[0]) < -1.0E-12) {
    			double dotprod = SIFT3D_CVEC_DOT(n, v[0]);
    			System.err.println("dotprod = " + dotprod);
    			System.err.println("SIFT3D_CVEC_DOT(n, v[0]) < 0 in init_geometry");
				return SIFT3D_FAILURE;	
    		}

    		// Ensure the triangle is equilateral
    		SIFT3D_CVEC_MINUS(v[2], v[0], temp3);
    		if (Math.abs(SIFT3D_CVEC_L2_NORM(temp1) - SIFT3D_CVEC_L2_NORM(temp2)) >= 1E-10) {
    			double norm_temp1 = SIFT3D_CVEC_L2_NORM(temp1);
    			double norm_temp2 = SIFT3D_CVEC_L2_NORM(temp2);
    			System.err.println("norm_temp1 = " + norm_temp1);
    			System.err.println("norm_temp2 = " + norm_temp2);
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
    
 // Operate element-wise on two Cartesian coordinate vectors, cc = ca + cb
    private void SIFT3D_CVEC_PLUS(Cvec ca, Cvec cb, Cvec cc) {
        cc.x = ca.x + cb.x;
        cc.y = ca.y + cb.y;
        cc.z = ca.z + cb.z;
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
	        //final Image first_level = 
	                //SIFT3D_PYR_IM_GET(pyr, pyr.first_octave, pyr.first_level);

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

	        //final Pyramid gpyr = sift3d.gpyr;

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
			System.err.println("In resize_SIFT3D failed on resize_Pyramid for gpyr");
			return SIFT3D_FAILURE;
		}
		status = resize_Pyramid(im, first_level, num_kp_levels, 
	                num_dog_levels, first_octave, num_octaves, dog);
		if (status == SIFT3D_FAILURE) {
			System.err.println("In resize_SIFT3D failed on resize_Pyramid for dog");
			return SIFT3D_FAILURE;
		}

	        // Do nothing more if we have no image
	        if (im.data == null) {
	                return SIFT3D_SUCCESS;
	        }

		// Compute the Gaussian filters
		status = make_gss(sift3d.gss, sift3d.gpyr);
		if (status == SIFT3D_FAILURE) {
			System.err.println("In resize_SIFT3D failed on make_gss");
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
	                        level.nx = dims[0];
	                        level.ny = dims[1];
	                        level.nz = dims[2];
	                        level.ux = units[0];
	                        level.uy = units[1];
	                        level.uz = units[2];
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

		//FIXME: This will not work for strange strides
		final int size = im.nx * im.ny * im.nz * im.nc;

		// Verify inputs
		for (i = 0; i < IM_NDIMS; i++) {

			if (i == 0) {
				if (im.nx == 0) {
					System.err.println("im_resize: invalid im.nx = 0");
					return SIFT3D_FAILURE;
				}
			}
			else if (i == 1) {
				if (im.ny == 0) {
					System.err.println("im_resize: invalid im.ny = 0");
					return SIFT3D_FAILURE;
				}
			}
			else {
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
	    int status_out[] = new int[1];
		Mat_rm match_src_mm = new Mat_rm();
		Mat_rm match_ref_mm = new Mat_rm();
	        int matches[];

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
		matches = SIFT3D_nn_match(desc_src, desc_ref, nn_thresh, status_out);
		if (status_out[0] == SIFT3D_FAILURE) {
			System.err.println("register_SIFT3D: failed to match descriptors");
			matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_FAILURE;
	    }

	        // Convert matches to coordinate matrices
		status = SIFT3D_matches_to_Mat_rm(desc_src, desc_ref, matches,match_src, match_ref);
		if (status == SIFT3D_FAILURE) {
			System.err.println("register_SIFT3D: failed to extract coordinate matrices");
			matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_FAILURE;
	    }

        // Quit if no tform was provided
        if (tform == null) {
        	matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_SUCCESS;
        }

        // Convert the coordinate matrices to real-world units
        status = im2mm(match_src, reg.src_units, match_src_mm);
        if (status == SIFT3D_FAILURE) {
			matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_FAILURE;
	    }

        status = im2mm(match_ref, reg.ref_units, match_ref_mm);
        if (status == SIFT3D_FAILURE) {
			matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_FAILURE;
	    }        

		// Find the transformation in real-world units
		status = find_tform_ransac(ran, match_src_mm, match_ref_mm, tform);
		if (status == SIFT3D_FAILURE) {
			matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_FAILURE;
	    }   

	    // Convert the transformation back to image space
	    status = mm2im(reg.src_units, reg.ref_units, tform);
	    if (status == SIFT3D_FAILURE) {
			matches = null;
		    cleanup_Mat_rm(match_src_mm); 
		    cleanup_Mat_rm(match_ref_mm); 
		    return SIFT3D_FAILURE;
	    }  
	    
	    
	    matches = null;
	    cleanup_Mat_rm(match_src_mm); 
	    cleanup_Mat_rm(match_ref_mm); 
		return SIFT3D_SUCCESS;
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
	private int[] SIFT3D_nn_match(SIFT3D_Descriptor_store d1,
			    SIFT3D_Descriptor_store d2,
			    double nn_thresh, int status_out[]) {
	
		int i;
	    int matches[] = null;
		final int num = d1.num;
	
	        // Verify inputs
		if (num < 1) {
			System.err.println("SIFT3D_nn_match: invalid number of descriptors in d1: " + num);
			status_out[0] = SIFT3D_FAILURE;
			return null;
		}
		
		try {
			matches = new int[num];
		}
		catch (OutOfMemoryError e) {
			System.err.println("SIFT3D_nn_match: out of memory!");
			status_out[0] = SIFT3D_FAILURE;
			return null;
		}
	
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
	
		status_out[0] = SIFT3D_SUCCESS;
		return matches;
	}
	
	/* Helper function to match desc against the descriptors in store. Returns the
	 * index of the match, or -1 if none was found. */
	private int match_desc(SIFT3D_Descriptor desc,
	        SIFT3D_Descriptor_store store, double nn_thresh) {

		    SIFT3D_Descriptor desc_best;
	        double ssd_best, ssd_nearest;
	        int i;
            Cvec dims = null;
            Cvec dmatch = null;
            double dist_thresh = 0.0;
	if (SIFT3D_MATCH_MAX_DIST > 0.0) {
	        dims = new Cvec();
	        dmatch = new Cvec();
					
	        // Compute spatial distance rejection threshold
	        dims.x = (double) store.nx;	
	        dims.y = (double) store.ny;	
	        dims.z = (double) store.nz;	
	        final double diag = SIFT3D_CVEC_L2_NORM(dims);	
	        dist_thresh = diag * SIFT3D_MATCH_MAX_DIST;
	} // if (SIFT3D_MATCH_MAX_DIST > 0.0)

	        // Linear search for the best and second-best SSD matches 
	        ssd_best = ssd_nearest = Double.MAX_VALUE;
	        desc_best = null;
	        int desc2_index = -1;
	        int desc_best_index = -1;
	        for (i = 0; i < store.num; i++) { 

	                double ssd;
	                int j;

	                SIFT3D_Descriptor desc2 = store.buf[i];
	                desc2_index = i;

	                // Compute the SSD of the two descriptors
	                ssd = 0.0;
	                for (j = 0; j < DESC_NUM_TOTAL_HIST; j++) {

	                        int a, p;

	                        Hist hist1 = desc.hists[j];
	                        Hist hist2 = desc2.hists[j];
	                        
	                     // Loop over all bins in a gradient histogram. If ICOS_HIST is defined, p
	                     // is not referenced
	                     if (ICOS_HIST) {
	                     	for (a = 0; a < HIST_NUMEL; a++) {
	                     		double diff = hist1.bins[a] - hist2.bins[a];
                                ssd += diff * diff;
	                     	}
	                     }
	                     else {
	                     	for (p = 0; p < NBINS_PO; p++) {
	                     	    for (a = 0; a < NBINS_AZ; a++) {
	                     	    	double diff = hist1.bins[a + p * NBINS_AZ] - hist2.bins[a + p * NBINS_AZ];
	                                ssd += diff * diff;	
	                     	    }
	                     	}
	                     }

	                        // Early termination
	                        if (ssd > ssd_nearest)
	                                break;
	                }

	                // Compare to the best matches
	                if (ssd < ssd_best) {
	                        desc_best = desc2; 
	                        desc_best_index = desc2_index;
	                        ssd_nearest = ssd_best;
	                        ssd_best = ssd;
	                } else  {
	                        ssd_nearest = Math.min(ssd_nearest, ssd);
	                }
	        }

	        // Reject a match if the nearest neighbor is too close
	        if (ssd_best / ssd_nearest > nn_thresh * nn_thresh)
	                        return -1;

	    if (SIFT3D_MATCH_MAX_DIST > 0.0) {
	        // Compute the spatial distance of the match
	        dmatch.x = desc_best.xd - desc.xd; 
	        dmatch.y = desc_best.yd - desc.yd; 
	        dmatch.z = desc_best.zd - desc.zd; 
	        double dist_match = SIFT3D_CVEC_L2_NORM(dmatch);

	        // Reject matches of great distance
	        if (dist_match > dist_thresh)
	                return -1;
	    } // if (SIFT3D_MATCH_MAX_DIST > 0.0)
	        // The match was a success
	        return desc_best_index;
	}
	
	/* Convert a list of matches to matrices of point coordinates.
	 * Only valid matches will be included in the output matrices.
	 *
	 * The format of "matches" is specified in SIFT3D_nn_match.
	 *
	 * All matrices must be initialized prior to calling this function.
	 *
	 * Output format:
	 *  m x 3 matrices [x11 y11 z11] [x21 y21 z21]
	 * 		   |x12 y12 z12| |x22 y22 z22|
	 *		        ...	      ...
	 * 		   [x1N y1N z1N] [x2N y2N z2N] 
	 *
	 * Where points on corresponding rows are matches. */
	private int SIFT3D_matches_to_Mat_rm(SIFT3D_Descriptor_store d1,
				     SIFT3D_Descriptor_store d2,
				     int matches[],
				     Mat_rm match1, 
				     Mat_rm match2) {
		int status;
	    int i, num_matches;

	    final int num = d1.num;

	    // Resize matrices 
	    match1.num_rows = match2.num_rows = d1.num;
	    match1.num_cols = match2.num_cols = 3;
	    match1.type = match2.type = Mat_rm_type.SIFT3D_DOUBLE;
	    status = resize_Mat_rm(match1); 
	    if (status == SIFT3D_FAILURE) {
	    	return SIFT3D_FAILURE;
	    }
	    status = resize_Mat_rm(match2);
	    if (status == SIFT3D_FAILURE) {
		    return SIFT3D_FAILURE;
	    }

	    // Populate the matrices
	    num_matches = 0;
	    for (i = 0; i < num; i++) {
	    	
	    	if (matches[i] == -1)
			    continue;

		    SIFT3D_Descriptor desc1 = d1.buf[i];
		    SIFT3D_Descriptor desc2 = d2.buf[matches[i]];

		    // Save the match
		    match1.data_double[num_matches][0] = desc1.xd; 
		    match1.data_double[num_matches][1] = desc1.yd; 
		    match1.data_double[num_matches][2] = desc1.zd; 
		    match2.data_double[num_matches][0] = desc2.xd; 
		    match2.data_double[num_matches][1] = desc2.yd; 
		    match2.data_double[num_matches][2] = desc2.zd; 
		    num_matches++;
	    }

	    // Release extra memory
	    match1.num_rows = match2.num_rows = num_matches;
	    status = resize_Mat_rm(match1);
	    if (status == SIFT3D_FAILURE) {
	    	return SIFT3D_FAILURE;
	    }
	    status = resize_Mat_rm(match2);
	    if (status == SIFT3D_FAILURE) {
		    return SIFT3D_FAILURE;
	    }
	    
	    return SIFT3D_SUCCESS;
	}
	
	/* Convert an [mxIM_NDIMS] coordinate matrix from image space to mm. 
	 *
	 * Parameters:
	 *   im: The input coordinate matrix, in image space. 
	 *   units: An array of length IM_NDIMS giving the units of image space.
	 *   mm: The output coordinate matrix, in mm.
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise.
	 */
	private int im2mm(Mat_rm im, double units[], 
	        Mat_rm mm) {

	    int status;    
		int row, col;

	        // Verify inputs
	        if (im.num_cols != IM_NDIMS) {
	                System.err.println("im2mm: input must have IM_NDIMS columns.");
	                return SIFT3D_FAILURE;
	        }
	        if (im.type != Mat_rm_type.SIFT3D_DOUBLE) {
	                System.err.println("im2mm: input must have type double.");
	                return SIFT3D_FAILURE;
	        }

	        // Copy the input 
	        status = copy_Mat_rm(im, mm);
	        if (status == SIFT3D_FAILURE) {
	            return SIFT3D_FAILURE;
	        }

	        // Convert the units
	        for (row = 0; row < mm.num_rows; row++) {
	        	for (col = 0; col < mm.num_cols; col++) {
	                mm.data_double[row][col] *= units[col];
	        	}
	        }

	        return SIFT3D_SUCCESS;
	}
	
	/* Copies a matrix. dst will be resized. */
	private int copy_Mat_rm(Mat_rm src, Mat_rm dst)
	{
		int status;
		int r,c;

		// Resize dst
		dst.type = src.type;
		dst.num_rows = src.num_rows;
		dst.num_cols = src.num_cols;
		status = resize_Mat_rm(dst);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Copy the data (use memmove because of static mode)
		for (r = 0; r < src.num_rows; r++) {
			for (c = 0; c < src.num_cols; c++) {
				dst.data_double[r][c] = src.data_double[r][c];
			}
		}

		return SIFT3D_SUCCESS;
	}
	
	/* Fit a transformation from ref to src points, using random sample concensus 
	 * (RANSAC).
	 * 
	 * Parameters:
	 *   ran - Struct storing RANSAC parameters.
	 *   src - The [mxn] source points.
	 *   ref - The [mxn] reference points.
	 *   tform - The output transform. Must be initialized with init_from prior to 
	 *           calling this function. 
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
	private int find_tform_ransac(Ransac ran, Mat_rm src, 
	        Mat_rm ref, Affine tform)
	{
        int status;
        int status_out[] = new int[1];
		Mat_rm ref_cset = new Mat_rm();
		Mat_rm src_cset = new Mat_rm();
		Affine tform_cur = new Affine();
		int cset[], cset_best[];
		int i, j, dim, num_terms, len_best, min_num_inliers;
		int len[] = new int[1];

		final int num_iter = ran.num_iter;
		final int num_pts = src.num_rows;
		//const size_t tform_size = tform_get_size(tform);
		//const tform_type type = tform_get_type(tform);

		// Initialize data structures
		cset = cset_best = null;
		len_best = 0;
		status = init_Affine(tform_cur, IM_NDIMS);
		if (status == SIFT3D_FAILURE) {
			// Clean up and return an error
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}
		status = init_Mat_rm(src_cset, len_best, IM_NDIMS, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		if (status == SIFT3D_FAILURE) {
			// Clean up and return an error
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}
		 
		status = init_Mat_rm(ref_cset, len_best, IM_NDIMS, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		if (status == SIFT3D_FAILURE) {
			// Clean up and return an error
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}

		// initialize type-specific variables
	        dim = tform.A.num_rows;
			num_terms = dim + 1;
			min_num_inliers = 5;
		
		if (num_pts < num_terms) {
			System.err.println("Not enough matched points");
			// Clean up and return an error
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}
		// Ransac iterations
		for (i = 0; i < num_iter; i++) {
			do {
				cset = ransac(src, ref, ran, tform_cur, cset, len,status_out);
			} while (status_out[0] == SIFT3D_SINGULAR);

			if (status_out[0] == SIFT3D_FAILURE) {
				if (cset != null)
					cset = null;
				if (cset_best != null)
					cset_best = null;
				cleanup_Affine(tform_cur);
				if (tform_cur != null) {
					tform_cur = null;
				}
			    cleanup_Mat_rm(ref_cset);
			    cleanup_Mat_rm(src_cset);
				return SIFT3D_FAILURE;	
			}

			if (len[0] > len_best) {
				len_best = len[0];
				try {
					cset_best = new int[len[0]];
				}
				catch (OutOfMemoryError e) {
					if (cset != null)
						cset = null;
					if (cset_best != null)
						cset_best = null;
					cleanup_Affine(tform_cur);
					if (tform_cur != null) {
						tform_cur = null;
					}
				    cleanup_Mat_rm(ref_cset);
				    cleanup_Mat_rm(src_cset);
					return SIFT3D_FAILURE;		
				}
			    status = copy_Affine(tform_cur, tform);
			    if (status == SIFT3D_FAILURE) {
					// Clean up and return an error
					if (cset != null)
						cset = null;
					if (cset_best != null)
						cset_best = null;
					cleanup_Affine(tform_cur);
					if (tform_cur != null) {
						tform_cur = null;
					}
				    cleanup_Mat_rm(ref_cset);
				    cleanup_Mat_rm(src_cset);
					return SIFT3D_FAILURE;	
				}	
			    for (j = 0; j < len[0]; j++) {
			    	cset_best[j] = cset[j];
			    }
			}
		}

		// Check if the minimum number of inliers was found
		if (len_best < min_num_inliers) {
			System.err.println("find_tform_ransac: No good model was found!");
			if (cset != null)
				cset = null;
			if (cset_best != null)
				cset_best = null;
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}

		// Resize the concensus set matrices
	    src_cset.num_rows = ref_cset.num_rows = len_best;
	    status = resize_Mat_rm(src_cset);
	    if (status == SIFT3D_FAILURE) {
			// Clean up and return an error
			if (cset != null)
				cset = null;
			if (cset_best != null)
				cset_best = null;
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}	
	    status = resize_Mat_rm(ref_cset);
	    if (status == SIFT3D_FAILURE) {
			// Clean up and return an error
			if (cset != null)
				cset = null;
			if (cset_best != null)
				cset_best = null;
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}	

		// Extract the concensus set
	    for (i = 0; i < src_cset.num_rows; i++) {
	    	for (j = 0; j < src_cset.num_cols; j++) {
	    		int idx = cset_best[i];
	    		src_cset.data_double[i][j] = src.data_double[idx][j];
	    		ref_cset.data_double[i][j] = ref.data_double[idx][j];
	    	}
	    }

	    if (SIFT3D_RANSAC_REFINE) {
		// Refine with least squares
		switch (solve_system(src_cset, ref_cset, tform_cur)) {
		case SIFT3D_SUCCESS:
			// Copy the refined transformation to the output
			status = copy_Affine(tform_cur, tform);
			if (status == SIFT3D_FAILURE) {
				// Clean up and return an error
				if (cset != null)
					cset = null;
				if (cset_best != null)
					cset_best = null;
				cleanup_Affine(tform_cur);
				if (tform_cur != null) {
					tform_cur = null;
				}
			    cleanup_Mat_rm(ref_cset);
			    cleanup_Mat_rm(src_cset);
				return SIFT3D_FAILURE;	
			}	
			break;
		case SIFT3D_SINGULAR:
			// Stick with the old transformation 
			System.out.println("find_tform_ransac: warning: least-squares refinement ");
			System.out.println("abandoned due to numerical precision");
			break;
		default:
			// Clean up and return an error
			if (cset != null)
				cset = null;
			if (cset_best != null)
				cset_best = null;
			cleanup_Affine(tform_cur);
			if (tform_cur != null) {
				tform_cur = null;
			}
		    cleanup_Mat_rm(ref_cset);
		    cleanup_Mat_rm(src_cset);
			return SIFT3D_FAILURE;	
		}
	    } // if (SIFT3D_RANSAC_REFINE) 

	        // Clean up
	    if (cset != null)
			cset = null;
		if (cset_best != null)
			cset_best = null;
		cleanup_Affine(tform_cur);
		if (tform_cur != null) {
			tform_cur = null;
		}
	    cleanup_Mat_rm(ref_cset);
	    cleanup_Mat_rm(src_cset);
		return SIFT3D_SUCCESS;

	
	}

	/* Perform one iteration of RANSAC. 
	 *
	 * Parameters:
	 *  src - The source points.
	 *  ref - The reference points.
	 *  tform - The output transformation. Must be initialized.
	 *  cset - An array in which to store the concensus set. The value *cset must
	 *         either be NULL, or a pointer to a previously allocated block.
	 *  len - A location in which to store the length of the cset. 
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_SINGULAR if the system is 
	 * near singular, and SIFT3D_FAILURE otherwise. 
	 * Returns cset_out*/
	private int[] ransac(Mat_rm src, Mat_rm ref, 
	        Ransac ran, Affine tform, int cset[], int len[], int status_out[])
	{
	    int rand_indices[] = null;
		Mat_rm src_rand = new Mat_rm();
	    Mat_rm ref_rand = new Mat_rm();
		int i, j, num_rand, cset_len;

		final double err_thresh = ran.err_thresh;
		final double err_thresh_sq = err_thresh * err_thresh;
		final int num_pts = src.num_rows;
	    final int num_dim = src.num_cols;
		//const tform_type type = tform_get_type(tform);

		// Verify inputs
		if (src.type != Mat_rm_type.SIFT3D_DOUBLE || src.type != ref.type) {
			System.err.println("ransac: all matrices must have type double");
			status_out[0] = SIFT3D_FAILURE;
			return null;
		}
		if (src.num_rows != ref.num_rows || src.num_cols != ref.num_cols) {
			System.err.println("ransac: src and ref must have the same dimensions");
			status_out[0] = SIFT3D_FAILURE;
			return null;
		}

	    // Get the number of points for this transform
	    num_rand = tform.A.num_rows + 1;

		// Initialize intermediates
		init_Mat_rm(src_rand, num_rand, num_dim, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		init_Mat_rm(ref_rand, num_rand, num_dim, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);

	    // Draw random point indices
	    rand_indices = n_choose_k(num_pts, num_rand, status_out);
	    if (status_out[0] == SIFT3D_FAILURE) {
	    	if (rand_indices != null) {
                rand_indices = null;
	    	}
	        cleanup_Mat_rm(src_rand);
	        cleanup_Mat_rm(ref_rand);
	        return null;
	    }

	        // Copy the random points
	    for (i = 0; i < src_rand.num_rows; i++) {
	    	for (j = 0; j < src_rand.num_cols; j++) {
	                int rand_idx = rand_indices[i];
	                src_rand.data_double[i][j] = src.data_double[rand_idx][j];
	                ref_rand.data_double[i][j] = ref.data_double[rand_idx][j];
	    	}
	    }

	        // Fit a transform to the random points
		switch (solve_system(src_rand, ref_rand, tform)) {
		case SIFT3D_SUCCESS:
			break;
		case SIFT3D_SINGULAR:
			if (rand_indices != null) {
                rand_indices = null;
	    	}
	        cleanup_Mat_rm(src_rand);
	        cleanup_Mat_rm(ref_rand);
	        status_out[0] = SIFT3D_SINGULAR;
	        return null;
		default:
			if (rand_indices != null) {
                rand_indices = null;
	    	}
	        cleanup_Mat_rm(src_rand);
	        cleanup_Mat_rm(ref_rand);
	        status_out[0] = SIFT3D_FAILURE;
	        return null;
		}

		// Extract the consensus set
		cset_len = 0;
		Vector<Integer>csetVec = new Vector<Integer>();
		for (i = 0; i < num_pts; i++) {

			// Calculate the error
			double err_sq = tform_err_sq(tform, src, ref, i);

			// Reject points below the error threshold
			if (err_sq > err_thresh_sq)
				continue;

			// Add to the consensus set (++cset_len cannot be zero)
			
			csetVec.add(i);
			++cset_len;

		}

		// Return the new length of cset
		len[0] = cset_len;
		int cset_out[] = new int[cset_len];
		for (i = 0; i < cset_len; i++) {
			cset_out[i] = csetVec.get(i);
		}
		csetVec.clear();
		if (rand_indices != null) {
            rand_indices = null;
    	}
        cleanup_Mat_rm(src_rand);
        cleanup_Mat_rm(ref_rand);
		status_out[0] = SIFT3D_SUCCESS;
		return cset_out;
	}
	
	/* Returns an array of k integers, (uniformly) randomly chosen from the 
	 * integers 0 through n - 1.
	 *
	 * The value of *ret must either be NULL, or a pointer to a previously
	 * allocated block. On successful return, *ret contains the k random integers.
	 *
	 * Returns SIFT3D_SUCCESS on succes, SIFT3D_FAILURE otherwise. */
	private int[] n_choose_k(int n, int k, int status_out[]) {

	        int i;
	        int ret[] = null;

	        // Verify inputs
	        if (n < k || k < 1) {
	            status_out[0] = SIFT3D_FAILURE;
	            return null;
	        }

	        // Allocate the array of k elements
	        // Must allocate ret before entry into routine
	        int retn[] = null;
	        try {
	            retn = new int[n];
	        }
	        catch (OutOfMemoryError e) {
	            status_out[0] = SIFT3D_FAILURE;
	            return null;
	        }
	        //if ((*ret = malloc(n * sizeof(int))) == NULL)
	                //goto n_choose_k_fail;

	        // Initialize the array of indices
	        for (i = 0; i < n; i++) {
	                retn[i] = i;
	        }

	        // Randomize the first k indices using Knuth shuffles
	        // Creating a object for Random class 
	        Random r = new Random();
	        for (i = 0; i < k; i++) {
	        	    // Pick a random index from 0 to n - i - 1
	                int j = r.nextInt(n-i);
	                int rand_idx = i + j;
                    int temp = retn[i];
	                retn[i] = retn[rand_idx];
	                retn[rand_idx] = temp;
	        }
	        
	        try {
	            ret = new int[k];
	        }
	        catch (OutOfMemoryError e) {
	            status_out[0] = SIFT3D_FAILURE;
	            return null;
	        }

	        for (i = 0; i < k; i++) {
	        	ret[i] = retn[i];
	        }
	        retn = null;

	        status_out[0] = SIFT3D_SUCCESS;
	        return ret;
	}
	
	/* Solve for a transformation struct. 
	 *
	 * Paramters:
	 *   src - See ransac().
	 *   ref - See ransac()
	 *   tform - See ransac()
	 *
	 * Returns SIFT3D_SUCCESS, SIFT3D_SINGULAR, or SIFT3D_FAILURE. See ransac() for
	 * interpretation. */
	private int solve_system(Mat_rm src, Mat_rm ref, Affine  tform)
	{
		//const tform_type type = tform_get_type(tform);

		//Mat_rm *kp_ref;
		Mat_rm ref_sys = new Mat_rm();
	    Mat_rm X = new Mat_rm();
		int dim, ret;

		init_Mat_rm(ref_sys, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		init_Mat_rm(X, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);

		//construct source matrix and initialize reference vector
	    dim = tform.A.num_rows;
	    make_affine_matrix(ref, dim, ref_sys);
			

		// solve for the coefficients                   
	    ret = ref_sys.num_rows == ref_sys.num_cols ?
	    solve_Mat_rm(ref_sys, src, -1.0, X) :
	    solve_Mat_rm_ls(ref_sys, src, X);

		switch (ret) {
		case SIFT3D_SUCCESS:
			break;
		case SIFT3D_SINGULAR:
			cleanup_Mat_rm(ref_sys);
			cleanup_Mat_rm(X);
			return SIFT3D_SINGULAR;
		default:
			cleanup_Mat_rm(ref_sys);
			cleanup_Mat_rm(X);
			return SIFT3D_FAILURE;
		}

		
		Mat_rm X_trans = new Mat_rm();

		init_Mat_rm(X_trans, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);

		ret = transpose_Mat_rm(X, X_trans);
		if (ret == SIFT3D_FAILURE) {
			cleanup_Mat_rm(X_trans);
			cleanup_Mat_rm(ref_sys);
			cleanup_Mat_rm(X);
			return SIFT3D_FAILURE;
		}
		ret = Affine_set_mat(X_trans, tform);

		cleanup_Mat_rm(X_trans);

		if (ret == SIFT3D_FAILURE) {
			cleanup_Mat_rm(ref_sys);
			cleanup_Mat_rm(X);
			return SIFT3D_FAILURE;
		}

			

	        // Clean up
		cleanup_Mat_rm(ref_sys);
		cleanup_Mat_rm(X);

		return SIFT3D_SUCCESS;
	}
	
	//make the system matrix for affine
	private int make_affine_matrix(Mat_rm pts_in, int dim, Mat_rm mat_out)
	{
        int status;
		int i, j;

		final int num_rows = pts_in.num_rows;

		mat_out.type = Mat_rm_type.SIFT3D_DOUBLE;
		mat_out.num_rows = num_rows;
		mat_out.num_cols = dim + 1;
		status = resize_Mat_rm(mat_out);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		for (i = 0; i < num_rows; i++) {

			//Add one row to the matrix
			for (j = 0; j < dim; j++) {
				mat_out.data_double[i][j] = pts_in.data_double[i][j];
			}
			mat_out.data_double[i][dim] = 1.0;
		}

		return SIFT3D_SUCCESS;
	}
	
	/* Solves the system AX=B exactly. A must be a square matrix.
	 * This function first computes the reciprocal condition number of A.
	 * If it is below the parameter "limit", it returns SIFT3D_SINGULAR. If limit 
	 * is less than 0, a default value of 100 * eps is used.
	 *
	 * The system is solved by LU decomposition.
	 * 
	 * This function returns an error if A and B do not have valid dimensions. 
	 * This function resizes X to [nx1] and changes the type to match B. 
	 * All matrices must be initialized prior to calling this function.
	 * All matrices must have type double.
	 */
	int solve_Mat_rm(Mat_rm A, Mat_rm B, 
	        double limit, Mat_rm X)
	{
        int status;
		Mat_rm A_trans = new Mat_rm();
		Mat_rm B_trans = new Mat_rm();
		double work[] = null;
		int ipiv[] = null;
		int iwork[] = null;
		double limit_arg, anorm;
		double rcond[] = new double[1];
		int info[] = new int[1];

		final int m = A.num_rows;
		final int n = A.num_cols;
		final int nrhs = B.num_cols;
		final int lda = m;
		final int ldb = B.num_rows;
		final char norm_type = '1';
		final char trans = 'N';

		// Default parameters
		if (limit < 0) {
			limit_arg = 100.0 * DBL_EPSILON;
		}
		else {
			limit_arg = limit;
		}

		// Verify inputs
		if (m != n || ldb != m) {
			System.err.println("solve_Mat_rm: invalid dimensions!");
			return SIFT3D_FAILURE;
		}
		if (A.type != Mat_rm_type.SIFT3D_DOUBLE || B.type != Mat_rm_type.SIFT3D_DOUBLE) {
			System.out.println("solve_mat_rm: All matrices must have type double");
			return SIFT3D_FAILURE;
		}
		// Initialize intermediate matrices and buffers
		status = init_Mat_rm(A_trans, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		if (status == SIFT3D_FAILURE) {
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}
		status = init_Mat_rm(B_trans, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		if (status == SIFT3D_FAILURE) {
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;		
		}
		try {
		    work = new double[4*n];
		    iwork = new int[n];
		    ipiv = new int[m];
		}
		catch (OutOfMemoryError e) {
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;			
		}

		// Transpose matrices for LAPACK
		status = transpose_Mat_rm(A, A_trans);
		if (status == SIFT3D_FAILURE) {
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;		
		}
		status = transpose_Mat_rm(B, B_trans);
		if (status == SIFT3D_FAILURE) {
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;		
		}


		// Compute the L1-norm of A
		GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
		LinearEquations2 le2 = new LinearEquations2();
		anorm = ge.dlange(norm_type, m, n, A_trans.data_double, lda, work);

		// Compute the LU decomposition of A in place
		le2.dgetrf(m, n, A_trans.data_double, lda, ipiv, info);
		if (info[0] < 0) {
			System.err.println("solve_Mat_rm: LAPACK dgetrf error code " + info[0]);
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		} else if (info[0] > 0) {
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_SINGULAR;
		}
		// Compute the reciprocal condition number of A
		le2.dgecon(norm_type, n, A_trans.data_double, lda, anorm, rcond,
			work, iwork, info);
		if (info[0] < 0) {
			System.err.println("solve_Mat_rm: LAPACK dgecon error code = " + info[0]);
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}
		// Return if A is singular
		if (rcond[0] < limit_arg) {
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_SINGULAR;
		}

		// Solve the system 
		le2.dgetrs(trans, n, nrhs, A_trans.data_double, lda, ipiv,
			B_trans.data_double, ldb, info);

		// Check for errors
		if (info[0] < 0) {
			System.err.println("solve_Mat_rm: LAPACK dgetrs error code " + info[0]);
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}
		// Transpose results
		status = transpose_Mat_rm(B_trans, X);
		if (status == SIFT3D_FAILURE) {
			if (ipiv != null) {
				ipiv = null;
			}
			if (work != null) {
				work = null;
			}
			if (iwork != null) {
				iwork = null;
			}
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}

		if (ipiv != null) {
			ipiv = null;
		}
		if (work != null) {
			work = null;
		}
		if (iwork != null) {
			iwork = null;
		}
		cleanup_Mat_rm(A_trans);
		cleanup_Mat_rm(B_trans);
		return SIFT3D_SUCCESS;
	}
	
	/* Tranposes a matrix. Resizes dst with the type of src. 
	 * All matrices must be initialized prior to calling this function. */
	private int transpose_Mat_rm(Mat_rm src, Mat_rm dst)
	{

		int status;
		int i, j;

		// Verify inputs
		if (src.num_rows < 1 || src.num_cols < 1)
			return SIFT3D_FAILURE;

		// Resize the output
		dst.type = src.type;
		dst.num_rows = src.num_cols;
		dst.num_cols = src.num_rows;
		status = resize_Mat_rm(dst);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}
	   
	    if (src.type == Mat_rm_type.SIFT3D_DOUBLE) {
	        for (i = 0; i < src.num_rows; i++) {
	    	   for (j = 0; j < src.num_cols; j++) {
	    	    	dst.data_double[j][i] = src.data_double[i][j];
	    	    }
	        }
	    }
	    else if (src.type == Mat_rm_type.SIFT3D_FLOAT) {
	    	for (i = 0; i < src.num_rows; i++) {
		        for (j = 0; j < src.num_cols; j++) {
		    	    	dst.data_float[j][i] = src.data_float[i][j];
		    	}
		    }
	    }
	    else if (src.type == Mat_rm_type.SIFT3D_INT) {
	    	for (i = 0; i < src.num_rows; i++) {
		        for (j = 0; j < src.num_cols; j++) {
		    	    	dst.data_int[j][i] = src.data_int[i][j];
		    	}
		    }	    	
	    }
	    else {
	    	System.err.println("transpose_Mat_rm: unknown type");
	        return SIFT3D_FAILURE;
	    }

		return SIFT3D_SUCCESS;
	}
	
	/* Solves the system AX=B by least-squares.
	 *
	 * A least-norm solution is computed using the singular 
	 * value decomposition. A need not be full-rank.
	 * 
	 * This function returns an error if A and B do not have valid dimensions. 
	 * This function resizes X to [nx1] and changes the type to match B. 
	 * All matrices must be initialized prior to calling this funciton.
	 * All matrices must have type double.
	 */
	int solve_Mat_rm_ls(Mat_rm A, Mat_rm B, 
	        Mat_rm X)
	{
        int status;
		Mat_rm A_trans = new Mat_rm();
		Mat_rm B_trans = new Mat_rm();
		double s[], work[];
		double lwork_ret[] = new double[1];
		int info[] = new int[1];
		int rank[] = new int[1];
		int lwork;
		int i, j;

		final double rcond = -1;
		final int m = A.num_rows;
		final int n = A.num_cols;
		final int nrhs = B.num_cols;
		final int lda = m;
		final int ldb = B.num_rows;
		final int lwork_query = -1;

		// Verify inputs 
		if (m != ldb) {
			System.err.println("solve_Mat_rm_ls: invalid dimensions");
			return SIFT3D_FAILURE;
		}
		if (A.type != Mat_rm_type.SIFT3D_DOUBLE || B.type != Mat_rm_type.SIFT3D_DOUBLE) {
			System.err.println("solve_mat_rm_ls: All matrices must have type double");
			return SIFT3D_FAILURE;
		}
		// Resize the output 
		X.type = Mat_rm_type.SIFT3D_DOUBLE;
		X.num_rows = A.num_cols;
		X.num_cols = B.num_cols;
		status = resize_Mat_rm(X);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Initialize intermediate matrices and buffers
		status = init_Mat_rm(A_trans, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		if (status == SIFT3D_FAILURE) {
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}
		status = init_Mat_rm(B_trans, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		if (status == SIFT3D_FAILURE) {
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}
		try {
			s = new double[Math.max(m,n)];
		}
		catch (OutOfMemoryError e) {
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}

		// Transpose matrices for LAPACK
		status = transpose_Mat_rm(A, A_trans);
		if (status == SIFT3D_FAILURE) {
			if (s != null)
				s = null;
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;		
		}
	    status = transpose_Mat_rm(B, B_trans);
	    if (status == SIFT3D_FAILURE) {
			if (s != null)
				s = null;
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;		
		}

		// Get the size of the workspace
	    GeneralizedInverse2 ge2 = new GeneralizedInverse2();
		ge2.dgelss(m, n, nrhs, A_trans.data_double, lda,
			B_trans.data_double, ldb, s, rcond, rank, lwork_ret,
			lwork_query, info);
		if (info[0] != 0) {
	        System.err.println("solve_mat_rm: LAPACK dgelss work query error code = " + info[0]);
		}
		lwork = (int)lwork_ret[0];

		// Allocate the workspace
		try {
			work = new double[lwork];
		}
		catch (OutOfMemoryError e) {
			if (s != null)
				s = null;
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}

		// Solve the system
		ge2.dgelss(m, n, nrhs, A_trans.data_double, lda,
			B_trans.data_double, ldb, s, rcond, rank, work, lwork,
			info);
		if (info[0] != 0) {
			System.err.println("solve_mat_rm: LAPACK dgelss error code = " + info[0]);
			if (s != null)
				s = null;
			if (work != null)
				work = null;
			cleanup_Mat_rm(A_trans);
			cleanup_Mat_rm(B_trans);
			return SIFT3D_FAILURE;	
		}
		// Transpose results to the new leading dimension
		for (i = 0; i < X.num_rows; i++) {
			for (j = 0; j < X.num_cols; j++) {
				X.data_double[i][j] = B_trans.data_double[j][i];
			}
		}
		
		if (s != null)
			s = null;
		if (work != null)
			work = null;
		cleanup_Mat_rm(A_trans);
		cleanup_Mat_rm(B_trans);
		return SIFT3D_SUCCESS;
	}

	/* Set an Affine transform to the given matrix.
	 * mat is copied. mat must be an n x (n + 1) matrix, where
	 * n is the dimensionality of the transformation. */
	private int Affine_set_mat(Mat_rm mat, Affine affine)
	{

		// Verify inputs
		if (mat.num_cols != mat.num_rows + 1 || mat.num_rows < 2)
			return SIFT3D_FAILURE;

		return convert_Mat_rm(mat, affine.A, Mat_rm_type.SIFT3D_DOUBLE);
	}
	
	/* Convert a matrix to a different type. in and out may be the same pointer.
	 * 
	 * This function resizes out.
	 * 
	 * All matrices must be initialized prior to calling this function. */
	private int convert_Mat_rm(Mat_rm in, Mat_rm out,
			   Mat_rm_type type)
	{

		int status;
		int i, j;

		// Resize the output
		out.num_rows = in.num_rows;
		out.num_cols = in.num_cols;
		out.type = type;
		status = resize_Mat_rm(out);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}
		
		if (in.type == Mat_rm_type.SIFT3D_DOUBLE) {
		    if (out.type == Mat_rm_type.SIFT3D_FLOAT) {
		        for (i = 0; i < in.num_rows; i++) {
		        	for (j = 0; j < in.num_cols; j++) {
		        		out.data_float[i][j] = (float)in.data_double[i][j];
		        	}
		        }
		    }
		    else if (out.type == Mat_rm_type.SIFT3D_INT) {
		    	for (i = 0; i < in.num_rows; i++) {
		        	for (j = 0; j < in.num_cols; j++) {
		        		out.data_int[i][j] = (int)in.data_double[i][j];
		        	}
		        }	
		    }
		}
		else if (in.type == Mat_rm_type.SIFT3D_FLOAT) {
            if (out.type == Mat_rm_type.SIFT3D_DOUBLE) {
            	for (i = 0; i < in.num_rows; i++) {
		        	for (j = 0; j < in.num_cols; j++) {
		        		out.data_double[i][j] = (double)in.data_float[i][j];
		        	}
		        }   	
		    }
		    else if (out.type == Mat_rm_type.SIFT3D_INT) {
		    	for (i = 0; i < in.num_rows; i++) {
		        	for (j = 0; j < in.num_cols; j++) {
		        		out.data_int[i][j] = (int)in.data_float[i][j];
		        	}
		        }   		
		    }	
		}
		else if (in.type == Mat_rm_type.SIFT3D_INT) {
            if (out.type == Mat_rm_type.SIFT3D_DOUBLE) {
            	for (i = 0; i < in.num_rows; i++) {
		        	for (j = 0; j < in.num_cols; j++) {
		        		out.data_double[i][j] = (double)in.data_int[i][j];
		        	}
		        }   		
		    }
		    else if (out.type == Mat_rm_type.SIFT3D_FLOAT) {
		    	for (i = 0; i < in.num_rows; i++) {
		        	for (j = 0; j < in.num_cols; j++) {
		        		out.data_float[i][j] = (float)in.data_int[i][j];
		        	}
		        }   	
		    }	
		}
		else {
			System.err.println("convert_Mat_rm: unknown type of input matrix");
	        return SIFT3D_FAILURE;	
		}

	    return SIFT3D_SUCCESS;
	}
	
	//Find the SSD error for the i'th point
	private double tform_err_sq(Affine tform, Mat_rm src, 
	        Mat_rm ref, int i)
	{

		double err = 0.0;
		//Initialization
		//in -- inputs coordinates of source points
		//out -- registered points
		//r -- reference points (ground truth)
		double x_in, y_in, z_in, x_r, y_r, z_r;
		double x_out[] = new double[1];
		double y_out[] = new double[1];
		double z_out[] = new double[1];

		//Find the source point
		x_in = ref.data_double[i][0];
		y_in = ref.data_double[i][1];
		z_in = ref.data_double[i][2];

		//Register
		apply_Affine_xyz(tform, x_in, y_in, z_in, x_out, y_out, z_out);

		//Find the reference point
		x_r = src.data_double[i][0];
		y_r = src.data_double[i][1];
		z_r = src.data_double[i][2];

		//Find the SSD error
		err = (x_r - x_out[0]) * (x_r - x_out[0]) + (y_r - y_out[0]) * (y_r - y_out[0]) +
		    (z_r - z_out[0]) * (z_r - z_out[0]);

		//return the result 
		return err;
	}
	
	/* Apply an Affine transformation to an [x, y, z] triple. */
	private void apply_Affine_xyz(Affine affine, double x_in,
				     double y_in, double z_in,
				     double x_out[], double y_out[],
				     double z_out[])
	{

		final Mat_rm A = affine.A;
		if (affine.A.num_rows != 3) {
			System.err.println("In apply_Affine_xyz affine.A.num_rows = " + affine.A.num_rows + " instead of the required 3");
			return;
		}
		x_out[0] = A.data_double[0][0] * x_in +
		    A.data_double[0][1] * y_in +
		    A.data_double[0][2] * z_in +
		    A.data_double[0][3];
		y_out[0] = A.data_double[1][0] * x_in +
		    A.data_double[1][1] * y_in +
		    A.data_double[1][2] * z_in +
		    A.data_double[1][3];
		z_out[0] = A.data_double[2][0] * x_in +
		    A.data_double[2][1] * y_in +
		    A.data_double[2][2] * z_in +
		    A.data_double[2][3];
	}
	
	/* Deep copy of one Affine to another. Both must be initialized. */
	private int copy_Affine(Affine src, Affine dst)
	{

		return Affine_set_mat(src.A, dst);
	}
	
	/* Convert a transformation from mm to image space.
	 *
	 * Parameters:
	 *   tform: The transformation, which shall be modified.
	 *   src_units: The units of the source image, array of length IM_NDIMS.
	 *   ref_units: As src_units, but for the reference image.
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise.
	 */
	private int mm2im(double src_units[], double ref_units[],
	        Affine aff) {

	        
            int i, j;

            Mat_rm A = aff.A;

            // Verify the dimensions
            if (A.num_rows != IM_NDIMS) {
                    System.err.println("mm2im: Invalid transform dimensionality: " + A.num_rows);
                    return SIFT3D_FAILURE;
            }

            // Convert the Affine transformation matrix in-place
            for (i = 0; i < A.num_rows; i++) {
            	for (j = 0; j < A.num_cols; j++) {
            		// Invert the input transformation ref->mm
                    A.data_double[i][j] *= 
                            j < IM_NDIMS ? ref_units[j] : 1.0;

                    // Invert the output transformation src->mm
                    A.data_double[i][j] /= 
                            src_units[i];	
            	}
            }
	                
	        return SIFT3D_SUCCESS; 
	}
	
	/* Write an affine transformation to a file. */
	private int write_Affine(String path, Affine affine)
	{
		return write_Mat_rm(path, affine.A);
	}

	/* Write a matrix to a .csv or .csv.gz file. */
	private int write_Mat_rm(String path, Mat_rm mat)
	{

		GZIPOutputStream gzout;
		String ext = null;
		int i, j;
		boolean compress;
		RandomAccessFile raFile = null;
		byte buf[];
		int gzipLen;
       

		// Get the file extension
        int index = path.lastIndexOf(".");
        if (index == -1) {
        	ext = null;
        }
        else {
        	ext = path.substring(index+1);
        }

		// Check if we need to compress the file
        if (ext != null) {
		    compress = ext.equalsIgnoreCase(ext_gz);
        }
        else {
        	compress = false;
        }

		// Open the file
		if (compress) {
			try {
                // Create the GZIP output stream
                gzout = new GZIPOutputStream(new FileOutputStream(fileDir + File.separator + path));
            } catch (final IOException e) {
                MipavUtil.displayError("IOException on new GZIPOutputStream");
                return SIFT3D_FAILURE;
            }
			if (mat.type == Mat_rm_type.SIFT3D_DOUBLE) {
	            for (i = 0; i < mat.num_rows; i++) {
	            	for (j = 0; j < mat.num_cols; j++) {
	            		String delim = j < mat.num_cols-1 ? "," : "\n";
	            		String value = String.valueOf(mat.data_double[i][j]) + delim;
	            		buf = value.getBytes();
	            		gzipLen = buf.length;
	            		 try {
                             gzout.write(buf, 0, gzipLen);
                         } catch (final IOException e) {
                             System.err.println("IOException on byte transfer to gzip file");
                             return SIFT3D_FAILURE;
                         }
	            	}
	            }
	        }
			else if (mat.type == Mat_rm_type.SIFT3D_FLOAT) {
	            for (i = 0; i < mat.num_rows; i++) {
	            	for (j = 0; j < mat.num_cols; j++) {
	            		String delim = j < mat.num_cols-1 ? "," : "\n";
	            		String value = String.valueOf(mat.data_float[i][j]) + delim;
	            		buf = value.getBytes();
	            		gzipLen = buf.length;
	            		 try {
                             gzout.write(buf, 0, gzipLen);
                         } catch (final IOException e) {
                             System.err.println("IOException on byte transfer to gzip file");
                             return SIFT3D_FAILURE;
                         }
	            	}
	            }
	        }
			else if (mat.type == Mat_rm_type.SIFT3D_INT) {
	            for (i = 0; i < mat.num_rows; i++) {
	            	for (j = 0; j < mat.num_cols; j++) {
	            		String delim = j < mat.num_cols-1 ? "," : "\n";
	            		String value = String.valueOf(mat.data_int[i][j]) + delim;
	            		buf = value.getBytes();
	            		gzipLen = buf.length;
	            		 try {
                             gzout.write(buf, 0, gzipLen);
                         } catch (final IOException e) {
                             System.err.println("IOException on byte transfer to gzip file");
                             return SIFT3D_FAILURE;
                         }
	            	}
	            }
	        }
			
			 // complete the gzip file
            try {
                gzout.finish();
            } catch (final IOException e) {
                MipavUtil.displayError("IOException on gzout.finish()");
                return  SIFT3D_FAILURE;
            }
            try {
                gzout.close();
            } catch (final IOException e) {
                MipavUtil.displayError("IOException on gzout.close()");
                return SIFT3D_FAILURE;
            }
		} else {
			File file = new File(fileDir + File.separator + path);
	        try {
	            raFile = new RandomAccessFile(file, "rw");
	        }
	        catch (FileNotFoundException e) {
	        	return SIFT3D_FAILURE;
	        }

	        // Necessary so that if this is an overwritten file there isn't any
	        // junk at the end
	        try {
	            raFile.setLength(0);
	        }
	        catch (IOException e) {
	        	return SIFT3D_FAILURE;
	        }
	        if (mat.type == Mat_rm_type.SIFT3D_DOUBLE) {
	            for (i = 0; i < mat.num_rows; i++) {
	            	for (j = 0; j < mat.num_cols; j++) {
	            		String delim = j < mat.num_cols-1 ? "," : "\n";
	            		String value = String.valueOf(mat.data_double[i][j]) + delim;
	            		try {
	            			raFile.write(value.getBytes());
	            		}
	            		catch (IOException e) {
	            			return SIFT3D_FAILURE;
	            		}
	            	}
	            }
	        }
	        else if (mat.type == Mat_rm_type.SIFT3D_FLOAT) {
	        	for (i = 0; i < mat.num_rows; i++) {
	            	for (j = 0; j < mat.num_cols; j++) {
	            		String delim = j < mat.num_cols-1 ? "," : "\n";
	            		String value = String.valueOf(mat.data_float[i][j]) + delim;
	            		try {
	            			raFile.write(value.getBytes());
	            		}
	            		catch (IOException e) {
	            			return SIFT3D_FAILURE;
	            		}
	            	}
	            }	
	        }
	        else if (mat.type == Mat_rm_type.SIFT3D_INT) {
	        	for (i = 0; i < mat.num_rows; i++) {
	            	for (j = 0; j < mat.num_cols; j++) {
	            		String delim = j < mat.num_cols-1 ? "," : "\n";
	            		String value = String.valueOf(mat.data_int[i][j]) + delim;
	            		try {
	            			raFile.write(value.getBytes());
	            		}
	            		catch (IOException e) {
	            			return SIFT3D_FAILURE;
	            		}
	            	}
	            }	
	        }
	        try {
	        	raFile.close();
	        }
	        catch (IOException e) {
	        	return SIFT3D_FAILURE;
	        }
		}
		return SIFT3D_SUCCESS;

	}
	
	/* Transform an image according to the inverse of the provided tform. 
	 * 
	 * Paramters:
	 *   tform: The transformation. 
	 *   src: The input image.
	 *   interp: The type of interpolation.
	 *   resize: If true, resizes the dst to be the same size as src. Otherwise,
	 *     uses the dimensions of dst. 
	 *   dst: The output image.
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
	private int im_inv_transform(Affine tform, Image src,
			     interp_type interp, int resize, 
	                     Image dst)
	{
		int status;
		int x, y, z, c;
		double transx[] = new double[1];
		double transy[] = new double[1];
		double transz[] = new double[1];

		// Optionally resize the output image
		if (resize == SIFT3D_TRUE) {
		    status = im_copy_dims(src, dst);
		    if (status == SIFT3D_FAILURE) {
			    return SIFT3D_FAILURE;
		    }
		}
		
		if (interp == interp_type.LINEAR) {
		    for (z = 0; z < dst.nz; z++) {
		    	for (y = 0; y < dst.ny; y++) {
		    		for (x = 0; x < dst.nx; x++) {
		    			apply_Affine_xyz(tform, (double)x, (double)y, (double)z, transx, transy, transz);
		    			for (c = 0; c < dst.nc; c++) {
		    			    dst.data[x * dst.xs + y * dst.ys + z * dst.zs + c] = resample_linear(src, transx[0], transy[0], transz[0], c);	
		    			}
		    		}
		    	}
		    }
		}
		else if (interp == interp_type.LANCZOS2) {
			for (z = 0; z < dst.nz; z++) {
		    	for (y = 0; y < dst.ny; y++) {
		    		for (x = 0; x < dst.nx; x++) {
		    			apply_Affine_xyz(tform, (double)x, (double)y, (double)z, transx, transy, transz);
		    			for (c = 0; c < dst.nc; c++) {
		    			    dst.data[x * dst.xs + y * dst.ys + z * dst.zs + c] = resample_lanczos2(src, transx[0], transy[0], transz[0], c);	
		    			}
		    		}
		    	}
		    }	
		}
		else {
			System.err.println("im_inv_transform: unrecognized interpolation type");
				return SIFT3D_FAILURE;	
		}

		return SIFT3D_SUCCESS;
	}
	
	/* Copy an image's dimensions and stride into another. 
	 * This function resizes dst.
	 * 
	 * @param src The source image.
	 * @param dst The destination image.
	 * @return Returns SIFT3D_SUCCESS or SIFT3D_FAILURE.
	 */
	private int im_copy_dims(Image src, Image dst)
	{
	        if (src.data == null)
	                return SIFT3D_FAILURE;

		dst.nx = src.nx;
		dst.ny = src.ny;
		dst.nz = src.nz;
		dst.xs = src.xs;
		dst.ys = src.ys;
		dst.zs = src.zs;
		dst.nc = src.nc;
	    dst.ux = src.ux;
	    dst.uy = src.uy;
	    dst.uz = src.uz;

		return im_resize(dst);
	}
	
	/* Helper routine for image transformation. Performs trilinear
	 * interpolation, setting out-of-bounds voxels to zero. */
	private double resample_linear(Image in, double x,
				      double y, double z, int c)
	{

		// Detect out-of-bounds
		if (x < 0 || x > in.nx - 1 ||
		    y < 0 || y > in.ny - 1 || z < 0 || z > in.nz - 1)
			return 0.0;

		int fx = (int)Math.floor(x);
		int fy = (int)Math.floor(y);
		int fz = (int)Math.floor(z);
		int cx = (int)Math.ceil(x);
		int cy = (int)Math.ceil(y);
		int cz = (int)Math.ceil(z);

		double dist_x = x - fx;
		double dist_y = y - fy;
		double dist_z = z - fz;

		double c0 = in.data[fx * in.xs + fy * in.ys + fz * in.zs + c];
		double c1 = in.data[fx * in.xs + cy * in.ys + fz * in.zs + c];
		double c2 = in.data[cx * in.xs + fy * in.ys + fz * in.zs + c];
		double c3 = in.data[cx * in.xs + cy * in.ys + fz * in.zs + c];
		double c4 = in.data[fx * in.xs + fy * in.ys + cz * in.zs + c];
		double c5 = in.data[fx * in.xs + cy * in.ys + cz * in.zs + c];
		double c6 = in.data[cx * in.xs + fy * in.ys + cz * in.zs + c];
		double c7 = in.data[cx * in.xs + cy * in.ys + cz * in.zs + c];

		double out = c0 * (1.0 - dist_x) * (1.0 - dist_y) * (1.0 - dist_z)
		    + c1 * (1.0 - dist_x) * dist_y * (1.0 - dist_z)
		    + c2 * dist_x * (1.0 - dist_y) * (1.0 - dist_z)
		    + c3 * dist_x * dist_y * (1.0 - dist_z)
		    + c4 * (1.0 - dist_x) * (1.0 - dist_y) * dist_z
		    + c5 * (1.0 - dist_x) * dist_y * dist_z
		    + c6 * dist_x * (1.0 - dist_y) * dist_z
		    + c7 * dist_x * dist_y * dist_z;

		return out;
	}
	
	/* Helper routine to resample an image at a point, using the Lanczos kernel */
	private double resample_lanczos2(Image im, double x,
					double y, double z, int c)
	{

		double val;
		int xs, ys, zs;

		//TODO: faster separable implementation

		// Kernel parameter
		final double a = 2;

		// Check bounds
		final double xMin = 0;
		final double yMin = 0;
		final double zMin = 0;
		final double xMax = im.nx - 1;
		final double yMax = im.ny - 1;
		final double zMax = im.nz - 1;
		if (x < xMin || y < yMin || z < zMin ||
		    x > xMax || y > yMax || z > zMax)
			return 0.0;

		// Window 
		final int x_start = (int)Math.max(Math.floor(x) - a, xMin);
		final int x_end = (int)Math.min(Math.floor(x) + a, xMax);
		final int y_start = (int)Math.max(Math.floor(y) - a, yMin);
		final int y_end = (int)Math.min(Math.floor(y) + a, yMax);
		final int z_start = (int)Math.max(Math.floor(z) - a, zMin);
		final int z_end = (int)Math.min(Math.floor(z) + a, zMax);

		// Iterate through the window 
		val = 0.0;
		for (zs = z_start; zs <= z_end; zs++) {
			for (ys = y_start; ys <= y_end; ys++) {
				for (xs = x_start; xs <= x_end; xs++) {

	                // Evaluate the kernel
					double xw = Math.abs((double)xs - x) + DBL_EPSILON;
					double yw = Math.abs((double)ys - y) + DBL_EPSILON;
					double zw = Math.abs((double)zs - z) + DBL_EPSILON;
					double kernel = lanczos(xw, a) * lanczos(yw, a) * lanczos(zw, a);
			
					// Accumulate
					val += kernel * im.data[xs * im.xs + ys * im.ys + zs * im.zs + c];
				}
		    }
		}
		return val;
	}

	/* Lanczos kernel function */
	private double lanczos(double x, double a)
	{
		double pi_x = Math.PI * x;
		return a * Math.sin(pi_x) * Math.sin(pi_x / a) / (pi_x * pi_x);
	}
	
	public ModelImage getResultImage( ) {
		return warpedImage;
	}
	
	/* Set the source image. This makes a deep copy of the data, so you are free
	 * to modify src after calling this function. */
	private int set_src_Reg_SIFT3D(Reg_SIFT3D reg, Image src) {
	        return set_im_Reg_SIFT3D(reg, src, reg.src_units, reg.desc_src);
	}

	/* The same as set_source_Reg_SIFT3D, but sets the reference image. */
	private int set_ref_Reg_SIFT3D(Reg_SIFT3D reg, Image ref) {
	        return set_im_Reg_SIFT3D(reg, ref, reg.ref_units, reg.desc_ref);
	}
	
	/* Helper function for set_src_Reg_SIFT3D and set_ref_Reg_SIFT3D.
	 * 
	 * Parameters:
	 *   reg - The Reg_SIFT3D struct.
	 *   im - The image, either source or reference.
	 *   units - The units array in Reg_SIFT3D to be modified.
	 *   desc - The descriptor store in Reg_SIFT3D to be modified.
	 * 
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
	private int set_im_Reg_SIFT3D(Reg_SIFT3D reg, Image im,
	        double units[], SIFT3D_Descriptor_store desc) {
		    
            int status;
	        Keypoint_store kp = new Keypoint_store(); 

	        SIFT3DC sift3d = reg.sift3d; 

	        /* Initialize intermediates */ 
	        init_Keypoint_store(kp); 

	        /* Save the units */ 
	        units[0] = im.ux;
	        units[1] = im.uy;
	        units[2] = im.uz;

	        /* Detect keypoints */ 
		    status = SIFT3D_detect_keypoints(sift3d, im, kp);
		    if (status == SIFT3D_FAILURE) {
			    System.err.println("im_Reg_SIFT3D: failed to detect keypoints"); 
	            cleanup_Keypoint_store(kp);
	            return SIFT3D_FAILURE;
	        } 

	        /* Extract descriptors */ 
		    status = SIFT3D_extract_descriptors(sift3d, kp, desc); 
		    if (status == SIFT3D_FAILURE) {
	            System.err.println("im_Reg_SIFT3D: failed to extract descriptors"); 
	            cleanup_Keypoint_store(kp);
	            return SIFT3D_FAILURE;
	        } 

	        /* Clean up */ 
	        cleanup_Keypoint_store(kp); 

	        return SIFT3D_SUCCESS; 
	} 
	
	/* Initialize a Keypoint_store for first use.
	 * This does not need to be called to reuse the store
	 * for a new image. */
	private void init_Keypoint_store(Keypoint_store kp) {
		init_Slab(kp.slab);
		kp.buf = kp.slab.buf;
	}
	
	/* Initialize a Slab for first use */
	private void init_Slab(Slab slab) {
	    slab.buf_size = slab.num = 0;
	    slab.buf = null;
	}
	
	/* Free all memory associated with a Keypoint_store. kp cannot be
	 * used after calling this function, unless re-initialized. */
	private void cleanup_Keypoint_store(Keypoint_store kp) {
	        cleanup_Slab(kp.slab);
	}
	
	/* Free all memory associated with a slab. Slab cannot be re-used after 
	 * calling this function, unless re-initialized. */
	private void cleanup_Slab(Slab slab)
	{
	        if (slab.buf != null)
		        slab.buf = null;
	}
	
	/* Detect keypoint locations and orientations. You must initialize
	 * the SIFT3D struct, image, and keypoint store with the appropriate
	 * functions prior to calling this function. */
	private int SIFT3D_detect_keypoints(SIFT3DC sift3d, Image im,
				    Keypoint_store kp) {

	        int status;
		    // Verify inputs
	        if (im.nc != 1) {
	            System.err.println("SIFT3D_detect_keypoints: invalid number of image channels: " + im.nc);
	            System.err.println("Only single-channel images are supported");
	            return SIFT3D_FAILURE;
	        }

	        // Set the image       
	        status = set_im_SIFT3D(sift3d, im);
	        if (status == SIFT3D_FAILURE) {
	        	System.err.println("SIFT3D_detect_keypoints failed on set_im_SIFT3D(sift3d, im)");
	            return SIFT3D_FAILURE;
	        }

		// Build the GSS pyramid
		status = build_gpyr(sift3d);
		if (status == SIFT3D_FAILURE) {
			System.err.println("SIFT3D_detect_keypoints failed on build_gpyr(sift3d)");
            return SIFT3D_FAILURE;
        }

		// Build the DoG pyramid
		status = build_dog(sift3d);
		if (status == SIFT3D_FAILURE) {
			System.err.println("SIFT3D_detect_keypoints failed on build_dog(sift3d)");
            return SIFT3D_FAILURE;
		}

		// Detect extrema
		status = detect_extrema(sift3d, kp);
		if (status == SIFT3D_FAILURE) {
			System.err.println("SIFT3D_detect_keypoints failed on detect_extrema(sift3d, kp)");
            return SIFT3D_FAILURE;
		}

		// Assign orientations
		status = assign_orientations(sift3d, kp);
		if (status == SIFT3D_FAILURE) {
			System.err.println("SIFT3D_detect_keypoints failed on assign_orientations(sift3d, kp)");
            return SIFT3D_FAILURE;
		}

		return SIFT3D_SUCCESS;
	}

	/* Helper routine to begin processing a new image. If the dimensions differ
	 * from the last one, this function resizes the SIFT3D struct. */
	private int set_im_SIFT3D(SIFT3DC sift3d, Image im) {

	    int status;    

	        final Pyramid gpyr = sift3d.gpyr;
	        final int num_kp_levels = gpyr.num_kp_levels;

	        // Make a copy of the input image
	        status = im_copy_data(im, sift3d.im);
	        if (status == SIFT3D_FAILURE) {
	        	System.err.println("In set_im_SIFT3D failed on im_copy_data(im, sift3d.im)");
	            return SIFT3D_FAILURE;
	        }

	        // Scale the input image to [-1, 1]
	        im_scale(sift3d.im);

	        // Resize the internal data, if necessary
	     
	        status = resize_SIFT3D(sift3d, num_kp_levels);
	        if (status == SIFT3D_FAILURE) {
	        	System.err.println("In set_im_SIFT3D failed on resize_SIFT3D(sift3d, num_kp_levels)");
	        	return SIFT3D_FAILURE;
	        }
	                
	        return SIFT3D_SUCCESS;
	}
	
	/* Copy an image's data into another. This function
	 * changes the dimensions and stride of dst,
	 * and allocates memory. */
	private int im_copy_data(Image src, Image dst)
	{

		int status;
		int x, y, z, c;

	        // Return if src has no data 
	        if (src.data == null)
	                return SIFT3D_FAILURE;

		// Return if src and dst are the same
		if (dst.data == src.data)
			return SIFT3D_SUCCESS;

		// Resize dst
		status = im_copy_dims(src, dst);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Copy data
		for (z = 0; z < dst.nz; z++) {
			for (y = 0; y < dst.ny; y++) {
				for (x = 0; x < dst.nx; x++) {
					for (c = 0; c < dst.nc; c++) {
					    dst.data[z*dst.zs + y*dst.ys + x*dst.xs + c] = src.data[z*src.zs + y*src.ys + x*src.xs + c];
					}
				}
			}
		}

		return SIFT3D_SUCCESS;
	}

	private double im_max_abs(Image im) {

        double max;
        int x, y, z, c;

	max = 0.0;
	for (z = 0; z < im.nz; z++) {
		for (y = 0; y < im.ny; y++) {
			for (x = 0; x < im.nx; x++) {
				for (c = 0; c < im.nc; c++) {
				    double samp = Math.abs(im.data[z*im.zs + y*im.ys + x*im.xs + c]);
				    max = Math.max(max,samp);
				}
			}
		}
	}

        return max;
}

	/* Scale an image to the [-1, 1] range, where
	 * the largest absolute value is 1. */
	private void im_scale(Image im)
	{
	
		int x, y, z, c;
	
	        // Find the maximum absolute value
		    final double max = im_max_abs(im);
	        if (max == 0.0)
		        return;
	
		// Divide by the max
	        for (z = 0; z < im.nz; z++) {
	    		for (y = 0; y < im.ny; y++) {
	    			for (x = 0; x < im.nx; x++) {
	    				for (c = 0; c < im.nc; c++) {
	    				    im.data[z*im.zs + y*im.ys + x*im.xs + c] /= max;
	    				}
	    			}
	    		}
	    	}
		
	}
	
	/* Build the GSS pyramid on a single CPU thread */
	private int build_gpyr(SIFT3DC sift3d) {

	    int status;
		Image prev;
		Sep_FIR_filter f;
		Image cur;
		int o, s;

		Pyramid gpyr = sift3d.gpyr;
		final GSS_filters gss = sift3d.gss;
		final int s_start = gpyr.first_level + 1;
		final int s_end = SIFT3D_PYR_LAST_LEVEL(gpyr);
		final int o_start = gpyr.first_octave;
		final int o_end = SIFT3D_PYR_LAST_OCTAVE(gpyr);
	    final double unit = 1.0;

		// Build the first image
		cur = SIFT3D_PYR_IM_GET(gpyr, o_start, s_start - 1);
		prev = sift3d.im;
	/*#ifdef SIFT3D_USE_OPENCL
		if (im_load_cl(cur, SIFT3D_FALSE))
			return SIFT3D_FAILURE;	
	#endif*/

		f = gss.first_gauss.f;
		status = apply_Sep_FIR_filter(prev, cur, f, unit);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Build the rest of the pyramid
		for (o = o_start; o <= o_end; o++) {
			for (s = s_start; s <= s_end; s++) {
				cur = SIFT3D_PYR_IM_GET(gpyr, o, s);
				prev = SIFT3D_PYR_IM_GET(gpyr, o, s - 1);
				f = gss.gauss_octave[s].f;
				status = apply_Sep_FIR_filter(prev, cur, f, unit);
				if (status == SIFT3D_FAILURE) {
					return SIFT3D_FAILURE;
				}
	/*#ifdef SIFT3D_USE_OPENCL
				if (im_read_back(cur, SIFT3D_FALSE))
					return SIFT3D_FAILURE;
	#endif*/
			}
			// Downsample
			if (o != o_end) {

	                        final int downsample_level = 
	                                Math.max(s_end - 2, gpyr.first_level);

				prev = SIFT3D_PYR_IM_GET(gpyr, o, downsample_level);
				cur = SIFT3D_PYR_IM_GET(gpyr, o + 1, s_start - 1);

	            if (Math.abs(prev.s - cur.s) >= FLT_EPSILON) {
	            	System.err.println("if (Math.abs(prev.s - cur.s) >= FLT_EPSILON)");
	            	return SIFT3D_FAILURE;
	            }

				status = im_downsample_2x(prev, cur);
				if (status == SIFT3D_FAILURE) {
					return SIFT3D_FAILURE;
				}

			}
		}

	/*#ifdef SIFT3D_USE_OPENCL
		clFinish_all();
	#endif*/

		return SIFT3D_SUCCESS;
	}
	
	/* Apply a separable filter in multiple dimensions. This function resamples the
	 * input to have the same units as f, then resamples the output to the
	 * original units.
	 *
	 * Parameters:
	 *  -src: The input image.
	 *  -dst: The filtered image.
	 *  -f: The filter to apply.
	 *  -unit: The physical units of the filter kernel. Use -1.0 for the default,
	 *      which is the same units as src.
	 *
	 * Return: SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
	private int apply_Sep_FIR_filter(Image src, Image dst,
				 Sep_FIR_filter f, double unit)
	{
        int status;
		Image temp = new Image();
		Image cur_src, cur_dst;
		int i;

	        final double unit_default = -1.0;

	        // Verify inputs
	        if (unit < 0 && unit != unit_default) {
	           System.err.println("apply_Sep_FIR_filter: invalid unit: " + unit);
	           System.err.println("use " + unit_default + " for default");
	           return SIFT3D_FAILURE;
	        }

	        // Resize the output
	        status = im_copy_dims(src, dst);
	        if (status == SIFT3D_FAILURE) {
	            return SIFT3D_FAILURE; 
	        }

		// Allocate temporary storage
		init_im(temp);
		status = im_copy_data(src, temp);
		if (status == SIFT3D_FAILURE) {
			im_free(temp);
			return SIFT3D_FAILURE;
		}

	/*#define SWAP_BUFFERS \
	    if (cur_dst == &temp) { \
	    cur_src = &temp; \
	    cur_dst = dst; \
	    } else { \
	    cur_src = dst; \
	    cur_dst = &temp; \
	    }*/

		// Apply in n dimensions
		cur_src = src;
		cur_dst = temp;
		double unit_arg;
		for (i = 0; i < IM_NDIMS; i++) {

	                // Check for default parameters
			        if (i == 0) { 
	                unit_arg = unit == unit_default ?
	                        src.ux : unit;
			        }
			        else if (i == 1) {
			        	unit_arg = unit == unit_default ?
		                        src.uy : unit;	
			        }
			        else {
			        	unit_arg = unit == unit_default ?
		                        src.uz : unit;
			        }

	//#ifdef SIFT3D_USE_OPENCL
	//                convolve_sep(cur_src, cur_dst, f, i, unit_arg);
	//		SWAP_BUFFERS
	//#else
	                // Transpose so that the filter dimension is x
	                if (i != 0) {
	                   status = im_permute(cur_src, 0, i, cur_dst);
	                   if (status == SIFT3D_FAILURE) {
	                	   im_free(temp);
	                	   return SIFT3D_FAILURE;
	                   }
	                   if (cur_dst == temp) {
	               	    cur_src = temp;
	               	    cur_dst = dst;
	               	    } else {
	               	    cur_src = dst;
	               	    cur_dst = temp;
	               	    }
	                }

			// Apply the filter
			convolve_sep(cur_src, cur_dst, f, 0, unit_arg);
			if (cur_dst == temp) {
       	    cur_src = temp;
       	    cur_dst = dst;
       	    } else {
       	    cur_src = dst;
       	    cur_dst = temp;
       	    }

	                // Transpose back
	                if (i != 0) {
				        status = im_permute(cur_src, 0, i, cur_dst);
				        if (status == SIFT3D_FAILURE) {
		                	   im_free(temp);
		                	   return SIFT3D_FAILURE;
		                   }
		                   if (cur_dst == temp) {
		               	    cur_src = temp;
		               	    cur_dst = dst;
		               	    } else {
		               	    cur_src = dst;
		               	    cur_dst = temp;
		               	    }

			}
	//#endif
		}

		// Swap back
		if (cur_dst == temp) {
       	    cur_src = temp;
       	    cur_dst = dst;
       	    } else {
       	    cur_src = dst;
       	    cur_dst = temp;
       	    };

	//#undef SWAP_BUFFERS

		// Copy result to dst, if necessary
		if (cur_dst != dst) {
			status = im_copy_data(cur_dst, dst);
			if (status == SIFT3D_FAILURE) {
         	   im_free(temp);
         	   return SIFT3D_FAILURE;
            }
		}

		// Clean up
		im_free(temp);
		return SIFT3D_SUCCESS;
	}
	
	/* Permute the dimensions of an image.
	 *
	 * Arguments: 
	 * src - input image (initialized)
	 * dim1 - input permutation dimension (x = 0, y = 1, z = 2)
	 * dim2 - output permutation dimension (x = 0, y = 1, z = 2)
	 * dst - output image (initialized)
	 * 
	 * example:
	 * im_permute(src, dst, 0, 1) -- permute x with y in src
	 *                              and save to dst
	 */
	private int im_permute(Image src, int dim1, int dim2,
			 Image dst)
	{
		int status;
		int x, y, z, c;

		// Verify inputs
		if (dim1 < 0 || dim2 < 0 || dim1 > 3 || dim2 > 3) {
			System.err.println("im_permute: invalid dimensions: dim1 = " + dim1 + " dim2 = " + dim2);
			return SIFT3D_FAILURE;
		}

		// Check for the trivial case
		if (dim1 == dim2) {
			return im_copy_data(src, dst);
	    }

	        // Permute the units
		    dst.ux = src.ux;
		    dst.uy = src.uy;
		    dst.uz = src.uz;
	        if (dim1 == 0) {
	        	if (dim2 == 1) {
	        		dst.ux = src.uy;
	        		dst.uy = src.ux;
	        	}
	        	else if (dim2 == 2) {
	        		dst.ux = src.uz;
	        		dst.uz = src.ux;
	        	}
	        }
	        else if (dim1 == 1) {
	        	if (dim2 == 0) {
	        		dst.uy = src.ux;
	        		dst.ux = src.uy;
	        	}
	        	else if (dim2 == 2) {
	        		dst.uy = src.uz;
	        		dst.uz = src.uy;
	        	}
	        }
	        else if (dim1 == 2) {
	        	if (dim2 == 0) {
	        	    dst.uz = src.ux;
	        	    dst.ux = src.uz;
	        	}
	        	else if (dim2 == 1) {
	        		dst.uz = src.uy;
	        		dst.uy = src.uz;
	        	}
	        }
	        
		// Resize the output
	        dst.nx = src.nx;
		    dst.ny = src.ny;
		    dst.nz = src.nz;
	        if (dim1 == 0) {
	        	if (dim2 == 1) {
	        		dst.nx = src.ny;
	        		dst.ny = src.nx;
	        	}
	        	else if (dim2 == 2) {
	        		dst.nx = src.nz;
	        		dst.nz = src.nx;
	        	}
	        }
	        else if (dim1 == 1) {
	        	if (dim2 == 0) {
	        		dst.ny = src.nx;
	        		dst.nx = src.ny;
	        	}
	        	else if (dim2 == 2) {
	        		dst.ny = src.nz;
	        		dst.nz = src.ny;
	        	}
	        }
	        else if (dim1 == 2) {
	        	if (dim2 == 0) {
	        	    dst.nz = src.nx;
	        	    dst.nx = src.nz;
	        	}
	        	else if (dim2 == 1) {
	        		dst.nz = src.ny;
	        		dst.ny = src.nz;
	        	}
	        }
	        
		
		dst.nc = src.nc;
		im_default_stride(dst);
		status = im_resize(dst);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Transpose the data
		for (z = 0; z < dst.nz; z++) {
			for (y = 0; y < dst.ny; y++) {
				for (x = 0; x < dst.nx; x++) {
					for (c = 0; c < dst.nc; c++) {
						int src_coords[] = new int[]{x, y, z};
		                int temp;

		                // Permute the coordinates
		                temp = src_coords[dim1];
		                src_coords[dim1] = src_coords[dim2];
		                src_coords[dim2] = temp;

		                // Copy the datum
		                dst.data[x*dst.xs + y*dst.ys + z*dst.zs + c] = src.data[src_coords[0]*src.xs + src_coords[1]*src.ys + src_coords[2]*src.zs + c];	
					}
				}
			}
		}

	        return SIFT3D_SUCCESS;
	}

	/* Horizontally convolves a separable filter with an image, 
	 * on CPU. Currently only works in 3D.
	 * 
	 * This function chooses among the best variant of convolve_sep* based on
	 * compilation options and filter parameters.
	 * 
	 * Parameters: 
	 * src - input image (initialized)
	 * dst - output image (initialized) 
	    int x, y, z;
	 * f - filter to be applied
	 * dim - dimension in which to convolve
	 * unit - the spacing of the filter coefficients
	 */
	private int convolve_sep(Image src,
				Image dst, Sep_FIR_filter f,
				int dim, double unit) {

	//#ifdef SIFT3D_USE_OPENCL
	//        return convolve_sep_cl(src, dst, f, dim, unit);
	//#else
		return (f.symmetric != 0) ? 
	                convolve_sep_sym(src, dst, f, dim, unit) : 
	                convolve_sep_gen(src, dst, f, dim, unit);
	//#endif
	}
	
	/* Convolve_sep for symmetric filters. */
	private int convolve_sep_sym(Image src, Image dst,
				    Sep_FIR_filter f, int dim,
	                            double unit)
	{

		// TODO: Symmetry-specific function
		return convolve_sep_gen(src, dst, f, dim, unit);
	}
	
	/* Convolve_sep for general filters */
	private int convolve_sep_gen(Image src,
				Image dst, Sep_FIR_filter f,
				int dim, double unit)
	{
		int status;
		int x, y, z, c, d;

		final int half_width = f.width / 2;
		final int nx = src.nx;
		final int ny = src.ny;
		final int nz = src.nz;
	    final double conv_eps = 0.1;
	    final int dim_end;
	    final double unit_factor;
	    if (dim == 0) {
	    	dim_end = src.nx -1;
	    	unit_factor = unit / src.ux;
	    }
	    else if (dim == 1) {
	    	dim_end = src.ny - 1;
	    	unit_factor = unit/ src.uy;
	    }
	    else {
	    	dim_end = src.nz - 1;
	    	unit_factor = unit / src.uz;
	    }
	        final int unit_half_width = 
	                (int) Math.ceil(half_width * unit_factor);
	        int start[] = {0, 0, 0};
	        int end[] = {nx - 1, ny - 1, nz - 1};

	        // Compute starting and ending points for the convolution dimension
	        start[dim] += unit_half_width;
	        end[dim] -= unit_half_width + 1;

		//TODO: Convert this to convolve_x, which only convolves in x,
		// then make a wrapper to restride, transpose, convolve x, and transpose 
		// back

		// Resize the output, with the default stride
	    status = im_copy_dims(src, dst);
	    if (status == SIFT3D_FAILURE) {
	        return SIFT3D_FAILURE;
	    }
		im_default_stride(dst);
		status = im_resize(dst);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Initialize the output to zeros
		im_zero(dst);

		// First pass: process the interior
	//#pragma omp parallel for private(x) private(y) private(c)
		for (z = start[2]; z <= end[2]; z++) {
			for (y = start[1]; y <= end[1]; y++) {
				for (x = start[0]; x <= end[0]; x++) {
					for (c = 0; c < dst.nc; c++) {

	                double coords[] = { x, y, z };

	                for (d = -half_width; d <= half_width; d++) {

	                        final double tap = f.kernel[d + half_width];
	                        final double step = d * unit_factor;

	                        // Adjust the sampling coordinates
	                        coords[dim] -= step;

	                        // Sample
	                        SAMP_AND_ACC(src, dst, tap, coords, x, y, z, c, dim);

	                        // Reset the sampling coordinates
	                        coords[dim] += step;
	                }	
					}
				}
			}
		}
		

	        // Second pass: process the boundaries
	//#pragma omp parallel for private(x) private(y) private(c)
	        for (z = 0; z < dst.nz; z++) {
	        	for (y = 0; y < dst.ny; y++) {
	        		for (x = 0; x < dst.nx; x++) {
	        			for (c = 0; c < dst.nc; c++) {
	        				final int i_coords[] = { x, y, z };

	    	                // Skip pixels we have already processed
	    	                if (i_coords[dim] >= start[dim] && i_coords[dim] <= end[dim]) 
	    	                        continue;

	    	                // Process the boundary pixel
	    	                for (d = -half_width; d <= half_width; d++) {

	    	                        double coords[] = new double[]{ x, y, z };
	    	                        double tap = f.kernel[d + half_width];
	    	                        double step = d * unit_factor;

	    	                        // Adjust the sampling coordinates
	    	                        coords[dim] -= step;

	    	                        // Mirror coordinates
	    	                        if ((int) coords[dim] < 0) {
	    	                                coords[dim] = -coords[dim];
	    	                                assert((int) coords[dim] >= 0);
	    	                        } else if ((int) coords[dim] >= dim_end) {
	    	                                coords[dim] = 2.0 * dim_end - coords[dim] -    
	    	                                        conv_eps;
	    	                                assert((int) coords[dim] < dim_end);
	    	                        }

	    	                        // Sample
	    	                        SAMP_AND_ACC(src, dst, tap, coords, x, y, z, c, dim);
	    	                }
	
	        			}
	        		}
	        	}
	        }

	        return SIFT3D_SUCCESS;
	}
	
	private void SAMP_AND_ACC(Image src, Image dst, double tap, double coords[], int x, int y, int z, int c, int dim) 
	{ 
        double frac;

        final int idx_lo[] = new int[]{(int)coords[0], (int)coords[1], (int)coords[2]};
        int idx_hi[] = new int[]{idx_lo[0], idx_lo[1], idx_lo[2]};

        /* Convert the physical coordinates to integer indices*/ 
        idx_hi[dim] += 1; 
        frac = (coords)[dim] - (float) idx_lo[dim]; 

        /* Sample with linear interpolation */ 
        dst.data[x*dst.xs + y*dst.ys + z*dst.zs + c] += tap *
                ((1.0f - frac) *
                src.data[idx_lo[0]*src.xs + idx_lo[1]*src.ys + idx_lo[2]*src.zs + c] +
                frac *
                src.data[idx_hi[0]*src.xs + idx_hi[1]*src.ys + idx_hi[2]*src.zs + c]);
}
	
	/* Zero an image. */
	private void im_zero(Image im)
	{

		int x, y, z, c;

		for (z = 0; z < im.nz; z++) {
			for (y = 0; y < im.ny; y++) {
				for (x = 0; x < im.nx; x++) {
					for (c = 0; c < im.nc; c++) {
						im.data[x*im.xs + y*im.ys + z*im.zs + c] = 0.0;
					}
				}
			}
		}
	}
	
	/* Downsample an image by a factor of 2 in each dimension.
	 * This function initializes dst with the proper 
	 * dimensions, and allocates memory. */
	private int im_downsample_2x(Image src, Image dst)
	{

		int status;
		int x, y, z, c;

		// Initialize dst
		dst.nx = (int)Math.floor((double)src.nx / 2.0);
		dst.ny = (int)Math.floor((double)src.ny / 2.0);
		dst.nz = (int)Math.floor((double)src.nz / 2.0);
		dst.nc = src.nc;
		im_default_stride(dst);
		status = im_resize(dst);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Downsample
		for (z = 0; z < dst.nz; z++) {
			for (y = 0; y < dst.ny; y++) {
				for (x = 0; x < dst.nx; x++) {
					for (c = 0; c < dst.nc; c++) {
						final int src_x = x << 1;
						final int src_y = y << 1;
						final int src_z = z << 1;

						dst.data[x*dst.xs + y*dst.ys + z*dst.zs + c] = 
								src.data[src_x*src.xs + src_y*src.ys + src_z*src.zs + c];	
					}
				}
			}
		}

		return SIFT3D_SUCCESS;
	}
	
	private int build_dog(SIFT3DC sift3d) {

		int status;
		Image gpyr_cur, gpyr_next, dog_level;
		int o, s;

		Pyramid dog = sift3d.dog;
		Pyramid gpyr = sift3d.gpyr;

		for (o = dog.first_octave; o <= SIFT3D_PYR_LAST_OCTAVE(dog); o++) {
	        for (s = dog.first_level; s <= SIFT3D_PYR_LAST_LEVEL(dog); s++) {
				gpyr_cur = SIFT3D_PYR_IM_GET(gpyr, o, s);
				gpyr_next = SIFT3D_PYR_IM_GET(gpyr, o, s + 1);			
				dog_level = SIFT3D_PYR_IM_GET(dog, o, s);
				
				status = im_subtract(gpyr_cur, gpyr_next, dog_level);
				if (status == SIFT3D_FAILURE) {
					return SIFT3D_FAILURE;
				}
	        }
		}

		return SIFT3D_SUCCESS;
	}
	
	/* Subtract src2 from src1, saving the result in
	 * dst.
	 * Resizes dst. 
	 */
	private int im_subtract(Image src1, Image src2, Image dst)
	{

		int status;
		int x, y, z, c;

		// Verify inputs
		if (src1.nx != src2.nx ||
		    src1.ny != src2.ny ||
		    src1.nz != src2.nz || src1.nc != src2.nc)
			return SIFT3D_FAILURE;

		// Resize the output image
		status = im_copy_dims(src1, dst);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		for (z = 0; z < dst.nz; z++) {
			for (y = 0; y < dst.ny; y++) {
				for (x = 0; x < dst.nx; x++) {
					for (c = 0; c < dst.nc; c++) {
						dst.data[x*dst.xs + y*dst.ys + z*dst.zs + c] =
		                src1.data[x*src1.xs + y*src1.ys + z*src1.zs + c] -
		                src2.data[x*src2.xs + y*src2.ys + z*src2.zs + c];
					}
				}
			}
		}
		
		return SIFT3D_SUCCESS;
	}
	
	/* Detect local extrema */
	private int detect_extrema(SIFT3DC sift3d, Keypoint_store kp) {

		int status;
		Image cur, prev, next;
		Keypoint key;
		double pcur, dogmax, peak_thresh;
		int o, s, x, y, z, x_start, x_end, y_start, y_end, z_start,
			z_end, num;

		final Pyramid dog = sift3d.dog;
		final int o_start = dog.first_octave;
		final int o_end = SIFT3D_PYR_LAST_OCTAVE(dog);
		final int s_start = dog.first_level + 1;
		final int s_end = SIFT3D_PYR_LAST_LEVEL(dog) - 1;

		// Verify the inputs
		if (dog.num_levels < 3) {
			System.err.println("detect_extrema: Requires at least 3 levels per octave,");
		    System.err.println("provided only " + dog.num_levels);
			return SIFT3D_FAILURE;
		}

		// Initialize dimensions of keypoint store
		cur = SIFT3D_PYR_IM_GET(dog, o_start, s_start);
		kp.nx = cur.nx;
		kp.ny = cur.ny;
		kp.nz = cur.nz;

		num = 0;
		for (o = o_start; o <= o_end; o++) {
			for (s = s_start; s <= s_end; s++) {

			// Select current and neighboring levels
			prev = SIFT3D_PYR_IM_GET(dog, o, s - 1);
			cur = SIFT3D_PYR_IM_GET(dog, o, s);
			next = SIFT3D_PYR_IM_GET(dog, o, s + 1);

			// Find maximum DoG value at this level
			dogmax = 0.0;
			for (z = 0; z < cur.nz; z++) {
				for (y = 0; y < cur.ny; y++) {
					for (x = 0; x < cur.nx; x++) {
						dogmax = Math.max(dogmax, 
                                Math.abs(cur.data[x*cur.xs + y*cur.ys + z*cur.zs]));
	
					}
				}
			}
				
			// Adjust threshold
			peak_thresh = sift3d.peak_thresh * dogmax;

			// Loop through all non-boundary pixels
			x_start = y_start = z_start = 1;
			x_end = cur.nx - 2;
			y_end = cur.ny - 2;
			z_end = cur.nz - 2;
			for (z = z_start; z <= z_end; z++) {
				for (y = y_start; y <= y_end; y++) {
					for (x = x_start; x <= x_end; x++) {
						// Sample the center value
						pcur = cur.data[x*cur.xs + y*cur.ys + z*cur.zs];

						// Apply the peak threshold
						if ((pcur > peak_thresh || pcur < -peak_thresh) && ((
							// Compare to the neighbors
							CMP_PREV_GT(prev, x, y, z, pcur) &&
							CMP_CUR_GT(cur, x, y, z, pcur) &&
							CMP_NEXT_GT(next, x, y, z, pcur)
							) || (
							CMP_PREV_LT(prev, x, y, z, pcur) &&
							CMP_CUR_LT(cur, x, y, z, pcur) &&
							CMP_NEXT_LT(next, x, y, z, pcur))))
							{

			                                // Add a keypoint candidate
			                                num++;
			                                status = resize_Keypoint_store(kp, num);
			                                if (status == SIFT3D_FAILURE) {
			                                        return SIFT3D_FAILURE;
			                                }
			                                key = kp.buf[num - 1];
			                                status = init_Keypoint(key);
			                                if (status == SIFT3D_FAILURE) {
			                                        return SIFT3D_FAILURE;
			                                }
			                                key.o = o;
			                                key.s = s;
			                                key.sd = cur.s;
							key.xd = (double) x;
							key.yd = (double) y;
							key.zd = (double) z;
			                        }	
					}
				}
			}
				
			}
		}

		return SIFT3D_SUCCESS;
	}
	
	private boolean CMP_PREV_GT(Image im, int x, int y, int z, double val) {
		if (CUBOID_EXTREMA) {
			return CMP_CUBE_GT(im, x, y, z, SIFT3D_FALSE, val);
		}
		else {
			return (val > im.data[x*im.xs + y*im.ys + z*im.zs]);
		}
		
	}
	
	private boolean CMP_PREV_LT(Image im, int x, int y, int z, double val) {
		if (CUBOID_EXTREMA) {
			return CMP_CUBE_LT(im, x, y, z, SIFT3D_FALSE, val);
		}
		else {
			return (val < im.data[x*im.xs + y*im.ys + z*im.zs]);
		}
		
	}
	
	private boolean CMP_CUR_GT(Image im, int x, int y, int z, double val) {
		if (CUBOID_EXTREMA) {
			return CMP_CUBE_GT(im, x, y, z, SIFT3D_TRUE, val);
		}
		else {
			return((val >  im.data[(x+1)*im.xs + y*im.ys + z*im.zs]) &&
					(val >  im.data[(x-1)*im.xs + y*im.ys + z*im.zs]) &&
					(val >  im.data[x*im.xs + (y+1)*im.ys + z*im.zs]) &&
					(val >  im.data[x*im.xs + (y-1)*im.ys + z*im.zs]) &&
					(val >  im.data[x*im.xs + y*im.ys + (z-1)*im.zs]) &&
					(val >  im.data[x*im.xs + y*im.ys + (z+1)*im.zs]));	
		}
	}
	
	private boolean CMP_CUR_LT(Image im, int x, int y, int z, double val) {
		if (CUBOID_EXTREMA) {
			return CMP_CUBE_LT(im, x, y, z, SIFT3D_TRUE, val);
		}
		else {
			return((val <  im.data[(x+1)*im.xs + y*im.ys + z*im.zs]) &&
					(val <  im.data[(x-1)*im.xs + y*im.ys + z*im.zs]) &&
					(val <  im.data[x*im.xs + (y+1)*im.ys + z*im.zs]) &&
					(val <  im.data[x*im.xs + (y-1)*im.ys + z*im.zs]) &&
					(val <  im.data[x*im.xs + y*im.ys + (z-1)*im.zs]) &&
					(val <  im.data[x*im.xs + y*im.ys + (z+1)*im.zs]));	
		}
	}
	
	 private boolean CMP_NEXT_GT(Image im, int x, int y, int z, double val) {
		 if (CUBOID_EXTREMA) {
			 return CMP_CUBE_GT(im, x, y, z, SIFT3D_FALSE, val);
		 }
		 else {
			 return CMP_PREV_GT(im, x, y, z, val); 
		 }
	 }
	 
	 private boolean CMP_NEXT_LT(Image im, int x, int y, int z, double val) {
		 if (CUBOID_EXTREMA) {
			 return CMP_CUBE_LT(im, x, y, z, SIFT3D_FALSE, val);
		 }
		 else {
			 return CMP_PREV_LT(im, x, y, z, val); 
		 }
	 }
	
	private boolean CMP_CUBE_GT(Image im, int x, int y, int z, int IGNORESELF, double val) {
			return ((val > im.data[x*im.xs + y*im.ys + (z-1)*im.zs]) && 
					(val > im.data[(x-1)*im.xs + y*im.ys + (z-1)*im.zs]) && 
					(val > im.data[(x+1)*im.xs + y*im.ys + (z-1)*im.zs]) && 
					(val > im.data[x*im.xs + (y-1)*im.ys + (z-1)*im.zs]) && 
					(val > im.data[x*im.xs + (y+1)*im.ys + (z-1)*im.zs]) && 
					(val > im.data[(x-1)*im.xs + (y-1)*im.ys + (z-1)*im.zs]) && 
					(val > im.data[(x+1)*im.xs + (y-1)*im.ys + (z-1)*im.zs]) && 
					(val > im.data[(x-1)*im.xs + (y+1)*im.ys + (z-1)*im.zs]) && 
					(val > im.data[(x+1)*im.xs + (y+1)*im.ys + (z-1)*im.zs]) && 
			        ((val > im.data[x*im.xs + y*im.ys + z*im.zs]) || (IGNORESELF != 0) ) &&
					(val > im.data[(x-1)*im.xs + y*im.ys + z*im.zs]) && 
					(val > im.data[(x+1)*im.xs + y*im.ys + z*im.zs]) && 
					(val > im.data[x*im.xs + (y-1)*im.ys + z*im.zs]) && 
					(val > im.data[x*im.xs + (y+1)*im.ys + z*im.zs]) && 
					(val > im.data[(x-1)*im.xs + (y-1)*im.ys + z*im.zs]) && 
					(val > im.data[(x+1)*im.xs + (y-1)*im.ys + z*im.zs]) && 
					(val > im.data[(x-1)*im.xs + (y+1)*im.ys + z*im.zs]) && 
					(val > im.data[(x+1)*im.xs + (y+1)*im.ys + z*im.zs]) && 
					(val > im.data[x*im.xs + y*im.ys + (z+1)*im.zs]) && 
					(val > im.data[(x-1)*im.xs + y*im.ys + (z+1)*im.zs]) && 
					(val > im.data[(x+1)*im.xs + y*im.ys + (z+1)*im.zs]) && 
					(val > im.data[x*im.xs + (y-1)*im.ys + (z+1)*im.zs]) && 
					(val > im.data[x*im.xs + (y+1)*im.ys + (z+1)*im.zs]) && 
					(val > im.data[(x-1)*im.xs + (y-1)*im.ys + (z+1)*im.zs]) && 
					(val > im.data[(x+1)*im.xs + (y-1)*im.ys + (z+1)*im.zs]) && 
					(val > im.data[(x-1)*im.xs + (y+1)*im.ys + (z+1)*im.zs]) && 
					(val > im.data[(x+1)*im.xs + (y+1)*im.ys + (z+1)*im.zs]));
	}
	
	private boolean CMP_CUBE_LT(Image im, int x, int y, int z, int IGNORESELF, double val) {
		return ((val < im.data[x*im.xs + y*im.ys + (z-1)*im.zs]) && 
				(val < im.data[(x-1)*im.xs + y*im.ys + (z-1)*im.zs]) && 
				(val < im.data[(x+1)*im.xs + y*im.ys + (z-1)*im.zs]) && 
				(val < im.data[x*im.xs + (y-1)*im.ys + (z-1)*im.zs]) && 
				(val < im.data[x*im.xs + (y+1)*im.ys + (z-1)*im.zs]) && 
				(val < im.data[(x-1)*im.xs + (y-1)*im.ys + (z-1)*im.zs]) && 
				(val < im.data[(x+1)*im.xs + (y-1)*im.ys + (z-1)*im.zs]) && 
				(val < im.data[(x-1)*im.xs + (y+1)*im.ys + (z-1)*im.zs]) && 
				(val < im.data[(x+1)*im.xs + (y+1)*im.ys + (z-1)*im.zs]) && 
		        ((val < im.data[x*im.xs + y*im.ys + z*im.zs]) || (IGNORESELF != 0) ) &&
				(val < im.data[(x-1)*im.xs + y*im.ys + z*im.zs]) && 
				(val < im.data[(x+1)*im.xs + y*im.ys + z*im.zs]) && 
				(val < im.data[x*im.xs + (y-1)*im.ys + z*im.zs]) && 
				(val < im.data[x*im.xs + (y+1)*im.ys + z*im.zs]) && 
				(val < im.data[(x-1)*im.xs + (y-1)*im.ys + z*im.zs]) && 
				(val < im.data[(x+1)*im.xs + (y-1)*im.ys + z*im.zs]) && 
				(val < im.data[(x-1)*im.xs + (y+1)*im.ys + z*im.zs]) && 
				(val < im.data[(x+1)*im.xs + (y+1)*im.ys + z*im.zs]) && 
				(val < im.data[x*im.xs + y*im.ys + (z+1)*im.zs]) && 
				(val < im.data[(x-1)*im.xs + y*im.ys + (z+1)*im.zs]) && 
				(val < im.data[(x+1)*im.xs + y*im.ys + (z+1)*im.zs]) && 
				(val < im.data[x*im.xs + (y-1)*im.ys + (z+1)*im.zs]) && 
				(val < im.data[x*im.xs + (y+1)*im.ys + (z+1)*im.zs]) && 
				(val < im.data[(x-1)*im.xs + (y-1)*im.ys + (z+1)*im.zs]) && 
				(val < im.data[(x+1)*im.xs + (y-1)*im.ys + (z+1)*im.zs]) && 
				(val < im.data[(x-1)*im.xs + (y+1)*im.ys + (z+1)*im.zs]) && 
				(val < im.data[(x+1)*im.xs + (y+1)*im.ys + (z+1)*im.zs]));
    }
	
	/* Make room for at least num Keypoint structs in kp. 
	 * 
	 * Note: This function must re-initialize some internal data if it was moved. 
	 * This does not affect the end user, but it affects the implementation of 
	 * init_Keypoint. */
	private int resize_Keypoint_store(Keypoint_store kp,int num) {

	    int status;
		Keypoint[] buf_old = kp.slab.buf;

	        // Resize the internal memory
		SIFT3D_RESIZE_SLAB(kp.slab, num);
		kp.buf = kp.slab.buf; 

	        // If the size has changed, re-initialize the keypoints
	        if (buf_old != kp.slab.buf) { 
	                int i; 
	                for (i = 0; i < kp.slab.num; i++) { 
	                        Keypoint key = kp.buf[i]; 
	                        status = init_Keypoint(key);
	                        if (status == SIFT3D_FAILURE) {
	                                return SIFT3D_FAILURE; 
	                        }
	                } 
	        } 

	        return SIFT3D_SUCCESS;
	}
	
	/* Resize a slab. If SIFT3D_SLAB_SIZE is defined, add
	* elements in increments of that number. Otherwise,
	* use a default of 500. This macro is meant to be
	* used whether or not the slab buffer actually needs
	* resizing -- it checks for that. */
	private void SIFT3D_RESIZE_SLAB(Slab slab, int num_new) {
		    int status;
		    int SIFT3D_SLAB_LEN = 500;
		    int i, j, k, r, c;
	        final int slabs_new = (num_new + SIFT3D_SLAB_LEN - 1) / 
	                        SIFT3D_SLAB_LEN;
	        final int size_new = slabs_new * SIFT3D_SLAB_LEN;

		if (size_new != slab.buf_size) {
	
			/* Re-initialize if the new size is 0 */ 
			if (size_new == 0) { 
				cleanup_Slab(slab); 
				init_Slab(slab);
			/* Else allocate new memory */ 
			}
			else {
				Keypoint kb[] = new Keypoint[size_new];
				for (i = 0; i < size_new; i++) {
					kb[i] = new Keypoint();
					if (slab.buf != null) {
						if (i < slab.buf.length) {
							status = copy_Keypoint(slab.buf[i], kb[i]);
							if (status == SIFT3D_FAILURE) {
								System.err.println("In SIFT3D_RESIZE_SLAB SIFT3D_FAILURE on copy_Keypoint");
								return;
							}
						    
						}
					}
				} 
				slab.buf = kb;
			}
			slab.buf_size = size_new;
		} 
		slab.num = num_new;
	}

	/* Initialize a Keypoint struct for use. This sets up the internal pointers,
	 * and nothing else. If called on a valid Keypoint struct, it has no effect. */
	private int init_Keypoint(Keypoint key) {
	        // Initialize the orientation matrix with static memory
	        return init_Mat_rm_p(key.R, key.r_data, IM_NDIMS, IM_NDIMS, 
			Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
	}
	
	/* Assign rotation matrices to the keypoints. 
	 * 
	 * Note that this stage will modify kp, likely
	 * rejecting some keypoints as orientationally
	 * unstable. */
	private int assign_orientations(SIFT3DC sift3d, 
				       Keypoint_store kp) {

		int status;
		Keypoint kp_pos[];
		int num;
		int i, err; 
		int r,c;

		// Iterate over the keypoints 
	        err = SIFT3D_SUCCESS;
	//#pragma omp parallel for
		for (i = 0; i < kp.slab.num; i++) {

			Keypoint key = kp.buf[i];
			final Image level = 
	                        SIFT3D_PYR_IM_GET(sift3d.gpyr, key.o, key.s);
	                Mat_rm R = key.R;
	                Cvec vcenter = new Cvec();
	                vcenter.x = key.xd;
	                vcenter.y = key.yd;
	                vcenter.z = key.zd;
	                double sigma = ori_sig_fctr * key.sd;

			// Compute dominant orientations
	        // assert(R->u.data_float == key->r_data);
	        for (r = 0; r < IM_NDIMS; r++) {
	        	for (c = 0; c < IM_NDIMS; c++) {
	        		if (R.data_double[r][c] != key.r_data[r][c]) {
	        			System.err.println("In assign_orientations found R.data_double != key.r_data");
	        			return SIFT3D_FAILURE;
	        		}
	        	}
	        }
			switch (assign_orientation_thresh(level, vcenter, sigma,
	                                       sift3d.corner_thresh, R)) {
				case SIFT3D_SUCCESS:
					// Continue processing this keypoint
					break;
				case REJECT:
					// Mark this keypoint as invalid
	                                key.xd = key.yd = key.zd = -1.0;
	                                continue;
				default:
					// Any other return value is an error
	                                err = SIFT3D_FAILURE;
	                                continue;
			}
			
		}

	        // Check for errors
	        if (err != 0) return err;

	        // Rebuild the keypoint buffer in place
		    num = 0; 
	        for (i = 0; i < kp.slab.num; i++) {

			Keypoint key = kp.buf[i];

	                // Check if the keypoint is valid
	                if (key.xd < 0.0)
	                        continue;

	                // Copy this keypoint to the next available spot
	                status = copy_Keypoint(key, kp.buf[num]);
	                if (status == SIFT3D_FAILURE) {
	                        return SIFT3D_FAILURE;
	                }
	               
	                num++;
	        }

		// Release unneeded keypoint memory
	        return resize_Keypoint_store(kp, num);
	}
	
	/* Copy one Keypoint struct into another. */
	int copy_Keypoint(Keypoint src, Keypoint dst) {

	        // Copy the shallow data 
	        dst.xd = src.xd;
	        dst.yd = src.yd;
	        dst.zd = src.zd;
	        dst.sd = src.sd;
	        dst.o = src.o;
	        dst.s = src.s;
	        for (int r = 0; r < IM_NDIMS; r++) {
	        	for (int c = 0; c < IM_NDIMS; c++) {
	        		dst.r_data[r][c] = src.r_data[r][c];
	        	}
	        }

	        // Copy the orienation matrix
	        return copy_Mat_rm(src.R, dst.R);
	}
	
	/* Helper function to call assign_eig_ori, and reject keypoints with
	 * confidence below the parameter "thresh." All other parameters are the same.
	 * All return values are the same, except REJECT is returned if 
	 * conf < thresh. */
	private int assign_orientation_thresh(Image im, 
	        Cvec vcenter, double sigma, double thresh,
	        Mat_rm R) {

	        double conf[] = new double[1];
	        int ret;

	        ret = assign_eig_ori(im, vcenter, sigma, R, conf);

	        return ret == SIFT3D_SUCCESS ? 
	                (conf[0] < thresh ? REJECT : SIFT3D_SUCCESS) : ret;
	}


	/* Assign an orientation to a point in an image.
	 *
	 * Parameters:
	 *   -im: The image data.
	 *   -vcenter: The center of the window, in image space.
	 *   -sigma: The scale parameter. The width of the window is a constant
	 *      multiple of this.
	 *   -R: The place to write the rotation matrix.
	 */
	private int assign_eig_ori(Image im, Cvec vcenter,
	                          double sigma, Mat_rm R, 
	                          double conf[]) {

	    int status;
		Cvec v[] = new Cvec[2];
	    v[0] = new Cvec();
	    v[1] = new Cvec();
	    Mat_rm A = new Mat_rm();
	    Mat_rm L = new Mat_rm();
	    Mat_rm Q = new Mat_rm();
	    Cvec vdisp = new Cvec();
	    Cvec vd_win = new Cvec();
	    Cvec vr = new Cvec();
	    double d, cos_ang, abs_cos_ang, corner_score;
	    double weight, sq_dist, sgn;
	    int i, x, y, z, m;
	  
	    final double win_radius = sigma * ori_rad_fctr; 

	    // Verify inputs
	    if (!SIFT3D_IM_CONTAINS_CVEC(im, vcenter)) {
	        System.err.println("assign_eig_ori: vcenter (" + vcenter.x + ", " + vcenter.y + ", " + vcenter.z+") lies ");
	        System.err.println("outside the boundaries of im [" + im.nx + " x " + im.ny + " x " + im.nz + "]"); 
	        return SIFT3D_FAILURE;
	    }
	    if (sigma < 0) {
	        System.err.println("assign_eig_ori: invalid sigma: " + sigma);
	        return SIFT3D_FAILURE;
	    }

	    // Initialize the intermediates
	    status = init_Mat_rm(A, 3, 3, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_TRUE);
	    if (status == SIFT3D_FAILURE) {
	        return SIFT3D_FAILURE;
	    }
	    status = init_Mat_rm(L, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_TRUE);
	    if (status == SIFT3D_FAILURE) {
	    	 if (conf != null)
	 	        conf[0] = 0.0;
	 	    cleanup_Mat_rm(A);
	 	    cleanup_Mat_rm(Q);
	 	    cleanup_Mat_rm(L);
	 	    return SIFT3D_FAILURE;
	    }
		status = init_Mat_rm(Q, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_TRUE);
		if (status == SIFT3D_FAILURE) {
	    	 if (conf != null)
	 	        conf[0] = 0.0;
	 	    cleanup_Mat_rm(A);
	 	    cleanup_Mat_rm(Q);
	 	    cleanup_Mat_rm(L);
	 	    return SIFT3D_FAILURE;
	    }

	    // Resize the output
	    R.num_rows = R.num_cols = IM_NDIMS;
	    R.type = Mat_rm_type.SIFT3D_DOUBLE;
	    status = resize_Mat_rm(R);
	    if (status == SIFT3D_FAILURE) {
	    	 if (conf != null)
	 	        conf[0] = 0.0;
	 	    cleanup_Mat_rm(A);
	 	    cleanup_Mat_rm(Q);
	 	    cleanup_Mat_rm(L);
	 	    return SIFT3D_FAILURE;
	    }

	    // Form the structure tensor and window gradient
	    vd_win.x = 0.0;
	    vd_win.y = 0.0;
	    vd_win.z = 0.0;
	    final double uxf = im.ux;
        final double uyf = im.uy;
        final double uzf = im.uz;
	    final int x_start = (int)Math.max(Math.floor(vcenter.x - win_radius / uxf), 1);
	    final int x_end   = (int)Math.min(Math.ceil(vcenter.x + win_radius / uxf),
                im.nx - 2);
	    final int y_start = (int)Math.max(Math.floor(vcenter.y - win_radius / uyf), 1);
	    final int y_end   = (int)Math.min(Math.ceil(vcenter.y + win_radius / uyf),
                im.ny - 2);
        final int z_start = (int)Math.max(Math.floor(vcenter.z - win_radius / uzf), 1);
	    final int z_end   = (int)Math.min(Math.ceil(vcenter.z + win_radius / uzf),
                im.nz - 2); 
	    Cvec vd = new Cvec();
	    for (z = z_start; z <= z_end; z++) {
	    	for (y = y_start; y <= y_end; y++) {
	    		for (x = x_start; x <= x_end; x++) {
                vdisp.x = (x - vcenter.x) * uxf;
                vdisp.y = (y - vcenter.y) * uyf;
                vdisp.z = (z - vcenter.z) * uzf;
                sq_dist = SIFT3D_CVEC_L2_NORM_SQ(vdisp);
                if (sq_dist > win_radius * win_radius) 
	                continue; 
	        

		// Compute Gaussian weighting, ignoring the constant factor
		weight = Math.exp(-0.5 * sq_dist / (sigma * sigma));		

		// Get the gradient	
		IM_GET_GRAD_ISO(im, x, y, z, 0, vd);

		// Update the structure tensor
		A.data_double[0][0] += vd.x * vd.x * weight;
		A.data_double[0][1] += vd.x * vd.y * weight;
		A.data_double[0][2] += vd.x * vd.z * weight;
		A.data_double[1][1] += vd.y * vd.y * weight;
		A.data_double[1][2] += vd.y * vd.z * weight;
		A.data_double[2][2] += vd.z * vd.z * weight;

		// Update the window gradient
	    SIFT3D_CVEC_SCALE(vd, weight);
		SIFT3D_CVEC_PLUS(vd_win, vd, vd_win);

	    		}
	    	}
	    }

	    // Fill in the remaining elements
	    A.data_double[1][0] = A.data_double[0][1];
	    A.data_double[2][0] = A.data_double[0][2];
	    A.data_double[2][1] = A.data_double[1][2];

	    // Reject keypoints with weak gradient 
	    if (SIFT3D_CVEC_L2_NORM_SQ(vd_win) < ori_grad_thresh) {
	    	if (conf != null)
		        conf[0] = 0.0;
		    cleanup_Mat_rm(A);
		    cleanup_Mat_rm(Q);
		    cleanup_Mat_rm(L);
		    return REJECT;
	    } 

	    // Get the eigendecomposition
	    status = eigen_Mat_rm(A, Q, L);
	    if (status == SIFT3D_FAILURE) {
	    	 if (conf != null)
	 	        conf[0] = 0.0;
	 	    cleanup_Mat_rm(A);
	 	    cleanup_Mat_rm(Q);
	 	    cleanup_Mat_rm(L);
	 	    return SIFT3D_FAILURE;
	    }

	    // Ensure we have distinct eigenvalues
	    m = L.num_rows;
	    if (m != 3) {
	    	if (conf != null)
		        conf[0] = 0.0;
		    cleanup_Mat_rm(A);
		    cleanup_Mat_rm(Q);
		    cleanup_Mat_rm(L);
		    return REJECT;
	    }

	    // Test the eigenvectors for stability
	    for (i = 0; i < m - 1; i++) {
			if (Math.abs(L.data_double[i][0] /
				 L.data_double[i + 1][0]) > max_eig_ratio) {
				if (conf != null)
			        conf[0] = 0.0;
			    cleanup_Mat_rm(A);
			    cleanup_Mat_rm(Q);
			    cleanup_Mat_rm(L);
			    return REJECT;
			}
	    }

	    // Assign signs to the first n - 1 vectors
	    corner_score = Double.MAX_VALUE;
	    for (i = 0; i < m - 1; i++) {

		final int eig_idx = m - i - 1;

		// Get an eigenvector, in descending order
		vr.x = Q.data_double[0][eig_idx];
		vr.y = Q.data_double[1][eig_idx];
		vr.z = Q.data_double[2][eig_idx];

		// Get the directional derivative
		d = SIFT3D_CVEC_DOT(vd_win, vr);

	        // Get the cosine of the angle between the eigenvector and the gradient
	        cos_ang = d / (SIFT3D_CVEC_L2_NORM(vr) * SIFT3D_CVEC_L2_NORM(vd_win));
	        abs_cos_ang = Math.abs(cos_ang);

	        // Compute the corner confidence score
	        corner_score = Math.min(corner_score, abs_cos_ang);

		// Get the sign of the derivative
	        sgn = d > 0.0 ? 1.0 : -1.0;

		// Enforce positive directional derivative
		SIFT3D_CVEC_SCALE(vr, sgn);

		// Add the vector to the rotation matrix
		R.data_double[0][i] = vr.x;
		R.data_double[1][i] = vr.y;
		R.data_double[2][i] = vr.z;

		// Save this vector for later use
		v[i] = vr;
	    }

	    // Take the cross product of the first two vectors
	    SIFT3D_CVEC_CROSS(v[0], v[1], vr);

	    // Add the last vector
	    R.data_double[0][2] = vr.x;
	    R.data_double[1][2] = vr.y;
	    R.data_double[2][2] = vr.z;

	    // Optionally write back the corner score
	    if (conf != null)
	        conf[0] = corner_score;

	    cleanup_Mat_rm(A);
	    cleanup_Mat_rm(Q);
	    cleanup_Mat_rm(L);
	    return SIFT3D_SUCCESS; 
	}
  
	// Evaluates to true (nonzero) if im contains cvec, false otherwise
	private boolean SIFT3D_IM_CONTAINS_CVEC(Image im, Cvec cvec) {
	       return ((cvec.x >= 0) && (cvec.y >= 0) && (cvec.z >= 0) &&
	        (cvec.x < im.nx) &&
	        (cvec.y < im.ny) &&
	        (cvec.z < im.nz));
	}
	
	// Return the square of the  L2 norm of a Cartesian coordinate vector
	private double SIFT3D_CVEC_L2_NORM_SQ(Cvec cvec) {
		return(cvec.x * cvec.x + cvec.y * cvec.y +
		cvec.z * cvec.z);
	}
	
	// As SIFT3D_IM_GET_GRAD, but with physical units (1, 1, 1)
	private void IM_GET_GRAD_ISO(Image im, int x, int y, int z, int c, Cvec vd) {
	        SIFT3D_IM_GET_GRAD(im, x, y, z, c, vd);
	        vd.x *=  1.0 / im.ux;
	        vd.y *= 1.0 / im.uy;
	        vd.z *= 1.0 / im.uz;
	}
	
	/* Take the Cartesian gradient of an image at [x, y, z, c]. The voxel cannot be
	 * on the boundary. */
	private void SIFT3D_IM_GET_GRAD(Image im, int x, int y, int z, int c, Cvec vd) {
			vd.x = 0.5 * (im.data[(x+1)*im.xs + y*im.ys + z*im.zs + c] -
					im.data[(x-1)*im.xs + y*im.ys + z*im.zs + c]);
			vd.y = 0.5 * (im.data[x*im.xs + (y+1)*im.ys + z*im.zs + c] -
					im.data[x*im.xs + (y-1)*im.ys + z*im.zs + c]);
			vd.z = 0.5 * (im.data[x*im.xs + y*im.ys + (z+1)*im.zs + c] -
					im.data[x*im.xs + y*im.ys + (z-1)*im.zs + c]);
	}
	
	/* Computes the eigendecomposition of a real symmetric matrix, 
	 * A = Q * diag(L) * Q', where Q is a real orthogonal matrix and L is a real 
	 * diagonal matrix.
	 *
	 * A must be an [nxn] matrix. Q is [nxm], where m is in the interval [1, n],
	 * depending on the values of A. L is [nx1], where the first m elements are
	 * sorted in ascending order. The remaining n - m elements are zero. 
	 * 
	 * If Q is NULL, the eigenvectors will not be computed.
	 *
	 * The eigendecomposition is computed by divide and conquer.
	 * 
	 * This function resizes all non-null outputs and sets their type to double.
	 *
	 * This function does not ensure that A is symmetric.
	 *
	 * All matrices must be initialized prior to calling this funciton.
	 * All matrices must have type double.
	 *
	 * Note: This function computes all of the eigenvalues, to a high degree of 
	 * accuracy. A faster implementation is possible if you do not need high
	 * precision, or if you do not need all of the eigenvalues, or if you do not 
	 * need eigenvalues outside of some interval. 
	 */
	private int eigen_Mat_rm(Mat_rm A, Mat_rm Q, Mat_rm L)
	{
        int status;
        int i;
		Mat_rm A_trans = new Mat_rm();
		double work[];
		int iwork[];
		double lwork_ret[] = new double[1];
		int info[] = new int[1];
		int lwork, liwork;

		final char jobz = Q == null ? 'N' : 'V';
		final char uplo = 'U';
		final int n = A.num_cols;
		final int lda = n;
		final int lwork_query = -1;
		final int liwork_query = -1;

		// Verify inputs
		if (A.num_rows != n) {
			System.err.println("eigen_Mat_rm: A be square");
			return SIFT3D_FAILURE;
		}
		if (A.type != Mat_rm_type.SIFT3D_DOUBLE) {
			System.err.println("eigen_Mat_rm: A must have type double");
			return SIFT3D_FAILURE;
		}
		// Resize outputs
		L.num_rows = n;
		L.num_cols = 1;
		L.type = Mat_rm_type.SIFT3D_DOUBLE;
		status = resize_Mat_rm(L);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

		// Initialize intermediate matrices and buffers
		work = null;
		iwork = null;
		status = init_Mat_rm(A_trans, 0, 0, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
		if (status == SIFT3D_FAILURE) {
			cleanup_Mat_rm(A_trans);
			return SIFT3D_FAILURE;
		}

		// Copy the input matrix (A = A')
		status = copy_Mat_rm(A, A_trans);
		if (status == SIFT3D_FAILURE) {
			cleanup_Mat_rm(A_trans);
			return SIFT3D_FAILURE;
		}
		
		GeneralizedEigenvalue ge = new GeneralizedEigenvalue();

		// Query for the workspace sizes
		double w[] = new double[n];
		ge.dsyev(jobz, uplo, n, A_trans.data_double,lda,w,lwork_ret,lwork_query,info);
		/*dsyevd_(&jobz, &uplo, &n, A_trans.u.data_double, &lda, L->u.data_double,
			&lwork_ret, &lwork_query, &liwork, &liwork_query, &info);*/

		if (info[0] != 0) {
			System.err.println("eigen_Mat_rm: LAPACK dsyev workspace query error code " + info[0]);
			w = null;
			cleanup_Mat_rm(A_trans);
			return SIFT3D_FAILURE;
		}
		// Allocate work spaces 
		lwork = (int)lwork_ret[0];
		try {
			work = new double[lwork];
		}
		catch (OutOfMemoryError e) {
			System.err.println("eigen_Mat_rm: Out of memory error on work = new double[lwork]");
			w = null;
			cleanup_Mat_rm(A_trans);
			return SIFT3D_FAILURE;
		}
		/*if ((work = (double *)malloc(lwork * sizeof(double))) == NULL ||
		    (iwork =
		     (fortran_int *) malloc(liwork * sizeof(fortran_int))) == NULL)
			goto EIGEN_MAT_RM_QUIT;*/

		// Compute the eigendecomposition
		ge.dsyev(jobz, uplo, n, A_trans.data_double, lda, w, work, lwork, info);
		/*dsyevd_(&jobz, &uplo, &n, A_trans.u.data_double, &lda, L->u.data_double,
			work, &lwork, iwork, &liwork, &info);*/

		if (info[0] != 0) {
			System.err.println("eigen_Mat_rm: LAPACK dsyev error code" + info[0]);
			w = null;
			work = null;
			cleanup_Mat_rm(A_trans);
			return SIFT3D_FAILURE;
		}
		for (i = 0; i < n; i++) {
			L.data_double[i][0] = w[i];
		}
		w = null;
		// Optionally return the eigenvectors
		if (Q != null) {
			status = transpose_Mat_rm(A_trans, Q);
			if (status == SIFT3D_FAILURE) {
				System.err.println("eigen_Mat_rm: failure on transpose_Mat_rm(A_trans, Q)");
				w = null;
				work = null;
				cleanup_Mat_rm(A_trans);
				return SIFT3D_FAILURE;	
			}
		}
		

		work = null;
		cleanup_Mat_rm(A_trans);
		return SIFT3D_SUCCESS;

	 
	}

	/* Extract SIFT3D descriptors from a list of keypoints. Uses the Gaussian
	 * scale-space pyramid from the previous call to SIFT3D_detect_keypoints on
	 * this SIFT3D struct. To extract from an image, see 
	 * SIFT3D_extract_raw_descriptors. 
	 *
	 * Note: To check if SIFT3D_detect_keypoints has been called on this struct,
	 * use SIFT3D_have_gpyr.
	 *
	 * Parameters:
	 *  sift3d - (initialized) struct defining the algorithm parameters. Must have
	 *      been used in some previous call to SIFT3D_detect_keypoints.
	 *  kp - keypoint list populated by a feature detector 
	 *  desc - (initialized) struct to hold the descriptors
	 *
	 * Return value:
	 *  Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise.
	 */
	private int SIFT3D_extract_descriptors(SIFT3DC sift3d, 
	        Keypoint_store kp, 
	        SIFT3D_Descriptor_store desc) {

		int status;
		// Verify inputs
		status = verify_keys(kp, sift3d.im);
		if (status == SIFT3D_FAILURE) {
			return SIFT3D_FAILURE;
		}

	        // Check if a Gaussian scale-space pyramid is available for processing
	        status = SIFT3D_have_gpyr(sift3d);
	        if (status == SIFT3D_FAILURE) {
	                System.err.println("SIFT3D_extract_descriptors: no Gaussian pyramid is ");
	                System.err.println("available. Make sure SIFT3D_detect_keypoints was ");
	                System.err.println("called prior to calling this function.");
	                return SIFT3D_FAILURE;
	        }

	        // Extract features
	        status = _SIFT3D_extract_descriptors(sift3d, sift3d.gpyr, kp, desc);
	        if (status == SIFT3D_FAILURE) {
	                return SIFT3D_FAILURE;
	        }

	        return SIFT3D_SUCCESS;
	}
	
	/* Verify that keypoints kp are valid in image im. Returns SIFT3D_SUCCESS if
	 * valid, SIFT3D_FAILURE otherwise. */
	private int verify_keys(Keypoint_store kp, Image im) {

	        int i;

		final int num = kp.slab.num;

		// Check the number of keypoints
		if (num < 1) {
			System.err.println("verify_keys: invalid number of keypoints: " + num);
			return SIFT3D_FAILURE;
		}

		// Check each keypoint
	        for (i = 0; i < num; i++) {

	                Keypoint key = kp.buf[i];

	                double octave_factor = Math.pow(2.0, key.o);

	                if (key.xd < 0 ||
	                        key.yd < 0 ||
	                        key.zd < 0 ||
	                        key.xd * octave_factor >= (double)im.nx || 
	                        key.yd * octave_factor >= (double)im.ny || 
	                        key.zd * octave_factor >= (double) im.nz) {
	                        System.err.println("verify_keys: keypoint " + i + " (" +key.xd + ", " + key.yd + ", " + key.zd + ") ");
	                        System.err.println("octave " + key.o + " exceeds image dimensions ");
	                        System.err.println("(" + im.nx + ", " +  im.ny + ", " + im.nz + ")");
	                        return SIFT3D_FAILURE; 
	                }

	                if (key.sd <= 0) {
	                        System.err.println("verify_keys: keypoint " + i + " has invalid scale " + key.sd);
	                        return SIFT3D_FAILURE;
	                }
	        }

	        return SIFT3D_SUCCESS;
	}
	
	/* Check if the Gaussian scale-space pyramid in a SIFT3D struct is valid. This
	 * shall return SIFT3D_TRUE if the struct was initialized, and 
	 * SIFT3D_detect_keypoints has been successfully called on it since 
	 * initialization. 
	 *
	 * Note: sift3d must be initialized before calling this function. */
	private int SIFT3D_have_gpyr(SIFT3DC sift3d) {

	        final Pyramid gpyr = sift3d.gpyr;

	        if (gpyr.levels != null && gpyr.num_levels != 0 && 
	                gpyr.num_octaves != 0) {
	        	return SIFT3D_SUCCESS;
	        }
	        else {
	        	return SIFT3D_FAILURE;
	        }
	}
	
	/* Helper funciton to extract SIFT3D descriptors from a list of keypoints and 
	 * an image. Called by SIFT3D_extract_descriptors and 
	 * SIFT3D_extract_raw_descriptors.
	 *
	 * parameters:
	 *  sift3d - (initialized) struct defining the algorithm parameters
	 *  gpyr - A Gaussian Scale-Space pyramid containing the image data
	 *  kp - keypoint list populated by a feature detector 
	 *  desc - (initialized) struct to hold the descriptors
	 *  use_gpyr - see im for details */
	private int _SIFT3D_extract_descriptors(SIFT3DC sift3d, 
	        Pyramid gpyr, Keypoint_store kp, 
	        SIFT3D_Descriptor_store desc) {

		int status;
		int i, ret;

		final Image first_level = 
	                SIFT3D_PYR_IM_GET(gpyr, gpyr.first_octave, gpyr.first_level);

		final int num = kp.slab.num;

		// Initialize the metadata 
		desc.nx = first_level.nx;	
		desc.ny = first_level.ny;	
		desc.nz = first_level.nz;	

		// Resize the descriptor store
	    status = resize_SIFT3D_Descriptor_store(desc, num);
	    if (status == SIFT3D_FAILURE) {
	                return SIFT3D_FAILURE;
	    }

	        // Extract the descriptors
	        ret = SIFT3D_SUCCESS;
	//#pragma omp parallel for
		for (i = 0; i < desc.num; i++) {

	                Keypoint key = kp.buf[i];
			SIFT3D_Descriptor descrip = desc.buf[i];
			Image level = 
	                        SIFT3D_PYR_IM_GET(gpyr, key.o, key.s);
			status = extract_descrip(sift3d, level, key, descrip);
			if (status == SIFT3D_FAILURE) {
	               ret = SIFT3D_FAILURE;
	         }
		}

		return ret;
	}

	/* Resize a SIFT3D_Descriptor_store to hold n descriptors. Must be initialized
	 * prior to calling this function. num must be positive.
	 *
	 * Returns SIFT3D_SUCCESS on success, SIFT3D_FAILURE otherwise. */
	private int resize_SIFT3D_Descriptor_store(SIFT3D_Descriptor_store desc,
	        int num) {

	        if (num < 1) {
	                System.err.println("resize_SIFT3D_Descriptor_store: invalid size: " + num);
	                return SIFT3D_FAILURE;
	        }

		    SIFT3D_Descriptor newbuf[] = new SIFT3D_Descriptor[num];
		    for (int i = 0; i < num; i++) {
		    	newbuf[i] = new SIFT3D_Descriptor();
		    	if (desc.buf != null) {
			    	if (i < desc.buf.length) {
			    	    newbuf[i].xd = desc.buf[i].xd;
			    	    newbuf[i].yd = desc.buf[i].yd;
			    	    newbuf[i].zd = desc.buf[i].zd;
			    	    newbuf[i].sd = desc.buf[i].sd;
			    	    for (int j = 0; j < DESC_NUM_TOTAL_HIST; j++) {
			    	        for (int k = 0; k < HIST_NUMEL; k++) {
			    	        	newbuf[i].hists[j].bins[k] = desc.buf[i].hists[j].bins[k];
			    	        }
			    	    }
			    	}
		    	}
		    }
	        desc.buf = newbuf;

		desc.num = num;
	        return SIFT3D_SUCCESS;
	}
	
	/* Helper routine to extract a single SIFT3D descriptor */
	private int extract_descrip(SIFT3DC sift3d, Image im,
		   Keypoint key, SIFT3D_Descriptor desc) {

	    int status;
		double buf[][] = new double[IM_NDIMS][IM_NDIMS];
	    Mat_rm Rt = new Mat_rm();
		Cvec vcenter = new Cvec();
		Cvec vim = new Cvec();
		Cvec vkp = new Cvec();
		Cvec vbins = new Cvec();
		Cvec grad = new Cvec();
		Cvec grad_rot = new Cvec();
		Hist hist;
		double weight, sq_dist;
		int i, x, y, z, a, p;

		// Compute basic parameters 
	    final double sigma = key.sd * desc_sig_fctr;
		final double win_radius = desc_rad_fctr * sigma;
		final double desc_half_width = win_radius / Math.sqrt(2);
		final double desc_width = 2.0 * desc_half_width;
	    final double desc_hist_width = desc_width / NHIST_PER_DIM;
		final double desc_bin_fctr = 1.0 / desc_hist_width;
		final double coord_factor = Math.pow(2.0, key.o);

	        // Invert the rotation matrix
	    status = init_Mat_rm_p(Rt, buf, IM_NDIMS, IM_NDIMS, Mat_rm_type.SIFT3D_DOUBLE, SIFT3D_FALSE);
	    if (status == SIFT3D_FAILURE) {
	    	return SIFT3D_FAILURE;
	    }
	    status = transpose_Mat_rm(key.R, Rt);
	    if (status == SIFT3D_FAILURE) {
	        return SIFT3D_FAILURE;
	    }

		// Zero the descriptor
		for (i = 0; i < DESC_NUM_TOTAL_HIST; i++) {
			hist = desc.hists[i];
	                hist_zero(hist);
		}

		// Iterate over a sphere window in real-world coordinates 
		vcenter.x = key.xd;
		vcenter.y = key.yd;
		vcenter.z = key.zd;
		final double uxf = im.ux;
        final double uyf = im.uy;
        final double uzf = im.uz;
	    final int x_start = (int)Math.max(Math.floor(vcenter.x - win_radius / uxf), 1);
	    final int x_end   = (int)Math.min(Math.ceil(vcenter.x + win_radius / uxf),
                im.nx - 2);
	    final int y_start = (int)Math.max(Math.floor(vcenter.y - win_radius / uyf), 1);
	    final int y_end   = (int)Math.min(Math.ceil(vcenter.y + win_radius / uyf),
                im.ny - 2);
        final int z_start = (int)Math.max(Math.floor(vcenter.z - win_radius / uzf), 1);
	    final int z_end   = (int)Math.min(Math.ceil(vcenter.z + win_radius / uzf),
                im.nz - 2); 
	    for (z = z_start; z <= z_end; z++) {
	    	for (y = y_start; y <= y_end; y++) {
	    		for (x = x_start; x <= x_end; x++) {
                vim.x = (x - vcenter.x) * uxf;
                vim.y = (y - vcenter.y) * uyf;
                vim.z = (z - vcenter.z) * uzf;
                sq_dist = SIFT3D_CVEC_L2_NORM_SQ(vim);
                if (sq_dist > win_radius * win_radius) 
	                continue; 

			// Rotate to keypoint space
			SIFT3D_MUL_MAT_RM_CVEC(Rt, vim, vkp);		

			// Compute spatial bins
			vbins.x = (vkp.x + desc_half_width) * desc_bin_fctr;
			vbins.y = (vkp.y + desc_half_width) * desc_bin_fctr;
			vbins.z = (vkp.z + desc_half_width) * desc_bin_fctr;

			// Reject points outside the rectangular descriptor 
			if (vbins.x < 0 || vbins.y < 0 || vbins.z < 0 ||
				vbins.x >= (float) NHIST_PER_DIM ||
				vbins.y >= (float) NHIST_PER_DIM ||
				vbins.z >= (float) NHIST_PER_DIM)
				continue;

			// Take the gradient
			IM_GET_GRAD_ISO(im, x, y, z, 0, grad);

			// Apply a Gaussian window
			weight = Math.exp(-0.5 * sq_dist / (sigma * sigma));
			SIFT3D_CVEC_SCALE(grad, weight);

	                // Rotate the gradient to keypoint space
			SIFT3D_MUL_MAT_RM_CVEC(Rt, grad, grad_rot);

			// Finally, accumulate bins by 5x linear interpolation
			SIFT3D_desc_acc_interp(sift3d, vbins, grad_rot, desc);
	    		}
	    	}
	    }

		// Histogram refinement steps
		for (i = 0; i < DESC_NUM_TOTAL_HIST; i++) {
			refine_Hist(desc.hists[i]);
		}

		// Normalize the descriptor
		normalize_desc(desc);

		// Truncate
		for (i = 0; i < DESC_NUM_TOTAL_HIST; i++) {
			hist = desc.hists[i];
			
			 if (ICOS_HIST) {
	            	for (a = 0; a < HIST_NUMEL; a++) {
	            	    hist.bins[a] = Math.min(hist.bins[a], trunc_thresh);	
	            	}
	            }
	            else {
	            	for (p = 0; p < NBINS_PO; p++) {
	            		for (a = 0; a < NBINS_AZ; a++) {
	            		    hist.bins[a + p * NBINS_AZ]	= Math.min(hist.bins[a + p * NBINS_AZ], trunc_thresh);
	            		}
	            	}
	            }
		}

		// Normalize again
		normalize_desc(desc);

		// Save the descriptor location in the original image
		// coordinates
		desc.xd = key.xd * coord_factor;
		desc.yd = key.yd * coord_factor;
		desc.zd = key.zd * coord_factor;
		desc.sd = key.sd;

	        return SIFT3D_SUCCESS;
	}
	
	/* Set a histogram to zero */
	private void hist_zero(Hist hist) {

	        int a, p;
            if (ICOS_HIST) {
            	for (a = 0; a < HIST_NUMEL; a++) {
            	    hist.bins[a] = 0.0;	
            	}
            }
            else {
            	for (p = 0; p < NBINS_PO; p++) {
            		for (a = 0; a < NBINS_AZ; a++) {
            		    hist.bins[a + p * NBINS_AZ]	= 0.0;
            		}
            	}
            }
	}

	/* Computes v_out = mat * v_in. Note that mat must be of FLOAT
	 * type, since this is the only type available for vectors. 
	 * Also note that mat must be (3 x 3). */
	private void SIFT3D_MUL_MAT_RM_CVEC(Mat_rm mat, Cvec v_in, Cvec v_out) {
		v_out.x = mat.data_double[0][0] * v_in.x +
		    	     mat.data_double[0][1] * v_in.y +
	                 mat.data_double[0][2] * v_in.z;
		
		v_out.y = mat.data_double[1][0] * v_in.x +
	              mat.data_double[1][1] * v_in.y +
	              mat.data_double[1][2] * v_in.z;
		
		v_out.z = mat.data_double[2][0] * v_in.x + 
	              mat.data_double[2][1] * v_in.y +
	              mat.data_double[2][2] * v_in.z;
	}
	
	/* Helper routine to interpolate over the histograms of a
	 * SIFT3D descriptor. */
	private void SIFT3D_desc_acc_interp(SIFT3DC sift3d, 
					Cvec vbins, 
				    Cvec grad,
					SIFT3D_Descriptor desc) {

		int status;
		Cvec dvbins = new Cvec();
		Hist hist;
		double weight;
		int dx, dy, dz, x, y, z;

	//#ifdef ICOS_HIST
		Cvec bary = new Cvec();
		double mag = 0.0;
		int bin[] = new int[1];;	
	//#else
		Svec sbins = null;
		Svec dsbins = null;
		int da, dp, a, p;
	//#endif
		Mesh mesh = null;

		final int y_stride = NHIST_PER_DIM;
		final int z_stride = NHIST_PER_DIM * NHIST_PER_DIM; 

		// Compute difference from integer bin values
		dvbins.x = vbins.x - Math.floor(vbins.x);
		dvbins.y = vbins.y - Math.floor(vbins.y);
		dvbins.z = vbins.z - Math.floor(vbins.z);

		// Compute the histogram bin
	if (ICOS_HIST) {
		mesh = sift3d.mesh;

		// Get the index of the intersecting face 
		status = icos_hist_bin(sift3d, grad, bary, bin);
		if (status == SIFT3D_FAILURE) {
			return;
		}
		
		// Get the magnitude of the vector
		mag = SIFT3D_CVEC_L2_NORM(grad);
	}

	else {
		sbins = new Svec();
		dsbins = new Svec();
		status = Cvec_to_sbins(grad, sbins);
		if (status == SIFT3D_FAILURE) {
			return;
		}
		dsbins.az = sbins.az - Math.floor(sbins.az);
		dsbins.po = sbins.po - Math.floor(sbins.po);
	}
		
		for (dx = 0; dx < 2; dx++) {
		for (dy = 0; dy < 2; dy++) {
	        for (dz = 0; dz < 2; dz++) {

	                x = (int) vbins.x + dx;
	                y = (int) vbins.y + dy;
	                z = (int) vbins.z + dz;

	                // Check boundaries
	                if (x < 0 || x >= NHIST_PER_DIM ||
	                        y < 0 || y >= NHIST_PER_DIM ||
	                        z < 0 || z >= NHIST_PER_DIM)
	                        continue;

	                // Get the histogram
	                hist = desc.hists[x + y * y_stride + z * z_stride];	

	                if (x + y * y_stride + z * z_stride >= DESC_NUM_TOTAL_HIST) {
	                	System.err.println("In SIFT3D_desc_acc_interp x + y * y_stride + z * z_stride >= DESC_NUM_TOTAL_HIST");
	                	return;
	                }
	                

	                // Get the spatial interpolation weight
	                weight = ((dx == 0) ? (1.0f - dvbins.x) : dvbins.x) *
	                        ((dy == 0) ? (1.0f - dvbins.y) : dvbins.y) *
	                        ((dz == 0) ? (1.0f - dvbins.z) : dvbins.z);

	                /* Add the value into the histogram */
	if (ICOS_HIST) {
	                if (HIST_NUMEL != ICOS_NVERT) {
	                	System.err.println("In SIFT3D_desc_acc_interp HIST_NUMEL != ICOS_NVERT");
	                	return;
	                }
	                if (bin[0] < 0 || bin[0] >= ICOS_NFACES) {
	                    System.err.println("In SIFT3D_desc_acc_interp bin[0] < 0 || bin[0] >= ICOS_NFACES");
	                    return;
	                }

	                // Interpolate over three vertices
	                hist.bins[mesh.tri[bin[0]].idx[0]] += mag * weight * bary.x;
	                hist.bins[mesh.tri[bin[0]].idx[1]] += mag * weight * bary.y;
	                hist.bins[mesh.tri[bin[0]].idx[2]] += mag * weight * bary.z; 
	}
	else {
	                // Iterate over all angles
	                for (dp = 0; dp < 2; dp ++) {
	                for (da = 0; da < 2; da ++) {

	                        a = ((int) sbins.az + da) % NBINS_AZ;
	                        p = (int) sbins.po + dp;
	                        if (p >= NBINS_PO) {
	                                // See HIST_GET_PO
	                                a = (a + NBINS_AZ / 2) % NBINS_AZ;
	                                p = NBINS_PO - 1;
	                        }
			
	                        if (a < 0) {
	                        	System.err.println("In SIFT3D_desc_acc_interp a < 0");
	                        	return;
	                        }
	                        if (a >= NBINS_AZ) {
	                        	System.err.println("In SIFT3D_desc_acc_interp a >= NBINS_AZ");
	                        	return;
	                        }
	                        if (p < 0) {
	                        	System.err.println("In SIFT3D_desc_acc_interp p < 0");
	                        	return;
	                        }
	                        if (p >= NBINS_PO) {
	                        	System.err.println("In SIFT3D_desc_acc_interp p >= NBINS_PO");
	                        	return;
	                        }

	                        hist.bins[a + p * NBINS_AZ] += sbins.mag * weight *
	                                ((da == 0) ? (1.0 - dsbins.az) : dsbins.az) *
	                                ((dp == 0) ? (1.0 - dsbins.po) : dsbins.po);
	                }}
	}
		}}}

	}
	
	/* Get the bin and barycentric coordinates of a vector in the icosahedral 
	 * histogram. */
	//SIFT3D_IGNORE_UNUSED
	private int icos_hist_bin(SIFT3DC sift3d,
				   Cvec x, Cvec bary,
				   int bin[]) { 

		int status;
		double k[] = new double[1];
		int i;

		final Mesh mesh = sift3d.mesh;

		// Check for very small vectors
		if (SIFT3D_CVEC_L2_NORM_SQ(x) < bary_eps)
			return SIFT3D_FAILURE;
		

		// Iterate through the faces
		for (i = 0; i < ICOS_NFACES; i++) {

			Tri tri = mesh.tri[i];

			// Convert to barycentric coordinates
			status = cart2bary(x, tri, bary, k);
			if (status == SIFT3D_FAILURE) {
				continue;
			}

			// Test for intersection
			if (bary.x < -bary_eps || bary.y < -bary_eps ||
			    bary.z < -bary_eps || k[0] < 0) {
				continue;
			}

			// Save the bin
			bin[0] = i;

			// No other triangles will be intersected
			return SIFT3D_SUCCESS;
		}	

		// Unreachable code
		System.err.println("SIFT3D_FAILURE from unreachable code icos_hist_bin");
		return SIFT3D_FAILURE;
	}
	
	/* Convert Cartesian coordinates to barycentric. bary is set to all zeros if
	 * the problem is unstable. 
	 *
	 * The output value k is the constant by which the ray is multiplied to
	 * intersect the supporting plane of the triangle.
	 *
	 * This code uses the Moller-Trumbore algorithm. */
	private int cart2bary(Cvec cart, Tri tri, 
			      Cvec bary, double k[]) {

		int i;
		Cvec e1 = new Cvec();
		Cvec e2 = new Cvec();
		Cvec t = new Cvec();
		Cvec p = new Cvec();
		Cvec q = new Cvec();
		double det, det_inv;

		Cvec v[] = new Cvec[3];
		for (i = 0; i < 3; i++) {
			v[i] = new Cvec();
		}
		v[0].x = tri.v[0].x;
		v[0].y = tri.v[0].y;
		v[0].z = tri.v[0].z;
		v[1].x = tri.v[1].x;
		v[1].y = tri.v[1].y;
		v[1].z = tri.v[1].z;
		v[2].x = tri.v[2].x;
		v[2].y = tri.v[2].y;
		v[2].z = tri.v[2].z;

		SIFT3D_CVEC_MINUS(v[1], v[0], e1);
		SIFT3D_CVEC_MINUS(v[2], v[0], e2);
		SIFT3D_CVEC_CROSS(cart, e2, p);
		det = SIFT3D_CVEC_DOT(e1, p);

		// Reject unstable points
		if (Math.abs(det) < bary_eps) {
			return SIFT3D_FAILURE;
		}

		det_inv = 1.0 / det;

		t.x = v[0].x;
		t.y = v[0].y;
		t.z = v[0].z;
		SIFT3D_CVEC_SCALE(t, -1.0);	

		SIFT3D_CVEC_CROSS(t, e1, q);

		bary.y = det_inv * SIFT3D_CVEC_DOT(t, p);	
		bary.z = det_inv * SIFT3D_CVEC_DOT(cart, q);
		bary.x = 1.0 - bary.y - bary.z;

		k[0] = SIFT3D_CVEC_DOT(e2, q) * det_inv;

	//#ifndef NDEBUG
		Cvec temp1, temp2, temp3;
	        double residual;

	        if (Double.isNaN(bary.x) || Double.isNaN(bary.y) || Double.isNaN(bary.z)) {
	                System.err.println("cart2bary: invalid bary (" + bary.x + ", " + bary.y + ", " + bary.z + ")");
	                return SIFT3D_FAILURE;
	                //exit(1);
	        }

		// Verify k * c = bary->x * v1 + bary->y * v2 + bary->z * v3
		temp1 = v[0];
		temp2 = v[1];
		temp3 = v[2];
		SIFT3D_CVEC_SCALE(temp1, bary.x);
		SIFT3D_CVEC_SCALE(temp2, bary.y);	
		SIFT3D_CVEC_SCALE(temp3, bary.z);	
		SIFT3D_CVEC_PLUS(temp1, temp2, temp1);
		SIFT3D_CVEC_PLUS(temp1, temp3, temp1);
		SIFT3D_CVEC_SCALE(temp1, 1.0 / k[0]);
		SIFT3D_CVEC_MINUS(temp1, cart, temp1);
	        residual = SIFT3D_CVEC_L2_NORM(temp1);
		if (residual > bary_eps) {
	                System.err.println("cart2bary: residual: " + residual);
	                return SIFT3D_FAILURE;
	                //exit(1);
	        }
	//#endif
		return SIFT3D_SUCCESS;
	}
	
	/* Bin a Cartesian gradient into Spherical gradient bins */
	//SIFT3D_IGNORE_UNUSED
	private int Cvec_to_sbins(Cvec vd, Svec bins) {

		// Convert to spherical coordinates
		SIFT3D_CVEC_TO_SVEC(vd, bins);
		//FIXME: Is this needed? SIFT3D_CVEC_TO_SVEC cannot divide by zero
		if (bins.mag < FLT_EPSILON * 1E2)
			return SIFT3D_FAILURE;

		// Compute bins
		bins.az *= (double) NBINS_AZ / SIFT3D_AZ_MAX_F; 
		bins.po *= (double) NBINS_PO / SIFT3D_PO_MAX_F;

		if (bins.az >= NBINS_AZ) {
			System.err.println("In Cvec_to_sbins bins.az >= NBIBS_AZ");
			return SIFT3D_FAILURE;
		}
		if (bins.po >= NBINS_PO) {
			System.err.println("in Cvec_to_sbins bins.po >= NBINS_PO");
			return SIFT3D_FAILURE;
		}

		return SIFT3D_SUCCESS;
	}
	
	// Convert a vector from Cartesian to Spherical coordinates.
	private void SIFT3D_CVEC_TO_SVEC(Cvec cvec, Svec svec) {
		svec.mag = Math.sqrt(cvec.x * cvec.x + cvec.y * cvec.y +
							cvec.z * cvec.z);
		svec.az = fmodf(Math.atan2(cvec.y, cvec.x) + SIFT3D_AZ_MAX_F,
						  SIFT3D_AZ_MAX_F);
		svec.po = fmodf(Math.acos(cvec.z / (svec.mag + FLT_EPSILON)),
			     SIFT3D_PO_MAX_F);
	}
	
	private double fmodf(double in1, double in2) {
		if (in2 == 0.0) {
			return Double.NaN;
		}
		else {
			double intdiv = Math.floor(Math.abs(in1/in2));
			double intprod;
			if (((in1 > 0) && (in2 > 0)) || ((in1 < 0) && (in2 < 0))) {
				intprod = in2 * intdiv;
			}
			else {
			    intprod = -in2 * intdiv;
			}
			double result = in1 - intprod;
			return result;
		}
	}
	
	/* Refine a gradient histogram with optional operations,
	 * such as solid angle weighting. */
	private void refine_Hist(Hist hist) {

	if (!ICOS_HIST) {

	if (SIFT3D_ORI_SOLID_ANGLE_WEIGHT) {	
		double po;
		int a, p;
		// TODO: could accelerate this with a lookup table		

		// Weight by the solid angle of the bins, ignoring constants
		for (p = 0; p < NBINS_PO; p++) {
    		for (a = 0; a < NBINS_AZ; a++) {
			po = p * SIFT3D_PO_MAX_F / NBINS_PO;
		 hist.bins[a + p * NBINS_AZ] /= Math.cos(po) - Math.cos(po + 
				SIFT3D_PO_MAX_F / NBINS_PO);
    		}
		}
		}

	} // if (!ICOS_HIST)

	}
	
	/* Normalize a descriptor */
	private void normalize_desc(SIFT3D_Descriptor desc) {

		double norm; 
		int i, a, p;

		norm = 0.0;
		double el;
		for (i = 0; i < DESC_NUM_TOTAL_HIST; i++) { 

	               Hist hist = desc.hists[i];
 
			if (ICOS_HIST) {
            	for (a = 0; a < HIST_NUMEL; a++) {
            	    el = hist.bins[a];	
            	    norm += el * el;
            	}
            }
            else {
            	for (p = 0; p < NBINS_PO; p++) {
            		for (a = 0; a < NBINS_AZ; a++) {
            		    el = hist.bins[a + p * NBINS_AZ];
            		    norm += el * el;
            		}
            	}
            }
		}

		norm = Math.sqrt(norm) + DBL_EPSILON; 

		for (i = 0; i < DESC_NUM_TOTAL_HIST; i++) {

	                Hist hist = desc.hists[i];
			        double norm_inv = 1.0 / norm; 

			 if (ICOS_HIST) {
	            	for (a = 0; a < HIST_NUMEL; a++) {
	            	    hist.bins[a] *= norm_inv;	
	            	}
	            }
	            else {
	            	for (p = 0; p < NBINS_PO; p++) {
	            		for (a = 0; a < NBINS_AZ; a++) {
	            		    hist.bins[a + p * NBINS_AZ]	*= norm_inv;
	            		}
	            	}
	            }
			
		}
	}

	
}
