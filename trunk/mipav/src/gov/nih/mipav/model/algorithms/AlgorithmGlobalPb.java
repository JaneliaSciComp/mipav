package gov.nih.mipav.model.algorithms;

import java.io.IOException;
import java.util.ArrayList;

import gov.nih.mipav.model.algorithms.filters.AlgorithmHilbertTransform;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.view.MipavUtil;

/**
 * Compute globalPb, the globalized probability of boundary of an image
 * Reference: Contour Detection and Hierarchical Image Segmentation by Pablo Arbelaez, Michael Maire,
 * Charless Fowlkes, and Jitendra Malik, IEEE Transactions on Pattern Analysis and Machine Intelligence,
 * Volume 33, Issue 5, August, 2010, pp. 898-916.
 * 
 * BSR package code written in MATLAB and C++ is ported to Java with the permission of 
 * Pablo Andres Arbelaez Escalante.
 */

public class AlgorithmGlobalPb extends AlgorithmBase {
	
	// Resizing factor in (0, 1], to speed up eigenvector
	private double rsz = 1.0;
	
	private int xDim;
	
	private int yDim;
	
	private int sliceSize;
	
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmGlobalPb - default constructor.
     */
    public AlgorithmGlobalPb() { }
    
    /**
     * 
     * @param destImg
     * @param srcImg
     */
    public AlgorithmGlobalPb(ModelImage destImg, ModelImage srcImg) {
    	super(destImg, srcImg);
    }
    
    /**
     * Original globalPb.m by Pablo Arbelaez on December, 2010
     */
    public void runAlgorithm() {
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        ModelImage inputImage;
      	AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
      	double weights[] = new double[13];
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        if ((rsz <= 0.0) || (rsz > 1.0)) {
        	MipavUtil.displayError("Resizing factor rsz out of range (0, 1]");
        	setCompleted(false);
        	return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Global Pb ...");
        
        if (srcImage.isColorImage()) {
        	inputImage = new ModelImage(ModelStorageBase.ARGB_FLOAT,
      				srcImage.getExtents(), "changeTypeImage");
        }
        else {
        	inputImage = new ModelImage(ModelStorageBase.DOUBLE,
      				srcImage.getExtents(), "changeTypeImage");
        }
  		inputImage.getFileInfo(0).setEndianess(FileBase.LITTLE_ENDIAN);
  		changeTypeAlgo = new AlgorithmChangeType(inputImage,
				srcImage, srcImage.getMin(),
				srcImage.getMax(), 0.0, 1.0, image25D);
  		changeTypeAlgo.run();
  		changeTypeAlgo.finalize();
  		changeTypeAlgo = null;
  		fileInfo = inputImage.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(inputImage.getExtents());
        fileInfo[0].setMax(inputImage.getMax());
        fileInfo[0].setMin(inputImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        
        // Default feature weights
        if (srcImage.isColorImage()) {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0039;
        	weights[3] = 0.0050;
        	weights[4] = 0.0058;
        	weights[5] = 0.0069;
        	weights[6] = 0.0040;
        	weights[7] = 0.0044;
        	weights[8] = 0.0049;
        	weights[9] = 0.0024;
        	weights[10] = 0.0027;
        	weights[11] = 0.0170;
        	weights[12] = 0.0074;
        }
        else {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0054;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0048;
        	weights[10] = 0.0049;
        	weights[11] = 0.0264;
        	weights[12] = 0.0090;
        }
        
        // mPb
        multiscalePb(inputImage);
        
        setCompleted(true);
        return;
    }
    
    /**
     * Compute local contour cues of an image
     * gradinets by Michael Maire
     * Original MATLAB code by Pablo Arbelaez December, 2010
     * @param im
     */
    private void multiscalePb(ModelImage im) {
        double weights[] = new double[12];
        double buffer[];
        double red[] = new double[sliceSize];
        double green[] = new double[sliceSize];
        double blue[] = new double[sliceSize];
        int ini;
        int outi;
        int x;
        int y;
        if (im.isColorImage()) {
        	buffer = new double[4*sliceSize];
        	try {
        	    im.exportData(0, 4* sliceSize, buffer);
        	}
        	catch (IOException e) {
        	    e.printStackTrace();	
        	}
        	im.disposeLocal();
        	im = null;
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			ini = x + y * xDim;
        			outi = y + x * yDim;
            	    red[outi] = buffer[4*ini+1];
            	    green[outi] = buffer[4*ini+2];
            	    blue[outi] = buffer[4*ini+3];
        		}
            }
            weights[0] = 0.0146;
            weights[1] = 0.0145;
            weights[2] = 0.0163;
            weights[3] = 0.0210;
            weights[4] = 0.0243;
            weights[5] = 0.0287;
            weights[6] = 0.0166;
            weights[7] = 0.0185;
            weights[8] = 0.0204;
            weights[9] = 0.0101;
            weights[10] = 0.0111;
            weights[11] = 0.0141;
        }
        else {
        	buffer = new double[sliceSize];
        	try {
        	    im.exportData(0, sliceSize, buffer);
        	}
        	catch (IOException e) {
        	    e.printStackTrace();	
        	}
        	im.disposeLocal();
        	im = null;
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			ini = x + y * xDim;
        			outi = y + x * yDim;
        			red[outi] = buffer[ini];
        			green[outi] = buffer[ini];
        			blue[outi] = buffer[ini];
        		}
        	}
        	weights[0] = 0.0245;
        	weights[1] = 0.0220;
        	weights[2] = 0.0233;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0208;
        	weights[10] = 0.0210;
        	weights[11] = 0.0229;
        }
        
        // Get gradients
        det_mPb(red, green, blue);
    }
    
    /**
     * Compute image gradients.  Implementation by Michael Maire.
     * @param im
     */
    private void det_mPb(double red[], double green[], double blue[]) {
        // Compute pb
    	mex_pb_parts_final_selected(red, green, blue);
    }
    
    private void mex_pb_parts_final_selected(double L[], double a[], double b[]) {
    	// Border pixels
    	int border = 30;
    	// Mirror border
    	int dstXDim = xDim + 2 * border;
    	int dstYDim = yDim + 2 * border;
    	double L_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(L, L_border, border, border, xDim, yDim);
    	double a_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(a, a_border, border, border, xDim, yDim);
    	double b_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(b, b_border, border, border, xDim, yDim);
    	// Converting RGB to grayscale
    	double gray[] = new double[L_border.length];
    	grayscale(gray, L_border, a_border, b_border);
    	// Gamma correct
    	rgb_gamma_correct(L_border, a_border, b_border, 2.5);
    	// Converting RGB to Lab
    	rgb_to_lab(L_border, a_border, b_border);
    	lab_normalize(L_border, a_border, b_border);
    	// Quantizing color channels
    	// # bins for bg
    	int numLBins = 25;
    	// # bins for cg_a
    	int numaBins = 25;
    	// # bins for cg_b
    	int numbBins = 25;
    	int Lq[] = new int[L_border.length];
    	quantize_values(Lq, L_border, numLBins);
    	int aq[] = new int[a_border.length];
    	quantize_values(aq, a_border, numaBins);
    	int bq[] = new int[b_border.length];
    	quantize_values(bq, b_border, numbBins);
    	// Computing filter set for textons
    	// Number of orientations
    	int n_ori = 8;
    	// sigma for small tg filters
    	double sigma_tg_filt_sm = 2.0;
    	ArrayList <double[][]> filters_small = new ArrayList<double[][]>();
    	texton_filters(filters_small, n_ori, sigma_tg_filt_sm);
    }
    
    private void compute_border_mirror_2D(double src[], double dst[], int borderX, int borderY, int xSrcSize, int ySrcSize) {
    	// Compute destination size
    	int xDstSize = xSrcSize + 2 * borderX;
    	int yDstSize = ySrcSize + 2 * borderY;
    	// Compute step sizes in destinatio marix (for copying interior)
    	int indInit = borderX * yDstSize + borderY;
    	int indStep = 2 * borderY;
    	int n;
    	int ind;
    	int x;
    	int y;
    	int indIntr;
    	int indBdr;
    	
    	// Copy interior
    	for (n = 0, ind = indInit, x = 0; x < xSrcSize; x++) {
    		for (y = 0; y < ySrcSize; y++, n++, ind++) {
    			dst[ind] = src[n];
    		}
    		ind += indStep;
    	}
    	
    	// Mirror top
    	indIntr = borderX * yDstSize;
    	indBdr = indIntr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    	    indBdr -= 2 * yDstSize;
    	    for (y = 0; y < yDstSize; y++) {
    	    	dst[indBdr++] = dst[indIntr++];
    	    }
    	}
    	
    	// Mirror bottom
    	indBdr = (xSrcSize + borderX) * yDstSize;
    	indIntr = indBdr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    		indIntr -= 2*yDstSize;
    		for (y = 0; y < yDstSize; y++) {
    			dst[indBdr++] = dst[indIntr++];
    		}
    	}
    	
    	// Mirror left
    	indBdr = 0;
    	indIntr = 2 * borderY;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    		    dst[indBdr++] = dst[--indIntr];	
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	
    	// Mirror right
    	indBdr = yDstSize - borderY;
    	indIntr = indBdr;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    			dst[indBdr++] = dst[--indIntr];
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	return;
    }
    
    private void grayscale(double m[], double r[], double g[], double b[]) {
    	int n;
    	for (n = 0; n < m.length; n++) {
    		m[n] = (0.29894 * r[n]) + (0.58704 * g[n]) + (0.11402 * b[n]);
    	}
    	return;
    }
    
    private void rgb_gamma_correct(double r[], double g[], double b[], double gamma) {
    	int n;
    	for (n = 0; n < r.length; n++) {
    	    r[n] = Math.pow(r[n], gamma);
    	    g[n] = Math.pow(g[n], gamma);
    	    b[n] = Math.pow(b[n], gamma);
    	}
    	return;
    }
    
    /**
     * Convert from RGB color space to Lab color space
     * @param r_l
     * @param g_a
     * @param b_b
     */
    private void rgb_to_lab(double r_l[], double g_a[], double b_b[]) {
    	rgb_to_xyz(r_l, g_a, b_b);
    	xyz_to_lab(r_l, g_a, b_b);
    }
    
    /**
     * Convert from RGB color space to XYZ color space
     * @param r_x
     * @param g_y
     * @param b_z
     */
    private void rgb_to_xyz(double r_x[], double g_y[], double b_z[]) {
    	int n;
    	double r;
    	double g;
    	double b;
    	for (n = 0; n < r_x.length; n++) {
    		r = r_x[n];
    		g = g_y[n];
    		b = b_z[n];
    		r_x[n] = (0.412453 * r) + (0.357580 * g) + (0.180423 * b);
    		g_y[n] = (0.212671 * r) + (0.715160 * g) + (0.072169 * b);
    		b_z[n] = (0.019334 * r) + (0.119193 * g) + (0.950227 * b);
    	}
    	return;
    }
    
    /**
     * Convert from XYZ color space to Lab color space
     * @param x_l
     * @param y_a
     * @param z_b
     */
    private void xyz_to_lab(double x_l[], double y_a[], double z_b[]) {
        // D65 white point reference
    	final double xRef = 0.950456;
    	final double yRef = 1.000000;
    	final double zRef = 1.088754;
    	// Threshold value
    	final double threshold = 0.008856;
    	// Convert XYZ to Lab
    	int n;
    	double x;
    	double y;
    	double z;
    	double fx;
    	double fy;
    	double fz;
    	for (n = 0; n < x_l.length; n++) {
    		// Extract xyz color value and normalize by reference point
    		x = x_l[n]/xRef;
    		y = y_a[n]/yRef;
    		z = z_b[n]/zRef;
    		// Compute fx, fy, fz
    		fx = (x > threshold) ? Math.pow(x, (1.0/3.0)) : (7.787 * x + (16.0/116.0));
    		fy = (y > threshold) ? Math.pow(y, (1.0/3.0)) : (7.787 * y + (16.0/116.0));
    		fz = (z > threshold) ? Math.pow(z, (1.0/3.0)) : (7.787 * z + (16.0/116.0));
    		// Compute Lab color value
    		x_l[n] = (y > threshold) ? (116.0 * Math.pow(y, (1.0/3.0)) - 16.0) : (903.3 * y);
    		y_a[n] = 500.0 * (fx - fy);
    		z_b[n] = 200.0 * (fy - fz);
    	}
    	return;
    }
    
    /**
     * Normalize an Lab image so that values for each channel lie in [0,1].
     * @param l
     * @param a
     * @param b
     * @return
     */
    private void lab_normalize(double l[], double a[], double b[]) {
        // Range for a, b channels
    	final double abMin = -73.0;
    	final double abMax = 95.0;
    	final double abRange = abMax - abMin;
    	// Normalize Lab image
    	int n;
    	double lVal;
    	double aVal;
    	double bVal;
    	for (n = 0; n < l.length; n++) {
    		lVal = l[n]/100.0;
    		aVal = (a[n] - abMin)/abRange;
    		bVal = (b[n] - abMin)/abRange;
    		if (lVal < 0.0) {
    			lVal = 0.0;
    		}
    		else if (lVal > 1.0) {
    			lVal = 1.0;
    		}
    		if (aVal < 0.0) {
    			aVal = 0.0;
    		}
    		else if (aVal > 1.0) {
    			aVal = 1.0;
    		}
    		if (bVal < 0.0) {
    			bVal = 0.0;
    		}
    		else if (bVal > 1.0) {
    			bVal = 1.0;
    		}
    		l[n] = lVal;
    		a[n] = aVal;
    		b[n] = bVal;
    	}
    }
    
    private void quantize_values(int assign[], double m[], int n_bins) {
    	int n;
    	int bin;
    	for (n = 0; n < m.length; n++) {
    	    bin = (int)Math.floor(m[n]*n_bins);
	    	if (bin == n_bins) {
	    		bin = n_bins-1;
	    	}
	    	assign[n] = bin;
    	}
    }
    
    /**
     * Filters for computing textons.
     * 
     * The set of texton filters is the union of the even and odd-symmetric
     * filter sets in aditon to a center-surround difference of Gaussians filter.
     * 
     * Each returned filter is an (s + 1) x (s + 1) matrix where s = 3 * sigma and
     * sigma is the specified standard deviation.
     * 
     * Filters are created in parallel when possible
     * @param filters
     * @param n_ori
     * @param sigma
     */
    private void texton_filters(ArrayList <double[][]> filters, int n_ori, double sigma) {
    	// Allocate collection to hold filters
    	ArrayList <double[][]> filters_even = new ArrayList <double[][]>();
    	ArrayList <double[][]> filters_odd = new ArrayList <double[][]>();
    	oe_filters_even(n_ori, sigma, filters_even);
    	oe_filters_odd(n_ori, sigma, filters_odd);
    }
    
    private void oe_filters_even(int n_ori, double sigma, ArrayList <double[][]> filters_even) {
    	gaussian_filters(n_ori, sigma, 2, false, 3.0, filters_even);
    }
    
    private void oe_filters_odd(int n_ori, double sigma, ArrayList <double[][]> filters_odd) {
    	gaussian_filters(n_ori, sigma, 2, true, 3.0, filters_odd);
    }
    
    private void gaussian_filters(int n_ori, double sigma, int deriv, boolean hlbrt, double elongation,
    		ArrayList <double[][]> filters) {
    	double oris[] = new double[n_ori];
    	standard_filter_orientations(n_ori, oris);
    	gaussian_filters(oris, sigma, deriv, hlbrt, elongation, filters);
    }
    
    private void standard_filter_orientations(int n_ori, double[] oris) {
    	int n;
    	double ori = 0.0;
    	double ori_step = (n_ori > 0) ? (Math.PI/(double)n_ori) : 0;
    	for (n = 0; n < n_ori; n++, ori += ori_step) {
    		oris[n] = ori;
    	}
    	return;
    }
    
    private void gaussian_filters(double[] oris, double sigma, int deriv, boolean hlbrt, double elongation,
        ArrayList <double[][]> filters) {
    	int n;
    	// Compute support from sigma
    	int support = (int)Math.ceil(3 * sigma);
    	double sigmaX = sigma;
    	double sigmaY = sigmaX/elongation;
    	// Setup filter creators
    	int n_ori = oris.length;
    	for (n = 0; n < n_ori; n++) {
    	    double f[][] = gaussian_2D(sigmaX, sigmaY, oris[n], deriv, hlbrt, support, support);	
    	    filters.add(f);
    	}
    }
    
    /**
     * Gaussian kernel (2D)
     * The kernel is normalized to have unit L1 norm.  If returning a 1st or 2nd derivative,
     * the kernel has zero mean.
     * @param sigmaX standard deviation along x axis
     * @param sigmaY standard deviation along y axis
     * @param ori orientation in radians
     * @param deriv The 1st or 2nd derivative can be taken along the y-axis prior to rotation
     * @param hlbrt The hilbert transform can be taken along the y-axis prior to rotation
     * @param supportX
     * @param supportY
     * @return
     */
    private double[][] gaussian_2D(double sigmaX, double sigmaY, double ori, int deriv, boolean hlbrt, int supportX,
    		int supportY) {
    	double m[][] = null;
    	// Compute size of larger grid for axis-aligned gaussian
    	// Reverse rotate corners of bounding box by orientation
    	int support_x_rot = support_x_rotated(supportX, supportY, -ori);
    	int support_y_rot = support_y_rotated(supportX, supportY, -ori);
    	// Compute 1D kernels
    	double mx[] = gaussian(sigmaX, 0, false, support_x_rot);
    	return m;
    }
    
    /*
     * Compute integer-valued supports for rotated 2D matrix.
     */
    private int support_x_rotated(
       int support_x, int support_y, double ori)
    {
       return (int)(
          Math.ceil(support_x_rotated(
             (double)(support_x), (double)(support_y), ori
          ))
       );
    }

    private int support_y_rotated(
       int support_x, int support_y, double ori)
    {
       return (int)(
          Math.ceil(support_y_rotated(
             (double)(support_x), (double)(support_y), ori
          ))
       );
    }

    
    private double support_x_rotated(double support_x, double support_y, double ori) {
    	   final double sx_cos_ori = support_x * Math.cos(ori);
    	   final double sy_sin_ori = support_y * Math.sin(ori);
    	   double x0_mag = Math.abs(sx_cos_ori - sy_sin_ori);
    	   double x1_mag = Math.abs(sx_cos_ori + sy_sin_ori);
    	   return (x0_mag > x1_mag) ? x0_mag : x1_mag;
    	}

    private double support_y_rotated(double support_x, double support_y, double ori) {
    	   final double sx_sin_ori = support_x * Math.sin(ori);
    	   final double sy_cos_ori = support_y * Math.cos(ori);
    	   double y0_mag = Math.abs(sx_sin_ori - sy_cos_ori);
    	   double y1_mag = Math.abs(sx_sin_ori + sy_cos_ori);
    	   return (y0_mag > y1_mag) ? y0_mag : y1_mag;
    	}
    
    /**
     * Gaussian kernel (1D).
     * The length of the returned vector is 2*support + 1.
     * The kernel is normalized to have unit L1 norm.
     * If returning a 1st or 2nd derivative, the kernel has zero mean
     * @param sigma
     * @param deriv
     * @param hlbrt
     * @param support
     * @return
     */
    private double[] gaussian(double sigma, int deriv, boolean hlbrt, int support) {
    	double m[] = null;
    	int n;
    	int i;
    	// Enlarge support so that hilbert transform can be done efficiently.
    	int support_big = support;
    	if (hlbrt) {
    		support_big = 1;
    		int temp = support;
    		while (temp > 0) {
    			support_big *= 2;
    			temp /= 2;
    		}
    	}
    	// Compute constants
    	final double sigma2_inv = 1.0/(sigma * sigma);
    	final double neg_two_sigma2_inv = -0.5 * sigma2_inv;
    	// Compute gaussian (or gaussian derivative).
    	int size = 2*support_big + 1;
    	m = new double[size];
    	double x = -(double)support_big;
    	if (deriv == 0) {
    		// Compute gaussian
    		for (n = 0; n < size; n++, x++) {
    		    m[n] = Math.exp(x*x*neg_two_sigma2_inv);	
    		}
    	}
    	else if (deriv == 1) {
    		// Compute gaussian first derivative
    		for (n = 0; n < size; n++, x++) {
    			m[n] = Math.exp(x*x*neg_two_sigma2_inv) * (-x);
    		}
    	}
    	else if (deriv == 2) {
    		// Compute gaussian second derivative
    		for (n = 0; n < size; n++, x++) {
    			double x2 = x * x;
    			m[n] = Math.exp(x2 * neg_two_sigma2_inv) * (x2*sigma2_inv - 1);
    		}
    	}
    	else {
    		MipavUtil.displayError("Only derivatives 0, 1, and 2 supported");
    		return null;
    	}
    	// Take hilbert transform (if requested)
    	if (hlbrt) {
    		// Grab power of two sized submatrix (ignore last element)
    		double mtemp[] = new double[m.length-1];
    		for (i = 0; i < m.length-1; i++) {
    			mtemp[i] = m[i];
    		}
    		m = null;
    		m = new double[mtemp.length];
    		for (i = 0; i < mtemp.length; i++) {
    			m[i] = mtemp[i];
    		}
    		mtemp = null;
    		// Grab desired submatrix after hilbert transform
    		int start = support_big - support;
    		int end = start + support + support;
    		int paddedfsamples = MipavMath.findMinimumPowerOfTwo(m.length);
       	    double paddedfy[] = new double[2 * paddedfsamples];
       	    for (i = 0; i < m.length; i++) {
       		    paddedfy[2*i] = m[i];
       	    }
       	    AlgorithmHilbertTransform ht = new AlgorithmHilbertTransform(paddedfy, paddedfsamples);
       	    ht.run();
       	    for (i = 0; i < m.length; i++) {
       		    m[i] = paddedfy[2*i+1];
       	    }
       	    mtemp = new double[end - start + 1];
       	    for (i = start; i <= end; i++) {
       	    	mtemp[i-start] = m[i];
       	    }
       	    m = null;
       	    m = new double[mtemp.length];
       	    for (i = 0; i < mtemp.length; i++) {
       	    	m[i] = mtemp[i];
       	    }
       	    mtemp = null;
    	}
    	double sum;
    	// Make zero mean (if returning derivative)
    	if (deriv > 0) {
    		 sum = 0.0;
    		for (i = 0; i < m.length; i++) {
    			sum += m[i];
    		}
    		double mean = sum/m.length; 
    		for (i = 0; i < m.length; i++) {
    			m[i] = m[i] - mean;
    		}
    	} // if (deriv > 0)
    	// Make unit L1 norm
    	sum = 0.0;
    	for (i = 0; i < m.length; i++) {
    		sum += Math.abs(m[i]);
    	}
    	for (i = 0; i < m.length; i++) {
    		m[i] = m[i]/sum;
    	}
    	return m;
    }
    
    
    


    
}
