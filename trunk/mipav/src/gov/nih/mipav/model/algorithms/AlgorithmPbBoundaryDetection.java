package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

import Jama.Matrix;

/**
 * Compute probability of boundary using brightness gradient and texture gradient
 * Original MATLAB code written by David R. Martin in April 2003
 * 
 * Reference: "Learning to Detect Natural Image Boundaries Using Local Brightness, Color,
 *             and Texture Cues" by David R. Martin, Charless C. Fowlkes, and Jitendra
 *             Malik, IEEE Transactions on Pattern Analysis and Machine Intelligence,
 *             Vol. 26, No. 5, May, 2004, pp. 530-549.
 * @author ilb
 *
 */

public class AlgorithmPbBoundaryDetection extends AlgorithmBase {
	private static final int BGTG = 1;
	
	private int gradientType = BGTG;
	
	private static final int GRAY_PRESENTATION = 1;
	
	private static final int COLOR_PRESENTATION = 2;
	
	private int presentation = GRAY_PRESENTATION;
	
	private double lowRadius = 0.01;
	
	private double highRadius = 0.02;
	
	private int numOrientations = 8;
	
	private int xDim;
	
	private int yDim;
	
	// epsilon = D1MACH(4)
	// Machine epsilon is the smallest positive epsilon such that
	// (1.0 + epsilon) != 1.0.
	// epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
	// epsilon = 2.2204460e-16
	// epsilon is called the largest relative spacing
	private final double epsilon = Math.pow(2.0, -52);
	
	
	 //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmPbBoundaryDetection - default constructor.
     */
    public AlgorithmPbBoundaryDetection() { }
    
    /**
     * AlgorithmPbBoundaryDetection.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  gradientType
       @param  presentation
       @param  lowRadius
       @param  highRadius
       @param  numOrientations
     */
    public AlgorithmPbBoundaryDetection(ModelImage destImg, ModelImage srcImg, int gradientType, int presentation, double lowRadius,
                                       double highRadius, int numOrientations) {
        super(destImg, srcImg);
        this.gradientType = gradientType;
        this.presentation = presentation;
        this.lowRadius = lowRadius;
        this.highRadius = highRadius;
        this.numOrientations = numOrientations;   
    }
    
    public void runAlgorithm() {
    	double pb[][];
    	double theta[][];
    	 xDim = srcImage.getExtents()[0];
         yDim = srcImage.getExtents()[1];
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Pb Boundary Detection ...");
        
        if (gradientType == BGTG) {
        	pb = new double[yDim][xDim];
        	theta = new double[yDim][xDim];
        	pbBGTG(pb, theta);
        }
    }
    
    
      private void pbBGTG(double pb[][], double theta[][]) {
      	double beta[] = new double[3];
      	double fstd[] = new double[3];
      	double diag;
      	ModelImage grayImage = null;
      	ModelImage inputImage;
      	double bg[][][];
      	double gtheta[];
      	AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
        
        // beta from logistic fits (trainBGTG.m)
        if ((lowRadius == 0.01) && (highRadius == 0.02)) {
        	// 64 textons
        	if (presentation == GRAY_PRESENTATION) {
        		// trained on grayscale segmentations
        	    beta[0] = -4.6522915;
        	    beta[1] = 0.71345115;
        	    beta[2] = 0.70333326;
        	    fstd[0] = 1.0;
        	    fstd[1] = 0.37408935;
        	    fstd[2] = 0.19171689;
        	}
        	else if (presentation == COLOR_PRESENTATION) {
        		// trained on color segmentations
        		beta[0] = -4.4880396;
        		beta[1] = 0.70690368;
        		beta[2] = 0.65740193;
        		fstd[0] = 1.0;
        		fstd[1] = 0.37401028;
        		fstd[2] = 0.19181055;
        	}
        	else {
        		MipavUtil.displayError("Unknown presentation");
        		setCompleted(false);
        		return;
        	}
        	beta[0] = beta[0]/fstd[0];
        	beta[1] = beta[1]/fstd[1];
        	beta[2] = beta[2]/fstd[2];
        } // if ((lowRadius == 0.01) && (highRadius == 0.02))
        else {
        	MipavUtil.displayError("No parameters for lowRadius = " + lowRadius + " highRadius = " + highRadius);
        	setCompleted(false);
        	return;
        }
        
        // Get gradients
        // Compute smoothed but not thinned BG and TG fields
        diag = Math.sqrt(xDim*xDim + yDim*yDim);
        if (srcImage.isColorImage()) {
			final boolean thresholdAverage = false;
			final float threshold = 0.0f;
			final boolean intensityAverage = false;
			final boolean equalRange = true;
			final float minR = 0.0f;
			final float minG = 0.0f;
			final float minB = 0.0f;
			float redValue;
			float greenValue;
			float blueValue;
			float maxR;
			float maxG;
			float maxB;
			AlgorithmRGBtoGray gAlgo;
			if (srcImage.getMinR() == srcImage.getMaxR()) {
				redValue = 0.0f;
				greenValue = 0.5f;
				blueValue = 0.5f;
			} else if (srcImage.getMinG() == srcImage.getMaxG()) {
				redValue = 0.5f;
				greenValue = 0.0f;
				blueValue = 0.5f;
			} else if (srcImage.getMinB() == srcImage.getMaxB()) {
				redValue = 0.5f;
				greenValue = 0.5f;
				blueValue = 0.0f;
			} else {
				redValue = (float) (1.0 / 3.0);
				greenValue = redValue;
				blueValue = redValue;

			}
			maxR = (float) srcImage.getMaxR();
			maxG = (float) srcImage.getMaxG();
			maxB = (float) srcImage.getMaxB();
			grayImage = new ModelImage(ModelStorageBase.DOUBLE,
					srcImage.getExtents(), "grayImage");
			gAlgo = new AlgorithmRGBtoGray(grayImage, srcImage,
					redValue, greenValue, blueValue, thresholdAverage,
					threshold, intensityAverage, equalRange, minR, maxR,
					minG, maxG, minB, maxB);
			gAlgo.run();
			gAlgo.finalize();
		} // if (srcImage.isColorImage())
        
        inputImage = new ModelImage(ModelStorageBase.DOUBLE,
				srcImage.getExtents(), "changeTypeImage");
		inputImage.getFileInfo(0).setEndianess(FileBase.LITTLE_ENDIAN);
		if (srcImage.isColorImage()) {
			changeTypeAlgo = new AlgorithmChangeType(inputImage,
					grayImage, grayImage.getMin(), grayImage.getMax(),
					0.0, 1.0, image25D);
		} else {
			changeTypeAlgo = new AlgorithmChangeType(inputImage,
					srcImage, srcImage.getMin(),
					srcImage.getMax(), 0.0, 1.0, image25D);
		}
		changeTypeAlgo.run();
		changeTypeAlgo.finalize();
		changeTypeAlgo = null;
		if (grayImage != null) {
			grayImage.disposeLocal();
			grayImage = null;
		}
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
        // Compute brightness gradient
        bg = new double[yDim][xDim][numOrientations];
        gtheta = new double[numOrientations];
        cgmo(bg, gtheta, inputImage, diag*lowRadius, numOrientations, "savgol", diag*lowRadius);
    }
      
    private void cgmo(double cg[][][], double theta[], ModelImage image, double radius, int numOrientations,
    		String smooth, double sigmaSmooth) {
        int nbins = 32;
        double sigmaSim = 0.1;
        double gamma = 2.5;
        cgmo(cg, theta, image, radius, numOrientations, nbins, sigmaSim, gamma, smooth, sigmaSmooth);
    }
    
    /**
     * Do a separate cgmoColor
     * Compute the color gradient at a single scale and multiple orientations
     * @param cg output [yDim][xDim][1][numOrientations] array for black and white 
     *           output [yDim][xDim][3][numOrientations] array for color
     * @param output [numOrientations] theta
     * @param image  Grayscale or RGB image, values in [0, 1].
     * @param radius Radius of disc for cg array
     * @param numOrientations Number of orientations for cg array
     * @param nbins Number of bins; should be > 1/sigmaSim.
     * @param sigmaSim For color similarity function
     * @param gamma Gamma correction for LAB [2.5].
     * @param smooth Smoothing method, one of {"gaussian", "savgol", "none"}, default none
     * @param sigmaSmooth Sigma for smoothing, default to radius
     */
    private void cgmo(double cg[][][], double theta[],
    		ModelImage image, double radius, int numOrientations, int nbins, double sigmaSim, double gamma, 
    		String smooth, double sigmaSmooth) {
        double abmin;
        double abmax;
        int cmap[][];
        int y;
        int x;
        double buffer[];
        int sliceSize = xDim * yDim;
        double bc[];
        int i;
        double xArr[][];
        double yArr[][];
        double csim[][];
        double diff;
        double denom;
        
        // Min and max values used for a,b channels of LAB
        // Used to scale values into the unit interval
        abmin = -73;
        abmax = 95;
        
        // Make sure bin is large enough with respect to sigmaSim
        if (nbins < 1.0/sigmaSim) {
        	MipavUtil.displayWarning("nbins < 1/sigmaSim is suspect");
        }
        
        if (image.getMin() < 0.0 || image.getMax() > 1.0) {
        	MipavUtil.displayError("Pixel values out of range 0 to 1");
        	return;
        }
        
        // Compute cg from gray values
        cmap = new int[yDim][xDim];
        buffer = new double[sliceSize];
        try {
        	image.exportData(0, sliceSize, buffer);
        }
        catch(IOException e) {
        	e.printStackTrace();
        }
        
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		cmap[y][x] = Math.max(1, (int)Math.ceil(buffer[x + y * xDim] * nbins));
        	}
        }
        
        // Compute color similarity matrix assuming colors are in [0, 1]
        bc = new double[nbins];
        for (i = 0; i < nbins; i++) {
        	// Calculate bin centers
        	bc[i] = (i + 0.5)/nbins;
        }
        xArr = new double[nbins][nbins];
        yArr = new double[nbins][nbins];
        for (y = 0; y <  nbins; y++) {
        	for (x = 0; x < nbins; x++) {
        		xArr[y][x] = bc[x];
        		yArr[y][x] = bc[y];
        	}
        }
        csim = new double[nbins][nbins];
        denom = 2.0 * sigmaSim * sigmaSim;
        for (y = 0; y < nbins; y++) {
        	for (x = 0; x < nbins; x++) {
        		diff = xArr[y][x] - yArr[y][x];
        		csim[y][x] = 1.0 - Math.exp(-diff*diff/denom);
        	}
        }
        tgmo(cg, theta, cmap, nbins, radius, numOrientations, csim, smooth, sigmaSmooth);
    }
    
    /**
     * Compute the texture gradient at a single scale and multiple orientations
     * @param output tg  [yDim][xDim][numOrientations] array
     * @param output [numOrientations] theta Disc orientations (which are orthogonal to the texture gradient). 
     * @param tmap [yDim][xDim] Texton map, values in [1, ntex]
     * @param ntext Number of textons
     * @param radius Radius of disc for texture gradient
     * @param numOrientations Number of orientations at which to compute the texture graident
     * @param tsim Texton similarity matrix.  If not provided, then use chi-squared.
     * @param smooth Smoothing method.  One of "gaussian", "savgol", "none".  Default "none".
     * @param sigma Sigma for smoothing.  Default to radius.
     */
    private void tgmo(double tg[][][], double theta[], int tmap[][], int ntex, double radius, int numOrientations, 
    		double tsim[][], String smooth, double sigma) {
    	int i;
    	boolean usechi2;
    	double tgArray[][];
    	int y;
    	int x;
    	
    	if (tsim != null) {
    		usechi2 = false;
    	}
    	else {
    		usechi2 = true;
    	}
    	
    	radius = Math.max(1.0,  radius);
    	numOrientations = Math.max(1, numOrientations);
    	for (i = 0; i < numOrientations; i++) {
    		theta[i] = ((double)i)/numOrientations*Math.PI;
    	}
    	tgArray = new double[yDim][xDim];
    	for (i = 0; i < numOrientations; i++) {
    		if (usechi2) {
    		    tgso(tgArray, tmap, ntex, radius, theta[i], smooth, sigma, null);	
    		}
    		else {
    			tgso(tgArray, tmap, ntex, radius, theta[i], smooth, sigma, tsim);
    		}
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				tg[y][x][i] = tgArray[y][x];
    			}
    		}
    	} // for (i = 0; i < numOrientations; i++)
    }
    
    /**
     * Compute the texture graident at a single orientation and scale
     * @param output tg [yDim][xDim] array
     * @param tmap [yDim][xDim] Texton map, values in [1, ntex].
     * @param ntex Number of textons
     * @param radius Radius of disc for tg
     * @param theta Orientation orthogonal to tg.
     * @param smooth Smoothing method, one of {"gaussian", "savgol", "none"}, default = "none".
     * @param sigma Sigma for smoothing.  Default to radius.
     * @param tsim [ntex][ntex] Texton similarity matrix.  If not provided, then use chi-squared.
     */
    private void tgso(double tg[][], int tmap[][], int ntex, double radius, double theta, String smooth, double sigma, double tsim[][]) {
        boolean usechi2;
        int y;
        int x;
        int wr;
        double xgrid[][];
        double ygrid[][];
        double gamma[][];
        byte mask[][];
        int count;
        byte side[][];
        int sum1;
        int sum2;
        double lmask[][];
        double rmask[][];
        int i;
        double im[][];
        double tgL[][];
        double tgR[][];
        double diff;
        double d[][];
        Matrix dMat;
        Matrix tsimMat;
        double dtsim[][];
        int hsz;
        int sz;
        double f[][];
        
        if (tsim != null) {
    		usechi2 = false;
    	}
    	else {
    		usechi2 = true;
    	}
        
        radius = Math.max(1.0,  radius);
        theta = theta % Math.PI;
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		if ((tmap[y][x] < 1) || (tmap[y][x] > ntex)) {
        			MipavUtil.displayError("texton label["+y+"]["+x+"] = " + tmap[y][x] + " is out of range");
        			return;
        		}
        	}
        }
        
        // Radius of discrete disc
        wr = (int)Math.floor(radius);
        
        // Count number of pixels in a disc
        xgrid = new double[2*wr+1][2*wr+1];
        ygrid = new double[2*wr+1][2*wr+1];
        for (y = -wr; y <= wr; y++) {
        	for (x = -wr; x <= wr; x++) {
        		ygrid[y+wr][x+wr] = y;
        		xgrid[y+wr][x+wr] = x;
        	}
        }
        gamma = new double[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; y++) {
        		gamma[y][x] = Math.atan2(ygrid[y][x], xgrid[y][x]);
        	}
        }
        mask = new byte[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; x++) {
        		if (xgrid[y][x]*xgrid[y][x] + ygrid[y][x]*ygrid[y][x] <= radius*radius) {
        			mask[y][x] = 1;
        		}
        	}
        }
        // Mask ot center pixel to remove bias
        mask[wr][wr] = 0;
        count = 0;
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; x++) {
        	    count += mask[y][x];	
        	}
        }
        
        // Determine which half of the disc pixels fall in
        // (0 = masked 1 = left 2 = right)
        sum1 = 0;
        sum2 = 0;
        side = new byte[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; x++) {
        		if (((gamma[y][x] - theta)%(2.0 * Math.PI)) < Math.PI) {
        			side[y][x] = (byte)(2 *mask[y][x]);
        			if (side[y][x] == 2) {
        				sum2++;
        			}
        		}
        		else {
        			side[y][x] = mask[y][x];
        			if (side[y][x] == 1) {
        				sum1++;
        			}
        		}
        	}
        } // for (y = 0; y < 2*wr+1; y++)
        if (sum1 != sum2) {
        	MipavUtil.displayError("Sum imbalance in tgso sum1 = " + sum1 + " sum2 = " + sum2);
        	return;
        }
        lmask = new double[2*wr+1][2*wr+1];
        rmask = new double[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr + 1; x++) {
        		if (side[y][x] == 1) {
        			lmask[y][x] = 1.0/count * 2;
        		}
        		else if (side[y][x] == 2) {
        			rmask[y][x] = 1.0/count * 2;
        		}
        	}
        }
        
        // Compute tg using 2*ntex convolutions
        im = new double[yDim][xDim];
        tgL = new double[yDim][xDim];
        tgR = new double[yDim][xDim];
        if (usechi2) {
            for (i = 1; i <= ntex; i++) {
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            	        if (tmap[y][x] == i) {
            	        	im[y][x] = 1.0;
            	        }
            	        else {
            	        	im[y][x] = 0.0;
            	        }
            		}
            	} // for (y = 0; y < yDim; y++)
            	conv2(im, lmask, tgL);
            	conv2(im, rmask, tgR);
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            			diff = tgL[y][x] - tgR[y][x];
            			tg[y][x] = tg[y][x] + diff*diff/(tgL[y][x] + tgR[y][x] + epsilon);
            		}
            	}
            } // for ( i = 1; i <= ntex; i++)
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			tg[y][x] = 0.5 * tg[y][x];
        		}
            }
        } // if (usechi2)
        else { // !usechi2
            d = new double[yDim*xDim][ntex];
            for (i = 1; i <= ntex; i++) {
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            	        if (tmap[y][x] == i) {
            	        	im[y][x] = 1.0;
            	        }
            	        else {
            	        	im[y][x] = 0.0;
            	        }
            		}
            	} // for (y = 0; y < yDim; y++)
            	conv2(im, lmask, tgL);
            	conv2(im, rmask, tgR);
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            			d[x + y * xDim][i] = Math.abs(tgL[y][x] - tgR[y][x]);
            		}
            	}
            } // for ( i = 1; i <= ntex; i++)
            dMat = new Matrix(d);
            tsimMat = new Matrix(tsim);
            dtsim = (dMat.times(tsimMat)).getArray();
            for (y = 0; y < xDim * yDim; y++) {
            	for (x = 0; x < ntex; x++) {
            		d[y][x] = dtsim[y][x] * d[y][x];
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		tg[y][x] = 0.0;
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		for (i = 0; i < ntex; i++) {
            			tg[y][x] = tg[y][x] + d[x + y * xDim][i];
            		}
            	}
            }
        } // else !usechi2
        
        if (smooth.equals("gaussian")) {
            hsz = (int)Math.max(Math.ceil(sigma * 3), Math.ceil(0.5 * 3));
            sz = 2 * hsz + 1;
            f = new double[sz][sz];
            oeFilter(f, sigma, 0.5, 3, theta + Math.PI/2.0)	;
        } // if (smooth.equals("gaussian"))
        else if (smooth.equals("savgol")) {
           
        } // else if (smooth.equals("savgol"))
    }
    
    private void oeFilter(double f[][], double sigmaX, double sigmaY, int support, double theta) {
       int deriv = 0;
       boolean dohil = false;
       boolean dovis = false;
       oeFilter(f, sigmaX, sigmaY, support, theta, deriv, dohil, dovis);
    }
    
    /**
     * Compute unit L1- norm 2D filter.
     * The filter is a Gaussian in the x direction
     * The filter is a Gaussian derivative with optional Hilbert transform in the y direction.
     * The filter is zero-meaned if deriv > 0.
     * @param f ouput square filter
     * @param sigmaX
     * @param sigmaY
     * @param support Make filter +/- this many sigma
     * @param theta Orientation of x axis, in radians
     * @param deriv Degree of y derivative, one of {0, 1, 2}.
     * @param dohil Do Hilbert transform in y direction?
     * @param dovis Visualization for debugging?
     */
    private void oeFilter(double f[][], double sigmaX, double sigmaY, int support, double theta,
    		int deriv, boolean dohil, boolean dovis) {
    	int hsz;
    	int sz;
    	int maxsamples;
    	int maxrate;
    	int frate;
    	int rate;
    	int samples;
    	double r;
    	double dom[];
    	double stepSize;
    	int i;
    	double sx[][];
    	double sy[][];
    	int x;
    	int y;
    	int mx[][];
    	int my[][];
    	int membership[][];
    	double su[][];
    	double sv[][];
    	double R;
    	int fsamples;
    	double fdom[];
    	double gap;
    	double fx[];
    	double fy[];
    	double denom;
    	int xi[][];
    	int yi[][];
    
    	if ((deriv < 0) || (deriv > 2)) {
    		MipavUtil.displayError("deriv = " + deriv + "in oeFilter");
    		return;
    	}
    	
    	// Calculate filter size, make sure it's odd
    	 hsz = (int)Math.max(Math.ceil(sigmaX * support), Math.ceil(sigmaY * support));
         sz = 2 * hsz + 1;
         
         // Sampling limits
         // Max samples in each dimension
         maxsamples = 1000;
         // Maximum sampling rate
         maxrate = 10;
         // Over-sampling rate for function evaluation
         frate = 10;
         
         // Calculate sampling rate and number of samples
         rate = (int)Math.min(maxrate,  Math.max(1, Math.floor(maxsamples/sz)));
         samples = sz * rate;
         
         // The 2D sampling grid
         r = Math.floor(sz/2.0) + 0.5 * (1.0 - 1.0/rate);
         dom = new double[samples];
         dom[0] = -r;
         dom[samples-1] = r;
         stepSize = (2.0*r)/(samples - 1.0);
         for (i = 1; i < samples-1; i++) {
             dom[i] = -r + i * stepSize;	 
         }
         sx = new double[samples][samples];
         sy = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 sx[y][x] = dom[x];
        		 sy[y][x] = dom[y];
        	 }
         }
         mx = new int[samples][samples];
         my = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 mx[y][x] = (int)Math.round(sx[y][x]);
        		 my[y][x] = (int)Math.round(sy[y][x]);
        	 }
         }
         membership = new int[sz][sz];
         for (y = 0; y < sz; y++) {
        	 for (x = 0; x < sz; x++) {
        	     membership[y][x] = (mx[y][x] + hsz) + (my[y][x]+ hsz)*sz;	 
        	 }
         }
         
         // Rotate the 2D sampling grid by theta
         su = new double[samples][samples];
         sv = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 su[y][x] = sx[y][x]*Math.sin(theta) + sy[y][x]*Math.cos(theta);
        		 sv[y][x] = sx[y][x]*Math.cos(theta) - sy[y][x]*Math.sin(theta);
        	 }
         }
         
         if (dovis) {
        	 
         } // if (dovis)
         
         // Evaluate the function separably on a finer grid
         // Radius of domain, enlarged by > sqrt(2)
         R = r * Math.sqrt(2.0) * 1.01;
         // Number of samples
         fsamples = (int)Math.ceil(R * rate * frate);
         // Must be odd
         fsamples = fsamples + ((fsamples+1)%2);
         // Domain for function evaluation
         fdom = new double[fsamples];
         fdom[0] = -R;
         fdom[fsamples-1] = R;
         // Distance between samples
         gap = (2.0*R)/(fsamples - 1.0);
         for (i = 1; i < fsamples-1; i++) {
             fdom[i] = -R + i * gap;	 
         } 
         
         // The function is a Gaussian in the x direction
         fx = new double[fsamples];
         denom = 2.0*sigmaX*sigmaX;
         for (i = 0; i < fsamples; i++) {
        	 fx[i] = Math.exp(-fdom[i]*fdom[i]/denom);
         }
         // .. and a Gaussian derivative in the y direction
         fy = new double[fsamples];
         denom = 2.0*sigmaY*sigmaY;
         for (i = 0; i < fsamples; i++) {
        	 fy[i] = Math.exp(-fdom[i]*fdom[i]/denom);
         }
         switch (deriv) {
         case 1:
        	 denom = sigmaY*sigmaY;
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = fy[i] * (-fdom[i]/denom);
        	 }
        	 break;
         case 2:
        	 denom = sigmaY *sigmaY;
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = fy[i] *(fdom[i]*fdom[i]/denom - 1.0);
        	 }
         } // switch(deriv)
         // an optional Hilbert transform
         if (dohil) {
        	 
         }
         
         // Evaluate the function with NN interpolation
         xi = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 xi[y][x] =(int) Math.round(su[y][x]/gap) + (int)Math.floor(fsamples/2) + 1;
        	 }
         }
         yi = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 yi[y][x] =(int) Math.round(sv[y][x]/gap) + (int)Math.floor(fsamples/2) + 1;
        	 }
         }
    }
    
    private void conv2(double A[][], double B[][], double Cout[][]) {
		double L[][];
		double S[][];
		int ml;
		int nl;
		int ms;
		int ns;
		int y;
		int x;
		int y2;
		int x2;
		double C[][];
		double small;
		int yoff;
		int xoff;
		if (A.length * A[0].length >= B.length * B[0].length) {
			ml = A.length;
			nl = A[0].length;
			L = A;
			ms = B.length;
			ns = B[0].length;
			S = B;
		} else {
			ml = B.length;
			nl = B[0].length;
			L = B;
			ms = A.length;
			ns = A[0].length;
			S = A;
		}
		C = new double[ml + ms - 1][nl + ns - 1];
		for (y = 0; y < ms; y++) {
			for (x = 0; x < ns; x++) {
				small = S[y][x];
				if (small != 0.0) {
					for (y2 = 0; y2 < ml; y2++) {
						for (x2 = 0; x2 < nl; x2++) {
							C[y + y2][x + x2] += L[y2][x2] * small;
						}
					}
				}
			}
		}
		yoff = (int) Math.floor(B.length / 2.0);
		xoff = (int) Math.floor(B[0].length / 2.0);
		for (y = 0; y < A.length; y++) {
			for (x = 0; x < A[0].length; x++) {
				Cout[y][x] = C[y + yoff][x + xoff];
			}
		}
		return;
	}
    
    
}