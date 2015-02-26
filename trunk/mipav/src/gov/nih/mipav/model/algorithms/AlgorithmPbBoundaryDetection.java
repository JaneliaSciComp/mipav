package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.filters.AlgorithmHilbertTransform;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.MipavMath;
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
        
        // Must read in 286,160 byte MATLAB file unitex_6_1_2_1.4_2_64.mat
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
        double a[][] = null;
        double b[][] = null;
        double c[][] = null;
        
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
            oeFilter(f, sigma, 0.5, 3, theta + Math.PI/2.0);
            fbRun(tg, f, tg);
        } // if (smooth.equals("gaussian"))
        else if (smooth.equals("savgol")) {
        	a = new double[tg.length][tg[0].length];
            fitparab(a, b, c, tg, sigma, sigma/4.0, theta);  
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		tg[y][x] = Math.max(0.0, a[y][x]);
            	}
            }
        } // else if (smooth.equals("savgol"))
    }
    
    private void fitparab(double a[][], double b[][], double c[][], double z[][], double ra, double rb, double theta) {
    	double ira2;
    	double irb2;
    	int wr;
    	double sint;
    	double cost;
    	int h;
    	int w;
    	double d0;
    	double d1;
    	double d2;
    	double d3;
    	double d4;
    	double v0;
    	double v1;
    	double v2;
    	int u;
    	int v;
    	int x;
    	int y;
    	int xi;
    	int yi;
    	double di;
    	double ei;
    	double zi;
    	double di2;
    	double detA;
    	double invA[][];
    	double param[];
    	// Fit cylindrical parabolas to elliptical patches of z at each pixel
    	
    	// Input
    	// z Values to fit
    	// ra, rb Radius of elliptical neighborhood, ra = major axis
    	// theta Orientation of fit (i.e. of minor axis).
    	
    	// Output
    	// a[][], b[][], c[][] Coefficients of fit: a + bx + cx^2
    	
    	ra = Math.max(1.5, ra);
    	rb = Math.max(1.5, rb);
    	ira2 = 1.0/(ra*ra);
    	irb2 = 1.0/(rb*rb);
    	wr = (int)Math.floor(Math.max(ra,rb));
    	sint = Math.sin(theta);
    	cost = Math.cos(theta);
    	
    	// Compute the interior quickly with convolutions
    	savgol(a, z, 2, 1, ra, rb, theta);
    	if (b != null) {
    		savgol(b, z, 2, 2, ra, rb, theta);	
    	}
    	if (c != null) {
    		savgol(c, z, 2, 3, ra, rb, theta);		
    	}
    	
    	// Recompute the border since the convolution screws it up
    	h = z.length;
    	w = z[0].length;
    	for (x = 1; x <= w; x++) {
    		for (y = 1; y <= h; y++) {
    			if ((x > wr) && (x <= w-wr) && (y > wr) && (y <= h-wr)) {
    				continue;
    			}
    			d0 = 0.0;
    			d1 = 0.0;
    			d2 = 0.0;
    			d3 = 0.0;
    			d4 = 0.0;
    			v0 = 0.0;
    			v1 = 0.0;
    			v2 = 0.0;
    			for (u = -wr; u <= wr; u++) {
    				xi = x + u;
    				if ((xi < 1) || (xi > w)) {
    					continue;
    				}
    				for (v = -wr; v <= wr; v++) {
    					yi = y + v;
    					if ((yi < 1) || (yi > h)) {
    						continue;
    					}
    					// Distance along major axis
    					di = -u*sint + v*cost;
    					// Distance along minor axis (at theta)
    					ei = u * cost + v * sint;
    					if (di*di*ira2 + ei*ei*irb2 >1) {
    						continue;
    					}
    					zi = z[yi-1][xi-1];
    					di2 = di*di;
    					d0 = d0 + 1;
    					d1 = d1 + di;
    					d2 = d2 + di2;
    					d3 = d3 + di*di2;
    					d4 = d4 + di2*di2;
    					v0 = v0 + zi;
    					v1 = v1 + zi*di;
    					v2 = v2 + zi*di2;
    				}
    			}
    			
    			// Much faster to do 3x3 matrix inverse manually
    			detA = -d2*d2*d2 + 2*d1*d2*d3 - d0*d3*d3 - d1*d1*d4 + d0*d2*d4;
    			invA = new double[3][3];
    			invA[0][0] = -d3*d3+d2*d4;
    			invA[0][1] = d2*d3-d1*d4;
    			invA[0][2] = -d2*d2+d1*d3;
    			invA[1][0] = d2*d3-d1*d4;
    			invA[1][1] = -d2*d2+d0*d4;
    			invA[1][2] = d1*d2-d0*d3;
    			invA[2][0] = -d2*d2+d1*d3;
    			invA[2][1] = d1*d2-d0*d3;
    			invA[2][2] = -d1*d1+d0*d2;
    			for (y = 0; y < 3; y++) {
    				for (x = 0; x < 3; x++) {
    					invA[y][x] = invA[y][x]/(detA + epsilon);
    				}
    			}
    			param = new double[3];
    			param[0] = invA[0][0]*v0 + invA[0][1]*v1 + invA[0][2]*v2;
    			a[y-1][x-1] = param[0];
    			if (b != null) {
    				param[1] = invA[1][0]*v0 + invA[1][1]*v1 + invA[1][2]*v2;
        			b[y-1][x-1] = param[1];	
    			}
    			if (c != null) {
    				param[2] = invA[2][0]*v0 + invA[2][1]*v1 + invA[2][2]*v2;
        			c[y-1][x-1] = param[2];	
    			}
    		}
    	}
    }
    
    private void savgol(double c[][], double z[][], int d, int k, double ra, double rb, double theta) {
        // Directional 2D Savitsky-Golay filtering with elliptical support.
    	// The computation is done with a convolution, so the boundary of the output will be biased.
    	// The boundary is of size floor(max(ra,rb)).
    	
    	// Input
    	// z Values to fit
    	// d Degree of fit, usually 2 or 4.
    	// k Coefficient to return in [1,d+1], 1 for smoothing.
    	// ra, rb Radius of elliptical neighborhood, ra = major axis.
    	// theta Orientation of fit (1.e. of minor axis).
    	
    	// Output 
    	// c[0] COefficient of fit
    	double ira2;
    	double irb2;
    	int wr;
    	int wd;
    	double sint;
    	double cost;
    	double filt[][][];
    	double filtk[][];
    	double xx[];
    	int u;
    	int v;
    	double ai;
    	double bi;
    	double A[][];
    	double yy[][];
    	int i;
    	int j;
    	Matrix matA;
    	Matrix matyy;
    	double prod[][];
    	int x;
    	int y;
    	
    	if (d < 0) {
    		MipavUtil.displayError("d = " + d + " is invalid in savgol");
    		return;
    	}
    	if ((k < 1) || ( k > d+1)) {
    		MipavUtil.displayError("k = " + k + " is invalid in savgol");
    		return;
    	}
    	
    	ra = Math.max(1.5, ra);
    	rb = Math.max(1.5, rb);
    	ira2 = 1.0/(ra*ra);
    	irb2 = 1.0/(rb*rb);
    	wr = (int)Math.floor(Math.max(ra,rb));
    	wd = 2*wr+1;
    	sint = Math.sin(theta);
    	cost = Math.cos(theta);
    	
    	// 1. Compute linear filters for coefficients
    	// (a) Compute inverse of least-squares problem matrix
    	filt = new double[wd][wd][d+1];
    	xx = new double[2*d+1];
    	for (u = -wr; u <= wr; u++) {
    		for (v = -wr; v <= wr; v++) {
    			// Distance along major axis
    			ai = -u*sint + v*cost;
    			// Distance along minor axis
    			bi = u*cost + v*sint;
    			if (ai*ai*ira2 + bi*bi*irb2 > 1) {
    				continue;
    			}
    			xx[0] = xx[0] + 1;
    			for (i = 1; i <= 2*d; i++) {
    			    xx[i] = xx[i] + Math.pow(ai,i);	
    			}
    		}
    	}
    	A = new double[d+1][d+1];
    	for (i = 0; i <= d; i++) {
    		for (j = i; j <= i+d; j++) {
    		    A[j][i] = xx[j];	
    		}
    	}
    	matA = new Matrix(A);
    	A = (matA.inverse()).getArray();
    	// (b) solve least-squares problem for delta function at each pixel
    	for (u = -wr; u <= wr; u++) {
    		for (v = -wr; v <= wr; v++) {
    		    yy = new double[d+1][1];
    		    // Distance along major axis
    		    ai = -u*sint + v*cost;
    		    // Distance along minor axis
    		    bi = u*cost + v*sint;
    		    if (ai*ai*ira2 + bi*bi*irb2 > 1) {
    		    	continue;
    		    }
		    	yy[0][0] = 1;
		    	for (i = 1; i <= d; i++) {
		    		yy[i][0] = Math.pow(ai,i); 
		    	}
		    	matA = new Matrix(A);
		    	matyy = new Matrix(yy);
		    	prod = (matA.times(matyy)).getArray();
		    	for (i = 0; i < d+1; i++) {
		    		filt[v+wr][u+wr][i] = prod[i][0];
		    	}
    		}
    	}
    	// 2. Apply the filter to get te fit coefficient at each pixel
    	filtk = new double[wd][wd];
    	for (y = 0; y < wd; y++) {
    		for (x = 0; x < wd; x++) {
    			filtk[y][x] = filt[y][x][k-1];
    		}
    	}
    	conv2(z, filtk, c);
    }
    
    private void fbRun(double fim[][], double fb[][], double im[][]) {
        // Run a filterbank on an image with reflected boundary conditions
    	int maxsz;
    	int r;
    	double impad[][];
    	double fimpad[][];
    	int y;
    	int x;
    	maxsz = Math.max(fb.length, fb[0].length);
    	
    	// Pad the image
    	r = (int)Math.floor(maxsz/2);
    	impad = new double[im.length+2*r][im[0].length + 2*r];
    	padReflect(impad, im, r);
    	fimpad = new double[impad.length][impad[0].length];
    	if (fb.length < 50) {
    	    conv2(impad,fb,fimpad);	
    	}
    	else {
    	    fftconv2(fimpad, impad, fb);	
    	}
    	for (y = r; y < impad.length - r; y++) {
    	    for (x = r; x < impad[0].length - r; x++) {
    	    	fim[y-r][x-r] = fimpad[y][x];
    	    }
    	}
    }
    
    private void fftconv2(double fim[][], double im[][], double f[][]) {
    	double padf[][];
    	int r;
    	int y;
    	int x;
    	FFTUtility fft;
    	int i;
    	// Convolution using fft
    	
    	// Wrap the filter around the origin and pad with zeros
    	padf = new double[im.length][im[0].length];
    	r = (int)Math.floor(f.length/2);
    	for (y = r; y < f.length; y++) {
    		for (x = r; x < f[0].length; x++) {
    			padf[y-r][x-r] = f[y][x];
    		}
    	}
    	for (y = r+1; y < f.length; y++) {
    		for (x = 0; x < r; x++) {
    			padf[y - r - 1][im[0].length - r + x] = f[y][x];
    		}
    	}
    	for (y = 0; y < r; y++) {
    		for (x = r+1; x < f[0].length; x++) {
    			padf[im.length - r + y][x - r - 1] = f[y][x];
    		}
    	}
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < r; x++) {
    			padf[im.length - r + y][im[0].length - r + x] = f[y][x];
    		}
    	}
    	
    	// Magic
    	double imFFT[] = new double[im.length * im[0].length];
    	double imFFTImag[] = new double[im.length * im[0].length];
    	double padfFFT[] = new double[im.length * im[0].length];
    	double padfFFTImag[] = new double[im.length * im[0].length];
    	double prod[] = new double[im.length * im[0].length];
    	double prodImag[] = new double[im.length * im[0].length];
    	for (y = 0; y < im.length; y++) {
    		for (x = 0; x < im[0].length; x++) {
    			imFFT[x + y * im[0].length] = im[y][x];
    			padfFFT[x + y * im[0].length] = padf[y][x];
    		}
    	}
    	fft = new FFTUtility(imFFT, imFFTImag, im.length, im[0].length, 1,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(imFFT, imFFTImag, 1, im.length, im[0].length,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		fft = new FFTUtility(padfFFT, padfFFTImag, im.length, im[0].length, 1,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(padfFFT, padfFFTImag, 1, im.length, im[0].length,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		for (i = 0; i < im.length * im[0].length; i++) {
			prod[i] = imFFT[i]*padfFFT[i] - imFFTImag[i]*padfFFTImag[i];
			prodImag[i] = imFFT[i]*padfFFTImag[i] + imFFTImag[i]*padfFFT[i];
		}
		// Inverse fft
		fft = new FFTUtility(prod, prodImag, im.length, im[0].length, 1,
				1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(prod, prodImag, 1, im.length, im[0].length,
				1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		for (y = 0; y < im.length; y++) {
			for (x = 0; x < im[0].length; x++) {
				fim[y][x] = prod[x + y * im[0].length];
			}
		}
		return;
    }
    
    private void padReflect(double impad[][], double im[][], int r) {
    	// Pad an image with a border of size r, and reflect the image into the border
    	int x;
    	int y;
    	// Middle
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[y + r][x + r] = im[y][x];
    		}
    	}
    	// Top
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[r - y - 1][x + r] = im[y][x];
    		}
    	}
    	// Bottom
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[2*yDim + r - y - 1][x + r] = im[y][x];
    		}
    	}
    	// Left
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < r; x++) {
    			impad[y+r][r - x - 1] = im[y][x];
    		}
    	}
    	// Right
    	for (y = 0; y < yDim; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[y+r][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    	// Top-left
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < r; x++) {
    			impad[r - y - 1][r - x - 1] = im[y][x];
    		}
    	}
    	// Top-right
    	for (y = 0; y < r; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[r - y - 1][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    	// Bottom-left
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = 0; x < r; x++) {
    			impad[2*yDim + r - y - 1][r - x - 1] = im[y][x];
    		}
    	}
    	// Bottom-right
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[2*yDim + r - y - 1][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
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
     * @param f output [sz][sz] square filter
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
    	double fprecursor[][];
    	int v;
    	double fsum;
    	double fmean;
    	double fsumabs;
    	int paddedfsamples;
    	double paddedfy[];
    	AlgorithmHilbertTransform ht;
    
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
         membership = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        	     membership[y][x] = (mx[y][x] + hsz + 1) + (my[y][x]+ hsz)*sz;	 
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
        	 paddedfsamples = MipavMath.findMinimumPowerOfTwo(fsamples);
        	 paddedfy = new double[2 * paddedfsamples];
        	 for (i = 0; i < fsamples; i++) {
        		 paddedfy[2*i] = fy[i];
        	 }
        	 ht = new AlgorithmHilbertTransform(paddedfy, paddedfsamples);
        	 ht.run();
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = paddedfy[2*i+1];
        	 }
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
         fprecursor = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        	     fprecursor[y][x] = fx[xi[y][x]] * fy[yi[y][x]]; 
        	 }
         }
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 v = membership[y][x];
        		 if (v < 1) {
        			 continue;
        		 }
        		 if (v > sz*sz) {
        			 continue;
        		 }
        		 f[(v-1)/sz][(v-1)%sz] += fprecursor[y][x];
        	 }
         }
         
         // Zero mean
         if (deriv > 0) {
	         fsum = 0.0;
	         for (y = 0; y < sz; y++) {
	        	 for (x = 0; x < sz; x++) {
	        	     fsum += f[y][x];	 
	        	 }
	         }
	         fmean = fsum/(sz*sz);
	         for (y = 0; y < sz; y++) {
	        	 for (x = 0; x < sz; x++) {
	        		 f[y][x] -= fmean;
	        	 }
	         }
         } // if (deriv > 0)
         
         // Unit L1-norm
         fsumabs = 0.0;
         for (y = 0; y < sz; y++) {
        	 for (x = 0; x < sz; x++) {
        		 fsumabs += Math.abs(f[y][x]);
        	 }
         }
         if (fsumabs > 0.0) {
        	 for (y = 0; y < sz; y++) {
        		 for (x = 0; x < sz; x++) {
        			 f[y][x] = f[y][x]/fsumabs;
        		 }
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