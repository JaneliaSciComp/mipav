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
import java.net.URISyntaxException;
import java.net.URL;

import Jama.Matrix;

/**
 * Compute probability of boundary using brightness gradient and texture gradient
 * Original MATLAB code written by David R. Martin in April 2003
 * 
 * Reference: "Learning to Detect Natural Image Boundaries Using Local Brightness, Color,
 *             and Texture Cues" by David R. Martin, Charles C. Fowlkes, and Jitendra
 *             Malik, IEEE Transactions on Pattern Analysis and Machine Intelligence,
 *             Vol. 26, No. 5, May, 2004, pp. 530-549.
 * @author ilb
 *
 */

public class AlgorithmPbBoundaryDetection extends AlgorithmBase {
	private static final int BGTG = 1;
	
	private static final int CGTG = 2;
	
	private static final int BG = 3;
	
	private static final int CG = 4;
	
	private static final int TG = 5;
	
	private static final int GM = 6;
	
	private static final int GM2 = 7;
	
	private static final int TWOMM = 8;
	
	private static final int TWOMM2 = 9;
	
	private static final int CANNY = 10;
	
	private int gradientType = BG;
	
	private static final int GRAY_PRESENTATION = 1;
	
	private static final int COLOR_PRESENTATION = 2;
	
	private int presentation = GRAY_PRESENTATION;
	
	private double lowRadius = 0.01;
	
	private double highRadius = 0.02;
	
	private int numOrientations = 8;
	
	// smooth is "savgol", "gaussian", or "none".
	private String smooth = "savgol";
	
	private double sigma = 2.0;
	
	// Resolution for pb
	private int nthresh = 100;
	
	// Multiplier for lower hysteresis threshold, in [0, 1].
	private double hmult = 1.0/3.0;
	
	private int xDim;
	
	private int yDim;
	
	private RandomAccessFile raFile;
	
	/** byte array for short * */
    private final byte[] byteShortBuffer = new byte[2];
    
    /** byte array for int * */
    private final byte[] byteIntBuffer = new byte[4];
	
	 /** byte array for long * */
    private final byte[] byteLongBuffer = new byte[8];
	
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
       @param  smooth
       @param  sigma
     */
    public AlgorithmPbBoundaryDetection(ModelImage destImg, ModelImage srcImg, int gradientType, int presentation, double lowRadius,
                                       double highRadius, int numOrientations, String smooth, double sigma,
                                       int nthresh, double hmult) {
        super(destImg, srcImg);
        this.gradientType = gradientType;
        this.presentation = presentation;
        this.lowRadius = lowRadius;
        this.highRadius = highRadius;
        this.numOrientations = numOrientations; 
        this.smooth = smooth;
        this.sigma = sigma;
        this.nthresh = nthresh;
        this.hmult = hmult;
    }
    
    public void runAlgorithm() {
    	double pb[][];
    	double theta[][];
    	 xDim = srcImage.getExtents()[0];
         yDim = srcImage.getExtents()[1];
         int sliceSize = xDim * yDim;
         double pbBuffer[];
         int x;
         int y;
         boolean testPadReflect = false;
         boolean testHilbertTransform = false;
         
         if (testPadReflect) {
        	 xDim = 10;
        	 yDim = 10;
        	 double im[][] = new double[yDim][xDim];
        	 for (y = 0; y < im.length; y++) {
        		 for (x = 0; x < im[0].length; x++) {
        			 im[y][x] = x + y * im[0].length;
        		 }
        	 }
        	 double impad[][] = new double[yDim+8][xDim+8];
        	 padReflect(impad, im, 4);
        	 for (y = 0; y < impad.length; y++) {
        		 for (x = 0; x < impad[0].length; x++) {
        			 Preferences.debug(impad[y][x] + " ", Preferences.DEBUG_FILEIO);
        		 }
        		 Preferences.debug("\n");
        	 }
        	 setCompleted(false);
        	 return;
         }
         
         if (testHilbertTransform) {
        	 int fsamples = 4;
        	 double fy[] = new double[fsamples];
        	 for (int i = 0; i < fsamples; i++) {
        	     fy[i] = i + 1.0;	 
        	 }
        	 int paddedfsamples = MipavMath.findMinimumPowerOfTwo(fsamples);
        	 double[] paddedfy = new double[2 * paddedfsamples];
        	 for (int i = 0; i < fsamples; i++) {
        		 paddedfy[2*i] = fy[i];
        	 }
        	 AlgorithmHilbertTransform ht = new AlgorithmHilbertTransform(paddedfy, paddedfsamples);
        	 ht.run();
        	 for (int i = 0; i < fsamples; i++) {
        		 fy[i] = paddedfy[2*i+1];
        	 }
        	 double actualHilbertTransform[] = new double[fsamples];
        	 actualHilbertTransform[0] = -1.0;
        	 actualHilbertTransform[1] = 1.0;
        	 actualHilbertTransform[2] = 1.0;
        	 actualHilbertTransform[3] = -1.0;
        	 double error;
        	 for (int i = 0; i < fsamples; i++) {
        	     error = actualHilbertTransform[i] - fy[i];
        	     System.out.println("error["+i+"] = " + error);
        	 }
        	 setCompleted(false);
        	 return;
         }
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Pb Boundary Detection ...");
        
        
    	pb = new double[yDim][xDim];
    	theta = new double[yDim][xDim];
    	if (gradientType == BGTG) {
    	    pbBGTG(pb, theta);
    	}
    	else if (gradientType == CGTG) {
    		pbCGTG(pb, theta);
    	}
    	else if (gradientType == BG) {
    		pbBG(pb, theta);
    	}
    	else if (gradientType == CG) {
    		pbCG(pb, theta);
    	}
    	else if (gradientType == TG) {
    		pbTG(pb, theta);
    	}
    	else if (gradientType == GM) {
    		pbGM(pb, theta);
    	}
    	else if (gradientType == GM2) {
    		pbGM2(pb, theta);
    	}
    	else if (gradientType == TWOMM) {
    		pb2MM(pb, theta);
    	}
    	else if (gradientType == TWOMM2) {
    		pb2MM2(pb, theta);
    	}
    	else if (gradientType == CANNY) {
    		pbCanny(pb);
    	}
    	pbBuffer = new double[sliceSize];
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			pbBuffer[x + y * xDim] = pb[y][x];
    		}
    	}
    	try {
    		destImage.importData(0, pbBuffer, true);
    	}
    	catch(IOException e) {
    		MipavUtil.displayError("IOException " + e + " on destImage.importData(0, pbBuffer, true");
    		setCompleted(false);
    		return;
    	}
        
        setCompleted(true);
        return;
    }
    
    private void pbCanny(double pb[][]) {
    	// Compute probability of boundary using Canny, i.e. gradient magnitude with hysteresis thresholding.
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	double pbgm[][];
    	double theta[][];
    	double thresh[];
    	int i;
    	double spacing;
    	int x;
    	int y;
    	boolean found;
    	byte b[][];
    	
    	// Start with the pb from gradient magnitude
    	pbgm = new double[yDim][xDim];
    	theta = new double[yDim][xDim];
    	pbGM(pbgm, theta);
    	// And apply hysteresis thresholding
    	thresh = new double[nthresh];
    	thresh[0] = 1.0/nthresh;
    	thresh[nthresh-1] = 1.0 -  1.0/nthresh;
    	spacing = (1.0 - 2.0/nthresh)/(nthresh - 1.0);
    	for (i = 1; i < nthresh-1; i++) {
    		thresh[i] = 1.0/nthresh + i * spacing;
    	}
    	b = new byte[yDim][xDim];
    	for (i = 0; i < nthresh; i++) {
    		found = false;
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				b[y][x] = 0;
    				if (pbgm[y][x] >= thresh[i]) {
    					pb[y][x] = Math.max(pb[y][x], thresh[i]);
    					b[y][x] = 1;
    					found = true;
    				}
    			}
    		}
    		while (found) {
    			found = false;
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					if (b[y][x] == 1) {
	    					if (x > 0 && pbgm[y][x-1] > hmult*thresh[i] && b[y][x-1] == 0) {
	    						pb[y][x-1] = Math.max(pb[y][x-1], thresh[i]);
	    						b[y][x-1] = 1;
	    						found = true;
	    					}
	    					if (x < xDim - 1 && pbgm[y][x+1] > hmult*thresh[i] && b[y][x+1] == 0) {
	    						pb[y][x+1] = Math.max(pb[y][x+1], thresh[i]);
	    						b[y][x+1] = 1;
	    						found = true;
	    					}
	    					if (y > 0 && pbgm[y-1][x] > hmult*thresh[i] && b[y-1][x] == 0) {
	    						pb[y-1][x] = Math.max(pb[y-1][x], thresh[i]);
	    						b[y-1][x] = 1;
	    						found = true;
	    					}
	    					if (y < yDim-1 && pbgm[y+1][x] > hmult*thresh[i] && b[y+1][x] == 0) {
	    						pb[y+1][x]= Math.max(pb[y+1][x], thresh[i]);
	    						b[y+1][x] = 1;
	    						found = true;
	    					}
	    					if (x > 0  && y > 0 && pbgm[y-1][x-1] > hmult*thresh[i] && b[y-1][x-1] == 0) {
	    						pb[y-1][x-1] = Math.max(pb[y-1][x-1], thresh[i]);
	    						b[y-1][x-1] = 1;
	    						found = true;
	    					}
	    					if (x > 0 && y < yDim-1 && pbgm[y+1][x-1] > hmult*thresh[i] && b[y+1][x-1] == 0) {
	    						pb[y+1][x-1] = Math.max(pbgm[y+1][x-1], thresh[i]);
	    						b[y+1][x-1] = 1;
	    						found = true;
	    					}
	    					if (x < xDim-1 && y > 0 && pbgm[y-1][x+1] > hmult*thresh[i] && b[y-1][x+1] == 0) {
	    						pb[y-1][x+1] = Math.max(pb[y-1][x+1], thresh[i]);
	    						b[y-1][x+1] = 1;
	    						found = true;
	    					}
	    					if (x < xDim-1 && y < yDim-1 && pbgm[y+1][x+1] > hmult*thresh[i] && b[y+1][x+1] == 0) {
	    						pb[y+1][x+1] = Math.max(pb[y+1][x+1], thresh[i]);
	    						b[y+1][x+1] = 1;
	    						found = true;
	    					}
    					}
    				}
    			}
    		} // while (found)
    	} // for (i = 0; i < nthresh; i++)
    }
    
    private void pb2MM2(double pb[][], double theta[][]) {
    	// Compute probability of boundary using the spatially averaged second moment matrix at 2 scales
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	double beta[] = new double[5];
    	double a1[][];
    	double b1[][];
    	double t1[][];
    	double a2[][];
    	double b2[][];
    	double t2[][];
    	int sliceSize = xDim * yDim;
    	int x;
    	int y;
    	double pbi[];
    	double xbeta;
    	double a1Col[];
    	double b1Col[];
    	double a2Col[];
    	double b2Col[];
    	double dt;
    	
    	// Beta from logistic fits (train2MM2.m)
      	switch ((int)Math.round(sigma)) {
      	case 1:
      		beta[0] = -3.2369080;
      		beta[1] = -7.9668057;
      		beta[2] = -0.71098318;
      		beta[3] = -0.46178498;
      		beta[4] = 31.575066;
      		break;
      	case 2:
      		beta[0] = -3.5512404;
      		beta[1] = -8.0101784;
      		beta[2] = 13.527093;
      		beta[3] = 10.531737;
      		beta[4] = 19.292447;
      		break;
  		default:
  			MipavUtil.displayError("No parameters for sigma = " + sigma);
        	setCompleted(false);
        	return;	
      	}
      	
      	a1 = new double[yDim][xDim];
      	b1 = new double[yDim][xDim];
      	t1 = new double[yDim][xDim];
      	det2MM(a1, b1, t1, sigma, sigma);
      	a2 = new double[yDim][xDim];
      	b2 = new double[yDim][xDim];
      	t2 = new double[yDim][xDim];
      	det2MM(a2, b2, t2, 2.0*sigma, 2.0*sigma);
      	a1Col = new double[sliceSize];
      	b1Col = new double[sliceSize];
      	a2Col = new double[sliceSize];
      	b2Col = new double[sliceSize];
      	for (x = 0; x < xDim; x++) {
      		for (y = 0; y < yDim; y++) {
      			a1Col[y + x * yDim] = Math.sqrt(a1[y][x]);
      			b1Col[y + x * yDim] = Math.sqrt(b1[y][x]);
      			a2Col[y + x * yDim] = Math.sqrt(a2[y][x]);
      			b2Col[y + x * yDim] = Math.sqrt(b2[y][x]);
      		}
      	}
      	pbi = new double[sliceSize];
      	for (y = 0; y < sliceSize; y++) {
        	xbeta = beta[0] + a1Col[y]*beta[1] + b1Col[y]*beta[2] + a2Col[y]*beta[3] + b2Col[y]*beta[4];
        	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
        }
      	for (x = 0; x < xDim; x++) {
        	for (y = 0; y < yDim; y++) {
        	    pb[y][x] = pbi[y + x*yDim];	
        	}
        }
      	
     // Average orientations for nonmax suppression
      	for (y = 0; y < yDim; y++) {
      		for (x = 0; x < xDim; x++) {
      			// [0, 2*PI)
      			dt = t2[y][x] - t1[y][x] - 2.0 * Math.PI*Math.floor((t2[y][x] - t1[y][x])/(2.0*Math.PI));
      			// [-PI, PI)
      			if (dt >= Math.PI) {
      				dt = dt - 2.0 * Math.PI;
      			}
      			theta[y][x] = t1[y][x] + dt/2.0;
      		}
      	}
      	nonmax(pb, pb, theta);
      	return;
    }
    
    private void pb2MM(double pb[][], double theta[][]) {
    	// Compute probability of boundary using the spatially averaged second moment matrix
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	double beta[] = new double[3];
    	double a[][];
    	double b[][];
    	double a2[];
    	double b2[];
    	int sliceSize = xDim * yDim;
    	int x;
    	int y;
    	double pbi[];
    	double xbeta;
    	
    	// Beta from logistic fits (train2MM.m)
      	switch ((int)Math.round(sigma)) {
      	case 1:
      		beta[0] = -2.9101440;
      		beta[1] = -14.635580;
      		beta[2] = 25.478994;
      		break;
      	case 2:
      		beta[0] = -3.2511132;
      		beta[1] = -7.3345180;
      		beta[2] = 30.920345;
      		break;
      	case 4:
      		beta[0] = -3.5676231;
      		beta[1] = 12.901075;
      		beta[2] = 28.938513;
      		break;
      	case 8:
      		beta[0] = -3.4692147;
      		beta[1] = 31.719655;
      		beta[2] = 28.143428;
      		break;
      	case 16:
      		beta[0] = -3.2552836;
      		beta[1] = 64.123944;
      		beta[2] = 25.891795;
      		break;
      		default:
      			MipavUtil.displayError("No parameters for sigma = " + sigma);
            	setCompleted(false);
            	return;	
      	} // switch ((int)Math.round(sigma))
      	a = new double[yDim][xDim];
      	b = new double[yDim][xDim];
      	det2MM(a, b, theta, sigma, sigma);
      	a2 = new double[sliceSize];
      	b2 = new double[sliceSize];
      	for (x = 0; x < xDim; x++) {
      		for (y = 0; y < yDim; y++) {
      			a2[y + x * yDim] = Math.sqrt(a[y][x]);
      			b2[y + x * yDim] = Math.sqrt(b[y][x]);
      		}
      	}
      	pbi = new double[sliceSize];
      	for (y = 0; y < sliceSize; y++) {
        	xbeta = beta[0] + a2[y]*beta[1] + b2[y]*beta[2];
        	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
        }
      	for (x = 0; x < xDim; x++) {
        	for (y = 0; y < yDim; y++) {
        	    pb[y][x] = pbi[y + x*yDim];	
        	}
        }
      	nonmax(pb, pb, theta);
      	return;
    }
    
    private void pbCG(double pb[][], double theta[][]) {
    	// Compute probability of boundary using CG
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// April 2003
      	double beta[] = new double[3];
      	double fstd[] = new double[3];
      	double cg[][][][];
      	double gtheta[];
      	double pball[][][];
      	int i;
      	int sliceSize = xDim * yDim;
      	double a[];
      	double b[];
      	int x;
      	int y;
      	double xbeta;
      	double pbi[];
      	int maxo[][];
      	double maxVal;
      	double r;
      	byte mask[][];
      	double z[][];
      	double a2[][];
      	double pbi2[][];
        
        // beta from logistic fits (trainCG.m)
        if ((lowRadius == 0.02) && (highRadius == 0.02)) {
    		beta[0] = -2.9216153;
    		beta[1] = 0.21939403;
    		beta[2] = 0.53764451;
    		fstd[0] = 1.0;
    		fstd[1] = 0.14210176;
    		fstd[2] = 0.19449891;
    		for (i = 0; i < 3; i++) {
    			beta[i] = beta[i]/fstd[i];
    		}
        } // if ((lowRadius == 0.01) && (highRadius == 0.02))
        else {
        	MipavUtil.displayError("No parameters for lowRadius = " + lowRadius + " highRadius = " + highRadius);
        	setCompleted(false);
        	return;
        }
        
        // Get gradients
        cg = new double[yDim][xDim][3][numOrientations];
        gtheta = new double[numOrientations];
        detCG(cg, gtheta);
        
        // Compute oriented pb
        pball = new double[yDim][xDim][numOrientations];
        a = new double[sliceSize];
        b = new double[sliceSize];
        pbi = new double[sliceSize];
        for (i = 0; i < numOrientations; i++) {
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            		a[y + x * yDim] = cg[y][x][1][i];
            	    b[y + x * yDim] = cg[y][x][2][i];
            	}
            }
            for (y = 0; y < sliceSize; y++) {
            	xbeta = beta[0] + a[y]*beta[1] + b[y]*beta[2];
            	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
            }
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            	    pball[y][x][i] = pbi[y + x*yDim];	
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        // nonmax suppression and max over orientations
        maxo = new int[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		maxVal = -Double.MAX_VALUE;
        		for (i = 0; i < numOrientations; i++)  {
        			if (pball[y][x][i] > maxVal) {
        				maxVal = pball[y][x][i];
        				maxo[y][x] = i;
        			}
        		}
        	}
        }
        r = 2.5;
        mask = new byte[yDim][xDim];
        z = new double[yDim][xDim];
        a2 = new double[yDim][xDim];
        pbi2 = new double[yDim][xDim];
        for (i = 0; i < numOrientations; i++) {
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		if (maxo[y][x] == i) {
            			mask[y][x] = 1;
            		}
            		else {
            			mask[y][x] = 0;
            		}
            		z[y][x] = pball[y][x][i];
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a2[y][x] = 0.0;
            	}
            }
            fitparab(a2, null, null, z, r, r, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a2[y][x] = Math.max(0.0, a2[y][x]);
            	}
            }
            nonmax(pbi2, a2, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		pb[y][x] = Math.max(pb[y][x], pbi2[y][x] * mask[y][x]);
            		theta[y][x] = theta[y][x] * (1 - mask[y][x]) + gtheta[i]*mask[y][x];
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		pb[y][x] = Math.max(0.0, Math.min(1.0, pb[y][x]));
        	}
        }
        
        // Mask out 1-pixel border where nonmax suppression fails
        for (x = 0; x < xDim; x++) {
        	pb[0][x] = 0.0;
        	pb[yDim-1][x] = 0.0;
        }
        for (y = 0; y < yDim; y++) {
        	pb[y][0] = 0.0;
        	pb[y][xDim-1] = 0.0;
        }
        return;
      }
    
    private void pbCGTG(double pb[][], double theta[][]) {
    	// Compute probability of boundary using CG and TG
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// April 2003

      	double beta[] = new double[5];
      	double fstd[] = new double[5];
      	double cg[][][][];
      	double tg[][][];
      	double gtheta[];
      	double pball[][][];
      	int i;
      	int sliceSize = xDim * yDim;
      	double l[];
      	double a[];
      	double b[];
      	double t[];
      	int x;
      	int y;
      	double xbeta;
      	double pbi[];
      	int maxo[][];
      	double maxVal;
      	double r;
      	byte mask[][];
      	double z[][];
      	double a2[][];
      	double pbi2[][];
        
        // beta from logistic fits (trainCGTG.m)
        if ((lowRadius == 0.01) && (highRadius == 0.02)) {
        	// 64 textons
    		beta[0] = -4.5015774;
    		beta[1] = 0.66845040;
    		beta[2] = 0.13588346;
    		beta[3] = 0.19537985;
    		beta[4] = 0.53922927;
    		fstd[0] = 1.0;
    		fstd[1] = 0.39505238;
    		fstd[2] = 0.14210176;
    		fstd[3] = 0.19449891;
    		fstd[4] = 0.19178634;
    		for (i = 0; i < 5; i++) {
    			beta[i] = beta[i]/fstd[i];
    		}
        } // if ((lowRadius == 0.01) && (highRadius == 0.02))
        else {
        	MipavUtil.displayError("No parameters for lowRadius = " + lowRadius + " highRadius = " + highRadius);
        	setCompleted(false);
        	return;
        }
        
        // Get gradients
        cg = new double[yDim][xDim][3][numOrientations];
        tg = new double[yDim][xDim][numOrientations];
        gtheta = new double[numOrientations];
        detCGTG(cg, tg, gtheta);
        
        // Compute oriented pb
        pball = new double[yDim][xDim][numOrientations];
        l = new double[sliceSize];
        a = new double[sliceSize];
        b = new double[sliceSize];
        t = new double[sliceSize];
        pbi = new double[sliceSize];
        for (i = 0; i < numOrientations; i++) {
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            		l[y + x * yDim] = cg[y][x][0][i];
            		a[y + x * yDim] = cg[y][x][1][i];
            	    b[y + x * yDim] = cg[y][x][2][i];
            	    t[y + x * yDim] = tg[y][x][i];
            	}
            }
            for (y = 0; y < sliceSize; y++) {
            	xbeta = beta[0] + l[y]*beta[1] + a[y]*beta[2] + b[y]*beta[3] + t[y]*beta[4];
            	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
            }
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            	    pball[y][x][i] = pbi[y + x*yDim];	
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        // nonmax suppression and max over orientations
        maxo = new int[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		maxVal = -Double.MAX_VALUE;
        		for (i = 0; i < numOrientations; i++)  {
        			if (pball[y][x][i] > maxVal) {
        				maxVal = pball[y][x][i];
        				maxo[y][x] = i;
        			}
        		}
        	}
        }
        r = 2.5;
        mask = new byte[yDim][xDim];
        z = new double[yDim][xDim];
        a2 = new double[yDim][xDim];
        pbi2 = new double[yDim][xDim];
        for (i = 0; i < numOrientations; i++) {
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		if (maxo[y][x] == i) {
            			mask[y][x] = 1;
            		}
            		else {
            			mask[y][x] = 0;
            		}
            		z[y][x] = pball[y][x][i];
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a2[y][x] = 0.0;
            	}
            }
            fitparab(a2, null, null, z, r, r, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a2[y][x] = Math.max(0.0, a2[y][x]);
            	}
            }
            nonmax(pbi2, a2, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		pb[y][x] = Math.max(pb[y][x], pbi2[y][x] * mask[y][x]);
            		theta[y][x] = theta[y][x] * (1 - mask[y][x]) + gtheta[i]*mask[y][x];
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		pb[y][x] = Math.max(0.0, Math.min(1.0, pb[y][x]));
        	}
        }
        
        // Mask out 1-pixel border where nonmax suppression fails
        for (x = 0; x < xDim; x++) {
        	pb[0][x] = 0.0;
        	pb[yDim-1][x] = 0.0;
        }
        for (y = 0; y < yDim; y++) {
        	pb[y][0] = 0.0;
        	pb[y][xDim-1] = 0.0;
        }
        return;
      }
    
    private void pbGM2(double pb[][], double theta[][]) {
    	// Compute probability of boundary using gradient magnitude at 2 scales
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003

      	double beta[] = new double[3];
      	int sliceSize = xDim * yDim;
      	int x;
      	int y;
      	double xbeta;
      	double pbi[];
      	double a[][];
      	double t1[][];
      	double b[][];
      	double t2[][];
      	double a2[];
      	double b2[];
      	double dt;
      	
      	// Beta from logistic fits (trainGM2.m)
      	switch ((int)Math.round(sigma)) {
      	case 1:
      	    beta[0] = -2.9845759;
      	    beta[1] = -4.4804245;
      	    beta[2] = 26.493560;
      	    break;
      	case 2:
      		beta[0] = -3.2341949;
      		beta[1] = 6.5031017;
      		beta[2] = 20.245465;
      		break;
      	case 3:
      		beta[0] = -3.0361378;
      		beta[1] = 15.965267;
      		beta[2] = 16.130494;
      		break;
        default:
        	MipavUtil.displayError("No parameters for sigma = " + sigma);
        	setCompleted(false);
        	return;
      	} // switch ((int)Math.round(sigma))
      	
      	a = new double[yDim][xDim];
      	t1 = new double[yDim][xDim];
      	detGM(a, t1, sigma);
      	b = new double[yDim][xDim];
      	t2 = new double[yDim][xDim];
      	detGM(b, t2, 2.0*sigma);
      	a2 = new double[sliceSize];
      	b2 = new double[sliceSize];
      	for (x = 0; x < xDim; x++) {
      		for (y = 0; y < yDim; y++) {
      			a2[y + x * yDim] = a[y][x];
      			b2[y + x * yDim] = b[y][x];
      		}
      	}
      	pbi = new double[sliceSize];
      	for (y = 0; y < sliceSize; y++) {
        	xbeta = beta[0] + a2[y]*beta[1] + b2[y]*beta[2];
        	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
        }
      	for (x = 0; x < xDim; x++) {
        	for (y = 0; y < yDim; y++) {
        	    pb[y][x] = pbi[y + x*yDim];	
        	}
        }
      	
      	// Average orientations for nonmax suppression
      	for (y = 0; y < yDim; y++) {
      		for (x = 0; x < xDim; x++) {
      			// [0, 2*PI)
      			dt = t2[y][x] - t1[y][x] - 2.0 * Math.PI*Math.floor((t2[y][x] - t1[y][x])/(2.0*Math.PI));
      			// [-PI, PI)
      			if (dt >= Math.PI) {
      				dt = dt - 2.0 * Math.PI;
      			}
      			theta[y][x] = t1[y][x] + dt/2.0;
      		}
      	}
      	nonmax(pb, pb, theta);
      	return;
    }
    
    private void pbGM(double pb[][], double theta[][]) {
    	// Compute probability of boundary using gradient magnitude
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003

      	double beta[] = new double[2];
      	double m[][];
      	int sliceSize = xDim * yDim;
      	double m2[];
      	int x;
      	int y;
      	double xbeta;
      	double pbi[];
      	
      	// Beta from logistic fits (trainGM.m)
      	switch ((int)Math.round(sigma)) {
      	case 1:
      		beta[0] = -2.6828268;
      		beta[1] = 16.251270;
      		break;
      	case 2:
      		beta[0] = -2.9906198;
      		beta[1] = 22.454909;
      		break;
      	case 4:
      		beta[0] = -3.2040961;
      		beta[1] = 25.838634;
      		break;
      	case 8:
      		beta[0] = -2.9314518;
      		beta[1] = 29.306847;
      		break;
      	case 16:
      		beta[0] = -2.4502722;
      		beta[1] = 34.139686;
      		break;
      	default:
      		MipavUtil.displayError("No parameters for sigma = " + sigma);
        	setCompleted(false);
        	return;	
      	} // switch ((int)Math.round(sigma))
      	
      	m = new double[yDim][xDim];
      	detGM(m, theta, sigma);
      	m2 = new double[sliceSize];
      	for (x = 0; x < xDim; x++) {
      		for (y = 0; y < yDim; y++) {
      			m2[y + x * yDim] = m[y][x];
      		}
      	}
      	pbi = new double[sliceSize];
      	for (y = 0; y < sliceSize; y++) {
        	xbeta = beta[0] + m2[y]*beta[1];
        	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
        }
      	for (x = 0; x < xDim; x++) {
        	for (y = 0; y < yDim; y++) {
        	    pb[y][x] = pbi[y + x*yDim];	
        	}
        }
      	nonmax(pb, pb, theta);
      	return;
    }
    
    private void pbBG(double pb[][], double theta[][]) {
    	// Compute probability of boundary using BG
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// April 2003

      	double beta[] = new double[2];
      	double fstd[] = new double[2];
      	double bg[][][];
      	double gtheta[];
      	double pball[][][];
      	int i;
      	int sliceSize = xDim * yDim;
      	double b[];
      	int x;
      	int y;
      	double xbeta;
      	double pbi[];
      	int maxo[][];
      	double maxVal;
      	double r;
      	byte mask[][];
      	double z[][];
      	double a[][];
      	double pbi2[][];
        
        // beta from logistic fits (trainBG.m)
        if (lowRadius == 0.01) {
    	    beta[0] = -3.6944544;
    	    beta[1] = 1.0261318;
    	    fstd[0] = 1.0;
    	    fstd[1] = 0.37408935;
        	beta[0] = beta[0]/fstd[0];
        	beta[1] = beta[1]/fstd[1];
        } // if (lowRadius == 0.01)
        else {
        	MipavUtil.displayError("No parameter for lowRadius = " + lowRadius);
        	setCompleted(false);
        	return;
        }
        
        // Get gradients
        bg = new double[yDim][xDim][numOrientations];
        gtheta = new double[numOrientations];
        detBG(bg, gtheta);
        
        // Compute oriented pb
        pball = new double[yDim][xDim][numOrientations];
        b = new double[sliceSize];
        pbi = new double[sliceSize];
        for (i = 0; i < numOrientations; i++) {
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            	    b[y + x * yDim] = bg[y][x][i];
            	}
            }
            for (y = 0; y < sliceSize; y++) {
            	xbeta = beta[0] + b[y]*beta[1];
            	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
            }
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            	    pball[y][x][i] = pbi[y + x*yDim];	
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        // nonmax suppression and max over orientations
        maxo = new int[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		maxVal = -Double.MAX_VALUE;
        		for (i = 0; i < numOrientations; i++)  {
        			if (pball[y][x][i] > maxVal) {
        				maxVal = pball[y][x][i];
        				maxo[y][x] = i;
        			}
        		}
        	}
        }
        r = 2.5;
        mask = new byte[yDim][xDim];
        z = new double[yDim][xDim];
        a = new double[yDim][xDim];
        pbi2 = new double[yDim][xDim];
        for (i = 0; i < numOrientations; i++) {
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		if (maxo[y][x] == i) {
            			mask[y][x] = 1;
            		}
            		else {
            			mask[y][x] = 0;
            		}
            		z[y][x] = pball[y][x][i];
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a[y][x] = 0.0;
            	}
            }
            fitparab(a, null, null, z, r, r, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a[y][x] = Math.max(0.0, a[y][x]);
            	}
            }
            nonmax(pbi2, a, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		pb[y][x] = Math.max(pb[y][x], pbi2[y][x] * mask[y][x]);
            		theta[y][x] = theta[y][x] * (1 - mask[y][x]) + gtheta[i]*mask[y][x];
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		pb[y][x] = Math.max(0.0, Math.min(1.0, pb[y][x]));
        	}
        }
        
        // Mask out 1-pixel border where nonmax suppression fails
        for (x = 0; x < xDim; x++) {
        	pb[0][x] = 0.0;
        	pb[yDim-1][x] = 0.0;
        }
        for (y = 0; y < yDim; y++) {
        	pb[y][0] = 0.0;
        	pb[y][xDim-1] = 0.0;
        }
        return;
      }
    
    
      private void pbBGTG(double pb[][], double theta[][]) {
    	// Compute probability of boundary using BG and TG
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// April 2003

      	double beta[] = new double[3];
      	double fstd[] = new double[3];
      	double bg[][][];
      	double tg[][][];
      	double gtheta[];
      	double pball[][][];
      	int i;
      	int sliceSize = xDim * yDim;
      	double b[];
      	double t[];
      	int x;
      	int y;
      	double xbeta;
      	double pbi[];
      	int maxo[][];
      	double maxVal;
      	double r;
      	byte mask[][];
      	double z[][];
      	double a[][];
      	double pbi2[][];
        
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
        bg = new double[yDim][xDim][numOrientations];
        tg = new double[yDim][xDim][numOrientations];
        gtheta = new double[numOrientations];
        detBGTG(bg, tg, gtheta);
        
        // Compute oriented pb
        pball = new double[yDim][xDim][numOrientations];
        b = new double[sliceSize];
        t = new double[sliceSize];
        pbi = new double[sliceSize];
        for (i = 0; i < numOrientations; i++) {
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            	    b[y + x * yDim] = bg[y][x][i];
            	    t[y + x * yDim] = tg[y][x][i];
            	}
            }
            for (y = 0; y < sliceSize; y++) {
            	xbeta = beta[0] + b[y]*beta[1] + t[y]*beta[2];
            	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
            }
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            	    pball[y][x][i] = pbi[y + x*yDim];	
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        // nonmax suppression and max over orientations
        maxo = new int[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		maxVal = -Double.MAX_VALUE;
        		for (i = 0; i < numOrientations; i++)  {
        			if (pball[y][x][i] > maxVal) {
        				maxVal = pball[y][x][i];
        				maxo[y][x] = i;
        			}
        		}
        	}
        }
        r = 2.5;
        mask = new byte[yDim][xDim];
        z = new double[yDim][xDim];
        a = new double[yDim][xDim];
        pbi2 = new double[yDim][xDim];
        for (i = 0; i < numOrientations; i++) {
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		if (maxo[y][x] == i) {
            			mask[y][x] = 1;
            		}
            		else {
            			mask[y][x] = 0;
            		}
            		z[y][x] = pball[y][x][i];
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a[y][x] = 0.0;
            	}
            }
            fitparab(a, null, null, z, r, r, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		a[y][x] = Math.max(0.0, a[y][x]);
            	}
            }
            nonmax(pbi2, a, gtheta[i]);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		pb[y][x] = Math.max(pb[y][x], pbi2[y][x] * mask[y][x]);
            		theta[y][x] = theta[y][x] * (1 - mask[y][x]) + gtheta[i]*mask[y][x];
            	}
            }
        } // for (i = 0; i < numOrientations; i++)
        
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		pb[y][x] = Math.max(0.0, Math.min(1.0, pb[y][x]));
        	}
        }
        
        // Mask out 1-pixel border where nonmax suppression fails
        for (x = 0; x < xDim; x++) {
        	pb[0][x] = 0.0;
        	pb[yDim-1][x] = 0.0;
        }
        for (y = 0; y < yDim; y++) {
        	pb[y][0] = 0.0;
        	pb[y][xDim-1] = 0.0;
        }
        return;
      }
      
      private void pbTG(double pb[][], double theta[][]) {
      	// Compute probability of boundary using TG
      	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
      	// April 2003

        	double beta[] = new double[2];
        	double fstd[] = new double[2];
        	double tg[][][];
        	double gtheta[];
        	double pball[][][];
        	int i;
        	int sliceSize = xDim * yDim;
        	double t[];
        	int x;
        	int y;
        	double xbeta;
        	double pbi[];
        	int maxo[][];
        	double maxVal;
        	double r;
        	byte mask[][];
        	double z[][];
        	double a[][];
        	double pbi2[][];
          
          // beta from logistic fits (trainTG.m)
          if (lowRadius == 0.02) {
          	// 64 textons
          	beta[0] = -4.7151584;
          	beta[1] = 1.2222425;
            fstd[0] = 1.0;
          	fstd[1] = 0.19171689;
          	beta[0] = beta[0]/fstd[0];
          	beta[1] = beta[1]/fstd[1];
          } // if ((lowRadius == 0.01) && (highRadius == 0.02))
          else {
          	MipavUtil.displayError("No parameters for lowRadius = " + lowRadius);
          	setCompleted(false);
          	return;
          }
          
          // Get gradients
          tg = new double[yDim][xDim][numOrientations];
          gtheta = new double[numOrientations];
          detTG(tg, gtheta);
          
          // Compute oriented pb
          pball = new double[yDim][xDim][numOrientations];
          t = new double[sliceSize];
          pbi = new double[sliceSize];
          for (i = 0; i < numOrientations; i++) {
              for (x = 0; x < xDim; x++) {
              	for (y = 0; y < yDim; y++) {
              	    t[y + x * yDim] = tg[y][x][i];
              	}
              }
              for (y = 0; y < sliceSize; y++) {
              	xbeta = beta[0] + t[y]*beta[1];
              	pbi[y] = 1.0/(1.0 + Math.exp(-xbeta));
              }
              for (x = 0; x < xDim; x++) {
              	for (y = 0; y < yDim; y++) {
              	    pball[y][x][i] = pbi[y + x*yDim];	
              	}
              }
          } // for (i = 0; i < numOrientations; i++)
          
          // nonmax suppression and max over orientations
          maxo = new int[yDim][xDim];
          for (y = 0; y < yDim; y++) {
          	for (x = 0; x < xDim; x++) {
          		maxVal = -Double.MAX_VALUE;
          		for (i = 0; i < numOrientations; i++)  {
          			if (pball[y][x][i] > maxVal) {
          				maxVal = pball[y][x][i];
          				maxo[y][x] = i;
          			}
          		}
          	}
          }
          r = 2.5;
          mask = new byte[yDim][xDim];
          z = new double[yDim][xDim];
          a = new double[yDim][xDim];
          pbi2 = new double[yDim][xDim];
          for (i = 0; i < numOrientations; i++) {
              for (y = 0; y < yDim; y++) {
              	for (x = 0; x < xDim; x++) {
              		if (maxo[y][x] == i) {
              			mask[y][x] = 1;
              		}
              		else {
              			mask[y][x] = 0;
              		}
              		z[y][x] = pball[y][x][i];
              	}
              }
              for (y = 0; y < yDim; y++) {
              	for (x = 0; x < xDim; x++) {
              		a[y][x] = 0.0;
              	}
              }
              fitparab(a, null, null, z, r, r, gtheta[i]);
              for (y = 0; y < yDim; y++) {
              	for (x = 0; x < xDim; x++) {
              		a[y][x] = Math.max(0.0, a[y][x]);
              	}
              }
              nonmax(pbi2, a, gtheta[i]);
              for (y = 0; y < yDim; y++) {
              	for (x = 0; x < xDim; x++) {
              		pb[y][x] = Math.max(pb[y][x], pbi2[y][x] * mask[y][x]);
              		theta[y][x] = theta[y][x] * (1 - mask[y][x]) + gtheta[i]*mask[y][x];
              	}
              }
          } // for (i = 0; i < numOrientations; i++)
          
          for (y = 0; y < yDim; y++) {
          	for (x = 0; x < xDim; x++) {
          		pb[y][x] = Math.max(0.0, Math.min(1.0, pb[y][x]));
          	}
          }
          
          // Mask out 1-pixel border where nonmax suppression fails
          for (x = 0; x < xDim; x++) {
          	pb[0][x] = 0.0;
          	pb[yDim-1][x] = 0.0;
          }
          for (y = 0; y < yDim; y++) {
          	pb[y][0] = 0.0;
          	pb[y][xDim-1] = 0.0;
          }
          return;
        }
      
      private void nonmax(double imout[][], double im[][], double theta[][]) {
    	  // Perform non-max suppression on im orthogonal to theta.  Theta can be
    	  // a matrix providing a different theta for each pixel or a scalar
    	  // proving the same theta for every pixel.
    	  
    	  // Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	  // March 2003
          int y;
          int x;
          int h = im.length;
          int w = im[0].length;
          boolean mask15[][] = new boolean[h][w];
          boolean mask26[][] = new boolean[h][w];
          boolean mask37[][] = new boolean[h][w];
          boolean mask48[][] = new boolean[h][w];
          byte mask[][];
          double imidxA;
          double imidxB;
          double d;
          double imI;
          
          // Do non-max suppression orthogonal to theta.
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  theta[y][x] = theta[y][x] + Math.PI/2.0 - Math.PI*Math.floor((theta[y][x] + Math.PI/2.0)/Math.PI);

        	  }
          }
    	  
    	  // The following diagram depicts the 8 cases for non-max suppression.
    	  // Theta is valued in [0,pi), measured clockwise from the positive x
    	  // axis.  The 'o' marks the pixel of interest, and the eight
    	  // neighboring pixels are marked with '.'.  The orientation is divided
    	  // into 8 45-degree blocks.  Within each block, we interpolate the
    	  // image value between the two neighboring pixels.
    	  //
    	  //        .66.77.                                
    	  //        5\ | /8                                
    	  //        5 \|/ 8                                
    	  //        .--o--.-----> x-axis                     
    	  //        4 /|\ 1                                
    	  //        4/ | \1                                
    	  //        .33.22.                                
    	  //           |                                   
    	  //           |
    	  //           v
    	  //         y-axis                                  
    	  //
    	  // In the code below, d is always the distance from A, so the distance
    	  // to B is (1-d).  A and B are the two neighboring pixels of interest
    	  // in each of the 8 cases.  Note that the clockwise ordering of A and B
    	  // changes from case to case in order to make it easier to compute d.

    	  // Determine which pixels belong to which cases.
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  mask15[y][x] = (theta[y][x] >= 0.0 && theta[y][x] < Math.PI/4.0);
                  mask26[y][x] = (theta[y][x] >= Math.PI/4.0 && theta[y][x] < Math.PI/2.0);
                  mask37[y][x] = (theta[y][x] >= Math.PI/2.0 && theta[y][x] < Math.PI*3.0/4.0);
                  mask48[y][x] = (theta[y][x] >= Math.PI*3.0/4.0 && theta[y][x] < Math.PI);  
        	  }
          }
          
          
          mask = new byte[h][w];
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  mask[y][x] = 1;
        	  }
          }
          
          // Case 1
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  if (mask15[y][x] && x < w-1 && y < h - 1) {
        			  imidxA = im[y][x+1];
        			  imidxB = im[y+1][x+1];
        			  d = Math.tan(theta[y][x]);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }
        		  }
              }
          }
              
          // Case 5
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  if (mask15[y][x] && (x > 0) && (y > 0)) {
        			  imidxA = im[y][x-1];
        			  imidxB = im[y-1][x-1];
        			  d = Math.tan(theta[y][x]);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }  
        		  }
        	  }
          }
          
          // case 2
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  if (mask26[y][x] && x < w-1 && y < h - 1) {
        			  imidxA = im[y+1][x];
        			  imidxB = im[y+1][x+1];
        			  d = Math.tan(Math.PI/2.0 - theta[y][x]);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }      
        		  }
        	  }
          }
          
          // Case 6
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  if (mask26[y][x] && (x > 0) && (y > 0)) {
        			  imidxA = im[y-1][x];
        			  imidxB = im[y-1][x-1];
        			  d = Math.tan(Math.PI/2.0 - theta[y][x]);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }      	  
        		  }
        	  }
          }
          
          // case 3
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  if (mask37[y][x] && (x > 0) && (y < h-1)) {
        			  imidxA = im[y+1][x];
        			  imidxB = im[y+1][x-1];
        			  d = Math.tan(theta[y][x] - Math.PI/2.0);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }      	    
        		  }
        	  }
          }
          
          // case 7
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  if (mask37[y][x] && (x < w-1) && (y > 0)) {
        			  imidxA = im[y-1][x];
        			  imidxB = im[y-1][x+1];
        			  d = Math.tan(theta[y][x] - Math.PI/2.0);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }      	    	  
        		  }
        	  }
          }
          
    	  // case 4
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        	      if (mask48[y][x] && (x > 0) && (y < h-1)) {
        	    	  imidxA = im[y][x-1];
        			  imidxB = im[y+1][x-1];
        			  d = Math.tan(Math.PI - theta[y][x]);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }      	    	    
        	      }
        	  }
          }
          
          // case 8
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  if (mask48[y][x] && (x < w-1) && (y > 0)) {
        			  imidxA = im[y][x+1];
        			  imidxB = im[y-1][x+1];
        			  d = Math.tan(Math.PI - theta[y][x]);
        			  imI = imidxA * (1.0 - d) + imidxB * d;
        			  if (im[y][x] < imI) {
        				  mask[y][x] = 0;
        			  }     
        		  }
        	  }
          }
          
          // Apply mask
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  imout[y][x] = im[y][x] * mask[y][x];
        	  }
          }
      }
      
      private void nonmax(double imout[][], double im[][], double theta) {
    	  // Perform non-max suppression on im orthogonal to theta.  Theta can be
    	  // a matrix providing a different theta for each pixel or a scalar
    	  // proving the same theta for every pixel.
    	  
    	  // Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	  // March 2003
          int y;
          int x;
          boolean mask15;
          boolean mask26;
          boolean mask37;
          boolean mask48;
          int h = im.length;
          int w = im[0].length;
          byte mask[][];
          double imidxA;
          double imidxB;
          double d;
          double imI;
          
          // Do non-max suppression orthogonal to theta.
    	  theta = theta + Math.PI/2.0 - Math.PI*Math.floor((theta + Math.PI/2.0)/Math.PI);

    	  // The following diagram depicts the 8 cases for non-max suppression.
    	  // Theta is valued in [0,pi), measured clockwise from the positive x
    	  // axis.  The 'o' marks the pixel of interest, and the eight
    	  // neighboring pixels are marked with '.'.  The orientation is divided
    	  // into 8 45-degree blocks.  Within each block, we interpolate the
    	  // image value between the two neighboring pixels.
    	  //
    	  //        .66.77.                                
    	  //        5\ | /8                                
    	  //        5 \|/ 8                                
    	  //        .--o--.-----> x-axis                     
    	  //        4 /|\ 1                                
    	  //        4/ | \1                                
    	  //        .33.22.                                
    	  //           |                                   
    	  //           |
    	  //           v
    	  //         y-axis                                  
    	  //
    	  // In the code below, d is always the distance from A, so the distance
    	  // to B is (1-d).  A and B are the two neighboring pixels of interest
    	  // in each of the 8 cases.  Note that the clockwise ordering of A and B
    	  // changes from case to case in order to make it easier to compute d.

    	  // Determine which pixels belong to which cases.
          mask15 = (theta >= 0.0 && theta < Math.PI/4.0);
          mask26 = (theta >= Math.PI/4.0 && theta < Math.PI/2.0);
          mask37 = (theta >= Math.PI/2.0 && theta < Math.PI*3.0/4.0);
          mask48 = (theta >= Math.PI*3.0/4.0 && theta < Math.PI);
          
          mask = new byte[h][w];
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  mask[y][x] = 1;
        	  }
          }
          
          if (mask15) {
        	  // Case 1
        	  d = Math.tan(theta);
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            		  if (x < w-1 && y < h - 1) {
            			  imidxA = im[y][x+1];
            			  imidxB = im[y+1][x+1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }
            		  }
            	  }
              }
              
              // Case 5
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            		  if ((x > 0) && (y > 0)) {
            			  imidxA = im[y][x-1];
            			  imidxB = im[y-1][x-1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }  
            		  }
            	  }
              }
          } // if (mask15)
          
          if (mask26) {
              // case 2
        	  d = Math.tan(Math.PI/2.0 - theta);
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            		  if (x < w-1 && y < h - 1) {
            			  imidxA = im[y+1][x];
            			  imidxB = im[y+1][x+1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }      
            		  }
            	  }
              }
              
              // Case 6
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            		  if ((x > 0) && (y > 0)) {
            			  imidxA = im[y-1][x];
            			  imidxB = im[y-1][x-1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }      	  
            		  }
            	  }
              }
          } // if (mask26)
          
          if (mask37) {
              // case 3
        	  d = Math.tan(theta - Math.PI/2.0);
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            		  if ((x > 0) && (y < h-1)) {
            			  imidxA = im[y+1][x];
            			  imidxB = im[y+1][x-1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }      	    
            		  }
            	  }
              }
              
              // case 7
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            		  if ((x < w-1) && (y > 0)) {
            			  imidxA = im[y-1][x];
            			  imidxB = im[y-1][x+1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }      	    	  
            		  }
            	  }
              }
          } // if (mask37)
          
          if (mask48) {
        	  d = Math.tan(Math.PI - theta);
        	  // case 4
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            	      if ((x > 0) && (y < h-1)) {
            	    	  imidxA = im[y][x-1];
            			  imidxB = im[y+1][x-1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }      	    	    
            	      }
            	  }
              }
              
              // case 8
              for (y = 0; y < h; y++) {
            	  for (x = 0; x < w; x++) {
            		  if ((x < w-1) && (y > 0)) {
            			  imidxA = im[y][x+1];
            			  imidxB = im[y-1][x+1];
            			  imI = imidxA * (1.0 - d) + imidxB * d;
            			  if (im[y][x] < imI) {
            				  mask[y][x] = 0;
            			  }     
            		  }
            	  }
              }
          } // if (mask48)
          
          // Apply mask
          for (y = 0; y < h; y++) {
        	  for (x = 0; x < w; x++) {
        		  imout[y][x] = im[y][x] * mask[y][x];
        	  }
          }
      }
      
      /**
       * 
       * @param e0 output smaller eigenvlaues
       * @param e1 output larger eigenvalues
       * @param theta output Orientation of first eigenvector + PI/2
       *                     (i.e. orientation of possible edge)
       * @param sigmaI input Inner scale (sigma for image derivatives)
       * @param sigmaO input Outer scale (sigma for spatial averaging)
       */
      private void det2MM(double e0[][], double e1[][], double theta[][], double sigmaI, double sigmaO) {
    	  // Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	  // March 2003
    	  ModelImage grayImage = null;
          ModelImage inputImage;
          AlgorithmChangeType changeTypeAlgo;
      	  FileInfoBase[] fileInfo;
      	  int support;
    	  int deriv;
    	  int hsz;
    	  int sz;
    	  double fb[][][][];
    	  double fb1[][];
    	  int x;
    	  int y;
    	  double fim[][][][];
    	  double im[][];
    	  double buffer[];
    	  int sliceSize = xDim * yDim;
    	  double dx[][];
    	  double dy[][];
    	  double sigmaX;
    	  double sigmaY;
    	  double f[][];
    	  double dx2[][];
    	  double dy2[][];
    	  double dxy[][];
    	  double dxSquared[][];
    	  double dySquared[][];
    	  double dxdyIn[][];
    	  double k;
    	  double eig0;
    	  double eig1;
    	  double t0;
    	  double t1;
    	  double diff;
    	  
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
				  redValue = 0.2989f;
				  greenValue = 0.5870f;
				  blueValue = 0.1140f;
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
          
          sigmaI = Math.max(0.5,  sigmaI);
          sigmaO = Math.max(0.5, sigmaO);
          
          // Compute the x and y image derivatives at inner scale
          sigmaX = sigmaI;
          sigmaY = sigmaI;
          support = 3;
       	  hsz = (int)Math.max(Math.ceil(sigmaX * support), Math.ceil(sigmaY * support));
          sz = 2 * hsz + 1;
          fb1 = new double[sz][sz];
          deriv = 1;
          oeFilter(fb1, sigmaX, sigmaY, support, Math.PI/2.0, deriv);
          fb = new double[2][1][sz][sz];
          for (y = 0; y < sz; y++) {
              for (x = 0; x < sz; x++) {
            	  fb[0][0][y][x] = fb1[y][x];
            	  fb[1][0][y][x] = fb1[x][y];
              }
          }
          fim = new double[2][1][yDim][xDim];
          buffer = new double[sliceSize];
          try {
        	  inputImage.exportData(0, sliceSize, buffer);
          }
          catch(IOException e) {
        	  MipavUtil.displayError("IOException " + e + " on inputImage.exportData(0, sliceSize, buffer");
        	  return;
          }
          
          inputImage.disposeLocal();
          inputImage = null;
          im = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  im[y][x] = buffer[x + y * xDim];
        	  }
          }
          fbRun(fim, fb, im);
          dx = new double[yDim][xDim];
          dy = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  dx[y][x] = fim[0][0][y][x];
        		  dy[y][x] = fim[1][0][y][x];
        	  }
          }
          
          // Compute smoothed squared image derivatives at outer scale
          sigmaX = sigmaO;
          sigmaY = sigmaO; 
          support = 3;
          hsz = (int)Math.max(Math.ceil(sigmaX * support), Math.ceil(sigmaY * support));
          sz = 2 * hsz + 1;
          f = new double[sz][sz];
          oeFilter(f, sigmaX, sigmaY, support);
          dxSquared = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  dxSquared[y][x] = dx[y][x]*dx[y][x];
        	  }
          }
          dx2 = new double[yDim][xDim];
          fbRun(dx2, f, dxSquared);
          dySquared = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  dySquared[y][x] = dy[y][x]*dy[y][x];
        	  }
          }
          dy2 = new double[yDim][xDim];
          fbRun(dy2, f, dySquared);
          dxdyIn = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  dxdyIn[y][x] = dx[y][x]*dy[y][x];
        	  }
          }
          dxy = new double[yDim][xDim];
          fbRun(dxy, f, dxdyIn);
          
          // Compute eigenvalues of the spatially averaged 2nd moment matrix
          // and the orientations of the eigenvectors
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  diff = dx2[y][x] - dy2[y][x];
        		  k = Math.sqrt(diff*diff + 4.0*dxy[y][x]*dxy[y][x]);
        		  eig0 = (dx2[y][x] + dy2[y][x] - k)/2.0;
        		  eig1 = (dx2[y][x] + dy2[y][x] + k)/2.0;
        		  t0 = Math.atan2(dx2[y][x] - eig0, -dxy[y][x]);
        		  t1 = Math.atan2(dx2[y][x] - eig1, -dxy[y][x]);
        		  // Order eigenvalues by their absolute values, so e0 <= e1, and pick
        		  // out the orientation corresponding to the largest eigenvalue
        		  if (Math.abs(eig1) > Math.abs(eig0)) {
        			  e0[y][x] = Math.abs(eig0);
        			  e1[y][x] = Math.abs(eig1);
        			  theta[y][x] = t1;
        		  }
        		  else {
        			  e0[y][x] = Math.abs(eig1);
        			  e1[y][x] = Math.abs(eig0); 
        			  theta[y][x] = t0;
        		  }
        		  theta[y][x] = theta[y][x] + Math.PI/2.0 - Math.PI*Math.floor((theta[y][x] + Math.PI/2.0)/Math.PI); 
        		  if (e0[y][x] > e1[y][x]) {
        			  MipavUtil.displayError("Error e0["+y+"]["+x+"] > e1["+y+"]["+x+"] in det2MM");
        			  return;
        		  }
        	  }
          }
      }
      
      private void detCG(double cg[][][][], double theta[]) {
      	// Compute smoothed but not thinned CG fields
      	double diag;
      	ModelImage inputImage;
      	AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
        double radiusArray[]; 
        double sigmaSmoothArray[];
      	
      	diag = Math.sqrt(xDim*xDim + yDim*yDim);
      	inputImage = new ModelImage(ModelStorageBase.ARGB_FLOAT,
  				srcImage.getExtents(), "changeTypeImage");
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
          
          // Compute color gradient
          
          radiusArray = new double[]{diag*lowRadius,diag*highRadius,diag*highRadius};
          sigmaSmoothArray = new double[]{diag*lowRadius,diag*highRadius,diag*highRadius};;
          cgmo(cg, theta, inputImage, radiusArray, numOrientations, smooth, sigmaSmoothArray);
          inputImage.disposeLocal();
          inputImage = null;
  		
          return;
        }
      
      private void detCGTG(double cg[][][][], double tg[][][], double theta[]) {
    	// Compute smoothed but not thinned CG and TG fields
    	double diag;
    	ModelImage inputImage;
    	AlgorithmChangeType changeTypeAlgo;
    	FileInfoBase[] fileInfo;
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
		ModelImage grayImage;
		double fb[][][][];
      	double tex[][];
      	double tsim[][];
      	int sliceSize;
      	double buffer[];
      	double im[][];
      	int x;
      	int y;
      	double fim[][][][];
      	int tmap[][];
      	int k;
      	double radiusArray[]; 
      	double sigmaSmoothArray[];
    	
    	diag = Math.sqrt(xDim*xDim + yDim*yDim);
    	inputImage = new ModelImage(ModelStorageBase.ARGB_FLOAT,
				srcImage.getExtents(), "changeTypeImage");
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
        
        // Compute color gradient
        
        radiusArray = new double[]{diag*lowRadius,diag*highRadius,diag*highRadius};
        sigmaSmoothArray = new double[]{diag*lowRadius,diag*highRadius,diag*highRadius};;
        cgmo(cg, theta, inputImage, radiusArray, numOrientations, smooth, sigmaSmoothArray);
        
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
			redValue = 0.2989f;
			greenValue = 0.5870f;
			blueValue = 0.1140f;

		}
		maxR = (float) inputImage.getMaxR();
		maxG = (float) inputImage.getMaxG();
		maxB = (float) inputImage.getMaxB();
		grayImage = new ModelImage(ModelStorageBase.DOUBLE,
				srcImage.getExtents(), "grayImage");
		gAlgo = new AlgorithmRGBtoGray(grayImage, inputImage,
				redValue, greenValue, blueValue, thresholdAverage,
				threshold, intensityAverage, equalRange, minR, maxR,
				minG, maxG, minB, maxB);
		gAlgo.run();
		gAlgo.finalize();
		inputImage.disposeLocal();
		inputImage = null;
		
		// Compute texture gradient
        // Must read in 286,160 byte MATLAB file unitex_6_1_2_1.4_2_64.mat
        // no = 6 the filter bank that we use for texture
        // processing. It contains six pairs of elongated, oriented
        // filters, as well as a center-surround filter.
        // ss = 1
        // ns = 2
        // sc = sqrt(2)
        // el = 2
        k = 64; // universal texture primitives called textons
        // To each pixel, we associate the vector of 13 filter responses centered at the pixel.
        fb = new double[12][2][][];
        for (x = 0; x < 1; x++) {
        	for (y = 0; y < 12; y++) {
        		fb[y][x] = new double[13][13];
        	}
        }
        for (x = 1; x < 2; x++) {
        	for (y = 0; y < 12; y++) {
        		fb[y][x] = new double[19][19];
        	}
        }
        tex = new double[24][64];
        tsim = new double[64][64];
        readFile(fb, tex, tsim);
       
        
        sliceSize = xDim * yDim;
        buffer = new double[sliceSize];
        try {
            grayImage.exportData(0, sliceSize, buffer);
        }
        catch(IOException e) {
        	e.printStackTrace();
        }
        grayImage.disposeLocal();
        grayImage = null;
        
        im = new double[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		im[y][x] = buffer[x + y * xDim];
        	}
        }
        
        fim = new double[12][2][yDim][xDim];
        fbRun(fim, fb, im);
        tmap = new int[yDim][xDim];
        assignTextons(tmap, fim, tex);
        tgmo(tg, theta, tmap, k, diag*highRadius, numOrientations, null, smooth, diag*highRadius);
        return;
      }
      
      /*
       * @param m Gradient magnitude
       * @param theta Orientation of gradient + pi/2
       *              (i.e. edge orientation)
       */
      private void detGM(double m[][], double theta[][], double sigma) {
    	  // Compute image gradient magnitude
    	  // Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
      	  // March 2003
          ModelImage grayImage = null;
          ModelImage inputImage;
          AlgorithmChangeType changeTypeAlgo;
      	  FileInfoBase[] fileInfo;
      	  double sigmaX;
      	  double sigmaY;
      	  int support;
      	  int deriv;
      	  int hsz;
      	  int sz;
      	  double fb[][][][];
      	  double fb1[][];
      	  int x;
      	  int y;
      	  double fim[][][][];
      	  double im[][];
      	  double buffer[];
      	  int sliceSize = xDim * yDim;
      	  double dx[][];
      	  double dy[][];
      	  double pretheta;
            
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
				  redValue = 0.2989f;
				  greenValue = 0.5870f;
				  blueValue = 0.1140f;
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
        
          sigma = Math.max(0.5,  sigma);
          sigmaX = sigma;
          sigmaY = sigma;
        
          // Compute x and y image derivatives
          // Use elongated Gaussian filter derivative filters
          // Calculate filter size, make sure it's odd
          support = 3;
       	  hsz = (int)Math.max(Math.ceil(sigmaX * support), Math.ceil(sigmaY * support));
          sz = 2 * hsz + 1;
          fb1 = new double[sz][sz];
          deriv = 1;
          oeFilter(fb1, sigmaX, sigmaY, support, Math.PI/2.0, deriv);
          fb = new double[2][1][sz][sz];
          for (y = 0; y < sz; y++) {
              for (x = 0; x < sz; x++) {
            	  fb[0][0][y][x] = fb1[y][x];
            	  fb[1][0][y][x] = fb1[x][y];
              }
          }
          fim = new double[2][1][yDim][xDim];
          buffer = new double[sliceSize];
          try {
        	  inputImage.exportData(0, sliceSize, buffer);
          }
          catch(IOException e) {
        	  MipavUtil.displayError("IOException " + e + " on inputImage.exportData(0, sliceSize, buffer");
        	  return;
          }
          
          inputImage.disposeLocal();
          inputImage = null;
          im = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  im[y][x] = buffer[x + y * xDim];
        	  }
          }
          fbRun(fim, fb, im);
          dx = new double[yDim][xDim];
          dy = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  dx[y][x] = fim[0][0][y][x];
        		  dy[y][x] = fim[1][0][y][x];
        	  }
          }
          
          // Compute gradient magnitude and orientation
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  m[y][x] = Math.sqrt(dx[y][x]*dx[y][x] + dy[y][x]*dy[y][x]);
        		  pretheta = Math.atan2(dy[y][x],dx[y][x]);
        		  theta[y][x] = pretheta + Math.PI/2.0 - Math.PI*Math.floor((pretheta + Math.PI/2.0)/Math.PI);
        	  }
          }
          return;
      }
      
      private void detBG(double bg[][][], double theta[]) {
          // Compute smoothed but not thinned BG fields
      	double diag;
        ModelImage grayImage = null;
        ModelImage inputImage;
        AlgorithmChangeType changeTypeAlgo;
    	FileInfoBase[] fileInfo;
          
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
  				redValue = 0.2989f;
  				greenValue = 0.5870f;
  				blueValue = 0.1140f;
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
          
          cgmo(bg, theta, inputImage, diag*lowRadius, numOrientations, smooth, diag*lowRadius);
          inputImage.disposeLocal();
          inputImage = null;   
          
          return;
        }
      
      private void detBGTG(double bg[][][], double tg[][][], double theta[]) {
        // Compute smoothed but not thinned BG and TG fields
    	double diag;
        ModelImage grayImage = null;
        ModelImage inputImage;
        AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
      	double fb[][][][];
      	double tex[][];
      	double tsim[][];
      	int sliceSize;
      	double buffer[];
      	double im[][];
      	int x;
      	int y;
      	double fim[][][][];
      	int tmap[][];
      	int k;
        
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
				redValue = 0.2989f;
				greenValue = 0.5870f;
				blueValue = 0.1140f;
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
        
        cgmo(bg, theta, inputImage, diag*lowRadius, numOrientations, smooth, diag*lowRadius);
        
        // Compute texture gradient
        // Must read in 286,160 byte MATLAB file unitex_6_1_2_1.4_2_64.mat
        // no = 6 the filter bank that we use for texture
        // processing. It contains six pairs of elongated, oriented
        // filters, as well as a center-surround filter.
        // ss = 1
        // ns = 2
        // sc = sqrt(2)
        // el = 2
        k = 64; // universal texture primitives called textons
        // To each pixel, we associate the vector of 13 filter responses centered at the pixel.
        fb = new double[12][2][][];
        for (x = 0; x < 1; x++) {
        	for (y = 0; y < 12; y++) {
        		fb[y][x] = new double[13][13];
        	}
        }
        for (x = 1; x < 2; x++) {
        	for (y = 0; y < 12; y++) {
        		fb[y][x] = new double[19][19];
        	}
        }
        tex = new double[24][64];
        tsim = new double[64][64];
        // Also has tim 64 X 1 cell and tperm 64 X 1 double
        // Read in of fb and tex confirmed on MATLAB
        readFile(fb, tex, tsim);
        
        sliceSize = xDim * yDim;
        buffer = new double[sliceSize];
        try {
           inputImage.exportData(0, sliceSize, buffer);
        }
        catch(IOException e) {
        	e.printStackTrace();
        }
        inputImage.disposeLocal();
        inputImage = null;
        
        im = new double[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		im[y][x] = buffer[x + y * xDim];
        	}
        }
        
        fim = new double[12][2][yDim][xDim];
        fbRun(fim, fb, im);                
        tmap = new int[yDim][xDim];
        assignTextons(tmap, fim, tex);
        tgmo(tg, theta, tmap, k, diag*highRadius, numOrientations, null, smooth, diag*highRadius);
        return;
      }
      
      private void detTG(double tg[][][], double theta[]) {
          // Compute smoothed but not thinned TG fields
      	double diag;
          ModelImage grayImage = null;
          ModelImage inputImage;
          AlgorithmChangeType changeTypeAlgo;
        	FileInfoBase[] fileInfo;
        	double fb[][][][];
        	double tex[][];
        	double tsim[][];
        	int sliceSize;
        	double buffer[];
        	double im[][];
        	int x;
        	int y;
        	double fim[][][][];
        	int tmap[][];
        	int k;
          
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
  				redValue = 0.2989f;
  				greenValue = 0.5870f;
  				blueValue = 0.1140f;
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
          
          // Compute texture gradient
          // Must read in 286,160 byte MATLAB file unitex_6_1_2_1.4_2_64.mat
          // no = 6 the filter bank that we use for texture
          // processing. It contains six pairs of elongated, oriented
          // filters, as well as a center-surround filter.
          // ss = 1
          // ns = 2
          // sc = sqrt(2)
          // el = 2
          k = 64; // universal texture primitives called textons
          // To each pixel, we associate the vector of 13 filter responses centered at the pixel.
          fb = new double[12][2][][];
          for (x = 0; x < 1; x++) {
          	for (y = 0; y < 12; y++) {
          		fb[y][x] = new double[13][13];
          	}
          }
          for (x = 1; x < 2; x++) {
          	for (y = 0; y < 12; y++) {
          		fb[y][x] = new double[19][19];
          	}
          }
          tex = new double[24][64];
          tsim = new double[64][64];
          // Also has tim 64 X 1 cell and tperm 64 X 1 double
          // Read in of fb and tex confirmed on MATLAB
          readFile(fb, tex, tsim);
          
          sliceSize = xDim * yDim;
          buffer = new double[sliceSize];
          try {
             inputImage.exportData(0, sliceSize, buffer);
          }
          catch(IOException e) {
          	e.printStackTrace();
          }
          inputImage.disposeLocal();
          inputImage = null;
          
          im = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
          	for (x = 0; x < xDim; x++) {
          		im[y][x] = buffer[x + y * xDim];
          	}
          }
          
          fim = new double[12][2][yDim][xDim];
          fbRun(fim, fb, im);                
          tmap = new int[yDim][xDim];
          assignTextons(tmap, fim, tex);
          tgmo(tg, theta, tmap, k, diag*highRadius, numOrientations, null, smooth, diag*highRadius);
          return;
        }
        
        private void assignTextons(int map[][], double fim[][][][], double textons[][]) {
        int d = fim.length * fim[0].length;
        int n = fim[0][0].length * fim[0][0][0].length;
        double data[][] = new double[d][n];
        double d2[][];
        int y;
        int x;
        int ys;
        int xs;
        int map1D[];
        double minValue;
        
        
        for (x = 0; x < fim[0].length; x++) {
        	for (y = 0; y < fim.length; y++) {
                for (xs = 0; xs < fim[0][0][0].length; xs++) {
        			for (ys = 0; ys < fim[0][0].length; ys++) {
        				data[y + x*fim.length][ys + xs * fim[0][0].length] = fim[y][x][ys][xs];
        			}
        		}
        	}
        }
    	d2 = new double[n][textons[0].length];
    	distsqr(d2, data, textons);
    	map1D = new int[n];
    	for (y = 0; y < n; y++) {
    	    minValue = Double.MAX_VALUE;
    	    map1D[y] = -1;
    	    for (x = 0; x < textons[0].length; x++) {
    	    	if (d2[y][x] < minValue) {
    	    		minValue = d2[y][x];
    	    		// tgso requires tmap have a minimum value of 1
    	    		map1D[y] = x+1;
    	    	}
    	    }
    	}
    	for (x = 0; x < fim[0][0][0].length; x++) {
    	    for (y = 0; y < fim[0][0].length; y++) {
    			map[y][x] = map1D[y + x * fim[0][0].length];
    		}
    	}
    	return;
    }
    
    private void distsqr(double z[][], double x[][], double y[][]) {
    	// Return matrix of all-pairs squared distances between the vectors in
    	// the columns of x and y
    	// Inputs
    	// x     dxn matrix of vectors
    	// y     dxm matrix of vectors
    	// Outputs
    	// z     nxm matrix of squared distances
    	// This routine is faster when m < n than when m > n.
    	
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	
    	// Based on dist2.m code
    	// Copyright (c) Christopher M. Bishop, Ian T. Nabney (1996, 1997)
        Matrix matx;
        Matrix maty;
        double zp[][];
        double x2p[];
        double x2[][];
        double y2p[];
        double y2[][];
        int i;
        int j;
        int n;
        int m;
        int d;
        
    	if (x.length != y.length) {
    		MipavUtil.displayError("x.length != y.length in distsqr");
    		return;
    	}
    	
    	d = x.length;
    	n = x[0].length;
    	m = y[0].length;
    	
        matx = new Matrix(x);
        maty = new Matrix(y);
        zp = ((matx.transpose()).times(maty)).getArray();
        for (i = 0; i < n; i++) {
        	for (j = 0; j < m; j++) {
        		z[i][j] = zp[i][j];
        	}
        }
        x2p = new double[n];
        for (i = 0; i < n; i++) {
        	for (j = 0; j < d; j++) {
        		x2p[i] += x[j][i]*x[j][i];
        	}
        }
        x2 = new double[n][m];
        for (i = 0; i < n; i++) {
        	for (j = 0; j < m; j++) {
        		x2[i][j] = x2p[i];
        	}
        }
        y2p = new double[m];
        for (i = 0; i < m; i++) {
        	for (j = 0; j < d; j++) {
        		y2p[i] += y[j][i]*y[j][i];
        	}
        }
        y2 = new double[n][m];
        for (i = 0; i < n; i++) {
        	for (j = 0; j < m; j++) {
        		y2[i][j] = y2p[j];
        	}
        }
        for (i = 0; i < n; i++) {
        	for (j = 0; j < m; j++) {
        		z[i][j] = x2[i][j] + y2[i][j] - 2.0 * z[i][j];
        	}
        }
        return;
    }
      
    private void readFile(double fb[][][][], double tex[][], double tsim[][]) {
    	/** 8 bit, signed */
    	final int miINT8 = 1;
    	/** 8 bit, unsigned */
    	final int miUINT8 = 2;
    	/** 16-bit, signed */
    	final int miINT16 = 3;
    	/** 16-bit, unsigned */
    	final int miUINT16 = 4;
    	/** 32-bit signed */
    	final int miINT32 = 5;
    	/** 32-bit, unsigned */
    	final int miUINT32 = 6;
    	/** IEEE 754 single format */
    	final int miSINGLE = 7;
    	/** IEEE 754 double format */
    	final int miDOUBLE = 9;
    	/** 64-bit, signed */
    	final int miINT64 = 12;
    	/** 64-bit, unsigned */
    	final int miUINT64 = 13;
    	/** MATLAB ARRAY */
    	final int miMATRIX = 14;
    	/** Compressed Data */
    	final int miCOMPRESSED = 15;
    	/** Unicode UTF-8 Encoded Character Data */
    	final int miUTF8 = 16;
    	/** Unicode UTF-16 Encoded Character Data */
    	final int miUTF16 = 17;
    	/** Unicode UTF-32 Encoded Character Data */
    	final int miUTF32 = 18;
    	// MATLAB Array Types (Classes)
    	/** Cell array */
    	final byte mxCELL_CLASS = 1;
    	/** Structure */
    	final byte mxSTRUCT_CLASS = 2;
    	/** Object */
        final byte mxOBJECT_CLASS = 3;
    	/** Character array */
    	final byte mxCHAR_CLASS = 4;
    	/** Sparse array */
    	final byte mxSPARSE_CLASS = 5;
    	/** Double precision array */
    	final byte mxDOUBLE_CLASS = 6;
    	/** Single precision array */
    	final byte mxSINGLE_CLASS = 7;
    	/** 8-bit, signed integer */
    	final byte mxINT8_CLASS = 8;
    	/** 8-bit, unsigned integer */
    	final byte mxUINT8_CLASS = 9;
    	/** 16-bit, signed integer */
    	final byte mxINT16_CLASS = 10;
    	/** 16-bit, unsigned integer */
    	final byte mxUINT16_CLASS = 11;
    	/** 32-bit, signed integer */
    	final byte mxINT32_CLASS = 12;
    	/** 32-bit, unsigned integer */
    	final byte mxUINT32_CLASS = 13;
    	/** 64-bit, signed integer */
    	final byte mxINT64_CLASS = 14;
    	/** 64-bit, unsigned integer */
    	final byte mxUINT64_CLASS = 15;
        String fileDir;
        String fileName;
        File file;
        long fileLength = 0L;
        byte firstEndianByte;
        byte secondEndianByte;
        boolean endianess = FileBase.BIG_ENDIAN;
        byte firstByte;
        byte secondByte;
        byte thirdByte;
        byte fourthByte;
        String headerTextField;
        long subsystemSpecificDataOffset;
        int version;
        long nextElementAddress;
        //int elementNumber = 0;
        int dataType;
        int elementBytes;
        int padBytes;
        byte buffer[] = null;
        int[] imageExtents = null;
        int logicalFields = 0;
        int arrayFlagsDataType;
        int arrayFlagsBytes;
        int arrayFlags;
        int arrayClass;
        @SuppressWarnings("unused")
        boolean complexFlag = false;
        @SuppressWarnings("unused")
        boolean globalFlag = false;
        @SuppressWarnings("unused")
        boolean logicalFlag = false;
        int dimensionsArrayDataType;
        int dimensionsArrayBytes;
        int nDim;
        int structureDimensions[];
        int i, j;
        int imageLength = 1;
        int newExtents[] = null;
        int arrayNameDataType;
        int arrayNameBytes;
        String arrayName;
        String arrayName2;
        int maximumFieldNameLengthBytes;
        int maximumFieldNameLengthDataType;
        int maximumFieldNameLength;
        int fieldNamesDataType;
        int fieldNamesBytes;
        int fieldNumber = 1;
        String fieldNames[] = null;
        int bytesRead;
        int field;
        int numericArrayDataType;
        int numericArrayBytes;
        int numericArrayFlagsDataType;
        int numericArrayFlagsBytes;
        int numericArrayFlags;
        int numericArrayClass;
        int numericArrayNameDataType;
        int numericArrayNameBytes;
        String numericArrayName;
        int adjustedFieldDim;
        int realDataType;
        int realDataBytes;
        boolean haveSmallRealData;
        int sliceSize;
        byte tBuffer[] = null;
        int numberSlices;
        int x;
        int y;
        int s;
        short shortBuffer[] = null;
        int index;
        int shortNumber;
        int intNumber;
        int longNumber;
        int floatNumber;
        int doubleNumber;
        int b1;
        int b2;
        int b3;
        int b4;
        long b1L;
        long b2L;
        long b3L;
        long b4L;
        long b5L;
        long b6L;
        long b7L;
        long b8L;
        int tmpInt;
        long tmpLong;
        int intBuffer[] = null;
        long longBuffer[] = null;
        float floatBuffer[] = null;
        double doubleBuffer[] = null;
        String str;
        int xm = 1;
        int ym = 1;
        int xb;
        int yb;
        
        try {
	        fileDir = "gov/nih/mipav/model/algorithms/";
	        fileName = "PbBoundaryDetection_unitex_6_1_2_1.4_2_64.mat";
	        //file = new File(fileDir + fileName);
	        final URL fileURL = Thread.currentThread().getContextClassLoader().getResource(fileDir + fileName);
	        
	        if (fileURL == null) {
	            Preferences.debug("Unable to open " + "PbBoundaryDetection_unitex_6_1_2_1.4_2_64.mat"
	                    + ".  Make sure it is in the same directory as MipavMain.class\n", Preferences.DEBUG_MINOR);

	        }
	        file = null;
	        
	        if (fileURL != null) {
                try {
                                file = new File(fileURL.toURI());
                } catch (URISyntaxException e) {
                                e.printStackTrace();
                }
	        }

	        raFile = new RandomAccessFile(file, "r");
	        fileLength = raFile.length();
	        Preferences.debug("fileLength = " + fileLength + "\n", Preferences.DEBUG_FILEIO);
	        raFile.seek(126L);
	        firstEndianByte = raFile.readByte();
	        secondEndianByte = raFile.readByte();
	        if ((firstEndianByte == 77) && (secondEndianByte == 73)) {
	        	// M followed by I
	        	endianess = FileBase.BIG_ENDIAN;
	        	Preferences.debug("The MATLAB file is big endian\n", Preferences.DEBUG_FILEIO);
	        }
	        else if ((firstEndianByte == 73) && (secondEndianByte == 77)) {
	        	// I followed by M
	        	endianess = FileBase.LITTLE_ENDIAN;
	        	Preferences.debug("The MATLAB file is little endian\n", Preferences.DEBUG_FILEIO);
	        }
	        else {
	        	raFile.close();
	        }
            
            raFile.seek(0L);
            firstByte = raFile.readByte();
            secondByte = raFile.readByte();
            thirdByte = raFile.readByte();
            fourthByte = raFile.readByte();
            if ((firstByte == 0) || (secondByte == 0) || (thirdByte == 0) || (fourthByte == 0)) {
            	 // MATLAB uses level 4 format
                 Preferences.debug("The MATLAB file uses level 4 format\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	// MATLAB uses level 5 format
            	Preferences.debug("The MATLAB file uses level 5 format\n", Preferences.DEBUG_FILEIO);
            }
            
            raFile.seek(0L);
            
            headerTextField = getString(116);
            Preferences.debug("Header text field = " + headerTextField.trim() + "\n", Preferences.DEBUG_FILEIO);
            
            // Location 116
            subsystemSpecificDataOffset = getLong(endianess);
            // All zeros or all spaces in this field indicate that there is no 
            // subsystem-specific data stored in this file
            if ((subsystemSpecificDataOffset == 0L) || (subsystemSpecificDataOffset == 0x2020202020202020L)) {
            	Preferences.debug("No subsystem specific data stored in file\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	Preferences.debug("Subystem specific data stored at location " + subsystemSpecificDataOffset + "\n", 
            			Preferences.DEBUG_FILEIO);
            }
            
            // Location 124
            version = getUnsignedShort(endianess);
            if (version == 256) {
                Preferences.debug("The version number is the expected 256\n", Preferences.DEBUG_FILEIO);	
            }
            else {
            	Preferences.debug("The version number = " + version + " instead of the expected 256\n", 
            			Preferences.DEBUG_FILEIO);
            }
            
         // Go to first data element location
            nextElementAddress = 128L;
            while (nextElementAddress < fileLength) {
                raFile.seek(nextElementAddress);
                dataType = getInt(endianess);
                if ((dataType & 0xffff0000) != 0) {
                	// Small Data Element Format
                	elementBytes = (dataType & 0xffff0000) >>> 16;
                	dataType = dataType & 0xffff;
                	nextElementAddress = nextElementAddress + 8;
                }
                else {
                    elementBytes = getInt(endianess);
                    // Must pad to make sure the tag of the next data element
                    // falls on a 64-bit boundary.
                    padBytes = 0;
                    if ((elementBytes % 8) != 0) {
                    	padBytes = 8 - (elementBytes % 8);
                    }
                    if (dataType == miCOMPRESSED) {
                        nextElementAddress = nextElementAddress + elementBytes + 8;
                    }
                    else {
                        nextElementAddress = nextElementAddress + elementBytes + padBytes + 8;
                    }
                }
                Preferences.debug("nextElementAddress = " + nextElementAddress + "\n", Preferences.DEBUG_FILEIO);
                
                switch(dataType) {
                case miINT8:
                	Preferences.debug("Data type = miINT8\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT8:
                	Preferences.debug("Data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miINT16:
                	Preferences.debug("Data type = miINT16\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT16:
                	Preferences.debug("Data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miINT32:
                	Preferences.debug("Data type = miINT32\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT32:
                	Preferences.debug("Data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miSINGLE:
                	Preferences.debug("Data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miDOUBLE:
                	Preferences.debug("Data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miINT64:
                	Preferences.debug("Data type = miINT64\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT64:
                	Preferences.debug("Data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miMATRIX:
                	Preferences.debug("Data type = miMATRIX\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	imageExtents = null;
                	logicalFields = 0;
                	arrayFlagsDataType = getInt(endianess);
                	if (arrayFlagsDataType == miUINT32) {
                		Preferences.debug("Array flags data type is the expected miUINT32\n", Preferences.DEBUG_FILEIO);
                	}
                	else {
                		Preferences.debug("Array flags data type is an unexpected " + arrayFlagsDataType + "\n", 
                				Preferences.DEBUG_FILEIO);
                	}
                    arrayFlagsBytes = getInt(endianess);
                    if (arrayFlagsBytes == 8) {
                    	Preferences.debug("Array flags byte length = 8 as expected\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	Preferences.debug("Array flags byte length is an unexpected " + arrayFlagsBytes + "\n", 
                    			Preferences.DEBUG_FILEIO);
                    }
                    arrayFlags = getInt(endianess);
                    arrayClass = arrayFlags & 0x000000ff;
                    switch(arrayClass) {
                    case mxCELL_CLASS:
                    	Preferences.debug("Array type is cell array\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxSTRUCT_CLASS:
                    	Preferences.debug("Array type is structure\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxOBJECT_CLASS:
                    	Preferences.debug("Array type is object\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxCHAR_CLASS:
                    	Preferences.debug("Array type is character\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxSPARSE_CLASS:
                    	Preferences.debug("Array type is sparse\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxDOUBLE_CLASS:
                    	Preferences.debug("Array type is 8 byte double\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxSINGLE_CLASS:
                    	Preferences.debug("Array type is 4 byte float\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT8_CLASS:
                    	Preferences.debug("Array type is signed byte\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxUINT8_CLASS:
                    	Preferences.debug("Array type is unsigned byte\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT16_CLASS:
                    	Preferences.debug("Array type is signed short\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxUINT16_CLASS:
                    	Preferences.debug("Array type is unsigned short\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT32_CLASS:
                        Preferences.debug("Array type is signed integer\n", Preferences.DEBUG_FILEIO);
                        break;
                    case mxUINT32_CLASS:
                    	Preferences.debug("Array type is unsigned integer\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT64_CLASS:
                    	Preferences.debug("Array type is signed long\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxUINT64_CLASS:
                    	Preferences.debug("Array type is unsigned long\n", Preferences.DEBUG_FILEIO);
                    	break;
                    default:
                    	Preferences.debug("Array type is an illegal = " + arrayClass + "\n", Preferences.DEBUG_FILEIO);
                    }
                    if (arrayClass == mxCHAR_CLASS) {
                    	continue;
                    }
                    
                    if ((arrayFlags & 0x00000800) != 0) {
                    	complexFlag = true;
                    	Preferences.debug("Complex flag is set\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	complexFlag = false;
                    	Preferences.debug("Complex flag is not set\n", Preferences.DEBUG_FILEIO);
                    }
                    if ((arrayFlags & 0x00000400) != 0) {
                    	globalFlag = true;
                    	Preferences.debug("Global flag is set\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	globalFlag = false;
                    	Preferences.debug("Global flag is not set\n", Preferences.DEBUG_FILEIO);
                    }
                    if ((arrayFlags & 0x00000200) != 0) {
                    	logicalFlag = true;
                    	Preferences.debug("Logical flag is set\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	logicalFlag = false;
                    	Preferences.debug("Logical flag is not set\n", Preferences.DEBUG_FILEIO);
                    }
                    // 4 undefined bytes
                	getInt(endianess);
                	dimensionsArrayDataType = getInt(endianess);
                	if (dimensionsArrayDataType == miINT32) {
                		Preferences.debug("Dimensions array data type is the expected miINT32\n", Preferences.DEBUG_FILEIO);
                	}
                	else {
                		Preferences.debug("Dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n", 
                				Preferences.DEBUG_FILEIO);
                	}
                	dimensionsArrayBytes = getInt(endianess);
                	Preferences.debug("dimensionsArrayBytes = " + dimensionsArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                	if ((dimensionsArrayBytes % 4) == 0) {
                		Preferences.debug("dimensionsArrayBytes is a multiple of 4 as expected\n", Preferences.DEBUG_FILEIO);
                	}
                	else {
                		Preferences.debug("dimensionArrayBytes is unexpectedly not a multiple of 4\n", 
                				Preferences.DEBUG_FILEIO);
                	}
                	nDim = dimensionsArrayBytes/4;
                	Preferences.debug("Number of dimensions = " + nDim + "\n", Preferences.DEBUG_FILEIO);
                	if (nDim < 2) {
                		Preferences.debug("Error! All numeric arrays should have at least 2 dimensions\n",
                				Preferences.DEBUG_FILEIO);
                	}
                	if (arrayClass == mxSTRUCT_CLASS) {
                		structureDimensions = new int[nDim];
                	    for (i = 0; i < nDim; i++) {
                	    	// Ignore structure dimensions
                	    	structureDimensions[i] = getInt(endianess);
                	    	Preferences.debug("Ignored structureDimensions[" + i + " ] = " + structureDimensions[i] + "\n", 
                	    			Preferences.DEBUG_FILEIO);
                	    }
                	}
                	else { // arrayClass != mxSTRUCT_CLASS
	                	imageExtents = new int[nDim];
	                	imageLength = 1;
	                	
	                	for (i = 0; i < nDim; i++) {
	                		if (i == 0) {
	                			imageExtents[1] = getInt(endianess);
	                			Preferences.debug("imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
	                		}
	                		else if (i == 1) {
	                			imageExtents[0] = getInt(endianess);
	                			Preferences.debug("imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
	                		}
	                		else {
	                		    imageExtents[i] = getInt(endianess);
	                		    Preferences.debug("imageExtents["+ i + "] = " + imageExtents[i] + "\n", 
	                		    		Preferences.DEBUG_FILEIO);
	                		}
	                		imageLength = imageLength * imageExtents[i];
	                	}
	                	if ((imageExtents[0] == 1) || (imageExtents[1] == 1)) {
	                    	continue;	
	                	}
	                	if ((nDim == 4) && (imageExtents[2] == 1)) {
	                		nDim = 3;
	                		newExtents = new int[3];
	                		newExtents[0] = imageExtents[0];
	                		newExtents[1] = imageExtents[1];
	                		newExtents[2] = imageExtents[3];
	                		imageExtents = new int[3];
	                		imageExtents[0] = newExtents[0];
	                		imageExtents[1] = newExtents[1];
	                		imageExtents[2] = newExtents[2];
	                	}
	                	
                	} // else arrayClass != mxSTRUCT_CLASS
                	if ((dimensionsArrayBytes % 8) != 0) {
                		// Skip over padding bytes
                		padBytes = 8 - (dimensionsArrayBytes % 8);
                		for (i = 0; i < padBytes; i++) {
                		    raFile.readByte();
                		}
                	} // if ((dimensionsArrayBytes % 8) != 0)
                	arrayNameDataType = getInt(endianess);
                    if ((arrayNameDataType & 0xffff0000) != 0) {
                        // Small data element format    
                    	arrayNameBytes = (arrayNameDataType & 0xffff0000) >>> 16;
                    	arrayNameDataType = arrayNameDataType & 0xffff;
                    	arrayName = getString(arrayNameBytes);
                    	if (arrayNameBytes < 4) {
                    		for (i = 0; i < 4 - arrayNameBytes; i++) {
                    			// Skip over padding bytes
                    			raFile.readByte();
                    		}
                    	}
                    }
                    else {
                    	arrayNameBytes = getInt(endianess);
                    	arrayName = getString(arrayNameBytes);
                    	// Skip over padding bytes
                    	if ((arrayNameBytes % 8) != 0) {
	                		padBytes = 8 - (arrayNameBytes % 8);
	                		for (i = 0; i < padBytes; i++) {
	                		    raFile.readByte();
	                		}
                    	}
                    }
                    Preferences.debug("Array name = " + arrayName + "\n", Preferences.DEBUG_FILEIO);
                    
                    if (arrayClass == mxSTRUCT_CLASS) {
                        // The field name length subelement always uses the compressed data element format
                      	maximumFieldNameLengthDataType = getInt(endianess);
                      	maximumFieldNameLengthBytes = (maximumFieldNameLengthDataType & 0xffff0000) >>> 16;
                      	maximumFieldNameLengthDataType = maximumFieldNameLengthDataType & 0xffff;
                      	if (maximumFieldNameLengthDataType == miINT32) {
                      		Preferences.debug("maximumFieldNameLengthDataType == miINT32 as expected\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	else {
                      		Preferences.debug("maximumFieldNameLengthDataType unexpectedly == " + 
                      				maximumFieldNameLengthDataType + "\n", Preferences.DEBUG_FILEIO);
                      	}
                      	if (maximumFieldNameLengthBytes == 4) {
                      		Preferences.debug("maximumFieldNameLengthBytes == 4 as expected\n", Preferences.DEBUG_FILEIO);
                      	}
                      	else {
                      		Preferences.debug("maximumFieldNameLengthBytes == " + maximumFieldNameLengthBytes +
                      				          " instead of the expected 4\n", Preferences.DEBUG_FILEIO);
                      	}
                      	maximumFieldNameLength = getInt(endianess);
                      	Preferences.debug("maximumFieldNameLength including null terminator = " + 
                      			maximumFieldNameLength + "\n", Preferences.DEBUG_FILEIO);
                      	if (maximumFieldNameLength > 32) {
                      		Preferences.debug("maximumFieldNameLength should not be greater than 32\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	fieldNamesDataType = getInt(endianess);
                      	if (fieldNamesDataType == miINT8) {
                      		Preferences.debug("fieldNamesDataType == miINT8 as expected\n", Preferences.DEBUG_FILEIO);
                      	}
                      	else {
                      		Preferences.debug("fieldNamesDataType unexpectely == " + fieldNamesDataType + "\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	fieldNamesBytes = getInt(endianess);
                      	Preferences.debug("fieldNamesBytes = " + fieldNamesBytes + "\n", Preferences.DEBUG_FILEIO);
                      	if ((fieldNamesBytes % maximumFieldNameLength) == 0) {
                      		Preferences.debug("fieldNamesBytes % maximumFieldNameLength == 0 as expected\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	else {
                      		Preferences.debug("fieldNamesBytes % maximumFieldNameLength unexpectedly == " +
                      				(fieldNamesBytes % maximumFieldNameLength) + "\n", Preferences.DEBUG_FILEIO);
                      	}
                      	fieldNumber = fieldNamesBytes / maximumFieldNameLength;
                      	Preferences.debug("Field number = " + fieldNumber + "\n", Preferences.DEBUG_FILEIO);
                      	fieldNames = new String[fieldNumber];
                      	for (i = 0; i < fieldNumber; i++) {
                      	    fieldNames[i] = readCString();
                      	    Preferences.debug("field name " + i + " = " + fieldNames[i] + "\n", Preferences.DEBUG_FILEIO);
                      	    bytesRead = fieldNames[i].length() + 1;
                      	    padBytes = maximumFieldNameLength - bytesRead;
                      	    for (j = 0; j < padBytes; j++) {
                      	    	raFile.readByte();
                      	    }
                      	}
                      	
                      	if ((fieldNamesBytes % 8) != 0) {
                      	    padBytes = 8 - (fieldNamesBytes % 8);
                      	    for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                      	}
                      } // if (arrayClass == mxSTRUCT_CLASS)
                      for (field = 0; field < fieldNumber; field++) {
                      if (arrayClass == mxSTRUCT_CLASS) {
                      	Preferences.debug("Reading numeric array number " + field + "\n", Preferences.DEBUG_FILEIO);
                          numericArrayDataType = getInt(endianess);
                          if (numericArrayDataType == miMATRIX) {
                          	Preferences.debug("Numeric array data type == miMATRIX as expected\n");
                          }
                          else {
                          	Preferences.debug("Numeric array data type unexpectedly == " + numericArrayDataType + "\n", 
                          			Preferences.DEBUG_FILEIO);
                          }
                          numericArrayBytes= getInt(endianess);
                          Preferences.debug("Numeric array bytes = " + numericArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                          numericArrayFlagsDataType = getInt(endianess);
                      	if (arrayFlagsDataType == miUINT32) {
                      		Preferences.debug("Numeric array flags data type is the expected miUINT32\n",
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	else {
                      		Preferences.debug("Numeric array flags data type is an unexpected " + numericArrayFlagsDataType +
                      				"\n", Preferences.DEBUG_FILEIO);
                      	}
                          numericArrayFlagsBytes = getInt(endianess);
                          if (numericArrayFlagsBytes == 8) {
                          	Preferences.debug("Numeric array flags byte length = 8 as expected\n", Preferences.DEBUG_FILEIO);
                          }
                          else {
                          	Preferences.debug("Numeric array flags byte length is an unexpected " + numericArrayFlagsBytes +
                          			"\n", Preferences.DEBUG_FILEIO);
                          }
                          numericArrayFlags = getInt(endianess);
                          numericArrayClass = numericArrayFlags & 0x000000ff;
                          switch(numericArrayClass) {
                          case mxCELL_CLASS:
                          	Preferences.debug("Numeric array type is cell array\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxSTRUCT_CLASS:
                          	Preferences.debug("Numeric array type is structure\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxOBJECT_CLASS:
                          	Preferences.debug("Numeric array type is object\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxCHAR_CLASS:
                          	Preferences.debug("Numeric array type is character\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxSPARSE_CLASS:
                          	Preferences.debug("Numereic array type is sparse\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxDOUBLE_CLASS:
                          	Preferences.debug("Numeric array type is 8 byte float\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxSINGLE_CLASS:
                          	Preferences.debug("Numeric array type is 4 byte float\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxINT8_CLASS:
                          	Preferences.debug("Numeric array type is signed byte\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxUINT8_CLASS:
                          	Preferences.debug("Numeric array type is unsigned byte\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxINT16_CLASS:
                          	Preferences.debug("Numeric array type is signed short\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxUINT16_CLASS:
                          	Preferences.debug("Numeric array type is unsigned short\n", Preferences.DEBUG_FILEIO);
                          	break;
                          case mxINT32_CLASS:
                              Preferences.debug("Numeric array type is signed integer\n", Preferences.DEBUG_FILEIO);
                              break;
                          case mxUINT32_CLASS:
                          	Preferences.debug("Numeric array type is unsigned integer\n", Preferences.DEBUG_FILEIO);
                          	break;
                          default:
                          	Preferences.debug("Numeric array type is an illegal = " + numericArrayClass + "\n", 
                          			Preferences.DEBUG_FILEIO);
                          }
                          
                          if ((numericArrayFlags & 0x00000800) != 0) {
                          	complexFlag = true;
                          	Preferences.debug("Complex flag is set\n", Preferences.DEBUG_FILEIO);
                          }
                          else {
                          	complexFlag = false;
                          	Preferences.debug("Complex flag is not set\n", Preferences.DEBUG_FILEIO);
                          }
                          if ((numericArrayFlags & 0x00000400) != 0) {
                          	globalFlag = true;
                          	Preferences.debug("Global flag is set\n", Preferences.DEBUG_FILEIO);
                          }
                          else {
                          	globalFlag = false;
                          	Preferences.debug("Global flag is not set\n", Preferences.DEBUG_FILEIO);
                          }
                          if ((numericArrayFlags & 0x00000200) != 0) {
                          	logicalFlag = true;
                          	Preferences.debug("Logical flag is set\n", Preferences.DEBUG_FILEIO);
                          	logicalFields++;
                          }
                          else {
                          	logicalFlag = false;
                          	Preferences.debug("Logical flag is not set\n", Preferences.DEBUG_FILEIO);
                          }
                          
                          // 4 undefined bytes
                      	getInt(endianess);
                      	dimensionsArrayDataType = getInt(endianess);
                      	if (dimensionsArrayDataType == miINT32) {
                      		Preferences.debug("Dimensions array data type is the expected miINT32\n", Preferences.DEBUG_FILEIO);
                      	}
                      	else {
                      		Preferences.debug("Dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	dimensionsArrayBytes = getInt(endianess);
                      	Preferences.debug("dimensionsArrayBytes = " + dimensionsArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                      	if ((dimensionsArrayBytes % 4) == 0) {
                      		Preferences.debug("dimensionsArrayBytes is a multiple of 4 as expected\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	else {
                      		Preferences.debug("dimensionArrayBytes is unexpectedly not a multiple of 4\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	nDim = dimensionsArrayBytes/4;
                      	Preferences.debug("Number of dimensions = " + nDim + "\n", Preferences.DEBUG_FILEIO);
                      	if (nDim < 2) {
                      		Preferences.debug("Error! All numeric arrays should have at least 2 dimensions\n", 
                      				Preferences.DEBUG_FILEIO);
                      	}
                      	
                    	imageExtents = new int[nDim+1];
	                	imageLength = 1;
	                	
	                	for (i = 0; i < nDim; i++) {
	                		if (i == 0) {
	                			imageExtents[1] = getInt(endianess);
	                			Preferences.debug("imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
	                		}
	                		else if (i == 1) {
	                			imageExtents[0] = getInt(endianess);
	                			Preferences.debug("imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
	                		}
	                		else {
	                		    imageExtents[i] = getInt(endianess);
	                		    Preferences.debug("imageExtents["+ i + "] = " + imageExtents[i] + "\n", 
	                		    		Preferences.DEBUG_FILEIO);
	                		}
	                		imageLength = imageLength * imageExtents[i];
	                		
	                	}
	                	
	                	
	                	// Note that imageLength only includes slices in one field of a structure
	                	imageExtents[nDim] = fieldNumber;
	                	Preferences.debug("imageExtents[" + nDim + "] = " + imageExtents[nDim] + "\n", 
	                			Preferences.DEBUG_FILEIO);
	                	
  	                	
  	                	if ((dimensionsArrayBytes % 8) != 0) {
  	                		// Skip over padding bytes
  	                		padBytes = 8 - (dimensionsArrayBytes % 8);
  	                		for (i = 0; i < padBytes; i++) {
  	                		    raFile.readByte();
  	                		}
  	                	} // if ((dimensionsArrayBytes % 8) != 0)
  	                	
  	                	numericArrayNameDataType = getInt(endianess);
  	                    if ((numericArrayNameDataType & 0xffff0000) != 0) {
  	                        // Small data element format    
  	                    	numericArrayNameBytes = (numericArrayNameDataType & 0xffff0000) >>> 16;
  	                    	numericArrayNameDataType = numericArrayNameDataType & 0xffff;
  	                    	numericArrayName = getString(numericArrayNameBytes);
  	                    	if (numericArrayNameBytes < 4) {
  	                    		for (i = 0; i < 4 - numericArrayNameBytes; i++) {
  	                    			// Skip over padding bytes
  	                    			raFile.readByte();
  	                    		}
  	                    	}
  	                    }
  	                    else {
  	                    	numericArrayNameBytes = getInt(endianess);
  	                    	numericArrayName = getString(numericArrayNameBytes);
  	                    	// Skip over padding bytes
  	                    	if (numericArrayNameBytes > 0) {
  	                		    padBytes = 8 - (numericArrayNameBytes % 8);
  	                		    for (i = 0; i < padBytes; i++) {
  	                		       raFile.readByte();
  	                		    }
  	                    	}
  	                    }
  	                    Preferences.debug("Numeric array name = " + numericArrayName + "\n", Preferences.DEBUG_FILEIO);
                      } // if (arrayClass == mxSTRUCT_CLASS)
                      if (arrayName.equals("fb")) {
                    	  ym = imageExtents[1];
                    	  xm = imageExtents[0];
                      }
                      else {
                    	  ym = 1;
                    	  xm = 1;
                      }
                      for (xb = 0; xb < xm; xb++) {
                    		for (yb = 0; yb < ym; yb++) {
                      realDataType = getInt(endianess);
                      if ((realDataType & 0xffff0000) != 0) {
                          // Small data element format    
                      	realDataBytes = (realDataType & 0xffff0000) >>> 16;
                      	realDataType = realDataType & 0xffff;
                      	haveSmallRealData = true;
                      }
                      else {
                          realDataBytes = getInt(endianess);
                          haveSmallRealData = false;
                      }
                      
                      sliceSize = imageExtents[0] * imageExtents[1];
                     
                      switch(realDataType) {
                      case miINT8:
                      	Preferences.debug("Real data type = miINT8\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                  		
                  		buffer = new byte[realDataBytes];
                    	raFile.read(buffer);
                    	tBuffer = new byte[buffer.length];
                  		numberSlices = buffer.length/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
                                      tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
	                    		}
	                    	}
                    	}
                    	if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                      	    }  
                      	
                      	break;
                      case miUINT8:
                      	Preferences.debug("Real data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                      	
                    	buffer = new byte[realDataBytes];
                    	raFile.read(buffer);
                    	shortBuffer = new short[realDataBytes];
                    	numberSlices = buffer.length/sliceSize;
                  	    j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
                                      shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
	                    		}
	                    	}
                    	}
                    	
                    	if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miINT16:
                      	Preferences.debug("Real data type = miINT16\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                  		
                  		buffer = new byte[realDataBytes];
                  		raFile.read(buffer);
                  		shortNumber = realDataBytes/2;
                  		shortBuffer = new short[shortNumber];
                  		numberSlices = shortNumber/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
	                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                      b1 = buffer[index] & 0xff;
                                      b2 = buffer[index+1] & 0xff;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	shortBuffer[j++] = (short)((b1 << 8) | b2);	
                                      }
                                      else {
                                      	shortBuffer[j++] = (short)((b2 << 8) | b1);
                                      }
	                    		}
	                    	}
                    	}
                  		 
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miUINT16:
                      	Preferences.debug("Real data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                      	
                  		buffer =  new byte[realDataBytes];
                  		raFile.read(buffer);
                  		shortNumber = realDataBytes/2;
                  		intBuffer = new int[shortNumber];
                  		numberSlices = shortNumber/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
	                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                      b1 = buffer[index] & 0xff;
                                      b2 = buffer[index+1] & 0xff;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	intBuffer[j++] = ((b1 << 8) | b2);	
                                      }
                                      else {
                                      	intBuffer[j++] = ((b2 << 8) | b1);
                                      }
	                    		}
	                    	}
                    	}
                  		 
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miINT32:
                      	Preferences.debug("Real data type = miINT32\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    
                  		buffer = new byte[realDataBytes];
                  		raFile.read(buffer);
                  		intNumber = realDataBytes/4;
                  		intBuffer = new int[intNumber];
                  		numberSlices = intNumber/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
	                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                      b1 = buffer[index] & 0xff;
                                      b2 = buffer[index+1] & 0xff;
                                      b3 = buffer[index+2] & 0xff;
                                      b4 = buffer[index+3] & 0xff;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	intBuffer[j++] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                      }
                                      else {
                                      	intBuffer[j++] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                      }
	                    		}
	                    	}
                    	}
                  		
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miUINT32:
                      	Preferences.debug("Real data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                 
                  		buffer = new byte[realDataBytes];
                  		raFile.read(buffer);
                  		intNumber = realDataBytes/4;
                  		longBuffer = new long[intNumber];
                  		numberSlices = intNumber/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
	                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                      b1L = buffer[index] & 0xffL;
                                      b2L = buffer[index+1] & 0xffL;
                                      b3L = buffer[index+2] & 0xffL;
                                      b4L = buffer[index+3] & 0xffL;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	longBuffer[j++] = ((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                      }
                                      else {
                                      	longBuffer[j++] = ((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                      }
	                    		}
	                    	}
                    	}
                  		
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miSINGLE:
                      	Preferences.debug("Real data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                
                  		buffer = new byte[realDataBytes];
                  		raFile.read(buffer);
                  		floatNumber = realDataBytes/4;
                  		floatBuffer = new float[floatNumber];
                  		numberSlices = floatNumber/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
	                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                      b1 = buffer[index] & 0xff;
                                      b2 = buffer[index+1] & 0xff;
                                      b3 = buffer[index+2] & 0xff;
                                      b4 = buffer[index+3] & 0xff;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                      }
                                      else {
                                      	tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                      }
                                      floatBuffer[j++] = Float.intBitsToFloat(tmpInt);
	                    		}
	                    	}
                    	}
                  		
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miDOUBLE:
                      	Preferences.debug("Real data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                      	
                  		buffer = new byte[realDataBytes];
                  		raFile.read(buffer);
                  		doubleNumber = realDataBytes/8;
                  		doubleBuffer = new double[doubleNumber];
                  		numberSlices = doubleNumber/sliceSize;
                  		if (arrayName.equals("tex")) {
                  			for (x = 0; x < imageExtents[0]; x++) {
	                    		for (y = 0; y < imageExtents[1]; y++) {
	                    			index = 8*(x * imageExtents[1] + y);
                                      b1L = buffer[index] & 0xffL;
                                      b2L = buffer[index+1] & 0xffL;
                                      b3L = buffer[index+2] & 0xffL;
                                      b4L = buffer[index+3] & 0xffL;
                                      b5L = buffer[index+4] & 0xffL;
                                      b6L = buffer[index+5] & 0xffL;
                                      b7L = buffer[index+6] & 0xffL;
                                      b8L = buffer[index+7] & 0xffL;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                      }
                                      else {
                                      	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                      }
                                      tex[y][x] = Double.longBitsToDouble(tmpLong);
	                    		}
	                    	}
                    	}
                  		else if (arrayName.equals("tsim")) {
                  			for (x = 0; x < imageExtents[0]; x++) {
	                    		for (y = 0; y < imageExtents[1]; y++) {
	                    			index = 8*(x * imageExtents[1] + y);
                                      b1L = buffer[index] & 0xffL;
                                      b2L = buffer[index+1] & 0xffL;
                                      b3L = buffer[index+2] & 0xffL;
                                      b4L = buffer[index+3] & 0xffL;
                                      b5L = buffer[index+4] & 0xffL;
                                      b6L = buffer[index+5] & 0xffL;
                                      b7L = buffer[index+6] & 0xffL;
                                      b8L = buffer[index+7] & 0xffL;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                      }
                                      else {
                                      	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                      }
                                      tsim[y][x] = Double.longBitsToDouble(tmpLong);
	                    		}
	                    	}
                    	}	
                  		else {
                  		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
	                                      b1L = buffer[index] & 0xffL;
	                                      b2L = buffer[index+1] & 0xffL;
	                                      b3L = buffer[index+2] & 0xffL;
	                                      b4L = buffer[index+3] & 0xffL;
	                                      b5L = buffer[index+4] & 0xffL;
	                                      b6L = buffer[index+5] & 0xffL;
	                                      b7L = buffer[index+6] & 0xffL;
	                                      b8L = buffer[index+7] & 0xffL;
	                                      if (endianess == FileBase.BIG_ENDIAN) {
	                                      	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
	                                      }
	                                      else {
	                                      	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
	                                      }
	                                      doubleBuffer[j++] = Double.longBitsToDouble(tmpLong);
		                    		}
		                    	}
	                    	}
                  		}
                  		
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miINT64:
                      	Preferences.debug("Real data type = miINT64\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
           
                  		buffer = new byte[realDataBytes];
                  		raFile.read(buffer);
                  		longNumber = realDataBytes/8;
                  		longBuffer = new long[longNumber];
                  		numberSlices = longNumber/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
	                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                      b1L = buffer[index] & 0xffL;
                                      b2L = buffer[index+1] & 0xffL;
                                      b3L = buffer[index+2] & 0xffL;
                                      b4L = buffer[index+3] & 0xffL;
                                      b5L = buffer[index+4] & 0xffL;
                                      b6L = buffer[index+5] & 0xffL;
                                      b7L = buffer[index+6] & 0xffL;
                                      b8L = buffer[index+7] & 0xffL;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                      }
                                      else {
                                      	longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                      }
	                    		}
	                    	}
                    	}
                  		
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miUINT64:
                      	Preferences.debug("Real data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                     
                  		buffer = new byte[realDataBytes];
                  		raFile.read(buffer);
                  		longNumber = realDataBytes/8;
                  		longBuffer = new long[longNumber];
                  		numberSlices = longNumber/sliceSize;
                  		j = 0;
                    	for (s = 0; s < numberSlices; s++) {
	                    	for (x = 0; x < imageExtents[1]; x++) {
	                    		for (y = 0; y < imageExtents[0]; y++) {
	                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                      b1L = buffer[index] & 0xffL;
                                      b2L = buffer[index+1] & 0xffL;
                                      b3L = buffer[index+2] & 0xffL;
                                      b4L = buffer[index+3] & 0xffL;
                                      b5L = buffer[index+4] & 0xffL;
                                      b6L = buffer[index+5] & 0xffL;
                                      b7L = buffer[index+6] & 0xffL;
                                      b8L = buffer[index+7] & 0xffL;
                                      if (endianess == FileBase.BIG_ENDIAN) {
                                      	longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                      }
                                      else {
                                      	longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                      }
	                    		}
	                    	}
                    	}
                  		
                  		if (haveSmallRealData) {
                  		    if (realDataBytes < 4) {
                  		    	padBytes = 4 - realDataBytes;
                  		    	for (i = 0; i < padBytes; i++) {
                  		    		raFile.readByte();
                  		    	}
                  		    }
                  		}
                  		else if ((realDataBytes % 8) != 0) {
                  	    	padBytes = 8 - (realDataBytes % 8);
                  	    	for (i = 0; i < padBytes; i++) {
                      	    	raFile.readByte();
                      	    }
                  	    }  
                      	
                      	break;
                      case miMATRIX:
                    	  Preferences.debug("fb real data type = miMATRIX\n", Preferences.DEBUG_FILEIO);
                          Preferences.debug("fb real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                
                    	  
                    	  
                    		  arrayFlagsDataType = getInt(endianess);
	                          if (arrayFlagsDataType == miUINT32) {
	                          		Preferences.debug("fb array flags data type is the expected miUINT32\n", Preferences.DEBUG_FILEIO);
	                          }
	                          else {
	                          		Preferences.debug("fb array flags data type is an unexpected " + arrayFlagsDataType + "\n", 
	                          				Preferences.DEBUG_FILEIO);
	                          }
                              arrayFlagsBytes = getInt(endianess);
                              if (arrayFlagsBytes == 8) {
                              	Preferences.debug("fb array flags byte length = 8 as expected\n", Preferences.DEBUG_FILEIO);
                              }
                              else {
                              	Preferences.debug("fb array flags byte length is an unexpected " + arrayFlagsBytes + "\n", 
                              			Preferences.DEBUG_FILEIO);
                              }
                              arrayFlags = getInt(endianess);
                              arrayClass = arrayFlags & 0x000000ff;
                              switch(arrayClass) {
                              case mxCELL_CLASS:
                              	Preferences.debug("fb array type is cell array\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxSTRUCT_CLASS:
                              	Preferences.debug("fb array type is structure\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxOBJECT_CLASS:
                              	Preferences.debug("fb array type is object\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxCHAR_CLASS:
                              	Preferences.debug("fb array type is character\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxSPARSE_CLASS:
                              	Preferences.debug("fb array type is sparse\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxDOUBLE_CLASS:
                              	Preferences.debug("fb array type is 8 byte double\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxSINGLE_CLASS:
                              	Preferences.debug("fb array type is 4 byte float\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxINT8_CLASS:
                              	Preferences.debug("fb array type is signed byte\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxUINT8_CLASS:
                              	Preferences.debug("fb array type is unsigned byte\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxINT16_CLASS:
                              	Preferences.debug("fb array type is signed short\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxUINT16_CLASS:
                              	Preferences.debug("fb array type is unsigned short\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxINT32_CLASS:
                                  Preferences.debug("fb array type is signed integer\n", Preferences.DEBUG_FILEIO);
                                  break;
                              case mxUINT32_CLASS:
                              	Preferences.debug("fb array type is unsigned integer\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxINT64_CLASS:
                              	Preferences.debug("fb array type is signed long\n", Preferences.DEBUG_FILEIO);
                              	break;
                              case mxUINT64_CLASS:
                              	Preferences.debug("fb array type is unsigned long\n", Preferences.DEBUG_FILEIO);
                              	break;
                              default:
                              	Preferences.debug("fb array type is an illegal = " + arrayClass + "\n", Preferences.DEBUG_FILEIO);
                              }
                              if (arrayClass == mxCHAR_CLASS) {
                              	continue;
                              }
                              
                              if ((arrayFlags & 0x00000800) != 0) {
                              	complexFlag = true;
                              	Preferences.debug("fb complex flag is set\n", Preferences.DEBUG_FILEIO);
                              }
                              else {
                              	complexFlag = false;
                              	Preferences.debug("fb complex flag is not set\n", Preferences.DEBUG_FILEIO);
                              }
                              if ((arrayFlags & 0x00000400) != 0) {
                              	globalFlag = true;
                              	Preferences.debug("fb global flag is set\n", Preferences.DEBUG_FILEIO);
                              }
                              else {
                              	globalFlag = false;
                              	Preferences.debug("fb global flag is not set\n", Preferences.DEBUG_FILEIO);
                              }
                              if ((arrayFlags & 0x00000200) != 0) {
                              	logicalFlag = true;
                              	Preferences.debug("fb logical flag is set\n", Preferences.DEBUG_FILEIO);
                              }
                              else {
                              	logicalFlag = false;
                              	Preferences.debug("fb logical flag is not set\n", Preferences.DEBUG_FILEIO);
                              }
                              // 4 undefined bytes
                          	getInt(endianess);
                          	dimensionsArrayDataType = getInt(endianess);
                          	if (dimensionsArrayDataType == miINT32) {
                          		Preferences.debug("fb dimensions array data type is the expected miINT32\n", Preferences.DEBUG_FILEIO);
                          	}
                          	else {
                          		Preferences.debug("fb dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n", 
                          				Preferences.DEBUG_FILEIO);
                          	}
                          	dimensionsArrayBytes = getInt(endianess);
                          	Preferences.debug("fb dimensionsArrayBytes = " + dimensionsArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                          	if ((dimensionsArrayBytes % 4) == 0) {
                          		Preferences.debug("fb dimensionsArrayBytes is a multiple of 4 as expected\n", Preferences.DEBUG_FILEIO);
                          	}
                          	else {
                          		Preferences.debug("fb dimensionArrayBytes is unexpectedly not a multiple of 4\n", 
                          				Preferences.DEBUG_FILEIO);
                          	}
                          	nDim = dimensionsArrayBytes/4;
                          	Preferences.debug("fb number of dimensions = " + nDim + "\n", Preferences.DEBUG_FILEIO);
                          	if (nDim < 2) {
                          		Preferences.debug("Error! All fb numeric arrays should have at least 2 dimensions\n",
                          				Preferences.DEBUG_FILEIO);
                          	}
                          	if (arrayClass == mxSTRUCT_CLASS) {
                          		structureDimensions = new int[nDim];
                          	    for (i = 0; i < nDim; i++) {
                          	    	// Ignore structure dimensions
                          	    	structureDimensions[i] = getInt(endianess);
                          	    	Preferences.debug("fb ignored structureDimensions[" + i + " ] = " + structureDimensions[i] + "\n", 
                          	    			Preferences.DEBUG_FILEIO);
                          	    }
                          	}
                          	else { // arrayClass != mxSTRUCT_CLASS
          	                	imageExtents = new int[nDim];
          	                	imageLength = 1;
          	                	
          	                	for (i = 0; i < nDim; i++) {
          	                		if (i == 0) {
          	                			imageExtents[1] = getInt(endianess);
          	                			Preferences.debug("fb imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
          	                		}
          	                		else if (i == 1) {
          	                			imageExtents[0] = getInt(endianess);
          	                			Preferences.debug("fb imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
          	                		}
          	                		else {
          	                		    imageExtents[i] = getInt(endianess);
          	                		    Preferences.debug("fb imageExtents["+ i + "] = " + imageExtents[i] + "\n", 
          	                		    		Preferences.DEBUG_FILEIO);
          	                		}
          	                		imageLength = imageLength * imageExtents[i];
          	                	}
          	                	if ((imageExtents[0] == 1) || (imageExtents[1] == 1)) {
          	                    	continue;	
          	                	}
          	                	if ((nDim == 4) && (imageExtents[2] == 1)) {
          	                		nDim = 3;
          	                		newExtents = new int[3];
          	                		newExtents[0] = imageExtents[0];
          	                		newExtents[1] = imageExtents[1];
          	                		newExtents[2] = imageExtents[3];
          	                		imageExtents = new int[3];
          	                		imageExtents[0] = newExtents[0];
          	                		imageExtents[1] = newExtents[1];
          	                		imageExtents[2] = newExtents[2];
          	                	}
          	                	
                          	} // else arrayClass != mxSTRUCT_CLASS
                          	if ((dimensionsArrayBytes % 8) != 0) {
                          		// Skip over padding bytes
                          		padBytes = 8 - (dimensionsArrayBytes % 8);
                          		for (i = 0; i < padBytes; i++) {
                          		    raFile.readByte();
                          		}
                          	} // if ((dimensionsArrayBytes % 8) != 0)
                          	arrayNameDataType = getInt(endianess);
                              if ((arrayNameDataType & 0xffff0000) != 0) {
                                  // Small data element format    
                              	arrayNameBytes = (arrayNameDataType & 0xffff0000) >>> 16;
                              	arrayNameDataType = arrayNameDataType & 0xffff;
                              	arrayName2 = getString(arrayNameBytes);
                              	if (arrayNameBytes < 4) {
                              		for (i = 0; i < 4 - arrayNameBytes; i++) {
                              			// Skip over padding bytes
                              			raFile.readByte();
                              		}
                              	}
                              }
                              else {
                              	arrayNameBytes = getInt(endianess);
                              	Preferences.debug("fb array name bytes = " + arrayNameBytes + "\n", Preferences.DEBUG_FILEIO);
                              	arrayName2 = getString(arrayNameBytes);
                              	// Skip over padding bytes
                              	if ((arrayNameBytes % 8) != 0) {
          	                		padBytes = 8 - (arrayNameBytes % 8);
          	                		for (i = 0; i < padBytes; i++) {
          	                		    raFile.readByte();
          	                		}
                              	}
                              }
                              if (arrayNameBytes > 0) {
                                  Preferences.debug("fb array name = " + arrayName2 + "\n", Preferences.DEBUG_FILEIO);
                              }
                              realDataType = getInt(endianess);
                              if ((realDataType & 0xffff0000) != 0) {
                                  // Small data element format    
                              	realDataBytes = (realDataType & 0xffff0000) >>> 16;
                              	realDataType = realDataType & 0xffff;
                              	haveSmallRealData = true;
                              }
                              else {
                                  realDataBytes = getInt(endianess);
                                  haveSmallRealData = false;
                              }
                              if (realDataType == miDOUBLE) {
                            	  Preferences.debug("fb real data type = the expected miDOUBLE\n", Preferences.DEBUG_FILEIO);
                              }
                              else {Preferences.debug("fb real data type unexpectedly = " + realDataType + "\n",
                            		  Preferences.DEBUG_FILEIO);
                            	  
                              }
                              if (xb < 6 &&realDataBytes == 1352) {
                            	  Preferences.debug("fb real data bytes = 1352 as expected\n", Preferences.DEBUG_FILEIO);
                              }
                              else if (realDataBytes == 2888) {
                            	  Preferences.debug("fb real data bytes = 2888 as expected\n", Preferences.DEBUG_FILEIO);  
                              }
                              else {
                            	  Preferences.debug("fb real data bytes unexpectedly = " + realDataBytes + "\n",
                            			  Preferences.DEBUG_FILEIO);
                              }
                              buffer = new byte[realDataBytes];
                        	  raFile.read(buffer);
                        	  if (arrayName.equals("fb")) {
                        	  for (x = 0; x < imageExtents[0]; x++) {
  	                    		for (y = 0; y < imageExtents[1]; y++) {
  	                    			index = 8*(x * imageExtents[1] + y);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == FileBase.BIG_ENDIAN) {
                                        	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                     (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                     (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
                                        fb[yb][xb][y][x] = Double.longBitsToDouble(tmpLong);
  	                    		}
  	                    	}
                    	  }
                    	  if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    } 
                    	  
               
                    	  break;
                      default:
                      	Preferences.debug("Illegal data type = " + realDataType + "\n", Preferences.DEBUG_FILEIO);
                      	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                      }
                    		} // for (yb = 0; yb < ym; yb++)
                      } // for (xb = 0; xb < xm; xb++)
                      
                      
                      } // for (field = 0; field < fieldNumber; field++)
                      if (logicalFields >= 1) {
                  	    adjustedFieldDim = imageExtents[imageExtents.length-1] - logicalFields;
                  	    if (adjustedFieldDim >= 2) {
                  	    	newExtents = new int[imageExtents.length];
                  	    	for (i = 0; i < imageExtents.length - 1; i++) {
                  	    		newExtents[i] = imageExtents[i];
                  	    	}
                  	    	newExtents[imageExtents.length-1] = adjustedFieldDim;
                  	    } // if (adjustedFieldDim >= 2)
                  	    else {
                  	    	newExtents = new int[imageExtents.length-1];
                  	    	for (i = 0; i < imageExtents.length - 1; i++) {
                  	    		newExtents[i] = imageExtents[i];
                  	    	}
                  	    }
                  	} // if (logicalFields >= 1)
                     
                  	break;
                  case miUTF8:
                  	Preferences.debug("Data type = miUTF8\n", Preferences.DEBUG_FILEIO);
                  	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                  	buffer = new byte[elementBytes];
                  	raFile.read(buffer);
                  	str = new String(buffer, 0, elementBytes, "UTF-8");
                  	Preferences.debug("UTF-8 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                  	break;
                  case miUTF16:
                  	Preferences.debug("Data type = miUTF16\n", Preferences.DEBUG_FILEIO);
                  	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                  	buffer = new byte[elementBytes];
                  	raFile.read(buffer);
                  	if (endianess == FileBase.BIG_ENDIAN) {
                  	    str = new String(buffer, 0, elementBytes, "UTF-16BE");
                  	}
                  	else {
                  		str = new String(buffer, 0, elementBytes, "UTF-16LE");	
                  	}
                  	Preferences.debug("UTF-16 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                  	break;
                  case miUTF32:
                  	Preferences.debug("Data type = miUTF32\n", Preferences.DEBUG_FILEIO);
                  	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                  	buffer = new byte[elementBytes];
                  	raFile.read(buffer);
                  	if (endianess == FileBase.BIG_ENDIAN) {
                  	    str = new String(buffer, 0, elementBytes, "UTF-32BE");
                  	}
                  	else {
                  		str = new String(buffer, 0, elementBytes, "UTF-32LE");	
                  	}
                  	Preferences.debug("UTF-32 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                  	break;
                  default:
                  	Preferences.debug("Illegal data type = " + dataType + "\n", Preferences.DEBUG_FILEIO);
                  	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                  }
              } // while (nextElementAddress)
              raFile.close();
             
        }
        catch(IOException e) {
        	e.printStackTrace();
        	return;
        }
        
    }
    
    /**
     * Reads a string from a file of given <code>length</code>.
     * 
     * @param length Number of bytes that form the string.
     * 
     * @return The string read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final String getString(final int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        final byte[] b = new byte[length];
        raFile.readFully(b);

        return new String(b);
    }
    
    /**
     * Reads eight unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the long read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final long getLong(final boolean bigEndian) throws IOException {

        raFile.readFully(byteLongBuffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (byteLongBuffer[0] & 0xffL) << 56) | ( (byteLongBuffer[1] & 0xffL) << 48)
                    | ( (byteLongBuffer[2] & 0xffL) << 40) | ( (byteLongBuffer[3] & 0xffL) << 32)
                    | ( (byteLongBuffer[4] & 0xffL) << 24) | ( (byteLongBuffer[5] & 0xffL) << 16)
                    | ( (byteLongBuffer[6] & 0xffL) << 8) | (byteLongBuffer[7] & 0xffL));

            return (tmpLong);
        } else {
            tmpLong = ( ( (byteLongBuffer[7] & 0xffL) << 56) | ( (byteLongBuffer[6] & 0xffL) << 48)
                    | ( (byteLongBuffer[5] & 0xffL) << 40) | ( (byteLongBuffer[4] & 0xffL) << 32)
                    | ( (byteLongBuffer[3] & 0xffL) << 24) | ( (byteLongBuffer[2] & 0xffL) << 16)
                    | ( (byteLongBuffer[1] & 0xffL) << 8) | (byteLongBuffer[0] & 0xffL));

            return (tmpLong);
        }
    }
    
    /**
     * Reads four signed bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the integer read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getInt(final boolean bigEndian) throws IOException {

        raFile.readFully(byteIntBuffer);

        if (bigEndian) {
            return ( ( (byteIntBuffer[0] & 0xff) << 24) | ( (byteIntBuffer[1] & 0xff) << 16)
                    | ( (byteIntBuffer[2] & 0xff) << 8) | (byteIntBuffer[3] & 0xff)); // Big Endian
        } else {
            return ( ( (byteIntBuffer[3] & 0xff) << 24) | ( (byteIntBuffer[2] & 0xff) << 16)
                    | ( (byteIntBuffer[1] & 0xff) << 8) | (byteIntBuffer[0] & 0xff));
        }
    }
    
    /**
     * Reads two unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of unsigned short read from the file returned as an int.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getUnsignedShort(final boolean bigEndian) throws IOException {

        raFile.readFully(byteShortBuffer);

        if (bigEndian) {
            return ( ( (byteShortBuffer[0] & 0xff) << 8) | (byteShortBuffer[1] & 0xff)); // Big
                                                                                                            // Endian
        } else {
            return ( ( (byteShortBuffer[1] & 0xff) << 8) | (byteShortBuffer[0] & 0xff)); // Little
                                                                                                            // Endian
        }
    }
    
    private String readCString() throws IOException {
        String cString = "";
        boolean nullFound = false;
        byte oneByte[] = new byte[1];
        while (!nullFound) {
            raFile.read(oneByte);
            if (oneByte[0]  == 0) {
                nullFound = true;
            }
            else {
                cString += new String(oneByte);
            }
        } // while (!nullFound)
        return cString;
    }
    
    private void cgmo(double cg[][][][], double theta[], ModelImage image, double radius[], int numOrientations,
    		String smooth, double sigmaSmooth[]) {
        int nbins = 32;
        double sigmaSim = 0.1;
        double gamma = 2.5;
        cgmo(cg, theta, image, radius, numOrientations, nbins, sigmaSim, gamma, smooth, sigmaSmooth);
    }
    
    /**
     * cgmo for color
     * Compute the color gradient at a single scale and multiple orientations
     * @param cg output output [yDim][xDim][3][numOrientations] array for color
     * @param output [numOrientations] theta
     * @param image  RGB image, values in [0, 1].
     * @param radius Radius of disc for cg array
     * @param numOrientations Number of orientations for cg array
     * @param nbins Number of bins; should be > 1/sigmaSim.
     * @param sigmaSim For color similarity function
     * @param gamma Gamma correction for LAB [2.5].
     * @param smooth Smoothing method, one of {"gaussian", "savgol", "none"}, default none
     * @param sigmaSmooth Sigma for smoothing, default to radius
     */
    private void cgmo(double cg[][][][], double theta[],
    		ModelImage image, double radius[], int numOrientations, int nbins, double sigmaSim, double gamma, 
    		String smooth, double sigmaSmooth[]) {
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// April 2003
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
        double red[];
        double green[];
        double blue[];
        double lab[][][];
        double cgcomp[][][];
        int j;
        
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
        
        // Convert gamma-corrected image to LAB and scale values into [0,1]
        buffer = new double[4*sliceSize];
        red = new double[sliceSize];
        green = new double[sliceSize];
        blue = new double[sliceSize];
        lab = new double[yDim][xDim][3];
        try {
        	image.exportData(0, 4*sliceSize, buffer);
        }
        catch(IOException e) {
        	e.printStackTrace();
        }
        for (i = 0; i < 4*sliceSize; i++) {
        	buffer[i] = Math.pow(buffer[i],gamma);
        }
        
        for (i = 0; i < sliceSize; i++) {
        	red[i] = buffer[4*i+1];
        	green[i] = buffer[4*i+2];
        	blue[i] = buffer[4*i+3];
        }
        
        RGB2Lab(lab, red, green, blue);
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		lab[y][x][0] = lab[y][x][0]/100.0;
        		lab[y][x][1] = (lab[y][x][1] - abmin)/(abmax - abmin);
        		lab[y][x][2] = (lab[y][x][2] - abmin)/(abmax - abmin);
        		lab[y][x][1] = Math.max(0.0,Math.min(1.0, lab[y][x][1]));
        		lab[y][x][2] = Math.max(0.0,Math.min(1.0, lab[y][x][2]));
        	}
        }
        
        // Compute cg from LAB values
        cmap = new int[yDim][xDim];
        
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
        cgcomp = new double[yDim][xDim][numOrientations];
        for (y = 0; y < nbins; y++) {
        	for (x = 0; x < nbins; x++) {
        		diff = xArr[y][x] - yArr[y][x];
        		csim[y][x] = 1.0 - Math.exp(-diff*diff/denom);
        	}
        }
        for (i = 0; i < 3; i++) {
        	for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		cmap[y][x] = Math.max(1, (int)Math.ceil(lab[y][x][i]* nbins));
	        	}
        	}
	        tgmo(cgcomp, theta, cmap, nbins, radius[i], numOrientations, csim, smooth, sigmaSmooth[i]);
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		for (j = 0; j < numOrientations; j++) {
	        		    cg[y][x][i][j] = cgcomp[y][x][j];	
	        		}
	        	}
	        }
        }
    }
    
    private void RGB2Lab(double L[][][], double red[], double green[], double blue[]) {
    	// RGB2Lab takes matrices corresponding to Red, Green, and Blue, and 
    	// transforms them into CIELab.  This transform is based on ITU-R 
    	// Recommendation  BT.709 using the D65 white point reference.
    	// The error in transforming RGB -> Lab -> RGB is approximately
    	// 10^-5.  RGB values can be either between 0 and 1 or between 0 and 255.  
    	// By Mark Ruzon from C code by Yossi Rubner, 23 September 1997.
    	// Updated for MATLAB 5 28 January 1998.
    	
    	// L has [yDim][xDim][3]
        
    	// Threshold
    	double T = 0.008856;
    	int sliceSize = red.length;
    	double MAT[][] = new double[3][3];
    	MAT[0][0] = 0.412453;
    	MAT[0][1] = 0.357580;
    	MAT[0][2] = 0.180423;
    	MAT[1][0] = 0.212671;
    	MAT[1][1] = 0.715160;
    	MAT[1][2] = 0.072169;
    	MAT[2][0] = 0.019334;
    	MAT[2][1] = 0.119193;
    	MAT[2][2] = 0.950227;
    	double XYZ[][] = new double[3][sliceSize];
    	int i;
    	int j;
    	int y;
    	int x;
    	
    	// RGB to XYZ
    	for (i = 0; i < 3; i++) {
	    	for (j = 0; j < sliceSize; j++) {
	    	    XYZ[i][j] = MAT[i][0]*red[j] + MAT[i][1]*green[j] + MAT[i][2]*blue[j];	
	    	}
    	}
    	
    	double X[] = new double[sliceSize];
    	double Y[] = new double[sliceSize];
    	double Z[] = new double[sliceSize];
    	for (i = 0; i < sliceSize; i++) {
    	    X[i] = XYZ[0][i]/0.950456;
    	    Y[i] = XYZ[1][i];
    	    Z[i] = XYZ[2][i]/1.088754;
    	}
    	
    	byte XT[] = new byte[sliceSize];
    	byte YT[] = new byte[sliceSize];
    	byte ZT[] = new byte[sliceSize];
    	for (i = 0; i < sliceSize; i++) {
    		if (X[i] > T) {
    			XT[i] = 1;
    		}
    		if (Y[i] > T) {
    			YT[i] = 1;
    		}
    		if (Z[i] > T) {
    			ZT[i] = 1;
    		}
    	}
    	
    	double fX[] = new double[sliceSize];
    	for (i = 0; i < sliceSize; i++) {
    		if (XT[i] == 1) {
    			fX[i] = Math.pow(X[i],(1.0/3.0));
    		}
    		else {
    			fX[i] = 7.787 * X[i] + 16.0/116.0;
    		}
    	}
    	
    	// Compute L
    	double Y3[] = new double[sliceSize];
    	for (i = 0; i < sliceSize; i++) {
    		Y3[i] = Math.pow(Y[i],(1.0/3.0));
    	}
    	double fY[] = new double[sliceSize];
    	for (i = 0; i < sliceSize; i++) {
    		if (YT[i] == 1) {
    			fY[i] = Y3[i];
    		}
    		else {
    			fY[i] = 7.787 * Y[i] + 16.0/116.0;
    		}
    	}
    	
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			i = x + y * xDim;
	    		if (YT[i]  == 1) {
	    			L[y][x][0] = 116.0 * Y3[i] - 16.0;
	    		}
	    		else {
	    			L[y][x][0] = 903.3 * Y[i];
	    		}
    		}
    	}
    	
    	double fZ[] = new double[sliceSize];
    	for (i = 0; i < sliceSize; i++) {
    		if (ZT[i] == 1) {
    			fZ[i] = Math.pow(Z[i],(1.0/3.0));
    		}
    		else {
    			fZ[i] = 7.787 * Z[i] + 16.0/116.0;
    		}
    	}
    	
    	// Compute a and b
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			i = x + y * xDim;
    			L[y][x][1] = 500.0 * (fX[i] - fY[i]);
    			L[y][x][2] = 200.0 * (fY[i] - fZ[i]);
    		}
    	}
    }
      
    private void cgmo(double cg[][][], double theta[], ModelImage image, double radius, int numOrientations,
    		String smooth, double sigmaSmooth) {
        int nbins = 32;
        double sigmaSim = 0.1;
        cgmo(cg, theta, image, radius, numOrientations, nbins, sigmaSim, smooth, sigmaSmooth);
    }
    
    /**
     * cgmo for black and white
     * Compute the color gradient at a single scale and multiple orientations
     * @param cg output [yDim][xDim][numOrientations] array for black and white 
     * @param output [numOrientations] theta
     * @param image  Grayscale image, values in [0, 1].
     * @param radius Radius of disc for cg array
     * @param numOrientations Number of orientations for cg array
     * @param nbins Number of bins; should be > 1/sigmaSim.
     * @param sigmaSim For color similarity function
     * @param smooth Smoothing method, one of {"gaussian", "savgol", "none"}, default none
     * @param sigmaSmooth Sigma for smoothing, default to radius
     */
    private void cgmo(double cg[][][], double theta[],
    		ModelImage image, double radius, int numOrientations, int nbins, double sigmaSim,
    		String smooth, double sigmaSmooth) {
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// April 2003
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
     * @param ntex Number of textons
     * @param radius Radius of disc for texture gradient
     * @param numOrientations Number of orientations at which to compute the texture graident
     * @param tsim Texton similarity matrix.  If not provided, then use chi-squared.
     * @param smooth Smoothing method.  One of "gaussian", "savgol", "none".  Default "none".
     * @param sigma Sigma for smoothing.  Default to radius.
     */
    private void tgmo(double tg[][][], double theta[], int tmap[][], int ntex, double radius, int numOrientations, 
    		double tsim[][], String smooth, double sigma) {
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
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
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
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
        double modf;
        
        if (tsim != null) {
    		usechi2 = false;
    	}
    	else {
    		usechi2 = true;
    	}
        
        radius = Math.max(1.0,  radius);
        theta = theta - Math.PI*Math.floor(theta/Math.PI);
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
        	for (x = 0; x < 2*wr+1; x++) {
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
        // Mask out center pixel to remove bias
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
        		modf = gamma[y][x] - theta - (2.0*Math.PI)*Math.floor((gamma[y][x] - theta)/(2.0*Math.PI));
        		if (modf < Math.PI) {
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
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			tg[y][x] = 0.0;
        		}
        	}
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
            	for (x = 0; x < xDim; x++) {
            		for (y = 0; y < yDim; y++) {
            			d[y + x * yDim][i-1] = Math.abs(tgL[y][x] - tgR[y][x]);
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
            for (x = 0; x < xDim; x++) {
            	for (y = 0; y < yDim; y++) {
            		for (i = 0; i < ntex; i++) {
            			tg[y][x] = tg[y][x] + d[y + x * yDim][i];
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
    	int yy;
    	int xx;
    	// Fit cylindrical parabolas to elliptical patches of z at each pixel
    	
    	// Input
    	// z Values to fit
    	// ra, rb Radius of elliptical neighborhood, ra = major axis
    	// theta Orientation of fit (i.e. of minor axis).
    	
    	// Output
    	// a[][], b[][], c[][] Coefficients of fit: a + bx + cx^2
    	
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	
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
    			for (yy = 0; yy < 3; yy++) {
    				for (xx = 0; xx < 3; xx++) {
    					invA[yy][xx] = invA[yy][xx]/(detA + epsilon);
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
    	// c[0] Coefficient of fit
    	
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
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
    		    A[j-i][i] = xx[j];	
    		}
    	}
    	matA = new Matrix(A);
    	A = (matA.inverse()).getArray();
    	matA = new Matrix(A);
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
		    	matyy = new Matrix(yy);
		    	prod = (matA.times(matyy)).getArray();
		    	for (i = 0; i < d+1; i++) {
		    		filt[v+wr][u+wr][i] = prod[i][0];
		    	}
    		}
    	}
    	// 2. Apply the filter to get the fit coefficient at each pixel
    	filtk = new double[wd][wd];
    	for (y = 0; y < wd; y++) {
    		for (x = 0; x < wd; x++) {
    			filtk[y][x] = filt[y][x][k-1];
    		}
    	}
    	conv2(z, filtk, c);
    }
    
    private void fbRun(double fim[][][][], double fb[][][][], double im[][]) {
        // Run a filterbank on an image with reflected boundary conditions
    	
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	int maxsz;
    	int r;
    	double impad[][];
    	double fimpad[][];
    	int y;
    	int x;
    	int ys;
    	int xs;
    	
    	// Find the maximum filter size
    	maxsz = 1;
    	for (y = 0; y < fb.length; y++) {
    		for (x = 0; x < fb[0].length; x++) {
    			maxsz = Math.max(maxsz, Math.max(fb[y][x].length,fb[y][x][0].length));
    		}
    	}
    	
    	// Pad the image
    	r = (int)Math.floor(maxsz/2);
    	impad = new double[im.length+2*r][im[0].length + 2*r];
    	padReflect(impad, im, r);
    	
    	// Run the filterbank on the padded image, and crop the result back to the original image size
    	fimpad = new double[impad.length][impad[0].length];
    	for (y = 0; y < fb.length; y++) {
    		for (x = 0; x < fb[0].length; x++) {
		    	if (fb[y][x].length < 50) {
		    	    conv2(impad,fb[y][x],fimpad);	
		    	}
		    	else {
		    	    fftconv2(fimpad, impad, fb[y][x]);	
		    	}
		    	for (ys = r; ys < impad.length - r; ys++) {
		    	    for (xs = r; xs < impad[0].length - r; xs++) {
		    	    	fim[y][x][ys-r][xs-r] = fimpad[ys][xs];
		    	    }
		    	}
    		}
    	}
    }
    
    private void fbRun(double fim[][], double fb[][], double im[][]) {
        // Run a filterbank on an image with reflected boundary conditions
    	
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
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
    	
    	// Run the filterbank on the padded image, and crop the result back to the original image size
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
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	
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
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
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
    
    private void oeFilter(double f[][], double sigmaX, double sigmaY, int support) {
    	double theta = 0.0;
        int deriv = 0;
        boolean dohil = false;
        boolean dovis = false;
        oeFilter(f, sigmaX, sigmaY, support, theta, deriv, dohil, dovis);
     }
    
    private void oeFilter(double f[][], double sigmaX, double sigmaY, int support, double theta) {
       int deriv = 0;
       boolean dohil = false;
       boolean dovis = false;
       oeFilter(f, sigmaX, sigmaY, support, theta, deriv, dohil, dovis);
    }
    
    private void oeFilter(double f[][], double sigmaX, double sigmaY, int support, double theta, int deriv) {
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
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
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
         // Bin membership for 2D grid points
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
        	     fprecursor[y][x] = fx[xi[y][x]-1] * fy[yi[y][x]-1]; 
        	 }
         }
         // Accumulate the samples into each bin
         for (y = 0; y < sz; y++) {
        	 for (x = 0; x < sz; x++) {
        	     f[y][x] = 0.0;	 
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
        		 f[(v-1)%sz][(v-1)/sz] += fprecursor[y][x];
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