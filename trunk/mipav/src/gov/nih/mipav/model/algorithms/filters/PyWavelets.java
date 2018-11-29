package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentGraph;
import gov.nih.mipav.view.ViewJFrameGraph;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/**
Copyright (c) 2006-2012 Filip Wasilewski <http://en.ig.ma/>
Copyright (c) 2012-2017 The PyWavelets Developers <https://github.com/PyWavelets/pywt>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

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

public  class PyWavelets extends AlgorithmBase {
	// Syntax defaults to the CDF 9/7 wavelets used in JPEG2000
	// Author of original code: Brian Moore
	// brimoor@umich.edu
	
	private final int CDF_type = 1;
	
	private final int spline_type = 2;
	
	private final int waveletMatrix_method = 1;
	
	private final int convolution_method = 2;
	
	/** D1MACH(4). */
    private double epsilon = Double.NaN;
    
    // If you used BWDesignTool to generate a biorthogonal wavelet filter and saved the output variables
    // [h, ht, g, gt, inds_h, inds_ht, inds_g, inds_gt] you may pass them directly into BiorthogonalWavellets.
    private double h[];
    
    private double ht[];
    
    private int inds_h[];
    
    private int inds_ht[];
    
    private double g[];
    
    private double gt[];
    
    private int inds_g[];
    
    private int inds_gt[];
    
    private ModelImage transformImage;
    
    private ModelImage compressedTransformImage;
    
    private ModelImage reconstructedImage;
    
    private int iterations;
    
    // 0 <= compressionFactor <= 1.
    // Alternatively, when compressionFactor == -1, the lowest frequency approximation is retained.
    private double compressionFactor;
    
    // Type can be "spline" or "CDF" and controls the type of biorthogonal wavelet filter used.
    private int type;
    
    // 2*(l+R) + 1 and 2*(lt + C) + 1 are the lengths of the lowpass biorthogonal filters h and ht,
    // where R and C are the number of real and complex zeros of P(t), deg(P) = l + lt - 1,
    // respectively when type == "CDF".
    private int l;
    
    private int lt;
    
    // 2*N+Nt-1 and Nt+1 are the lengths of the lowpass biorthogonal spline filters h and ht,
    // respectively when type == "spline".
    private int N;
    
    private int Nt;
    
    // Plot result or not
    private boolean display = true;
    
    // Forward transform computation method
    // convolution_method or waveletMatrix_method
    private int method = convolution_method;
    
    private double hRealRoots[];
    
    private double hComplexRoots[][];
    
    private double htRealRoots[];
    
    private double htComplexRoots[][];
    
    private double P[];
    
    private double h1[];
    
    private double ht1[];
    
    private String str;
    
    private double HwReal[];
    private double HwImag[];
    private float absHw[];
    
    private double HtwReal[];
    private double HtwImag[];
    private float absHtw[];
    
    private double GwReal[];
    private double GwImag[];
    private float absGw[];
    
    private double GtwReal[];
    private double GtwImag[];
    private float absGtw[];
    
    /* Wavelet symmetry properties */
    private enum SYMMETRY{
        UNKNOWN (-1),
        ASYMMETRIC (0),
        NEAR_SYMMETRIC (1),
        SYMMETRIC (2),
        ANTI_SYMMETRIC (3);
        
        private final int type;
        SYMMETRY(int type) {
        	this.type = type;
        }
        private int type() {return type;}
    }
    
    /* Wavelet name */
    private enum WAVELET_NAME {
        HAAR,
        RBIO,
        DB,
        SYM,
        COIF,
        BIOR,
        DMEY,
        GAUS,
        MEXH,
        MORL,
        CGAU,
        SHAN,
        FBSP,
        CMOR;
    }
    
    /* Wavelet structure holding pointers to filter arrays and property attributes */
    private class BaseWavelet {
        /* Wavelet properties */

        int support_width;

        SYMMETRY symmetry;

        int orthogonal = 1;
        int biorthogonal = 1;
        int compact_support = 1;

        int _builtin;
        String family_name;
        String short_name;
    }
    
    private class DiscreteWavelet {
        BaseWavelet base;
        double dec_hi_double[];  /* highpass decomposition */
        double dec_lo_double[];  /* lowpass decomposition */
        double rec_hi_double[];  /* highpass reconstruction */
        double rec_lo_double[];  /* lowpass reconstruction */
        float dec_hi_float[];
        float dec_lo_float[];
        float rec_hi_float[];
        float rec_lo_float[];
        int dec_len;   /* length of decomposition filter */
        int rec_len;   /* length of reconstruction filter */

        int vanishing_moments_psi;
        int vanishing_moments_phi;
    } 
    
    private class ContinuousWavelet {

        BaseWavelet base;
        float lower_bound;
        float upper_bound;
        /* Parameters for shan, fbsp, cmor*/
        int complex_cwt;
        float center_frequency;
        float bandwidth_frequency;
        int fbsp_order;
    }
    
    public PyWavelets(ModelImage transformImage, ModelImage compressedTransformImage, 
    		ModelImage reconstructedImage, ModelImage srcImg, int iterations, double compressionFactor,
    		boolean display, int method,
    		int type, int len, int lent) {
    	super(null, srcImg);
    	this.transformImage = transformImage;
    	this.compressedTransformImage = compressedTransformImage;
    	this.reconstructedImage = reconstructedImage;
    	this.iterations = iterations;
    	this.compressionFactor = compressionFactor;
    	this.display = display;
    	this.method = method;
    	this.type = type;
    	if (type == CDF_type) {
    		l = len;
    		lt = lent;
    	}
    	else if (type == spline_type) {
    		N = len;
    		Nt = lent;
    	}
    }
    
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int bound;
    	int maxIters;
    	int factor2inXDim;
    	int factor2inYDim;
    	int xTest;
    	int yTest;
    	int z;
    	int ii;
    	double WMt[][];
    	double WN[][];
    	int length;
    	double buffer[];
    	double IM[][][];
    	double IMt[][][];
    	int x;
    	int y;
    	int yLim;
    	int xLim;
    	int i;
    	double prod[][];
    	double IMtInput[][];
    	double output[][];
    	double IMtc[][][];
    	double IMhat[][][];
    	double coeffs[];
    	double thresh;
    	double WM[][];
    	double WNt[][];
    	double diff;
    	double MSE;
    	
        if (type == spline_type) {
        	//SplineWavelets();
        }
        else if (type == CDF_type) {
        	//CDFWavelets();
        }
        else {
        	MipavUtil.displayError("Incorrect type = " + type);
        	setCompleted(false);
        	return;
        }
        
        // Make sure transforms will be well-defined.
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new double[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        IM = new double[yDim][xDim][zDim];
        IMt = new double[yDim][xDim][zDim];
        IMtc = new double[yDim][xDim][zDim];
        IMhat = new double[yDim][xDim][zDim];
        
        // Image must have even dimensions
        if (((xDim % 2) == 1) || ((yDim % 2) == 1)) {
        	MipavUtil.displayError("Image must have even dimensions");
        	setCompleted(false);
        	return;
        }
        
        // Determine the maximum number of iterations possible
        bound = Math.max(Math.max(h.length,ht.length),Math.max(g.length,gt.length));
        maxIters = 1;
        while (Math.min(xDim,yDim)/Math.pow(2.0, maxIters) >= bound) {
        	maxIters = maxIters + 1;
        }
        factor2inXDim = 0;
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	factor2inXDim++;
        	xTest = xTest/2;
        }
        factor2inYDim = 0;
        yTest = yDim;
        while ((yTest % 2) == 0) {
        	factor2inYDim++;
        	yTest = yTest/2;
        }
        maxIters = Math.min(maxIters, Math.min(factor2inXDim, factor2inYDim));
        
        // Must have iterations <= maxIters
        if (iterations > maxIters) {
        	MipavUtil.displayError("For that image and those wavelets, you must have iterations <= " + maxIters);
        	setCompleted(false);
        	return;
        }
        
        // Compute iterated wavelet transform
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                return;
            }
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			IM[y][x][z] = buffer[x + y * xDim];
        			IMt[y][x][z] = IM[y][x][z];
        		}
        	}
            for (ii = 1; ii <= iterations; ii++) {
            	xLim = (int)Math.round(xDim/Math.pow(2.0, (ii-1)));
            	yLim = (int)Math.round(yDim/Math.pow(2.0, (ii-1)));
                
            } // for (ii = 1; ii <= iterations; ii++)
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			buffer[x + y * xDim] = IMt[y][x][z];
        			IMtc[y][x][z] = IMt[y][x][z];
         		}
        	}
            try {
                transformImage.importData(z*length, buffer, false);
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                return;
            }
        } // for (z = 0; z < zDim; z++)
        transformImage.calcMinMax();
        
        // Compress image by throwing away compressionFactor % of the wavelet coefficients;
        if (compressionFactor > 0) {
            coeffs = new double[length * zDim];
            for (z = 0; z < zDim; z++) {
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            		    coeffs[length*z + y*xDim + x] = Math.abs(IMt[y][x][z]);
            		}
            	}
            }
            Arrays.sort(coeffs);
            thresh = coeffs[(int)Math.ceil(compressionFactor*length*zDim)-1];
            for (z = 0; z < zDim; z++) {
            	for (i = 0; i < length; i++) {
     	    		buffer[i] = 0;
     	    	}
                for (y = 0; y < yDim; y++) {
            	    for (x = 0; x < xDim; x++) {
            			if (Math.abs(IMt[y][x][z]) <= thresh) {
            				IMtc[y][x][z] = 0;
            			}
            			buffer[x + y * xDim] = IMtc[y][x][z];
            		}
                 }
                 try {
                    compressedTransformImage.importData(z*length, buffer, false);
                 } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                    return;
                 }
            } // for (z = 0; z < zDim; z++)
        } // if (compressionFactor > 0)
        else if (compressionFactor == 0) {
        	for (z = 0; z < zDim; z++) {
            	for (i = 0; i < length; i++) {
     	    		buffer[i] = 0;
     	    	}
                for (y = 0; y < yDim; y++) {
            	    for (x = 0; x < xDim; x++) {
            			buffer[x + y * xDim] = IMtc[y][x][z];
            		}
                 }
                 try {
                    compressedTransformImage.importData(z*length, buffer, false);
                 } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                    return;
                 }
            } // for (z = 0; z < zDim; z++)
        }
        else if (compressionFactor == -1) {
       	    IMtc = new double[yDim][xDim][zDim];
       	    xLim = (int)Math.round(xDim/Math.pow(2.0, iterations));
     	    yLim = (int)Math.round(yDim/Math.pow(2.0, iterations));
     	    for (z = 0; z < zDim; z++) {
     	    	for (i = 0; i < length; i++) {
     	    		buffer[i] = 0;
     	    	}
     	    	for (y = 0; y < yLim; y++) {
     	    		for (x = 0; x < xLim; x++) {
     	    			IMtc[y][x][z] = IMt[y][x][z];
     	    			buffer[x + y * xDim] = IMt[y][x][z];
     	    		}
     	    	}
     	    	try {
                    compressedTransformImage.importData(z*length, buffer, false);
                 } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                    return;
                 }
     	    }
        } // else if (compressionFactor == -1)
        compressedTransformImage.calcMinMax();
        
        // Iteratively reconstruct the image
        for (z = 0; z < zDim; z++) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        		    IMhat[y][x][z] = IMtc[y][x][z];	
        		}
        	}
        }
        
        for (z = 0; z < zDim; z++) {
        	for (i = 0; i < length; i++) {
        		buffer[i] = 0;
        	}
        	
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			buffer[x + y * xDim] = IMhat[y][x][z];
         		}
        	}
            try {
                reconstructedImage.importData(z*length, buffer, false);
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                return;
            }

        } // for (z = 0; z < zDim; z++)
        reconstructedImage.calcMinMax();
        
        // Compute mean square reconstruction error
        MSE = 0.0;
        for (z = 0; z < zDim; z++) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        		    diff = IM[y][x][z] - IMhat[y][x][z];
        		    MSE += (diff * diff);
        		}
        	}
        }
        MSE = MSE/(zDim * length);
        System.out.println("Mean square reconstruction error = " + MSE);
        Preferences.debug("Mean square reconstruction error = " + MSE + "\n", Preferences.DEBUG_ALGORITHM);
        setCompleted(true);
        return;
    }
    
    private void computeEpsilon() {
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        double neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)	
    }
	
	
}
