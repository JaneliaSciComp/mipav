package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;

public class PhasePreservingDenoising extends AlgorithmBase {
	
/**
 * Phase preserving wavelet image denoising
 * MIT License

Copyright (c) 2018 Peter Kovesi

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

## Documentation

* [The main documentation page](https://peterkovesi.github.io/ImagePhaseCongruency.jl/dev/index.html)
* [Examples](https://peterkovesi.github.io/ImagePhaseCongruency.jl/dev/examples/)
* [Function reference](https://peterkovesi.github.io/ImagePhaseCongruency.jl/dev/functions/)
* 
* "Phase Preserving Denoising of Images" by Peter Kovesi
* The Australia Pattern Recognition Society Conference: DICTA'99.
* December, 1999. Perth WA, pp.212-217.
* http://www.peterkovesi.com/papers/denoise.pdf
 */
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	// Number of filter scales to use (5-7) - the more scales used the more low frequencies are covered
	private int nscale = 5;
	// Number of orientations to use (6)
	private int norient = 6;
	// Multiplying factor between successive scales (2.5-3)
	private double mult = 2.5;
	// Wavelength of smallest scale factor (2)
	private double minwavelength = 2.0;
	// Ratio of the standard deviation of the Gaussian describing the log Gabor's transfer function
	// in the frequency domain to the filter center frequency (0.55)
	private double sigmaonf = 0.55;
	// Ratio of angular interval between filter orientations and the standard deviation of the
	// angular Gaussian (1)
	// function used to construct filters in the frequency plane
	private double dthetaonsigma = 1.0;
	// Number of standard deviations of noise to reject (2-3)
	private double k = 3.0;
	// Degree of soft thresholding (0-hard to 1-soft)
	private double softness = 1.0;
	private boolean useProgressBar = true;
	
	/* Assigned to srcImage if replace image, assigned to destImage if new image */
    private ModelImage targetImage = null;
	
//~ Constructors ---------------------------------------------------------------------------------------------------
    
    public PhasePreservingDenoising() {
    	
    }
    
    public PhasePreservingDenoising(ModelImage srcImg, int nscale, int norient, 
    		double mult, double minwavelength, double sigmaonf, double dthetaonsigma,
    		double k, double softness, boolean useProgressBar) {
    	this(null, srcImg, nscale, norient, mult, minwavelength, sigmaonf, dthetaonsigma,
    			k, softness, useProgressBar);
    }
    
    public PhasePreservingDenoising(ModelImage destImg, ModelImage srcImg, int nscale, int norient, 
    		double mult, double minwavelength, double sigmaonf, double dthetaonsigma,
    		double k, double softness, boolean useProgressBar) {
    	super(destImg, srcImg);
    	this.nscale = nscale;
    	this.norient = norient;
    	this.mult = mult;
    	this.minwavelength = minwavelength;
    	this.sigmaonf = sigmaonf;
    	this.dthetaonsigma = dthetaonsigma;
    	this.k = k;
    	this.softness = softness;
    	this.useProgressBar = useProgressBar;
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int length;
    	double img[];
    	double imgImag[];
    	int i,y,x,or;
    	double angl;
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
    	
    	if (useProgressBar) {
            fireProgressStateChanged(0, srcImage.getImageName(), "Phase Preserving Denoising on image ...");
        }
    	
    	if (destImage == null) {
            targetImage = srcImage;
        }
        else {
            targetImage = destImage;
        }
    	
    	xDim = srcImage.getExtents()[0];
    	yDim = srcImage.getExtents()[1];
    	length = xDim * yDim;
    	img = new double[length];
    	try {
    		srcImage.exportData(0, length, img);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException " + e + " srcImage.exportData(0. length, buffer)");
    		setCompleted(false);
    		return;
    	}
    	
        double epsilon = 1.0E-5; // Used to prevent division by zero
        
        // Calculate the standard deviation of the angular Gaussian function used to
        // construct filters in the frequency plane
        double thetaSigma = Math.PI/norient/dthetaonsigma;
        
        imgImag = new double[length];
        FFTUtility fft = new FFTUtility(img, imgImag, yDim, xDim, 1, -1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(img, imgImag, 1, yDim, xDim, -1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		// Generate grids for constructing frequency domain filters
		// freq - Grid of size (yDim,xDim) containing frequency
		// values from 0 to 0.5, where freq =
		// sqrt(fx^2 + fy^2).  The grid is quadrant
		// shifted so that 0 frequency is at freq(1,1)
		
		// fx,fy - Grids containing normalized frequency values
		// ranging from -0.5 to 0.5 in x and y directions
		// respectively.  fx and fy are quadrant shifted
		
		// Set up X and Y spatial frequency matrices, fx and fy, with ranges
		// normalized to +/-0.5.  The following code adjusts things appropriately for
		// odd and even values of yDim and xDim so that the 0 frequency point is
		// placed appropriately.
		double fxrange[] = new double[2*xDim-1];
		if ((xDim % 2) == 1) {
		    fxrange[0] = -(xDim-1.0)/(2.0*xDim);
		}
		else {
			fxrange[0] = -0.5;
		}
		for (i = 1; i < 2*xDim-1; i++) {
			fxrange[i] = fxrange[0] + i/(2.0*xDim);
		}
		
		double fyrange[] = new double[2*yDim-1];
		if ((yDim % 2) == 1) {
			fyrange[0] = -(yDim-1.0)/(2.0*yDim);
		}
		else {
			fyrange[0] = -0.5;
		}
		for (i = 1; i < 2*yDim-1; i++) {
			fyrange[i] = fyrange[0] + i/(2.0*yDim);
		}
		;
		double fx[][] = new double[2*yDim-1][2*xDim-1];
		for (y = 0; y < 2*yDim-1; y++) {
			for (x = 0; x < 2*xDim-1; x++) {
				fx[y][x] = fxrange[x];
			}
		}
		
		double fy[][] = new double[2*yDim-1][2*xDim-1];
		for (y = 0; y < 2*yDim-1; y++) {
			for (x = 0; x < 2*xDim-1; x++) {
				fy[y][x] = fyrange[y];
			}
		}
		
		// Quadrant shift so that filters are constructed with 0 frequency at
		// the corners
		fx = ifftshift(fx);
		fy = ifftshift(fy);
		
		// Construct spatial frequency filters in terms of normalized radius from
		// center
		double freq[][] = new double[2*yDim-1][2*xDim-1];
		for (y = 0; y < 2*yDim-1; y++) {
			for (x = 0; x < 2*xDim-1; x++) {
				freq[y][x] = Math.sqrt(fx[y][x]*fx[y][x] + fy[y][x]*fy[y][x]);
			}
		}
		
		// Generate arrays of filter grid angles
		double sintheta[][] = new double[2*yDim-1][2*xDim-1];
		double costheta[][] = new double[2*yDim-1][2*xDim-1];
		// Avoid divide by zero
		freq[0][0] = 1.0;
		for (y = 0; y < 2*yDim-1; y++) {
			for (x = 0; x < 2*xDim-1; x++) {
				sintheta[y][x] = fx[y][x]/freq[y][x];
				costheta[y][x] = fy[y][x]/freq[y][x];
			}
		}
		// Restore 0 DC
		freq[0][0] = 0.0;
		
		// Response at each orientation
		double totalEnergy[][] = new double[yDim][xDim];
		double totalEnergyImag[][] = new double[yDim][xDim];
		double filter[][] = new double[yDim][xDim];
		double angfilter[][] = new double[yDim][xDim];
		double EO[][] = new double[yDim][xDim];
		double EOImag[][] = new double[yDim][xDim];
		double aEO[][] = new double[yDim][xDim];
		
		// make main scope
		double RayMean = 0.0;
		double RayVar = 0.0;
		
		// For each orientation
		for (or = 1; or <= norient; or++) {
			// Calculate the filter angle
			angl = (or-1.0)*Math.PI/norient;
			// Generate angular filter
		} // for (or = 1; or <= norient; or++)
    }
    
    private double[][] ifftshift(double in[][]) {
    	int yDim = in.length;
    	int xDim = in[0].length;
		double out[][] = new double[yDim][xDim];
		int highestxquad1Index;
		int highestyquad1Index;
		int quad1height;
		int quad1width;
		int quad2height;
		int quad2width;
		int quad3height;
		int quad3width;
		int quad4height;
		int quad4width;
		int y,x;
		if ((yDim %2) == 1) {
			//yodd = true;
			highestyquad1Index = (yDim - 1)/2 - 1;
		}
		else {
			//yodd = false;
			highestyquad1Index = yDim/2 - 1;
		}
		if ((xDim % 2) == 1) {
			//xodd = true;
			highestxquad1Index = (xDim - 1)/2 - 1;
		}
		else {
			//xodd = false;
			highestxquad1Index = xDim/2 - 1;
		}
		quad1width = highestxquad1Index + 1;
		quad1height = highestyquad1Index + 1;
		quad2width = xDim - quad1width;
		quad2height = quad1height;
		quad3width = quad2width;
		quad3height = yDim - quad1height;
		quad4width = quad1width;
		quad4height = quad3height;
		double quad1[][] = new double[quad1height][quad1width];
		double quad2[][] = new double[quad2height][quad2width];
		double quad3[][] = new double[quad3height][quad3width];
		double quad4[][] = new double[quad4height][quad4width];
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad1[y][x] = in[y][x];
			}
		}
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad2[y][x - highestxquad1Index-1] = in[y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad3[y - highestyquad1Index - 1][x - highestxquad1Index - 1] = in[y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad4[y - highestyquad1Index - 1][x] = in[y][x];
			}
		}
		
		// Move third quadrant to first
		for (y = 0; y < quad3height; y++) {
			for (x = 0; x < quad3width; x++) {
				out[y][x] = quad3[y][x];
			}
		}
		// Move fourth quadrant to second
		for (y = 0; y < quad4height; y++) {
			for (x = 0; x < quad4width; x++) {
				out[y][x + quad3width] = quad4[y][x];
			}
		}
		// Move first quadrant to third
		for (y = 0; y < quad1height; y++) {
			for (x = 0; x < quad1width; x++) {
				out[y+quad3height][x + quad3width] = quad1[y][x];
			}
		}
		// Move second quadrant to fourth
		for (y = 0; y < quad2height; y++) {
			for (x = 0; x < quad2width; x++) {
				out[y+quad3height][x] = quad2[y][x];
			}
		}
		return out;
	}
	
}