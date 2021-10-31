package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;


/**
 This is a port of openCVtrilateralFilter.h and openCVtrilateralFilter.cpp 
 written by Tobi Vaudrey.  These files were released under the 3-clause BSD license
References: 1.) "Fast Trilateral Filtering" by Tobi Vaudrey and Reinhard Klette,
                 Conference Paper in Lecture Notes in Computer Science, September,
                 2009, DOI: 10.1007/978-3-642-03767-2_66
            2.) "The Trilateral Filter for High Contrast Images and Meshes" by
                 Prasun Choudhury and Jack Tumblin, Eurographics Symposium on
                 Rendering 2003, pp. 1-11, Per Christensen and Daniel Cohen-Or (Editors)
                 
By downloading, copying, installing or using the software you agree to this license.
If you do not agree to this license, do not download, install,
copy or use the software.


                          License Agreement
               For Open Source Computer Vision Library
                       (3-clause BSD License)

Copyright (C) 2000-2020, Intel Corporation, all rights reserved.
Copyright (C) 2009-2011, Willow Garage Inc., all rights reserved.
Copyright (C) 2009-2016, NVIDIA Corporation, all rights reserved.
Copyright (C) 2010-2013, Advanced Micro Devices, Inc., all rights reserved.
Copyright (C) 2015-2016, OpenCV Foundation, all rights reserved.
Copyright (C) 2015-2016, Itseez Inc., all rights reserved.
Copyright (C) 2019-2020, Xperience AI, all rights reserved.
Third party copyrights are property of their respective owners.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  * Neither the names of the copyright holders nor the names of the contributors
    may be used to endorse or promote products derived from this software
    without specific prior written permission.

This software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are disclaimed.
In no event shall copyright holders or contributors be liable for any direct,
indirect, incidental, special, exemplary, or consequential damages
(including, but not limited to, procurement of substitute goods or services;
loss of use, data, or profits; or business interruption) however caused
and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of
the use of this software, even if advised of the possibility of such damage.
© 2021 GitHub, Inc.
*/

    public class AlgorithmTrilateralFilter extends AlgorithmBase {
    	
    	public final int BORDER_CONSTANT = 0; // iiiiii|abcdefgh|iiiiiii with some specified i
    	public final int BORDER_REPLICATE = 1; // aaaaaa|abcdefgh|hhhhhhh
    	public final int BORDER_REFLECT = 2; // fedcba|abcdefgh|hgfedcb
    	public final int BORDER_WRAP = 3; // cdefgh|abcdefgh|abcdefg
    	public final int BORDER_REFLECT_101 = 4; // gfedcb|abcdefgh|gfedcba
    	public final int BORDER_DEFAULT = BORDER_REFLECT_101;
    	
    	// spatial sigma
    	private double sigmaC = 5.0;
    	
    	// epsilon only used in TYPE_FAST
    	private double epsilon = 1.0E-12;
    	
    	private final int TYPE_LUT = 0;
    	
    	private final int TYPE_FAST = 1;
    	
    	private int filterType = TYPE_FAST;
    	private int xDim;
    	private int yDim;
    	private int length;
    	
    	//~ Constructors ---------------------------------------------------------------------------------------------------
        
        public AlgorithmTrilateralFilter() {
        	
        }
        
        public AlgorithmTrilateralFilter(ModelImage destImg, ModelImage srcImg, double sigmaC,
        		double epsilon, int filterType) {
        	super(destImg, srcImg);
        	this.sigmaC = sigmaC;
        	this.epsilon = epsilon;
        	this.filterType = filterType;
        }
        
        // Default is roundUp = false
        private int log2(int input, boolean roundUp) {
            double temp = Math.log10( (double)(input) ) / Math.log10(2.0);
            if (roundUp) {
                return (int)( Math.ceil(temp) );
            }
            else {
                return (int)( temp );
            }
       }


       // Cannot be color image
       public void runAlgorithm() {
    	   int x,y;
    	   ModelImage outputImage;
      	   if (destImage != null) {
      		   outputImage = destImage;
      	   }
      	   else {
      		   outputImage = srcImage;
      	   }
      	   
      	   xDim = srcImage.getExtents()[0];
      	   yDim = srcImage.getExtents()[1];
      	   length = xDim * yDim;
      	   
      	   double buffer[] = new double[length];
      	   
      	   try {
		    	srcImage.exportData(0, length, buffer);
		    }
		    catch (IOException e) {
		    	MipavUtil.displayError("IOException on srcImage.exportData(0, length, buffer)");
		    	setCompleted(false);
		    	return;
		    }
      	   
      	    double inputImg[][] = new double[yDim][xDim];
      	    for (y = 0; y < yDim; y++) {
      	    	for (x = 0; x < xDim; x++) {
      	    		inputImg[y][x] = buffer[x + y*xDim];
      	    	}
      	    }
      	    
      	   
      	    // Adaptive Neighbourhood pixelwise image
      	    double adaptiveNeighbourhood[][] = new double[yDim][xDim];
      	    
      	    // x and y gradients
      	    double xGradient[][] = new double[yDim][xDim];
      	    double yGradient[][] = new double[yDim][xDim];
      	    
      	    // Smoothed x and y gradients
      	    double xGradientSmooth[][] = new double[yDim][xDim];
      	    double yGradientSmooth[][] = new double[yDim][xDim];
      	    
      	    // Gradient magnitude
      	    double gradientMagnitude[][] = new double[yDim][xDim];
      	    
      	    // domain variance for the two filters: sigmaC
      	    // range variance of the two filters: sigmaR
      	    // beta = emperical value
      	    double sigmaR; 
      	    final double beta = 0.15; //Always set between 0.1 and 0.2
      	    
      	    // Computes X and Y gradients of the input image
      	    computeGradients(inputImg, xGradient, yGradient );
      	    
      	    // Computes the gradient magnitude
      	    computeMagnitude(xGradient, yGradient, gradientMagnitude);
      	    
      	    // Get min and max gradient for sigmaR
      	    double minGrad[] = new double[1];
      	    double maxGrad[] = new double[1];
      	    minMaxLoc(gradientMagnitude, minGrad, maxGrad);
      	    sigmaR = beta * (maxGrad[0] - minGrad[0]);
      	    
      	    // Level Max and Maximum LUT values
      	    int levelMax, maxLUT;
      	    
      	    // If using LUT
      	    if (filterType == TYPE_LUT) {
      	        // maximum level = log2(xsize) or log2(ysize)
      	    	levelMax = log2(Math.min(xDim, yDim), false);
      	    	maxLUT = (int)Math.round(Math.pow( 2.0, levelMax-1)) + 1;
      	    }
      	    else {
      	        // Using fast-trilateral
      	        // Find threshold for truncation
      	    	maxLUT = (int)( sigmaC * Math.sqrt( Math.abs( Math.log(epsilon) ) ) ) + 1;
      	        // Calculate maximum level
      	    	levelMax = log2(2 * maxLUT + 1, true);
      	    }
      	    
      	    // Calculate Adaptive Neighbourhood
      	    setAdaptiveNeighbourHood(gradientMagnitude, sigmaR, levelMax, adaptiveNeighbourhood);
      	    
      	    // Bilaterally filter the X and Y gradients of the input image
      	    // to produce xGradientSmooth and yGradientSmooth.
      	    if (filterType == TYPE_LUT) {
      	    	BilateralGradientFilterLUT(xGradient,yGradient, gradientMagnitude, sigmaC, sigmaR,
      	    			xGradientSmooth, yGradientSmooth);
      	    }
      	    else {
      	        BilateralGradientFilter(xGradient, yGradient, gradientMagnitude, sigmaC, sigmaR, epsilon, 
      		    xGradientSmooth, yGradientSmooth );
      	    }
      	    
      	    double outputImg[][] = new double[yDim][xDim];
      	    // Performs bilateral filter on the detail signal 
      	    if (filterType == TYPE_LUT) {
                DetailBilateralFilterLUT(inputImg, adaptiveNeighbourhood, xGradientSmooth, yGradientSmooth, 
      		        sigmaC, sigmaR, maxLUT, outputImg);
      	    }
	        else {
	      	    DetailBilateralFilter(inputImg, adaptiveNeighbourhood, xGradientSmooth, yGradientSmooth, 
	      	        sigmaC, sigmaR, maxLUT, epsilon, outputImg);
	        }
	
	        for (y = 0; y < yDim; y++) {
	  	       for (x = 0; x < xDim; x++) {
	  	           buffer[x + y*xDim] = outputImg[y][x];
	  	       }
	  	    }
	      	
	        try {
	      	    outputImage.importData(0, buffer, true);
	        }
	        catch (IOException e) {
	            MipavUtil.displayError("IOException on outputImage.importData(0, buffer, true)");
	      		setCompleted(false);
	      		return;
	      	}
      	
      	    setCompleted(true);
      	    return;

    	} // public void runAlgorithm
    	
    	// Computes X and Y gradients of the input image
    	public void computeGradients(double inputImg[][], double xGradient[][], double yGradient[][]) {
    		int x,y;
    		// Set up convolution kernels for forward differences
    		// double kernel[] = new double[]{ -1, 1 };
    		double inputImgPad[][] = copyMakeBorder(inputImg, 0, 1, 0, 1, BORDER_DEFAULT, 0.0);
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				xGradient[y][x] = inputImgPad[y][x+1] - inputImgPad[y][x];
    				yGradient[y][x] = inputImgPad[y+1][x] - inputImgPad[y][x];
    			}
    		}
    	}
    	
    	public void computeMagnitude(double xGradient[][], double yGradient[][], double gradientMagnitude[][]) {
    		int x,y;
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				gradientMagnitude[y][x] = Math.sqrt(xGradient[y][x]*xGradient[y][x] + yGradient[y][x]*yGradient[y][x]);
    			}
    		}
    	}
    	
    	public void minMaxLoc(double input[][], double minGrad[], double maxGrad[]) {
    		int x,y;
    		minGrad[0] = Double.MAX_VALUE;
    		maxGrad[0] = -Double.MAX_VALUE;
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				if (input[y][x] < minGrad[0]) {
    					minGrad[0] = input[y][x];
    				}
    				if (input[y][x] > maxGrad[0]) {
    					maxGrad[0] = input[y][x];
    				}
    			}
    		}
    	}
    	
    	// Find the adaptive neighbourhood for image
    	public void setAdaptiveNeighbourHood(double gradientMagnitude[][], double sigmaR, int maxLevel, double adaptiveNeighborhood[][]) {
    		int x,y;
    		// Image stacks for max and min neighbourhoods	
    		// Create image stack
    		double minGradientStack[][][] = new double[maxLevel][yDim][xDim];
    		double maxGradientStack[][][] = new double[maxLevel][yDim][xDim];
    		
    		// Build the min-max stack
    		buildMinMaxImageStack(gradientMagnitude, minGradientStack, maxGradientStack);

    		// Set up image data references
    		double[][] minImg, maxImg;

    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				int lev;
    				final double upperThreshold = gradientMagnitude[y][x] + sigmaR;
    				final double lowerThreshold = gradientMagnitude[y][x] - sigmaR;
    				
    				// Compute the adaptive neighbourhood based on the similarity of
    				// the neighborhood gradients
    				for(lev = 0; lev < maxLevel; lev++) {
    					minImg = minGradientStack[lev];
    					maxImg = maxGradientStack[lev];
    					if ( maxImg[y][x] > upperThreshold || minImg[y][x] < lowerThreshold ) {
    						break;
    					}
    				}
    				
    				// Sets the (half) size of the adaptive region
    				// i.e., floor( ( pow(2.f, lev) + 1 ) / 2.f )
    				adaptiveNeighborhood[y][x] = Math.pow(2.0, lev-1);
    			}
    		}
    	}
    	
    	// Building the Min-Max Stack of Image Gradients
    	public void buildMinMaxImageStack(double gradientMagnitude[][], double minStack[][][],
    			double maxStack[][][]) {
    	    int i,x,y,m,n;
    	    double outMin, outMax;
    		// Set up image data references
    		double[][] minImg1, maxImg1, minImg2, maxImg2;
    		
    		// Set up the bottom level of the pyramid
    		minImg1 = minStack[0];
    		maxImg1 = maxStack[0];
    		
    		// Loop through image setting up bottom stack
    		for(y = 0; y < yDim; y++) {
    		    for (x = 0; x < xDim; x++) {
    		        outMin = Double.MAX_VALUE;
    		        outMax = -Double.MAX_VALUE;

    		        // Loop through local neighbourhood
    		        // To find maximum and minimum values
    		        for(n = Math.max(y-1,0) ; n < Math.min(y+2, yDim); n++) {
    		            for(m = Math.max(x-1,0); m < Math.min(x+2, xDim); m++) {
    		                outMin = Math.min(gradientMagnitude[n][m], outMin);
    		                outMax = Math.max(gradientMagnitude[n][m], outMax);
    		            }
    		        }
    		        minImg1[y][x] = outMin;
    		        maxImg1[y][x] = outMax;
    		    }
    		}

    		// Loop through image stack
    		for (i = 1 ; i < minStack.length; i++) {
    		    // Lower level
    		    minImg1 = minStack[i-1];
    		    maxImg1 = maxStack[i-1];
    		 
    		    // Current level
    		    minImg2 = minStack[i];
    		    maxImg2 = maxStack[i];
    		    
    		    for (y = 0; y < yDim; y++) {
    		        for (x = 0; x < xDim; x++) {
    		    	    outMin = Double.MAX_VALUE;
    		    	    outMax = -Double.MAX_VALUE;
    		    	   
    		    	    // Loop through local neighbourhood
    		    	    // To find maximum and minimum values
    		    	    for (n = Math.max(y-1,0); n < Math.min(y+2, yDim); n++) {
    		    	        for (m = Math.max(x-1,0); m < Math.min(x+2, xDim); m++) {
    		    	            outMin = Math.min(minImg1[n][m], outMin);
    		    	            outMax = Math.max(maxImg1[n][m], outMax);
    		    	        }
    		    	    }
    		    	    minImg2[y][x] = outMin;
    		    	    maxImg2[y][x] = outMax;
    		    	}
    		    }
    		}
    	}
    	
    	// =======================================================================
    	// Specific for LUT version of Trilateral Fitler
    	// =======================================================================
    
    	// Bilaterally filters the X and Y gradients of the input image.
    	// To produce smoothed x and y gradients

    	public void BilateralGradientFilterLUT(double xGradient[][], double yGradient[][], double gradientMagnitude[][],
    	                  double sigmaC, double sigmaR, double xGradientSmooth[][], double yGradientSmooth[][]) {
    		int m,n,x,y;
    		// Constants used for domain / range calculations
    		final double domainConst = -2.0 * sigmaC * sigmaC;
    		final double rangeConst = -2.0 * sigmaR * sigmaR;
    		
    		// Compute the weight for the domain filter (domainWeight). 
    		// The domain filter is a Gaussian lowpass filter
    		final int halfSize = (int)(sigmaC - 1.0 / 2.0);
    		double domainWeightLUT[][] = new double[halfSize+1][halfSize+1];
    		
    		for (y = 0; y < halfSize+1; y++) {
    			for (x = 0; x < halfSize+1; x++) {
    				// weight for the domain filter (domainWeight)
    				final double diff = (double) (x*x+y*y);
    				domainWeightLUT[y][x] = Math.exp( diff / domainConst );
    			}
    		}
    		
    		// Loop through image
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				double normFactor = 0.0;
    				double tmpX = 0.0;
    				double tmpY = 0.0;
    				
    				// Calculate Middle Pixel Normalised-gradient
    				final double g2 = gradientMagnitude[y][x];
    				
    				// Loop through local neighbourhood
    				for (n = -halfSize; n <= halfSize; n++) {
    					for (m = -halfSize; m <= halfSize; m++) {
    						// Compute the weight for the domain filter (domainWeight). 
    						final double dWeight = domainWeightLUT[Math.abs(n)][Math.abs(m)];
    						
    						// Only perform calculationg if within bounds
    						final int localX = x + m;
    						if (localX < 0) continue;
    						if (localX >= xDim) continue;
    						
    						final int localY = y + n;
    						if (localY < 0) continue;
    						if (localY >= yDim) continue;
    						
    						// Calculate Local Normalised Gradient
    						final double g1 = gradientMagnitude[localY][localX];
    						
    						// Compute the gradient difference between a pixel and its neighborhood pixel 
    						final double gradDiffSq = Math.pow(g1 - g2, 2);
    						
    						// Compute the weight for the range filter (rangeWeight). The range filter
    						// is a Gaussian filter defined by the difference in gradient magnitude.
    						final double rangeWeight = Math.exp( gradDiffSq / rangeConst );
    						
    						tmpX += xGradient[localY][localX] * dWeight * rangeWeight;
    						tmpY += yGradient[localY][localX] * dWeight * rangeWeight;
    						
    						// Bilateral filter normalized by normFactor
    						normFactor += dWeight * rangeWeight;
    					}
    				}
    				// Set smoothed image to normalised value
    				xGradientSmooth[y][x] = tmpX / normFactor;
    				yGradientSmooth[y][x] = tmpY / normFactor;
    			}
    		}
    	}

    	// =======================================================================
    	// Specific for fast version of Trilateral Fitler
    	// =======================================================================
    	
    	 
    	// Bilaterally filters the X and Y gradients of the input image.
    	// To produce smoothed x and y gradients
    
    	public void BilateralGradientFilter(double xGradient[][], double yGradient[][], double gradientMagnitude[][],
    	            double sigmaC, double sigmaR, double epsilon, double xGradientSmooth[][], double yGradientSmooth[][]) {
    		int m,n,x,y;
    		// Constants used for domain / range calculations
    		final double domainConst = -2.0 * sigmaC * sigmaC;
    		final double rangeConst = -2.0 * sigmaR * sigmaR;
    		
    		// Compute the weight for the domain filter (domainWeight). 
    		// The domain filter is a Gaussian lowpass filter
    		final int halfSize = (int)(sigmaC / 2.0);
    		double domainWeightLUT[][] = new double[halfSize+1][halfSize+1];

            for (y = 0; y < halfSize+1; y++) {
            	for (x = 0; x < halfSize+1; x++) {
            		// weight for the domain filter (domainWeight)
            		final double diff = (double) (x*x+y*y);
            		domainWeightLUT[y][x] = Math.exp( diff / domainConst );
            	}
            }
            
            // Loop through image
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		double normFactor = 0.0;
            		double tmpX = 0.0;
            		double tmpY = 0.0;
            		  
            		// Calculate Middle Pixel Normalised-gradient
            		final double g2 = gradientMagnitude[y][x];
            		 
            	    // Loop through local neighbourhood
            		for (n = -halfSize; n <= halfSize; n++) {
            			for (m = -halfSize; m <= halfSize; m++) {
            				// Compute the weight for the domain filter (domainWeight). 
            				final double dWeight = domainWeightLUT[Math.abs(n)][Math.abs(m)];
            				
            				// Only perform calculation if weight above zero
            				if (dWeight < epsilon ) continue;
            				
            				// Only perform calculation if within bounds
            				final int localX = x + m;
            				if (localX < 0) continue;
            				if (localX >= xDim) continue;
            				   
                            final int localY = y + n;
                            if (localY < 0) continue;
                            if (localY >= yDim) continue;
                            
                            // Calculate Local Normalised Gradient
                            final double g1 = gradientMagnitude[localY][localX];
                            
                            // Compute the gradient difference between a pixel and its neighborhood pixel 
                            final double gradDiffSq = Math.pow(g1 - g2, 2);
                            
                            // Compute the weight for the range filter (rangeWeight). The range filter
                            // is a Gaussian filter defined by the difference in gradient magnitude.
                            final double rangeWeight = Math.exp( gradDiffSq / rangeConst );
                            
                            // Only compute if less than epsilon
                            if (rangeWeight < epsilon) continue;
                            
                            tmpX += xGradient[localY][localX] * dWeight * rangeWeight;
                            tmpY += yGradient[localY][localX] * dWeight * rangeWeight;
                            
                            // Bilateral filter normalized by normFactor
                            normFactor += dWeight * rangeWeight;
            			}
            		}
            		// Set smoothed image to normalised value
            		xGradientSmooth[y][x] = tmpX / normFactor;
            		yGradientSmooth[y][x] = tmpY / normFactor;
            	}
            }
    	}
    	
    	// Filters the detail signal and computes the output (2nd filtering pass for trilateral filter).
    	public void DetailBilateralFilterLUT(double inputImage[][], double adaptiveRegion[][],
    	            double xGradientSmooth[][], double yGradientSmooth[][], double sigmaC, double sigmaR,
    	            int maxDomainSize, double outputImg[][]) {
    		int m,n,x,y;
            // Create constants used throughout code
    		final double domainConst = -2.0 * sigmaC * sigmaC;
    		final double rangeConst = -2.0 * sigmaR * sigmaR;
    		
    		// Define Look Up Table for weightings
    		
    	    // Compute the weight for the domain filter (domainWeight). The domain filter
    		// is a Gaussian lowpass filter

            double domainWeightLUT[][] = new double[maxDomainSize][maxDomainSize];
            for (y = 0; y < maxDomainSize; y++) {
            	for (x = 0; x < maxDomainSize; x++) {
            		// weight for the domain filter (domainWeight)
            		final double diff = (double) (x*x+y*y);
            		domainWeightLUT[y][x] = Math.exp( diff / domainConst );
            	}
            }
            
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		double normFactor = 0.0;
            		double tmp = 0.0;
            		 
            		// filter window width is calculated from adaptive neighbourhood
            		// halfsize is half of the filter window width (or maximum LUT value)
            		final int halfSize = (int) adaptiveRegion[y][x];
            		
            		// Coefficients defining the centerplane is calculated
            		// from the smoothed image gradients
            		// Plane Equation is z = coeffA.x + coeffB.y + coeffC
            		// coeffA = dI/dx, coeffB = dI/dy, coeffC = I at center pixel of the filter kernel
            		final double coeffA = xGradientSmooth[y][x];
            		final double coeffB = yGradientSmooth[y][x];
            	    final double coeffC = inputImage[y][x];

            	    for (n = -halfSize; n <= halfSize; n++) { // y scan line
            	    	for (m = -halfSize; m <= halfSize; m++) { // x scan line
            	            // Get domain weight from LUT
            	    		final double dWeight = domainWeightLUT[Math.abs(n)][Math.abs(m)];
            	    		 
            	    		// Only perform calculationg if within bounds
            	    		final int localX = x + m;
            	    		if (localX < 0) continue;
            	    		if (localX >= xDim) continue;
            	    		 
            	    		final int localY = y + n;
            	    		if (localY < 0) continue;
            	    		if (localY >= yDim) continue;
            	    		  
            	    		// Compute the detail signal (detail) based on the difference between a 
            	    		// neighborhood pixel and the centerplane passing through the center-pixel 
            	    		// of the filter window. 
            	    		final double detail = 
            	    		    inputImage[localY][localX] - coeffA * (double)(m) - coeffB * (double)(n) - coeffC;
            	    		
            	    		// Compute the weight for the range filter (rangeWeight). The range filter
            	    		// is a Gaussian filter defined by the detail signal.
            	    		final double rangeWeight = Math.exp(Math.pow(detail,2) / rangeConst ); 
            	    		 
            	    		// Add to function
            	    		tmp += detail * dWeight * rangeWeight;
            	    		
            	    		// Detail Bilateral filter normalized by normFactor 
            	    		normFactor += dWeight * rangeWeight;

            	    	}
            	    }
            	    // Normalise according to weight
            	    outputImg[y][x] = tmp / normFactor + coeffC;
            	}
            }
    	}

    	// Filters the detail signal and computes the output (2nd filtering pass for trilateral filter).
    	public void DetailBilateralFilter(double inputImage[][], double adaptiveRegion[][],
    	            double xGradientSmooth[][], double yGradientSmooth[][], double sigmaC, double sigmaR,
    	            int maxDomainSize, double epsilon, double outputImg[][]) {
    		int m,n,x,y;
            // Create constants used throughout code
    		final double domainConst = -2.0 * sigmaC * sigmaC;
    		final double rangeConst = -2.0 * sigmaR * sigmaR;

    		// Define Look Up Table for weightings
    		  
    	    // Compute the weight for the domain filter (domainWeight). The domain filter
    		// is a Gaussian lowpass filter
    		double domainWeightLUT[][] = new double[maxDomainSize+1][maxDomainSize+1];

    		for (y = 0; y < maxDomainSize+1 ; y++) {
    	        for (x = 0; x < maxDomainSize+1; x++) {
    			    // weight for the domain filter (domainWeight)
    			    final double diff = (double) (x*x+y*y);
    			    domainWeightLUT[y][x] = Math.exp( diff / domainConst );
    			}
    	    }
    		
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				double normFactor = 0.0;
    				double tmp = 0.0;
    				  
    				// filter window width is calculated from adaptive neighbourhood
    				// halfsize is half of the filter window width (or maximum LUT value)
    				final int halfSize = (int) Math.min( adaptiveRegion[y][x], maxDomainSize );
    				 
    				// Coefficients defining the centerplane is calculated
    				// from the smoothed image gradients
    				// Plane Equation is z = coeffA.x + coeffB.y + coeffC
    				// coeffA = dI/dx, coeffB = dI/dy, coeffC = I at center pixel of the filter kernel
    			    final double coeffA = xGradientSmooth[y][x];
    				final double coeffB = yGradientSmooth[y][x];
    				final double coeffC = inputImage[y][x];
    				 
    				for (n = -halfSize; n <= halfSize; n++)  { // y scan line
    				    for (m = -halfSize; m <= halfSize; m++) { // x scan line
    				        // Get domain weight from LUT
    				        final double dWeight = domainWeightLUT[Math.abs(n)][Math.abs(m)];
    				 
    				        // Only perform calculation if weight above zero
    				        if ( dWeight < epsilon ) continue;
    				   
    				        // Only perform calculationg if within bounds
    				        final int localX = x + m;
    				        if (localX < 0) continue;
    				        if (localX >= xDim) continue;
    				  
    				        final int localY = y + n;
    				        if (localY < 0) continue;
    				        if (localY >= yDim) continue;
    				 
    				        // Compute the detail signal (detail) based on the difference between a 
    				        // neighborhood pixel and the centerplane passing through the center-pixel 
    				        // of the filter window. 
    				        final double detail = 
    				              inputImage[localY][localX] - coeffA * (double)(m) - coeffB * (double)(n) - coeffC;
    				 
    				        // Compute the weight for the range filter (rangeWeight). The range filter
    				        // is a Gaussian filter defined by the detail signal.
    				        final double rangeWeight = Math.exp(Math.pow(detail,2) / rangeConst ); 
    				 
    				        if ( dWeight < epsilon ) continue;
    				 
    				        tmp += detail * dWeight * rangeWeight;
    				  
    				        // Detail Bilateral filter normalized by normFactor (eq. 9, Section 3.1)
    				        normFactor += dWeight * rangeWeight;
    				   }
    			}	  
    	        // Write result to output image
    		    outputImg[y][x] = tmp / normFactor + coeffC;
    			}
    		}

    	}

    	
    	public double[][] copyMakeBorder(double src[][], int top, int bottom, int left, int right, int borderType, double borderValue) {
        	int i,j;
        	double dst[][] = new double[src.length + top + bottom][src[0].length + left + right];
        	for (i = 0; i < src.length; i++) {
        		for (j = 0; j < src[0].length; j++) {
        			dst[i+top][j+left] = src[i][j];
        		}
        	}
        	
        	for (i = 0; i < top; i++) {
        		for (j = 0; j < left; j++) {
        		   switch (borderType) {
        		   case BORDER_CONSTANT:
        			   dst[i][j] = borderValue;
        			   break;
        		   case BORDER_REPLICATE:
        			   dst[i][j] = src[0][0];
        			   break;
        		   case BORDER_REFLECT:
        			   dst[i][j] = src[top - 1 - i][left - 1 - j];
        			   break;
        		   case BORDER_REFLECT_101:
         			   dst[i][j] = src[top - i][left - j];
         			   break;
        		   }
        		  
     		   }
        		
        		for (j = left; j < src[0].length + left; j++) {
        			switch (borderType) {
        			case BORDER_CONSTANT:
        				dst[i][j] = borderValue;
        				break;
        			case BORDER_REPLICATE:
        				dst[i][j] = src[0][j - left];
        				break;
        			case BORDER_REFLECT:
        				dst[i][j] = src[top - 1 - i][j - left];
        				break;
        			case BORDER_REFLECT_101:
        				dst[i][j] = src[top - i][j - left];
        				break;
        			}
        		}
        		
        		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
        		    switch(borderType) {
        		    case BORDER_CONSTANT:
         			   dst[i][j] = borderValue;
         			   break;
        		    case BORDER_REPLICATE:
        		    	dst[i][j] = src[0][src[0].length-1];
        		    	break;
        		    case BORDER_REFLECT:
        		    	dst[i][j] = src[top - 1 - i][2*src[0].length + left - 1 - j];
        		    	break;
        		    case BORDER_REFLECT_101:
        		    	dst[i][j] = src[top - i][2*src[0].length + left - 2 - j];
        		    	break;
        		    }
        		}
        	}
        	
        	for (i = top + src.length; i < top + src.length + bottom; i++) {
                for (j = 0; j < left; j++) {
        		    switch (borderType) {
        		    case BORDER_CONSTANT:
         			   dst[i][j] = borderValue;
         			   break;
        		    case BORDER_REPLICATE:
        		    	dst[i][j] = src[src.length-1][0];
        		    	break;
        		    case BORDER_REFLECT:
        		    	dst[i][j] = src[2*src.length + top - 1 - i][left - 1 - j];
        		    	break;
        		    case BORDER_REFLECT_101:
        		    	dst[i][j] = src[2*src.length + top - 2 - i][left - j];
        		    	break;
        		    }
        		}
                
                for (j = left; j < src[0].length + left; j++) {
        			switch (borderType) {
        			case BORDER_CONSTANT:
        				dst[i][j] = borderValue;
        				break;
        			case BORDER_REPLICATE:
        				dst[i][j] = src[src.length-1][j - left];
        				break;
        			case BORDER_REFLECT:
        				dst[i][j] = src[2*src.length + top - 1 - i][j - left];
        				break;
        			case BORDER_REFLECT_101:
        				dst[i][j] = src[2*src.length + top - 2 - i][j - left];
        				break;
        			}
        		}
        		
        		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
        		    switch(borderType) {
        		    case BORDER_CONSTANT:
         			   dst[i][j] = borderValue;
         			   break;
        		    case BORDER_REPLICATE:
        		    	dst[i][j] = src[src.length-1][src[0].length-1];
        		    	break;
        		    case BORDER_REFLECT:
        		        dst[i][j] = src[2*src.length + top - 1 - i][2*src[0].length + left - 1 - j];
        		        break;
        		    case BORDER_REFLECT_101:
        		        dst[i][j] = src[2*src.length + top - 2 - i][2*src[0].length + left - 2 - j];
        		        break;
        		    }
        		}	
        	}
        	
        	for (i = top; i < src.length + top; i++) {
        		for (j = 0; j < left; j++) {
        			switch(borderType) {
        			case BORDER_CONSTANT:
        				dst[i][j] = borderValue;
        				break;
        			case BORDER_REPLICATE:
        				dst[i][j] = src[i - top][0];
        				break;
        			case BORDER_REFLECT:
        				dst[i][j] = src[i - top][left - 1 - j];
        				break; 
        			case BORDER_REFLECT_101:
        				dst[i][j] = src[i - top][left - j];
        				break;  
        			}
        		}
        		
        		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
        		    switch(borderType) {
        		    case BORDER_CONSTANT:
         			   dst[i][j] = borderValue;
         			   break;
        		    case BORDER_REPLICATE:
        		    	dst[i][j] = src[i - top][src[0].length-1];
        		    	break;
        		    case BORDER_REFLECT:
        		        dst[i][j] = src[i - top][2*src[0].length + left - 1 - j];
        		        break;
        		    case BORDER_REFLECT_101:
        		        dst[i][j] = src[i - top][2*src[0].length + left - 2 - j];
        		        break;
        		    }
        		}	
        	}
        	
        	
        	return dst;
        }
    	
    }