package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.IOException;

public class AlgorithmSFTA extends AlgorithmBase  {
	
	/**Copyright (c) 2013, Alceu Costa
	Copyright (c) 2013, GBDI-ICMC-USP
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are
	met:

	    * Redistributions of source code must retain the above copyright
	      notice, this list of conditions and the following disclaimer.
	    * Redistributions in binary form must reproduce the above copyright
	      notice, this list of conditions and the following disclaimer in
	      the documentation and/or other materials provided with the distribution

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
	ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
	LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
	SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
	CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
	ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
	
	Ported from MATLAB to Java by William Gandler
	
	SFTA extracts texture features from the grayscale image I using the SFTA 
    algorithm (Segmentation-based Fractal Texture Analysis).

    Returns a 1 by 6*nt vector D extracted from the input grayscale image I 
    using the SFTA (Segmentation-based Fractal Texture Analysis) algorithm. The
    feature vector corresponds to texture information extracted from the input
    image I.

    If necessary, the input image is converted to a grayscale image with 
    bit-depth of 8.

    Reference:
    Costa, A. F., G. E. Humpire-Mamani, A. J. M. Traina. 2012. "An Efficient 
     Algorithm for Fractal Analysis of Textures." In SIBGRAPI 2012 (XXV 
     Conference on Graphics, Patterns and Images), 39-46, Ouro Preto, Brazil.

    Author
    ------
    Alceu Ferraz Costa 
    email: alceufc [at] icmc [dot] usp [dot] br

	*/
	
	private int nt;
	
	private int numBins;
	
	private short T[];
	
	private int counts[];
	
	private double D[];
	
	public AlgorithmSFTA(ModelImage srcImg, int nt) {
		super(null, srcImg);
		this.nt = nt;
	}
	
	/**
	 * 
	 * @return
	 */
	public double[] getD() {
	    return D;	
	}
	
	public void runAlgorithm() {
		ModelImage image;
		float redValue;
		float greenValue;
		float blueValue;
		AlgorithmRGBtoGray gAlgo;
		boolean thresholdAverage = false;
		float threshold = 0.0f;
		boolean intensityAverage = false;
		boolean equalRange = true;
		float minR = 0.0f;
		float minG = 0.0f;
		float minB = 0.0f;
		float maxR;
		float maxG;
		float maxB;
		AlgorithmChangeType changeTypeAlgo;
		boolean image25D = false;
		short buffer[];
		int length;
		int i;
		int dSize;
		int pos;
		short thresh;
		byte Ib[];
		int j;
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int valLength;
		double vals[];
		double sumVals;
		int inc;
		short lowerThresh;
		short upperThresh;
		
		fireProgressStateChanged(srcImage.getImageName(), "Extracting texture features ...");
		
		// If necessary convert I to a grayscale image with a bit depth of 8.
		if (srcImage.isColorImage()) {
		    image = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "grayImage");
		    if (srcImage.getMinR() == srcImage.getMaxR()) {
                redValue = 0.0f;
                greenValue = 0.5f;
                blueValue = 0.5f;
            }
            else if (srcImage.getMinG() ==srcImage.getMaxG()) {
            	redValue = 0.5f;
            	greenValue = 0.0f;
            	blueValue = 0.5f;
            }
            else if (srcImage.getMinB() == srcImage.getMaxB()) {
            	redValue = 0.5f;
            	greenValue = 0.5f;
            	blueValue = 0.0f;
            }
            else {
            	redValue = (float)(1.0/3.0);
            	greenValue = redValue;
            	blueValue = redValue;
            	
            }
            maxR = (float)srcImage.getMaxR();
            maxG = (float)srcImage.getMaxG();
            maxB = (float)srcImage.getMaxB();
            gAlgo = new AlgorithmRGBtoGray(image, srcImage, redValue, greenValue, blueValue, thresholdAverage, threshold, intensityAverage,
            		equalRange, minR, maxR, minG, maxG, minB, maxB);
            gAlgo.run();
            gAlgo.finalize();
		} // if (srcImage.isColorImage())
		else if ((srcImage.getType() == ModelStorageBase.FLOAT) || (srcImage.getType() == ModelStorageBase.DOUBLE) ||
				(srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getMin() < 0) || (srcImage.getMax() > 255)) {
			 image = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "grayImage");
			 changeTypeAlgo = new AlgorithmChangeType(image, srcImage, srcImage.getMin(), srcImage.getMax(), 0.0, 255.0, image25D);
			 changeTypeAlgo.run();
			 changeTypeAlgo.finalize();
		}
		else {
			image = srcImage;
		}
		length = xDim * yDim;
		
		buffer = new short[length];
		try {
			image.exportData(0, length, buffer);
		}
		catch(IOException e) {
			MipavUtil.displayError("IOException " + e + " on image.exportData(0, length, buffer");
			setCompleted(false);
			return;
		}
		if (image != srcImage) {
			image.disposeLocal();
			image = null;
		}
		otsurec(buffer, nt);
		counts = null;
		dSize = 6 * nt;
		D = new double[dSize];
		pos = 0;
		Ib = new byte[length];
		for (i = 0; i < nt; i++) {
		    thresh = T[i];
		    valLength = 0;
		    for (j = 0; j < length; j++) {
		    	if (buffer[j] > thresh) {
		    		Ib[j] = 1;
		    		valLength++;
		    	}
		    	else {
		    		Ib[j] = 0;
		    	}
		    }
		    Ib = findBorders(Ib , xDim, yDim);
		    
		    vals = new double[valLength];
		    sumVals = 0.0;
		    inc = 0;
		    for (j = 0; j < length; j++) {
		        if (Ib[j] == 1) {
		        	vals[inc++] = (double)buffer[j];
		        	sumVals += vals[j];
		        }
		    }
		    
		    D[pos] = hausDim(Ib, xDim, yDim);
		    pos++;
		    
		    D[pos] = sumVals/valLength;
		    pos++;
		    
		    D[pos] = valLength;
		    pos++;
		} // for (i = 0; i < nt; i++)
		
		for (i = 0; i < nt; i++) {
		    lowerThresh = T[i];
		    if (i < nt - 1) {
		    	upperThresh = T[i+1];
		    }
		    else {
		    	upperThresh = 255;
		    }
		    
		    valLength = 0;
		    for (j = 0; j < length; j++) {
		    	if ((buffer[j] > lowerThresh) && (buffer[j] < upperThresh)) {
		    		Ib[j] = 1;
		    		valLength++;
		    	}
		    	else {
		    		Ib[j] = 0;
		    	}
		    } // for (j = 0; j < length; j++)
            Ib = findBorders(Ib , xDim, yDim);
		    
		    vals = new double[valLength];
		    sumVals = 0.0;
		    inc = 0;
		    for (j = 0; j < length; j++) {
		        if (Ib[j] == 1) {
		        	vals[inc++] = (double)buffer[j];
		        	sumVals += vals[j];
		        }
		    }
		    
		    D[pos] = hausDim(Ib, xDim, yDim);
		    pos++;
		    
		    D[pos] = sumVals/valLength;
		    pos++;
		    
		    D[pos] = valLength;
		    pos++;
		} // for (i = 0; i < nt; i++)
		T = null;
		setCompleted(true);
		return;
	}
	
	/**
	 HAUSDIM Returns the Haussdorf fractal dimension of an object represented by
     a binary image.

     Returns the Haussdorf fractal dimension D of an object represented by the
     binary image I. Nonzero pixels belong to an object and 0 pixels 
     constitute the background.
 
     Algorithm
     ---------
     1 - Pad the image with background pixels so that its dimensions are a 
         power of 2.
     2 - Set the box size 'e' to the size of the image.
     3 - Compute N(e), which corresponds to the number of boxes of size 'e' 
         which contains at least one object pixel.
     4 - If e > 1 then e = e / 2 and repeat step 3.
     5 - Compute the points log(N(e)) x log(1/e) and use the least squares 
         method to fit a line to the points.
     6 - The returned Haussdorf fractal dimension D is the slope of the line.
 
     Author
     ------
     Alceu Ferraz Costa 
     email: alceufc [at] icmc [dot] usp [dot] br 

	 * @param I
	 * @return
	 */
	private double hausDim(byte[] I, int xDim, int yDim) {
		double D;
		int maxDim;
		int newDimSize;
		byte Ip[];
		int y;
		int x;
		int ceilDim;
		int boxCounts[];
		double resolutions[];
		int iSize;
		int boxSize;
		int boxesPerDim;
		int idx;
		int boxCount;
		int minBox[];
		int maxBox[];
		int minBoxSize;
		int maxBoxSize;
		int i;
		int inc;
		int boxRow;
		int boxCol;
		boolean objFound;
		int row;
		int col;
		double logr;
		double logb;
		double sumX;
		double sumXY;
		double sumY;
		double sumX2;
		// Pad the image with background pixels so that its dimensions are a power of 2
		maxDim = Math.max(xDim, yDim);
		ceilDim = (int)Math.ceil(Math.log(maxDim)/Math.log(2.0));
		newDimSize = (int)Math.round(Math.pow(2, ceilDim));
		Ip = new byte[newDimSize * newDimSize];
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				Ip[x + y * newDimSize] = I[x + y * xDim];
			}
		}
		
		boxCounts = new int[ceilDim];
		resolutions = new double[ceilDim];
		
		iSize = yDim;
		boxSize = iSize;
		boxesPerDim = 1;
		idx = -1;
		while (boxSize >= 1) {
		    boxCount = 0;
		    minBoxSize = 0;
		    for (i = 0; i <= (iSize - boxSize); i += boxSize) {
		        minBoxSize++;	
		    }
		    minBox = new int[minBoxSize];
		    for (inc = 0, i = 0; i <= (iSize - boxSize); i += boxSize) {
		    	minBox[inc++] = i;
		    }
		    maxBoxSize = 0;
		    for (i = boxSize - 1; i <= iSize - 1; i += boxSize) {
		    	maxBoxSize++;
		    }
		    maxBox = new int[maxBoxSize];
		    for (inc = 0, i = boxSize - 1; i <= iSize - 1; i += boxSize) {
		    	maxBox[inc++] = i;
		    }
		    
		    for (boxRow = 0; boxRow < boxesPerDim; boxRow++) {
		    	for (boxCol = 0; boxCol < boxesPerDim; boxCol++) {
		    	    objFound = false;
		    	    for (row = minBox[boxRow]; row <= maxBox[boxRow]; row++) {
		    	    	for (col = minBox[boxCol]; col <= maxBox[boxCol]; col++) {
		    	    	    if (Ip[col + row * newDimSize] == 1) {
		    	    	    	boxCount++;
		    	    	    	objFound = true;
		    	    	    	break;
		    	    	    }
		    	    	}
		    	    	if (objFound) {
		    	    		break;
		    	    	}
		    	    }
		    	}
		    }
		    idx++;
		    boxCounts[idx] = boxCount;
		    resolutions[idx] = 1.0/(double)boxSize;
		    
		    boxesPerDim = boxesPerDim * 2;
		    boxSize = boxSize / 2;
		} // while (boxSize >= 1)
		sumX = 0.0;
	    sumXY = 0.0;
	    sumY = 0.0;
	    sumX2 = 0.0;
	    for (i = 0; i < ceilDim; i++) {
	    	logr = Math.log(resolutions[i]);
	    	logb = Math.log(boxCounts[i]);
	        sumX += logr;
	        sumXY += (logr * logb);
	        sumY += logb;
	        sumX2 += (logr * logr);
	    }
	    D = (sumXY - sumX*sumY/ceilDim)/(sumX2 - sumX*sumX/ceilDim);
		return D;
	}
	
	
	/**
	 FINDBORDERS Returns an binary image with the regions' boundaries of the input 
     image I.

     FINDBORDERS returns a binary image with the regions' boundaries of the 
     input image I. The input image I must be a binary image. The returned image
     Im takes the value 1 if the corresponding pixel in I has the value 1 and 
     at least one neighboring pixel with value 0. Otherwise Im takes the value
     0.
 
     Author
     ------
     Alceu Ferraz Costa 
     email: alceufc [at] icmc [dot] usp [dot] br

	 * @param I
	 * @return
	 */
	private byte[] findBorders(byte I[], int xDim, int yDim) {
	    byte Im[] = new byte[I.length];
	    byte Ip[] = new byte[(xDim + 2)*(yDim + 2)];
	    int x;
	    int y;
	    boolean bkgFound;
	    int i;
	    int j;
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		Ip[(x + 1) + (y + 1)*(xDim + 2)] = I[x + y * xDim];
	    	}
	    }
	    for (y = 0; y < yDim+2; y++) {
	    	Ip[y*(xDim+2)] = 1;
	    	Ip[xDim+1 + y*(xDim+2)] = 1;
	    }
	    for (x = 0; x < xDim+2; x++) {
	    	Ip[x] = 1;
	    	Ip[x + (yDim+1)*(xDim+2)] = 1;
	    }
	    
	    bkgFound = false;
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		if (Ip[x+1 + (y+1)*(xDim+2)] == 1) {
	    			bkgFound  = false;
	    			for (i = 0; i <= 2; i++) {
	    				for (j = 0; j <= 2; j++) {
	    					if (Ip[x + j + (y + i)*(xDim+2)] == 0) {
	    						Im[x + y * xDim] = 1;
	    						bkgFound = true;
	    						break;
	    					}
	    				}
	    				if (bkgFound) {
	    					break;
	    				}
	    			}
	    		}
	    	}
	    }
	    return Im;
	}
	
	/**
	 OTSUREC Returns a set of thresholds for the input image using the multi-level
     otsu algorith.
 
     OTSUREC computes a set T of thresholds from the input image I employing the
     multi-level Otsu algorithm. The multi-level Otsu algorithm consists in 
     finding the threshold that minimizes the input image intra-class variance.
     Then, recursively, the Otsu algorithm is applied to each image region until
     ttotal threholds are found.

	 * @param I
	 * @param ttotal
	 * @return
	 */
	private void otsurec(short[] I, int ttotal) {
	    numBins = 256;
	    int counts[] = new int[numBins];
	    int i;
	    for (i = 0; i < I.length; i++) {
		    counts[I[i]]++;
	    }
	    T = new short[ttotal];
	    otsurec_helper(0, numBins-1, 0, ttotal-1);
	    
	    return;
	}
	
	private void otsurec_helper(int lowerBin, int upperBin, int tLower, int tUpper) {
		int pCounts[];
		int i;
		int level;
		int insertPos;
		if ((tUpper < tLower) || (lowerBin >= upperBin)) {
			return;
		}
		else {
		    pCounts = new int[upperBin - lowerBin + 1];	
		    for (i = lowerBin; i <= upperBin; i++) {
		    	pCounts[i-lowerBin] = counts[i];
		    }
		    level = otsu(pCounts) + lowerBin;
		    insertPos = (int)Math.ceil((double)(tLower + tUpper)/2.0);
		    // Dividing by numBins gives threshold as a fraction of the maximum possible range value
		    // Don't divide to give T as an actual gray value
		    //T[insertPos] = (short)(level/numBins);
		    T[insertPos] = (short)level;
		    otsurec_helper(lowerBin, level, tLower, insertPos - 1);
		    otsurec_helper(level+1, upperBin, insertPos + 1, tUpper);
		}
	}
	
	private int otsu(int counts[]) {
		// Variable names are chosen to be similar to the formulas in the Otsu paper
		int sum = 0;
		int i;
		double p[];
		double omega[];
		double mu[];
		double mu_t;
		double sigma_b_squared[];
		double diff;
		boolean allNaNorInfinite;
		double maxval;
		int firstIndex;
		int numLocations;
		int locationSum;
		for (i = 0; i < counts.length; i++) {
			sum += counts[i];
		}
		p = new double[counts.length];
		for (i = 0; i < counts.length; i++) {
			p[i] = (double)counts[i]/(double)sum;
		}
		omega = new double[counts.length];
		omega[0] = p[0];
		for (i = 1; i < counts.length; i++) {
		    omega[i] = omega[i-1] + p[i];	
		}
		mu = new double[counts.length];
		mu[0] = p[0];
		for (i = 1; i < counts.length; i++) {
			mu[i] = mu[i-1] + p[i] * (i+1);
		}
		mu_t = mu[counts.length-1];
		sigma_b_squared = new double[counts.length];
		// Find the location of the maximum value of sigma_b_squared.
	    // The maximum may extend over several bins, so average together the
	    // locations.  If maxval is NaN or infinite, meaning that sigma_b_squared is all NaN or infinite,
	    //  then return 0.
		allNaNorInfinite = true;
		maxval = -Double.MAX_VALUE;
		firstIndex = -1;
		for (i = 0; i < counts.length; i++) {
			diff = mu_t * omega[i] - mu[i];
			sigma_b_squared[i] = (diff *diff)/(omega[i] * (1.0 - omega[i]));
			if ((!Double.isNaN(sigma_b_squared[i])  && (!Double.isInfinite(sigma_b_squared[i])))) {
			    allNaNorInfinite = false;	
			    if (sigma_b_squared[i] > maxval) {
			    	maxval = sigma_b_squared[i];
			    	firstIndex = i;
			    }
			} // if ((!Double.isNaN(sigma_b_squared[i])  && (!Double.isInfinite(sigma_b_squared[i]))))
		} // for (i = 0; i < counts.length; i++)
		if (allNaNorInfinite) {
			return 0;
		}
		numLocations = 0;
		locationSum = 0;
	    for (i = firstIndex; i < counts.length; i++) {
	        if (sigma_b_squared[i] == maxval) {
	            locationSum += i;
	            numLocations++;
	        }
	    }
        return (int)Math.round((double)locationSum/(double)numLocations);
	}
	
}