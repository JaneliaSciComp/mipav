package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.DiscreteWavelet;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.MODE;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.WAVELET_NAME;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Arrays;

   /**
	The MIT License (MIT)
	
	Copyright (c) 2014 Atilim Cetin
	
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


public class AlgorithmGuidedFilter extends AlgorithmBase {
	
	 //~ Instance fields ------------------------------------------------------------------------------------------------
	
	public final int BORDER_CONSTANT = 0; // iiiiii|abcdefgh|iiiiiii with some specified i
	public final int BORDER_REPLICATE = 1; // aaaaaa|abcdefgh|hhhhhhh
	public final int BORDER_REFLECT = 2; // fedcba|abcdefgh|hgfedcb
	public final int BORDER_WRAP = 3; // cdefgh|abcdefgh|abcdefg
	public final int BORDER_REFLECT_101 = 4; // gfedcb|abcdefgh|gfedcba
	public final int BORDER_DEFAULT = BORDER_REFLECT_101;
	private ModelImage guidedImage;
	private int r;
	private double eps;
	
	public AlgorithmGuidedFilter() {
		
	}
	
	public AlgorithmGuidedFilter(ModelImage destImg, ModelImage srcImg, ModelImage guidedImage, int r, double eps) {
		super(destImg, srcImg);
		this.guidedImage = guidedImage;
		this.r = r;
		this.eps = eps;
	}
	
	public void runAlgorithm() {
		
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
	
	private double[][] boxFilter(double img[][], int r) {
		double imgpad[][] = copyMakeBorder(img, r, r, r, r, BORDER_REPLICATE, 0.0);
		int h = img.length;
		int w = img[0].length;
		double result[][] = new double[h][w];
		double area = (2*r+1)*(2*r+1);
		double sum;
		int y,x,i,j;
		for (y = r; y < h + r; y++) {
			for (x = r; x < w + r; x++) {
			    sum = 0.0;
			    for (i = -r; i <= r; i++) {
			    	for (j = -r; j <= r; j++) {
			    		sum += imgpad[y + i][x + j];
			    	}
			    }
			    result[y-r][x-r] = sum/area;
			}
		}
		return result;
	}
}