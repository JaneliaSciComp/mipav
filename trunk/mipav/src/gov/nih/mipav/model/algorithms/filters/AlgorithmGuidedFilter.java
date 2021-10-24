package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

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
	Reference:
        K.He, J.Sun, and X.Tang. Guided Image Filtering. TPAMI'12.
        Original C++ source code by Atilim Cetin
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
	private int yDim;
	private int xDim;
	private int length;
	private int r;
	private double eps;
	private double I[][];
	private double mean_I[][];
	private double var_I[][];
	private double Ichannels[][][];
	private double mean_I_r[][];
	private double mean_I_g[][];
	private double mean_I_b[][];
	private double invrr[][];
	private double invrg[][];
	private double invrb[][];
	private double invgg[][];
	private double invgb[][];
	private double invbb[][];
	
	public AlgorithmGuidedFilter() {
		
	}
	
	public AlgorithmGuidedFilter(ModelImage destImg, ModelImage srcImg, ModelImage guidedImage, int r, double eps) {
		super(destImg, srcImg);
		this.guidedImage = guidedImage;
		this.r = r;
		this.eps = eps;
	}
	
	public void runAlgorithm() {
		xDim = guidedImage.getExtents()[0];
		yDim = guidedImage.getExtents()[1];
		length = xDim * yDim;
		if (!guidedImage.isColorImage()) {
			GuidedFilterMono();
		}
		else {
			GuidedFilterColor();
		}
	}
	
	public void GuidedFilterMono() {
		ModelImage outputImage;
		if (destImage != null) {
			outputImage = destImage;
		}
		else {
			outputImage = srcImage;
		}
		int i, x, y;
		// The guided image is gray scale
		double buffer[] = new double[length];
		double p2[][] = new double[yDim][xDim];
		double result[][];
		try {
			guidedImage.exportData(0, length, buffer);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on guidedImage.exportData(0, length, buffer");
			setCompleted(false);
			return;
		}
		I = new double[yDim][xDim];
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
			    I[y][x] = buffer[x + y*xDim];	
			}
		}
		mean_I = boxfilter(I, r);
		double Isquared[][] = new double[yDim][xDim];
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				Isquared[y][x] = I[y][x] * I[y][x];
			}
		}
		double mean_II[][] = boxfilter(Isquared, r);
		var_I = new double[yDim][xDim];
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				var_I[y][x] = mean_II[y][x] - (mean_I[y][x] * mean_I[y][x]);	
			}
		}


		if (!srcImage.isColorImage()) {
			try {
				srcImage.exportData(0, length, buffer);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, length, buffer");
				setCompleted(false);
				return;
			} 
			
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					p2[y][x] = buffer[x + y*xDim];
				}
			}
			result = filterSingleChannel(p2);
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					buffer[x + y*xDim] = result[y][x];
				}
			}
			
			try {
				outputImage.importData(0, buffer, true);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException " + e + " on outputImage.importData(0, buffer, true)");
				setCompleted(false);
				return;
			}
			setCompleted(true);
			return;
		} // if (!srcImage.isColorImage())
		else { // srcImage.isColorImage()
		    float floatBuf[][] = new float[3][length];
		    for (i = 1; i <= 3; i++) {
		        try {
		        	srcImage.exportRGBData(i, 0, length, floatBuf[i-1]);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData(" + i + ", 0, length, floatBuf["+(i-1)+"])");
					setCompleted(false);
					return;	
		        }
		        for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						p2[y][x] = floatBuf[i-1][x + y*xDim];
					}
				}
				result = filterSingleChannel(p2);
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						floatBuf[i-1][x + y*xDim] = (float)result[y][x];
					}
				}
		    } // for (i = 1; i <= 3; i++)
		    
		    for (i = 1; i <= 3; i++) {
		    	try {
		    		outputImage.importRGBData(i, 0, floatBuf[i-1], false);
		    	}
		    	catch (IOException e) {
		    		MipavUtil.displayError("IOException " + e + " on outputImage.importRGBData("+i+", 0, floatBuf["+(i-1)+"], false)");
		    		setCompleted(false);
		    		return;
		    	}
		    }
		    outputImage.calcMinMax();
		    setCompleted(true);
		    return;
		} // else srcImage.isColorImage();
	}
	
	private double[][] filterSingleChannel(double p[][]) {
		int x, y;
		double mean_p[][] = boxfilter(p, r);
		double Ip[][] = new double[yDim][xDim];
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				Ip[y][x] = I[y][x] * p[y][x];
			}
		}
	    double mean_Ip[][] = boxfilter(Ip, r);
	    double cov_Ip[][] = new double[yDim][xDim];
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		cov_Ip[y][x] = mean_Ip[y][x] - (mean_I[y][x] * mean_p[y][x]); // this is the covariance of (I, p) in each local patch.	
	    	}
	    }
	    
        double a[][] = new double[yDim][xDim];
        double b[][] = new double[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
	            a[y][x] = cov_Ip[y][x] / (var_I[y][x] + eps); // Eqn. (5) in the paper;
	            b[y][x] = mean_p[y][x] - (a[y][x] * mean_I[y][x]); // Eqn. (6) in the paper;
        	}
        }

	    double mean_a[][] = boxfilter(a, r);
	    double mean_b[][] = boxfilter(b, r);
	    
	    double result[][] = new double[yDim][xDim];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
	            result[y][x] = (mean_a[y][x] * I[y][x]) + mean_b[y][x];
        	}
        }
        return result;
	}
	
	public void GuidedFilterColor() {
		// Guided image is color image
		ModelImage outputImage;
		if (destImage != null) {
			outputImage = destImage;
		}
		else {
			outputImage = srcImage;
		}
		int i, x, y;
		double buffer[] = new double[length];
		float floatBuf[][] = new float[3][length];
		double p2[][] = new double[yDim][xDim];
		double result[][];
		Ichannels = new double[3][yDim][xDim];
	    for (i = 1; i <= 3; i++) {
	        try {
	        	guidedImage.exportRGBData(i, 0, length, floatBuf[i-1]);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException " + e + " on guidedImage.exportRGBData(" + i + ", 0, length, floatBuf[("+(i-1)+"])");
				setCompleted(false);
				return;	
	        }
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		Ichannels[i-1][y][x] = floatBuf[i-1][x + y*xDim];
	        	}
	        }
	    } // for (i = 1; i <= 3; i++)
	    


	    mean_I_r = boxfilter(Ichannels[0], r);
	    mean_I_g = boxfilter(Ichannels[1], r);
	    mean_I_b = boxfilter(Ichannels[2], r);
	    
	    // variance of I in each local patch: the matrix Sigma in Eqn (14).
	    // Note the variance in each local patch is a 3x3 symmetric matrix:
	    //           rr, rg, rb
	    //   Sigma = rg, gg, gb
	    //           rb, gb, bb
	    double var_I_rr[][] = add(sub(boxfilter(mul(Ichannels[0],Ichannels[0]), r),mul(mean_I_r,mean_I_r)),eps);
	    double var_I_rg[][] = sub(boxfilter(mul(Ichannels[0],Ichannels[1]), r),mul(mean_I_r,mean_I_g));
	    double var_I_rb[][] = sub(boxfilter(mul(Ichannels[0],Ichannels[2]), r),mul(mean_I_r,mean_I_b));
	    double var_I_gg[][] = add(sub(boxfilter(mul(Ichannels[1],Ichannels[1]), r),mul(mean_I_g,mean_I_g)),eps);
	    double var_I_gb[][] = sub(boxfilter(mul(Ichannels[1],Ichannels[2]), r),mul(mean_I_g,mean_I_b));
	    double var_I_bb[][] = add(sub(boxfilter(mul(Ichannels[2],Ichannels[2]), r),mul(mean_I_b,mean_I_b)),eps);

	    // Inverse of Sigma + eps * I
	    invrr = sub(mul(var_I_gg,var_I_bb),mul(var_I_gb,var_I_gb));
	    invrg = sub(mul(var_I_gb,var_I_rb),mul(var_I_rg,var_I_bb));
	    invrb = sub(mul(var_I_rg,var_I_gb),mul(var_I_gg,var_I_rb));
	    invgg = sub(mul(var_I_rr,var_I_bb),mul(var_I_rb,var_I_rb));
	    invgb = sub(mul(var_I_rb,var_I_rg),mul(var_I_rr,var_I_gb));
	    invbb = sub(mul(var_I_rr,var_I_gg),mul(var_I_rg,var_I_rg));

	    double covDet[][] = add(add(mul(invrr,var_I_rr),mul(invrg,var_I_rg)),mul(invrb,var_I_rb));

	    invrr = div(invrr,covDet);
	    invrg = div(invrg,covDet);
	    invrb = div(invrb,covDet);
	    invgg = div(invgg,covDet);
	    invgb = div(invgb,covDet);
	    invbb = div(invbb,covDet);
	    
	    if (!srcImage.isColorImage()) {
			try {
				srcImage.exportData(0, length, buffer);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, length, buffer");
				setCompleted(false);
				return;
			} 
			
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					p2[y][x] = buffer[x + y*xDim];
				}
			}
			result = filterSingleColorChannel(p2);
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					buffer[x + y*xDim] = result[y][x];
				}
			}
			
			try {
				outputImage.importData(0, buffer, true);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException " + e + " on outputImage.importData(0, buffer, true)");
				setCompleted(false);
				return;
			}
			setCompleted(true);
			return;
		} // if (!srcImage.isColorImage())
		else { // srcImage.isColorImage()
		    floatBuf = new float[3][length];
		    for (i = 1; i <= 3; i++) {
		        try {
		        	srcImage.exportRGBData(i, 0, length, floatBuf[i-1]);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData(" + i + ", 0, length, floatBuf["+(i-1)+"])");
					setCompleted(false);
					return;	
		        }
		        for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						p2[y][x] = floatBuf[i-1][x + y*xDim];
					}
				}
				result = filterSingleColorChannel(p2);
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						floatBuf[i-1][x + y*xDim] = (float)result[y][x];
					}
				}
		    } // for (i = 1; i <= 3; i++)
		    
		    for (i = 1; i <= 3; i++) {
		    	try {
		    		outputImage.importRGBData(i, 0, floatBuf[i-1], false);
		    	}
		    	catch (IOException e) {
		    		MipavUtil.displayError("IOException " + e + " on outputImage.importRGBData("+i+", 0, floatBuf["+(i-1)+"], false)");
		    		setCompleted(false);
		    		return;
		    	}
		    }
		    outputImage.calcMinMax();
		    setCompleted(true);
		    return;
		} // else srcImage.isColorImage();
	}
	
	private double[][] filterSingleColorChannel(double p[][]) {
	    double mean_p[][] = boxfilter(p, r);

	    double mean_Ip_r[][] = boxfilter(mul(Ichannels[0],p), r);
	    double mean_Ip_g[][] = boxfilter(mul(Ichannels[1],p), r);
	    double mean_Ip_b[][] = boxfilter(mul(Ichannels[2],p), r);

	    // covariance of (I, p) in each local patch.
	    double cov_Ip_r[][] = sub(mean_Ip_r,mul(mean_I_r,mean_p));
	    double cov_Ip_g[][] = sub(mean_Ip_g,mul(mean_I_g,mean_p));
	    double cov_Ip_b[][] = sub(mean_Ip_b,mul(mean_I_b,mean_p));

	    double a_r[][] = add(add(mul(invrr,cov_Ip_r),mul(invrg,cov_Ip_g)),mul(invrb,cov_Ip_b));
	    double a_g[][] = add(add(mul(invrg,cov_Ip_r),mul(invgg,cov_Ip_g)),mul(invgb,cov_Ip_b));
	    double a_b[][] = add(add(mul(invrb,cov_Ip_r),mul(invgb,cov_Ip_g)),mul(invbb,cov_Ip_b));

	    double b[][] = sub(sub(sub(mean_p,mul(a_r,mean_I_r)),mul(a_g,mean_I_g)),mul(a_b,mean_I_b)); // Eqn. (15) in the paper;

	    double result[][] = add(add(add(mul(boxfilter(a_r, r),Ichannels[0]),
	          mul(boxfilter(a_g, r),Ichannels[1])),
	          mul(boxfilter(a_b, r),Ichannels[2])),
	          boxfilter(b, r));  // Eqn. (16) in the paper;
	    
	    return result;
	}

	
	private double[][] mul(double a[][], double b[][]) {
	    int h = a.length;
	    int w = a[0].length;
	    double result[][] = new double[h][w];
	    int y,x;
	    for (y = 0; y < h; y++) {
	    	for (x = 0; x < w; x++) {
	    		result[y][x] = a[y][x] * b[y][x];
	    	}
	    }
	    return result;
	}
	
	private double[][] sub(double a[][], double b[][]) {
	    int h = a.length;
	    int w = a[0].length;
	    double result[][] = new double[h][w];
	    int y,x;
	    for (y = 0; y < h; y++) {
	    	for (x = 0; x < w; x++) {
	    		result[y][x] = a[y][x] - b[y][x];
	    	}
	    }
	    return result;
	}
	
	private double[][] add(double a[][], double b) {
		int h = a.length;
	    int w = a[0].length;
	    double result[][] = new double[h][w];
	    int y,x;
	    for (y = 0; y < h; y++) {
	    	for (x = 0; x < w; x++) {
	    		result[y][x] = a[y][x] + b;
	    	}
	    }
	    return result;
	}
	
	private double[][] add(double a[][], double b[][]) {
		int h = a.length;
	    int w = a[0].length;
	    double result[][] = new double[h][w];
	    int y,x;
	    for (y = 0; y < h; y++) {
	    	for (x = 0; x < w; x++) {
	    		result[y][x] = a[y][x] + b[y][x];
	    	}
	    }
	    return result;
	}
	
	private double[][] div(double a[][], double b[][]) {
		int h = a.length;
	    int w = a[0].length;
	    double result[][] = new double[h][w];
	    int y,x;
	    for (y = 0; y < h; y++) {
	    	for (x = 0; x < w; x++) {
	    		result[y][x] = a[y][x]/b[y][x];
	    	}
	    }
	    return result;
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
	
	private double[][] boxfilter(double img[][], int r) {
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