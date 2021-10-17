package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Arrays;

import Jama.Matrix;

    /**
     * This is a port of guided.py in the image_denoising_matching package by Yiqian Wang and You-Yi Jau.
     * References:
     * 1.) Feature preserving image denoising with multiresolution filters by
     *     Yiqian Wang and You-Yi Jau.
     * 2.) Guided Image Filtering by Kaiming He, Jian Sun, and Xiaoon Tang.
     */

    public class AlgorithmGuidedFilter extends AlgorithmBase {
    	private ModelImage guidedImage;
    	// Filter radius
    	private int radius = 3;
    	// Value controlling sharpness
    	private double eps = 1.0E-5;
    	private int xDim;
    	private int yDim;
    	
    	public AlgorithmGuidedFilter(ModelImage destImg, ModelImage srcImg, ModelImage guidedImage,
    			int radius, double eps) {
    		 super(destImg, srcImg);
    		 this.guidedImage = guidedImage;
    		 this.radius = radius;
    		 this.eps = eps;
    	}
    	
    	public void runAlgorithm() {
    		if (guidedImage.isColorImage()) {
    		    MultiDimGuidedFilter();	
    		}
    		else {
    			GrayGuidedFilter();
    		}
    	}
    	
    	public void GrayGuidedFilter() {
    	     // Specific guided filter for gray guided image	
    		 ModelImage outputImage;
      	     if (destImage != null) {
      		     outputImage = destImage;
      	     }
      	     else {
      		     outputImage = srcImage;
      	     }
    		int i,y,x;
    		xDim = srcImage.getExtents()[0];
    	    yDim = srcImage.getExtents()[1];
    		int length = xDim * yDim;
    		double buffer[] = new double[length];
    		double src[][] = new double[yDim][xDim];
    		double guided[][] = new double[yDim][xDim];
    		AlgorithmBilateralFilter bf = new AlgorithmBilateralFilter();
    		try {
    			srcImage.exportData(0, length, buffer);
    		}
    		catch (IOException e) {
    			MipavUtil.displayError("IOException on srcImage.exportData(0, length, buffer)");
    			setCompleted(false);
    			return;
    		}
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				src[y][x] = buffer[x + y*xDim];
    			}
    		}
    		
    		double guidedMin = guidedImage.getMin();
    		double guidedMax = guidedImage.getMax();
    		double denom = guidedMax - guidedMin;
    		try {
    			guidedImage.exportData(0, length, buffer);
    		}
    		catch (IOException e) {
    			MipavUtil.displayError("IOException on guidedImage.exportData(0, length, buffer)");
    			setCompleted(false);
    			return;
    		}
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				guided[y][x] = (buffer[x + y*xDim] - guidedMin)/denom;
    			}
    		}
    		
    		// Step 1
    		double guided_pad[][] = bf.copyMakeBorder(guided, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		double meanGuided[][] = boxFilter(guided_pad);
    		double src_pad[][] = bf.copyMakeBorder(src, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		double meanSource[][] = boxFilter(src_pad);
    		double guidedSquared[][] = new double[yDim][xDim];
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				guidedSquared[y][x] = guided[y][x] * guided[y][x];
    			}
    		}
    		double guidedSquared_pad[][] = bf.copyMakeBorder(guidedSquared, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		double corrGuided[][] = boxFilter(guidedSquared_pad);
    		double guidedSource[][] = new double[yDim][xDim];
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				guidedSource[y][x] = guided[y][x] * src[y][x];
    			}
    		}
    		double guidedSource_pad[][] = bf.copyMakeBorder(guidedSource, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		double corrGuidedSource[][] = boxFilter(guidedSource_pad);
    		
    		// Step 2
    		double varGuided[][] = new double[yDim][xDim];
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				varGuided[y][x] = corrGuided[y][x] - meanGuided[y][x] * meanGuided[y][x];
    			}
    		}
    		
    		double covGuidedSource[][] = new double[yDim][xDim];
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				covGuidedSource[y][x] = corrGuidedSource[y][x] - meanGuided[y][x] * meanSource[y][x];
    			}
    		}
    		
    		// Step 3
    		double a[][] = new double[yDim][xDim];
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				a[y][x] = covGuidedSource[y][x] / (varGuided[y][x] + eps);	
    			}
    		}
    		double b[][] = new double[yDim][xDim];
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				b[y][x] = meanSource[y][x] - a[y][x] * meanGuided[y][x];	
    			}
    		}
    		
    		// Step 4
    		double a_pad[][] = bf.copyMakeBorder(a, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		double meana[][] = boxFilter(a_pad);
    		double b_pad[][] = bf.copyMakeBorder(b, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		double meanb[][] = boxFilter(b_pad);
    		
    		// Step 5
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				buffer[x + y*xDim] = meana[y][x] * guided[y][x] + meanb[y][x]; 
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
    	}
    	
    	public double[][] boxFilter(double input[][]) {
    		int x,y,i,j;
    		double out[][] = new double[yDim][xDim];
    		int width = 2*radius + 1;
    		double area = width * width;
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    			    for (i = -radius; i <= radius; i++) {
    			    	for (j = -radius; j <= radius; j++) {
    			    		out[y][x] += input[y + radius + i][x + radius + j];
    			    	}
    			    }
    			    out[y][x] = out[y][x]/area;
    			}
    		}
    		return out;
    	}
    	
    	public void MultiDimGuidedFilter() {
    		// Specific guided filter for color guided image
    	    // or multi-dimensional feature map.
    		// Guided image is color
    		// Source image is black and white
    		// Guided and source images must be different images
    		// Destination image is color image
     	    AlgorithmBilateralFilter bf = new AlgorithmBilateralFilter();
    		xDim = srcImage.getExtents()[0];
    		yDim = srcImage.getExtents()[1];
    		int length = xDim * yDim;
    		float floatBuf[] = new float[length];
    		double guidedMin = guidedImage.getMin();
    		double guidedMax = guidedImage.getMax();
    		double denom = guidedMax - guidedMin;
    		double guided[][][] = new double[3][yDim][xDim];
    		double buffer[] = new double[length];
    		double src[][] = new double[yDim][xDim];
    		int i, j, y, x;
    		try {
				guidedImage.exportRGBData(1, 0, length, floatBuf);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("IOException on guidedImage.exportRGBData(1, 0, length, floatBuf)");
		    	setCompleted(false);
		    	return;
		    }
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    			    guided[0][y][x] = (floatBuf[x + y*xDim] - guidedMin)/denom;	
    			}
    		}
    		
    		try {
				guidedImage.exportRGBData(2, 0, length, floatBuf);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("IOException on guidedImage.exportRGBData(2, 0, length, floatBuf)");
		    	setCompleted(false);
		    	return;
		    }
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    			    guided[1][y][x] = (floatBuf[x + y*xDim] - guidedMin)/denom;	
    			}
    		}
    		
    		try {
				guidedImage.exportRGBData(3, 0, length, floatBuf);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("IOException on guidedImage.exportRGBData(3, 0, length, floatBuf)");
		    	setCompleted(false);
		    	return;
		    }
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    			    guided[2][y][x] = (floatBuf[x + y*xDim] - guidedMin)/denom;	
    			}
    		}
    		
    		try {
    			srcImage.exportData(0, length, buffer);
    		}
    		catch (IOException e) {
    			MipavUtil.displayError("IOException on srcImage.exportData(0, length, buffer)");
    			setCompleted(false);
    			return;
    		}
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				src[y][x] = buffer[x + y*xDim];
    			}
    		}
    		
    		double guided_pad[][][] = new double[3][][];
    		for (i = 0; i < 3; i++) {
    			guided_pad[i] = bf.copyMakeBorder(guided[i], radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		}
    		double meanGuided[][][] = new double[3][][];
    		for (i = 0; i < 3; i++) {
    			meanGuided[i] = boxFilter(guided_pad[i]);
    		}
    		double src_pad[][] = bf.copyMakeBorder(src, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    		double meanSource[][] = boxFilter(src_pad);
    		double corrGuided_[][][][] = new double[3][3][yDim][xDim];
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				for (i = 0; i < 3; i++) {
    					for (j = 0; j < 3; j++) {
    				        corrGuided_[i][j][y][x] = guided[i][y][x]*guided[j][y][x];
    					}
    				}
    			}
    		}
    		double corrGuided_pad[][][][] = new double[3][3][][];
    		for (i = 0; i < 3; i++) {
    			for (j = 0; j < 3; j++) {
    			    corrGuided_pad[i][j] = bf.copyMakeBorder(corrGuided_[i][j], radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    			}
    		}
    		double corrGuided[][][][] = new double[3][3][][];
    		for (i = 0; i < 3; i++) {
    			for (j = 0; j < 3; j++) {
    				corrGuided[i][j] = boxFilter(corrGuided_pad[i][j]);
    			}
    		}
    		
    	    for (y = 0; y < yDim; y++) {
    	    	for (x = 0; x < xDim; x++) {
    	    		for (i = 0; i < 3; i++) {
    	    			corrGuided[i][i][y][x] += eps;
    	    		}
    	    	}
    	    }
    	    
    	    Matrix mat = new Matrix(3,3);
    	    Matrix matInv;
    	    double left[][][][] = new double[3][3][yDim][xDim];
    	    for (y = 0; y < yDim; y++) {
    	    	for (x = 0; x < xDim; x++) {
    	    	    for (i = 0; i < 3; i++) {
    	    	    	for (j = 0; j < 3; j++) {
    	    	    		mat.set(i, j, corrGuided[i][j][y][x]);
    	    	    	}
    	    	    }
    	    	    matInv = mat.inverse();
    	    	    for (i = 0; i < 3; i++) {
    	    	    	for (j = 0; j < 3; j++) {
    	    	    		left[i][j][y][x] = matInv.get(i, j);
    	    	    	}
    	    	    }
    	    	}
    	    }
    	    
    	    double guidedSource[][][] = new double[3][yDim][xDim];
    	    for (i = 0; i < 3; i++) {
    	    	for (y = 0; y < yDim; y++) {
    	    		for (x = 0; x < xDim; x++) {
    	    			guidedSource[i][y][x] = guided[i][y][x] * src[y][x];
    	    		}
    	    	}
    	    }
    	    double guidedSource_pad[][][] = new double[3][][];
    	    for (i = 0; i < 3; i++) {
    	    	guidedSource_pad[i] = bf.copyMakeBorder(guidedSource[i], radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    	    }
    	    double corrGuidedSource[][][] = new double[3][][];
    	    for (i = 0; i < 3; i++) {
    	    	corrGuidedSource[i] = boxFilter(guidedSource_pad[i]);
    	    }
    	    double covGuidedSource[][][] = new double[3][][];
    	    for (i = 0; i < 3; i++) {
    	    	for (y = 0; y < yDim; y++) {
    	    		for (x = 0; x < xDim; x++) {
    	    			covGuidedSource[i][y][x] = corrGuidedSource[i][y][x] - meanGuided[i][y][x] * meanSource[y][x];
    	    		}
    	    	}
    	    }
    	    
    	    double a[][][] = new double[3][yDim][xDim];
    	    for (i = 0; i < 3; i++) {
    	    	for (j = 0; j < 3; j++) {
    	    		for (y = 0; y < yDim; y++) {
    	    			for (x = 0; x < xDim; x++) {
    	    				a[i][y][x] += (left[i][j][y][x] * covGuidedSource[j][y][x]);
    	    			}
    	    		}
    	    	}
    	    }
    	    
    	    double axmeanGuided[][] = new double[yDim][xDim];
    	    for (i = 0; i < 3; i++) {
    	    	for (y = 0; y < yDim; y++) {
    	    		for (x = 0; x < xDim; x++) {
    	    			axmeanGuided[y][x] += (a[i][y][x] * meanGuided[i][y][x]);
    	    		}
    	    	}
    	    }
    	    
    	    double b[][] = new double[yDim][xDim];
    	    for (y = 0; y < yDim; y++) {
    	    	for (x = 0; x < xDim; x++) {
    	    		 b[y][x] = meanSource[y][x] - axmeanGuided[y][x];
    	    	}
    	    }
    	    
    	    double a_pad[][][] = new double[3][][];
    	    for (i = 0; i < 3; i++) {
    	    	a_pad[i] = bf.copyMakeBorder(a[i], radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    	    }
    	    double meana[][][] = new double[3][][];
    	    for (i = 0; i < 3; i++) {
    	    	meana[i] = boxFilter(a_pad[i]);
    	    }
    	    double b_pad[][] = bf.copyMakeBorder(b, radius, radius, radius, radius, bf.BORDER_REFLECT_101, 0.0);
    	    double meanb[][] = boxFilter(b_pad);
    	    
	    	float colorBuffer[][] = new float[3][length];
    	    for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    			for (i = 0; i < 3; i++) {
	    			    colorBuffer[i][x + y*xDim] = (float)((meana[i][y][x] * guided[i][y][x]) + meanb[y][x]);
	    			}
	    		}
	    	}
	    	
	    	for (i = 0; i < 3; i++) {
	    	    try {
			    	destImage.importRGBData((i+1), 0, colorBuffer[i], false);
			    }
			    catch (IOException e) {
			    	MipavUtil.displayError("IOException on destImage.importRGBData("+(i+1)+", 0, colorBuffer["+i+"], false)");
			    	setCompleted(false);
			    	return;
			    }
	    	}
	    	destImage.calcMinMax();
		    
		    setCompleted(true);
		    return;
    	}
    }
