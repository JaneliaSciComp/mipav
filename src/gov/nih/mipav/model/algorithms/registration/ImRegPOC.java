package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.structures.*;
import Jama.Matrix;

import gov.nih.mipav.view.*;

import java.io.IOException;

/**
 * 
 * @author aailb
 * This is a port of the MATLAB code poc_prototype.m by Yoshi Ri at the University of Tokyo updated on 2017/8/11.
 * This code is ported to Java with the 2-clause BSD license:
 * The 2-clause BSD License

Copyright (c) 2018, Yoshi Ri
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of the FreeBSD Project.

The warpPolar routine is derived from warpPolar in the Open Source Computer Vision Library with the license:
/*M///////////////////////////////////////////////////////////////////////////////////////
	//
	//  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
	//
	//  By downloading, copying, installing or using the software you agree to this license.
	//  If you do not agree to this license, do not download, install,
	//  copy or use the software.
	//
	//
	//                           License Agreement
	//                For Open Source Computer Vision Library
	//
	// Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
	// Copyright (C) 2009, Willow Garage Inc., all rights reserved.
	// Copyright (C) 2014-2015, Itseez Inc., all rights reserved.
	// Third party copyrights are property of their respective owners.
	//
	// Redistribution and use in source and binary forms, with or without modification,
	// are permitted provided that the following conditions are met:
	//
	//   * Redistribution's of source code must retain the above copyright notice,
	//     this list of conditions and the following disclaimer.
	//
	//   * Redistribution's in binary form must reproduce the above copyright notice,
	//     this list of conditions and the following disclaimer in the documentation
	//     and/or other materials provided with the distribution.
	//
	//   * The name of the copyright holders may not be used to endorse or promote products
	//     derived from this software without specific prior written permission.
	//
	// This software is provided by the copyright holders and contributors "as is" and
	// any express or implied warranties, including, but not limited to, the implied
	// warranties of merchantability and fitness for a particular purpose are disclaimed.
	// In no event shall the Intel Corporation or contributors be liable for any direct,
	// indirect, incidental, special, exemplary, or consequential damages
	// (including, but not limited to, procurement of substitute goods or services;
	// loss of use, data, or profits; or business interruption) however caused
	// and on any theory of liability, whether in contract, strict liability,
	// or tort (including negligence or otherwise) arising in any way out of
	// the use of this software, even if advised of the possibility of such damage.
	//
	//M*/


/*This program provides robust image registration method using "Phase Correlation" technique.

With this toolbox, you can estimate 
Translation, Rotation and Scaling between two 2D gray scale images with identical dimensions.
That is you can calculate 4 degrees of freedom.  You must use a global scale factor. You
cannot use separate x and y scale factors.  You cannot calculate shear.

The merit of this method is that you can detect the success of this estimation via checking certain value.
You can use isSucceed() function to check it. (0/1 to failure/succeed)

References:
1.) Y. Ri and H. Fujimoto, "Drift-free Motion Estimation from Video Images using Phase Correlation and Linear Optimization,"
 in The 15th International Workshop on Advanced Motion Control, 2018, pp. 295-300.
 2.) Y. Ri and H. Fujimoto, "Image Based Visual Servo Application on Video Tracking with Monocular Camera Based on Phase Correlation Method," 
 The 3rd IEEJ international workshop on Sensing, Actuation, Motion Control, and Optimization, 2017.
 3.) Kenji Takita, Takafumi Aoki, Yoshifumi Sasaki, Tatsuo Higuchi, and Koji Kobayashi, "High-Accuracy Subpixel Image
 Registration Based on Phase-Only Correlation," IEEE Trans. Fundamentals, Vol. E86-A, No.8, August, 2003, pp. 1925-1934.

 *
 */

public class ImRegPOC extends AlgorithmBase {
	
	/** The inputImage will be registered to this reference image. */
    private ModelImage refImage;
    
    /** This image is to registered to the reference image. */
    private ModelImage inputImage;
    
    private int height, width, length;
    
    private boolean isMATLABVersion = true;
    
    private double alpha = 0.5;
    
    private double beta = 0.8;
    
    private double hanw[];
    
    public ImRegPOC(final ModelImage dstImage, final ModelImage imageA, final ModelImage imageB) {
    	super(dstImage, imageB);
        refImage = imageA;
        inputImage = imageB;
        isMATLABVersion = true;
    }
    
    public ImRegPOC(final ModelImage dstImage, final ModelImage imageA, final ModelImage imageB,
    		double alpha, double beta) {
    	super(dstImage, imageB);
        refImage = imageA;
        inputImage = imageB;
        this.alpha = alpha;
        this.beta = beta;
        isMATLABVersion = false;
    }
    
    public void runAlgorithm() {
    	if (refImage.getNDims() != 2) {
			MipavUtil.displayError("In ImRegPOC refImage has " + refImage.getNDims() + " dimensions instead of the required 2 dimensions");
			setCompleted(false);
			return;
		}
		
		if (inputImage.getNDims() != 2) {
			MipavUtil.displayError("In ImRegPOC inputImage has " + inputImage.getNDims() + " dimensions instead of the required 2 dimensions");
			setCompleted(false);
			return;
		}
		
		if (refImage.isColorImage()) {
			MipavUtil.displayError("In ImRegPOC refImage cannot be a color image");
			setCompleted(false);
			return;
		}
		
		if (inputImage.isColorImage()) {
			MipavUtil.displayError("In ImRegPOC inputImage cannot be a color image");
			setCompleted(false);
			return;
		}
		
		if (refImage.isComplexImage()) {
			MipavUtil.displayError("In ImRegPOC refImage cannot be a complex image");
			setCompleted(false);
			return;
		}
		
		if (inputImage.isComplexImage()) {
			MipavUtil.displayError("In ImRegPOC inputImage cannot be a complex image");
			setCompleted(false);
			return;
		}
		
		if (inputImage.getExtents()[0] != refImage.getExtents()[0]) {
			MipavUtil.displayError("In imRegPOC inputImage.getExtents()[0] = != refImage.getExtents()[0]");
			setCompleted(false);
			return;
		}
		
		if (inputImage.getExtents()[1] != refImage.getExtents()[1]) {
			MipavUtil.displayError("In imRegPOC inputImage.getExtents()[1] = != refImage.getExtents()[1]");
			setCompleted(false);
			return;
		}
		if (isMATLABVersion) {
			runMATLABVersion();
		}
		else {
			runPythonVersion();
		}
    }
    
    public void runPythonVersion() {
        ModelImage outputImage;
        double ref[];
        double cmp[];
        double Diff[] = new double[2];
        double peak[] = new double[1];
        double G_a[];
        double G_aImag[];
        double G_b[];
        double G_bImag[];
        double Amag[];
        double Bmag[];
        double LA[][];
        double LB[][];
        double LPA[];
        double LPB[];
        double LPA_filt[];
        double LPB_filt[];
        int i,j;
        double cx;
        double cy;
        double Mag;
        double r;
        double x,y;
        int x0,y0,x1,y1;
        double w0,w1,h0,h1;
        double val;
        int LPmin;
        int LPmax;
        double r_rotatescale[][];
        double theta1;
        double theta2;
        double invscale;
        double b1[];
        double b2[];
        double diff1[] = new double[2];
        double peak1[] = new double[1];
        double r1[][];
        double diff2[] = new double[2];
        double peak2[] = new double[1];
        double r2[][];
        double Trans[];
        double theta;
        double outOfBoundsValue;
	    if (destImage != null) {
		    outputImage = destImage;
	    }
	    else {
		    outputImage = srcImage;
	    }
	   
	    height = refImage.getExtents()[1];
		width = refImage.getExtents()[0];
		length = width * height;
		ref = new double[length];
		try {
			refImage.exportData(0, length, ref);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on refImage.exportData(0, length, ref)");
			setCompleted(false);
			return;
		}
		
		cmp = new double[length];
		try {
			inputImage.exportData(0, length, cmp);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on inputImage.exportData(0, length, cmp)");
			setCompleted(false);
			return;
		}
		
		cx = (double)width/2.0;
		cy = (double)height/2.0;
		hanw = createHanningWindow();
		
		// Windowing and FFT
		G_a = new double[length];
		G_aImag = new double[length];
		for (i = 0; i < length; i++) {
		    G_a[i] = ref[i]*hanw[i];
		}
		FFTUtility fftA = new FFTUtility(G_a, G_aImag, height, width, 1, -1, FFTUtility.FFT);
		fftA.run();
		FFTUtility fftA2 = new FFTUtility(G_a, G_aImag, 1, height, width, -1, FFTUtility.FFT);
		fftA2.run();
		
		G_b = new double[length];
		G_bImag = new double[length];
		for (i = 0; i < length; i++) {
		    G_b[i] = cmp[i]*hanw[i];
		}
		FFTUtility fftB = new FFTUtility(G_b, G_bImag, height, width, 1, -1, FFTUtility.FFT);
		fftB.run();
		FFTUtility fftB2 = new FFTUtility(G_b, G_bImag, 1, height, width, -1, FFTUtility.FFT);
		fftB2.run();
		
		// 1.1: Frequency Whitening
		Amag = new double[length];
		Bmag = new double[length];
		for (i = 0; i < length; i++) {
			Amag[i] = Math.sqrt(G_a[i]*G_a[i] + G_aImag[i]*G_aImag[i]);
			Bmag[i] = Math.sqrt(G_b[i]*G_b[i] + G_bImag[i]*G_bImag[i]);
		}
		
		for (i = 0; i < length; i++) {
			Amag[i] = Math.log(Amag[i] + 1.0);
			Bmag[i] = Math.log(Bmag[i] + 1.0);
		}
		LA = fftshift(Amag, height, width);
	    LB = fftshift(Bmag, height, width);
	    
	    // 1.2: Log polar Transformation
        Mag = (double)width/Math.log(width);
        // logPolar with maxRadius = Mag calls warpPolar with
        // double M = maxRadius > 0 ? std::exp(ssize.width/maxRadius) : 1
        // so M = exp(width/(width/log(width)) = exp(log(width)) = width
        // warpPolar is called with the WARP_POLAR_LOG flag set
        // flags=cv2.INTER_LINEAR+cv2.WARP_FILL_OUTLIERS+cv2.WARP_POLAR_FLAG
        // remap code for warpPolar is overly complicated; just use logPolar
        // from MATLAB code.
       LPA = new double[length];
       LPB = new double[length];
        for (i= 0; i <= width-1; i++) {
		    r = Math.pow(width,(double)(i)/(double)width);
		    for (j = 0; j <=height-1; j++) {
		        x=r*Math.cos(2*Math.PI*j/height)+cx;
		        if ((x >= 0) && (x < width - 1)) {
			    y=r*Math.sin(2*Math.PI*j/height)+cy;
			        if ((y >= 0) && (y < height-1)) {
			             x0 = (int)Math.floor(x);
			             y0 = (int)Math.floor(y);
			             x1 = x0 + 1;
			             y1 = y0 + 1;
			            w0=x1-x;
			            w1=x-x0;
			            h0=y1-y;
			            h1=y-y0;
			            // Bilinear Interpolation
			            val=LA[y0][x0]*w0*h0 + LA[y0][x1]*w1*h0+ LA[y1][x0]*w0*h1 + LA[y1][x1]*w1*h1;
			            LPA[i + j*width] =val;
			            val=LB[y0][x0]*w0*h0 + LB[y0][x1]*w1*h0+ LB[y1][x0]*w0*h1 + LB[y1][x1]*w1*h1;
			            LPB[i + j*width] = val;
			        }
		        }
		    }
		}
        
        // 1.3:filtering
        LPmin = (int)Math.floor(Mag*Math.log(alpha*width/2.0/Math.PI));
        LPmax = Math.min(width, (int)Math.floor(Mag*Math.log(width*beta/2)));
        if (LPmax <= LPmin) {
        	MipavUtil.displayError("LPmax <= LPmin.  Increase LPmax or decrease LPmin");
        	setCompleted(false);
        	return;
        }
        LPA_filt = new double[length];
        LPB_filt = new double[length];
        for (j = 0; j < height; j++) {
        	for (i = LPmin-1; i <= LPmax-1; i++) {
        		LPA_filt[i + j*width] = LPA[i + j*width];
        		LPB_filt[i + j*width] = LPB[i + j*width];
        	}
        }
        
        // 1.4: Phase Correlate to Get Rotation and Scaling
        r_rotatescale = PhaseCorrelation(LPA_filt,LPB_filt, Diff, peak);
        theta1 = 2*Math.PI * Diff[1] / height; // deg
        theta2 = theta1 + Math.PI; // deg theta ambiguity
        invscale = Math.exp(Diff[0]/Mag);
        // 2.1: Correct rotation and scaling
        b1 = Warp_4dof(cmp,0.0,0.0,theta1,invscale,0.0);
        b2 = Warp_4dof(cmp,0.0,0.0,theta2,invscale,0.0);
        
        // 2.2 : Translation estimation
        r1 = PhaseCorrelation(ref,b1,diff1,peak1);
        r2 = PhaseCorrelation(ref,b2,diff2,peak2);
        
        // 2.3: Compare peaks and choose true rotational error
        if (peak1[0] > peak2[0]) {
            Trans = diff1;
            peak[0] = peak1[0];
            theta = -theta1;
        }
        else {
            Trans = diff2;
            peak[0] = peak2[0];
            theta = -theta2;
        }
        
        if (theta > Math.PI) {
            theta -= Math.PI*2;
        }
        else if (theta < -Math.PI) {
            theta += Math.PI*2;
        }
        
        outOfBoundsValue = inputImage.getMin();
        double result[] = Warp_4dof(cmp, -Trans[0],-Trans[1], -theta, invscale, outOfBoundsValue);
        
        try {
      	    outputImage.importData(0, result, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on outputImage.importData(0, result, true)");
      		setCompleted(false);
      		return;
      	}
  	
  	    setCompleted(true);
  	    return;
    }
    
    // Warp Image based on poc parameter
    private double[] Warp_4dof(double Img[], double dx, double dy, double theta, double scale, double outOfBoundsValue) {
    	int x,y,i;
    	int x0, x1, y0, y1;
    	double w0, w1, h0, h1;
    	double val;
        double cx = (double)width/2.0;
        double cy = (double)height/2.0;
        Matrix Affine = poc2warp(cx, cy, dx, dy, theta, scale);
        // Affine is inverted when the flag WARP_INVERSE_MAP is not set
        //outImg = cv2.warpPerspective(Img, Affine, (cols,rows), cv2.INTER_LINEAR)
        double M[][] = (Affine.inverse()).getArray();
        double outImg[] = new double[length];
        for (i = 0; i < length; i++) {
        	outImg[i] = outOfBoundsValue;
        }
        for (y = 0; y < height; y++) {
        	for (x = 0; x < width; x++) {
        		double denom = M[2][0]*x + M[2][1]*y + M[2][2];
        		double xnumerator = M[0][0]*x + M[0][1]*y + M[0][2];
        		double imgx = xnumerator/denom;
        		if ((imgx >= 0) && (imgx <= width-1)) {
        			double ynumerator = M[1][0]*x + M[1][1]*y + M[1][2];
        			double imgy = ynumerator/denom;
        			if ((imgy >= 0) && (imgy <= height-1)) {
        			 x0 = (int)Math.floor(imgx);
   		             y0 = (int)Math.floor(imgy);
   		             if (x0 == width - 1) {
   		            	 x1 = x0;
   		             }
   		             else {
   		                 x1 = x0 + 1;
   		             }
   		             if (y0 == height-1) {
   		            	 y1 = y0;
   		             }
   		             else {
   		                 y1 = y0 + 1;
   		             }
   		            w0=x1-imgx;
   		            w1=imgx-x0;
   		            h0=y1-imgy;
   		            h1=imgy-y0;
   		          
   		            val=Img[y0*width + x0]*w0*h0 + Img[y0*width + x1]*w1*h0+ Img[y1*width + x0]*w0*h1 + Img[y1*width + x1]*w1*h1;
   		            outImg[y*width + x]=val;	
        			}
        		}
        	}
        }
        return outImg;
    }
        		
	private Matrix poc2warp(double cx, double cy, double dx, double dy, double theta, double scale) {
        double cs = scale * Math.cos(theta);
        double sn = scale * Math.sin(theta);
        
        Matrix Rot = new Matrix(new double[][] {{cs, sn, 0},{-sn, cs,0},{0,0,1}});
        Matrix center_Trans = new Matrix(new double[][] {{1,0,cx},{0,1,cy},{0,0,1}});
        Matrix center_iTrans = new Matrix(new double[][] {{1,0,-cx},{0,1,-cy},{0,0,1}});
        Matrix cRot = (center_Trans.times(Rot)).times(center_iTrans);
        
        Matrix Trans = new Matrix(new double[][] {{1,0,dx},{0,1,dy},{0,0,1}});
        Matrix Affine = cRot.times(Trans);
        return Affine;
	}
    
    // Correlation
    private double[][] PhaseCorrelation(double a[], double b[],double Diff[], double peak[]) {
    	int i,j;
    	int DY, DX;
    	double rmax;
    	double Rmag;
    	double r[][];
    	int boxsize;
    	double box[][];
    	int halfbox;
    	double T[];
    	double TY;
    	double TX;
    	double sDY;
    	double sDX;
    	// FFT
    	double G_a[] = new double[length];
    	double G_aImag[] = new double[length];
    	double G_b[] = new double[length];
    	double G_bImag[] = new double[length];
    	for (i = 0; i < length; i++) {
    		G_a[i] = a[i] * hanw[i];
    		G_b[i] = b[i] * hanw[i];
    	}
    	FFTUtility fftA = new FFTUtility(G_a, G_aImag, height, width, 1, -1, FFTUtility.FFT);
		fftA.run();
		FFTUtility fftA2 = new FFTUtility(G_a, G_aImag, 1, height, width, -1, FFTUtility.FFT);
		fftA2.run();
		
		FFTUtility fftB = new FFTUtility(G_b, G_bImag, height, width, 1, -1, FFTUtility.FFT);
		fftB.run();
		FFTUtility fftB2 = new FFTUtility(G_b, G_bImag, 1, height, width, -1, FFTUtility.FFT);
		fftB2.run();
		
		double R[] = new double[length];
		double RImag[] = new double[length];
		for (i = 0; i < length; i++) {
			R[i] = G_a[i]*G_b[i] + G_aImag[i]*G_bImag[i];
			RImag[i] = -G_a[i]*G_bImag[i] + G_aImag[i]*G_b[i];
			Rmag = Math.sqrt(R[i]*R[i] + RImag[i]*RImag[i]);
			R[i] /= Rmag;
			RImag[i] /= Rmag;
		}
		
		// Real part of inverse transform
		FFTUtility fftR = new FFTUtility(R, RImag, height, width, 1, 1, FFTUtility.FFT);
	    fftR.run();
		FFTUtility fftR2 = new FFTUtility(R, RImag, 1, height, width, 1, FFTUtility.FFT);
		fftR2.run();
		r = fftshift(R, height, width);
		
		// Get result and interpolation
		rmax = -Double.MAX_VALUE;
		DX = -1;
		DY = -1;
		for (i = 0; i < height; i++) {
			for (j = 0; j < width; j++) {
				if (r[i][j] > rmax) {
					rmax = r[i][j];
					DY = i;
					DX = j;
				}
			}
		}
		
		// Subpixel Axccuracy
		boxsize = 5;
		box = new double[boxsize][boxsize];
		halfbox = (boxsize - 1)/2;
		for (i = 0; i < boxsize; i++) {
			for (j = 0; j < boxsize; j++) {
				box[i][j] = r[DY + i - halfbox][DX + j - halfbox];
			}
		}
		// TY,TX= CenterOfGravity(box)
		T = WeightedCOG(box);
		TY = T[0];
		TX = T[1];
		sDY = TY+DY;
		sDX = TX+DX;
		// Show the result
		Diff[0] = Math.floor(width/2)-sDX;
		Diff[1] = Math.floor(height/2)-sDY;
		peak[0] = r[DY][DX];
		return r;
    }
    
    private double[] WeightedCOG(double mat[][]) {
    	int i,j;
    	double peak;
    	double val;
    	double newmat[][];
    	double Res[];
    	if (mat.length == 0) {
    		System.out.println("Skip subpixel estimation");
    		Res = new double[] {0.0, 0.0};
    		return Res;
    	}
    	else {
    		peak = -Double.MAX_VALUE;
    		for (i = 0; i < mat.length; i++) {
    			for (j = 0; j < mat[i].length; j++) {
    				if (mat[i][j] > peak) {
    					peak = mat[i][j];
    				}
    			}
    		}
    		newmat = new double[mat.length][];
    		for (i = 0; i < mat.length; i++) {
    			newmat[i] = new double[mat[i].length];
    		}
    		val = peak/10.0;
    		for (i = 0; i < mat.length; i++) {
    			for (j = 0; j < mat[i].length; j++) {
    			    if (mat[i][j] > val) {
    			    	newmat[i][j] = mat[i][j];
    			    }
    			}
    		}
    		Res = CenterOfGravity(newmat);
    		return Res;
    	}
    }
    
    // Get peak point
    private double[] CenterOfGravity(double mat[][]) {
    	double A[];
    	double Tile[];
    	int i,j;
    	double off;
    	int hei = mat.length;
    	int wid = mat[0].length;
    	double Ax;
    	double Ay;
    	double Sum;
    	double Sumx;
    	double Sumy;
    	if (hei != wid) {
    		// if mat size is not square, there must be something wrong
    		System.out.println("Skip subpixel estimation");
    		A = new double[] {0.0, 0.0};
    		return A;
    	}
        Tile = new double[wid];
        off = (wid - 1.0)/2.0;
    	for (i = 0; i < wid; i++) {
    		Tile[i] = (double)i - off;
    	}
    	Sum = 0.0;
    	for (i = 0; i < hei; i++) {
    		for (j = 0; j < wid; j++) {
    			Sum += mat[i][j];
    		}
    	}
    	Sumx = 0.0;
    	for (i = 0; i < hei; i++) {
    		for (j = 0; j < wid; j++) {
    			Sumx += mat[i][j] * Tile[j];
    		}
    	}
    	Ax = Sumx/Sum;
    	Sumy = 0.0;
    	for (i = 0; i < hei; i++) {
    		for (j = 0; j < wid; j++) {
    			Sumy += mat[i][j] * Tile[i];
    		}
    	}
    	Ay = Sumy/Sum;
    	A = new double[] {Ay,Ax};
    	return A;
    }
    
    /*private void warpPolar(double _src[][], double _dst[][], int width, int height, 
           double centerx, double centery, double maxRadius, int flags) {
        // if dest size is empty given than calculate using proportional setting
        // thus we calculate needed angles to keep same area as bounding circle
        if ((width <= 0) && (height <= 0))
        {
            width = (int)Math.round(maxRadius);
            height = (int)Math.round(maxRadius * Math.PI);
        }
        else if (height <= 0)
        {
            height = (int)Math.round(width * Math.PI);
        }
        
        double mapx[][] = new double[height][width];
        double mapy[][] = new double[height][width];
        boolean semiLog = (flags & WARP_POLAR_LOG) != 0;
        if ((flags & WARP_INVERSE_MAP) == 0) {
            double Kangle = 2.0*Math.PI / height;
            int phi, rho;


            // precalculate scaled rho
            double bufRhos[] = new double[width];
            if (semiLog)
            {
                double Kmag = Math.log(maxRadius) / width;
                for (rho = 0; rho < width; rho++)
                    bufRhos[rho] = Math.exp(rho * Kmag) - 1.0;


            }
            else
            {
                double Kmag = maxRadius / width;
                for (rho = 0; rho < width; rho++)
                    bufRhos[rho] = (rho * Kmag);
            }
            for (phi = 0; phi < height; phi++)
            {
                double KKy = Kangle * phi;
                double cp = Math.cos(KKy);
                double sp = Math.sin(KKy);


                for (rho = 0; rho < width; rho++)
                {
                    double x = bufRhos[rho] * cp + centerx;
                    double y = bufRhos[rho] * sp + centery;


                    mapx[phi][rho] = x;
                    mapy[phi][rho] = y;
                }
            }
            //remap(_src, _dst, mapx, mapy, flags & cv::INTER_MAX, (flags & CV_WARP_FILL_OUTLIERS) ? cv::BORDER_CONSTANT : cv::BORDER_TRANSPARENT);
            // flags & CV::INTER_MAX gives INTER_LINEAR
            // (flags & CV_WARP_FILL_OUTLIERS) gives BORDER_CONSTANT
        } // if ((flags & WARP_INVERSE_MAP) == 0)

    }*/

    
    private double[] createHanningWindow() {
    	int i,j;
    	double window[] = new double[length];
    	double coeff0 = 2.0 * Math.PI / (double)(width - 1);
    	double coeff1 = 2.0 * Math.PI / (double)(height - 1);
    	double wc[] = new double[width];
        for (j = 0; j < width; j++) {
            wc[j] = 0.5 * (1.0 - Math.cos(coeff0 * j));
        }
        
        for(i = 0; i < height; i++)
        {
            double wr = 0.5 * (1.0 - Math.cos(coeff1 * i));
            for(j = 0; j < width; j++) {
                window[i*width + j] = wr * wc[j];
            }
        }
        return window;
    }
	
	
	public void runMATLABVersion() {
	   ModelImage outputImage;
   	   if (destImage != null) {
   		   outputImage = destImage;
   	   }
   	   else {
   		   outputImage = srcImage;
   	   }
		int cy,cx;
		double han_win[];
		//double Rhan_win[];
		double AI[];
		double BI[];
		double IA[];
		double IB[][];
		double B[];
		double IAImag[];
		double BImag[];
		double At[];
		double AtImag[];
		double Bt[];
		double BtImag[];
		double Amag[];
		double Bmag[];
		double As[][];
		double Bs[][];
		double lpcA[];
		double lpcB[];
		double lpcAImag[];
		double lpcBImag[];
		double Ap;
		double ApImag;
		double Bp;
		double BpImag;
		double Pp[];
		double PpImag[];
		double PpShift[][];
		double mag;
		double LPmin;
		int i,j;
		double r;
		double x,y;
		int x0,y0,x1,y1;
		double w0,w1,h0,h1;
		double val;
		double maxVal;
		int px;
		int py;
		double vert[] = new double[3];
		double horz[] = new double[3];
		double sum11;
		double pyy;
		double pxx;
		double dx;
		double dy;
		double theta1;
		double theta2;
		double scale;
		double IB_recover1[][];
		double IB_recover2[][];
		int index;
		double IB_R1[];
		double IB_R1Imag[];
		double IB_R2[];
		double IB_R2Imag[];
		double IB1p[];
		double IB1pImag[];
		double IB2p[];
		double IB2pImag[];
		double Pp1[];
		double Pp1Imag[];
		double Pp2[];
		double Pp2Imag[];
		double Pp1Shift[][];
		double Pp2Shift[][];
		int px1;
		int py1;
		int px2;
		int py2;
		double maxVal1;
		double maxVal2;
		double theta;
		double peak;
		double pxx1;
		double pyy1;
		double pxx2;
		double pyy2;
		double result[];
		double outOfBoundsValue;
		
		
		height = refImage.getExtents()[1];
		width = refImage.getExtents()[0];
		length = width * height;
		cy = height/2;
		cx = width/2;
		
	    // Create window
		// hannig window and root of hanning window
		han_win = new double[length];
		//Rhan_win = new double[length];
		
		// make window 
		for (i = 1; i <= height; i++) {
		    for (j = 1; j <= width; j++) {
		            han_win[(i-1)*width + j-1] = 0.25 * (1.0 + Math.cos(Math.PI*Math.abs(cy- i) / height))*(1.0 + Math.cos(Math.PI*Math.abs(cx - j) / width));
		            // Root han_win
		            /*Rhan_win[(i-1)*width + j-1] = Math.abs(Math.cos(Math.PI*Math.abs(cy - i) / height)*Math.cos(Math.PI*Math.abs(cx - j) / width));
		            if i >height/8  &&  i<height*7/8
		                fi = 1;
		            elseif i <= height/8
		                fi = (i-1)/height * 8;
		            elseif i >= height*7/8
		                fi = (height - i)/height *8;
		            end
		            if j >width/8  &&  j<width*7/8
		                fj = 1;
		            elseif j <= width/8
		                fj = (j-1)/width * 8;
		            elseif j >= width*7/8
		                fj = (width - j)/width *8;
		            end
		            trapez_win(i-1,j-1)=fi*fj;*/
		    }
		}
		
		AI = new double[length];
		try {
			refImage.exportData(0, length, AI);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on refImage.exportData(0, length, AI)");
			setCompleted(false);
			return;
		}
		
		BI = new double[length];
		try {
			inputImage.exportData(0, length, BI);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on inputImage.exportData(0, length, BI)");
			setCompleted(false);
			return;
		}
		
		// Apply Windowing 
		IA = new double[length];
		IB = new double[height][width];
		B = new double[length];
		for (j = 0; j < height; j++) {
			for (i = 0; i < width; i++) {
				index = i + j*width;
				IA[index] = han_win[index] * AI[index];
				IB[j][i] = han_win[index] * BI[index];
				B[index] = IB[j][i];
			}
		}
		
		// 2D fft
		IAImag = new double[length];
		BImag = new double[length];
		FFTUtility fftA = new FFTUtility(IA, IAImag, height, width, 1, -1, FFTUtility.FFT);
		fftA.run();
		FFTUtility fftA2 = new FFTUtility(IA, IAImag, 1, height, width, -1, FFTUtility.FFT);
		fftA2.run();
		FFTUtility fftB = new FFTUtility(B, BImag, height, width, 1, -1, FFTUtility.FFT);
		fftB.run();
		FFTUtility fftB2 = new FFTUtility(B, BImag, 1, height, width, -1, FFTUtility.FFT);
		fftB2.run(); 
		
		At = new double[length];
		AtImag = new double[length];
		Amag = new double[length];
		Bt = new double[length];
		BtImag = new double[length];
		Bmag = new double[length];
		for (i = 0; i < length; i++) {
		    Amag[i] = Math.sqrt(IA[i]*IA[i] + IAImag[i]*IAImag[i]);
		    At[i] = IA[i]/Amag[i];
		    AtImag[i] = IAImag[i]/Amag[i];
		    Bmag[i] = Math.sqrt(B[i]*B[i] + BImag[i]*BImag[i]);
		    Bt[i] = B[i]/Bmag[i];
		    BtImag[i] = -BImag[i]/Bmag[i];
		}
		
		// get magnitude and whitening
		for (i = 0; i < length; i++) {
			Amag[i] = Math.log(Amag[i] + 1.0);
			Bmag[i] = Math.log(Bmag[i] + 1.0);
		}
		As = fftshift(Amag, height, width);
		Bs = fftshift(Bmag, height, width);
		
		// Log-Poler Transformation
	    // need bilinear interpolation

		lpcA = new double[length];
		lpcB = new double[length];
		
		// cut off val of LPF
		LPmin = width*(1-log2(4*Math.PI)/log2(width));
		
		// start logpolar 
		// Used refImage with single pixel width rectangle
		// Used cmpImage with rectangle shifted by +1 pixel x and +1 pixel y
		// Mean squared error of registered cmpImage to refImage:
		// cx, cy 44.756
		// cx-1, cy-1 44.805
		// cx-1, cy 44.801
		// cx,cy-1 44.800
		// cx+1, cy 44.801
		// cx, cy+1 44.800
		// cx+1, cy+1 44.805
		// cx-1, cy+1 44.802
		// cx+1, cy-1 44.802
		
		// For a 512 by 512 image r < (cx * 0.6) gives:
		// minx = 102.74762465587082
		// maxx = 409.2523753441292
	    // miny = 102.74762465587082
	    // maxy = 409.2523753441292
		//double minx = Double.MAX_VALUE;
		//double maxx = -Double.MAX_VALUE;
		//double miny = Double.MAX_VALUE;
		//double maxy = -Double.MAX_VALUE;
		for (i= 0; i <= width-1; i++) {
		    r = Math.pow(width,(double)(i)/(double)width);
		    for (j = 0; j <=height-1; j++) {
		        x=r*Math.cos(2*Math.PI*j/height)+cx;
			    y=r*Math.sin(2*Math.PI*j/height)+cy;
			    if ((y >= 0) && (y < height-1)) {
			        if (r < (cx * 0.6)) { // in the circle
			        	/*if (x < minx) {
			        		minx = x;
			        	}
			        	if (x > maxx) {
			        		maxx = x;
			        	}
			        	if (y < miny) {
			        		miny = y;
			        	}
			        	if (y > maxy) {
			        		maxy = y;
			        	}*/
			             x0 = (int)Math.floor(x);
			             y0 = (int)Math.floor(y);
			             x1 = x0 + 1;
			             y1 = y0 + 1;
			            w0=x1-x;
			            w1=x-x0;
			            h0=y1-y;
			            h1=y-y0;
			            // Bilinear Interpolation
			            val=As[y0][x0]*w0*h0 + As[y0][x1]*w1*h0+ As[y1][x0]*w0*h1 + As[y1][x1]*w1*h1;
			            // High pass
			            if (i > LPmin) { 
			                 lpcA[i + j*width] =val;
			            }
			            else {
			                 lpcA[i + j*width] =0;
			            }
			            val=Bs[y0][x0]*w0*h0 + Bs[y0][x1]*w1*h0+ Bs[y1][x0]*w0*h1 + Bs[y1][x1]*w1*h1;
			            if (i > LPmin) { 
			                 lpcB[i + j*width] = val;
			            }
			            else {
			                 lpcB[i + j*width] = 0;
			            }
			        }
		        }
		    }
		}
		/*System.err.println("minx = " + minx);
		System.err.println("maxx = " + maxx);
		System.err.println("miny = " + miny);
		System.err.println("maxy = " + maxy);*/

	   // end LogPoler
		
	   // phase correlation to get rotation and scaling
		lpcAImag = new double[length];
		lpcBImag = new double[length];
		FFTUtility fftlA = new FFTUtility(lpcA, lpcAImag, height, width, 1, -1, FFTUtility.FFT);
		fftlA.run();
		FFTUtility fftlA2 = new FFTUtility(lpcA, lpcAImag, 1, height, width, -1, FFTUtility.FFT);
		fftlA2.run();
		FFTUtility fftlB = new FFTUtility(lpcB, lpcBImag, height, width, 1, -1, FFTUtility.FFT);
		fftlB.run();
		FFTUtility fftlB2 = new FFTUtility(lpcB, lpcBImag, 1, height, width, -1, FFTUtility.FFT);
		fftlB2.run(); 
		Pp = new double[length];
		PpImag = new double[length];
		for (i = 0; i < length; i++) {
		    mag = Math.sqrt(lpcA[i]*lpcA[i] + lpcAImag[i]*lpcAImag[i]);
		    Ap = lpcA[i]/mag;
		    ApImag = lpcAImag[i]/mag;
		    mag = Math.sqrt(lpcB[i]*lpcB[i] + lpcBImag[i]*lpcBImag[i]);
		    Bp = lpcB[i]/mag;
		    BpImag = -lpcBImag[i]/mag;
		    Pp[i] = Ap*Bp - ApImag * BpImag;
		    PpImag[i] = Ap*BpImag + ApImag * Bp;
		}
		// Real part of inverse transform
		FFTUtility fftPp = new FFTUtility(Pp, PpImag, height, width, 1, 1, FFTUtility.FFT);
		fftPp.run();
		FFTUtility fftPp2 = new FFTUtility(Pp, PpImag, 1, height, width, 1, FFTUtility.FFT);
		fftPp2.run();
		PpShift = fftshift(Pp, height, width);
		
		px = 0;
		py = 0;
		maxVal = -Double.MAX_VALUE;
		for (j = 0; j < height; j++) {
			for (i = 0; i < width; i++) {
				if (PpShift[j][i] > maxVal) {
					maxVal = PpShift[j][i];
					py = j;
					px = i;
				}
			}
		}

		// Bilinear Interpolation
		for (i = px-1; i <= px+1; i++) {
			vert[i-(px-1)] = PpShift[py-1][i] + PpShift[py][i] + PpShift[py+1][i];
		}
		for (i = py-1; i <= py+1; i++) {
			horz[i-(py-1)] = PpShift[i][px-1] + PpShift[i][px] + PpShift[i][px+1];
		}
		sum11 = vert[0] + vert[1] + vert[2];
		pyy = ((py-1)*horz[0] + py*horz[1] + (py+1)*horz[2])/sum11;
		pxx = ((px-1)*vert[0] + px*vert[1] + (px+1)*vert[2])/sum11;
		
		dx = Math.floor(width/2.0) - pxx + 1;
		dy = Math.floor(height/2.0)- pyy + 1;
		
	    //  Translation Extraction
		theta1 = 360 * dy / height;
		theta2 = theta1 + 180;
		scale = 1/Math.pow(width,dx/width);
		
		// Compensate Rotation and scaling
		IB_recover1 = ImageRotateScale(IB, theta1,scale,width,height);
		IB_recover2 = ImageRotateScale(IB, theta2,scale,width,height);
		
		// Translation estimation
		IB_R1 = new double[length];
		IB_R1Imag = new double[length];
		IB_R2 = new double[length];
		IB_R2Imag = new double[length];
		
		for (j = 0; j < height; j++) {
			for (i = 0; i < width; i++) {
				index = i + j*width;
				IB_R1[index] = IB_recover1[j][i];
				IB_R2[index] = IB_recover2[j][i];
			}
		}
		
		FFTUtility fftR1 = new FFTUtility(IB_R1, IB_R1Imag, height, width, 1, -1, FFTUtility.FFT);
		fftR1.run();
		FFTUtility fftR12 = new FFTUtility(IB_R1, IB_R1Imag, 1, height, width, -1, FFTUtility.FFT);
		fftR12.run();
		FFTUtility fftR2 = new FFTUtility(IB_R2, IB_R2Imag, height, width, 1, -1, FFTUtility.FFT);
		fftR2.run();
		FFTUtility fftR22 = new FFTUtility(IB_R2, IB_R2Imag, 1, height, width, -1, FFTUtility.FFT);
		fftR22.run(); 
		
		IB1p = new double[length];
		IB1pImag = new double[length];
		IB2p = new double[length];
		IB2pImag = new double[length];
		for (i = 0; i < length; i++) {
			mag = Math.sqrt(IB_R1[i]*IB_R1[i] + IB_R1Imag[i]*IB_R1Imag[i]);
			IB1p[i] = IB_R1[i]/mag;
			IB1pImag[i] = -IB_R1Imag[i]/mag;
			mag = Math.sqrt(IB_R2[i]*IB_R2[i] + IB_R2Imag[i]*IB_R2Imag[i]);
			IB2p[i] = IB_R2[i]/mag;
			IB2pImag[i] = -IB_R2Imag[i]/mag;
		}
		
		// App = A./abs(A);
		// but we had At = A./abs(A);
		// so use At in place of App
		Pp1 = new double[length];
		Pp1Imag = new double[length];
		Pp2 = new double[length];
		Pp2Imag = new double[length];
		for (i = 0; i < length; i++) {
		    Pp1[i] = At[i]*IB1p[i] - AtImag[i] * IB1pImag[i];
		    Pp1Imag[i] = At[i]*IB1pImag[i] + AtImag[i]*IB1p[i];
		    Pp2[i] = At[i]*IB2p[i] - AtImag[i] * IB2pImag[i];
		    Pp2Imag[i] = At[i]*IB2pImag[i] + AtImag[i]*IB2p[i];
		}
		
		// Real part of inverse transform
		FFTUtility fftPp1 = new FFTUtility(Pp1, Pp1Imag, height, width, 1, 1, FFTUtility.FFT);
		fftPp1.run();
		FFTUtility fftPp12 = new FFTUtility(Pp1, Pp1Imag, 1, height, width, 1, FFTUtility.FFT);
		fftPp12.run();
		Pp1Shift = fftshift(Pp1, height, width);
		
		FFTUtility fftPp22 = new FFTUtility(Pp2, Pp2Imag, height, width, 1, 1, FFTUtility.FFT);
		fftPp22.run();
		FFTUtility fftPp222 = new FFTUtility(Pp2, Pp2Imag, 1, height, width, 1, FFTUtility.FFT);
		fftPp222.run();
		Pp2Shift = fftshift(Pp2, height, width);
		
		px1 = 0;
		py1 = 0;
		maxVal1 = -Double.MAX_VALUE;
		for (j = 0; j < height; j++) {
			for (i = 0; i < width; i++) {
				if (Pp1Shift[j][i] > maxVal1) {
					maxVal1 = Pp1Shift[j][i];
					py1 = j;
					px1 = i;
				}
			}
		}
		
		px2 = 0;
		py2 = 0;
		maxVal2 = -Double.MAX_VALUE;
		for (j = 0; j < height; j++) {
			for (i = 0; i < width; i++) {
				if (Pp2Shift[j][i] > maxVal2) {
					maxVal2 = Pp2Shift[j][i];
					py2 = j;
					px2 = i;
				}
			}
		}
		
		// Comparison to get True rotation
		if (maxVal1 > maxVal2) {
			theta = theta1;
			for (i = px1-1; i <= px1+1; i++) {
				vert[i-(px1-1)] = Pp1Shift[py1-1][i] + Pp1Shift[py1][i] + Pp1Shift[py1+1][i];
			}
			for (i = py1-1; i <= py1+1; i++) {
				horz[i-(py1-1)] = Pp1Shift[i][px1-1] + Pp1Shift[i][px1] + Pp1Shift[i][px1+1];
			}
			sum11 = vert[0] + vert[1] + vert[2];
			pyy1 = ((py1-1)*horz[0] + py1*horz[1] + (py1+1)*horz[2])/sum11;
			pxx1 = ((px1-1)*vert[0] + px1*vert[1] + (px1+1)*vert[2])/sum11;
			
			dx = Math.floor(width/2.0) - pxx1 + 1;
			dy = Math.floor(height/2.0)- pyy1 + 1;
			
			peak = maxVal1;
			outOfBoundsValue = inputImage.getMin();
			result = imtranslate(IB_recover1,dx, dy, outOfBoundsValue);
		} // if (maxVal1 > maxVal2)
		else {
			theta = theta2;
			for (i = px2-1; i <= px2+1; i++) {
				vert[i-(px2-1)] = Pp2Shift[py2-1][i] + Pp2Shift[py2][i] + Pp2Shift[py2+1][i];
			}
			for (i = py2-1; i <= py2+1; i++) {
				horz[i-(py2-1)] = Pp2Shift[i][px2-1] + Pp2Shift[i][px2] + Pp2Shift[i][px2+1];
			}
			sum11 = vert[0] + vert[1] + vert[2];
			pyy2 = ((py2-1)*horz[0] + py2*horz[1] + (py2+1)*horz[2])/sum11;
			pxx2 = ((px2-1)*vert[0] + px2*vert[1] + (px2+1)*vert[2])/sum11;
			
			dx = Math.floor(width/2.0) - pxx2 + 1;
			dy = Math.floor(height/2.0)- pyy2 + 1;
			
			peak = maxVal2; 
			outOfBoundsValue = inputImage.getMin();
			result = imtranslate(IB_recover2, dx, dy, outOfBoundsValue);
		}
		
		try {
      	    outputImage.importData(0, result, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on outputImage.importData(0, result, true)");
      		setCompleted(false);
      		return;
      	}
  	
  	    setCompleted(true);
  	    return;
	}
	
	private double[] imtranslate(double Image[][], double translateX, double translateY, double outOfBoundsValue) {
		int i,j;
		double x,y;
		int x0,y0,x1,y1;
		double w0, w1, h0, h1;
		double val;
		int height = Image.length;
		int width = Image[0].length;
		
		double translateimage[] = new double[height * width];
		for (i = 0; i < height*width; i++) {
			translateimage[i] = outOfBoundsValue;
		}
		
		for (i= 0; i <= width-1; i++) {
		    for (j= 0; j <= height-1; j++) {
		        x= i + translateX;
		        y= j + translateY;
		        if ((0 < x) && (x < width - 1) && (0 < y) && (y < height - 1)) {
		             x0 = (int)Math.floor(x);
		             y0 = (int)Math.floor(y);
		             x1 = x0 + 1;
		             y1 = y0 + 1;
		            w0=x1-x;
		            w1=x-x0;
		            h0=y1-y;
		            h1=y-y0;
		          
		            val=Image[y0][x0]*w0*h0 + Image[y0][x1]*w1*h0+ Image[y1][x0]*w0*h1 + Image[y1][x1]*w1*h1;
		                translateimage[j*width + i]=val;
		        }
		    }
	    }
		
		return translateimage;
	}
	
	private double[][] ImageRotateScale(double Image[][], double theta, double scale, int width, int height) {
		double cx, cy;
		double ct,st;
		int i,j;
		double x,y;
		int x0,y0,x1,y1;
		double w0, w1, h0, h1;
		double val;
		cx = width/2.0 + 0.5;
		cy = height/2.0 + 0.5;	
		
		ct = Math.cos(theta*Math.PI/180.0);
		st = Math.sin(theta*Math.PI/180.0);
		
		// tform = affine2d([cosd(theta) -sind(theta) 0; sind(theta) cosd(theta) 0; 0 0 1]);
		
		double resizeimage[][] = new double[height][width];
		
		for (i= 0; i <= width-1; i++) {
			    for (j= 0; j <= height-1; j++) {
			        x=(ct*(i-cx)-st*(j-cy))*scale+cx;
			        y=(ct*(j-cy)+st*(i-cx))*scale+cy;
			        if ((0 < x) && (x < width - 1) && (0 < y) && (y < height - 1)) {
			             x0 = (int)Math.floor(x);
			             y0 = (int)Math.floor(y);
			             x1 = x0 + 1;
			             y1 = y0 + 1;
			            w0=x1-x;
			            w1=x-x0;
			            h0=y1-y;
			            h1=y-y0;
			          
			            val=Image[y0][x0]*w0*h0 + Image[y0][x1]*w1*h0+ Image[y1][x0]*w0*h1 + Image[y1][x1]*w1*h1;
			                resizeimage[j][i]=val;
			        }
			    }
		}

		
		return resizeimage;
	}
	
	 private double log2(double input) {
          return (Math.log10(input) / Math.log10(2.0));
	 }
	
	private double[][] fftshift(double in[], int yDim, int xDim) {
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
			highestyquad1Index = (yDim - 1)/2;
		}
		else {
			//yodd = false;
			highestyquad1Index = yDim/2 - 1;
		}
		if ((xDim % 2) == 1) {
			//xodd = true;
			highestxquad1Index = (xDim - 1)/2;
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
		double quad1[] = new double[quad1width * quad1height];
		double quad2[] = new double[quad2width * quad2height];
		double quad3[] = new double[quad3width * quad3height];
		double quad4[] = new double[quad4width * quad4height];
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad1[x + y*quad1width] = in[x + y*xDim];
			}
		}
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad2[(x - highestxquad1Index-1) + y*quad2width] = in[x + y*xDim];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad3[(x - highestxquad1Index - 1) + (y - highestyquad1Index - 1)*quad3width] = in[x + y*xDim];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad4[x + (y - highestyquad1Index - 1)*quad4width] = in[x + y*xDim];
			}
		}
		
		// Move third quadrant to first
		for (y = 0; y < quad3height; y++) {
			for (x = 0; x < quad3width; x++) {
				out[y][x] = quad3[x + y*quad3width];
			}
		}
		// Move fourth quadrant to second
		for (y = 0; y < quad4height; y++) {
			for (x = 0; x < quad4width; x++) {
				out[y][x + quad3width] = quad4[x + y*quad4width];
			}
		}
		// Move first quadrant to third
		for (y = 0; y < quad1height; y++) {
			for (x = 0; x < quad1width; x++) {
				out[y+quad3height][x + quad3width] = quad1[x + y*quad1width];
			}
		}
		// Move second quadrant to fourth
		for (y = 0; y < quad2height; y++) {
			for (x = 0; x < quad2width; x++) {
				out[y+quad3height][x] = quad2[x + y*quad2width];
			}
		}
		return out;
	}
}