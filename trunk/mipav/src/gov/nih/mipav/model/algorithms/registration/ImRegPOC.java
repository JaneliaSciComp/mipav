package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.structures.*;

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

This program provides robust image registration method using "Phase Correlation" technique.

With this toolbox, you can estimate 
Translation, Rotation and Scaling between two 2D gray scale images with identical dimensions.
That is you can calculate 4 degrees of freedom.  You must use a global scale factor. You
cannot use separate x and y scale factors.  You cannot calculate shear.

The merit of this method is that you can detect the success of this estimation via checking certain value.
You can use isSucceed() function to check it. (0/1 to failure/succeed)

References:
1.) Y. Ri and H. Fujimoto, “Drift-free Motion Estimation from Video Images using Phase Correlation and Linear Optimization,”
 in The 15th International Workshop on Advanced Motion Control, 2018, pp. 295-300.
 2.) Y. Ri and H. Fujimoto, “Image Based Visual Servo Application on Video Tracking with Monocular Camera Based on Phase Correlation Method,” 
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
    
    public ImRegPOC(final ModelImage dstImage, final ModelImage imageA, final ModelImage imageB) {
    	super(dstImage, imageB);
        refImage = imageA;
        inputImage = imageB;
    }
	
	
	public void runAlgorithm() {
		ModelImage outputImage;
   	   if (destImage != null) {
   		   outputImage = destImage;
   	   }
   	   else {
   		   outputImage = srcImage;
   	   }
		int cy,cx;
		double han_win[];
		double Rhan_win[];
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
		
		height = refImage.getExtents()[1];
		width = refImage.getExtents()[0];
		length = width * height;
		cy = height/2;
		cx = width/2;
		
	    // Create window
		// hannig window and root of hanning window
		han_win = new double[length];
		Rhan_win = new double[length];
		
		// make window 
		for (i = 1; i <= height; i++) {
		    for (j = 1; j <= width; j++) {
		            han_win[(i-1)*width + j-1] = 0.25 * (1.0 + Math.cos(Math.PI*Math.abs(cy- i) / height))*(1.0 + Math.cos(Math.PI*Math.abs(cx - j) / width));
		            // Root han_win
		            Rhan_win[(i-1)*width + j-1] = Math.abs(Math.cos(Math.PI*Math.abs(cy - i) / height)*Math.cos(Math.PI*Math.abs(cx - j) / width));
		            /*if i >height/8  &&  i<height*7/8
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
		
		// start logplolar 
		for (i= 0; i <= width-1; i++) {
		    r = Math.pow(width,(double)(i)/(double)width);
		    for (j = 0; j <=height-1; j++) {
		        x=r*Math.cos(2*Math.PI*j/height)+cx;
		        y=r*Math.sin(2*Math.PI*j/height)+cy;
		        if (r < (cx * 0.6)) { // in the circle
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
			result = imtranslate(IB_recover1,-dx, -dy);
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
			result = imtranslate(IB_recover2, -dx, -dy);
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
	
	private double[] imtranslate(double Image[][], double translateX, double translateY) {
		int i,j;
		double x,y;
		int x0,y0,x1,y1;
		double w0, w1, h0, h1;
		double val;
		int height = Image.length;
		int width = Image[0].length;
		
		double translateimage[] = new double[height * width];
		
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