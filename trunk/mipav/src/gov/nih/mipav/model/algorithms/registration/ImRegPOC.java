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
    
    public ImRegPOC(final ModelImage imageA, final ModelImage imageB) {
    	super(null, imageB);
        refImage = imageA;
        inputImage = imageB;
    }
	
	
	public void runAlgorithm() {
		int cy,cx;
		double han_win[];
		double Rhan_win[];
		double AI[];
		double BI[];
		double IA[];
		double IB[];
		double IAImag[];
		double IBImag[];
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
		double LPmin;
		int i,j;
		double r;
		double x,y;
		int x0,y0,x1,y1;
		double w0,w1,h0,h1;
		double val;
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
		IB = new double[length];
		for (i = 0; i < length; i++) {
			IA[i] = han_win[i] * AI[i];
			IB[i] = han_win[i] * BI[i];
		}
		
		// 2D fft
		IAImag = new double[length];
		IBImag = new double[length];
		FFTUtility fftA = new FFTUtility(IA, IAImag, length, width, 1, -1, FFTUtility.FFT);
		fftA.run();
		FFTUtility fftA2 = new FFTUtility(IA, IAImag, 1, length, width, -1, FFTUtility.FFT);
		fftA2.run();
		FFTUtility fftB = new FFTUtility(IB, IBImag, length, width, 1, -1, FFTUtility.FFT);
		fftB.run();
		FFTUtility fftB2 = new FFTUtility(IB, IBImag, 1, length, width, -1, FFTUtility.FFT);
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
		    Bmag[i] = Math.sqrt(IB[i]*IB[i] + IBImag[i]*IBImag[i]);
		    Bt[i] = IB[i]/Bmag[i];
		    BtImag[i] = -IBImag[i]/Bmag[i];
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