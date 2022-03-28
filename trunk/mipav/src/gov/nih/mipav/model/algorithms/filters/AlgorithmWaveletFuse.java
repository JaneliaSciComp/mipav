package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.DiscreteWavelet;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.MODE;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;

/**
MIT License

Copyright (c) 2021 Hans Brouwer, Riyo Wanagiri

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
    Image Fusion Using Wavelet Transform by Aggariyo Wanagiri and 
    Hans Brouwer, Delft University of Technology.

This is a port of wavelet_fuse.py

*/

public class AlgorithmWaveletFuse extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	private ModelImage fuseImage;
	private int kernel_size = 37;
	private PyWavelets.WAVELET_NAME names[] = new PyWavelets.WAVELET_NAME[] {PyWavelets.WAVELET_NAME.DB};
	private int orders[] = new int[] {13};
	private int max_depth = 999;
	boolean slow = false;
	
    public AlgorithmWaveletFuse() {
		
	}
	
	public AlgorithmWaveletFuse(ModelImage destImg, ModelImage srcImg, ModelImage fuseImage,
			int kernel_size, PyWavelets.WAVELET_NAME names[], int orders[],
			short max_depth, boolean slow) {
		super(destImg, srcImg);
		this.fuseImage = fuseImage;
		this.kernel_size = kernel_size;
		this.names = names;
		this.orders = orders;
		this.max_depth = max_depth;
	}
	
	public void runAlgorithm() {
	   int color;
	   int xDim = srcImage.getExtents()[0];
	   int yDim = srcImage.getExtents()[1];
	   int length = xDim * yDim;
	   int i,x,y;
	   int index;
	   int depth;
	   PyWavelets py = new PyWavelets();
	   DiscreteWavelet w = py.discrete_wavelet(names[0], orders[0]);
	   short xDim_max_level = py.dwt_max_level(xDim, w);
	   short yDim_max_level = py.dwt_max_level(yDim, w);
	   max_depth = Math.min(max_depth, Math.min(xDim_max_level,yDim_max_level));
	   DiscreteWavelet wavelets[] = new DiscreteWavelet[]{w, w};
	   MODE modes[] = new MODE[]{MODE.MODE_SYMMETRIC, MODE.MODE_SYMMETRIC};
	   int axes[] = new int[]{0,1};
	   double buffer[] = new double[length];
	   double xbuf[][] = new double[yDim][xDim];
	   double ybuf[][] = new double[yDim][xDim];
	   double arrx[][][];
	   double arry[][][];
	   double arri[][][] = new double[4][][];
	   int numColors = 1;
	   float fbuffer[] = null;
	   if (srcImage.isColorImage() || fuseImage.isColorImage()) {
		   numColors = 3;
		   fbuffer = new float[length];
	   }
	   double srcMin = srcImage.getMin();
	   double srcMax = srcImage.getMax();
	   double fuseMin = fuseImage.getMin();
	   double fuseMax = fuseImage.getMax();
	   Vector<double[][]> stack = new Vector<double[][]>();
	   for (color = 1; color <= numColors; color++) {
	       if (srcImage.isColorImage()) {
	    	   try {
	    	       srcImage.exportRGBData(color, 0, length, buffer);
	    	   }
	    	   catch (IOException e) {
	    		   System.err.println("IOException " + e + " on srcImage.exportRGBData(color,0,length,buffer");
	    		   setCompleted(false);
	    		   return;
	    	   }
	    	   if (color > 1) {
	    		   xbuf = new double[yDim][xDim];
	    	   }
	    	   for (y = 0; y < yDim; y++) {
	    		   for (x = 0; x < xDim; x++) {
	    			   xbuf[y][x] = (buffer[x+ y * xDim] -srcMin)/srcMax;
	    		   }
	    	   }
	    	   
	       }  // if (srcImage.isColorImage())
	       else { // srcImage gray scale image
	    	   try {
	    	       srcImage.exportData(0, length, buffer);
	    	   }
	    	   catch (IOException e) {
	    		   System.err.println("IOException " + e + " on srcImage.exportData(0,length,buffer");
	    		   setCompleted(false);
	    		   return;
	    	   }
	    	   for (y = 0; y < yDim; y++) {
	    		   for (x = 0; x < xDim; x++) {
	    			   xbuf[y][x] = (buffer[x+ y * xDim] -srcMin)/srcMax;
	    		   }
	    	   }
	    	   
	       } // else srcImage gray scale image
	       if (fuseImage.isColorImage()) {
	    	   try {
	    	       fuseImage.exportRGBData(color, 0, length, buffer);
	    	   }
	    	   catch (IOException e) {
	    		   System.err.println("IOException " + e + " on fuseImage.exportRGBData(color,0,length,buffer");
	    		   setCompleted(false);
	    		   return;
	    	   }
	    	   if (color > 1) {
	    		   ybuf = new double[yDim][xDim];
	    	   }
	    	   for (y = 0; y < yDim; y++) {
	    		   for (x = 0; x < xDim; x++) {
	    			   ybuf[y][x] = (buffer[x+ y * xDim] - fuseMin)/fuseMax;
	    		   }
	    	   }   
	       }
	       else { // fuseImage gray scale image
	    	   try {
	    	       fuseImage.exportData(0, length, buffer);
	    	   }
	    	   catch (IOException e) {
	    		   System.err.println("IOException " + e + " on fuseImage.exportData(0,length,buffer");
	    		   setCompleted(false);
	    		   return;
	    	   }
	    	   for (y = 0; y < yDim; y++) {
	    		   for (x = 0; x < xDim; x++) {
	    			   ybuf[y][x] = (buffer[x+ y * xDim] -fuseMin)/fuseMax;
	    		   }
	    	   }      
	       } // else fuseImage gray scale image
	       
	       // Move down the wavelet pyramid, fuse along the way
	       depth = 1;
	       while ((xbuf.length >= 2*kernel_size) && (xbuf[0].length >= 2*kernel_size)) {
	           if (depth > max_depth) {
	        	   break;
	           }
	           arrx = py.dwt2(xbuf, wavelets, modes, axes);
	           arry = py.dwt2(ybuf, wavelets, modes, axes);
	           // fuse CD coefficents
	           stack.add(fuse(arrx[3], arry[3], kernel_size, slow));
	           // fuse CV coefficients
	           stack.add(fuse(arrx[2], arry[2], kernel_size, slow));
	           // fuse CH coefficients
	           stack.add(fuse(arrx[1], arry[1], kernel_size, slow));
	           xbuf = arrx[0];
	           ybuf = arry[0];
	           depth += 1;
	       } // while ((xbuf.length >= 2*kernel_size) && (xbuf[0].length >= 2*kernel_size))
	       
	       // fuse the DC offset
	       double fused[][] = fuse(xbuf, ybuf, kernel_size, slow);
	       
	       // Inverse wavelet transform back up
	       while (stack.size() > 0) {
	           arri[0] = fused;
	           arri[1] = stack.remove(stack.size()-1);
	           arri[2] = stack.remove(stack.size()-1);
	           arri[3] = stack.remove(stack.size()-1);
	           fused = py.idwt2(arri, wavelets, modes, axes);
	       } // while (stack.size() > 0)
	       
	       // Clip to image range
	       if (destImage.isColorImage()) {
	    	   for (y = 0; y < yDim; y++) {
		    	   for (x = 0; x < xDim; x++) {
		    		   index = x + y * xDim;
		    		   if (fused[y][x] > 1.0) {
		    			   fbuffer[index] = 1.0f;
		    		   }
		    		   else if (fused[y][x] < 0.0) {
		    			   fbuffer[index] = 0.0f;
		    		   }
		    		   else {
		    			   fbuffer[index] = (float)fused[y][x];
		    		   }
		    	   }
		       }
	    	   try {
	    	       destImage.importRGBData(color, length, fbuffer, false);
	    	   }
	    	   catch (IOException e) {
	    		   System.err.println("IOException " + e + " on destImage.importRGBData(color, length, fbuffer, false)");
	    		   setCompleted(false);
	    		   return;
	    	   }
	    	   if (color == 3) {
	    		   destImage.calcMinMax();
	    	   }
	       } // if (destImage.isColorImage())
	       else { // destImage gray scale image
	    	   for (y = 0; y < yDim; y++) {
		    	   for (x = 0; x < xDim; x++) {
		    		   index = x + y * xDim;
		    		   if (fused[y][x] > 1.0) {
		    			   buffer[index] = 1.0;
		    		   }
		    		   else if (fused[y][x] < 0.0) {
		    			   buffer[index] = 0.0;
		    		   }
		    		   else {
		    			   buffer[index] = fused[y][x];
		    		   }
		    	   }
		       }
	    	   try {
	    	       destImage.importData(length, buffer, true);
	    	   }
	    	   catch (IOException e) {
	    		   System.err.println("IOException " + e + " on destImage.importData(length, buffer, true)");
	    		   setCompleted(false);
	    		   return;
	    	   }   
	       } // else destImage gray scale image
	   } // for (colors = 1; colors <= numColors; colors++)
	   setCompleted(true);
	   return;
    }
	
	private double[][] fuse(double img1[][], double img2[][], int ks, boolean slow) {
	    return null;	
	}

}