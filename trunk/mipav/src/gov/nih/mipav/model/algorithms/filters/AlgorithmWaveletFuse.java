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

This is a port of wavelet_fuse.py and part of util.py.

*/

public class AlgorithmWaveletFuse extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	private ModelImage fuseImage;
	private int kernel_size = 37;
	private PyWavelets.WAVELET_NAME names[] = new PyWavelets.WAVELET_NAME[] {PyWavelets.WAVELET_NAME.DB};
	private int orders[] = new int[] {13};
	private int max_depth = 999;
	private boolean slow = false;
	
	private int numXColors;
	private int numYColors;
	private int numDestColors;
	
    public AlgorithmWaveletFuse() {
		
	}
	
	public AlgorithmWaveletFuse(ModelImage destImg, ModelImage srcImg, ModelImage fuseImage,
			int kernel_size, PyWavelets.WAVELET_NAME names[], int orders[],
			int max_depth, boolean slow) {
		super(destImg, srcImg);
		this.fuseImage = fuseImage;
		this.kernel_size = kernel_size;
		this.names = names;
		this.orders = orders;
		this.max_depth = max_depth;
		this.slow = slow;
	}
	
	public void runAlgorithm() {
	   int color;
	   int xDim = srcImage.getExtents()[0];
	   int yDim = srcImage.getExtents()[1];
	   int length = xDim * yDim;
	   int x,y;
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
	   double arrx[][][];
	   double arry[][][];
	   double arrx2[][][] = null;
	   double arry2[][][] = null;
	   double arrx3[][][] = null;
	   double arry3[][][] = null;
	   double arri[][][] = new double[4][][];
	   double arri2[][][] = new double[4][][];
	   double arri3[][][] = new double[4][][];
	   numXColors = 1;
	   numYColors = 1;
	   float fbuffer[] = null;
	   if (srcImage.isColorImage() || fuseImage.isColorImage()) {
		   fbuffer = new float[length];
	   }
	   if (srcImage.isColorImage()) {
		   numXColors = 3;
	   }
	   if (fuseImage.isColorImage()) {
		   numYColors = 3;
	   }
	   numDestColors = Math.max(numXColors, numYColors);
	   double xbuf[][][] = new double[numXColors][yDim][xDim];
	   double ybuf[][][] = new double[numYColors][yDim][xDim];
	   double dx[][][] = new double[numXColors][][];
	   double dy[][][] = new double[numYColors][][];
	   double di[][][] = new double[numDestColors][][];
	   double hx[][][] = new double[numXColors][][];
	   double hy[][][] = new double[numYColors][][];
	   double hi[][][] = new double[numDestColors][][];
	   double vx[][][] = new double[numXColors][][];
	   double vy[][][] = new double[numYColors][][];
	   double vi[][][] = new double[numDestColors][][];
	   double srcMin = srcImage.getMin();
	   double srcMax = srcImage.getMax();
	   double fuseMin = fuseImage.getMin();
	   double fuseMax = fuseImage.getMax();
	   Vector<double[][][]> stack = new Vector<double[][][]>();
       if (srcImage.isColorImage()) {
    	   try {
    	       srcImage.exportRGBData(1, 0, length, buffer);
    	   }
    	   catch (IOException e) {
    		   System.err.println("IOException " + e + " on srcImage.exportRGBData(1,0,length,buffer");
    		   setCompleted(false);
    		   return;
    	   }
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   xbuf[0][y][x] = (buffer[x+ y * xDim] -srcMin)/srcMax;
    		   }
    	   }
    	   
    	   try {
    	       srcImage.exportRGBData(2, 0, length, buffer);
    	   }
    	   catch (IOException e) {
    		   System.err.println("IOException " + e + " on srcImage.exportRGBData(2,0,length,buffer");
    		   setCompleted(false);
    		   return;
    	   }
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   xbuf[1][y][x] = (buffer[x+ y * xDim] -srcMin)/srcMax;
    		   }
    	   }
    	   
    	   try {
    	       srcImage.exportRGBData(3, 0, length, buffer);
    	   }
    	   catch (IOException e) {
    		   System.err.println("IOException " + e + " on srcImage.exportRGBData(3,0,length,buffer");
    		   setCompleted(false);
    		   return;
    	   }
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   xbuf[2][y][x] = (buffer[x+ y * xDim] -srcMin)/srcMax;
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
    			   xbuf[0][y][x] = (buffer[x+ y * xDim] -srcMin)/srcMax;
    		   }
    	   }
    	   
       } // else srcImage gray scale image
       if (fuseImage.isColorImage()) {
    	   try {
    	       fuseImage.exportRGBData(1, 0, length, buffer);
    	   }
    	   catch (IOException e) {
    		   System.err.println("IOException " + e + " on fuseImage.exportRGBData(1,0,length,buffer");
    		   setCompleted(false);
    		   return;
    	   }
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   ybuf[0][y][x] = (buffer[x+ y * xDim] - fuseMin)/fuseMax;
    		   }
    	   }
    	   
    	   try {
    	       fuseImage.exportRGBData(2, 0, length, buffer);
    	   }
    	   catch (IOException e) {
    		   System.err.println("IOException " + e + " on fuseImage.exportRGBData(2,0,length,buffer");
    		   setCompleted(false);
    		   return;
    	   }
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   ybuf[1][y][x] = (buffer[x+ y * xDim] - fuseMin)/fuseMax;
    		   }
    	   } 
    	   
    	   try {
    	       fuseImage.exportRGBData(3, 0, length, buffer);
    	   }
    	   catch (IOException e) {
    		   System.err.println("IOException " + e + " on fuseImage.exportRGBData(3,0,length,buffer");
    		   setCompleted(false);
    		   return;
    	   }
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   ybuf[2][y][x] = (buffer[x+ y * xDim] - fuseMin)/fuseMax;
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
    			   ybuf[0][y][x] = (buffer[x+ y * xDim] -fuseMin)/fuseMax;
    		   }
    	   }      
       } // else fuseImage gray scale image
       
       // Move down the wavelet pyramid, fuse along the way
       depth = 1;
       while ((xbuf[0].length >= 2*kernel_size) && (xbuf[0][0].length >= 2*kernel_size)) {
           if (depth > max_depth) {
        	   break;
           }
           arrx = py.dwt2(xbuf[0], wavelets, modes, axes);
           dx[0] = arrx[3];
           vx[0] = arrx[2];
           hx[0] = arrx[1];
           arry = py.dwt2(ybuf[0], wavelets, modes, axes);
           dy[0] = arry[3];
           vy[0] = arry[2];
           hy[0] = arry[1];
           if (srcImage.isColorImage()) {
        	   arrx2 = py.dwt2(xbuf[1], wavelets, modes, axes);
        	   dx[1] = arrx2[3];
               vx[1] = arrx2[2];
               hx[1] = arrx2[1];
        	   arrx3 = py.dwt2(xbuf[2], wavelets, modes, axes);
        	   dx[2] = arrx3[3];
               vx[2] = arrx3[2];
               hx[2] = arrx3[1];
           }
           if (fuseImage.isColorImage()) {
        	   arry2 = py.dwt2(ybuf[1], wavelets, modes, axes);
        	   dy[1] = arry2[3];
               vy[1] = arry2[2];
               hy[1] = arry2[1];
               arry3 = py.dwt2(ybuf[2], wavelets, modes, axes);
               dy[2] = arry3[3];
               vy[2] = arry3[2];
               hy[2] = arry3[1];
           }
           // fuse CD coefficents
           stack.add(fuse(dx, dy, kernel_size, slow));
           // fuse CV coefficients
           stack.add(fuse(vx, vy, kernel_size, slow));
           // fuse CH coefficients
           stack.add(fuse(hx, hy, kernel_size, slow));
           xbuf[0] = arrx[0];
           if (srcImage.isColorImage()) {
        	   xbuf[1] = arrx2[0];
        	   xbuf[2] = arrx3[0];
           }
           ybuf[0] = arry[0];
           if (fuseImage.isColorImage()) {
        	   ybuf[1] = arry2[0];
        	   ybuf[2] = arry3[0];
           }
           depth += 1;
       } // while ((xbuf[0].length >= 2*kernel_size) && (xbuf[0][0].length >= 2*kernel_size))
       
       // fuse the DC offset
       double fused[][][] = fuse(xbuf, ybuf, kernel_size, slow);
       
       // Inverse wavelet transform back up
       while (stack.size() > 0) {
           arri[0] = fused[0];
           if (numDestColors == 3) {
        	   arri2[0] = fused[1];
        	   arri3[0] = fused[2];
           }
           hi = stack.remove(stack.size()-1);
           arri[1] = hi[0];
           if ((arri[0].length != arri[1].length) || (arri[0][0].length != arri[1][0].length)) {
        	   // Interpolate arri[0] to be the same size as arri[1]
        	   arri[0] = transformBilinear(arri[0], arri[1][0].length, arri[1].length);
        	   if (numDestColors == 3) {
        		   arri2[0] = transformBilinear(arri2[0], arri[1][0].length, arri[1].length); 
        		   arri3[0] = transformBilinear(arri3[0], arri[1][0].length, arri[1].length);
        	   }
           }
           if (numDestColors == 3) {
        	   arri2[1] = hi[1];
        	   arri3[1] = hi[2];
           }
           vi = stack.remove(stack.size()-1);
           arri[2] = vi[0];
           if (numDestColors == 3) {
        	   arri2[2] = vi[1];
        	   arri3[2] = vi[2];
           }
           di = stack.remove(stack.size()-1);
           arri[3] = di[0];
           if (numDestColors == 3) {
        	   arri2[3] = di[1];
        	   arri3[3] = di[2];
           }
           fused[0] = py.idwt2(arri, wavelets, modes, axes);
           if (numDestColors == 3) {
        	   fused[1] = py.idwt2(arri2, wavelets, modes, axes);  
        	   fused[2] = py.idwt2(arri3, wavelets, modes, axes);
           }
       } // while (stack.size() > 0)
       
       // Clip to image range
       if (destImage.isColorImage()) {
    	   for (color = 1; color <= 3; color++) {
	    	   for (y = 0; y < yDim; y++) {
		    	   for (x = 0; x < xDim; x++) {
		    		   index = x + y * xDim;
		    		   if (fused[color-1][y][x] > 1.0) {
		    			   fbuffer[index] = 1.0f;
		    		   }
		    		   else if (fused[color-1][y][x] < 0.0) {
		    			   fbuffer[index] = 0.0f;
		    		   }
		    		   else {
		    			   fbuffer[index] = (float)fused[color-1][y][x];
		    		   }
		    	   }
		       }
	    	   try {
	    	       destImage.importRGBData(color, 0, fbuffer, false);
	    	   }
	    	   catch (IOException e) {
	    		   System.err.println("IOException " + e + " on destImage.importRGBData(color, 0, fbuffer, false)");
	    		   setCompleted(false);
	    		   return;
	    	   }
	    	   if (color == 3) {
	    		   destImage.calcMinMax();
	    	   }
    	   } // for (color = 1; color <= 3; color++) 
       } // if (destImage.isColorImage())
       else { // destImage gray scale image
    	   for (y = 0; y < yDim; y++) {
	    	   for (x = 0; x < xDim; x++) {
	    		   index = x + y * xDim;
	    		   if (fused[0][y][x] > 1.0) {
	    			   buffer[index] = 1.0;
	    		   }
	    		   else if (fused[0][y][x] < 0.0) {
	    			   buffer[index] = 0.0;
	    		   }
	    		   else {
	    			   buffer[index] = fused[0][y][x];
	    		   }
	    	   }
	       }
    	   try {
    	       destImage.importData(0, buffer, true);
    	   }
    	   catch (IOException e) {
    		   System.err.println("IOException " + e + " on destImage.importData(0, buffer, true)");
    		   setCompleted(false);
    		   return;
    	   }   
       } // else destImage gray scale image
	   setCompleted(true);
	   return;
    }
	
	/**
     * Transforms and resamples volume using bilinear interpolation.
     * 
     * @param imgBuf - image array
     * @param xfrm - TransMatrix to be applied
     */
    private double[][] transformBilinear(double[][] imgBuf, int newXDim, int newYDim) {
        int i, j;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        double X, Y;
        double x0, y0;
        double x1, y1;
        double value;
        double imm, jmm;
        double temp1, temp2;
        int roundX, roundY;
        double transformedBuf[][] = new double[newYDim][newXDim];
        int xDim = imgBuf[0].length;
        int yDim = imgBuf.length;

        // int length = orgDim[0]*orgDim[1];
        // int mod = orgDim[0]/50;
        // int counter = 0; //used for progress bar
        float T00, T01, T02, T10, T11, T12;
        double Sx = ((double)newXDim) / ((double)xDim);
        double Sy = ((double)newYDim) / ((double)yDim);
        TransMatrix xfrm = new TransMatrix(3);
        xfrm.setZoom(Sx, Sy);
        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);

        for (i = 0; i < newXDim; i++) {

            // if ( isProgressBarVisible()&& i%mod ==0) {
            // progressBar.setValue((int)((float)i/oXdim * 100+0.5));
            // }
            imm = i;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; j < newYDim; j++) {

                // transform i,j
                value = 0; // remains zero if voxel is transformed out of bounds
                jmm = j;
                X = (temp1 + (jmm * T01));
                roundX = (int) (X + 0.5);

                if ( (X >= 0) && (roundX < xDim)) {
                    Y = (temp2 + (jmm * T11));
                    roundY = (int) (Y + 0.5);

                    if ( (Y >= 0) && (roundY < yDim)) {

                        if ( (roundX == (xDim - 1)) || (roundY == (yDim - 1))) { // cannot interpolate on
                            // last
                            // X or Y
                            X0pos = roundX;
                            Y0pos = roundY;
                            value = imgBuf[Y0pos][X0pos];
                        } else {

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            x0 = X - (int) X;
                            y0 = Y - (int) Y;
                            x1 = 1 - x0;
                            y1 = 1 - y0;
                            X0pos = (int) X;
                            Y0pos = (int) Y;
                            X1pos = X0pos + 1;
                            Y1pos = Y0pos + 1;
                            value = (x1 * y1 * imgBuf[Y0pos][X0pos]) + (x0 * y1 * imgBuf[Y0pos][X1pos])
                                    + (x1 * y0 * imgBuf[Y1pos][X0pos]) + (x0 * y0 * imgBuf[Y1pos][X1pos]);
                        }
                    } // end if Y in bounds
                } // end if X in bounds

                transformedBuf[j][i] = value;
                // counter++;
            } // end for j
        } // end for i
        return transformedBuf;
    }
	
	private double[][][] fuse(double img1[][][], double img2[][][], int ks, boolean slow) {
		int c,y,x;
		boolean binary_map[][][];
		if (slow) {
			binary_map = slow_decision_map(img1, img2, ks);
		}	
		else {
			binary_map = decision_map(img1, img2, ks);
		}
		if (slow) {
			binary_map = slow_majority_filter(binary_map,ks);
		}
		else {
			binary_map = majority_filter(binary_map,ks);
		}
	    for (c = 0; c < binary_map.length; c++) {
	    	for (y = 0; y < binary_map[0].length; y++) {
	    		for (x = 0; x < binary_map[0][0].length; x++) {
	    			binary_map[c][y][x] = !binary_map[c][y][x];
	    		}
	    	}
	    }
	    if (slow) {
			binary_map = slow_majority_filter(binary_map,ks);
		}
		else {
			binary_map = majority_filter(binary_map,ks);
		}
	    for (c = 0; c < binary_map.length; c++) {
	    	for (y = 0; y < binary_map[0].length; y++) {
	    		for (x = 0; x < binary_map[0][0].length; x++) {
	    			binary_map[c][y][x] = !binary_map[c][y][x];
	    		}
	    	}
	    }
	    double newImage[][][] = new double[binary_map.length][binary_map[0].length][binary_map[0][0].length];
	    for (c = 0; c < binary_map.length; c++) {
	    	for (y = 0; y < binary_map[0].length; y++) {
	    		for (x = 0; x < binary_map[0][0].length; x++) {
	    		    if (binary_map[c][y][x]) {
	    		    	newImage[c][y][x] = img1[Math.min(c,numXColors-1)][y][x];
	    		    }
	    		    else {
	    		    	newImage[c][y][x] = img2[Math.min(c,numYColors-1)][y][x];
	    		    }
	    		}
	    	}
	    }
	    return newImage;
	}
	
	private boolean[][][] majority_filter(boolean map[][][], int ks) {
		// because the map is binary, comparing the sum with the area of
		// the kernel is equivalent to majority vote
		int c,x,y;
		int r = (ks-1)/2;
		int xcumsum[][][] = new int[map.length][map[0].length][map[0][0].length];
		for (c = 0; c < map.length; c++) {
			for (y = 0; y < map[0].length; y++) {
				if (map[c][y][0]) {
					xcumsum[c][y][0] = 1;
				}
				else {
					xcumsum[c][y][0] = 0;
				}
				for (x = 1; x < map[0][0].length; x++) {
					if (map[c][y][x]) {
						xcumsum[c][y][x] = xcumsum[c][y][x-1] + 1;
					}
					else {
						xcumsum[c][y][x] = xcumsum[c][y][x-1];
					}
				}
			}
		}
		int left[][][] = new int[map.length][map[0].length][r+1];
		for (c = 0; c < map.length; c++) {
			for (y = 0; y < map[0].length; y++) {
				for (x = r; x <= 2*r; x++) {
					left[c][y][x-r] = xcumsum[c][y][x];
				}
			}
		}
		int middle[][][] = new int[map.length][map[0].length][map[0][0].length-2*r-1];
		for (c = 0; c < map.length; c++) {
			for (y = 0; y < map[0].length; y++) {
				for (x = 2*r+1; x < map[0][0].length; x++) {
					middle[c][y][x-2*r-1] = xcumsum[c][y][x] - xcumsum[c][y][x-2*r-1];
				}
			}
		}
		int right[][][] = new int[map.length][map[0].length][r];
		for (c = 0; c < map.length; c++) {
			for (y = 0; y < map[0].length; y++) {
				for (x = map[0][0].length -2*r-1 ; x < map[0][0].length - r - 1; x++) {
					right[c][y][x-(map[0][0].length - 2*r-1)] = xcumsum[c][y][map[0][0].length-1] - xcumsum[c][y][x];
				}
			}
		}
		int xdiff[][][] = new int[map.length][map[0].length][map[0][0].length];
		for (c = 0; c < map.length; c++) {
			for (y = 0; y < map[0].length; y++) {
				for (x = 0; x < r+1; x++) {
					xdiff[c][y][x] = left[c][y][x];
				}
				for (x = r+1; x < map[0][0].length-r; x++) {
					xdiff[c][y][x] = middle[c][y][x-r-1];
				}
				for (x = map[0][0].length-r; x < map[0][0].length; x++) {
					xdiff[c][y][x] = right[c][y][x-map[0][0].length+r];
				}
			}
		}
		int ycumsum[][][] = new int[map.length][map[0].length][map[0][0].length];
		for (c = 0; c < map.length; c++) {
			for (x = 0; x < map[0][0].length; x++) {
				ycumsum[c][0][x] = xdiff[c][0][x]; 
				for (y = 1; y < map[0].length; y++) {
					ycumsum[c][y][x] = ycumsum[c][y-1][x] + xdiff[c][y][x];
				}
			}
		}
		left = new int[map.length][r+1][map[0][0].length];
		for (c = 0; c < map.length; c++) {
			for (x = 0; x < map[0][0].length; x++) {
				for ( y = r; y <= 2*r; y++) {
					left[c][y-r][x] = ycumsum[c][y][x];
				}
			}
		}
		middle = new int[map.length][map[0].length-2*r-1][map[0][0].length];
		for (c = 0; c < map.length; c++) {
			for (x = 0; x < map[0][0].length; x++) {
				for (y = 2*r+1; y < map[0].length; y++) {
					middle[c][y-2*r-1][x] = ycumsum[c][y][x] - ycumsum[c][y-2*r-1][x];
				}
			}
		}
		right = new int[map.length][r][map[0][0].length];
		for (c = 0; c < map.length; c++) {
			for (x = 0; x < map[0][0].length; x++) {
				for (y = map[0].length -2*r-1 ; y < map[0].length - r - 1; y++) {
					right[c][y-(map[0].length - 2*r-1)][x] = ycumsum[c][map[0].length-1][x] - ycumsum[c][y][x];
				}
			}
		}
		int box_filter[][][] = new int[map.length][map[0].length][map[0][0].length];
		for (c = 0; c < map.length; c++) {
			for (x = 0; x < map[0][0].length; x++) {
				for (y = 0; y < r+1; y++) {
					box_filter[c][y][x] = left[c][y][x];
				}
				for (y = r+1; y < map[0].length-r; y++) {
					box_filter[c][y][x] = middle[c][y-r-1][x];
				}
				for (y = map[0].length-r; y < map[0].length; y++) {
					box_filter[c][y][x] = right[c][y-map[0].length+r][x];
				}
			}
		}
		boolean output[][][] = new boolean[map.length][map[0].length][map[0][0].length];
		double sum;
		double threshold = (ks * ks)/2.0;
		for (c = 0; c < map.length; c++) {
			for (y = 0; y < map[0].length; y++) {
				for (x = 0; x < map[0][0].length; x++) {
					if (box_filter[c][y][x] > threshold) {
						output[c][y][x] = true;
					}
				}
			}
		}
		return output;
	}
	
	private boolean[][][] slow_majority_filter(boolean map[][][], int ks) {
		int c,x,y,i,j;
		int p = (ks - 1)/2;
		boolean map_p[][][] = new boolean[map.length][map[0].length + 2*p][map[0][0].length + 2*p];
		for (c = 0; c < map.length; c++) {
			for (y = 0; y < map[0].length; y++) {
				for (x = 0; x < map[0][0].length; x++) {
					map_p[c][y + p][x+p] = map[c][y][x];
				}
			}
		}
		
		boolean output[][][] = new boolean[map.length][map[0].length][map[0][0].length];
		double sum;
		double threshold = (ks * ks)/2.0;
		for (j = 0; j < map[0].length; j++) {
			for (i = 0; i < map[0][0].length; i++) {
				for (c = 0; c < numDestColors; c++) {
				    sum = 0.0;
				    for (y = 0; y < ks; y++) {
				    	for (x = 0; x < ks; x++) {
				    		if (map_p[c][y+j][x+i]) {
				    			sum += 1.0;
				    		}
				    	}
				    }
				    output[c][j][i] = sum > threshold;
				}
			}
		}
		return output;
	}
	
	private boolean[][][] decision_map(double img1[][][], double img2[][][], int ks) {
		int c,x,y,i,j;
		int halfks = (ks-1)/2;
		double maxval;
		double filty1[][][] = new double[numXColors][img1[0].length][img1[0][0].length];
		double max1[][][] = new double[numXColors][img1[0].length][img1[0][0].length];
		for (c = 0; c < numXColors; c++) {
			for (x = 0; x < img1[0][0].length; x++) {
				for (y = 0; y < img1[0].length; y++) {
					maxval = -Double.MAX_VALUE;
					for (j = -halfks; j <= halfks; j++) {
					    if (((y + j) >= 0) && ((y+j) < img1[0].length) && (img1[c][y+j][x] > maxval)) {
					    	maxval = img1[c][y+j][x];
					    }
					}
					filty1[c][y][x] = maxval;
				}
			}
		}
		for (c = 0; c < numXColors; c++) {
			for (y = 0; y < img1[0].length; y++) {
				for (x = 0; x < img1[0][0].length; x++) {
				    maxval = -Double.MAX_VALUE;
				    for (i = -halfks; i <= halfks; i++) {
				    	if (((x + i) >= 0) && ((x+i) < img1[0][0].length) && (filty1[c][y][x+i] > maxval)) {
				    		maxval = filty1[c][y][x+i];
				    	}
				    }
				    max1[c][y][x] = maxval;
				}
			}
		}
		
		double filty2[][][] = new double[numYColors][img2[0].length][img2[0][0].length];
		double max2[][][] = new double[numYColors][img2[0].length][img2[0][0].length];
		for (c = 0; c < numYColors; c++) {
			for (x = 0; x < img2[0][0].length; x++) {
				for (y = 0; y < img2[0].length; y++) {
					maxval = -Double.MAX_VALUE;
					for (j = -halfks; j <= halfks; j++) {
					    if (((y + j) >= 0) && ((y+j) < img2[0].length) && (img2[c][y+j][x] > maxval)) {
					    	maxval = img2[c][y+j][x];
					    }
					}
					filty2[c][y][x] = maxval;
				}
			}
		}
		for (c = 0; c < numYColors; c++) {
			for (y = 0; y < img2[0].length; y++) {
				for (x = 0; x < img2[0][0].length; x++) {
				    maxval = -Double.MAX_VALUE;
				    for (i = -halfks; i <= halfks; i++) {
				    	if (((x + i) >= 0) && ((x+i) < img2[0][0].length) && (filty2[c][y][x+i] > maxval)) {
				    		maxval = filty2[c][y][x+i];
				    	}
				    }
				    max2[c][y][x] = maxval;
				}
			}
		}
		
		boolean output[][][] = new boolean[numDestColors][img1[0].length][img1[0][0].length];
		for (c = 0; c < numDestColors; c++) {
			for (y = 0; y < img1[0].length; y++) {
				for (x = 0; x < img1[0][0].length; x++) {
					output[c][y][x] = max1[Math.min(c,numXColors-1)][y][x] > max2[Math.min(c,numYColors-1)][y][x];	
				}
			}
		}
		return output;
	}
	
	private boolean[][][] slow_decision_map(double img1[][][], double img2[][][], int ks) {
		int i,j,x,y,c;
		int p = (ks - 1)/2;
		double img1_p[][][] = new double[numXColors][img1[0].length + 2*p][img1[0][0].length + 2*p];
		for (c = 0; c < numXColors; c++) {
			for (y = 0; y < img1[0].length; y++) {
				for (x = 0; x < img1[0][0].length; x++) {
					img1_p[c][y + p][x+p] = img1[c][y][x];
				}
			}
		}
		double img2_p[][][] = new double[numYColors][img2[0].length + 2*p][img2[0][0].length + 2*p];
		for (c = 0; c < numYColors; c++) {
			for (y = 0; y < img2[0].length; y++) {
				for (x = 0; x < img2[0][0].length; x++) {
					img2_p[c][y+p][x+p] = img2[c][y][x];
				}
			}
		}
		
		boolean output[][][] = new boolean[numDestColors][img1[0].length][img1[0][0].length];
		double w1[] = new double[numXColors];
		double w2[] = new double[numYColors];
		for (j = 0; j < img1[0].length; j++) {
			for (i = 0; i < img1[0][0].length; i++) {
				for (c = 0; c < numXColors; c++) {
					w1[c] = 0.0;
					for (y = 0; y < ks; y++) {
						for (x = 0; x < ks; x++) {
							if (Math.abs(img1_p[c][j+y][i+x]) > w1[c]) {
							    w1[c] = Math.abs(img1_p[c][j+y][i+x]);	
							}
						}
					}
				} // for (c = 0; c < numXColors; c++)
				for (c = 0; c < numYColors; c++) {
					w2[c] = 0.0;
					for (y = 0; y < ks; y++) {
						for (x = 0; x < ks; x++) {
							if (Math.abs(img2_p[c][j+y][i+x]) > w2[c]) {
							    w2[c] = Math.abs(img2_p[c][j+y][i+x]);	
							}
						}
					}
				} // for (c = 0; c < numYColors; c++)
				for (c = 0; c < numDestColors; c++) {
					output[c][j][i] = w1[Math.min(c,numXColors-1)] > w2[Math.min(c,numYColors-1)];
				}
			}
		}
		return output;
	}

}