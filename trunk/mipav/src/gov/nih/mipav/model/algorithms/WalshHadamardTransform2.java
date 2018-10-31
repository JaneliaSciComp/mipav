package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

/*
FWHT - Fast Walsh Hadamard Transform in C
BSD license.
by Sven Nilsen, 2012
http://www.cutoutpro.com

Version: 0.001 in angular degrees version notation
http://isprogrammingeasy.blogspot.no/2012/08/angular-degrees-versioning-notation.html
*/
/*
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
*/

/*

Fast Walsh Hadamard Transform

The input souce has to be in size 2^n.

Example use: You have two similar images where the camera
is moved or rotated slightly and you want to find the
transformation of the camera movement. You then take
two 4x4 pieces of each image and transform each color channel
with FWHT and then take the differences, sum them
and you will have a number that tells you how similar the
two pieces are.

When you apply a FWHT twice, you get the same as before,
only multiplied with a factor of the size of the data.

*/

public class WalshHadamardTransform2 extends AlgorithmBase {
	
	ModelImage transformImage;
	ModelImage inverseImage;
	
	public WalshHadamardTransform2() {
		
	}
	
	public WalshHadamardTransform2(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
	}
	
	public void runAlgorithm() {
		int xDim;
		int yDim;
		int zDim;
		int length;
		int intBuffer[];
		int xTest;
		int z;
		int src[][];
        int dst[][];
        int x;
        int y;
		xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        intBuffer = new int[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        
        // Image must be square
        if (xDim != yDim) {
        	MipavUtil.displayError("Must have xDim == yDim");
        	setCompleted(false);
        	return;
        }
        
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	xTest = xTest/2;
        }
        if (xTest != 1) {
        	MipavUtil.displayError("X dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        src = new int[xDim][xDim];
        dst = new int[xDim][xDim];
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, intBuffer); // locks and releases lock
            } catch (IOException error) {
                intBuffer = null;
                errorCleanUp("Walsh-Hadamard Transform2: Image(s) locked", true);

                return;
            }
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			src[y][x] = intBuffer[x + y * xDim];
        			dst[y][x] = 0;
        		}
        	}
        	fwht_transform2D(xDim, src, dst);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			intBuffer[x + y * xDim] = dst[y][x];
        			src[y][x] = 0;
        		}
        	}
        	try {
                transformImage.importData(z*length, intBuffer, false);
             } catch (IOException error) {
                intBuffer = null;
                errorCleanUp("Walsh Hadamard Transform2: Image(s) locked", true);

                return;
             }
        	// Inverse transform
        	fwht_transform2D(xDim, dst, src);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			intBuffer[x + y * xDim] = src[y][x]/length;
        		}
        	}
        	try {
                inverseImage.importData(z*length, intBuffer, false);
             } catch (IOException error) {
                intBuffer = null;
                errorCleanUp("Walsh Hadamard Transform2: Image(s) locked", true);

                return;
             }
        } // for (z = 0; z < zDim; z++)
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}
	
	public void fwht_transform2D(int n, int src[][], int dst[][]) {
		int i, j;
		int transT[][] = new int[n][n];
		int dstT[][] = new int[n][n];
		for (i = 0; i < n; i++) {
		    fwht_transform(n, src[i], dst[i]);	
		}
		for (i = 0; i < n; i++) {
			for (j = 0; j < n; j++) {
				transT[i][j] = dst[j][i];
			}
		}
		for (i = 0; i < n; i++) {
			fwht_transform(n, transT[i], dstT[i]);
		}
		for (i  = 0; i < n; i++) {
			for (j = 0; j < n; j++) {
				dst[i][j] = dstT[j][i];
			}
		}
	}
	
	// Transforms a vector of integers
	public void fwht_transform(int n, int src[], int dst[]) {
		int i, j, s;
		int adata[] = new int[n];
		int bdata[] = new int[n];
		int tmp[] = new int[n];
		
		for (i = 0; i < n; i++) {
			adata[i] = src[i];
		}
		
		// Fast Walsh Hadamard Transform
		for (i = n>>1; i > 0; i >>= 1) {
			for (j = 0; j < n; j++) {
				s = (j/i)%2;
				if (s == 1) {
					bdata[j] = adata[j-i] - adata[j];
				}
				else {
					bdata[j] = adata[j] + adata[j+i];
				}
			} // for (j = 0; j < n; j++)
			tmp = adata;
			adata = bdata;
			bdata = tmp;
		} // for (i = n>>1; i > 0; i >>= 1)
		
		for (i = 0; i < n; i++) {
			dst[i] = adata[i];
		}
	}
	
	// Normalizes the data with respect to the length.
	// You might get rounding errors if the values are not
	// divisible by n.
	public void fwht_normalize(int n, int src[]) {
		int i;
		for (i = 0; i < n; i++) {
			src[i] /= n;
		}
	}
	
	// Finds the sum of differences between two sets of data.
	// The resulting difference is divided by the length.
	public double fwht_sum_absolute_difference(int n, int a[], int b[]) {
		long sum = 0;
		int diff;
		int i;
		for (i = 0; i < n; i++) {
			diff = b[i] - a[i];
			sum = sum + Math.abs(diff);
		}
		return sum/(double)n;
	}
	
	public void test() {
	    int a[]	= new int[]{1,0,1,0,0,1,1,0};
	    int n = 8;
	    int b[] = new int[n];
	    fwht_transform(n, a, b);
	    if (b[0] != 4) System.err.println("b[0] = " + b[0] + " instead of the correct 4");
	    if (b[1] != 2) System.err.println("b[1] = " + b[1] + " instead of the correct 2");
	    if (b[2] != 0) System.err.println("b[2] = " + b[2] + " instead of the correct 0");
	    if (b[3] != -2) System.err.println("b[3] = " + b[3] + " instead of the correct -2");
	    if (b[4] != 0) System.err.println("b[4] = " + b[4] + " instead of the correct 0");
	    if (b[5] != 2) System.err.println("b[5] = " + b[5] + " instead of the correct 2");
	    if (b[6] != 0) System.err.println("b[6] = " + b[6] + " instead of the correct 0");
	    if (b[7] != 2) System.err.println("b[7] = " + b[7] + " instead of the correct 2");
	    fwht_transform(n, b, a);
	    fwht_normalize(n, a);
	    if (a[0] != 1) System.err.println("a[0] = " + a[0] + " instead of the correct 1");
	    if (a[1] != 0) System.err.println("a[1] = " + a[1] + " instead of the correct 0");
	    if (a[2] != 1) System.err.println("a[2] = " + a[2] + " instead of the correct 1");
	    if (a[3] != 0) System.err.println("a[3] = " + a[3] + " instead of the correct 0");
	    if (a[4] != 0) System.err.println("a[4] = " + a[4] + " instead of the correct 0");
	    if (a[5] != 1) System.err.println("a[5] = " + a[5] + " instead of the correct 1");
	    if (a[6] != 1) System.err.println("a[6] = " + a[6] + " instead of the correct 1");
	    if (a[7] != 0) System.err.println("a[7] = " + a[7] + " instead of the correct 0");
	    
	    a= new int[]{1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        n = a.length;
        b = new int[n];
        fwht_transform(n, a, b);
        fwht_transform(n, b, a);
        fwht_normalize(n, a);
        int ans[] = {1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        int i;
        for (i = 0; i < n; i++) {
            if (a[i] != ans[i]) System.err.println("a["+i+"] = " + a[i] + " instead of the correct " + ans[i]);
        }

        a = new int[]{1,0,1,0,0,1,1,0};
        b = new int[]{1,0,0,0,0,1,1,0};
        n = 8;
        double diff_a = fwht_sum_absolute_difference(n, a, b);
        double diff_b = fwht_sum_absolute_difference(n, b, a);
        if (diff_a != diff_b) System.err.println("diff_a = " + diff_a + " diff_b = " + diff_b);
        
        int a2[] = new int[n];
        int b2[] = new int[n];
        fwht_transform(n, a, a2);
        fwht_transform(n, b, b2);
        double diff_a2 = fwht_sum_absolute_difference(n, a2, b2);
        double diff_b2 = fwht_sum_absolute_difference(n, b2, a2);
        if (diff_a2 != diff_b2) System.err.println("diff_a2 = " + diff_a2 + " diff_b2 = " + diff_b2);
        
        fwht_transform(n, a2, a);
        fwht_transform(n, b2, b);
        fwht_normalize(n, a);
        fwht_normalize(n, b);
        
        double diff_a3 = fwht_sum_absolute_difference(n, a, b);
        double diff_b3 = fwht_sum_absolute_difference(n, b, a);
        
        if (diff_a != diff_a3) System.err.println("diff_a = " + diff_a + " diff_a3 = " + diff_a3);
        if (diff_b != diff_b3) System.err.println("diff_b = " + diff_b + " diff_b3 = " + diff_b3);
	}
}