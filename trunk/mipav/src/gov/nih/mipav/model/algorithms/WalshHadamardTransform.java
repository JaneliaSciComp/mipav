package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

/**Copyright (c) 2016, suresh
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
*/

public class WalshHadamardTransform extends AlgorithmBase {

 // program to find the 2D Walsh-Hadamard Transform
 // specially helpful for grayscale images

 // Lots more @ www.surkur.blogspot.com

 // Please choose small images, large images take too long time for execution
 // Image size M=N square image
 // M=N=2 power of m
	ModelImage transformImage;
	ModelImage inverseImage;
	
	public WalshHadamardTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
	}
	
	public void runAlgorithm() {
		int xDim; // n
    	int yDim;
    	int zDim;
    	int length;
    	int m; // log2(n)
    	int xTest;
    	double buffer[];
    	int intBuffer[];
    	int z;
    	int n;
    	int u;
    	int v;
    	int x;
    	int y;
    	int i;
    	int pu;
    	int pv;
    	int sel;
    	int pu2;
    	int pv2;
    	int c;
    	int c1;
    	double g[][][];
    	int in;
    	double t[][];
    	double zb[][];
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new double[length];
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
        n = xDim;
        
        m = 0;
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	m++;
        	xTest = xTest/2;
        }
        if (xTest != 1) {
        	MipavUtil.displayError("X dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Walsh-Hadamard Transform: Image(s) locked", true);

                return;
            }
        	g = new double[n][n][n*n];
            t = new double[n][n];
            zb = new double[n][n];
            c1 = 0;
            in = 0;
        	for (u = 0; u <= n-1; u++) {
        	    for (v = 0; v <= n-1; v++) {
        	        for (x = 0; x <= n-1; x++) {
        	            for (y = 0; y <= n-1; y++) {
        	                for (i = 1; i <= m; i++) {
        	                    if (i == 1) {
        	                    	sel = (1 << (m-1));
        	                    	pu = ((u & sel) >>> (m-1));
        	                    	pv = ((v & sel) >>> (m-1));
        	                    }
        	                    else {
        	                        sel = (1 << (m-i+1));
        	                        pu = ((u & sel) >>> (m-i+1));
        	                        pv = ((v & sel) >>> (m-i+1));
        	                        sel = (1 << (m-i));
        	                        pu2 = ((u & sel) >>> (m-i));
        	                        pv2 = ((v & sel) >>> (m-i));
        	                        pu = (pu + pu2)%2;
        	                        pv = (pv + pv2)%2;
        	                    }
        	                    sel = (1 << (i-1));
        	                    pu2 = ((x & sel) >>> (i-1));
        	                    pu = pu * pu2;
        	                    pv2 = ((y & sel) >>> (i-1));
        	                    pv= pv * pv2;
        	                    c = (pu + pv)%2;
        	                    c1 = (c1+c)%2;
        	                } // for (i = 1; i <= m; i++)
        	                if (c1 == 0) {
        	                	g[x][y][in] = 1.0/n;
        	                }
        	                else {
        	                	g[x][y][in] = -1.0/n;
        	                }
        	                c1 = 0;
        	            } // for (y = 0; y <= n-1; y++)
        	        } // for (x = 0; x <= n-1; x++)
        	        for (y = 0; y < yDim; y++) {
        	        	for (x = 0; x < xDim; x++) {
        	        	    t[u][v] = t[u][v] + g[x][y][in]	* buffer[x + y * xDim];
        	        	}
        	        }
        	        in = in+1;
        	    } // for (v = 0; v <= n-1; v++)
        	} // for (u = 0; u <= n-1; u++)
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			buffer[x + y* xDim] = t[x][y];
        		}
        	}
        	try {
                transformImage.importData(z*length, buffer, false);
             } catch (IOException error) {
                buffer = null;
                errorCleanUp("Walsh Hadamard Transform: Image(s) locked", true);

                return;
             }
        	
        	// Inverse Transform
        	in = 0;
        	for (u = 0; u <= n-1; u++) {
        		for (v = 0; v <= n-1; v++) {
        			for (y = 0; y < yDim; y++) {
        				for (x = 0; x < xDim; x++) {
        			        zb[u][v] = zb[u][v] + g[x][y][in] * t[x][y];
        				}
        			}
        			in = in+1;
        		}
        	}
        	
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			intBuffer[x + y* xDim] = (int)Math.round(zb[x][y]);
        		}
        	}
        	try {
                transformImage.importData(z*length, intBuffer, false);
             } catch (IOException error) {
                buffer = null;
                errorCleanUp("Walsh Hadamard Transform: Image(s) locked", true);

                return;
             }
        } // for (z = 0; z < zDim; z++)
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}
}
