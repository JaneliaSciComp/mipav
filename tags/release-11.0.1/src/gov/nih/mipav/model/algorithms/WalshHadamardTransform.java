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
	private final int FILTER_NONE = 0;
	private final int FILTER_SOFT = 1;
	private final int FILTER_NN_GARROTE = 2;
	private final int FILTER_HARD = 3;
	private final int FILTER_GREATER = 4;
	private final int FILTER_LESS = 5;
	private final int FILTER_THRESHOLD_FIRM = 6;
	private int filterType;
	private double filterVal1;
	private double filterVal2;
	
	public WalshHadamardTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg,
			int filterType, double filterVal1, double filterVal2) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
		this.filterType = filterType;
		this.filterVal1 = filterVal1;
		this.filterVal2 = filterVal2;
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
        	if (filterType != FILTER_NONE) {
        		filter(buffer, filterType,filterVal1,filterVal2);
        		for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            			t[x][y] = buffer[x + y* xDim];
            		}
            	}
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
                inverseImage.importData(z*length, intBuffer, false);
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
	
	private void filter(double data[], int filterType, double filterVal1, double filterVal2) {
    	if (filterType == FILTER_SOFT) {
    		soft(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_NN_GARROTE) {
    		nn_garrote(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_HARD) {
    		hard(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_GREATER) {
    		greater(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_LESS) {
    		less(data, filterVal1, filterVal2);
    	}
    	else if (filterType == FILTER_THRESHOLD_FIRM) {
    		threshold_firm(data, filterVal1, filterVal2);
    	}
    	return;
    }
    
    public double[] soft(double data[], double value, double substitute) {
    	// Default substitute = 0
    	int i;
    	double magnitude[] = new double[data.length];
    	double thresholded[] = new double[data.length];
    	for (i = 0; i < data.length; i++) {
    		if (data[i] == 0.0) {
    			thresholded[i] = 0.0;
    		}
    		else {
    		    magnitude[i] = Math.abs(data[i]);
    		    thresholded[i] = (1.0 - value/magnitude[i]);
    		    if (thresholded[i] < 0.0) {
    		    	thresholded[i] = 0.0;
    		    }
    		    thresholded[i] = data[i] * thresholded[i];
    		}
    	}
    	
    	if (substitute == 0) {
    		return thresholded;
    	}
    	else {
    		for (i = 0; i < thresholded.length; i++) {
    			if (magnitude[i] < value) {
    				thresholded[i] = substitute;
    			}
    		}
    		return thresholded;
    	}
    }
    
    public double[] nn_garrote(double data[], double value, double substitute) {
        // Non-negative Garrote
    	// Default substitute = 0
    	int i;
    	double magnitude[] = new double[data.length];
    	double valueSquared = value * value;
    	double thresholded[] = new double[data.length];
    	for (i = 0; i < data.length; i++) {
    		if (data[i] == 0.0) {
    			thresholded[i] = 0.0;
    		}
    		else {
    		    magnitude[i] = Math.abs(data[i]);
    		    thresholded[i] = (1.0 - valueSquared/(magnitude[i] * magnitude[i]));
    		    if (thresholded[i] < 0.0) {
    		    	thresholded[i] = 0.0;
    		    }
    		    thresholded[i] = data[i] * thresholded[i];
    		}
    	}
    	
    	if (substitute == 0) {
    		return thresholded;
    	}
    	else {
    		for (i = 0; i < thresholded.length; i++) {
    			if (magnitude[i] < value) {
    				thresholded[i] = substitute;
    			}
    		}
    		return thresholded;
    	}
    }
    
    public double[] hard(double data[], double value, double substitute) {
    	// default substitute = 0.0
    	double data2[] = data.clone();
    	int i;
        for (i = 0; i < data2.length; i++) {
            if (Math.abs(data2[i]) < value) {
            	data2[i] = substitute;
            }
        }
        return data2;
    }
    
    public double[][] hard(double data[][], double value, double substitute) {
    	// default substitute = 0.0
    	double data2[][] = data.clone();
    	int i,j;
        for (i = 0; i < data2.length; i++) {
        	for (j = 0; j < data2[i].length; j++) {
	            if (Math.abs(data2[i][j]) < value) {
	            	data2[i][j] = substitute;
	            }
        	}
        }
        return data2;
    }
    
    public double[] greater(double data[], double value, double substitute) {
    	double data2[] = data.clone();
        // default substitute = 0.0
        // greater thresholding only supports real data
    	int i;
    	for (i = 0; i  < data2.length; i++) {
    		if (data2[i] < value) {
    			data2[i] = substitute;
    		}
    	}
        return data2;
    }
    
    public double[] less(double data[], double value, double substitute) {
    	// default substitute = 0.0
        // less thresholding only supports real data
    	double data2[] = data.clone();
    	int i;
    	for (i = 0; i < data2.length; i++) {
    		if (data2[i] > value) {
    			data2[i] = substitute;
    		}
    	}
    	return data2;
    }
    
    public double[] threshold_firm(double data[], double value_low, double value_high) {
        // Firm threshold.

        // The approach is intermediate between soft and hard thresholding [1]_. It
        // behaves the same as soft-thresholding for values below `value_low` and
        // the same as hard-thresholding for values above `thresh_high`.  For
        // intermediate values, the thresholded value is in between that corresponding
        // to soft or hard thresholding.

        // Parameters
        // ----------
        // data : array-like
        //    The data to threshold.  This can be either real or complex-valued.
        // value_low : float
        //    Any values smaller then `value_low` will be set to zero.
        // value_high : float
        //    Any values larger than `value_high` will not be modified.

        // Notes
        // -----
        // This thresholding technique is also known as semi-soft thresholding [2]_.

        // For each value, `x`, in `data`. This function computes::

        //    if np.abs(x) <= value_low:
        //        return 0
        //    elif np.abs(x) > value_high:
        //        return x
        //    elif value_low < np.abs(x) and np.abs(x) <= value_high:
        //        return x * value_high * (1 - value_low/x)/(value_high - value_low)

        // ``firm`` is a continuous function (like soft thresholding), but is
        // unbiased for large values (like hard thresholding).

        // If ``value_high == value_low`` this function becomes hard-thresholding.
        // If ``value_high`` is infinity, this function becomes soft-thresholding.

        // Returns
        // -------
        // val_new : array-like
        //    The values after firm thresholding at the specified thresholds.

        // See Also
        // --------
        // threshold

        // References
        // ----------
        // .. [1] H.-Y. Gao and A.G. Bruce. Waveshrink with firm shrinkage.
        //    Statistica Sinica, Vol. 7, pp. 855-874, 1997.
        // .. [2] A. Bruce and H-Y. Gao. WaveShrink: Shrinkage Functions and
        //    Thresholds. Proc. SPIE 2569, Wavelet Applications in Signal and
        //    Image Processing III, 1995.
        //    DOI:10.1117/12.217582
        int i;

        if (value_low < 0) {
            MipavUtil.displayError("value_low must be non-negative.");
            return null;
        }

        if (value_high < value_low) {
            MipavUtil.displayError("value_high must be greater than or equal to value_low.");
            return null;
        }

        
        double magnitude[] = new double[data.length];
        double thresholded[] = new double[data.length];
        double vdiff = value_high - value_low;
        for (i = 0; i < data.length; i++) {
        	if (data[i] == 0.0) {
        		thresholded[i] = 0.0;
        	}
        	else {
        	    magnitude[i] = Math.abs(data[i]);
        	    thresholded[i] = value_high * (1 - value_low/magnitude[i]) / vdiff;
        	    if (thresholded[i] < 0.0) {
        	    	thresholded[i] = 0.0;
        	    }
        	    thresholded[i] = data[i] * thresholded[i];
        	}
        }

        // restore hard-thresholding behavior for values > value_high
        for (i = 0; i < magnitude.length; i++) {
        	if (magnitude[i] > value_high) {
        		thresholded[i] = data[i];
        	}
        }
        return thresholded;
    }
}
