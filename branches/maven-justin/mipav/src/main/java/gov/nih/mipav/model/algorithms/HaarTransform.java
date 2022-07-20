package gov.nih.mipav.model.algorithms;


import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;

/**
 * @author ilb
 * Reference:
 * Digital Image Processing 4th Edition by Rafael C. Gonzalez and Richard E. Woods, Chapter 6.9, Haar Transform, pp. 490-492.
 *
 */
 

public class HaarTransform extends AlgorithmBase  {

    // ~ Static fields/initializers
    // -----------------------
	
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

    private double H[][];
    private ModelImage transformImage;
	private ModelImage inverseImage;
    
    public HaarTransform() {
		
	}
    
    public HaarTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg,
    		int filterType, double filterVal1, double filterVal2) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
		this.filterType = filterType;
		this.filterVal1 = filterVal1;
		this.filterVal2 = filterVal2;
	}
    
    public void runAlgorithm() {
		int xDim;
		int yDim;
		int zDim;
		int length;
		double doubleBuffer[];
		int xTest;
		int yTest;
		int z;
		double src[][];
        double dst[][];
        int x;
        int y;
		xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        doubleBuffer = new double[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
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
        yTest = yDim;
        while ((yTest % 2) == 0) {
        	yTest = yTest/2;
        }
        if (yTest != 1) {
        	MipavUtil.displayError("Y dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        src = new double[yDim][xDim];
        dst = new double[yDim][xDim];
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, doubleBuffer); // locks and releases lock
            } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Haar Transform: Image(s) locked", true);

                return;
            }
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			src[y][x] = doubleBuffer[x + y * xDim];
        			dst[y][x] = 0;
        		}
        	}
        	forward2D(yDim, xDim, src, dst);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			doubleBuffer[x + y * xDim] = dst[y][x];
        			src[y][x] = 0;
        		}
        	}
        	try {
                transformImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Haar Transform: Image(s) locked", true);

                return;
             }
        	if (filterType != FILTER_NONE) {
        		filter(doubleBuffer, filterType,filterVal1,filterVal2);	
        		for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            			dst[y][x] = doubleBuffer[x + y * xDim];
            		}
            	}
        	}
        	// Inverse transform
        	inverse2D(yDim, xDim, dst, src);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			doubleBuffer[x + y * xDim] = src[y][x];
        		}
        	}
        	try {
                inverseImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Haar Transform: Image(s) locked", true);

                return;
             }
        }
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}
    
    public void test1D() {
    	int i;
    	double arr[] = new double[]{1.0,2.0,5.0,7.0};
    	generateTransformationMatrix(arr.length);
    	double forward[];
    	forward = forward1D(arr);
    	double inverse[];
    	inverse = inverse1D(forward);
    	for (i = 0; i < inverse.length; i++) {
    		System.out.println("inverse["+i+"] = " + inverse[i]);
    	}
    }
    
    public void forward2D(int yDim, int xDim, double src[][], double dst[][]) {
		int i, j;
		double transT[][] = new double[xDim][yDim];
		double dstT[][] = new double[xDim][yDim];
		generateTransformationMatrix(xDim);
		for (i = 0; i < yDim; i++) {
		    dst[i] = forward1D(src[i]);	
		}
		for (i = 0; i < xDim; i++) {
			for (j = 0; j < yDim; j++) {
				transT[i][j] = dst[j][i];
			}
		}
		generateTransformationMatrix(yDim);
		for (i = 0; i < xDim; i++) {
			dstT[i] = forward1D(transT[i]);
		}
		for (i  = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				dst[i][j] = dstT[j][i];
			}
		}
	}
    
    public void inverse2D(int yDim, int xDim, double src[][], double dst[][]) {
		int i, j;
		double transT[][] = new double[xDim][yDim];
		double dstT[][] = new double[xDim][yDim];
		generateTransformationMatrix(xDim);
		for (i = 0; i < yDim; i++) {
		    dst[i] = inverse1D(src[i]);	
		}
		for (i = 0; i < xDim; i++) {
			for (j = 0; j < yDim; j++) {
				transT[i][j] = dst[j][i];
			}
		}
		generateTransformationMatrix(yDim);
		for (i = 0; i < xDim; i++) {
			dstT[i] = inverse1D(transT[i]);
		}
		for (i  = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				dst[i][j] = dstT[j][i];
			}
		}
	}
    
    public double[] forward1D(double arr[]) {
    	int i, j;
    	int Ntest;
    	double output[];
        int N = arr.length;
        Ntest = N;
        while ((Ntest % 2) == 0) {
        	Ntest = Ntest/2;
        }
        if (Ntest != 1) {
        	MipavUtil.displayError("Input array not a power of 2");
        	return null;	
        }
        output = new double[N];
        for (i = 0; i < N; i++) {
            for (j = 0; j < N; j++) {
            	output[i] += H[i][j] * arr[j];
            }
        }
        return output;
    }
    
    public double[] inverse1D(double arr[]) {
    	int i, j;
    	int Ntest;
    	double Hinv[][];
    	double output[];
        int N = arr.length;
        Ntest = N;
        while ((Ntest % 2) == 0) {
        	Ntest = Ntest/2;
        }
        if (Ntest != 1) {
        	MipavUtil.displayError("Input array not a power of 2");
        	return null;	
        }
        Hinv = new double[N][N];
        for (i = 0; i < N; i++) {
        	for (j = 0; j < N; j++) {
        		Hinv[i][j] = H[j][i];
        	}
        }
        output = new double[N];
        for (i = 0; i < N; i++) {
            for (j = 0; j < N; j++) {
            	output[i] += Hinv[i][j] * arr[j];
            }
        }
        return output;	
    }
    
    public void generateTransformationMatrix(int N) {
    	// u = 2**p + q
    	// u, p, and q are integers
    	// u goes from 1 to N-1
    	int i;
    	int j;
    	int u;
        int p = 0;
        int q = 0;
        int pow;
        int xN;
        H = new double[N][N];
        int base;
        int mid;
        int top;
        double scale;
        int Ntest;
        Ntest = N;
        int exp = 0;
        while ((Ntest % 2) == 0) {
        	Ntest = Ntest/2;
        	exp++;
        }
        if (Ntest != 1) {
        	MipavUtil.displayError("Input number not a power of 2");
        	return;	
        }
        for (i = 0; i < N; i++) {
        	H[0][i] = 1.0;
        }
        for (u = 1; u <= N-1; u++) {
        	pow = (int)Math.pow(2, p+1);
        	if (pow == u) {
        		p++;
        		q = 0;
        	}
        	else {
	        	pow = (int)Math.pow(2, p);
	            if (pow	== u) {
	            	q = 0;
	            }
	            else {
	            	q = u - pow;
	            }
        	}
        	for (xN = 0; xN < N; xN++) {
        		base = q * (int)Math.pow(2, exp-p);
        		mid = base + (int)Math.pow(2, exp-p-1);
        		top = base + (int)Math.pow(2, exp-p);
        		if ((xN >= base) && (xN < mid)) {
        			H[u][xN] = Math.pow(2, 0.5*p);
        		}
        		else if ((xN >= mid) && (xN < top)) {
        			H[u][xN] = -Math.pow(2, 0.5*p);
        		}
        		else {
        			H[u][xN] = 0.0;
        		}
        	}
        } // for (u = 1; u <= N-1; u++)
        scale = 1.0/Math.sqrt(N);
        for (i = 0; i < N; i++) {
        	for (j = 0; j < N; j++) {
        		H[i][j] = scale * H[i][j];
        	}
        }
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