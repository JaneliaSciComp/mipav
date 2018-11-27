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

    private double H[][];
    private ModelImage transformImage;
	private ModelImage inverseImage;
    
    public HaarTransform() {
		
	}
    
    public HaarTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
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

}