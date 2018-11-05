package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

public class WalshHadamardTransform3 extends AlgorithmBase {
	
	public final static int SEQUENCY = 1;
	public final static int DYADIC = 2;
	public final static int NATURAL = 3;
	
	private ModelImage transformImage;
	private ModelImage inverseImage;
	private int type;
	
	public WalshHadamardTransform3() {
		
	}
	
	public WalshHadamardTransform3(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg, int type) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
		this.type = type;
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
                errorCleanUp("Walsh-Hadamard Transform3: Image(s) locked", true);

                return;
            }
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			src[y][x] = doubleBuffer[x + y * xDim];
        			dst[y][x] = 0;
        		}
        	}
        	switch (type) {
        	case SEQUENCY:
        		fhtseq2D(yDim, xDim, src, dst, true);
        		break;
        	case DYADIC:
        		fhtdya2D(yDim, xDim, src, dst, true);
        		break;
        	case NATURAL:
        		fhtnat2D(yDim, xDim, src, dst, true);
        		break;
        	}
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
                errorCleanUp("Walsh Hadamard Transform3: Image(s) locked", true);

                return;
             }
        	// Inverse transform
        	switch (type) {
        	case SEQUENCY:
        		fhtseq2D(yDim, xDim, dst, src, false);
        		break;
        	case DYADIC:
        		fhtdya2D(yDim, xDim, dst, src, false);
        		break;
        	case NATURAL:
        		fhtnat2D(yDim, xDim, dst, src, false);
        		break;
        	}
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			doubleBuffer[x + y * xDim] = src[y][x];
        		}
        	}
        	try {
                inverseImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Walsh Hadamard Transform3: Image(s) locked", true);

                return;
             }
        }
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}
	
	public void fhtseq2D(int yDim, int xDim, double src[][], double dst[][], boolean forwardTransform) {
		int i, j;
		double transT[][] = new double[xDim][yDim];
		double dstT[][] = new double[xDim][yDim];
		for (i = 0; i < yDim; i++) {
		    dst[i] = fhtseq(src[i], forwardTransform);	
		}
		for (i = 0; i < xDim; i++) {
			for (j = 0; j < yDim; j++) {
				transT[i][j] = dst[j][i];
			}
		}
		for (i = 0; i < xDim; i++) {
			dstT[i] = fhtseq(transT[i], forwardTransform);
		}
		for (i  = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				dst[i][j] = dstT[j][i];
			}
		}
	}
	
	public void fhtdya2D(int yDim, int xDim, double src[][], double dst[][], boolean forwardTransform) {
		int i, j;
		double transT[][] = new double[xDim][yDim];
		double dstT[][] = new double[xDim][yDim];
		for (i = 0; i < yDim; i++) {
		    dst[i] = fhtdya(src[i], forwardTransform);	
		}
		for (i = 0; i < xDim; i++) {
			for (j = 0; j < yDim; j++) {
				transT[i][j] = dst[j][i];
			}
		}
		for (i = 0; i < xDim; i++) {
			dstT[i] = fhtdya(transT[i], forwardTransform);
		}
		for (i  = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				dst[i][j] = dstT[j][i];
			}
		}
	}
	
	public void fhtnat2D(int yDim, int xDim, double src[][], double dst[][], boolean forwardTransform) {
		int i, j;
		double transT[][] = new double[xDim][yDim];
		double dstT[][] = new double[xDim][yDim];
		for (i = 0; i < yDim; i++) {
		    dst[i] = fhtnat(src[i], forwardTransform);	
		}
		for (i = 0; i < xDim; i++) {
			for (j = 0; j < yDim; j++) {
				transT[i][j] = dst[j][i];
			}
		}
		for (i = 0; i < xDim; i++) {
			dstT[i] = fhtnat(transT[i], forwardTransform);
		}
		for (i  = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				dst[i][j] = dstT[j][i];
			}
		}
	}
	
	//// =======================================================
	// FUNCTIONS FOR 1-D SEQUENCY(WALSH),DYADIC(PALEY) AND 
    // NATURAL(HADAMARD)ORDERED FAST WALSH-HADAMARD TRANSFORM
	// =======================================================

	// -------------------------------------------------------
	// 1D sequency(Walsh)ordered Fast Walsh-Hadamard Transform
	// -------------------------------------------------------
	public double[] fhtseq(double data[], boolean forwardTransform) {
		// The function implement the 1D sequency(Walsh)ordered 
		// fast Walsh-Hadamard transform,
		// This algorithm is implemented in N log2 N additions and subtractions. 
		// Data sequence length should be an integer power of 2.
		// Otherwise last elements will be truncated.
		// The inverse transform is the same as the forward transform 
		// except for the multiplication factor N.
		 
		// Example:
		// x=[1 2 1 1]
		// y=fhtseq(x)
		 
		// Author: Gylson Thomas
		// e-mail: gylson_thomas@yahoo.com
		// Asst. Professor, Electrical and Electronics Engineering Dept.
		// MES College of Engineering Kuttippuram,
		// Kerala, India, February 2005.
		// copyright 2007.
		double x[];
		int N;
		int k1;
		int k2;
		int k3;
		int i1;
		int i2;
		int i3;
		int L1;
		int NTest;
		int log2;
		int i;
		int j;
		double temp1;
		double temp2;
	
		x=bitrevorder(data);
		N = x.length;
		k1=N; k2=1; k3=N/2;
		NTest = N;
		log2 = 0;
		while ((NTest % 2) == 0) {
	    	NTest = NTest/2;
	    	log2++;
	    }
		for (i1=1; i1 <= log2; i1++) {  // In-place iteration begins here 
		    L1=1;
		    for (i2=1; i2 <= k2; i2++) {
		        for (i3=1; i3 <= k3; i3++) {
		            i=i3+L1-1; j=i+k3;
		            temp1= x[i-1]; temp2 = x[j-1]; 
		            if((i2 % 2) == 0) {
		              x[i-1] = temp1 - temp2;
		              x[j-1] = temp1 + temp2;
		            }
		            else {
		              x[i-1] = temp1 + temp2;
		              x[j-1] = temp1 - temp2;
		            }
		        } // for (i3=1; i3 <= k3; i3++)
		        L1=L1+k1;
		    } // for (i2=1; i2 <= k2; i2++)
		    k1 = k1/2;  k2 = k2*2;  k3 = k3/2;
		} // for (i1=1; i1 <= log2; i1++)
		if (forwardTransform) {
			for (i1 = 0; i1 < N; i1++) {
		        x[i1]= (x[i1]/(double)N);
			}
		} // if (forwardTransform)
		
		return x;
	}


	// ------------------------------------------------------
	// 1D Dyadic(Paley)ordered Fast Hadamard Transform
	// ------------------------------------------------------
    public double[] fhtdya(double data[], boolean forwardTransform) {
		// The function implement the 1D dyadic (Paley) ordered fast Hadamard transform,
    	double x[];
    	int N;
    	int k1;
    	int k2;
    	int k3;
    	int NTest;
    	int log2;
    	int i1;
    	int i2;
    	int i3;
    	int L1;
    	double temp1;
    	double temp2;
    	int i;
    	int j;
		x=bitrevorder(data);
		N = x.length;
		k1=N; k2=1; k3=N/2;
		NTest = N;
		log2 = 0;
		while ((NTest % 2) == 0) {
	    	NTest = NTest/2;
	    	log2++;
	    }
		for (i1=1; i1 <= log2; i1++) {   
		    L1=1;
		    for (i2=1; i2 <= k2; i2++) {
		        for (i3=1; i3 <= k3; i3++) {
		            i=i3+L1-1; j=i+k3;
		            temp1= x[i-1]; temp2 = x[j-1]; 
		            x[i-1] = temp1 + temp2;
		            x[j-1] = temp1 - temp2;
		        } // for (i3=1; i3 <= k3; i3++)
		        L1=L1+k1;
		    } // for (i1=1; i1 <= log2; i1++)
		    k1 = k1/2;  k2 = k2*2;  k3 = k3/2;
		} // for (i1=1: i1 <= log2; i1++)
		if (forwardTransform) {
			for (i1 = 0; i1 < N; i1++) {
		        x[i1]= (x[i1]/(double)N);
			}
		} // if (forwardTransform)
		
		return x;
    }

	// ------------------------------------------------------
	// 1D Natural(Hadamard)ordered Fast Hadamard Transform
	// ------------------------------------------------------
	private double[] fhtnat(double data[], boolean forwardTransform) {
		// The function implement the 1D natural(Hadamard)ordered Fast Hadamard Transform
		double x[];
    	int N;
    	int k1;
    	int k2;
    	int k3;
    	int NTest;
    	int log2;
    	int i1;
    	int i2;
    	int i3;
    	int L1;
    	double temp1;
    	double temp2;
    	int i;
    	int j;
    	N = data.length;
    	NTest = N;
    	log2 = 0;
		while ((NTest % 2) == 0) {
	    	NTest = NTest/2;
	    	log2++;
	    }
		x = new double[N];
		for (i = 0; i < N; i++) {
			x[i] = data[i];
		}
		k1=N; k2=1; k3=N/2;
		for (i1=1; i1 <= log2; i1++) {
		    L1=1;
		    for (i2=1; i2 <= k2; i2++) {
		        for (i3=1; i3 <= k3; i3++) {
		            i=i3+L1-1; j=i+k3;
		            temp1= x[i-1]; temp2 = x[j-1]; 
		            x[i-1] = temp1 + temp2;
		            x[j-1] = temp1 - temp2;
		        } // for (i3=1; i3 <= k3; i3++)
		        L1=L1+k1;
		    } // for (i2=1; i2 <= k2; i2++)
		    k1 = k1/2;  k2 = k2*2;  k3 = k3/2;
		} // for (i1=1; i1 <= log2; i1++)
		if (forwardTransform) {
			for (i1 = 0; i1 < N; i1++) {
		        x[i1]= (x[i1]/(double)N);
			}
	    } // if (forwardTransform)
		
		return x;
	}


	// ------------------------------------------------------
	//  Function for bit reversal
	// ------------------------------------------------------
	private double[] bitrevorder(double X[]) {
	    // Rearrange vector X to reverse bit order,upto max 2^k size <= length(X)
		int N;
		double R[];
		int NTest;
		int log2;
		int i;
		int j;
		N = X.length;
		R = new double[N];
		NTest = N;
		log2 = 0;
		int biti;
		int bitReversed = 0;
		while ((NTest % 2) == 0) {
	    	NTest = NTest/2;
	    	log2++;
	    }
		for (j = 0; j < N; j++) {
			bitReversed = 0;
			for (i = 0; i <= log2-1; i++) {
				biti = (j >>> i) & 0x1;
				bitReversed = bitReversed | (biti << log2 - 1 - i);
			}
		    R[j] = X[bitReversed];
		}
		return R;
	}
	
	public void test() {
	    double a[]	= new double[]{1,0,1,0,0,1,1,0};
	    int n = 8;
	    double b[] = fhtseq(a, true);
	    if (b[0] != 0.5) System.err.println("fhtseq b[0] = " + b[0] + " instead of the correct 4");
	    if (b[1] != 0.0) System.err.println("fhtseq b[1] = " + b[1] + " instead of the correct 2");
	    if (b[2] != 0.0) System.err.println("fhtseq b[2] = " + b[2] + " instead of the correct 0");
	    if (b[3] != 0.0) System.err.println("fhtseq b[3] = " + b[3] + " instead of the correct -2");
	    if (b[4] != -0.25) System.err.println("fhtseq b[4] = " + b[4] + " instead of the correct 0");
	    if (b[5] != 0.25) System.err.println("fhtseq b[5] = " + b[5] + " instead of the correct 2");
	    if (b[6] != 0.25) System.err.println("fhtseq b[6] = " + b[6] + " instead of the correct 0");
	    if (b[7] != 0.25) System.err.println("fhtseq b[7] = " + b[7] + " instead of the correct 2");
	    a = fhtseq(b, false);
	    if (a[0] != 1) System.err.println("fhtseq a[0] = " + a[0] + " instead of the correct 1");
	    if (a[1] != 0) System.err.println("fhtseq a[1] = " + a[1] + " instead of the correct 0");
	    if (a[2] != 1) System.err.println("fhtseq a[2] = " + a[2] + " instead of the correct 1");
	    if (a[3] != 0) System.err.println("fthseq a[3] = " + a[3] + " instead of the correct 0");
	    if (a[4] != 0) System.err.println("fhtseq a[4] = " + a[4] + " instead of the correct 0");
	    if (a[5] != 1) System.err.println("fhtseq a[5] = " + a[5] + " instead of the correct 1");
	    if (a[6] != 1) System.err.println("fhtseq a[6] = " + a[6] + " instead of the correct 1");
	    if (a[7] != 0) System.err.println("fhtseq a[7] = " + a[7] + " instead of the correct 0");
	    
	    a= new double[]{1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        n = a.length;
        b = fhtseq(a, true);
        a = fhtseq(b, false);
        int ans[] = {1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        int i;
        for (i = 0; i < n; i++) {
            if (a[i] != ans[i]) System.err.println("fhtseq a["+i+"] = " + a[i] + " instead of the correct " + ans[i]);
        }

        a = new double[]{1,0,1,0,0,1,1,0};
        b = new double[]{1,0,0,0,0,1,1,0};
        n = 8;
        double diff_a = fwht_sum_absolute_difference(n, a, b);
        double diff_b = fwht_sum_absolute_difference(n, b, a);
        if (diff_a != diff_b) System.err.println("fhtseq diff_a = " + diff_a + " diff_b = " + diff_b);
        
        double a2[] = fhtseq(a, true);
        double b2[] = fhtseq(b, true);
        double diff_a2 = fwht_sum_absolute_difference(n, a2, b2);
        double diff_b2 = fwht_sum_absolute_difference(n, b2, a2);
        if (diff_a2 != diff_b2) System.err.println("fhtseq diff_a2 = " + diff_a2 + " diff_b2 = " + diff_b2);
        
        a = fhtseq(a2, false);
        b = fhtseq(b2, false);
        
        double diff_a3 = fwht_sum_absolute_difference(n, a, b);
        double diff_b3 = fwht_sum_absolute_difference(n, b, a);
        
        if (diff_a != diff_a3) System.err.println("fhtseq diff_a = " + diff_a + " diff_a3 = " + diff_a3);
        if (diff_b != diff_b3) System.err.println("fhtseq diff_b = " + diff_b + " diff_b3 = " + diff_b3);
        
        a = new double[]{1,0,1,0,0,1,1,0};
	    b = fhtdya(a, true);
	    if (b[0] != 0.5) System.err.println("fhtdya b[0] = " + b[0] + " instead of the correct 4");
	    if (b[1] != 0.0) System.err.println("fhtdya b[1] = " + b[1] + " instead of the correct 2");
	    if (b[2] != 0.0) System.err.println("fhtdya b[2] = " + b[2] + " instead of the correct 0");
	    if (b[3] != 0.0) System.err.println("fhtdya b[3] = " + b[3] + " instead of the correct -2");
	    if (b[4] != 0.25) System.err.println("fhtdya b[4] = " + b[4] + " instead of the correct 0");
	    if (b[5] != 0.25) System.err.println("fhtdya b[5] = " + b[5] + " instead of the correct 2");
	    if (b[6] != -0.25) System.err.println("fhtdya b[6] = " + b[6] + " instead of the correct 0");
	    if (b[7] != 0.25) System.err.println("fhtdya b[7] = " + b[7] + " instead of the correct 2");
	    a = fhtdya(b, false);
	    if (a[0] != 1) System.err.println("fhtdya a[0] = " + a[0] + " instead of the correct 1");
	    if (a[1] != 0) System.err.println("fhtdya a[1] = " + a[1] + " instead of the correct 0");
	    if (a[2] != 1) System.err.println("fhtdya a[2] = " + a[2] + " instead of the correct 1");
	    if (a[3] != 0) System.err.println("fthdya a[3] = " + a[3] + " instead of the correct 0");
	    if (a[4] != 0) System.err.println("fhtdya a[4] = " + a[4] + " instead of the correct 0");
	    if (a[5] != 1) System.err.println("fhtdya a[5] = " + a[5] + " instead of the correct 1");
	    if (a[6] != 1) System.err.println("fhtdya a[6] = " + a[6] + " instead of the correct 1");
	    if (a[7] != 0) System.err.println("fhtdya a[7] = " + a[7] + " instead of the correct 0");
	    
	    a= new double[]{1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        n = a.length;
        b = fhtdya(a, true);
        a = fhtdya(b, false);
        ans = new int[]{1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        for (i = 0; i < n; i++) {
            if (a[i] != ans[i]) System.err.println("fhtdya a["+i+"] = " + a[i] + " instead of the correct " + ans[i]);
        }

        a = new double[]{1,0,1,0,0,1,1,0};
        b = new double[]{1,0,0,0,0,1,1,0};
        n = 8;
        diff_a = fwht_sum_absolute_difference(n, a, b);
        diff_b = fwht_sum_absolute_difference(n, b, a);
        if (diff_a != diff_b) System.err.println("fhtdya diff_a = " + diff_a + " diff_b = " + diff_b);
        
        a2 = fhtdya(a, true);
        b2 = fhtdya(b, true);
        diff_a2 = fwht_sum_absolute_difference(n, a2, b2);
        diff_b2 = fwht_sum_absolute_difference(n, b2, a2);
        if (diff_a2 != diff_b2) System.err.println("fhtdya diff_a2 = " + diff_a2 + " diff_b2 = " + diff_b2);
        
        a = fhtdya(a2, false);
        b = fhtdya(b2, false);
        
        diff_a3 = fwht_sum_absolute_difference(n, a, b);
        diff_b3 = fwht_sum_absolute_difference(n, b, a);
        
        if (diff_a != diff_a3) System.err.println("fhtdya diff_a = " + diff_a + " diff_a3 = " + diff_a3);
        if (diff_b != diff_b3) System.err.println("fhtdya diff_b = " + diff_b + " diff_b3 = " + diff_b3);
        
        a = new double[]{1,0,1,0,0,1,1,0};
	    b = fhtnat(a, true);
	    if (b[0] != 0.5) System.err.println("fhtnat b[0] = " + b[0] + " instead of the correct 4");
	    if (b[1] != 0.25) System.err.println("fhtnat b[1] = " + b[1] + " instead of the correct 2");
	    if (b[2] != 0) System.err.println("fhtnat b[2] = " + b[2] + " instead of the correct 0");
	    if (b[3] != -0.25) System.err.println("fhtnat b[3] = " + b[3] + " instead of the correct -2");
	    if (b[4] != 0) System.err.println("fhtnat b[4] = " + b[4] + " instead of the correct 0");
	    if (b[5] != 0.25) System.err.println("fhtnat b[5] = " + b[5] + " instead of the correct 2");
	    if (b[6] != 0) System.err.println("fhtnat b[6] = " + b[6] + " instead of the correct 0");
	    if (b[7] != 0.25) System.err.println("fhtnat b[7] = " + b[7] + " instead of the correct 2");
	    a = fhtnat(b, false);
	    if (a[0] != 1) System.err.println("fhtnat a[0] = " + a[0] + " instead of the correct 1");
	    if (a[1] != 0) System.err.println("fhtnat a[1] = " + a[1] + " instead of the correct 0");
	    if (a[2] != 1) System.err.println("fhtnat a[2] = " + a[2] + " instead of the correct 1");
	    if (a[3] != 0) System.err.println("fthnat a[3] = " + a[3] + " instead of the correct 0");
	    if (a[4] != 0) System.err.println("fhtnat a[4] = " + a[4] + " instead of the correct 0");
	    if (a[5] != 1) System.err.println("fhtnat a[5] = " + a[5] + " instead of the correct 1");
	    if (a[6] != 1) System.err.println("fhtnat a[6] = " + a[6] + " instead of the correct 1");
	    if (a[7] != 0) System.err.println("fhtnat a[7] = " + a[7] + " instead of the correct 0");
	    
	    a= new double[]{1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        n = a.length;
        b = fhtnat(a, true);
        a = fhtnat(b, false);
        ans = new int[]{1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0};
        for (i = 0; i < n; i++) {
            if (a[i] != ans[i]) System.err.println("fhtnat a["+i+"] = " + a[i] + " instead of the correct " + ans[i]);
        }

        a = new double[]{1,0,1,0,0,1,1,0};
        b = new double[]{1,0,0,0,0,1,1,0};
        n = 8;
        diff_a = fwht_sum_absolute_difference(n, a, b);
        diff_b = fwht_sum_absolute_difference(n, b, a);
        if (diff_a != diff_b) System.err.println("fhtnat diff_a = " + diff_a + " diff_b = " + diff_b);
        
        a2 = fhtnat(a, true);
        b2 = fhtnat(b, true);
        diff_a2 = fwht_sum_absolute_difference(n, a2, b2);
        diff_b2 = fwht_sum_absolute_difference(n, b2, a2);
        if (diff_a2 != diff_b2) System.err.println("fhtnat diff_a2 = " + diff_a2 + " diff_b2 = " + diff_b2);
        
        a = fhtnat(a2, false);
        b = fhtnat(b2, false);
        
        diff_a3 = fwht_sum_absolute_difference(n, a, b);
        diff_b3 = fwht_sum_absolute_difference(n, b, a);
        
        if (diff_a != diff_a3) System.err.println("fhtnat diff_a = " + diff_a + " diff_a3 = " + diff_a3);
        if (diff_b != diff_b3) System.err.println("fhtnat diff_b = " + diff_b + " diff_b3 = " + diff_b3);
	}
	
	// Finds the sum of differences between two sets of data.
	// The resulting difference is divided by the length.
	public double fwht_sum_absolute_difference(int n, double a[], double b[]) {
		double sum = 0;
		double diff;
		int i;
		for (i = 0; i < n; i++) {
			diff = b[i] - a[i];
			sum = sum + Math.abs(diff);
		}
		return sum/(double)n;
	}

	}