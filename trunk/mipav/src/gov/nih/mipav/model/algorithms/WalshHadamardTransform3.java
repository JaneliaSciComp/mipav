package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

public class WalshHadamardTransform3 extends AlgorithmBase {
	
	ModelImage transformImage;
	ModelImage inverseImage;
	
	public WalshHadamardTransform3() {
		
	}
	
	public WalshHadamardTransform3(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg) {
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
        }
	}
	
	//// =======================================================
	// FUNCTIONS FOR 1-D SEQUENCY(WALSH),DYADIC(PALEY) AND 
    // NATURAL(HADAMARD)ORDERED FAST WALSH-HADAMARD TRANSFORM
	// =======================================================

	// -------------------------------------------------------
	// 1D sequency(Walsh)ordered Fast Walsh-Hadamard Transform
	// -------------------------------------------------------
	public int[] fhtseq(int data[], boolean forwardTransform) {
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
		int x[];
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
		int temp1;
		int temp2;
	
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
		        } // 
		        L1=L1+k1;
		    } // for (i2=1; i2 <= k2; i2++)
		    k1 = k1/2;  k2 = k2*2;  k3 = k3/2;
		} // for (i1=1; i1 <= log2; i1++)
		if (forwardTransform) {
			for (i1 = 0; i1 < N; i1++) {
		        x[i1]= (int)Math.round(x[i1]/(double)N);
			}
		} // if (forwardTransform)
		return x;
	}


	// ------------------------------------------------------
	// 1D Dyadic(Paley)ordered Fast Hadamard Transform
	// ------------------------------------------------------
    public int[] fhtdya(int data[], boolean forwardTransform) {
		// The function implement the 1D dyadic (Paley) ordered fast Hadamard transform,
    	int x[];
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
    	int temp1;
    	int temp2;
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
		        x[i1]= (int)Math.round(x[i1]/(double)N);
			}
		} // if (forwardTransform)
		return x;
    }

	// ------------------------------------------------------
	// 1D Natural(Hadamard)ordered Fast Hadamard Transform
	// ------------------------------------------------------
	private int[] fhtnat(int data[], boolean forwardTransform) {
		// The function implement the 1D natural(Hadamard)ordered Fast Hadamard Transform
		int x[];
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
    	int temp1;
    	int temp2;
    	int i;
    	int j;
    	N = data.length;
    	NTest = N;
    	log2 = 0;
		while ((NTest % 2) == 0) {
	    	NTest = NTest/2;
	    	log2++;
	    }
		x = new int[N];
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
		        x[i1]= (int)Math.round(x[i1]/(double)N);
			}
	    } // if (forwardTransform)
		return x;
	}


	// ------------------------------------------------------
	//  Function for bit reversal
	// ------------------------------------------------------
	private int[] bitrevorder(int X[]) {
	    // Rearrange vector X to reverse bit order,upto max 2^k size <= length(X)
		int N;
		int R[];
		int NTest;
		int log2;
		int i;
		int j;
		int si;
		int sip1;
		int gi;
		N = X.length;
		R = new int[N];
		NTest = N;
		log2 = 0;
		int gray = 0;
		int bitReversedGray;
		while ((NTest % 2) == 0) {
	    	NTest = NTest/2;
	    	log2++;
	    }
		for (j = 0; j < N; j++) {
		    for (i = 0; i <= log2-2; i++) {
		    	gray = 0;
		        si = (X[j] >>> i) & 0x1;;
		        sip1 = (X[j] >>> (i+1)) & 0x1;
		        if (((si == 1) && (sip1 == 0)) || ((si == 0) && (sip1 == 1))) {
		        	gray = gray | (1 << i);
		        }
		    }
		    gi = (X[j] >>> (log2-1)) & 0x1;
		    gray = gray | (gi << (log2-1));
		    bitReversedGray = 0;
		    for (i = 0; i <= log2-1; i++) {
		    	gi = (gray >>> i) & 0x1;
		    	bitReversedGray = bitReversedGray | (gi << log2- 1 - i);
		    }
		    R[j] = X[bitReversedGray];
		}
		return R;
	}

	}