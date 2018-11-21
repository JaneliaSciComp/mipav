package gov.nih.mipav.model.algorithms;


import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;


 /**
  * One dimensional forward and inverse slant transforms are ported from  FORATRAN programs in Appendix A
  * of AD-767 758 Technical Report Slant Transform Image Coding by Wen-Hsiung Cheng of the University of 
  * Southern California, prepared for the Advanced Research Projects Agency, Air Force Eastern Test Range, May 1973.
  * 
  * @author ilb
  *
  */

public class SlantTransform extends AlgorithmBase {

    // ~ Static fields/initializers
    // -----------------------

   private static final int TRANSFORM_COLUMNS = 1;
   private static final int TRANSFORM_ROWS = 2;
    
    private ModelImage transformImage;
    private ModelImage inverseImage;
    private int transform;
    
    public SlantTransform() {
		
	}
    
    public SlantTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg, int transform) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
		this.transform = transform;
	}
    
    public void forward1D(int N, double C[][]) {
    	// This program performs a one dimensional slant transform
    	// of a N by N image, where N is a power of 2
    	double A[] = new double[N];
    	double B[] = new double[N];
    	double ENN;
    	double B1;
    	double A1;
    	int M;
    	int K1;
    	int II;
    	int IIA;
    	int I;
    	int J;
    	int K;
    	int L;
    	int KK;
    	int K2;
    	int LL;
    	double SUM;
    	double SUM1;
    	double SUM2;
    	double A2;
    	double B2;
    	int IIB;
    	int L2;
    	double T;
    	int L3;
    	int I1;
    	int L1;
    	int I2;
    	double DD;
    	double E2;
    	double F2;
    	
    	ENN = Math.sqrt((double)N);
    	B1 = 1.0/Math.sqrt(5.0);
    	A1 = 3.0 * B1;
    	for (M = 1; M <= N; M++) {
    		K1 = N/4;
    		for (II = 1; II <= K1; II++) {
    		   IIA = 4 * (II - 1);
    		   for (I = 1; I <= 4; I++) {
    			   J = IIA + I;
    			   B[I-1] = C[M-1][J-1];
    		   } // for (I = 1; I <= 4; I++)
    		   
    		   A[0] = B[0] + B[3];
    		   A[1] = B[1] + B[2];
    		   A[2] = B[0] - B[3];
    		   A[3] = B[1] - B[2];
    		   
    		   B[0] = A[0] + A[1];
    		   B[1] = A1*A[2] + B1*A[3];
    		   B[2] = A[0] - A[1];
    		   B[3] = B1*A[2] - A1*A[3];
    		   
    		   for (I = 1; I <= 4; I++) {
    			   J = IIA + I;
    			   C[M-1][J-1] = B[I-1];
    		   } // for (I = 1; I <= 4; I++)
    		} // for (II = 1; II <= K1; II++)
    		K = 8;
    		L = 3;
    		while (K <= N) {
	    		KK = K/2;
	    		K2 = N/K;
	    		LL = L-1;
	    		SUM1 = 0.0;
	    		for (I = 1; I <= LL; I++) {
	    		    SUM1 = SUM1 + Math.pow(2,2*(I-1));	
	    		} // for (I = 1; I <= LL; I++) 
	    		SUM2 = SUM1 + Math.pow(2, 2*LL);
	    		A2 = (double)KK/Math.sqrt(SUM2);
	    		B2 = Math.sqrt(SUM1)/Math.sqrt(SUM2);
	    		for (II = 1; II <= K2; II++) {
	    		    IIB = K*(II-1);
	    		    for (I = 1; I <= K; I++) {
	    		    	J = IIB + I;
	    		    	B[I-1] = C[M-1][J-1];
	    		    } // for (I = 1; I <= K; I++)
	    		    for (I = 1; I <= KK; I++) {
	    		        for (L2 = 1; L2 <= 2; L2++) {
	    		            T = 0.0;
	    		            for (L3 = 1; L3 <= 2; L3++) {
	    		                I1 = KK*(L3-1) + I;
	    		                if ((L3 + L2 - 4) == 0) {
	    		                	T = T - B[I1-1];
	    		                }
	    		                else {
	    		                	T = T + B[I1-1];
	    		                }
	    		            } // for (L3 = 1; L3 <= 2; L3++)
	    		            if ((I & 0x1) == 1) {
	    		            	L1 = 2*(I-1);
	    		            	I2 = L1 + L2;
	    		            } // if ((I & 0x1) == 1) 
	    		            else {
	    		            	L1 = 2*I + 1;
	    		            	I2 = L1 - L2;
	    		            }
	    		            A[I2-1] = T;
	    		        } // for (L2 = 1; L2 <= 2; L2++)
	    		    } // for (I = 1; I <= KK; I++) 
	    		    
	    		    DD = A[3];
	    		    A[3] = A[2];
	    		    A[2] = A[1];
	    		    A[1] = DD;
	    		    
	    		    E2 = B2*A[1] + A2*A[2];
	    		    F2 = A2*A[1] - B2*A[2];
	    		    A[1] = E2;
	    		    A[2] = A[3];
	    		    A[3] = F2;
	    		    
	    		    for (I = 1; I <= K; I++) {
	    		        J = IIB + I;
	    		        C[M-1][J-1] = A[I-1];
	    		    } // for (I = 1; I <= K; I++)
	    		} // for (II = 1; II <= K2; II++)
    		
	    		K = K * 2;
	    		L++;
    		} // while (K <= N)
    		for (I = 1; I <= N; I++) {
    		    C[M-1][I-1] = C[M-1][I-1]/ENN;	
    		} // for (I = 1; I <= N; I++)
    	} // for (M = 1; M <= N; M++)
    	return;
    }

   public void inverse1D(int N, double C[][]) {
       // This program performs a one dimensional inverse slant transform
   	   // of a N by N image, where N is a power of 2
	   double A[] = new double[N];
	   double B[] = new double[N];
	   double S;
	   double B1;
	   double A1;
	   int M;
	   int K;
	   int L;
	   int KK;
	   int K2;
	   int LL;
	   double SUM1;
	   int I;
	   double SUM2;
	   double A2;
	   double B2;
	   int II;
	   int IIQ;
	   int J;
	   double E2;
	   double F2;
	   int JK;
	   int JKK;
	   int IJ;
	   int L2;
	   int I2;
	   double T;
	   int L3;
	   int I1;
	   int JJK;
	   int IC;
	   int K1;
	   int KTEST;
	   
	   S = Math.sqrt((double)N);
	   B1 = 1.0/Math.sqrt(5.0);
	   A1 = 3.0 * B1;
	   for (M = 1; M <= N; M++) {
	       K = N;
	       L = 0;
	       KTEST = K;
	       while ((KTEST % 2) == 0) {
	    	   KTEST = KTEST/2;
	    	   L++;
	       }
	       while (K >= 8) {
		       KK = K/2;
		       K2 = N/K;
		       LL = L-1;
		       SUM1 = 0.0;
		       for (I = 1; I <= LL; I++) {
		    	   SUM1 = SUM1 + Math.pow(2,2*(I-1));   
		       } // for (I = 1; I <= LL; I++)
		       SUM2 = SUM1 + Math.pow(2,2*LL);
		       A2 = (double)KK/Math.sqrt(SUM2);
		       B2 = Math.sqrt(SUM1)/Math.sqrt(SUM2);
		       for (II = 1; II <= K2; II++) {
		    	   IIQ = K*(II-1);
		    	   for (I = 1; I <= K; I++) {
		    	       J = I + IIQ;
		    	       B[I-1] = C[M-1][J-1];
		    	   } // for (I = 1; I <= K; I++)
		    	   
		    	   E2 = B2*B[1] + A2*B[3];
		    	   F2 = A2*B[1] - B2*B[3];
		    	   B[1] = E2;
		    	   B[3] = B[2];
		    	   B[2] = F2;
		    	   JK = KK + 1;
		    	   JKK = JK + 1;
		    	   A[0] = B[0] + B[2];
		    	   A[1] = B[1] + B[3];
		    	   A[JK-1] = B[0] - B[2];
		    	   A[JKK-1] = B[1] - B[3];
		    	   for (I = 3; I <= KK; I++) {
		    		   IJ = 2*I - 1;
		    		   for (L2 = 1; L2 <= 2; L2++) {
		    		       T = 0.0;
		    		       for (L3 = 1; L3 <= 2; L3++) {
		    		    	   I1 = L3 - 1 + IJ;
		    		    	   if ((L2 + L3 - 4) == 0) {
		    		    		   T = T - B[I1-1];
		    		    	   }
		    		    	   else {
		    		    		   T = T + B[I1-1];
		    		    	   }
		    		       } // for (L3 = 1; L3 <= 2; L3++)
		    		       I2 = KK*(L2-1) + I;
		    		       A[I2-1] = T;
		    		   } // for (L2 = 1; L2 <= 2; L2++)
		    	   } // for (I = 3; I <= KK; I++)
		    	   
		    	   JJK = KK + 4;
		    	   for (IC = JJK; IC <= K; IC += 2) {
		    		   A[IC-1] = -A[IC-1];   
		    	   } // for (IC = JJK; IC <= K; IC += 2)
		    	   
		    	   for (I = 1; I <= K; I++) {
		    		   J = I + IIQ;
		    		   C[M-1][J-1] = A[I-1];
		    	   } // for (I = 1; I <= K; I++)
		       } // for (II = 1; II <= K2; II++) 
	           K = K/2;
	           L--;
	       } // while (K >= 8)
	       K1 = N/4;
	       for (II = 1; II <= K1; II++) {
	    	   IIQ = 4*(II-1);
	    	   for (I = 1; I <= 4; I++) {
	    		   J = I + IIQ;
	    		   B[I-1] = C[M-1][J-1];
	    	   } // for (I = 1; I <= 4; I++)
	    	   A[0] = B[0] + B[2];
	    	   A[1] = B[0] - B[2];
	    	   A[2] = A1 * B[1] + B1 * B[3];
	    	   A[3] = B1 * B[1] - A1 * B[3];
	    	   B[0] = A[0] + A[2];
	    	   B[1] = A[1] + A[3];
	    	   B[2] = A[1] - A[3];
	    	   B[3] = A[0] - A[2];
	    	   for (I = 1; I <= 4; I++) {
	    		   J = I + IIQ;
	    		   C[M-1][J-1] = B[I-1]/S;
	    	   } // for (I = 1; I <= 4; I++)
	       } // for (II = 1; II <= K1; II++)
	   } // for (M = 1; M <= N; M++)
	   return;
   }
   
    public void runAlgorithm() {
		int xDim;
		int yDim;
		int zDim;
		int length;
		double doubleBuffer[];
		int xTest;
		int z;
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
        
        if (xDim != yDim) {
        	MipavUtil.displayError("Must have X dimension = Y Dimension");
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
        dst = new double[yDim][xDim];
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, doubleBuffer); // locks and releases lock
            } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Slant Transform: Image(s) locked", true);

                return;
            }
        	if (transform == TRANSFORM_ROWS) {
	        	for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			dst[y][x] = doubleBuffer[x + y * xDim];
	        		}
	        	}
        	}
        	else {
        		for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			dst[x][y] = doubleBuffer[x + y * xDim];
	        		}
	        	}
        	}
        	forward1D(xDim, dst);
        	if (transform == TRANSFORM_ROWS) {
	        	for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			doubleBuffer[x + y * xDim] = dst[y][x];
	        		}
	        	}
        	}
        	else {
        		for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			doubleBuffer[x + y * xDim] = dst[x][y];
	        		}
	        	}	
        	}
        	try {
                transformImage.importData(z*length, doubleBuffer, false);
            } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Slant Transform: Image(s) locked", true);

                return;
            }
        	inverse1D(xDim, dst);
        	if (transform == TRANSFORM_ROWS) {
	        	for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			doubleBuffer[x + y * xDim] = dst[y][x];
	        		}
	        	}
        	}
        	else {
        		for (y = 0; y < yDim; y++) {
	        		for (x = 0; x < xDim; x++) {
	        			doubleBuffer[x + y * xDim] = dst[x][y];
	        		}
	        	}	
        	}
        	try {
                inverseImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Slant Transform: Image(s) locked", true);

                return;
             }
        }
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}

}