package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.MipavUtil;

public class DoublyConnectedSC extends AlgorithmBase {
	// This is a port of the FORTRAN ACM TOMS Algorithm 785, collected
	// algorithms form ACM.  The original ACM work was published in Transactions
	// on Mathematical Software, Vol. 24, No. 3, September, 1998, pp. 317-333.
	// The original FORTRAN code was written by Dr. Chenglie Hu as described in
	// his article "A Software Package for Computing Schwarz-Christoffel Conformal
	// Transformation for Doubly Connected Polyhgonal Regions."
	
	// geometries of the polygon region in the 7 test routines
	private final int SQUARE_SYMMETRIC_REGION = 1;
	private final int MILDLY_CROWDED_INFINITE_REGION = 2;
	private final int HEAVILY_CROWDED_REGION = 3;
	private final int CHINESE_CHARACTER_STRUCTURED_REGION = 4;
	private final int FOUR_DIRECTION_INFINITE_REGION = 5;
	private final int EXAMPLE_GIVEN_FOR_CHECKING_INVERSE_MAP = 6;
	private final int UPPER_HALF_PLANE_WITH_HORIZONTAL_SLIT = 7;
	
	// Below are the 4 variables in the common block PARAM4
	private double DLAM;
	private int IU;
	private double UARY[] = new double[8];
	private double VARY[] = new double[3];
	
	private SchwarzChristoffelMapping scm = new SchwarzChristoffelMapping();
	
	// Specifies the geometry of the polygon region for 7 test examples
	private int IPOLY;
	// The number of Gauss-Jacobi points
	// Recommended values for NPTQ are 2-8.
	private int NPTQ;
	private boolean testRoutine = false;
	private double MACHEP = 2.2204460E-16;
	
	public DoublyConnectedSC() {
		
	}
	
	public DoublyConnectedSC(int IPOLY, int NPTQ) {
		this.IPOLY = IPOLY;
		this.NPTQ = NPTQ;
		testRoutine = true;
	}
	
	public void runAlgorithm() {
		int M[] = new int[1];
		int N[] = new int[1];
		double Z0[][] = new double[30][2];
		double Z1[][] = new double[30][2];
		double ALFA0[] = new double[30];
		double ALFA1[] = new double[30];
		double QWORK[] = new double[1660];
		int ISHAPE;
		
double neweps;
		
		// MACHEP is a machine dependent parameter specifying the relative precision of
		// floating point arithmetic.
		 // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        MACHEP = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                MACHEP = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
		
		if (testRoutine) {
			DSCDATA(IPOLY, M, N, Z0, Z1, ALFA0, ALFA1);
			ANGLES(N[0], Z1, ALFA1, 1);
			if ((IPOLY == 1) || (IPOLY == 3) || (IPOLY == 4) || (IPOLY == 6)) {
				ANGLES(M[0], Z0, ALFA0, 0);
			}
			
			// Generate the Gauss-Jacobi weights & nodes and check the input
			QINIT(M[0],N[0],ALFA0, ALFA1, NPTQ, QWORK);
			ISHAPE = 0;
			if ((IPOLY == 2) || (IPOLY == 5) || (IPOLY == 7)) {
				ISHAPE = 1;
			}
			CHECK(ALFA0, ALFA1, M[0], N[0], ISHAPE);
		} // if (testRoutine)
		
		
	}
	
	private void DSCDATA(int IPOLY, int M[], int N[], double Z0[][], double Z1[][], double ALFA0[], double ALFA1[]) {
		// DSCDATA generates data.
		double Q;
		double S;
		if (IPOLY == 1) {
			M[0] = 4;
			N[0] = 4;
			Q = Math.sqrt(2.0);
			Z0[0][0] = 1.0 + Q;
			Z0[0][1] = 1.0 + Q;
			Z0[1][0] = -1.0 - Q;
			Z0[1][1] = 1.0 + Q;
			Z0[2][0] = -1.0 - Q;
			Z0[2][1] = -1.0 - Q;
			Z0[3][0] = 1.0 + Q;
			Z0[3][1] = -1.0 - Q;
			Z1[0][0] = Q;
			Z1[0][1] = 0.0;
			Z1[1][0] = 0.0;
			Z1[1][1] = Q;
			Z1[2][0] = -Q;
			Z1[2][1] = 0.0;
			Z1[3][0] = 0.0;
			Z1[3][1] = -Q;
		} // if (IPOLY == 1)
		else if (IPOLY == 2) {
			M[0] = 12;
			N[0] = 6;
			Z0[0][0] = 0.6875;
			Z0[0][1] = 0.875;
			Z0[1][0] = 0.0;
			Z0[1][1] = 0.0;
			Z0[2][0] = 1.0;
			Z0[2][1] = 0.0;
			Z0[3][0] = 0.875;
			Z0[3][1] = 0.875;
			Z0[4][0] = 1.125;
			Z0[4][1] = 1.375;
			Z0[5][0] = 2.0;
			Z0[5][1] = 1.375;
			Z0[6][0] = 1.25;
			Z0[6][1] = 2.0;
			Z0[7][0] = 2.25;
			Z0[7][1] = 2.0;
			Z0[8][0] = 2.375;
			Z0[8][1] = 2.75;
			Z0[9][0] = 1.625;
			Z0[9][1] = 2.25;
			Z0[10][0] = 1.125;
		    Z0[10][1] = 2.625;
		    Z0[11][0] = -0.5;
		    Z0[11][1] = 2.75;
		    Z1[0][0] = 0.375;
		    Z1[0][1] = 1.875;
		    Z1[1][0] = 0.5;
		    Z1[1][1] = 2.0;
		    Z1[2][0] = 1.0;
		    Z1[2][1] = 1.5;
		    Z1[3][0] = 0.5;
		    Z1[3][1] = 2.1875;
		    Z1[4][0] = 0.5;
		    Z1[4][1] = 2.5;
		    Z1[5][0] = Z1[3][0];
		    Z1[5][1] = Z1[3][1];
		    ALFA0[0] = 1.39169261159339475;
            ALFA0[1] = 0.28801540784794967;
            ALFA0[2] = 0.454832764699133488;
            ALFA0[3] = 1.19275085295129979;
            ALFA0[4] = 1.35241638234956651;
            ALFA0[5] = 0.0;
            ALFA0[6] = 2.0;
            ALFA0[7] = 0.552568456711253445;
            ALFA0[8] = 0.260264501477747753;
            ALFA0[9] = 1.39199980651013222;
            ALFA0[10] = 0.819604487273064009;
            ALFA0[11] = 0.295854728586457991;
		} // else if (IPOLY == 2)
		else if (IPOLY == 3) {
			M[0] = 11;
			N[0] = 6;
			Z0[0][0] = 0.5;
			Z0[0][1] = 2.5;
			Z0[1][0] = 0.5;
			Z0[1][1] = 0.5;
			Z0[2][0] = 1.0;
			Z0[2][1] = 0.5;
			Z0[3][0] = 1.0;
			Z0[3][1] = 1.0;
			Z0[4][0] = 1.0;
			Z0[4][1] = 0.5;
			Z0[5][0] = 0.5;
			Z0[5][1] = 0.5;
			Z0[6][0] = 0.5;
			Z0[6][1] = 2.5;
			Z0[7][0] = 0.0;
			Z0[7][1] = 2.5;
			Z0[8][0] = 0.0;
			Z0[8][1] = 0.0;
			Z0[9][0] = 2.0;
			Z0[9][1] = 0.0;
			Z0[10][0] = 2.0;
			Z0[10][1] = 2.5;
			Z1[0][0] = 1.0;
			Z1[0][1] = 2.0;
			Z1[1][0] = 1.0;
			Z1[1][1] = 1.5;
			Z1[2][0] = 1.0;
			Z1[2][1] = 2.0;
			Z1[3][0] = 1.5;
			Z1[3][1] = 2.0;
			Z1[4][0] = 1.5;
			Z1[4][1] = 0.5;
			Z1[5][0] = 1.5;
			Z1[5][0] = 2.0;
			ALFA0[0] = 1.0/2.0;
            ALFA0[1] = 1.0/2.0;
            ALFA0[2] = 1.0/2.0;
            ALFA0[3] = 2.0;
            ALFA0[4] = 3.0/2.0;
            ALFA0[5] = 3.0/2.0;
            ALFA0[6] = 1.0/2.0;
            ALFA0[7] = 1.0/2.0;
            ALFA0[8] = 1.0/2.0;
            ALFA0[9] = 1.0/2.0;
            ALFA0[10] = 1.0/2.0;
            ALFA1[0] = 3.0/2.0;
            ALFA1[1] = 2.0;
            ALFA1[2] = 1.0/2.0;
            ALFA1[3] = 1.0/2.0;
            ALFA1[4] = 2.0;
            ALFA1[5] = 3.0/2.0;
		} // else if (IPOLY == 3)
		else if (IPOLY == 4) {
		    M[0] = 4;
		    N[0] = 17;
		    Z0[0][0] = -1.0;
		    Z0[0][1] = -1.0;
		    Z0[1][0] = 1.0;
		    Z0[1][1] = -1.0;
		    Z0[2][0] = 1.0;
		    Z0[2][1] = 1.0;
		    Z0[3][0] = -1.0;
		    Z0[3][1] = 1.0;
		    Z1[0][0] = 0.0;
		    Z1[0][1] = 0.5;
		    Z1[1][0] = 0.0;
		    Z1[1][1] = 0.0;
		    Z1[2][0] = -0.5;
		    Z1[2][1] = 0.0;
		    Z1[3][0] = Z1[1][0];
		    Z1[3][1] = Z1[1][1];
		    Z1[4][0] = 0.0;
		    Z1[4][1] = -0.5;
		    Z1[5][0] = -0.5;
		    Z1[5][1] = -0.5;
		    Z1[6][0] = 0.5;
		    Z1[6][1] = -0.5;
		    Z1[7][0] = 0.25;
		    Z1[7][1] = -0.5;
		    Z1[8][0] = 0.25;
		    Z1[8][1] = -0.25;
		    Z1[9][0] = Z1[7][0];
		    Z1[9][1] = Z1[7][1];
		    Z1[10][0] = Z1[4][0];
		    Z1[10][1] = Z1[4][1];
		    Z1[11][0] = Z1[1][0];
		    Z1[11][1] = Z1[1][1];
		    Z1[12][0] = 0.5;
		    Z1[12][1] = 0.0;
		    Z1[13][0] = Z1[1][0];
		    Z1[13][1] = Z1[1][1];
		    Z1[14][0] = Z1[0][0];
		    Z1[14][1] = Z1[0][1];
		    Z1[15][0] = 0.5;
		    Z1[15][1] = 0.5;
		    Z1[16][0] = -0.5;
		    Z1[16][1] = 0.5;
		} // else if (IPOLY == 4)
		else if (IPOLY == 5) {
			M[0] = 12;
			N[0] = 4;
			S = 1.0/6.0;
			Z0[0][0] = 0.0;
			Z0[0][1] = -8.0*S;
			Z0[1][0] = 0.0;
			Z0[1][1] = 0.0;
			Z0[2][0] = 1.0;
			Z0[2][1] = -1.5;
			Z0[3][0] = 1.0;
			Z0[3][1] = -0.5;
			Z0[4][0] = 0.0;
			Z0[4][1] = 0.0;
			Z0[5][0] = 0.0;
			Z0[5][1] = 2.0*S;
			Z0[6][0] = 0.0;
			Z0[6][1] = 0.0;
			Z0[7][0] = -1.0;
			Z0[7][1] = 0.0;
			Z0[8][0] = 0.0;
			Z0[8][1] = 0.0;
			Z0[9][0] = -7.0*S;
			Z0[9][1] = -1.0;
			Z0[10][0] = 0.0;
			Z0[10][1] = 0.0;
			Z0[11][0] = -2.0*S;
			Z0[11][1] = -10.0*S;
			ALFA0[0] = 1.75;
            ALFA0[1] = 0.0;
            ALFA0[2] = 1.0;
            ALFA0[3] = 1.0;
            ALFA0[4] = (Math.PI-3.5)/Math.PI;
            ALFA0[5] = 3.5/Math.PI;
            ALFA0[6] = 1.5;
            ALFA0[7] = 1.0;
            ALFA0[8] = 0.0;
            ALFA0[9] = 1.75;
            ALFA0[10] = 0.0;
            ALFA0[11] = 1.0;
            Z1[0][0] = 2.0*S;
            Z1[0][1] = -0.5;
            Z1[1][0] = -S;
            Z1[1][1] = -2.0*S;
            Z1[2][0] = -4.0*S;
            Z1[2][1] = -5.0*S;
            Z1[3][0] = 0.0;
            Z1[3][1] = -1.0;
		} // else if (IPOLY == 5)
		else if (IPOLY == 6) {
		    M[0] = 7;
		    N[0] = 2;
		    Z0[0][0] = -2.0;
		    Z0[0][1] = -1.0;
		    Z0[1][0] = 2.0;
		    Z0[1][1] = -1.0;
		    Z0[2][0] = 2.0;
		    Z0[2][1] = 2.0;
		    Z0[3][0] = -0.8;
		    Z0[3][1] = 2.0;
		    Z0[4][0] = 1.0;
		    Z0[4][1] = 0.5;
		    Z0[5][0] = -1.0;
		    Z0[5][1] = 2.0;
		    Z0[6][0] = -2.0;
		    Z0[6][1] = 2.0;
		    Z1[0][0] = 0.0;
		    Z1[0][1] = 0.0;
		    Z1[1][0] = -1.0;
		    Z1[1][1] = 0.0;
		} // else if (IPOLY == 6)
		else {
			M[0] = 3;
			N[0] = 2;
			Z0[0][0] = 1.01;
			Z0[0][1] = 0.0;
			Z0[1][0] = 100.0;
			Z0[1][1] = 100.0;
			Z0[2][0] = -1.01;
			Z0[2][1] = 0.0;
			Z1[0][0] = 0.0;
			Z1[0][1] = 2.0;
			Z1[1][0] = 0.0;
			Z1[1][1] = 1.0;
			ALFA0[0] = 1.0;
			ALFA0[1] = -1.0;
			ALFA0[2] = 1.0;
		} // else		
	}
	
	private void ANGLES(int MN, double Z01[][], double ALFA01[], int I01) {
	    // Computes the interior angles of a doubly
		// connected and bounded polygonal region.
		// I01 := 0(outer polygon) := 1(inner polygon)
		// MN: The number of vertices
		// Z01: Array containing vertices
		// ALFA01: Array containing interior turning angles on return
		
		int K, KL, KR;
		double cr[] = new double[1];
		double ci[] = new double[1];
		
		for (K = 1; K <= MN; K++) {
			KL = (K+MN-2)%MN + 1;
			KR = K%MN + 1;
			scm.zdiv(Z01[KL-1][0] - Z01[K-1][0], Z01[KL-1][1] - Z01[K-1][1], 
					 Z01[KR-1][0] - Z01[K-1][0], Z01[KR-1][1] - Z01[K-1][1], cr, ci);
			ALFA01[K-1] = Math.atan2(ci[0], cr[0])/Math.PI;
			if (ALFA01[K-1] <= 0.0) {
				ALFA01[K-1] = ALFA01[K-1] + 2.0;
			}
		} // for (K = 1; K <= MN; K++)
		if (I01 == 1) {
			for (K = 1; K <= MN; K++) {
			    if (ALFA01[K-1] != 2.0) {
			    	ALFA01[K-1] = 2.0 - ALFA01[K-1];
			    }
			} // for (K = 1; K <= MN; K++)
		} // if (I01 == 1)
		return;
	}
	
	private void CHECK(double ALFA0[], double ALFA1[], int M, int N, int ISHAPE) {
		//   CHECKS IF THE INPUT DATA AND PARAMETERS ARE CORRECT.
		//   NOTE1:    ANGLE-CHECKING MAKES SENSE ONLY IF ANGLES
		//             ARE USER-PROVIDED.
		//   NOTE2:    USERS ARE RESPONSIBLE FOR CHECKING THE FOLLOWING:
		//   1. EACH COMPONENT OF THE OUTER POLYGON CONTAINS AT LEAST ONE
		//      FINITE VERTEX.
		//   2. Z1(N) IS THE CLOSEST (OR ALMOST CLOSEST)
		//      INNER VERTEX TO Z1(M).
		//   3. COUNTERCLOCKWISE ORIENTATION OF THE VERTICES.
		
		// ALFA0(M), ALFA1(N)
		
		if (!((M >= 3) && (M <= 30) && (N <= 30) && (M+N <= 40))) {
			System.err.println("M must be no less than 3");
			System.err.println("M, N must be no greater than 30");
			System.err.println("M+N must be no greater than 40");
			System.exit(-1);
		}
	
	}
	
	private void QINIT(int M, int N, double ALFA0[], double ALFA1[], int NPTQ, double QWORK[]) {
	    // Computes the Gauss-Jacobi nodes & weights for Gauss-Jacobi quadrature.  Work array
		// QWORK must be dimensioned at least NPTQ*(2(M+N) + 3).  It is divided up into
		// 2(M+N)+3 vectors of length NPTQ:  The first M+N+1 contain quadrature nodes on output,
		// the next M+N+1 contain the corresponding weights on output, and the last one is a scratch
		// vector used by GAUSSJ.  NPTQ is the number of G-J nodes (same as weights) used.  See
		// comment on routine WPROD for the rest of the calling sequence.
		
		// For each finite vertex, compute nodes & weights
		// for one-sided Gauss-Jacobi quadrature:
		
		// ALFA0(M), ALFA1(N), QWORK(1660)
		double ALPHA;
		int INODES, ISCR, IWTS, J, K;
		int i;
		// B is used for input scratch array, T  for nodes output and W for weights output
		double B[] = new double[NPTQ];
		double T[] = new double[NPTQ];
		double W[] = new double[NPTQ];
		
		ISCR = NPTQ * (2 * (M+N) + 2) + 1;
		for (K = 1; K <= M + N; K++) {
		    INODES = NPTQ * (K-1) + 1;
		    IWTS = NPTQ * (M+N+K) + 1;
		    if (K <= M) {
		    	ALPHA = ALFA0[K-1] - 1.0;
		    	if (ALFA0[K-1] > 0.0) {
		    	    GAUSSJ(NPTQ, 0.0, ALPHA, B, T, W);
		    	    for (i = 0; i < NPTQ; i++) {
		    	    	QWORK[INODES-1+i] = T[i];
		    	    	QWORK[IWTS-1+i] = W[i];
		    	    }
		    	} // if (ALFA0[K-1] > 0.0)
		    	else {
		    		for (J = 0; J < NPTQ; J++) {
		    			QWORK[IWTS+J-1] = 0.0;
		    			QWORK[INODES+J-1] = 0.0;
		    		}
		    	} // else
		    } // if (K <= M)
		    else {
		    	ALPHA = ALFA1[K-M-1] - 1.0;
		    	GAUSSJ(NPTQ, 0.0, ALPHA, B, T, W);
	    	    for (i = 0; i < NPTQ; i++) {
	    	    	QWORK[INODES-1+i] = T[i];
	    	    	QWORK[IWTS-1+i] = W[i];
	    	    }
		    } // else
		    
		    // Take singularities into account in advance for the purpose of saving
		    // certain amount of calculation in WQSUM:
		    for (J = 0; J < NPTQ; J++) {
		    	QWORK[IWTS+J-1] = QWORK[IWTS+J-1] * Math.pow((1.0 + QWORK[INODES+J-1]), (-ALPHA));
		    }
		} // for (K = 1; K <= M + N; K++)
		
		// Compute weights and nodes for pure Gaussian quadrature:
		INODES = NPTQ *(M+N) + 1;
		IWTS = NPTQ * (2* (M+N) + 1) + 1;
		GAUSSJ(NPTQ, 0.0, 0.0, B, T, W);
		for (i = 0; i < NPTQ; i++) {
	    	QWORK[INODES-1+i] = T[i];
	    	QWORK[IWTS-1+i] = W[i];
	    }
		return;
	}
	
	private void GAUSSJ(int N, double ALPHA, double BETA, double B[], double T[], double W[]) {
	    // B(N), T(N), W(N)
	    //        THIS ROUTINE COMPUTES THE NODES T(J) AND WEIGHTS
		//        W(J) FOR GAUSS-JACOBI QUADRATURE FORMULAS.
		//        THESE ARE USED WHEN ONE WISHES TO APPROXIMATE
		//
		//                 INTEGRAL (FROM A TO B)  F(X) W(X) DX
		//
		//                              N
		//        BY                   SUM W  F(T )
		//                             J=1  J    J
		//
		//        (W(X) AND W(J) HAVE NO CONNECTION WITH EACH OTHER)
		//        WHERE W(X) IS THE WEIGHT FUNCTION
		//
		//                   W(X) = (1-X)**ALPHA * (1+X)**BETA
		//
		//        ON (-1, 1), ALPHA, BETA .GT. -1.
		//
		//     INPUT:
		//
		//        N        THE NUMBER OF POINTS USED FOR THE QUADRATURE RULE
		//        ALPHA    SEE ABOVE
		//        BETA     SEE ABOVE
		//        B        REAL SCRATCH ARRAY OF LENGTH N
		//
		//     OUTPUT PARAMETERS (BOTH DOUBLE PRECISION ARRAYS OF LENGTH N)
		//
		//        T        WILL CONTAIN THE DESIRED NODES.
		//        W        WILL CONTAIN THE DESIRED WEIGHTS W(J).
		//
		//     SUBROUTINES REQUIRED: CLASS, IMTQL2
		//
		//     REFERENCE:
		//
		//        THE ROUTINE HAS BEEN ADAPTED FROM THE MORE GENERAL
		//        ROUTINE GAUSSQ BY GOLUB AND WELSCH.  SEE
		//        GOLUB, G. H., AND WELSCH, J. H., "CALCULATION OF GAUSSIAN
		//        QUADRATURE RULES," MATHEMATICS OF COMPUTATION 23 (APRIL,
		//        1969), PP. 221-230.
		
		double MUZERO[] = new double[1];
		int I;
		int IERR[] = new int[1];
		
		CLASS(N, ALPHA, BETA, B, T, MUZERO);
        W[0] = 1.0;
        for (I = 1; I < N; I++) {
        	W[I] = 0.0;
        }
        IMTQL2(N, T, B, W, IERR);
        for (I = 0; I < N; I++) {
        	W[I] = MUZERO[0]*W[I]*W[I];
        }
        return;
	}
	
	private void CLASS(int N, double ALPHA, double BETA, double B[], double A[], double MUZERO[]) {
	//
	//           THIS PROCEDURE SUPPLIES THE COEFFICIENTS A(J), B(J) OF THE
	//        RECURRENCE RELATION
	//
	//             B P (X) = (X - A ) P   (X) - B   P   (X)
	//              J J            J   J-1       J-1 J-2
	//
	//        FOR THE VARIOUS CLASSICAL (NORMALIZED) ORTHOGONAL POLYNOMIALS,
	//        AND THE ZERO-TH MOMENT
	//
	//             MUZERO = INTEGRAL W(X) DX
	//
	//        OF THE GIVEN POLYNOMIAL'S WEIGHT FUNCTION W(X).  SINCE THE
	//        POLYNOMIALS ARE ORTHONORMALIZED, THE TRIDIAGONAL MATRIX IS
	//        GUARANTEED TO BE SYMMETRIC.
	
	//      DOUBLE PRECISION A(N),B(N)
	
	      double A2B2,AB,ABI;
	      int I,NM1;
	//
	//     .. External Functions ..
	//     double DGAMMA
	//      EXTERNAL DGAMMA
	
	      NM1 = N - 1;
	//
	      AB = ALPHA + BETA;
	      ABI = 2.0 + AB;
	      MUZERO[0] = Math.pow(2.0, (AB+1.0))*DGAMMA(ALPHA+1.0)*
	              DGAMMA(BETA+1.0)/DGAMMA(ABI);
	      A[0] = (BETA-ALPHA)/ABI;
	      B[0] = Math.sqrt(4.0* (1.0+ALPHA)* (1.0+BETA)/
	            ((ABI+1.0)*ABI*ABI));
	      A2B2 = BETA*BETA - ALPHA*ALPHA;
	      for (I = 2; I <= NM1; I++) {
	          ABI = 2.0*I + AB;
	          A[I-1] = A2B2/ ((ABI-2.0)*ABI);
	          B[I-1] = Math.sqrt(4.0*I* (I+ALPHA)* (I+BETA)* (I+AB)/
	                ((ABI*ABI-1)*ABI*ABI));
	      } // for (I = 2; I <= NM1; I++)
	      ABI = 2.0*N + AB;
	      A[N-1] = A2B2/ ((ABI-2.0)*ABI);
	      return;
	}
	
	private void IMTQL2(int N, double D[], double E[], double Z[], int IERR[]) {
		// This is a modified version of the EISPACK routine IMTQL2.
		// It finds the eigenvalues and first components of the eigenvectors
		// of a symmetric tridiagonal matrix by the implicit QL method.
		
		// D(N), E(N), Z(N)
		double B, C, F, G, P, R, S;
		int I, II, K, L, M, MML;
		int J = 0;
		boolean zeroJ = true;
		IERR[0] = 0;
		if (N == 1) {
			return;
		}
		
		E[N-1] = 0.0;
		for (L = 1; L <= N; L++) {
			if (zeroJ) {
		        J = 0;
			}
			else {
				zeroJ = true;
			}
		    // Look for small sub-diagonal element
		    for (M = L; M <= N; M++) {
		        if (M == N) {
		        	break;
		        }
		        if (Math.abs(E[M-1]) <= MACHEP * (Math.abs(D[M-1] + Math.abs(D[M])))) {
		        	break;
		        }
		    } // for (M = L; M <= N; M++)
		    
		    P = D[L-1];
		    if (M == L) {
		    	continue;
		    }
		    if (J == 30) {
		    	// Set error -- no convergence to an eigenvalue after 30 iterations.
		    	IERR[0] = L;
		    	return;
		    }
		    J = J+1;
		    // Form shift
		    G = (D[L] - P)/(2.0*E[L-1]);
		    R = Math.sqrt(G*G + 1.0);
		    if (G >= 0) {
		    	G = D[M-1] - P + E[L-1]/(G + Math.abs(R));
		    }
		    else {
		    	G = D[M-1] - P + E[L-1]/(G - Math.abs(R));	
		    }
		    S = 1.0;
		    C = 1.0;
		    P = 0.0;
		    MML = M - L;
		    // For I=M-1 step -1 until L do
		    for (II = 1; II <= MML; II++) {
		        I = M - II;
		        F = S * E[I-1];
		        B = C * E[I-1];
		        if (Math.abs(F) >= Math.abs(G)) {
		            C = G/F;
		            R = Math.sqrt(C*C + 1.0);
		            E[I] = F*R;
		            S = 1.0/R;
		            C = C*S;
		        }
		        else {
		            S= F/G;
		            R = Math.sqrt(S*S + 1.0);
		            E[I] = G*R;
		            C = 1.0/R;
		            S = S * C;
		        }
		        G = D[I] - P;
		        R = (D[I-1] - G)*S + 2.0*C*B;
		        P = S*R;
		        D[I] = G + P;
		        G = C*R - B;
		        // Form first component of vector
		        F = Z[I];
		        Z[I] = S * Z[I-1] + C * F;
		        Z[I-1] = C * Z[I-1] - S * F;
		    } // for (II = 1; II <= MML; II++)
		    
		    D[L-1] = D[L-1] - P;
		    E[L-1] = G;
		    E[M-1] = 0.0;
		    zeroJ = false;
		} // for (L = 1; L <= N; L++)
		
		// Order eigenvalues and eigenvectors
		for (II = 2; II <= N; II++) {
		    I = II - 1;
		    K = I;
		    P = D[I-1];
		    
		    for (J = II; J <= N; J++) {
		        if (D[J-1] >= P) {
		        	continue;
		        }
		        K = J;
		        P = D[J-1];
		    } // for (J = II; J <= N; J++)
		    
		    if (K == I) {
		    	continue;
		    }
		    D[K-1] = D[I-1];
		    D[I-1] = P;
		    P = Z[I-1];
		    Z[I-1] = Z[K-1];
		    Z[K-1] = P;
		} // for (II = 2; II <= N; II++)
		return;
	}

	private double DGAMMA(double X) {
		//
		// COMPUTES THE GAMMA FUNCTION VIA A SERIES EXPANSION FROM
		// ABRAMOWITZ & STEGUN
		//
		// L. N. TREFETHEN, 1/13/84
		//
	    double FAC, G, Y;
	    int I, II;
	    double result;
	    
	    double C[] = new double[]{1.0000000000000000,.5772156649015329,
	    	          -.6558780715202538,-.0420026350340952,
	    	          .1665386113822915,-.0421977345555443,
	    	          -.0096219715278770,.0072189432466630,
	    	          -.0011651675918591,-.0002152416741149,
	    	          .0001280502823882,-.0000201348547807,
	    	          -.0000012504934821,.0000011330272320,
	    	          -.0000002056338417,.0000000061160950,.0000000050020075,
	    	          -.0000000011812746,.0000000001043427,.0000000000077823,
	    	          -.0000000000036968,.0000000000005100,
	    	          -.0000000000000206,-.0000000000000054,
	    	          .0000000000000014,.0000000000000001};
	    
	    // ARGUMENT REDUCTION:
	        FAC = 1.0;
	        Y = X;
	        while (Y > 1.5) {
	            Y = Y - 1.0;
	            FAC = FAC*Y;
	        }

	     while (Y < 0.5) {
	        FAC = FAC/Y;
	        Y = Y + 1.0;
	     }
	  
	  // SERIES:
	     G = C[25];
	     for (I = 1; I <= 25; I++) {
	            II = 26 - I;
	            G = Y*G + C[II-1];
	     }
         G = Y*G;
         result = FAC/G;
         return result;
	}
}