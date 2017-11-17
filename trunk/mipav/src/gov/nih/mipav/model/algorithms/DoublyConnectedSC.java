package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.MipavUtil;

public class DoublyConnectedSC extends AlgorithmBase {
	// This is a port of the FORTRAN ACM TOMS Algorithm 785, collected
	// algorithms form ACM.  The original ACM work was published in Transactions
	// on Mathematical Software, Vol. 24, No. 3, September, 1998, pp. 317-333.
	// The original FORTRAN code was written by Dr. Chenglie Hu as described in
	// his article "A Software Package for Computing Schwarz-Christoffel Conformal
	// Transformation for Doubly Connected Polyhgonal Regions."
	
	//************* DSCPACK *************************************************
	// THIS IS A COLLECTION OF SUBROUTINES TO SOLVE PARAMETER PROBLEM OF   *
	// THE SCHWARZ-CHRISTOFFEL TRANSFORMATION FOR DOUBLY CONNECTED REGIONS *
    // A DRIVER IS NEEDED FOR A/AN PARTICULAR PROBLEM OR APPLICATION.      *
	// AUTHOR:                                                             *
	// CHENGLIE HU   APRIL,1994 (REVISED JULY,1995) AT WICHITA STATE UNIV. *
	//               A FURTHER REVISION WAS DONE AT FHSU (JULY, 1997)      *
	// REFERENCES:1. H.DAEPPEN, DIE SCHWARZ-CRISTOFFEL ABBILDUNG FUER      *
	//               ZWEIFACH ZUSAMMENHAENGENDE GEBIETE MIT ANWENDUNGEN.   *
	//               PH.D. DISSERTATION, ETH ZUERICH.                      *
	//            2. L.N.TREFETHEN, SCPACK USER'S GUIDE(MIT REPORT 1989)   *
	//            3. HENRICI,APPLIED & COMPUTATIONAL COMPLEX ANALYSIS,VOL.3*
	//            4. C.HU, APPLICATION OF COMPUTATIONAL COMPLEX ANALYSIS TO*
	//               SOME FREE BOUNDARY AND VORTEX FLOWS.PH.D. DISS. 1995  *
	//**********************************************************************

	
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
	// 1 for solving the nonlinear system, 2 for not solving the nonlinear system
	private int ISOLV;
	private boolean testRoutine = false;
	private double MACHEP = 2.2204460E-16;
	
	public DoublyConnectedSC() {
		
	}
	
	public DoublyConnectedSC(int IPOLY, int NPTQ, int ISOLV) {
		this.IPOLY = IPOLY;
		this.NPTQ = NPTQ;
		this.ISOLV = ISOLV;
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
		int IGUESS;
		int LINEARC;
		double TOL;
		double U[] = new double[1];
		double C[] = new double[2];
		double W0[][] = new double[30][2];
		double W1[][] = new double[30][2];
		double PHI0[] = new double[30];
		double PHI1[] = new double[30];
		
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
			
			// Specify some parameters of the calling sequence of DSCSOLV:
			IGUESS = 1;
			LINEARC = 1;
			TOL = 1.0E-10;
			
			// Solve the accessory parameter problem
			if (ISOLV == 1) {
				DSCSOLV(TOL, IGUESS, M[0], N[0], U, C, W0, W1, PHI0, PHI1,
						Z0, Z1, ALFA0, ALFA1, NPTQ, QWORK, ISHAPE, LINEARC);
			}
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
	
	private void THDATA(double U[]) {
	    //    ----------------------
	    //    GENERATES DATA RELATED ONLY TO INNER RADIUS
	    //    U AND USED IN COMPUTING THE THETA-FUNCTION.

		// .. Local Scalars ..
	    double PI;
	    int  K,N;
	
	    //    .. Common blocks ..
	    // COMMON /PARAM4/UARY,VARY,DLAM,IU
	
	    PI = Math.PI;
	    if (U[0] >= 0.63) {
	    	VARY[0] = Math.exp(PI*PI/Math.log(U[0]));
	    	DLAM = -Math.log(U[0])/PI;
	    	return;
	    }
	    if (U[0] < 0.06) {
	        IU = 3;
	    }
	    else if (U[0] < 0.19) {
	        IU = 4;
	    }
	    else if (U[0] < 0.33) {
	        IU = 5;
	    }
	    else if (U[0] < 0.45) {
	        IU = 6;
	    }
	    else if (U[0] < 0.55) {
	        IU = 7;
	    }
	    else {
	        IU = 8;
	    }

	    for (K = 1; K <= IU; K++) {
	          N = K*K;
	          UARY[K-1] = Math.pow(U[0], N);
	    } // for (K = 1; K <= IU; K++)
	    return;

	}
	
	private double[] WQSUM(double WA[], double PHIA,int KWA, int IC, double WB[],
			double PHIB, double RADIUS,int M,int N, double U, double W0[][], double W1[][],
			double ALFA0[], double ALFA1[], int NPTQ,double QWORK[], int LINEARC) {
		//   ------------------------------------------------------------
		//   CALCULATES THE  COMPLEX  INTEGRAL  FROM WA TO WB ALONG  A
		//   LINE SEGMENT (LINEARC=0)OR A CIRCULAR ARC (LINEARC=1)WITH
		//   POSSIBLE SINGULARITY AT WA, WHERE  KWA IS THE INDEX OF WA
		//   IN W0 (IC=0) OR IN W1 (IC=1). KWA=0 AND IC=2 ( NO OTHER
		//   VALUES PERMITTED ) IF WA IS NOT A PREVERTEX, WHICH THEN
		//   INDICATES THAT ONLY PURE GAUSSIAN QUARATURE IS NEEDED.
		//   PHIA & PHIB  ARE  ARGUMENTS OF  WA & WB  RESPECTIVELY. IF
		//   INTEGRATING ALONG A CIRCULAR ARC,RADIUS SHOULD BE ASSIGNED
		//   TO BE EITHER 1 OR U. ANY VALUE,HOWEVER,CAN BE ASSIGNED TO
		//   RADIUS IF INTEGRATING ALONG A LINE SEGMENT.SEE DOCUMENTATIONS
		//   OF WPROD AND QINIT FOR THE REST OF CALLING SEQUENCE.
		
		//     .. Scalar Arguments ..
		//    DOUBLE COMPLEX WA,WB
		
		//     .. Array Arguments ..
		//      DOUBLE COMPLEX W0(M),W1(N)
		//      DOUBLE PRECISION ALFA0(M),ALFA1(N),QWORK(NPTQ* (2* (M+N)+3))
		
		//     .. Local Scalars ..
		double result[] = new double[2];
		/*      DOUBLE COMPLEX W,WC,WH,ZI
		      DOUBLE PRECISION PWC,PWH
		      INTEGER I,IOFFST,IWT1,IWT2
		C     ..
		C     .. External Functions ..
		      DOUBLE COMPLEX WPROD
		      EXTERNAL WPROD
		C     ..
		C     .. Intrinsic Functions ..
		      INTRINSIC EXP
		C     ..
		//     .. Common blocks ..
		//      COMMON /PARAM4/UARY,VARY,DLAM,IU
		C     ..
		      WQSUM = (0.D0,0.D0)
		C
		C   INDEX ARRANGEMENT:
		      IWT1 = NPTQ* (IC*M+KWA-1) + 1
		      IF (KWA.EQ.0) IWT1 = NPTQ* (M+N) + 1
		      IWT2 = IWT1 + NPTQ - 1
		      IOFFST = NPTQ* (M+N+1)
		C
		C   COMPUTE GAUSS-JACOBI SUM(W(J)*PROD(X(J))):
		      IF (LINEARC.EQ.1) GO TO 20
		C
		C   INTEGRATE ALONG A LINE SEGMENT:
		      WH = (WB-WA)/2.D0
		      WC = (WA+WB)/2.D0
		      DO 10 I = IWT1,IWT2
		          W = WC + WH*QWORK(I)
		          WQSUM = WQSUM + QWORK(IOFFST+I)*
		     +            WPROD(W,M,N,U,W0,W1,ALFA0,ALFA1)
		   10 CONTINUE
		      WQSUM = WQSUM*WH
		      RETURN
		C
		C   INTEGRATE ALONG A CIRCULAR ARC:
		   20 ZI = (0.D0,1.D0)
		      PWH = (PHIB-PHIA)/2.D0
		      PWC = (PHIB+PHIA)/2.D0
		      DO 30 I = IWT1,IWT2
		          W = RADIUS*EXP(ZI* (PWC+PWH*QWORK(I)))
		          WQSUM = WQSUM + QWORK(IOFFST+I)*W*
		     +            WPROD(W,M,N,U,W0,W1,ALFA0,ALFA1)
		   30 CONTINUE
		      WQSUM = WQSUM*PWH*ZI*/
		   return result;
	}

	
	private double[] WQUAD1(double WA[], double PHIA, int KWA,int IC, double WB[],
			double PHIB, double RADIUS,int M,int N, double U, double W0[][], double W1[][],
			double ALFA0[], double ALFA1[], int NPTQ, double QWORK[], int LINEARC) {
		//   -------------------------------------------------------------
		//   CALCULATES THE COMPLEX INTEGRAL OF WPROD FROM WA TO WB ALONG
		//   EITHER A CIRCULAR ARC OR A LINE-EGMENT.  COMPOUND ONE-SIDED
		//   GAUSS-JACOBI QUDRATURE IS USED.SEE SUBROUTINE WQSUM FOR THE
		//   CALLING SEQUENCE.
		
		//   CHECK FOR ZERO-LENGTH INTEGRAL:
		//     .. Scalar Arguments ..
		// DOUBLE COMPLEX WA,WB
		     
		//     .. Array Arguments ..
		//      DOUBLE COMPLEX W0(M),W1(N)
		//      DOUBLE PRECISION ALFA0(M),ALFA1(N),QWORK(NPTQ* (2* (N+M)+3))
		
		//     .. Local Scalars ..
		double WAA[] = new double[2];
		double WBB[] = new double[2];
		double ZI[] = new double[2];
		//    DOUBLE COMPLEX WAA,WBB,ZI
		double PHAA,PHBB,R;
		double result[] = new double[2];
		double result2[] = new double[2];
		double expb;
		double arg;
		     
		//     .. External Functions ..
		//      DOUBLE COMPLEX WQSUM
		//      DOUBLE PRECISION DIST
		//      EXTERNAL WQSUM,DIST
		
		//     .. Common blocks ..
		//      COMMON /PARAM4/UARY,VARY,DLAM,IU
	
	    if (scm.zabs(WA[0]-WB[0], WA[1]-WB[1]) <= 0.0) {
	        result[0] = 0.0;
	    	result[1] = 0.0;
	        return result;
	    }
	    
	    ZI[0] = 0.0;
	    ZI[1] = 1.0;
	    if (LINEARC != 1) {
		
		    //   LINE SEGMENT INTEGRATION PATH IS CONCERNED BELOW:
		    // STEP1:ONE-SIDED G-J QUADRATURE FOR LEFT ENDPT WA:
		    R = Math.min(1.0,DIST(M,N,W0,W1,WA,KWA,IC)/scm.zabs(WB[0]-WA[0],WB[1]-WA[1]));
		    WAA[0] = WA[0] + R* (WB[0]-WA[0]);
		    WAA[1] = WA[1] + R* (WB[1]-WA[1]); 
		    result = WQSUM(WA,0.0,KWA,IC,WAA,0.0,0.0,M,N,U,W0,W1,ALFA0,
		              ALFA1,NPTQ,QWORK,LINEARC);
		
		    //   STEP2:ADJOIN INTERVALS OF PURE GAUSS QUADRATURE IF NECESSARY:
		   while (R != 1.0) {
		      R = Math.min(1.0,DIST(M,N,W0,W1,WAA,0,IC)/scm.zabs(WAA[0]-WB[0], WAA[1] - WB[1]));
		      WBB[0] = WAA[0] + R* (WB[0]-WAA[0]);
		      WBB[1] = WAA[1] + R* (WB[1]-WAA[1]);
		      result2 = WQSUM(WAA,0.0,0,2,WBB,0.0,0.0,M,N,U,W0,W1,
		              ALFA0,ALFA1,NPTQ,QWORK,LINEARC);
		      result[0] = result[0] + result2[0];
		      result[1] = result[1] + result2[1];
		      WAA[0] = WBB[0];
		      WAA[1] = WBB[1];
		   }
		   return result;
	    } // if (LINEARC != 1)
		
		//   CIRCULAR ARC INTEGRATION PATH IS CONCERNED BELOW:
		//   STEP1:ONE-SIDED G-J QUADRATURE FOR LEFT ENDPT WA:
		R = Math.min(1.0,DIST(M,N,W0,W1,WA,KWA,IC)/scm.zabs(WB[0]-WA[0],WB[1]-WA[1]));
		PHAA = PHIA + R* (PHIB-PHIA);
		expb = RADIUS * Math.exp(ZI[0]*PHAA);
		arg = ZI[1] * PHAA;
		WAA[0] = expb * Math.cos(arg);
		WAA[1] = expb * Math.sin(arg);
		result = WQSUM(WA,PHIA,KWA,IC,WAA,PHAA,RADIUS,M,N,U,W0,W1,ALFA0,
		              ALFA1,NPTQ,QWORK,LINEARC);
		
		//   STEP2:ADJOIN INTERVALS OF PURE GAUSS QUADRATURE IF NECESSARY:
		while (R != 1.0) {
		      R = Math.min(1.0,DIST(M,N,W0,W1,WAA,0,IC)/scm.zabs(WAA[0]-WB[0],WAA[1]-WB[1]));
		      PHBB = PHAA + R* (PHIB-PHAA);
		      expb = RADIUS * Math.exp(ZI[0]*PHBB);
		      arg = ZI[1] * PHBB;
		      WBB[0] = expb * Math.cos(arg);
		      WBB[1] = expb * Math.sin(arg);
		      result2 = WQSUM(WAA,PHAA,0,2,WBB,PHBB,RADIUS,M,N,U,W0,W1,
		             ALFA0,ALFA1,NPTQ,QWORK,LINEARC);
		      result[0] = result[0] + result2[0];
		      result[1] = result[1] + result2[1];
		      PHAA = PHBB;
		      WAA[0] = WBB[0];
		      WAA[1] = WBB[1];
		} //while (R != 1.0)
		return result;
	}

	
	private double[] WQUAD(double WA[], double PHIA,int KWA,int ICA, double WB[],
			double PHIB, int KWB,int ICB,double RADIUS,int M,int N, double U, double W0[][],
		    double W1[][], double ALFA0[], double ALFA1[], int NPTQ, double QWORK[],
		    int LINEARC,int IEVL) {
		//   ---------------------------------------------------------------
		//   CALCULATES THE COMPLEX INTEGRAL OF WPROD FROM WA TO WB
		//   ALONG A CIRCULAR ARC OR A LINE-SEGMENT.FUNCTION WQUAD1
		//   IS CALLED FOUR TIMES,ONE FOR EACH 1/4 OF THE INTERVAL.
		//   NOTE:  WQUAD1 ALLOWS  ONLY THE LEFT ENDPOINT  TO  BE A
		//   POSSIBLE SINGULARITY. SEE ROUTINE WQSUM FOR THE CALLING
	    //   SEQUENCE.
		
		//     .. Scalar Arguments ..
		//    DOUBLE COMPLEX WA,WB
		     
		//     .. Array Arguments ..
		//     DOUBLE COMPLEX W0(M),W1(N)
		//     DOUBLE PRECISION ALFA0(M),ALFA1(N),QWORK(NPTQ* (2* (M+N)+3))
	
		//     .. Local Scalars 
		double WMID[] = new double[2];
		double WMIDA[] = new double[2];
		double WMIDB[] = new double[2];
		double WQA[] = new double[2];
		double WQA1[] = new double[2];
		double WQA2[] = new double[2];
		double WQB[] = new double[2];
		double WQB1[] = new double[2];
		double WQB2[] = new double[2];
		double ZI[] = new double[2];
		// DOUBLE COMPLEX WMID,WMIDA,WMIDB,WQA,WQB,ZI
		double PHMID,PHMIDA,PHMIDB,PI;
		double expb;
		double arg;
		double result[] = new double[2];
		//     .. Common blocks ..
		//     COMMON /PARAM4/UARY,VARY,DLAM,IU
		
		PI = Math.PI;
		ZI[0] = 0.0;
		ZI[1] = 1.0;
		
		//   DETERMINE MIDPTS ON A LINE SEGMENT OR ON A CIRCULAR ARC:
		if (LINEARC == 0) {
			WMID[0] = (WA[0] + WB[0])/2.0;
			WMID[1] = (WA[1] + WB[1])/2.0;
		    WMIDA[0] = (WA[0] + WMID[0])/2.0;
		    WMIDA[1] = (WA[1] + WMID[1])/2.0;
		    WMIDB[0] = (WB[0] + WMID[0])/2.0;
		    WMIDB[1] = (WB[1] + WMID[1])/2.0;
		    PHMID = 0.0;
		    PHMIDA = 0.0;
		    PHMIDB = 0.0;
		}
		else {
	        if (IEVL != 1) {
		        if (PHIB < PHIA) {
		            PHIA = PHIA - 2.0*PI;
		        }
	        } // if (IEVL != 1)
		   PHMID = (PHIA+PHIB)/2.0;
		   expb = RADIUS * Math.exp(ZI[0] * PHMID);
		   arg = ZI[1] * PHMID;
		   WMID[0] = expb * Math.cos(arg);
		   WMID[1] = expb * Math.sin(arg);
		   PHMIDA = (PHIA+PHMID)/2.0;
		   expb = RADIUS * Math.exp(ZI[0] * PHMIDA);
		   arg = ZI[1] * PHMIDA;
		   WMIDA[0] = expb * Math.cos(arg);
		   WMIDA[1] = expb * Math.sin(arg);
		   PHMIDB = (PHIB+PHMID)/2.0;
		   expb = RADIUS * Math.exp(ZI[0]*PHMIDB);
		   arg = ZI[1] * PHMIDB;
		   WMIDB[0] = expb * Math.cos(arg);
		   WMIDB[1] = expb * Math.sin(arg);
		}
		
		//   COMPOUND GAUSS-JACOBI PROCESS ACCORDING TO ONE-QUATER RULE:
        WQA1 = WQUAD1(WA,PHIA,KWA,ICA,WMIDA,PHMIDA,RADIUS,M,N,U,W0,W1,
           ALFA0,ALFA1,NPTQ,QWORK,LINEARC);
        WQA2 = WQUAD1(WMID,PHMID,0,2,WMIDA,PHMIDA,RADIUS,M,N,U,W0,W1,ALFA0,
           ALFA1,NPTQ,QWORK,LINEARC);
        WQA[0] = WQA1[0] - WQA2[0];
        WQA[1] = WQA1[1] - WQA2[1];
        WQB1 = WQUAD1(WB,PHIB,KWB,ICB,WMIDB,PHMIDB,RADIUS,M,N,U,W0,W1,
           ALFA0,ALFA1,NPTQ,QWORK,LINEARC);
        WQB2 = WQUAD1(WMID,PHMID,0,2,WMIDB,PHMIDB,RADIUS,M,N,U,W0,W1,ALFA0,
           ALFA1,NPTQ,QWORK,LINEARC);
        WQB[0] = WQB1[0] - WQB2[0];
        WQB[1] = WQB1[1] - WQB2[1];
        result[0] = WQA[0] - WQB[0];
        result[1] = WQA[1] - WQB[1];
		return result;
	}


	
	private void XWTRAN(int M,int N, double X[], double U[],
			double C[], double W0[][], double W1[][], double PHI0[],
			double PHI1[]) {
		//   -----------------------------------------------
		//  TRANSFORMS X[K-1](UNCONSTRAINED PARAMETERS) TO ACTUAL
		//  D-SC PARAMETERS:U,C,W0,W1.PHI0 & PHI1 ARE ARGUMENTS
		//  OF THE PREVERTICES CONTAINED IN W0 & W1.
		
		//     .. Scalar Arguments ..
		//      DOUBLE COMPLEX C
		      
		//     .. Array Arguments ..
		//      DOUBLE COMPLEX W0(M),W1(N)
		//      DOUBLE PRECISION PHI0(M),PHI1(N),X(M+N+2)
	
		//     .. Local Scalars ..
	      double DPH, PH, PHSUM, PI;
	      int  I;
	
	      PI = Math.PI;
	      if (Math.abs(X[0]) <= 1.0E-14) {
	          U[0] = 0.5;
	      }
	      else {
	          U[0] = (X[0]-2.0-Math.sqrt(0.9216*X[0]*X[0]+4.0))/ (2.0*X[0]);
	          U[0] = (0.0196*X[0]-1.0)/ (U[0]*X[0]);
	      }

	      C[0] = X[1];
	      C[1] = X[2];
	      if (Math.abs(X[N+2]) <= 1.0E-14) {
	          PHI1[N-1] = 0.0;
	      }
	      else {
	          PH = (1.0+Math.sqrt(1.0+PI*PI*X[N+2]*X[N+2]))/X[N+2];
	          PHI1[N-1] = PI*PI/PH;
	      }

	      DPH = 1.0;
	      PHSUM = DPH;
	      
	      for (I = 1; I <= N - 1; I++) {
	          DPH = DPH/Math.exp(X[2+I]);
	          PHSUM = PHSUM + DPH;
	      } // for (I = 1; I <= N - 1; I++)
	      DPH = 2.0*PI/PHSUM;
	      PHI1[0] = PHI1[N-1] + DPH;
	      W1[0][0] = U[0] * Math.cos(PHI1[0]);
	      W1[0][1] = U[0] * Math.sin(PHI1[0]);
	      W1[N-1][0] = U[0] * Math.cos(PHI1[N-1]);
	      W1[N-1][1] = U[0] * Math.sin(PHI1[N-1]);
	      PHSUM = PHI1[0];
	      for (I = 1; I <= N - 2; I++) {
	          DPH = DPH/Math.exp(X[2+I]);
	          PHSUM = PHSUM + DPH;
	          PHI1[I] = PHSUM;
	          W1[I][0] = U[0] * Math.cos(PHSUM);
	          W1[I][1] = U[0] * Math.sin(PHSUM);
	      } // for (I = 1; I <= N - 2; I++)
	      DPH = 1.0;
	      PHSUM = DPH;
	      for (I = 1; I <= M - 1; I++) {
	          DPH = DPH/Math.exp(X[N+2+I]);
	          PHSUM = PHSUM + DPH;
	      } // for (I = 1; I <= M - 1; I++)
	      DPH = 2.0*PI/PHSUM;
	      PHSUM = DPH;
	      PHI0[0] = DPH;
	      W0[0][0] = Math.cos(DPH);
	      W0[0][1] = Math.sin(DPH);
	      for (I = 1; I <= M - 2; I++) {
	          DPH = DPH/Math.exp(X[N+2+I]);
	          PHSUM = PHSUM + DPH;
	          PHI0[I] = PHSUM;
	          W0[I][0] = Math.cos(PHSUM);
	          W0[I][1] = Math.sin(PHSUM);
	      } // for (I = 1; I <= M - 2; I++)
		  return;
	}

	
	private void DSCSOLV(double TOL,int IGUESS,int M,int N, double U[],
			double C[], double W0[][], double W1[][], double PHI0[],
			double PHI1[], double Z0[][],  double Z1[][], double ALFA0[],
		   double ALFA1[], int NPTQ, double QWORK[], int ISHAPE, int LINEARC) {
	//  -----------------------------------------------------------------
	//  SOLVES THE NONLINEAR SYSTEM FOR D-SC PARAMETERS.
	//  CALLING SEQUENCE:
	//  TOL       A TOLERANCE TO CONTROL THE CONVERGENCE IN HYBRD
	//  IGUESS    (=0 )A NON-EQUALLY SPACED INITIAL GUESS OR(=1)THE
	//            OTHER  EQUALLY-SPACED  INITIAL GUESS PROVIDED, OR
	//            (=2)USER-SUPPLIED INITIAL GUESS WHICH IS REQUIRED
	//            TO BE THE ARGUMENTS OF THE INITIAL PREVERTICES.
	//            ROUTINE ARGUM MAY BE USED FOR COMPUTING ARGUMENTS
	//            IF NEEDED. NOTE: C WILL BE COMPUTED IN THE ROUTINE
	//            (NOT SUPPLIED!)
	//  M,N,U,C,W0,W1,PHI0,PHI1,ALFA0,ALFA1,Z0,Z1
	//            CONSTITUTE THE GEOMETRY OF THE POLYGONAL REGION
	//            AND THE MAPPING FUNCTION. ON RETURN U,C,W0,& W1
	//            WILL  CONTAIN COMPUTED PARAMETERS. (PHI0 & PHI1
	//            WILL CONTAIN THE ARGUMENTS OF THE PREVERTICES.)
	//  QWORK     SEE CALLING SEQUENCE DOCUMENTATION IN QINIT.THE ARRAY
	//            MUST HAVE BEEN FILLED BY QINIT BEFORE CALLING DSCSOLV.
	//  NPTQ      THE NUMBER OF GAUSS-JACOBI POINTS USED
	//  ISHAPE    INDICATES  THAT  OUTER POLYGON  CONTAINS NO INFINITE
	//            VERTICES (ISHAPE=0) OR IT HAS SOME INFINITE VERTICES
	//            (ISHAPE=1).
	//  LINEARC   INTEGER VARIABLE TO CONTROL INTEGRATION PATH.IN PATICULAR:
	//            LINEARC=0:INTEGRATING ALONG LINE SEGMENT WHENEVER POSSIBLE
	//            LINEARC=1:INTEGRATING ALONG CIRCULAR ARC WHENEVER POSSIBLE
	//  THE DISCRIPTION OF ARRAY IND & VARIABLE NSHAPE NEEDED IN DSCFUN:
	//  IND       CONTAINS INDICES  CORRESPONDING TO  THOSE INFINITE
	//            VERTICES,BUT THE FIRST & THE LAST ENTRIS MUST BE -1
	//           & M-2(M IS THE # OF VERTICES ON THE OUTER POLYGON).
	//  NSHAPE    THE DIMENSION OF THE INTERGE ARRAY IND(NSHAPE)
	
	
	//     .. Scalar Arguments ..
	//      DOUBLE COMPLEX C
	      
	//     .. Array Arguments ..
	//      DOUBLE COMPLEX W0(M),W1(N),Z0(M),Z1(N)
	//      DOUBLE PRECISION ALFA0(M),ALFA1(N),PHI0(M),PHI1(N),
	//                      QWORK(NPTQ* (2* (N+M)+3))
		
		// Scalars in Common
		double C2[] = new double[2];
		double U2;
		int ICOUNT, ISHAPE2, ISPRT, LINEARC2, M2, N2, NPTQ2, NSHAPE;
		
		// Arrays in Common
		double W02[][] = new double[30][2];
		double W12[][] = new double[30][2];
		double Z02[][] = new double[30][2];
		double Z12[][] = new double[30][2];
		double ALFA02[] = new double[30];
		double ALFA12[] = new double[30];
		double PHI02[] = new double[30];
		double PHI12[] = new double[30];
		double QWORK2[] = new double[1660];
		int IND[] = new int[20];
		
		// Local scalars
		double C1[] = new double[2];
		double WINT[] = new double[2];
		double ZI[] = new double[2];
		double AVE, BOTM, DSTEP, FACTOR, PI, TOP;
		int I, INFO, K, KM, KN, MAXFUN, NFEV, NM, NWDIM;
		
		// Local arrays
		double DIAG[] = new double[42];
		double FJAC[][] = new double[42][42];
		double FVAL[] = new double[42];
		double QW[] = new double[1114];
		double X[] = new double[42];
		
	    // Common blocks ..
	    // COMMON /PARAM1/W02,W12,Z02,Z12,C2
	    // COMMON /PARAM2/U2,PHI02,PHI12,ALFA02,ALFA12,QWORK2
	    // COMMON /PARAM3/M2,N2,NPTQ2,ISHAPE2,LINEARC2,NSHAPE,IND
	    // COMMON /PARAM4/UARY,VARY,DLAM,IU
	    // COMMON /PARAM5/ISPRT,ICOUNT

		ZI[0] = 0.0;
		ZI[1] = 1.0;
		PI = Math.PI;
		ICOUNT = 0;
		if (ISHAPE != 0) {
			NSHAPE = 1;
			for (I = 2; I <= M-1; I++) {
			    if (ALFA0[I-1] >= 0.0) {
			    	continue;
			    }
			    NSHAPE = NSHAPE + 1;
			    IND[NSHAPE-1] = I-1;
			} // for (I = 2; I <= M-1; I++)
			IND[0] = -1;
			NSHAPE = NSHAPE + 1;
			IND[NSHAPE-1] = M-2;
		} // if (ISHAPE != 0)
		
		// Fix one prevertex
		W0[M-1][0] = 1.0;
		W0[M-1][1] = 0.0;
		PHI0[M-1] = 0.0;
		
		// Following two value assignments are to satisfy the compiler WATFOR77:
		X[1] = 0.0;
		X[2] = 0.0;
		// Initial guess (IGUESS = 0):
		if (IGUESS == 0) {
		    X[0] = 1.0/0.5 - 1.0/0.46;
		    AVE = 2.0 * PI/(double)N;
		    for (I = 1; I <= N-2; I++) {
		    	X[2 + I] = Math.log((AVE + 0.0001*I)/(AVE + 0.0001*(I+1)));
		    } // for (I = 1; I <= N-2; I++)
		    X[N+1] = Math.log((AVE+0.0001*(N-1))/(2.0*PI-(N-1)* (AVE+N*0.00005)));
		    X[N+2] = 1.0/ (4.0-0.1) - 1.0/ (4.0+0.1);
		    AVE = 2.0*PI/M;
		    for (I = 1; I <= M - 2; I++) {
		        X[N+2+I] = Math.log((AVE+0.0001*(I-1))/(AVE+0.0001*I));
		    } // for (I = 1; I <= M - 2; I++)
		    X[M+N+1] = Math.log((AVE+0.0001*(M-2))/(2.0*PI-(M-1)* (AVE+(M-2)*0.00005)));
		} // if (IGUESS == 0)
		else if (IGUESS == 1) {
			//  INITIAL GUESS (IGUESS=1):
		    X[0] = 1.0/0.53 - 1.0/0.43;
		    for (I = 1; I <= N - 1; I++) {
		        X[2+I] = 0.0;
		    } // for (I = 1; I <= N - 1; I++)
		    X[N+2] = 1.0/ (4.0-0.1) - 1.0/ (4.0+0.1);
		    for (I = 1; I <= M - 1; I++) {
		        X[N+2+I] = 0.0;
		    } // for (I = 1; I <= M - 1; I++)
		} // else if (IGUESS == 1)
		else {
			X[0] = 1.0/ (0.98-U[0]) - 1.0/ (U[0]-0.02);
			for (K = 1; K <= N - 1; K++) {
			    KN = K - 1;
			    if (KN == 0) {
			    	KN = N;
			    }
			    TOP = PHI1[K-1] - PHI1[KN-1];
			    if (TOP < 0.0) {
			    	TOP = TOP + 2.0*PI;
			    }
			    BOTM = PHI1[K] - PHI1[K-1];
			    if (BOTM < 0.0) {
			    	BOTM = BOTM + 2.0*PI;
			    }
			    X[2+K] = Math.log(TOP) - Math.log(BOTM);
			} // for (K = 1; K <= N - 1; K++)
			X[N+2] = 1.0/ (PI-PHI1[N-1]) - 1.0/ (PI+PHI1[N-1]);
			for (K = 1; K <= M - 1; K++) {
			    KM = K - 1;
			    if (KM == 0) {
			    	KM = M;
			    }
			    TOP = PHI0[K-1] - PHI0[KM-1];
			    if (TOP < 0.0) {
			    	TOP = TOP + 2.0*PI;
			    }
			    BOTM = PHI0[K] - PHI0[K-1];
			    if (BOTM < 0.0) {
			    	BOTM = BOTM + 2.0*PI;
			    }
			    X[N+2+K] = Math.log(TOP) - Math.log(BOTM);
			} // for (K = 1; K <= M - 1; K++)
		} // else
		
		// CALCULATE THE INITIAL GUESS X[1] & X[2] TO MATCH
		// THE CHOICE FOR X[0],X[3],...,X[M+N+1]:
		XWTRAN(M,N,X,U,C,W0,W1,PHI0,PHI1);
        THDATA(U);
        WINT = WQUAD(W0[M-1],0.0,M,0,W1[N-1],0.0,N,1,0.0,M,N,U[0],W0,W1,ALFA0,
        	            ALFA1,NPTQ,QWORK,0,2);

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
	
	private double DIST(int M,int N, double W0[][], double W1[][],
			double W[], int KWA, int IC) {
	//   -----------------------------------
	//    DETERMINES THE DISTANCE FROM W TO THE NEAREST SINGULARITY
	//    OTHER THAN W ITSELF.( W COULD BE ONE OF THE PREVERTICES.)
	//    KWA IS THE INDEX OF W IN W0 (IF IC=0) OR W1 (IF IC=1), OR
	//    WK COULD BE 0 (IF ONE CHOOSES) WHEN W IS NOT A PREVERTEX.
    //
	//     .. Scalar Arguments ..
	//     DOUBLE COMPLEX W
	
	//     .. Array Arguments ..
	//      DOUBLE COMPLEX W0(M),W1(N)
		
	//     .. Local Scalars ..
	      double D;
	      int I;
	      double result;
	
	      result = 2.0;
	      for (I = 1; I <= M; I++) {
	          D = scm.zabs(W[0]-W0[I-1][0],W[1]-W0[I-1][1]);
	          if (I == KWA && IC == 0) {
	        	  continue;
	          }
	          result = Math.min(result,D);
	      } // for (I = 1; I <= M; I++)
	      for (I = 1; I <= N; I++) {
	          D = scm.zabs(W[0]-W1[I-1][0], W[1]-W1[I-1][1]);
	          if (I == KWA && IC == 1) {
	        	  continue;
	          }
	          result = Math.min(result,D);
	      } // for (I = 1; I <= N; I++)
	      return result;

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
		double EPS, SUM;
		int K, MK;
		
		if (!((M >= 3) && (M <= 30) && (N <= 30) && (M+N <= 40))) {
			System.err.println("M must be no less than 3");
			System.err.println("M, N must be no greater than 30");
			System.err.println("M+N must be no greater than 40");
			System.exit(-1);
		}
	
		EPS = 0.00001;
		SUM = 0.0;
		MK = 0;
		for (K = 0; K <= M; K++) {
			if (ALFA0[K] > 0.0) {
				MK = MK + 1;
			}
			SUM = SUM + ALFA0[K];
		} // for (K = 0; K <= M; K++)
		if (!((MK < M && ISHAPE == 1) || (MK == M && ISHAPE == 0))) {
			System.err.println("For finite regions ISHAPE must be 0");
			System.err.println("and for infinite regions ISHAPE must be 1");
			System.exit(-1);
		}
		
		if (Math.abs(SUM - (M-2)) >= EPS) {
			System.err.println("Some angles for outer polygon are wrong");
			System.exit(-1);
		}
		
		SUM = 0.0;
		for (K = 0; K < N; K++) {
			SUM = SUM + ALFA1[K];
		}
		if (Math.abs(SUM - (N+2)) >= EPS) {
			System.err.println("Some angles for inner polygon are wrong");
			System.exit(-1);
		}
		
		if (ALFA0[0] <= 0.0) {
			System.err.println("Z0[0] must be finite");
			System.exit(-1);
		}
		
		if (ALFA0[M-1] <= 0.0) {
			System.err.println("Z0[M-1] must be finite");
			System.exit(-1);
		}
		
		if (ALFA0[M-3] <= 0.0) {
			System.err.println("Z0[M-3] must be finite");
			System.exit(-1);
		}
		
		if (ALFA0[M-2] >= 2.0-EPS) {
			System.err.println("Z0[M-2] must no be a re-entrant corner");
			System.exit(-1);
		}
		
		if ((ALFA0[M-2] >= 1.0-EPS) && (ALFA0[M-2] <= 1.0+EPS)) {
			System.err.println("Z0[M-2] must not be an artificial corner");
			System.exit(-1);
		}
		
		System.out.println("Inputs have been checked with no error being found");
		return;
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