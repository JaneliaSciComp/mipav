package gov.nih.mipav.model.structures.jama;


import java.text.DecimalFormat;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

/** These sparse symmetric eigenvalue and eigenvector routines are ported from the FORTRAN ARPACK package.
 *  The license with the orginal ARPACK package is:
 *  BSD Software License

Pertains to ARPACK and P_ARPACK

Copyright (c) 1996-2008 Rice University.  
Developed by D.C. Sorensen, R.B. Lehoucq, C. Yang, and K. Maschhoff.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

- Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer. 
  
- Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer listed
  in this license in the documentation and/or other materials
  provided with the distribution.
  
- Neither the name of the copyright holders nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.
  
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT  
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

 * 
 * @author ilb
 *
 */

public class SparseEigenvalue implements java.io.Serializable {
	GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
	LinearEquations le = new LinearEquations();
	
	private ViewUserInterface UI = ViewUserInterface.getReference();
	
	DecimalFormat nf = new DecimalFormat("0.00000E0");
	
	private int dsaupd_bounds;
    private int dsaupd_ierr;
    private int dsaupd_ih;
    private int dsaupd_iq;
    private int dsaupd_ishift;
    private int dsaupd_iupd;
    private int dsaupd_iw;
    private int dsaupd_ldh;
    private int dsaupd_ldq;
    private int dsaupd_msglvl;
    private int dsaupd_mxiter[] = new int[1];
    private int dsaupd_mode;
    private int dsaupd_nb;
    private int dsaupd_nev0[] = new int[1];
    private int dsaupd_next;
    private int dsaupd_np[] = new int[1];
    private int dsaupd_ritz;
    
    // From stat.h
    private long t0;
    private long t1;
    private long t2;
    private long t3;
    private long t4;
    private long t5;
    private int nopx;
    private int nbx;
    private int nrorth;
    private int nitref;
    private int nrstrt;
       
    private double tsaupd;
    private double tsaup2;
    private double tsaitr;
    private double tseigt;
    private double tsgets;
    private double tsapps;
    private double tsconv;
    private double titref;
    private double tgetv0;
    private double tmvopx;
    private double tmvbx;
    
    // From debug.h
    private int mgetv0;
    private int msaupd;
    private int msaup2;
    private int msaitr; 
    private int mseigt; 
    private int msapps;
    private int msgets;
    private int mseupd;
    
    private boolean dsaup2_cnorm;
    private boolean dsaup2_getv0;
    private boolean dsaup2_initv;
    private boolean dsaup2_update;
    private boolean dsaup2_ushift;
    private int dsaup2_iter;
    private int dsaup2_kplusp;
    private int dsaup2_msglvl;
    private int dsaup2_nconv[] = new int[1];
    private int dsaup2_nev0;
    private int dsaup2_np0;
    private double dsaup2_rnorm[] = new double[1];
    private double dsaup2_eps23;
    
    private boolean dgetv0_first;
    private boolean dgetv0_inits = true;
    private boolean dgetv0_orth;
    private int dgetv0_iseed[] = new int[4];
    private int dgetv0_iter;
    private int dgetv0_msglvl;
    private double dgetv0_rnorm0;
    
    private boolean dsaitr_orth1;
    private boolean dsaitr_orth2;
    private boolean dsaitr_rstart;
    private boolean dsaitr_step3;
    private boolean dsaitr_step4;
    private boolean dsaitr_first = true;
    private int dsaitr_ierr[] = new int[1];
    private int dsaitr_ipj;
    private int dsaitr_irj;
    private int dsaitr_ivj;
    private int dsaitr_iter;
    private int dsaitr_itry;
    private int dsaitr_j;
    private int dsaitr_msglvl;
    private double dsaitr_rnorm1;
    private double dsaitr_safmin;
    private double dsaitr_wnorm;
    
    private double dsapps_epsmch;
    private boolean dsapps_first = true;
	// ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new SparseEigenvalue object.
     */
    public SparseEigenvalue() {}
    
    public void simpleEigenvalueTest1 (){
    	double A[][] = new double [3][3];
    	A[0][0] = 7.0;
    	A[0][1] = -2.0;
    	A[0][2] = 0.0;
    	A[1][0] = -2.0;
    	A[1][1] = 6.0;
    	A[1][2] = -2.0;
    	A[2][0] = 0.0;
    	A[2][1] = -2.0;
    	A[2][2] = 5.0;
    	int nev = 2;
    	int ncv = 3;
    	double d[] = new double[nev];
    	double Z[][] = new double[3][nev];
    	String which = "SM";
    	double tol[] = new double[]{0.0};
    	simpleEigenvalue(A, nev, which, tol, ncv, d, Z);
    	// Answer are eigenvalues of 3, 6, and 9
    	// Eigenvectors:
    	// (1/3, 2/3, 2/3)
    	// (-2/3, -1/3, -2/3)
    	// (2/3,-2/3, 1/3)
    	// String which = "LA"  and which = "LM" give:
    	// Eigenvalues of 6 and 9
    	// Eigenvectors:
    	// (2/3, 1/3, -2/3)
    	// (-2/3, 2/3, -1/3)
    	// String which = "SA" and which = "SM"  and which = "BE" give:
    	// Eigenvalues of 3 and 6
    	// Eigenvectors:
    	// (-1/3, -2/3, -2/3)
    	// (2/3, 1/3, -2/3)
    	return;
    }
    
    /**
     * Calculate requested eigenvalues and eigenvectors of a matrix
     * @param A Symmetric real matrix for which eigenvalues and eigenvectors will be calculated
     *        n = A.length = A[0].length
     * @param nev Number of eigenvalues and accompanying eigenvectors to be calculated
     * @param which WHICH must be one of "LM", "SM", "LA", "SA" or "BE"
     *        largest and smallest magnitudes, largest and smallest amplitudes,
     *        both ends of the spectrum
     * @param tol determines the stopping criterion.                                                    
     *       Expect                                         |
     *                  abs(lambdaC - lambdaT) < tol*abs(lambdaC)
     *                                 computed   true                                                      
     *                  If tol .le. 0,  then tol <- macheps
     *                  (machine precision) is used.
     * @param ncv The largest number of basis vectors that will be used in the Implicitly Restarted 
     *            Arnoldi Process.  Work per major iteration is proportional to n*ncv*ncv. 
     *            As a rule of thumb, ncv >= 2 * nev is reasonable.
     *            ncv must be >= nev+1 and ncv must be <= n.     
     * @param d User supplied array of size nev will hold the calculated eigenvalues
     * @param Z User supplied array of size [A.length][nev] to hold the calculated eigenvalues  
     */
    public void simpleEigenvalue(double A[][], int nev, String which, double tol[], int ncv,
    		double d[], double Z[][]) {
       int n = A.length;
       int ldv = n;
       int ldz = n;
       int lworkl = ncv*(ncv+8);
       double v[][] = new double[ldv][ncv];
       double workl[] = new double[lworkl];
       double workd[] = new double[3*n];
       double resid[] = new double[n];
       boolean select[] = new boolean[ncv];
       int iparam[] = new int[11];
       int ipntr[] = new int[11];
  
  //     %---------------%
  //     | Local Scalars |
  //     %---------------%
       
       String bmat;
       int ido[] = new int[1];
       int info[] = new int[1];
       int ierr[] = new int[1];
       int nconv = 0;
       int        i, j, 
                    maxitr, mode1, ishfts;
       boolean          rvec;
       // sigma not intialized in dssimp
       double sigma = 0.0;
       double v1[];
       double v2[];
       
//     %-------------------------------------------------%
//     | The following include statement and assignments |
//     | initiate trace output from the internal         |
//     | actions of ARPACK.  See debug.doc in the        |
//     | DOCUMENTS directory for usage.  Initially, the  |
//     | most useful information will be a breakdown of  |
//     | time spent in the various stages of computation |
//     | given by setting msaupd = 1.                    |
//     %-------------------------------------------------%

    msgets = 0;
    msaitr = 0; 
    msapps = 0;
    msaupd = 1;
    msaup2 = 0;
    mseigt = 0;
    mseupd = 0;

// bmat = "I" indicates a standard problem    
    bmat  = "I";

    

//     %-----------------------------------------------------%
//     |                                                     |
//     | Specification of stopping rules and initial         |
//     | conditions before calling DSAUPD                    |
//     |                                                     |
//     | IDO  is the REVERSE COMMUNICATION parameter         |
//     |      used to specify actions to be taken on return  |
//     |      from DSAUPD. (See usage below.)                |
//     |                                                     |
//     |      It MUST initially be set to 0 before the first |
//     |      call to DSAUPD.                                | 
//     |                                                     |
//     | INFO on entry specifies starting vector information |
//     |      and on return indicates error codes            |
//     |                                                     |
//     |      Initially, setting INFO=0 indicates that a     | 
//     |      random starting vector is requested to         |
//     |      start the ARNOLDI iteration.  Setting INFO to  |
//     |      a nonzero value on the initial call is used    |
//     |      if you want to specify your own starting       |
//     |      vector (This vector must be placed in RESID.)  | 
//     |                                                     |
//     | The work array WORKL is used in DSAUPD as           | 
//     | workspace.  Its dimension LWORKL is set as          |
//     | illustrated below.                                  |
//     |                                                     |
//     %-----------------------------------------------------%

    info[0] = 0;
    ido[0] = 0;

//     %---------------------------------------------------%
//     | Specification of Algorithm Mode:                  |
//     |                                                   |
//     | This program uses the exact shift strategy        |
//     | (indicated by setting PARAM(1) = 1).              |
//     | IPARAM(3) specifies the maximum number of Arnoldi |
//     | iterations allowed.  Mode 1 of DSAUPD is used     |
//     | (IPARAM(7) = 1). All these options can be changed |
//     | by the user. For details see the documentation in |
//     | DSAUPD.                                           |
//     %---------------------------------------------------%

    ishfts = 1;
    maxitr = 300; 
    mode1 = 1;

    iparam[0] = ishfts;
                
    iparam[2] = maxitr;
                  
    iparam[6] = mode1;

//     %------------------------------------------------%
//     | M A I N   L O O P (Reverse communication loop) |
//     %------------------------------------------------%

    v1 = new double[n];
    v2 = new double[n];   
    while (true) {

//        %---------------------------------------------%
//        | Repeatedly call the routine DSAUPD and take | 
//        | actions indicated by parameter IDO until    |
//        | either convergence is indicated or maxitr   |
//        | has been exceeded.                          |
//        %---------------------------------------------%

        dsaupd ( ido, bmat, n, which, nev, tol, resid, 
                 ncv, v, ldv, iparam, ipntr, workd, workl,
                 lworkl, info );

		if (ido[0] != -1 && ido[0] != 1) {
			 break;
		}

//           %--------------------------------------%
//           | Perform matrix vector multiplication |
//           |              y <--- OP*x             |
//           | The user should supply his/her own   |
//           | matrix vector multiplication routine |
//           | here that takes workd(ipntr(1)) as   |
//           | the input, and return the result to  |
//           | workd(ipntr(2)).                     |
//           %--------------------------------------%
//
		for (i = 0; i < n; i++) {
        	v1[i] = workd[ipntr[0]-1+i];
        }
        matMult(n, A, v1, v2);
		for (i = 0; i < n; i++) {
			workd[ipntr[1]-1+i] = v2[i];
		}

//           %-----------------------------------------%
//           | L O O P   B A C K to call DSAUPD again. |
//           %-----------------------------------------%
       } // while (true)

//     %----------------------------------------%
//     | Either we have convergence or there is |
//     | an error.                              |
//     %----------------------------------------%

    if ( info[0] < 0 ) {

//        %--------------------------%
//        | Error message. Check the |
//        | documentation in DSAUPD. |
//        %--------------------------%

       UI.setDataText("Error with dsaupd, info[0] = " + info[0] + "\n");
    } // if (info[0] < 0)
    else { // info[0] >= 0

//        %-------------------------------------------%
//        | No fatal errors occurred.                 |
//        | Post-Process using DSEUPD.                |
//        |                                           |        | Computed eigenvalues may be extracted.    |  
//        |                                           |
//        | Eigenvectors may be also computed now if  |
//        | desired.  (indicated by rvec = .true.)    | 
//        |                                           |
//        | The routine DSEUPD now called to do this  |
//        | post processing (Other modes may require  |
//        | more complicated post processing than     |
//        | mode1.)                                   |
//        |                                           |
//        %-------------------------------------------%
           
        rvec = true;

        dseupd ( rvec, "A", select, d, Z, ldz, sigma, 
                bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                iparam, ipntr, workd, workl, lworkl, ierr );
		

//         %----------------------------------------------%
//         | Eigenvalues are returned in the first column |
//         | of the two dimensional array D and the       |
//         | corresponding eigenvectors are returned in   |
//         | the first NCONV (=IPARAM(5)) columns of the  |
//         | two dimensional array V if requested.        |
//         | Otherwise, an orthogonal basis for the       |
//         | invariant subspace corresponding to the      |
//         | eigenvalues in D is returned in V.           |
//         %----------------------------------------------%

        if ( ierr[0] != 0) {

//            %------------------------------------%
//            | Error condition:                   |
//            | Check the documentation of DSEUPD. |
//            %------------------------------------%

           UI.setDataText("Error with dseupd, ierr[0] = " + ierr[0] + "\n");
        } // if( ierr[0] != 0)
        else { // ierr[0] == 0

        	nconv =  iparam[4];
        	for (i = 0; i < nconv; i++) {
        		UI.setDataText("Eigenvalue # " + (i+1) + " = " + nf.format(d[i]) + "\n");
        		UI.setDataText("Eigenvector " + (i+1) + " = ");
        		for (j = 0; j < n-1; j++) {
        			UI.setDataText(nf.format(Z[j][i]) + "  ");
        		}
        		UI.setDataText(nf.format(Z[n-1][i]) + "\n");
        	}

        } // else ierr[0] == 0
        
  //        %------------------------------------------%
  //        | Print additional convergence information |
  //        %------------------------------------------%
  
           if ( info[0] == 1) {
          	UI.setDataText("Maximum number of iterations reached.\n");
           }
           else if ( info[0] == 3) {
          	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
           }     
  
           UI.setDataText("\n");
           UI.setDataText("simple Eigevalue\n");
           UI.setDataText("======\n");
           UI.setDataText("\n");
           UI.setDataText("Size of the matrix = " +  n + "\n");
           UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
           UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
           UI.setDataText("What portion of the spectrum: " +  which + "\n");
           UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
           UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
           UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
           UI.setDataText("The convergence criterion = " + tol[0] + "\n");
           UI.setDataText("\n");
    } // else info[0] >= 0

     return;
    }
    
    private void matMult(int n, double A[][], double x[], double y[]) {
    	int i, j;
    	for (i = 0; i < n; i++) {
    		y[i] = 0.0;
    	}
    	for (i = 0; i < n; i++) {
    		for (j = 0; j < n; j++) {
    			y[i] = y[i] + A[i][j]*x[j];
    		}
    	}
    	return;
    }
    
    public void dssimp() {
    	
// Test passes:
// Values from running FORTRAN original:
//    	Ritz values and relative residuals
//    	 ----------------------------------
//    	               Col   1       Col   2
//    	  Row   1:    8.91167D+02   1.16872D-15
//    	  Row   2:    9.19781D+02   1.57393D-15
//    	  Row   3:    9.19781D+02   2.14076D-15
//    	  Row   4:    9.48395D+02   1.42151D-15
    	  
    	  
//    	  _SSIMP 
//    	  ====== 
    	  
//    	  Size of the matrix is          100
//    	  The number of Ritz values requested is            4
//    	  The number of Arnoldi vectors generated (NCV) is           20
//    	  What portion of the spectrum: LM
//    	  The number of converged Ritz values is            4
//    	  The number of Implicit Arnoldi update iterations taken is            8
//    	  The number of OP*x is          125
//    	  The convergence criterion is    1.1102230246251565E-016

    	
// Values from running Java port:
//    	Ritz values and relative residuals: 
//    		d[0][0] = 8.91167E2 d[0][1] = 9.02859E-16
//    		d[1][0] = 9.19781E2 d[1][1] = 1.09065E-15
//    		d[2][0] = 9.19781E2 d[2][1] = 2.34073E-15
//    		d[3][0] = 9.48395E2 d[3][1] = 1.18950E-15

//    		DSSIMP
//    		======

//    		Size of the matrix = 100
//    		The number of Ritz values requested = 4
//    		The number of Arnoldi vectors generated ncv = 20
//    		What portion of the spectrum: LM
//    		The number of converged Ritz values = 4
//    		The number of Implicit Arnoldi update iterations taken = 8
//    		The number of OP*x = 125
//    		The convergence criterion = 1.1102230246251565E-16

//     This example program is intended to illustrate the 
//     simplest case of using ARPACK in considerable detail.  
//     This code may be used to understand basic usage of ARPACK
//     and as a template for creating an interface to ARPACK.  
   
//     This code shows how to use ARPACK to find a few eigenvalues 
//     (lambda) and corresponding eigenvectors (x) for the standard 
//     eigenvalue problem:
          
//                        A*x = lambda*x
 
//     where A is an n by n real symmetric matrix.

//     The main points illustrated here are 

//        1) How to declare sufficient memory to find NEV 
//           eigenvalues of largest magnitude.  Other options
//           are available.

//        2) Illustration of the reverse communication interface 
//           needed to utilize the top level ARPACK routine DSAUPD 
//           that computes the quantities needed to construct
//           the desired eigenvalues and eigenvectors(if requested).

//        3) How to extract the desired eigenvalues and eigenvectors
//           using the ARPACK routine DSEUPD.

//     The only thing that must be supplied in order to use this
//     routine on your problem is to change the array dimensions 
//     appropriately, to specify WHICH eigenvalues you want to compute 
//     and to supply a matrix-vector product

//                         w <-  Av

//     in place of the call to avsimp( ) below.

//     Once usage of this routine is understood, you may wish to explore
//     the other available options to improve convergence, to solve generalized
//     problems, etc.  Look at the file ex-sym.doc in DOCUMENTS directory.
//     This codes implements  

// \Example-1
//     ... Suppose we want to solve A*x = lambda*x in regular mode,
//         where A is derived from the central difference discretization
//         of the 2-dimensional Laplacian on the unit square with
//         zero Dirichlet boundary condition.
//     ... OP = A  and  B = I.
//     ... Assume "call avsimp (n,x,y)" computes y = A*x
//     ... Use mode 1 of DSAUPD.

// \BeginLib

// \Routines called:
//     dsaupd  ARPACK reverse communication interface routine.
//     dseupd  ARPACK routine that returns Ritz values and (optionally)
//             Ritz vectors.
//     dnrm2   Level 1 BLAS that computes the norm of a vector.
//     daxpy   Level 1 BLAS that computes y <- alpha*x+y.

// \Author
//     Richard Lehoucq
//     Danny Sorensen
//     Chao Yang
//     Dept. of Computational &
//     Applied Mathematics
//     Rice University
//     Houston, Texas

// \SCCS Information: @(#)
// FILE: ssimp.F   SID: 2.5   DATE OF SID: 9/5/96   RELEASE: 2

// \Remarks
//     1. None

// \EndLib

// -----------------------------------------------------------------------

//     %------------------------------------------------------%
//     | Storage Declarations:                                |
//     |                                                      |
//     | The maximum dimensions for all arrays are            |
//     | set here to accommodate a problem size of            |
//     | N .le. MAXN                                          |
//     |                                                      |
//     | NEV is the number of eigenvalues requested.          |
//     |     See specifications for ARPACK usage below.       |
//     |                                                      |
//     | NCV is the largest number of basis vectors that will |
//     |     be used in the Implicitly Restarted Arnoldi      |
//     |     Process.  Work per major iteration is            |
//     |     proportional to N*NCV*NCV.                       |
//     |                                                      |
//     | You must set:                                        |
//     |                                                      |
//     | MAXN:   Maximum dimension of the A allowed.          |
//     | MAXNEV: Maximum NEV allowed.                         |
//     | MAXNCV: Maximum NCV allowed.                         |
//     %------------------------------------------------------%

    
      final int maxn = 256;
  	  final int maxnev = 10;
  	  final int maxncv = 25;
  	  final int ldv = maxn;
  
  //     %--------------%
  //     | Local Arrays |
  //     %--------------%
  //
       double v[][] = new double[ldv][maxncv];
       double workl[] = new double[maxncv*(maxncv+8)];
       double workd[] = new double[3*maxn];
       double d[][] = new double[maxncv][2];
       double resid[] = new double[maxn];
       double ax[] = new double[maxn];
       boolean select[] = new boolean[maxncv];
       int iparam[] = new int[11];
       int ipntr[] = new int[11];
       double ds[] = new double[2 * maxncv];
  
  //     %---------------%
  //     | Local Scalars |
  //     %---------------%
       
       String bmat;
       String which;
       int ido[] = new int[1];
       int info[] = new int[1];
       int ierr[] = new int[1];
       int nconv = 0;
       int          n, nev, ncv, lworkl, i, j, 
                    nx, maxitr, mode1, ishfts;
       boolean          rvec;
       double tol[] = new double[]{0.0};
       // sigma not intialized in dssimp
       double sigma = 0.0;
       double v1[];
       double v2[];
       int index;
   
 //     %-----------------------------%
 //     | BLAS & LAPACK routines used |
 //     %-----------------------------%
 
 //      Double precision           
 //     &                 dnrm2
 //      external         dnrm2, daxpy
 
 //     %--------------------%
 //     | Intrinsic function |
 //     %--------------------%
 
 //      intrinsic        abs
 
 //     %-----------------------%
 //     | Executable Statements |
 //     %-----------------------%
    	
//     %-------------------------------------------------%
//     | The following include statement and assignments |
//     | initiate trace output from the internal         |
//     | actions of ARPACK.  See debug.doc in the        |
//     | DOCUMENTS directory for usage.  Initially, the  |
//     | most useful information will be a breakdown of  |
//     | time spent in the various stages of computation |
//     | given by setting msaupd = 1.                    |
//     %-------------------------------------------------%

    msgets = 0;
    msaitr = 0; 
    msapps = 0;
    msaupd = 1;
    msaup2 = 0;
    mseigt = 0;
    mseupd = 0;
     
//     %-------------------------------------------------%
//     | The following sets dimensions for this problem. |
//     %-------------------------------------------------%

    nx = 10;
    n = nx*nx;

//     %-----------------------------------------------%
//     |                                               | 
//     | Specifications for ARPACK usage are set       | 
//     | below:                                        |
//     |                                               |
//     |    1) NEV = 4  asks for 4 eigenvalues to be   |  
//     |       computed.                               | 
//     |                                               |
//     |    2) NCV = 20 sets the length of the Arnoldi |
//     |       factorization                           |
//     |                                               |
//     |    3) This is a standard problem              |
//     |         (indicated by bmat  = 'I')            |
//     |                                               |
//     |    4) Ask for the NEV eigenvalues of          |
//     |       largest magnitude                       |
//     |         (indicated by which = 'LM')           |
//     |       See documentation in DSAUPD for the     |
//     |       other options SM, LA, SA, LI, SI.       | 
//     |                                               |
//     | Note: NEV and NCV must satisfy the following  |
//     | conditions:                                   |
//     |              NEV <= MAXNEV                    |
//     |          NEV + 1 <= NCV <= MAXNCV             |
//     %-----------------------------------------------%

    nev   = 4;
    ncv   = 20; 
    bmat  = "I";
    which = "LM";

    if ( n > maxn ) {
        UI.setDataText("ERROR with DSSIMP: N is greater than MAXN");
        return;
     }
     else if ( nev > maxnev ) {
   	  UI.setDataText("ERROR with DSSIMP: NEV is greater than MAXNEV");
         return;
     }
     else if ( ncv > maxncv ) {
        UI.setDataText("ERROR with DSSIMP: NCV is greater than MAXNCV");
        return;
     }

//     %-----------------------------------------------------%
//     |                                                     |
//     | Specification of stopping rules and initial         |
//     | conditions before calling DSAUPD                    |
//     |                                                     |
//     | TOL  determines the stopping criterion.             |
//     |                                                     |
//     |      Expect                                         |
//     |           abs(lambdaC - lambdaT) < TOL*abs(lambdaC) |
//     |               computed   true                       |
//     |                                                     |
//     |      If TOL .le. 0,  then TOL <- macheps            |
//     |           (machine precision) is used.              |
//     |                                                     |
//     | IDO  is the REVERSE COMMUNICATION parameter         |
//     |      used to specify actions to be taken on return  |
//     |      from DSAUPD. (See usage below.)                |
//     |                                                     |
//     |      It MUST initially be set to 0 before the first |
//     |      call to DSAUPD.                                | 
//     |                                                     |
//     | INFO on entry specifies starting vector information |
//     |      and on return indicates error codes            |
//     |                                                     |
//     |      Initially, setting INFO=0 indicates that a     | 
//     |      random starting vector is requested to         |
//     |      start the ARNOLDI iteration.  Setting INFO to  |
//     |      a nonzero value on the initial call is used    |
//     |      if you want to specify your own starting       |
//     |      vector (This vector must be placed in RESID.)  | 
//     |                                                     |
//     | The work array WORKL is used in DSAUPD as           | 
//     | workspace.  Its dimension LWORKL is set as          |
//     | illustrated below.                                  |
//     |                                                     |
//     %-----------------------------------------------------%

    lworkl = ncv*(ncv+8);
    info[0] = 0;
    ido[0] = 0;

//     %---------------------------------------------------%
//     | Specification of Algorithm Mode:                  |
//     |                                                   |
//     | This program uses the exact shift strategy        |
//     | (indicated by setting PARAM(1) = 1).              |
//     | IPARAM(3) specifies the maximum number of Arnoldi |
//     | iterations allowed.  Mode 1 of DSAUPD is used     |
//     | (IPARAM(7) = 1). All these options can be changed |
//     | by the user. For details see the documentation in |
//     | DSAUPD.                                           |
//     %---------------------------------------------------%

    ishfts = 1;
    maxitr = 300; 
    mode1 = 1;

    iparam[0] = ishfts;
                
    iparam[2] = maxitr;
                  
    iparam[6] = mode1;

//     %------------------------------------------------%
//     | M A I N   L O O P (Reverse communication loop) |
//     %------------------------------------------------%

    v1 = new double[nx*nx];
    v2 = new double[nx*nx];   
    while (true) {

//        %---------------------------------------------%
//        | Repeatedly call the routine DSAUPD and take | 
//        | actions indicated by parameter IDO until    |
//        | either convergence is indicated or maxitr   |
//        | has been exceeded.                          |
//        %---------------------------------------------%

        dsaupd ( ido, bmat, n, which, nev, tol, resid, 
                 ncv, v, ldv, iparam, ipntr, workd, workl,
                 lworkl, info );

		if (ido[0] != -1 && ido[0] != 1) {
			 break;
		}

//           %--------------------------------------%
//           | Perform matrix vector multiplication |
//           |              y <--- OP*x             |
//           | The user should supply his/her own   |
//           | matrix vector multiplication routine |
//           | here that takes workd(ipntr(1)) as   |
//           | the input, and return the result to  |
//           | workd(ipntr(2)).                     |
//           %--------------------------------------%
//
        for (i = 0; i < nx*nx; i++) {
        	v1[i] = workd[ipntr[0]-1+i];
        }
		avsimp (nx, v1, v2);
		for (i = 0; i < nx*nx; i++) {
			workd[ipntr[1]-1+i] = v2[i];
		}

//           %-----------------------------------------%
//           | L O O P   B A C K to call DSAUPD again. |
//           %-----------------------------------------%
       } // while (true)

//     %----------------------------------------%
//     | Either we have convergence or there is |
//     | an error.                              |
//     %----------------------------------------%

    if ( info[0] < 0 ) {

//        %--------------------------%
//        | Error message. Check the |
//        | documentation in DSAUPD. |
//        %--------------------------%

       UI.setDataText("Error with _daupd, info[0] = " + info[0] + "\n");
    } // if (info[0] < 0)
    else { // info[0] >= 0

//        %-------------------------------------------%
//        | No fatal errors occurred.                 |
//        | Post-Process using DSEUPD.                |
//        |                                           |//c        | Computed eigenvalues may be extracted.    |  
//        |                                           |
//        | Eigenvectors may be also computed now if  |
//        | desired.  (indicated by rvec = .true.)    | 
//        |                                           |
//        | The routine DSEUPD now called to do this  |
//        | post processing (Other modes may require  |
//        | more complicated post processing than     |
//        | mode1.)                                   |
//        |                                           |
//        %-------------------------------------------%
           
        rvec = true;

        dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                iparam, ipntr, workd, workl, lworkl, ierr );
		index = 0;
		for (j = 0; j < 2; j++) {
			 for (i = 0; i < maxncv; i++) {
			     d[i][j] = ds[index++]; 
			 }
		}

//         %----------------------------------------------%
//         | Eigenvalues are returned in the first column |
//         | of the two dimensional array D and the       |
//         | corresponding eigenvectors are returned in   |
//         | the first NCONV (=IPARAM(5)) columns of the  |
//         | two dimensional array V if requested.        |
//         | Otherwise, an orthogonal basis for the       |
//         | invariant subspace corresponding to the      |
//         | eigenvalues in D is returned in V.           |
//         %----------------------------------------------%

        if ( ierr[0] != 0) {

//            %------------------------------------%
//            | Error condition:                   |
//            | Check the documentation of DSEUPD. |
//            %------------------------------------%

           UI.setDataText("Error with dseupd, ierr[0] = " + ierr[0] + "\n");
        } // if( ierr[0] != 0)
        else { // ierr[0] == 0

           nconv =  iparam[4];
           v1 = new double[nx*nx];
           for (j = 0; j < nconv; j++) {

//               %---------------------------%
//               | Compute the residual norm |
//               |                           |
//               |   ||  A*x - lambda*x ||   |
//               |                           |
//               | for the NCONV accurately  |
//               | computed eigenvalues and  |
//               | eigenvectors.  (iparam(5) |
//               | indicates how many are    |
//               | accurate to the requested |
//               | tolerance)                |
//               %---------------------------%

              for (i = 0; i < nx*nx; i++) {
            	  v1[i] = v[i][j];
              }
        	  avsimp(nx, v1, ax);
        	  for (i = 0; i < n; i++) {
        		  ax[i] = ax[i] + (-d[j][0])*v[i][j];
        	  }
              d[j][1] = ge.dnrm2(n, ax, 1);
              d[j][1] = d[j][1] / Math.abs(d[j][0]);

           } // for (j = 0; j < nconv; j++)
           
//         %-------------------------------%
//         | Display computed residuals    |
//         %-------------------------------%
           UI.setDataText("Ritz values and relative residuals: \n");
           for (i = 0; i < nconv; i++) {
       	       UI.setDataText("d["+i+"][0] = " + nf.format(d[i][0]) + " d["+i+"][1] = " + nf.format(d[i][1]) + "\n");
           }

        } // else ierr[0] == 0
        
  //        %------------------------------------------%
  //        | Print additional convergence information |
  //        %------------------------------------------%
  
           if ( info[0] == 1) {
          	UI.setDataText("Maximum number of iterations reached.\n");
           }
           else if ( info[0] == 3) {
          	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
           }     
  
           UI.setDataText("\n");
           UI.setDataText("DSSIMP\n");
           UI.setDataText("======\n");
           UI.setDataText("\n");
           UI.setDataText("Size of the matrix = " +  n + "\n");
           UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
           UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
           UI.setDataText("What portion of the spectrum: " +  which + "\n");
           UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
           UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
           UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
           UI.setDataText("The convergence criterion = " + tol[0] + "\n");
           UI.setDataText("\n");
    } // else info[0] >= 0

     return;
    } // dssimp
    


 // ------------------------------------------------------------------
//      matrix vector subroutine

//      The matrix used is the 2 dimensional discrete Laplacian on unit
//      square with zero Dirichlet boundary condition.

//      Computes w <--- OP*v, where OP is the nx*nx by nx*nx block 
//      tridiagonal matrix

//                   | T -I          | 
//                   |-I  T -I       |
//              OP = |   -I  T       |
//                   |        ...  -I|
//                   |           -I T|
 //
//      The subroutine tvsimp is called to computed y<---T*x.

     private void avsimp (int nx, double v[], double w[]) {
     int           i, j, lo, n2;
     //Double precision
     //&                  v(nx*nx), w(nx*nx)
     final double one = 1.0;
     final double h2;
     double v1[] = new double[nx];
     double v2[] = new double[nx];
    
     tvsimp(nx,v,w);
     for (i = 0; i < nx; i++) {
     	w[i] = w[i] + (-one)*v[nx+i];
     }
     for ( j =2; j <= nx-1; j++) {
        lo = (j-1)*nx;
        for (i = 0; i < nx; i++) {
     	   v1[i] = v[lo+i];
        }
        tvsimp(nx, v1, v2);
        for (i = 0; i < nx; i++) {
     	   w[lo+i] = v2[i];
        }
        for (i = 0; i < nx; i++) {
     	   w[lo+i] = w[lo+i] + (-one) * v[lo-nx+i];
        }
        for (i = 0; i < nx; i++) {
     	   w[lo+i] = w[lo+i] + (-one) * v[lo+nx+i];
        }
     } // for ( j =2; j <= nx-1; j++)

     lo = (nx-1)*nx;
     for (i = 0; i < nx; i++) {
  	   v1[i] = v[lo+i];
     }
     tvsimp(nx, v1, v2);
     for (i = 0; i < nx; i++) {
  	   w[lo+i] = v2[i];
     }
     for (i = 0; i < nx; i++) {
     	w[lo+i] = w[lo+i] + (-one)*v[lo-nx+i];
     }

//      Scale the vector w by (1/h^2), where h is the mesh size

     n2 = nx*nx;
     h2 = one / (double)((nx+1)*(nx+1));
     for (i = 0; i < n2; i++) {
     	w[i] = (one/h2) * w[i];
     }
     return;
     } // avsimp

 // -------------------------------------------------------------------
     private void tvsimp (int nx, double x[], double y[]) {

     int           j; 
     //Double precision
     //&                  x(nx), y(nx),
     final double one = 1.0;
     final double four = 4.0;
     double dd, dl, du;

//      Compute the matrix vector multiplication y<---T*x
//      where T is a nx by nx tridiagonal matrix with DD on the 
//      diagonal, DL on the subdiagonal, and DU on the superdiagonal.
      

     dd  = four; 
     dl  = -one; 
     du  = -one;
  
     y[0] =  dd*x[0] + du*x[1];
     for (j = 1; j < nx-1; j++) {
        y[j] = dl*x[j-1] + dd*x[j] + du*x[j+1]; 
     } // for (j = 1; j < nx - 1; j++)
     y[nx-1] =  dl*x[nx-2] + dd*x[nx-1]; 
     return;
     } // tvsimp


    
    public void dsvd() {
// Values from running FORTRAN original:
//    	 Singular values and direct residuals
//    	 ------------------------------------
//    	               Col   1       Col   2
//    	  Row   1:    4.10123D-02   9.69610D-18
//    	  Row   2:    6.04881D-02   6.28624D-18
//    	  Row   3:    1.17844D-01   1.09866D-16
//    	  Row   4:    5.57234D-01   1.26958D-16
    	  
    	  
//    	  _SVD 
//    	  ==== 
    	  
//    	  Size of the matrix is          100
//    	  The number of Ritz values requested is            4
//    	  The number of Arnoldi vectors generated (NCV) is           10
//    	  What portion of the spectrum: LM
//    	  The number of converged Ritz values is            4
//    	  The number of Implicit Arnoldi update iterations taken is            4
//    	  The number of OP*x is           22
//    	  The convergence criterion is    1.1102230246251565E-016
    	
// Values from running Java port:
//    	Singular values and direct residuals: 
//    		s[0][0] = 4.10123E-2 s[0][1] = 7.50635E-18
//    		s[1][0] = 6.04881E-2 s[1][1] = 2.10185E-17
//    		s[2][0] = 1.17844E-1 s[2][1] = 5.11551E-17
//    		s[3][0] = 5.57234E-1 s[3][1] = 7.72810E-16

//    		DSVD
//    		======

//    		Size of the matrix = 100
//    		The number of Ritz values requested = 4
//    		The number of Arnoldi vectors generated ncv = 10
//    		What portion of the spectrum: LM
//    		The number of converged Ritz values = 4
//    		The number of Implicit Arnoldi update iterations taken = 4
//    		The number of OP*x = 22
//    		The convergence criterion = 1.1102230246251565E-16

//     This example program is intended to illustrate the 
//     the use of ARPACK to compute the Singular Value Decomposition.
   
//     This code shows how to use ARPACK to find a few of the
//     largest singular values(sigma) and corresponding right singular 
//     vectors (v) for the the matrix A by solving the symmetric problem:
          
//                        (A'*A)*v = sigma*v
 
//     where A is an m by n real matrix.

//     This code may be easily modified to estimate the 2-norm
//     condition number  largest(sigma)/smallest(sigma) by setting
//     which = 'BE' below.  This will ask for a few of the smallest
//     and a few of the largest singular values simultaneously.
//     The condition number could then be estimated by taking
//     the ratio of the largest and smallest singular values.

//     This formulation is appropriate when  m  .ge.  n.
//     Reverse the roles of A and A' in the case that  m .le. n.

//     The main points illustrated here are 

//        1) How to declare sufficient memory to find NEV 
//           largest singular values of A .  

//        2) Illustration of the reverse communication interface 
//           needed to utilize the top level ARPACK routine DSAUPD 
//           that computes the quantities needed to construct
//           the desired singular values and vectors(if requested).

//        3) How to extract the desired singular values and vectors
//           using the ARPACK routine DSEUPD.

//        4) How to construct the left singular vectors U from the 
//           right singular vectors V to obtain the decomposition

//                        A*V = U*S

//           where S = diag(sigma_1, sigma_2, ..., sigma_k).

//     The only thing that must be supplied in order to use this
//     routine on your problem is to change the array dimensions 
//     appropriately, to specify WHICH singular values you want to 
//     compute and to supply a the matrix-vector products 

//                         w <-  Ax
//                         y <-  A'w

//     in place of the calls  to svdvd( ) and ATV( ) respectively below.  

//     Further documentation is available in the header of DSAUPD
//     which may be found in the SRC directory.

//     This codes implements

// \Example-1
//     ... Suppose we want to solve A'A*v = sigma*v in regular mode,
//         where A is derived from the simplest finite difference 
//         discretization of the 2-dimensional kernel  K(s,t)dt  where

//                 K(s,t) =  s(t-1)   if 0 .le. s .le. t .le. 1,
//                           t(s-1)   if 0 .le. t .lt. s .le. 1. 

//         See subroutines avsvd  and ATV for details.
//     ... OP = A'*A  and  B = I.
//     ... Assume "call avsvd (n,x,y)" computes y = A*x
//     ... Assume "call atv (n,y,w)" computes w = A'*y
//     ... Assume exact shifts are used
//     ...

// \BeginLib

// \Routines called:
//     dsaupd  ARPACK reverse communication interface routine.
//     dseupd  ARPACK routine that returns Ritz values and (optionally)
//             Ritz vectors.
//     dnrm2   Level 1 BLAS that computes the norm of a vector.
//     daxpy   Level 1 BLAS that computes y <- alpha*x+y.
//     dscal   Level 1 BLAS thst computes x <- x*alpha.
//     dcopy   Level 1 BLAS thst computes y <- x.

// \Author
//     Richard Lehoucq
//     Danny Sorensen
//     Chao Yang
//     Dept. of Computational &
//     Applied Mathematics
//     Rice University
//     Houston, Texas

// \SCCS Information: @(#)
// FILE: svd.F   SID: 2.3   DATE OF SID: 8/21/96   RELEASE: 2

// \Remarks
//     1. None

// \EndLib

// -----------------------------------------------------------------------
//
//     %------------------------------------------------------%
//     | Storage Declarations:                                |
//     |                                                      |
//     | It is assumed that A is M by N with M .ge. N.        |
//     |                                                      |
//     | The maximum dimensions for all arrays are            |
//     | set here to accommodate a problem size of            |
//     | M .le. MAXM  and  N .le. MAXN                        |
//     |                                                      |
//     | The NEV right singular vectors will be computed in   |
//     | the N by NCV array V.                                |
//     |                                                      |
//     | The NEV left singular vectors will be computed in    |
//     | the M by NEV array U.                                |
//     |                                                      |
//     | NEV is the number of singular values requested.      |
//     |     See specifications for ARPACK usage below.       |
//     |                                                      |
//     | NCV is the largest number of basis vectors that will |
//     |     be used in the Implicitly Restarted Arnoldi      |
//     |     Process.  Work per major iteration is            |
//     |     proportional to N*NCV*NCV.                       |
//     |                                                      |
//     | You must set:                                        |
//     |                                                      |
//     | MAXM:   Maximum number of rows of the A allowed.     |
//     | MAXN:   Maximum number of columns of the A allowed.  |
//     | MAXNEV: Maximum NEV allowed                          |
//     | MAXNCV: Maximum NCV allowed                          |
//     %------------------------------------------------------%

    final int maxm = 500;
    final int maxn = 250;
    final int maxnev = 10;
    final int maxncv = 25;
    final int ldu = maxm;
    final int ldv = maxn;
    
//     %--------------%
//     | Local Arrays |
//     %--------------%

    double v[][] = new double[ldv][maxncv];
    double u[][] = new double[ldu][maxnev];
    double workl[] = new double[maxncv*(maxncv+8)];
    double workd[] = new double[3*maxn];
    double s[][] = new double[maxncv][2];
    double ds[] = new double[2*maxncv];
    double resid[] = new double[maxn];
    double ax[] = new double[maxm];
    boolean select[] = new boolean[maxncv];
    int iparam[] = new int[11];
    int ipntr[] = new int[11];
    double v1[];
    
//     %---------------%
//     | Local Scalars |
//     %---------------%

    String bmat;
    String which;
    int ido[] = new int[1];
    int info[] = new int[1];
    int ierr[] = new int[1];
    int nconv = 0;
    int          m, n, nev, ncv, lworkl, index,
                 i, j, ishfts, maxitr, mode1;
    boolean          rvec;
    // sigma not specified
    double sigma = 0.0;      
    double tol[] = new double[]{0.0};
    double temp;

//     %------------%
//     | Parameters |
//     %------------%

    final double one = 1.0;
    
//     %-----------------------------%
//     | BLAS & LAPACK routines used |
//     %-----------------------------%

    //Double precision           
    //&                 dnrm2
    //external         dnrm2, daxpy, dcopy, dscal

//     %-----------------------%
//     | Executable Statements |
//     %-----------------------%

//     %-------------------------------------------------%
//     | The following include statement and assignments |
//     | initiate trace output from the internal         |
//     | actions of ARPACK.  See debug.doc in the        |
//     | DOCUMENTS directory for usage.  Initially, the  |
//     | most useful information will be a breakdown of  |
//     | time spent in the various stages of computation |
//     | given by setting msaupd = 1.                    |
//     %-------------------------------------------------%

    msgets = 0;
    msaitr = 0; 
    msapps = 0;
    msaupd = 1;
    msaup2 = 0;
    mseigt = 0;
    mseupd = 0;

//     %-------------------------------------------------%
//     | The following sets dimensions for this problem. |
//     %-------------------------------------------------%

    m = 500;
    n = 100;

//     %------------------------------------------------%
//     | Specifications for ARPACK usage are set        | 
//     | below:                                         |
//     |                                                |
//     |    1) NEV = 4 asks for 4 singular values to be |  
//     |       computed.                                | 
//     |                                                |
//     |    2) NCV = 20 sets the length of the Arnoldi  |
//     |       factorization                            |
//     |                                                |
//     |    3) This is a standard problem               |
//     |         (indicated by bmat  = 'I')             |
//     |                                                |
//     |    4) Ask for the NEV singular values of       |
//     |       largest magnitude                        |
//     |         (indicated by which = 'LM')            |
//     |       See documentation in DSAUPD for the      |
//     |       other options SM, BE.                    | 
//     |                                                |
//     | Note: NEV and NCV must satisfy the following   |
//     |       conditions:                              |
//     |                 NEV <= MAXNEV,                 |
//     |             NEV + 1 <= NCV <= MAXNCV           |
//     %------------------------------------------------%

    nev   = 4;
    ncv   = 10; 
    bmat  = "I";
    which = "LM";

    if ( n > maxn ) {
       UI.setDataText("ERROR with DSVD: N is greater than MAXN\n");
       return;
    }
    else if ( m > maxm ) {
       UI.setDataText("ERROR with DSVD: M is greater than MAXM\n");
       return;
    }
    else if ( nev > maxnev ) {
       UI.setDataText("ERROR with DSVD: NEV is greater than MAXNEV\n");
       return;
    }
    else if ( ncv > maxncv ) {
       UI.setDataText("ERROR with DSVD: NCV is greater than MAXNCV\n");
       return;
    }

//     %-----------------------------------------------------%
//     | Specification of stopping rules and initial         |
//     | conditions before calling DSAUPD                    |
//     |                                                     |
//     |           abs(sigmaC - sigmaT) < TOL*abs(sigmaC)    |
//     |               computed   true                       |
//     |                                                     |
//     |      If TOL .le. 0,  then TOL <- macheps            |
//     |              (machine precision) is used.           |
//     |                                                     |
//     | IDO  is the REVERSE COMMUNICATION parameter         |
//     |      used to specify actions to be taken on return  |
//     |      from DSAUPD. (See usage below.)                |
//     |                                                     |
//     |      It MUST initially be set to 0 before the first |
//     |      call to DSAUPD.                                | 
//     |                                                     |
//     | INFO on entry specifies starting vector information |
//     |      and on return indicates error codes            |
//     |                                                     |
//     |      Initially, setting INFO=0 indicates that a     | 
//     |      random starting vector is requested to         |
//     |      start the ARNOLDI iteration.  Setting INFO to  |
//     |      a nonzero value on the initial call is used    |
//     |      if you want to specify your own starting       |
//     |      vector (This vector must be placed in RESID.)  | 
//     |                                                     |
//     | The work array WORKL is used in DSAUPD as           | 
//     | workspace.  Its dimension LWORKL is set as          |
//     | illustrated below.                                  |
//     %-----------------------------------------------------%

    lworkl = ncv*(ncv+8); 
    info[0] = 0;
    ido[0] = 0;

//     %---------------------------------------------------%
//     | Specification of Algorithm Mode:                  |
//     |                                                   |
//     | This program uses the exact shift strategy        |
//     | (indicated by setting IPARAM(1) = 1.)             |
//     | IPARAM(3) specifies the maximum number of Arnoldi |
//     | iterations allowed.  Mode 1 of DSAUPD is used     |
//     | (IPARAM(7) = 1). All these options can be changed |
//     | by the user. For details see the documentation in |
//     | DSAUPD.                                           |
//     %---------------------------------------------------%

    ishfts = 1;
    maxitr = n;
    mode1 = 1;

    iparam[0] = ishfts;
                
    iparam[2] = maxitr;
                  
    iparam[6] = mode1;

//     %------------------------------------------------%
//     | M A I N   L O O P (Reverse communication loop) |
//     %------------------------------------------------%
  
    while (true) {

//        %---------------------------------------------%
//        | Repeatedly call the routine DSAUPD and take | 
//        | actions indicated by parameter IDO until    |
//        | either convergence is indicated or maxitr   |
//        | has been exceeded.                          |
//        %---------------------------------------------%

       dsaupd ( ido, bmat, n, which, nev, tol, resid, 
                ncv, v, ldv, iparam, ipntr, workd, workl,
                lworkl, info );

       if (ido[0] != -1 && ido[0] != 1) {
			 break;
		}

//           %---------------------------------------%
//           | Perform matrix vector multiplications |
//           |              w <--- A*x    (avsvd())  |
//           |              y <--- A'*w      (atv()) |
//           | The user should supply his/her own    |
//           | matrix vector multiplication routines |
//           | here that takes workd(ipntr(1)) as    |
//           | the input, and returns the result in  |
//           | workd(ipntr(2)).                      |
//           %---------------------------------------%
          v1 = new double[n];
          for (i = 0; i < n; i++) {
        	  v1[i] = workd[ipntr[0]-1+i];
          }
          avsvd (m, n, v1, ax); 
          atv (m, n, ax, v1);
          for (i = 0; i < n; i++) {
        	  workd[ipntr[1]-1+i]= v1[i];
          }

//           %-----------------------------------------%
//           | L O O P   B A C K to call DSAUPD again. |
//           %-----------------------------------------%
    } // while (true) 

//     %----------------------------------------%
//     | Either we have convergence or there is |
//     | an error.                              |
//     %----------------------------------------%

    if ( info[0] < 0 ) {

//        %--------------------------%
//        | Error message. Check the |
//        | documentation in DSAUPD. |
//        %--------------------------%

       UI.setDataText("Error with dsaupd, info[0] = " + info[0] + "\n");
    } // if (info[0] < 0)
    else { // info[0] >= 0

//        %--------------------------------------------%
//        | No fatal errors occurred.                  |
//        | Post-Process using DSEUPD.                 |
//        |                                            |
//        | Computed singular values may be extracted. |  
//        |                                            |
//        | Singular vectors may also be computed now  |
//        | if desired.  (indicated by rvec = .true.)  | 
//        |                                            |
//        | The routine DSEUPD now called to do this   |
//        | post processing                            | 
//        %--------------------------------------------%
//           
       rvec = true;

       dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
               iparam, ipntr, workd, workl, lworkl, ierr );
       index = 0;
		for (j = 0; j < 2; j++) {
			 for (i = 0; i < maxncv; i++) {
			     s[i][j] = ds[index++]; 
			 }
		}

//        %-----------------------------------------------%
//        | Singular values are returned in the first     |
//        | column of the two dimensional array S         |
//        | and the corresponding right singular vectors  | 
//        | are returned in the first NEV columns of the  |
//        | two dimensional array V as requested here.    |
//        %-----------------------------------------------%

       if ( ierr[0] != 0) {

//           %------------------------------------%
//           | Error condition:                   |
//           | Check the documentation of DSEUPD. |
//           %------------------------------------%

          UI.setDataText("Error with dseupd ierr[0] = " + ierr[0] + "\n");
       } // if (ierr[0] != 0
       else { // ierr[0] == 0

          nconv =  iparam[4];
          for (j = 0; j < nconv; j++) {

             s[j][0] = Math.sqrt(s[j][0]);

//              %-----------------------------%
//              | Compute the left singular   |
//              | vectors from the formula    |
//              |                             |
//              |     u = Av/sigma            |
//              |                             |
//              | u should have norm 1 so     |
//              | divide by norm(Av) instead. |
//              %-----------------------------%
             v1 = new double[n];
             for (i = 0; i < n; i++) {
            	 v1[i] = v[i][j];
             }
              avsvd(m, n, v1, ax);
              for (i = 0; i < m; i++) {
            	  u[i][j] = ax[i];
              }
              v1 = new double[m];
              for (i = 0; i < m; i++) {
            	  v1[i] = u[i][j];
              }
             temp = one/ge.dnrm2(m, v1, 1);
             for (i = 0; i < m; i++) {
            	 u[i][j] = temp * u[i][j];
             }

//              %---------------------------%
//              |                           |
//              | Compute the residual norm |
//              |                           |
//              |   ||  A*v - sigma*u ||    |
//              |                           |
//              | for the NCONV accurately  |
//              | computed singular values  |
//              | and vectors.  (iparam(5)  |
//              | indicates how many are    |
//              | accurate to the requested |
//              | tolerance).               |
//              | Store the result in 2nd   |
//              | column of array S.        |
//              %---------------------------%
//
             for (i = 0; i < m; i++) {
            	 ax[i] = ax[i] + (-s[j][0])*u[i][j];
             }
             s[j][1] = ge.dnrm2(m, ax, 1);
          } // for (j = 0; j < nconv; j++)
          
          
//        %-------------------------------%
//        | Display computed residuals    |
//        %-------------------------------%
          UI.setDataText("Singular values and direct residuals: \n");
          for (i = 0; i < nconv; i++) {
      	       UI.setDataText("s["+i+"][0] = " + nf.format(s[i][0]) + " s["+i+"][1] = " + nf.format(s[i][1]) + "\n");
          }
    } // ierr[0] == 0
    
//        %------------------------------------------%
//        | Print additional convergence information |
//        %------------------------------------------%

     if ( info[0] == 1) {
    	UI.setDataText("Maximum number of iterations reached.\n");
     }
     else if ( info[0] == 3) {
    	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
     }     

     UI.setDataText("\n");
     UI.setDataText("DSVD\n");
     UI.setDataText("======\n");
     UI.setDataText("\n");
     UI.setDataText("Size of the matrix = " +  n + "\n");
     UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
     UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
     UI.setDataText("What portion of the spectrum: " +  which + "\n");
     UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
     UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
     UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
     UI.setDataText("The convergence criterion = " + tol[0] + "\n");
     UI.setDataText("\n");

    } // else info[0] >= 0
    } // dsvd
 
// ------------------------------------------------------------------
//     matrix vector subroutines

//     The matrix A is derived from the simplest finite difference 
//     discretization of the integral operator 

//                     f(s) = integral(K(s,t)x(t)dt).
      
//     Thus, the matrix A is a discretization of the 2-dimensional kernel 
//     K(s,t)dt, where

//                 K(s,t) =  s(t-1)   if 0 .le. s .le. t .le. 1,
//                           t(s-1)   if 0 .le. t .lt. s .le. 1.

//     Thus A is an m by n matrix with entries

//                 A(i,j) = k*(si)*(tj - 1)  if i .le. j,
//                          k*(tj)*(si - 1)  if i .gt. j

//     where si = i/(m+1)  and  tj = j/(n+1)  and k = 1/(n+1).
      
// -------------------------------------------------------------------

    private void avsvd (int m, int n, double x[], double w[]) {

//     computes  w <- A*x

    int          i, j;
    //Double precision
    //&                 x(n), w(m)
    final double one = 1.0;
    final double zero = 0.0;
    double h, k, s, t;
    
    h = one / (double)(m+1);
    k = one / (double)(n+1);
    for (i = 0; i < m; i++) {
       w[i] = zero;
    }
    t = zero;
      
    for (j = 1; j <= n; j++) {
       t = t+k;
       s = zero;
       for (i = 1; i <= j; i++) {
         s = s+h;
         w[i-1] = w[i-1] + k*s*(t-one)*x[j-1];
       } // for (i = 1; i <= j; i++)
       for (i = j+1; i <= m; i++) {
         s = s+h;
         w[i-1] = w[i-1] + k*t*(s-one)*x[j-1]; 
       } // for (i = j+1; i <= m; i++)
    } // for (j = 1; j <= n; j++)     

    return;
    }

//-------------------------------------------------------------------

    private void atv (int m, int n, double w[], double y[]) {

//     computes  y <- A'*w

    int         i, j;
    //Double precision
    //&                w(m), y(n)
    final double one = 1.0;
    final double zero = 0.0;  
    double h, k, s, t;
    
    h = one / (double)(m+1);
    k = one / (double)(n+1);
    for (i = 0; i < n; i++) {
       y[i] = zero;
    }
    t = zero;

    for (j = 1; j <= n; j++) {
       t = t+k;
       s = zero;
       for (i = 1; i <= j; i++) {
         s = s+h;
         y[j-1] = y[j-1] + k*s*(t-one)*w[i-1];
       } // for (i = 1; i <= j; i++)
       for (i = j+1; i <= m; i++) {
         s = s+h;
         y[j-1] = y[j-1] + k*t*(s-one)*w[i-1];
       } // for (i = j+1; i <= m; i++)
    } // for (j = 1; j <= n; j++)

    return;
    } // atv
    
    public void dsdrv1() { 
    // At http://docs.roguewave.com/imsl/fortran/7.0/math/docs/arpacksymmetric.htm:
//    	Output
//    	Number of eigenvalues requested, and declared accurate
//   	 ------------------------------------------------------
//    	        5        0
//    	 Number of Matrix-Vector Products Recorded, EX-11
//    	 ------------------------------------------------
//    	        0
//    	 Smallest Laplacian Eigenvalues
//    	            1   19.61
//    	            2   48.22
//    	            3   48.22
//    	            4   76.83
//    	            5   93.33

   
// From running FORTRAN original:
//    	 Ritz values and relative residuals
//    	 ----------------------------------
//    	               Col   1       Col   2
//    	  Row   1:    1.96054D+01   2.00732D-14
//    	  Row   2:    4.82193D+01   7.24012D-15
//    	  Row   3:    4.82193D+01   7.52582D-15
//    	  Row   4:    7.68333D+01   5.27870D-15
    	  
    	  
//    	  _SDRV1 
//    	  ====== 
    	  
//    	  Size of the matrix is          100
//    	  The number of Ritz values requested is            4
//    	  The number of Arnoldi vectors generated (NCV) is           10
//    	  What portion of the spectrum: SM
//    	  The number of converged Ritz values is            4
//    	  The number of Implicit Arnoldi update iterations taken is           30
//    	  The number of OP*x is          153
//    	  The convergence criterion is    1.1102230246251565E-016

// When I change the original nev = 4; to nev = 5; and run I have:
//    	All Ritz Values and Vectors have small residuals.
//    	Ritz values and relative residuals: 
//    		d[0][0] = 1.96054E1 d[0][1] = 2.82139E-14
//    		d[1][0] = 4.82193E1 d[1][1] = 9.10478E-15
//    		d[2][0] = 4.82193E1 d[2][1] = 1.14049E-14
//    		d[3][0] = 7.68333E1 d[3][1] = 5.72781E-15
//    		d[4][0] = 9.33264E1 d[4][1] = 8.30138E-15

//    		DSDRV1
//    		======

//    		Size of the matrix = 100
//    		The number of Ritz values requested = 5
//    		The number of Arnoldi vectors generated ncv = 10
//    		What portion of the spectrum: SM
//    		The number of converged Ritz values = 5
//    		The number of Implicit Arnoldi update iterations taken = 65
//    		The number of OP*x = 235
//    		The convergence criterion = 1.1102230246251565E-16
    
    //     Simple program to illustrate the idea of reverse communication
    //     in regular mode for a standard symmetric eigenvalue problem.
    
    //     We implement example one of ex-sym.doc in SRC directory
    
    // \Example-1
    //     ... Suppose we want to solve A*x = lambda*x in regular mode,
    //         where A is derived from the central difference discretization
    //         of the 2-dimensional Laplacian on the unit square [0,1]x[0,1]
    //         with zero Dirichlet boundary condition.
    
    //     ... OP = A  and  B = I.
    
    //     ... Assume "call av (n,x,y)" computes y = A*x.
    
    //     ... Use mode 1 of DSAUPD.
    
    // \BeginLib
    
    // \Routines called:
    //     dsaupd  ARPACK reverse communication interface routine.
    //     dseupd  ARPACK routine that returns Ritz values and (optionally)
    //             Ritz vectors.
    //     dnrm2   Level 1 BLAS that computes the norm of a vector.
    //     daxpy   Level 1 BLAS that computes y <- alpha*x+y.
    //     av      Matrix vector multiplication routine that computes A*x.
    //     tv      Matrix vector multiplication routine that computes T*x, 
    //             where T is a tridiagonal matrix.  It is used in routine
    //             av.
    
    // \Author
    //     Richard Lehoucq
    //     Danny Sorensen
    //     Chao Yang
    //     Dept. of Computational &
    //     Applied Mathematics
    //     Rice University
    //     Houston, Texas
    
    // \SCCS Information: @(#)
    // FILE: sdrv1.F   SID: 2.4   DATE OF SID: 4/22/96   RELEASE: 2
    
    // \Remarks
    //     1. None
    
    // \EndLib
    
    // -----------------------------------------------------------------------
    
    //     %-----------------------------%
    //     | Define leading dimensions   |
    //     | for all arrays.             |
    //     | MAXN:   Maximum dimension   |
    //     |         of the A allowed.   |
    //     | MAXNEV: Maximum NEV allowed |
    //     | MAXNCV: Maximum NCV allowed |
    //     %-----------------------------%
    //
    	  final int maxn = 256;
    	  final int maxnev = 10;
    	  final int maxncv = 25;
    	  final int ldv = maxn;
    
    //     %--------------%
    //     | Local Arrays |
    //     %--------------%
    //
         double v[][] = new double[ldv][maxncv];
         double workl[] = new double[maxncv*(maxncv+8)];
         double workd[] = new double[3*maxn];
         double d[][] = new double[maxncv][2];
         double resid[] = new double[maxn];
         double ax[] = new double[maxn];
         boolean select[] = new boolean[maxncv];
         int iparam[] = new int[11];
         int ipntr[] = new int[11];
         double ds[] = new double[2 * maxncv];
    
    //     %---------------%
    //     | Local Scalars |
    //     %---------------%
    
          String bmat;
          String which;
          int ido[] = new int[1];
          int info[] = new int[1];
          int ierr[] = new int[1];
          int nconv = 0;
          int          n, nev, ncv, lworkl, i, j, 
                       nx, maxitr, mode, ishfts;
          boolean          rvec;
          double tol[] = new double[]{0.0};
          // sigma not intialized in dsdrv1
          double sigma = 0.0;
          double v1[];
          double v2[];
          int index;
   
    //     %-----------------------------%
    //     | BLAS & LAPACK routines used |
    //     %-----------------------------%
    
    //      Double precision           
    //     &                 dnrm2
    //      external         dnrm2, daxpy
    
    //     %--------------------%
    //     | Intrinsic function |
    //     %--------------------%
    
    //      intrinsic        abs
    
    //     %-----------------------%
    //     | Executable Statements |
    //     %-----------------------%
    
    //     %----------------------------------------------------%
    //     | The number NX is the number of interior points     |
    //     | in the discretization of the 2-dimensional         |
    //     | Laplacian on the unit square with zero Dirichlet   |
    //     | boundary condition.  The number N(=NX*NX) is the   |
    //     | dimension of the matrix.  A standard eigenvalue    |
    //     | problem is solved (BMAT = 'I'). NEV is the number  |
    //     | of eigenvalues to be approximated.  The user can   |
    //     | modify NEV, NCV, WHICH to solve problems of        |
    //     | different sizes, and to get different parts of the |
    //     | spectrum.  However, The following conditions must  |
    //     | be satisfied:                                      |
    //     |                   N <= MAXN,                       | 
    //     |                 NEV <= MAXNEV,                     |
    //     |             NEV + 1 <= NCV <= MAXNCV               | 
    //     %----------------------------------------------------% 
    
          nx = 10;
          n = nx*nx;
          // nev = 4;
          nev =  5; 
          ncv =  10; 
          if ( n > maxn ) {
             UI.setDataText("ERROR with DSDRV1: N is greater than MAXN");
             return;
          }
          else if ( nev > maxnev ) {
        	  UI.setDataText("ERROR with DSDRV1: NEV is greater than MAXNEV");
              return;
          }
          else if ( ncv > maxncv ) {
             UI.setDataText("ERROR with DSDRV1: NCV is greater than MAXNCV");
             return;
          }
          bmat = "I";
          which = "SM";
    
    //     %--------------------------------------------------%
    //     | The work array WORKL is used in DSAUPD as        |
    //     | workspace.  Its dimension LWORKL is set as       |
    //     | illustrated below.  The parameter TOL determines |
    //     | the stopping criterion.  If TOL<=0, machine      |
    //     | precision is used.  The variable IDO is used for |
    //     | reverse communication and is initially set to 0. |
    //     | Setting INFO=0 indicates that a random vector is |
    //     | generated in DSAUPD to start the Arnoldi         |
    //     | iteration.                                       |
    //     %--------------------------------------------------%
    
          lworkl = ncv*(ncv+8); 
          info[0] = 0;
          ido[0] = 0;
    
    //     %---------------------------------------------------%
    //     | This program uses exact shifts with respect to    |
    //     | the current Hessenberg matrix (IPARAM(1) = 1).    |
    //     | IPARAM(3) specifies the maximum number of Arnoldi |
    //     | iterations allowed.  Mode 1 of DSAUPD is used     |
    //     | (IPARAM(7) = 1).  All these options may be        |
    //     | changed by the user. For details, see the         |
    //     | documentation in DSAUPD.                          |
    //     %---------------------------------------------------%
    
          ishfts = 1;
          maxitr = 300;
          mode   = 1;
          
          iparam[0] = ishfts; 
          iparam[2] = maxitr; 
          iparam[6] = mode; 
    
    //     %-------------------------------------------%
    //     | M A I N   L O O P (Reverse communication) |
    //     %-------------------------------------------%
    
          v1 = new double[nx*nx];
          v2 = new double[nx*nx];
          while (true) {
    
    //        %---------------------------------------------%
    //        | Repeatedly call the routine DSAUPD and take | 
    //        | actions indicated by parameter IDO until    |
    //        | either convergence is indicated or maxitr   |
    //        | has been exceeded.                          |
    //        %---------------------------------------------%
    
             dsaupd ( ido, bmat, n, which, nev, tol, resid, ncv, v, ldv, iparam, ipntr, workd, workl,
                          lworkl, info );
    
             if (ido[0] != -1 && ido[0] != 1) {
            	 break;
             }
    
    //           %--------------------------------------%
    //           | Perform matrix vector multiplication |
    //           |              y <--- OP*x             |
    //           | The user should supply his/her own   |
    //           | matrix vector multiplication routine |
    //           | here that takes workd(ipntr(1)) as   |
    //           | the input, and return the result to  |
    //           | workd(ipntr(2)).                     |
    //           %--------------------------------------%
                 for (i = 0; i < nx*nx; i++) {
                	 v1[i] = workd[ipntr[0]-1+i];
                 }
                 av (nx, v1, v2);
                 for (i = 0; i < nx*nx; i++) {
                	 workd[ipntr[1]-1+i] = v2[i];
                 }
    
    //           %-----------------------------------------%
    //           | L O O P   B A C K to call DSAUPD again. |
    //           %-----------------------------------------%
    
    } // while (true)
    
    //     %----------------------------------------%
    //     | Either we have convergence or there is |
    //     | an error.                              |
    //     %----------------------------------------%
    
          if ( info[0] < 0 ) {
    
    //        %--------------------------%
    //        | Error message. Check the |
    //        | documentation in DSAUPD. |
    //        %--------------------------%
    //
             UI.setDataText("Error with dsaupd info[0] = " + info[0] + "\n");
             return;
          } // if (info[0] < 0)
          else  { // info[0] >= 0
    
    //        %-------------------------------------------%
    //        | No fatal errors occurred.                 |
    //        | Post-Process using DSEUPD.                |
    //        |                                           |
    //        | Computed eigenvalues may be extracted.    |  
    //        |                                           |
    //        | Eigenvectors may also be computed now if  |
    //        | desired.  (indicated by rvec = .true.)    | 
    //        %-------------------------------------------%
               
             rvec = true;
    
             // dsdrv1 has d(maxncv,2)
             // dseupd has d(nev)
             dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                 bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                 iparam, ipntr, workd, workl, lworkl, ierr );
             index = 0;
             for (j = 0; j < 2; j++) {
            	 for (i = 0; i < maxncv; i++) {
            	     d[i][j] = ds[index++]; 
            	 }
             }

    //        %----------------------------------------------%
    //        | Eigenvalues are returned in the first column |
    //        | of the two dimensional array D and the       |
    //        | corresponding eigenvectors are returned in   |
    //        | the first NEV columns of the two dimensional |
    //        | array V if requested.  Otherwise, an         |
    //        | orthogonal basis for the invariant subspace  |
    //        | corresponding to the eigenvalues in D is     |
    //        | returned in V.                               |
    //        %----------------------------------------------%
    
             if ( ierr[0] != 0) {
    //
    //            %------------------------------------%
    //            | Error condition:                   |
    //            | Check the documentation of DSEUPD. |
    //            %------------------------------------%
                 UI.setDataText("Error with dseupd ierr[0] = " + ierr[0] + "\n");
             } // if (ierr[0] != 0)
             else { // ierr[0] == 0
    
                 nconv =  iparam[4];
                 for (j = 0; j < nconv; j++) {
    
    //               %---------------------------%
    //               | Compute the residual norm |
    //               |                           |
    //               |   ||  A*x - lambda*x ||   |
    //               |                           |
    //               | for the NCONV accurately  |
    //               | computed eigenvalues and  |
    //               | eigenvectors.  (iparam(5) |
    //               | indicates how many are    |
    //               | accurate to the requested |
    //               | tolerance)                |
    //               %---------------------------%
    //
                    for (i = 0; i < nx*nx; i++) {
                    	v1[i] = v[i][j];
                    }
                	av(nx, v1, ax);
                	for (i = 0; i < n; i++) {
                		ax[i] = ax[i] + (-d[j][0])*v[i][j];
                	}
                    d[j][1] = ge.dnrm2(n, ax, 1);
                    d[j][1] = d[j][1] / Math.abs(d[j][0]);
    
                 } // for (j = 0; j < nconv; j++)
    
    //            %-------------------------------%
    //            | Display computed residuals    |
    //            %-------------------------------%
                 UI.setDataText("Ritz values and relative residuals: \n");
                 for (i = 0; i < nconv; i++) {
                	 UI.setDataText("d["+i+"][0] = " + nf.format(d[i][0]) + " d["+i+"][1] = " + nf.format(d[i][1]) + "\n");
                 }
             } // else ierr[0] == 0
    
    //        %------------------------------------------%
    //        | Print additional convergence information |
    //        %------------------------------------------%
    
             if ( info[0] == 1) {
            	UI.setDataText("Maximum number of iterations reached.\n");
             }
             else if ( info[0] == 3) {
            	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
             }     
    
             UI.setDataText("\n");
             UI.setDataText("DSDRV1\n");
             UI.setDataText("======\n");
             UI.setDataText("\n");
             UI.setDataText("Size of the matrix = " +  n + "\n");
             UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
             UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
             UI.setDataText("What portion of the spectrum: " +  which + "\n");
             UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
             UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
             UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
             UI.setDataText("The convergence criterion = " + tol[0] + "\n");
             UI.setDataText("\n");
          } // else info[0] >= 0
          return;
    } // dsdrv1
     
    // ------------------------------------------------------------------
    //     matrix vector subroutine
    
    //     The matrix used is the 2 dimensional discrete Laplacian on unit
    //     square with zero Dirichlet boundary condition.
    
    //     Computes w <--- OP*v, where OP is the nx*nx by nx*nx block 
    //     tridiagonal matrix
    
    //                  | T -I          | 
    //                  |-I  T -I       |
    //             OP = |   -I  T       |
    //                  |        ...  -I|
    //                  |           -I T|
    
    //     The subroutine TV is called to computed y<---T*x.
    
          private void av (int nx, double v[], double w[]) {
          int           j, lo, n2, i;
          double v1[];
          double v2[];
          //Double precision
          //&                  v(nx*nx), w(nx*nx), 
          final double one = 1.0;
          double h2;
    
          tv(nx,v,w);
          for (i = 0; i < nx; i++) {
        	  w[i] = w[i] + (-one)*v[nx+i]; 
          }
    
          v1 = new double[nx];
          v2 = new double[nx];
          for (j = 2; j <= nx-1; j++) {
             lo = (j-1)*nx;
             for (i = 0; i < nx; i++) {
            	 v1[i] = v[lo+i];
             }
             tv(nx, v1, v2);
             for (i = 0; i < nx; i++) {
            	 w[lo+i] = v2[i];
             }
             for (i = 0; i < nx; i++) {
            	 w[lo+i] = w[lo+i] + (-one)*v[lo-nx+i];
             }
             for (i = 0; i < nx; i++) {
            	 w[lo+i] = w[lo+i] + (-one)*v[lo+nx+i];
             }
           } // for (j = 2; j <= nx-1; j++)
    
          lo = (nx-1)*nx;
          for (i = 0; i < nx; i++) {
         	 v1[i] = v[lo+i];
          }
          tv(nx, v1, v2);
          for (i = 0; i < nx; i++) {
         	 w[lo+i] = v2[i];
          }
          for (i = 0; i < nx; i++) {
        	  w[lo+i] = w[lo+i] + (-one)*v[lo-nx+i];
          }
    
    //     Scale the vector w by (1/h^2), where h is the mesh size
    
          n2 = nx*nx;
          h2 = one / (double)((nx+1)*(nx+1));
          for (i = 0; i < n2; i++) {
        	 w[i] = w[i] * (one/h2);
          }
          return;
          } // av
        
    
    // -------------------------------------------------------------------
          private void tv (int nx, double x[], double y[]) {
    
          int j ;
          //Double precision
          // &                  x(nx), y(nx),
          double dd, dl, du;
    
          final double one = 1.0;
    
    //     Compute the matrix vector multiplication y<---T*x
    //     where T is a nx by nx tridiagonal matrix with DD on the 
    //     diagonal, DL on the subdiagonal, and DU on the superdiagonal.
         
    
          dd  = 4.0;
          dl  = -one; 
          du  = -one;
     
          y[0] =  dd*x[0] + du*x[1];
          for (j = 1; j <= nx-2; j++) {
             y[j] = dl*x[j-1] + dd*x[j] + du*x[j+1]; 
          }
          y[nx-1] =  dl*x[nx-2] + dd*x[nx-1]; 
          return;
          } // tv
          
      public void dsdrv2() { 
      // At http://docs.roguewave.com/imsl/fortran/7.0/math/docs/arpacksymmetric.htm:  
//    	  Output
//    	  Number of Matrix-Vector Products Required, EX-2
//    	  -----------------------------------------------
//    	        24
//    	  Largest Laplacian Eigenvalues Near Zero Shift
//    	                    1     9.9
//    	                    2    39.5
//    	                    3    88.8
//    	                    4   157.7
    	  
// Running the original FORTRAN gave:
//    	  Ritz values and relative residuals
//    	  ----------------------------------
//    	                Col   1       Col   2
//    	   Row   1:    9.86881D+00   6.27324D-13
//    	   Row   2:    3.94657D+01   2.62012D-13
//    	   Row   3:    8.87620D+01   1.29824D-13
//    	   Row   4:    1.57710D+02   1.03253D-13
    	   
    	   
//    	   _SDRV2 
//    	   ====== 
    	   
//    	   Size of the matrix is          100
//    	   The number of Ritz values requested is            4
//    	   The number of Arnoldi vectors generated (NCV) is           10
//    	   What portion of the spectrum: LM
//    	   The number of converged Ritz values is            4
//    	   The number of Implicit Arnoldi update iterations taken is            5
//    	   The number of OP*x is           27
//    	   The convergence criterion is    1.1102230246251565E-016	  
    	  
    	  
//    	 All Ritz Values and Vectors have small residuals.    	 
//    My run gave:
//    	  Ritz values and relative residuals: 
//    		  d[0][0] = 9.86881E0 d[0][1] = 6.79022E-13
//    		  d[1][0] = 3.94657E1 d[1][1] = 2.42190E-13
//    		  d[2][0] = 8.87620E1 d[2][1] = 1.14934E-13
//    		  d[3][0] = 1.57710E2 d[3][1] = 1.80516E-13

//    		  DSDRV2
//    		  ======

//    		  Size of the matrix = 100
//    		  The number of Ritz values requested = 4
//    		  The number of Arnoldi vectors generated ncv = 10
//    		  What portion of the spectrum: LM
//    		  The number of converged Ritz values = 4
//    		  The number of Implicit Arnoldi update iterations taken = 5
//    		  The number of OP*x = 27
//    		  The convergence criterion = 1.1102230246251565E-16
      
      //     Program to illustrate the idea of reverse communication
      //     in shift and invert mode for a standard symmetric eigenvalue
      //     problem.  The following program uses the two LAPACK subroutines 
      //     dgttrf.f and dgttrs.f to factor and solve a tridiagonal system of 
      //     equations.
      
      //     We implement example two of ex-sym.doc in DOCUMENTS directory
      
      // \Example-2
      //     ... Suppose we want to solve A*x = lambda*x in shift-invert mode,
      //         where A is derived from the central difference discretization
      //         of the 1-dimensional Laplacian on [0,1]  with zero Dirichlet
      //         boundary condition.
      //     ... OP = (inv[A - sigma*I]) and  B = I.
      //     ... Use mode 3 of DSAUPD.
      
      // \BeginLib
      
      // \Routines called:
      //     dsaupd  ARPACK reverse communication interface routine.
      //     dseupd  ARPACK routine that returns Ritz values and (optionally)
      //             Ritz vectors.
      //     dgttrf  LAPACK tridiagonal factorization routine.
      //     dgttrs  LAPACK tridiagonal solve routine.
      //     daxpy   daxpy   Level 1 BLAS that computes y <- alpha*x+y.
      //     dnrm2   Level 1 BLAS that computes the norm of a vector.     
      //     av2      Matrix vector multiplication routine that computes A*x.
      
      // \Author
      //     Richard Lehoucq
      //     Danny Sorensen
      //     Chao Yang
      //     Dept. of Computational &
      //     Applied Mathematics
      //     Rice University
      //     Houston, Texas
      
      // \SCCS Information: @(#)
      // FILE: sdrv2.F   SID: 2.4   DATE OF SID: 4/22/96   RELEASE: 2
      
      // \Remarks
      //     1. None
      
      // \EndLib
      // ----------------------------------------------------------------------
      
      //     %-----------------------------%
      //     | Define leading dimensions   |
      //     | for all arrays.             |
      //     | MAXN:   Maximum dimension   |
      //     |         of the A allowed.   |
      //     | MAXNEV: Maximum NEV allowed |
      //     | MAXNCV: Maximum NCV allowed |
      //     %-----------------------------%
      
            final int maxn = 256;
            final int maxnev = 10;
            final int maxncv = 25;
            final int ldv = maxn;
      
      //     %--------------%
      //     | Local Arrays |
      //     %--------------%
      
           double v[][] = new double[ldv][maxncv];
           double workl[] = new double[maxncv*(maxncv+8)];
           double workd[] = new double[3*maxn];
           double d[][] = new double[maxncv][2];
           double ds[] = new double[2 * maxncv];
           double resid[] = new double[maxn];
           double ad[] = new double[maxn];
           double adl[] = new double[maxn];
           double adu[] = new double[maxn];
           double adu2[] = new double[maxn];
           double ax[] = new double[maxn];
           boolean select[] = new boolean[maxncv];
           int iparam[] = new int[11];
           int ipntr[] = new int[11];
           int ipiv[] = new int[maxn];
      
      //     %---------------%
      //     | Local Scalars |
      //     %---------------%
      
            String bmat;
            String which;
            int ido[] = new int[1];
            int info[] = new int[1];
            int ierr[] = new int[1];
            int nconv = 0;
            int              n, nev, ncv, lworkl, i, j,
                             maxitr, ishfts, mode;
            boolean          rvec;
            double tol[] = new double[]{0.0};
            double sigma, h2;
            double array[][];
            int index;
            double v1[];
      
      //     %------------%
      //     | Parameters |
      //     %------------%
      
            final double zero = 0.0;
            final double one = 1.0;
            final double two = 2.0;
      
      //     %-----------------------------%
      //     | BLAS & LAPACK routines used |
      //     %-----------------------------%
      
      //      Double precision
      //     &                 dnrm2
      //      external         daxpy, dnrm2, dgttrf, dgttrs
      
      //     %--------------------%
      //     | Intrinsic function |
      //     %--------------------%
      
      //      intrinsic        abs
      
      //     %-----------------------%
      //     | Executable Statements |
      //     %-----------------------%
      
      //     %----------------------------------------------------%
      //     | The number N is the dimension of the matrix.  A    |
      //     | standard eigenvalue problem is solved (BMAT = 'I'. |
      //     | NEV is the number of eigenvalues (closest to       |
      //     | SIGMA) to be approximated.  Since the shift-invert |
      //     | mode is used, WHICH is set to 'LM'.  The user can  |
      //     | modify NEV, NCV, SIGMA to solve problems of        | 
      //     | different sizes, and to get different parts of the |
      //     | spectrum.  However, The following conditions must  |
      //     | be satisfied:                                      |
      //     |                   N <= MAXN,                       | 
      //     |                 NEV <= MAXNEV,                     |
      //     |             NEV + 1 <= NCV <= MAXNCV               | 
      //     %----------------------------------------------------% 
      
            n = 100;
            nev = 4;
            ncv = 10;
            if ( n > maxn ) {
               UI.setDataText("ERROR with DSDRV2: N is greater than MAXN");
               return;
            }
            else if ( nev > maxnev ) {
               UI.setDataText("ERROR with DSDRV2: NEV is greater than MAXNEV");
               return;
            }
            else if ( ncv > maxncv ) {
               UI.setDataText("ERROR with DSDRV2: NCV is greater than MAXNCV");
               return;
            }
      
            bmat = "I";
            which = "LM";
            sigma = zero; 
      
      //     %--------------------------------------------------%
      //     | The work array WORKL is used in DSAUPD as        |
      //     | workspace.  Its dimension LWORKL is set as       |
      //     | illustrated below.  The parameter TOL determines |
      //     | the stopping criterion.  If TOL<=0, machine      |
      //     | precision is used.  The variable IDO is used for |
      //     | reverse communication and is initially set to 0. |
      //     | Setting INFO=0 indicates that a random vector is |
      //     | generated in DSAUPD to start the Arnoldi         |
      //     | iteration.                                       |
      //     %--------------------------------------------------%
      
            lworkl = ncv*(ncv+8); 
            ido[0] = 0;
            info[0] = 0;
      
      //     %---------------------------------------------------%
      //     | This program uses exact shifts with respect to    |
      //     | the current Hessenberg matrix (IPARAM(1) = 1).    |
      //     | IPARAM(3) specifies the maximum number of Arnoldi |
      //     | iterations allowed.  Mode 3 of DSAUPD is used     |
      //     | (IPARAM(7) = 3).  All these options may be        |
      //     | changed by the user. For details, see the         |
      //     | documentation in DSAUPD.                          |
      //     %---------------------------------------------------%
      
            ishfts = 1;
            maxitr = 300;
            mode   = 3;
      
            iparam[0] = ishfts; 
            iparam[2] = maxitr; 
            iparam[6] = mode; 
      
      //     %-----------------------------------------------------%
      //     | Call LAPACK routine to factor (A-SIGMA*I), where A  |
      //     | is the 1-d Laplacian.                               | 
      //     %-----------------------------------------------------%
      
            h2 = one / (double)((n+1)*(n+1));
            for (j = 0; j < n; j++) {
               ad[j] = two / h2 - sigma;
               adl[j] = -one / h2;
            } // for (j = 0; j < n; j++)
            for (j = 0; j < n; j++) {
            	adu[j] = adl[j];
            }
            dgttrf (n, adl, ad, adu, adu2, ipiv, ierr);
            if (ierr[0] != 0) {
               UI.setDataText("Error with dgttrg in dsdrv2");
               return;
            }
      
      //     %-------------------------------------------%
      //     | M A I N   L O O P (Reverse communication) |
      //     %-------------------------------------------%
      
      while (true) {
      
      //        %---------------------------------------------%
      //        | Repeatedly call the routine DSAUPD and take |
      //        | actions indicated by parameter IDO until    |
      //        | either convergence is indicated or maxitr   |
      //        | has been exceeded.                          |
      //        %---------------------------------------------%
      
               dsaupd ( ido, bmat, n, which, nev, tol, resid,
                            ncv, v, ldv, iparam, ipntr, workd, workl,
                            lworkl, info );
      
               if (ido[0] != -1 && ido[0] != 1) {
              	 break;
               }
      
      //           %----------------------------------------%
      //           | Perform y <-- OP*x = inv[A-sigma*I]*x. |
      //           | The user only need the linear system   |
      //           | solver here that takes workd(ipntr(1)) |
      //           | as input, and returns the result to    |
      //           | workd(ipntr(2)).                       |
      //           %----------------------------------------%
      
                  for (i = 0; i < n; i++) {
                	  workd[ipntr[1]-1+i] = workd[ipntr[0]-1+i];  
                  }
      
                  array = new double[n][1];
                  for (i = 0; i < n; i++) {
                	  array[i][0] = workd[ipntr[1]-1+i];
                  }
                  dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv,
                              array, n, ierr);
                  for (i = 0; i < n; i++) {
                	  workd[ipntr[1]-1+i] = array[i][0];
                  }
                  if (ierr[0] != 0) {
                	 UI.setDataText("Error with dgttrs in DSDRV2");
                	 return;
                  }
      
      //           %-----------------------------------------%
      //           | L O O P   B A C K to call DSAUPD again. |
      //           %-----------------------------------------%
      } // while (true)
      
      //     %----------------------------------------%
      //     | Either we have convergence or there is |
      //     | an error.                              |
      //     %----------------------------------------%
      
            if ( info[0] < 0 ) {
      
      //        %----------------------------%
      //        | Error message.  Check the  |
      //        | documentation in DSAUPD    |
      //        %----------------------------%
      
               UI.setDataText("Error with dsaupd, info[0] = " + info[0] + "\n");
               return;
            } // if (info[0] < 0)
            else { // info[0] >= 0
      
      //        %-------------------------------------------%
      //        | No fatal errors occurred.                 |
      //        | Post-Process using DSEUPD.                |
      //        |                                           |
      //        | Computed eigenvalues may be extracted.    |
      //        |                                           |
      //        | Eigenvectors may also be computed now if  |
      //        | desired.  (indicated by rvec = .true.)    |
      //        %-------------------------------------------%
      
               rvec = true;
      
               dseupd ( rvec, "A", select, ds, v, ldv, sigma,
                   bmat, n, which, nev, tol[0], resid, ncv, v, ldv,
                  iparam, ipntr, workd, workl, lworkl, ierr );
               index = 0;
               for (j = 0; j < 2; j++) {
              	 for (i = 0; i < maxncv; i++) {
              	     d[i][j] = ds[index++]; 
              	 }
               }
      
      //        %----------------------------------------------%
      //        | Eigenvalues are returned in the first column |
      //        | of the two dimensional array D and the       |
      //        | corresponding eigenvectors are returned in   |
      //        | the first NEV columns of the two dimensional |
      //        | array V if requested.  Otherwise, an         |
      //        | orthogonal basis for the invariant subspace  |
      //        | corresponding to the eigenvalues in D is     |
      //        | returned in V.                               |
      //        %----------------------------------------------%

               if ( ierr[0] != 0 ) { 
      
      //           %------------------------------------%
      //           | Error condition:                   |
      //           | Check the documentation of DSEUPD. |
      //           %------------------------------------%
      
                  UI.setDataText("Error with dseupd ierr[0] = " + ierr[0] + "\n");
               } // if (ierr[0] != 0)
               else { // ierr[0] == 0
      
                  nconv =  iparam[4];
                  for (j=0; j < nconv; j++) {
      
      //              %---------------------------%
      //              | Compute the residual norm |
      //              |                           |
      //              |   ||  A*x - lambda*x ||   |
      //              |                           |
      //              | for the NCONV accurately  |
      //              | computed eigenvalues and  |
      //              | eigenvectors.  (iparam(5) |
      //              | indicates how many are    |
      //              | accurate to the requested |
      //              | tolerance)                |
      //              %---------------------------%
      
                     v1 = new double[n];
                     for (i = 0; i < n; i++) {
                    	 v1[i] = v[i][j];
                     }
                	 av2(n, v1, ax);
                	 for (i = 0; i < n; i++) {
                		 ax[i] = ax[i] + (-d[j][0])*v[i][j];
                	 }
                     d[j][1] = ge.dnrm2(n, ax, 1);
                     d[j][1] = d[j][1] / Math.abs(d[j][0]);
      
                  } // for (j=0; j < nconv; j++)
      
      //           %-------------------------------%
      //           | Display computed residuals    |
      //           %-------------------------------%
                  UI.setDataText("Ritz values and relative residuals: \n");
                  for (i = 0; i < nconv; i++) {
                 	 UI.setDataText("d["+i+"][0] = " + nf.format(d[i][0]) + " d["+i+"][1] = " + nf.format(d[i][1]) + "\n");
                  }
               } // ierr[0] == 0
      
      //        %------------------------------------------%
      //        | Print additional convergence information |
      //        %------------------------------------------%
               
               if ( info[0] == 1) {
               	UI.setDataText("Maximum number of iterations reached.\n");
                }
                else if ( info[0] == 3) {
               	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
                }     
       
                UI.setDataText("\n");
                UI.setDataText("DSDRV2\n");
                UI.setDataText("======\n");
                UI.setDataText("\n");
                UI.setDataText("Size of the matrix = " +  n + "\n");
                UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
                UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
                UI.setDataText("What portion of the spectrum: " +  which + "\n");
                UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
                UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
                UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
                UI.setDataText("The convergence criterion = " + tol[0] + "\n");
                UI.setDataText("\n");
            } // info[0] >= 0
            return;
      } // dsdrv2
      
      //
      // ------------------------------------------------------------------------
      //     Matrix vector subroutine
      //     where the matrix is the 1 dimensional discrete Laplacian on
      //     the interval [0,1] with zero Dirichlet boundary condition.
      
            private void av2 (int n, double v[], double w[]) {
            int           j;
            //Double precision
            // &                  v(n), w(n)
            double h2;
            final double one = 1.0;
            final double two = 2.0;

            w[0] =  two*v[0] - v[1];
            for (j = 1; j < n-1; j++) {
               w[j] = - v[j-1] + two*v[j] - v[j+1]; 
            } // for (j = 1; j < n-1; j++)
            j = n-1;
            w[j] = - v[j-1] + two*v[j]; 
      
      //     Scale the vector w by (1 / h^2).
      
            h2 = one / (double)((n+1)*(n+1));
            for (j = 0; j < n; j++) {
            	w[j] = (one/h2) * w[j];
            }
            return;
            } // av2
            
        public void dsdrv3() {
        // Running FORTRAN gave:
        //	Ritz values and relative residuals
        //	 ----------------------------------
        //	               Col   1       Col   2
       // 	  Row   1:    1.21003D+05   9.03615D-17
       // 	  Row   2:    1.21617D+05   2.76940D-16
       // 	  Row   3:    1.22057D+05   2.74617D-16
       // 	  Row   4:    1.22323D+05   1.12042D-16
        	  
        	  
       //	  _SDRV3 
       // 	  ====== 
        	  
       // 	  Size of the matrix is          100
       // 	  The number of Ritz values requested is            4
       // 	  The number of Arnoldi vectors generated (NCV) is           10
       // 	  What portion of the spectrum: LM
       // 	  The number of converged Ritz values is            4
       // 	  The number of Implicit Arnoldi update iterations taken is           56
       // 	  The number of OP*x is          304
       // 	  The convergence criterion is    1.1102230246251565E-016
        	
       // Running Java gave:
       // 	Ritz values and relative residuals: 
       //	d[0][0] = 1.21003E5 d[0][1] = 2.99170E-16
       //	d[1][0] = 1.21617E5 d[1][1] = 1.42103E-16
       //	d[2][0] = 1.22057E5 d[2][1] = 1.12708E-16
       //	d[3][0] = 1.22323E5 d[3][1] = 3.04680E-16

       //	DSDRV3
       //	======

       //	Size of the matrix = 100
       //	The number of Ritz values requested = 4
       //	The number of Arnoldi vectors generated ncv = 10
       // 	What portion of the spectrum: LM
       //	The number of converged Ritz values = 4
       //	The number of Implicit Arnoldi update iterations taken = 56
       //	The number of OP*x = 304
       //	The convergence criterion = 1.1102230246251565E-16
        
        //     Program to illustrate the idea of reverse communication in
        //     inverse mode for a generalized symmetric eigenvalue problem.
        //     The following program uses the two LAPACK subroutines dgttrf.f 
        //     and dgttrs.f to factor and solve a tridiagonal system of equations.
        
        //     We implement example three of ex-sym.doc in DOCUMENTS directory
        
        // \Example-3
        //     ... Suppose we want to solve A*x = lambda*M*x in inverse mode,
        //         where A and M are obtained by the finite element of the 
        //         1-dimensional discrete Laplacian
        //                             d^2u / dx^2
        //         on the interval [0,1] with zero Dirichlet boundary condition
        //         using piecewise linear elements.
        
        //     ... OP = inv[M]*A  and  B = M.
        
        //     ... Use mode 2 of DSAUPD.    
        
        // \BeginLib
        
        // \Routines called:
        //     dsaupd  ARPACK reverse communication interface routine.
        //     dseupd  ARPACK routine that returns Ritz values and (optionally)
        //             Ritz vectors.
        //     dgttrf  LAPACK tridiagonal factorization routine.
        //     dgttrs  LAPACK tridiagonal solve routine.
        //     daxpy   Level 1 BLAS that computes y <- alpha*x+y.
        //     dscal   Level 1 BLAS that scales a vector by a scalar.
        //     dcopy   Level 1 BLAS that copies one vector to another.
        //     dnrm2   Level 1 BLAS that computes the norm of a vector.     
        //     av3      Matrix vector multiplication routine that computes A*x.
        //     mv3      Matrix vector multiplication routine that computes M*x.
        
        // \Author
        //     Richard Lehoucq
        //     Danny Sorensen
        //     Chao Yang
        //     Dept. of Computational &
        //     Applied Mathematics
        //     Rice University
        //     Houston, Texas
        
        // \SCCS Information: @(#)
        // FILE: sdrv3.F   SID: 2.4   DATE OF SID: 4/22/96   RELEASE: 2
        
        // \Remarks
        //     1. None
        
        // \EndLib
        // --------------------------------------------------------------------------
        
        //     %-----------------------------%
        //     | Define leading dimensions   |
        //     | for all arrays.             |
        //     | MAXN:   Maximum dimension   |
        //     |         of the A allowed.   |
        //     | MAXNEV: Maximum NEV allowed |
        //     | MAXNCV: Maximum NCV allowed |
        //     %-----------------------------%
        
              final int maxn = 256;
              final int maxnev = 10;
              final int maxncv = 25;
              final int ldv = maxn;
        	 
        //     %--------------%
        //     | Local Arrays |
        //     %--------------%
              double v[][] = new double[ldv][maxncv];
              double workl[] = new double[maxncv*(maxncv+8)];
              double workd[] = new double[3*maxn];
              double d[][] = new double[maxncv][2];
              double ds[] = new double[2 * maxncv];
              double resid[] = new double[maxn];
              double ad[] = new double[maxn];
              double adl[] = new double[maxn];
              double adu[] = new double[maxn];
              double adu2[] = new double[maxn];
              double ax[] = new double[maxn];
              double mx[] = new double[maxn];
              boolean select[] = new boolean[maxncv];
              int iparam[] = new int[11];
              int ipntr[] = new int[11];
              int ipiv[] = new int[maxn];
              
              //     %---------------%
              //     | Local Scalars |
              //     %---------------%
              
            String bmat;
            String which;
            int ido[] = new int[1];
            int info[] = new int[1];
            int ierr[] = new int[1];
            int nconv = 0;
            int              n, nev, ncv, lworkl, i, j,
                             maxitr, ishfts, mode;
            boolean          rvec;
            double sigma = 0.0;
            double tol[] = new double[]{0.0};
            double r1, r2, h;
            double array[][];
            int index;
            double v1[];
            double v2[];
      
      //     %------------%
      //     | Parameters |
      //     %------------%
      
            final double one = 1.0;
            final double four = 4.0;
            final double six = 6.0;
        
        //     %-----------------------------%
        //     | BLAS & LAPACK routines used |
        //     %-----------------------------%
        
        //      Double precision
        //     &                 dnrm2
        //      external         daxpy, dcopy, dscal, dnrm2, dgttrf, dgttrs
        
        //     %--------------------%
        //     | Intrinsic function |
        //     %--------------------%
        
        //       intrinsic        abs
        
        //     %-----------------------%
        //     | Executable statements |
        //     %-----------------------%
        
        //     %----------------------------------------------------%
        //     | The number N is the dimension of the matrix. A     |
        //     | generalized eigenvalue problem is solved (BMAT =   |
        //     | 'G'.) NEV is the number of eigenvalues to be       |
        //     | approximated.  The user can modify NEV, NCV, WHICH |
        //     | to solve problems of different sizes, and to get   |
        //     | different parts of the spectrum.  However, The     |
        //     | following conditions must be satisfied:            |
        //     |                     N <= MAXN,                     | 
        //     |                   NEV <= MAXNEV,                   |
        //     |               NEV + 1 <= NCV <= MAXNCV             | 
        //     %----------------------------------------------------%
        
              n = 100;
              nev = 4;
              ncv = 10;
              if ( n > maxn ) {
                 UI.setDataText("ERROR with DSDRV3: N is greater than MAXN \n");
                 return;
              }
              else if ( nev > maxnev ) {
                 UI.setDataText("ERROR with DSDRV3: NEV is greater than MAXNEV \n");
                 return;
              }
              else if ( ncv > maxncv ) {
                 UI.setDataText("ERROR with DSDRV3: NCV is greater than MAXNCV \n");
                 return;
              }
              bmat = "G";
              which = "LM";
        
        //     %--------------------------------------------------%
        //     | The work array WORKL is used in DSAUPD as        |
        //     | workspace.  Its dimension LWORKL is set as       |
        //     | illustrated below.  The parameter TOL determines |
        //     | the stopping criterion.  If TOL<=0, machine      |
        //     | precision is used.  The variable IDO is used for |
        //     | reverse communication and is initially set to 0. |
        //     | Setting INFO=0 indicates that a random vector is |
        //     | generated in DSAUPD to start the Arnoldi         |
        //     | iteration.                                       |
        //     %--------------------------------------------------%
        
              lworkl = ncv*(ncv+8);
              ido[0] = 0;
              info[0] = 0;
        
        //     %---------------------------------------------------%
        //     | This program uses exact shifts with respect to    |
        //     | the current Hessenberg matrix (IPARAM(1) = 1).    |
        //     | IPARAM(3) specifies the maximum number of Arnoldi |
        //     | iterations allowed.  Mode 2 of DSAUPD is used     |
        //     | (IPARAM(7) = 2).  All these options may be        |
        //     | changed by the user. For details, see the         |
        //     | documentation in DSAUPD.                          |
        //     %---------------------------------------------------%
        
              ishfts = 1;
              maxitr = 300;
              mode   = 2;
        
              iparam[0] = ishfts;
              iparam[2] = maxitr; 
              iparam[6] = mode;
        
        //     %------------------------------------------------%
        //     | Call LAPACK routine to factor the mass matrix. |
        //     | The mass matrix is the tridiagonal matrix      |
        //     | arising from using piecewise linear finite     |
        //     | elements on the interval [0, 1].               |
        //     %------------------------------------------------%
        
              h = one / (double)(n+1);
        
              r1 = (four / six) * h; 
              r2 = (one / six) * h;
              for (j=0; j < n; j++) {
                 ad[j] =  r1;
                 adl[j] = r2;
              } 
              for (i = 0; i < n; i++) {
            	  adu[i] = adl[i];
              }
              dgttrf (n, adl, ad, adu, adu2, ipiv, ierr);
              if (ierr[0] != 0) { 
                 UI.setDataText("Error with dgttrf in DSDRV3. \n");
                 return;
              }
        
        //     %-------------------------------------------%
        //     | M A I N   L O O P (Reverse communication) |
        //     %-------------------------------------------%
        
        while (true) {
        
        //        %---------------------------------------------%
        //        | Repeatedly call the routine DSAUPD and take | 
        //        | actions indicated by parameter IDO until    |
        //        | either convergence is indicated or maxitr   |
        //        | has been exceeded.                          |
        //        %---------------------------------------------%
        
                 dsaupd ( ido, bmat, n, which, nev, tol, resid, 
                          ncv, v, ldv, iparam, ipntr, workd, workl,
                          lworkl, info );
        
                 if (ido[0] == -1 || ido[0] == 1) {
        
        //           %--------------------------------------%
        //           | Perform  y <--- OP*x = inv[M]*A*x.   |
        //           | The user should supply his/her own   |
        //           | matrix vector multiplication (A*x)   |
        //           | routine and a linear system solver   |
        //           | here.  The matrix vector             |
        //           | multiplication routine takes         |
        //           | workd(ipntr(1)) as the input vector. | 
        //           | The final result is returned to      |
        //           | workd(ipntr(2)). The result of A*x   |
        //           | overwrites workd(ipntr(1)).          |
        //           %--------------------------------------%
        
                    v1 = new double[n];
                    for (i = 0; i <n; i++) {
                    	v1[i] = workd[ipntr[0]-1+i];
                    }
                    v2 = new double[n];
                	av3 (n, v1, v2);
                	for (i = 0; i < n; i++) {
                		workd[ipntr[1]-1+i] = v2[i];
                	}
                	for (i = 0; i < n; i++) {
                		workd[ipntr[0]-1+i] = workd[ipntr[1]-1+i];
                	}
                	array = new double[n][1];
                	for (i = 0; i < n; i++) {
                		array[i][0] = workd[ipntr[1]-1+i];
                	}
                    dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv, 
                             array, n, ierr);
                    for (i = 0; i < n; i++) {
                		workd[ipntr[1]-1+i] = array[i][0];
                	}
                    if (ierr[0] != 0) { 
                       UI.setDataText("Error with dgttrs in dsdrv3.\n");
                       return;
                    }
        
        //           %-----------------------------------------%
        //           | L O O P   B A C K to call DSAUPD again. |
        //           %-----------------------------------------%
                 } // if (ido[0] == -1 || ido[0] == 1)
                 else if (ido[0] == 2) {
        
        //           %-----------------------------------------%
        //           |         Perform  y <--- M*x.            |
        //           | Need the matrix vector multiplication   |
        //           | routine here that takes workd(ipntr(1)) |
        //           | as the input and returns the result to  |
        //           | workd(ipntr(2)).                        |
        //           %-----------------------------------------%
        
                	 v1 = new double[n];
                     for (i = 0; i <n; i++) {
                     	v1[i] = workd[ipntr[0]-1+i];
                     }
                     v2 = new double[n];
                	 mv3 (n, v1, v2);
                	 for (i = 0; i < n; i++) {
                		workd[ipntr[1]-1+i] = v2[i];
                	}
        
        //           %-----------------------------------------%
        //           | L O O P   B A C K to call DSAUPD again. |
        //           %-----------------------------------------%
                 } // else if (ido[0] == 2)
                 else {
                	 break;
                 }
        } // while (true)
        
        
        
        //     %-----------------------------------------%
        //     | Either we have convergence, or there is |
        //     | an error.                               |
        //     %-----------------------------------------%
        
              if ( info[0] < 0 ) {
        
        //        %--------------------------%
        //        | Error message, check the |
        //        | documentation in DSAUPD  |
        //        %--------------------------%
        
                 UI.setDataText("Error with dsaupd info[0] = " + info[0] + "\n");
              } // if (info[0] < 0)
              else  { // info[0] >= 0
        
        //        %-------------------------------------------%
        //        | No fatal errors occurred.                 |
        //        | Post-Process using DSEUPD.                |
        //        |                                           |
        //        | Computed eigenvalues may be extracted.    |  
        //        |                                           |
        //        | Eigenvectors may also be computed now if  |
        //        | desired.  (indicated by rvec = .true.)    | 
        //        %-------------------------------------------%
                   
                 rvec = true;
        
                 dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                     bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                     iparam, ipntr, workd, workl, lworkl, ierr );
                 index = 0;
                 for (j = 0; j < 2; j++) {
                	 for (i = 0; i < maxncv; i++) {
                	     d[i][j] = ds[index++]; 
                	 }
                 }
        
        //        %----------------------------------------------%
        //        | Eigenvalues are returned in the first column |
        //        | of the two dimensional array D and the       |
        //        | corresponding eigenvectors are returned in   |
        //        | the first NEV columns of the two dimensional |
        //        | array V if requested.  Otherwise, an         |
        //        | orthogonal basis for the invariant subspace  |
        //        | corresponding to the eigenvalues in D is     |
        //        | returned in V.                               |
        //        %----------------------------------------------%
        
                 if (ierr[0] != 0) { 
        
        //           %------------------------------------%
        //           | Error condition:                   |
        //           | Check the documentation of DSEUPD. |
        //           %------------------------------------%
         
                    UI.setDataText("Error with dseupd ierr[0] = " + ierr[0] + "\n");
                 } // if (ierr[0] != 0)
                 else  { // ierr[0] == 0
        
                    nconv =  iparam[4];
                    for (j=0; j < nconv; j++) {
        
        //              %---------------------------%
        //              | Compute the residual norm |
        //              |                           |
        //              |  ||  A*x - lambda*M*x ||  |
        //              |                           |
        //              | for the NCONV accurately  |
        //              | computed eigenvalues and  |
        //              | eigenvectors.  (iparam(5) |
        //              | indicates how many are    |
        //              | accurate to the requested |
        //              | tolerance)                |
        //              %---------------------------%
        //
                       v1 = new double[n];
                       for (i = 0; i < n; i++) {
                    	   v1[i] = v[i][j];
                       }
                       av3(n, v1, ax);
                       mv3(n, v1, mx);
                       for (i = 0; i < n; i++) {
                    	   ax[i] = ax[i] + (-d[j][0])*mx[i];
                       }
                       d[j][1] = ge.dnrm2(n, ax, 1);
                       d[j][1] = d[j][1] / Math.abs(d[j][0]);
                    } // for (j = 0; j < nconv; j++);
                    //            %-------------------------------%
                    //           | Display computed residuals    |
                    //           %-------------------------------%
                                UI.setDataText("Ritz values and relative residuals: \n");
                                for (i = 0; i < nconv; i++) {
                               	 UI.setDataText("d["+i+"][0] = " + nf.format(d[i][0]) + " d["+i+"][1] = " + nf.format(d[i][1]) + "\n");
                                }
                 } // else ierr[0] == 0
                 //        %------------------------------------------%
                 //        | Print additional convergence information |
                 //        %------------------------------------------%
                          
                          if ( info[0] == 1) {
                          	UI.setDataText("Maximum number of iterations reached.\n");
                           }
                           else if ( info[0] == 3) {
                          	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
                           }     
                  
                           UI.setDataText("\n");
                           UI.setDataText("DSDRV3\n");
                           UI.setDataText("======\n");
                           UI.setDataText("\n");
                           UI.setDataText("Size of the matrix = " +  n + "\n");
                           UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
                           UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
                           UI.setDataText("What portion of the spectrum: " +  which + "\n");
                           UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
                           UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
                           UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
                           UI.setDataText("The convergence criterion = " + tol[0] + "\n");
                           UI.setDataText("\n");
              } // else info[0] >= 0
              return;
        } // dsdrv3
        
        // ------------------------------------------------------------------------
        //     Matrix vector subroutine
        //     where the matrix is the 1 dimensional mass matrix
        //     on the interval [0,1].
        
              private void mv3 (int n, double v[], double w[]) {
              int           j;
              //Double precision
              //&                  v(n),w(n),
              double h;
              final double one = 1.0;
              final double four = 4.0;
              final double six = 6.0;
              
              w[0] = four*v[0] + v[1];
              for  (j = 1; j < n-1; j++) {
                 w[j] = v[j-1] + four*v[j] + v[j+1]; 
              }
              j = n-1;
              w[j] = v[j-1] + four*v[j]; 
        
        //     Scale the vector w by h.
        
               h = one / ((double)(n+1)*six);
               for (j = 0; j < n; j++) {
            	   w[j] = h * w[j];
               }
              return;
              } // mv3
        
        // --------------------------------------------------------------------
        //     matrix vector subroutine
        
        //     The matrix used is the stiffness matrix obtained from the finite 
        //     element discretization of the 1-dimensional discrete Laplacian
        //     on the interval [0,1] with zero Dirichlet boundary condition using
        //     piecewise linear elements.
        
              private void av3 (int n, double v[], double w[]) {
              int           j;
              //Double precision
              //&                  v(n),w(n), two, one, h
              double h;
              final double one = 1.0;
              final double two = 2.0;
             
              w[0] =  two*v[0] - v[1];
              for (j = 1; j < n-1; j++) {
                 w[j] = - v[j-1] + two*v[j] - v[j+1]; 
              }
              j = n-1;
              w[j] = - v[j-1] + two*v[j]; 
        
              // Scale the vector w by (1 / h).
        
              h = one / (double)(n+1);
              for (j = 0; j < n; j++) {
            	  w[j] = (one/h) * w[j];
              }
              return;
              } // av3
              
      public void dsdrv4() {
      // Running FORTRAN gave:
//    	  Ritz values and relative residuals
//    	  ----------------------------------
//    	                Col   1       Col   2
//    	   Row   1:    9.87040D+00   5.12002D-14
//    	   Row   2:    3.94912D+01   1.50394D-14
//    	   Row   3:    8.88909D+01   6.89506D-15
//    	   Row   4:    1.58117D+02   6.47958D-15
    	   
    	   
//    	   _SDRV4 
//    	   ====== 
    	   
//    	   Size of the matrix is          100
//    	   The number of Ritz values requested is            4
//    	   The number of Arnoldi vectors generated (NCV) is           10
//    	   What portion of the spectrum: LM
//    	   The number of converged Ritz values is            4
//    	   The number of Implicit Arnoldi update iterations taken is            4
//    	   The number of OP*x is           24
//    	   The convergence criterion is    1.1102230246251565E-016
    	  
      // Running Java gave:
//    	  Ritz values and relative residuals: 
//		  d[0][0] = 9.87040E0 d[0][1] = 5.00608E-14
//		  d[1][0] = 3.94912E1 d[1][1] = 2.13788E-14
//		  d[2][0] = 8.88909E1 d[2][1] = 7.96537E-15
//		  d[3][0] = 1.58117E2 d[3][1] = 9.47002E-15

//		  DSDRV4
//		  ======

//		  Size of the matrix = 100
//		  The number of Ritz values requested = 4
//		  The number of Arnoldi vectors generated ncv = 10
//		  What portion of the spectrum: LM
//		  The number of converged Ritz values = 4
//		  The number of Implicit Arnoldi update iterations taken = 4
//		  The number of OP*x = 24
//		  The convergence criterion = 1.1102230246251565E-16
      
      //     Program to illustrate the idea of reverse communication
      //     in shift and invert mode for a generalized symmetric eigenvalue
      //     problem.  The following program uses the two LAPACK subroutines 
      //     dgttrf.f and dgttrs to factor and solve a tridiagonal system of 
      //     equations.
      
      //     We implement example four of ex-sym.doc in DOCUMENTS directory
      
      // \Example-4
      //     ... Suppose we want to solve A*x = lambda*M*x in inverse mode,
      //         where A and M are obtained from the finite element discretrization
      //         of the 1-dimensional discrete Laplacian
      //                             d^2u / dx^2
      //         on the interval [0,1] with zero Dirichlet boundary condition
      //         using piecewise linear elements.
      
      //     ... OP = (inv[A - sigma*M])*M  and  B = M.
      
      //     ... Use mode 3 of DSAUPD.
      
      // \BeginLib
      
      // \Routines called:
      //     dsaupd  ARPACK reverse communication interface routine.
      //     dseupd  ARPACK routine that returns Ritz values and (optionally)
      //             Ritz vectors.
      //     dgttrf  LAPACK tridiagonal factorization routine.
      //     dgttrs  LAPACK tridiagonal solve routine.
      //     daxpy   Level 1 BLAS that computes y <- alpha*x+y.
      //     dcopy   Level 1 BLAS that copies one vector to another.
      //     dscal   Level 1 BLAS that scales a vector by a scalar.
      //     dnrm2   Level 1 BLAS that computes the norm of a vector.
      //     av4      Matrix vector multiplication routine that computes A*x.
      //     mv4      Matrix vector multiplication routine that computes M*x.
       
      // \Author
      //     Richard Lehoucq
      //     Danny Sorensen
      //     Chao Yang
      //     Dept. of Computational &
      //     Applied Mathematics
      //     Rice University
      //     Houston, Texas
      
      // \SCCS Information: @(#)
      // FILE: sdrv4.F   SID: 2.4   DATE OF SID: 4/22/96   RELEASE: 2
      
      // \Remarks
      //     1. None
      
      // \EndLib
      // ----------------------------------------------------------------------     
      
      //     %-----------------------------%
      //     | Define leading dimensions   |
      //     | for all arrays.             |
      //     | MAXN:   Maximum dimension   |
      //     |         of the A allowed.   |
      //     | MAXNEV: Maximum NEV allowed |
      //     | MAXNCV: Maximum NCV allowed |
      //     %-----------------------------%
    	  
    	  final int maxn = 256;
          final int maxnev = 10;
          final int maxncv = 25;
          final int ldv = maxn;
    	 
    //     %--------------%
    //     | Local Arrays |
    //     %--------------%
          double v[][] = new double[ldv][maxncv];
          double workl[] = new double[maxncv*(maxncv+8)];
          double workd[] = new double[3*maxn];
          double d[][] = new double[maxncv][2];
          double ds[] = new double[2 * maxncv];
          double resid[] = new double[maxn];
          double ad[] = new double[maxn];
          double adl[] = new double[maxn];
          double adu[] = new double[maxn];
          double adu2[] = new double[maxn];
          boolean select[] = new boolean[maxncv];
          int iparam[] = new int[11];
          int ipntr[] = new int[11];
          int ipiv[] = new int[maxn];
          
          //     %---------------%
          //     | Local Scalars |
          //     %---------------%
          
        String bmat;
        String which;
        int ido[] = new int[1];
        int info[] = new int[1];
        int ierr[] = new int[1];
        int nconv = 0;
        int              n, nev, ncv, lworkl, i, j,
                         maxitr, ishfts, mode;
        boolean          rvec;
        double sigma;
        double tol[] = new double[]{0.0};
        double r1, r2, h;
        double array[][];
        int index;
        double v1[];
        double v2[];
  
  //     %------------%
  //     | Parameters |
  //     %------------%
  
        final double zero = 0.0;
        final double one = 1.0;
        final double two = 2.0;
        final double four = 4.0;
        final double six = 6.0;
      
      //     %-----------------------------%
      //     | BLAS & LAPACK routines used |
      //     %-----------------------------%
      
      //      Double precision
      //     &                 dnrm2
      //      external         daxpy, dcopy, dscal, dnrm2, dgttrf, dgttrs
      
      //     %--------------------%
      //     | Intrinsic function |
      //     %--------------------%
      
      //      intrinsic        abs
      
      //     %-----------------------%
      //     | Executable statements |
      //     %-----------------------%
      
      //     %----------------------------------------------------%
      //     | The number N is the dimension of the matrix.  A    |
      //     | generalized eigenvalue problem is solved (BMAT =   |
      //     | 'G'.) NEV is the number of eigenvalues (closest to |
      //     | the shift SIGMA) to be approximated.  Since the    |
      //     | shift-invert mode is used, WHICH is set to 'LM'.   |
      //     | The user can modify NEV, NCV, SIGMA to solve       |
      //     | problems of different sizes, and to get different  |
      //     | parts of the spectrum. However, The following      |
      //     | conditions must be satisfied:                      |
      //     |                   N <= MAXN,                       | 
      //     |                 NEV <= MAXNEV,                     |
      //     |             NEV + 1 <= NCV <= MAXNCV               | 
      //     %----------------------------------------------------% 
      
            n = 100;
            nev = 4;
            ncv = 10;
            if ( n > maxn ) {
               UI.setDataText("ERROR with DSDRV4: N is greater than MAXN \n");
               return;
            }
            else if ( nev > maxnev ) {
               UI.setDataText("ERROR with DSDRV4: NEV is greater than MAXNEV \n");
               return;
            }
            else if ( ncv > maxncv ) {
               UI.setDataText("ERROR with DSDRV4: NCV is greater than MAXNCV \n");
               return;
            }
            bmat = "G";
            which = "LM";
            sigma = zero; 
      
      //     %--------------------------------------------------%
      //     | The work array WORKL is used in DSAUPD as        |
      //     | workspace.  Its dimension LWORKL is set as       |
      //     | illustrated below.  The parameter TOL determines |
      //     | the stopping criterion.  If TOL<=0, machine      |
      //     | precision is used.  The variable IDO is used for |
      //     | reverse communication and is initially set to 0. |
      //     | Setting INFO=0 indicates that a random vector is |
      //     | generated in DSAUPD to start the Arnoldi         |
      //     | iteration.                                       |
      //     %--------------------------------------------------%
      
            lworkl = ncv*(ncv+8);
            ido[0] = 0;
            info[0] = 0;
      
      //     %---------------------------------------------------%
      //     | This program uses exact shifts with respect to    |
      //     | the current Hessenberg matrix (IPARAM(1) = 1).    |
      //     | IPARAM(3) specifies the maximum number of Arnoldi |
      //     | iterations allowed.  Mode 3 specified in the      |
      //     | documentation of DSAUPD is used (IPARAM(7) = 3).  |
      //     | All these options may be changed by the user.     |
      //     | For details, see the documentation in DSAUPD.     |
      //     %---------------------------------------------------%
      
            ishfts = 1;
            maxitr = 300;
            mode   = 3;
      
            iparam[0] = ishfts;
            iparam[2] = maxitr; 
            iparam[6] = mode ;
      
      //     %-------------------------------------------------------%
      //     | Call LAPACK routine to factor the tridiagonal matrix  |
      //     | (A-SIGMA*M).  The matrix A is the 1-d discrete        |
      //     | Laplacian. The matrix M is the associated mass matrix |
      //     | arising from using piecewise linear finite elements   |
      //     | on the interval [0, 1].                               |
      //     %-------------------------------------------------------%
      
            h = one / (double)(n+1);
            r1 = (four / six) * h;
            r2 = (one / six) * h;
            for (j=0; j < n; j++) {
               ad[j] = two/h - sigma * r1;
               adl[j] = -one/h - sigma * r2;
            }
            for (j = 0; j < n; j++) {
            	adu[j] = adl[j];
            }
            dgttrf (n, adl, ad, adu, adu2, ipiv, ierr);
            if (ierr[0] != 0) {
               UI.setDataText("Error with dgttrf in dsdrv4.\n");
               return;
            }
      
      //     %-------------------------------------------%
      //     | M A I N   L O O P (Reverse communication) |
      //     %-------------------------------------------%
      
      while (true) {
      
      //        %---------------------------------------------%
      //        | Repeatedly call the routine DSAUPD and take |
      //        | actions indicated by parameter IDO until    |
      //        | either convergence is indicated or maxitr   |
      //        | has been exceeded.                          |
      //        %---------------------------------------------%
      
               dsaupd ( ido, bmat, n, which, nev, tol, resid,
                        ncv, v, ldv, iparam, ipntr, workd, workl,
                        lworkl, info );
      
               if (ido[0] == -1) {
      
      //           %--------------------------------------------%
      //           | Perform  y <--- OP*x = inv[A-SIGMA*M]*M*x  |
      //           | to force the starting vector into the      |
      //           | range of OP.  The user should supply       |
      //           | his/her own matrix vector multiplication   |
      //           | routine and a linear system solver here.   |
      //           | The matrix vector multiplication routine   |
      //           | takes workd(ipntr(1)) as the input vector. |
      //           | The final result is returned to            |
      //           | workd(ipntr(2)).                           |
      //           %--------------------------------------------%
      
                  v1 = new double[n];
                  for (i = 0; i < n; i++) {
                	  v1[i] = workd[ipntr[0]-1+i];
                  }
                  v2 = new double[n];
            	  mv4 (n, v1, v2);
            	  for (i = 0; i < n; i++) {
            		  workd[ipntr[1]-1+i] = v2[i];
            	  }
      
            	  array = new double[n][1];
              	  for (i = 0; i < n; i++) {
              		array[i][0] = workd[ipntr[1]-1+i];
              	  }
                  dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv, 
                          array, n, ierr); 
                  for (i = 0; i < n; i++) {
              		workd[ipntr[1]-1+i] = array[i][0];
              	  }
                  if (ierr[0] != 0) { 
                	 UI.setDataText("Error with dgttrs in dsdrv4\n");
                	 return;
                  }
      //           %-----------------------------------------%
      //           | L O O P   B A C K to call DSAUPD again. |
      //           %-----------------------------------------%
               } // if (ido[0] == -1)
                  else if (ido[0] == 1) {
      
      //           %-----------------------------------------%
      //           | Perform y <-- OP*x = inv[A-sigma*M]*M*x |
      //           | M*x has been saved in workd(ipntr(3)).  |
      //           | the user only needs the linear system   |
      //           | solver here that takes workd(ipntr(3)   |
      //           | as input, and returns the result to     |
      //           | workd(ipntr(2)).                        | 
      //           %-----------------------------------------%
      
                  for (i = 0; i < n; i++) {
                	  workd[ipntr[1]-1+i] = workd[ipntr[2]-1+i];
                  }
                  array = new double[n][1];
              	  for (i = 0; i < n; i++) {
              		array[i][0] = workd[ipntr[1]-1+i];
              	  }
                  dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv, 
                          array, n, ierr);
                  for (i = 0; i < n; i++) {
                		workd[ipntr[1]-1+i] = array[i][0];
                  }
                  if (ierr[0] != 0) { 
                 	 UI.setDataText("Error with dgttrs in dsdrv4\n");
                 	 return;
                   }
                  } // else if (ido[0] == 1)
                  else if (ido[0] == 2) {
      
      //           %-----------------------------------------%
      //           |          Perform  y <--- M*x            |
      //           | Need the matrix vector multiplication   |
      //           | routine here that takes workd(ipntr(1)) |
      //           | as the input and returns the result to  |
      //           | workd(ipntr(2)).                        |
      //           %-----------------------------------------%
      
                  v1 = new double[n];
                  for (i = 0; i <n; i++) {
                    v1[i] = workd[ipntr[0]-1+i];
                  }
                  v2 = new double[n];
                  mv4 (n, v1, v2);
                  for (i = 0; i < n; i++) {
              		workd[ipntr[1]-1+i] = v2[i];
              	}
      
      //           %-----------------------------------------%
      //           | L O O P   B A C K to call DSAUPD again. |
      //           %-----------------------------------------%
             } // else if (ido[0] == 2)
                  else {
                	  break;
                  }
      
      } // while (true)
      
      //     %-----------------------------------------%
      //     | Either we have convergence, or there is |
      //     | an error.                               |
      //     %-----------------------------------------%
      
            if ( info[0] < 0 ) {
      
      //        %--------------------------%
      //        | Error message, check the |
      //        | documentation in DSAUPD. |
      //        %--------------------------%
      
               UI.setDataText("Error with dsaupd info[0] = " + info[0] + "\n");
            } // if ( info[0] < 0 )
            else { // info[0] >= 0
      
      //        %-------------------------------------------%
      //        | No fatal errors occurred.                 |
      //        | Post-Process using DSEUPD.                |
      //        |                                           |
      //        | Computed eigenvalues may be extracted.    |
      //        |                                           |
      //        | Eigenvectors may also be computed now if  |
      //        | desired.  (indicated by rvec = .true.)    |
      //        %-------------------------------------------%
      //
               rvec = true;
      
               dseupd ( rvec, "A", select, ds, v, ldv, sigma,
                   bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                   iparam, ipntr, workd, workl, lworkl, ierr );
               index = 0;
               for (j = 0; j < 2; j++) {
              	 for (i = 0; i < maxncv; i++) {
              	     d[i][j] = ds[index++]; 
              	 }
               }
      
      //        %----------------------------------------------%
      //        | Eigenvalues are returned in the first column |
      //        | of the two dimensional array D and the       |
      //        | corresponding eigenvectors are returned in   |
      //        | the first NEV columns of the two dimensional |
      //        | array V if requested.  Otherwise, an         |
      //        | orthogonal basis for the invariant subspace  |
      //        | corresponding to the eigenvalues in D is     |
      //        | returned in V.                               |
      //        %----------------------------------------------%
      //
               if ( ierr[0] != 0 ) { 
            	   UI.setDataText("Error with dseupd ierr[0] = " + ierr[0] + "\n");
               } // if (ierr[0] != 0)
               else { // ierr[0] == 0
      
                  nconv =  iparam[4];
                  for (j = 0; j < nconv; j++) {
      
      //              %---------------------------%
      //              | Compute the residual norm |
      //              |                           |
      //              |   ||  A*x - lambda*x ||   |
      //              |                           |
      //              | for the NCONV accurately  |
      //              | computed eigenvalues and  |
      //              | eigenvectors.  (iparam(5) |
      //              | indicates how many are    |
      //              | accurate to the requested |
      //              | tolerance)                |
      //              %---------------------------%
        
                     v1 = new double[n];
                     for (i = 0; i < n; i++) {
                    	 v1[i] = v[i][j];
                     }
                	 av4(n, v1, workd);
                	 v2 = new double[n];
                     mv4(n, v1, v2);
                     for (i = 0; i < n; i++) {
                    	 workd[n+i] = v2[i];
                     }
                     for (i = 0; i < n; i++) {
                    	 workd[i] = workd[i] + (-d[j][0])*workd[n+i];
                     }
                     d[j][1] =  ge.dnrm2(n, workd, 1);
                     d[j][1] = d[j][1] / Math.abs(d[j][0]);
                  } // for (j = 0; j < nconv; j++)
                              UI.setDataText("Ritz values and relative residuals: \n");
                              for (i = 0; i < nconv; i++) {
                             	 UI.setDataText("d["+i+"][0] = " + nf.format(d[i][0]) + " d["+i+"][1] = " + nf.format(d[i][1]) + "\n");
                              }
               } // else ierr[0] == 0
               //        %------------------------------------------%
               //        | Print additional convergence information |
               //        %------------------------------------------%
                        
                        if ( info[0] == 1) {
                        	UI.setDataText("Maximum number of iterations reached.\n");
                         }
                         else if ( info[0] == 3) {
                        	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
                         }     
                
                         UI.setDataText("\n");
                         UI.setDataText("DSDRV4\n");
                         UI.setDataText("======\n");
                         UI.setDataText("\n");
                         UI.setDataText("Size of the matrix = " +  n + "\n");
                         UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
                         UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
                         UI.setDataText("What portion of the spectrum: " +  which + "\n");
                         UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
                         UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
                         UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
                         UI.setDataText("The convergence criterion = " + tol[0] + "\n");
                         UI.setDataText("\n");
            } // else info[0] >= 0
            return;
      } // dsdrv4
      // ------------------------------------------------------------------------
      //     matrix vector subroutine
      //     The matrix used is the 1 dimensional mass matrix
      //     on the interval [0,1].
      
            private void mv4 (int n, double v[], double w[]) {
            int         j;
            //Double precision
            //&                v(n),w(n)
            double h;
            final double one = 1.0;
            final double four = 4.0;
            final double six = 6.0;
            
            w[0] =  four*v[0] + v[1];
            for (j = 1; j < n-1; j++) {
               w[j] = v[j-1] + four*v[j] + v[j+1]; 
            }
            j = n-1;
            w[j] = v[j-1] + four*v[j]; 
      
      //     Scale the vector w by h.
      
            h = one / ( six*(double)(n+1));
            for (j = 0; j < n; j++) {
            	w[j] = h * w[j];
            }
            return;
            } // mv4
      // ------------------------------------------------------------------------
      //     matrix vector subroutine
      //     where the matrix is the finite element discretization of the 
      //     1 dimensional discrete Laplacian on [0,1] with zero Dirichlet 
      //     boundary condition using piecewise linear elements.
      
            private void av4 (int n, double v[], double w[]) {
            int           j;
            //Double precision
            //&                  v(n), w(n)
            double h;
            final double one = 1.0;
            final double two = 2.0;
            
            w[0] =  two*v[0] - v[1];
            for (j = 1; j < n-1; j++) {
               w[j] = - v[j-1] + two*v[j] - v[j+1]; 
            }
            j = n-1;
            w[j] = - v[j-1] + two*v[j]; 
      
      //     Scale the vector w by (1/h)
      
            h = one / (double)(n+1);
            for (j = 0; j < n; j++) {
            	w[j] = (one/h) * w[j];
            }
            return;
            } // av4

        public void dsdrv5() {
        // Running the original FORTRAN gave:
//        	 Ritz values and relative residuals
//        	 ----------------------------------
//        	               Col   1       Col   2
//        	  Row   1:    9.87040D+00   6.05293D-14
//        	  Row   2:    3.94912D+01   3.13129D-14
//        	  Row   3:    8.88909D+01   2.25381D-14
//        	  Row   4:    1.58117D+02   2.96018D-14
        	  
        	  
//        	  _SDRV5 
//        	  ====== 
        	  
//        	  Size of the matrix is         100
//        	  The number of Ritz values requested is           4
//        	  The number of Arnoldi vectors generated (NCV) is           10
//        	  What portion of the spectrum: LM
//        	  The number of converged Ritz values is            4
//        	  The number of Implicit Arnoldi update iterations taken is           4
//        	  The number of OP*x is           24
//        	  The convergence criterion is    1.1102230246251565E-016
        	
        //  Running the Java gave:
//        	Ritz values and relative residuals: 
//    		d[0][0] = 9.87040E0 d[0][1] = 7.14257E-14
//    		d[1][0] = 3.94912E1 d[1][1] = 3.83807E-14
//    		d[2][0] = 8.88909E1 d[2][1] = 3.49951E-14
//    		d[3][0] = 1.58117E2 d[3][1] = 4.06059E-14

//    		DSDRV5
//    		======

//    		Size of the matrix = 100
//    		The number of Ritz values requested = 4
//    		The number of Arnoldi vectors generated ncv = 10
//    		What portion of the spectrum: LM
//    		The number of converged Ritz values = 4
//    		The number of Implicit Arnoldi update iterations taken = 4
//    		The number of OP*x = 24
//    		The convergence criterion = 1.1102230246251565E-16
        
        //     Program to illustrate the idea of reverse communication
        //     in Buckling mode for a generalized symmetric eigenvalue
        //     problem.  The following program uses the two LAPACK subroutines 
        //     dgttrf.f and dgttrs.f to factor and solve a tridiagonal system of 
        //     equations.
        
        //     We implement example five of ex-sym.doc in DOCUMENTS directory
        
        // \Example-5
        //     ... Suppose we want to solve K*x = lambda*KG*x in Buckling mode
        //         where K and KG are obtained by the finite element of the
        //         1-dimensional discrete Laplacian
        //                             d^2u / dx^2
        //         on the interval [0,1] with zero Dirichlet boundary condition
        //         using piecewise linear elements.
        //     ... OP = (inv[K - sigma*KG])*K  and  B = K.
        //     ... Use mode 4 of DSAUPD.
        
        // \BeginLib
        
        // \References:
        //  1. R.G. Grimes, J.G. Lewis and H.D. Simon, "A Shifted Block Lanczos 
        //     Algorithm for Solving Sparse Symmetric Generalized Eigenproblems", 
        //     SIAM J. Matr. Anal. Apps.,  January (1993).
        
        // \Routines called:
        //     dsaupd  ARPACK reverse communication interface routine.
        //     dseupd  ARPACK routine that returns Ritz values and (optionally)
        //             Ritz vectors.
        //     dgttrf  LAPACK tridiagonal factorization routine.
        //     dgttrs  LAPACK tridiagonal solve routine.
        //     daxpy   Level 1 BLAS that computes y <- alpha*x+y.
        //     dcopy   Level 1 BLAS that copies one vector to another.
        //     dscal   Level 1 BLAS that scales a vector by a scalar.
        //     dnrm2   Level 1 BLAS that computes the norm of a vector.   
        //     av5      Matrix vector multiplication routine that computes A*x.
        //     mv5      Matrix vector multiplication routine that computes M*x.
        
        // \Author
        //     Richard Lehoucq
        //     Danny Sorensen
        //     Chao Yang
        //     Dept. of Computational &
        //     Applied Mathematics
        //     Rice University
        //     Houston, Texas
        
        // \SCCS Information: @(#)
        // FILE: sdrv5.F   SID: 2.4   DATE OF SID: 4/22/96   RELEASE: 2
        
        // \Remarks
        //     1. None
        
        // \EndLib
        // ----------------------------------------------------------------------     
        
        //     %-----------------------------%
        //     | Define leading dimensions   |
        //     | for all arrays.             |
        //     | MAXN:   Maximum dimension   |
        //     |         of the A allowed.   |
        //     | MAXNEV: Maximum NEV allowed |
        //     | MAXNCV: Maximum NCV allowed |
        //     %-----------------------------%
            
              final int maxn = 256;
              final int maxnev = 10;
              final int maxncv = 25;
              final int ldv = maxn;
        	 
        //     %--------------%
        //     | Local Arrays |
        //     %--------------%
              double v[][] = new double[ldv][maxncv];
              double workl[] = new double[maxncv*(maxncv+8)];
              double workd[] = new double[3*maxn];
              double d[][] = new double[maxncv][2];
              double ds[] = new double[2 * maxncv];
              double resid[] = new double[maxn];
              double ad[] = new double[maxn];
              double adl[] = new double[maxn];
              double adu[] = new double[maxn];
              double adu2[] = new double[maxn];
              double ax[] = new double[maxn];
              double mx[] = new double[maxn];
              boolean select[] = new boolean[maxncv];
              int iparam[] = new int[11];
              int ipntr[] = new int[11];
              int ipiv[] = new int[maxn];
              
              //     %---------------%
              //     | Local Scalars |
              //     %---------------%
              
            String bmat;
            String which;
            int ido[] = new int[1];
            int info[] = new int[1];
            int ierr[] = new int[1];
            int nconv = 0;
            int              n, nev, ncv, lworkl, i, j,
                             maxitr, ishfts, mode;
            boolean          rvec;
            double sigma;
            double tol[] = new double[]{0.0};
            double r1, r2, h;
            double array[][];
            int index;
            double v1[];
            double v2[];
      
      //     %------------%
      //     | Parameters |
      //     %------------%
      
            final double one = 1.0;
            final double two = 2.0;
            final double four = 4.0;
            final double six = 6.0;
        
        //     %-----------------------------%
        //     | BLAS & LAPACK routines used |
        //     %-----------------------------%
        
        //      Double precision
        //     &                 dnrm2
        //      external         daxpy, dcopy, dscal, dnrm2, dgttrf, dgttrs
        
        //     %--------------------%
        //     | Intrinsic function |
        //     %--------------------%
        
        //       intrinsic        abs
        
        //     %-----------------------%
        //     | Executable statements |
        //     %-----------------------%
        
        //     %--------------------------------------------------%
        //     | The number N is the dimension of the matrix. A   |
        //     | generalized eigenvalue problem is solved (BMAT = |
        //     | 'G'.) NEV is the number of eigenvalues to be     |
        //     | approximated.  Since the buckling mode is used,  |
        //     | WHICH is set to 'LM'. The user can modify NEV,   |
        //     | NCV, SIGMA to solve problems of different sizes, |
        //     | and to get different parts of the spectrum.      |
        //     | However, The following conditions must be        |
        //     | satisfied:                                       |
        //     |                 N <= MAXN,                       | 
        //     |               NEV <= MAXNEV,                     |
        //     |           NEV + 1 <= NCV <= MAXNCV               |
        //     |                                                  | 
        //     | The  shift SIGMA cannot be zero!!!               |
        //     %--------------------------------------------------% 
        
              n = 100;
              nev = 4;
              ncv = 10;
              if ( n > maxn ) {
                 UI.setDataText("ERROR with DSDRV5: N is greater than MAXN \n");
                 return;
              }
              else if ( nev > maxnev ) {
                 UI.setDataText("ERROR with DSDRV5: NEV is greater than MAXNEV \n");
                 return;
              }
              else if ( ncv > maxncv ) {
                 UI.setDataText("ERROR with _SDRV5: NCV is greater than MAXNCV \n");
                 return;
              }
              bmat = "G";
              which = "LM";
              sigma = one; 
        
        //     %-----------------------------------------------------%
        //     | The work array WORKL is used in DSAUPD as           |
        //     | workspace.  Its dimension LWORKL is set as          |
        //     | illustrated below.  The parameter TOL determines    |
        //     | the stopping criterion. If TOL<=0, machine          |
        //     | precision is used.  The variable IDO is used for    |
        //     | reverse communication, and is initially set to 0.   |
        //     | Setting INFO=0 indicates that a random vector is    |
        //     | generated in DSAUPD to start the Arnoldi iteration. |
        //     %-----------------------------------------------------%
        
              lworkl = ncv*(ncv+8);
              ido[0] = 0;
              info[0] = 0;
        
        //     %---------------------------------------------------%
        //     | This program uses exact shifts with respect to    |
        //     | the current Hessenberg matrix (IPARAM(1) = 1).    |
        //     | IPARAM(3) specifies the maximum number of Arnoldi |
        //     | iterations allowed.  Mode 4 specified in the      |
        //     | documentation of DSAUPD is used (IPARAM(7) = 4).  |
        //     | All these options may be changed by the user. For |
        //     | details, see the documentation in DSAUPD.         |
        //     %---------------------------------------------------%
        
              ishfts = 1;
              maxitr = 300;
              mode   = 4;
        
              iparam[0] = ishfts;
              iparam[2] = maxitr; 
              iparam[6] = mode; 
        
        //     %------------------------------------------------------%
        //     | Call LAPACK routine to factor the tridiagonal matrix |
        //     | (K-SIGMA*KG).  The matrix A is the 1-d discrete      |
        //     | Laplacian on the interval [0,1] with zero Dirichlet  |
        //     | boundary condition.  The matrix M is the associated  |
        //     | mass matrix arising from using piecewise linear      |
        //     | finite elements on the interval [0, 1].              |
        //     %------------------------------------------------------%
        
              h = one / (double)(n+1);
              r1 = (four / six) * h;
              r2 = (one / six) * h;
              for (j=0; j < n; j++) {
                 ad[j] = two / h - sigma * r1;
                 adl[j] = -one / h- sigma * r2;
              }
              for (j = 0; j < n; j++) {
            	  adu[j] = adl[j];
              }
              dgttrf (n, adl, ad, adu, adu2, ipiv, ierr);
              if (ierr[0] !=  0) { 
                 UI.setDataText("Error with dgttrf in DSDRV5.\n");
                 return;
              }
        
        //     %-------------------------------------------%
        //     | M A I N   L O O P (Reverse communication) |
        //     %-------------------------------------------%
        
        while (true) {
        
        //        %---------------------------------------------%
        //        | Repeatedly call the routine DSAUPD and take |
        //        | actions indicated by parameter IDO until    |
        //        | either convergence is indicated or maxitr   |
        //        | has been exceeded.                          |
        //        %---------------------------------------------%
        
                 dsaupd ( ido, bmat, n, which, nev, tol, resid, 
                     ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, 
                     info );
        
                 if (ido[0] == -1) {
        
        //           %-------------------------------------------%
        //           | Perform y <--- OP*x = inv[K-SIGMA*KG]*K*x |
        //           | to force starting vector into the range   |
        //           | of OP.  The user should provide his/her   |
        //           | matrix vector multiplication routine and  |
        //           | a linear system solver here.  The matrix  |
        //           | vector multiplication routine (K*x) takes |
        //           | workd(ipntr(1)) as the input vector.  The |
        //           | final result is returned to               |
        //           | workd(ipntr(2)).                          |
        //           %-------------------------------------------%
        
                     v1 = new double[n];
                     for (i = 0; i < n; i++) {
                    	 v1[i] = workd[ipntr[0]-1+i];
                     }
                     v2 = new double[n];
                	 av5 (n, v1, v2);
                	 for (i = 0; i < n; i++) {
                		 workd[ipntr[1]-1+i] = v2[i];
                	 }
        
                	 array = new double[n][1];
                 	  for (i = 0; i < n; i++) {
                 		array[i][0] = workd[ipntr[1]-1+i];
                 	  }
                	 dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv, 
                                array, n, ierr);
                     for (i = 0; i < n; i++) {
                    	 workd[ipntr[1]-1+i] = array[i][0];
                     }
                    if (ierr[0] != 0) { 
                       UI.setDataText("Error with dgttrs in DSDRV5.\n");
                       return;
                    }
        
        //           %-----------------------------------------%
        //           | L O O P   B A C K to call DSAUPD again. |
        //           %-----------------------------------------%
                 } // if (ido[0] == -1)
                    else if (ido[0] == 1) {
        
        //           %------------------------------------------%
        //           | Perform y <-- OP*x=inv(K-sigma*KG)*K*x.  |
        //           | K*x has been saved in workd(ipntr(3)).   |
        //           | The user only needs the linear system    |
        //           | solver here that takes workd(ipntr(3))   |
        //           | as input, and returns the result to      |
        //           | workd(ipntr(2)).                         |
        //           %------------------------------------------%
        
                    for (i = 0; i < n; i++) {
                    	workd[ipntr[1]-1+i] = workd[ipntr[2]-1+i];
                    }
                    array = new double[n][1];
               	    for (i = 0; i < n; i++) {
               		  array[i][0] = workd[ipntr[1]-1+i];
               	    }
                    dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv, 
                                array, n, ierr);
                    for (i = 0; i < n; i++) {
                   	 workd[ipntr[1]-1+i] = array[i][0];
                    }
                    if (ierr[0] != 0) { 
                       UI.setDataText("Error with dgttrs in DSDRV5.\n");
                       return;
                    }
        
        //           %-----------------------------------------%
        //           | L O O P   B A C K to call DSAUPD again. |
        //           %-----------------------------------------%
                    } // else if (ido[0] == 1)
                 else if (ido[0] == 2) {
        
        //           %---------------------------------------------%
        //           |          Perform  y <--- K*x                |
        //           | Need matrix vector multiplication routine   |
        //           | here that takes workd(ipntr(1)) as input    |
        //           | and returns the result to workd(ipntr(2)).  |
        //           %---------------------------------------------%
        //
			        v1 = new double[n];
			        for (i = 0; i < n; i++) {
			       	 v1[i] = workd[ipntr[0]-1+i];
			        }
			        v2 = new double[n];
                    av5 (n, v1, v2);
                    for (i = 0; i < n; i++) {
               		   workd[ipntr[1]-1+i] = v2[i];
               	    }
        
        
        //           %-----------------------------------------%
        //           | L O O P   B A C K to call DSAUPD again. |
        //           %-----------------------------------------%
                 } // else if (ido[0] == 2)
                 else {
                	 break;
                 }
        } // while (true)
        
        //     %-----------------------------------------%
        //     | Either we have convergence, or there is |
        //     | an error.                               |
        //     %-----------------------------------------%
        
              if ( info[0] < 0 ) {
        
        //        %--------------------------%
        //        | Error message, check the |
        //        | documentation in DSAUPD. |
        //        %--------------------------%
        
                 UI.setDataText("Error with dsaupd, info[0] = " + info[0] + "\n");
              } // if (info[0] < 0)
              else { // info[0] >= 0
        
        //        %-------------------------------------------%
        //        | No fatal errors occurred.                 |
        //        | Post-Process using DSEUPD.                |
        //        |                                           |
        //        | Computed eigenvalues may be extracted.    |
        //        |                                           |
        //        | Eigenvectors may also be computed now if  |
        //        | desired.  (indicated by rvec = .true.)    |
        //        %-------------------------------------------%
        
                 rvec = true;
        
                 dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                     bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                     iparam, ipntr, workd, workl, lworkl, ierr );
                 index = 0;
                 for (j = 0; j < 2; j++) {
                	 for (i = 0; i < maxncv; i++) {
                	     d[i][j] = ds[index++]; 
                	 }
                 }
        
                 if (ierr[0] != 0) { 
        
        //           %------------------------------------%
        //           | Error condition:                   |
        //           | Check the documentation of DSEUPD. |
        //           %------------------------------------%
        
                    UI.setDataText("Error with dseupd, ierr[0]  = " + ierr[0] + "\n");
                 } // if (ierr[0] != 0)
                 else { // ierr[0] == 0
        
                    nconv =  iparam[4]; 
                    for (j=0; j < nconv; j++) {
        
        //              %---------------------------%
        //              | Compute the residual norm |
        //              |                           |
        //              |   ||  A*x - lambda*x ||   |
        //              |                           |
        //              | for the NCONV accurately  |
        //              | computed eigenvalues and  |
        //              | eigenvectors.  (iparam(5) |
        //              | indicates how many are    |
        //              | accurate to the requested |
        //              | tolerance)                |
        //              %---------------------------%
        
                       v1 = new double[n];
                       for (i = 0; i < n; i++) {
                    	   v1[i] = v[i][j] ;  
                       }
                       av5(n, v1, ax);
                       mv5(n, v1, mx);
                       for (i = 0; i < n; i++) {
                    	   ax[i] = ax[i] + (-d[j][0])*mx[i];
                       }
                       d[j][1] =  ge.dnrm2(n, ax, 1);
                       d[j][1] = d[j][1] / Math.abs(d[j][0]);
                    } // for (j = 0; j < nconv; j++)
                    UI.setDataText("Ritz values and relative residuals: \n");
                    for (i = 0; i < nconv; i++) {
                   	 UI.setDataText("d["+i+"][0] = " + nf.format(d[i][0]) + " d["+i+"][1] = " + nf.format(d[i][1]) + "\n");
                    }
                 } // if (ierr[0] == 0)
                 //        %------------------------------------------%
                 //        | Print additional convergence information |
                 //        %------------------------------------------%
                          
                          if ( info[0] == 1) {
                          	UI.setDataText("Maximum number of iterations reached.\n");
                           }
                           else if ( info[0] == 3) {
                          	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
                           }     
                  
                           UI.setDataText("\n");
                           UI.setDataText("DSDRV5\n");
                           UI.setDataText("======\n");
                           UI.setDataText("\n");
                           UI.setDataText("Size of the matrix = " +  n + "\n");
                           UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
                           UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
                           UI.setDataText("What portion of the spectrum: " +  which + "\n");
                           UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
                           UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
                           UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
                           UI.setDataText("The convergence criterion = " + tol[0] + "\n");
                           UI.setDataText("\n");
              } // if (info[0] >= 0)
              return;
        } // dsdrv5
        
        // ------------------------------------------------------------------------
        //     Matrix vector subroutine
        //     where the matrix is the 1-dimensional mass matrix
        //     arising from using piecewise linear finite elements on the 
        //     interval [0,1].
        
              private void mv5 (int n, double v[], double w[]) {
              int         j;
              //Double precision
              //&                v(n),w(n)
              double h;
              final double one = 1.0;
              final double four = 4.0;
              final double six = 6.0;
              
              w[0] =  four*v[0] + v[1];
              for (j = 1; j < n-1; j++) {
                 w[j] = v[j-1] + four*v[j] + v[j+1]; 
              }
              j = n-1;
              w[j] = v[j-1] + four*v[j]; 
        
        //     Scale the vector w by h.
        
              h = one / (six*(double)(n+1));
              for (j = 0; j < n; j++) {
            	  w[j] = h * w[j];
              }
              return;
              } // mv5
        // ------------------------------------------------------------------------
        //     Matrix vector subroutine
        //     where the matrix is the stiffness matrix obtained from the
        //     finite element discretization of the 1-dimensional discrete Laplacian
        //     on the interval [0,1] with zero Dirichlet boundary condition
        //     using piecewise linear elements.
        
              private void av5 (int n, double v[], double w[]) {
              int           j;
              //Double precision
              //&                  v(n), w(n)
              double h;
              final double one = 1.0;
              final double two = 2.0;
              
              w[0] =  two*v[0] - v[1];
              for ( j = 1; j < n-1; j++) {
                 w[j] = - v[j-1] + two*v[j] - v[j+1]; 
              }
              j = n-1;
              w[j] = - v[j-1] + two*v[j]; 
        
        //     Scale the vector w by (1/h)
        
              h = one / (n+1);
              for (j = 0; j < n; j++) {
                  w[j] = (one/h) * w[j];  
              }
              return;
              } // av5

      public void dsdrv6() { 
      // Running the original FORTRAN gave:
//    	  Ritz values and relative residuals
//    	  ----------------------------------
//    	                Col   1       Col   2
//    	   Row   1:    8.88909D+01   1.29327D-13
//    	   Row   2:    1.58117D+02   4.23220D-15
//    	   Row   3:    2.47238D+02   5.12768D-15
//    	   Row   4:    3.56338D+02   3.66649D-14
    	   
    	   
//    	   _SDRV6 
//    	   ====== 
    	   
//    	   Size of the matrix is         100
//    	   The number of Ritz values requested is           4
//    	   The number of Arnoldi vectors generated (NCV) is           20
//    	   What portion of the spectrum: LM
//    	   The number of converged Ritz values is            4
//    	   The number of Implicit Arnoldi update iterations taken is           2
//    	   The number of OP*x is           34
//    	   The convergence criterion is    1.1102230246251565E-016
    	  
      // Running the Java gave:
//    	  Ritz values and relative residuals: 
//		  d[0][0] = 8.88909E1 d[0][1] = 1.03162E-13
//		  d[1][0] = 1.58117E2 d[1][1] = 6.31679E-15
//		  d[2][0] = 2.47238E2 d[2][1] = 1.83362E-14
//		  d[3][0] = 3.56338E2 d[3][1] = 1.28592E-14

//		  DSDRV6
//		  ======

//		  Size of the matrix = 100
//		  The number of Ritz values requested = 4
//		  The number of Arnoldi vectors generated ncv = 20
//		  What portion of the spectrum: LM
//		  The number of converged Ritz values = 4
//		  The number of Implicit Arnoldi update iterations taken = 2
//		  The number of OP*x = 34
//		  The convergence criterion = 1.1102230246251565E-16
      
      //     Program to illustrate the idea of reverse communication
      //     in Cayley mode for a generalized symmetric eigenvalue 
      //     problem.  The following program uses the two LAPACK subroutines 
      //     dgttrf.f and dgttrs.f to factor and solve a tridiagonal system of 
      //     equations.
      
      //     We implement example six of ex-sym.doc in DOCUMENTS directory
      
      // \Example-6
      //     ... Suppose we want to solve A*x = lambda*M*x in inverse mode,
      //         where A and M are obtained by the finite element of the
      //         1-dimensional discrete Laplacian
      //                             d^2u / dx^2
      //         on the interval [0,1] with zero Dirichlet boundary condition
      //         using piecewise linear elements.
      
      //     ... OP = (inv[A-sigma*M])*(A+sigma*M)  and  B = M.
      
      //     ... Use mode 5 of DSAUPD.
      
      // \BeginLib
      
      // \References:
      //  1. R.G. Grimes, J.G. Lewis and H.D. Simon, "A Shifted Block Lanczos 
      //     Algorithm for Solving Sparse Symmetric Generalized Eigenproblems", 
      //     SIAM J. Matr. Anal. Apps.,  January (1993).
      
      // \Routines called:
      //     dsaupd  ARPACK reverse communication interface routine.
      //     dseupd  ARPACK routine that returns Ritz values and (optionally)
      //             Ritz vectors.
      //     dgttrf  LAPACK tridiagonal factorization routine.
      //     dgttrs  LAPACK tridiagonal solve routine.
      //     daxpy   Level 1 BLAS that computes y <- alpha*x+y.
      //     dcopy   Level 1 BLAS that copies one vector to another.
      //     dscal   Level 1 BLAS that scales a vector by a scalar.
      //     dnrm2   Level 1 BLAS that computes the norm of a vector. 
      //     av6      Matrix vector multiplication routine that computes A*x.
      //     mv6      Matrix vector multiplication routine that computes M*x.
      
      // \Author
      //     Danny Sorensen
      //     Richard Lehoucq
      //     Chao Yang
      //     Dept. of Computational &
      //     Applied Mathematics
      //     Rice University 
      //     Houston, Texas 
       
      // \SCCS Information: @(#)
      // FILE: sdrv6.F   SID: 2.4   DATE OF SID: 4/22/96   RELEASE: 2
      
      // \Remarks
      //     1. None
      
      //\EndLib
      // -----------------------------------------------------------------------
      
      //     %-----------------------------%
      //     | Define leading dimensions   |
      //     | for all arrays.             |
      //     | MAXN:   Maximum dimension   |
      //     |         of the A allowed.   |
      //     | MAXNEV: Maximum NEV allowed |
      //     | MAXNCV: Maximum NCV allowed |
      //     %-----------------------------%
    	  
    	  final int maxn = 256;
          final int maxnev = 10;
          final int maxncv = 25;
          final int ldv = maxn;
    	 
    //     %--------------%
    //     | Local Arrays |
    //     %--------------%
          double v[][] = new double[ldv][maxncv];
          double workl[] = new double[maxncv*(maxncv+8)];
          double workd[] = new double[3*maxn];
          double d[][] = new double[maxncv][2];
          double ds[] = new double[2 * maxncv];
          double resid[] = new double[maxn];
          double ad[] = new double[maxn];
          double adl[] = new double[maxn];
          double adu[] = new double[maxn];
          double adu2[] = new double[maxn];
          double temp[] = new double[maxn];
          double ax[] = new double[maxn];
          double mx[] = new double[maxn];
          boolean select[] = new boolean[maxncv];
          int iparam[] = new int[11];
          int ipntr[] = new int[11];
          int ipiv[] = new int[maxn];
          
          //     %---------------%
          //     | Local Scalars |
          //     %---------------%
          
        String bmat;
        String which;
        int ido[] = new int[1];
        int info[] = new int[1];
        int ierr[] = new int[1];
        int nconv = 0;
        int              n, nev, ncv, lworkl, i, j,
                         maxitr, ishfts, mode;
        boolean          rvec;
        double sigma;
        double tol[] = new double[]{0.0};
        double r1, r2, h;
        double array[][];
        int index;
        double v1[];
        double v2[];
  
  //     %------------%
  //     | Parameters |
  //     %------------%
  
        final double one = 1.0;
        final double two = 2.0;
        final double four = 4.0;
        final double six = 6.0;
    
    //     %-----------------------------%
    //     | BLAS & LAPACK routines used |
    //     %-----------------------------%
    
    //      Double precision
    //     &                 dnrm2
    //      external         daxpy, dcopy, dscal, dnrm2, dgttrf, dgttrs
    
    //     %--------------------%
    //     | Intrinsic function |
    //     %--------------------%
    
    //       intrinsic        abs
    
    //     %-----------------------%
    //     | Executable statements |
    //     %-----------------------%
      
      //     %--------------------------------------------------%
      //     | The number N is the dimension of the matrix. A   |
      //     | generalized eigenvalue problem is solved (BMAT = |
      //     | 'G'.) NEV is the number of eigenvalues to be     |
      //     | approximated.  Since the Cayley mode is used,    |
      //     | WHICH is set to 'LM'.  The user can modify NEV,  |
      //     | NCV, SIGMA to solve problems of different sizes, |
      //     | and to get different parts of the spectrum.      |
      //     | However, The following conditions must be        |
      //     | satisfied:                                       |
      //     |                 N <= MAXN,                       | 
      //     |               NEV <= MAXNEV,                     |
      //     |           NEV + 1 <= NCV <= MAXNCV               | 
      //     %--------------------------------------------------%
      
            n = 100;
            nev = 4;
            ncv = 20;
            if ( n > maxn ) {
               UI.setDataText("ERROR with DSDRV6: N is greater than MAXN \n");
               return;
            }
            else if ( nev > maxnev ) {
               UI.setDataText("ERROR with DSDRV6: NEV is greater than MAXNEV \n");
               return;
            }
            else if ( ncv > maxncv ) {
               UI.setDataText("ERROR with DSDRV6: NCV is greater than MAXNCV \n");
               return;
            }
            bmat = "G";
            which = "LM";
            sigma = 150.0;
      
      //     %--------------------------------------------------%
      //     | The work array WORKL is used in DSAUPD as        |
      //     | workspace.  Its dimension LWORKL is set as       |
      //     | illustrated below.  The parameter TOL determines |
      //     | the stopping criterion.  If TOL<=0, machine      |
      //     | precision is used.  The variable IDO is used for |
      //     | reverse communication and is initially set to 0. |
      //     | Setting INFO=0 indicates that a random vector is |
      //     | generated in DSAUPD to start the Arnoldi         |
      //     | iteration.                                       |
      //     %--------------------------------------------------%
      
            lworkl = ncv*(ncv+8);
            ido[0] = 0;
            info[0] = 0;
      
      //     %---------------------------------------------------%
      //     | This program uses exact shifts with respect to    |
      //     | the current Hessenberg matrix (IPARAM(1) = 1).    |
      //     | IPARAM(3) specifies the maximum number of Arnoldi |
      //     | iterations allowed.  Mode 5 specified in the      |
      //     | documentation of DSAUPD is used (IPARAM(7) = 5).  |
      //     | All these options may be changed by the user. For |
      //     | details, see the documentation in DSAUPD.         |
      //     %---------------------------------------------------%
      
            ishfts = 1;
            maxitr = 300;
            mode   = 5;
      
            iparam[0] = ishfts;
            iparam[2] = maxitr; 
            iparam[6] = mode; 
      
      //     %------------------------------------------------------%
      //     | Call LAPACK routine to factor (A-sigma*M).  The      |
      //     | stiffness matrix A is the 1-d discrete Laplacian.    |
      //     | The mass matrix M is the associated mass matrix      |
      //     | arising from using piecewise linear finite elements  |
      //     | on the interval [0, 1].                              |
      //     %------------------------------------------------------%
      
            h = one / (double)(n+1);
            r1 = (four / six) * h;
            r2 = (one / six) * h;
            for (j = 0; j < n; j++) {
               ad[j] = two / h - sigma * r1;
               adl[j] = -one / h - sigma * r2;
            } 
            for (j = 0; j < n; j++) {
            	adu[j] = adl[j];
            }
            dgttrf (n, adl, ad, adu, adu2, ipiv, ierr);
            if (ierr[0] != 0) { 
               UI.setDataText("Error with dgttrf in DSDRV6.\n");
               return;
            }
      
      //     %-------------------------------------------%
      //     | M A I N   L O O P (Reverse communication) |
      //     %-------------------------------------------%
      
      while (true) {
      
      //        %---------------------------------------------%
      //        | Repeatedly call the routine DSAUPD and take |
      //        | actions indicated by parameter IDO until    |
      //        | either convergence is indicated or maxitr   |
      //        | has been exceeded.                          |
      //        %---------------------------------------------%
      
               dsaupd ( ido, bmat, n, which, nev, tol, resid, ncv, 
                            v, ldv, iparam, ipntr, workd, workl, lworkl, 
                            info );
      
               if (ido[0] == -1) {
      
      //           %-------------------------------------------------------%
      //           | Perform  y <--- OP*x = (inv[A-SIGMA*M])*(A+SIGMA*M)*x |
      //           | to force starting vector into the range of OP.  The   |
      //           | user should provide his/her matrix vector (A*x, M*x)  |
      //           | multiplication routines and a linear system solver    |
      //           | here.  The matrix vector multiplication routine takes |
      //           | workd(ipntr(1)) as the input vector.  The final       |
      //           | result is returned to workd(ipntr(2)).                | 
      //           %-------------------------------------------------------%
      
                  v1 = new double[n];
                  for (i = 0; i < n; i++) {
                	  v1[i] = workd[ipntr[0]-1+i];
                  }
                  v2 = new double[n];
            	  av6 (n, v1, v2);
            	  for (i = 0; i < n; i++) {
            		  workd[ipntr[1]-1+i] = v2[i];
            	  }
                  mv6 (n, v1, temp);
                  for (i = 0; i < n; i++) {
                	  workd[ipntr[1]-1+i] = workd[ipntr[1]-1+i] + sigma * temp[i];
                  }
      
                  array = new double[n][1];
                  for (i = 0; i < n; i++) {
                	  array[i][0] = workd[ipntr[1]-1+i];
                  }
                  dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv, 
                           array, n, ierr);
                  for (i = 0; i < n; i++) {
                	  workd[ipntr[1]-1+i] = array[i][0];
                  }      
                  if (ierr[0] != 0) { 
                     UI.setDataText("Error with dgttrs in DSDRV6.\n");
                     return;
                  }
      
      //           %-----------------------------------------%
      //           | L O O P   B A C K to call DSAUPD again. |
      //           %-----------------------------------------%
               } // if (ido[0] == -1)
               else if (ido[0] == 1) {
      
      //           %----------------------------------------------------%
      //           | Perform y <-- OP*x = inv[A-SIGMA*M]*(A+SIGMA*M)*x. |
      //           | M*x has been saved in workd(ipntr(3)).  The user   |
      //           | need the matrix vector multiplication (A*x)        |
      //           | routine and a linear system solver here.  The      |
      //           | matrix vector multiplication routine takes         |
      //           | workd(ipntr(1)) as the input, and the result is    |
      //           | combined with workd(ipntr(3)) to form the input    |
      //           | for the linear system solver.  The final result is |
      //           | returned to workd(ipntr(2)).                       | 
      //           %----------------------------------------------------%
                 
            	   v1 = new double[n];
                   for (i = 0; i < n; i++) {
                 	  v1[i] = workd[ipntr[0]-1+i];
                   }
                   v2 = new double[n];
                  av6 (n, v1, v2);
                  for (i = 0; i < n; i++) {
            		  workd[ipntr[1]-1+i] = v2[i];
            	  }
                  for (i = 0; i < n; i++) {
                	  workd[ipntr[1]-1+i] = workd[ipntr[1]-1+i] + sigma * workd[ipntr[2]-1+i];
                  }
                  array = new double[n][1];
                  for (i = 0; i < n; i++) {
                	  array[i][0] = workd[ipntr[1]-1+i];
                  }
                  dgttrs ('N', n, 1, adl, ad, adu, adu2, ipiv, 
                              array, n, ierr);
                  for (i = 0; i < n; i++) {
                	  workd[ipntr[1]-1+i] = array[i][0];
                  }      
                  if (ierr[0] != 0) { 
                     UI.setDataText("Error with dgttrs in DSDRV6.\n");
                     return;
                  }
      
      //           %-----------------------------------------%
      //           | L O O P   B A C K to call DSAUPD again. |
      //           %-----------------------------------------%
               } // else if (ido[0] == 1)
               else if (ido[0] == 2) {
      
      //           %--------------------------------------------%
      //           |             Perform  y <--- M*x.           |
      //           | Need matrix vector multiplication routine  |
      //           | here that takes workd(ipntr(1)) as input   |
      //           | and returns the result to workd(ipntr(2)). |
      //           %--------------------------------------------%
      
            	   v1 = new double[n];
                   for (i = 0; i < n; i++) {
                 	  v1[i] = workd[ipntr[0]-1+i];
                   }
                   v2 = new double[n];
            	   mv6 (n, v1, v2);
            	   for (i = 0; i < n; i++) {
             		  workd[ipntr[1]-1+i] = v2[i];
             	  }
      
      //           %-----------------------------------------%
      //           | L O O P   B A C K to call DSAUPD again. |
      //           %-----------------------------------------%
               } // else if (ido[0] == 2)
               else {
            	   break;
               }
      } // while (true)
      
      //     %-----------------------------------------%
      //     | Either we have convergence, or there is |
      //     | an error.                               |
      //     %-----------------------------------------%
      
            if ( info[0] < 0 ) {
      
      //        %--------------------------%
      //        | Error message, check the |
      //        | documentation in DSAUPD  |
      //        %--------------------------%
      
                 UI.setDataText("Error with dsaupd, info[0] = " + info[0] + "\n");
            } // if (info[0] < 0)
            else { // info[0] >= 0
      
      //        %-------------------------------------------%
      //        | No fatal errors occurred.                 |
      //        | Post-Process using DSEUPD.                |
      //        |                                           |
      //        | Computed eigenvalues may be extracted.    |
      //        |                                           |
      //        | Eigenvectors may also be computed now if  |
      //        | desired.  (indicated by rvec = .true.)    |
      //        %-------------------------------------------%
      
               rvec = true;
      
               dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                   bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                   iparam, ipntr, workd, workl, lworkl, ierr );
    		  index = 0;
              for (j = 0; j < 2; j++) {
             	 for (i = 0; i < maxncv; i++) {
             	     d[i][j] = ds[index++]; 
             	 }
              }
      
      //        %----------------------------------------------%
      //        | Eigenvalues are returned in the first column |
      //        | of the two dimensional array D and the       |
      //        | corresponding eigenvectors are returned in   |
      //        | the first NEV columns of the two dimensional |
      //        | array V if requested.  Otherwise, an         |
      //        | orthogonal basis for the invariant subspace  |
      //        | corresponding to the eigenvalues in D is     |
      //        | returned in V.                               |
      //        %----------------------------------------------%
      
               if ( ierr[0] != 0 ) { 
      
      //           %------------------------------------%
      //           | Error condition:                   |
      //           | Check the documentation of DSEUPD. |
      //           %------------------------------------%
      
                  UI.setDataText("Error with dseupd ierr[0] = " + ierr[0] + "\n");
                  
               } // if (ierr[0] != 0)
               else { // ierr[0] == 0
      
      //           %---------------------------%
      //           | Compute the residual norm |
      //           |                           |
      //           |   ||  A*x - lambda*x ||   |
      //           |                           |
      //           | for the NCONV accurately  |
      //           | computed eigenvalues and  |
      //           | eigenvectors.  (iparam(5) |
      //           | indicates how many are    |
      //           | accurate to the requested |
      //           | tolerance)                |
      //           %---------------------------%
       
                  nconv = iparam[4];
                  for (j = 0; j < nconv; j++) {
                	 v1 = new double[n];
                	 for (i = 0; i < n; i++) {
                		 v1[i] = v[i][j];
                	 }
                     av6(n, v1, ax);
                     mv6(n, v1, mx);
                     for (i = 0; i < n; i++) {
                    	 ax[i] = ax[i] + (-d[j][0])*mx[i];
                     }
                     d[j][1] = ge.dnrm2(n, ax, 1);
                     d[j][1] = d[j][1] / Math.abs(d[j][0]);
                  }
                  UI.setDataText("Ritz values and relative residuals: \n");
                  for (i = 0; i < nconv; i++) {
                 	 UI.setDataText("d["+i+"][0] = " + nf.format(d[i][0]) + " d["+i+"][1] = " + nf.format(d[i][1]) + "\n");
                  }
               } // else ierr[0] == 0
               
               //        %------------------------------------------%
               //        | Print additional convergence information |
               //        %------------------------------------------%
                        
                        if ( info[0] == 1) {
                        	UI.setDataText("Maximum number of iterations reached.\n");
                         }
                         else if ( info[0] == 3) {
                        	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
                         }     
                
                         UI.setDataText("\n");
                         UI.setDataText("DSDRV6\n");
                         UI.setDataText("======\n");
                         UI.setDataText("\n");
                         UI.setDataText("Size of the matrix = " +  n + "\n");
                         UI.setDataText("The number of Ritz values requested = " +  nev + "\n");
                         UI.setDataText("The number of Arnoldi vectors generated ncv = " +  ncv + "\n");
                         UI.setDataText("What portion of the spectrum: " +  which + "\n");
                         UI.setDataText("The number of converged Ritz values = " +nconv + "\n");
                         UI.setDataText("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n");
                         UI.setDataText("The number of OP*x = " +  iparam[8] + "\n");
                         UI.setDataText("The convergence criterion = " + tol[0] + "\n");
                         UI.setDataText("\n");
            } // else info[0] >= 0
            return;
      } // dsdrv6
      
      // ------------------------------------------------------------------------
      //     Matrix vector subroutine
      //     where the matrix used is the 1 dimensional mass matrix
      //     arising from using the piecewise linear finite element
      //     on the interval [0,1].
      
            private void mv6 (int n, double v[], double w[]) {
            int           j;
            //Double precision 
            //&                  v(n), w(n), one, four, six, h
            double h;
            final double one = 1.0;
            final double four = 4.0;
            final double six = 6.0;
      
            w[0] =  four*v[0] + v[1];
            for (j = 1; j < n-1; j++) {
               w[j] = v[j-1] + four*v[j] + v[j+1]; 
            }
            j = n-1;
            w[j] = v[j-1] + four*v[j]; 
      
      //     Scale the vector w by h.
      
            h = one / (six*(double)(n+1));
            for (j = 0; j < n; j++) {
            	w[j] = h * w[j];
            }
            return;
            } // mv6
      
      // ------------------------------------------------------------------------
      //     Matrix vector subroutine
      //     where the matrix is the stiffness matrix obtained from the 
      //     finite element discretization of the 1-dimensional discrete Laplacian
      //     on the interval [0,1] with zero Dirichlet boundary condition
      //     using piecewise linear elements.
      
            private void av6 (int n, double v[], double w[]) {
            int           j;
            //Double precision
            //&                  v(n), w(n)
            double h;
            final double one = 1.0;
            final double two = 2.0;
            
            w[0] =  two*v[0] - v[1];
            for (j = 1; j < n-1; j++) {
               w[j] = - v[j-1] + two*v[j] - v[j+1]; 
            }
            j = n-1;
            w[j] = - v[j-1] + two*v[j];
           
      //     Scale the vector w by (1/h).
      
            h = one / (double)(n+1);
            for (j = 0; j < n; j++) {
            	w[j] = (one/h) * w[j];
            }
            return;
            } // av6
            
            /**
             * This dchkgt_test routine is a port of a portion of the version 3.4.1 LAPACK test routine DCHKAA by Univ. of
             * Tennessee, Univ. Of California Berkeley and NAG Ltd., April, 2012. and some values from the test data file
             * dtest.in.
             * 
             * Received message In dchkgt all 2694 tests run passed the threshold.
             */
            public void dchkgt_test() {

                // Number of values of n
                final int nn = 7;

                // Values of n (column dimension)
                // dtest.in uses 50 rather than 16
                final int[] nval = new int[] {0, 1, 2, 3, 5, 10, 16};
                
                // Number of values of nsval
                final int nns = 3;

                // Values of nsval or nrhs (number of right hand sides)
                final int[] nsval = new int[] {1, 2, 15};
                // Largest entry in nsval
                final int nsmax = 15;

                // Threshold value of test ratio
                // dchkaa has 20.0, dtest.in has 30.0
                final double thresh = 20.0;

                // The maximum allowable value for n
                final int nmax = 132;

                final int ntypes = 12                        ;
                final boolean dotype[] = new boolean[ntypes];
                final double A[] = new double[4*nmax];
                final double AF[] = new double[4*nmax];
                final double B[] = new double[nmax*nsmax];
                final double X[] = new double[nmax*nsmax];
                final double XACT[] = new double[nmax*nsmax];
                final double work[] = new double[nmax * Math.max(3,nsmax)];
                final double rwork[] = new double[Math.max(nmax,2*nsmax)];
                final int iwork[] = new int[2*nmax];
                
                int i;
                double eps;

                for (i = 0; i < ntypes; i++) {
                    dotype[i] = true;
                }

                // Output the machine dependent constants
                eps = ge.dlamch('U');
                Preferences.debug("Underflow threshold = " + eps + "\n");
                eps = ge.dlamch('O');
                Preferences.debug("Overflow threshold = " + eps + "\n");
                eps = ge.dlamch('E');
                Preferences.debug("Precision = " + eps + "\n");

                dchkgt(dotype, nn, nval, nns, nsval, thresh, A, AF, B, X,
                        XACT, work, rwork, iwork);
            } // dchkgt_test
            
       // \par Purpose:
	   // =============
	   
	   // \verbatim
	   
	   // DCHKGT tests DGTTRF, -TRS, -RFS, and -CON
	   // \endverbatim
	   
	   // Arguments:
	   // ==========
	  
	   // \param[in] DOTYPE
	   // \verbatim
	   //          DOTYPE is LOGICAL array, dimension (NTYPES)
	   //          The matrix types to be used for testing.  Matrices of type j
	   //          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
	   //          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
	   // \endverbatim
	   
	   // \param[in] NN
	   // \verbatim
	   //          NN is INTEGER
	   //          The number of values of N contained in the vector NVAL.
	   // \endverbatim
	  
	   // \param[in] NVAL
	   // \verbatim
	   //          NVAL is INTEGER array, dimension (NN)
	   //          The values of the matrix dimension N.
	   // \endverbatim
	   
	   // \param[in] NNS
	   // \verbatim
	   //          NNS is INTEGER
	   //          The number of values of NRHS contained in the vector NSVAL.
	   // \endverbatim
	   
	   // \param[in] NSVAL
	   // \verbatim
	   //          NSVAL is INTEGER array, dimension (NNS)
	   //          The values of the number of right hand sides NRHS.
	   // \endverbatim
	   
	   // \param[in] THRESH
	   // \verbatim
	   //          THRESH is DOUBLE PRECISION
	   //          The threshold value for the test ratios.  A result is
	   //          included in the output file if RESULT >= THRESH.  To have
	   //          every test ratio printed, use THRESH = 0.
	   // \endverbatim
	   
	   // \param[in] TSTERR
	   // \verbatim
	   //          TSTERR is LOGICAL
	   //          Flag that indicates whether error exits are to be tested.
	   // \endverbatim
	   
	   // \param[out] A
	   // \verbatim
	   //          A is DOUBLE PRECISION array, dimension (NMAX*4)
	   // \endverbatim
	   
	   // \param[out] AF
	   // \verbatim
	   //          AF is DOUBLE PRECISION array, dimension (NMAX*4)
	   // \endverbatim
	   
	   // \param[out] B
	   // \verbatim
	   //          B is DOUBLE PRECISION array, dimension (NMAX*NSMAX)
	   //         where NSMAX is the largest entry in NSVAL.
	   // \endverbatim
	   
	   // \param[out] X
	   // \verbatim
	   //          X is DOUBLE PRECISION array, dimension (NMAX*NSMAX)
	   // \endverbatim
	 
	   // \param[out] XACT
	   // \verbatim
	   //          XACT is DOUBLE PRECISION array, dimension (NMAX*NSMAX)
	   // \endverbatim
	 
	   // \param[out] WORK
	   // \verbatim
	   //          WORK is DOUBLE PRECISION array, dimension
	   //                      (NMAX*max(3,NSMAX))
	   // \endverbatim
	  
	   // \param[out] RWORK
	   // \verbatim
	   //          RWORK is DOUBLE PRECISION array, dimension
	   //                      (max(NMAX,2*NSMAX))
	   // \endverbatim
	  
	   // \param[out] IWORK
	   // \verbatim
	   //          IWORK is INTEGER array, dimension (2*NMAX)
	   // \endverbatim
	 
	   // \param[in] NOUT
	   // \verbatim
	   //          NOUT is INTEGER
	   //         The unit number for output.
	   // \endverbatim
	  
	   //  Authors:
	   //   ========
	 
	   // \author Univ. of Tennessee 
	   // \author Univ. of California Berkeley 
	   // \author Univ. of Colorado Denver 
	   // \author NAG Ltd. 
	  
	   // \date November 2011
	 
	   // \ingroup double_lin
	   //
	   //  =====================================================================
	  private void dchkgt( boolean dotype[], int nn, int nval[], int nns, int nsval[], double thresh,
	                    double a[], double af[], double b[], double x[], double xact[], double work[], 
	                    double rwork[], int iwork[]) {
	  
	  //  -- LAPACK test routine (version 3.4.0) --
	  //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	  //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	  //     November 2011
	 
	  //     .. Scalar Arguments ..
	  //       LOGICAL            tsterr
	  //       INTEGER            nn, nns, nout
	  //       DOUBLE PRECISION   thresh
	  //     ..
	  //     .. Array Arguments ..
	  //       LOGICAL            dotype( * )
	  //       INTEGER            iwork( * ), nsval( * ), nval( * )
	  //       DOUBLE PRECISION   a( * ), af( * ), b( * ), rwork( * ), work( * ),
	  //      $                   x( * ), xact( * )
	  //     ..
	  
	  //  =====================================================================
	 
	  //     .. Parameters ..
      final double one = 1.0;
      final double zero = 0.0;
	  final int ntypes = 12;
	  final int ntests = 7;
	  
	  //     ..
	  //     .. Local Scalars ..
	  boolean            trfcon, zerot;
	  char type[] = new char[1];
	  char dist[] = new char[1];
	  char          norm, trans;
	  // CHARACTER*3 path
	  String        path = "DGT";
	  int kl[] = new int[1];
	  int ku[] = new int[1];
	  int mode[] = new int[1];
	  int info[] = new int[1];
	  int nerrs;
	  int izero = 0;
	  int            i, imat, in, irhs, itran, ix, j,
                     k, koff, lda, m, n, nfail,
                     nimat, nrhs, nrun;
	  int index;
	  double anorm[] = new double[1];
	  double cond[] = new double[1];
	  double rcond[] = new double[1];
	  double rcondi = 0.0;
	  double rcondo = 0.0;
	  double   ainvnm, rcondc;
	  //     ..
	  //     .. Local Arrays ..
	  char transs[] = new char[]{'N','T','C'};
	  int iseed[] = new int[4 ];
	  int iseedy[] = new int[]{0,0,0,1};
	  double result[ ] = new double[ntests];
	  double z[] = new double[ 3 ];
	  double array[][];
	  double array2[][];
	  double array3[][];
	  double v1[];
	  double v2[];
	  double v3[];
	  double v4[];
	  double v5[];
	  double v6[];
	  double absSum;
	  int iwork2[];
	  double res[] = new double[2];
	  //     ..
	  //     .. External Functions ..
	  //       DOUBLE PRECISION   dasum, dget06, dlangt
	  //       EXTERNAL           dasum, dget06, dlangt
	  //     ..
	  //     .. External Subroutines ..
	  //       EXTERNAL           alaerh, alahd, alasum, dcopy, derrge, dget04,
	  //      $                   dgtcon, dgtrfs, dgtt01, dgtt02, dgtt05, dgttrf,
	  //      $                   dgttrs, dlacpy, dlagtm, dlarnv, dlatb4, dlatms,
	  //      $                   dscal
	  //     ..
	  //     .. Intrinsic Functions ..
	  //       INTRINSIC          max
	  //     ..
	  //     .. Scalars in Common ..
	  //       LOGICAL            lerr, ok
	  //       CHARACTER*32       srnamt
	  //       INTEGER            infot, nunit
	  //     ..
	  //     .. Common blocks ..
	  //       COMMON             / infoc / infot, nunit, ok, lerr
	  //       COMMON             / srnamc / srnamt
	  //     ..
	  // *     .. Executable Statements ..
	 
	         nrun = 0;
	         nfail = 0;
	         nerrs = 0;
	         for (i = 0; i < 4; i++) {
	            iseed[ i ] = iseedy[ i ];
	         }
	
	         for (in = 1; in <= nn; in++) {
	 
	  //        Do for each value of N in NVAL.
	  
	            n = nval[in-1];
	            m = Math.max( n-1, 0 );
	            lda = Math.max( 1, n );
	            nimat = ntypes;
	            if ( n <= 0 ) {
	                nimat = 1;
	            }
	  
	            for  (imat = 1; imat <= nimat; imat++) {
	 
	  //           Do the tests only if DOTYPE( IMAT ) is true.
	  
	               if (!dotype[ imat-1 ] ) {
	                   continue;
	               }
	 
	  //           Set up parameters with DLATB4.
	  
	               ge.dlatb4( path, imat, n, n, type, kl, ku, anorm, mode,
	                    cond, dist );
	 
	               zerot = imat >= 8 && imat <= 10;
	               if ( imat <= 6 ) {
	  
	  //              Types 1-6:  generate matrices of known condition number.
	 
	                  koff = Math.max( 2-ku[0], 3-Math.max( 1, n ) );
	                  array = new double[3][n];
	                  index = 0;
	                  for (j = 0; j < n; j++) {
	                	  for (i = 0; i < 3; i++) {
	                		  array[i][j] = af[koff-1+index];
	                		  index++;
	                	  }
	                  }
	                  ge.dlatms( n, n, dist[0], iseed, type[0], rwork, mode[0], cond[0],
	                  anorm[0], kl[0], ku[0], 'Z', array, 3, work, info );
	                  index = 0;
	                  for (j = 0; j < n; j++) {
	                	  for (i = 0; i < 3; i++) {
	                		  af[koff-1+index] = array[i][j];
	                		  index++;
	                	  }
	                  }
	  
	  //              Check the error code from DLATMS.
	  
	                  if (info[0] != 0 ) {
	                	 if (nfail == 0 && nerrs == 0) {
	                	     printHeader();
	                	 }
	                	 nerrs = nerrs+1;
	                	 UI.setDataText("Error code from dlatms info[0] = " + info[0] + "\n");
	                	 UI.setDataText("N = " + n + "\n");
	                	 UI.setDataText("type = " + imat + "\n");
	                     continue;
	                  } // if (info[0] != 0)
	                  izero = 0;
	 
	                  if ( n > 1 ) {
	                	  for (i = 0; i < n-1; i++) {
	                		  a[i] = af[3 + 3*i];
	                	  }
	                      for (i = 0; i < n-1; i++) {
	                    	  a[n+m+i] = af[2 + 3*i];
	                      }
	                  }
	                  for (i = 0; i < n; i++) {
	                	  a[m+i] = af[1 + 3*i];
	                  }
	               } // if (imat <= 6)
	               else { // imat > 6
	  
	  //              Types 7-12:  generate tridiagonal matrices with
	  //              unknown condition numbers.
	  
	                  if (!zerot || !dotype[6] ) {
	  
	  //                 Generate a matrix with elements from [-1,1].
	  
	                     ge.dlarnv( 2, iseed, n+2*m, a );
	                     if (anorm[0] != one ) {
	                    	 for (i = 0; i < n+2*m; i++) {
	                    		 a[i] = anorm[0] * a[i];
	                    	 }
	                     } // IF (anorm[0] != one)
	                  } // if (!zerot || !dotype[6] )
	                  else if ( izero > 0 ) {
	  
	  //                 Reuse the last matrix by copying back the zeroed out
	  //                 elements.
	  
	                     if ( izero == 1 ) {
	                        a[ n-1 ] = z[ 1 ];
	                        if ( n > 1 ) {
	                           a[ 0 ] = z[ 2 ];
	                        }
	                     } // if (izero == 1)
	                     else if ( izero == n ) {
	                        a[ 3*n-3 ] = z[ 0 ];
	                        a[ 2*n-2 ] = z[ 1 ];
	                     } // else if (izero == n)
	                     else {
	                        a[ 2*n-3+izero ] = z[ 0 ];
	                        a[ n-2+izero ] = z[ 1 ];
	                        a[ izero-1 ] = z[ 2 ];
	                     } // else
	                  }  // else if (izero > 0)    
	  
	  //              If IMAT > 7, set one column of the matrix to 0.
	 
	                  if ( !zerot ) {
	                     izero = 0;
	                  }
	                  else if ( imat == 8 ) {
	                     izero = 1;
	                     z[ 1 ] = a[ n-1 ];
	                     a[ n-1 ] = zero;
	                     if ( n > 1 ) {
	                        z[ 2 ] = a[ 0 ];
	                        a[ 0 ] = zero;
	                     } // if (n > 1)
	                  } // else if (imat == 8)
	                  else if ( imat == 9 ) {
	                     izero = n;
	                     z[ 0 ] = a[ 3*n-3 ];
	                     z[ 1 ] = a[ 2*n-2 ];
	                     a[ 3*n-3 ] = zero;
	                     a[ 2*n-2 ] = zero;
	                  } // else if (imat == 9)
	                  else {
	                     izero = ( n+1 ) / 2;
	                     for (i = izero; i <= n - 1; i++) {
	                        a[ 2*n-3+i ] = zero;
	                        a[ n-2+i ] = zero;
	                        a[ i-1 ] = zero;
	                     } // for (i = izero; i <= n - 1; i++)
	                     a[ 3*n-3 ] = zero;
	                     a[ 2*n-2 ] = zero;
	                  } // else 
	               } // else imat > 6
	  
	  //    TEST 1
	  //           Factor A as L*U and compute the ratio
	  //              norm(L*U - A) / (n * norm(A) * EPS )
	  
	               for (i = 0; i < n+2*m; i++) {
	            	   af[i] = a[i];
	               }
	               v1 = new double[n];
	               for (i = 0; i < n; i++) {
	            	   v1[i] = af[m+i];
	               }
	               v2 = new double[Math.max(0,n-1)];
	               for (i = 0; i < n-1; i++) {
	            	   v2[i] = af[n+m+i];
	               }
	               v3 = new double[Math.max(0,n-2)];
	               dgttrf( n, af, v1, v2, v3,
	                      iwork, info );
	               for (i = 0; i < n; i++) {
	            	   af[m+i] = v1[i];
	               }
	               for (i = 0; i < n-1; i++) {
	            	   af[n+m+i] = v2[i];
	               }
	               for (i = 0; i < n-2; i++) {
	            	   af[n+2*m+i] = v3[i];
	               }
	 
	  //           Check error code from DGTTRF.
	  
	               if ( info[0] != izero ) {
	            	     if (nfail == 0 && nerrs == 0) {
	            	         printHeader();
	            	     }
	            	     nerrs = nerrs + 1;
	            	     if (info[0] != izero && izero != 0) {
	            	    	 UI.setDataText("dgttrf returned with info[0] = " + info[0] + " instead of " + izero + "\n");
	            	    	 UI.setDataText("N = " + n + "\n");
	            	    	 UI.setDataText("type = " + imat + "\n");
	            	     }
	            	     else {
	            	    	 UI.setDataText("dgttrf returned with info[0] = " + info[0] + "\n");
	            	    	 UI.setDataText("N = " + n + "\n");
	            	    	 UI.setDataText("type = " + imat + "\n");	 
	            	     }
	               } // if (info[0] != izero)
	               trfcon = info[0] != 0;
	  
	               v1 = new double[n];
	               for (i = 0; i < n; i++) {
	            	   v1[i] = a[m+i];   
	               }
	               v2 = new double[Math.max(0,n-1)];
	               for (i = 0; i < n-1; i++) {
	            	   v2[i] = a[n+m+i];
	               }
	               v3 = new double[n];
	               for (i = 0; i < n; i++) {
	            	   v3[i] = af[m+i];
	               }
	               v4 = new double[Math.max(0,n-1)];
	               for (i = 0; i < n-1; i++) {
	            	   v4[i] = af[n+m+i];
	               }
	               v5 = new double[Math.max(0,n-2)];
	               for (i = 0; i < n-2; i++) {
	            	   v5[i] = af[n+2*m+i];
	               }
	               array = new double[n][n];
	               dgtt01( n, a, v1, v2, af, v3,
	                       v4, v5, iwork, array, lda,
	                       rwork, result );
	               index = 0;
	               for (j = 0; j < n; j++) {
	            	   for (i = 0; i < n; i++) {
	            		   work[index++] = array[i][j];
	            	   }
	               }
	  
	  //           Print the test ratio if it is .GE. THRESH.
	 
	               if ( result[0] >= thresh ) {
	                  if ( nfail == 0 && nerrs == 0 ) {
	                      printHeader();
	                  }
	                  UI.setDataText("N = " + n + "\n");
	                  UI.setDataText("type = " + imat + "\n");
	                  UI.setDataText("Test 1 gives result[0] = " + nf.format(result[0]) + "\n");
	                  nfail = nfail + 1;
	               } // if (result[0] >= thresh)
	               nrun = nrun + 1;
	  
	               for ( itran = 1; itran <= 2; itran++) {
	                  trans = transs[ itran-1];
	                  if ( itran == 1 ) {
	                     norm = 'O';
	                  }
	                  else {
	                     norm = 'I';
	                  }
	                  v1 = new double[n];
	                  for (i = 0; i < n; i++) {
	                	  v1[i] = a[m+i];
	                  }
	                  v2 = new double[Math.max(0,n-1)];
	                  for (i = 0; i < n-1; i++) {
	                	  v2[i] = a[n+m+i];
	                  }
	                  anorm[0] = dlangt( norm, n, a, v1, v2 );
	  
	                  if (!trfcon ) {
	  
	  //                 Use DGTTRS to solve for one column at a time of inv(A)
	  //                 or inv(A^T), computing the maximum column sum as we
	  //                 go.
	  
	                     ainvnm = zero;
	                     v3 = new double[Math.max(0,n-2)];
	                    for (i = 0; i < n; i++) {
	                        for (j = 0; j < n; j++) {
	                           x[ j ] = zero;
	                        } // for (j = 0; j < n; j++)
	                        x[ i ] = one;
	                        for (k = 0; k < n; k++) {
	                        	v1[k] = af[m+k];
	                        }
	                        for (k = 0; k < n-1; k++) {
	                        	v2[k] = af[n+m+k];
	                        }
	                        for (k = 0; k < n-2; k++) {
	                        	v3[k] = af[n+2*m+k];
	                        } 
	                        array = new double[n][1];
	                        for (j = 0; j < n; j++) {
	                        	array[j][0] = x[j];
	                        }
	                        dgttrs( trans, n, 1, af, v1,
	                                v2, v3, iwork, array,
	                                lda, info );
	                        for (j = 0; j < n; j++) {
	                        	x[j] = array[j][0];
	                        }
	                        absSum = 0.0;
	                        for (k = 0; k < n; k++) {
	                        	absSum += Math.abs(x[k]);
	                        }
	                        ainvnm = Math.max( ainvnm, absSum);
	                    } // for (i = 0; i < n; i++)
	  
	  //                 Compute RCONDC = 1 / (norm(A) * norm(inv(A))
	  
	                     if ( anorm[0] <= zero || ainvnm <= zero ) {
	                        rcondc = one;
	                     }
	                     else {
	                        rcondc = ( one / anorm[0] ) / ainvnm;
	                     }
	                     if ( itran == 1 ) {
	                        rcondo = rcondc;
	                     }
	                     else {
	                        rcondi = rcondc;
	                     }
	                  } // if (!trfcon)
	                  else {
	                     rcondc = zero;
	                  }
	  
	  //    TEST 7
	  //              Estimate the reciprocal of the condition number of the
	  //              matrix.
	  
	                  v1 = new double[n];
	                  for (i = 0; i < n; i++) {
	                	  v1[i] = af[m+i];
	                  }
	                  v2 = new double[Math.max(0,n-1)];
	                  for (i = 0; i < n-1; i++) {
	                	  v2[i] = af[n+m+i];
	                  }
	                  v3 = new double[Math.max(0,n-1)];
	                  for (i = 0; i < n-2; i++) {
	                	  v3[i] = af[n+2*m+i];
	                  }
	                  iwork2 = new int[n];
	                  dgtcon( norm, n, af, v1, v2,
	                          v3, iwork, anorm[0], rcond, work,
	                          iwork2, info );
	  
	  //              Check error code from DGTCON.
	  
	                  if (info[0] != 0 ) {
	                	  if (nfail == 0 && nerrs == 0) {
		                	   printHeader();
		                  }
		                  nerrs = nerrs+1;
		                  UI.setDataText("dgtcon had info[0] = " + info[0] + "\n");
		                  UI.setDataText("NORM = " + norm + "\n");
		                  UI.setDataText("N = " + m + "\n");
		                  UI.setDataText("type = " + imat + "\n");
	                  } // if (info[0] != 0)
	  
	                  result[ 6 ] = le.dget06( rcond[0], rcondc );
	  
	  //              Print the test ratio if it is .GE. THRESH.
	  
	                  if ( result[ 6 ] >= thresh ) {
	                     if ( nfail == 0 && nerrs == 0 ) {
	                    	 printHeader();
	                     }
	                     UI.setDataText("NORM = " + norm + "\n");
	                     UI.setDataText("N = " + n + "\n");
	                     UI.setDataText("type = " + imat + "\n");
	                     UI.setDataText("Test 7 has result[6] = " + nf.format(result[6]) + "\n");
	                     nfail = nfail + 1;
	                  } // if ( result[ 6 ] >= thresh )
	                  nrun = nrun + 1;
	               } // for ( itran = 1; itran <= 2; itran++)
	  
	  //           Skip the remaining tests if the matrix is singular.
	  
	               if ( trfcon ) {
	                  continue;
	               }
	  
	               for (irhs = 1; irhs <= nns; irhs++) {
	                  nrhs = nsval[ irhs-1 ];
	  
	  //              Generate NRHS random solution vectors.
	  
	                  ix = 0;
	                  v1 = new double[n];
	                  for (j = 1; j <= nrhs; j++) {
	                     ge.dlarnv( 2, iseed, n, v1);
	                     for (i = 0; i < n; i++) {
	                        xact[ix + i] = v1[i];
	                     }
	                     ix = ix + lda;
	                  } // for (j = 1; j <= nrhs; j++)
	  
	                  for (itran = 1; itran <= 3; itran++) {
	                     trans = transs[ itran-1 ];
	                     if ( itran == 1 ) {
	                        rcondc = rcondo;
	                     }
	                     else {
	                        rcondc = rcondi;
	                     }
	  
	  //                 Set the right hand side.
	  
	                     v1 = new double[n];
	                     for (i = 0; i < n; i++) {
	                    	 v1[i] = a[m+i];
	                     }
	                     v2 = new double[Math.max(0,n-1)];
	                     for (i = 0; i < n-1; i++) {
	                    	 v2[i] = a[n+m+i];
	                     }
	                     array = new double[n][nrhs];
	                     array2 = new double[n][nrhs];
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 array2[i][j] = xact[index++];
	                    	 }
	                     }
	                     dlagtm( trans, n, nrhs, one, a, v1,
	                             v2, array2, lda, zero, array, lda );
	                     index = 0;
		                 for (j = 0; j < nrhs; j++) {
		                    for (i = 0; i < n; i++) {
		                    	b[index++] = array[i][j];
		                    }
		                 }
	  
	  //    TEST 2
	  //                 Solve op(A) * X = B and compute the residual.
		                 array2 = new double[n][ nrhs]; 
	                     ge.dlacpy( 'F', n, nrhs, array, lda, array2, lda );
	                     index = 0;
		                    for (j = 0; j < nrhs; j++) {
		                    	for (i = 0; i < n; i++) {
		                    	    x[index++] = array2[i][j];	
		                    	}
		                    }
	                     v1 = new double[n];
	                     for (i = 0; i < n; i++) {
	                    	 v1[i] = af[m+i];
	                     }
	                     v2 = new double[Math.max(0,n-1)];
	                     for (i = 0; i < n-1; i++) {
	                    	 v2[i] = af[n+m+i];
	                     }
	                     v3 = new double[Math.max(0,n-2)];
	                     for (i = 0; i < n-2; i++) {
	                    	 v3[i] = af[n+2*m+i];
	                     }
	                     array = new double[n][nrhs];
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                         for (i = 0; i < n; i++) {
	                        	 array[i][j] = x[index++];
	                         }
	                     }
	                     dgttrs( trans, n, nrhs, af, v1,
	                             v2, v3, iwork, array,
	                             lda, info );
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                         for (i = 0; i < n; i++) {
	                        	 x[index++] = array[i][j];
	                         }
	                     }
	  
	  //                 Check error code from DGTTRS.
	  
	                    if ( info[0] != 0 ) {
	                    	if (nfail == 0 && nerrs == 0) {
		            	         printHeader();
		            	     }
		            	     nerrs = nerrs + 1;
		            	     UI.setDataText("dgttrs returned with info[0] = " + info[0] + "\n");
	            	    	 UI.setDataText("trans = " + trans + "\n");
	            	    	 UI.setDataText("n = " + n + "\n");
	            	    	 UI.setDataText("nrhs = " + nrhs + "\n");
	            	    	 UI.setDataText("type = " + imat + "\n");	 
	                    } // if (info[0] != 0)
	  
	                    array = new double[n][nrhs];
	                    index = 0;
	                    for (j = 0; j < nrhs; j++) {
	                    	for (i = 0; i < n; i++) {
	                    		array[i][j] = b[index++];
	                    	}
	                    }
	                    array2 = new double[n][ nrhs]; 
	                    ge.dlacpy( 'F', n, nrhs, array, lda, array2, lda );
	                    
	                   v1 = new double[n];
	                   for (i = 0; i < n; i++) {
	                	   v1[i] = a[m+i];
	                   }
	                   v2 = new double[Math.max(0,n-1)];
	                   for (i = 0; i < n-1; i++) {
	                	   v2[i] = a[n+m+i];
	                   }
	                   array = new double[n][nrhs];
	                   index = 0;
	                   for (j = 0; j < nrhs; j++) {
	                    	for (i = 0; i < n; i++) {
	                    		array[i][j] = x[index++];
	                    	}
	                    }
	                   dgtt02( trans, n, nrhs, a, v1, v2,
	                            array, lda, array2, lda, rwork, res);
	                   index = 0;
	                    for (j = 0; j < nrhs; j++) {
	                    	for (i = 0; i < n; i++) {
	                    	    work[index++] = array2[i][j];	
	                    	}
	                    }
	                   result[1] = res[0];
	  
	  //    TEST 3
	  //                 Check solution from generated exact solution.
	                   index = 0;
	                    for (j = 0; j < nrhs; j++) {
	                    	for (i = 0; i < n; i++) {
	                    		array2[i][j] = xact[index++];
	                    	}
	                     }
	                     le.dget04( n, nrhs, array, lda, array2, lda, rcondc, res);
	                     result[2] = res[0];
	  //    TESTS 4, 5, and 6
	  //                 Use iterative refinement to improve the solution.
	  //
	                     for (i = 0; i < n; i++) {
	                    	 v1[i] = a[m+i];
	                     }
	                     for (i = 0; i < n-1; i++) {
	                    	 v2[i] = a[n+m+i];
	                     }
	                     v3 = new double[n];
	                     for (i = 0; i < n; i++) {
	                    	 v3[i] = af[m+i];
	                     }
	                     v4 = new double[Math.max(0,n-1)];
	                     for (i = 0; i < n-1; i++) {
	                    	 v4[i] = af[n+m+i];
	                     }
	                     v5 = new double[Math.max(0,n-2)];
	                     for (i = 0; i < n-2; i++) {
	                    	 v5[i] = af[n+2*m+i];
	                     }
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 array[i][j] = b[index++];
	                    	 }
	                     }
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 array2[i][j] = x[index++];
	                    	 }
	                     }
	                     v6 = new double[nrhs];
	                     iwork2 = new int[n];
	                     dgtrfs( trans, n, nrhs, a, v1, v2,
	                             af, v3, v4,
	                             v5, iwork, array, lda, array2, lda,
	                             rwork, v6, work,
	                             iwork2, info );
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 x[index++] = array2[i][j];
	                    	 }
	                     }
	                     for (i = 0; i < nrhs; i++) {
	                    	 rwork[nrhs + i] = v6[i];
	                     }
	                     for (i = 0; i < n; i++) {
	                    	 iwork[n+i] = iwork2[i];
	                     }
	
	  //                 Check error code from DGTRFS.
	 
	                     if ( info[0] != 0 ) {
	                    	 if (nfail == 0 && nerrs == 0) {
		            	         printHeader();
		            	     }
		            	     nerrs = nerrs + 1;
		            	     UI.setDataText("dgtrfs returned with info[0] = " + info[0] + "\n");
	            	    	 UI.setDataText("trans = " + trans + "\n");
	            	    	 UI.setDataText("n = " + n + "\n");
	            	    	 UI.setDataText("nrhs = " + nrhs + "\n");
	            	    	 UI.setDataText("type = " + imat + "\n");	 
	                     } // if ( info[0] != 0 ) 
	  
	                     
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 array[i][j] = xact[index++];
	                    	 }
	                     }
	                     le.dget04( n, nrhs, array2, lda, array, lda, rcondc,
	                             res);
	                     result[3] = res[0];
	                     v1 = new double[n];
	                     for (i = 0; i < n; i++) {
	                    	 v1[i] = a[m+i];
	                     }
	                     v2 = new double[Math.max(0,n-1)];
	                     for (i = 0; i < n-1; i++) {
	                    	 v2[i] = a[n+m+i];
	                     }
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 array[i][j] = b[index++];
	                    	 }
	                     }
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 array2[i][j] = x[index++];
	                    	 }
	                     }
	                     array3 = new double[n][nrhs];
	                     index = 0;
	                     for (j = 0; j < nrhs; j++) {
	                    	 for (i = 0; i < n; i++) {
	                    		 array3[i][j] = xact[index++];
	                    	 }
	                     }
	                     v3 = new double[nrhs];
	                     for (i = 0; i < nrhs; i++) {
	                    	 v3[i] = rwork[nrhs+i];
	                     }
	                     dgtt05( trans, n, nrhs, a, v1, v2,
	                             array, lda, array2, lda, array3, lda, rwork,
	                             v3, res );
	                     result[4] = res[0];
	                     result[5] = res[1];
	 
	  //                 Print information about the tests that did not pass
	  //                 the threshold.
	  
	                     for (k = 2; k <= 6; k++) {
	                        if ( result[ k-1] >= thresh ) {
	                           if ( nfail == 0 && nerrs == 0 ) {
	                        	   printHeader();
	                           }
	                           UI.setDataText("trans = " + trans + "\n");
	                           UI.setDataText("n = " + n + "\n");
	                           UI.setDataText("nrhs = " + nrhs + "\n");
	                           UI.setDataText("type = " + imat + "\n");
	                           UI.setDataText("Test " + k + " has result["+(k-1)+"] = " + nf.format(result[k-1]) + "\n");
	                           nfail = nfail + 1;
	                        } // if ( result[ k-1] >= thresh )
	                     } // for (k = 2; k <= 6; k++)
	                     nrun = nrun + 5;
	                  } // for (itran = 1; itran <= 3; itran++)
	               } // for (irhs = 1; irhs <= nns; irhs++)
	 
	           } // for  (imat = 1; imat <= nimat; imat++)
	         } // for (in = 1; in <= nn; in++)
	  
	  //     Print a summary of the results.
	         if (nfail > 0) {
	             UI.setDataText("In dchkgt " + nfail + " out of " + nrun + " tests failed to pass the threshold\n");
	         } else {
	             UI.setDataText("In dchkgt all " + nrun + " tests run passed the threshold\n");
	         }
	         if (nerrs > 0) {
	             UI.setDataText("In dchkgt " + nerrs + " errors occurred\n");
	         }
	         return;
	  } // dchkgt
	  
	   // \par Purpose:
	   //  =============
	   
	   // \verbatim
	   
	   // DGTT05 tests the error bounds from iterative refinement for the
	   // computed solution to a system of equations A*X = B, where A is a
	   // general tridiagonal matrix of order n and op(A) = A or A**T,
	   // depending on TRANS.
	   
	   // RESLTS(1) = test of the error bound
	   //           = norm(X - XACT) / ( norm(X) * FERR )
	   
	   // A large value is returned if this ratio is not less than one.
	   
	   // RESLTS(2) = residual from the iterative refinement routine
	   //           = the maximum of BERR / ( NZ*EPS + (*) ), where
	   //             (*) = NZ*UNFL / (min_i (abs(op(A))*abs(X) +abs(b))_i )
	   //             and NZ = max. number of nonzeros in any row of A, plus 1
	   // \endverbatim
	   
	   //  Arguments:
	   //  ==========
	   
	   // \param[in] TRANS
	   // \verbatim
	   //          TRANS is CHARACTER*1
	   //          Specifies the form of the system of equations.
	   //          = 'N':  A * X = B     (No transpose)
	   //          = 'T':  A**T * X = B  (Transpose)
	   //          = 'C':  A**H * X = B  (Conjugate transpose = Transpose)
	   // \endverbatim
	   
	   // \param[in] N
	   // \verbatim
	   //          N is INTEGER
	   //          The number of rows of the matrices X and XACT.  N >= 0.
	   // \endverbatim
	   
	   // \param[in] NRHS
	   // \verbatim
	   //          NRHS is INTEGER
	   //          The number of columns of the matrices X and XACT.  NRHS >= 0.
	   // \endverbatim
	   
	   // \param[in] DL
	   // \verbatim
	   //          DL is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) sub-diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] D
	   // \verbatim
	   //          D is DOUBLE PRECISION array, dimension (N)
	   //          The diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] DU
	   // \verbatim
	   //          DU is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) super-diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] B
	   // \verbatim
	   //          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
	   //          The right hand side vectors for the system of linear
	   //          equations.
	   // \endverbatim
	   
	   // \param[in] LDB
	   // \verbatim
	   //          LDB is INTEGER
	   //          The leading dimension of the array B.  LDB >= max(1,N).
	   // \endverbatim
	  
	  // \param[in] X
	  // \verbatim
	  //          X is DOUBLE PRECISION array, dimension (LDX,NRHS)
	  //          The computed solution vectors.  Each vector is stored as a
	  //          column of the matrix X.
	  // \endverbatim
	  
	  // \param[in] LDX
	  // \verbatim
	  //          LDX is INTEGER
	  //          The leading dimension of the array X.  LDX >= max(1,N).
	  // \endverbatim
	  
	  // \param[in] XACT
	  // \verbatim
	  //          XACT is DOUBLE PRECISION array, dimension (LDX,NRHS)
	  //          The exact solution vectors.  Each vector is stored as a
	  //          column of the matrix XACT.
	  // \endverbatim
	  
	  // \param[in] LDXACT
	  // \verbatim
	  //          LDXACT is INTEGER
	  //          The leading dimension of the array XACT.  LDXACT >= max(1,N).
	  // \endverbatim
	  
	  // \param[in] FERR
	  // \verbatim
	  //          FERR is DOUBLE PRECISION array, dimension (NRHS)
	  //          The estimated forward error bounds for each solution vector
	  //          X.  If XTRUE is the true solution, FERR bounds the magnitude
	  //          of the largest entry in (X - XTRUE) divided by the magnitude
	  //          of the largest entry in X.
	  // \endverbatim
	  
	  // \param[in] BERR
	  // \verbatim
	  //          BERR is DOUBLE PRECISION array, dimension (NRHS)
	  //          The componentwise relative backward error of each solution
	  //          vector (i.e., the smallest relative change in any entry of A
	  //          or B that makes X an exact solution).
	  // \endverbatim
	  
	  // \param[out] RESLTS
	  // \verbatim
	  //          RESLTS is DOUBLE PRECISION array, dimension (2)
	  //          The maximum over the NRHS solution vectors of the ratios:
	  //          RESLTS(1) = norm(X - XACT) / ( norm(X) * FERR )
	  //          RESLTS(2) = BERR / ( NZ*EPS + (*) )
	  // \endverbatim
	  
	  //  Authors:
	  //  ========
	  
	  // \author Univ. of Tennessee 
	  // \author Univ. of California Berkeley 
	  // \author Univ. of Colorado Denver 
	  // \author NAG Ltd. 
	  
	  // \date November 2011
	  
	  // \ingroup double_lin
	  
	  //  =====================================================================
	         private void dgtt05(char trans, int n, int nrhs, double dl[], double d[], double du[],
	        		             double b[][], int ldb, double x[][], int ldx,
	                             double xact[][], int ldxact, double ferr[], double berr[], double reslts[] ) {
	  
	  //  -- LAPACK test routine (version 3.4.0) --
	  //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	  //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	  //     November 2011
	  
	  //     .. Scalar Arguments ..
	  //       CHARACTER          trans
	  //       INTEGER            ldb, ldx, ldxact, n, nrhs
	  //     ..
	  //     .. Array Arguments ..
	  //       DOUBLE PRECISION   b( ldb, * ), berr( * ), d( * ), dl( * ),
	  //      $                   du( * ), ferr( * ), reslts( * ), x( ldx, * ),
	  //      $                   xact( ldxact, * )
	  //     ..
	  
	  //  =====================================================================
	  
	  //     .. Parameters ..
	         final double zero = 0.0;
	         final double one = 1.0;
	  //     ..
	  //     .. Local Scalars ..
	         boolean            notran;
	         int            i, imax, j, k, nz;
	         double   axbi, diff, eps, errbnd, ovfl, tmp, unfl, xnorm;
	         double maxVal;
	  //     ..
	  //     .. External Functions ..
	  //       LOGICAL            lsame
	  //       INTEGER            idamax
	  //       DOUBLE PRECISION   dlamch
	  //       EXTERNAL           lsame, idamax, dlamch
	  //     ..
	  //     .. Intrinsic Functions ..
	  //       INTRINSIC          abs, max, min
	  //     ..
	  //     .. Executable Statements ..
	  
	  //     Quick exit if N = 0 or NRHS = 0.
	  
	         if ( n <= 0 || nrhs <= 0 ) {
	            reslts[ 0 ] = zero;
	            reslts[ 1 ] = zero;
	            return;
	         }
	  
	         eps = ge.dlamch( 'E' );
	         unfl = ge.dlamch( 'S' );
	         ovfl = one / unfl;
	         notran = (( trans == 'N' ) || (trans == 'n'));
	         nz = 4;
	  
	  //     Test 1:  Compute the maximum of
	  //        norm(X - XACT) / ( norm(X) * FERR )
	  //     over all the vectors X and XACT using the infinity-norm.
	  
	         errbnd = zero;
	         for (j = 0; j < nrhs; j++) {
	        	   imax = -1;
	        	   maxVal = -Double.MAX_VALUE;
	        	   for (i = 0; i < n; i++) {
	        		   if (Math.abs(x[i][j]) > maxVal) {
	        			   maxVal = Math.abs(x[i][j]);
	        			   imax = i;
	        		   }
	        	   }
	            xnorm = Math.max( Math.abs( x[ imax][ j ] ), unfl );
	            diff = zero;
	            for (i = 0; i < n; i++) {
	               diff = Math.max( diff, Math.abs( x[ i][ j ]-xact[ i][ j ] ) );
	            }
	  
	            if ( xnorm > one ) {
	            }
	            else if ( diff <= ovfl*xnorm ) {
	            }
	            else {
	               errbnd = one / eps;
	               continue;
	            }
	  
	            if ( diff / xnorm <= ferr[ j ] ) {
	               errbnd = Math.max( errbnd, ( diff / xnorm ) / ferr[ j ] );
	            }
	            else {
	               errbnd = one / eps;
	            }
	         } // for (j = 0; j < nrhs; j++)
	         reslts[ 0 ] = errbnd;
	  
	  //     Test 2:  Compute the maximum of BERR / ( NZ*EPS + (*) ), where
	  //     (*) = NZ*UNFL / (min_i (abs(op(A))*abs(X) +abs(b))_i )
	  
	         for (k = 0; k < nrhs; k++) {
	            if ( notran ) {
	               if ( n == 1 ) {
	                  axbi = Math.abs( b[ 0][ k ] ) + Math.abs( d[ 0 ]*x[ 0][ k ] );
	               } // if (n == 1)
	               else { // n != 1
	                  axbi = Math.abs( b[ 0][ k ] ) + Math.abs( d[ 0 ]*x[ 0][ k ] ) +
	                         Math.abs( du[ 0 ]*x[1][ k ] );
	                  for (i = 1; i < n - 1; i++) {
	                     tmp = Math.abs( b[ i][ k ] ) + Math.abs( dl[ i-1 ]*x[ i-1][ k ] )
	                           + Math.abs( d[ i ]*x[ i][ k ] ) +
	                           Math.abs( du[ i ]*x[ i+1][ k ] );
	                     axbi = Math.min( axbi, tmp );
	                  } // for (i = 1; i < n - 1; i++)
	                  tmp = Math.abs( b[ n-1][ k ] ) + Math.abs( dl[ n-2 ]*x[ n-2][ k ] ) +
	                        Math.abs( d[ n-1 ]*x[ n-1][ k ] );
	                  axbi = Math.min( axbi, tmp );
	               } // else n != 1
	            } // if (notran)
	            else { // !notran
	               if ( n == 1 ) {
	                  axbi = Math.abs( b[ 0][ k ] ) + Math.abs( d[ 0 ]*x[ 0][ k ] );
	               }
	               else { // n != 1
	                  axbi = Math.abs( b[0][ k ] ) + Math.abs( d[ 0 ]*x[ 0][ k ] ) +
	                         Math.abs( dl[ 0 ]*x[ 1][ k ] );
	                  for (i = 1; i < n - 1; i++) {
	                     tmp = Math.abs( b[ i][ k ] ) + Math.abs( du[ i-1 ]*x[ i-1][ k ] )
	                            + Math.abs( d[ i ]*x[ i][ k ] ) +
	                            Math.abs( dl[ i ]*x[ i+1][ k ] );
	                     axbi = Math.min( axbi, tmp );
	                  } // for (i = 1; i < n - 1; i++)
	                  tmp = Math.abs( b[ n-1][ k ] ) + Math.abs( du[ n-2 ]*x[ n-2][ k ] ) +
	                        Math.abs( d[ n -1]*x[ n-1][ k ] );
	                  axbi = Math.min( axbi, tmp );
	               } // else n != 1
	            } // else !notran
	            tmp = berr[ k ] / ( nz*eps+nz*unfl / Math.max( axbi, nz*unfl ) );
	            if ( k == 0) {
	               reslts[ 1 ] = tmp;
	            }
	            else {
	               reslts[ 1 ] = Math.max( reslts[ 1 ], tmp );
	            }
	         } // for (k = 0; k < nrhs; k++)
	  
	         return;
	      } // dgtt05
	  
	   // \par Purpose:
	   // =============
	   
	   // \verbatim
	   
	   // DGTRFS improves the computed solution to a system of linear
	   //  equations when the coefficient matrix is tridiagonal, and provides
	   // error bounds and backward error estimates for the solution.
	   // \endverbatim
	  
	   //  Arguments:
	   //  ==========
	   
	   // \param[in] TRANS
	   // \verbatim
	   //          TRANS is CHARACTER*1
	   //          Specifies the form of the system of equations:
	   //          = 'N':  A * X = B     (No transpose)
	   //          = 'T':  A**T * X = B  (Transpose)
	   //          = 'C':  A**H * X = B  (Conjugate transpose = Transpose)
	   // \endverbatim
	   
	   // \param[in] N
	   // \verbatim
	   //          N is INTEGER
	   //          The order of the matrix A.  N >= 0.
	   // \endverbatim
	  
	   // \param[in] NRHS
	   // \verbatim
	   //          NRHS is INTEGER
	   //          The number of right hand sides, i.e., the number of columns
	   //          of the matrix B.  NRHS >= 0.
	   // \endverbatim
	   
	   // \param[in] DL
	   // \verbatim
	   //          DL is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) subdiagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] D
	   // \verbatim
	   //          D is DOUBLE PRECISION array, dimension (N)
	   //          The diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] DU
	   // \verbatim
	   //          DU is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) superdiagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] DLF
	   // \verbatim
	   //          DLF is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) multipliers that define the matrix L from the
	   //          LU factorization of A as computed by DGTTRF.
	   // \endverbatim
	  
	   // \param[in] DF
	   // \verbatim
	  //          DF is DOUBLE PRECISION array, dimension (N)
	  //          The n diagonal elements of the upper triangular matrix U from
	  //          the LU factorization of A.
	  // \endverbatim
	  
	  // \param[in] DUF
	  // \verbatim
	  //          DUF is DOUBLE PRECISION array, dimension (N-1)
	  //          The (n-1) elements of the first superdiagonal of U.
	  // \endverbatim
	 
	  // \param[in] DU2
	  // \verbatim
	  //          DU2 is DOUBLE PRECISION array, dimension (N-2)
	  //          The (n-2) elements of the second superdiagonal of U.
	  // \endverbatim
	  
	  // \param[in] IPIV
	  // \verbatim
	  //          IPIV is INTEGER array, dimension (N)
	  //         The pivot indices; for 1 <= i <= n, row i of the matrix was
	  //          interchanged with row IPIV(i).  IPIV(i) will always be either
	  //          i or i+1; IPIV(i) = i indicates a row interchange was not
	  //          required.
	  // \endverbatim
	  
	  // \param[in] B
	  // \verbatim
	  //          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
	  //          The right hand side matrix B.
	  // \endverbatim
	  
	  // \param[in] LDB
	  // \verbatim
	  //          LDB is INTEGER
	  //          The leading dimension of the array B.  LDB >= max(1,N).
	  // \endverbatim
	  
	  // \param[in,out] X
	  // \verbatim
	  //          X is DOUBLE PRECISION array, dimension (LDX,NRHS)
	  //          On entry, the solution matrix X, as computed by DGTTRS.
	  //          On exit, the improved solution matrix X.
	  // \endverbatim
	  
	  // \param[in] LDX
	  // \verbatim
	  //          LDX is INTEGER
	  //          The leading dimension of the array X.  LDX >= max(1,N).
	  // \endverbatim
	  
	  // \param[out] FERR
	  // \verbatim
	  //          FERR is DOUBLE PRECISION array, dimension (NRHS)
	  //          The estimated forward error bound for each solution vector
	  //          X(j) (the j-th column of the solution matrix X).
	  //          If XTRUE is the true solution corresponding to X(j), FERR(j)
	  //          is an estimated upper bound for the magnitude of the largest
	  //          element in (X(j) - XTRUE) divided by the magnitude of the
	  //          largest element in X(j).  The estimate is as reliable as
	  //          the estimate for RCOND, and is almost always a slight
	  //          overestimate of the true error.
	  // \endverbatim
	  
	  // \param[out] BERR
	  // \verbatim
	  //          BERR is DOUBLE PRECISION array, dimension (NRHS)
	  //          The componentwise relative backward error of each solution
	  //          vector X(j) (i.e., the smallest relative change in
	  //          any element of A or B that makes X(j) an exact solution).
	  // \endverbatim
	  
	  // \param[out] WORK
	  // \verbatim
	  //          WORK is DOUBLE PRECISION array, dimension (3*N)
	  // \endverbatim
	  
	  // \param[out] IWORK
	  // \verbatim
	  //          IWORK is INTEGER array, dimension (N)
	  // \endverbatim
	  
	  // \param[out] INFO
	  // \verbatim
	  //          INFO is INTEGER
	  //          = 0:  successful exit
	  //          < 0:  if INFO = -i, the i-th argument had an illegal value
	  // \endverbatim
	  
	  // \par Internal Parameters:
	  //  =========================
	  
	  // \verbatim
	  //  ITMAX is the maximum number of steps of iterative refinement.
	  // \endverbatim
	  
	  //  Authors:
	  //  ========
	  
	  // \author Univ. of Tennessee 
	  // \author Univ. of California Berkeley 
	  // \author Univ. of Colorado Denver 
	  // \author NAG Ltd. 
	  
	  // \date September 2012
	  
	  // \ingroup doubleGTcomputational
	 
	  //  =====================================================================
	         private void dgtrfs(char trans, int n, int nrhs, double dl[], double d[], double du[],
	        		             double dlf[], double df[], double duf[], double du2[],
	                             int ipiv[], double b[][], int ldb, double x[][], int ldx,
	                             double ferr[], double berr[], double work[], int iwork[],
	                             int info[] ) {
	  
	  //  -- LAPACK computational routine (version 3.4.2) --
	  //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	  //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	  //     September 2012
	  
	  //     .. Scalar Arguments ..
	  //        char          trans
	  //       INTEGER            info, ldb, ldx, n, nrhs
	  //     ..
	  //     .. Array Arguments ..
	  //       INTEGER            ipiv( * ), iwork( * )
	  //       DOUBLE PRECISION   b( ldb, * ), berr( * ), d( * ), df( * ),
	  //      $                   dl( * ), dlf( * ), du( * ), du2( * ), duf( * ),
	  //      $                   ferr( * ), work( * ), x( ldx, * )
	  //     ..
	  
	  //  =====================================================================
	  
	  //     .. Parameters ..
	         final int itmax = 5;
	         final double zero = 0.0;
	         final double one = 1.0;
	         final double two = 2.0;
	         final double three = 3.0; 
	  //     ..
	  //     .. Local Scalars ..
	         boolean            notran;
	         char          transn, transt;
	         int kase[] = new int[1];
	         int            count, i, j, nz;
	         double   eps, lstres, s, safe1, safe2, safmin;
	  //     ..
	  //     .. Local Arrays ..
	         int isave[] = new int[ 3 ];
	         double array[][];
	         double array2[][];
	         double v1[];
	         double v2[];
	         double v3[];
	  //     ..
	  //     .. External Subroutines ..
	  //       EXTERNAL           daxpy, dcopy, dgttrs, dlacn2, dlagtm, xerbla
	  //     ..
	  //     .. Intrinsic Functions ..
	  //       INTRINSIC          abs, max
	  //     ..
	  //     .. External Functions ..
	  //       LOGICAL            lsame
	  //       DOUBLE PRECISION   dlamch
	  //       EXTERNAL           lsame, dlamch
	  //     ..
	  //     .. Executable Statements ..
	  
	  //     Test the input parameters.
	  
	         info[0] = 0;
	         notran = ( (trans == 'N') || (trans == 'n') );
	         if (!notran && !(( trans == 'T' ) || (trans == 't')) && !( (trans == 'C') || (trans == 'c'))) { 
	            info[0] = -1;
	         }
	         else if ( n < 0 ) {
	            info[0] = -2;
	         }
	         else if ( nrhs < 0 ) {
	            info[0] = -3;
	         }
	         else if( ldb < Math.max( 1, n ) ) {
	            info[0] = -13;
	         }
	         else if ( ldx < Math.max( 1, n ) ) {
	            info[0] = -15;
	         }
	         if ( info[0] != 0 ) {
	            UI.setDataText("dgtrfs has info[0] = " + info[0] + "\n");
	            return;
	         }
	  
	  //     Quick return if possible
	  
	         if ( n == 0 || nrhs == 0 ) {
	            for (j = 0; j < nrhs; j++) {
	               ferr[ j ] = zero;
	               berr[ j ] = zero;
	            }
	            return;
	         } // if ( n == 0 || nrhs == 0 )
	  
	         if ( notran ) {
	            transn = 'N';
	            transt = 'T';
	         }
	         else {
	            transn = 'T';
	            transt = 'N';
	         }
	  
	  //     NZ = maximum number of nonzero elements in each row of A, plus 1
	  
	         nz = 4;
	         eps = ge.dlamch( 'E' );
	         safmin = ge.dlamch( 'S' );
	         safe1 = nz*safmin;
	         safe2 = safe1 / eps;
	  
	  //     Do for each right hand side
	  
	         for (j = 0; j < nrhs; j++) {
	 
	            count = 1;
	            lstres = three;
	            while (true) {
	  
	  //        Loop until stopping criterion is satisfied.
	  
	  //        Compute residual R = B - op(A) * X,
	  //        where op(A) = A, A**T, or A**H, depending on TRANS.
	  
	               for (i = 0; i < n; i++) {
	            	   work[n+i] = b[i][j];
	               }
	            array = new double[n][1];
	            for (i = 0; i < n; i++) {
	            	array[i][0] = x[i][j];
	            }
	            array2 = new double[n][1];
	            for (i = 0; i < n; i++) {
	            	array2[i][0] = work[n+i];
	            }
	            dlagtm( trans, n, 1, -one, dl, d, du, array, ldx, one,
	                  array2, n );
	            for (i = 0; i < n; i++) {
	            	work[n+i] = array2[i][0];
	            }
	  
	  //        Compute abs(op(A))*abs(x) + abs(b) for use in the backward
	  //        error bound.
	 
	            if ( notran ) {
	               if ( n == 1 ) {
	                  work[ 0 ] = Math.abs( b[ 0][ j ] ) + Math.abs( d[ 0 ]*x[ 0][ j ] );
	               }
	               else { // n != 1
	                  work[ 0 ] = Math.abs( b[ 0][ j ] ) + Math.abs( d[ 1 ]*x[ 0][ j ] ) +
	                              Math.abs( du[ 0 ]*x[ 1][ j ] );
	                  for ( i = 1; i < n - 1; i++) {
	                     work[ i ] = Math.abs( b[ i][ j ] ) +
	                                 Math.abs( dl[ i-1 ]*x[ i-1][ j ] ) +
	                                 Math.abs( d[ i ]*x[ i][ j ] ) +
	                                 Math.abs( du[ i ]*x[ i+1][ j ] );
	                  } // for ( i = 1; i < n - 1; i++)
	                  work[ n-1 ] = Math.abs( b[ n-1][ j ] ) +
	                                Math.abs( dl[ n-2 ]*x[ n-2][ j ] ) +
	                                Math.abs( d[ n-1 ]*x[ n-1][ j ] );
	               } // else n != 1
	            } // if (notran)
	            else { // !notran
	               if ( n == 1 ) {
	                  work[ 0 ] = Math.abs( b[ 0][ j ] ) + Math.abs( d[ 0 ]*x[ 0][ j ] );
	               } // if (n == 1)
	               else { // n != 1
	                  work[ 0 ] = Math.abs( b[ 0][ j ] ) + Math.abs( d[ 0 ]*x[ 0][ j ] ) +
	                              Math.abs( dl[ 0 ]*x[ 1][ j ] );
	                  for (i = 1; i < n - 1; i++) {
	                     work[ i ] = Math.abs( b[ i][ j ] ) +
	                                 Math.abs( du[ i-1 ]*x[ i-1][ j ] ) +
	                                 Math.abs( d[ i ]*x[ i][ j ] ) +
	                                 Math.abs( dl[ i ]*x[ i+1][ j ] );
	                  } // for (i = 1; i < n - 1; i++)
	                  work[ n-1 ] = Math.abs( b[ n-1][ j ] ) +
	                                Math.abs( du[ n-2 ]*x[ n-2][ j ] ) +
	                                Math.abs( d[ n-1 ]*x[ n-1][ j ] );
	               } // else n != 1
	            } // else !notran
	  
	  //        Compute componentwise relative backward error from formula
	  
	  //        max(i) ( abs(R(i)) / ( abs(op(A))*abs(X) + abs(B) )(i) )
	  
	  //        where abs(Z) is the componentwise absolute value of the matrix
	  //        or vector Z.  If the i-th component of the denominator is less
	  //        than SAFE2, then SAFE1 is added to the i-th components of the
	  //        numerator and denominator before dividing.
	  
	            s = zero;
	            for (i = 0; i < n; i++) {
	               if ( work[ i ] > safe2 ) {
	                  s = Math.max( s, Math.abs( work[ n+i ] ) / work[ i ] );
	               }
	               else {
	                  s = Math.max( s, ( Math.abs( work[ n+i ] )+safe1 ) /
	                      ( work[ i ]+safe1 ) );
	               }
	            } // for (i = 0; i < n; i++)
	            berr[ j ] = s;
	  
	  //        Test stopping criterion. Continue iterating if
	  //           1) The residual BERR(J) is larger than machine epsilon, and
	  //           2) BERR(J) decreased by at least a factor of 2 during the
	  //              last iteration, and
	  //           3) At most ITMAX iterations tried.
	 
	            if ( berr[ j ] > eps && two*berr[ j ] <= lstres &&
	                 count <= itmax ) {
	  
	  //           Update solution and try again.
	               array = new double[n][1];
	               for (i = 0; i < n; i++) {
	            	   array[i][0] = work[n+i];
	               }
	               dgttrs( trans, n, 1, dlf, df, duf, du2, ipiv,
	                       array, n, info );
	               for (i = 0; i < n; i++) {
	            	   work[n+i] = array[i][0];
	               }
	               for (i = 0; i < n; i++) {
	            	   x[i][j] = x[i][j] + work[n+i];
	               }
	               lstres = berr[ j ];
	               count = count + 1;
	            } // if ( berr[ j ] > eps && two*berr[ j ] <= lstres &&
	            else {
	            	break;
	            }
	         } // while (true)
	  
	  //        Bound error from formula
	  
	  //        norm(X - XTRUE) / norm(X) .le. FERR =
	  //        norm( abs(inv(op(A)))*
	  //           ( abs(R) + NZ*EPS*( abs(op(A))*abs(X)+abs(B) ))) / norm(X)
	  
	  //        where
	  //          norm(Z) is the magnitude of the largest component of Z
	  //          inv(op(A)) is the inverse of op(A)
	  //          abs(Z) is the componentwise absolute value of the matrix or
	  //             vector Z
	  //          NZ is the maximum number of nonzeros in any row of A, plus 1
	  //          EPS is machine epsilon
	  
	  //        The i-th component of abs(R)+NZ*EPS*(abs(op(A))*abs(X)+abs(B))
	  //        is incremented by SAFE1 if the i-th component of
	  //        abs(op(A))*abs(X) + abs(B) is less than SAFE2.
	  
	  //        Use DLACN2 to estimate the infinity-norm of the matrix
	  //           inv(op(A)) * diag(W),
	  //        where W = abs(R) + NZ*EPS*( abs(op(A))*abs(X)+abs(B) )))
	  
	            for (i = 0; i < n; i++) {
	               if ( work[ i ] > safe2 ) {
	                  work[ i ] = Math.abs( work[ n+i ] ) + nz*eps*work[ i ];
	               }
	               else {
	                  work[ i ] = Math.abs( work[ n+i ] ) + nz*eps*work[ i ] + safe1;
	               }
	            } // for (i = 0; i < n; i++)
	  
	            kase[0] = 0;
	            while (true) {
	            v1 = new double[n];
	            v2 = new double[n];
	            for (i = 0; i < n; i++) {
	            	v2[i] = work[n+i];
	            }
	            v3 = new double[1];
	            v3[0] = ferr[j];
	            le.dlacn2( n, v1, v2, iwork, v3,
	                    kase, isave );
	            for (i = 0; i < n; i++) {
	            	work[2*n+i] = v1[i];
	            	work[n+i] = v2[i];
	            }
	            ferr[j] = v3[0];
	            if ( kase[0] != 0 ) {
	               if ( kase[0] == 1 ) {
	  
	  //              Multiply by diag(W)*inv(op(A)**T).
	  
	                   array = new double[n][1];
	                   for (i = 0; i < n; i++) {
	                	   array[i][0] = work[n+i];
	                   }
	            	   dgttrs( transt, n, 1, dlf, df, duf, du2, ipiv,
	                          array, n, info );
	            	   for (i = 0; i < n; i++) {
	            		   work[n+i] = array[i][0];
	            	   }
	                   for (i = 0; i < n; i++) {
	                      work[ n+i ] = work[ i ]*work[ n+i];
	                   }
	               } // if (kase[0] == 1)
	               else { // kase[0] != 1
	  
	  //              Multiply by inv(op(A))*diag(W).
	  
	                  for (i = 0; i < n; i++) {
	                     work[ n+i ] = work[ i ]*work[ n+i ];
	                  }
	                  array = new double[n][1];
	                  for (i = 0; i < n; i++) {
	                	  array[i][0] = work[n+i];
	                  }
	                  dgttrs( transn, n, 1, dlf, df, duf, du2, ipiv,
	                          array, n, info );
	                  for (i = 0; i < n; i++) {
	                	  work[n+i] = array[i][0];
	                  }
	               } // else kase[0] != 1
	            } // if (kase[0] != 0)
	            else { // kase[0] == 0
	            	break;
	            }
	        } // while (true)
	               
	  //        Normalize error.
	 
	            lstres = zero;
	            for (i = 0; i < n; i++) {
	               lstres = Math.max( lstres, Math.abs( x[ i][ j ] ) );
	            }
	            if ( lstres != zero ) {
	                ferr[ j ] = ferr[ j ] / lstres;
	            }
	  
	         } // for (j = 0; j < nrhs; j++)
	  
	         return;
	     } // dgtrfs
	  
	  private void dgtt02(char TRANS, int N, int NRHS, double DL[], double D[], double DU[], 
			                double X[][], int LDX, double B[][], int LDB,
	                        double RWORK[], double RESID[] ) {
	  
	  //  -- LAPACK test routine (version 3.1) --
	  //     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
	  //     November 2006
	  
	  //     .. Scalar Arguments ..
	  //       CHARACTER          TRANS
	  //       INTEGER            LDB, LDX, N, NRHS
	  //       DOUBLE PRECISION   RESID
	  //     ..
	  //     .. Array Arguments ..
	  //       DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * ),
	  //      $                   RWORK( * ), X( LDX, * )
	  //     ..
	 
	  //  Purpose
	  //  =======
	  
	  //  DGTT02 computes the residual for the solution to a tridiagonal
	  //  system of equations:
	  //     RESID = norm(B - op(A)*X) / (norm(A) * norm(X) * EPS),
	  //  where EPS is the machine epsilon.
	  
	  //  Arguments
	  //  =========
	  
	  //  TRANS   (input) CHARACTER
	  //          Specifies the form of the residual.
	  //          = 'N':  B - A * X  (No transpose)
	  //          = 'T':  B - A'* X  (Transpose)
	  //          = 'C':  B - A'* X  (Conjugate transpose = Transpose)
	  
	  //  N       (input) INTEGTER
	  //          The order of the matrix A.  N >= 0.
	  
	  //  NRHS    (input) INTEGER
	  //          The number of right hand sides, i.e., the number of columns
	  //          of the matrices B and X.  NRHS >= 0.
	  
	  //  DL      (input) DOUBLE PRECISION array, dimension (N-1)
	  //          The (n-1) sub-diagonal elements of A.
	  
	  //  D       (input) DOUBLE PRECISION array, dimension (N)
	  //          The diagonal elements of A.
	  
	  //  DU      (input) DOUBLE PRECISION array, dimension (N-1)
	  //          The (n-1) super-diagonal elements of A.
	  
	  //  X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
	  //          The computed solution vectors X.
	  
	  //  LDX     (input) INTEGER
	  //          The leading dimension of the array X.  LDX >= max(1,N).
	  
	  //  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
	  //          On entry, the right hand side vectors for the system of
	  //          linear equations.
	  //          On exit, B is overwritten with the difference B - op(A)*X.
	  
	  //  LDB     (input) INTEGER
	  //          The leading dimension of the array B.  LDB >= max(1,N).
	  
	  //  RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
	  
	  //  RESID   (output) DOUBLE PRECISION
	  //          norm(B - op(A)*X) / (norm(A) * norm(X) * EPS)
	  
	  //  =====================================================================
	  
	  //     .. Parameters ..
		     final double ONE = 1.0;
		     final double ZERO = 0.0;
	  
	  //     .. Local Scalars ..
	         int            I, J;
	         double   ANORM, BNORM, EPS, XNORM;
	  //     ..
	  //     .. External Functions ..
	  //       LOGICAL            LSAME
	  //       DOUBLE PRECISION   DASUM, DLAMCH, DLANGT
	  //       EXTERNAL           LSAME, DASUM, DLAMCH, DLANGT
	  //     ..
	  //     .. External Subroutines ..
	  //       EXTERNAL           DLAGTM
	  //     ..
	  //     .. Intrinsic Functions ..
	  //       INTRINSIC          MAX
	  //     ..
	  //     .. Executable Statements ..
	  
	  //     Quick exit if N = 0 or NRHS = 0
	  
	         RESID[0] = ZERO;
	         if ( N <= 0 || NRHS == 0 ) {
	            return;
	         }
	  
	  //     Compute the maximum over the number of right hand sides of
	  //        norm(B - op(A)*X) / ( norm(A) * norm(X) * EPS ).
	  
	         if ((TRANS == 'N') || (TRANS == 'n')) {
	            ANORM = dlangt( '1', N, DL, D, DU );
	         }
	         else {
	            ANORM = dlangt( 'I', N, DL, D, DU );
	         }
	  
	  //     Exit with RESID = 1/EPS if ANORM = 0.
	  
	         EPS = ge.dlamch( 'E' );
	         if ( ANORM <= ZERO ) {
	            RESID[0] = ONE / EPS;
	            return;
	         }
	  
	  //     Compute B - op(A)*X.
	  
	         dlagtm( TRANS, N, NRHS, -ONE, DL, D, DU, X, LDX, ONE, B, LDB );
	 
	         for (J = 0; J < NRHS; J++) {
	        	     BNORM = 0.0;
	        	     for (I = 0; I < N; I++) {
	        	    	 BNORM += Math.abs(B[I][J]);
	        	     }
	        	     XNORM = 0.0;
	        	     for (I = 0; I < N; I++) {
	        	    	 XNORM += Math.abs(X[I][J]);
	        	     }
	                 if ( XNORM <= ZERO ) {
	                    RESID[0] = ONE / EPS;
	                 }
	                 else {
	                    RESID[0] = Math.max( RESID[0], ( ( BNORM / ANORM ) / XNORM ) / EPS );
	                 }
	         } // for (J = 0; J < NRHS; J++)
	  
	         return;
	  } // dgtt02
	  


	  
      private void printHeader() {
 	     UI.setDataText("DGT: General tridaigonal\n");
 	     UI.setDataText("Matrix types (1-6 have specfied condition numbers):\n");
 	     UI.setDataText("1. Diagonal\n");
 	     UI.setDataText("2. Random, CNDNUM = 2\n");
 	     UI.setDataText("3. Random, CNDNUM = sqrt(0.1/EPS)\n");
 	     UI.setDataText("4. Random, CNDNUM = 0.1/EPS\n");
 	     UI.setDataText("5. Scaled near underflow\n");
 	     UI.setDataText("6. Scaled near overflow\n");
 	     UI.setDataText("7. Random, unspecified CNDNUM\n");
 	     UI.setDataText("8. First column zero\n");
 	     UI.setDataText("9. Last column zero\n");
 	     UI.setDataText("10. Last n/2 columns zero\n");
 	     UI.setDataText("11. Scaled near underflow\n");
 	     UI.setDataText("12. Scaled near overflow\n");
 	     UI.setDataText("Test ratios: \n");
 	     UI.setDataText("1. norm (L * U - A) / (N * norm(A) * EPS)\n");
 	     UI.setDataText("2. norm(B - A * X) / (norm(A) * norm(X) * EPS)\n");
 	     UI.setDataText("3. norm(X - XACT) / (norm(XACT) * CNDNUM * EPS)\n");
 	     UI.setDataText("4. norm(X - XACT) / (norm(XACT) * CNDNUM * EPS), refined\n");
 	     UI.setDataText("5. norm(X - XACT) / (norm(XACT) * (error bound))\n");
 	     UI.setDataText("6. (backward error) / EPS)\n");
 	     UI.setDataText("7. RCOND * CNDNUM - 1.0\n");
      }
      
       // \par Purpose:
	   //  =============
	   
	   // \verbatim
	   
	   // DLAGTM performs a matrix-vector product of the form
	   
	   //    B := alpha * A * X + beta * B
	  
	   // where A is a tridiagonal matrix of order N, B and X are N by NRHS
	   // matrices, and alpha and beta are real scalars, each of which may be
	   // 0., 1., or -1.
	   // \endverbatim
	  
	   //  Arguments:
	   //  ==========
	  
	   // \param[in] TRANS
	   // \verbatim
	   //          TRANS is CHARACTER*1
	   //          Specifies the operation applied to A.
	   //          = 'N':  No transpose, B := alpha * A * X + beta * B
	   //          = 'T':  Transpose,    B := alpha * A'* X + beta * B
	   //          = 'C':  Conjugate transpose = Transpose
	   // \endverbatim
	   
	   // \param[in] N
	   // \verbatim
	   //          N is INTEGER
	   //          The order of the matrix A.  N >= 0.
	   // \endverbatim
	   
	   // \param[in] NRHS
	   // \verbatim
	   //          NRHS is INTEGER
	   //          The number of right hand sides, i.e., the number of columns
	   //          of the matrices X and B.
	   // \endverbatim
	   
	   // \param[in] ALPHA
	   // \verbatim
	   //          ALPHA is DOUBLE PRECISION
	   //          The scalar alpha.  ALPHA must be 0., 1., or -1.; otherwise,
	   //          it is assumed to be 0.
	   // \endverbatim
	   
	   // \param[in] DL
	   // \verbatim
	   //          DL is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) sub-diagonal elements of T.
	   // \endverbatim
	   
	   // \param[in] D
	   // \verbatim
	   //          D is DOUBLE PRECISION array, dimension (N)
	   //          The diagonal elements of T.
	   // \endverbatim
	   
	   // \param[in] DU
	   // \verbatim
	   //          DU is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) super-diagonal elements of T.
	   // \endverbatim
	  
	  // \param[in] X
	  // \verbatim
	  //          X is DOUBLE PRECISION array, dimension (LDX,NRHS)
	  //          The N by NRHS matrix X.
	  // \endverbatim
	  
	  // \param[in] LDX
	  // \verbatim
	  //          LDX is INTEGER
	  //          The leading dimension of the array X.  LDX >= max(N,1).
	  // \endverbatim
	  //
	  // \param[in] BETA
	  // \verbatim
	  //          BETA is DOUBLE PRECISION
	  //          The scalar beta.  BETA must be 0., 1., or -1.; otherwise,
	  //          it is assumed to be 1.
	  // \endverbatim
	  
	  // \param[in,out] B
	  // \verbatim
	  //          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
	  //          On entry, the N by NRHS matrix B.
	  //          On exit, B is overwritten by the matrix expression
	  //          B := alpha * A * X + beta * B.
	  // \endverbatim
	  
	  // \param[in] LDB
	  // \verbatim
	  //          LDB is INTEGER
	  //          The leading dimension of the array B.  LDB >= max(N,1).
	  // \endverbatim
	  
	  //  Authors:
	  //  ========
	  
	  // \author Univ. of Tennessee 
	  // \author Univ. of California Berkeley 
	  // \author Univ. of Colorado Denver 
	  // \author NAG Ltd. 
	  
	  // \date September 2012
	  
	  // \ingroup doubleOTHERauxiliary
	  
	  //  =====================================================================
	         private void dlagtm( char trans, int n, int nrhs, double alpha, double dl[], 
	        		              double d[], double du[], double x[][], int ldx, double beta,
	                              double b[][], int ldb ) {
	  
	  //  -- LAPACK auxiliary routine (version 3.4.2) --
	  //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	  //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	  //     September 2012
	  
	  //     .. Scalar Arguments ..
	  //       CHARACTER          trans
	  //       INTEGER            ldb, ldx, n, nrhs
	  //       DOUBLE PRECISION   alpha, beta
	  //     ..
	  //     .. Array Arguments ..
	  //       DOUBLE PRECISION   b( ldb, * ), d( * ), dl( * ), du( * ),
	  //     $                   x( ldx, * )
	  //     ..
	  
	  //  =====================================================================
	  
	  //     .. Parameters ..
	         final double one = 1.0;
	         final double zero = 0.0;
	  //     ..
	  //     .. Local Scalars ..
	         int            i, j;
	  //     ..
	  //     .. External Functions ..
	  //       LOGICAL            lsame
	  //       EXTERNAL           lsame
	  //     ..
	  //     .. Executable Statements ..
	  
	        if ( n == 0 ) {
	            return;
	        }
	  
	  //     Multiply B by BETA if BETA.NE.1.
	  
	         if ( beta == zero ) {
	            for( j = 0; j < nrhs; j++) {
	               for (i = 0; i < n; i++) {
	                  b[ i][ j ] = zero;
	               }
	            }
	         } // if ( beta == zero )
	         else if ( beta == -one ) {
	            for (j = 0; j < nrhs; j++) {
	               for (i = 0; i < n; i++) {
	                  b[ i][ j ] = -b[ i][ j ];
	               }
	            }
	         } // else if ( beta == -one )
	  
	         if ( alpha == one ) {
	            if ((trans == 'N' ) || (trans == 'n')) {
	  
	  //           Compute B := B + A*X
	  
	               for (j = 0; j < nrhs; j++) {
	                  if ( n == 1 ) {
	                     b[ 0][ j] = b[ 0][ j ] + d[ 0 ]*x[ 0][ j ];
	                  }
	                  else {
	                     b[ 0][ j ] = b[ 0][ j ] + d[ 0 ]*x[ 0][ j ] +
	                                 du[ 0 ]*x[ 1][ j ];
	                     b[ n-1][ j ] = b[ n-1][ j ] + dl[ n-2 ]*x[ n-2][ j ] +
	                                 d[ n-1 ]*x[ n-1][ j ];
	                     for (i = 1; i < n - 1; i++) {
	                        b[ i][ j ] = b[ i][ j ] + dl[ i-1 ]*x[ i-1][ j ] +
	                                    d[ i ]*x[ i][ j ] + du[ i ]*x[ i+1][ j];
	                     }
	                  }
	               } // for (j = 0; j < nrhs; j++)
	            } // if ((trans == 'N' ) || (trans == 'n'))
	            else {
	  
	  //           Compute B := B + A**T*X
	  
	               for (j = 0; j < nrhs; j++) {
	                  if ( n == 1 ) {
	                     b[ 0][ j ] = b[ 0][ j ] + d[ 0 ]*x[0][ j ];
	                  }
	                  else {
	                     b[ 0][ j ] = b[0][ j ] + d[ 0 ]*x[ 0][ j ] +
	                                 dl[ 0 ]*x[ 1][ j ];
	                     b[ n-1][ j ] = b[ n-1][ j ] + du[ n-2 ]*x[ n-2][ j ] +
	                                 d[ n - 1]*x[ n-1][ j ];
	                     for (i = 1; i < n - 1; i++) {
	                        b[ i][ j ] = b[ i][ j ] + du[ i-1 ]*x[ i-1][ j ] +
	                                     d[ i ]*x[ i][ j ] + dl[ i ]*x[ i+1][ j ];
	                     } // for (i = 1; i < n - 1; i++)
	                  }
	               } // for (j = 0; j < nrhs; j++)
	            } // else
	         } // if ( alpha == one )
	         else if ( alpha == -one ) {
	            if (( trans == 'N' ) || (trans == 'n')) {
	  
	  //           Compute B := B - A*X
	  
	               for (j = 0; j < nrhs; j++) {
	                  if ( n == 1 ) {
	                     b[ 0][ j ] = b[ 0][ j ] - d[ 0 ]*x[ 0][ j ];
	                  }
	                  else {
	                     b[ 0][ j ] = b[0][ j ] - d[ 0 ]*x[0][ j ] -
	                                  du[ 0 ]*x[ 1][ j ];
	                     b[ n-1][ j ] = b[ n-1][ j ] - dl[ n-2 ]*x[ n-2][ j ] -
	                                    d[ n-1 ]*x[ n-1][ j ];
	                     for ( i = 1; i < n - 1; i++) {
	                        b[ i][ j ] = b[ i][ j ] - dl[ i-1 ]*x[ i-1][ j ] -
	                                     d[ i ]*x[ i][ j ] - du[ i ]*x[ i+1][ j ];
	                     } // for ( i = 1; i < n - 1; i++)
	                  }
	               } // for (j = 0; j < nrhs; j++)
	            }
	            else {
	  
	  //           Compute B := B - A**T*X
	  
	               for (j = 0; j < nrhs; j++) {
	                  if ( n == 1 ) {
	                     b[ 0][ j ] = b[ 0][ j ] - d[ 0 ]*x[ 0][ j ];
	                  }
	                  else {
	                     b[ 0][ j ] = b[ 0][ j ] - d[ 0 ]*x[ 0][ j ] -
	                                  dl[ 0 ]*x[ 1][ j ];
	                     b[ n-1][ j ] = b[ n-1][ j ] - du[ n-2 ]*x[ n-2][ j ] -
	                                    d[ n-1 ]*x[ n-1][ j ];
	                     for ( i = 1; i < n - 1; i++) {
	                        b[ i][ j ] = b[ i][ j ] - du[ i-1 ]*x[ i-1][ j ] -
	                                     d[ i ]*x[ i][ j ] - dl[ i ]*x[ i+1][ j ];
	                     }
	                  }
	               } // for (j = 0; j < nrhs; j++)
	            }
	         } // else if ( alpha == -one )
	         return;
	         } // dlagtm
	  

      
      private void dgtcon(char NORM, int N, double DL[], double D[], double DU[], double DU2[],
    		              int IPIV[], double ANORM, double RCOND[],
	                      double WORK[], int IWORK[], int INFO[]) {
	 
	  //  -- LAPACK routine (version 3.3.1) --
	  //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	  //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	  //  -- April 2011                                                      --
	  //
	  //     Modified to call DLACN2 in place of DLACON, 5 Feb 03, SJH.
	  
	  //     .. Scalar Arguments ..
	  //       CHARACTER          NORM
	  //       INTEGER            INFO, N
	  //       DOUBLE PRECISION   ANORM, RCOND
	  //     ..
	  //     .. Array Arguments ..
	  //       INTEGER            IPIV( * ), IWORK( * )
	  //       DOUBLE PRECISION   D( * ), DL( * ), DU( * ), DU2( * ), WORK( * )
	  //     ..
	  
	  //  Purpose
	  //  =======
	  //
	  //  DGTCON estimates the reciprocal of the condition number of a real
	  //  tridiagonal matrix A using the LU factorization as computed by
	  //  DGTTRF.
	 
	  //  An estimate is obtained for norm(inv(A)), and the reciprocal of the
	  //  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
	  
	  //  Arguments
	  //  =========
	 
	  //  NORM    (input) CHARACTER*1
	  //          Specifies whether the 1-norm condition number or the
	  //          infinity-norm condition number is required:
	  //          = '1' or 'O':  1-norm;
	  //          = 'I':         Infinity-norm.
	  
	  //  N       (input) INTEGER
	  //          The order of the matrix A.  N >= 0.
	  
	  //  DL      (input) DOUBLE PRECISION array, dimension (N-1)
	  //          The (n-1) multipliers that define the matrix L from the
	  //          LU factorization of A as computed by DGTTRF.
	  
	  //  D       (input) DOUBLE PRECISION array, dimension (N)
	  //          The n diagonal elements of the upper triangular matrix U from
	  //          the LU factorization of A.
	  
	  //  DU      (input) DOUBLE PRECISION array, dimension (N-1)
	  //          The (n-1) elements of the first superdiagonal of U.
	  
	  //  DU2     (input) DOUBLE PRECISION array, dimension (N-2)
	  //          The (n-2) elements of the second superdiagonal of U.
	 
	  //  IPIV    (input) INTEGER array, dimension (N)
	  //          The pivot indices; for 1 <= i <= n, row i of the matrix was
	  //          interchanged with row IPIV(i).  IPIV(i) will always be either
	  //          i or i+1; IPIV(i) = i indicates a row interchange was not
	  //          required.
	  
	  //  ANORM   (input) DOUBLE PRECISION
	  //          If NORM = '1' or 'O', the 1-norm of the original matrix A.
	  //          If NORM = 'I', the infinity-norm of the original matrix A.
	  
	  //  RCOND   (output) DOUBLE PRECISION
	  //          The reciprocal of the condition number of the matrix A,
	  //          computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
	  //          estimate of the 1-norm of inv(A) computed in this routine.
	 
	  //  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
	  
	  //  IWORK   (workspace) INTEGER array, dimension (N)
	  
	  //  INFO    (output) INTEGER
	  //          = 0:  successful exit
	  //          < 0:  if INFO = -i, the i-th argument had an illegal value
	 
	  //  =====================================================================
	  
	  //     .. Parameters ..
    	          final double ONE = 1.0;
    	          final double ZERO = 0.0;
	  //     ..
	  //     .. Local Scalars ..
	         boolean            ONENRM;
	         int            I, KASE1;
	         int KASE[] = new int[1];
	         double AINVNM[] = new double[1];
	         double array[][] = new double[N][1];
	         double v1[] = new double[N];
	  //     ..
	  //     .. Local Arrays ..
	         int ISAVE[] = new int[ 3 ];
	  //     ..
	  //     .. External Functions ..
	  //       LOGICAL            LSAME
	  //       EXTERNAL           LSAME
	  //     ..
	  //     .. External Subroutines ..
	  //       EXTERNAL           DGTTRS, DLACN2, XERBLA
	  //     ..
	  //     .. Executable Statements ..
	  
	  //     Test the input arguments.
	  
	         INFO[0] = 0;
	         ONENRM = NORM == '1' || NORM == 'O' || NORM == 'o';
	         if (!ONENRM && !(( NORM == 'I' ) || (NORM == 'i'))) {
	            INFO[0] = -1;
	         }
	         else if ( N < 0 ) {
	            INFO[0] = -2;
	         }
	         else if ( ANORM < ZERO ) {
	            INFO[0] = -8;
	         }
	         if ( INFO[0] != 0 ) {
	            UI.setDataText("dgtcon has INFO[0] = " + INFO[0] + "\n" );
	            return;
	         }
	 
	  //     Quick return if possible
	  
	         RCOND[0] = ZERO;
	         if ( N == 0 ) {
	            RCOND[0] = ONE;
	            return;
	         }
	         else if ( ANORM == ZERO ) {
	            return;
	         }
	  
	  //     Check that D(0:N-1) is non-zero.
	  
	         for (I = 0; I < N; I++) {
	            if ( D[ I ] == ZERO ) {
	               return;
	            }
	         }
	  
	  //     AINVNM = ZERO;
	         if ( ONENRM ) {
	          KASE1 = 1;
	         }
	         else {
	            KASE1 = 2;
	         }
	         KASE[0] = 0;
	         while (true) {
	             le.dlacn2( N, v1, WORK, IWORK, AINVNM, KASE, ISAVE );
	             for (I = 0; I < N; I++) {
	            	   array[I][0] = WORK[I];
	              }
	             if( KASE[0]!= 0 ) {
	                if ( KASE[0] == KASE1 ) {
	  
	  //           Multiply by inv(U)*inv(L).
	               dgttrs( 'N', N, 1, DL, D, DU, DU2, IPIV,
	                        array, N, INFO );
	                } // if (KASE == KASE1
	                else {
	  
	                    // Multiply by inv(L**T)*inv(U**T).
	 
	                    dgttrs( 'T', N, 1, DL, D, DU, DU2, IPIV, array,
	                            N, INFO );
	                } // else
	                for (I = 0; I < N; I++) {
	                	WORK[I] = array[I][0];
	                }
	             } // if (KASE != 0)
	             else {
	            	 break;
	             }
	         } // while (true)
	  
	  //     Compute the estimate of the reciprocal condition number.
	  
	         if ( AINVNM[0] != ZERO ) {
	            RCOND[0] = ( ONE / AINVNM[0] ) / ANORM;
	         }
	         return;
      } // dgtcon


    
       // \par Purpose:
	   //  =============
	   
	   // \verbatim
	   
	   // DGTT01 reconstructs a tridiagonal matrix A from its LU factorization
	   // and computes the residual
	   //    norm(L*U - A) / ( norm(A) * EPS ),
	   // where EPS is the machine epsilon.
	   // \endverbatim
	  
	   //  Arguments:
	   //  ==========
	  
	   // \param[in] N
	   // \verbatim
	   //          N is INTEGTER
	   //          The order of the matrix A.  N >= 0.
	   // \endverbatim
	   
	   // \param[in] DL
	   // \verbatim
	   //          DL is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) sub-diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] D
	   // \verbatim
	   //          D is DOUBLE PRECISION array, dimension (N)
	   //          The diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] DU
	   // \verbatim
	   //          DU is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) super-diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] DLF
	   // \verbatim
	   //          DLF is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) multipliers that define the matrix L from the
	   //          LU factorization of A.
	   // \endverbatim
	   
	   // \param[in] DF
	   // \verbatim
	   //          DF is DOUBLE PRECISION array, dimension (N)
	   //          The n diagonal elements of the upper triangular matrix U from
	   //          the LU factorization of A.
	   // \endverbatim
	   
	   // \param[in] DUF
	   // \verbatim
	   //          DUF is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) elements of the first super-diagonal of U.
	   // \endverbatim
	   
	   // \param[in] DU2
	   // \verbatim
	   //          DU2 is DOUBLE PRECISION array, dimension (N-2)
	   //          The (n-2) elements of the second super-diagonal of U.
	   // \endverbatim
	   
	   // \param[in] IPIV
	   // \verbatim
	   //          IPIV is INTEGER array, dimension (N)
	   //          The pivot indices; for 1 <= i <= n, row i of the matrix was
	   //          interchanged with row IPIV(i).  IPIV(i) will always be either
	   //          i or i+1; IPIV(i) = i indicates a row interchange was not
	   //          required.
	   // \endverbatim
	   
	  // \param[out] WORK
	  // \verbatim
	  //          WORK is DOUBLE PRECISION array, dimension (LDWORK,N)
	  // \endverbatim
	  
	  // \param[in] LDWORK
	  // \verbatim
	  //          LDWORK is INTEGER
	  //          The leading dimension of the array WORK.  LDWORK >= max(1,N).
	  // \endverbatim
	 
	  // \param[out] RWORK
	  // \verbatim
	  //          RWORK is DOUBLE PRECISION array, dimension (N)
	  // \endverbatim
	  
	  // \param[out] RESID
	  // \verbatim
	  //          RESID is DOUBLE PRECISION
	  //          The scaled residual:  norm(L*U - A) / (norm(A) * EPS)
	  // \endverbatim
	  
	  //  Authors:
	  //  ========
	  
	  // \author Univ. of Tennessee 
	  // \author Univ. of California Berkeley 
	  // \author Univ. of Colorado Denver 
	  // \author NAG Ltd. 
	  
	  // \date November 2011
	  
	  // \ingroup double_lin
	  
	  //  =====================================================================
	         private void dgtt01( int n, double dl[], double d[], double du[], double dlf[], 
	        		            double df[], double duf[], double du2[], int ipiv[], double work[][],
	                           int ldwork, double rwork[], double resid[] ) {
	  
	  //  -- LAPACK test routine (version 3.4.0) --
	  //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	  //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	  //     November 2011
	  
	  //     .. Scalar Arguments ..
	  //       INTEGER            ldwork, n
	  //       DOUBLE PRECISION   resid
	  //     ..
	  //     .. Array Arguments ..
	  //       INTEGER            ipiv( * )
	  //       DOUBLE PRECISION   d( * ), df( * ), dl( * ), dlf( * ), du( * ),
	  //      $                   du2( * ), duf( * ), rwork( * ),
	  //      $                   work( ldwork, * )
	  //     ..
	  
	  //  =====================================================================
	  
	  //     .. Parameters ..
	        	final double one = 1.0;
	        	final double zero = 0.0;
	  
	  //     .. Local Scalars ..
	         int            i, ip, j, lastj, k;
	         double   anorm, eps, li, temp;
	 
	  //     .. External Functions ..
	  //       DOUBLE PRECISION   dlamch, dlangt, dlanhs
	  //       EXTERNAL           dlamch, dlangt, dlanhs
	 
	  //     .. Intrinsic Functions ..
	  //       INTRINSIC          min
	  
	  //     .. External Subroutines ..
	  //       EXTERNAL           daxpy, dswap
	  
	  //    .. Executable Statements ..
	  
	  //     Quick return if possible
	  
	         if ( n <= 0 ) {
	            resid[0] = zero;
	            return;
	         }
	  
	         eps = ge.dlamch( 'E' );
	  
	  //     Copy the matrix U to WORK.
	  
	         for (j = 0; j < n; j++) {
	            for (i = 0; i < n; i++) {
	               work[ i][ j ] = zero;
	            }
	         }
	         for (i = 1; i <= n; i++) {
	            if ( i == 1 ) {
	               work[ i-1][ i-1] = df[ i-1];
	               if ( n >= 2 ) {
	                  work[ i-1][ i] = duf[ i-1 ];
	               }
	               if ( n >= 3 ) {
	                  work[ i-1][ i+1 ] = du2[ i-1 ];
	               }
	            } // if (i == 1)
	            else if ( i == n ) {
	               work[ i-1][ i-1 ] = df[ i-1 ];
	            }
	            else {
	               work[ i-1][ i-1 ] = df[ i-1 ];
	               work[ i-1][ i ] = duf[ i-1 ];
	               if ( i < n-1 ) {
	                  work[ i-1][ i+1 ] = du2[ i-1 ];
	               }
	            } // else
	         } // for (i = 1; i <= n; i++)
	  
	  //     Multiply on the left by L.
	  
	         lastj = n;
	         for (i = n - 1; i >= 1; i--) {
	            li = dlf[ i-1 ];
	            for (k = 0; k < lastj-i+1; k++) {
	            	work[i][i-1+k] = work[i][i-1+k] + li*work[i-1][i-1+k];
	            }
	            ip = ipiv[ i-1 ];
	            if ( ip == i ) {
	               lastj = Math.min( i+2, n );
	            }
	            else {
	            	for (k = 0; k < lastj-i+1; k++) {
	            		temp = work[i-1][i-1+k];
	            		work[i-1][i-1+k] = work[i][i-1+k];
	            		work[i][i-1+k] = temp;
	            	}
	            }
	         } // for (i = n - 1; i >= 1; i--)
	  
	  //     Subtract the matrix A.
	 
	         work[ 0][ 0 ] = work[ 0][ 0 ] - d[ 0 ];
	         if ( n > 1 ) {
	            work[ 0][ 1 ] = work[ 0][ 1 ] - du[ 0 ];
	            work[ n-1][ n-2] = work[n-1][ n-2] - dl[ n-2];
	            work[ n-1][ n-1] = work[ n-1][n-1] - d[ n-1];
	            for (i = 1; i < n - 1; i++) {
	               work[ i][ i-1 ] = work[ i][ i-1 ] - dl[ i-1 ];
	               work[ i][ i ] = work[ i][ i ] - d[ i ];
	               work[ i][ i+1 ] = work[ i][ i+1 ] - du[ i ];
	            } // for (i = 1; i < n - 1; i++) 
	         } // if (n > 1)
	  
	  //     Compute the 1-norm of the tridiagonal matrix A.
	  
	         anorm = dlangt( '1', n, dl, d, du );
	  
	  //    Compute the 1-norm of WORK, which is only guaranteed to be
	  //     upper Hessenberg.
	  
	         resid[0] = ge.dlanhs( '1', n, work, ldwork, rwork );
	  
	  //     Compute norm(L*U - A) / (norm(A) * EPS)
	  
	         if ( anorm <= zero ) {
	            if ( resid[0] != zero ) {
	               resid[0] = one / eps;
	            }
	         }
	         else {
	            resid[0] = ( resid[0] / anorm ) / eps;
	         }
	  
	         return;
	      } // dgtt01
	  
       // \par Purpose:
	   //  =============
	   //
	   // \verbatim
	   
	   // DLANGT  returns the value of the one norm,  or the Frobenius norm, or
	   // the  infinity norm,  or the  element of  largest absolute value  of a
	   // real tridiagonal matrix A.
	   // \endverbatim
	   
	   // \return DLANGT
	   // \verbatim
	   
	   //    DLANGT = ( max(abs(A(i,j))), NORM = 'M' or 'm'
	   //             (
	   //             ( norm1(A),         NORM = '1', 'O' or 'o'
	   //             (
	   //             ( normI(A),         NORM = 'I' or 'i'
	   //             (
	   //             ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
	   
	   // where  norm1  denotes the  one norm of a matrix (maximum column sum),
	   // normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
	   // normF  denotes the  Frobenius norm of a matrix (square root of sum of
	   // squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
	   // \endverbatim
	  
	   //  Arguments:
	   //  ==========
	   
	   // \param[in] NORM
	   // \verbatim
	   //          NORM is CHARACTER*1
	   //          Specifies the value to be returned in DLANGT as described
	   //          above.
	   // \endverbatim
	   
	   // \param[in] N
	   // \verbatim
	   //          N is INTEGER
	   //          The order of the matrix A.  N >= 0.  When N = 0, DLANGT is
	   //          set to zero.
	   // \endverbatim
	   
	   // \param[in] DL
	   // \verbatim
	   //          DL is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) sub-diagonal elements of A.
	   // \endverbatim
	   
	   // \param[in] D
	   // \verbatim
	   //          D is DOUBLE PRECISION array, dimension (N)
	   //          The diagonal elements of A.
	   // \endverbatim
	   //
	   // \param[in] DU
	   // \verbatim
	   //          DU is DOUBLE PRECISION array, dimension (N-1)
	   //          The (n-1) super-diagonal elements of A.
	   // \endverbatim
	  
	   //  Authors:
	   //  ========
	   
	   // \author Univ. of Tennessee 
	   // \author Univ. of California Berkeley 
	   // \author Univ. of Colorado Denver 
	  // \author NAG Ltd. 
	  
	  // \date September 2012
	  
	  // \ingroup doubleOTHERauxiliary
	  
	  //  =====================================================================
	        private double dlangt(char norm, int n, double dl[], double d[], double du[] ) {
	  
	  //  -- LAPACK auxiliary routine (version 3.4.2) --
	  //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	  //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	  //     September 2012
	  
	  //     .. Scalar Arguments ..
	  //       CHARACTER          norm
	  //       INTEGER            n
	  //     ..
	  //     .. Array Arguments ..
	  //       DOUBLE PRECISION   d( * ), dl( * ), du( * )
	  //     ..
	  
	  //  =====================================================================
	  
	  //     .. Parameters ..
	         final double one = 1.0;
	         final double zero = 0.0;
	  
	  //     .. Local Scalars ..
	         int            i;
	         double scale[] = new double[1];
	         double sum[] = new double[1];
	         double anorm = 0.0;
	         double temp;
	 
	  //     .. External Functions ..
	  //       LOGICAL            lsame, disnan
	  //       EXTERNAL           lsame, disnan
	  //     ..
	  //     .. External Subroutines ..
	  //       EXTERNAL           dlassq
	  //     ..
	  //     .. Intrinsic Functions ..
	  //       INTRINSIC          abs, sqrt
	  //     ..
	  //     .. Executable Statements ..
	  //
	         if ( n <= 0 ) {
	            anorm = zero;
	         }
	         else if ((norm == 'M') || (norm == 'm')) {
	  
	  //        Find max(abs(A(i,j))).
	  
	            anorm = Math.abs( d[ n-1 ] );
	            for (i = 0; i <  n - 1; i++) {
	               if ( anorm < Math.abs( dl[ i ] ) || Double.isNaN( Math.abs( dl[ i ] ) ) ) {
	                   anorm = Math.abs(dl[i]);
	               }
	               if ( anorm < Math.abs( d[ i ] ) || Double.isNaN( Math.abs( d[ i ] ) ) ) {
	                    anorm = Math.abs(d[i]);
	               }
	               if ( anorm < Math.abs( du[ i ] ) || Double.isNaN(Math.abs( du[ i ] ) ) ) { 
	                    anorm = Math.abs(du[i]);
	               }
	            } // for (i = 0; i <  n - 1; i++) 
	         } // else if ((norm == 'M') || (norm == 'm'))
	         else if ((norm == 'O') || (norm == 'o') || (norm == '1' )) {
	  
	  //        Find norm1(A).
	  
	            if ( n == 1 ) {
	               anorm = Math.abs( d[ 0 ] );
	            }
	            else {
	               anorm = Math.abs( d[ 0 ] )+Math.abs( dl[ 0 ] );
	               temp = Math.abs( d[ n-1 ] )+Math.abs( du[ n-2 ] ); 
	               if ( anorm < temp || Double.isNaN( temp ) ) anorm = temp;
	               for (i = 1; i < n - 1; i++) {
	                  temp = Math.abs( d[ i ] )+Math.abs( dl[ i ] )+Math.abs( du[ i-1 ] );
	                  if ( anorm < temp || Double.isNaN( temp ) ) anorm = temp;
	               } // for (i = 1; i < n - 1; i++)
	            } // else
	         } // else if ((norm == 'O') || (norm == 'o') || (norm == '1' ))
	         else if ((norm == 'I') || (norm == 'i')) {
	 
	  //        Find normI(A).
	  
	            if ( n == 1 ) {
	               anorm = Math.abs( d[ 0 ] );
	            }
	            else {
	               anorm = Math.abs( d[ 0 ] )+Math.abs( du[ 0 ] );
	               temp = Math.abs( d[ n-1 ] )+Math.abs( dl[ n-2 ] );
	               if ( anorm < temp || Double.isNaN( temp ) ) anorm = temp;
	               for (i = 1; i < n - 1; i++) {
	                  temp = Math.abs( d[ i ] )+Math.abs( du[ i ] )+Math.abs( dl[ i-1 ] );
	                  if ( anorm < temp || Double.isNaN( temp ) ) anorm = temp;
	               } // for (i = 1; i < n - 1; i++)
	            } // else
	         } // else if ((norm == 'I') || (norm == 'i'))
	         else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e')) {
	  
	  //        Find normF(A).
	  
	            scale[0] = zero;
	            sum[0] = one;
	            ge.dlassq( n, d, 1, scale, sum );
	            if ( n >1 ) {
	               ge.dlassq( n-1, dl, 1, scale, sum );
	               ge.dlassq( n-1, du, 1, scale, sum );
	            }
	            anorm = scale[0]*Math.sqrt( sum[0] );
	         } // else if ((norm == 'F') || (norm == 'f') || (norm == 'E') || (norm == 'e'))
	         return anorm;
	        } // dlangt


    // \par Purpose:
    	//  =============
    	//
    	// \verbatim
    	//
    	// DGTTRF computes an LU factorization of a real tridiagonal matrix A
    	// using elimination with partial pivoting and row interchanges.
    	//
    	// The factorization has the form
    	//    A = L * U
    	// where L is a product of permutation and unit lower bidiagonal
    	// matrices and U is upper triangular with nonzeros in only the main
    	// diagonal and first two superdiagonals.
    	// \endverbatim
    	
    	//  Arguments:
    	//  ==========
    	//
    	// \param[in] N
    	// \verbatim
    	//          N is INTEGER
    	//          The order of the matrix A.
    	// \endverbatim
    	
    	// \param[in,out] DL
    	// \verbatim
    	//          DL is DOUBLE PRECISION array, dimension (N-1)
    	//          On entry, DL must contain the (n-1) sub-diagonal elements of
    	//          A.
    	
    	//          On exit, DL is overwritten by the (n-1) multipliers that
    	//          define the matrix L from the LU factorization of A.
    	// \endverbatim
    	
    	// \param[in,out] D
    	// \verbatim
    	//          D is DOUBLE PRECISION array, dimension (N)
    	//          On entry, D must contain the diagonal elements of A.
    	
    	//          On exit, D is overwritten by the n diagonal elements of the
    	//          upper triangular matrix U from the LU factorization of A.
    	// \endverbatim
    	
    	// \param[in,out] DU
    	// \verbatim
    	//          DU is DOUBLE PRECISION array, dimension (N-1)
    	//          On entry, DU must contain the (n-1) super-diagonal elements
    	//          of A.
    	
    	//          On exit, DU is overwritten by the (n-1) elements of the first
    	//          super-diagonal of U.
    	// \endverbatim
    	
    	// \param[out] DU2
    	// \verbatim
    	//          DU2 is DOUBLE PRECISION array, dimension (N-2)
    	//          On exit, DU2 is overwritten by the (n-2) elements of the
    	//          second super-diagonal of U.
    	// \endverbatim
    	
    	// \param[out] IPIV
    	// \verbatim
    	//          IPIV is INTEGER array, dimension (N)
    	//          The pivot indices; for 1 <= i <= n, row i of the matrix was
    	//          interchanged with row IPIV(i).  IPIV(i) will always be either
    	//          i or i+1; IPIV(i) = i indicates a row interchange was not
    	//          required.
    	// \endverbatim
    	
    	// \param[out] INFO
    	// \verbatim
    	//          INFO is INTEGER
    	//          = 0:  successful exit
    	//          < 0:  if INFO = -k, the k-th argument had an illegal value
    	//          > 0:  if INFO = k, U(k,k) is exactly zero. The factorization
    	//                has been completed, but the factor U is exactly
    	//                singular, and division by zero will occur if it is used
    	//                to solve a system of equations.
    	// \endverbatim
    	
    	//  Authors:
    	//  ========
    	
    	// \author Univ. of Tennessee 
    	// \author Univ. of California Berkeley 
    	// \author Univ. of Colorado Denver 
    	// \author NAG Ltd. 
    	
    	// \date September 2012
    	
    	// \ingroup doubleGTcomputational
            
    private void dgttrf(int N, double DL[], double D[], double DU[], double DU2[], int IPIV[], int INFO[] ) {
    
    //  -- LAPACK computational routine (version 3.4.2) --
    //  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    //  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    //     September 2012
    
    //     .. Scalar Arguments ..
    //      INTEGER            INFO, N
    //     ..
    //     .. Array Arguments ..
    //      INTEGER            IPIV( * )
    //      DOUBLE PRECISION   D( * ), DL( * ), DU( * ), DU2( * )
    //     ..
    
    //  =====================================================================
    
    //     .. Parameters ..
    	  final double ZERO = 0.0;
    	  
    //     ..
    //     .. Local Scalars ..
          int            I;
          double   FACT, TEMP;
    //     ..
    //     .. Intrinsic Functions ..
    //      INTRINSIC          ABS
    //     ..
    //     .. External Subroutines ..
    //      EXTERNAL           XERBLA
    //     ..
    //    .. Executable Statements ..
    
          INFO[0] = 0;
          if ( N < 0 ) {
             INFO[0] = -1;
             UI.setDataText("DGTTRF had N < 0 INFO[0] = -1\n");
             return;
          }
    
    //     Quick return if possible
    
          if ( N == 0 ) {
              return;
          }
    
    //     Initialize IPIV(i) = i and DU2(I) = 0
    
          for (I = 1; I <= N; I++) {
             IPIV[ I-1] = I;
          }
          for (I = 1; I <= N - 2; I++) {
             DU2[ I-1 ] = ZERO;
          }
    
          for (I = 1; I <= N - 2; I++) {
             if ( Math.abs( D[ I-1 ] ) >= Math.abs( DL[ I-1 ] ) ) {
    
    //           No row interchange required, eliminate DL(I)
    
                if ( D[ I-1 ] != ZERO ) {
                   FACT = DL[ I-1 ] / D[ I-1];
                   DL[ I-1 ] = FACT;
                   D[ I ] = D[ I ] - FACT*DU[ I-1 ];
                } //  if ( D[ I-1 ] != ZERO )
             } // if ( Math.abs( D[ I-1 ] ) >= Math.abs( DL[ I-1 ] ) )
             else {
    
    //           Interchange rows I and I+1, eliminate DL(I)
    
                FACT = D[ I-1 ] / DL[ I-1 ];
                D[ I-1 ] = DL[ I-1 ];
                DL[ I-1 ] = FACT;
                TEMP = DU[ I-1 ];
                DU[ I-1 ] = D[ I ];
                D[ I ] = TEMP - FACT*D[ I ];
                DU2[ I-1 ] = DU[ I];
                DU[ I ] = -FACT*DU[ I ];
                IPIV[ I -1] = I + 1;
             } // else
          } // for (I = 1; I <= N - 2; I++)
          if ( N > 1 ) {
             I = N - 1;
             if ( Math.abs( D[ I-1 ] ) >= Math.abs( DL[ I-1 ] ) ) {
                if ( D[ I-1 ] != ZERO ) {
                   FACT = DL[ I-1] / D[ I-1];
                   DL[ I-1 ] = FACT;
                   D[ I ] = D[ I ] - FACT*DU[ I-1 ];
                } // if ( D[ I-1 ] != ZERO )
             } // if ( Math.abs( D[ I-1 ] ) >= Math.abs( DL[ I-1 ] ) )
             else {
                FACT = D[ I-1 ] / DL[ I-1 ];
                D[ I-1 ] = DL[ I-1 ];
                DL[ I-1 ] = FACT;
                TEMP = DU[ I-1 ];
                DU[ I-1 ] = D[ I ];
                D[ I ] = TEMP - FACT*D[ I ];
                IPIV[ I-1 ] = I + 1;
             } // else
          } // if (N > 1)
    
    //     Check for a zero on the diagonal of U.
    
        for (I = 1; I <= N; I++) {
             if ( D[ I-1] == ZERO ) {
                INFO[0] = I;
                return;
             } // if ( D[ I-1] == ZERO )
        } // for (I = 1; I <= N; I++)
       return;
    } // dgttrf
    
    // \par Purpose:
    	//  =============
    	//
    	// \verbatim
    	//
    	// DGTTRS solves one of the systems of equations
    	//    A*X = B  or  A**T*X = B,
    	// with a tridiagonal matrix A using the LU factorization computed
    	// by DGTTRF.
    	// \endverbatim
    	//
    	//  Arguments:
    	//  ==========
    	
    	// \param[in] TRANS
    	// \verbatim
    	//          TRANS is CHARACTER*1
    	//          Specifies the form of the system of equations.
    	//          = 'N':  A * X = B  (No transpose)
    	//          = 'T':  A**T* X = B  (Transpose)
    	//          = 'C':  A**T* X = B  (Conjugate transpose = Transpose)
    	// \endverbatim
    	
    	// \param[in] N
    	// \verbatim
    	//          N is INTEGER
    	//          The order of the matrix A.
    	// \endverbatim
    	
    	// \param[in] NRHS
    	// \verbatim
    	//          NRHS is INTEGER
    	//          The number of right hand sides, i.e., the number of columns
    	//          of the matrix B.  NRHS >= 0.
    	// \endverbatim
    	
    	// \param[in] DL
    	// \verbatim
    	//          DL is DOUBLE PRECISION array, dimension (N-1)
    	//          The (n-1) multipliers that define the matrix L from the
    	//          LU factorization of A.
    	// \endverbatim
    	
    	// \param[in] D
    	// \verbatim
    	//          D is DOUBLE PRECISION array, dimension (N)
    	//          The n diagonal elements of the upper triangular matrix U from
    	//          the LU factorization of A.
    	// \endverbatim
    	//
    	// \param[in] DU
    	// \verbatim
    	//          DU is DOUBLE PRECISION array, dimension (N-1)
    	//          The (n-1) elements of the first super-diagonal of U.
    	// \endverbatim
    	
    	// \param[in] DU2
    	// \verbatim
    	//          DU2 is DOUBLE PRECISION array, dimension (N-2)
    	//          The (n-2) elements of the second super-diagonal of U.
    	// \endverbatim
    	
    	// \param[in] IPIV
    	// \verbatim
    	//          IPIV is INTEGER array, dimension (N)
    	//          The pivot indices; for 1 <= i <= n, row i of the matrix was
    	//          interchanged with row IPIV(i).  IPIV(i) will always be either
    	//          i or i+1; IPIV(i) = i indicates a row interchange was not
    	//          required.
    	// \endverbatim
    	
    	// \param[in,out] B
    	// \verbatim
    	//          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
    	//          On entry, the matrix of right hand side vectors B.
    	//          On exit, B is overwritten by the solution vectors X.
    	// \endverbatim
    	
    	// \param[in] LDB
    	// \verbatim
    	//          LDB is INTEGER
    	//          The leading dimension of the array B.  LDB >= max(1,N).
    	// \endverbatim
    	
    	// \param[out] INFO
    	// \verbatim
    	//          INFO is INTEGER
    	//          = 0:  successful exit
    	//          < 0:  if INFO = -i, the i-th argument had an illegal value
    	// \endverbatim
    	
    	//  Authors:
    	//  ========
    	
    	// \author Univ. of Tennessee 
    	// \author Univ. of California Berkeley 
    	// \author Univ. of Colorado Denver 
    	// \author NAG Ltd. 
    	
    	// \date September 2012
    	
    	// \ingroup doubleGTcomputational
    
	    //  =====================================================================
	    private void dgttrs(char TRANS, int N, int NRHS, double DL[], double D[], double DU[], 
	    		       double DU2[], int IPIV[], double B[][], int LDB, int INFO[] ) {
	
	//  -- LAPACK computational routine (version 3.4.2) --
	//  -- LAPACK is a software package provided by Univ. of Tennessee,    --
	//  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	//     September 2012
	
	//     .. Scalar Arguments ..
    //   CHARACTER          TRANS
	//    INTEGER            INFO, LDB, N, NRHS
	//     ..
	//     .. Array Arguments ..
	//    INTEGER            IPIV( * )
	//    DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * ), DU2( * )
	//     ..
	
	//  =====================================================================
	
	//     .. Local Scalars ..
	    boolean            NOTRAN;
	    int            ITRANS, J, JB, NB, I, K;
	    double array[][];
	//     ..
	//     .. External Functions ..
	//    INTEGER            ILAENV
	//    EXTERNAL           ILAENV
	//     ..
	//     .. External Subroutines ..
	//    EXTERNAL           DGTTS2, XERBLA
	//     ..
	//     .. Intrinsic Functions ..
	//    INTRINSIC          MAX, MIN
	//     ..
	//     .. Executable Statements ..
	
	    INFO[0] = 0;
	    NOTRAN = ( TRANS =='N' || TRANS == 'n' );
	    if (!NOTRAN &&  !( TRANS == 'T' || TRANS ==
	       't' ) && !( TRANS == 'C' || TRANS == 'c' ) ) {
	       INFO[0] = -1;
	    }
	    else if ( N < 0 ) {
	       INFO[0] = -2;
	    }
	    else if ( NRHS < 0 ) {
	       INFO[0] = -3;
	    }
	    else if ( LDB < Math.max( N, 1 ) ) {
	       INFO[0] = -10;
	    }
	    if ( INFO[0] != 0 ) {
	       UI.setDataText("In dgttrs INFO[0] = " + INFO[0] + "\n");
	       return;
	    }
	
	//
	    
	    if ( N == 0 || NRHS == 0 ) {
	       return;
	    }
	
	//     Decode TRANS
	
	    if ( NOTRAN ) {
	       ITRANS = 0;
	    }
	    else {
	       ITRANS = 1;
	    }
	
	//     Determine the number of right-hand sides to solve at a time.
	
	    if ( NRHS == 1 ) {
	       NB = 1;
	    }
	    else {
	    	char ch[] = new char[1];
	    	ch[0] = TRANS;
	    	String opts = new String(ch);
	       NB = Math.max( 1, ge.ilaenv( 1, "DGTTRS", opts, N, NRHS, -1, -1 ) );
	    }

	    if ( NB >= NRHS ) {
	       DGTTS2( ITRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB );
	    }
	    else {
	       for (J = 1; J <= NRHS; J+= NB) {
	          JB = Math.min( NRHS-J+1, NB );
	          array = new double[Math.max(1, N)][JB];
	          for (I = 0; I < Math.max(1, N); I++) {
	        	  for (K = 0; K < JB; K++) {
	        		  array[I][K] = B[I][J-1+K];
	        	  }
	          }
	          DGTTS2( ITRANS, N, JB, DL, D, DU, DU2, IPIV, array,
	                      Math.max(1, N) );
	          for (I = 0; I < Math.max(1, N); I++) {
	        	  for (K = 0; K < JB; K++) {
	        		  B[I][J-1+K] = array[I][K];
	        	  }
	          }
	       } 
	    }
	    return;
	    } // dgttrs
	
	    
    // \par Purpose:
    	//  =============
    	
    	// \verbatim
    	
    	// DGTTS2 solves one of the systems of equations
    	//    A*X = B  or  A**T*X = B,
    	// with a tridiagonal matrix A using the LU factorization computed
    	// by DGTTRF.
    	// \endverbatim
    	
    	//  Arguments:
    	//  ==========
    	
    	// \param[in] ITRANS
    	// \verbatim
    	//          ITRANS is INTEGER
    	//          Specifies the form of the system of equations.
    	//          = 0:  A * X = B  (No transpose)
    	//          = 1:  A**T* X = B  (Transpose)
    	//          = 2:  A**T* X = B  (Conjugate transpose = Transpose)
    	// \endverbatim
    	
    	// \param[in] N
    	// \verbatim
    	//          N is INTEGER
    	//          The order of the matrix A.
    	// \endverbatim
    	//
    	// \param[in] NRHS
    	// \verbatim
    	//          NRHS is INTEGER
    	//          The number of right hand sides, i.e., the number of columns
    	//          of the matrix B.  NRHS >= 0.
    	// \endverbatim
    	
    	// \param[in] DL
    	// \verbatim
    	//          DL is DOUBLE PRECISION array, dimension (N-1)
    	//          The (n-1) multipliers that define the matrix L from the
    	//          LU factorization of A.
    	// \endverbatim
    	
    	// \param[in] D
    	// \verbatim
    	//          D is DOUBLE PRECISION array, dimension (N)
    	//          The n diagonal elements of the upper triangular matrix U from
    	//          the LU factorization of A.
    	// \endverbatim
    	
    	// \param[in] DU
    	// \verbatim
    	//          DU is DOUBLE PRECISION array, dimension (N-1)
    	//          The (n-1) elements of the first super-diagonal of U.
    	// \endverbatim
    	
    	// \param[in] DU2
    	// \verbatim
    	//          DU2 is DOUBLE PRECISION array, dimension (N-2)
    	//          The (n-2) elements of the second super-diagonal of U.
    	// \endverbatim
    	
    	// \param[in] IPIV
    	// \verbatim
    	//          IPIV is INTEGER array, dimension (N)
    	//          The pivot indices; for 1 <= i <= n, row i of the matrix was
    	//          interchanged with row IPIV(i).  IPIV(i) will always be either
    	//          i or i+1; IPIV(i) = i indicates a row interchange was not
    	//          required.
    	// \endverbatim
    	
    	// \param[in,out] B
    	// \verbatim
    	//          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
    	//          On entry, the matrix of right hand side vectors B.
    	//          On exit, B is overwritten by the solution vectors X.
    	// \endverbatim
    	
    	// \param[in] LDB
    	// \verbatim
    	//          LDB is INTEGER
    	//          The leading dimension of the array B.  LDB >= max(1,N).
    	// \endverbatim
    	
    	//  Authors:
    	//  ========
    	
    	// \author Univ. of Tennessee 
    	// \author Univ. of California Berkeley 
    	// \author Univ. of Colorado Denver 
    	// \author NAG Ltd. 
    	
    	// \date September 2012
    	
    	// \ingroup doubleGTcomputational
    	
    	//  =====================================================================
    	      private void DGTTS2( int ITRANS, int N, int NRHS, double DL[], double D[], 
    	    		               double DU[], double DU2[], int IPIV[], double B[][], int LDB ) {
    	
    	//  -- LAPACK computational routine (version 3.4.2) --
    	//  -- LAPACK is a software package provided by Univ. of Tennessee,    --
    	//  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
    	//     September 2012
    	
    	//     .. Scalar Arguments ..
    	//      INTEGER            ITRANS, LDB, N, NRHS
    	//     ..
    	//     .. Array Arguments ..
    	//      INTEGER            IPIV( * )
    	//      DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * ), DU2( * )
    	//     ..
    	
    	//  =====================================================================
    	
    	//     .. Local Scalars ..
    	      int            I, IP, J;
    	      double   TEMP;
    	//     ..
    	//     .. Executable Statements ..
    	
    	//     Quick return if possible
    	
    	      if ( N == 0 || NRHS == 0 ) {
    	          return;
    	      }
    	
    	      if ( ITRANS == 0 ) {
    	
    	//        Solve A*X = B using the LU factorization of A,
    	//        overwriting each right hand side vector with its solution.
    	
    	         if ( NRHS <= 1 ) {
    	            J = 1;
    	            while (true) {
    	
    	//           Solve L*x = b.
    	
    	            for (I = 1; I <= N - 1; I++) {
    	               IP = IPIV[ I-1 ];
    	               TEMP = B[ I-IP+I][ J-1] - DL[ I-1]*B[ IP-1][ J-1 ];
    	               B[ I-1][ J-1 ] = B[ IP-1][ J-1];
    	               B[ I][ J-1 ] = TEMP;
    	            } // for (I = 1; I <= N - 1; I++)
    	
    	//           Solve U*x = b.
    	
    	            B[ N-1][ J-1 ] = B[ N-1][ J-1 ] / D[ N-1 ];
    	            if ( N > 1 ) {
    	              B[ N-2][ J-1] = ( B[ N-2][ J-1 ]-DU[ N-2 ]*B[ N-1][ J-1 ] ) /
    	                            D[ N-2 ];
    	            } // if (N > 1)
    	            for (I = N - 2; I >= 1; I--) {
    	               B[ I-1][ J-1 ] = ( B[ I-1][ J-1 ]-DU[ I-1 ]*B[ I][ J -1]-DU2[ I-1 ]*
    	                          B[ I+1][ J-1] ) / D[ I-1 ];
    	            } // for (I = N - 2; I >= 1; I--)
    	            if ( J < NRHS ) {
    	               J = J + 1;
    	            }
    	            else {
    	               return;
    	            }
    	            } // while (true)
    	         } // if ( NRHS <= 1 )
    	         else { // NRHS > 1
    	            for (J = 1; J <= NRHS; J++) {
    	
    	//              Solve L*x = b.
    	
    	               for (I = 1; I <= N - 1; I++) {
    	                  if ( IPIV[ I-1 ] == I ) {
    	                     B[ I][ J-1 ] = B[ I][ J-1] - DL[ I-1 ]*B[ I-1][ J-1 ];
    	                  }
    	                  else {
    	                     TEMP = B[ I-1][ J-1 ];
    	                     B[ I-1][ J-1 ] = B[ I][ J-1 ];
    	                     B[ I][ J-1] = TEMP - DL[ I-1 ]*B[ I-1][ J-1];
    	                  }
    	               } // for (I = 1; I <= N - 1; I++)
    	
    	//              Solve U*x = b.
    	
    	               B[ N-1][ J-1] = B[ N-1][ J-1 ] / D[ N-1 ];
    	               if ( N > 1 ) {
    	                 B[ N-2][ J-1 ] = ( B[ N-2][ J-1 ]-DU[ N-2 ]*B[ N-1][ J-1] ) /
    	                               D[ N-2 ];
    	               } // if (N > 1)
    	               for (I = N - 2; I >= 1; I--) {
    	                  B[ I-1][ J-1 ] = ( B[ I-1][ J-1 ]-DU[ I-1 ]*B[ I][ J-1 ]-DU2[ I-1 ]*
    	                            B[ I+1][ J-1 ] ) / D[ I-1];
    	               } // for (I = N - 2; I >= 1; I--)
    	            } // for (J = 1; J <= NRHS; J++)
    	         } // else NRHS > 1
    	      } // if ( ITRANS == 0 )
    	      else { // ITRANS != 0
    	
    	//        Solve A**T * X = B.
    	
    	         if ( NRHS <= 1 ) {
    	
    	//           Solve U**T*x = b.
    	
    	            J = 1;
    	            while (true) {
    	            B[ 0][ J-1] = B[0][ J-1] / D[ 0 ];
    	            if ( N > 1 ) {
    	               B[ 1][ J-1 ] = ( B[ 1][ J -1]-DU[ 0 ]*B[ 0][ J-1 ] ) / D[ 1 ];
    	            } // if (N > 1)
    	            for (I = 3; I <= N; I++) {
    	               B[ I-1][ J-1 ] = ( B[ I-1][ J-1 ]-DU[ I-2 ]*B[ I-2][ J-1 ]-DU2[ I-3 ]*
    	                           B[ I-3][ J-1 ] ) / D[ I-1 ];
    	            } // for (I = 3; I <= N; I++) 
    	
    	//           Solve L**T*x = b.
    	
    	            for (I = N - 1; I >= 1; I--) {
    	               IP = IPIV[ I-1];
    	               TEMP = B[ I-1][ J-1] - DL[ I-1 ]*B[I][ J-1];
    	               B[ I-1][ J-1 ] = B[ IP-1][ J-1 ];
    	               B[ IP-1][ J -1] = TEMP;
    	            } // for (I = N - 1; I >= 1; I--)
    	            if ( J < NRHS ) {
    	               J = J + 1;
    	            }
    	            else {
    	               return;
    	            }
    	            } // while (true)
    	         } // if (NRHS <= 1)
    	         else { // NRHS > 1
    	            for (J = 1; J <= NRHS; J++) {
    	
    	//              Solve U**T*x = b.
    	
    	               B[ 0][ J-1 ] = B[0][ J-1 ] / D[ 0 ];
    	               if ( N > 1 ) {
    	                  B[1][ J-1 ] = ( B[ 1][ J -1]-DU[ 0 ]*B[ 0][ J-1 ] ) / D[ 1 ];
    	               } // if (N > 1)
    	               for (I = 3; I <= N; I++) {
    	                  B[ I-1][ J-1 ] = ( B[ I-1][ J-1 ]-DU[ I-2 ]*B[ I-2][ J-1 ]-
    	                             DU2[ I-3 ]*B[ I-3][ J -1] ) / D[ I-1 ];
    	               } // for (I = 3; I <= N; I++)
    	                  for (I = N - 1; I >=  1; I--) {
    	                  if ( IPIV[ I-1 ] == I ) {
    	                     B[ I-1][ J -1] = B[ I-1][ J-1 ] - DL[ I-1 ]*B[ I][ J-1 ];
    	                  }
    	                  else {
    	                     TEMP = B[ I][ J -1];
    	                     B[ I][ J-1 ] = B[ I-1][ J-1 ] - DL[ I-1 ]*TEMP;
    	                     B[ I-1][ J -1] = TEMP;
    	                  }
    	                  } // for (I = N - 1; I >=  1; I--)
    	            } // for (J = 1; J <= NRHS; J++)
    	         } // else NRHS > 1
    	      } // else ITRANS != 0
    	      return;
    	      } // DGTTS2
    
    // \BeginDoc
    
    // \Name: dseupd
    
    // \Description: 
    
    //  This subroutine returns the converged approximations to eigenvalues
    //  of A*z = lambda*B*z and (optionally):
    
    //      (1) the corresponding approximate eigenvectors,
    
    //      (2) an orthonormal (Lanczos) basis for the associated approximate
    //          invariant subspace,
    
    //      (3) Both.
    
    //  There is negligible additional cost to obtain eigenvectors.  An orthonormal
    //  (Lanczos) basis is always computed.  There is an additional storage cost 
    //  of n*nev if both are requested (in this case a separate array Z must be 
    //  supplied).
    
    //  These quantities are obtained from the Lanczos factorization computed
    //  by DSAUPD for the linear operator OP prescribed by the MODE selection
    //  (see IPARAM(7) in DSAUPD documentation.)  DSAUPD must be called before
    //  this routine is called. These approximate eigenvalues and vectors are 
    //  commonly called Ritz values and Ritz vectors respectively.  They are 
    //  referred to as such in the comments that follow.   The computed orthonormal 
    //  basis for the invariant subspace corresponding to these Ritz values is 
    //  referred to as a Lanczos basis.
    
    //  See documentation in the header of the subroutine DSAUPD for a definition 
    //  of OP as well as other terms and the relation of computed Ritz values 
    //  and vectors of OP with respect to the given problem  A*z = lambda*B*z.  
    
    //  The approximate eigenvalues of the original problem are returned in
    //  ascending algebraic order.  The user may elect to call this routine
    //  once for each desired Ritz vector and store it peripherally if desired.
    //  There is also the option of computing a selected set of these vectors
    //  with a single call.
    
    // \Usage:
    //  call dseupd 
    //     ( RVEC, HOWMNY, SELECT, D, Z, LDZ, SIGMA, BMAT, N, WHICH, NEV, TOL,
    //       RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, WORKL, LWORKL, INFO )
    
    //  RVEC    LOGICAL  (INPUT) 
    //          Specifies whether Ritz vectors corresponding to the Ritz value 
    //          approximations to the eigenproblem A*z = lambda*B*z are computed.
    
    //             RVEC = .FALSE.     Compute Ritz values only.
    
    //             RVEC = .TRUE.      Compute Ritz vectors.
    
    //  HOWMNY  Character*1  (INPUT) 
    //          Specifies how many Ritz vectors are wanted and the form of Z
    //          the matrix of Ritz vectors. See remark 1 below.
    //          = 'A': compute NEV Ritz vectors;
    //          = 'S': compute some of the Ritz vectors, specified
    //                 by the logical array SELECT.
    
    //  SELECT  Logical array of dimension NEV.  (INPUT)
    //          If HOWMNY = 'S', SELECT specifies the Ritz vectors to be
    //          computed. To select the Ritz vector corresponding to a
    //          Ritz value D(j), SELECT(j) must be set to .TRUE.. 
    //          If HOWMNY = 'A' , SELECT is not referenced.
    
    //  D       Double precision array of dimension NEV.  (OUTPUT)
    //          On exit, D contains the Ritz value approximations to the
    //          eigenvalues of A*z = lambda*B*z. The values are returned
    //          in ascending order. If IPARAM(7) = 3,4,5 then D represents
    //          the Ritz values of OP computed by dsaupd transformed to
    //          those of the original eigensystem A*z = lambda*B*z. If 
    //          IPARAM(7) = 1,2 then the Ritz values of OP are the same 
    //          as the those of A*z = lambda*B*z.
    
    //  Z       Double precision N by NEV array if HOWMNY = 'A'.  (OUTPUT)
    //          On exit, Z contains the B-orthonormal Ritz vectors of the
    //          eigensystem A*z = lambda*B*z corresponding to the Ritz
    //          value approximations.
    //          If  RVEC = .FALSE. then Z is not referenced.
    //          NOTE: The array Z may be set equal to first NEV columns of the 
    //          Arnoldi/Lanczos basis array V computed by DSAUPD.
    
    //  LDZ     Integer.  (INPUT)
    //          The leading dimension of the array Z.  If Ritz vectors are
    //          desired, then  LDZ .ge.  max( 1, N ).  In any case,  LDZ .ge. 1.
    
    //  SIGMA   Double precision  (INPUT)
    //          If IPARAM(7) = 3,4,5 represents the shift. Not referenced if
    //          IPARAM(7) = 1 or 2.
    
    
    //  **** The remaining arguments MUST be the same as for the   ****
    //  **** call to DNAUPD that was just completed.               ****
    
    //  NOTE: The remaining arguments
    
    //           BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR,
    //           WORKD, WORKL, LWORKL, INFO
    
    //         must be passed directly to DSEUPD following the last call
    //         to DSAUPD.  These arguments MUST NOT BE MODIFIED between
    //         the the last call to DSAUPD and the call to DSEUPD.
    
    //  Two of these parameters (WORKL, INFO) are also output parameters:
    
    //  WORKL   Double precision work array of length LWORKL.  (OUTPUT/WORKSPACE)
    //          WORKL(1:4*ncv) contains information obtained in
    //          dsaupd.  They are not changed by dseupd.
    //          WORKL(4*ncv+1:ncv*ncv+8*ncv) holds the
    //          untransformed Ritz values, the computed error estimates,
    //          and the associated eigenvector matrix of H.
    
    //          Note: IPNTR(8:10) contains the pointer into WORKL for addresses
    //          of the above information computed by dseupd.
    //          -------------------------------------------------------------
    //          IPNTR(8): pointer to the NCV RITZ values of the original system.
    //          IPNTR(9): pointer to the NCV corresponding error bounds.
    //          IPNTR(10): pointer to the NCV by NCV matrix of eigenvectors
    //                     of the tridiagonal matrix T. Only referenced by
    //                     dseupd if RVEC = .TRUE. See Remarks.
    //          -------------------------------------------------------------
    
    //  INFO    Integer.  (OUTPUT)
    //          Error flag on output.
    //          =  0: Normal exit.
    //          = -1: N must be positive.
    //          = -2: NEV must be positive.
    //          = -3: NCV must be greater than NEV and less than or equal to N.
    //          = -5: WHICH must be one of 'LM', 'SM', 'LA', 'SA' or 'BE'.
    //          = -6: BMAT must be one of 'I' or 'G'.
    //          = -7: Length of private work WORKL array is not sufficient.
    //          = -8: Error return from trid. eigenvalue calculation;
    //                Information error from LAPACK routine dsteqr.
    //          = -9: Starting vector is zero.
    //          = -10: IPARAM(7) must be 1,2,3,4,5.
    //          = -11: IPARAM(7) = 1 and BMAT = 'G' are incompatible.
    //          = -12: NEV and WHICH = 'BE' are incompatible.
    //          = -14: DSAUPD did not find any eigenvalues to sufficient
    //                 accuracy.
    //          = -15: HOWMNY must be one of 'A' or 'S' if RVEC = .true.
    //          = -16: HOWMNY = 'S' not yet implemented
    
    // \BeginLib
    
    // \References:
    //  1. D.C. Sorensen, "Implicit Application of Polynomial Filters in
    //     a k-Step Arnoldi Method", SIAM J. Matr. Anal. Apps., 13 (1992),
    //     pp 357-385.
    //  2. R.B. Lehoucq, "Analysis and Implementation of an Implicitly 
    //     Restarted Arnoldi Iteration", Rice University Technical Report
    //     TR95-13, Department of Computational and Applied Mathematics.
    //  3. B.N. Parlett, "The Symmetric Eigenvalue Problem". Prentice-Hall,
    //     1980.
    //  4. B.N. Parlett, B. Nour-Omid, "Towards a Black Box Lanczos Program",
    //     Computer Physics Communications, 53 (1989), pp 169-179.
    //  5. B. Nour-Omid, B.N. Parlett, T. Ericson, P.S. Jensen, "How to
    //     Implement the Spectral Transformation", Math. Comp., 48 (1987),
    //     pp 663-673.
    //  6. R.G. Grimes, J.G. Lewis and H.D. Simon, "A Shifted Block Lanczos 
    //     Algorithm for Solving Sparse Symmetric Generalized Eigenproblems", 
    //     SIAM J. Matr. Anal. Apps.,  January (1993).
    //  7. L. Reichel, W.B. Gragg, "Algorithm 686: FORTRAN Subroutines
    //     for Updating the QR decomposition", ACM TOMS, December 1990,
    //     Volume 16 Number 4, pp 369-377.
    
    // \Remarks
    //  1. The converged Ritz values are always returned in increasing 
    //     (algebraic) order.
    
    //  2. Currently only HOWMNY = 'A' is implemented. It is included at this
    //     stage for the user who wants to incorporate it. 
    
    // \Routines called:
    //     dsesrt  ARPACK routine that sorts an array X, and applies the
    //             corresponding permutation to a matrix A.
    //     dsortr  dsortr  ARPACK sorting routine.
    //     ivout   ARPACK utility routine that prints integers.
    //     dvout   ARPACK utility routine that prints vectors.
    //     dgeqr2  LAPACK routine that computes the QR factorization of
    //             a matrix.
    //     dlacpy  LAPACK matrix copy routine.
    //     dlamch  LAPACK routine that determines machine constants.
    //     dorm2r  LAPACK routine that applies an orthogonal matrix in
    //             factored form.
    //     dsteqr  LAPACK routine that computes eigenvalues and eigenvectors
    //             of a tridiagonal matrix.
    //     dger    Level 2 BLAS rank one update to a matrix.
    //     dcopy   Level 1 BLAS that copies one vector to another .
    //     dnrm2   Level 1 BLAS that computes the norm of a vector.
    //     dscal   Level 1 BLAS that scales a vector.
    //     dswap   Level 1 BLAS that swaps the contents of two vectors.

    // \Authors
    //     Danny Sorensen               Phuong Vu
    //     Richard Lehoucq              CRPC / Rice University
    //     Chao Yang                    Houston, Texas
    //     Dept. of Computational & 
    //     Applied Mathematics
    //     Rice University           
    //     Houston, Texas            
     
    // \Revision history:
    //     12/15/93: Version ' 2.1'
    
    // \SCCS Information: @(#) 
    // FILE: seupd.F   SID: 2.7   DATE OF SID: 8/27/96   RELEASE: 2
    
    // \EndLib
    
    // -----------------------------------------------------------------------
          public void dseupd (boolean rvec, String howmny, boolean select[], double d[], 
        		               double z[][], int ldz, double sigma, String bmat,
                               int n, String which, int nev, double tol, double resid[],
                               int ncv,double v[][], int ldv, int iparam[], 
                               int ipntr[], double workd[], double workl[], int lworkl, int info[] ) {
    
    //     %----------------------------------------------------%
    //     | Include files for debugging and timing information |
    //     %----------------------------------------------------%
    
    //      include   'debug.h'
    //      include   'stat.h'
    
    //     %------------------%
    //     | Scalar Arguments |
    //     %------------------%
    
    //      character  bmat, howmny, which*2
    //      logical    rvec, select(ncv)
    //      integer    info, ldz, ldv, lworkl, n, ncv, nev
    //      Double precision     
    //    &           sigma, tol
    
    //     %-----------------%
    //     | Array Arguments |
    //     %-----------------%
    
    //      integer    iparam(7), ipntr(11)
    //      Double precision
    //     &           d(nev), resid(n), v(ldv,ncv), z(ldz, nev), 
    //     &           workd(2*n), workl(lworkl)
    
    //     %------------%
    //     | Parameters |
    //     %------------%
    
           final double zero = 0.0;
           final double one = 1.0;
    
    //     %---------------%
    //    | Local Scalars |
    //     %---------------%
    
          String type = null;
          int ierr[] = new int[1];
          int    bounds, ih, ihb, ihd, iq, iw, j, k, 
                    ldh, ldq, mode, msglvl, nconv, next, ritz,
                    irz, ibd, ktrord, leftptr, rghtptr, ism, ilg;
          double thres1 = 0.0;
          double thres2 = 0.0;
          double bnorm2 = 0.0;
          double rnorm, temp, tempbnd, eps23;
          boolean    reord;
          double v1[];
          double v2[];
          double array[][];
          int index;
          double work[];
          int i;
          double array2[][];
          double dum[] = new double[1];
    
    //     %----------------------%
    //     | External Subroutines |
    //     %----------------------%
    
    //      external   dcopy, dger, dgeqr2, dlacpy, dorm2r, dscal, 
    //     &           dsesrt, dsteqr, dswap, dvout, ivout, dsortr
    
    //     %--------------------%
    //     | External Functions |
    //     %--------------------%
    
    //      Double precision
    //     &           dnrm2, dlamch
    //      external   dnrm2, dlamch
    
    //     %---------------------%
    //     | Intrinsic Functions |
    //     %---------------------%
    
    //      intrinsic    min
    
    //     %-----------------------%
    //     | Executable Statements |
    //     %-----------------------%
     
    //     %------------------------%
    //     | Set default parameters |
    //     %------------------------%
    
          msglvl = mseupd;
          mode = iparam[6];
          nconv = iparam[4];
          info[0] = 0;
    
    //     %--------------%
    //     | Quick return |
    //     %--------------%
    
          if (nconv == 0) {
        	  return;
          }
          ierr[0] = 0;
    
          if (nconv <= 0)                        ierr[0] = -14; 
          if (n <= 0)                            ierr[0] = -1;
          if (nev <= 0)                          ierr[0] = -2;
          if (ncv <= nev ||  ncv > n)       ierr[0] = -3;
          if (!which.equalsIgnoreCase("LM") &&
             !which.equalsIgnoreCase("SM") &&
             !which.equalsIgnoreCase("LA") &&
             !which.equalsIgnoreCase("SA") &&
             !which.equalsIgnoreCase("BE"))                     ierr[0] = -5;
          if (!bmat.equalsIgnoreCase("I") && !bmat.equalsIgnoreCase("G"))   ierr[0] = -6;                               
          if ( (!howmny.equalsIgnoreCase("A") &&
                !howmny.equalsIgnoreCase("P") &&
                !howmny.equalsIgnoreCase("S")) && rvec ) 
                                                  ierr[0] = -15;
          if (rvec && howmny.equalsIgnoreCase("S"))           ierr[0] = -16;
    
          if (rvec && lworkl < ncv*ncv+8*ncv) ierr[0] = -7;
         
          if (mode == 1 || mode == 2) {
             type = "REGULR";
          }
          else if (mode == 3 ) {
             type = "SHIFTI";
          }
          else if (mode == 4 ) {
             type = "BUCKLE";
          }
          else if (mode == 5 ) {
             type = "CAYLEY";
          }
          else  {
        	  ierr[0] = -10;  
          }
                                                   
          if (mode == 1 && bmat.equalsIgnoreCase("G"))     ierr[0] = -11;
          if (nev == 1 && which.equalsIgnoreCase("BE"))    ierr[0] = -12;
    
    //     %------------%
    //     | Error Exit |
    //     %------------%
    
          if (ierr[0] != 0) {
             info[0] = ierr[0];
             return;
          }
         
    //     %-------------------------------------------------------%
    //     | Pointer into WORKL for address of H, RITZ, BOUNDS, Q  |
    //     | etc... and the remaining workspace.                   |
    //     | Also update pointer to be used on output.             |
    //     | Memory is laid out as follows:                        |
    //     | workl(1:2*ncv) := generated tridiagonal matrix H      |
    //     |       The subdiagonal is stored in workl(2:ncv).      |
    //     |       The dead spot is workl(1) but upon exiting      |
    //     |       dsaupd stores the B-norm of the last residual   |
    //     |       vector in workl(1). We use this !!!             |
    //     | workl(2*ncv+1:2*ncv+ncv) := ritz values               |
    //     |       The wanted values are in the first NCONV spots. |
    //     | workl(3*ncv+1:3*ncv+ncv) := computed Ritz estimates   |
    //     |       The wanted values are in the first NCONV spots. |
    //     | NOTE: workl(1:4*ncv) is set by dsaupd and is not      |
    //     |       modified by dseupd.                             |
    //     %-------------------------------------------------------%
    
    //     %-------------------------------------------------------%
    //     | The following is used and set by dseupd.              |
    //     | workl(4*ncv+1:4*ncv+ncv) := used as workspace during  |
    //     |       computation of the eigenvectors of H. Stores    |
    //     |       the diagonal of H. Upon EXIT contains the NCV   |
    //     |       Ritz values of the original system. The first   |
    //     |       NCONV spots have the wanted values. If MODE =   |
    //     |       1 or 2 then will equal workl(2*ncv+1:3*ncv).    |
    //     | workl(5*ncv+1:5*ncv+ncv) := used as workspace during  |
    //     |       computation of the eigenvectors of H. Stores    |
    //     |       the subdiagonal of H. Upon EXIT contains the    |
    //     |       NCV corresponding Ritz estimates of the         |
    //     |       original system. The first NCONV spots have the |
    //     |       wanted values. If MODE = 1,2 then will equal    |
    //     |       workl(3*ncv+1:4*ncv).                           |
    //     | workl(6*ncv+1:6*ncv+ncv*ncv) := orthogonal Q that is  |
    //     |       the eigenvector matrix for H as returned by     |
    //     |       dsteqr. Not referenced if RVEC = .False.        |
    //     |       Ordering follows that of workl(4*ncv+1:5*ncv)   |
    //     | workl(6*ncv+ncv*ncv+1:6*ncv+ncv*ncv+2*ncv) :=         |
    //     |       Workspace. Needed by dsteqr and by dseupd.      |
    //     | GRAND total of NCV*(NCV+8) locations.                 |
    //     %-------------------------------------------------------%
    
    
          ih     = ipntr[4];
          ritz   = ipntr[5];
          bounds = ipntr[6];
          ldh    = ncv;
          ldq    = ncv;
          ihd    = bounds + ldh;
          ihb    = ihd    + ldh;
          iq     = ihb    + ldh;
          iw     = iq     + ldh*ncv;
          next   = iw     + 2*ncv;
          ipntr[3]  = next;
          ipntr[7]  = ihd;
          ipntr[8]  = ihb;
          ipntr[9] = iq;
    
    //     %----------------------------------------%
    //     | irz points to the Ritz values computed |
    //     |     by _seigt before exiting _saup2.   |
    //     | ibd points to the Ritz estimates       |
    //     |     computed by _seigt before exiting  |
    //     |     _saup2.                            |
    //     %----------------------------------------%
    
          irz = ipntr[10]+ncv;
          ibd = irz+ncv;
    
    
    //     %---------------------------------%
    //     | Set machine dependent constant. |
    //     %---------------------------------%
    
          eps23 = ge.dlamch('E'); 
          eps23 = Math.pow(eps23,(2.0 / 3.0));
    
    //     %---------------------------------------%
    //     | RNORM is B-norm of the RESID(1:N).    |
    //     | BNORM2 is the 2 norm of B*RESID(1:N). |
    //     | Upon exit of dsaupd WORKD(1:N) has    |
    //     | B*RESID(1:N).                         |
    //     %---------------------------------------%
    
          rnorm = workl[ih-1];
          if (bmat.equalsIgnoreCase("I")) { 
             bnorm2 = rnorm;
          }
          else if (bmat.equalsIgnoreCase("G")) {
             bnorm2 = ge.dnrm2(n, workd, 1);
          }
    
          if (rvec) {
    
    //        %------------------------------------------------%
    //        | Get the converged Ritz value on the boundary.  |
    //        | This value will be used to dermine whether we  |
    //        | need to reorder the eigenvalues and            |
    //        | eigenvectors comupted by _steqr, and is        |
    //        | referred to as the "threshold" value.          |
    //        |                                                |
    //        | A Ritz value gamma is said to be a wanted      |
    //        | one, if                                        |
    //        | abs(gamma) .ge. threshold, when WHICH = 'LM';  |
    //        | abs(gamma) .le. threshold, when WHICH = 'SM';  |
    //        | gamma      .ge. threshold, when WHICH = 'LA';  |
    //        | gamma      .le. threshold, when WHICH = 'SA';  |
    //        | gamma .le. thres1 .or. gamma .ge. thres2       |
    //        |                            when WHICH = 'BE';  |
    //        |                                                |
    //        | Note: converged Ritz values and associated     |
    //        | Ritz estimates have been placed in the first   |
    //        | NCONV locations in workl(ritz) and             |
    //        | workl(bounds) respectively. They have been     |
    //        | sorted (in _saup2) according to the WHICH      |
    //        | selection criterion. (Except in the case       |
    //        | WHICH = 'BE', they are sorted in an increasing |
    //        | order.)                                        |
    //        %------------------------------------------------%
    
             if (which.equalsIgnoreCase("LM") || which.equalsIgnoreCase("SM")
                || which.equalsIgnoreCase("LA") || which.equalsIgnoreCase("SA")) {
    
                 thres1 = workl[ritz-1];
    
                 if (msglvl > 2) {
                	UI.setDataText("dseupd: Threshold eigenvalue used for re-ordering thres1 = " + nf.format(thres1) + "\n");
                 } // if (msglvl > 2)
             } // if (which.equalsIgnoreCase("LM") || which.equalsIgnoreCase("SM")
             else if (which.equalsIgnoreCase("BE")) {
    
    //            %------------------------------------------------%
    //            | Ritz values returned from _saup2 have been     |
    //            | sorted in increasing order.  Thus two          |
    //            | "threshold" values (one for the small end, one |
    //            | for the large end) are in the middle.          |
    //            %------------------------------------------------%
    
            	 ism = Math.max(nev,nconv) / 2;
                 ilg = ism + 1;
                 thres1 = workl[ism-1];
                 thres2 = workl[ilg-1];
    
                 if (msglvl > 2) {
                	UI.setDataText("dseupd: Threshold eigenvalues used for re-ordering: \n");
                	UI.setDataText("thres1 = " + nf.format(thres1) + "\n");
                	UI.setDataText("thres2 = " + nf.format(thres2) + "\n");
                 } // if (msglvl > 2)
             } // else if (which.equalsIgnoreCase("BE"))
    
    //        %----------------------------------------------------------%
    //        | Check to see if all converged Ritz values appear within  |
    //        | the first NCONV diagonal elements returned from _seigt.  |
    //        | This is done in the following way:                       |
    //        |                                                          |
    //        | 1) For each Ritz value obtained from _seigt, compare it  |
    //        |    with the threshold Ritz value computed above to       |
    //        |    determine whether it is a wanted one.                 |
    //        |                                                          |
    //        | 2) If it is wanted, then check the corresponding Ritz    |
    //        |    estimate to see if it has converged.  If it has, set  |
    //        |    correponding entry in the logical array SELECT to     |
    //        |    .TRUE..                                               |
    //        |                                                          |
    //        | If SELECT(j) = .TRUE. and j > NCONV, then there is a     |
    //        | converged Ritz value that does not appear at the top of  |
    //        | the diagonal matrix computed by _seigt in _saup2.        |
    //        | Reordering is needed.                                    |
    //        %----------------------------------------------------------%
    
             reord = false;
             ktrord = 0;
             for (j = 0; j <= ncv-1; j++) {
                select[j] = false;
                if (which.equalsIgnoreCase("LM")) {
                   if (Math.abs(workl[irz+j-1]) >= Math.abs(thres1)) {
                       tempbnd = Math.max( eps23, Math.abs(workl[irz+j-1]) );
                       if (workl[ibd+j-1] <= tol*tempbnd) {
                          select[j] = true;
                       }
                   } // if (Math.abs(workl[irz+j-1]) >= Math.abs(thres1))
                } // if (which.equalsIgnoreCase("LM"))
                else if (which.equalsIgnoreCase("SM")) {
                   if (Math.abs(workl[irz+j-1]) <= Math.abs(thres1)) {
                       tempbnd = Math.max( eps23, Math.abs(workl[irz+j-1]) );
                       if (workl[ibd+j-1] <= tol*tempbnd) {
                          select[j] = true;
                       }
                   } // if (Math.abs(workl[irz+j-1]) <= Math.abs(thres1))
                } // else if (which.equalsIgnoreCase("SM"))
                else if (which.equalsIgnoreCase("LA")) {
                   if (workl[irz+j-1] >= thres1) {
                      tempbnd = Math.max( eps23, Math.abs(workl[irz+j-1]) );
                      if (workl[ibd+j-1] <= tol*tempbnd) {
                         select[j] = true;
                      }
                   } // if (workl[irz+j-1] >= thres1)
                } // else if (which.equalsIgnoreCase("LA"))
                else if (which.equalsIgnoreCase("SA")) {
                   if (workl[irz+j-1] <= thres1) {
                      tempbnd = Math.max( eps23, Math.abs(workl[irz+j-1]) );
                      if (workl[ibd+j-1] <= tol*tempbnd) {
                         select[j] = true;
                      }
                   } // if (workl[irz+j-1] <= thres1)
                } // else if (which.equalsIgnoreCase("SA")) 
                else if (which.equalsIgnoreCase("BE")) {
                   if ( workl[irz+j-1] <= thres1 || workl[irz+j-1] >= thres2 ) {
                      tempbnd = Math.max( eps23, Math.abs(workl[irz+j-1]) );
                      if (workl[ibd+j-1] <= tol*tempbnd) {
                         select[j] = true;
                      }
                   } // if ( workl[irz+j-1] <= thres1 || workl[irz+j-1]) >= thres2 )
                } // else if (which.equalsIgnoreCase("BE"))
                if (j+1 > nconv ) reord = select[j] || reord;
                if (select[j]) ktrord = ktrord + 1;
             } // for (j = 0; j <= ncv-1; j++)

    //        %-------------------------------------------%
    //        | If KTRORD .ne. NCONV, something is wrong. |
    //        %-------------------------------------------%
    
             if (msglvl > 2) {
            	 UI.setDataText("dseupd: Number of specified eigenvalues ktrord = " + ktrord + "\n");
                 UI.setDataText("dseupd: Number of \"converged\" eigenvalues nconv = " + nconv + "\n");
             } // if (msglvl > 2)
    
    //        %-----------------------------------------------------------%
    //        | Call LAPACK routine _steqr to compute the eigenvalues and |
    //        | eigenvectors of the final symmetric tridiagonal matrix H. |
    //        | Initialize the eigenvector matrix Q to the identity.      |
    //        %-----------------------------------------------------------%
    
             for (i = 0; i < ncv-1; i++) {
            	 workl[ihb-1+i] = workl[ih+i];
             }
             for (i = 0; i < ncv; i++) {
                 workl[ihd-1+i] = workl[ih+ldh-1+i];	 
             }
             
             v1 = new double[ncv];
             for (i = 0; i < ncv; i++) {
            	 v1[i] = workl[ihd-1+i];
             }
             v2 = new double[ncv-1];
             for (i = 0; i < ncv-1; i++) {
            	 v2[i] = workl[ihb-1+i];
             }
             array = new double[ldq][ncv];
             index = 0;
             for (j = 0; j < ncv; j++) {
            	 for (i = 0; i < ldq; i++) {
            		 array[i][j] = workl[iq-1+index];
            		 index++;
            	 }
             }
             work = new double[Math.max(1,2*ncv-2)];
             ge.dsteqr ('I', ncv, v1, v2, array, ldq, work, ierr);
             for (i = 0; i < ncv; i++) {
            	 workl[ihd-1+i] = v1[i];
             }
             for (i = 0; i < ncv-1; i++) {
            	 workl[ihb-1+i] = v2[i];
             }
             index = 0;
             for (j = 0; j < ncv; j++) {
            	 for (i = 0; i < ldq; i++) {
            		 workl[iq-1+index] = array[i][j];
            		 index++;
            	 }
             }
    
             if (ierr[0] != 0) {
                info[0] = -8;
                return;
             }
    
             if (msglvl > 1) {
            	for (i = 0; i < ncv; i++) {
            		workl[iw-1+i] = workl[iq+ncv-2+i];
            	}
                UI.setDataText("dseupd: NCV Ritz values of the final H matrix:\n");
                for (i = 0; i < ncv; i++) {
                	UI.setDataText("workl["+(ihd-1+i)+"] = " + nf.format(workl[ihd-1+i]) + "\n");
                }
                UI.setDataText("dseupd: last row of the eigenvector matrix for H: \n");
                for (i = 0; i < ncv; i++) {
                	UI.setDataText("workl["+(iw-1+i)+"] = " + nf.format(workl[iw-1+i]) + "\n");
                }
             } // if (msglvl > 1)
    
             if (reord) {
    
    //           %---------------------------------------------%
    //           | Reordered the eigenvalues and eigenvectors  |
    //           | computed by _steqr so that the "converged"  |
    //           | eigenvalues appear in the first NCONV       |
    //           | positions of workl(ihd), and the associated |
    //           | eigenvectors appear in the first NCONV      |
    //           | columns.                                    |
    //           %---------------------------------------------%
    
                leftptr = 1;
                rghtptr = ncv;
    
                if (ncv != 1) {
                   while (true) {
    
                    if (select[leftptr-1]) {
    
    //              %-------------------------------------------%
    //              | Search, from the left, for the first Ritz |
    //              | value that has not converged.             |
    //              %-------------------------------------------%
    //
                   leftptr = leftptr + 1;
                    } // if (select[leftptr-1])
                else if (! select[rghtptr-1]) {
    
    //              %----------------------------------------------%
    //              | Search, from the right, the first Ritz value |
    //              | that has converged.                          |
    //              %----------------------------------------------%
    
                   rghtptr = rghtptr - 1;
                } // else if (! select[rghtptr-1])
                else {
    
    //              %----------------------------------------------%
    //              | Swap the Ritz value on the left that has not |
    //              | converged with the Ritz value on the right   |
    //              | that has converged.  Swap the associated     |
    //              | eigenvector of the tridiagonal matrix H as   |
    //              | well.                                        |
    //              %----------------------------------------------%
    
                   temp = workl[ihd+leftptr-2];
                   workl[ihd+leftptr-2] = workl[ihd+rghtptr-2];
                   workl[ihd+rghtptr-2] = temp;
                   for (i = 0; i < ncv; i++) {
                	   workl[iw-1+i] = workl[iq + ncv*(leftptr-1)-1+i];
                   }
                   for (i = 0; i < ncv; i++) {
                	   workl[iq+ncv*(leftptr-1)-1+i] = workl[iq+ncv*(rghtptr-1)-1+i];
                   }
                   for (i = 0; i < ncv; i++) {
                	   workl[iq+ncv*(rghtptr-1)-1+i] = workl[iw-1+i];
                   }
                   leftptr = leftptr + 1;
                   rghtptr = rghtptr - 1;
                } // else
                    
                if (leftptr >= rghtptr) {
                	break;
                }
                   } // while (true)
                } // if (ncv != 1)
             } // if (reord)
    
             if (msglvl > 2) {
            	 UI.setDataText("dseupd: The eigenvalues of H--reordered: \n");
            	 for (i = 0; i < ncv; i++) {
            		 UI.setDataText("workl["+(ihd-1+i)+"] = " + nf.format(workl[ihd-1+i]) + "\n");
            	 }
             }
    
    //        %----------------------------------------%
    //        | Load the converged Ritz values into D. |
    //        %----------------------------------------%
    
             for (i = 0; i < nconv; i++) {
            	 d[i] = workl[ihd-1+i];
             }
          } // if (rvec)
          else { // !rvec
    
    //        %-----------------------------------------------------%
    //        | Ritz vectors not required. Load Ritz values into D. |
    //        %-----------------------------------------------------%
    
        	 for (i = 0; i < nconv; i++) {
        		 d[i] = workl[ritz-1+i];
        	 }
             for (i = 0; i < ncv; i++) {
            	 workl[ihd-1+i] = workl[ritz-1+i];
             }
    
          } // else !rvec
    
    //     %------------------------------------------------------------------%
    //     | Transform the Ritz values and possibly vectors and corresponding |
    //     | Ritz estimates of OP to those of A*x=lambda*B*x. The Ritz values |
    //     | (and corresponding data) are returned in ascending order.        |
    //     %------------------------------------------------------------------%
    
          if (type.equalsIgnoreCase("REGULR")) {
    
    //        %---------------------------------------------------------%
    //        | Ascending sort of wanted Ritz values, vectors and error |
    //        | bounds. Not necessary if only Ritz values are desired.  |
    //        %---------------------------------------------------------%
    
             if (rvec) {
            	array = new double[ncv][nconv];
            	index = 0;
            	for (j = 0; j < nconv; j++) {
            		for (i = 0; i < ncv; i++) {
            			array[i][j] = workl[iq-1+index];
            			index++;
            		}
            	}
                dsesrt ("LA", rvec , nconv, d, ncv, array, ldq);
                index = 0;
            	for (j = 0; j < nconv; j++) {
            		for (i = 0; i < ncv; i++) {
            			workl[iq-1+index] = array[i][j];
            			index++;
            		}
            	}
             }
             else {
            	for (i = 0; i < ncv; i++) {
            		workl[ihb-1+i] = workl[bounds-1+i];
            	}
             }
          } // if (type.equalsIgnoreCase("REGULR"))
          else { // !"REGULR"
     
    //        %-------------------------------------------------------------%
    //        | *  Make a copy of all the Ritz values.                      |
    //        | *  Transform the Ritz values back to the original system.   |
    //        |    For TYPE = 'SHIFTI' the transformation is                |
    //        |             lambda = 1/theta + sigma                        |
    //        |    For TYPE = 'BUCKLE' the transformation is                |
    //        |             lambda = sigma * theta / ( theta - 1 )          |
    //        |    For TYPE = 'CAYLEY' the transformation is                |
    //        |             lambda = sigma * (theta + 1) / (theta - 1 )     |
    //        |    where the theta are the Ritz values returned by dsaupd.  |
    //        | NOTES:                                                      |
    //        | *The Ritz vectors are not affected by the transformation.   |
    //        |  They are only reordered.                                   |
    //        %-------------------------------------------------------------%
    
             for (i = 0; i < ncv; i++) {
            	 workl[iw-1+i] = workl[ihd-1+i];
             }
             if (type.equalsIgnoreCase("SHIFTI")) { 
                for (k = 0; k < ncv; k++) {
                   workl[ihd+k-1] = one / workl[ihd+k-1] + sigma;
                }
             } // if (type.equalsIgnoreCase("SHIFTI"))
             else if (type.equalsIgnoreCase("BUCKLE")) {
                for (k = 0; k < ncv; k++) {
                   workl[ihd+k-1] = sigma * workl[ihd+k-1] / 
                                   (workl[ihd+k-1] - one);
                } // for (k = 0; k < ncv; k++)
             } // else if (type.equalsIgnoreCase("BUCKLE"))
             else if (type.equalsIgnoreCase("CAYLEY")) {
                for (k = 0; k < ncv; k++) {
                   workl[ihd+k-1] = sigma * (workl[ihd+k-1] + one) /
                                   (workl[ihd+k-1] - one);
                }
             } // else if (type.equalsIgnoreCase("CAYLEY"))
     
    //        %-------------------------------------------------------------%
    //        | *  Store the wanted NCONV lambda values into D.             |
    //        | *  Sort the NCONV wanted lambda in WORKL(IHD:IHD+NCONV-1)   |
    //        |    into ascending order and apply sort to the NCONV theta   |
    //        |    values in the transformed system. We'll need this to     |
    //        |    compute Ritz estimates in the original system.           |
    //        | *  Finally sort the lambda's into ascending order and apply |
    //        |    to Ritz vectors if wanted. Else just sort lambda's into  |
    //        |    ascending order.                                         |
    //        | NOTES:                                                      |
    //        | *workl(iw:iw+ncv-1) contain the theta ordered so that they  |
    //        |  match the ordering of the lambda. We'll use them again for |
    //        |  Ritz vector purification.                                  |
    //        %-------------------------------------------------------------%
    
             for (i = 0; i < nconv; i++) {
            	 d[i] = workl[ihd-1+i];
             }
             v1 = new double[nconv];
             v2 = new double[nconv];
             for (i = 0; i < nconv; i++) {
            	 v1[i] = workl[ihd-1+i];
            	 v2[i] = workl[iw-1+i];
             }
             dsortr ("LA", true, nconv, v1, v2);
             for (i = 0; i < nconv; i++) {
            	 workl[ihd-1+i] = v1[i];
            	 workl[iw-1+i] = v2[i];
             }
             if (rvec) {
            	 array = new double[ncv][nconv];
             	index = 0;
             	for (j = 0; j < nconv; j++) {
             		for (i = 0; i < ncv; i++) {
             			array[i][j] = workl[iq-1+index];
             			index++;
             		}
             	}
                 dsesrt ("LA", rvec , nconv, d, ncv, array, ldq);
                 index = 0;
             	for (j = 0; j < nconv; j++) {
             		for (i = 0; i < ncv; i++) {
             			workl[iq-1+index] = array[i][j];
             			index++;
             		}
             	}
             }
             else {
            	 for (i = 0; i < ncv; i++) {
            		 workl[ihb-1+i] = workl[bounds-1+i];
            		 workl[ihb-1+i] = (bnorm2/rnorm)*workl[ihb-1+i];
            	 }
            	 v1 = new double[nconv];
            	 for (i = 0; i < nconv; i++) {
            		 v1[i] = workl[ihb-1+i];
            	 }
                dsortr ("LA", true, nconv, d, v1);
                for (i = 0; i < nconv; i++) {
                	workl[ihb-1+i] = v1[i];
                }
             }
    
          } // else !"REGULR"
     
    //     %------------------------------------------------%
    //     | Compute the Ritz vectors. Transform the wanted |
    //     | eigenvectors of the symmetric tridiagonal H by |
    //     | the Lanczos basis matrix V.                    |
    //     %------------------------------------------------%
    //
          if (rvec && howmny.equalsIgnoreCase("A")) {
        
    //        %----------------------------------------------------------%
    //        | Compute the QR factorization of the matrix representing  |
    //        | the wanted invariant subspace located in the first NCONV |
    //        | columns of workl(iq,ldq).                                |
    //        %----------------------------------------------------------%
         
        	 array = new double[ncv][nconv];
           	index = 0;
           	for (j = 0; j < nconv; j++) {
           		for (i = 0; i < ncv; i++) {
           			array[i][j] = workl[iq-1+index];
           			index++;
           		}
           	}
           	  v1 = new double[Math.min(ncv, nconv)];
           	  v2 = new double[nconv];
        	  ge.dgeqr2 (ncv, nconv, array, ldq, v1, v2, ierr);
        	  index = 0;
          	for (j = 0; j < nconv; j++) {
          		for (i = 0; i < ncv; i++) {
          			workl[iq-1+index] = array[i][j];
          			index++;
          		}
          	}
          	for (i = 0; i < Math.min(ncv, nconv); i++) {
          		workl[iw+ncv-1+i] = v1[i];
          	}
    
         
    //        %--------------------------------------------------------%
    //        | * Postmultiply V by Q.                                 |   
    //        | * Copy the first NCONV columns of VQ into Z.           |
    //        | The N by NCONV matrix Z is now a matrix representation |
    //        | of the approximate invariant subspace associated with  |
    //        | the Ritz values in workl(ihd).                         |
    //        %--------------------------------------------------------%
           	index = 0;
           	for (j = 0; j < nconv; j++) {
           		for (i = 0; i < ncv; i++) {
           			array[i][j] = workl[iq-1+index];
           			index++;
           		}
           	} 
           	 v1 = new double[nconv];
           	 for (i = 0; i < nconv; i++) {
           		 v1[i] = workl[iw+ncv-1+i];
           	 }
           	 v2 = new double[n];
             ge.dorm2r ('R', 'N', n, ncv, nconv, array,
                 ldq, v1, v, ldv, v2, ierr);
             ge.dlacpy ('A', n, nconv, v, ldv, z, ldz);
    
    //        %-----------------------------------------------------%
    //        | In order to compute the Ritz estimates for the Ritz |
    //        | values in both systems, need the last row of the    |
    //        | eigenvector matrix. Remember, it's in factored form |
    //        %-----------------------------------------------------%
    
             for (j = 0; j < ncv-1; j++) {
                workl[ihb+j-1] = zero; 
             }
             workl[ihb+ncv-2] = one;
             array2 = new double[ncv][1];
             for (i = 0; i < ncv; i++) {
            	 array2[i][0] = workl[ihb-1+i];
             }
             ge.dorm2r ('L', 'T', ncv, 1, nconv, array,
                 ldq, v1, array2, ncv, dum, ierr);
             for (i = 0; i < ncv; i++) {
            	 workl[ihb-1+i] = array2[i][0];
             }
          } // if (rvec && howmny.equalsIgnoreCase("A"))
          else if (rvec && howmny.equalsIgnoreCase("S")) {
    
    //     Not yet implemented. See remark 2 above.
    
          } //  else if (rvec && howmny.equalsIgnoreCase("S"))
    
          if (type.equalsIgnoreCase("REGULR") && rvec) {
    
                for (j=0; j < ncv; j++) {
                   workl[ihb+j-1] = rnorm * Math.abs( workl[ihb+j-1] );
                }
    
          } // if (type.equalsIgnoreCase("REGULR") && rvec)
          else if (!type.equalsIgnoreCase("REGULR") && rvec) {
    
    //        %-------------------------------------------------%
    //        | *  Determine Ritz estimates of the theta.       |
    //        |    If RVEC = .true. then compute Ritz estimates |
    //        |               of the theta.                     |
    //        |    If RVEC = .false. then copy Ritz estimates   |
    //        |              as computed by dsaupd.             |
    //        | *  Determine Ritz estimates of the lambda.      |
    //        %-------------------------------------------------%
    //
             for (i = 0; i < ncv; i++) {
            	 workl[ihb-1+i] = bnorm2 * workl[ihb-1+i];
             }
             if (type.equalsIgnoreCase("SHIFTI")) { 
    
                for (k=0; k < ncv; k++) {
                   workl[ihb+k-1] = Math.abs( workl[ihb+k-1] ) /( workl[iw+k-1] * workl[iw+k-1]);
                }
    
             } // if (type.equalsIgnoreCase("SHIFTI"))
             else if (type.equalsIgnoreCase("BUCKLE")) {
    
                for (k=0; k < ncv; k++) {
                   temp = workl[iw+k-1] - one;
                   workl[ihb+k-1] = sigma * Math.abs( workl[ihb+k-1] ) / (temp*temp);
                }
    
             } //  else if (type.equalsIgnoreCase("BUCKLE"))
             else if (type.equalsIgnoreCase("CAYLEY")) {
    
                for (k=0; k < ncv; k++) {
                   workl[ihb+k-1] = Math.abs( workl[ihb+k-1] / 
                                   workl[iw+k-1]*(workl[iw+k-1]-one) );
                }
    
             } // else if (type.equalsIgnoreCase("CAYLEY"))
    
          } // else if (!type.equalsIgnoreCase("REGULR") && rvec)
    
          if (!type.equalsIgnoreCase("REGULR") && msglvl > 1) {
        	 UI.setDataText("dseupd: Untransformed converged Ritz values: \n");
        	 for (i = 0; i < nconv; i++) {
        		 UI.setDataText("d["+i+"] = " + nf.format(d[i]) + "\n");
        	 }
             UI.setDataText("dseupd: Ritz estimates of the untransformed Ritz values: \n");
             for (i = 0; i < nconv; i++) {
            	 UI.setDataText("workl["+(ihb-1+i)+"] = " + nf.format(workl[ihb-1+i]) + "\n");
             }
          } // if (!type.equalsIgnoreCase("REGULR") && msglvl > 1)
          else if (msglvl > 1) {
        	 UI.setDataText("dseupd: Converged Ritz values: \n");
        	 for (i = 0; i < nconv; i++) {
        		 UI.setDataText("d["+i+"] = " + nf.format(d[i]) + "\n");
        	 }
             UI.setDataText("dseupd: Associated Ritz estimates: \n");
             for (i = 0; i < nconv; i++) {
            	 UI.setDataText("workl["+(ihb-1+i)+"] = " + nf.format(workl[ihb-1+i]) + "\n");
             }
          } // else if (msglvl > 1)
     
    //     %-------------------------------------------------%
    //     | Ritz vector purification step. Formally perform |
    //     | one of inverse subspace iteration. Only used    |
    //     | for MODE = 3,4,5. See reference 7               |
    //     %-------------------------------------------------%
    
          if (rvec && (type.equalsIgnoreCase("SHIFTI") || type.equalsIgnoreCase("CAYLEY"))) {
    
             for (k=0; k <= nconv-1; k++) {
                workl[iw+k-1] = workl[iq+k*ldq+ncv-2] / workl[iw+k-1];
             }
    
          } //  if (rvec && (type.equalsIgnoreCase("SHIFTI") || type.equalsIgnoreCase("CAYLEY")))
          else if (rvec && type.equalsIgnoreCase("BUCKLE")) {
    
        	  for (k=0; k <= nconv-1; k++) {
                workl[iw+k-1] = workl[iq+k*ldq+ncv-2] / (workl[iw+k-1]-one);
        	  }
    
          } // else if (rvec && type.equalsIgnoreCase("BUCKLE")) 
    
          if (!type.equalsIgnoreCase("REGULR")) {
        	v1 = new double[nconv];
        	for (i = 0; i < nconv; i++) {
        		v1[i] = workl[iw-1+i];
        	}
            ge.dger (n, nconv, one, resid, 1, v1, 1, z, ldz);
          }
    
          return;
          } // dseupd
          
      // -----------------------------------------------------------------------
      // \BeginDoc
      
      // \Name: dsesrt
      
      // \Description:
      //  Sort the array X in the order specified by WHICH and optionally 
      //  apply the permutation to the columns of the matrix A.
      
      // \Usage:
      //  call dsesrt
      //     ( WHICH, APPLY, N, X, NA, A, LDA)
      
      // \Arguments
      //  WHICH   Character*2.  (Input)
      //          'LM' -> X is sorted into increasing order of magnitude.
      //          'SM' -> X is sorted into decreasing order of magnitude.
      //          'LA' -> X is sorted into increasing order of algebraic.
      //          'SA' -> X is sorted into decreasing order of algebraic.
      
      //  APPLY   Logical.  (Input)
      //          APPLY = .TRUE.  -> apply the sorted order to A.
      //          APPLY = .FALSE. -> do not apply the sorted order to A.
      
      //  N       Integer.  (INPUT)
      //          Dimension of the array X.
      
      //  X      Double precision array of length N.  (INPUT/OUTPUT)
      //          The array to be sorted.
      
      //  NA      Integer.  (INPUT)
      //          Number of rows of the matrix A.
      
      //  A      Double precision array of length NA by N.  (INPUT/OUTPUT)
               
      //  LDA     Integer.  (INPUT)
      //          Leading dimension of A.
      
      // \EndDoc
      
      // -----------------------------------------------------------------------
      
      // \BeginLib
      
      // \Routines
      //     dswap  Level 1 BLAS that swaps the contents of two vectors.
      
      //\Authors
      //     Danny Sorensen               Phuong Vu
      //     Richard Lehoucq              CRPC / Rice University 
      //     Dept. of Computational &     Houston, Texas 
      //     Applied Mathematics
      //     Rice University           
      //     Houston, Texas            
      
      // \Revision history:
      //     12/15/93: Version ' 2.1'.
      //               Adapted from the sort routine in LANSO and 
      //               the ARPACK code dsortr
      
      // \SCCS Information: @(#) 
      // FILE: sesrt.F   SID: 2.3   DATE OF SID: 4/19/96   RELEASE: 2
      
      // \EndLib
      
      // -----------------------------------------------------------------------
      
            private void dsesrt (String which, boolean apply, int n, double x[], int na, double a[][], int lda) {
      
      //     %------------------%
      //     | Scalar Arguments |
      //     %------------------%
      
      //      character*2 which
      //      logical    apply
      //      integer    lda, n, na
      
      //     %-----------------%
      //     | Array Arguments |
      //     %-----------------%
      
      //      Double precision
      //     &           x(0:n-1), a(lda, 0:n-1)
      
      //     %---------------%
      //     | Local Scalars |
      //     %---------------%
      
            int    i, igap, j, k;
            double temp;
      
      //     %----------------------%
      //     | External Subroutines |
      //     %----------------------%
      
      //      external   dswap
      
      //     %-----------------------%
      //     | Executable Statements |
      //     %-----------------------%
      
            igap = n / 2;
       
            if (which.equalsIgnoreCase("SA")) {
      
      //        X is sorted into decreasing order of algebraic.
      
               while (true) {
               if (igap == 0) {
            	   return;
               }
               loop1: for(i = igap; i <= n-1; i++) {
                  j = i-igap;
                  while (true) {
      
                  if (j < 0) {
                	  continue loop1;
                  }
      
                  if (x[j] < x[j+igap]) {
                     temp = x[j];
                     x[j] = x[j+igap];
                     x[j+igap] = temp;
                     if (apply) {
                    	 for (k = 0; k < na; k++) {
                    		 temp = a[k][j];
                    		 a[k][j] = a[k][j+igap];
                    		 a[k][j+igap] = temp;
                    	 }
                     }
                  } //  if (x[j] < x[j+igap])
                  else {
                     continue loop1;
                  }
                  j = j-igap;
                  } // while (true)
               } // loop1: for(i = igap; i <= n-1; i++)
               igap = igap / 2;
               } // while (true)
            } //  if (which.equalsIgnoreCase("SA"))
            else if (which.equalsIgnoreCase("SM")) {
      
      //        X is sorted into decreasing order of magnitude.
      
               while (true) {
               if (igap == 0) {
            	   return;
               }
               loop2: for (i = igap; i <= n-1; i++) {
                  j = i-igap;
                  while (true) {
      
                  if (j < 0) {
                	  continue loop2;
                  }
      
                  if (Math.abs(x[j]) < Math.abs(x[j+igap])) {
                     temp = x[j];
                     x[j] = x[j+igap];
                     x[j+igap] = temp;
                     if (apply) {
                    	 for (k = 0; k < na; k++) {
                    		 temp = a[k][j];
                    		 a[k][j] = a[k][j+igap];
                    		 a[k][j+igap] = temp;
                    	 }
                     }
                  } // if (Math.abs(x[j]) < Math.abs(x[j+igap]))
                  else {
                     continue loop2;
                  }
                  j = j-igap;
                  } // while (true)
               } // loop2: for (i = igap; i <= n-1; i++)
               igap = igap / 2;
               } // while (true)
            } // else if (which.equalsIgnoreCase("SM"))
            else if (which.equalsIgnoreCase("LA")) {
      
      //        X is sorted into increasing order of algebraic.
      
               while (true) {
               if (igap == 0) {
            	   return;
               }
               loop3: for (i = igap; i <= n-1; i++) {
                  j = i-igap;
                  while (true) {
      
                  if (j < 0) {
                	  continue loop3;
                  }
                 
                  if (x[j] > x[j+igap]) {
                	  temp = x[j];
                      x[j] = x[j+igap];
                      x[j+igap] = temp;
                      if (apply) {
                     	 for (k = 0; k < na; k++) {
                     		 temp = a[k][j];
                     		 a[k][j] = a[k][j+igap];
                     		 a[k][j+igap] = temp;
                     	 }
                      }  
                  } // if (x[j] > x[j+igap])
                  else {
                     continue loop3;
                  }
                  j = j-igap;
                  } // while (true)
               } // loop3: for (i = igap; i <= n-1; i++)
               igap = igap / 2;
               } // while (true)
            } // else if (which.equalsIgnoreCase("LA"))
            else if (which.equalsIgnoreCase("LM")) { 
      
      //        X is sorted into increasing order of magnitude.
      
               while (true) {
               if (igap == 0) {
            	   return;
               }
               loop4: for (i = igap; i <= n-1; i++) {
                  j = i-igap;
                  while (true) {
      
                  if (j < 0) {
                	  continue loop4;
                  }
      
                  if (Math.abs(x[j]) > Math.abs(x[j+igap])) {
                	  temp = x[j];
                      x[j] = x[j+igap];
                      x[j+igap] = temp;
                      if (apply) {
                     	 for (k = 0; k < na; k++) {
                     		 temp = a[k][j];
                     		 a[k][j] = a[k][j+igap];
                     		 a[k][j+igap] = temp;
                     	 }
                      }     
                  } // if (Math.abs(x[j]) > Math.abs(x[j+igap]))
                  else {
                     continue loop4;
                  }
                  j = j-igap;
                  } // while (true)
               } // loop4: for (i = igap; i <= n-1; i++)
               igap = igap / 2;
               } // while (true)
            } // else if (which.equalsIgnoreCase("LM"))
            return;
            } // dsesrt
    
    //-----------------------------------------------------------------------
    //\BeginDoc
    //
    //\Name: dsaupd
    //
    //\Description: 
    //
    //  Reverse communication interface for the Implicitly Restarted Arnoldi 
    //  Iteration.  For symmetric problems this reduces to a variant of the Lanczos 
    //  method.  This method has been designed to compute approximations to a 
    //  few eigenpairs of a linear operator OP that is real and symmetric 
    //  with respect to a real positive semi-definite symmetric matrix B, 
    //  i.e.
    //                   
    //       B*OP = (OP')*B.  
    //
    //  Another way to express this condition is 
    //
    //       < x,OPy > = < OPx,y >  where < z,w > = z'Bw  .
    //  
    //  In the standard eigenproblem B is the identity matrix.  
    //  ( A' denotes transpose of A)
    //
    //  The computed approximate eigenvalues are called Ritz values and
    //  the corresponding approximate eigenvectors are called Ritz vectors.
    //
    //  dsaupd is usually called iteratively to solve one of the 
    //  following problems:
    //
    //  Mode 1:  A*x = lambda*x, A symmetric 
    //           ===> OP = A  and  B = I.
    //
    //  Mode 2:  A*x = lambda*M*x, A symmetric, M symmetric positive definite
    //           ===> OP = inv[M]*A  and  B = M.
    //           ===> (If M can be factored see remark 3 below)
    //
    //  Mode 3:  K*x = lambda*M*x, K symmetric, M symmetric positive semi-definite
    //           ===> OP = (inv[K - sigma*M])*M  and  B = M. 
    //           ===> Shift-and-Invert mode
    //
    //  Mode 4:  K*x = lambda*KG*x, K symmetric positive semi-definite, 
    //           KG symmetric indefinite
    //           ===> OP = (inv[K - sigma*KG])*K  and  B = K.
    //           ===> Buckling mode
    //
    //  Mode 5:  A*x = lambda*M*x, A symmetric, M symmetric positive semi-definite
    //           ===> OP = inv[A - sigma*M]*[A + sigma*M]  and  B = M.
    //           ===> Cayley transformed mode
    //
    //  NOTE: The action of w <- inv[A - sigma*M]*v or w <- inv[M]*v
    //        should be accomplished either by a direct method
    //        using a sparse matrix factorization and solving
    //
    //           [A - sigma*M]*w = v  or M*w = v,
    //
    //        or through an iterative method for solving these
    //        systems.  If an iterative method is used, the
    //        convergence test must be more stringent than
    //        the accuracy requirements for the eigenvalue
    //        approximations.
    //
    //\Usage:
    //  call dsaupd 
    //     ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
    //       IPNTR, WORKD, WORKL, LWORKL, INFO )
    //
    //\Arguments
    //  IDO     Integer.  (INPUT/OUTPUT)
    //          Reverse communication flag.  IDO must be zero on the first 
    //          call to dsaupd.  IDO will be set internally to
    //          indicate the type of operation to be performed.  Control is
    //          then given back to the calling routine which has the
    //          responsibility to carry out the requested operation and call
    //          dsaupd with the result.  The operand is given in
    //          WORKD(IPNTR[0]), the result must be put in WORKD(IPNTR[1]).
    //          (If Mode = 2 see remark 5 below)
    //          -------------------------------------------------------------
    //          IDO =  0: first call to the reverse communication interface
    //          IDO = -1: compute  Y = OP * X  where
    //                    IPNTR[0] is the pointer into WORKD for X,
    //                    IPNTR[1] is the pointer into WORKD for Y.
    //                    This is for the initialization phase to force the
    //                    starting vector into the range of OP.
    //          IDO =  1: compute  Y = OP * X where
    //                    IPNTR[0] is the pointer into WORKD for X,
    //                    IPNTR[1] is the pointer into WORKD for Y.
    //                    In mode 3,4 and 5, the vector B * X is already
    //                    available in WORKD(ipntr[2]).  It does not
    //                    need to be recomputed in forming OP * X.
    //          IDO =  2: compute  Y = B * X  where
    //                    IPNTR[0] is the pointer into WORKD for X,
    //                    IPNTR[1] is the pointer into WORKD for Y.
    //          IDO =  3: compute the IPARAM[7] shifts where
    //                    IPNTR[10] is the pointer into WORKL for
    //                    placing the shifts. See remark 6 below.
    //          IDO = 99: done
    //          -------------------------------------------------------------
    //             
    //  BMAT    Character*1.  (INPUT)
    //          BMAT specifies the type of the matrix B that defines the
    //          semi-inner product for the operator OP.
    //          B = 'I' -> standard eigenvalue problem A*x = lambda*x
    //          B = 'G' -> generalized eigenvalue problem A*x = lambda*B*x
    //
    //  N       Integer.  (INPUT)
    //          Dimension of the eigenproblem.
    //
    //  WHICH   Character*2.  (INPUT)
    //          Specify which of the Ritz values of OP to compute.
    //
    //          'LA' - compute the NEV largest (algebraic) eigenvalues.
    //          'SA' - compute the NEV smallest (algebraic) eigenvalues.
    //          'LM' - compute the NEV largest (in magnitude) eigenvalues.
    //          'SM' - compute the NEV smallest (in magnitude) eigenvalues. 
    //          'BE' - compute NEV eigenvalues, half from each end of the
    //                 spectrum.  When NEV is odd, compute one more from the
    //                 high end than from the low end.
    //           (see remark 1 below)
    //
    //  NEV     Integer.  (INPUT)
    //          Number of eigenvalues of OP to be computed. 0 < NEV < N.
    //
    //  TOL     Double precision scalar.  (INPUT)
    //          Stopping criterion: the relative accuracy of the Ritz value 
    //          is considered acceptable if BOUNDS(I) .LE. TOL*ABS(RITZ(I)).
    //          If TOL .LE. 0. is passed a default is set:
    //          DEFAULT = DLAMCH('EPS')  (machine precision as computed
    //                    by the LAPACK auxiliary subroutine DLAMCH).
    //
    //  RESID   Double precision array of length N.  (INPUT/OUTPUT)
    //          On INPUT: 
    //          If INFO .EQ. 0, a random initial residual vector is used.
    //          If INFO .NE. 0, RESID contains the initial residual vector,
    //                          possibly from a previous run.
    //          On OUTPUT:
    //          RESID contains the final residual vector. 
    //
    //  NCV     Integer.  (INPUT)
    //          Number of columns of the matrix V (less than or equal to N).
    //          This will indicate how many Lanczos vectors are generated 
    //          at each iteration.  After the startup phase in which NEV 
    //          Lanczos vectors are generated, the algorithm generates 
    //          NCV-NEV Lanczos vectors at each subsequent update iteration.
    //          Most of the cost in generating each Lanczos vector is in the 
    //          matrix-vector product OP*x. (See remark 4 below).
    //
    //  V       Double precision N by NCV array.  (OUTPUT)
    //          The NCV columns of V contain the Lanczos basis vectors.
    //
    //  LDV     Integer.  (INPUT)
    //          Leading dimension of V exactly as declared in the calling
    //          program.
    //
    //  IPARAM  Integer array of length 11.  (INPUT/OUTPUT)
    //          IPARAM[0] = ISHIFT: method for selecting the implicit shifts.
    //          The shifts selected at each iteration are used to restart
    //          the Arnoldi iteration in an implicit fashion.
    //          -------------------------------------------------------------
    //          ISHIFT = 0: the shifts are provided by the user via
    //                      reverse communication.  The NCV eigenvalues of
    //                      the current tridiagonal matrix T are returned in
    //                      the part of WORKL array corresponding to RITZ.
    //                      See remark 6 below.
    //          ISHIFT = 1: exact shifts with respect to the reduced 
    //                      tridiagonal matrix T.  This is equivalent to 
    //                      restarting the iteration with a starting vector 
    //                      that is a linear combination of Ritz vectors 
    //                      associated with the "wanted" Ritz values.
    //          -------------------------------------------------------------
    //
    //          IPARAM[1] = LEVEC
    //          No longer referenced. See remark 2 below.
    //
    //          IPARAM[2] = MXITER
    //          On INPUT:  maximum number of Arnoldi update iterations allowed. 
    //          On OUTPUT: actual number of Arnoldi update iterations taken. 
    //
    //          IPARAM[3] = NB: blocksize to be used in the recurrence.
    //          The code currently works only for NB = 1.
    //
    //          IPARAM[4] = NCONV: number of "converged" Ritz values.
    //          This represents the number of Ritz values that satisfy
    //          the convergence criterion.
    //
    //          IPARAM[5] = IUPD
    //          No longer referenced. Implicit restarting is ALWAYS used. 
    //
    //          IPARAM[6] = MODE
    //          On INPUT determines what type of eigenproblem is being solved.
    //          Must be 1,2,3,4,5; See under \Description of dsaupd for the 
    //          five modes available.
    //
    //          IPARAM[7] = NP
    //          When ido = 3 and the user provides shifts through reverse
    //          communication (IPARAM[0]=0), dsaupd returns NP, the number
    //          of shifts the user is to provide. 0 < NP <=NCV-NEV. See Remark
    //          6 below.
    //
    //          IPARAM[8] = NUMOP, IPARAM[9] = NUMOPB, IPARAM[10] = NUMREO,
    //          OUTPUT: NUMOP  = total number of OP*x operations,
    //                  NUMOPB = total number of B*x operations if BMAT='G',
    //                  NUMREO = total number of steps of re-orthogonalization.        
    //
    //  IPNTR   Integer array of length 11.  (OUTPUT)
    //          Pointer to mark the starting locations in the WORKD and WORKL
    //          arrays for matrices/vectors used by the Lanczos iteration.
    //          -------------------------------------------------------------
    //          IPNTR[0]: pointer to the current operand vector X in WORKD.
    //          IPNTR[1]: pointer to the current result vector Y in WORKD.
    //          IPNTR[2]: pointer to the vector B * X in WORKD when used in 
    //                    the shift-and-invert mode.
    //          IPNTR[3]: pointer to the next available location in WORKL
    //                    that is untouched by the program.
    //          IPNTR[4]: pointer to the NCV by 2 tridiagonal matrix T in WORKL.
    //          IPNTR[5]: pointer to the NCV RITZ values array in WORKL.
    //          IPNTR[6]: pointer to the Ritz estimates in array WORKL associated
    //                    with the Ritz values located in RITZ in WORKL.
    //          IPNTR[10]: pointer to the NP shifts in WORKL. See Remark 6 below.
    //
    //          Note: IPNTR(7:9) is only referenced by dseupd. See Remark 2.
    //          IPNTR[7]: pointer to the NCV RITZ values of the original system.
    //          IPNTR[8]: pointer to the NCV corresponding error bounds.
    //          IPNTR[9]: pointer to the NCV by NCV matrix of eigenvectors
    //                     of the tridiagonal matrix T. Only referenced by
    //                     dseupd if RVEC = .TRUE. See Remarks.
    //          -------------------------------------------------------------
    //          
    //  WORKD   Double precision work array of length 3*N.  (REVERSE COMMUNICATION)
    //          Distributed array to be used in the basic Arnoldi iteration
    //          for reverse communication.  The user should not use WORKD 
    //          as temporary workspace during the iteration. Upon termination
    //          WORKD(1:N) contains B*RESID(1:N). If the Ritz vectors are desired
    //          subroutine dseupd uses this output.
    //          See Data Distribution Note below.  
    //
    //  WORKL   Double precision work array of length LWORKL.  (OUTPUT/WORKSPACE)
    //          Private (replicated) array on each PE or array allocated on
    //          the front end.  See Data Distribution Note below.
    //
    //  LWORKL  Integer.  (INPUT)
    //          LWORKL must be at least NCV**2 + 8*NCV .
    //
    //  INFO    Integer.  (INPUT/OUTPUT)
    //          If INFO .EQ. 0, a randomly initial residual vector is used.
    //          If INFO .NE. 0, RESID contains the initial residual vector,
    //                          possibly from a previous run.
    //          Error flag on output.
    //          =  0: Normal exit.
    //          =  1: Maximum number of iterations taken.
    //                All possible eigenvalues of OP has been found. IPARAM[4]  
    //                returns the number of wanted converged Ritz values.
    //          =  2: No longer an informational error. Deprecated starting
    //                with release 2 of ARPACK.
    //          =  3: No shifts could be applied during a cycle of the 
    //                Implicitly restarted Arnoldi iteration. One possibility 
    //                is to increase the size of NCV relative to NEV. 
    //                See remark 4 below.
    //          = -1: N must be positive.
    //          = -2: NEV must be positive.
    //          = -3: NCV must be greater than NEV and less than or equal to N.
    //          = -4: The maximum number of Arnoldi update iterations allowed
    //                must be greater than zero.
    //          = -5: WHICH must be one of 'LM', 'SM', 'LA', 'SA' or 'BE'.
    //          = -6: BMAT must be one of 'I' or 'G'.
    //          = -7: Length of private work array WORKL is not sufficient.
    //          = -8: Error return from trid. eigenvalue calculation;
    //                Informatinal error from LAPACK routine dsteqr.
    //          = -9: Starting vector is zero.
    //          = -10: IPARAM[6] must be 1,2,3,4,5.
    //          = -11: IPARAM[6] = 1 and BMAT = 'G' are incompatable.
    //          = -12: IPARAM[0] must be equal to 0 or 1.
    //          = -13: NEV and WHICH = 'BE' are incompatable.
    //          = -9999: Could not build an Arnoldi factorization.
    //                   IPARAM[4] returns the size of the current Arnoldi
    //                   factorization. The user is advised to check that
    //                   enough workspace and array storage has been allocated.
    //
    //
    //\Remarks
    //  1. The converged Ritz values are always returned in ascending 
    //     algebraic order.  The computed Ritz values are approximate
    //     eigenvalues of OP.  The selection of WHICH should be made
    //     with this in mind when Mode = 3,4,5.  After convergence, 
    //     approximate eigenvalues of the original problem may be obtained 
    //     with the ARPACK subroutine dseupd. 
    //
    //  2. If the Ritz vectors corresponding to the converged Ritz values
    //     are needed, the user must call dseupd immediately following completion
    //     of dsaupd. This is new starting with version 2.1 of ARPACK.
    //
    //  3. If M can be factored into a Cholesky factorization M = LL'
    //     then Mode = 2 should not be selected.  Instead one should use
    //     Mode = 1 with  OP = inv(L)*A*inv(L').  Appropriate triangular 
    //     linear systems should be solved with L and L' rather
    //     than computing inverses.  After convergence, an approximate
    //     eigenvector z of the original problem is recovered by solving
    //     L'z = x  where x is a Ritz vector of OP.
    //
    //  4. At present there is no a-priori analysis to guide the selection
    //     of NCV relative to NEV.  The only formal requrement is that NCV > NEV.
    //     However, it is recommended that NCV .ge. 2*NEV.  If many problems of
    //     the same type are to be solved, one should experiment with increasing
    //     NCV while keeping NEV fixed for a given test problem.  This will 
    //     usually decrease the required number of OP*x operations but it
    //     also increases the work and storage required to maintain the orthogonal
    //     basis vectors.   The optimal "cross-over" with respect to CPU time
    //     is problem dependent and must be determined empirically.
    //
    //  5. If IPARAM[6] = 2 then in the Reverse commuication interface the user
    //     must do the following. When IDO = 1, Y = OP * X is to be computed.
    //     When IPARAM[6] = 2 OP = inv(B)*A. After computing A*X the user
    //     must overwrite X with A*X. Y is then the solution to the linear set
    //     of equations B*Y = A*X.
    //
    //  6. When IPARAM[0] = 0, and IDO = 3, the user needs to provide the 
    //     NP = IPARAM[7] shifts in locations: 
    //     1   WORKL(IPNTR[10])           
    //     2   WORKL(IPNTR[10]+1)         
    //                        .           
    //                        .           
    //                        .      
    //     NP  WORKL(IPNTR[10]+NP-1). 
    //
    //     The eigenvalues of the current tridiagonal matrix are located in 
    //     WORKL(IPNTR[5]) through WORKL(IPNTR[5]+NCV-1). They are in the
    //     order defined by WHICH. The associated Ritz estimates are located in
    //     WORKL(IPNTR[7]), WORKL(IPNTR[7]+1), ... , WORKL(IPNTR[7]+NCV-1).
    //
    // -----------------------------------------------------------------------
    //
    // \Data Distribution Note:
    //
    //  Fortran-D syntax:
    //  ================
    //  REAL       RESID(N), V(LDV,NCV), WORKD(3*N), WORKL(LWORKL)
    //  DECOMPOSE  D1(N), D2(N,NCV)
    //  ALIGN      RESID(I) with D1(I)
    //  ALIGN      V(I,J)   with D2(I,J)
    //  ALIGN      WORKD(I) with D1(I)     range (1:N)
    //  ALIGN      WORKD(I) with D1(I-N)   range (N+1:2*N)
    //  ALIGN      WORKD(I) with D1(I-2*N) range (2*N+1:3*N)
    //  DISTRIBUTE D1(BLOCK), D2(BLOCK,:)
    //  REPLICATED WORKL(LWORKL)
    //
    //  Cray MPP syntax:
    //  ===============
    //  REAL       RESID(N), V(LDV,NCV), WORKD(N,3), WORKL(LWORKL)
    //  SHARED     RESID(BLOCK), V(BLOCK,:), WORKD(BLOCK,:)
    //  REPLICATED WORKL(LWORKL)
    //  
    //
    //\BeginLib
    //
    // \References:
    //  1. D.C. Sorensen, "Implicit Application of Polynomial Filters in
    //     a k-Step Arnoldi Method", SIAM J. Matr. Anal. Apps., 13 (1992),
    //     pp 357-385.
    //  2. R.B. Lehoucq, "Analysis and Implementation of an Implicitly 
    //     Restarted Arnoldi Iteration", Rice University Technical Report
    //     TR95-13, Department of Computational and Applied Mathematics.
    //  3. B.N. Parlett, "The Symmetric Eigenvalue Problem". Prentice-Hall,
    //     1980.
    //  4. B.N. Parlett, B. Nour-Omid, "Towards a Black Box Lanczos Program",
    //     Computer Physics Communications, 53 (1989), pp 169-179.
    //  5. B. Nour-Omid, B.N. Parlett, T. Ericson, P.S. Jensen, "How to
    //     Implement the Spectral Transformation", Math. Comp., 48 (1987),
    //     pp 663-673.
    //  6. R.G. Grimes, J.G. Lewis and H.D. Simon, "A Shifted Block Lanczos 
    //     Algorithm for Solving Sparse Symmetric Generalized Eigenproblems", 
    //     SIAM J. Matr. Anal. Apps.,  January (1993).
    //  7. L. Reichel, W.B. Gragg, "Algorithm 686: FORTRAN Subroutines
    //     for Updating the QR decomposition", ACM TOMS, December 1990,
    //     Volume 16 Number 4, pp 369-377.
    //  8. R.B. Lehoucq, D.C. Sorensen, "Implementation of Some Spectral
    //     Transformations in a k-Step Arnoldi Method". In Preparation.
    //
    // \Routines called:
    //     dsaup2  ARPACK routine that implements the Implicitly Restarted
    //             Arnoldi Iteration.
    //     dstats  ARPACK routine that initialize timing and other statistics
    //             variables.
    //     ivout   ARPACK utility routine that prints integers.
    //     second  ARPACK utility routine for timing.
    //     dvout   ARPACK utility routine that prints vectors.
    //     dlamch  LAPACK routine that determines machine constants.
    //
    // \Authors
    //     Danny Sorensen               Phuong Vu
    //     Richard Lehoucq              CRPC / Rice University
    //     Dept. of Computational &     Houston, Texas
    //     Applied Mathematics
    //     Rice University           
    //     Houston, Texas            
    // 
    // \Revision history:
    //     12/15/93: Version ' 2.4'
    //
    // \SCCS Information: @(#) 
    // FILE: saupd.F   SID: 2.7   DATE OF SID: 8/27/96   RELEASE: 2 
    //
    // \Remarks
    //     1. None
    //
    // \EndLib
    //
    // -----------------------------------------------------------------------
    //
          public void dsaupd
            (int ido[], String bmat, int n, String which, int nev, double tol[], double resid[], int ncv, double v[][], int ldv, int iparam[], 
              int ipntr[], double workd[], double workl[], int lworkl, int info[] ) {
    //
    //     %----------------------------------------------------%
    //     | Include files for debugging and timing information |
    //     %----------------------------------------------------%
    //
    //     include   'debug.h'
    //     include   'stat.h'
    //
    //     %------------------%
    //     | Scalar Arguments |
    //     %------------------%
    //
    //     character  bmat*1, which*2
    //     integer    ido, info, ldv, lworkl, n, ncv, nev
    //     Double precision
    //    &           tol
    //
    //     %-----------------%
    //     | Array Arguments |
    //     %-----------------%
    //
    //     integer    iparam(11), ipntr(11)
    //     Double precision
    //    &           resid(n), v(ldv,ncv), workd(3*n), workl(lworkl)
    //
    //     %------------%
    //     | Parameters |
    //     %------------%
    //
          final double zero = 0.0;
   
    //
    //     %---------------%
    //     | Local Scalars |
    //     %---------------%
    //
          int i;
          int  j;
          int index;
    //
    //     %----------------------%
    //     | External Subroutines |
    //     %----------------------%
    //
    //     external   dsaup2,  dvout, ivout, second, dstats
    //
    //     %--------------------%
    //     | External Functions |
    //     %--------------------%
    //
    //      Double precision
    //    &           dlamch
    //     external   dlamch
    //
    //     %-----------------------%
    //     | Executable Statements |
    //     %-----------------------%
    // 
          if (ido[0] == 0) {
    //
    //        %-------------------------------%
    //        | Initialize timing statistics  |
    //        | & message level for debugging |
    //        %-------------------------------%
    //
             dstats();
             t0 = System.currentTimeMillis();
             dsaupd_msglvl = msaupd;
    
             dsaupd_ierr   = 0;
             dsaupd_ishift = iparam[0];
             dsaupd_mxiter[0] = iparam[2];
             dsaupd_nb     = iparam[3];
    
    //        %--------------------------------------------%
    //        | Revision 2 performs only implicit restart. |
    //        %--------------------------------------------%
    
             dsaupd_iupd   = 1;
             dsaupd_mode   = iparam[6];
    
    //        %----------------%
    //        | Error checking |
    //        %----------------%
    
             if (n <= 0) {
                dsaupd_ierr = -1;
             }
             else if (nev <= 0) {
                dsaupd_ierr = -2;
             }
             else if (ncv <= nev ||  ncv > n) {
                dsaupd_ierr = -3;
             }
    
    //        %----------------------------------------------%
    //        | NP is the number of additional steps to      |
    //        | extend the length NEV Lanczos factorization. |
    //        %----------------------------------------------%
    //
             dsaupd_np[0]     = ncv - nev;
    // 
             if (dsaupd_mxiter[0] <= 0)  {
            	 dsaupd_ierr = -4;
             }
             if (!which.equalsIgnoreCase("LM") &&
                 !which.equalsIgnoreCase("SM") &&
                 !which.equalsIgnoreCase("LA") &&
                 !which.equalsIgnoreCase("SA") &&
                 !which.equalsIgnoreCase("BE")) {
            	 dsaupd_ierr = -5;
             }
             if (!bmat.equalsIgnoreCase("I") && !bmat.equalsIgnoreCase("G")) {
            	 dsaupd_ierr = -6;
             }
             if (lworkl < ncv*ncv + 8*ncv) {
            	 dsaupd_ierr = -7;
             }
             if (dsaupd_mode < 1 || dsaupd_mode > 5) {
            	 dsaupd_ierr = -10;	 
             }                                       
             else if (dsaupd_mode == 1 && bmat.equalsIgnoreCase("G")) {
            	 dsaupd_ierr = -11;	 
             }                                        
             else if (dsaupd_ishift < 0 || dsaupd_ishift > 1) {
                 dsaupd_ierr = -12;
             }
             else if (nev == 1 && which.equalsIgnoreCase("BE")) {
                 dsaupd_ierr = -13;
             }
     
    //        %------------%
    //        | Error Exit |
    //        %------------%
    
             if (dsaupd_ierr != 0) {
                info[0] = dsaupd_ierr;
                ido[0]  = 99;
                return;
             }
     
    //        %------------------------%
    //        | Set default parameters |
    //        %------------------------%
    
             if (dsaupd_nb <= 0)  {
            	 dsaupd_nb = 1;
             }
             if (tol[0] <= zero)  {
            	 tol[0] = ge.dlamch('E');
             }
    
    //        %----------------------------------------------%
    //        | NP is the number of additional steps to      |
    //        | extend the length NEV Lanczos factorization. |
    //        | NEV0 is the local variable designating the   |
    //        | size of the invariant subspace desired.      |
    //        %----------------------------------------------%
    
             dsaupd_np[0]     = ncv - nev;
             dsaupd_nev0[0]   = nev; 
     
    //        %-----------------------------%
    //        | Zero out internal workspace |
    //        %-----------------------------%
    
             for (j = 0; j <= ncv*ncv + 8*ncv - 1; j++) {
                workl[j] = zero;
             }
     
    //        %-------------------------------------------------------%
    //        | Pointer into WORKL for address of H, RITZ, BOUNDS, Q  |
    //        | etc... and the remaining workspace.                   |
    //        | Also update pointer to be used on output.             |
    //        | Memory is laid out as follows:                        |
    //        | workl(1:2*ncv) := generated tridiagonal matrix        |
    //        | workl(2*ncv+1:2*ncv+ncv) := ritz values               |
    //        | workl(3*ncv+1:3*ncv+ncv) := computed error bounds     |
    //        | workl(4*ncv+1:4*ncv+ncv*ncv) := rotation matrix Q     |
    //        | workl(4*ncv+ncv*ncv+1:7*ncv+ncv*ncv) := workspace     |
    //        %-------------------------------------------------------%
    
             dsaupd_ldh    = ncv;
             dsaupd_ldq    = ncv;
             dsaupd_ih     = 1;
             dsaupd_ritz   = dsaupd_ih     + 2*dsaupd_ldh;
             dsaupd_bounds = dsaupd_ritz   + ncv;
             dsaupd_iq     = dsaupd_bounds + ncv;
             dsaupd_iw     = dsaupd_iq     + ncv*ncv;
             dsaupd_next   = dsaupd_iw     + 3*ncv;
    
             ipntr[3] = dsaupd_next;
             ipntr[4] = dsaupd_ih;
             ipntr[5] = dsaupd_ritz;
             ipntr[6] = dsaupd_bounds;
             ipntr[10] = dsaupd_iw;
          } // if (ido[0] == 0)
    
    //     %-------------------------------------------------------%
    //     | Carry out the Implicitly restarted Lanczos Iteration. |
    //     %-------------------------------------------------------%
    
         //dsaup2 
            //( ido, bmat, n, which, dsaupd_nev0, dsaupd_np, tol, resid, dsaupd_mode, dsaupd_iupd,
              //dsaupd_ishift, dsaupd_mxiter, v, ldv, workl(dsaupd_ih), dsaupd_ldh, workl(dsaupd_ritz),
              //workl(dsaupd_bounds), workl(dsaupd_iq), dsaupd_ldq, workl(dsaupd_iw), ipntr, workd,
              //info );
         double H[][] = new double[ncv][2];
         double ritz[] = new double[ncv];
         double bounds[] = new double[ncv];
         double Q[][] = new double[ncv][ncv];
         double workl2[] = new double[3*ncv];
         index = 0;
         for (j = 0; j < 2; j++) {
        	 for (i = 0; i < ncv; i++) {
        		 H[i][j] = workl[dsaupd_ih-1+index];
        		 index++;
        	 }
         }
         for (i = 0; i < ncv; i++) {
        	 ritz[i] = workl[dsaupd_ritz - 1 + i];
         }
         for (i = 0; i < ncv; i++) {
        	 bounds[i] = workl[dsaupd_bounds - 1 + i];
         }
         index = 0;
         for (j = 0; j < ncv; j++) {
        	 for (i = 0; i < ncv; i++) {
        		 Q[i][j] = workl[dsaupd_iq-1+index];
        		 index++;
        	 }
         }
         for (i = 0; i < 3*ncv; i++) {
        	 workl2[i] = workl[dsaupd_iw-1+i];
         }
         dsaup2 
         ( ido, bmat, n, which, dsaupd_nev0, dsaupd_np, tol, resid, dsaupd_mode, dsaupd_iupd,
           dsaupd_ishift, dsaupd_mxiter, v, ldv, H, dsaupd_ldh, ritz,
           bounds, Q, dsaupd_ldq, workl2, ipntr, workd,
           info );
         index = 0;
         for (j = 0; j < 2; j++) {
        	 for (i = 0; i < ncv; i++) {
        		 workl[dsaupd_ih-1+index] = H[i][j];
        		 index++;
        	 }
         }
         for (i = 0; i < ncv; i++) {
        	 workl[dsaupd_ritz - 1 + i] = ritz[i];
         }
         for (i = 0; i < ncv; i++) {
        	 workl[dsaupd_bounds - 1 + i] = bounds[i];
         }
         index = 0;
         for (j = 0; j < ncv; j++) {
        	 for (i = 0; i < ncv; i++) {
        		 workl[dsaupd_iq-1+index] = Q[i][j];
        		 index++;
        	 }
         }
         for (i = 0; i < 3*ncv; i++) {
        	workl[dsaupd_iw-1+i] = workl2[i];
         }
    
    //     %--------------------------------------------------%
    //     | ido .ne. 99 implies use of reverse communication |
    //     | to compute operations involving OP or shifts.    |
    //     %--------------------------------------------------%
    //
          if (ido[0] == 3) iparam[7] = dsaupd_np[0];
          if (ido[0] != 99) {
        	  return;
          }
     
          iparam[2] = dsaupd_mxiter[0];
          iparam[4] = dsaupd_np[0];
          iparam[8] = nopx;
          iparam[9] = nbx;
          iparam[10] = nrorth;
    
    //     %------------------------------------%
    //     | Exit if there was an informational |
    //     | error within dsaup2.               |
    //     %------------------------------------%
    
          if (info[0] < 0) {
        	  if (info[0] == -9999) {
        		  UI.setDataText("dsaupd: size of the current Arnoldi factorization = " + iparam[4] + "\n");
        	  }
        	  return;
          }
          if (info[0] == 2) info[0] = 3;
    
          if (dsaupd_msglvl > 0) {
        	 UI.setDataText("dsaupd: number of update iterations taken dsaupd_mxiter[0] = " + dsaupd_mxiter[0] + "\n");
             UI.setDataText("dsaupd: number of \"converged\" Ritz values dsaupd_np[0] = " + dsaupd_np[0] + "\n");
             UI.setDataText("dsaupd: final ritz values: \n");
             for (i = 0; i < dsaupd_np[0]; i++) {
            	 UI.setDataText("ritz["+i+"] = " + nf.format(ritz[i]) + "\n");
             }
             UI.setDataText("dsaupd: corresponding error bounds: \n");
             for (i = 0; i < dsaupd_np[0]; i++) {
            	 UI.setDataText("bounds["+i+"] = " + nf.format(bounds[i]) + "\n");
             }
          } // if (dsaupd_msglvl > 0) 
    
          t1 = System.currentTimeMillis();
          tsaupd = t1 - t0;
    
          if (dsaupd_msglvl > 0) {
    
    //        %--------------------------------------------------------%
    //        | Version Number & Version Date are defined in version.h |
    //        %--------------------------------------------------------%
    
             UI.setDataText("Symmetric implicit Arnoldi update code\n");
             UI.setDataText("Version Number 2.4\n");
             UI.setDataText("Version Date 07/31/96\n");
             UI.setDataText("Summary of timing statistics with time in milliseconds: \n");
        	 UI.setDataText("Total number update iterations = " + dsaupd_mxiter[0] + "\n");
        	 UI.setDataText("Total number of OP*x operations = " + nopx + "\n");
        	 UI.setDataText("Total number of B*x operations = " + nbx + "\n");
        	 UI.setDataText("Total number of reorthogonalization steps = " + nrorth + "\n");
        	 UI.setDataText("Total number of iterative refinement steps = " + nitref + "\n");
        	 UI.setDataText("Total number of restart steps = " + nrstrt + "\n");
        	 UI.setDataText("Total time in user OP*x operation = " + tmvopx + "\n");
        	 UI.setDataText("Total time in user B*x operation = " + tmvbx + "\n");
        	 UI.setDataText("Total time in Arnoldi update routine = " + tsaupd + "\n");
        	 UI.setDataText("Total time in dsaup2 routine = " + tsaup2 + "\n");
        	 UI.setDataText("Total time in basic Arnoldi iteration loop = " + tsaitr + "\n");
        	 UI.setDataText("Total time in reorthogonalization phase = " + titref + "\n");
        	 UI.setDataText("Total time in (re)start vector generation = " + tgetv0 + "\n");
        	 UI.setDataText("Total time in trid eigenvalue subproblem = " + tseigt + "\n");
        	 UI.setDataText("Total time in getting the shifts = " + tsgets + "\n");
        	 UI.setDataText("Total time in applying the shifts = " + tsapps + "\n");
        	 UI.setDataText("Total time in convergence testing = " + tsconv + "\n");
          } // if (dsaupd_msglvl > 0)
          return;
   
          } // dsaupd
          
          // -----------------------------------------------------------------------
          // \BeginDoc
          // 
          // \Name: dsaup2
          // 
          // \Description: 
          //  Intermediate level interface called by dsaupd.
          // 
          // \Usage:
          // call dsaup2 
          //     ( IDO, BMAT, N, WHICH, NEV, NP, TOL, RESID, MODE, IUPD,
          //       ISHIFT, MXITER, V, LDV, H, LDH, RITZ, BOUNDS, Q, LDQ, WORKL, 
          //       IPNTR, WORKD, INFO )
          //
          //\Arguments
          //
          //  IDO, BMAT, N, WHICH, NEV, TOL, RESID: same as defined in dsaupd.
          //  MODE, ISHIFT, MXITER: see the definition of IPARAM in dsaupd.
          //  
          //  NP      Integer.  (INPUT/OUTPUT)
          //          Contains the number of implicit shifts to apply during 
          //          each Arnoldi/Lanczos iteration.  
          //          If ISHIFT=1, NP is adjusted dynamically at each iteration 
          //          to accelerate convergence and prevent stagnation.
          //          This is also roughly equal to the number of matrix-vector 
          //          products (involving the operator OP) per Arnoldi iteration.
          //          The logic for adjusting is contained within the current
          //          subroutine.
          //          If ISHIFT=0, NP is the number of shifts the user needs
          //          to provide via reverse comunication. 0 < NP < NCV-NEV.
          //          NP may be less than NCV-NEV since a leading block of the current
          //          upper Tridiagonal matrix has split off and contains "unwanted"
          //          Ritz values.
          //          Upon termination of the IRA iteration, NP contains the number 
          //          of "converged" wanted Ritz values.
          //
          //  IUPD    Integer.  (INPUT)
          //          IUPD .EQ. 0: use explicit restart instead implicit update.
          //          IUPD .NE. 0: use implicit update.
          //
          //  V       Double precision N by (NEV+NP) array.  (INPUT/OUTPUT)
          //          The Lanczos basis vectors.
          //
          //  LDV     Integer.  (INPUT)
          //          Leading dimension of V exactly as declared in the calling 
          //          program.
          //
          //  H       Double precision (NEV+NP) by 2 array.  (OUTPUT)
          //          H is used to store the generated symmetric tridiagonal matrix
          //          The subdiagonal is stored in the first column of H starting 
          //          at H(2,1).  The main diagonal is stored in the second column
          //          of H starting at H(1,2). If dsaup2 converges store the 
          //          B-norm of the final residual vector in H(1,1).
          //
          //  LDH     Integer.  (INPUT)
          //          Leading dimension of H exactly as declared in the calling 
          //          program.
          //
          //  RITZ    Double precision array of length NEV+NP.  (OUTPUT)
          //          RITZ(1:NEV) contains the computed Ritz values of OP.
          //
          //  BOUNDS  Double precision array of length NEV+NP.  (OUTPUT)
          //          BOUNDS(1:NEV) contain the error bounds corresponding to RITZ.
          //
          //  Q       Double precision (NEV+NP) by (NEV+NP) array.  (WORKSPACE)
          //          Private (replicated) work array used to accumulate the 
          //          rotation in the shift application step.
          //
          //  LDQ     Integer.  (INPUT)
          //          Leading dimension of Q exactly as declared in the calling
          //          program.
          //          
          //  WORKL   Double precision array of length at least 3*(NEV+NP).  (INPUT/WORKSPACE)
          //          Private (replicated) array on each PE or array allocated on
          //          the front end.  It is used in the computation of the 
          //          tridiagonal eigenvalue problem, the calculation and
          //          application of the shifts and convergence checking.
          //          If ISHIFT .EQ. O and IDO .EQ. 3, the first NP locations
          //          of WORKL are used in reverse communication to hold the user 
          //          supplied shifts.
          
          //  IPNTR   Integer array of length 3.  (OUTPUT)
          //          Pointer to mark the starting locations in the WORKD for 
          //          vectors used by the Lanczos iteration.
          //          -------------------------------------------------------------
          //          IPNTR(1): pointer to the current operand vector X.
          //          IPNTR(2): pointer to the current result vector Y.
          //          IPNTR(3): pointer to the vector B * X when used in one of  
          //                    the spectral transformation modes.  X is the current
          //                    operand.
          //          -------------------------------------------------------------
          //          
          //  WORKD   Double precision work array of length 3*N.  (REVERSE COMMUNICATION)
          //          Distributed array to be used in the basic Lanczos iteration
          //          for reverse communication.  The user should not use WORKD
          //          as temporary workspace during the iteration !!!!!!!!!!
          //          See Data Distribution Note in dsaupd.
          //
          //  INFO    Integer.  (INPUT/OUTPUT)
          //          If INFO .EQ. 0, a randomly initial residual vector is used.
          //          If INFO .NE. 0, RESID contains the initial residual vector,
          //                          possibly from a previous run.
          //          Error flag on output.
          //          =     0: Normal return.
          //          =     1: All possible eigenvalues of OP has been found.  
          //                   NP returns the size of the invariant subspace
          //                   spanning the operator OP. 
          //          =     2: No shifts could be applied.
          //          =    -8: Error return from trid. eigenvalue calculation;
          //                   This should never happen.
          //          =    -9: Starting vector is zero.
          //          = -9999: Could not build an Lanczos factorization.
          //                   Size that was built in returned in NP.
          //
          // \EndDoc
          //
          // -----------------------------------------------------------------------
          //
          // \BeginLib
          
          // \References:
          //  1. D.C. Sorensen, "Implicit Application of Polynomial Filters in
          //     a k-Step Arnoldi Method", SIAM J. Matr. Anal. Apps., 13 (1992),
          //     pp 357-385.
          //  2. R.B. Lehoucq, "Analysis and Implementation of an Implicitly 
          //     Restarted Arnoldi Iteration", Rice University Technical Report
          //     TR95-13, Department of Computational and Applied Mathematics.
          //  3. B.N. Parlett, "The Symmetric Eigenvalue Problem". Prentice-Hall,
          //     1980.
          //  4. B.N. Parlett, B. Nour-Omid, "Towards a Black Box Lanczos Program",
          //     Computer Physics Communications, 53 (1989), pp 169-179.
          //  5. B. Nour-Omid, B.N. Parlett, T. Ericson, P.S. Jensen, "How to
          //     Implement the Spectral Transformation", Math. Comp., 48 (1987),
          //     pp 663-673.
          //  6. R.G. Grimes, J.G. Lewis and H.D. Simon, "A Shifted Block Lanczos 
          //     Algorithm for Solving Sparse Symmetric Generalized Eigenproblems", 
          //     SIAM J. Matr. Anal. Apps.,  January (1993).
          //  7. L. Reichel, W.B. Gragg, "Algorithm 686: FORTRAN Subroutines
          //     for Updating the QR decomposition", ACM TOMS, December 1990,
          //     Volume 16 Number 4, pp 369-377.
          
          // \Routines called:
          //     dgetv0  ARPACK initial vector generation routine. 
          //     dsaitr  ARPACK Lanczos factorization routine.
          //     dsapps  ARPACK application of implicit shifts routine.
          //     dsconv  ARPACK convergence of Ritz values routine.
          //     dseigt  ARPACK compute Ritz values and error bounds routine.
          //     dsgets  ARPACK reorder Ritz values and error bounds routine.
          //     dsortr  ARPACK sorting routine.
          //     ivout   ARPACK utility routine that prints integers.
          //     second  ARPACK utility routine for timing.
          //     dvout   ARPACK utility routine that prints vectors.
          //     dlamch  LAPACK routine that determines machine constants.
          //     dcopy   Level 1 BLAS that copies one vector to another.
          //     ddot    Level 1 BLAS that computes the scalar product of two vectors. 
          //     dnrm2   Level 1 BLAS that computes the norm of a vector.
          //     dscal   Level 1 BLAS that scales a vector.
          //     dswap   Level 1 BLAS that swaps two vectors.
          
          // \Author
          //     Danny Sorensen               Phuong Vu
          //     Richard Lehoucq              CRPC / Rice University
          //     Dept. of Computational &     Houston, Texas
          //     Applied Mathematics
          //     Rice University           
          //     Houston, Texas            
           
          // \Revision history:
          //     12/15/93: Version ' 2.4'
          //     xx/xx/95: Version ' 2.4'.  (R.B. Lehoucq)
          
          // \SCCS Information: @(#) 
          // FILE: saup2.F   SID: 2.6   DATE OF SID: 8/16/96   RELEASE: 2
          
          // \EndLib
          
          // -----------------------------------------------------------------------
          
                private void dsaup2
                  ( int ido[], String bmat, int n, String which, int nev[], int np[], double tol[], double resid[], int mode, int iupd, 
                    int ishift, int mxiter[], double v[][], int ldv, double h[][], int ldh, double ritz[], double bounds[], 
                    double q[][], int ldq, double workl[], int ipntr[], double workd[], int info[] ) {
          
          //     %----------------------------------------------------%
          //     | Include files for debugging and timing information |
          //     %----------------------------------------------------%
          
          //     include   'debug.h'
          //     include   'stat.h'
          
          //     %------------------%
          //     | Scalar Arguments |
          //     %------------------%
          
          //     character  bmat*1, which*2
          //     integer    ido, info, ishift, iupd, ldh, ldq, ldv, mxiter,
          //     &           n, mode, nev, np
          //     Double precision
          //     &           tol
          
          //     %-----------------%
          //     | Array Arguments |
          //     %-----------------%
          
          //      integer    ipntr(3)
          //      Double precision
          //     &           bounds(nev+np), h(ldh,2), q(ldq,nev+np), resid(n), 
          //     &           ritz(nev+np), v(ldv,nev+np), workd(3*n), 
          //     &           workl(3*(nev+np))
          //
          //     %------------%
          //     | Parameters |
          //     %------------%
          
                final double zero = 0.0;
          
          //     %---------------%
          //     | Local Scalars |
          //     %---------------%
          
                String  wprime = null;
                int ierr[] = new int[1];
                int    j, nevbef, nptemp, nevd2, nevm2;
                double temp;
                int i;
                boolean seg1;
                boolean seg2;
                boolean seg3;
                boolean seg4;
                boolean seg5;
                boolean seg6;
                double array1[];
                double array2[];
          
          //     %----------------------%
          //     | External Subroutines |
          //     %----------------------%
          //
          //      external   dcopy, dgetv0, dsaitr, dscal, dsconv, dseigt, dsgets, 
          //    &           dsapps, dsortr, dvout, ivout, second, dswap
          
          //     %--------------------%
          //     | External Functions |
          //     %--------------------%
          
          //      Double precision
          //   &           ddot, dnrm2, dlamch
          //      external   ddot, dnrm2, dlamch
          
          //     %---------------------%
          //     | Intrinsic Functions |
          //     %---------------------%
          
          //      intrinsic    min
          
          //     %-----------------------%
          //     | Executable Statements |
          //     %-----------------------%
          //
                if (ido[0] == 0) {
           
          //        %-------------------------------%
          //        | Initialize timing statistics  |
          //       | & message level for debugging |
          //        %-------------------------------%
          
                   t0 = System.currentTimeMillis();
                   dsaup2_msglvl = msaup2;
          
          //        %---------------------------------%
          //        | Set machine dependent constant. |
          //        %---------------------------------%
          
                   dsaup2_eps23 = ge.dlamch('E');
                   dsaup2_eps23 = Math.pow(dsaup2_eps23,(2.0/3.0));
          
          //        %-------------------------------------%
          //        | nev0 and np0 are integer variables  |
          //        | hold the initial values of NEV & NP |
          //        %-------------------------------------%
          
                   dsaup2_nev0   = nev[0];
                   dsaup2_np0    = np[0];
          
          //        %-------------------------------------%
          //        | kplusp is the bound on the largest  |
          //        |        Lanczos factorization built. |
          //        | nconv is the current number of      |
          //        |        "converged" eigenvlues.      |
          //        | iter is the counter on the current  |
          //        |      iteration step.                |
          //        %-------------------------------------%
          
                   dsaup2_kplusp = dsaup2_nev0 + dsaup2_np0;
                   dsaup2_nconv[0]  = 0;
                   dsaup2_iter = 0;
           
          //        %--------------------------------------------%
          //        | Set flags for computing the first NEV steps |
          //        | of the Lanczos factorization.              |
          //        %--------------------------------------------%
          
                   dsaup2_getv0 = true;
                   dsaup2_update = false;
                   dsaup2_ushift = false;
                   dsaup2_cnorm = false;
          
                   if (info[0] != 0) {
          
          //        %--------------------------------------------%
          //        | User provides the initial residual vector. |
          //        %--------------------------------------------%
          
                      dsaup2_initv = true;
                      info[0]  = 0;
                   } // if (info[0] != 0)
                   else {
                      dsaup2_initv = false;
                   }
                } // if (ido[0] == 0)
          
          //     %---------------------------------------------%
          //     | Get a possibly random starting vector and   |
          //     | force it into the range of the operator OP. |
          //     %---------------------------------------------%
          
          
                 if (dsaup2_getv0) {
                   dgetv0 (ido, bmat, 1, dsaup2_initv, n, 1, v, ldv, resid, dsaup2_rnorm,
                               ipntr, workd, info);
          
                   if (ido[0] != 99) {
                	   return;
                   }
          
                   if (dsaup2_rnorm[0] == zero) {
          
          //           %-----------------------------------------%
          //           | The initial vector is zero. Error exit. | 
          //           %-----------------------------------------%
          
                      info[0] = -9;
                      ido[0] = 99;
                      t1 = System.currentTimeMillis();
                      tsaup2 = t1 - t0;
                      return;
                   } // if (dsaup2_rnorm == zero)
                   dsaup2_getv0 = false;
                   ido[0]  = 0;
                } // if (dsaup2_getv0)
           
          //     %------------------------------------------------------------%
          //     | Back from reverse communication: continue with update step |
          //     %------------------------------------------------------------%
          
                seg1 = true;
                seg4 = true;
                seg5 = true;
                seg6 = true;
                 if (dsaup2_update) {
                	seg1 = false;
                	seg6 = false;
                }
                
                if (seg1) {
          
          //     %-------------------------------------------%
          //     | Back from computing user specified shifts |
          //     %-------------------------------------------%
          
                seg2 = true;	
                if (dsaup2_ushift) {
                	seg2 = false;
                	seg5 = false;
                }
                
                if (seg2) {
          
          //     %-------------------------------------%
          //     | Back from computing residual norm   |
          //     | at the end of the current iteration |
          //     %-------------------------------------%
          
                seg3 = true;
                if (dsaup2_cnorm)  {
                	seg3 = false;
                	seg4 = false;
                }
                
                if (seg3) {
           
          //     %----------------------------------------------------------%
          //     | Compute the first NEV steps of the Lanczos factorization |
          //     %----------------------------------------------------------%
          //
                	dsaitr(ido, bmat, n, 0, dsaup2_nev0, mode, resid, dsaup2_rnorm, v, ldv, 
                            h, ldh, ipntr, workd, info);
           
          //     %---------------------------------------------------%
          //     | ido .ne. 99 implies use of reverse communication  |
          //     | to compute operations involving OP and possibly B |
          //     %---------------------------------------------------%
          
                if (ido[0] != 99) {
                	return;
                }
          
                if (info[0] > 0) {
          
          //        %-----------------------------------------------------%
          //        | dsaitr was unable to build an Lanczos factorization |
          //        | of length NEV0. INFO is returned with the size of   |
          //        | the factorization built. Exit main loop.            |
          //        %-----------------------------------------------------%
          
                   np[0]   = info[0];
                   mxiter[0] = dsaup2_iter;
                   info[0] = -9999;
                   ido[0] = 99;
                   t1 = System.currentTimeMillis();
                   tsaup2 = t1 - t0;
                   return;
                } // if (info[0] > 0)
                } // if (seg3)
                } // if (seg2)
                } // if (seg1)
           
          //     %--------------------------------------------------------------%
          //     |                                                              |
          //     |           M A I N  LANCZOS  I T E R A T I O N  L O O P       |
          //     |           Each iteration implicitly restarts the Lanczos     |
          //     |           factorization in place.                            |
          //     |                                                              |
          //     %--------------------------------------------------------------%
           
           while (true) {
        	   if (seg4) {
        		   if (seg5) {
        			   if (seg6) {
          
                   dsaup2_iter = dsaup2_iter + 1;
          
                   if (dsaup2_msglvl > 0) {
                	   UI.setDataText("dsaup2: **** Start of major iteration number **** dsaup2_iter = " + dsaup2_iter + "\n");
                   }
                   if (dsaup2_msglvl > 1) {
                	  UI.setDataText("dsaup2: The length of the current Lanczos factorization nev[0] = " + nev[0] + "\n");
                	  UI.setDataText("dsaup2: Extend the Lanczos factorization by " + np + "\n");
                   }
           
          //        %------------------------------------------------------------%
          //        | Compute NP additional steps of the Lanczos factorization. |
          //        %------------------------------------------------------------%
          
                   ido[0] = 0;
        			   } // if (seg6)
        			   seg6 = true;
                   dsaup2_update = true;
          
                   dsaitr (ido, bmat, n, nev[0], np[0], mode, resid, dsaup2_rnorm, v, 
                               ldv, h, ldh, ipntr, workd, info);
           
          //        %---------------------------------------------------%
          //        | ido .ne. 99 implies use of reverse communication  |
          //        | to compute operations involving OP and possibly B |
          //        %---------------------------------------------------%
          
                   if (ido[0] != 99) {
                	   return;
                   }
          
                   if (info[0] > 0) {
          
          //           %-----------------------------------------------------%
          //           | dsaitr was unable to build an Lanczos factorization |
          //           | of length NEV0+NP0. INFO is returned with the size  |  
          //           | of the factorization built. Exit main loop.         |
          //           %-----------------------------------------------------%
          
                      np[0] = info[0];
                      mxiter[0] = dsaup2_iter;
                      info[0] = -9999;
                      ido[0] = 99;
                      t1 = System.currentTimeMillis();
                      tsaup2 = t1 - t0;
                      return;
                   } // if (info[0] > 0)
                   dsaup2_update = false;
          
                   if (dsaup2_msglvl > 1) {
                	   UI.setDataText("dsaup2: Current B-norm of residual for factorization dsaup2_rnorm[0] = " +
                                     nf.format(dsaup2_rnorm[0]) + "\n");
                   }
           
          //        %--------------------------------------------------------%
          //        | Compute the eigenvalues and corresponding error bounds |
          //        | of the current symmetric tridiagonal matrix.           |
          //        %--------------------------------------------------------%
          
                   dseigt (dsaup2_rnorm[0], dsaup2_kplusp, h, ldh, ritz, bounds, workl, ierr);
          
                   if (ierr[0] != 0) {
                      info[0] = -8;
                      ido[0] = 99;
                      t1 = System.currentTimeMillis();
                      tsaup2 = t1 - t0;
                      return;
                   }
          
          //        %----------------------------------------------------%
          //        | Make a copy of eigenvalues and corresponding error |
          //        | bounds obtained from _seigt.                       |
          //        %----------------------------------------------------%
          
                   for (i = 0; i < dsaup2_kplusp; i++) {
                	   workl[dsaup2_kplusp + i] = ritz[i];
                	   workl[2*dsaup2_kplusp + i] = bounds[i];
                   }
          
          //        %---------------------------------------------------%
          //        | Select the wanted Ritz values and their bounds    |
          //        | to be used in the convergence test.               |
          //        | The selection is based on the requested number of |
          //        | eigenvalues instead of the current NEV and NP to  |
          //        | prevent possible misconvergence.                  |
          //        | * Wanted Ritz values := RITZ(NP+1:NEV+NP)         |
          //        | * Shifts := RITZ(1:NP) := WORKL(1:NP)             |
          //        %---------------------------------------------------%
          //
                   nev[0] = dsaup2_nev0;
                   np[0] = dsaup2_np0;
                   dsgets (ishift, which, nev[0], np[0], ritz, bounds, workl);
           
          //        %-------------------%
          //        | Convergence test. |
          //        %-------------------%
          //
                   for (i = 0; i < nev[0]; i++) {
                	   workl[np[0]+i] = bounds[np[0]+i];
                   }
                   array1 = new double[nev[0]];
                   array2 = new double[nev[0]];
                   for (i = 0; i < nev[0]; i++) {
                	   array1[i] = ritz[np[0]+i];
                	   array2[i] = workl[np[0]+i];
                   }
                   dsconv (nev[0], array1, array2, tol[0], dsaup2_nconv);
                   for (i = 0; i < nev[0]; i++) {
                	   ritz[np[0]+i] = array1[i];
                	   workl[np[0]+i] = array2[i];
                   }
          
                   if (dsaup2_msglvl > 2) {
                      UI.setDataText("In dsaup2 nev[0] = " + nev[0] + " np[0] = " + np[0] + " dsaup2_nconv[0] = " + dsaup2_nconv[0] + "\n");
                      UI.setDataText("dsaup2: The eigenvalues of H: \n");
                      for (i = 0; i < dsaup2_kplusp; i++) {
                    	  UI.setDataText("ritz["+i+"] = " + nf.format(ritz[i]) + "\n");
                      }
                      UI.setDataText("dsaup2: Ritz estimates of the current NCV Ritz values: \n");
                      for (i = 0; i < dsaup2_kplusp; i++) {
                    	  UI.setDataText("bounds["+i+"] = " + nf.format(bounds[i]) + "\n");
                      }
                   } // if (dsaup2_msglvl > 2)
          
          //        %---------------------------------------------------------%
          //        | Count the number of unwanted Ritz values that have zero |
          //        | Ritz estimates. If any Ritz estimates are equal to zero |
          //        | then a leading block of H of order equal to at least    |
          //        | the number of Ritz values with zero Ritz estimates has  |
          //        | split off. None of these Ritz values may be removed by  |
          //        | shifting. Decrease NP the number of shifts to apply. If |
          //        | no shifts may be applied, then prepare to exit          |
          //        %---------------------------------------------------------%
          
                   nptemp = np[0];
                   for (j=0; j < nptemp; j++) {
                      if (bounds[j] == zero) {
                         np[0] = np[0] - 1;
                         nev[0] = nev[0] + 1;
                      }
                   } // for (j=0; j < nptemp; j++)
           
                   if ( (dsaup2_nconv[0] >= dsaup2_nev0) || (dsaup2_iter > mxiter[0]) ||(np[0] == 0) ) {
               
          //           %------------------------------------------------%
          //           | Prepare to exit. Put the converged Ritz values |
          //           | and corresponding bounds in RITZ(1:NCONV) and  |
          //           | BOUNDS(1:NCONV) respectively. Then sort. Be    |
          //           | careful when NCONV > NP since we don't want to |
          //           | swap overlapping locations.                    |
          //           %------------------------------------------------%
          
                      if (which.equalsIgnoreCase("BE")) { 
          
          //              %-----------------------------------------------------%
          //              | Both ends of the spectrum are requested.            |
          //              | Sort the eigenvalues into algebraically decreasing  |
          //              | order first then swap low end of the spectrum next  |
          //              | to high end in appropriate locations.               |
          //              | NOTE: when np < floor(nev/2) be careful not to swap |
          //              | overlapping locations.                              |
          //              %-----------------------------------------------------%
          
                         wprime = "SA";
                         dsortr (wprime, true, dsaup2_kplusp, ritz, bounds);
                         nevd2 = nev[0] / 2;
                         nevm2 = nev[0] - nevd2; 
                         if ( nev[0] > 1 ) {
                        	for (i = 0; i < Math.min(nevd2, np[0]); i++) {
                        		temp = ritz[nevm2+i];
                        		ritz[nevm2+i] = ritz[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i];
                        		ritz[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i] = temp;
                        		temp = bounds[nevm2+i];
                        		bounds[nevm2+i] = bounds[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i];
                        		bounds[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i] = temp;
                        	}
                         } // if ( nev[0 > 1 )
                      } // if (which.equalsIgnoreCase("BE"))
                       else {
          
          //              %--------------------------------------------------%
          //              | LM, SM, LA, SA case.                             |
          //              | Sort the eigenvalues of H into the an order that |
          //              | is opposite to WHICH, and apply the resulting    |
          //              | order to BOUNDS.  The eigenvalues are sorted so  |
          //              | that the wanted part are always within the first |
          //              | NEV locations.                                   |
          //              %--------------------------------------------------%
          
                         if (which.equalsIgnoreCase("LM")) wprime = "SM";
                         if (which.equalsIgnoreCase("SM")) wprime = "LM";
                         if (which.equalsIgnoreCase("LA")) wprime = "SA";
                         if (which.equalsIgnoreCase("SA")) wprime = "LA";
          
                         dsortr (wprime, true, dsaup2_kplusp, ritz, bounds);
          
                       }
          
          //           %--------------------------------------------------%
          //           | Scale the Ritz estimate of each Ritz value       |
          //           | by 1 / max(eps23,magnitude of the Ritz value).   |
          //           %--------------------------------------------------%
          
                      for (j = 0; j < dsaup2_nev0; j++) {
                         temp = Math.max( dsaup2_eps23, Math.abs(ritz[j]) );
                         bounds[j] = bounds[j]/temp;
                      }
          
          //           %----------------------------------------------------%
          //           | Sort the Ritz values according to the scaled Ritz  |
          //           | esitmates.  This will push all the converged ones  |
          //           | towards the front of ritzr, ritzi, bounds          |
          //           | (in the case when NCONV < NEV.)                    |
          //           %----------------------------------------------------%
          
                      wprime = "LA";
                      dsortr(wprime, true, dsaup2_nev0, bounds, ritz);
          
          //           %----------------------------------------------%
          //           | Scale the Ritz estimate back to its original |
          //           | value.                                       |
          //           %----------------------------------------------%
          
                      for (j = 0; j < dsaup2_nev0; j++) {
                          temp = Math.max( dsaup2_eps23, Math.abs(ritz[j]) );
                          bounds[j] = bounds[j]*temp;
                      }
          
          //           %--------------------------------------------------%
          //           | Sort the "converged" Ritz values again so that   |
          //           | the "threshold" values and their associated Ritz |
          //           | estimates appear at the appropriate position in  |
          //           | ritz and bound.                                  |
          //           %--------------------------------------------------%
          
                      if (which.equalsIgnoreCase("BE")) { 
          
          //              %------------------------------------------------%
          //              | Sort the "converged" Ritz values in increasing |
          //              | order.  The "threshold" values are in the      |
          //              | middle.                                        |
          //              %------------------------------------------------%
          
                         wprime = "LA";
                         dsortr(wprime, true, dsaup2_nconv[0], ritz, bounds);
                      } // if (which.equalsIgnoreCase("BE"))
                      else {
          
          //              %----------------------------------------------%
          //              | In LM, SM, LA, SA case, sort the "converged" |
          //              | Ritz values according to WHICH so that the   |
          //              | "threshold" value appears at the front of    |
          //              | ritz.                                        |
          //              %----------------------------------------------%

                         dsortr(which, true, dsaup2_nconv[0], ritz, bounds);
          
                      }
          
          //           %------------------------------------------%
          //           |  Use h( 1,1 ) as storage to communicate  |
          //           |  rnorm to _seupd if needed               |
          //           %------------------------------------------%
          
                      h[0][0] = dsaup2_rnorm[0];
          
                      if (dsaup2_msglvl > 1) {
                    	 UI.setDataText("dsaup2: Sorted Ritz values: \n");
                    	 for (i = 0; i < dsaup2_kplusp; i++) {
                    		 UI.setDataText("ritz["+i+"] = " + nf.format(ritz[i]) + "\n");
                    	 }
                         UI.setDataText("dsaup2: Sorted ritz estimates: \n");
                         for (i = 0; i < dsaup2_kplusp; i++) {
                        	 UI.setDataText("bounds["+i+"] = " + nf.format(bounds[i]) + "\n");
                         }
                      }
          
          //           %------------------------------------%
          //           | Max iterations have been exceeded. | 
          //           %------------------------------------%
          
                      if (dsaup2_iter > mxiter[0] && dsaup2_nconv[0] < nev[0]) info[0] = 1;
          
          //           %---------------------%
          //           | No shifts to apply. | 
          //           %---------------------%
          
                      if (np[0] == 0 && dsaup2_nconv[0] < dsaup2_nev0) info[0] = 2;
          
                      np[0] = dsaup2_nconv[0];
                      mxiter[0] = dsaup2_iter;
                      nev[0] = dsaup2_nconv[0];
                      ido[0] = 99;
                      t1 = System.currentTimeMillis();
                      tsaup2 = t1 - t0;
                      return;
                       } // } // if ( (dsaup2_nconv[0] >= dsaup2_nev0) || (dsaup2_iter > mxiter) ||(np == 0) )
                   else if (dsaup2_nconv[0] < nev[0] && ishift == 1) {
          
          //           %---------------------------------------------------%
          //           | Do not have all the requested eigenvalues yet.    |
          //           | To prevent possible stagnation, adjust the number |
          //           | of Ritz values and the shifts.                    |
          //           %---------------------------------------------------%
          
                      nevbef = nev[0];
                      nev[0] = nev[0] + Math.min (dsaup2_nconv[0], np[0]/2);
                      if (nev[0] == 1 && dsaup2_kplusp >= 6) {
                         nev[0] = dsaup2_kplusp / 2;
                      }
                      else if (nev[0] == 1 && dsaup2_kplusp > 2) {
                         nev[0] = 2;
                      }
                      np[0]  = dsaup2_kplusp - nev[0];
               
          //           %---------------------------------------%
          //           | If the size of NEV was just increased |
          //           | resort the eigenvalues.               |
          //           %---------------------------------------%
               
                      if (nevbef < nev[0])  {
                         dsgets (ishift, which, nev[0], np[0], ritz, bounds, workl);
                      }
          
                   } // else if (dsaup2_nconv .lt. nev .and. ishift .eq. 1)
          
                   if (dsaup2_msglvl > 0) {
                	  UI.setDataText("saup2: no. of \"converged\" Ritz values at this iter dsaup2_nconv = " + dsaup2_nconv + "\n");
                      if (dsaup2_msglvl > 1) {
                    	 UI.setDataText("In dsaup2 nev[0] = " + nev[0] + " np[0] = " + np[0] + "\n");
                         UI.setDataText("dsaup2: \"wanted\" Ritz values: \n");
                         for (i = 0; i < nev[0]; i++) {
                        	 UI.setDataText("ritz["+(np[0]+i)+"] = " + nf.format(ritz[np[0]+i]) + "\n");
                         }
                         UI.setDataText("dsaup2: Ritz estimates of the \"wanted\" values: \n");
                         for (i = 0; i < nev[0]; i++) {
                        	 UI.setDataText("bounds["+(np[0]+i)+"] = " + nf.format(bounds[np[0]+i]) + "\n");
                         }
                      } // if (dsaup2_msglvl > 1)
                   } // if (dsaup2_msglvl > 0)

           
                   if (ishift == 0) {
          
          //           %-----------------------------------------------------%
          //           | User specified shifts: reverse communication to     |
          //           | compute the shifts. They are returned in the first  |
          //           | NP locations of WORKL.                              |
          //           %-----------------------------------------------------%
          
                      dsaup2_ushift = true;
                      ido[0] = 3;
                      return;
                   } // if (ishift == 0)
        		   } // if (seg5)
        		   seg5 = true;
          
          //        %------------------------------------%
          //        | Back from reverse communication;   |
          //        | User specified shifts are returned |
          //        | in WORKL(1:*NP)                   |
          //        %------------------------------------%
          
                   dsaup2_ushift = false;
           
           
          //        %---------------------------------------------------------%
          //        | Move the NP shifts to the first NP locations of RITZ to |
          //        | free up WORKL.  This is for the non-exact shift case;   |
          //        | in the exact shift case, dsgets already handles this.   |
          //        %---------------------------------------------------------%
          
                   if (ishift == 0) {
                	   for (i = 0; i < np[0]; i++) {
                		   ritz[i] = workl[i];
                	   }
                   }
          
                   if (dsaup2_msglvl > 2) {
                	  UI.setDataText("dsaup2: The number of shifts to apply np = " + np + "\n");
                      UI.setDataText("dsaup2: shifts selected: \n");
                      for (i = 0; i < np[0]; i++) {
                    	  UI.setDataText("workl["+i+"] = " + nf.format(workl[i]) + "\n");
                      }
                      if (ishift == 1) {
                    	 UI.setDataText("dsaup2: corresponding Ritz estimates:\n");
                    	 for (i = 0; i < np[0]; i++) {
                    		 UI.setDataText("bounds["+i+"] = " + nf.format(bounds[i]) + "\n");
                    	 }
                      } // if (ishift == 1)
                   } // if (dsaup2_msglvl > 2)
           
          //        %---------------------------------------------------------%
          //        | Apply the NP0 implicit shifts by QR bulge chasing.      |
          //        | Each shift is applied to the entire tridiagonal matrix. |
          //        | The first 2*N locations of WORKD are used as workspace. |
          //        | After dsapps is done, we have a Lanczos                 |
          //        | factorization of length NEV.                            |
          //        %---------------------------------------------------------%
          
                   dsapps (n, nev[0], np[0], ritz, v, ldv, h, ldh, resid, q, ldq, workd);
          
          //        %---------------------------------------------%
          //        | Compute the B-norm of the updated residual. |
          //        | Keep B*RESID in WORKD(1:N) to be used in    |
          //        | the first step of the next call to dsaitr.  |
          //        %---------------------------------------------%
          
                   dsaup2_cnorm = true;
                   t2 = System.currentTimeMillis();
                   if (bmat.equalsIgnoreCase("G")) {
                      nbx = nbx + 1;
                      for (i = 0; i < n; i++) {
                    	  workd[n+i] = resid[i];
                      }
                      ipntr[0] = n + 1;
                      ipntr[1] = 1;
                      ido[0] = 2;
           
          //           %----------------------------------%
          //           | Exit in order to compute B*RESID |
          //           %----------------------------------%
           
                      return;
                   } // if (bmat.equalsIgnoreCase("G"))
                   else if (bmat.equalsIgnoreCase("I")) {
                	  for (i = 0; i < n; i++) {
                		  workd[i] = resid[i];
                	  }
                   }
        	   } // if (seg4) 
        	   seg4 = true;
           
          //        %----------------------------------%
          //        | Back from reverse communication; |
          //        | WORKD(1:N) := B*RESID            |
          //        %----------------------------------%
          
                   if (bmat.equalsIgnoreCase("G")) {
                	  t3 = System.currentTimeMillis();
                      tmvbx = tmvbx + (t3 - t2);
                   }
           
                   if (bmat.equalsIgnoreCase("G")) {      
                      dsaup2_rnorm[0] = ge.ddot (n, resid, 1, workd, 1);
                      dsaup2_rnorm[0] = Math.sqrt(Math.abs(dsaup2_rnorm[0]));
		           } // if (bmat.equalsIgnoreCase("G"))
		           else if (bmat.equalsIgnoreCase("I")) {
                      dsaup2_rnorm[0] = ge.dnrm2(n, resid, 1);
		           }
                   dsaup2_cnorm = false;
        
          
                   if (dsaup2_msglvl > 2) {
                	  UI.setDataText("dsaup2: B-norm of residual for NEV factorization dsaup2_rnorm[0] = " +
                	                nf.format(dsaup2_rnorm[0]) + "\n");
                      UI.setDataText("dsaup2: main diagonal of compressed H matrix: \n");
                      for (i = 0; i < nev[0]; i++) {
                    	  UI.setDataText("h["+i+"][1] = " + nf.format(h[i][1]) + "\n");
                      }
                      UI.setDataText("dsaup2: subdiagonal of compressed H matrix: \n");
                      for (i = 0; i < nev[0]-1; i++) {
                    	  UI.setDataText("h["+(i+1)+"][0] = " + nf.format(h[i+1][0]) + "\n");
                      }
                   } // if (dsaup2_msglvl > 2)
           
           } // while (true)
          
          
                } // dsaup2
                
        // -----------------------------------------------------------------------
        // \BeginDoc
        
        // \Name: dsapps
        
        // \Description:
        //  Given the Arnoldi factorization
        
        //     A*V_{k} - V_{k}*H_{k} = r_{k+p}*e_{k+p}^T,
        
        //  apply NP shifts implicitly resulting in
        
        //     A*(V_{k}*Q) - (V_{k}*Q)*(Q^T* H_{k}*Q) = r_{k+p}*e_{k+p}^T * Q
        
        //  where Q is an orthogonal matrix of order KEV+NP. Q is the product of 
        //  rotations resulting from the NP bulge chasing sweeps.  The updated Arnoldi 
        //  factorization becomes:
        
        //     A*VNEW_{k} - VNEW_{k}*HNEW_{k} = rnew_{k}*e_{k}^T.
        
        // \Usage:
        //  call dsapps
        //     ( N, KEV, NP, SHIFT, V, LDV, H, LDH, RESID, Q, LDQ, WORKD )
        
        // \Arguments
        //  N       Integer.  (INPUT)
        //          Problem size, i.e. dimension of matrix A.
        
        //  KEV     Integer.  (INPUT)
        //          INPUT: KEV+NP is the size of the input matrix H.
        //          OUTPUT: KEV is the size of the updated matrix HNEW.
        
        //  NP      Integer.  (INPUT)
        //          Number of implicit shifts to be applied.
        
        //  SHIFT   Double precision array of length NP.  (INPUT)
        //          The shifts to be applied.
        
        //  V       Double precision N by (KEV+NP) array.  (INPUT/OUTPUT)
        //          INPUT: V contains the current KEV+NP Arnoldi vectors.
        //  VNEW = V(1:n,1:KEV); the updated Arnoldi vectors
        //          are in the first KEV columns of V.
        
        //  LDV     Integer.  (INPUT)
        //          Leading dimension of V exactly as declared in the calling
        //          program.
        
        //  H       Double precision (KEV+NP) by 2 array.  (INPUT/OUTPUT)
        //          INPUT: H contains the symmetric tridiagonal matrix of the
        //          Arnoldi factorization with the subdiagonal in the 1st column
        //          starting at H(2,1) and the main diagonal in the 2nd column.
        //          OUTPUT: H contains the updated tridiagonal matrix in the 
        //          KEV leading submatrix.
        
        //  LDH     Integer.  (INPUT)
        //          Leading dimension of H exactly as declared in the calling
        //          program.
        
        //  RESID   Double precision array of length (N).  (INPUT/OUTPUT)
        //          INPUT: RESID contains the the residual vector r_{k+p}.
        //          OUTPUT: RESID is the updated residual vector rnew_{k}.
        
        //  Q       Double precision KEV+NP by KEV+NP work array.  (WORKSPACE)
        //          Work array used to accumulate the rotations during the bulge
        //          chase sweep.
        
        //  LDQ     Integer.  (INPUT)
        //          Leading dimension of Q exactly as declared in the calling
        //          program.
        
        //  WORKD   Double precision work array of length 2*N.  (WORKSPACE)
        //          Distributed array used in the application of the accumulated
        //          orthogonal matrix Q.
        
        // \EndDoc
        
        // -----------------------------------------------------------------------
        
        // \BeginLib
        
        // \Local variables:
        //     xxxxxx  real
        
        // \References:
        //  1. D.C. Sorensen, "Implicit Application of Polynomial Filters in
        //     a k-Step Arnoldi Method", SIAM J. Matr. Anal. Apps., 13 (1992),
        //     pp 357-385.
        //  2. R.B. Lehoucq, "Analysis and Implementation of an Implicitly 
        //     Restarted Arnoldi Iteration", Rice University Technical Report
        //     TR95-13, Department of Computational and Applied Mathematics.
        
        // \Routines called:
        //     ivout   ARPACK utility routine that prints integers. 
        //     second  ARPACK utility routine for timing.
        //     dvout   ARPACK utility routine that prints vectors.
        //     dlamch  LAPACK routine that determines machine constants.
        //     dlartg  LAPACK Givens rotation construction routine.
        //     dlacpy  LAPACK matrix copy routine.
        //     dlaset  LAPACK matrix initialization routine.
        //     dgemv   Level 2 BLAS routine for matrix vector multiplication.
        //     daxpy   Level 1 BLAS that computes a vector triad.
        //     dcopy   Level 1 BLAS that copies one vector to another.
        //     dscal   Level 1 BLAS that scales a vector.
        
        // \Author
        //     Danny Sorensen               Phuong Vu
        //     Richard Lehoucq              CRPC / Rice University
        //     Dept. of Computational &     Houston, Texas
        //     Applied Mathematics
        //     Rice University           
        //     Houston, Texas            
        
        // \Revision history:
        //     12/16/93: Version ' 2.1'
        
        // \SCCS Information: @(#) 
        // FILE: sapps.F   SID: 2.5   DATE OF SID: 4/19/96   RELEASE: 2
        
        // \Remarks
        //  1. In this version, each shift is applied to all the subblocks of
        //     the tridiagonal matrix H and not just to the submatrix that it 
        //     comes from. This routine assumes that the subdiagonal elements 
        //     of H that are stored in h(1:kev+np,1) are nonegative upon input
        //     and enforce this condition upon output. This version incorporates
        //     deflation. See code for documentation.
        
        // \EndLib
        
        // -----------------------------------------------------------------------
        
              private void dsapps
                (int n, int kev, int np, double shift[], double v[][], int ldv, 
                		double h[][], int ldh, double resid[], double q[][], int ldq, double workd[] ) {
        
        //     %----------------------------------------------------%
        //     | Include files for debugging and timing information |
        //     %----------------------------------------------------%
        
        //      include   'debug.h'
        //      include   'stat.h'
        
        //     %------------------%
        //     | Scalar Arguments |
        //     %------------------%
        
        //      integer    kev, ldh, ldq, ldv, n, np
        
        //     %-----------------%
        //     | Array Arguments |
        //     %-----------------%
        
        //      Double precision
        //     &           h(ldh,2), q(ldq,kev+np), resid(n), shift(np), 
        //     &           v(ldv,kev+np), workd(2*n)
        
        //     %------------%
        //     | Parameters |
        //     %------------%
        //
               final double zero = 0.0;
               final double one = 1.0;
        
        //     %---------------%
        //     | Local Scalars |
        //     %---------------%
        
              int    i, istart, itop, j, jj, kplusp, msglvl;
              int iend = 0;
              int m;
              int p;
              double a1, a2, a3, a4, big, f, g;
              double c[] = new double[1];
              double r[] = new double[1];
              double s[] = new double[1];
              boolean seg1 = true;
              double array1[];
              double array2[];
              double array2D[][];
              double temp;
        
        
        //     %----------------------%
        //     | External Subroutines |
        //     %----------------------%
        
        //      external   daxpy, dcopy, dscal, dlacpy, dlartg, dlaset, dvout, 
        //     &           ivout, second, dgemv
        
        //     %--------------------%
        //     | External Functions |
        //     %--------------------%
        
        //      Double precision
        //     &           dlamch
        //      external   dlamch
        
        //     %----------------------%
        //     | Intrinsics Functions |
        //     %----------------------%
        
        //      intrinsic  abs
        
        //     %-----------------------%
        //     | Executable Statements |
        //     %-----------------------%
        
              if (dsapps_first) {
                 dsapps_epsmch = ge.dlamch('E');
                 dsapps_first = false;
              }
              itop = 1;
        
        //     %-------------------------------%
        //     | Initialize timing statistics  |
        //     | & message level for debugging |
        //     %-------------------------------%
        
              t0 = System.currentTimeMillis();
              msglvl = msapps;
         
              kplusp = kev + np; 
         
        //     %----------------------------------------------%
        //     | Initialize Q to the identity matrix of order |
        //     | kplusp used to accumulate the rotations.     |
        //     %----------------------------------------------%
        //
              ge.dlaset ('A', kplusp, kplusp, zero, one, q, ldq);
        
        //     %----------------------------------------------%
        //     | Quick return if there are no shifts to apply |
        //     %----------------------------------------------%
        
              if (np == 0) {
            	  return;
              }
         
        //     %----------------------------------------------------------%
        //     | Apply the np shifts implicitly. Apply each shift to the  |
        //     | whole matrix and not just to the submatrix from which it |
        //     | comes.                                                   |
        //     %----------------------------------------------------------%
        
              loop1: for(jj = 1; jj <= np; jj++) {
         
                 istart = itop;
        
        //        %----------------------------------------------------------%
        //        | Check for splitting and deflation. Currently we consider |
        //        | an off-diagonal element h(i+1,1) negligible if           |
        //        |         h(i+1,1) .le. epsmch*( |h(i,2)| + |h(i+1,2)| )   |
        //        | for i=1:KEV+NP-1.                                        |
        //        | If above condition tests true then we set h(i+1,1) = 0.  |
        //        | Note that h(1:KEV+NP,1) are assumed to be non negative.  |
        //        %----------------------------------------------------------%
        
            loop2: while (true) {
        
        //        %------------------------------------------------%
        //        | The following loop exits early if we encounter |
        //        | a negligible off diagonal element.             |
        //        %------------------------------------------------%
        
                 loop3: for (i = istart; i <= kplusp-1; i++) {
                    big   = Math.abs(h[i-1][1]) + Math.abs(h[i][1]);
                    if (h[i][0] <= dsapps_epsmch*big) {
                       if (msglvl > 0) {
                    	  UI.setDataText("dsapps: deflation at row/column no. "  + i + "\n");
                          UI.setDataText("dsapps: occurred before shift number " + jj + "\n");
                          UI.setDataText("dsapps: the corresponding off diagonal element h["+i+"][0] = " +
                                           nf.format(h[i][0]) + "\n");
                       } // if (msglvl > 0)
                       h[i][0] = zero;
                       iend = i;
                       seg1 = false;
                    break loop3;
                    } // if (h[i][0] <= dsapps_epsmch*big)
                 } // loop3: for (i = istart; i <= kplusp-1; i++)
                 if (seg1) {
                     iend = kplusp;
                 }
                 seg1 = true;
        
                 if (istart < iend) {
         
        //           %--------------------------------------------------------%
        //           | Construct the plane rotation G'(istart,istart+1,theta) |
        //           | that attempts to drive h(istart+1,1) to zero.          |
        //           %--------------------------------------------------------%
        
                     f = h[istart-1][1] - shift[jj-1];
                     g = h[istart][0];
                     ge.dlartg (f, g, c, s, r);
         
        //            %-------------------------------------------------------%
        //            | Apply rotation to the left and right of H;            |
        //            | H <- G' * H * G,  where G = G(istart,istart+1,theta). |
        //            | This will create a "bulge".                           |
        //            %-------------------------------------------------------%
        
                     a1 = c[0]*h[istart-1][1]   + s[0]*h[istart][0];
                     a2 = c[0]*h[istart][0] + s[0]*h[istart][1];
                     a4 = c[0]*h[istart][1] - s[0]*h[istart][0];
                     a3 = c[0]*h[istart][0] - s[0]*h[istart-1][1];
                     h[istart-1][1]   = c[0]*a1 + s[0]*a2;
                     h[istart][1] = c[0]*a4 - s[0]*a3;
                     h[istart][0] = c[0]*a3 + s[0]*a4;
         
        //            %----------------------------------------------------%
        //            | Accumulate the rotation in the matrix Q;  Q <- Q*G |
        //            %----------------------------------------------------%
        
                     for (j = 1; j <= Math.min(istart+jj,kplusp); j++) {
                        a1            =   c[0]*q[j-1][istart-1] + s[0]*q[j-1][istart];
                        q[j-1][istart] = - s[0]*q[j-1][istart-1] + c[0]*q[j-1][istart];
                        q[j-1][istart-1]   = a1;
                     }
        
        
        //            %----------------------------------------------%
        //            | The following loop chases the bulge created. |
        //            | Note that the previous rotation may also be  |
        //            | done within the following loop. But it is    |
        //            | kept separate to make the distinction among  |
        //            | the bulge chasing sweeps and the first plane |
        //            | rotation designed to drive h(istart+1,1) to  |
        //            | zero.                                        |
        //            %----------------------------------------------%
        
                     for (i = istart+1; i <= iend-1; i++) {
         
        //               %----------------------------------------------%
        //               | Construct the plane rotation G'(i,i+1,theta) |
        //               | that zeros the i-th bulge that was created   |
        //               | by G(i-1,i,theta). g represents the bulge.   |
        //               %----------------------------------------------%
        
                        f = h[i-1][0];
                        g = s[0]*h[i][0];
        
        //               %----------------------------------%
        //               | Final update with G(i-1,i,theta) |
        //               %----------------------------------%
        
                        h[i][0] = c[0]*h[i][0];
                        ge.dlartg (f, g, c, s, r);
        
        //               %-------------------------------------------%
        //               | The following ensures that h(1:iend-1,1), |
        //               | the first iend-2 off diagonal of elements |
        //               | H, remain non negative.                   |
        //               %-------------------------------------------%
        
                        if (r[0] < zero) {
                           r[0] = -r[0];
                           c[0] = -c[0];
                           s[0] = -s[0];
                        }
         
        //               %--------------------------------------------%
        //               | Apply rotation to the left and right of H; |
        //               | H <- G * H * G',  where G = G(i,i+1,theta) |
        //               %--------------------------------------------%
        
                        h[i-1][0] = r[0];
         
                        a1 = c[0]*h[i-1][1]   + s[0]*h[i][0];
                        a2 = c[0]*h[i][0] + s[0]*h[i][1];
                        a3 = c[0]*h[i][0] - s[0]*h[i-1][1];
                        a4 = c[0]*h[i][1] - s[0]*h[i][0];
         
                        h[i-1][1]   = c[0]*a1 + s[0]*a2;
                        h[i][1] = c[0]*a4 - s[0]*a3;
                        h[i][0] = c[0]*a3 + s[0]*a4;
         
        //               %----------------------------------------------------%
        //               | Accumulate the rotation in the matrix Q;  Q <- Q*G |
        //               %----------------------------------------------------%
        
                        for (j = 1; j <= Math.min( j+jj, kplusp ); j++) {
                           a1       =   c[0]*q[j-1][i-1] + s[0]*q[j-1][i];
                           q[j-1][i] = - s[0]*q[j-1][i-1] + c[0]*q[j-1][i];
                           q[j-1][i-1]   = a1;
                        }
        
                     } // for (i = istart+1; i <= iend-1; i++)
        
                 } // if (istart < iend)
        
        //        %--------------------------%
        //        | Update the block pointer |
        //        %--------------------------%
        
                 istart = iend + 1;
        
        //        %------------------------------------------%
        //        | Make sure that h(iend,1) is non-negative |
        //        | If not then set h(iend,1) <-- -h(iend,1) |
        //        | and negate the last column of Q.         |
        //        | We have effectively carried out a        |
        //        | similarity on transformation H           |
        //        %------------------------------------------%
        
                 if (h[iend-1][0] < zero) {
                     h[iend-1][0] = -h[iend-1][0];
                     for (m = 0; m < kplusp; m++) {
                    	 q[m][iend-1] = -one * q[m][iend-1];
                     }
                 }
        
        //        %--------------------------------------------------------%
        //        | Apply the same shift to the next block if there is any |
        //        %--------------------------------------------------------%
        
                 if (iend >= kplusp) {
                	 break loop2;
                 }
            } // loop2: while (true)
        
        //        %-----------------------------------------------------%
        //        | Check if we can increase the the start of the block |
        //        %-----------------------------------------------------%
        
                 for (i = itop; i <= kplusp-1; i++) {
                    if (h[i][0] > zero) {
                    	continue loop1;
                    }
                    itop  = itop + 1;
                 }
        
        //        %-----------------------------------%
        //        | Finished applying the jj-th shift |
        //        %-----------------------------------%
        
              } // loop1: for(jj = 1; jj <= np; jj++)
        
        //     %------------------------------------------%
        //     | All shifts have been applied. Check for  |
        //     | more possible deflation that might occur |
        //     | after the last shift is applied.         |                               
        //     %------------------------------------------%
        
              for (i = itop; i <= kplusp-1; i++) {
                 big   = Math.abs(h[i-1][1]) + Math.abs(h[i][1]);
                 if (h[i][0] <= dsapps_epsmch*big) {
                    if (msglvl > 0) {
                       UI.setDataText("dsapps: deflation at row/column no. " + i + "\n");
                       UI.setDataText("dsapps: the corresponding off diagonal element h["+i+"][0] = " +
                                nf.format(h[i][0]) + "\n");
                    } // if (msglvl > 0)
                    h[i][0] = zero;
                 } // if (h[i][0] <= dsapps_epsmch*big)
              } // for (i = itop; i <= kplusp-1; i++)
        
        //     %-------------------------------------------------%
        //     | Compute the (kev+1)-st column of (V*Q) and      |
        //     | temporarily store the result in WORKD(N+1:2*N). |
        //     | This is not necessary if h(kev+1,1) = 0.         |
        //     %-------------------------------------------------%
        
              if ( h[kev][0] > zero )  {
            	 array1 = new double[kplusp];
            	 for (m = 0; m < kplusp; m++) {
            		 array1[m] = q[m][kev];
            	 }
            	 array2 = new double[n];
            	 for (m = 0; m < n; m++) {
            		 array2[m] = workd[n+m];
            	 }
                 ge.dgemv ('N', n, kplusp, one, v, ldv,
                             array1, 1, zero, array2, 1);
                 for (m = 0; m < n; m++) {
                	 workd[n+m] = array2[m];
                 }
              } // if ( h[kev][0] > zero ) 
         
        //     %-------------------------------------------------------%
        //     | Compute column 1 to kev of (V*Q) in backward order    |
        //     | taking advantage that Q is an upper triangular matrix |    
        //     | with lower bandwidth np.                              |
        //     | Place results in v(:,kplusp-kev:kplusp) temporarily.  |
        //     %-------------------------------------------------------%
        
              for (i = 1; i <= kev; i++) {
            	 array1 = new double[kplusp-i+1];
            	 for (m = 0; m < kplusp-i+1; m++) {
            		 array1[m] = q[m][kev-i];
            	 }
                 ge.dgemv ('N', n, kplusp-i+1, one, v, ldv,
                            array1, 1, zero, workd, 1);
                 for (m = 0; m < n; m++) {
                	 v[m][kplusp-i] = workd[m];
                 }
              } // for (i = 1; i <= kev; i++)
        
        //     %-------------------------------------------------%
        //     |  Move v(:,kplusp-kev+1:kplusp) into v(:,1:kev). |
        //     %-------------------------------------------------%
        
              array2D = new double[n][kev];
              for (m = 0; m < n; m++) {
            	  for (p = 0; p < kev; p++) {
            		  array2D[m][p] = v[m][np+p];
            	  }
              }
              ge.dlacpy ('A', n, kev, array2D, n, v, ldv);
         
        //     %--------------------------------------------%
        //     | Copy the (kev+1)-st column of (V*Q) in the |
        //     | appropriate place if h(kev+1,1) .ne. zero. |
        //     %--------------------------------------------%
        
              if ( h[kev][0] > zero ) {
            	  for (m = 0; m < n; m++) {
            		  v[m][kev] = workd[n+m];
            	  }
              } // if ( h[kev][0] > zero )
         
        //     %-------------------------------------%
        //     | Update the residual vector:         |
        //     |    r <- sigmak*r + betak*v(:,kev+1) |
        //     | where                               |
        //     |    sigmak = (e_{kev+p}'*Q)*e_{kev}  |
        //     |    betak = e_{kev+1}'*H*e_{kev}     |
        //     %-------------------------------------%
        
              for (m = 0; m < n; m++) {
            	  resid[m] = q[kplusp-1][kev-1] * resid[m];
              }
              if (h[kev][0] > zero) { 
            	 temp = h[kev][0];
            	 array1 = new double[n];
            	 for (m = 0; m < n; m++) {
            		 array1[m] = v[m][kev];
            	 }
                 ge.daxpy (n, temp, array1, 1, resid, 1);
              } // if (h[kev][0] > zero)
        
              if (msglvl > 1) {
            	 UI.setDataText("dsapps: sigmak of the updated residual vector q[kplusp-1][kev-1] = " +
                                  nf.format(q[kplusp-1][kev-1]) + "\n");
                 UI.setDataText("dsapps: betak of the updated residual vector h[kev][0] = " +
                                  nf.format(h[kev][0]) + "\n");
                 UI.setDataText("dsapps: updated main diagonal of H for next iteration: \n");
                 for (m = 0; m < kev; m++) {
                	 UI.setDataText("h["+m+"][1] = " + nf.format(h[m][1]) + "\n");
                 }
                 if (kev > 1) {
                	UI.setDataText("dsapps: updated sub diagonal of H for next iteration: \n");
                	for (m = 0; m < kev-1; m++) {
                		UI.setDataText("h["+(m+1)+"][0] = " + nf.format(h[m+1][0]) + "\n");
                	}
                 } // if (kev > 1)
              } // if (msglvl > 1) 
        
              t1 = System.currentTimeMillis();
              tsapps = tsapps + (t1 - t0); 
              return;
              } // dsapps
                
                
        // -----------------------------------------------------------------------
        // \BeginDoc
        
        // \Name: dsconv
        
        // \Description: 
        //  Convergence testing for the symmetric Arnoldi eigenvalue routine.
        
        // \Usage:
        //  call dsconv
        //     ( N, RITZ, BOUNDS, TOL, NCONV )
        
        // \Arguments
        //  N       Integer.  (INPUT)
        //          Number of Ritz values to check for convergence.
        
        //  RITZ    Double precision array of length N.  (INPUT)
        //          The Ritz values to be checked for convergence.
        
        //  BOUNDS  Double precision array of length N.  (INPUT)
        //          Ritz estimates associated with the Ritz values in RITZ.
        
        //  TOL     Double precision scalar.  (INPUT)
        //          Desired relative accuracy for a Ritz value to be considered
        //          "converged".
        
        //  NCONV   Integer scalar.  (OUTPUT)
        //          Number of "converged" Ritz values.
        
        // \EndDoc
        
        // -----------------------------------------------------------------------
        
        // \BeginLib
        
        // \Routines called:
        //     second  ARPACK utility routine for timing.
        //     dlamch  LAPACK routine that determines machine constants. 
        
        // \Author
        //     Danny Sorensen               Phuong Vu
        //     Richard Lehoucq              CRPC / Rice University 
        //     Dept. of Computational &     Houston, Texas 
        //     Applied Mathematics
        //     Rice University           
        //     Houston, Texas            
        
        // \SCCS Information: @(#) 
        // FILE: sconv.F   SID: 2.4   DATE OF SID: 4/19/96   RELEASE: 2
        
        // \Remarks
        //     1. Starting with version 2.4, this routine no longer uses the
        //         Parlett strategy using the gap conditions. 
        
        // \EndLib
        
        // -----------------------------------------------------------------------
        
              private void dsconv (int n, double ritz[], double bounds[], double tol, int nconv[]) {
        
        //     %----------------------------------------------------%
        //     | Include files for debugging and timing information |
        //     %----------------------------------------------------%
        
        //      include   'debug.h'
        //      include   'stat.h'
        
        //     %------------------%
        //     | Scalar Arguments |
        //     %------------------%
        
        //      integer    n, nconv
        //      Double precision
        //     &           tol
        
        //     %-----------------%
        //     | Array Arguments |
        //     %-----------------%
        
        //     Double precision
        //     &           ritz(n), bounds(n)
        
        //     %---------------%
        //     | Local Scalars |
        //     %---------------%
        
              int    i;
              double temp, eps23;
        
        //     %-------------------%
        //     | External routines |
        //     %-------------------%
        
        //      Double precision
        //    &           dlamch
        //      external   dlamch

        //     %---------------------%
        //     | Intrinsic Functions |
        //     %---------------------%
        
        //      intrinsic    abs
        
        //     %-----------------------%
        //     | Executable Statements |
        //     %-----------------------%
        
              t0 = System.currentTimeMillis();
        
              eps23 = ge.dlamch('E'); 
              eps23 = Math.pow(eps23,(2.0 / 3.0));
        
              nconv[0]  = 0;
              for (i = 0; i < n; i++) {
        
        //        %-----------------------------------------------------%
        //        | The i-th Ritz value is considered "converged"       |
        //        | when: bounds(i) .le. TOL*max(eps23, abs(ritz(i)))   |
        //        %-----------------------------------------------------%
        
                 temp = Math.max( eps23, Math.abs(ritz[i]) );
                 if ( bounds[i] <= tol*temp ) {
                    nconv[0] = nconv[0] + 1;
                 }
        
              } // for (i = 0; i < n; i++)
         
              t1 = System.currentTimeMillis();
              tsconv = tsconv + (t1 - t0);
         
              return;
              } // dsconv
                
        // -----------------------------------------------------------------------
        // \BeginDoc
       
        // \Name: dsgets
        
        // \Description: 
        //  Given the eigenvalues of the symmetric tridiagonal matrix H,
        //  computes the NP shifts AMU that are zeros of the polynomial of 
        //  degree NP which filters out components of the unwanted eigenvectors 
        //  corresponding to the AMU's based on some given criteria.
        
        //  NOTE: This is called even in the case of user specified shifts in 
        //  order to sort the eigenvalues, and error bounds of H for later use.
        
        // \Usage:
        //  call dsgets
        //     ( ISHIFT, WHICH, KEV, NP, RITZ, BOUNDS, SHIFTS )
        
        // \Arguments
        //  ISHIFT  Integer.  (INPUT)
        //          Method for selecting the implicit shifts at each iteration.
        //          ISHIFT = 0: user specified shifts
        //          ISHIFT = 1: exact shift with respect to the matrix H.
        
        //  WHICH   Character*2.  (INPUT)
        //          Shift selection criteria.
        //          'LM' -> KEV eigenvalues of largest magnitude are retained.
        //          'SM' -> KEV eigenvalues of smallest magnitude are retained.
        //          'LA' -> KEV eigenvalues of largest value are retained.
        //          'SA' -> KEV eigenvalues of smallest value are retained.
        //          'BE' -> KEV eigenvalues, half from each end of the spectrum.
        //                  If KEV is odd, compute one more from the high end.
        
        //  KEV      Integer.  (INPUT)
        //          KEV+NP is the size of the matrix H.
        
        //  NP      Integer.  (INPUT)
        //          Number of implicit shifts to be computed.
        
        //  RITZ    Double precision array of length KEV+NP.  (INPUT/OUTPUT)
        //          On INPUT, RITZ contains the eigenvalues of H.
        //          On OUTPUT, RITZ are sorted so that the unwanted eigenvalues 
        //          are in the first NP locations and the wanted part is in 
        //          the last KEV locations.  When exact shifts are selected, the
        //          unwanted part corresponds to the shifts to be applied.
        
        //  BOUNDS  Double precision array of length KEV+NP.  (INPUT/OUTPUT)
        //          Error bounds corresponding to the ordering in RITZ.
        
        //  SHIFTS  Double precision array of length NP.  (INPUT/OUTPUT)
        //          On INPUT:  contains the user specified shifts if ISHIFT = 0.
        //          On OUTPUT: contains the shifts sorted into decreasing order 
        //          of magnitude with respect to the Ritz estimates contained in
        //          BOUNDS. If ISHIFT = 0, SHIFTS is not modified on exit.
        
        // \EndDoc
        
        // -----------------------------------------------------------------------
        
        // \BeginLib
        
        // \Local variables:
        //     xxxxxx  real
        
        // \Routines called:
        //     dsortr  ARPACK utility sorting routine.
        //     ivout   ARPACK utility routine that prints integers.
        //     second  ARPACK utility routine for timing.
        //     dvout   ARPACK utility routine that prints vectors.
        //     dcopy   Level 1 BLAS that copies one vector to another.
        //     dswap   Level 1 BLAS that swaps the contents of two vectors.
        
        // \Author
        //     Danny Sorensen               Phuong Vu
        //     Richard Lehoucq              CRPC / Rice University
        //     Dept. of Computational &     Houston, Texas
        //     Applied Mathematics
        //     Rice University           
        //     Houston, Texas            
        
        // \Revision history:
        //     xx/xx/93: Version ' 2.1'
        
        // \SCCS Information: @(#) 
        // FILE: sgets.F   SID: 2.4   DATE OF SID: 4/19/96   RELEASE: 2
        
        // \Remarks
        
        // \EndLib
        
        // -----------------------------------------------------------------------
        
              private void dsgets (int ishift, String which, int kev, int np, double ritz[], double bounds[], double shifts[] ) {
        
        //     %----------------------------------------------------%
        //     | Include files for debugging and timing information |
        //     %----------------------------------------------------%
        
        //      include   'debug.h'
        //      include   'stat.h'
        
        //     %------------------%
        //     | Scalar Arguments |
        //     %------------------%
        
        //      character*2 which
        //      integer    ishift, kev, np
        
        //     %-----------------%
        //     | Array Arguments |
        //     %-----------------%
        
        //      Double precision
        //     &           bounds(kev+np), ritz(kev+np), shifts(np)
        
        //     %---------------%
        //     | Local Scalars |
        //     %---------------%
        
              int    kevd2, msglvl;
              int i;
              double temp;
        
        //     %----------------------%
        //     | External Subroutines |
        //     %----------------------%
        
        //      external   dswap, dcopy, dsortr, second
        
        //     %---------------------%
        //     | Intrinsic Functions |
        //     %---------------------%
        
        //      intrinsic    max, min
        
        //     %-----------------------%
        //     | Executable Statements |
        //     %-----------------------%
         
        //     %-------------------------------%
        //     | Initialize timing statistics  |
        //     | & message level for debugging |
        //     %-------------------------------%
        
              t0 = System.currentTimeMillis();
              msglvl = msgets;
         
              if (which.equalsIgnoreCase("BE")) {
        
        //        %-----------------------------------------------------%
        //        | Both ends of the spectrum are requested.            |
        //        | Sort the eigenvalues into algebraically increasing  |
        //        | order first then swap high end of the spectrum next |
        //        | to low end in appropriate locations.                |
        //        | NOTE: when np < floor(kev/2) be careful not to swap |
        //        | overlapping locations.                              |
        //        %-----------------------------------------------------%
        
                 dsortr ("LA", true, kev+np, ritz, bounds);
                 kevd2 = kev / 2; 
                 if ( kev > 1 ) {
                	for (i = 0; i < Math.min(kevd2, np); i++) {
                		temp = ritz[i];
                		ritz[i] = ritz[Math.max(kevd2,np)+i];
                		ritz[Math.max(kevd2,np)+i] =  temp;
                		temp = bounds[i];
                		bounds[i] = bounds[Math.max(kevd2,np)+i];
                		bounds[Math.max(kevd2,np)+i] =  temp;
                	}
                 } // if (kev > 1)
        
              } // if (which.equalsIgnoreCase("BE"))
              else {
        
        //        %----------------------------------------------------%
        //        | LM, SM, LA, SA case.                               |
        //        | Sort the eigenvalues of H into the desired order   |
        //        | and apply the resulting order to BOUNDS.           |
        //        | The eigenvalues are sorted so that the wanted part |
        //        | are always in the last KEV locations.               |
        //        %----------------------------------------------------%
        
                 dsortr (which, true, kev+np, ritz, bounds);
              }
        
              if (ishift == 1 && np > 0) {
             
        //        %-------------------------------------------------------%
        //        | Sort the unwanted Ritz values used as shifts so that  |
        //        | the ones with largest Ritz estimates are first.       |
        //        | This will tend to minimize the effects of the         |
        //        | forward instability of the iteration when the shifts  |
        //        | are applied in subroutine dsapps.                     |
        //        %-------------------------------------------------------%
             
                 dsortr ("SM", true, np, bounds, ritz);
                 for (i = 0; i < np; i++) {
                	 shifts[i] = ritz[i];
                 }
              } // if (ishift == 1 && np > 0)
         
              t1 = System.currentTimeMillis();
              tsgets = tsgets + (t1 - t0);
        
              if (msglvl > 0) {
            	 UI.setDataText("dsgets: kev = " + kev + "\n");
            	 UI.setDataText("dsgets: np = " + np + "\n");
            	 UI.setDataText("dsgets: Eigenvalues of current H matrix: \n");
            	 for (i = 0; i < kev+np; i++) {
            		 UI.setDataText("ritz["+i+"] = " + nf.format(ritz[i]) + "\n");
            	 }
                 UI.setDataText("dsgets: Associated Ritz estimates: \n");
                 for (i = 0; i < kev+np ; i++) {
                	 UI.setDataText("bounds["+i+"] = " + nf.format(bounds[i]) + "\n");
                 }
              } // if (msglvl > 0)
        
              return;
              } // dsgets
              
      // -----------------------------------------------------------------------
      // \BeginDoc
      // 
      // \Name: dsortr
      
      // \Description:
      //  Sort the array X1 in the order specified by WHICH and optionally 
      //  applies the permutation to the array X2.
      
      // \Usage:
      //  call dsortr
      //     ( WHICH, APPLY, N, X1, X2 )
      
      // \Arguments
      //   Character*2.  (Input)
      //          'LM' -> X1 is sorted into increasing order of magnitude.
      //          'SM' -> X1 is sorted into decreasing order of magnitude.
      //          'LA' -> X1 is sorted into increasing order of algebraic.
      //          'SA' -> X1 is sorted into decreasing order of algebraic.
      
      //  APPLY   Logical.  (Input)
      //          APPLY = .TRUE.  -> apply the sorted order to X2.
      //          APPLY = .FALSE. -> do not apply the sorted order to X2.
      
      //  N       Integer.  (INPUT)
      //          Size of the arrays.
      
      //  X1      Double precision array of length N.  (INPUT/OUTPUT)
      //          The array to be sorted.
      
      //  X2      Double precision array of length N.  (INPUT/OUTPUT)
      //          Only referenced if APPLY = .TRUE.
      
      // \EndDoc
      
      // -----------------------------------------------------------------------
      
      // \BeginLib
      
      // \Author
      //     Danny Sorensen               Phuong Vu
      //     Richard Lehoucq              CRPC / Rice University 
      //     Dept. of Computational &     Houston, Texas 
      //     Applied Mathematics
      //     Rice University           
      //     Houston, Texas            
      
      // \Revision history:
      //     12/16/93: Version ' 2.1'.
      //               Adapted from the sort routine in LANSO.
      
      // \SCCS Information: @(#) 
      // FILE: sortr.F   SID: 2.3   DATE OF SID: 4/19/96   RELEASE: 2
      
      // \EndLib
      
      // -----------------------------------------------------------------------
      
            private void dsortr (String which, boolean apply, int n, double x1[], double x2[]) {
      
      //     %------------------%
      //     | Scalar Arguments |
      //     %------------------%
      
      //      character*2 which
      //      logical    apply
      //      integer    n
      
      //     %-----------------%
      //     | Array Arguments |
      //     %-----------------%
      
      //      Double precision
      //     &           x1(0:n-1), x2(0:n-1)
      
      //     %---------------%
      //     | Local Scalars |
      //     %---------------%
      
            int    i, igap, j;
            double temp;
      
      //     %-----------------------%
      //     | Executable Statements |
      //     %-----------------------%
      
            igap = n / 2;
       
            if (which.equalsIgnoreCase("SA")) {
      
      //        X1 is sorted into decreasing order of algebraic.
      
         while (true) {
               if (igap == 0) {
            	   return;
               }
            loop2: for (i = igap; i <= n-1; i++) {
                  j = i-igap;
            while (true) {
      
                  if (j < 0) {
                	  continue loop2;
                  }
      
                  if (x1[j] < x1[j+igap]) {
                     temp = x1[j];
                     x1[j] = x1[j+igap];
                     x1[j+igap] = temp;
                     if (apply) {
                        temp = x2[j];
                        x2[j] = x2[j+igap];
                        x2[j+igap] = temp;
                     } // if (apply)
                  } // if (x1[j] < x1[j+igap])
                  else {
                     continue loop2;
                  }
                  j = j-igap;
                  } // while (true) 
               } // loop2: for (i = igap; i <= n-1; i++)
               igap = igap / 2;
         } // while (true)
            } // if (which.equalsIgnoreCase("SA"))
            else if (which.equalsIgnoreCase("SM")) { 
      
      //        X1 is sorted into decreasing order of magnitude.
      
         while (true) {
               if (igap == 0) {
            	   return;
               }
               loop5: for (i = igap; i <= n-1; i++) {
                  j = i-igap;
                  while(true) {
      
                  if (j < 0) {
                	  continue loop5;
                  }
      
                  if (Math.abs(x1[j]) < Math.abs(x1[j+igap])) {
                     temp = x1[j];
                     x1[j] = x1[j+igap];
                     x1[j+igap] = temp;
                     if (apply) {
                        temp = x2[j];
                        x2[j] = x2[j+igap];
                        x2[j+igap] = temp;
                     } // if (apply)
                  } // if (Math.abs(x1[j]) < Math.abs(x1[j+igap]))
                  else {
                     continue loop5;
                  }
                  j = j-igap;
               } // while(true)
               } // loop5: for (i = igap; i <= n-1; i++) 
               igap = igap / 2;
         } // while (true);
            } // else if (which.equalsIgnoreCase("SM"))
            else if (which.equalsIgnoreCase("LA")) {
      
      //        X1 is sorted into increasing order of algebraic.
      
         while (true) {
               if (igap == 0) {
            	   return;
               }
               loop8: for (i = igap; i <= n-1; i++) {
                  j = i-igap;
                  while (true) {
      
                  if (j < 0) {
                	  continue loop8;
                  }
                
                  if (x1[j] > x1[j+igap]) {
                     temp = x1[j];
                     x1[j] = x1[j+igap];
                     x1[j+igap] = temp;
                     if (apply) {
                        temp = x2[j];
                        x2[j] = x2[j+igap];
                        x2[j+igap] = temp;
                     } // if (apply)
                  } // if (x1[j] > x1[j+igap])
                  else {
                     continue loop8;
                  }
                  j = j-igap;
               } // while (true)
               } // loop8: for (i = igap; i <= n-1; i++)
               igap = igap / 2;
         } // while (true)
            } // else if (which.equalsIgnoreCase("LA")) 
            else if (which.equalsIgnoreCase("LM")) {
      
      //        X1 is sorted into increasing order of magnitude.
      
        while (true) {
               if (igap == 0) {
            	   return;
               }
               loop11: for(i = igap; i <= n-1; i++) {
                  j = i-igap;
               while (true) {
      
                  if (j < 0) {
                	  continue loop11;
                  }
      
                  if (Math.abs(x1[j]) > Math.abs(x1[j+igap])) {
                     temp = x1[j];
                     x1[j] = x1[j+igap];
                     x1[j+igap] = temp;
                     if (apply) {
                        temp = x2[j];
                        x2[j] = x2[j+igap];
                        x2[j+igap] = temp;
                     } // if (apply)
                  } // if (Math.abs(x1[j]) > Math.abs(x1[j+igap]))
                  else {
                     continue loop11;
                  }
                  j = j-igap;
                  } // while (true)
               } // loop11: for(i = igap; i <= n-1; i++)
               igap = igap / 2;
        } // while (true)
            } // else if (which.equalsIgnoreCase("LM"))
            } // dsortr
             

                
        // -----------------------------------------------------------------------
        // \BeginDoc
        
        // \Name: dseigt
        
        // \Description: 
        //  Compute the eigenvalues of the current symmetric tridiagonal matrix
        //  and the corresponding error bounds given the current residual norm.
        
        // \Usage:
        //  call dseigt
        //      ( RNORM, N, H, LDH, EIG, BOUNDS, WORKL, IERR )
        
        // \Arguments
        //  RNORM   Double precision scalar.  (INPUT)
        //          RNORM contains the residual norm corresponding to the current
        //          symmetric tridiagonal matrix H.
        
        //  N       Integer.  (INPUT)
        //          Size of the symmetric tridiagonal matrix H.
        
        //  H       Double precision N by 2 array.  (INPUT)
        //          H contains the symmetric tridiagonal matrix with the 
        //          subdiagonal in the first column starting at H(2,1) and the 
        //          main diagonal in second column.
        
        //  LDH     Integer.  (INPUT)
        //          Leading dimension of H exactly as declared in the calling 
        //          program.
        
        //  EIG     Double precision array of length N.  (OUTPUT)
        //          On output, EIG contains the N eigenvalues of H possibly 
        //          unsorted.  The BOUNDS arrays are returned in the
        //          same sorted order as EIG.
        
        //  BOUNDS  Double precision array of length N.  (OUTPUT)
        //          On output, BOUNDS contains the error estimates corresponding
        //          to the eigenvalues EIG.  This is equal to RNORM times the
        //          last components of the eigenvectors corresponding to the
        //          eigenvalues in EIG.
        
        //  WORKL   Double precision work array of length 3*N.  (WORKSPACE)
        //          Private (replicated) array on each PE or array allocated on
        //          the front end.
        
        //  IERR    Integer.  (OUTPUT)
        //          Error exit flag from dstqrb.
        
        // \EndDoc
        
        // -----------------------------------------------------------------------
        
        // \BeginLib
        
        // \Local variables:
        //     xxxxxx  real
        
        // \Routines called:
        //     dstqrb  ARPACK routine that computes the eigenvalues and the
        //             last components of the eigenvectors of a symmetric
        //             and tridiagonal matrix.
        //     second  ARPACK utility routine for timing.
        //     dvout   ARPACK utility routine that prints vectors.
        //     dcopy   Level 1 BLAS that copies one vector to another.
        
        // \Author
        //     Danny Sorensen               Phuong Vu
        //     Richard Lehoucq              CRPC / Rice University 
        //     Dept. of Computational &     Houston, Texas 
        //     Applied Mathematics
        //     Rice University           
        //     Houston, Texas            
        
        // \Revision history:
        //     xx/xx/92: Version ' 2.4'
        
        // \SCCS Information: @(#) 
        // FILE: seigt.F   SID: 2.4   DATE OF SID: 8/27/96   RELEASE: 2
        
        // \Remarks
        //     None
        
        // \EndLib
        
        // -----------------------------------------------------------------------
        
              private void dseigt 
                ( double rnorm, int n, double h[][], int ldh, double eig[], double bounds[], double workl[], int ierr[] ) {
        
        //     %----------------------------------------------------%
        //     | Include files for debugging and timing information |
        //     %----------------------------------------------------%
        
        //      include   'debug.h'
        //      include   'stat.h'
        
        //     %------------------%
        //     | Scalar Arguments |
        //     %------------------%
        
        //      integer    ierr, ldh, n
        //      Double precision
        //     &           rnorm
        
        //     %-----------------%
        //     | Array Arguments |
        //     %-----------------%
        
        //      Double precision
        //     &           eig(n), bounds(n), h(ldh,2), workl(3*n)
        
        //     %---------------%
        //     | Local Scalars |
        //     %---------------%
        
              int    i, k, msglvl;
        
        //     %----------------------%
        //     | External Subroutines |
        //     %----------------------%
        
        //      external   dcopy, dstqrb, dvout, second
        
        //     %-----------------------%
        //     | Executable Statements |
        //     %-----------------------%
        
        //     %-------------------------------%
        //     | Initialize timing statistics  |
        //     | & message level for debugging |
        //     %-------------------------------% 
        
              t0 = System.currentTimeMillis();
              msglvl = mseigt;
        
              if (msglvl > 0) {
            	 UI.setDataText("dseigt: main diagonal of matrix H\n");
            	 for (i = 0; i < n; i++) {
            		 UI.setDataText("h["+i+"][1] = " + h[i][1] + "\n");
            	 }
                 if (n > 1) {
                	 UI.setDataText("dseigt: sub diagonal of matrix H\n");
                	 for (i = 0; i < n-1; i++) {
                		 UI.setDataText("h["+(i+1)+"][0] = " + h[i+1][0] + "\n");
                	 }
                 } // if (n > 1)
              } // if (msglvl > 0)
        
              for (i = 0; i < n; i++) {
            	  eig[i] = h[i][1];
              }
              for (i = 0; i < n-1; i++) {
            	  workl[i] = h[i+1][0];
              }
              double buffer[] = new double[Math.max(1, 2*n-2)];
              dstqrb (n, eig, workl, bounds, buffer, ierr);
              for (i = 0; i < Math.max(1, 2*n-2); i++) {
            	  workl[n+i] = buffer[i];
              }
              if (ierr[0] != 0) {
            	  return;
              }
              if (msglvl > 1) {
            	 UI.setDataText("dseigt: last row of the eigenvector matrix for H\n");
            	 for (i = 0; i < n; i++) {
            		 UI.setDataText("bounds["+i+"] = " + bounds[i] + "\n");
            	 }
              }
        
        //     %-----------------------------------------------%
        //     | Finally determine the error bounds associated |
        //     | with the n Ritz values of H.                  |
        //     %-----------------------------------------------%
        
              for (k = 0; k < n; k++) {
                 bounds[k] = rnorm*Math.abs(bounds[k]);
              }
         
              t1 = System.currentTimeMillis();
              tseigt = tseigt + (t1 - t0);
        
              return;
              } // dseigt
              
              
      // -----------------------------------------------------------------------
      // \BeginDoc
      
      // \Name: dstqrb
      
      // \Description:
      //  Computes all eigenvalues and the last component of the eigenvectors
      //  of a symmetric tridiagonal matrix using the implicit QL or QR method.
      
      //  This is mostly a modification of the LAPACK routine dsteqr.
      //  See Remarks.
      
      // \Usage:
      //  call dstqrb
      //     ( N, D, E, Z, WORK, INFO )
      
      // \Arguments
      //  N       Integer.  (INPUT)
      //          The number of rows and columns in the matrix.  N >= 0.
      
      //  D       Double precision array, dimension (N).  (INPUT/OUTPUT)
      //          On entry, D contains the diagonal elements of the
      //          tridiagonal matrix.
      //          On exit, D contains the eigenvalues, in ascending order.
      //          If an error exit is made, the eigenvalues are correct
      //          for indices 1,2,...,INFO-1, but they are unordered and
      //          may not be the smallest eigenvalues of the matrix.
      
      //  E       Double precision array, dimension (N-1).  (INPUT/OUTPUT)
      //          On entry, E contains the subdiagonal elements of the
      //          tridiagonal matrix in positions 1 through N-1.
      //          On exit, E has been destroyed.
      
      //  z       Double precision array, dimension (N).  (OUTPUT)
      //          On exit, Z contains the last row of the orthonormal 
      //          eigenvector matrix of the symmetric tridiagonal matrix.  
      //          If an error exit is made, Z contains the last row of the
      //          eigenvector matrix associated with the stored eigenvalues.
      
      //  WORK    Double precision array, dimension (max(1,2*N-2)).  (WORKSPACE)
      //          Workspace used in accumulating the transformation for 
      //          computing the last components of the eigenvectors.
      
      //  INFO    Integer.  (OUTPUT)
      //          = 0:  normal return.
      //          < 0:  if INFO = -i, the i-th argument had an illegal value.
      //          > 0:  if INFO = +i, the i-th eigenvalue has not converged
      //                              after a total of  30*N  iterations.
      
      // \Remarks
      //  1. None.
      
      // -----------------------------------------------------------------------
      
      // \BeginLib
      
      // \Local variables:
      //     xxxxxx  real
      
      // \Routines called:
      //     daxpy   Level 1 BLAS that computes a vector triad.
      //     dcopy   Level 1 BLAS that copies one vector to another.
      //     dswap   Level 1 BLAS that swaps the contents of two vectors.
      //     lsame   LAPACK character comparison routine.
      //     dlae2   LAPACK routine that computes the eigenvalues of a 2-by-2 
      //             symmetric matrix.
      //     dlaev2  LAPACK routine that eigendecomposition of a 2-by-2 symmetric 
      //             matrix.
      //     dlamch  LAPACK routine that determines machine constants.
      //     dlanst  LAPACK routine that computes the norm of a matrix.
      //     dlapy2  LAPACK routine to compute sqrt(x**2+y**2) carefully.
      //     dlartg  LAPACK Givens rotation construction routine.
      //     dlascl  LAPACK routine for careful scaling of a matrix.
      //     dlaset  LAPACK matrix initialization routine.
      //     dlasr   LAPACK routine that applies an orthogonal transformation to 
      //             a matrix.
      //     dlasrt  LAPACK sorting routine.
      //     dsteqr  LAPACK routine that computes eigenvalues and eigenvectors
      //             of a symmetric tridiagonal matrix.
      //     xerbla  LAPACK error handler routine.
      
      // \Authors
      //     Danny Sorensen               Phuong Vu
      //     Richard Lehoucq              CRPC / Rice University
      //     Dept. of Computational &     Houston, Texas
      //     Applied Mathematics
      //     Rice University           
      //     Houston, Texas            
      
      // \SCCS Information: @(#) 
      // FILE: stqrb.F   SID: 2.5   DATE OF SID: 8/27/96   RELEASE: 2
      //
      // \Remarks
      //     1. Starting with version 2.5, this routine is a modified version
      //        of LAPACK version 2.0 subroutine SSTEQR. No lines are deleted,
      //        only commeted out and new lines inserted.
      //        All lines commented out have "c$$$" at the beginning.
      //        Note that the LAPACK version 1.0 subroutine SSTEQR contained
      //        bugs. 
      
      // \EndLib
      
      // -----------------------------------------------------------------------
      
            private void dstqrb (int n, double d[], double e[], double z[], double work[], int info[] ) {
      
      //     %------------------%
      //     | Scalar Arguments |
      //     %------------------%
      
      //      integer    info, n
      
      //     %-----------------%
      //     | Array Arguments |
      //     %-----------------%
      
      //      Double precision
      //     &           d( n ), e( n-1 ), z( n ), work( 2*n-2 )
      
      //     .. parameters ..
            final double zero = 0.0;
            final double one = 1.0;
            final double two = 2.0;
            final double three = 3.0;
            final int maxit = 30;
            
      //     .. local scalars ..
           int            i, icompz, ii, iscale, j, jtot, k, l, l1, lend,
                              lendm1, lendp1, lendsv, lm1, lsv, m, mm, mm1,
                              nm1, nmaxit; 
           double c[] = new double[1];
           double r[] = new double[1];
           double rt1[] = new double[1];
           double rt2[] = new double[1];
           double s[] = new double[1];
           double         anorm, b, eps, eps2, f, g, p,
                          safmax, safmin, ssfmax, ssfmin, tst;
           double vector1[];
           double vector2[];
           int ptr1;
           double array1[][];
           double val;
     
      //     .. external functions ..
      //      logical            lsame
      //      Double precision
      //     &                   dlamch, dlanst, dlapy2
      //      external           lsame, dlamch, dlanst, dlapy2
    
      //     .. external subroutines ..
      //      external           dlae2, dlaev2, dlartg, dlascl, dlaset, dlasr,
      //     &                   dlasrt, dswap, xerbla
   
      //     .. intrinsic functions ..
      //      intrinsic          abs, max, sign, sqrt
    
      //     .. executable statements ..
      
      //     test the input parameters.
      
            info[0] = 0;
      
      //$$$      IF( LSAME( COMPZ, 'N' ) ) THEN
      //$$$         ICOMPZ = 0
      //$$$      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
      //$$$         ICOMPZ = 1
      //$$$      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
      //$$$         ICOMPZ = 2
      //$$$      ELSE
      //$$$         ICOMPZ = -1
      //$$$      END IF
      //$$$      IF( ICOMPZ.LT.0 ) THEN
      //$$$         INFO = -1
      //$$$      ELSE IF( N.LT.0 ) THEN
      //$$$         INFO = -2
      //$$$      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1,
      //$$$     $         N ) ) ) THEN
      //$$$         INFO = -6
      //$$$      END IF
      //$$$      IF( INFO.NE.0 ) THEN
      //$$$         CALL XERBLA( 'SSTEQR', -INFO )
      //$$$         RETURN
      //$$$      END IF
      
      //    *** New starting with version 2.5 ***
      
            icompz = 2;
      //    *************************************
      
      //     quick return if possible
      
            if( n == 0 ) {
              return;
            }
      
            if( n == 1 ) {
               if( icompz == 2 ) {
            	   z[0] = one;
               }
               return;
            }
      
      //     determine the unit roundoff and over/underflow thresholds.
      
            eps = ge.dlamch( 'E' );
            eps2 = eps*eps;
            safmin = ge.dlamch( 'S' );
            safmax = one / safmin;
            ssfmax = Math.sqrt( safmax ) / three;
            ssfmin = Math.sqrt( safmin ) / eps2;
      
      //     compute the eigenvalues and eigenvectors of the tridiagonal
      //     matrix.
      
      //$$      if( icompz.eq.2 )
      //$$$     $   call dlaset( 'full', n, n, zero, one, z, ldz )
      
      //     *** New starting with version 2.5 ***
      
            if ( icompz == 2 ) {
               for (j = 0; j < n-1; j++) {
            	   z[j] = zero;
               }
               z[ n-1 ] = one;
            }
      //     *************************************
      
            nmaxit = n*maxit;
            jtot = 0;
      
      //     determine where the matrix splits and choose ql or qr iteration
      //     for each block, according to whether top or bottom diagonal
      //     element is smaller.
      
            l1 = 1;
            nm1 = n - 1;
            primary: {
            	// loop1:
                do {
                	 // loop2:
                    do {
                    	 // loop3:
                        do {
                        	if (l1 > n) {
                                break primary;
                            } // if (l1 > n)
                        	if (l1 > 1) {
                                e[l1 - 2] = 0.0;
                            } // if (l1 > 1)
                        	
                        	set4: {
                            	if (l1 <= nm1) {
                            		for (m = l1; m <= nm1; m++) {
                            			tst = Math.abs(e[m - 1]);
                            			 if (tst == 0.0) {
                                             break set4;
                                         } // if (test == 0.0)
                            			 if (tst <= (Math.sqrt(Math.abs(d[m - 1])) * Math.sqrt(Math.abs(d[m])) * eps)) {
                                             e[m - 1] = 0.0;

                                             break set4;
                                         } // if (tst <= (Math.sqrt(Math.abs(d[m-1])) *
                            		} // for (m = L1; m <= nm1; m++)
                            	 } // if (l1 <= nm1)
                                 m = n;
                        	} // set4
      
				            l = l1;
				            lsv = l;
				            lend = m;
				            lendsv = lend;
				            l1 = m + 1;
                        } // loop3
                        while (lend == l);
                        // Scale submatrix in rows and columns l-1 to lend-1.
                        vector1 = new double[lend - l + 1];

                        for (ptr1 = 0; ptr1 < (lend - l + 1); ptr1++) {
                            vector1[ptr1] = d[ptr1 + l - 1];
                        }

                        vector2 = new double[lend - l];

                        for (ptr1 = 0; ptr1 < (lend - l); ptr1++) {
                            vector2[ptr1] = e[ptr1 + l - 1];
                        }

                        anorm = ge.dlanst('I', lend - l + 1, vector1, vector2);
                        iscale = 0;
                    } // loop2
                    while (anorm == 0.0);
                    if (anorm > ssfmax) {
                        iscale = 1;
                        array1 = new double[lend - l + 1][1];

                        for (ptr1 = 0; ptr1 < (lend - l + 1); ptr1++) {
                            array1[ptr1][0] = d[ptr1 + l - 1];
                        }

                        ge.dlascl('G', 0, 0, anorm, ssfmax, lend - l + 1, 1, array1, lend - l + 1, info);

                        if (info[0] != 0) {
                            UI.setDataText("dstqrb call to dlascl #1 had info[0] = " + info[0] + "\n");
                        }

                        for (ptr1 = 0; ptr1 < (lend - l + 1); ptr1++) {
                            d[ptr1 + l - 1] = array1[ptr1][0];
                        }

                        array1 = new double[lend - l][1];

                        for (ptr1 = 0; ptr1 < (lend - l); ptr1++) {
                            array1[ptr1][0] = e[ptr1 + l - 1];
                        }

                        ge.dlascl('G', 0, 0, anorm, ssfmax, lend - l, 1, array1, lend - l, info);

                        if (info[0] != 0) {
                            UI.setDataText("dstqrb call to dlascl #2 had info[0] = " + info[0] + "\n");
                        }

                        for (ptr1 = 0; ptr1 < (lend - l); ptr1++) {
                            e[ptr1 + l - 1] = array1[ptr1][0];
                        }
                    } // if (anorm > ssfmax)
                    else if (anorm < ssfmin) {
                        iscale = 2;
                        array1 = new double[lend - l + 1][1];

                        for (ptr1 = 0; ptr1 < (lend - l + 1); ptr1++) {
                            array1[ptr1][0] = d[ptr1 + l - 1];
                        }

                        ge.dlascl('G', 0, 0, anorm, ssfmin, lend - l + 1, 1, array1, lend - l + 1, info);

                        if (info[0] != 0) {
                            UI.setDataText("dstqrb call to dlascl #3 had info[0] = " + info[0] + "\n");
                        }

                        for (ptr1 = 0; ptr1 < (lend - l + 1); ptr1++) {
                            d[ptr1 + l - 1] = array1[ptr1][0];
                        }

                        array1 = new double[lend - l][1];

                        for (ptr1 = 0; ptr1 < (lend - l); ptr1++) {
                            array1[ptr1][0] = e[ptr1 + l - 1];
                        }

                        ge.dlascl('G', 0, 0, anorm, ssfmin, lend - l, 1, array1, lend - l, info);

                        if (info[0] != 0) {
                            UI.setDataText("dstqrb call to dlascl #4 had info[0] = " + info[0] + "\n");
                        }

                        for (ptr1 = 0; ptr1 < (lend - l); ptr1++) {
                            e[ptr1 + l - 1] = array1[ptr1][0];
                        }
                    } // else if (anorm < ssfmin)
                    // Choose between QL and QR iteration
                    if (Math.abs(d[lend - 1]) < Math.abs(d[l - 1])) {
                        lend = lsv;
                        l = lendsv;
                    } // if (Math.abs(d[lend-1]) < Math.abs(d[l-1]))
                    
                    set5: {
      
            if( lend > l ) {
            	// QL iteration
                // Look for small subdiagonal element
                // loop6:
                do {
                	loop7: do {
                		set8: {

                		if (l != lend) {
                			lendm1 = lend - 1;

                            for (m = l; m <= lendm1; m++) {
                                tst = e[m - 1] * e[m - 1];

                                if (tst <= ( (eps2 * Math.abs(d[m - 1]) * Math.abs(d[m])) + safmin)) {
                                    break set8;
                                } // if (tst <= (eps2 * Math.abs(d[m-1]) *
                            } // for (m = l; m <= lendm1; m++)
                		} // if (l != lend) 
      
               m = lend;
                	} // set8
      
                		 if (m < lend) {
                             e[m - 1] = zero;
                         } // if (m < lend)

                         p = d[l - 1];
              
            		   if (m == l) {
                           break loop7;
                       } // if (m == l)
      
      //        if remaining matrix is 2-by-2, use dlae2 or dlaev2
      //        to compute its eigensystem.
      
               if( m == l+1 ) {
                  if( icompz > 0 ) {
                	 ge.dlaev2(d[l - 1], e[l - 1], d[l], rt1, rt2, c, s);
                     work[ l-1 ] = c[0];
                     work[ n-2+l ] = s[0];
      //$$$               call dlasr( 'r', 'v', 'b', n, 2, work( l ),
      //$$$     $                     work( n-1+l ), z( 1, l ), ldz )
      //
      //              *** New starting with version 2.5 ***
      //
                     tst      = z[l];
                     z[l] = c[0]*tst - s[0]*z[l-1];
                     z[l-1]   = s[0]*tst + c[0]*z[l-1];
      //              *************************************
                  } // if (icompz > 0)
                  else {
                     ge.dlae2( d[l -1], e[l-1], d[l], rt1, rt2 );
                  }
                  d[l-1] = rt1[0];
                  d[l] = rt2[0];
                  e[l-1] = zero;
                  l = l + 2;
                		  if (l <= lend) {
                              continue loop7;
                          }
                  break set5;
               } // if( m == l+1 ) 
      
                  if (jtot == nmaxit) {
                      break set5;
                  } // if (jtot == nmaxit)
               jtot = jtot + 1;
      
      //        form shift.
     
               g = ( d[l]-p ) / ( two*e[l-1] );
               r[0] = ge.dlapy2( g, one );
               if (g >= 0) {
                   val = Math.abs(r[0]);
               } else {
                   val = -Math.abs(r[0]);
               }

               g = d[m - 1] - p + (e[l - 1] / (g + val));
               s[0] = one;
               c[0] = one;
               p = zero;
               
               // Inner loop
               mm1 = m - 1;
               vector1 = new double[m - l];
               vector2 = new double[m - l];

               for (i = mm1; i >= l; i--) {
                   f = s[0] * e[i - 1];
                   b = c[0] * e[i - 1];
                   ge.dlartg(g, f, c, s, r);

                   if (i != (m - 1)) {
                       e[i] = r[0];
                   } // if (i != (m-1))

                   g = d[i] - p;
                   r[0] = ( (d[i - 1] - g) * s[0]) + (2.0 * c[0] * b);
                   p = s[0] * r[0];
                   d[i] = g + p;
                   g = (c[0] * r[0]) - b;

                   // If eigenvectors are desired, then save rotations
                   if (icompz > 0) {
                       vector1[i - l] = c[0];
                       vector2[i - l] = -s[0];
                   } // if (icompz > 0)
               } // for (i = mm1; i >= l; i--)

               // If eigenvectors are desired, then apply saved rotations.
               if (icompz > 0) {
                   mm = m - l + 1;
                   array1 = new double[1][mm];
                   
                   for (ptr1 = 0; ptr1 < mm; ptr1++) {
                	   array1[0][ptr1] = z[l-1+ptr1];
                   }

                   ge.dlasr('R', 'V', 'B', 1, mm, vector1, vector2, array1, 1);

                   for (ptr1 = 0; ptr1 < mm; ptr1++) {
                       z[l-1+ptr1] = array1[0][ptr1];
                   }
               } // if (icompz > 0)

               d[l - 1] = d[l - 1] - p;
               e[l - 1] = g;
      
                	} // loop7
                    while (true);
      
      //        eigenvalue found.
      
               d[l-1] = p;
      
               l = l + 1;
                } // loop6
                while (l <= lend);
            } // if (lend > l)
            else {
            	// QR iteration
                // Look for small superdiagonal element
                // loop9:
                do {
                	loop10: do {
                		set11: {
                		 if (l != lend) {
                             lendp1 = lend + 1;

                             for (m = l; m >= lendp1; m--) {
                                 tst = e[m - 2] * e[m - 2];

                                 if (tst <= ( (eps2 * Math.abs(d[m - 1]) * Math.abs(d[m - 2])) + safmin)) {
                                     break set11;
                                 } // if (tst <= (eps2 * Math.abs(d[m-1]) *
                             } // for (m = l; m >= lendp1; m--)
                         } // if (l != lend)
                	
               m = lend;
                	} // set11
                	if (m > lend) {
                        e[m - 2] = 0.0;
                    } // if (m > lend)

                    p = d[l - 1];
            		   if (m == l) {
                           break loop10;
                       } // if (m == l)
      
      //        if remaining matrix is 2-by-2, use dlae2 or dlaev2
      //        to compute its eigensystem.
      
               if( m == l-1 ) {
                  if( icompz > 0 ) {
                     ge.dlaev2( d[l-2], e[ l-2], d[ l-1], rt1, rt2, c, s );
      //$$$               work( m ) = c
      //$$$               work( n-1+m ) = s
      //$$$               call dlasr( 'r', 'v', 'f', n, 2, work( m ),
      //$$$     $                     work( n-1+m ), z( 1, l-1 ), ldz )
      
      //               *** New starting with version 2.5 ***
      
                      tst      = z[l-1];
                      z[l-1]   = c[0]*tst - s[0]*z[l-2];
                      z[l-2] = s[0]*tst + c[0]*z[l-2];
      //               ************************************* 
                  } // if (icompz > 0)
                  else {
                     ge.dlae2( d[l-2], e[l-2], d[l-1], rt1, rt2 );
                  }
                  d[l-2] = rt1[0];
                  d[l-1] = rt2[0];
                  e[ l-2] = 0.0;
                  l = l - 2;
                		  if (l >= lend) {
                              continue loop10;
                          } // if (l >= lend)

                          break set5;
                  
               } // if( m == l-1 )
      
               if( jtot == nmaxit ) {
                   break set5;
               }
               jtot = jtot + 1;
      
      //        form shift.
      
               g = ( d[ l-2 ]-p ) / ( two*e[ l-2 ] );
               r[0] = ge.dlapy2( g, one );
               if (g >= 0.0) {
                   val = Math.abs(r[0]);
               } else {
                   val = -Math.abs(r[0]);
               }

               g = d[m - 1] - p + (e[l - 2] / (g + val));

               s[0] = one;
               c[0] = one;
               p = zero;
               
               // Inner loop
               lm1 = l - 1;
               vector1 = new double[l - m];
               vector2 = new double[l - m];

               for (i = m; i <= lm1; i++) {
                   f = s[0] * e[i - 1];
                   b = c[0] * e[i - 1];
                   ge.dlartg(g, f, c, s, r);

                   if (i != m) {
                       e[i - 2] = r[0];
                   } // if (i != m)

                   g = d[i - 1] - p;
                   r[0] = ( (d[i] - g) * s[0]) + (2.0 * c[0] * b);
                   p = s[0] * r[0];
                   d[i - 1] = g + p;
                   g = (c[0] * r[0]) - b;

                   // If eigenvalues are desired, then save rotations
                   if (icompz > 0) {
                       vector1[i - m] = c[0];
                       vector2[i - m] = s[0];
                   } // if (icompz > 0)
               } // for (i = m; i <= lm1; i++)
               
            // If eigenvectors are desired, then apply saved rotations
               if (icompz > 0) {
                   mm = l - m + 1;
                   array1 = new double[1][mm];

                   for (ptr1 = 0; ptr1 < mm; ptr1++) {
                       array1[0][ptr1] = z[m-1+ptr1];
                   }

                   ge.dlasr('R', 'V', 'F', 1, mm, vector1, vector2, array1, 1);

                   for (ptr1 = 0; ptr1 < mm; ptr1++) {
                       z[m-1+ptr1] = array1[0][ptr1];
                   }
               } // if (icompz > 0)

               d[l - 1] = d[l - 1] - p;
               e[lm1 - 1] = g;
      
                	} // loop10
                    while (true);
      
      //        eigenvalue found.
      
               d[ l-1 ] = p;
               l = l - 1;
                } // loop9
                while (l >= lend);
            } // else QR iteration
                    } // set5
         // Undo scaling if necessary
            if (iscale == 1) {
                array1 = new double[lendsv - lsv + 1][1];

                for (ptr1 = 0; ptr1 < (lendsv - lsv + 1); ptr1++) {
                    array1[ptr1][0] = d[ptr1 + lsv - 1];
                }

                ge.dlascl('G', 0, 0, ssfmax, anorm, lendsv - lsv + 1, 1, array1, lendsv - lsv + 1, info);

                if (info[0] != 0) {
                    UI.setDataText("dstqrb call to dlascl #5 had info[0] = " + info[0] + "\n");
                }

                for (ptr1 = 0; ptr1 < (lendsv - lsv + 1); ptr1++) {
                    d[ptr1 + lsv - 1] = array1[ptr1][0];
                }

                array1 = new double[lendsv - lsv][1];

                for (ptr1 = 0; ptr1 < (lendsv - lsv); ptr1++) {
                    array1[ptr1][0] = e[ptr1 + lsv - 1];
                }

                ge.dlascl('G', 0, 0, ssfmax, anorm, lendsv - lsv, 1, array1, lendsv - lsv, info);

                if (info[0] != 0) {
                    UI.setDataText("dstqrb call to dlascl #6 had info[0] = " + info[0] + "\n");
                }

                for (ptr1 = 0; ptr1 < (lendsv - lsv); ptr1++) {
                    e[ptr1 + lsv - 1] = array1[ptr1][0];
                }
            } // if (iscale == 1)
            else if (iscale == 2) {
                array1 = new double[lendsv - lsv + 1][1];

                for (ptr1 = 0; ptr1 < (lendsv - lsv + 1); ptr1++) {
                    array1[ptr1][0] = d[ptr1 + lsv - 1];
                }

                ge.dlascl('G', 0, 0, ssfmin, anorm, lendsv - lsv + 1, 1, array1, lendsv - lsv + 1, info);

                if (info[0] != 0) {
                    UI.setDataText("dstqrb call to dlascl #7 had info[0] = " + info[0] + "\n");
                }

                for (ptr1 = 0; ptr1 < (lendsv - lsv + 1); ptr1++) {
                    d[ptr1 + lsv - 1] = array1[ptr1][0];
                }

                array1 = new double[lendsv - lsv][1];

                for (ptr1 = 0; ptr1 < (lendsv - lsv); ptr1++) {
                    array1[ptr1][0] = e[ptr1 + lsv - 1];
                }

                ge.dlascl('G', 0, 0, ssfmin, anorm, lendsv - lsv, 1, array1, lendsv - lsv, info);

                if (info[0] != 0) {
                    UI.setDataText("dstqrb call to dlascl #8 had info[0] = " + info[0] + "\n");
                }

                for (ptr1 = 0; ptr1 < (lendsv - lsv); ptr1++) {
                    e[ptr1 + lsv - 1] = array1[ptr1][0];
                }
            } // else if (iscale == 2)

            // Check for no convergence to an eigenvalue after a total of
            // n*maxit iterations
                } // loop1
                while (jtot < nmaxit);
                for (i = 0; i < (n - 1); i++) {

                    if (e[i] != 0.0) {
                        info[0] = info[0] + 1;
                    }
                } // for (i = 0; i < n-1; i++)

                if (info[0] != 0) {
                    UI.setDataText("dstqrb nonzero e produced info[0] = " + info[0] + "\n");
                }

                return;
            } // primary
             // Order eigenvalues and eigenvectors.
                if (icompz == 0) {

                    // Use quick sort
                    ge.dlasrt('I', n, d, info);

                    if (info[0] != 0) {
                        UI.setDataText("dstqrb call to dlasrt produced info[0] = " + info[0] + "\n");
                    }
                } // if (icompz == 0)
                else { // icompz != 0

                    // Use selection sort to minimize swaps of eigenvectors
                    for (ii = 2; ii <= n; ii++) {
                        i = ii - 1;
                        k = i;
                        p = d[i - 1];

                        for (j = ii; j <= n; j++) {

                            if (d[j - 1] < p) {
                                k = j;
                                p = d[j - 1];
                            } // if (d[j-1] < p)
                        } // for (j == ii; j <= n; j++)

                        if (k != i) {
                            d[k - 1] = d[i - 1];
                            d[i - 1] = p;

                            p = z[k-1];
                            z[k-1] = z[i-1];
                            z[i-1] = p;
                        } // if (k != i)
                    } // for (ii = 2; ii <= n; ii++)
                } // else icompz != 0

                return;
            
              } // dstqrb
      


                
                
        // -----------------------------------------------------------------------
        // \BeginDoc
        
        // \Name: dsaitr
        
        // \Description: 
        //  Reverse communication interface for applying NP additional steps to 
        //  a K step symmetric Arnoldi factorization.
        
        //  Input:  OP*V_{k}  -  V_{k}*H = r_{k}*e_{k}^T
        
        //          with (V_{k}^T)*B*V_{k} = I, (V_{k}^T)*B*r_{k} = 0.
        
        //  Output: OP*V_{k+p}  -  V_{k+p}*H = r_{k+p}*e_{k+p}^T
        
        //          with (V_{k+p}^T)*B*V_{k+p} = I, (V_{k+p}^T)*B*r_{k+p} = 0.
        
        //  where OP and B are as in dsaupd.  The B-norm of r_{k+p} is also
        //  computed and returned.
        
        // \Usage:
        //  call dsaitr
        //     ( IDO, BMAT, N, K, NP, MODE, RESID, RNORM, V, LDV, H, LDH, 
        //       IPNTR, WORKD, INFO )
        
        // \Arguments
        //  IDO     Integer.  (INPUT/OUTPUT)
        //          Reverse communication flag.
        //          -------------------------------------------------------------
        //          IDO =  0: first call to the reverse communication interface
        //          IDO = -1: compute  Y = OP * X  where
        //                    IPNTR[0] is the pointer into WORK for X,
        //                    IPNTR[1] is the pointer into WORK for Y.
        //                    This is for the restart phase to force the new
        //                    starting vector into the range of OP.
        //          IDO =  1: compute  Y = OP * X  where
        //                    IPNTR[0] is the pointer into WORK for X,
        //                    IPNTR[1] is the pointer into WORK for Y,
        //                    IPNTR[2] is the pointer into WORK for B * X.
        //          IDO =  2: compute  Y = B * X  where
        //                    IPNTR[0] is the pointer into WORK for X,
        //                    IPNTR[1] is the pointer into WORK for Y.
        //          IDO = 99: done
        //          -------------------------------------------------------------
        //          When the routine is used in the "shift-and-invert" mode, the
        //          vector B * Q is already available and does not need to be
        //          recomputed in forming OP * Q.
        
        //  BMAT    String.  (INPUT)
        //          BMAT specifies the type of matrix B that defines the
        //          semi-inner product for the operator OP.  See dsaupd.
        //          B = 'I' -> standard eigenvalue problem A*x = lambda*x
        //          B = 'G' -> generalized eigenvalue problem A*x = lambda*M*x
        
        //  N       Integer.  (INPUT)
        //          Dimension of the eigenproblem.
        
        //  K       Integer.  (INPUT)
        //          Current order of H and the number of columns of V.
        
        //  NP      Integer.  (INPUT)
        //          Number of additional Arnoldi steps to take.
        
        //  MODE    Integer.  (INPUT)
        //          Signifies which form for "OP". If MODE=2 then
        //          a reduction in the number of B matrix vector multiplies
        //          is possible since the B-norm of OP*x is equivalent to
        //          the inv(B)-norm of A*x.
        
        //  RESID   Double precision array of length N.  (INPUT/OUTPUT)
        //          On INPUT:  RESID contains the residual vector r_{k}.
        //          On OUTPUT: RESID contains the residual vector r_{k+p}.
        
        //  RNORM   Double precision scalar.  (INPUT/OUTPUT)
        //          On INPUT the B-norm of r_{k}.
        //          On OUTPUT the B-norm of the updated residual r_{k+p}.
        
        //  V       Double precision N by K+NP array.  (INPUT/OUTPUT)
        //          On INPUT:  V contains the Arnoldi vectors in the first K 
        //          columns.
        //          On OUTPUT: V contains the new NP Arnoldi vectors in the next
        //          NP columns.  The first K columns are unchanged.
        
        //  LDV     Integer.  (INPUT)
        //          Leading dimension of V exactly as declared in the calling 
        //          program.
        
        //  H       Double precision (K+NP) by 2 array.  (INPUT/OUTPUT)
        //          H is used to store the generated symmetric tridiagonal matrix
        //          with the subdiagonal in the first column starting at H(2,1)
        //          and the main diagonal in the second column.
        
        //  LDH     Integer.  (INPUT)
        //          Leading dimension of H exactly as declared in the calling 
        //          program.
        
        //  IPNTR   Integer array of length 3.  (OUTPUT)
        //          Pointer to mark the starting locations in the WORK for 
        //          vectors used by the Arnoldi iteration.
        //          -------------------------------------------------------------
        //          IPNTR[0]: pointer to the current operand vector X.
        //          IPNTR[1]: pointer to the current result vector Y.
        //          IPNTR[2]: pointer to the vector B * X when used in the 
        //                    shift-and-invert mode.  X is the current operand.
        //          -------------------------------------------------------------
                  
        //  WORKD   Double precision work array of length 3*N.  (REVERSE COMMUNICATION)
        //          Distributed array to be used in the basic Arnoldi iteration
        //          for reverse communication.  The calling program should not 
        //          use WORKD as temporary workspace during the iteration !!!!!!
        //          On INPUT, WORKD(1:N) = B*RESID where RESID is associated
        //          with the K step Arnoldi factorization. Used to save some 
        //          computation at the first step. 
        //          On OUTPUT, WORKD(1:N) = B*RESID where RESID is associated
        //          with the K+NP step Arnoldi factorization.
        
        //  INFO    Integer.  (OUTPUT)
        //          = 0: Normal exit.
        //          > 0: Size of an invariant subspace of OP is found that is
        //               less than K + NP.
        
        // \EndDoc
        
        // -----------------------------------------------------------------------
        
        // \BeginLib
        
        // \Local variables:
        //     xxxxxx  real
        
        // \Routines called:
        //     dgetv0  ARPACK routine to generate the initial vector.
        //     ivout   ARPACK utility routine that prints integers.
        //     dmout   ARPACK utility routine that prints matrices.
        //     dvout   ARPACK utility routine that prints vectors.
        //     dlamch  LAPACK routine that determines machine constants.
        //     dlascl  LAPACK routine for careful scaling of a matrix.
        //     dgemv   Level 2 BLAS routine for matrix vector multiplication.
        //     daxpy   Level 1 BLAS that computes a vector triad.
        //     dscal   Level 1 BLAS that scales a vector.
        //     dcopy   Level 1 BLAS that copies one vector to another .
        //     Level 1 BLAS that computes the scalar product of two vectors. 
        //     dnrm2   Level 1 BLAS that computes the norm of a vector.
        
        // \Author
        //     Danny Sorensen               Phuong Vu
        //     Richard Lehoucq              CRPC / Rice University
        //     Dept. of Computational &     Houston, Texas
        //     Applied Mathematics
        //     Rice University           
        //     Houston, Texas            
         
        // \Revision history:
        //     xx/xx/93: Version ' 2.4'
        
        // \SCCS Information: @(#) 
        //  FILE: saitr.F   SID: 2.6   DATE OF SID: 8/28/96   RELEASE: 2
        
        // \Remarks
        //  The algorithm implemented is:
        //  
        //  restart = .false.
        //  Given V_{k} = [v_{1}, ..., v_{k}], r_{k}; 
        //  r_{k} contains the initial residual vector even for k = 0;
        //  Also assume that rnorm = || B*r_{k} || and B*r_{k} are already 
        //  computed by the calling program.
        
        //  betaj = rnorm ; p_{k+1} = B*r_{k} ;
        //  For  j = k+1, ..., k+np  Do
        //     1) if ( betaj < tol ) stop or restart depending on j.
        //        if ( restart ) generate a new starting vector.
        //     2) v_{j} = r(j-1)/betaj;  V_{j} = [V_{j-1}, v_{j}];  
        //        p_{j} = p_{j}/betaj
        //     3) r_{j} = OP*v_{j} where OP is defined as in dsaupd
        //        For shift-invert mode p_{j} = B*v_{j} is already available.
        //        wnorm = || OP*v_{j} ||
        //     4) Compute the j-th step residual vector.
        //        w_{j} =  V_{j}^T * B * OP * v_{j}
        //        r_{j} =  OP*v_{j} - V_{j} * w_{j}
        //        alphaj <- j-th component of w_{j}
        //        rnorm = || r_{j} ||
        //        betaj+1 = rnorm
        //        If (rnorm > 0.717*wnorm) accept step and go back to 1)
        //     5) Re-orthogonalization step:
        //        s = V_{j}'*B*r_{j}
        //        r_{j} = r_{j} - V_{j}*s;  rnorm1 = || r_{j} ||
        //        alphaj = alphaj + s_{j};   
        //     6) Iterative refinement step:
        //        If (rnorm1 > 0.717*rnorm) then
        //           rnorm = rnorm1
        //           accept step and go back to 1)
        //        Else
        //           rnorm = rnorm1
        //           If this is the first time in step 6), go to 5)
        //           Else r_{j} lies in the span of V_{j} numerically.
        //              Set r_{j} = 0 and rnorm = 0; go to 1)
        //        EndIf 
        //  End Do
        
        // \EndLib
        
        // -----------------------------------------------------------------------
        
             private void dsaitr
                (int ido[], String bmat, int n, int k, int np, int mode, double resid[], double rnorm[], double v[][], 
                		int ldv, double h[][], int ldh, int ipntr[], double workd[], int info[]) {
        
        //     %----------------------------------------------------%
        //     | Include files for debugging and timing information |
        //     %----------------------------------------------------%
        
        //      include   'debug.h'
        //      include   'stat.h'
        
        //     %------------------%
        //     | Scalar Arguments |
        //     %------------------%
        
        //     character  bmat*1
        //     integer    ido, info, k, ldh, ldv, n, mode, np
        //     Double precision
        //     &           rnorm
        
        //     %-----------------%
        //     | Array Arguments |
        //     %-----------------%
        
        //      integer    ipntr(3)
        //      Double precision
        //     &           h(ldh,2), resid(n), v(ldv,k+np), workd(3*n)
        
        //     %------------%
        //     | Parameters |
        //     %------------%
        
              final double zero = 0.0;
              final double one = 1.0;
             
        
        //     %---------------%
        //     | Local Scalars |
        //     %---------------%
        
              int    i = 0;
              int jj, m;
              int infol[] = new int[1];
              double temp1;
        
        //     %----------------------%
        //     | External Subroutines |
        //     %----------------------%
        
        //      external   daxpy, dcopy, dscal, dgemv, dgetv0, dvout, dmout,
        //     &           dlascl, ivout, second
        
        //     %--------------------%
        //     | External Functions |
        //     %--------------------%
        
        //      Double precision
        //     &           ddot, dnrm2, dlamch
        //      external   ddot, dnrm2, dlamch
       
        
        //     %-----------------------%
        //     | Executable Statements |
        //     %-----------------------%
        
              if (dsaitr_first) {
                 dsaitr_first = false;
        
        //        %--------------------------------%
        //        | safmin = safe minimum is such  |
        //        | that 1/sfmin does not overflow |
        //        %--------------------------------%
        
                 dsaitr_safmin = ge.dlamch('S');
              } // if (dsaitr_first)
        
              if (ido[0] == 0) {
         
        //        %-------------------------------%
        //        | Initialize timing statistics  |
        //        | & message level for debugging |
        //        %-------------------------------%
        
            	 t0 = System.currentTimeMillis();
                 dsaitr_msglvl = msaitr;
         
        //        %------------------------------%
        //        | Initial call to this routine |
        //        %------------------------------%
        
                 info[0]   = 0;
                 dsaitr_step3  = false;
                 dsaitr_step4  = false;
                 dsaitr_rstart = false;
                 dsaitr_orth1  = false;
                 dsaitr_orth2  = false;
         
        //        %--------------------------------%
        //        | Pointer to the current step of |
        //        | the factorization to build     |
        //        %--------------------------------%
        
                 dsaitr_j      = k + 1;
        
        //        %------------------------------------------%
        //        | Pointers used for reverse communication  |
        //        | when using WORKD.                        |
        //        %------------------------------------------%
        
                 dsaitr_ipj    = 1;
                 dsaitr_irj    = dsaitr_ipj   + n;
                 dsaitr_ivj    = dsaitr_irj   + n;
              } // if (ido[0] == 0)
         
        //     %-------------------------------------------------%
        //     | When in reverse communication mode one of:      |
        //     | STEP3, STEP4, ORTH1, ORTH2, RSTART              |
        //     | will be .true.                                  |
        //     | STEP3: return from computing OP*v_{j}.          |
        //     | STEP4: return from computing B-norm of OP*v_{j} |
        //     | ORTH1: return from computing B-norm of r_{j+1}  |
        //     | ORTH2: return from computing B-norm of          |
        //     |        correction to the residual vector.       |
        //     | RSTART: return from OP computations needed by   |
        //     |         dgetv0.                                 |
        //     %-------------------------------------------------%
        
              boolean seg1 = true;
              boolean seg2 = true;
              boolean seg3 = true;
              boolean seg4 = true;
              boolean seg5 = true;
              boolean seg6 = true;
              boolean seg7 = true;
              boolean seg8 = true;
              boolean seg9 = true;
              boolean seg10 = true;
              boolean seg11 = true;
              boolean seg12 = true;
              boolean seg13 = true;
              boolean seg14 = true;
              boolean seg15 = true;
              if (dsaitr_step3)  {
            	  seg4 = false;
              }
              else if (dsaitr_step4)  {
            	  seg3 = false;
              }
              else if (dsaitr_orth1)  {
                  seg2 = false;  
              }
              else if (dsaitr_orth2)  {
            	  seg1 = false;
              }
              else if (dsaitr_rstart) {
            	  seg5 = false;
              }
        
        //     %------------------------------%
        //     | Else this is the first step. |
        //     %------------------------------%
         
        //     %--------------------------------------------------------------%
        //     |                                                              |
        //     |        A R N O L D I     I T E R A T I O N     L O O P       |
        //     |                                                              |
        //     | Note:  B*r_{j-1} is already in WORKD(1:N)=WORKD(IPJ:IPJ+N-1) |
        //     %--------------------------------------------------------------%
        
         loop1: while (true) {
                if (seg1) {
                if (seg2) {
                if (seg3) {
                if (seg4) {
                if (seg5) {
                if (seg8) {
                 if (dsaitr_msglvl > 2) {
                	UI.setDataText("dsaitr: generating Arnoldi vector no. dsaitr_j = " + dsaitr_j + "\n");
                	UI.setDataText("dsaitr: B-norm of the current residual rnorm[0] = " + nf.format(rnorm[0]) + "\n");
                 } // if (dsaitr_msglvl > 2)
         
        //        %---------------------------------------------------------%
        //        | Check for exact zero. Equivalent to determing whether a |
        //        | j-step Arnoldi factorization is present.                |
        //        %---------------------------------------------------------%
        
                 if (rnorm[0] > zero) {
                	 seg6 = false;
                	 seg7 = false;
                	 seg15 = false;
                 }
                 if (seg6) {
        
        //           %---------------------------------------------------%
        //           | Invariant subspace found, generate a new starting |
        //           | vector which is orthogonal to the current Arnoldi |
        //           | basis and continue the iteration.                 |
        //           %---------------------------------------------------%
        
                    if (dsaitr_msglvl > 0) {
                       UI.setDataText("saitr: ****** restart at step ****** dsaitr_j = "+ dsaitr_j + "\n");
                    }
         
        //           %---------------------------------------------%
        //           | ITRY is the loop variable that controls the |
        //           | maximum amount of times that a restart is   |
        //           | attempted. NRSTRT is used by stat.h         |
        //           %---------------------------------------------%
        
                    nrstrt = nrstrt + 1;
                    dsaitr_itry   = 1;
                 } // if (seg6)
                 seg6 = true;
                 } // if (seg8)
                 seg8 = true;
                 if (seg7) {
                    dsaitr_rstart = true;
                    ido[0]    = 0;
                 } // if (seg7)
                } // if (seg5)
                seg5 = true;
               if (seg15) {
        
        //           %--------------------------------------%
        //           | If in reverse communication mode and |
        //           | RSTART = .true. flow returns here.   |
        //           %--------------------------------------%
        
                    dgetv0 (ido, bmat, dsaitr_itry, false, n, dsaitr_j, v, ldv, 
                                resid, rnorm, ipntr, workd, dsaitr_ierr);
                    if (ido[0] != 99) {
                    	return;
                    }
                    if (dsaitr_ierr[0] < 0) {
                       dsaitr_itry = dsaitr_itry + 1;
                       if (dsaitr_itry <= 3) {
                    	   seg8 = false;
                    	   continue loop1;
                       }
        
        //              %------------------------------------------------%
        //              | Give up after several restart attempts.        |
        //              | Set INFO to the size of the invariant subspace |
        //              | which spans OP and exit.                       |
        //              %------------------------------------------------%
        
                       info[0] = dsaitr_j - 1;
                       t1 = System.currentTimeMillis();
                       tsaitr = tsaitr + (t1 - t0);
                       ido[0] = 99;
                       return;
                    } // if (dsaitr_ierr[0] < 0)
               } // if (seg15)
           seg15 = true;
        
        //        %---------------------------------------------------------%
        //        | STEP 2:  v_{j} = r_{j-1}/rnorm and p_{j} = p_{j}/rnorm  |
        //        | Note that p_{j} = B*r_{j-1}. In order to avoid overflow |
        //        | when reciprocating a small RNORM, test against lower    |
        //        | machine bound.                                          |
        //        %---------------------------------------------------------%
        
	             for (m = 0; m < n; m++) {
	        	     v[m][dsaitr_j-1] = resid[m];
	             }
                 if (rnorm[0] >= dsaitr_safmin) {
                     temp1 = one / rnorm[0];
                     for (m = 0; m < n; m++) {
                    	 v[m][dsaitr_j-1] = temp1 * v[m][dsaitr_j-1];
                     }
                     for (m = 0; m < n; m++) {
                    	 workd[dsaitr_ipj - 1 + m] = temp1 * workd[dsaitr_ipj - 1 + m];
                     }
                 }
                 else {
        
        //            %-----------------------------------------%
        //            | To scale both v_{j} and p_{j} carefully |
        //            | use LAPACK routine SLASCL               |
        //            %-----------------------------------------%
        
                     double A[][] = new double[n][1];
                     for (m = 0; m < n; m++) {
                    	 A[m][0] = v[m][dsaitr_j-1];
                     }
                     ge.dlascl('G', i, i, rnorm[0], one, n, 1, A, n, infol);
                     for (m = 0; m < n; m++) {
                    	 v[m][dsaitr_j-1] = A[m][0];
                     }
                     for (m = 0; m < n; m++) {
                    	 A[m][0] = workd[dsaitr_ipj-1+m];
                     }
                     ge.dlascl('G', i, i, rnorm[0], one, n, 1, A, n, infol);
                     for (m = 0; m < n; m++) {
                    	 workd[dsaitr_ipj-1+m] = A[m][0];
                     }
                 }
         
        //        %------------------------------------------------------%
        //        | STEP 3:  r_{j} = OP*v_{j}; Note that p_{j} = B*v_{j} |
        //        | Note that this is not quite yet r_{j}. See STEP 4    |
        //        %------------------------------------------------------%
        
                 dsaitr_step3 = true;
                 nopx  = nopx + 1;
                 t2 = System.currentTimeMillis();
                 for (m = 0; m < n; m++) {
                	 workd[dsaitr_ivj-1+m] = v[m][dsaitr_j-1];
                 }
                 ipntr[0] = dsaitr_ivj;
                 ipntr[1] = dsaitr_irj;
                 ipntr[2] = dsaitr_ipj;
                 ido[0] = 1;
         
        //       %-----------------------------------%
        //        | Exit in order to compute OP*v_{j} |
        //        %-----------------------------------%
         
                 return;
                } // if (seg4)
                seg4 = true;
         
        //        %-----------------------------------%
        //        | Back from reverse communication;  |
        //        | WORKD(IRJ:IRJ+N-1) := OP*v_{j}.   |
        //        %-----------------------------------%
        
                 t3 = System.currentTimeMillis();
                 tmvopx = tmvopx + (t3 - t2);
         
                 dsaitr_step3 = false;
        
        //        %------------------------------------------%
        //        | Put another copy of OP*v_{j} into RESID. |
        //        %------------------------------------------%
        
                 for (m = 0; m < n; m++) {
                	 resid[m] = workd[dsaitr_irj-1+m];
                 }
         
        //        %-------------------------------------------%
        //        | STEP 4:  Finish extending the symmetric   |
        //        |          Arnoldi to length j. If MODE = 2 |
        //        |          then B*OP = B*inv(B)*A = A and   |
        //        |          we don't need to compute B*OP.   |
        //        | NOTE: If MODE = 2 WORKD(IVJ:IVJ+N-1) is   |
        //        | assumed to have A*v_{j}.                  |
        //        %-------------------------------------------%
        
                 if (mode == 2) {
                	 seg9 = false;
                	 seg10 = false;
                 }
                 if (seg9) {
                	 t2 = System.currentTimeMillis();
                 if (bmat.equalsIgnoreCase("G")) {
                    nbx = nbx + 1;
                    dsaitr_step4 = true;
                    ipntr[0] = dsaitr_irj;
                    ipntr[1] = dsaitr_ipj;
                    ido[0] = 2;
         
        //           %-------------------------------------%
        //           | Exit in order to compute B*OP*v_{j} |
        //           %-------------------------------------%
         
                    return;
                 } // if (bmat.equalsIgnoreCase("G"))
                 else if (bmat.equalsIgnoreCase("I")) {
                	 for (m = 0; m < n; m++) {
                		 workd[dsaitr_ipj-1+m] = resid[m];
                	 }
                 }
                 } // if (seg9)
                 seg9 = true;
                } // if (seg3)
                seg3 = true;
           if (seg10) {
         
        //        %-----------------------------------%
        //        | Back from reverse communication;  |
        //        | WORKD(IPJ:IPJ+N-1) := B*OP*v_{j}. |
        //        %-----------------------------------%
        
        	   if (bmat.equalsIgnoreCase("G")) {
        		    t3 = System.currentTimeMillis();
                    tmvbx = tmvbx + (t3 - t2);
        	   }
        
                 dsaitr_step4 = false;
           } // if (seg10)
           seg10 = true;
        
        //        %-------------------------------------%
        //        | The following is needed for STEP 5. |
        //        | Compute the B-norm of OP*v_{j}.     |
        //        %-------------------------------------%
        
                 if (mode == 2) {
        
        //           %----------------------------------%
        //           | Note that the B-norm of OP*v_{j} |
        //           | is the inv(B)-norm of A*v_{j}.   |
        //           %----------------------------------%
                    double buffer[] = new double[n];
                    for (m = 0; m < n; m++) {
                    	buffer[m] = workd[dsaitr_ivj-1+m];
                    }
                    dsaitr_wnorm = ge.ddot (n, resid, 1, buffer, 1);
                    dsaitr_wnorm = Math.sqrt(Math.abs(dsaitr_wnorm));
                 }
                 else if (bmat.equalsIgnoreCase("G")) { 
                	 double buffer[] = new double[n];
                     for (m = 0; m < n; m++) {
                     	buffer[m] = workd[dsaitr_ipj-1+m];
                     }
                    dsaitr_wnorm = ge.ddot (n, resid, 1, buffer, 1);
                    dsaitr_wnorm = Math.sqrt(Math.abs(dsaitr_wnorm));
                 }
                 else if (bmat.equalsIgnoreCase("I")) {
                    dsaitr_wnorm = ge.dnrm2(n, resid, 1);
                 }
        
        //        %-----------------------------------------%
        //        | Compute the j-th residual corresponding |
        //        | to the j step factorization.            |
        //        | Use Classical Gram Schmidt and compute: |
        //        | w_{j} <-  V_{j}^T * B * OP * v_{j}      |
        //        | r_{j} <-  OP*v_{j} - V_{j} * w_{j}      |
        //        %-----------------------------------------%
        
        
        //        %------------------------------------------%
        //        | Compute the j Fourier coefficients w_{j} |
        //        | WORKD(IPJ:IPJ+N-1) contains B*OP*v_{j}.  |
        //        %------------------------------------------%
        
                 double buffer[] = new double[n];
                 double buffer2[] = new double[dsaitr_j];
                 if (mode != 2 ) {
                	 for (m = 0; m < n; m++) {
                		 buffer[m] = workd[dsaitr_ipj-1+m];
                	 }
                	 for (m = 0; m < dsaitr_j; m++) {
                		 buffer2[m] = workd[dsaitr_irj-1+m];
                	 }
                    ge.dgemv('T', n, dsaitr_j, one, v, ldv, buffer, 1, zero, buffer2, 1);
                    for (m = 0; m < dsaitr_j; m++) {
                    	workd[dsaitr_irj-1+m] = buffer2[m];	
                    }
                 }
                 else if (mode == 2) {
                	 for (m = 0; m < n; m++) {
                		 buffer[m] = workd[dsaitr_ivj-1+m];
                	 }
                	 for (m = 0; m < dsaitr_j; m++) {
                		 buffer2[m] = workd[dsaitr_irj-1+m];
                	 }
                    ge.dgemv('T', n, dsaitr_j, one, v, ldv, buffer, 1, zero, buffer2, 1);
                    for (m = 0; m < dsaitr_j; m++) {
                    	workd[dsaitr_irj-1+m] = buffer2[m];	
                    }
                 }
        
        //        %--------------------------------------%
        //        | Orthgonalize r_{j} against V_{j}.    |
        //        | RESID contains OP*v_{j}. See STEP 3. | 
        //        %--------------------------------------%
                 ge.dgemv('N', n, dsaitr_j, -one, v, ldv, buffer2, 1, one, resid, 1);
        
        //        %--------------------------------------%
        //        | Extend H to have j rows and columns. |
        //        %--------------------------------------%
        
                 h[dsaitr_j-1][1] = workd[dsaitr_irj + dsaitr_j - 2];
                 if (dsaitr_j == 1  ||  dsaitr_rstart) {
                    h[dsaitr_j-1][0] = zero;
                 }
                 else {
                    h[dsaitr_j-1][0] = rnorm[0];
                 }
                 t4 = System.currentTimeMillis();
         
                 dsaitr_orth1 = true;
                 dsaitr_iter  = 0;
        
                 t2 = System.currentTimeMillis();
                 if (bmat.equalsIgnoreCase("G")) { 
                    nbx = nbx + 1;
                    for (m = 0; m < n; m++) {
                    	workd[dsaitr_irj-1+m] = resid[m];
                    }
                    ipntr[0] = dsaitr_irj;
                    ipntr[1] = dsaitr_ipj;
                    ido[0] = 2;
         
        //           %----------------------------------%
        //           | Exit in order to compute B*r_{j} |
        //           %----------------------------------%
       
                    return;
                 } // if (bmat.equalsIgnoreCase("G"))
                 else if (bmat.equalsIgnoreCase("I")) {
                	 for (m = 0; m < n; m++) {
                     	workd[dsaitr_ipj-1+m] = resid[m];
                     }
                 }
                } // if (seg2)
                seg2 = true;
               if (seg13) {
         
        //        %---------------------------------------------------%
        //        | Back from reverse communication if ORTH1 = .true. |
        //        | WORKD(IPJ:IPJ+N-1) := B*r_{j}.                    |
        //        %---------------------------------------------------%
        
            	 if (bmat.equalsIgnoreCase("G")) { 
            		t3 = System.currentTimeMillis();
                    tmvbx = tmvbx + (t3 - t2);
            	 }
         
                 dsaitr_orth1 = false;
        
        //        %------------------------------%
        //        | Compute the B-norm of r_{j}. |
        //        %------------------------------%
        
                 if (bmat.equalsIgnoreCase("G")) {
                	double buffer[] = new double[n];
                	for (m = 0; m < n; m++) {
                		buffer[m] = workd[dsaitr_ipj-1+m];
                	}
                    rnorm[0] = ge.ddot (n, resid, 1, buffer, 1);
                    rnorm[0] = Math.sqrt(Math.abs(rnorm[0]));
                 } // if (bmat.equalsIgnoreCase("G"))
                 else if (bmat.equalsIgnoreCase("I")) {
                    rnorm[0] = ge.dnrm2(n, resid, 1);
                 }
        
        //        %-----------------------------------------------------------%
        //        | STEP 5: Re-orthogonalization / Iterative refinement phase |
        //        | Maximum NITER_ITREF tries.                                |
        //        |                                                           |
        //        |          s      = V_{j}^T * B * r_{j}                     |
        //        |          r_{j}  = r_{j} - V_{j}*s                         |
        //        |          alphaj = alphaj + s_{j}                          |
        //        |                                                           |
        //        | The stopping criteria used for iterative refinement is    |
        //        | discussed in Parlett's book SEP, page 107 and in Gragg &  |
        //        | Reichel ACM TOMS paper; Algorithm 686, Dec. 1990.         |
        //        | Determine if we need to correct the residual. The goal is |
        //        | to enforce ||v(:,1:j)^T * r_{j}|| .le. eps * || r_{j} ||  |
        //        %-----------------------------------------------------------%
        
                 if (rnorm[0] > 0.717*dsaitr_wnorm) {
                	 seg11 = false;
                	 seg12 = false;
                 }
               } // if (seg13)
               seg13 = true;
                 if (seg11) {
                	 if (seg14) {
                 nrorth = nrorth + 1;
                	 } // if (seg14)
                	 seg14 = true;
         
        //        %---------------------------------------------------%
        //        | Enter the Iterative refinement phase. If further  |
        //        | refinement is necessary, loop back here. The loop |
        //        | variable is ITER. Perform a step of Classical     |
        //        | Gram-Schmidt using all the Arnoldi vectors V_{j}  |
        //        %---------------------------------------------------%
        
                 if (dsaitr_msglvl > 2) {
                    UI.setDataText("dsaitr: re-orthogonalization ; wnorm = " + nf.format(dsaitr_wnorm) +
                    		" rnorm[0] = " + nf.format(rnorm[0]) + "\n");
                    
                 }
        
        //        %----------------------------------------------------%
        //        | Compute V_{j}^T * B * r_{j}.                       |
        //        | WORKD(IRJ:IRJ+J-1) = v(:,1:J)'*WORKD(IPJ:IPJ+N-1). |
        //        %----------------------------------------------------%
                 double buffer[] = new double[n];
                 double buffer2[] = new double[dsaitr_j];
                 for (m = 0; m < n; m++) {
                	 buffer[m] = workd[dsaitr_ipj-1+m];
                 }
                 for (m = 0; m < dsaitr_j; m++) {
                	 buffer2[m] = workd[dsaitr_irj-1+m];
                 }
                 ge.dgemv ('T', n, dsaitr_j, one, v, ldv, buffer, 1, zero, buffer2, 1);
                 for (m = 0; m < dsaitr_j; m++) {
                	 workd[dsaitr_irj-1+m] = buffer2[m];	 
                 }
        
        //        %----------------------------------------------%
        //        | Compute the correction to the residual:      |
        //        | r_{j} = r_{j} - V_{j} * WORKD(IRJ:IRJ+J-1).  |
        //        | The correction to H is v(:,1:J)*H(1:J,1:J) + |
        //        | v(:,1:J)*WORKD(IRJ:IRJ+J-1)*e'_j, but only   |
        //        | H(j,j) is updated.                           |
        //        %----------------------------------------------%
        
                 ge.dgemv ('N', n, dsaitr_j, -one, v, ldv, buffer2, 1, one, resid, 1);
        
                 if (dsaitr_j == 1  ||  dsaitr_rstart) h[dsaitr_j-1][0] = zero;
                 h[dsaitr_j-1][1] = h[dsaitr_j-1][1] + workd[dsaitr_irj + dsaitr_j - 2];
        
                 dsaitr_orth2 = true;
                 t2 = System.currentTimeMillis();
                 if (bmat.equalsIgnoreCase("G")) {
                    nbx = nbx + 1;
                    for (m = 0; m < n; m++) {
                    	workd[dsaitr_irj-1+m] = resid[m];
                    }
                    ipntr[0] = dsaitr_irj;
                    ipntr[1] = dsaitr_ipj;
                    ido[0] = 2;
         
        //           %-----------------------------------%
        //           | Exit in order to compute B*r_{j}. |
        //           | r_{j} is the corrected residual.  |
        //           %-----------------------------------%
         
                    return;
                 } // if (bmat.equalsIgnoreCase("G"))
                 else if (bmat.equalsIgnoreCase("I")) {
                	for (m = 0; m < n; m++) {
                		workd[dsaitr_ipj-1+m] = resid[m];
                	}
                 }
                 } // if (seg11)
                 seg11 = true;
                } // if (seg1)
                seg1 = true;
           if (seg12) {
        
        //        %---------------------------------------------------%
        //        | Back from reverse communication if ORTH2 = .true. |
        //        %---------------------------------------------------%
        
        	   if (bmat.equalsIgnoreCase("G")) {
        		    t3 = System.currentTimeMillis();
                    tmvbx = tmvbx + (t3 - t2);
        	   }
        
        //        %-----------------------------------------------------%
        //        | Compute the B-norm of the corrected residual r_{j}. |
        //        %-----------------------------------------------------%
         
        	     if (bmat.equalsIgnoreCase("G")) {
        	    	 double buffer[] = new double[n];
        	    	 for (m = 0; m < n; m++) {
        	    		 buffer[m] = workd[dsaitr_ipj-1+m];
        	    	 }
                     dsaitr_rnorm1 = ge.ddot (n, resid, 1, buffer, 1);
                     dsaitr_rnorm1 = Math.sqrt(Math.abs(dsaitr_rnorm1));
        	     } // if (bmat.equalsIgnoreCase("G"))
                 else if (bmat.equalsIgnoreCase("I")) {
                     dsaitr_rnorm1 = ge.dnrm2(n, resid, 1);
                 }
        
                 if (dsaitr_msglvl > 0 && dsaitr_iter > 0) {
                	UI.setDataText("dsaitr: Iterative refinement for Arnoldi residual dsaitr_j = " + dsaitr_j + "\n");
                    if (dsaitr_msglvl > 2) {
                        UI.setDataText("dsaitr: iterative refinement ; rnorm[0] = " + nf.format(rnorm[0]) +
                        		" dsaitr_rnorm1 = " + nf.format(dsaitr_rnorm1) + "\n");
                    }
                 }
         
        //        %-----------------------------------------%
        //        | Determine if we need to perform another |
        //        | step of re-orthogonalization.           |
        //        %-----------------------------------------%
        
                 if (dsaitr_rnorm1 > 0.717*rnorm[0]) {
        
        //           %--------------------------------%
        //           | No need for further refinement |
        //           %--------------------------------%
        
                    rnorm[0] = dsaitr_rnorm1;
         
                 }
                 else {
        
        //           %-------------------------------------------%
        //           | Another step of iterative refinement step |
        //           | is required. NITREF is used by stat.h     |
        //           %-------------------------------------------%
        
                    nitref = nitref + 1;
                    rnorm[0]  = dsaitr_rnorm1;
                    dsaitr_iter   = dsaitr_iter + 1;
                    if (dsaitr_iter <= 1) {
                    	seg2 = false;
                    	seg13 = false;
                    	seg14 = false;
                    	continue loop1;
                    }
        
        //           %-------------------------------------------------%
        //           | Otherwise RESID is numerically in the span of V |
        //           %-------------------------------------------------%
        
                    for (jj = 0; jj < n; jj++) {
                       resid[jj] = zero;
                    }
                    rnorm[0] = zero;
                 }
        
        //        %----------------------------------------------%
        //        | Branch here directly if iterative refinement |
        //        | wasn't necessary or after at most NITER_REF  |
        //        | steps of iterative refinement.               |
        //        %----------------------------------------------%
        
           } // if (seg12)
           seg12 = true;
         
                 dsaitr_rstart = false;
                 dsaitr_orth2  = false;
         
                 t5 = System.currentTimeMillis();
                 titref = titref + (t5 - t4);
         
        //        %----------------------------------------------------------%
        //        | Make sure the last off-diagonal element is non negative  |
        //        | If not perform a similarity transformation on H(1:j,1:j) |
        //        | and scale v(:,j) by -1.                                  |
        //        %----------------------------------------------------------%
        
                 if (h[dsaitr_j-1][0] < zero) {
                    h[dsaitr_j-1][0] = -h[dsaitr_j-1][0];
                    if ( dsaitr_j < k+np) { 
                       for (m = 0; m < n; m++) {
                    	   v[m][dsaitr_j] = -one * v[m][dsaitr_j];
                       }
                    }
                    else {
                       for (m = 0; m < n; m++) {
                    	   resid[m] = -one * resid[m];
                       }
                    }
                 }
         
        //        %------------------------------------%
        //        | STEP 6: Update  j = j+1;  Continue |
        //        %------------------------------------%
        
                 dsaitr_j = dsaitr_j + 1;
                 if (dsaitr_j > k+np) {
                    t1 = System.currentTimeMillis();
                    tsaitr = tsaitr + (t1 - t0);
                    ido[0] = 99;
        
                    if (dsaitr_msglvl > 1) {
                       UI.setDataText("dsaitr: main diagonal of matrix H of step K+NP.\n");
                       for (m = 0; m < k+np; m++) {
                    	   UI.setDataText("h["+m+"][1] = " + nf.format(h[m][1]) + "\n");
                       }
                       if (k+np > 1) {
                    	   UI.setDataText("dsaitr: sub diagonal of matrix H of step K+NP.\n");
                           for (m = 0; m < k+np-1; m++) {
                        	   UI.setDataText("h["+(m+1)+"][0] = " + nf.format(h[m+1][0]) + "\n");
                           }
                       }
                    }
        
                    return;
                 }
        
        //        %--------------------------------------------------------%
        //        | Loop back to extend the factorization by another step. |
        //        %--------------------------------------------------------%
        
         } // loop1: while (true)
         
        //     %---------------------------------------------------------------%
        //     |                                                               |
        //     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
        //     |                                                               |
        //     %---------------------------------------------------------------%
        
             } // dsaitr
                
                
        // -----------------------------------------------------------------------
        // \BeginDoc
        //
        // \Name: dgetv0
        //
        // \Description: 
        //  Generate a random initial residual vector for the Arnoldi process.
        //  Force the residual vector to be in the range of the operator OP.  
        
        // \Usage:
        //  call dgetv0
        //     ( IDO, BMAT, ITRY, INITV, N, J, V, LDV, RESID, RNORM, 
        //       IPNTR, WORKD, IERR )
        
        // \Arguments
        //  IDO     Integer.  (INPUT/OUTPUT)
        //          Reverse communication flag.  IDO must be zero on the first
        //          call to dgetv0.
        //          -------------------------------------------------------------
        //          IDO =  0: first call to the reverse communication interface
        //          IDO = -1: compute  Y = OP * X  where
        //                    IPNTR[0] is the pointer into WORKD for X,
        //                    IPNTR[1] is the pointer into WORKD for Y.
        //                    This is for the initialization phase to force the
        //                    starting vector into the range of OP.
        //          IDO =  2: compute  Y = B * X  where
        //                    IPNTR[0] is the pointer into WORKD for X,
        //                    IPNTR[1] is the pointer into WORKD for Y.
        //          IDO = 99: done
        //          -------------------------------------------------------------
        
        //  BMAT   String  (INPUT)
        //          BMAT specifies the type of the matrix B in the (generalized)
        //          eigenvalue problem A*x = lambda*B*x.
        //          B = 'I' -> standard eigenvalue problem A*x = lambda*x
        //          B = 'G' -> generalized eigenvalue problem A*x = lambda*B*x
        
        //  ITRY    Integer.  (INPUT)
        //          ITRY counts the number of times that dgetv0 is called.  
        //          It should be set to 1 on the initial call to dgetv0.
        
        //  INITV   Logical variable.  (INPUT)
        //          .TRUE.  => the initial residual vector is given in RESID.
        //          .FALSE. => generate a random initial residual vector.
        
        //  N       Integer.  (INPUT)
        //          Dimension of the problem.
        
        //  J       Integer.  (INPUT)
        //          Index of the residual vector to be generated, with respect to
        //          the Arnoldi process.  J > 1 in case of a "restart".
        
        //  V       Double precision N by J array.  (INPUT)
        //          The first J-1 columns of V contain the current Arnoldi basis
        //          if this is a "restart".
        
        //  LDV     Integer.  (INPUT)
        //          Leading dimension of V exactly as declared in the calling 
        //          program.
        
        //  RESID   Double precision array of length N.  (INPUT/OUTPUT)
        //          Initial residual vector to be generated.  If RESID is 
        //          provided, force RESID into the range of the operator OP.
        
        //  RNORM   Double precision scalar.  (OUTPUT)
        //          B-norm of the generated residual.
        
        //  IPNTR   Integer array of length 3.  (OUTPUT)
        
        //  WORKD   Double precision work array of length 2*N.  (REVERSE COMMUNICATION).
        //          On exit, WORK(1:N) = B*RESID to be used in SSAITR.
        
        //  IERR    Integer.  (OUTPUT)
        //          =  0: Normal exit.
        //          = -1: Cannot generate a nontrivial restarted residual vector
        //                in the range of the operator OP.
        //
        // \EndDoc
        
        // -----------------------------------------------------------------------
        //
        // \BeginLib
        //
        // \Local variables:
        //    xxxxxx  real
        
        // \References:
        //  1. D.C. Sorensen, "Implicit Application of Polynomial Filters in
        //     a k-Step Arnoldi Method", SIAM J. Matr. Anal. Apps., 13 (1992),
        //     pp 357-385.
        //  2. R.B. Lehoucq, "Analysis and Implementation of an Implicitly 
        //     Restarted Arnoldi Iteration", Rice University Technical Report
        //     TR95-13, Department of Computational and Applied Mathematics.
        
        // \Routines called:
        //     second  ARPACK utility routine for timing.
        //     dvout   ARPACK utility routine for vector output.
        //     dlarnv  LAPACK routine for generating a random vector.
        //     dgemv   Level 2 BLAS routine for matrix vector multiplication.
        //     dcopy   Level 1 BLAS that copies one vector to another.
        //     ddot    Level 1 BLAS that computes the scalar product of two vectors. 
        //     dnrm2   Level 1 BLAS that computes the norm of a vector.
        
        // \Author
        //     Danny Sorensen               Phuong Vu
        //     Richard Lehoucq              CRPC / Rice University
        //     Dept. of Computational &     Houston, Texas
        //      Applied Mathematics
        //     Rice University           
        //     Houston, Texas            
        
        // \SCCS Information: @(#) 
        // FILE: getv0.F   SID: 2.6   DATE OF SID: 8/27/96   RELEASE: 2
        
        // \EndLib
        
        // -----------------------------------------------------------------------
        
              private void dgetv0 
                (int ido[], String bmat, int itry, boolean initv, int n, int j, double v[][], int ldv, double resid[], double rnorm[], 
                  int ipntr[], double workd[], int ierr[] ) {
         
        //     %----------------------------------------------------%
        //     | Include files for debugging and timing information |
        //     %----------------------------------------------------%
        
        //     include   'debug.h'
        //     include   'stat.h'
        
        //     %------------------%
        //     | Scalar Arguments |
        //     %------------------%
        
        //      character  bmat*1
        //      logical    initv
        //      integer    ido, ierr, itry, j, ldv, n
        //      Double precision
        //     &           rnorm
        
        //     %-----------------%
        //     | Array Arguments |
        //     %-----------------%
        
        //      integer    ipntr(3)
        //      Double precision
        //    &           resid(n), v(ldv,j), workd(2*n)
        
        //     %------------%
        //     | Parameters |
        //     %------------%
        
              final double zero = 0.0;
              final double one = 1.0;
        
        //     %------------------------%
        //     | Local Scalars & Arrays |
        //     %------------------------%
        
              int    idist, jj, i;
              boolean seg1;
        
        //     %----------------------%
        //     | External Subroutines |
        //     %----------------------%
        
        //     external   dlarnv, dvout, dcopy, dgemv, second
        
        //     %--------------------%
        //     | External Functions |
        //     %--------------------%
        
        //      Double precision
        //     &           ddot, dnrm2
        //      external   ddot, dnrm2
        
        //     %---------------------%
        //     | Intrinsic Functions |
        //     %---------------------%
        
        //     intrinsic    abs, sqrt
        
        //     %-----------------------%
        //     | Executable Statements |
        //     %-----------------------%
        
        
        //     %-----------------------------------%
        //     | Initialize the seed of the LAPACK |
        //     | random number generator           |
        //     %-----------------------------------%
        
              if (dgetv0_inits) {
                  dgetv0_iseed[0] = 1;
                  dgetv0_iseed[1] = 3;
                  dgetv0_iseed[2] = 5;
                  dgetv0_iseed[3] = 7;
                  dgetv0_inits = false;
              }
        
              if (ido[0] ==  0) {
         
        //        %-------------------------------%
        //        | Initialize timing statistics  |
        //        | & message level for debugging |
        //        %-------------------------------%
        
            	 t0 = System.currentTimeMillis();
                 dgetv0_msglvl = mgetv0;
        
                 ierr[0]   = 0;
                 dgetv0_iter   = 0;
                 dgetv0_first = false;
                 dgetv0_orth = false;
        
        //        %-----------------------------------------------------%
        //        | Possibly generate a random starting vector in RESID |
        //        | Use a LAPACK random number generator used by the    |
        //        | matrix generation routines.                         |
        //        |    idist = 1: uniform (0,1)  distribution;          |
        //        |    idist = 2: uniform (-1,1) distribution;          |
        //        |    idist = 3: normal  (0,1)  distribution;          |
        //        %-----------------------------------------------------%
        
                 if (!initv) {
                    idist = 2;
                    ge.dlarnv(idist, dgetv0_iseed, n, resid);
                 }
         
        //        %----------------------------------------------------------%
        //        | Force the starting vector into the range of OP to handle |
        //        | the generalized problem when B is possibly (singular).   |
        //        %----------------------------------------------------------%
        
                 t2 = System.currentTimeMillis();
                 if (bmat.equalsIgnoreCase("G")) {
                    nopx = nopx + 1;
                    ipntr[0] = 1;
                    ipntr[1] = n + 1;
                    for (i = 0; i < n; i++) {
                    	workd[i] = resid[i];
                    }
                    ido[0] = -1;
                    return;
                 } // if (bmat.equalsIgnoreCase("G"))
              } // if (ido[0] ==  0)
         
        //     %-----------------------------------------%
        //     | Back from computing OP*(initial-vector) |
        //     %-----------------------------------------%
        
              seg1 = true;
              loop1: while (true) {
              loop2: while (true) {
            	  if (seg1) {
            	  loop3: while (true) {
              if (dgetv0_first) {
            	  break loop3;
              }
        
        //     %-----------------------------------------------%
        //     | Back from computing B*(orthogonalized-vector) |
        //     %-----------------------------------------------%
        
              if (dgetv0_orth) {
            	  break loop2;
              }
        
              if (bmat.equalsIgnoreCase("G")) {
            	  t3 = System.currentTimeMillis();
                 tmvopx = tmvopx + (t3 - t2);
              }
         
        //     %------------------------------------------------------%
        //     | Starting vector is now in the range of OP; r = OP*r; |
        //     | Compute B-norm of starting vector.                   |
        //     %------------------------------------------------------%
        
              t2 = System.currentTimeMillis();
              dgetv0_first = true;
              if (bmat.equalsIgnoreCase("G")) {
                 nbx = nbx + 1;
                 for (i = 0; i < n; i++) {
                	 resid[i] = workd[n+i];
                 }
                 ipntr[0] = n + 1;
                 ipntr[1] = 1;
                 ido[0] = 2;
                 return;
              } // if (bmat.equalsIgnoreCase("G"))
              else if (bmat.equalsIgnoreCase("I")) {
                 for (i = 0; i < n; i++) {
                	 workd[i] = resid[i];
                 }
              }
              break loop3;
            	  } // loop3: while (true)
        
              if (bmat.equalsIgnoreCase("G")) {
            	  t3 = System.currentTimeMillis();
                 tmvbx = tmvbx + (t3 - t2);
              } //  if (bmat.equalsIgnoreCase("G"))
         
              dgetv0_first = false;
              if (bmat.equalsIgnoreCase("G")) {
                  dgetv0_rnorm0 = ge.ddot (n, resid, 1, workd, 1);
                  dgetv0_rnorm0 = Math.sqrt(Math.abs(dgetv0_rnorm0));
              } // if (bmat.equalsIgnoreCase("G"))
              else if (bmat.equalsIgnoreCase("I")) {
                   dgetv0_rnorm0 = ge.dnrm2(n, resid, 1);
              }
              rnorm[0]  = dgetv0_rnorm0;
        
        //     %---------------------------------------------%
        //     | Exit if this is the very first Arnoldi step |
        //     %---------------------------------------------%
        
              if (j == 1) {
            	  break loop1;
              }
         
        //     %----------------------------------------------------------------
        //     | Otherwise need to B-orthogonalize the starting vector against |
        //     | the current Arnoldi basis using Gram-Schmidt with iter. ref.  |
        //     | This is the case where an invariant subspace is encountered   |
        //     | in the middle of the Arnoldi factorization.                   |
        //     |                                                               |
        //     |       s = V^{T}*B*r;   r = r - V*s;                           |
        //     |                                                               |
        //     | Stopping criteria used for iter. ref. is discussed in         |
        //     | Parlett's book, page 107 and in Gragg & Reichel TOMS paper.   |
        //     %---------------------------------------------------------------%
        
              dgetv0_orth = true;
              } // if (seg1)
              double buffer[] = new double[j-1];
              for (i = 0; i < j-1; i++) {
            	  buffer[i] = workd[n+i];
              }
              ge.dgemv('T', n, j-1, one, v, ldv, workd, 1, 
                        zero, buffer, 1);
              ge.dgemv ('N', n, j-1, -one, v, ldv, buffer, 1, 
                         one, resid, 1);
              for (i = 0; i < j-1; i++) {
                  workd[n+i] = buffer[i];
              }
         
        //     %----------------------------------------------------------%
        //     | Compute the B-norm of the orthogonalized starting vector |
        //     %----------------------------------------------------------%
        
              t2 = System.currentTimeMillis();
              if (bmat.equalsIgnoreCase("G")) {
                 nbx = nbx + 1;
                 for (i = 0; i < n; i++) {
                	 workd[n+i] = resid[i];
                 }
                 ipntr[0] = n + 1;
                 ipntr[1] = 1;
                 ido[0] = 2;
                 return;
              } // if (bmat.equalsIgnoreCase("G"))
              else if (bmat.equalsIgnoreCase("I")) {
            	 for (i = 0; i < n; i++) {
            		 workd[i] = resid[i];
            	 }
              }
              break loop2;
              } // loop2: while (true)
        
              if (bmat.equalsIgnoreCase("G")) {
            	 t3 = System.currentTimeMillis();
                 tmvbx = tmvbx + (t3 - t2);
              } // if (bmat.equalsIgnoreCase("G"))
         
              if (bmat.equalsIgnoreCase("G")) {
                 rnorm[0] = ge.ddot(n, resid, 1, workd, 1);
                 rnorm[0] = Math.sqrt(Math.abs(rnorm[0]));
              } // if (bmat.equalsIgnoreCase("G"))
              else if (bmat.equalsIgnoreCase("I")) {
                 rnorm[0] = ge.dnrm2(n, resid, 1);
              }
        
        //     %--------------------------------------%
        //     | Check for further orthogonalization. |
        //     %--------------------------------------%
        
              if (dgetv0_msglvl > 2) {
            	  UI.setDataText("dgetv0: re-orthonalization ; dgetv0_rnorm0 is " + nf.format(dgetv0_rnorm0) + "\n");
            	  UI.setDataText("dgetv0: re-orthonalization ; rnorm[0] is " + nf.format(rnorm[0]) + "\n");
              } // if (dgetv0_msglvl > 2)
        
              if (rnorm[0] > 0.717*dgetv0_rnorm0) {
            	  break loop1;
              }
         
              dgetv0_iter = dgetv0_iter + 1;
              if (dgetv0_iter <= 1) {
        
        //        %-----------------------------------%
        //       | Perform iterative refinement step |
        //        %-----------------------------------%
        
                 dgetv0_rnorm0 = rnorm[0];
                 seg1 = false;
                 continue loop1;
              } // if (dgetv0_iter <= 1)
              else {
        
        //        %------------------------------------%
        //        | Iterative refinement step "failed" |
        //        %------------------------------------%
        
                 for (jj = 0; jj < n; jj++) {
                    resid[jj] = zero;
                 }
                 rnorm[0] = zero;
                 ierr[0] = -1;
                 break loop1;
              } // else
              } // loop1: while (true)
        
              if (dgetv0_msglvl > 0) {
            	 UI.setDataText("dgetv0: B-norm of initial / restarted starting vector rnorm[0] = " + nf.format(rnorm[0]) + "\n");
              }
              if (dgetv0_msglvl > 2) {
            	  UI.setDataText("dgetv0: initial / restarted starting vector resid\n");
            	  for (i = 0; i < n; i++) {
            		  UI.setDataText("resid["+i+"] = " + nf.format(resid[i]) + "\n");
            	  }
              }
              ido[0] = 99;
         
              t1 = System.currentTimeMillis();
              tgetv0 = tgetv0 + (t1 - t0);
   
              return;
              } // dgetv0


          
          //
          //\SCCS Information: @(#) 
          // FILE: stats.F   SID: 2.1   DATE OF SID: 4/19/96   RELEASE: 2
          //     %---------------------------------------------%
          //     | Initialize statistic and timing information |
          //     | for symmetric Arnoldi code.                 |
          //     %---------------------------------------------%
           
                private void dstats() {

          //     %--------------------------------%
          //     | See stat.doc for documentation |
          //     %--------------------------------%
          //     include   'stat.h'
           
          //     %-----------------------%
          //     | Executable Statements |
          //     %-----------------------%

                nopx   = 0;
                nbx    = 0;
                nrorth = 0;
                nitref = 0;
                nrstrt = 0;
           
                tsaupd = 0.0;
                tsaup2 = 0.0;
                tsaitr = 0.0;
                tseigt = 0.0;
                tsgets = 0.0;
                tsapps = 0.0;
                tsconv = 0.0;
                titref = 0.0;
                tgetv0 = 0.0;
           
          //     %----------------------------------------------------%
          //     | User time including reverse communication overhead |
          //     %----------------------------------------------------%
                tmvopx = 0.0;
                tmvbx  = 0.0;
           
                return;
                } // dstats


}