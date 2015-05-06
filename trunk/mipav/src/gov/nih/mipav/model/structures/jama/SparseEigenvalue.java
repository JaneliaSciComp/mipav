package gov.nih.mipav.model.structures.jama;


import java.text.DecimalFormat;

import gov.nih.mipav.view.MipavUtil;
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
	
	private ViewUserInterface UI = ViewUserInterface.getReference();
	
	DecimalFormat nf = new DecimalFormat("0.0000E0");
	
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
    private int dsaupd_mxiter;
    private int dsaupd_mode;
    private int dsaupd_nb;
    private int dsaupd_nev0;
    private int dsaupd_next;
    private int dsaupd_np;
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
    private double tnaupd;
    private double tnaup2;
    private double tnaitr;
    private double tneigt;
    private double tngets;
    private double tnapps;
    private double tnconv;
    private double tcaupd;
    private double tcaup2;
    private double tcaitr;
    private double tceigt;
    private double tcgets;
    private double tcapps;
    private double tcconv;
    private double titref;
    private double tgetv0;
    private double trvec;
    private double tmvopx;
    private double tmvbx;
    
    // From debug.h
    private int logfil;
    private int ndigit;
    private int mgetv0;
    private int msaupd;
    private int msaup2;
    private int msaitr; 
    private int mseigt; 
    private int msapps;
    private int msgets;
    private int mseupd;
    private int mnaupd;
    private int mnaup2;
    private int mnaitr;
    private int mneigh;
    private int mnapps;
    private int mngets;
    private int mneupd;
    private int mcaupd;
    private int mcaup2;
    private int mcaitr;
    private int mceigh;
    private int mcapps;
    private int mcgets;
    private int mceupd;
    
    private boolean dsaup2_cnorm;
    private boolean dsaup2_getv0;
    private boolean dsaup2_initv;
    private boolean dsaup2_update;
    private boolean dsaup2_ushift;
    private int dsaup2_iter;
    private int dsaup2_kplusp;
    private int dsaup2_msglvl;
    private int dsaup2_nconv;
    private int dsaup2_nev0;
    private int dsaup2_np0;
    private double dsaup2_rnorm;
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

	// ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new SparseEigenvalue object.
     */
    public SparseEigenvalue() {}
    
    
    
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
          private void dsaupd
            (int ido[], String bmat, int n, String which, int nev, double tol, double resid[], int ncv, double v[][], int ldv, int iparam[], 
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
          final double one = 1.0;
          final double zero = 0.0;
   
    //
    //     %---------------%
    //     | Local Scalars |
    //     %---------------%
    //
          int  j;
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
             dsaupd_mxiter = iparam[2];
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
             dsaupd_np     = ncv - nev;
    // 
             if (dsaupd_mxiter <= 0)  {
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
             if (tol <= zero)  {
            	 tol = ge.dlamch('E');
             }
    
    //        %----------------------------------------------%
    //        | NP is the number of additional steps to      |
    //        | extend the length NEV Lanczos factorization. |
    //        | NEV0 is the local variable designating the   |
    //        | size of the invariant subspace desired.      |
    //        %----------------------------------------------%
    
             dsaupd_np     = ncv - nev;
             dsaupd_nev0   = nev; 
     
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
    
             ipntr[3] = dsaupd_next-1;
             ipntr[4] = dsaupd_ih-1;
             ipntr[5] = dsaupd_ritz-1;
             ipntr[6] = dsaupd_bounds-1;
             ipntr[10] = dsaupd_iw-1;
          } // if (ido[0] == 0)
    
    //     %-------------------------------------------------------%
    //     | Carry out the Implicitly restarted Lanczos Iteration. |
    //     %-------------------------------------------------------%
    
         /** dsaup2 
            ( ido, bmat, n, which, dsaupd_nev0, dsaupd_np, tol, resid, dsaupd_mode, dsaupd_iupd,
              dsaupd_ishift, dsaupd_mxiter, v, ldv, workl(dsaupd_ih), dsaupd_ldh, workl(dsaupd_ritz),
              workl(dsaupd_bounds), workl(dsaupd_iq), dsaupd_ldq, workl(dsaupd_iw), ipntr, workd,
              info );
    c
    c     %--------------------------------------------------%
    c     | ido .ne. 99 implies use of reverse communication |
    c     | to compute operations involving OP or shifts.    |
    c     %--------------------------------------------------%
    c
          if (ido .eq. 3) iparam[7] = dsaupd_np
          if (ido .ne. 99) go to 9000
    c 
          iparam[2] = dsaupd_mxiter
          iparam[4] = dsaupd_np
          iparam[8] = nopx
          iparam[9] = nbx
          iparam[10] = nrorth
    c
    c     %------------------------------------%
    c     | Exit if there was an informational |
    c     | error within dsaup2.               |
    c     %------------------------------------%
    c
          if (info .lt. 0) go to 9000
          if (info .eq. 2) info = 3
    c
          if (dsaupd_msglvl .gt. 0) then
             call ivout (logfil, 1, dsaupd_mxiter, ndigit,
         &               '_saupd: number of update iterations taken')
             call ivout (logfil, 1, dsaupd_np, ndigit,
         &               '_saupd: number of "converged" Ritz values')
             call dvout (logfil, dsaupd_np, workl(Ritz), ndigit, 
         &               '_saupd: final Ritz values')
             call dvout (logfil, dsaupd_np, workl(Bounds), ndigit, 
         &               '_saupd: corresponding error bounds')
          end if 
    c
          call second (t1)
          tsaupd = t1 - t0
    c 
          if (dsaupd_msglvl .gt. 0) then
    c
    c        %--------------------------------------------------------%
    c        | Version Number & Version Date are defined in version.h |
    c        %--------------------------------------------------------%
    c
             write (6,1000)
             write (6,1100) dsaupd_mxiter, nopx, nbx, nrorth, nitref, nrstrt,
         &                  tmvopx, tmvbx, tsaupd, tsaup2, tsaitr, titref,
         &                  tgetv0, tseigt, tsgets, tsapps, tsconv
     1000    format (//,
         &      5x, '==========================================',/
         &      5x, '= Symmetric implicit Arnoldi update code =',/
         &      5x, '= Version Number:', ' 2.4', 19x, ' =',/
         &      5x, '= Version Date:  ', ' 07/31/96', 14x, ' =',/
         &      5x, '==========================================',/
         &      5x, '= Summary of timing statistics           =',/
         &      5x, '==========================================',//)
     1100    format (
         &      5x, 'Total number update iterations             = ', i5,/
         &      5x, 'Total number of OP*x operations            = ', i5,/
         &      5x, 'Total number of B*x operations             = ', i5,/
         &      5x, 'Total number of reorthogonalization steps  = ', i5,/
         &      5x, 'Total number of iterative refinement steps = ', i5,/
         &      5x, 'Total number of restart steps              = ', i5,/
         &      5x, 'Total time in user OP*x operation          = ', f12.6,/
         &      5x, 'Total time in user B*x operation           = ', f12.6,/
         &      5x, 'Total time in Arnoldi update routine       = ', f12.6,/
         &      5x, 'Total time in saup2 routine                = ', f12.6,/
         &      5x, 'Total time in basic Arnoldi iteration loop = ', f12.6,/
         &      5x, 'Total time in reorthogonalization phase    = ', f12.6,/
         &      5x, 'Total time in (re)start vector generation  = ', f12.6,/
         &      5x, 'Total time in trid eigenvalue subproblem   = ', f12.6,/
         &      5x, 'Total time in getting the shifts           = ', f12.6,/
         &      5x, 'Total time in applying the shifts          = ', f12.6,/
         &      5x, 'Total time in convergence testing          = ', f12.6)
          end if
    c 
     9000 continue
    c 
          return
    c
    c     %---------------%
    c     | End of dsaupd |
    c     %---------------%
    c
          end*/
          }
          
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
                  ( int ido[], String bmat, int n, String which, int nev, int np[], double tol, double resid[], int mode, int iupd, 
                    int ishift, int mxiter, double v[][], int ldv, double h[][], int ldh, double ritz[], double bounds[], 
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
                final double one = 1.0;
          
          //     %---------------%
          //     | Local Scalars |
          //     %---------------%
          
                String  wprime;
                int    ierr, j, nevbef, nptemp, nevd2, nevm2;
                int kp[] = new int [3]; 
                double temp;
                boolean seg1;
                boolean seg2;
                boolean seg3;
                boolean seg4;
                boolean seg5;
                boolean seg6;
          
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
          
                   dsaup2_nev0   = nev;
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
                   dsaup2_nconv  = 0;
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
          
          
                 /*if (dsaup2_getv0) {
                   dgetv0 (ido, bmat, 1, dsaup2_initv, n, 1, v, ldv, resid, dsaup2_rnorm,
                               ipntr, workd, info);
          
                   if (ido[0] != 99) {
                	   return;
                   }
          
                   if (dsaup2_rnorm == zero) {
          
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
                call dsaitr (ido, bmat, n, 0, dsaup2_nev0, mode, resid, dsaup2_rnorm, v, ldv, 
               &             h, ldh, ipntr, workd, info)
          c 
          c     %---------------------------------------------------%
          c     | ido .ne. 99 implies use of reverse communication  |
          c     | to compute operations involving OP and possibly B |
          c     %---------------------------------------------------%
          c
                if (ido .ne. 99) go to 9000
          c
                if (info .gt. 0) then
          c
          c        %-----------------------------------------------------%
          c        | dsaitr was unable to build an Lanczos factorization |
          c        | of length NEV0. INFO is returned with the size of   |
          c        | the factorization built. Exit main loop.            |
          c        %-----------------------------------------------------%
          c
                   np   = info
                   mxiter = dsaup2_iter;
                   info = -9999
                   go to 1200
                end if
                } // if (seg3)
                } // if (seg2)
                } // if (seg1)
          c 
          c     %--------------------------------------------------------------%
          c     |                                                              |
          c     |           M A I N  LANCZOS  I T E R A T I O N  L O O P       |
          c     |           Each iteration implicitly restarts the Lanczos     |
          c     |           factorization in place.                            |
          c     |                                                              |
          c     %--------------------------------------------------------------%
          c 
           loop1: while (true) {
        	   if (seg4) {
        		   if (seg5) {
        			   if (seg6) {
          c
                   dsaup2_iter = dsaup2_iter + 1;
          c
                   if (dsaup2_msglvl .gt. 0) then
                      call ivout (logfil, 1, dsaup2_iter, ndigit, 
               &           '_saup2: **** Start of major iteration number ****')
                   end if
                   if (dsaup2_msglvl .gt. 1) then
                      call ivout (logfil, 1, nev, ndigit, 
               &     '_saup2: The length of the current Lanczos factorization')
                      call ivout (logfil, 1, np, ndigit, 
               &           '_saup2: Extend the Lanczos factorization by')
                   end if
          c 
          c        %------------------------------------------------------------%
          c        | Compute NP additional steps of the Lanczos factorization. |
          c        %------------------------------------------------------------%
          c
                   ido = 0
        			   } // if (seg6)
        			   seg6 = true;
             20    continue
                   dsaup2_update = true;
          c
                   call dsaitr (ido, bmat, n, nev, np, mode, resid, dsaup2_rnorm, v, 
               &                ldv, h, ldh, ipntr, workd, info)
          c 
          c        %---------------------------------------------------%
          c        | ido .ne. 99 implies use of reverse communication  |
          c        | to compute operations involving OP and possibly B |
          c        %---------------------------------------------------%
          c
                   if (ido .ne. 99) go to 9000
          c
                   if (info .gt. 0) then
          c
          c           %-----------------------------------------------------%
          c           | dsaitr was unable to build an Lanczos factorization |
          c           | of length NEV0+NP0. INFO is returned with the size  |  
          c           | of the factorization built. Exit main loop.         |
          c           %-----------------------------------------------------%
          c
                      np = info
                      mxiter = dsaup2_iter;
                      info = -9999
                      go to 1200
                   end if
                   dsaup2_update = false;
          c
                   if (dsaup2_msglvl .gt. 1) then
                      call dvout (logfil, 1, dsaup2_rnorm, ndigit, 
               &           '_saup2: Current B-norm of residual for factorization')
                   end if
          c 
          c        %--------------------------------------------------------%
          c        | Compute the eigenvalues and corresponding error bounds |
          c        | of the current symmetric tridiagonal matrix.           |
          c        %--------------------------------------------------------%
          c
                   call dseigt (dsaup2_rnorm, dsaup2_kplusp, h, ldh, ritz, bounds, workl, ierr)
          c
                   if (ierr .ne. 0) then
                      info = -8
                      go to 1200
                   end if
          c
          c        %----------------------------------------------------%
          c        | Make a copy of eigenvalues and corresponding error |
          c        | bounds obtained from _seigt.                       |
          c        %----------------------------------------------------%
          c
                   call dcopy(dsaup2_kplusp, ritz, 1, workl(dsaup2_kplusp+1), 1)
                   call dcopy(dsaup2_kplusp, bounds, 1, workl(2*dsaup2_kplusp+1), 1)
          c
          c        %---------------------------------------------------%
          c        | Select the wanted Ritz values and their bounds    |
          c        | to be used in the convergence test.               |
          c        | The selection is based on the requested number of |
          c        | eigenvalues instead of the current NEV and NP to  |
          c        | prevent possible misconvergence.                  |
          c        | * Wanted Ritz values := RITZ(NP+1:NEV+NP)         |
          c        | * Shifts := RITZ(1:NP) := WORKL(1:NP)             |
          c        %---------------------------------------------------%
          c
                   nev = dsaup2_nev0;
                   np = dsaup2_np0;
                   call dsgets (ishift, which, nev, np, ritz, bounds, workl)
          c 
          c        %-------------------%
          c        | Convergence test. |
          c        %-------------------%
          c
                   call dcopy (nev, bounds(np+1), 1, workl(np+1), 1)
                   call dsconv (nev, ritz(np+1), workl(np+1), tol, dsaup2_nconv)
          c
                   if (dsaup2_msglvl .gt. 2) then
                      kp(1) = nev
                      kp(2) = np
                      kp(3) = dsaup2_nconv;
                      call ivout (logfil, 3, kp, ndigit,
               &                  '_saup2: NEV, NP, NCONV are')
                      call dvout (logfil, dsaup2_kplusp, ritz, ndigit,
               &           '_saup2: The eigenvalues of H')
                      call dvout (logfil, dsaup2_kplusp, bounds, ndigit,
               &          '_saup2: Ritz estimates of the current NCV Ritz values')
                   end if
          c
          c        %---------------------------------------------------------%
          c        | Count the number of unwanted Ritz values that have zero |
          c        | Ritz estimates. If any Ritz estimates are equal to zero |
          c        | then a leading block of H of order equal to at least    |
          c        | the number of Ritz values with zero Ritz estimates has  |
          c        | split off. None of these Ritz values may be removed by  |
          c        | shifting. Decrease NP the number of shifts to apply. If |
          c        | no shifts may be applied, then prepare to exit          |
          c        %---------------------------------------------------------%
          c
                   nptemp = np
                   do 30 j=1, nptemp
                      if (bounds(j) .eq. zero) then
                         np = np - 1
                         nev = nev + 1
                      end if
           30      continue
          c 
                   if ( (dsaup2_nconv .ge. dsaup2_nev0) .or. 
               &        (dsaup2_iter .gt. mxiter) .or.
               &        (np .eq. 0) ) then
          c     
          c           %------------------------------------------------%
          c           | Prepare to exit. Put the converged Ritz values |
          c           | and corresponding bounds in RITZ(1:NCONV) and  |
          c           | BOUNDS(1:NCONV) respectively. Then sort. Be    |
          c           | careful when NCONV > NP since we don't want to |
          c           | swap overlapping locations.                    |
          c           %------------------------------------------------%
          c
                      if (which .eq. 'BE') then
          c
          c              %-----------------------------------------------------%
          c              | Both ends of the spectrum are requested.            |
          c              | Sort the eigenvalues into algebraically decreasing  |
          c              | order first then swap low end of the spectrum next  |
          c              | to high end in appropriate locations.               |
          c              | NOTE: when np < floor(nev/2) be careful not to swap |
          c              | overlapping locations.                              |
          c              %-----------------------------------------------------%
          c
                         wprime = 'SA'
                         call dsortr (wprime, .true., dsaup2_kplusp, ritz, bounds)
                         nevd2 = nev / 2
                         nevm2 = nev - nevd2 
                         if ( nev .gt. 1 ) then
                            call dswap ( min(nevd2,np), ritz(nevm2+1), 1,
               &                 ritz( max(dsaup2_kplusp-nevd2+1,dsaup2_kplusp-np+1) ), 1)
                            call dswap ( min(nevd2,np), bounds(nevm2+1), 1,
               &                 bounds( max(dsaup2_kplusp-nevd2+1,dsaup2_kplusp-np)+1 ), 1)
                         end if
          c
                      else
          c
          c              %--------------------------------------------------%
          c              | LM, SM, LA, SA case.                             |
          c              | Sort the eigenvalues of H into the an order that |
          c              | is opposite to WHICH, and apply the resulting    |
          c              | order to BOUNDS.  The eigenvalues are sorted so  |
          c              | that the wanted part are always within the first |
          c              | NEV locations.                                   |
          c              %--------------------------------------------------%
          c
                         if (which .eq. 'LM') wprime = 'SM'
                         if (which .eq. 'SM') wprime = 'LM'
                         if (which .eq. 'LA') wprime = 'SA'
                         if (which .eq. 'SA') wprime = 'LA'
          c
                         call dsortr (wprime, .true., dsaup2_kplusp, ritz, bounds)
          c
                      end if
          c
          c           %--------------------------------------------------%
          c           | Scale the Ritz estimate of each Ritz value       |
          c           | by 1 / max(eps23,magnitude of the Ritz value).   |
          c           %--------------------------------------------------%
          c
                      do 35 j = 1, dsaup2_nev0
                         temp = max( dsaup2_eps23, abs(ritz(j)) )
                         bounds(j) = bounds(j)/temp
           35         continue
          c
          c           %----------------------------------------------------%
          c           | Sort the Ritz values according to the scaled Ritz  |
          c           | esitmates.  This will push all the converged ones  |
          c           | towards the front of ritzr, ritzi, bounds          |
          c           | (in the case when NCONV < NEV.)                    |
          c           %----------------------------------------------------%
          c
                      wprime = 'LA'
                      call dsortr(wprime, .true., dsaup2_nev0, bounds, ritz)
          c
          c           %----------------------------------------------%
          c           | Scale the Ritz estimate back to its original |
          c           | value.                                       |
          c           %----------------------------------------------%
          c
                      do 40 j = 1, dsaup2_nev0
                          temp = max( dsaup2_eps23, abs(ritz(j)) )
                          bounds(j) = bounds(j)*temp
           40         continue
          c
          c           %--------------------------------------------------%
          c           | Sort the "converged" Ritz values again so that   |
          c           | the "threshold" values and their associated Ritz |
          c           | estimates appear at the appropriate position in  |
          c           | ritz and bound.                                  |
          c           %--------------------------------------------------%
          c
                      if (which .eq. 'BE') then
          c
          c              %------------------------------------------------%
          c              | Sort the "converged" Ritz values in increasing |
          c              | order.  The "threshold" values are in the      |
          c              | middle.                                        |
          c              %------------------------------------------------%
          c
                         wprime = 'LA'
                         call dsortr(wprime, .true., dsaup2_nconv, ritz, bounds)
          c
                      else
          c
          c              %----------------------------------------------%
          c              | In LM, SM, LA, SA case, sort the "converged" |
          c              | Ritz values according to WHICH so that the   |
          c              | "threshold" value appears at the front of    |
          c              | ritz.                                        |
          c              %----------------------------------------------%

                         call dsortr(which, .true., dsaup2_nconv, ritz, bounds)
          c
                      end if
          c
          c           %------------------------------------------%
          c           |  Use h( 1,1 ) as storage to communicate  |
          c           |  rnorm to _seupd if needed               |
          c           %------------------------------------------%
          c
                      h(1,1) = dsaup2_rnorm;
          c
                      if (dsaup2_msglvl .gt. 1) then
                         call dvout (logfil, dsaup2_kplusp, ritz, ndigit,
               &            '_saup2: Sorted Ritz values.')
                         call dvout (logfil, dsaup2_kplusp, bounds, ndigit,
               &            '_saup2: Sorted ritz estimates.')
                      end if
          c
          c           %------------------------------------%
          c           | Max iterations have been exceeded. | 
          c           %------------------------------------%
          c
                      if (dsaup2_iter .gt. mxiter .and. dsaup2_nconv .lt. nev) info = 1
          c
          c           %---------------------%
          c           | No shifts to apply. | 
          c           %---------------------%
          c
                      if (np .eq. 0 .and. dsaup2_nconv .lt. dsaup2_nev0) info = 2
          c
                      np = dsaup2_nconv;
                      go to 1100
          c
                   else if (dsaup2_nconv .lt. nev .and. ishift .eq. 1) then
          c
          c           %---------------------------------------------------%
          c           | Do not have all the requested eigenvalues yet.    |
          c           | To prevent possible stagnation, adjust the number |
          c           | of Ritz values and the shifts.                    |
          c           %---------------------------------------------------%
          c
                      nevbef = nev
                      nev = nev + min (dsaup2_nconv, np/2)
                      if (nev .eq. 1 .and. dsaup2_kplusp .ge. 6) then
                         nev = dsaup2_kplusp / 2
                      else if (nev .eq. 1 .and. dsaup2_kplusp .gt. 2) then
                         nev = 2
                      end if
                      np  = dsaup2_kplusp - nev
          c     
          c           %---------------------------------------%
          c           | If the size of NEV was just increased |
          c           | resort the eigenvalues.               |
          c           %---------------------------------------%
          c     
                      if (nevbef .lt. nev) 
               &         call dsgets (ishift, which, nev, np, ritz, bounds,
               &              workl)
          c
                   end if
          c
                   if (dsaup2_msglvl .gt. 0) then
                      call ivout (logfil, 1, dsaup2_nconv, ndigit,
               &           '_saup2: no. of "converged" Ritz values at this iter.')
                      if (dsaup2_msglvl .gt. 1) then
                         kp(1) = nev
                         kp(2) = np
                         call ivout (logfil, 2, kp, ndigit,
               &              '_saup2: NEV and NP are')
                         call dvout (logfil, nev, ritz(np+1), ndigit,
               &              '_saup2: "wanted" Ritz values.')
                         call dvout (logfil, nev, bounds(np+1), ndigit,
               &              '_saup2: Ritz estimates of the "wanted" values ')
                      end if
                   end if

          c 
                   if (ishift .eq. 0) then
          c
          c           %-----------------------------------------------------%
          c           | User specified shifts: reverse communication to     |
          c           | compute the shifts. They are returned in the first  |
          c           | NP locations of WORKL.                              |
          c           %-----------------------------------------------------%
          c
                      dsaup2_ushift = true;
                      ido = 3
                      go to 9000
                   end if
        		   } // if (seg5)
        		   seg5 = true;
             50    continue
          c
          c        %------------------------------------%
          c        | Back from reverse communication;   |
          c        | User specified shifts are returned |
          c        | in WORKL(1:*NP)                   |
          c        %------------------------------------%
          c
                   dsaup2_ushift = false;
          c 
          c 
          c        %---------------------------------------------------------%
          c        | Move the NP shifts to the first NP locations of RITZ to |
          c        | free up WORKL.  This is for the non-exact shift case;   |
          c        | in the exact shift case, dsgets already handles this.   |
          c        %---------------------------------------------------------%
          c
                   if (ishift .eq. 0) call dcopy (np, workl, 1, ritz, 1)
          c
                   if (dsaup2_msglvl .gt. 2) then
                      call ivout (logfil, 1, np, ndigit,
               &                  '_saup2: The number of shifts to apply ')
                      call dvout (logfil, np, workl, ndigit,
               &                  '_saup2: shifts selected')
                      if (ishift .eq. 1) then
                         call dvout (logfil, np, bounds, ndigit,
               &                  '_saup2: corresponding Ritz estimates')
                       end if
                   end if
          c 
          c        %---------------------------------------------------------%
          c        | Apply the NP0 implicit shifts by QR bulge chasing.      |
          c        | Each shift is applied to the entire tridiagonal matrix. |
          c        | The first 2*N locations of WORKD are used as workspace. |
          c        | After dsapps is done, we have a Lanczos                 |
          c        | factorization of length NEV.                            |
          c        %---------------------------------------------------------%
          c
                   call dsapps (n, nev, np, ritz, v, ldv, h, ldh, resid, q, ldq,
               &        workd)
          c
          c        %---------------------------------------------%
          c        | Compute the B-norm of the updated residual. |
          c        | Keep B*RESID in WORKD(1:N) to be used in    |
          c        | the first step of the next call to dsaitr.  |
          c        %---------------------------------------------%
          c
                   dsaup2_cnorm = true;
                   call second (t2)
                   if (bmat .eq. 'G') then
                      nbx = nbx + 1
                      call dcopy (n, resid, 1, workd(n+1), 1)
                      ipntr(1) = n + 1
                      ipntr(2) = 1
                      ido = 2
          c 
          c           %----------------------------------%
          c           | Exit in order to compute B*RESID |
          c           %----------------------------------%
          c 
                      go to 9000
                   else if (bmat .eq. 'I') then
                      call dcopy (n, resid, 1, workd, 1)
                   end if
        	   } // if (seg4) 
        	   seg4 = true;
            100    continue
          c 
          c        %----------------------------------%
          c        | Back from reverse communication; |
          c        | WORKD(1:N) := B*RESID            |
          c        %----------------------------------%
          c
                   if (bmat .eq. 'G') then
                      call second (t3)
                      tmvbx = tmvbx + (t3 - t2)
                   end if
          c 
                   if (bmat .eq. 'G') then         
                      dsaup2_rnorm = ddot (n, resid, 1, workd, 1)
                      dsaup2_rnorm = sqrt(abs(dsaup2_rnorm));
                   else if (bmat .eq. 'I') then
                      dsaup2_rnorm = dnrm2(n, resid, 1);
                   end if
                   dsaup2_cnorm = false;
            130    continue
          c
                   if (dsaup2_msglvl .gt. 2) then
                      call dvout (logfil, 1, dsaup2_rnorm, ndigit, 
               &      '_saup2: B-norm of residual for NEV factorization')
                      call dvout (logfil, nev, h(1,2), ndigit,
               &           '_saup2: main diagonal of compressed H matrix')
                      call dvout (logfil, nev-1, h(2,1), ndigit,
               &           '_saup2: subdiagonal of compressed H matrix')
                   end if
           
           } // loop1: while (true)
          c
          c     %---------------------------------------------------------------%
          c     |                                                               |
          c     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
          c     |                                                               |
          c     %---------------------------------------------------------------%
          c 
           1100 continue
          c
                mxiter = dsaup2_iter;
                nev = dsaup2_nconv;
          c 
           1200 continue
                ido = 99
          c
          c     %------------%
          c     | Error exit |
          c     %------------%
          c
                call second (t1)
                tsaup2 = t1 - t0
          c 
           9000 continue
                return
          c
          c     %---------------%
          c     | End of dsaup2 |
          c     %---------------%
          c*/
                }
                
                
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
        
        //     %-----------------------%
        //     | Local Array Arguments | 
        //     %-----------------------%
        
              double xtemp[] = new double[2];
        
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
                 } // if (seg8)
                 seg8 = true;
                    dsaitr_rstart = true;
                    ido[0]    = 0;
                 } // if (seg6)
                 seg6 = true;
                } // if (seg5)
                seg5 = true;
               if (seg7) {
        
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
               } // if (seg7)
           seg7 = true;
        
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
                 for (m = 0; m < n; m++) {
                	 buffer2[m] = workd[dsaitr_irj-1+m];
                 }
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
                    xtemp[0] = dsaitr_wnorm;
                    xtemp[1] = rnorm[0];
                    UI.setDataText("dsaitr: re-orthonalization ; wnorm = " + nf.format(dsaitr_wnorm) +
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
                        xtemp[0] = rnorm[0];
                        xtemp[1] = dsaitr_rnorm1;
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
        
              t3 = System.currentTimeMillis();
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
                trvec  = 0.0;
           
          //     %----------------------------------------------------%
          //     | User time including reverse communication overhead |
          //     %----------------------------------------------------%
                tmvopx = 0.0;
                tmvbx  = 0.0;
           
                return;
                } // dstats


}