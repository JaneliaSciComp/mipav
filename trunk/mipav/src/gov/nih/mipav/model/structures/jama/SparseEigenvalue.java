package gov.nih.mipav.model.structures.jama;


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

	// ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new SparseEigenvalue object.
     */
    public SparseEigenvalue() {}
    
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
          /**c
          c     %----------------------------------------------------%
          c     | Include files for debugging and timing information |
          c     %----------------------------------------------------%
          c
                include   'debug.h'
                include   'stat.h'
          c
          c     %------------------%
          c     | Scalar Arguments |
          c     %------------------%
          c
                character  bmat*1, which*2
                integer    ido, info, ishift, iupd, ldh, ldq, ldv, mxiter,
               &           n, mode, nev, np
                Double precision
               &           tol
          c
          c     %-----------------%
          c     | Array Arguments |
          c     %-----------------%
          c
                integer    ipntr(3)
                Double precision
               &           bounds(nev+np), h(ldh,2), q(ldq,nev+np), resid(n), 
               &           ritz(nev+np), v(ldv,nev+np), workd(3*n), 
               &           workl(3*(nev+np))
          c
          c     %------------%
          c     | Parameters |
          c     %------------%
          c
                Double precision
               &           one, zero
                parameter (one = 1.0D+0, zero = 0.0D+0)
          c
          c     %---------------%
          c     | Local Scalars |
          c     %---------------%
          c
                character  wprime*2
                logical    cnorm, getv0, initv, update, ushift
                integer    ierr, iter, j, kplusp, msglvl, nconv, nevbef, nev0, 
               &           np0, nptemp, nevd2, nevm2, kp(3) 
                Double precision
               &           rnorm, temp, eps23
                save       cnorm, getv0, initv, update, ushift,
               &           iter, kplusp, msglvl, nconv, nev0, np0,
               &           rnorm, eps23
          c
          c     %----------------------%
          c     | External Subroutines |
          c     %----------------------%
          c
                external   dcopy, dgetv0, dsaitr, dscal, dsconv, dseigt, dsgets, 
               &           dsapps, dsortr, dvout, ivout, second, dswap
          c
          c     %--------------------%
          c     | External Functions |
          c     %--------------------%
          c
                Double precision
               &           ddot, dnrm2, dlamch
                external   ddot, dnrm2, dlamch
          c
          c     %---------------------%
          c     | Intrinsic Functions |
          c     %---------------------%
          c
                intrinsic    min
          c
          c     %-----------------------%
          c     | Executable Statements |
          c     %-----------------------%
          c
                if (ido .eq. 0) then
          c 
          c        %-------------------------------%
          c        | Initialize timing statistics  |
          c        | & message level for debugging |
          c        %-------------------------------%
          c
                   call second (t0)
                   msglvl = msaup2
          c
          c        %---------------------------------%
          c        | Set machine dependent constant. |
          c        %---------------------------------%
          c
                   eps23 = dlamch('Epsilon-Machine')
                   eps23 = eps23**(2.0D+0/3.0D+0)
          c
          c        %-------------------------------------%
          c        | nev0 and np0 are integer variables  |
          c        | hold the initial values of NEV & NP |
          c        %-------------------------------------%
          c
                   nev0   = nev
                   np0    = np
          c
          c        %-------------------------------------%
          c        | kplusp is the bound on the largest  |
          c        |        Lanczos factorization built. |
          c        | nconv is the current number of      |
          c        |        "converged" eigenvlues.      |
          c        | iter is the counter on the current  |
          c        |      iteration step.                |
          c        %-------------------------------------%
          c
                   kplusp = nev0 + np0
                   nconv  = 0
                   iter   = 0
          c 
          c        %--------------------------------------------%
          c        | Set flags for computing the first NEV steps |
          c        | of the Lanczos factorization.              |
          c        %--------------------------------------------%
          c
                   getv0    = .true.
                   update   = .false.
                   ushift   = .false.
                   cnorm    = .false.
          c
                   if (info .ne. 0) then
          c
          c        %--------------------------------------------%
          c        | User provides the initial residual vector. |
          c        %--------------------------------------------%
          c
                      initv = .true.
                      info  = 0
                   else
                      initv = .false.
                   end if
                end if
          c 
          c     %---------------------------------------------%
          c     | Get a possibly random starting vector and   |
          c     | force it into the range of the operator OP. |
          c     %---------------------------------------------%
          c
             10 continue
          c
                if (getv0) then
                   call dgetv0 (ido, bmat, 1, initv, n, 1, v, ldv, resid, rnorm,
               &                ipntr, workd, info)
          c
                   if (ido .ne. 99) go to 9000
          c
                   if (rnorm .eq. zero) then
          c
          c           %-----------------------------------------%
          c           | The initial vector is zero. Error exit. | 
          c           %-----------------------------------------%
          c
                      info = -9
                      go to 1200
                   end if
                   getv0 = .false.
                   ido  = 0
                end if
          c 
          c     %------------------------------------------------------------%
          c     | Back from reverse communication: continue with update step |
          c     %------------------------------------------------------------%
          c
                if (update) go to 20
          c
          c     %-------------------------------------------%
          c     | Back from computing user specified shifts |
          c     %-------------------------------------------%
          c
                if (ushift) go to 50
          c
          c     %-------------------------------------%
          c     | Back from computing residual norm   |
          c     | at the end of the current iteration |
          c     %-------------------------------------%
          c
                if (cnorm)  go to 100
          c 
          c     %----------------------------------------------------------%
          c     | Compute the first NEV steps of the Lanczos factorization |
          c     %----------------------------------------------------------%
          c
                call dsaitr (ido, bmat, n, 0, nev0, mode, resid, rnorm, v, ldv, 
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
                   mxiter = iter
                   info = -9999
                   go to 1200
                end if
          c 
          c     %--------------------------------------------------------------%
          c     |                                                              |
          c     |           M A I N  LANCZOS  I T E R A T I O N  L O O P       |
          c     |           Each iteration implicitly restarts the Lanczos     |
          c     |           factorization in place.                            |
          c     |                                                              |
          c     %--------------------------------------------------------------%
          c 
           1000 continue
          c
                   iter = iter + 1
          c
                   if (msglvl .gt. 0) then
                      call ivout (logfil, 1, iter, ndigit, 
               &           '_saup2: **** Start of major iteration number ****')
                   end if
                   if (msglvl .gt. 1) then
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
             20    continue
                   update = .true.
          c
                   call dsaitr (ido, bmat, n, nev, np, mode, resid, rnorm, v, 
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
                      mxiter = iter
                      info = -9999
                      go to 1200
                   end if
                   update = .false.
          c
                   if (msglvl .gt. 1) then
                      call dvout (logfil, 1, rnorm, ndigit, 
               &           '_saup2: Current B-norm of residual for factorization')
                   end if
          c 
          c        %--------------------------------------------------------%
          c        | Compute the eigenvalues and corresponding error bounds |
          c        | of the current symmetric tridiagonal matrix.           |
          c        %--------------------------------------------------------%
          c
                   call dseigt (rnorm, kplusp, h, ldh, ritz, bounds, workl, ierr)
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
                   call dcopy(kplusp, ritz, 1, workl(kplusp+1), 1)
                   call dcopy(kplusp, bounds, 1, workl(2*kplusp+1), 1)
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
                   nev = nev0
                   np = np0
                   call dsgets (ishift, which, nev, np, ritz, bounds, workl)
          c 
          c        %-------------------%
          c        | Convergence test. |
          c        %-------------------%
          c
                   call dcopy (nev, bounds(np+1), 1, workl(np+1), 1)
                   call dsconv (nev, ritz(np+1), workl(np+1), tol, nconv)
          c
                   if (msglvl .gt. 2) then
                      kp(1) = nev
                      kp(2) = np
                      kp(3) = nconv
                      call ivout (logfil, 3, kp, ndigit,
               &                  '_saup2: NEV, NP, NCONV are')
                      call dvout (logfil, kplusp, ritz, ndigit,
               &           '_saup2: The eigenvalues of H')
                      call dvout (logfil, kplusp, bounds, ndigit,
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
                   if ( (nconv .ge. nev0) .or. 
               &        (iter .gt. mxiter) .or.
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
                         call dsortr (wprime, .true., kplusp, ritz, bounds)
                         nevd2 = nev / 2
                         nevm2 = nev - nevd2 
                         if ( nev .gt. 1 ) then
                            call dswap ( min(nevd2,np), ritz(nevm2+1), 1,
               &                 ritz( max(kplusp-nevd2+1,kplusp-np+1) ), 1)
                            call dswap ( min(nevd2,np), bounds(nevm2+1), 1,
               &                 bounds( max(kplusp-nevd2+1,kplusp-np)+1 ), 1)
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
                         call dsortr (wprime, .true., kplusp, ritz, bounds)
          c
                      end if
          c
          c           %--------------------------------------------------%
          c           | Scale the Ritz estimate of each Ritz value       |
          c           | by 1 / max(eps23,magnitude of the Ritz value).   |
          c           %--------------------------------------------------%
          c
                      do 35 j = 1, nev0
                         temp = max( eps23, abs(ritz(j)) )
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
                      call dsortr(wprime, .true., nev0, bounds, ritz)
          c
          c           %----------------------------------------------%
          c           | Scale the Ritz estimate back to its original |
          c           | value.                                       |
          c           %----------------------------------------------%
          c
                      do 40 j = 1, nev0
                          temp = max( eps23, abs(ritz(j)) )
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
                         call dsortr(wprime, .true., nconv, ritz, bounds)
          c
                      else
          c
          c              %----------------------------------------------%
          c              | In LM, SM, LA, SA case, sort the "converged" |
          c              | Ritz values according to WHICH so that the   |
          c              | "threshold" value appears at the front of    |
          c              | ritz.                                        |
          c              %----------------------------------------------%

                         call dsortr(which, .true., nconv, ritz, bounds)
          c
                      end if
          c
          c           %------------------------------------------%
          c           |  Use h( 1,1 ) as storage to communicate  |
          c           |  rnorm to _seupd if needed               |
          c           %------------------------------------------%
          c
                      h(1,1) = rnorm
          c
                      if (msglvl .gt. 1) then
                         call dvout (logfil, kplusp, ritz, ndigit,
               &            '_saup2: Sorted Ritz values.')
                         call dvout (logfil, kplusp, bounds, ndigit,
               &            '_saup2: Sorted ritz estimates.')
                      end if
          c
          c           %------------------------------------%
          c           | Max iterations have been exceeded. | 
          c           %------------------------------------%
          c
                      if (iter .gt. mxiter .and. nconv .lt. nev) info = 1
          c
          c           %---------------------%
          c           | No shifts to apply. | 
          c           %---------------------%
          c
                      if (np .eq. 0 .and. nconv .lt. nev0) info = 2
          c
                      np = nconv
                      go to 1100
          c
                   else if (nconv .lt. nev .and. ishift .eq. 1) then
          c
          c           %---------------------------------------------------%
          c           | Do not have all the requested eigenvalues yet.    |
          c           | To prevent possible stagnation, adjust the number |
          c           | of Ritz values and the shifts.                    |
          c           %---------------------------------------------------%
          c
                      nevbef = nev
                      nev = nev + min (nconv, np/2)
                      if (nev .eq. 1 .and. kplusp .ge. 6) then
                         nev = kplusp / 2
                      else if (nev .eq. 1 .and. kplusp .gt. 2) then
                         nev = 2
                      end if
                      np  = kplusp - nev
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
                   if (msglvl .gt. 0) then
                      call ivout (logfil, 1, nconv, ndigit,
               &           '_saup2: no. of "converged" Ritz values at this iter.')
                      if (msglvl .gt. 1) then
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
                      ushift = .true.
                      ido = 3
                      go to 9000
                   end if
          c
             50    continue
          c
          c        %------------------------------------%
          c        | Back from reverse communication;   |
          c        | User specified shifts are returned |
          c        | in WORKL(1:*NP)                   |
          c        %------------------------------------%
          c
                   ushift = .false.
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
                   if (msglvl .gt. 2) then
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
                   cnorm = .true.
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
          c 
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
                      rnorm = ddot (n, resid, 1, workd, 1)
                      rnorm = sqrt(abs(rnorm))
                   else if (bmat .eq. 'I') then
                      rnorm = dnrm2(n, resid, 1)
                   end if
                   cnorm = .false.
            130    continue
          c
                   if (msglvl .gt. 2) then
                      call dvout (logfil, 1, rnorm, ndigit, 
               &      '_saup2: B-norm of residual for NEV factorization')
                      call dvout (logfil, nev, h(1,2), ndigit,
               &           '_saup2: main diagonal of compressed H matrix')
                      call dvout (logfil, nev-1, h(2,1), ndigit,
               &           '_saup2: subdiagonal of compressed H matrix')
                   end if
          c 
                go to 1000
          c
          c     %---------------------------------------------------------------%
          c     |                                                               |
          c     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
          c     |                                                               |
          c     %---------------------------------------------------------------%
          c 
           1100 continue
          c
                mxiter = iter
                nev = nconv
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