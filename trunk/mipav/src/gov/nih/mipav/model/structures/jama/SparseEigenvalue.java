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
    
    private void dsdrv1() { 
    
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
          double tol;
          // sigma not intialized in dsdrv1
          double sigma = 0.0;
          double v1[];
          double v2[];
          int index;
    
    //     %------------%
    //     | Parameters |
    //     %------------%
    
          final double zero = 0.0;
      
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
          nev =  4; 
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
          tol = zero; 
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
    
          v1 = new double[nx];
          v2 = new double[nx];
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
                 for (i = 0; i < nx; i++) {
                	 v1[i] = workd[ipntr[0]-1+i];
                 }
                 av (nx, v1, v2);
                 for (i = 0; i < nx; i++) {
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
    
             dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                 bmat, n, which, nev, tol, resid, ncv, v, ldv, 
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
                    for (i = 0; i < nx; i++) {
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
             UI.setDataText("The convergence criterion = " + tol + "\n");
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
          private void dseupd (boolean rvec, String howmny, boolean select[], double d[], 
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
            		 work[iq-1+index] = array[i][j];
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
      
                  if (x[j-1] < x[j+igap-1]) {
                     temp = x[j-1];
                     x[j-1] = x[j+igap-1];
                     x[j+igap-1] = temp;
                     if (apply) {
                    	 for (k = 0; k < na; k++) {
                    		 temp = a[k][j-1];
                    		 a[k][j-1] = a[k][j+igap-1];
                    		 a[k][j+igap-1] = temp;
                    	 }
                     }
                  } //  if (x[j-1] < x[j+igap-1])
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
      
                  if (Math.abs(x[j-1]) < Math.abs(x[j+igap-1])) {
                     temp = x[j-1];
                     x[j-1] = x[j+igap-1];
                     x[j+igap-1] = temp;
                     if (apply) {
                    	 for (k = 0; k < na; k++) {
                    		 temp = a[k][j-1];
                    		 a[k][j-1] = a[k][j+igap-1];
                    		 a[k][j+igap-1] = temp;
                    	 }
                     }
                  } // if (Math.abs(x[j-1]) < Math.abs(x[j+igap-1]))
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
                 
                  if (x[j-1] > x[j+igap-1]) {
                	  temp = x[j-1];
                      x[j-1] = x[j+igap-1];
                      x[j+igap-1] = temp;
                      if (apply) {
                     	 for (k = 0; k < na; k++) {
                     		 temp = a[k][j-1];
                     		 a[k][j-1] = a[k][j+igap-1];
                     		 a[k][j+igap-1] = temp;
                     	 }
                      }  
                  } // if (x[j-1] > x[j+igap-1])
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
      
                  if (Math.abs(x[j-1]) > Math.abs(x[j+igap-1])) {
                	  temp = x[j-1];
                      x[j-1] = x[j+igap-1];
                      x[j+igap-1] = temp;
                      if (apply) {
                     	 for (k = 0; k < na; k++) {
                     		 temp = a[k][j-1];
                     		 a[k][j-1] = a[k][j+igap-1];
                     		 a[k][j+igap-1] = temp;
                     	 }
                      }     
                  } // if (Math.abs(x[j-1]) > Math.abs(x[j+igap-1]))
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
          final double zero = 0.0;
   
    //
    //     %---------------%
    //     | Local Scalars |
    //     %---------------%
    //
          int i;
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
             dsaupd_np[0]     = ncv - nev;
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
    
             dsaupd_np[0]     = ncv - nev;
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
    
         //dsaup2 
            //( ido, bmat, n, which, dsaupd_nev0, dsaupd_np, tol, resid, dsaupd_mode, dsaupd_iupd,
              //dsaupd_ishift, dsaupd_mxiter, v, ldv, workl(dsaupd_ih), dsaupd_ldh, workl(dsaupd_ritz),
              //workl(dsaupd_bounds), workl(dsaupd_iq), dsaupd_ldq, workl(dsaupd_iw), ipntr, workd,
              //info );
         double H[][] = new double[dsaupd_nev0+dsaupd_np[0]][2];
         double ritz[] = new double[dsaupd_nev0+dsaupd_np[0]];
         double bounds[] = new double[dsaupd_nev0+dsaupd_np[0]];
         double Q[][] = new double[dsaupd_nev0+dsaupd_np[0]][dsaupd_nev0+dsaupd_np[0]];
         double workl2[] = new double[3*(dsaupd_nev0+dsaupd_np[0])];
         for (i = 0; i < 3*(dsaupd_nev0+dsaupd_np[0]); i++) {
        	 workl2[i] = workl[dsaupd_iw-1+i];
         }
         dsaup2 
         ( ido, bmat, n, which, dsaupd_nev0, dsaupd_np, tol, resid, dsaupd_mode, dsaupd_iupd,
           dsaupd_ishift, dsaupd_mxiter, v, ldv, H, dsaupd_ldh, ritz,
           bounds, Q, dsaupd_ldq, workl2, ipntr, workd,
           info );
         for (i = 0; i < 3*(dsaupd_nev0+dsaupd_np[0]); i++) {
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
     
          iparam[2] = dsaupd_mxiter;
          iparam[4] = dsaupd_np[0];
          iparam[8] = nopx;
          iparam[9] = nbx;
          iparam[10] = nrorth;
    
    //     %------------------------------------%
    //     | Exit if there was an informational |
    //     | error within dsaup2.               |
    //     %------------------------------------%
    
          if (info[0] < 0) {
        	  return;
          }
          if (info[0] == 2) info[0] = 3;
    
          if (dsaupd_msglvl > 0) {
        	 UI.setDataText("dsaupd: number of update iterations taken dsaupd_mxiter = " + dsaupd_mxiter + "\n");
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
        	 UI.setDataText("Total number update iterations = " + dsaupd_mxiter + "\n");
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
                   mxiter = dsaup2_iter;
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
                	  UI.setDataText("dsaup2: The length of the current Lanczos factorization nev = " + nev + "\n");
                	  UI.setDataText("dsaup2: Extend the Lanczos factorization by " + np + "\n");
                   }
           
          //        %------------------------------------------------------------%
          //        | Compute NP additional steps of the Lanczos factorization. |
          //        %------------------------------------------------------------%
          
                   ido[0] = 0;
        			   } // if (seg6)
        			   seg6 = true;
                   dsaup2_update = true;
          
                   dsaitr (ido, bmat, n, nev, np[0], mode, resid, dsaup2_rnorm, v, 
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
                      mxiter = dsaup2_iter;
                      info[0] = -9999;
                      ido[0] = 99;
                      t1 = System.currentTimeMillis();
                      tsaup2 = t1 - t0;
                      return;
                   } // if (info[0] > 0)
                   dsaup2_update = false;
          
                   if (dsaup2_msglvl > 1) {
                	   UI.setDataText("dsaup2: Current B-norm of residual for factorization dsaup2_rnorm = " +
                                     nf.format(dsaup2_rnorm) + "\n");
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
                   nev = dsaup2_nev0;
                   np[0] = dsaup2_np0;
                   dsgets (ishift, which, nev, np[0], ritz, bounds, workl);
           
          //        %-------------------%
          //        | Convergence test. |
          //        %-------------------%
          //
                   for (i = 0; i < nev; i++) {
                	   workl[np[0]+i] = bounds[np[0]+i];
                   }
                   array1 = new double[nev];
                   array2 = new double[nev];
                   for (i = 0; i < nev; i++) {
                	   array1[i] = ritz[np[0]+i];
                	   array2[i] = workl[np[0]+i];
                   }
                   dsconv (nev, array1, array2, tol, dsaup2_nconv);
          
                   if (dsaup2_msglvl > 2) {
                      UI.setDataText("In dsaup2 nev = " + nev + " np[0] = " + np[0] + " dsaup2_nconv[0] = " + dsaup2_nconv[0] + "\n");
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
                         nev = nev + 1;
                      }
                   } // for (j=0; j < nptemp; j++)
           
                   if ( (dsaup2_nconv[0] >= dsaup2_nev0) || (dsaup2_iter > mxiter) ||(np[0] == 0) ) {
               
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
                         nevd2 = nev / 2;
                         nevm2 = nev - nevd2; 
                         if ( nev > 1 ) {
                        	for (i = 0; i < Math.min(nevd2, np[0]); i++) {
                        		temp = ritz[nevm2+i];
                        		ritz[nevm2+i] = ritz[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i];
                        		ritz[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i] = temp;
                        		temp = bounds[nevm2+i];
                        		bounds[nevm2+i] = bounds[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i];
                        		bounds[Math.max(dsaup2_kplusp-nevd2, dsaup2_kplusp-np[0])+i] = temp;
                        	}
                         } // if ( nev > 1 )
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
          
                      if (dsaup2_iter > mxiter && dsaup2_nconv[0] < nev) info[0] = 1;
          
          //           %---------------------%
          //           | No shifts to apply. | 
          //           %---------------------%
          
                      if (np[0] == 0 && dsaup2_nconv[0] < dsaup2_nev0) info[0] = 2;
          
                      np[0] = dsaup2_nconv[0];
                      mxiter = dsaup2_iter;
                      nev = dsaup2_nconv[0];
                      ido[0] = 99;
                      t1 = System.currentTimeMillis();
                      tsaup2 = t1 - t0;
                      return;
                       } // } // if ( (dsaup2_nconv[0] >= dsaup2_nev0) || (dsaup2_iter > mxiter) ||(np == 0) )
                   else if (dsaup2_nconv[0] < nev && ishift == 1) {
          
          //           %---------------------------------------------------%
          //           | Do not have all the requested eigenvalues yet.    |
          //           | To prevent possible stagnation, adjust the number |
          //           | of Ritz values and the shifts.                    |
          //           %---------------------------------------------------%
          
                      nevbef = nev;
                      nev = nev + Math.min (dsaup2_nconv[0], np[0]/2);
                      if (nev == 1 && dsaup2_kplusp >= 6) {
                         nev = dsaup2_kplusp / 2;
                      }
                      else if (nev == 1 && dsaup2_kplusp > 2) {
                         nev = 2;
                      }
                      np[0]  = dsaup2_kplusp - nev;
               
          //           %---------------------------------------%
          //           | If the size of NEV was just increased |
          //           | resort the eigenvalues.               |
          //           %---------------------------------------%
               
                      if (nevbef < nev)  {
                         dsgets (ishift, which, nev, np[0], ritz, bounds, workl);
                      }
          
                   } // else if (dsaup2_nconv .lt. nev .and. ishift .eq. 1)
          
                   if (dsaup2_msglvl > 0) {
                	  UI.setDataText("saup2: no. of \"converged\" Ritz values at this iter dsaup2_nconv = " + dsaup2_nconv + "\n");
                      if (dsaup2_msglvl > 1) {
                    	 UI.setDataText("In dsaup2 nev = " + nev + " np = " + np + "\n");
                         UI.setDataText("dsaup2: \"wanted\" Ritz values: \n");
                         for (i = 0; i < nev; i++) {
                        	 UI.setDataText("ritz["+(np[0]+i)+"] = " + nf.format(ritz[np[0]+i]) + "\n");
                         }
                         UI.setDataText("dsaup2: Ritz estimates of the \"wanted\" values: \n");
                         for (i = 0; i < nev; i++) {
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
          
                   dsapps (n, nev, np[0], ritz, v, ldv, h, ldh, resid, q, ldq, workd);
          
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
                      for (i = 0; i < nev; i++) {
                    	  UI.setDataText("h["+i+"][1] = " + nf.format(h[i][1]) + "\n");
                      }
                      UI.setDataText("dsaup2: subdiagonal of compressed H matrix: \n");
                      for (i = 0; i < nev-1; i++) {
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
              ge.dlacpy ('A', n, kev, array2D, ldv, v, ldv);
         
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
              } // if (h[kev][0] > zero)
        
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
      
                  if (x1[j-1] < x1[j+igap-1]) {
                     temp = x1[j-1];
                     x1[j-1] = x1[j+igap-1];
                     x1[j+igap-1] = temp;
                     if (apply) {
                        temp = x2[j-1];
                        x2[j-1] = x2[j+igap-1];
                        x2[j+igap-1] = temp;
                     } // if (apply)
                  } // if (x1[j-1] < x1[j+igap-1])
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
      
                  if (Math.abs(x1[j-1]) < Math.abs(x1[j+igap-1])) {
                     temp = x1[j-1];
                     x1[j-1] = x1[j+igap-1];
                     x1[j+igap-1] = temp;
                     if (apply) {
                        temp = x2[j-1];
                        x2[j-1] = x2[j+igap-1];
                        x2[j+igap-1] = temp;
                     } // if (apply)
                  } // if (Math.abs(x1[j-1]) < Math.abs(x1[j+igap-1]))
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
                
                  if (x1[j-1] > x1[j+igap-1]) {
                     temp = x1[j-1];
                     x1[j-1] = x1[j+igap-1];
                     x1[j+igap-1] = temp;
                     if (apply) {
                        temp = x2[j-1];
                        x2[j-1] = x2[j+igap-1];
                        x2[j+igap-1] = temp;
                     } // if (apply)
                  } // if (x1[j-1] > x1[j+igap-1])
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
      
                  if (Math.abs(x1[j-1]) > Math.abs(x1[j+igap-1])) {
                     temp = x1[j-1];
                     x1[j-1] = x1[j+igap-1];
                     x1[j+igap-1] = temp;
                     if (apply) {
                        temp = x2[j-1];
                        x2[j-1] = x2[j+igap-1];
                        x2[j+igap-1] = temp;
                     } // if (apply)
                  } // if (Math.abs(x1[j-1]) > Math.abs(x1[j+igap-1]))
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

                   for (ptr1 = 0; ptr1 < n; ptr1++) {
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
           
          //     %----------------------------------------------------%
          //     | User time including reverse communication overhead |
          //     %----------------------------------------------------%
                tmvopx = 0.0;
                tmvbx  = 0.0;
           
                return;
                } // dstats


}