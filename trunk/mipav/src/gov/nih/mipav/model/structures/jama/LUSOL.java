package gov.nih.mipav.model.structures.jama;


import java.text.DecimalFormat;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

/** LUSOL maintains LU factors of a square or rectangular sparse matrix.
LUSOL is maintained by
*     Michael Saunders, Systems Optimization Laboratory,
*     Dept of Management Science & Engineering, Stanford University.
*     (saunders@stanford.edu)

Professor Michael Saunders has kindly granted the NIH MIPAV project permission to port LUSOL from FORTRAN to Java
under a BSD license.
Porting performed by William Gandler.

The website for LUSOL is: [http://www.stanford.edu/group/SOL/software/lusol.html][LUSOL]
LUSOL: Sparse LU for Ax = b

    AUTHOR: Michael Saunders
    CONTRIBUTORS: Philip Gill, Walter Murray, Margaret Wright, Michael O'Sullivan, Kjell Eikland, Yin Zhang, Nick Henderson, Ding Ma
    CONTENTS: A sparse LU factorization for square and rectangular matrices A, with Bartels-Golub-Reid updates for column replacement
     and other rank-1 modifications. Typically used for a sequence of linear equations as in the simplex method:

            Solve Ax = b and/or A'y = c
            Replace a column of A
            Repeat with different b, c
        

    The matrix A may have any shape and rank. Rectangular LU factors may be used to form a sparse null-space matrix operator.

    Special feature 1: Three sparse pivoting options in the Factor routine:

            Threshold partial pivoting (TPP)
            Threshold rook pivoting    (TRP)
            Threshold complete pivoting (TCP)
        

    All options choose row and column permutations as they go, balancing sparsity and stability according to different rules.
    TPP is normally most efficient for solving Ax = b. TRP and TCP are rank-revealing factorizations. In practice, TRP is
    an effective method for estimating rank(A). TCP tends to be too dense and expensive to be useful, although MINOS and SNOPT
    switch from TPP to TRP and even TCP if necessary in case of persistent numerical difficulty.

    Special feature 2: Multiple update routines:

            Add, delete, or replace a column of A
            Add, delete, or replace a row    of A
            Add a general (sparse) rank-1 matrix to A
        

    Numerical stability: LUSOL maintains LU factors with row and column permutations P, Q such that A = LU with PLP' lower
    triangular (with unit diagonals) and PUQ upper triangular. The condition of L is controlled throughout by maintaining
    |Lij| <= factol (= 10 or 5 or 2 or 1.1, ...), so that U tends to reflect the condition of A. This is essential for
    subsequent Bartels-Golub-type updates (which are implemented in a manner similar to John Reid's LA05 and LA15 packages
    in the HSL library).

    If a fresh factorization is thought of as A = LDU (with unit diagonals on PLP' and PUQ), then TRP and TCP control the
    condition of both L and U by maintaining |Lij| <= factol and |Uij| <= factol, so that D reflects the condition of A.
    This is why TRP and TCP have rank-revealing properties.

    Proven applications: LUSOL is the basis factorization package (BFP) for MINOS, SQOPT, SNOPT, lp_solve, AMPL/PATH, GAMS/PATH.

    Shortcomings:
    Factor: No special handling of dense columns.
    Solve: No special treatment of sparse right-hand sides.
    Documentation: No user's manual. Primary documentation is in-line comments within the f77 source code (and the more recent f90 version).
    REFERENCES:

    J. K. Reid (1982). A sparsity-exploiting variant of the Bartels-Golub decomposition for linear programming bases,
    Mathematical Programming 24, 55-69.

    P. E. Gill, W. Murray, M. A. Saunders and M. H. Wright (1987). Maintaining LU factors of a general sparse matrix,
    Linear Algebra and its Applications 88/89, 239-270.

    P. E. Gill, W. Murray and M. A. Saunders (2005). SNOPT: An SQP algorithm for large-scale constrained optimization,
    SIGEST article, SIAM Review 47(1), 99-131. (See sections 4 and 5.) 
    
## Basic usage

In Matlab:

```
% get L and U factors
[L U P Q] = lusol(A);
```

See `>>> help lusol`.

## Advanced usage

In Matlab:

```
% create lusol object
mylu = lusol_obj(A);

% solve with lusol object (ie x = A\b)
x = mylu.solveA(b);

% update factorization to replace a column
mylu.repcol(v,1);

% solve again with updated factorization
x1 = mylu.solveA(b);
```

See `>>> help lusol_obj`.


All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
 conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer
 in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

public class LUSOL implements java.io.Serializable {
    private ViewUserInterface UI = ViewUserInterface.getReference();
	
	DecimalFormat nf = new DecimalFormat("0.00000E0");
	private int ip = 8;
	private int rp = 8;
	private long ip_huge = Long.MAX_VALUE;
	private double huge = Double.MAX_VALUE;
	private double eps = 2.22044604925031308E-16;
	
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// File:       lusol.f90
	
	// Contains    lu1fac   lu1fad   lu1gau   lu1mar   lu1mRP   lu1mCP   lu1mSP
	// Contains    lu1pen   lu1mxc   lu1mxr   lu1or1   lu1or2   lu1or3   lu1or4
	// Contains    lu1pq1   lu1pq2   lu1pq3   lu1rec   lu1slk
	//             lu1ful   lu1DPP   lu1DCP
	// Contains    Hbuild   Hchange  Hdelete  Hdown    Hinsert  Hup
	// Contains    lu6sol   lu6L     lu6Lt    lu6U     Lu6Ut    lu6LD    lu6chk
	// Contains    lu7add   lu7cyc   lu7elm   lu7for   lu7rnk   lu7zap
	// Contains    lu8rpc
	
	// Contains    jdamax
	
	// This file is an f90 version of most parts of the f77 sparse LU package LUSOL
	// (the parts needed by MINOS, SQOPT and SNOPT).  The parts included are
	
	//    lusol1.f    Factor a given matrix A from scratch (lu1fac).
	//    lusol2.f    Heap-management routines for lu1fac.
	//    lusol6a.f   Solve with the current LU factors.
	//    lusol7a.f   Utilities for all update routines.
	//    lusol8a.f   Replace a column (Bartels-Golub update).
	
	// 10 Jan 2010: First f90 version.
	// 12 Dec 2011: Had to change ip, iq to p, q to avoid clash with ip, rp.
	// 17 Dec 2011: BLAS idamax replaced by private jdamax (taken from sn17util.f90).
	//              Note: jdamax( lencol, a(k,k), 1 ) has to become
	//                    jdamax( lencol,a(k:m,k),1 )
	// 03 Feb 2012: It's ok to have a(k,k) above, but a(k:m) is more illuminating.
	// 03 Feb 2012: Bug fixed in lu1DPP and lu1DCP (translation of call daxpy).
	// 09 Mar 2013: Begin project for improving efficiency of TRP.
	//              Mostly this needs a new version of lu1mxr.
	//              Ding Ma and Michael Saunders, Stanford University.
	// 03 Apr 2013: New lu1mxr finds max element Amaxr(i) in each modified row i
	//              much more efficiently.  Three new local arrays needed:
	//              markc(n) and markr(m) in lu1fad, and cols(n) in lu1mxr.
	//              This is easy in f90.
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	//module lusol
	//  use  lusol_precision, only : ip, rp
	// use  snModuleIO,        only : snPRNT
	// use  snModuleWork,      only : snAj  , snWork
	// use  sn15blas,          only : idamax

	 // implicit none
	 // private
	 // public    :: lu1fac, lu6sol, lu8rpc
	 // private   :: jdamax
	 // intrinsic :: abs, int, max, min, real

	// contains

	  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	  // History from lusol1.f
	  //
	  // 26 Apr 2002: TCP implemented using heap data structure.
	  // 01 May 2002: lu1DCP implemented.
	  // 07 May 2002: lu1mxc must put 0.0 at top of empty columns.
	  // 09 May 2002: lu1mCP implements Markowitz with cols searched
	  //              in heap order.
	  //              Often faster (searching 20 or 40 cols) but more dense.
	  // 11 Jun 2002: TRP implemented.
	  //              lu1mRP implements Markowitz with Threshold Rook Pivoting.
	  //              lu1mxc maintains max col elements.  (Previously lu1max.)
	  //              lu1mxr maintains max row elements.
	  // 12 Jun 2002: lu1mCP seems too slow on big problems (e.g. memplus).
	  //              Disabled it for the moment.  (Use lu1mar + TCP.)
	  // 14 Dec 2002: TSP implemented.
	  //              lu1mSP implements Markowitz with
	  //              Threshold Symmetric Pivoting.
	  // 07 Mar 2003: character*1, character*2 changed to f90 form.
	  //              Comments changed from * in column to ! in column 1.
	  //              Comments kept within column 72 to avoid compiler warning.
	  // 19 Dec 2004: Hdelete(...) has new input argument Hlenin.
	  // 21 Dec 2004: Print Ltol and Lmax with e10.2 instead of e10.1.
	  // 26 Mar 2006: lu1fad: Ignore nsing from lu1ful.
	  //              lu1DPP: nsing redefined (but not used by lu1fad).
	  //              lu1DCP: nsing redefined (but not used by lu1fad).
	  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	  public void lu1fac(int m, int n, int nelem, int lena , int luparm[], double parmlu[],
	                     double a[], int indc[], int indr[], int p[], int q[],
	                     int lenc[], int lenr[], int locc[], int locr[],
	                     int iploc[], int iqloc[], int ipinv[], int iqinv[], double w[], int inform[]) {

	    // integer(ip),   intent(in)    :: m, n, nelem, lena
	    // integer(ip),   intent(inout) :: luparm(30)
	    // integer(ip),   intent(out)   :: inform
	    // real(rp),      intent(inout) :: parmlu(30), a(lena), w(n)
	    // integer(ip),   intent(inout) :: indc(lena), indr(lena), &
	    //                               p(m)      , q(n)      , &
	    //                                lenc(n)   , lenr(m)   , &
	    //                                iploc(n)  , iqloc(m)  , &
	    //                                ipinv(m)  , iqinv(n)  , &
	    //                                locc(n)   , locr(m)

	    //------------------------------------------------------------------
	    // lu1fac computes a factorization A = L*U, where A is a sparse
	    // matrix with m rows and n columns, P*L*P' is lower triangular
	    // and P*U*Q is upper triangular for certain permutations P, Q
	    // (which are returned in the arrays p, q).
	    // Stability is ensured by limiting the size of the elements of L.
	    
	    // The nonzeros of A are input via the parallel arrays a, indc, indr,
	    // which should contain nelem entries of the form    aij,    i,    j
	    // in any order.  There should be no duplicate pairs         i,    j.
	    
	    // ******************************************************************
	    // *        Beware !!!   The row indices i must be in indc,         *
	    // *              and the column indices j must be in indr.         *
	    // *              (Not the other way round!)                        *
	    // ******************************************************************
	    
	    // It does not matter if some of the entries in a(*) are zero.
	    // Entries satisfying  abs( a(i) ) .le. parmlu(3)  are ignored.
	    // Other parameters in luparm and parmlu are described below.
	    
	    // The matrix A may be singular.  On exit, nsing = luparm(11) gives
	    // the number of apparent singularities.  This is the number of
	    // "small" diagonals of the permuted factor U, as judged by
	    // the input tolerances Utol1 = parmlu(4) and  Utol2 = parmlu(5).
	    // The diagonal element diagj associated with column j of A is
	    // "small" if
	    //                 abs( diagj ) .le. Utol1
	    // or
	    //                 abs( diagj ) .le. Utol2 * max( uj ),
	    
	    // where max( uj ) is the maximum element in the j-th column of U.
	    // The position of such elements is returned in w(*).  In general,
	    // w(j) = + max( uj ),  but if column j is a singularity,
	    // w(j) = - max( uj ).  Thus, w(j) .le. 0 if column j appears to be
	    // dependent on the other columns of A.
	    //
	    // NOTE: lu1fac (like certain other sparse LU packages) does not
	    // treat dense columns efficiently.  This means it will be slow
	    // on "arrow matrices" of the form
	    //                  A = (x       a)
	    //                      (  x     b)
	    //                      (    x   c)
	    //                      (      x d)
	    //                      (x x x x e)
	    // if the numerical values in the dense column allow it to be
	    // chosen LATE in the pivot order.
	    
	    // With TPP (Threshold Partial Pivoting), the dense column is
	    // likely to be chosen late.
	    
	    // With TCP (Threshold Complete Pivoting), if any of a,b,c,d
	    // is significantly larger than other elements of A, it will
	    // be chosen as the first pivot and the dense column will be
	    // eliminated, giving reasonably sparse factors.
	    // However, if element e is so big that TCP chooses it, the factors
	    // will become dense.  (It's hard to win on these examples!)
	    // ==================================================================
	    
	    
	    // Notes on the array names
	    // ------------------------
	    
	    // During the LU factorization, the sparsity pattern of the matrix
	    // being factored is stored twice: in a column list and a row list.
	    
	    // The column list is ( a, indc, locc, lenc )
	    // where
	    //       a(*)    holds the nonzeros,
	    //       indc(*) holds the indices for the column list,
	    //       locc(j) points to the start of column j in a(*) and indc(*),
	    //       lenc(j) is the number of nonzeros in column j.
	    
	    // The row list is    (    indr, locr, lenr )
	    // where
	    //       indr(*) holds the indices for the row list,
	    //       locr(i) points to the start of row i in indr(*),
	    //       lenr(i) is the number of nonzeros in row i.
	    
	    
	    // At all stages of the LU factorization, p contains a complete
	    // row permutation.  At the start of stage k,  p(1), ..., p(k-1)
	    // are the first k-1 rows of the final row permutation P.
	    // The remaining rows are stored in an ordered list
	    //                    ( p, iploc, ipinv )
	    // where
	    //       iploc(nz) points to the start in p(*) of the set of rows
	    //                 that currently contain nz nonzeros,
	    //       ipinv(i)  points to the position of row i in p(*).
	    //
	    // For example,
	    //       iploc(1) = k   (and this is where rows of length 1 begin),
	    //       iploc(2) = k+p  if there are p rows of length 1
	    //                      (and this is where rows of length 2 begin).
	    
	    // Similarly for q, iqloc, iqinv.
	    //==================================================================
	    
	    
	    // 00 Jun 1983  Original version.
	    // 00 Jul 1987  nrank  saved in luparm(16).
	    // 12 Apr 1989  ipinv, iqinv added as workspace.
	    // 26 Apr 1989  maxtie replaced by maxcol in Markowitz search.
	    // 16 Mar 1992  jumin  saved in luparm(19).
	    // 10 Jun 1992  lu1fad has to move empty rows and cols to the bottom
	    //              (via lu1pq3) before doing the dense LU.
	    // 12 Jun 1992  Deleted dense LU (lu1ful, lu1vlu).
	    // 25 Oct 1993  keepLU implemented.
	    // 07 Feb 1994  Added new dense LU (lu1ful, lu1den).
	    // 21 Dec 1994  Bugs fixed in lu1fad (nrank) and lu1ful (ipvt).
	    // 08 Aug 1995  Use p instead of w as parameter to lu1or3 (for F90).
	    // 13 Sep 2000  TPP and TCP options implemented.
	    // 17 Oct 2000  Fixed troubles due to A = empty matrix (Todd Munson).
	    // 01 Dec 2000  Save Lmax, Umax, etc. after both lu1fad and lu6chk.
	    //              lu1fad sets them when keepLU = false.
	    //              lu6chk sets them otherwise, and includes items
	    //              from the dense LU.
	    // 11 Mar 2001  lu6chk now looks at diag(U) when keepLU = false.
	    // 26 Apr 2002  New TCP implementation using heap routines to
	    //              store largest element in each column.
	    //              New workspace arrays Ha, Hj, Hk required.
	    //              For compatibility, borrow space from a, indc, indr
	    //              rather than adding new input parameters.
	    // 01 May 2002  lu1den changed to lu1DPP (dense partial  pivoting).
	    //              lu1DCP implemented       (dense complete pivoting).
	    //              Both TPP and TCP now switch to dense mode and end.
	   
	    // 10 Jan 2010: First f90 version.
	    // ---------------------------------------------------------------------
	    
	    
	    //  INPUT PARAMETERS
	    
	    //  m      (not altered) is the number of rows in A.
	    //  n      (not altered) is the number of columns in A.
	    //  nelem  (not altered) is the number of matrix entries given in
	    //         the arrays a, indc, indr.
	    //  lena   (not altered) is the dimension of  a, indc, indr.
	    //         This should be significantly larger than nelem.
	    //         Typically one should have
	    //            lena > max( 2*nelem, 10*m, 10*n, 10000 )
	    //         but some applications may need more.
	    //         On machines with virtual memory it is safe to have
	    //         lena "far bigger than necessary", since not all of the
	    //         arrays will be used.
	    //  a      (overwritten) contains entries   Aij  in   a(1:nelem).
	    //  indc   (overwritten) contains the indices i in indc(1:nelem).
	    //  indr   (overwritten) contains the indices j in indr(1:nelem).
	    
	    //  luparm input parameters:                                Typical value
	    
	    //  luparm( 1) = nout     File number for printed messages.         6
	    
	    //  luparm( 2) = lprint   Print level.                              0
	    //                   <  0 suppresses output.
	    //                   =  0 gives error messages.
	    //                  >= 10 gives statistics about the LU factors.
	    //                  >= 50 gives debug output from lu1fac
	    //                        (the pivot row and column and the
	    //                        no. of rows and columns involved at
	    //                        each elimination step).
	    
	    //  luparm( 3) = maxcol   lu1fac: maximum number of columns         5
	    //                        searched allowed in a Markowitz-type
	    //                        search for the next pivot element.
	    //                        For some of the factorization, the
	    //                        number of rows searched is
	    //                        maxrow = maxcol - 1.
	    
	    //  luparm( 6) = 0    =>  TPP: Threshold Partial   Pivoting.        0
	    //             = 1    =>  TRP: Threshold Rook      Pivoting.
	    //             = 2    =>  TCP: Threshold Complete  Pivoting.
	    //             = 3    =>  TSP: Threshold Symmetric Pivoting.
	    //             = 4    =>  TDP: Threshold Diagonal  Pivoting.
	    //                             (TDP not yet implemented).
	    //                        TRP and TCP are more expensive than TPP but
	    //                        more stable and better at revealing rank.
	    //                        Take care with setting parmlu(1), especially
	    //                        with TCP.
	    //                        NOTE: TSP and TDP are for symmetric matrices
	    //                        that are either definite or quasi-definite.
	    //                        TSP is effectively TRP for symmetric matrices.
	    //                        TDP is effectively TCP for symmetric matrices.
	    
	    //  luparm( 8) = keepLU   lu1fac: keepLU = 1 means the numerical    1
	    //                        factors will be computed if possible.
	    //                        keepLU = 0 means L and U will be discarded
	    //                        but other information such as the row and
	    //                        column permutations will be returned.
	    //                        The latter option requires less storage.
	    
	    //  parmlu input parameters:                                Typical value
	    
	    //  parmlu( 1) = Ltol1    Max Lij allowed during Factor.
	    //                                                  TPP     10.0 or 100.0
	    //                                                  TRP      4.0 or  10.0
	    //                                                  TCP      5.0 or  10.0
	    //                                                  TSP      4.0 or  10.0
	    //                        With TRP and TCP (Rook and Complete Pivoting),
	    //                        values less than 25.0 may be expensive
	    //                        on badly scaled data.  However,
	    //                        values less than 10.0 may be needed
	    //                        to obtain a reliable rank-revealing
	    //                        factorization.
	    //  parmlu( 2) = Ltol2    Max Lij allowed during Updates.            10.0
	    //                        during updates.
	    //  parmlu( 3) = small    Absolute tolerance for       eps**0.8 = 3.0d-13
	    //                        treating reals as zero.
	    //  parmlu( 4) = Utol1    Absolute tol for flagging    eps**0.67= 3.7d-11
	    //                        small diagonals of U.
	    //  parmlu( 5) = Utol2    Relative tol for flagging    eps**0.67= 3.7d-11
	    //                        small diagonals of U.
	    //                        (eps = machine precision)
	    //  parmlu( 6) = Uspace   Factor limiting waste space in  U.      3.0
	    //                        In lu1fac, the row or column lists
	    //                        are compressed if their length
	    //                        exceeds Uspace times the length of
	    //                        either file after the last compression.
	    //  parmlu( 7) = dens1    The density at which the Markowitz      0.3
	    //                        pivot strategy should search maxcol
	    //                        columns and no rows.
	    //                        (Use 0.3 unless you are experimenting
	    //                        with the pivot strategy.)
	    //  parmlu( 8) = dens2    the density at which the Markowitz      0.5
	    //                        strategy should search only 1 column,
	    //                        or (if storage is available)
	    //                        the density at which all remaining
	    //                        rows and columns will be processed
	    //                        by a dense LU code.
	    //                        For example, if dens2 = 0.1 and lena is
	    //                        large enough, a dense LU will be used
	    //                        once more than 10 per cent of the
	    //                        remaining matrix is nonzero.
	    
	    
	    //   OUTPUT PARAMETERS
	    
	    //  a, indc, indr     contain the nonzero entries in the LU factors of A.
	    //         If keepLU = 1, they are in a form suitable for use
	    //         by other parts of the LUSOL package, such as lu6sol.
	    //         U is stored by rows at the start of a, indr.
	    //         L is stored by cols at the end   of a, indc.
	    //         If keepLU = 0, only the diagonals of U are stored, at the
	    //         end of a.
	    //  p, q   are the row and column permutations defining the
	    //         pivot order.  For example, row p(1) and column q(1)
	    //         defines the first diagonal of U.
	    //  lenc(1:numl0) contains the number of entries in nontrivial
	    //         columns of L (in pivot order).
	    //  lenr(1:m) contains the number of entries in each row of U
	    //         (in original order).
	    //  locc(1:n) = 0 (ready for the LU update routines).
	    //  locr(1:m) points to the beginning of the rows of U in a, indr.
	    //  iploc, iqloc, ipinv, iqinv  are undefined.
	    //  w      indicates singularity as described above.
	    //  inform = 0 if the LU factors were obtained successfully.
	    //         = 1 if U appears to be singular, as judged by lu6chk.
	    //         = 3 if some index pair indc(l), indr(l) lies outside
	    //             the matrix dimensions 1:m , 1:n.
	    //         = 4 if some index pair indc(l), indr(l) duplicates
	    //             another such pair.
	    //         = 7 if the arrays a, indc, indr were not large enough.
	    //             Their length "lena" should be increase to at least
	    //             the value "minlen" given in luparm(13).
	    //         = 8 if there was some other fatal error.  (Shouldn't happen!)
	    //         = 9 if no diagonal pivot could be found with TSP or TDP.
	    //             The matrix must not be sufficiently definite
	    //             or quasi-definite.
	    
	    //  luparm output parameters:
	    
	    //  luparm(10) = inform   Return code from last call to any LU routine.
	    //  luparm(11) = nsing    No. of singularities marked in the
	    //                        output array w(*).
	    //  luparm(12) = jsing    Column index of last singularity.
	    //  luparm(13) = minlen   Minimum recommended value for  lena.
	    //  luparm(14) = maxlen   ?
	    //  luparm(15) = nupdat   No. of updates performed by the lu8 routines.
	    //  luparm(16) = nrank    No. of nonempty rows of U.
	    //  luparm(17) = ndens1   No. of columns remaining when the density of
	    //                        the matrix being factorized reached dens1.
	    //  luparm(18) = ndens2   No. of columns remaining when the density of
	    //                        the matrix being factorized reached dens2.
	    //  luparm(19) = jumin    The column index associated with DUmin.
	    //  luparm(20) = numL0    No. of columns in initial  L.
	    //  luparm(21) = lenL0    Size of initial  L  (no. of nonzeros).
	    //  luparm(22) = lenU0    Size of initial  U.
	    //  luparm(23) = lenL     Size of current  L.
	    //  luparm(24) = lenU     Size of current  U.
	    //  luparm(25) = lrow     Length of row file.
	    //  luparm(26) = ncp      No. of compressions of LU data structures.
	    //  luparm(27) = mersum   lu1fac: sum of Markowitz merit counts.
	    //  luparm(28) = nUtri    lu1fac: triangular rows in U.
	    //  luparm(29) = nLtri    lu1fac: triangular rows in L.
	    //  luparm(30) =
	    
	    
	    
	    //  parmlu output parameters:
	    
	    //  parmlu(10) = Amax     Maximum element in  A.
	    //  parmlu(11) = Lmax     Maximum multiplier in current  L.
	    //  parmlu(12) = Umax     Maximum element in current  U.
	    //  parmlu(13) = DUmax    Maximum diagonal in  U.
	    //  parmlu(14) = DUmin    Minimum diagonal in  U.
	    //  parmlu(15) = Akmax    Maximum element generated at any stage
	    //                        during TCP factorization.
	    //  parmlu(16) = growth   TPP: Umax/Amax    TRP, TCP, TSP: Akmax/Amax
	    //  parmlu(17) =
	    //  parmlu(18) =
	    //  parmlu(19) =
	    //  parmlu(20) = resid    lu6sol: residual after solve with U or U'.
	    //  ...
	    //  parmlu(30) =
	    //---------------------------------------------------------------------

	    char mnkey;
	    String kPiv[] = new String[4];
	    int lena2 = 0;
	    int lenH = 0;
	    int lmaxr = 0;
	    int locH = 0;
	    int i, idummy, j, jsing, jumin,
	        k, l, l2, 
	        lenLk, lenUk,
	        ll, llsave, lm,
	        lprint, lPiv, lrow, ltopl,
	        lu, nbump,
	        ncp,
	        nmove, nout,
	        nsing, numl0;
	    int lenL[] = new int[1];
	    int lenU[] = new int[1];
	    int lerr[] = new int[1];
	    int mersum[] = new int[1];
	    int minlen[] = new int[1];
	    int ndens1[] = new int[1];
	    int ndens2[] = new int[1];
	    int nLtri[] = new int[1];
	    int nrank[] = new int[1];
	    int numnz[] = new int[1];
	    int nUtri[] = new int[1];
	    boolean  keepLU, TCP, TPP, TRP, TSP;
	    double Agrwth, avgmer,
	           condU, delem, densty, dincr,
	           dm, dn, growth,
	           Ltol, small, Ugrwth;
	    double Akmax[] = new double[1];
	    double Amax[] = new double[1];
	    double Lmax[] = new double[1];
	    double Umax[] = new double[1];
	    double DUmax[] = new double[1];
	    double DUmin[] = new double[1];
	    final int i1 = 1;
	    final double zero = 0.0;
	    final double one = 1.0;

	    // Grab relevant input parameters.

	    nout   = luparm[0];
	    lprint = luparm[1];
	    lPiv   = luparm[5];
	    keepLU = luparm[7] != 0;

	    Ltol   = parmlu[0];  // Limit on size of Lij
	    small  = parmlu[2];  // Drop tolerance

	    TPP    = lPiv == 0;  // Threshold Partial   Pivoting (normal).
	    TRP    = lPiv == 1;  // Threshold Rook      Pivoting
	    TCP    = lPiv == 2;  // Threshold Complete  Pivoting.
	    TSP    = lPiv == 3;  // Threshold Symmetric Pivoting.
	    kPiv[0] = "PP";
	    kPiv[1] = "RP";
	    kPiv[2] = "CP";
	    kPiv[3] = "SP";

	    // Initialize output parameters.

	    inform[0] = 0;
	    minlen[0] = nelem + 2*(m + n);
	    numl0  = 0;
	    lenL[0]   = 0;
	    lenU[0]   = 0;
	    lrow   = 0;
	    mersum[0] = 0;
	    nUtri[0]  = m;
	    nLtri[0]  = 0;
	    ndens1[0] = 0;
	    ndens2[0] = 0;
	    nrank[0]  = 0;
	    nsing  = 0;
	    jsing  = 0;
	    jumin  = 0;

	    Amax[0]   = zero;
	    Lmax[0]   = zero;
	    Umax[0]   = zero;
	    DUmax[0]  = zero;
	    DUmin[0]  = zero;
	    Akmax[0]  = zero;

	    if (m > n) {
	        mnkey  = '>';
	    }
	    else if (m == n) {
	        mnkey  = '=';
	    }
	    else {
	        mnkey  = '<';
	    }

	    // Float version of dimensions.

	    dm     = m;
	    dn     = n;
	    delem  = nelem;

	    // Initialize workspace parameters.
 
	    luparm[25] = 0;             // ncp
	    while (true) {
		    if (lena < minlen[0]) {
		    	inform[0] = 7;
		    	if (lprint >= 0) {
		    		UI.setDataText("lena = " + lena + " minlen[0] = " + minlen[0] + "\n");
		    	}
		    	break;
		    } // if (lena < minlen[0])
	
		    // -------------------------------------------------------------------
		    // Organize the  aij's  in  a, indc, indr.
		    // lu1or1  deletes small entries, tests for illegal  i,j's,
		    //         and counts the nonzeros in each row and column.
		    // lu1or2  reorders the elements of  A  by columns.
		    // lu1or3  uses the column list to test for duplicate entries
		    //         (same indices  i,j).
		    // lu1or4  constructs a row list from the column list.
		    //-------------------------------------------------------------------
		    lu1or1( m   , n    , nelem, lena , small,
		                 a   , indc , indr , lenc , lenr, 
		                 Amax, numnz, lerr , inform );
	
		    if (nout > 0  &&  lprint >= 10) {
		       densty = 100.0 * delem / (dm * dn);
		       UI.setDataText("m = " + m + " mnkey = " + mnkey + " n = " + n + "\n");
		       UI.setDataText("nelem = " + nelem + " Amax[0] = " + nf.format(Amax[0]) + 
		    		   "densty = " + nf.format(densty) + "\n");
		    }
		    if (inform[0] != 0) {
		    	inform[0] = 3;
		    	if (lprint >= 0) {
		    	    UI.setDataText("lerr[0] = " + lerr[0] + " indc[lerr[0] - 1] = " + indc[lerr[0]-1] +
		    	    		" indr[lerr[0] - 1] = " + indr[lerr[0]-1] + "\n");	
		    	}
		    	break;
		    } // if (inform[0] != 0)
	
		// nelem  = numnz     !!! Don't change nelem.
		// nelem is now numnz below (it might be less than the input value).
	
		    lu1or2( n, numnz[0], lena, a, indc, indr, lenc, locc );
		    lu1or3( m, n, lena, indc, lenc, locc, p, lerr, inform );
	
		    if (inform[0] != 0) {
		    	inform[0] = 4;
		    	if (lprint >= 0) {
		    	    UI.setDataText("lerr[0] = " + lerr[0] + " indc[lerr[0] - 1] = " + indc[lerr[0]-1] +
		    	    		" indr[lerr[0] - 1] = " + indr[lerr[0]-1] + "\n");	
		    	}
		    	break;
		    } // if (inform[0] != 0)
	
		    lu1or4( m, n, numnz[0], lena, indc, indr, lenc, lenr, locc, locr);
	
		    // ------------------------------------------------------------------
		    // Set up lists of rows and columns with equal numbers of nonzeros,
		    // using  indc(*)  as workspace.
		    // ------------------------------------------------------------------
		    int num[] = new int[n];
		    lu1pq1( m, n, lenr, p, iploc, ipinv, num);
		    num = new int[m];
		    lu1pq1( n, m, lenc, q, iqloc, iqinv, num);
	
		    // ------------------------------------------------------------------
		    // For TCP, allocate Ha, Hj, Hk at the end of a, indc, indr.
		    // Then compute the factorization  A = L*U.
		    // ------------------------------------------------------------------
		    if (TPP || TSP) {
		       lenH   = 1;
		       lena2  = lena;
		       locH   = lena;
		       lmaxr  = 1;
		    }
		    else if (TRP) {
		       lenH   = 1;             // Dummy
		       lena2  = lena  - m;     // Reduced length of      a
		       locH   = lena;          // Dummy
		       lmaxr  = lena2 + 1;     // Start of Amaxr      in a
		    }
		    else if (TCP) {
		       lenH   = n;             // Length of heap
		       lena2  = lena  - lenH;  // Reduced length of      a, indc, indr
		       locH   = lena2 + 1;     // Start of Ha, Hj, Hk in a, indc, indr
		       lmaxr  = 1;             // Dummy
		    }
	
		    double Ha[] = new double[lenH];
		    int Hj[] = new int[lenH];
		    int Hk[] = new int[lenH];
		    double Amaxr[] = new double[m];
		    for (i = 0; i < lenH; i++) {
		    	Ha[i] = a[locH - 1 + i];
		    	Hj[i] = indc[locH-1+i];
		    	Hk[i] = indr[locH-1+i];
		    }
		    for (i = 0; i < m; i++) {
		    	Amaxr[i] = a[lmaxr-1+i];
		    }
		    lu1fad( m, n, numnz[0] , lena2, luparm, parmlu,
		                 a     , indc  , indr  , p     , q,
		                 lenc  , lenr  , locc  , locr,
		                 iploc , iqloc , ipinv , iqinv , w,
		                 lenH, Ha, Hj, Hk, Amaxr,
		                 inform, lenL  , lenU  , minlen, mersum,
		                 nUtri , nLtri , ndens1, ndens2, nrank ,
		                 Lmax  , Umax  , DUmax , DUmin , Akmax );
		    for (i = 0; i < lenH; i++) {
		    	a[locH - 1 + i] = Ha[i];
		    	indc[locH-1+i] = Hj[i];
		    	indr[locH-1+i] = Hk[i];
		    }
		    for (i = 0; i < m; i++) {
		    	a[lmaxr-1+i] = Amaxr[i];
		    }
	
		    luparm[15] = nrank[0];
		    luparm[22] = lenL[0];
		    if (inform[0] == 7) {
		    	if (lprint >= 0) {
		    		UI.setDataText("lena = " + lena + " minlen[0] = " + minlen[0] + "\n");
		    	}
		    	break;
		    }
		    if (inform[0] == 9) {
		        if (lprint >= 0) {
		        	UI.setDataText("lu1fac error... TSP used but diagonal pivot cold not be found\n");
		        }
		        break;
		    }
		    if (inform[0] >  0) {
		    	inform[0] = 8;
		    	if (lprint >= 0) {
		    		UI.setDataText("lu1fac error... fatal bug sorry --- this should never happen\n");
		    	}
		    	break;
		    }
	
		    if ( keepLU ) {
		       // ---------------------------------------------------------------
		       // The LU factors are at the top of  a, indc, indr,
		       // with the columns of  L  and the rows of  U  in the order
		       //
		       // ( free )   ... ( u3 ) ( l3 ) ( u2 ) ( l2 ) ( u1 ) ( l1 ).
		       //
		       // Starting with ( l1 ) and ( u1 ), move the rows of  U  to the
		       // left and the columns of  L  to the right, giving
		       //
		       // ( u1 ) ( u2 ) ( u3 ) ...   ( free )   ... ( l3 ) ( l2 ) ( l1 ).
		       //
		       // Also, set  numl0 = the number of nonempty columns of L.
		       // ---------------------------------------------------------------
		       lu     = 0;
		       ll     = lena  + 1;
		       lm     = lena2 + 1;
		       ltopl  = ll - lenL[0] - lenU[0];
		       lrow   = lenU[0];
	
		       for (k = 1; k <= nrank[0]; k++) {
		          i       =   p[k-1];
		          lenUk   = - lenr[i-1];
		          lenr[i-1] =   lenUk;
		          j       =   q[k-1];
		          lenLk   = - lenc[j-1] - 1;
		          if (lenLk > 0) {
		             numl0        = numl0 + 1;
		             iqloc[numl0-1] = lenLk;
		          }
	
		          if (lu + lenUk < ltopl) {
		             // =========================================================
		             // There is room to move ( uk ).  Just right-shift ( lk ).
		             // =========================================================
		             for (idummy = 1; idummy <= lenLk; idummy++) {
		                ll       = ll - 1;
		                lm       = lm - 1;
		                a[ll-1]    = a[lm-1];
		                indc[ll-1] = indc[lm-1];
		                indr[ll-1] = indr[lm-1];
		             } // for (idummy = 1; idummy <= lenLk; idummy++)
		          } // if (lu + lenUk < ltopl)
		          else {
		             // =========================================================
		             // There is no room for ( uk ) yet.  We have to
		             // right-shift the whole of the remaining LU file.
		             // Note that ( lk ) ends up in the correct place.
		             // =========================================================
		             llsave = ll - lenLk;
		             nmove  = lm - ltopl;
	
		             for (idummy = 1; idummy <= nmove; idummy++) {
		                ll       = ll - 1;
		                lm       = lm - 1;
		                a[ll-1]    = a[lm-1];
		                indc[ll-1] = indc[lm-1];
		                indr[ll-1] = indr[lm-1];
		             } // for (idummy = 1; idummy <= nmove; idummy++)
	
		             ltopl  = ll;
		             ll     = llsave;
		             lm     = ll;
		          } // else
	
		          // ======================================================
		          // Left-shift ( uk ).
		          // ======================================================
		          locr[i-1] = lu + 1;
		          l2      = lm - 1;
		          lm      = lm - lenUk;
	
		          for (l = lm; l <= l2; l++) {
		             lu       = lu + 1;
		             a[lu-1]    = a[l-1];
		             indr[lu-1] = indr[l-1];
		          } // for (l = lm; l <= l2; l++) 
		       } // for (k = 1; k <= nrank[0]; k+)
	
		       // ---------------------------------------------------------------
		       // Save the lengths of the nonempty columns of  L,
		       // and initialize  locc(j)  for the LU update routines.
		       // ---------------------------------------------------------------
		       for (k = 1; k <= numl0; k++) {
		          lenc[k-1] = iqloc[k-1];
		       }
	
		       for (j = 1; j <= n; j++) {
		          locc[j-1] = 0;
		       }
	
		       // ---------------------------------------------------------------
		       // Test for singularity.
		       // lu6chk  sets  nsing, jsing, jumin, Lmax, Umax, DUmax, DUmin
		       // (including entries from the dense LU).
		       // input      i1 = 1 means we're calling lu6chk from LUSOL.
		       // output inform = 1 if there are singularities (nsing > 0).
		       // ---------------------------------------------------------------
		       lu6chk( i1, m, n, w, lena, luparm, parmlu,
		                    a, indc, indr, p, q,
		                    lenc, lenr, locc, locr, inform);
		       nsing  = luparm[10];
		       jsing  = luparm[11];
		       jumin  = luparm[18];
		       Lmax[0]   = parmlu[10];
		       Umax[0]   = parmlu[11];
		       DUmax[0]  = parmlu[12];
		       DUmin[0]  = parmlu[13];
		    } // if (keepLU)
	
		    else {
		       // ---------------------------------------------------------------
		       // keepLU = 0.  L and U were not kept, just the diagonals of U.
		       // lu1fac will probably be called again soon with keepLU = .true.
		       // 11 Mar 2001: lu6chk revised.  We can call it with keepLU = 0,
		       //              but we want to keep Lmax, Umax from lu1fad.
		       // 05 May 2002: Allow for TCP with new lu1DCP.  Diag(U) starts
		       //              below lena2, not lena.  Need lena2 in next line.
		       // ---------------------------------------------------------------
		       lu6chk( i1, m, n, w, lena2, luparm, parmlu,
		                    a, indc, indr, p, q,
		                    lenc, lenr, locc, locr, inform );
		       nsing  = luparm[10];
		       jsing  = luparm[11];
		       jumin  = luparm[18];
		       DUmax[0]  = parmlu[12];
		       DUmin[0]  = parmlu[13];
		    } // else 
	
		    break;
	    } // while (true)

	    

	    // Store output parameters.

	    luparm[9] = inform[0];
	    luparm[10] = nsing;
	    luparm[11] = jsing;
	    luparm[12] = minlen[0];
	    luparm[14] = 0;
	    luparm[15] = nrank[0];
	    luparm[16] = ndens1[0];
	    luparm[17] = ndens2[0];
	    luparm[18] = jumin;
	    luparm[19] = numl0;
	    luparm[20] = lenL[0];
	    luparm[21] = lenU[0];
	    luparm[22] = lenL[0];
	    luparm[23] = lenU[0];
	    luparm[24] = lrow;
	    luparm[26] = mersum[0];
	    luparm[27] = nUtri[0];
	    luparm[28] = nLtri[0];

	    parmlu[9] = Amax[0];
	    parmlu[10] = Lmax[0];
	    parmlu[11] = Umax[0];
	    parmlu[12] = DUmax[0];
	    parmlu[13] = DUmin[0];
	    parmlu[14] = Akmax[0];

	    Agrwth = Akmax[0]  / (Amax[0] + 1.0e-20);
	    Ugrwth = Umax[0]   / (Amax[0] + 1.0e-20);
	    if ( TPP ) {
	        growth = Ugrwth;
	    }
	    else { // TRP or TCP or TSP
	        growth = Agrwth;
	    }
	    parmlu[15] = growth;

	    // ------------------------------------------------------------------
	    // Print statistics for the LU factors.
	    // ------------------------------------------------------------------
	    ncp    = luparm[25];
	    condU  = DUmax[0] / Math.max( DUmin[0], 1.0e-20);
	    dincr  = lenL[0] + lenU[0] - nelem;
	    dincr  = dincr * 100.0 / Math.max( delem, one );
	    avgmer = mersum[0];
	    avgmer = avgmer / dm;
	    nbump  = m - nUtri[0] - nLtri[0];

	    if (nout > 0  &&  lprint >= 10) {
	       if ( TPP ) {
	          UI.setDataText("avgmer = " + nf.format(avgmer) + "\n");
	          UI.setDataText("lenL[0] = " + lenL[0] + "\n");
	          UI.setDataText("(lenL[0] + lenU[0]) = " + (lenL[0] + lenU[0]) + "\n");
	          UI.setDataText("ncp = " + ncp + "\n");
	          UI.setDataText("dincr = " + nf.format(dincr) + "\n");
	          UI.setDataText("nUtri[0] = " + nUtri[0] + "\n");
	          UI.setDataText("lenU[0] = " + lenU[0] + "\n");
	          UI.setDataText("Ltol = " + nf.format(Ltol) + "\n");
	          UI.setDataText("Umax[0] = " + nf.format(Umax[0]) + "\n");
	          UI.setDataText("Ugrwth = " + nf.format(Ugrwth) + "\n");
	          UI.setDataText("ndens1[0] = " + ndens1[0] + "\n");
	          UI.setDataText("Lmax[0] = " + nf.format(Lmax[0]) + "\n");
	       }

	       else {
	          UI.setDataText("kPiv[lPiv] = " + kPiv[lPiv] + "\n");
	          UI.setDataText("avgmer = " + nf.format(avgmer) + "\n");
	          UI.setDataText("lenL[0] = " + lenL[0] + "\n");
	          UI.setDataText("(lenL[0] + lenU[0]) = " + (lenL[0] + lenU[0]) + "\n");
	          UI.setDataText("ncp = " + ncp + "\n");
	          UI.setDataText("dincr = " + nf.format(dincr) + "\n");
	          UI.setDataText("nUtri[0] = " + nUtri[0] + "\n");
	          UI.setDataText("lenU[0] = " + lenU[0] + "\n");
	          UI.setDataText("Ltol = " + nf.format(Ltol) + "\n");
	          UI.setDataText("Umax[0] = " + nf.format(Umax[0]) + "\n");
	          UI.setDataText("Ugrwth = " + nf.format(Ugrwth) + "\n");
	          UI.setDataText("nLtri[0] = " + nLtri[0] + "\n");
	          UI.setDataText("ndens1[0] = " + ndens1[0] + "\n");
	          UI.setDataText("Lmax[0] = " + nf.format(Lmax[0]) + "\n");
	          UI.setDataText("Akmax[0] = " + nf.format(Akmax[0]) + "\n");
	          UI.setDataText("Agrwth = " + nf.format(Agrwth) + "\n");
	       }

	       UI.setDataText("nbump = " + nbump + "\n");
	       UI.setDataText("ndens2 = " + ndens2 + "\n");
	       UI.setDataText("DUmax[0] = " + nf.format(DUmax[0]) + "\n");
	       UI.setDataText("DUmin[0] = " + nf.format(DUmin[0]) + "\n");
	       UI.setDataText("condU = " + nf.format(condU) + "\n");
	    } // if (nout > 0  &&  lprint >= 10)

	    return;
	  } // lu1fqac

    private void lu1or1( int m, int n, int nelem, int lena, double small,
              double a[], int indc[], int indr[], int lenc[], int lenr[],
              double Amax[], int numnz[], int lerr[], int inform[] ) {

//integer(ip),   intent(in)    :: m, n, nelem, lena
//integer(ip),   intent(out)   :: lerr, inform
//real(rp),      intent(in)    :: small
//real(rp),      intent(inout) :: a(lena)
//integer(ip),   intent(inout) :: indc(lena), indr(lena)
//integer(ip),   intent(out)   :: lenc(n), lenr(m)

//------------------------------------------------------------------
// lu1or1  organizes the elements of an  m by n  matrix  A  as
// follows.  On entry, the parallel arrays   a, indc, indr,
// contain  nelem  entries of the form     aij,    i,    j,
// in any order.  nelem  must be positive.

// Entries not larger than the input parameter  small  are treated as
// zero and removed from   a, indc, indr.  The remaining entries are
// defined to be nonzero.  numnz  returns the number of such nonzeros
// and  Amax  returns the magnitude of the largest nonzero.
// The arrays  lenc, lenr  return the number of nonzeros in each
// column and row of  A.

// inform = 0  on exit, except  inform = 1  if any of the indices in
// indc, indr  imply that the element  aij  lies outside the  m by n
// dimensions of  A.

// xx Feb 1985: Original version.
// 17 Oct 2000: a, indc, indr now have size lena to allow nelem = 0.
//
// 10 Jan 2010: First f90 version.
// 12 Dec 2011: Declare intent and local variables.
// ------------------------------------------------------------------

int i, j, l;
final double zero = 0.0;

for (i = 0; i < m; i++) {
	lenr[i] = 0;
}
for (i = 0; i < n; i++) {
	lenc[i] = 0;
}

lerr[0]   = 0;
Amax[0]   = zero;
numnz[0]  = nelem;

for (l = nelem; l >= 1; l--) {
if (Math.abs(a[l-1]) > small) {
   i      = indc[l-1];
   j      = indr[l-1];
   Amax[0]   = Math.max( Amax[0], Math.abs(a[l-1]) );
   if (i < 1  ||  i > m) {
	   lerr[0] = l;
	   inform[0] = 1;
	   return;
   }
   if (j < 1 ||  j > n) {
	   lerr[0] = l;
	   inform[0] = 1;
	   return;   
   }
   lenr[i-1] = lenr[i-1] + 1;
   lenc[j-1] = lenc[j-1] + 1;
}
else {

   // Replace a negligible element by last element.  Since
  //! we are going backwards, we know the last element is ok.

   a[l-1]    = a[numnz[0]-1];
   indc[l-1] = indc[numnz[0]-1];
   indr[l-1] = indr[numnz[0]-1];
   numnz[0]   = numnz[0] - 1;
} // else
} // for (l = nelem; l >= 1; l--)

inform[0] = 0;
return;
    } // lu1or1

    private void lu1or2(int n, int numa, int lena, double a[], int inum[], int jnum[], int lenc[], int locc[]) {

    //integer(ip),   intent(in)    :: n, numa, lena
    //integer(ip),   intent(in)    :: lenc(n)
    //integer(ip),   intent(inout) :: inum(lena), jnum(lena)
    //integer(ip),   intent(out)   :: locc(n)
    //real(rp),      intent(inout) :: a(lena)

    // ------------------------------------------------------------------
    // lu1or2  sorts a list of matrix elements  a(i,j)  into column
    // order, given  numa  entries  a(i,j),  i,  j  in the parallel
    // arrays  a, inum, jnum  respectively.  The matrix is assumed
    // to have n columns and an arbitrary number of rows.
    //
    // On entry, lenc(*) must contain the length of each column.
    
    // On exit,  a(*) and inum(*)  are sorted,  jnum(*) = 0,  and
    // locc(j)  points to the start of column j.
    
    // lu1or2  is derived from mc20ad, a routine in the Harwell
    // Subroutine Library, author J. K. Reid.

    // xx Feb 1985: Original version.
    // 17 Oct 2000: a, inum, jnum now have size lena to allow nelem = 0.
    
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ------------------------------------------------------------------

    int i, ice, icep, j, ja, jb, jce, jcep, l;
    double ace, acep;

    // Set  loc(j)  to point to the beginning of column  j.

    l = 1;
    for (j  = 1; j <= n; j++) {
       locc[j-1] = l;
       l       = l + lenc[j-1];
    }

    // Sort the elements into column order.
    // The algorithm is an in-place sort and is of order  numa.

    for (i = 1; i <= numa; i++) {
       // Establish the current entry.
       jce     = jnum[i-1];
       if (jce == 0) {
    	   continue;
       }
       ace     = a[i-1];
       ice     = inum[i-1];
       jnum[i-1] = 0;

       // Chain from current entry.

       for (j = 1; j <= numa; j++) {

          // The current entry is not in the correct position.
          // Determine where to store it.

          l         = locc[jce-1];
          locc[jce-1] = locc[jce-1] + 1;

          // Save the contents of that location.

          acep = a[l-1];
          icep = inum[l-1];
          jcep = jnum[l-1];

          // Store current entry.

          a[l-1]    = ace;
          inum[l-1] = ice;
          jnum[l-1] = 0;

          // If next current entry needs to be processed,
          // copy it into current entry.

          if (jcep == 0) {
        	  break;
          }
          ace = acep;
          ice = icep;
          jce = jcep;
       } // for (j = 1; j <= numa; j++)
    } // for (i = 1; i <= numa; i++)

    // Reset loc(j) to point to the start of column j.

    ja = 1;
    for (j  = 1; j <= n; j++) {
       jb      = locc[j-1];
       locc[j-1] = ja;
       ja      = jb;
    } // for (j  = 1; j <= n; j++)

    } // lu10r2

    private void lu1or3(int m, int n, int lena, int indc[], int lenc[], int locc[], int iw[], int lerr[], int inform[]) {

    //integer(ip),   intent(in)    :: m, n, lena
    //integer(ip),   intent(out)   :: lerr, inform
    //integer(ip),   intent(in)    :: indc(lena), lenc(n), locc(n)
    //integer(ip),   intent(out)   :: iw(m)

    //------------------------------------------------------------------
    // lu1or3  looks for duplicate elements in an m by n matrix A
    // defined by the column list  indc, lenc, locc.
    // iw  is used as a work vector of length  m.
    
    // xx Feb 1985: Original version.
    // 17 Oct 2000: indc, indr now have size lena to allow nelem = 0.
    
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ------------------------------------------------------------------

    int i, j, l, l1, l2;

    for (i = 0; i < m; i++) {
    	iw[i] = 0;
    }
    lerr[0]    = 0;

    for (j = 1; j <= n; j++) {
       if (lenc[j-1] > 0) {
          l1   = locc[j-1];
          l2   = l1 + lenc[j-1] - 1;

          for (l = l1; l <= l2; l++) {
             i = indc[l-1];
             if (iw[i-1] == j) {
            	 lerr[0] = l;
            	 inform[0] = 1;
            	 return;
             }
             iw[i-1] = j;
          } // for (l = l1; l <= l2; l++)
       } // if (lenc[j-1] > 0)
    } // for (j = 1; j <= n; j++)

    inform[0] = 0;
    return;
    } // lu1or3

    private void lu1or4(int m, int n, int nelem, int lena, int indc[], int indr[], int lenc[], int lenr[], int locc[], int locr[]) {

    //integer(ip),   intent(in)    :: m, n, nelem, lena
    //integer(ip),   intent(in)    :: indc(lena), lenc(n), locc(n)
    //integer(ip),   intent(out)   :: indr(lena), lenr(m), locr(m)

    // ------------------------------------------------------------------
    // lu1or4     constructs a row list  indr, locr
    // from a corresponding column list  indc, locc,
    // given the lengths of both columns and rows in  lenc, lenr.
    //
    // xx Feb 1985: Original version.
    // 17 Oct 2000: indc, indr now have size lena to allow nelem = 0.
    //
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ------------------------------------------------------------------

    int i, j, jdummy, l, l1, l2, lr;

    // Initialize  locr(i)  to point just beyond where the
    // last component of row  i  will be stored.

    l      = 1;
    for (i = 1; i <= m; i++) {
       l       = l + lenr[i-1];
       locr[i-1] = l;
    }

    // By processing the columns backwards and decreasing  locr(i)
    // each time it is accessed, it will end up pointing to the
    // beginning of row  i  as required.

    l2     = nelem;
    j      = n + 1;

    for (jdummy = 1; jdummy <= n; jdummy++) {
       j  = j - 1;
       if (lenc[j-1] > 0) {
          l1 = locc[j-1];

          for (l = l1; l <= l2; l++) {
             i        = indc[l-1];
             lr       = locr[i-1] - 1;
             locr[i-1]  = lr;
             indr[lr-1] = j;
          } // for (l = l1; l <= l2; l++)

          l2     = l1 - 1;
    } // if (lenc[j-1] > 0)
    } // for (jdummy = 1; jdummy <= n; jdummy++)

    } // lu1or4
    
    private void lu1pq1(int m, int n, int len[], int iperm[], int loc[], int inv[], int num[]) {

    //integer(ip),   intent(in)    :: m, n
    //integer(ip),   intent(in)    :: len(m)
    //integer(ip),   intent(out)   :: iperm(m), loc(n), inv(m)
    //integer(ip),   intent(out)   :: num(n) ! workspace

    // ------------------------------------------------------------------
    // lu1pq1  constructs a permutation  iperm  from the array  len.
    
    // On entry:
    // len(i)  holds the number of nonzeros in the i-th row (say)
    //         of an m by n matrix.
    // num(*)  can be anything (workspace).
    
    // On exit:
    // iperm   contains a list of row numbers in the order
    //         rows of length 0,  rows of length 1,..., rows of length n.
    // loc(nz) points to the first row containing  nz  nonzeros,
    //         nz = 1, n.
    // inv(i)  points to the position of row i within iperm(*).
    //
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ------------------------------------------------------------------

    int i, l, nz, nzero;

    // Count the number of rows of each length.

    nzero    = 0;
    for (i = 0; i < n; i++) {
    	num[i] = 0;
    	loc[i] = 0;
    }

    for (i = 1; i <= m; i++) {
       nz      = len[i-1];
       if (nz == 0) {
          nzero   = nzero   + 1;
       }
       else {
          num[nz-1] = num[nz-1] + 1;
       }
    } // for (i = 1; i <= m; i++)

    // Set starting locations for each length.

    l      = nzero + 1;
    for (nz  = 1; nz <= n; nz++) {
       loc[nz-1] = l;
       l       = l + num[nz-1];
       num[nz-1] = 0;
    } // for (nz  = 1; nz <= n; nz++)

    // Form the list.

    nzero  = 0;
    for (i = 1; i <= m; i++) {
       nz = len[i-1];
       if (nz == 0) {
          nzero = nzero + 1;
          iperm[nzero-1] = i;
       }
       else {
          l        = loc[nz-1] + num[nz-1];
          iperm[l-1] = i;
          num[nz-1]  = num[nz-1] + 1;
       }
    } // for (i = 1; i <= m; i++)

    // Define the inverse of iperm.

    for (l = 1; l <= m; l++) {
       i      = iperm[l-1];
       inv[i-1] = l;
    }

    } // lu1pq1

    private void lu1fad(int m, int n, int nelem, int lena, int luparm[], double parmlu[],
            double a[], int indc[], int indr[], int p[], int q[],
            int lenc[], int lenr[], int locc[], int locr[],
            int iploc[], int iqloc[], int ipinv[], int iqinv[], double w[],
            int lenH  , double Ha[], int Hj[], int Hk[], double Amaxr[],
            int inform[], int lenL[], int lenU[], int minlen[], int mersum[],
            int nUtri[] , int nLtri[], int ndens1[], int ndens2[], int nrank[],
            double Lmax[], double Umax[], double DUmax[], double DUmin[], double Akmax[]) {

//integer(ip),   intent(in)    :: m, n, nelem, lena, lenH
//integer(ip),   intent(inout) :: luparm(30)
//real(rp),      intent(inout) :: parmlu(30), a(lena), Amaxr(m), w(n), Ha(lenH)
//integer(ip),   intent(inout) :: indc(lena), indr(lena), p(m)    , q(n)    , &
//                           lenc(n)   , lenr(m)   , locc(n) , locr(m) , &
//                           iploc(n)  , iqloc(m)  , ipinv(m), iqinv(n), &
//                           Hj(lenH)  , Hk(lenH)
//integer(ip),   intent(out)   :: inform, lenL  , lenU  , minlen, mersum,     &
//                           nUtri , nLtri , ndens1, ndens2, nrank
//real(rp),      intent(out)   :: Lmax, Umax, DUmax, DUmin, Akmax

// ------------------------------------------------------------------
// lu1fad  is a driver for the numerical phase of lu1fac.
// At each stage it computes a column of  L  and a row of  U,
// using a Markowitz criterion to select the pivot element,
// subject to a stability criterion that bounds the elements of  L.
//
// 00 Jan 1986  Version documented in LUSOL paper:
//              Gill, Murray, Saunders and Wright (1987),
//              Maintaining LU factors of a general sparse matrix,
//              Linear algebra and its applications 88/89, 239-270.

// 02 Feb 1989  Following Suhl and Aittoniemi (1987), the largest
//              element in each column is now kept at the start of
//              the column, i.e. in position locc(j) of a and indc.
//              This should speed up the Markowitz searches.
//              To save time on highly triangular matrices, we wait
//              until there are no further columns of length 1
//              before setting and maintaining that property.

// 12 Apr 1989  ipinv and iqinv added (inverses of p and q)
//              to save searching p and q for rows and columns
//              altered in each elimination step.  (Used in lu1pq2)

// 19 Apr 1989  Code segmented to reduce its size.
//              lu1gau does most of the Gaussian elimination work.
//              lu1mar does just the Markowitz search.
//              lu1mxc moves biggest elements to top of columns.
//              lu1pen deals with pending fill-in in the row list.
//              lu1pq2 updates the row and column permutations.

// 26 Apr 1989  maxtie replaced by maxcol, maxrow in the Markowitz
//              search.  maxcol, maxrow change as density increases.

// 25 Oct 1993  keepLU implemented.

// 07 Feb 1994  Exit main loop early to finish off with a dense LU.
//              densLU tells lu1fad whether to do it.
// 21 Dec 1994  Bug fixed.  nrank was wrong after the call to lu1ful.
// 12 Nov 1999  A parallel version of dcopy gave trouble in lu1ful
//              during left-shift of dense matrix D within a(*).
//              Fixed this unexpected problem here in lu1fad
//              by making sure the first and second D don't overlap.

// 13 Sep 2000  TCP (Threshold Complete Pivoting) implemented.
//              lu2max added
//              (finds aijmax from biggest elems in each col).
//              Utri, Ltri and Spars1 phases apply.
//              No switch to Dense CP yet.  (Only TPP switches.)
// 14 Sep 2000  imax needed to remember row containing aijmax.
// 22 Sep 2000  For simplicity, lu1mxc always fixes
//              all modified cols.
//              (TPP spars2 used to fix just the first maxcol cols.)
// 08 Nov 2000: Speed up search for aijmax.
//              Don't need to search all columns if the elimination
//              didn't alter the col containing the current aijmax.
// 21 Nov 2000: lu1slk implemented for Utri phase with TCP
//              to guard against deceptive triangular matrices.
//              (Utri used to have aijtol >= 0.9999 to include
//              slacks, but this allows other 1s to be accepted.)
//              Utri now accepts slacks, but applies normal aijtol
//              test to other pivots.
// 28 Nov 2000: TCP with empty cols must call lu1mxc and lu2max
//              with ( lq1, n, ... ), not just ( 1, n, ... ).
// 23 Mar 2001: lu1fad bug with TCP.
//              A col of length 1 might not be accepted as a pivot.
//              Later it appears in a pivot row and temporarily
//              has length 0 (when pivot row is removed
//              but before the column is filled in).  If it is the
//              last column in storage, the preceding col also thinks
//              it is "last".  Trouble arises when the preceding col
//              needs fill-in -- it overlaps the real "last" column.
//              (Very rarely, same trouble might have happened if
//              the drop tolerance caused columns to have length 0.)!
    	
//              Introduced ilast to record the last row in row file,
//                         jlast to record the last col in col file.
//              lu1rec returns ilast = indr(lrow + 1)
//                          or jlast = indc(lcol + 1).
//              (Should be an output parameter, but didn't want to
//              alter lu1rec's parameter list.)
//              lu1rec also treats empty rows or cols safely.
//              (Doesn't eliminate them!)

// 26 Apr 2002: Heap routines added for TCP.
//              lu2max no longer needed.
//              imax, jmax used only for printing.
// 01 May 2002: lu1DCP implemented (dense complete pivoting).
//              Both TPP and TCP now switch to dense LU
//              when density exceeds dens2.
// 06 May 2002: In dense mode, store diag(U) in natural order.
// 09 May 2002: lu1mCP implemented (Markowitz TCP via heap).
// 11 Jun 2002: lu1mRP implemented (Markowitz TRP).
// 28 Jun 2002: Fixed call to lu1mxr.
// 14 Dec 2002: lu1mSP implemented (Markowitz TSP).
// 15 Dec 2002: Both TPP and TSP can grab cols of length 1
//              during Utri.
// 19 Dec 2004: Hdelete(...) has new input argument Hlenin.
// 26 Mar 2006: lu1fad returns nrank  = min( mrank, nrank )
//              and ignores nsing from lu1ful

// 10 Jan 2010: First f90 version.
// 03 Apr 2013: lu1mxr recoded to improve efficiency of TRP.
// ------------------------------------------------------------------

boolean Utri, Ltri, spars1, spars2, dense,
        densLU, keepLU, TCP, TPP, TRP, TSP;
double aijmax = 0.0;
double aijtol = 0.0;
double abest, amax, 
       dens1, dens2, diag,
       Lij, Ltol, small, Uspace;
final int i1 = 1;
final double zero = 0.0;
final double one = 1.0;


// ------------------------------------------------------------------
// Local variables
// ---------------

// lcol   is the length of the column file.  It points to the last
//        nonzero in the column list.
// lrow   is the analogous quantity for the row file.
// lfile  is the file length (lcol or lrow) after the most recent
//        compression of the column list or row list.
// nrowd  and  ncold  are the number of rows and columns in the
//        matrix defined by the pivot column and row.  They are the
//        dimensions of the submatrix D being altered at this stage.
// melim  and  nelim  are the number of rows and columns in the
//        same matrix D, excluding the pivot column and row.
// mleft  and  nleft  are the number of rows and columns
//        still left to be factored.
// nzchng is the increase in nonzeros in the matrix that remains
//        to be factored after the current elimination
//        (usually negative).
// nzleft is the number of nonzeros still left to be factored.
// nspare is the space we leave at the end of the last row or
//        column whenever a row or column is being moved to the end
//        of its file.  nspare = 1 or 2 might help reduce the
//        number of file compressions when storage is tight.
//
// The row and column ordering permutes A into the form
//
//                        ------------------------
//                         \                     |
//                          \         U1         |
//                           \                   |
//                            --------------------
//                            |\
//                            | \
//                            |  \
//            P A Q   =       |   \
//                            |    \
//                            |     --------------
//                            |     |            |
//                            |     |            |
//                            | L1  |     A2     |
//                            |     |            |
//                            |     |            |
//                            --------------------

// where the block A2 is factored as  A2 = L2 U2.
// The phases of the factorization are as follows.

// Utri   is true when U1 is being determined.
//        Any column of length 1 is accepted immediately (if TPP).

// Ltri   is true when L1 is being determined.
//        lu1mar exits as soon as an acceptable pivot is found
//        in a row of length 1.

// spars1 is true while the density of the (modified) A2 is less
//        than the parameter dens1 = parmlu(7) = 0.3 say.
//        lu1mar searches maxcol columns and maxrow rows,
//        where  maxcol = luparm(3),  maxrow = maxcol - 1.
//        lu1mxc is used to keep the biggest element at the top
//        of all remaining columns.

// spars2 is true while the density of the modified A2 is less
//        than the parameter dens2 = parmlu(8) = 0.6 say.
//        lu1mar searches maxcol columns and no rows.
//        lu1mxc could fix up only the first maxcol cols (with TPP).
//        22 Sep 2000:  For simplicity, lu1mxc fixes all modified cols.

// dense  is true once the density of A2 reaches dens2.
//        lu1mar searches only 1 column (the shortest).
//        lu1mxc could fix up only the first column (with TPP).
//        22 Sep 2000:  For simplicity, lu1mxc fixes all modified cols.
// ------------------------------------------------------------------

int h[] = new int[1];
int Hlen[] = new int[1];
int hops[] = new int[1];
int ibest[] = new int[1];
int ilast[] = new int[1];
int jbest[] = new int[1];
int jlast[] = new int[1];
int lcol[] = new int[1];
int lD = 0;
int ldiagU = 0;
int lenD = 0;
int lfirst[] = new int[1];
int lrow[] = new int[1];
int lu[] = new int[1];
int mark = 0;
int mbest[] = new int[1];
int mrank[] = new int[1];
int nfill[] = new int[1];
int nsing[] = new int[1];
int nzchng[] = new int[1];
int Hlenin,
    i, imax,
    j, jmax, lPiv,
    k, kbest, kk, l, last, lc, lc1,
    leni, lenj,
    lfile, lfree, limit,
    ll, ll1, lpivc, lpivc1, lpivc2,
    lpivr, lpivr1, lpivr2, lprint,
    lq, lq1, lq2, lr, lr1,
    ls, lsave, lu1,
    maxcol, maxmn, maxrow,
    melim, minfre, minmn, mleft,
    ncold, nelim,
    nfree, nleft, nrowd, nrowu,
    nspare, nzleft;
int markc[] = new int[n];
int markr[] = new int[m];
double v;
boolean seg1 = true;
boolean seg2 = true;
boolean seg3 = true;
boolean seg4 = true;
double al[];
int markl[];
double au[];
int ifill[];
int jfill[];
int ind2[];
int lenold[];
double d[];

lprint = luparm[1];
maxcol = luparm[2];
lPiv   = luparm[5];
keepLU = luparm[7] != 0;

TPP    = lPiv == 0;  // Threshold Partial   Pivoting (normal).
TRP    = lPiv == 1;  // Threshold Rook      Pivoting
TCP    = lPiv == 2;  // Threshold Complete  Pivoting.
TSP    = lPiv == 3;  // Threshold Symmetric Pivoting.

densLU = false;
maxrow = maxcol - 1;
ilast[0]  = m;                 // Assume row m is last in the row file.
jlast[0]  = n;                 // Assume col n is last in the col file.
lfile  = nelem;
lrow[0]   = nelem;
lcol[0]   = nelem;
minmn  = Math.min( m, n );
maxmn  = Math.max( m, n );
nzleft = nelem;
nspare = 1;

if ( keepLU ) {
lu1    = lena   + 1;
}
else {
// Store only the diagonals of U in the top of memory.
ldiagU = lena   - n;
lu1    = ldiagU + 1;
}

Ltol   = parmlu[0];
small  = parmlu[2];
Uspace = parmlu[5];
dens1  = parmlu[6];
dens2  = parmlu[7];
Utri   = true;
Ltri   = false;
spars1 = false;
spars2 = false;
dense  = false;

// Check parameters.

Ltol   = Math.max( Ltol, 1.0001);
dens1  = Math.min( dens1, dens2 );

// Initialize output parameters.
// lenL, lenU, minlen, mersum, nUtri, nLtri, ndens1, ndens2, nrank
// are already initialized by lu1fac.

Lmax[0]   = zero;
Umax[0]   = zero;
DUmax[0]  = zero;
DUmin[0]  = 1.0e+20;
if (nelem == 0) {
	DUmin[0] = zero;
}
Akmax[0]  = zero;
hops[0]   = 0;

// More initialization.

if (TPP || TSP) {
	// Don't worry yet about lu1mxc.
    aijmax = zero;
    aijtol = zero;
    Hlen[0]   = 1;
}
else { // TRP or TCP
    // Move biggest element to top of each column.
    // Set w(*) to mark slack columns (unit vectors).

    lu1mxc( i1, n, q, a, indc, lenc, locc );
    lu1slk( m, n, lena, q, iqloc, a, locc, w );
}

if (TRP) {
// Find biggest element in each row.

mark = 0;
lu1mxr( mark, i1, m, m, n, lena,
        a, indc, lenc, locc, indr, lenr, locr,
           p, markc, markr, Amaxr );
} // if (TRP)

if (TCP) {
// Set Ha(1:Hlen) = biggest element in each column,
// Hj(1:Hlen) = corresponding column indices.

Hlen[0]  = 0;
for (kk = 1; kk <= n; kk++) {
 Hlen[0]     = Hlen[0] + 1;
 j        = q[kk-1];
 lc       = locc[j-1];
 Ha[Hlen[0]-1] = Math.abs( a[lc-1] );
 Hj[Hlen[0]-1] = j;
 Hk[j-1]    = Hlen[0];
} // for (kk = 1; kk <= n; kk++) 

// Build the heap, creating new Ha, Hj and setting Hk(1:Hlen).

Hbuild( Ha, Hj, Hk, Hlen[0], Hlen[0], hops );
} // if (TCP)

// ------------------------------------------------------------------
//  Start of main loop.
// ------------------------------------------------------------------
mleft  = m + 1;
nleft  = n + 1;

for (nrowu = 1; nrowu <= minmn; nrowu++) {

// mktime = (nrowu / ntime) + 4
// eltime = (nrowu / ntime) + 9
mleft  = mleft - 1;
nleft  = nleft - 1;

// Bail out if there are no nonzero rows left.

if (iploc[0] > m) {
	break;
}
// For TCP, the largest Aij is at the top of the heap.

if ( TCP ) {
 aijmax = Ha[0];      // Marvelously easy !
 Akmax[0]  = Math.max( Akmax[0], aijmax );
 aijtol = aijmax / Ltol;
} // if (TCP)

// ===============================================================
//  Find a suitable pivot element.
// ===============================================================

if ( Utri ) {
 // ------------------------------------------------------------
 // So far all columns have had length 1.
 // We are still looking for the (backward) triangular part of A
 // that forms the first rows and columns of U.
 // ------------------------------------------------------------

 lq1    = iqloc[0];
 lq2    = n;
 if (m   >   1) {
	 lq2 = iqloc[1] - 1;
 }

 if (lq1 <= lq2) {  // There are more cols of length 1.
    if (TPP || TSP) {
       jbest[0]  = q[lq1-1];   // Grab the first one.
    } // if (TPP || TSP)
    else { // TRP or TCP     Scan all columns of length 1.
       jbest[0]  = 0;

       for (lq = lq1; lq <= lq2; lq++) {
          j      = q[lq-1];
          if (w[j-1] > zero) { // Accept a slack
             jbest[0]  = j;
             break;
          } // if (w[j-1] > zero)

          lc     = locc[j-1];
          amax   = Math.abs( a[lc-1] );
          if (TRP) {
             i      = indc[lc-1];
             aijtol = Amaxr[i-1] / Ltol;
          } // if (TRP)

          if (amax >= aijtol) {
             jbest[0]  = j;
             break;
          } // if (amax >= aijtol)
       } // for (lq = lq1; lq <= lq2; lq++)
    } // else TRP or TCP
    
    if (jbest[0] > 0) {
       lc     = locc[jbest[0]-1];
       ibest[0]  = indc[lc-1];
       mbest[0]  = 0;
       seg1 = false;
       seg2 = false;
    } // if (jbest[0 > 0)
 } // if (lq1 <= lq2)

 // This is the end of the U triangle.
 // We will not return to this part of the code.
 // TPP and TSP call lu1mxc for the first time
 // (to move biggest element to top of each column).

 if (seg1) {
 if (lprint >= 50) {
    UI.setDataText("Utri ended.  spars1 = true\n");
 } // if (lprint >= 50)
 Utri   = false;
 Ltri   = true;
 spars1 = true;
 nUtri[0]  =  nrowu - 1;
 if (TPP || TSP) {
    lu1mxc( lq1, n, q, a, indc, lenc, locc );
 } // if (TPP || TSP)
 } // if (seg1)
 seg1 = true;
} // if (Utri)

if (seg2) {
if ( spars1 ) {
 // ------------------------------------------------------------
 // Perform a Markowitz search.
 // Search cols of length 1, then rows of length 1,
 // then   cols of length 2, then rows of length 2, etc.
 // ------------------------------------------------------------
 // if (TPP) then ! 12 Jun 2002: Next line disables lu1mCP below
 if (TPP || TCP) {
    lu1mar( m    , n     , lena  , maxmn,
                 TCP  , aijtol, Ltol  , maxcol, maxrow,
                 ibest, jbest , mbest ,
                 a    , indc  , indr  , p     , q,
                 lenc , lenr  , locc  , locr  , 
                 iploc, iqloc );
 } // if (TPP || TCP)
 else if (TRP) {
    lu1mRP( m    , n     , lena  , maxmn,
              Ltol , maxcol, maxrow,
              ibest, jbest , mbest ,
              a    , indc  , indr  , p    , q,
              lenc , lenr  , locc  , locr ,
              iploc, iqloc , Amaxr );

    // else if (TCP) then ! Disabled by test above
    // call lu1mCP( m    , n     , lena  , aijtol, &
    //              ibest, jbest , mbest ,         &
    //              a    , indc  , indr  ,         &
    //              lenc , lenr  , locc  ,         &
    //              Hlen , Ha    , Hj    )
 } // else if (TRP)
 else if (TSP) {
    lu1mSP( m    , n     , lena  , maxmn,
                 Ltol , maxcol,
                 ibest, jbest , mbest ,
                 a    , indc  , q    , locc , iqloc );
    if (ibest[0] == 0) {
    	return;
    }
 } // else if (TSP)

 if ( Ltri ) {

    // So far all rows have had length 1.
    // We are still looking for the (forward) triangle of A
    // that forms the first rows and columns of L.

    if (mbest[0] > 0) {
       Ltri   = false;
       nLtri[0]  =  nrowu - 1 - nUtri[0];
       if (lprint >= 50) {
          UI.setDataText("Ltri ended.\n");
       }
    } // if (mbest[0] > 0)
    } // if (Ltri)
 else { // not Ltri   ! See if what's left is as dense as dens1.

    if (nzleft  >=  (dens1 * mleft) * nleft) {
       spars1 = false;
       spars2 = true;
       ndens1[0] =  nleft;
       maxrow =  0;
       if (lprint >= 50) {
          UI.setDataText("spars1 ended.  spars2 = true\n");
       }
    } //  if (nzleft  >=  (dens1 * mleft) * nleft)
 } // else not Ltri
} // if (spars1)
else if ( spars2 || dense ) {
 // ------------------------------------------------------------
 // Perform a restricted Markowitz search,
 // looking at only the first maxcol columns.  (maxrow = 0.)
 // ------------------------------------------------------------
// if (TPP) then ! 12 Jun 2002: Next line disables lu1mCP below
 if (TPP || TCP) {
    lu1mar( m    , n     , lena  , maxmn,
                 TCP  , aijtol, Ltol  , maxcol, maxrow,
                 ibest, jbest , mbest ,           
                 a    , indc  , indr  , p     , q,   
                 lenc , lenr  , locc  , locr  ,      
                 iploc, iqloc );
 } // if (TPP || TCP)
 else if (TRP) {
    lu1mRP( m    , n     , lena  , maxmn, 
                 Ltol , maxcol, maxrow,        
                 ibest, jbest , mbest ,          
                 a    , indc  , indr  , p    , q,
                 lenc , lenr  , locc  , locr ,   
                 iploc, iqloc , Amaxr );

    // else if (TCP) then ! Disabled by test above
    // call lu1mCP( m    , n     , lena  , aijtol, &
    //              ibest, jbest , mbest ,         &
    //              a    , indc  , indr  ,         &
    //              lenc , lenr  , locc  ,         &
    //              Hlen , Ha    , Hj    )
 } // else if (TRP)
 else if (TSP) {
    lu1mSP( m    , n     , lena  , maxmn,
                 Ltol , maxcol,               
                 ibest, jbest , mbest ,       
                 a    , indc  , q    , locc , iqloc );
    if (ibest[0] == 0) {
    	inform[0] = 9;
    	return;
    }
 } // else if (TSP)

 // See if what's left is as dense as dens2.

 if ( spars2 ) {
    if (nzleft  >=  (dens2 * mleft) * nleft) {
       spars2 = false;
       dense  = true;
       ndens2[0] =  nleft;
       maxcol =  1;
       if (lprint >= 50) {
          UI.setDataText("spars2 ended.  dense = true\n");
       }
    } // if (nzleft  >=  (dens2[0] * mleft) * nleft)
 } // if (spars2)
} // else if ( spars2 || dense )

// ---------------------------------------------------------------
// See if we can finish quickly.
// ---------------------------------------------------------------
if ( dense  ) {
 lenD   = mleft * nleft;
 nfree  = lu1 - 1;

 if (nfree >= 2 * lenD) {

    // There is room to treat the remaining matrix as
    // a dense matrix D.
    // We may have to compress the column file first.
    // 12 Nov 1999: D used to be put at the
    //              beginning of free storage (lD = lcol + 1).
    //              Now put it at the end     (lD = lu1 - lenD)
    //              so the left-shift in lu1ful will not
    //              involve overlapping storage
    //              (fatal with parallel dcopy).

    densLU = true;
    ndens2[0] = nleft;
    lD     = lu1 - lenD;
    if (lcol[0] >= lD) {
       lu1rec( n, true, luparm, lcol, lena, a, indc, lenc, locc );
       lfile  = lcol[0];
       jlast[0]  = indc[lcol[0]];
    } // if (lcol[0] >= lD)

    break;
 } // if (nfree >= 2 * lenD)
} // if (dense)
} // if (seg2)
seg2 = true;

// ===============================================================
// The best  aij  has been found.
// The pivot row  ibest  and the pivot column  jbest
// define a dense matrix  D  of size  nrowd x ncold.
// ===============================================================
ncold  = lenr[ibest[0]-1];
nrowd  = lenc[jbest[0]-1];
melim  = nrowd  - 1;
nelim  = ncold  - 1;
mersum[0] = mersum[0] + mbest[0];
lenL[0]   = lenL[0]   + melim;
lenU[0]   = lenU[0]   + ncold;
if (lprint >= 50) {
 if (nrowu == 1) {
    UI.setDataText("lu1fad debug:\n");
 } // if (nrowu == 1)
 if ( TPP || TRP || TSP ) {
	UI.setDataText("nrowu = " + nrowu + "\n");
	UI.setDataText("ibest[0] = " + ibest[0] + "\n");
	UI.setDataText("jbest[0] = " + jbest[0] + "\n");
	UI.setDataText("nrowd = " + nrowd + "\n");
	UI.setDataText("ncold = " + ncold + "\n");
 }
 else { // TCP
    jmax   = Hj[0];
    imax   = indc[locc[jmax-1]-1];
    UI.setDataText("nrowu = " + nrowu + "\n");
	UI.setDataText("ibest[0] = " + ibest[0] + "\n");
	UI.setDataText("jbest[0] = " + jbest[0] + "\n");
	UI.setDataText("nrowd = " + nrowd + "\n");
	UI.setDataText("ncold = " + ncold + "\n");
	UI.setDataText("imax = " + imax + "\n");
	UI.setDataText("jmax = " + jmax + "\n");
	UI.setDataText("aijmax = " + nf.format(aijmax) + "\n");
 } // else TCP
} // if (lprint >= 50)

// ===============================================================
// Allocate storage for the next column of  L  and next row of  U.
// Initially the top of a, indc, indr are used as follows:

//            ncold       melim       ncold        melim

// a      |...........|...........|ujbest..ujn|li1......lim|

// indc   |...........|  lenr(i)  |  lenc(j)  |  markl(i)  |

// indr   |...........| iqloc(i)  |  jfill(j) |  ifill(i)  |

//       ^           ^             ^           ^            ^
//       lfree   lsave             lu1         ll1          oldlu1

// Later the correct indices are inserted:

// indc   |           |           |           |i1........im|

// indr   |           |           |jbest....jn|ibest..ibest|

// ===============================================================
if ( keepLU ) {
 // relax
}
else {
 // Always point to the top spot.
 // Only the current column of L and row of U will
 // take up space, overwriting the previous ones.
 lu1    = ldiagU + 1;
}
ll1    = lu1   - melim;
lu1    = ll1   - ncold;
lsave  = lu1   - nrowd;
lfree  = lsave - ncold;

// Make sure the column file has room.
// Also force a compression if its length exceeds a certain limit.

limit  = (int)(Uspace*(double)(lfile))  +  m  +  n  +  1000;
minfre = ncold  + melim;
nfree  = lfree  - lcol[0];
if (nfree < minfre  ||  lcol[0] > limit) {
 lu1rec( n, true, luparm, lcol, lena, a, indc, lenc, locc );
 lfile  = lcol[0];
 jlast[0]  = indc[lcol[0]];
 nfree  = lfree - lcol[0];
 if (nfree < minfre) {
	 // Not enough space free after a compress.
	 // Set  minlen  to an estimate of the necessary value of  lena.
	 inform[0] = 7;
     minlen[0] = lena  +  lfile  +  2*(m + n);
     return;
 }
} // if (nfree < minfre  ||  lcol[0] > limit)

// Make sure the row file has room.

minfre = melim + ncold;
nfree  = lfree - lrow[0];
if (nfree < minfre  ||  lrow[0] > limit) {
 lu1rec( m, false, luparm, lrow, lena, a, indr, lenr, locr );
 lfile  = lrow[0];
 ilast[0]  = indr[lrow[0]];
 nfree  = lfree - lrow[0];
 if (nfree < minfre){
	 // Not enough space free after a compress.
	 // Set  minlen  to an estimate of the necessary value of  lena.
	 inform[0] = 7;
     minlen[0] = lena  +  lfile  +  2*(m + n);
     return;
 }
} // if (nfree < minfre  ||  lrow[0 > limit)

// ===============================================================
// Move the pivot element to the front of its row
// and to the top of its column.
// ===============================================================
lpivr  = locr[ibest[0]-1];
lpivr1 = lpivr + 1;
lpivr2 = lpivr + nelim;

for (l = lpivr; l <= lpivr2; l++) {
 if (indr[l-1] == jbest[0]) break;
} // for (l = lpivr; l <= lpivr2; l++)

indr[l-1]     = indr[lpivr-1];
indr[lpivr-1] = jbest[0];

lpivc  = locc[jbest[0]-1];
lpivc1 = lpivc + 1;
lpivc2 = lpivc + melim;

for (l = lpivc; l <= lpivc2; l++) {
 if (indc[l-1] == ibest[0]) break;
} // for (l = lpivc; l <= lpivc2; l++) 

indc[l-1]     = indc[lpivc-1];
indc[lpivc-1] = ibest[0];
abest       = a[l-1];
a[l-1]        = a[lpivc-1];
a[lpivc-1]    = abest;

if ( keepLU ) {
 // relax
}
else {
 // Store just the diagonal of U, in natural order.
 //   a(ldiagU + nrowu) = abest ! This was in pivot order.
 a[ldiagU + jbest[0]-1] = abest;
}

// ==============================================================
// Delete pivot col from heap.
// Hk tells us where it is in the heap.
// ==============================================================
if ( TCP ) {
 kbest  = Hk[jbest[0]-1];
 Hlenin = Hlen[0];
 Hdelete( Ha, Hj, Hk, Hlenin, Hlen, n, kbest, h );
 hops[0]   = hops[0] + h[0];
} // if (TCP)

// ===============================================================
// Delete the pivot row from the column file
// and store it as the next row of  U.
// set  indr(lu) = 0     to initialize jfill ptrs on columns of D,
// indc(lu) = lenj  to save the original column lengths.
// ===============================================================
a[lu1-1]    = abest;
indr[lu1-1] = jbest[0];
indc[lu1-1] = nrowd;
lu [0]       = lu1;

diag      = Math.abs( abest );
Umax[0]      = Math.max(  Umax[0], diag );
DUmax[0]     = Math.max( DUmax[0], diag );
DUmin[0]     = Math.min( DUmin[0], diag );

for (lr = lpivr1; lr <= lpivr2; lr++) {
 lu[0]      = lu[0] + 1;
 j       = indr[lr-1];
 lenj    = lenc[j-1];
 lenc[j-1] = lenj - 1;
 lc1     = locc[j-1];
 last    = lc1 + lenc[j-1];

 for (l = lc1; l <= last; l++) {
    if (indc[l-1] == ibest[0]) break;
 } // for (l = lc1; l <= last; l++)

 a[lu[0]-1]      = a[l-1];
 indr[lu[0]-1]   = 0;
 indc[lu[0]-1]   = lenj;
 Umax[0]       = Math.max( Umax[0], Math.abs( a[lu[0]-1] ) );
 a[l-1]       = a[last-1];
 indc[l-1]    = indc[last-1];
 indc[last-1] = 0;       // Free entry
 //??? if (j == jlast) lcol[0] = lcol[0] - 1
} // for (lr = lpivr1; lr <= lpivr2; lr++)

// ===============================================================
// Delete the pivot column from the row file
// and store the nonzeros of the next column of  L.
// Set  indc(ll) = 0      to initialize markl(*) markers,
// indr(ll) = 0           to initialize ifill(*) row fill-in cntrs,
// indc(ls) = leni        to save the original row lengths,
// indr(ls) = iqloc(i)    to save parts of  iqloc(*),
// iqloc(i) = lsave - ls  to point to the nonzeros of  L
//          = -1, -2, -3, ... in mark(*).
// ===============================================================
indc[lsave-1] = ncold;
if (melim == 0) {
	seg3 = false;
}

if (seg3) {
ll     = ll1 - 1;
ls     = lsave;
abest  = one / abest;

for (lc = lpivc1; lc <= lpivc2; lc++) {
 ll       = ll + 1;
 ls       = ls + 1;
 i        = indc[lc-1];
 leni     = lenr[i-1];
 lenr[i-1]  = leni - 1;
 lr1      = locr[i-1];
 last     = lr1 + lenr[i-1];

 for (l = lr1; l <= last; l++) {
    if (indr[l-1] == jbest[0]) break;
 } // for (l = lr1; l <= last; l++)

 indr[l-1]    = indr[last-1];
 indr[last-1] = 0;       // Free entry
 //???  if (i == ilast) lrow = lrow - 1

 a[ll-1]      = - a[lc-1] * abest;
 Lij        = Math.abs( a[ll-1] );
 Lmax[0]       = Math.max( Lmax[0], Lij );
 //!!!! DEBUG
 // if (Lij > Ltol) then
 // write( *  ,*) ' Big Lij!!!', nrowu
 // write(nout,*) ' Big Lij!!!', nrowu
 // end if

 indc[ll-1]   = 0;
 indr[ll-1]   = 0;
 indc[ls-1]   = leni;
 indr[ls-1]   = iqloc[i-1];
 iqloc[i-1]   = lsave - ls;
} // for (lc = lpivc1; lc <= lpivc2; lc++)

// ===============================================================
// Do the Gaussian elimination.
// This involves adding a multiple of the pivot column
// to all other columns in the pivot row.
//
// Sometimes more than one call to lu1gau is needed to allow
// compression of the column file.
// lfirst  says which column the elimination should start with.
// minfre  is a bound on the storage needed for any one column.
// lu      points to off-diagonals of u.
// nfill   keeps track of pending fill-in in the row file.
// ===============================================================
if (nelim == 0) {
	seg4 = false;
}
if (seg4) {
lfirst[0] = lpivr1;
minfre = mleft + nspare;
lu[0]     = 1;
nfill[0]  = 0;

while (true) {
al = new double[melim];
markl = new int[melim];
au = new double[ncold];
ifill = new int[melim];
jfill = new int[ncold];
for (i = 0; i < melim; i++) {
	al[i] = a[ll1-1+i];
	markl[i] = indc[ll1-1+i];
	ifill[i] = indr[ll1-1+i];
}
for (i = 0; i < ncold; i++) {
	au[i] = a[lu1-1+i];
	jfill[i] = indr[lu1-1+i];
}
lu1gau( m     , melim , ncold , nspare, small ,
           lpivc1, lpivc2, lfirst, lpivr2, lfree , minfre,
           ilast , jlast , lrow  , lcol  , lu    , nfill ,
           a     , indc  , indr  ,  
           lenc  , lenr  , locc  , locr  , 
           iqloc , al, markl,  
           au, ifill, jfill);
for (i = 0; i < melim; i++) {
	indc[ll1-1+i] = markl[i];
	indr[ll1-1+i] = ifill[i];
}
for (i = 0; i < ncold; i++) {
	indr[lu1-1+i] = jfill[i];
}

if (lfirst[0] > 0) {

 // The elimination was interrupted.
 // Compress the column file and try again.
 // lfirst, lu and nfill have appropriate new values.

 lu1rec( n, true, luparm, lcol, lena, a, indc, lenc, locc );
 lfile  = lcol[0];
 jlast[0]  = indc[lcol[0]];
 lpivc  = locc[jbest[0]-1];
 lpivc1 = lpivc + 1;
 lpivc2 = lpivc + melim;
 nfree  = lfree - lcol[0];
 if (nfree < minfre) {
	 // Not enough space free after a compress.
	 // Set  minlen  to an estimate of the necessary value of  lena.
	 inform[0] = 7;
     minlen[0] = lena  +  lfile  +  2*(m + n);
     return;
 }
 continue;
} // if (lfirst[0 > 0)
else {
	break;
}
} // while (true)

// ===============================================================
// The column file has been fully updated.
// Deal with any pending fill-in in the row file.
// ===============================================================
if (nfill[0] > 0) {

 // Compress the row file if necessary.
 // lu1gau has set nfill to be the number of pending fill-ins
 // plus the current length of any rows that need to be moved.

 minfre = nfill[0];
 nfree  = lfree - lrow[0];
 if (nfree < minfre) {
    lu1rec( m, false, luparm, lrow, lena, a, indr, lenr, locr );
    lfile  = lrow[0];
    ilast[0]  = indr[lrow[0]];
    lpivr  = locr[ibest[0]-1];
    lpivr1 = lpivr + 1;
    lpivr2 = lpivr + nelim;
    nfree  = lfree - lrow[0];
    if (nfree < minfre) {
    	// Not enough space free after a compress.
    	// Set  minlen  to an estimate of the necessary value of  lena.
    	inform[0] = 7;
        minlen[0] = lena  +  lfile  +  2*(m + n);
        return;
    }
 } // if (nfree < minfre)

 // Move rows that have pending fill-in to end of the row file.
 // Then insert the fill-in.
 ifill = new int[melim];
 jfill = new int[ncold];
 for (i = 0; i < melim; i++) {
	 ifill[i] = indr[ll1-1+i];
 }
 for (i = 0; i < ncold; i++) {
	 jfill[i] =  indr[lu1-1+i];
 }
 lu1pen(m, melim , ncold , nspare, ilast,
              lpivc1, lpivc2, lpivr1, lpivr2, lrow ,
              lenc  , lenr  , locc  , locr  , 
              indc  , indr  , ifill, jfill);
} // if (nfill[0] > 0)
} // if (seg4)
seg4 = true;
} // if (seg3)
seg3 = true;

// ===============================================================
// Restore the saved values of  iqloc.
// Insert the correct indices for the col of L and the row of U.
// ===============================================================
lenr[ibest[0]-1] = 0;
lenc[jbest[0]-1] = 0;

ll          = ll1 - 1;
ls          = lsave;

for (lc  = lpivc1; lc <= lpivc2; lc++) {
 ll       = ll + 1;
 ls       = ls + 1;
 i        = indc[lc-1];
 iqloc[i-1] = indr[ls-1];
 indc[ll-1] = i;
 indr[ll-1] = ibest[0];
} // for (lc  = lpivc1; lc <= lpivc2; lc++)

lu[0]          = lu1 - 1;

for (lr  = lpivr; lr <= lpivr2; lr++) {
 lu[0]       = lu[0] + 1;
 indr[lu[0]-1] = indr[lr-1];
} // for (lr  = lpivr; lr <= lpivr2; lr++)

// ===============================================================
// Free the space occupied by the pivot row
// and update the column permutation.
// Then free the space occupied by the pivot column
// and update the row permutation.
//
// nzchng is found in both calls to lu1pq2, but we use it only
// after the second.
// ===============================================================
ind2 = new int[ncold];
lenold = new int[ncold];
for (i = 0; i < ncold; i++) {
	ind2[i] = indr[lpivr-1+i];
	lenold[i] = indc[lu1-1+i];
}
lu1pq2( ncold, nzchng,
           ind2, lenold, lenc, iqloc, q, iqinv );
for (i = 0; i < ncold; i++) {
	indr[lpivr-1+i] = ind2[i];
}

ind2 = new int[nrowd];
lenold = new int[nrowd];
for (i = 0; i < nrowd; i++) {
	ind2[i] = indc[lpivc-1+i];
	lenold[i] = indc[lsave-1+i];
}
lu1pq2( nrowd, nzchng,
           ind2, lenold, lenr, iploc, p, ipinv );
for (i = 0; i < nrowd; i++) {
	indc[lpivc-1+i] = ind2[i];
}

nzleft = nzleft + nzchng[0];

// ===============================================================
// lu1mxr resets Amaxr(i) in each modified row i.
// lu1mxc moves the largest aij to the top of each modified col j.
// 28 Jun 2002: Note that cols of L have an implicit diag of 1.0,
//              so lu1mxr is called with ll1, not ll1+1, whereas
//              lu1mxc is called with             lu1+1.
// ===============================================================
if (Utri && TPP) {
 // Relax -- we're not keeping big elements at the top yet.
}
else { // not (Utri && TPP)
 if (TRP  &&  melim > 0) {
    // Beware: The parts of p that we need are in indc(ll1:ll)
    mark = mark + 1;
    lu1mxr( mark, ll1, ll, m, n, lena, 
                 a, indc, lenc, locc, indr, lenr, locr,
                 indc, markc, markr, Amaxr );
               // ^^^^  Here are the p(k1:k2) needed by lu1mxr.
 } // if (TRP  &&  melim > 0)

 if (nelim > 0) {
    lu1mxc( lu1+1, lu[0], indr, a, indc, lenc, locc );

    if (TCP) { // Update modified columns in heap
       for (kk = lu1+1; kk <= lu[0]; kk++) {
          j    = indr[kk-1];
          k    = Hk[j-1];
          v    = Math.abs( a[locc[j-1]-1] ); // Biggest aij in column j
          Hchange( Ha, Hj, Hk, Hlen[0], n, k, v, j, h );
          hops[0] = hops[0] + h[0];
       } // for (kk = lu1+1; kk <= lu[0]; kk++)
    } // if (TCP)
 } // if (nelim > 0)
} // else not (Utri && TPP)

// ===============================================================
// Negate lengths of pivot row and column so they will be
// eliminated during compressions.
// ===============================================================
lenr[ibest[0]-1] = - ncold;
lenc[jbest[0]-1] = - nrowd;

// Test for fatal bug: row or column lists overwriting L and U.

if (lrow[0] > lsave) {
	// Fatal error.  This will never happen!
	// (Famous last words.)

	inform[0] = 8;
	return;
}
if (lcol[0] > lsave) {
	// Fatal error.  This will never happen!
	// (Famous last words.)

	inform[0] = 8;
	return;	
}

// Reset the file lengths if pivot row or col was at the end.

if (ibest[0] == ilast[0]) {
 lrow[0] = locr[ibest[0]-1];
}

if (jbest[0] == jlast[0]) {
 lcol[0] = locc[jbest[0]-1];
}
} // for (nrowu = 1; nrowu <= minmn; nrowu++)

// ------------------------------------------------------------------
// End of main loop.
// ------------------------------------------------------------------

// ------------------------------------------------------------------
//  Normal exit.
//  Move empty rows and cols to the end of p, q.
//  Then finish with a dense LU if necessary.
// ------------------------------------------------------------------
inform[0] = 0;
lu1pq3( m, lenr, p, ipinv, mrank );
lu1pq3( n, lenc, q, iqinv, nrank );
nrank[0]  = Math.min( mrank[0], nrank[0] );

if ( densLU ) {
	d = new double[lenD];
lu1ful( m     , n    , lena , lenD , lu1 , TPP,
           mleft , nleft, nrank[0], nrowu,
           lenL  , lenU , nsing,
           keepLU, small,
           a, d , indc , indr , p   , q,
           lenc  , lenr , locc , ipinv, locr );
for (i = 0; i < lenD; i++) {
	a[lD-1+i] = d[i];
}
//***     21 Dec 1994: Bug in next line.
//***     nrank  = nrank - nsing.  Changed to next line:
//***     nrank  = minmn - nsing

//***     26 Mar 2006: Previous line caused bug with m<n and nsing>0.
// Don't mess with nrank any more.  Let end of lu1fac handle it.
} // if (densLU)

minlen[0] = lenL[0]  +  lenU[0]  +  2*(m + n);
return;
} // lu1fad
    
    private void lu1ful(int m, int n, int lena, int lenD , int lu1 , boolean TPP,
            int mleft, int nleft, int nrank, int nrowu,
            int lenL[], int lenU[], int nsing[],
            boolean keepLU, double small,
            double a[], double d[], int indc[], int indr[], int p[], int q[],
            int lenc[], int lenr[], int locc[], int ipinv[], int ipvt[]) {

//logical,       intent(in)    :: TPP, keepLU
//integer(ip),   intent(in)    :: m, n, lena, lenD, lu1,   &
//                           mleft, nleft, nrank, nrowu
//integer(ip),   intent(inout) :: lenL, lenU
//integer(ip),   intent(out)   :: nsing  ! not used outside
//integer(ip),   intent(in)    :: locc(n)
//integer(ip),   intent(inout) :: indc(lena), indr(lena), p(m), q(n), &
//                           lenc(n)   , lenr(m)
//integer(ip),   intent(out)   :: ipvt(m), ipinv(m)   ! workspace
//real(rp),      intent(in)    :: small
//real(rp),      intent(inout) :: a(lena)
//real(rp),      intent(out)   :: d(lenD)

// ------------------------------------------------------------------
// lu1ful computes a dense (full) LU factorization of the
// mleft by nleft matrix that remains to be factored at the
// beginning of the nrowu-th pass through the main loop of lu1fad.

// 02 May 1989: First version.
// 05 Feb 1994: Column interchanges added to lu1DPP.
// 08 Feb 1994: ipinv reconstructed, since lu1pq3 may alter p.

// 10 Jan 2010: First f90 version.
// 12 Dec 2011: Declare intent and local variables.
// ------------------------------------------------------------------

int i, ibest, ipbase, j, jbest, k, l, l1, l2,
    la, lc, lc1, lc2, ld, ldbase, ldiagU,
    lkk, lkn, ll, lq, lu, nrowd, ncold;
double ai, aj;
final double zero = 0.0;
int q2[];
double arr[][];
int index;

// ------------------------------------------------------------------
// If lu1pq3 moved any empty rows, reset ipinv = inverse of p.
// ------------------------------------------------------------------
if (nrank < m) {
for (l = 1; l <= m; l++) {
 i        = p[l-1];
 ipinv[i-1] = l;
} // for (l = 1; l <= m; l++)
} // if (nrank < m)

// ------------------------------------------------------------------
// Copy the remaining matrix into the dense matrix D.
// ------------------------------------------------------------------
for (i = 0; i < lenD; i++) {
	d[i] = zero;
}

ipbase = nrowu - 1;
ldbase = 1 - nrowu;

for (lq = nrowu; lq <= n; lq++) {
j      = q[lq-1];
lc1    = locc[j-1];
lc2    = lc1 + lenc[j-1] - 1;

for  (lc = lc1; lc <= lc2; lc++) {
 i      = indc[lc-1];
 ld     = ldbase + ipinv[i-1];
 d[ld-1]  = a[lc-1];
} // for  (lc = lc1; lc <= lc2; lc++)

ldbase = ldbase + mleft;
} // for (lq = nrowu; lq <= n; lq++)

// ------------------------------------------------------------------
// Call our favorite dense LU factorizer.
// ------------------------------------------------------------------
arr = new double[mleft][nleft];
index = 0;
for (j = 0; j < nleft; j++) {
	for (i = 0; i < mleft; i++) {
		arr[i][j] = d[index++];
	}
}
q2 = new int[nleft];
for (i = 0; i < nleft; i++) {
	q2[i] = q[nrowu-1+i];
}
if ( TPP ) {
lu1DPP( arr, mleft, mleft, nleft, small, nsing, ipvt, q2);
}
else {
lu1DCP( arr, mleft, mleft, nleft, small, nsing, ipvt, q2);
}
index = 0;
for (j = 0; j < nleft; j++) {
	for (i = 0; i < mleft; i++) {
		d[index++] = arr[i][j];
	}
}
for (i = 0; i < nleft; i++) {
	q[nrowu-1+i] = q2[i];
}

// ------------------------------------------------------------------
// Move D to the beginning of A,
// and pack L and U at the top of a, indc, indr.
// In the process, apply the row permutation to p.
// lkk points to the diagonal of U.
// ------------------------------------------------------------------
for (i = 0; i < lenD; i++) {
	a[i] = d[i];
}

ldiagU = lena - n;
lkk    = 1;
lkn    = lenD - mleft + 1;
lu     = lu1;

for (k  = 1; k <= Math.min( mleft, nleft ); k++) {
l1 = ipbase + k;
l2 = ipbase + ipvt[k-1];
if (l1 != l2) {
 i      = p[l1-1];
 p[l1-1]  = p[l2-1];
 p[l2-1]  = i;
} // if (l1 != l2)
ibest  = p[l1-1];
jbest  = q[l1-1];

if ( keepLU ) {
 // ===========================================================
 // Pack the next column of L.
 // ===========================================================
 la     = lkk;
 ll     = lu;
 nrowd  = 1;

 for (i  = k + 1; i <= mleft; i++) {
    la = la + 1;
    ai = a[la-1];
    if (Math.abs( ai ) > small) {
       nrowd    = nrowd + 1;
       ll       = ll    - 1;
       a[ll-1]    = ai;
       indc[ll-1] = p[ ipbase + i -1];
       indr[ll-1] = ibest;
    } // if (Math.abs( ai ) > small) 
 } // for (i  = k + 1; i <= mleft; i++)

 // ===========================================================
 // Pack the next row of U.
 // We go backwards through the row of D
 // so the diagonal ends up at the front of the row of  U.
 // Beware -- the diagonal may be zero.
 // ===========================================================
 la     = lkn + mleft;
 lu     = ll;
 ncold  = 0;

 for (j = nleft; j >= k; j--) {
    la     = la - mleft;
    aj     = a[la-1];
    if (Math.abs( aj ) > small ||  j == k) {
       ncold    = ncold + 1;
       lu       = lu    - 1;
       a[lu-1]    = aj;
       indr[lu-1] = q[ipbase + j-1];
    } // if (Math.abs( aj ) > small ||  j == k)
 } // for (j = nleft; j >= k; j--)

 lenr[ibest-1] = - ncold;
 lenc[jbest] = - nrowd;
 lenL[0]        =   lenL[0] + nrowd - 1;
 lenU[0]        =   lenU[0] + ncold;
 lkn         =   lkn  + 1;
} // if (keepLU)
else {
 // ===========================================================
 // Store just the diagonal of U, in natural order.
 // ===========================================================
 a[ldiagU + jbest - 1] = a[lkk-1];
}

lkk    = lkk  + mleft + 1;
} // for (k  = 1; k <= Math.min( mleft, nleft ); k++)

} // lu1ful
    
    private void lu1DCP(double a[][], int lda, int m, int n, double small, int nsing[], int ipvt[], int q[]) {

    //integer(ip),   intent(in)    :: lda, m, n
    //integer(ip),   intent(out)   :: nsing   ! not used outside
    //integer(ip),   intent(out)   :: ipvt(m)
    //integer(ip),   intent(inout) :: q(n)
    //real(rp),      intent(in)    :: small
    //real(rp),      intent(inout) :: a(lda,n)

    // ------------------------------------------------------------------
    // lu1DCP factors a dense m x n matrix A by Gaussian elimination,
    // using Complete Pivoting (row and column interchanges) for
    // stability.
    // This version also uses column interchanges if all elements in a
    // pivot column are smaller than (or equal to) "small".  Such columns
    // are changed to zero and permuted to the right-hand end.
    
    // As in LINPACK's dgefa, ipvt(*) keeps track of pivot rows.
    // Rows of U are interchanged, but we don't have to physically
    // permute rows of L.  In contrast, column interchanges are applied
    // directly to the columns of both L and U, and to the column
    // permutation vector q(*).
    
    // 01 May 2002: First dense Complete Pivoting, derived from lu1DPP.
    // 07 May 2002: Another break needed at end of first loop.
    // 26 Mar 2006: Cosmetic mods while looking for "nsing" bug when m<n.
    //              nsing redefined (see below).
    //              Changed to implicit none.
    
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // 03 Feb 2012: a(kp1:m,j) = t*a(kp1:m,k) + a(kp1:m,j)  needs the last :m
    // ------------------------------------------------------------------
    
    // On entry:
    // a       Array holding the matrix A to be factored.
    // lda     The leading dimension of the array  a.
    // m       The number of rows    in  A.
    // n       The number of columns in  A.
    // small   A drop tolerance.  Must be zero or positive.
    
    // On exit:
    // a       An upper triangular matrix and the multipliers
    //         that were used to obtain it.
    //         The factorization can be written A = L*U, where
    //         L is a product of permutation and unit lower
    //         triangular matrices and U is upper triangular.
    // nsing   Number of singularities detected.
    
    // 26 Mar 2006: nsing redefined to be more meaningful.
    //              Users may define rankU = n - nsing and regard
    //              U as upper-trapezoidal, with the first rankU columns
    //              being triangular and the rest trapezoidal.
    //              It would be better to return rankU, but we still
    //              return nsing for compatibility (even though lu1fad
    //              no longer uses it).
    // ipvt    Records the pivot rows.
    // q       A vector to which column interchanges are applied.
    // ------------------------------------------------------------------

  // integer(ip), external  :: idamax
    double aijmax, ajmax, t;
    int i, imax, j, jlast, jmax, jnew,
        k, kp1, l, last, lencol, rankU;
    final double zero = 0.0;
    final double one = 1.0;
    double vec[];

    rankU  = 0;
    lencol = m + 1;
    last   = n;

    // -----------------------------------------------------------------
    // Start of elimination loop.
    // -----------------------------------------------------------------
    for (k = 1; k <= n; k++) {
       kp1    = k + 1;
       lencol = lencol - 1;

       // Find the biggest aij in row imax and column jmax.

       aijmax = zero;
       imax   = k;
       jmax   = k;
       jlast  = last;

       l2: for(j = k; j <= jlast; j++) {
    	  while (true) {
          vec = new double[m-k+1];
          for (i = 0; i < m-k+1; i++) {
        	  vec[i] = a[k-1+i][j-1];
          }
          l      = jdamax( lencol, vec, 1 ) + k - 1;
          ajmax  = Math.abs(a[l-1][j-1]);

          if (ajmax <= small) {
             // ========================================================
             // Do column interchange, changing old column to zero.
             // Reduce "last" and try again with same j.
             // ========================================================
             jnew    = q[last-1];
             q[last-1] = q[j-1];
             q[j-1]    = jnew;

             for (i = 1; i <= k - 1; i++) {
                t         = a[i-1][last-1];
                a[i-1][last-1] = a[i-1][j-1];
                a[i-1][j-1]    = t;
             } // for (i = 1; i <= k - 1; i++)

             for (i = k; i <= m; i++) {
                t         = a[i-1][last-1];
                a[i-1][last-1] = zero;
                a[i-1][j-1]    = t;
             } // for (i = k; i <= m; i++)

             last   = last - 1;
             if (j <= last) {
            	 continue; // repeat
             }
             else {
                 break l2;
             }
    	  } // if (ajmax <= small)
    	  else {
    		  break;
    	  }
    	  } // while (true)

          // Check if this column has biggest aij so far.

          if (aijmax < ajmax) {
             aijmax  =   ajmax;
             imax    =   l;
             jmax    =   j;
          } // if (aijmax < ajmax)

          if (j >= last) {
        	  break;
          }
       } // for(j = k; j <= jlast; j++)

       ipvt[k-1] = imax;

       if (jmax != k) {   // Do column interchange (k and jmax).
          jnew    = q[jmax-1];
          q[jmax-1] = q[k-1];
          q[k-1]    = jnew;

          for (i = 1; i <= m; i++) {
             t         = a[i-1][jmax-1];
             a[i-1][jmax-1] = a[i-1][k-1];
             a[i-1][k-1]    = t;
          } // for (i = 1; i <= m; i++)
       } // if (jmax != k)

       if (k < m) {       // Do row interchange if necessary.
          t         = a[imax-1][k-1];
          if (imax != k) {
             a[imax-1][k-1] = a[k-1][k-1];
             a[k-1][k-1]    = t;
          } // if (imax != k)

          // ===========================================================
          // Compute multipliers.
          // Do row elimination with column indexing.
          // ===========================================================
          t      = - one / t;
          // call dscal ( m-k, t, a(kp1,k), 1 )
          for (i = kp1-1; i < m; i++) {
        	  a[i][k-1] = t * a[i][k-1];
          }

          for (j = kp1; j <= last; j++) {
             t         = a[imax-1][j-1];
             if (imax != k) {
                a[imax-1][j-1] = a[k-1][j-1];
                a[k-1][j-1]    = t;
             } // if (imax != k)
             // call daxpy ( m-k, t, a(kp1,k), 1, a(kp1,j), 1 )
             for (i = kp1-1; i < m; i++) {
            	 a[i][j-1] = t * a[i][k-1] + a[i][j-1];
             }
          } // for (j = kp1; j <= last; j++) 
       } // if (k < m)
       else {
          break;
       }

       if (k >= last) {
    	   break;
       }
    } // for (k = 1; k <= n; k++)

    // Set ipvt(*) for singular rows.

    for (k = last + 1; k <= m; k++) {
       ipvt[k-1] = k;
    } // for (k = last + 1; k <= m; k++) 

    nsing[0]  = n - rankU;

} // lu1DCP


    private void lu1DPP(double a[][], int lda, int m, int n, double small, int nsing[], int ipvt[], int q[]) {

      //integer(ip),   intent(in)    :: lda, m, n
      //integer(ip),   intent(out)   :: nsing   ! not used outside
      //integer(ip),   intent(out)   :: ipvt(m)
      //integer(ip),   intent(inout) :: q(n)
      //real(rp),      intent(in)    :: small
      //real(rp),      intent(inout) :: a(lda,n)

      // ------------------------------------------------------------------
      // lu1DPP factors a dense m x n matrix A by Gaussian elimination,
      // using row interchanges for stability, as in dgefa from LINPACK.
      // This version also uses column interchanges if all elements in a
      // pivot column are smaller than (or equal to) "small".  Such columns
      // are changed to zero and permuted to the right-hand end.
      
      // As in LINPACK, ipvt(*) keeps track of pivot rows.
      // Rows of U are interchanged, but we don't have to physically
      // permute rows of L.  In contrast, column interchanges are applied
      // directly to the columns of both L and U, and to the column
      // permutation vector q(*).
      
      // 02 May 1989: First version derived from dgefa
      //              in LINPACK (version dated 08/14/78).
      // 05 Feb 1994: Generalized to treat rectangular matrices
      //              and use column interchanges when necessary.
      //              ipvt is retained, but column permutations are applied
      //              directly to q(*).
      // 21 Dec 1994: Bug found via example from Steve Dirkse.
      //              Loop 100 added to set ipvt(*) for singular rows.
      // 26 Mar 2006: nsing redefined (see below).
      //              Changed to implicit none.
      
      // 10 Jan 2010: First f90 version.  Need to do more f90-ing.
      // 12 Dec 2011: Declare intent and local variables.
      // 03 Feb 2012: a is intent(inout), not (out).
      //              a(kp1:m,j) = t*a(kp1:m,k) + a(kp1:m,j)  needs the last :m
      // ------------------------------------------------------------------
      
      // On entry:
      
      // a       Array holding the matrix A to be factored.
      // lda     The leading dimension of the array  a.
      // m       The number of rows    in  A.
      // n       The number of columns in  A.
      // small   A drop tolerance.  Must be zero or positive.
      
      // On exit:
      
      // a       An upper triangular matrix and the multipliers
      //         which were used to obtain it.
      //         The factorization can be written  A = L*U  where
      // L       is a product of permutation and unit lower
      //         triangular matrices and  U  is upper triangular.
      // nsing   Number of singularities detected.
      // 26 Mar 2006: nsing redefined to be more meaningful.
      //              Users may define rankU = n - nsing and regard
      //              U as upper-trapezoidal, with the first rankU columns
      //              being triangular and the rest trapezoidal.
      //              It would be better to return rankU, but we still
      //              return nsing for compatibility (even though lu1fad
      //              no longer uses it).
      // ipvt    Records the pivot rows.
      // q       A vector to which column interchanges are applied.
      // ------------------------------------------------------------------

    // integer(ip), external  :: idamax
      int i, j, k, kp1, l, last, lencol, rankU;
      double t;
      final double zero = 0.0;
      final double one = 1.0;
      double vec[];

      rankU  = 0;
      k      = 1;
      last   = n;

      // ------------------------------------------------------------------
      // Start of elimination loop.
      // ------------------------------------------------------------------
      while (true) {
      kp1    = k + 1;
      lencol = m - k + 1;

      // Find l, the pivot row.
      vec = new double[m - k + 1];
      for (i = 0; i < m - k + 1; i++) {
    	  vec[i] = a[i + k - 1][k-1];
      }
      l       = jdamax( lencol, vec, 1 ) + k - 1;
      ipvt[k-1] = l;

      if (Math.abs( a[l-1][k-1] ) <= small) {
         // ==============================================================
         // Do column interchange, changing old pivot column to zero.
         // Reduce "last" and try again with same k.
         // ==============================================================
         j       = q[last-1];
         q[last-1] = q[k-1];
         q[k-1]    = j;

         for (i = 1; i <= k - 1; i++) {
            t         = a[i-1][last-1];
            a[i-1][last-1] = a[i-1][k-1];
            a[i-1][k-1]    = t;
         } // for (i = 1; i <= k - 1; i++)

         for (i = k; i <= m; i++) {
            t         = a[i-1][last-1];
            a[i-1][last-1] = zero;
            a[i-1][k-1]    = t;
         } // for (i = k; i <= m; i++)

         last     = last - 1;
         if (k <= last) {
        	 continue;
         }
         else {
        	 break;
         }
      } // if (Math.abs( a[l-1][k-1] ) <= small)
      else { // Math.abs( a[l-1][k-1] ) > small
         rankU  = rankU + 1;
         if (k < m) {
            // ===========================================================
            // Do row interchange if necessary.
            // ===========================================================
            if (l != k) {
               t      = a[l-1][k-1];
               a[l-1][k-1] = a[k-1][k-1];
               a[k-1][k-1] = t;
            } // if (l != k)

            // ===========================================================
            // Compute multipliers.
            // Do row elimination with column indexing.
            // ===========================================================
            t = - one / a[k-1][k-1];
            // call dscal ( m-k, t, a(kp1,k), 1 )
            for (i = kp1-1; i < m; i++) {
            	a[i][k-1] = t * a[i][k-1];
            }

            for (j = kp1; j <= last; j++) {
               t    = a[l-1][j-1];
               if (l != k) {
                  a[l-1][j-1] = a[k-1][j-1];
                  a[k-1][j-1] = t;
               } // if (l != k)
               // call daxpy ( m-k, t, a(kp1,k), 1, a(kp1,j), 1 )
               for (i = kp1-1; i < m; i++) {
            	   a[i][j-1] = t * a[i][k-1] + a[i][j-1];
               }
            } // for (j = kp1; j <= last; j++)

            k = k + 1;
            if (k <= last) {
            	continue;
            }
            else {
            	break;
            }
         } // if (k < m)
         else {
        	 break;
         }
      } // Math.abs( a[l-1][k-1] ) > small
      } // while (true)

      // Set ipvt(*) for singular rows.

      for (k = last + 1; k <= m; k++) {
         ipvt[k-1] = k;
      }

      nsing[0]  = n - rankU;

} // lu1DPP
    
    private int jdamax(int n, double x[], int incx) {
    	//result(iAmax)
    

    //integer(ip), intent(in)    :: n
    //integer,     intent(in)    :: incx   ! default integer size
    //real(rp),    intent(in)    :: x(:)
    int iAmax;

    // ===========================================================================
    // jdamax does the same as idamax in most cases.
    // jdamax > 0 if x contains normal values.
    // jdamax = 0 if n = 0.
    // jdamax < 0 means x(-jdamax) contains the first NaN or Inf.
    //
    // 29 Jul 2003: First version of jdamax implemented for s5setx.
    // 29 Jul 2003: Current version of jdamax
    // 15 Mar 2008: First f90 version.
    // ===========================================================================

    //intrinsic           :: huge
    int i, ix, kmax;
    double dmax, xi;
    final double realmax = Double.MAX_VALUE;
    final double zero    = 0.0;

    if (n < 1) {
       iAmax = 0;
       return iAmax;
    } // if (n < 1)

    dmax  = zero;
    ix    = 1;
    kmax  = 1;

    for (i = 1; i <= n; i++) {
       xi = Math.abs( x[ix-1] );
       if (xi <= realmax)  {  // false if xi = Nan or Inf
          if (dmax < xi) {
             dmax   = xi;
             kmax   = ix;
          } // if (dmax < xi)
       } // if (xi <= realmax)
       else {
          iAmax = -ix;
          return iAmax;
       }
       ix = ix + incx;
    } // for (i = 1; i <= n; i++)

    iAmax = kmax;
    return iAmax;
    } // jdamax



    
    private void lu1gau(int m, int melim, int ncold, int nspare, double small,
            int lpivc1, int lpivc2, int lfirst[], int lpivr2, int lfree, int minfre,
            int ilast[], int jlast[], int lrow[], int lcol[], int lu[], int nfill[],
            double a[], int indc[], int indr[],
            int lenc[], int lenr[], int locc[], int locr[],
            int mark[], double al[], int markl[],
            double au[], int ifill[], int jfill[]) {

//integer(ip),   intent(in)    :: m, melim, ncold, nspare,          &
//                           lpivc1, lpivc2, lpivr2, lfree, minfre
//integer(ip),   intent(inout) :: ilast, jlast, lfirst, lrow, lcol, lu, nfill
//real(rp),      intent(in)    :: small
//real(rp),      intent(in)    :: al(melim), au(ncold)
//real(rp),      intent(inout) :: a(*)
//integer(ip),   intent(in)    :: locr(*), mark(*)
//integer(ip),   intent(inout) :: locc(*), indc(*), indr(*), lenc(*), lenr(*), &
//                           markl(melim), ifill(melim), jfill(ncold)

// ------------------------------------------------------------------
// lu1gau does most of the work for each step of
// Gaussian elimination.
// A multiple of the pivot column is added to each other column j
// in the pivot row.  The column list is fully updated.
// The row list is updated if there is room, but some fill-ins may
// remain, as indicated by ifill and jfill.

// Input:
// ilast    is the row    at the end of the row    list.
// jlast    is the column at the end of the column list.
// lfirst   is the first column to be processed.
// lu + 1   is the corresponding element of U in au(*).
// nfill    keeps track of pending fill-in.
// a(*)     contains the nonzeros for each column j.
// indc(*)  contains the row indices for each column j.
// al(*)    contains the new column of L.  A multiple of it is
//          used to modify each column.
// mark(*)  has been set to -1, -2, -3, ... in the rows
//          corresponding to nonzero 1, 2, 3, ... of the col of L.
// au(*)    contains the new row of U.  Each nonzero gives the
//          required multiple of the column of L.

// Workspace:
// markl(*) marks the nonzeros of L actually used.
//          (A different mark, namely j, is used for each column.)

// Output:
// ilast     New last row    in the row    list.
// jlast     New last column in the column list.
// lfirst    = 0 if all columns were completed,
//           > 0 otherwise.
// lu        returns the position of the last nonzero of U
//           actually used, in case we come back in again.
// nfill     keeps track of the total extra space needed in the
//           row file.
// ifill(ll) counts pending fill-in for rows involved in the new
//           column of L.
// jfill(lu) marks the first pending fill-in stored in columns
//           involved in the new row of U.

// 16 Apr 1989: First version of lu1gau.
// 23 Apr 1989: lfirst, lu, nfill are now input and output
//              to allow re-entry if elimination is interrupted.
// 23 Mar 2001: Introduced ilast, jlast.
// 27 Mar 2001: Allow fill-in "in situ" if there is already room
//              up to but NOT INCLUDING the end of the
//              row or column file.
//              Seems safe way to avoid overwriting empty rows/cols
//              at the end.  (May not be needed though, now that we
//              have ilast and jlast.)
//
// 10 Jan 2010: First f90 version.
// 28 Feb 2010: Declare intent and local variables.
// ------------------------------------------------------------------

boolean atend;
int i, j, k, l, l1, l2, last, lc, lc1, lc2,
    leni, lenj, ll, lr, lr1, lrep,
    ndone, ndrop, nfree;
double aij, uj;
boolean seg1 = true;
boolean seg2 = true;
boolean seg3 = true;
boolean seg4 = true;
boolean seg5 = true;
boolean seg6 = true;
boolean seg7 = true;

for (lr = lfirst[0]; lr <= lpivr2; lr++) {
j      = indr[lr-1];
lenj   = lenc[j-1];
nfree  = lfree - lcol[0];
if (nfree < minfre) {
	// Interruption.  We have to come back in after the
	// column file is compressed.  Give lfirst a new value.
	// lu and nfill will retain their current values.
	lfirst[0] = lr;
    return;
}

// ---------------------------------------------------------------
// Inner loop to modify existing nonzeros in column  j.
// The "do l = lc1, lc2" loop performs most of the arithmetic
// involved in the whole LU factorization.
// ndone  counts how many multipliers were used.
// ndrop  counts how many modified nonzeros are negligibly small.
// ---------------------------------------------------------------
lu[0]     = lu[0] + 1;
uj     = au[lu[0]-1];
lc1    = locc[j-1];
lc2    = lc1 + lenj - 1;
atend  = j == jlast[0];
ndone  = 0;
if (lenj == 0) {
	seg1 = false;
}

if (seg1) {
ndrop  = 0;

for (l = lc1; l <= lc2; l++) {
 i        =   indc[l-1];
 ll       = - mark[i-1];
 if (ll > 0) {
    ndone     = ndone + 1;
    markl[ll-1] = j;
    a[l-1]      = a[l-1]  +  al[ll-1] * uj;
    if (Math.abs( a[l-1] ) <= small) {
       ndrop  = ndrop + 1;
    }
 } // if (ll > 0)
} // for (l = lc1; l <= lc2; l++)

// ---------------------------------------------------------------
// Remove any negligible modified nonzeros from both
// the column file and the row file.
// ---------------------------------------------------------------
if (ndrop == 0) {
	seg2 = false;
}
if (seg2) {
k      = lc1;

for (l = lc1; l <= lc2; l++) {
 i        = indc[l-1];
 if (Math.abs( a[l-1] ) > small) {
    a[k-1]     = a[l-1];
    indc[k-1]  = i;
    k        = k + 1;
    continue;
 } // if (Math.abs( a[l-1] ) > small)

 // Delete the nonzero from the row file.

 lenj     = lenj    - 1;
 lenr[i-1]  = lenr[i-1] - 1;
 lr1      = locr[i-1];
 last     = lr1 + lenr[i-1];

 for (lrep = lr1; lrep <= last; lrep++) {
    if (indr[lrep-1] == j) break;
 } // for (lrep = lr1; lrep <= last; lrep++)

 indr[lrep-1] = indr[last-1];
 indr[last-1] = 0;
 if (i == ilast[0]) lrow[0] = lrow[0] - 1;
} // for (l = lc1; l <= lc2; l++)

// Free the deleted elements from the column file.

for (l = k; l <= lc2; l++) {
 indc[l-1] = 0;
} // for (l = k; l <= lc2; l++)
if (atend) lcol[0] = k - 1;
} // if (seg2)
seg2 = true;
} // if (seg1)
seg1 = true;

// ---------------------------------------------------------------
// Deal with the fill-in in column j.
// ---------------------------------------------------------------
if (ndone == melim) {
	seg3 = false;
}

if (seg3) {
// See if column j already has room for the fill-in.

if (atend) {
	seg4 = false;
}
if (seg4) {
last   = lc1  + lenj - 1;
l1     = last + 1;
l2     = last + (melim - ndone);
// 27 Mar 2001: Be sure it's not at or past end of the col file.
if (l2 >= lcol[0]) {
	seg5 = false;
}

if (seg5) {
for (l = l1; l <= l2; l++) {
 if (indc[l-1] != 0) {
	 seg6 = false;
	 break;
 }
} // for (l = l1; l <= l2; l++)
if (seg6) {
    seg7 = false;
} // if (seg6)
seg6 = true;
} // if (seg5)
seg5 = true;

if (seg7) {
// We must move column j to the end of the column file.
// First, leave some spare room at the end of the
// current last column.

for (l = lcol[0] + 1; l <= lcol[0] + nspare; l++) {
 lcol[0]    = l;
 indc[l-1] = 0;     // Spare space is free.
} // for (l = lcol[0] + 1; l <= lcol[0] + nspare; l++)

atend   = true;
jlast[0]   = j;
l1      = lc1;
lc1     = lcol[0] + 1;
locc[j-1] = lc1;

for (l = l1; l <= last; l++) {
 lcol[0]       = lcol[0] + 1;
 a[lcol[0]-1]    = a[l-1];
 indc[lcol[0]-1] = indc[l-1];
 indc[l-1]    = 0;      // Free space.
} // for (l = l1; l <= last; l++)
} // if (seg7)
seg7 = true;
} // if (seg4)
seg4 = true;

// ---------------------------------------------------------------
// Inner loop for the fill-in in column j.
// This is usually not very expensive.
// ---------------------------------------------------------------
last   = lc1 + lenj - 1;
ll     = 0;

for (lc = lpivc1; lc <= lpivc2; lc++) {
 ll         = ll + 1;
 if (markl[ll-1] ==  j  ) continue;
 aij        = al[ll-1]*uj;
 if (Math.abs(aij) <= small) continue;
 lenj       = lenj + 1;
 last       = last + 1;
 a[last-1]    = aij;
 i          = indc[lc-1];
 indc[last-1] = i;
 leni       = lenr[i-1];

 // Add 1 fill-in to row i if there is already room.
 // 27 Mar 2001: Be sure it's not at or past the end
 // of the row file.

 l      = locr[i-1] + leni;
 if (l < lrow[0] &&  indr[l-1] <= 0) {
    indr[l-1] = j;
    lenr[i-1] = leni + 1;
 }
 else {

    // Row i does not have room for the fill-in.
    // Increment ifill(ll) to count how often this has
    // happened to row i.  Also, add m to the row index
    // indc(last) in column j to mark it as a fill-in that is
    // still pending.

    // If this is the first pending fill-in for row i,
    // nfill includes the current length of row i
    // (since the whole row has to be moved later).

    // If this is the first pending fill-in for column j,
    // jfill(lu) records the current length of column j
    // (to shorten the search for pending fill-ins later).

    if (ifill[ll-1] == 0) nfill[0]     = nfill[0] + leni + nspare;
    if (jfill[lu[0]-1] == 0) jfill[lu[0]-1] = lenj;
    nfill[0]      = nfill[0]     + 1;
    ifill[ll-1]  = ifill[ll-1] + 1;
    indc[last-1] = m + i;
 } // else
} // for (lc = lpivc1; lc <= lpivc2; lc++)

if ( atend ) lcol[0] = last;
} // if (seg3)
seg3 = true;

// End loop for column  j.  Store its final length.

lenc[j-1] = lenj;
} // for (lr = lfirst[0; lr <= lpivr2; lr++)

// Successful completion.

lfirst[0] = 0;
return;
} // lu1gau

    
    private void lu1mar(int m, int n, int lena, int maxmn,
            boolean TCP, double aijtol,double Ltol, int maxcol, int maxrow,
            int ibest[], int jbest[], int mbest[],
            double a[], int indc[], int indr[], int p[], int q[],
            int lenc[], int lenr[], int locc[], int locr[], int iploc[], int iqloc[]) {

//integer(ip),   intent(in)    :: m, n, lena, maxmn, maxcol, maxrow
//integer(ip),   intent(out)   :: ibest, jbest, mbest
//logical,       intent(in)    :: TCP
//real(rp),      intent(in)    :: aijtol, Ltol, a(lena)
//integer(ip),   intent(in)    :: indc(lena), indr(lena), p(m)    , q(n)    , &
//                           lenc(n)   , lenr(m)   , iploc(n), iqloc(m), &
//                           locc(n)   , locr(m)

// ------------------------------------------------------------------
// lu1mar  uses a Markowitz criterion to select a pivot element
// for the next stage of a sparse LU factorization,
// subject to a Threshold Partial Pivoting stability criterion (TPP)
// that bounds the elements of L.

// 00 Jan 1986: Version documented in LUSOL paper:
//              Gill, Murray, Saunders and Wright (1987),
//              "Maintaining LU factors of a general sparse matrix",
//              Linear algebra and its applications 88/89, 239-270.

// 02 Feb 1989: Following Suhl and Aittoniemi (1987), the largest
//              element in each column is now kept at the start of
//              the column, i.e. in position locc(j) of a and indc.
//              This should speed up the Markowitz searches.

// 26 Apr 1989: Both columns and rows searched during spars1 phase.
//              Only columns searched during spars2 phase.
//              maxtie replaced by maxcol and maxrow.
// 05 Nov 1993: Initializing  "mbest = m * n"  wasn't big enough when
//              m = 10, n = 3, and last column had 7 nonzeros.
// 09 Feb 1994: Realised that "mbest = maxmn * maxmn" might overflow.
//              Changed to    "mbest = maxmn * 1000".
// 27 Apr 2000: On large example from Todd Munson,
//              that allowed  "if (mbest .le. nz1**2) go to 900"
//              to exit before any pivot had been found.
//              Introduced kbest = mbest / nz1.
//              Most pivots can be rejected with no integer(ip) multiply.
//              True merit is evaluated only if it's as good as the
//              best so far (or better).  There should be no danger
//              of integer(ip) overflow unless A is incredibly
//              large and dense.

// 10 Sep 2000  TCP, aijtol added for Threshold Complete Pivoting.

// 10 Jan 2010: First f90 version.
// 12 Dec 2011: Declare intent.
// ------------------------------------------------------------------

int i, j, kbest, lc, lc1, lc2, len1,
    lp, lp1, lp2, lq, lq1, lq2, lr, lr1, lr2,
    merit, ncol, nrow, nz, nz1;
double abest, aij, amax, cmax, lbest;
final double zero = 0.0;
final double one = 1.0;
final double gamma = 2.0;
boolean seg1 = true;
boolean seg2 = true;
boolean seg3 = true;
boolean seg4 = true;

// gamma  is "gamma" in the tie-breaking rule TB4 in the LUSOL paper.

// ------------------------------------------------------------------
// Search cols of length nz = 1, then rows of length nz = 1,
// then   cols of length nz = 2, then rows of length nz = 2, etc.
// ------------------------------------------------------------------
abest  = zero;
lbest  = zero;
ibest[0]  = 0;
kbest  = maxmn + 1;
mbest[0]  = -1;
ncol   = 0;
nrow   = 0;
nz1    = 0;

NZS: for (nz = 1; nz <= maxmn; nz++) {
// nz1    = nz - 1
// if (mbest .le. nz1**2) go to 900
if (kbest <= nz1) {
	break NZS;
}
if (ibest[0] >  0  ) {
 if (ncol >= maxcol) {
	 seg1 = false;
	 seg2 = false;
 }
} // if (ibest[0] > 0)
if (seg1) {
if (nz    >  m  ) {
	seg2 = false;
}
} // if (seg1)
seg1 = true;
if (seg2) {
// ---------------------------------------------------------------
// Search the set of columns of length  nz.
// ---------------------------------------------------------------
lq1    = iqloc[nz-1];
lq2    = n;
if (nz < m) lq2 = iqloc[nz] - 1;

Cols:  for (lq = lq1; lq <= lq2; lq++) {
 ncol   = ncol + 1;
 j      = q[lq-1];
 lc1    = locc[j-1];
 lc2    = lc1 + nz1;
 amax   = Math.abs( a[lc1-1] );

 // Test all aijs in this column.
 // amax is the largest element (the first in the column).
 // cmax is the largest multiplier if aij becomes pivot.

 if ( TCP ) {
    if (amax < aijtol) {
    	continue Cols; // Nothing in whole column
    }
 } // if (TCP)

Colj: for (lc = lc1; lc <= lc2; lc++) {
    i      = indc[lc-1];
    len1   = lenr[i-1] - 1;
    // merit  = nz1 * len1
  // if (merit > mbest) cycle
    if (len1  > kbest) continue Colj;

    // aij  has a promising merit.
    // Apply the stability test.
    // We require  aij  to be sufficiently large compared to
    // all other nonzeros in column  j.  This is equivalent
    // to requiring cmax to be bounded by Ltol.

    if (lc == lc1) {

       // This is the maximum element, amax.
       // Find the biggest element in the rest of the column
       // and hence get cmax.  We know cmax .le. 1, but
       // we still want it exactly in order to break ties.
       // 27 Apr 2002: Settle for cmax = 1.

       aij    = amax;
       cmax   = one;

       // cmax   = zero
       // do 140 l = lc1 + 1, lc2
       // cmax  = max( cmax, abs( a(l) ) )
       // 140            continue
       // cmax   = cmax / amax
    } // if (lc == lc1)
    else { // lc != lc1

       // aij is not the biggest element, so cmax >= 1.
       // Bail out if cmax will be too big.

       aij    = Math.abs( a[lc-1] );
       if ( TCP ) { // Absolute test for Complete Pivoting
          if (aij      < aijtol) continue Colj;
       } // if (TCP)
       else { // TPP
          if (aij*Ltol < amax  ) continue Colj;
       } // else TPP
       cmax   = amax / aij;
    } // else lc != lc1

    // aij  is big enough.  Its maximum multiplier is cmax.

    merit  = nz1 * len1;
    if (merit == mbest[0]) {

       // Break ties.
       // (Initializing mbest < 0 prevents getting here if
       // nothing has been found yet.)
       // In this version we minimize cmax
       // but if it is already small we maximize the pivot.

       if (lbest <= gamma  &&  cmax <= gamma) {
          if (abest >= aij ) continue Colj;
       }
       else {
          if (lbest <= cmax) continue Colj;
       }
    } // if (merit == mbest[0])

    // aij  is the best pivot so far.

    ibest[0]  = i;
    jbest[0]  = j;
    kbest  = len1;
    mbest[0]  = merit;
    abest  = aij;
    lbest  = cmax;
    if (nz == 1) break NZS;
} // Colj: for (lc = lc1; lc <= lc2; lc++)

 // Finished with that column.

 if (ibest[0] > 0) {
    if (ncol >= maxcol) break Cols;
 }
} // Cols:  for (lq = lq1; lq <= lq2; lq++)
} // if (seg2)
seg2 = true;

// ---------------------------------------------------------------
// Search the set of rows of length  nz.
// ---------------------------------------------------------------
// 200  if (mbest .le. nz*nz1) go to 900
if (kbest <= nz    ) break NZS;
if (ibest[0] > 0) {
 if (nrow >= maxrow) {
	 seg3 = false;
	 seg4 = false;
 } // if (nrow >= maxrow)
} // if (ibest[0] > 0)
if (seg3) {
if (nz > n) {
	seg4 = false;
}
} // if (seg3)
seg3 = true;

if (seg4) {
lp1    = iploc[nz-1];
lp2    = m;
if (nz < n) lp2 = iploc[nz] - 1;

Rows:  for (lp = lp1; lp <= lp2; lp++) {
 nrow   = nrow + 1;
 i      = p[lp-1];
 lr1    = locr[i-1];
 lr2    = lr1 + nz1;

Rowi:     for (lr = lr1; lr <= lr2; lr++) {
    j      = indr[lr-1];
    len1   = lenc[j-1] - 1;
    // merit  = nz1 * len1
  // if (merit > mbest) cycle
    if (len1  > kbest) continue Rowi;

    // aij  has a promising merit.
    // Find where  aij  is in column  j.

    lc1    = locc[j-1];
    lc2    = lc1 + len1;
    amax   = Math.abs( a[lc1-1] );
    for (lc = lc1; lc <= lc2; lc++) {
       if (indc[lc-1] == i) break;
    }

    // Apply the same stability test as above.

    aij    = Math.abs( a[lc-1] );
    if ( TCP ) {   // Absolute test for Complete Pivoting
       if (aij < aijtol) continue Rowi;
    } // if (TCP)

    if (lc == lc1) {

       // This is the maximum element, amax.
       // Find the biggest element in the rest of the column
       // and hence get cmax.  We know cmax .le. 1, but
       // we still want it exactly in order to break ties.
       // 27 Apr 2002: Settle for cmax = 1.

       cmax   = one;

       // cmax   = zero
       //     do 240 l = lc1 + 1, lc2
       //        cmax  = max( cmax, abs( a(l) ) )
       // 240 continue
       // cmax   = cmax / amax
    } // if (lc == lc1)
    else { // lc != lc1

       // aij is not the biggest element, so cmax .ge. 1.
       // Bail out if cmax will be too big.

       if ( TCP ) {
          // relax
       }
       else {
          if (aij*Ltol < amax) continue Rowi;
       }
       cmax   = amax / aij;
    } // else lc != lc1

    // aij  is big enough.  Its maximum multiplier is cmax.

    merit  = nz1 * len1;
    if (merit == mbest[0]) {

       // Break ties as before.
       // (Initializing mbest < 0 prevents getting here if
       // nothing has been found yet.)

       if (lbest <= gamma  &&  cmax <= gamma) {
          if (abest >= aij ) continue Rowi;
       }
       else {
          if (lbest <= cmax) continue Rowi;
       }
    } // if (merit == mbest[0])

    // aij  is the best pivot so far.

    ibest[0]  = i;
    jbest[0]  = j;
    kbest  = len1;
    mbest[0]  = merit;
    abest  = aij;
    lbest  = cmax;
    if (nz == 1) break NZS;
} // Rowi:     for (lr = lr1; lr <= lr2; lr++)

 // Finished with that row.

 if (ibest[0] > 0) {
    if (nrow >= maxrow) break Rows;
 } // if (ibest[0] > 0)
} // Rows:  for (lp = lp1; lp <= lp2; lp++)
} // if (seg4)
seg4 = true;

// See if it's time to quit.

if (ibest[0] > 0) {
 if (nrow >= maxrow &&  ncol >= maxcol) break NZS;
} // if (ibest[0] > 0)

// Press on with next nz.

nz1    = nz;
if (ibest[0] > 0) kbest = mbest[0] / nz1;
} // NZS: for (nz = 1; nz <= maxmn; nz++)

} // lu1mar

private void lu1mRP(int m, int n, int lena, int maxmn,
        double Ltol, int maxcol, int maxrow,
        int ibest[], int jbest[], int mbest[],
        double a[], int indc[], int indr[], int p[], int q[],
        int lenc[], int lenr[], int locc[], int locr[],
        int iploc[], int iqloc[], double Amaxr[]) {

//integer(ip),   intent(in)    :: m, n, lena, maxmn, maxcol, maxrow
//integer(ip),   intent(out)   :: ibest, jbest, mbest
//real(rp),      intent(in)    :: Ltol
//integer(ip),   intent(in)    :: indc(lena), indr(lena), p(m)    , q(n)    , &
//                       lenc(n)   , lenr(m)   , iploc(n), iqloc(m), &
//                       locc(n)   , locr(m)
//real(rp),      intent(in)    :: a(lena)   , Amaxr(m)

// ------------------------------------------------------------------
// lu1mRP  uses a Markowitz criterion to select a pivot element
// for the next stage of a sparse LU factorization,
// subject to a Threshold Rook Pivoting stability criterion (TRP)
// that bounds the elements of L and U.

// 11 Jun 2002: First version of lu1mRP derived from lu1mar.
// 11 Jun 2002: Current version of lu1mRP.

// 10 Jan 2010: First f90 version.
// 12 Dec 2011: Declare intent.
// ------------------------------------------------------------------

int i, j, kbest, lc, lc1, lc2, len1,
    lp, lp1, lp2, lq, lq1, lq2, lr, lr1, lr2,
    merit, ncol, nrow, nz, nz1;
double abest, aij, amax, atoli, atolj;
final double zero = 0.0;
boolean seg1 = true;
boolean seg2 = true;
boolean seg3 = true;
boolean seg4 = true;

// ------------------------------------------------------------------
// Search cols of length nz = 1, then rows of length nz = 1,
// then   cols of length nz = 2, then rows of length nz = 2, etc.
// ------------------------------------------------------------------
abest  = zero;
ibest[0]  = 0;
kbest  = maxmn + 1;
mbest[0]  = -1;
ncol   = 0;
nrow   = 0;
nz1    = 0;

NZS: for(nz = 1; nz <= maxmn; nz++) {
// nz1    = nz - 1
// if (mbest .le. nz1**2) go to 900
if (kbest <= nz1) break NZS;
if (ibest[0] >  0  ) {
if (ncol >= maxcol) {
	seg1 = false;
	seg2 = false;
}
} // if (ibest[0] > 0)
if (seg1) {
if (nz    >  m  ) {
	seg2 = false;
}
} // if (seg1)
seg1 = true;

if (seg2) {
// ---------------------------------------------------------------
// Search the set of columns of length  nz.
// ---------------------------------------------------------------
lq1    = iqloc[nz-1];
lq2    = n;
if (nz < m) lq2 = iqloc[nz] - 1;

Cols:  for (lq = lq1; lq <= lq2; lq++) {
ncol   = ncol + 1;
j      = q[lq-1];
lc1    = locc[j-1];
lc2    = lc1 + nz1;
amax   = Math.abs( a[lc1-1] );
atolj  = amax / Ltol;    // Min size of pivots in col j

// Test all aijs in this column.

Colj:  for (lc = lc1; lc <= lc2; lc++) {
i      = indc[lc-1];
len1   = lenr[i-1] - 1;
// merit  = nz1 * len1
// if (merit > mbest) cycle Colj
if (len1  > kbest) continue Colj;

// aij  has a promising merit.
// Apply the Threshold Rook Pivoting stability test.
// First we require aij to be sufficiently large
// compared to other nonzeros in column j.
// Then  we require aij to be sufficiently large
// compared to other nonzeros in row    i.

aij    = Math.abs( a[lc-1] );
if (aij      < atolj   ) continue Colj;
if (aij*Ltol < Amaxr[i-1]) continue Colj;

// aij  is big enough.

merit  = nz1 * len1;
if (merit == mbest[0]) {

   // Break ties.
   // (Initializing mbest < 0 prevents getting here if
   // nothing has been found yet.)

   if (abest >= aij) continue Colj;
} // if (merit == mbest[0])

// aij  is the best pivot so far.

ibest[0]  = i;
jbest[0]  = j;
kbest  = len1;
mbest[0]  = merit;
abest  = aij;
if (nz == 1) break NZS;
} // Colj:  for (lc = lc1; lc <= lc2; lc++)

// Finished with that column.

if (ibest[0] > 0) {
if (ncol >= maxcol) break Cols;
} // if (ibest[0] > 0)
} // Cols:  for (lq = lq1; lq <= lq2; lq++)
} // if (seg2)
seg2 = true;

// ---------------------------------------------------------------
// Search the set of rows of length  nz.
// ---------------------------------------------------------------
// 200  if (mbest .le. nz*nz1) go to 900
if (kbest <= nz    ) break NZS;
if (ibest[0] > 0) {
if (nrow >= maxrow) {
	seg3 = false;
	seg4 = false;
}
} // if (ibest[0] > 0)
if (seg3) {
if (nz > n) {
	seg4 = false;
}
} // if (seg3)
seg3 = true;

if (seg4) {
lp1    = iploc[nz-1];
lp2    = m;
if (nz < n) lp2 = iploc[nz] - 1;

Rows:  for (lp = lp1; lp <= lp2; lp++) {
nrow   = nrow + 1;
i      = p[lp-1];
lr1    = locr[i-1];
lr2    = lr1 + nz1;
atoli  = Amaxr[i-1] / Ltol;   // Min size of pivots in row i

Rowi: for (lr = lr1; lr <= lr2; lr++) {
j      = indr[lr-1];
len1   = lenc[j-1] - 1;
// merit  = nz1 * len1
// if (merit > mbest) cycle
if (len1  > kbest) continue Rowi;

// aij  has a promising merit.
// Find where  aij  is in column j.

lc1    = locc[j-1];
lc2    = lc1 + len1;
amax   = Math.abs( a[lc1-1] );
for (lc = lc1; lc <= lc2; lc++) {
   if (indc[lc-1] == i) break;
} // for (lc = lc1; lc <= lc2; lc++)

// Apply the Threshold Rook Pivoting stability test.
// First we require aij to be sufficiently large
// compared to other nonzeros in row    i.
// Then  we require aij to be sufficiently large
// compared to other nonzeros in column j.

aij    = Math.abs( a[lc-1] );
if (aij      < atoli) continue Rowi;
if (aij*Ltol < amax ) continue Rowi;

// aij  is big enough.

merit  = nz1 * len1;
if (merit == mbest[0]) {

   // Break ties as before.
   // (Initializing mbest < 0 prevents getting here if
   // nothing has been found yet.)

   if (abest >= aij ) continue Rowi;
} // if (merit == mbest[0])

// aij  is the best pivot so far.

ibest[0]  = i;
jbest[0]  = j;
kbest  = len1;
mbest[0]  = merit;
abest  = aij;
if (nz == 1) break NZS;
} // Rowi: for (lr = lr1; lr <= lr2; lr++)

// Finished with that row.

if (ibest[0] > 0) {
if (nrow >= maxrow) break Rows;
} // if (ibest[0] > 0)
} // Rows:  for (lp = lp1; lp <= lp2; lp++)
} // if (seg4)
seg4 = true;

// See if it's time to quit.

if (ibest[0] > 0) {
if (nrow >= maxrow  &&  ncol >= maxcol) break NZS;
} // if (ibest[0] > 0)

// Press on with next nz.

nz1    = nz;
if (ibest[0] > 0) kbest  = mbest[0] / nz1;
} // NZS: for(nz = 1; nz <= maxmn; nz++)

} // lu1mRP

private void lu1mSP(int m, int n, int lena, int maxmn,
        double Ltol , int maxcol,
        int ibest[], int jbest[], int mbest[],
        double a[], int indc[], int q[], int locc[], int iqloc[]) {

//integer(ip),   intent(in)    :: m, n, lena, maxmn, maxcol
//integer(ip),   intent(out)   :: ibest, jbest, mbest
//real(rp),      intent(in)    :: Ltol, a(lena)
//integer(ip),   intent(in)    :: indc(lena), q(n), iqloc(m), locc(n)

// ------------------------------------------------------------------
// lu1mSP  is intended for symmetric matrices that are either
// definite or quasi-definite.
// lu1mSP  uses a Markowitz criterion to select a pivot element for
// the next stage of a sparse LU factorization of a symmetric matrix,
// subject to a Threshold Symmetric Pivoting stability criterion
// (TSP) restricted to diagonal elements to preserve symmetry.
// This bounds the elements of L and U and should have rank-revealing
// properties analogous to Threshold Rook Pivoting for unsymmetric
// matrices.

// 14 Dec 2002: First version of lu1mSP derived from lu1mRP.
//              There is no safeguard to ensure that A is symmetric.
// 14 Dec 2002: Current version of lu1mSP.

// 10 Jan 2010: First f90 version.
// 12 Dec 2011: Declare intent.
// ------------------------------------------------------------------

int i, j, kbest, lc, lc1, lc2,
    lq, lq1, lq2, merit, ncol, nz, nz1;
double abest, aij, amax, atolj;
final double zero = 0.0;
boolean seg1 = true;
boolean seg2 = true;

// ------------------------------------------------------------------
// Search cols of length nz = 1, then cols of length nz = 2, etc.
// ------------------------------------------------------------------
abest  = zero;
ibest[0]  = 0;
kbest  = maxmn + 1;
mbest[0]  = -1;
ncol   = 0;
nz1    = 0;

NZS: for (nz = 1; nz <= maxmn; nz++) {
// nz1    = nz - 1
// if (mbest <= nz1**2) exit
if (kbest <= nz1   ) break NZS;
if (ibest[0] > 0) {
if (ncol >= maxcol) {
	seg1 = false;
	seg2 = false;
}
} // if (ibest[0] > 0)
if (seg1) {
if (nz > m) {
	seg2 = false;
}
} // if (seg1)
seg1 = true;

if (seg2) {
// ---------------------------------------------------------------
// Search the set of columns of length  nz.
// ---------------------------------------------------------------
lq1    = iqloc[nz-1];
lq2    = n;
if (nz < m) lq2 = iqloc[nz] - 1;

Cols:  for (lq = lq1; lq <= lq2; lq++) {
ncol   = ncol + 1;
j      = q[lq-1];
lc1    = locc[j-1];
lc2    = lc1 + nz1;
amax   = Math.abs( a[lc1-1] );
atolj  = amax / Ltol;    // Min size of pivots in col j

// Test all aijs in this column.
// Ignore everything except the diagonal.

Colj: for (lc = lc1; lc <= lc2; lc++) {
i      = indc[lc-1];
if (i != j) continue Colj;     // Skip off-diagonals.
// merit  = nz1 * nz1
// if (merit > mbest) cycle
if (nz1   > kbest) continue Colj;

// aij  has a promising merit.
// Apply the Threshold Partial Pivoting stability test
// (which is equivalent to Threshold Rook Pivoting for
// symmetric matrices).
// We require aij to be sufficiently large
// compared to other nonzeros in column j.

aij    = Math.abs( a[lc-1] );
if (aij < atolj  ) continue Colj;

// aij  is big enough.

merit  = nz1 * nz1;
if (merit == mbest[0]) {

   // Break ties.
   // (Initializing mbest < 0 prevents getting here if
   // nothing has been found yet.)

   if (abest >= aij) continue Colj;
} // if (merit == mbest[0])

// aij  is the best pivot so far.

ibest[0]  = i;
jbest[0]  = j;
kbest  = nz1;
mbest[0]  = merit;
abest  = aij;
if (nz == 1) break NZS;
} // Colj: for (lc = lc1; lc <= lc2; lc++)

// Finished with that column.

if (ibest[0] > 0) {
if (ncol >= maxcol) break Cols;
} // if (ibest[0] > 0)
} // Cols:  for (lq = lq1; lq <= lq2; lq++)
} // if (seg2)
seg2 = true;

// See if it's time to quit.

if (ibest[0] > 0) {
if (ncol >= maxcol) break NZS;
} // if (ibest[0] > 0)

// Press on with next nz.

nz1    = nz;
if (ibest[0] > 0) kbest  = mbest[0] / nz1;
} // NZS: for (nz = 1; nz <= maxmn; nz++)

} // lu1mSP

    
    private void lu1mxc(int k1, int k2, int q[], double a[], int indc[], int lenc[], int locc[]) {

    //integer(ip),   intent(in)    :: k1, k2
    //integer(ip),   intent(in)    :: q(k2), lenc(*), locc(*)
    //integer(ip),   intent(inout) :: indc(*)
    //real(rp),      intent(inout) :: a(*)

    // ------------------------------------------------------------------
    // lu1mxc  moves the largest element in each of columns q(k1:k2)
    // to the top of its column.
    // If k1 > k2, nothing happens.
    //
    // 06 May 2002: (and earlier)
    //              All columns k1:k2 must have one or more elements.
    // 07 May 2002: Allow for empty columns.  The heap routines need to
    //              find 0.0 as the "largest element".
    //
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent.
    // ------------------------------------------------------------------

    int i, j, k, l, lc, lc1, lc2, lenj;
    double amax;
    final double zero = 0.0;


    for (k = k1; k <= k2; k++) {
       j      = q[k-1];
       lc1    = locc[j-1];
       lenj   = lenc[j-1];

       if (lenj == 0) {
          a[lc1-1] = zero;
       }
       else {

          // The next 10 lines are equivalent to
          // l      = idamax( lenc(j), a(lc1), 1 )  +  lc1 - 1
          // >>>>>>>>
          lc2    = lc1 + lenc[j-1] - 1;
          amax   = Math.abs( a[lc1-1] );
          l      = lc1;

          for (lc = lc1+1; lc <= lc2; lc++) {
             if (amax < Math.abs( a[lc-1] )) {
                amax   =  Math.abs( a[lc-1] );
                l      =  lc;
             }
          } // for (lc = lc1+1; lc <= lc2; lc++)
          // >>>>>>>>

          if (l > lc1) {
             amax      = a[l-1];
             a[l-1]      = a[lc1-1];
             a[lc1-1]    = amax;
             i         = indc[l-1];
             indc[l-1]   = indc[lc1-1];
             indc[lc1-1] = i;
          }
       }
    } // for (k = k1; k <= k2; k++)

    } // lu1mxc
    
    private void lu1pen(int m, int melim, int ncold, int nspare, int ilast[],
            int lpivc1, int lpivc2, int lpivr1, int lpivr2, int lrow[],
            int lenc[], int lenr[], int locc[], int locr[],
            int indc[], int indr[], int ifill[], int jfill[]) {

//integer(ip),   intent(in)    :: m, melim, ncold, nspare, &
//                           lpivc1, lpivc2, lpivr1, lpivr2
//integer(ip),   intent(inout) :: lrow
//integer(ip),   intent(out)   :: ilast
//integer(ip),   intent(inout) :: indc(*), indr(*), lenc(*), lenr(*)
//integer(ip),   intent(in)    :: locc(*), ifill(melim), jfill(ncold)
//integer(ip),   intent(inout) :: locr(*)

// ------------------------------------------------------------------
// lu1pen deals with pending fill-in in the row file.
// ifill(ll) says if a row involved in the new column of L
// has to be updated.  If positive, it is the total
// length of the final updated row.
// jfill(lu) says if a column involved in the new row of U
// contains any pending fill-ins.  If positive, it points
// to the first fill-in in the column that has yet to be
// added to the row file.

// 16 Apr 1989: First version of lu1pen.
// 23 Mar 2001: ilast used and updated.

// 10 Jan 2010: First f90 version.
// 12 Dec 2011: Declare intent.
// ------------------------------------------------------------------

int i, j, l, last, lc, lc1, lc2, ll, lr, lr1, lr2, lu;

ll     = 0;

for (lc = lpivc1; lc <= lpivc2; lc++) {
ll = ll + 1;
if (ifill[ll-1] == 0) continue;

// Another row has pending fill.
// First, add some spare space at the end
// of the current last row.

for (l = lrow[0] + 1; l <= lrow[0] + nspare; l++) {
 lrow[0]    = l;
 indr[l-1] = 0;
} // for (l = lrow[0] + 1; l <= lrow[0] + nspare; l++)

// Now move row i to the end of the row file.

i       = indc[lc-1];
ilast[0]   = i;
lr1     = locr[i-1];
lr2     = lr1 + lenr[i-1] - 1;
locr[i-1] = lrow[0] + 1;

for (lr = lr1; lr <= lr2; lr++) {
 lrow[0]       = lrow[0] + 1;
 indr[lrow[0]-1] = indr[lr-1];
 indr[lr-1]   = 0;
} // for (lr = lr1; lr <= lr2; lr++)

lrow[0]    = lrow[0] + ifill[ll-1];
} // for (lc = lpivc1; lc <= lpivc2; lc++)

// Scan all columns of  D  and insert the pending fill-in
// into the row file.

lu     = 1;

for (lr = lpivr1; lr <= lpivr2; lr++) {
lu     = lu + 1;
if (jfill[lu-1] == 0) continue;
j      = indr[lr-1];
lc1    = locc[j-1] + jfill[lu-1] - 1;
lc2    = locc[j-1] + lenc[j-1]   - 1;

for (lc = lc1; lc <= lc2; lc++) {
 i      = indc[lc-1] - m;
 if (i > 0) {
    indc[lc-1]   = i;
    last       = locr[i-1] + lenr[i-1];
    indr[last-1] = j;
    lenr[i-1]    = lenr[i-1] + 1;
 } // if (i > 0)
} // for (lc = lc1; lc <= lc2; lc++)
} // for (lr = lpivr1; lr <= lpivr2; lr++)

} // lu1pen
    
    private void lu1pq2(int nzpiv, int nzchng[], int indr[], int lenold[], int lennew[], int iqloc[], int q[], int iqinv[]) {

    //integer(ip),   intent(in)    :: nzpiv
    //integer(ip),   intent(out)   :: nzchng
    //integer(ip),   intent(in)    :: lenold(nzpiv), lennew(*)
    //integer(ip),   intent(inout) :: indr(nzpiv), iqloc(*), q(*), iqinv(*)

    // ===============================================================
    // lu1pq2 frees the space occupied by the pivot row,
    // and updates the column permutation q.
    //
    // Also used to free the pivot column and update the row perm p.
    //
    // nzpiv   (input)    is the length of the pivot row (or column).
    // nzchng  (output)   is the net change in total nonzeros.
    //
    // 14 Apr 1989:  First version.
    //
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ===============================================================

    int j, jnew, l, lnew, lr, next, nz, nznew;

    nzchng[0] = 0;

    for (lr  = 1; lr <= nzpiv; lr++) {
       j        = indr[lr-1];
       indr[lr-1] = 0;
       nz       = lenold[lr-1];
       nznew    = lennew[j-1];

       if (nz != nznew) {
          l        = iqinv[j-1];
          nzchng[0]   = nzchng[0] + (nznew - nz);

          // l above is the position of column j in q  (so j = q(l)).

          if (nz < nznew) {

             // Column  j  has to move towards the end of  q.
             while (true) {
             next        = nz + 1;
             lnew        = iqloc[next-1] - 1;
             if (lnew != l) {
                jnew        = q[lnew-1];
                q[l-1]        = jnew;
                iqinv[jnew-1] = l;
             } // if (lnew != l)
             l           = lnew;
             iqloc[next-1] = lnew;
             nz          = next;
             if (nz < nznew) {
            	 continue;
             }
             else {
            	 break;
             }
             } // while (true)
          } // if (nz < nznew)
          else {

             // Column  j  has to move towards the front of  q.
             while (true) {
             lnew        = iqloc[nz-1];
             if (lnew != l) {
                jnew        = q[lnew-1];
                q[l-1]        = jnew;
                iqinv[jnew-1] = l;
         } // if (lnew != l)
             l           = lnew;
             iqloc[nz-1]   = lnew + 1;
             nz          = nz   - 1;
             if (nz > nznew) {
            	 continue;
             }
             else {
            	 break;
             }
             } // while (true)
          } // else

          q[lnew-1]  = j;
          iqinv[j-1] = lnew;
       } // if (nz != nznew)
    } // for (lr  = 1; lr <= nzpiv; lr++)

} // lu1pq2

    private void lu1pq3(int n, int len[], int iperm[], int iw[], int nrank[]) {

    //integer(ip),   intent(in)    :: n
    //integer(ip),   intent(in)    :: len(n)
    //integer(ip),   intent(inout) :: iperm(n)
    //integer(ip),   intent(out)   :: iw(n)   ! workspace

    // ------------------------------------------------------------------
    // lu1pq3  looks at the permutation  iperm(*)  and moves any entries
    // to the end whose corresponding length  len(*)  is zero.
    
    // 09 Feb 1994: Added work array iw(*) to improve efficiency.
    
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ------------------------------------------------------------------

    int i, k, nzero;

    nrank[0]  = 0;
    nzero  = 0;

    for (k = 1; k <= n; k++) {
       i = iperm[k-1];

       if (len[i-1] == 0) {
          nzero     = nzero + 1;
          iw[nzero-1] = i;
       }
       else {
          nrank[0]        = nrank[0] + 1;
          iperm[nrank[0]-1] = i;
       }
    } // for (k = 1; k <= n; k++)

    for (k = 1; k <= nzero; k++) {
       iperm[nrank[0] + k-1] = iw[k-1];
    }

} // lu1pq3

    
    private void lu1rec(int n, boolean reals, int luparm[], int ltop[], int lena, double a[], int ind[],
    		int lenc[], int locc[]) {

    //logical,       intent(in)    :: reals
    //integer(ip),   intent(in)    :: n, lena
    //integer(ip),   intent(out)   :: ltop
    //integer(ip),   intent(inout) :: luparm(30), ind(lena), lenc(n), locc(n)
    //real(rp),      intent(inout) :: a(lena)

    // ------------------------------------------------------------------
    // lu1rec recovers space in the column or row lists.
    // 00 Jun 1983: Original version of lu1rec followed John Reid's
    //              compression routine in LA05.  It recovered space
    //              in ind(*) and optionally a(*) by eliminating entries
    //              with ind(l) = 0.
    //              The elements of ind(*) could not be negative.
    //              If len(i) was positive, entry i contained
    //              that many elements, starting at  loc(i).
    //              Otherwise, entry i was eliminated.
    //
    // 23 Mar 2001: Realised we could have len(i) = 0 in rare cases!
    //              (Mostly during TCP when the pivot row contains
    //              a column of length 1 that couldn't be a pivot.)
    //              Revised storage scheme to
    //                 keep        entries with       ind(l) >  0,
    //                 squeeze out entries with -n <= ind(l) <= 0,
    //              and to allow len(i) = 0.
    //              Empty items are moved to the end of the compressed
    //              ind(*) and/or a(*) arrays are given one empty space.
    //              Items with len(i) < 0 are still eliminated.
    //
    // 27 Mar 2001: Decided to use only ind(l) > 0 and = 0 in lu1fad.
    //              Still have to keep entries with len(i) = 0.
    //
    // On exit:
    // ltop         is the length of useful entries in ind(*), a(*).
    // ind(ltop+1)  is "i" such that len(i), loc(i) belong to the last
    //              item in ind(*), a(*).
    //
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ------------------------------------------------------------------

    int i, ilast, k, klast, l, leni, lprint, nempty;

    nempty = 0;

    for (i = 1; i <= n; i++) {
       leni = lenc[i-1];
       if (leni > 0) {
          l       = locc[i-1] + leni - 1;
          lenc[i-1] = ind[l-1];
          ind[l-1]  = - (n + i);
       } // if (leni > 0)
       else if (leni == 0) {
          nempty  = nempty + 1;
       }
    } // for (i = 1; i <= n; i++)

    k      = 0;
    klast  = 0;    // Previous k
    ilast  = 0;    // Last entry moved.

    for (l = 1; l <= ltop[0]; l++) {
       i = ind[l-1];
       if (i > 0) {
          k      = k + 1;
          ind[k-1] = i;
          if (reals) a[k-1] = a[l-1];
       } // if (i > 0)
       else if (i < -n) {     // This is the end of entry  i.
          i       = - (i + n);
          ilast   = i;
          k       = k + 1;
          ind[k-1]  = lenc[i-1];
          if (reals) a[k-1] = a[l-1];
          locc[i-1] = klast + 1;
          lenc[i-1] = k     - klast;
          klast   = k;
       } // else if (i < -n)
    } // for (l = 1; l <= ltop[0]; l++)

    // Move any empty items to the end, adding 1 free entry for each.

    if (nempty > 0) {
       for (i = 1; i <= n; i++) {
          if (lenc[i-1] == 0) {
             k       = k + 1;
             locc[i-1] = k;
             ind[k-1]  = 0;
             ilast   = i;
          } // if (lenc[i-1] == 0)
       } // for (i = 1; i <= n; i++)
    } // if (nempty > 0)

    lprint = luparm[1];
    if (lprint >= 50) {
    	UI.setDataText("lu1rec.  File compressed from " + ltop[0] + " to " + k + " " + reals + " nempty = " + nempty + "\n");
    }
    luparm[25] = luparm[25] + 1;  // ncp

    // Return ilast in ind(ltop + 1).

    ltop[0]        = k;
    ind[ltop[0]] = ilast;
    return;
} //lu1rec

    
    private void lu1slk(int m, int n, int lena, int q[], int iqloc[], double a[], int locc[], double w[]) {

    //integer(ip),   intent(in)    :: m, n, lena
    //integer(ip),   intent(in)    :: q(n), iqloc(m), locc(n)
    //real(rp),      intent(in)    :: a(lena)
    //real(rp),      intent(out)   :: w(n)

    // ------------------------------------------------------------------
    // lu1slk  sets w(j) > 0 if column j is a unit vector.
    //
    // 21 Nov 2000: First version.  lu1fad needs it for TCP.
    //              Note that w(*) is nominally an integer(ip) array,
    //              but the only spare space is the double array w(*).
    //
    // 10 Jan 2010: First f90 version.
    // 12 Dec 2011: Declare intent and local variables.
    // ------------------------------------------------------------------

    int j, lc1, lq, lq1, lq2;

    for (j = 0; j < n; j++) {
    	w[j] = 0.0;
    }
    lq1    = iqloc[0];
    lq2    = n;
    if (m > 1){
    	lq2 = iqloc[1] - 1;
    }

    for (lq = lq1; lq <= lq2; lq++) {
       j      = q[lq-1];
       lc1    = locc[j-1];
       if (Math.abs( a[lc1-1] ) == 1.0) {
          w[j-1] = 1.0;
       }
    } // for (lq = lq1; lq <= lq2; lq++)

    } // lu1slk

    private void lu1mxr(int mark, int k1, int k2, int m, int n, int lena,
            double a[], int indc[], int lenc[], int locc[], int indr[], int lenr[], int locr[],
            int p[], int markc[], int markr[], double Amaxr[]) {

//implicit       none
//integer(ip),   intent(in)    :: mark, k1, k2, m, n, lena
//integer(ip),   intent(in)    :: indc(lena), lenc(n), locc(n),       &
//                           indr(lena), lenr(m), locr(m), p(k2)
//integer(ip),   intent(inout) :: markc(n), markr(m)
//real(rp),      intent(in)    :: a(lena)
//real(rp),      intent(inout) :: Amaxr(m)

// ------------------------------------------------------------------
// lu1mxr  finds the largest element in each of rows i = p(k1:k2)
// and stores it in each Amaxr(i).
// The nonzeros are stored column-wise in (a,indc,lenc,locc)
// and their structure is     row-wise in (  indr,lenr,locr).
//
// 11 Jun 2002: First version of lu1mxr.
//              Allow for empty columns.
// 10 Jan 2010: First f90 version.
// 12 Dec 2011: Declare intent.
// 03 Apr 2013: Recoded to improve efficiency.  Need new arrays
//              markc(n), markr(m) and local array cols(n).
//
//              First call:  mark = 0, k1 = 1, k2 = m.
//              Initialize all of markc(n), markr(m), Amaxr(m).
//              Columns are searched only once.
//              cols(n) is not used.
//
//              Later: mark := mark + 1 (greater than for previous call).
//              Cols involved in rows p(k1:k2) are searched only once.
//              cols(n) is local storage.
//              markc(:), markr(:) are marked (= mark) in some places.
//              For next call with new mark,
//              all of markc, markr will initially appear unmarked.
// ------------------------------------------------------------------

int cols[] = new int[n];
int i, j, k, lc, lc1, lc2, lr, lr1, lr2, ncol;
final double zero = 0.0;

if (mark == 0) {    // First call: Find Amaxr(1:m) for original A.
for (i = 0; i < m; i++) {
	markr[i] = 0;
}
for (i = 0; i < n; i++) {
	markc[i] = 0;
}
for (i = 0; i < m; i++) {
	Amaxr[i] = zero;
}
for (j = 1; j <= n; j++) {
 lc1   = locc[j-1];
 lc2   = lc1 + lenc[j-1] - 1;
 for (lc = lc1; lc <= lc2; lc++) {
    i  = indc[lc-1];
    Amaxr[i-1] = Math.max( Amaxr[i-1], Math.abs(a[lc-1]) );
 } // for (lc = lc1; lc <= lc2; lc++)
} // for (j = 1; j <= m; j++)
} // if (mark == 0)
else {               // Later calls: Find Amaxr(i) for rows i = p(k1:k2).

ncol = 0;
for (k = k1; k <= k2; k++) {        // Search rows to find which cols are involved.
 i        = p[k-1];
 markr[i-1] = mark;   // Mark this row
 Amaxr[i-1] = zero;
 lr1   = locr[i-1];
 lr2   = lr1 + lenr[i-1] - 1;
 for (lr = lr1; lr <= lr2; lr++) {     // Mark all unmarked cols in this row.
    j  = indr[lr-1];     // Build up a list of which ones they are.
    if (markc[j-1] != mark) {
        markc[j-1]  = mark;
        ncol      = ncol + 1;
        cols[ncol-1] = j;
    } // if (markc[j-1] != mark)
 } //  for (lr = lr1; lr <= lr2; lr++)
} // for (k = k1; k <= k2; k++)

for (k = 1; k <= ncol; k++) {       // Search involved columns.
 j     = cols[k-1];
 lc1   = locc[j-1];
 lc2   = lc1 + lenc[j-1] - 1;
 for (lc = lc1; lc <= lc2; lc++) {
    i  = indc[lc-1];
    if (markr[i-1] == mark) {
        Amaxr[i-1]  = Math.max( Amaxr[i-1], Math.abs(a[lc-1]) );
    }
 } // for (lc = lc1; lc <= lc2; lc++)
} // for (k = 1; k <= ncol; k++)
} // else later calls

    } // lu1mxr
    
    private void Hbuild(double Ha[], int Hj[], int Hk[], int N, int Nk, int hops[]) {

    //integer(ip),   intent(in)    :: N, Nk
    //integer(ip),   intent(out)   :: hops
    //integer(ip),   intent(inout) :: Hj(N), Hk(Nk)
    //real(rp),      intent(out)   :: Ha(N)

    // ==================================================================
    // Hbuild initializes the heap by inserting each element of Ha.
    // Input:  Ha, Hj.
    // Output: Ha, Hj, Hk, hops.
    //
    // 01 May 2002: Use k for new length of heap, not k-1 for old length.
    // 05 May 2002: Use kk in call to stop loop variable k being altered.
    //              (Actually Hinsert no longer alters that parameter.)
    // 07 May 2002: ftnchek wants us to protect Nk, Ha(k), Hj(k) too.
    // 07 May 2002: Current version of Hbuild.
    // 12 Dec 2011: First f90 version.
    // ==================================================================

    int kk[] = new int[1];
    int h[] = new int[1];
    int jv, k, Nkk;
    double v;

    Nkk  = Nk;
    hops[0] = 0;
    for (k = 1; k <= N; k++) {
       kk[0]    = k;
       v     = Ha[k-1];
       jv    = Hj[k-1];
       Hinsert( Ha, Hj, Hk, kk, Nkk, v, jv, h );
       hops[0]  = hops[0] + h[0];
    } // for (k = 1; k <= N; k++)

    } // Hbuild
    
    private void Hchange(double Ha[], int Hj[], int Hk[], int N, int Nk, int k, double v, int jv, int hops[] ) {

    //integer(ip),   intent(in)    :: N, Nk, k, jv
    //integer(ip),   intent(out)   :: hops
    //integer(ip),   intent(inout) :: Hj(N), Hk(Nk)
    //real(rp),      intent(in)    :: v
    //real(rp),      intent(inout) :: Ha(N)

    // ==================================================================
    // Hchange changes Ha(k) to v in heap of length N.
    //
    // 01 May 2002: Need Nk for length of Hk.
    // 07 May 2002: Protect input parameters N, Nk, k.
    // 07 May 2002: Current version of Hchange.
    // 12 Dec 2011: First f90 version.
    // ==================================================================

    int kx, Nx, Nkx;
    double v1;

    Nx     = N;
    Nkx    = Nk;
    kx     = k;
    v1     = Ha[k-1];
    Ha[k-1]  = v;
    Hj[k-1]  = jv;
    Hk[jv-1] = k;
    if (v1 < v) {
       Hup ( Ha, Hj, Hk, Nx, Nkx, kx, hops );
    }
    else {
       Hdown ( Ha, Hj, Hk, Nx, Nkx, kx, hops );
    }
} // Hchange

    
    private void Hdelete(double Ha[], int Hj[], int Hk[], int Nin, int N[], int Nk, int k, int hops[]) {

    //integer(ip),   intent(in)    :: Nin, Nk, k
    //integer(ip),   intent(inout) :: N
    //integer(ip),   intent(out)   :: hops
    //integer(ip),   intent(inout) :: Hj(Nin), Hk(Nk)
    //real(rp),      intent(inout) :: Ha(Nin)

    // ==================================================================
    // Hdelete deletes Ha(k) from heap of length N.
    //
    // 03 Apr 2002: Current version of Hdelete.
    // 01 May 2002: Need Nk for length of Hk.
    // 07 May 2002: Protect input parameters N, Nk, k.
    // 19 Dec 2004: Nin is new input parameter for length of Hj, Ha.
    // 19 Dec 2004: Current version of Hdelete.
    // 12 Dec 2011: First f90 version.
    // ==================================================================

    int jv, kx, Nkx, Nx;
    double v;

    kx    = k;
    Nkx   = Nk;
    Nx    = N[0];
    v     = Ha[N[0]-1];
    jv    = Hj[N[0]-1];
    N[0]     = N[0] - 1;
    hops[0]  = 0;
    if (k <= N[0]) {
       Hchange( Ha, Hj, Hk, Nx, Nkx, kx, v, jv, hops );
    }
} // Hdelete
    
    private void Hdown (double Ha[], int Hj[], int Hk[], int N, int Nk, int kk, int hops[]) {

    //integer(ip),   intent(in)    :: N, Nk, kk
    //integer(ip),   intent(out)   :: hops
    //integer(ip),   intent(inout) :: Hj(N), Hk(Nk)
    //real(rp),      intent(inout) :: Ha(N)

    // ==================================================================
    // Hdown  updates heap by moving down tree from node k.
    
    // 01 May 2002: Need Nk for length of Hk.
    // 05 May 2002: Change input paramter k to kk to stop k being output.
    // 05 May 2002: Current version of Hdown.
    // 12 Dec 2011: First f90 version.
    // ==================================================================

    int j, jj, jv, k, N2;
    double v;

    k     = kk;
    hops[0]  = 0;
    v     = Ha[k-1];
    jv    = Hj[k-1];
    N2    = N/2;

    while (true) {
       if (k > N2) break;
       hops[0]   = hops[0] + 1;
       j      = k+k;
       if (j < N) {
          if (Ha[j-1] < Ha[j]) j = j+1;
       }
       if (v >= Ha[j-1]) break;
       Ha[k-1]  = Ha[j-1];
       jj     = Hj[j-1];
       Hj[k-1]  = jj;
       Hk[jj-1] =  k;
       k      =  j;
    } // while (true)

    Ha[k-1]  =  v;
    Hj[k-1]  = jv;
    Hk[jv-1] =  k;

} // Hdown


    
    private void Hinsert(double Ha[], int Hj[], int Hk[], int N[], int Nk, double v, int jv, int hops[]) {

    //integer(ip),   intent(in)    :: Nk, jv
    //integer(ip),   intent(inout) :: N
    //integer(ip),   intent(out)   :: hops
    //integer(ip),   intent(inout) :: Hj(N), Hk(Nk)
    //real(rp),      intent(in)    :: v
    //real(rp),      intent(inout) :: Ha(N)

    // ==================================================================
    // Hinsert inserts (v,jv) into heap of length N-1
    // to make heap of length N.
    //
    // 03 Apr 2002: First version of Hinsert.
    // 01 May 2002: Require N to be final length, not old length.
    //              Need Nk for length of Hk.
    // 07 May 2002: Protect input parameters N, Nk.
    // 07 May 2002: Current version of Hinsert.
    // 12 Dec 2011: First f90 version.
    // ==================================================================

    int kk, Nkk, Nnew;

    Nnew     = N[0];
    Nkk      = Nk;
    kk       = Nnew;
    Ha[Nnew-1] =  v;
    Hj[Nnew-1] = jv;
    Hk[jv-1]   = Nnew;
    Hup   ( Ha, Hj, Hk, Nnew, Nkk, kk, hops );

    } // Hinsert
    
    private void Hup   (double Ha[], int Hj[], int Hk[], int N, int Nk, int kk, int hops[]) {

    //integer(ip),   intent(in)    :: N, Nk, kk
    //integer(ip),   intent(out)   :: hops
    //integer(ip),   intent(inout) :: Hj(N), Hk(Nk)
    //real(rp),      intent(inout) :: Ha(N)

    // ==================================================================
    // Hup updates heap by moving up tree from node k.
    //
    // 01 May 2002: Need Nk for length of Hk.
    // 05 May 2002: Change input parameter k to kk to stop k being output.
    // 05 May 2002: Current version of Hup.
    // 13 Dec 2011: First f90 version.
    // ==================================================================

    int j, jv, k, k2;
    double v;

    k     = kk;
    hops[0]  = 0;
    v     = Ha[k-1];
    jv    = Hj[k-1];

    while (true) {
       if (k <  2) {
    	   break;
       }
       k2    = k/2;
       if (v < Ha[k2-1]) {
    	   break;
       }
       hops[0]  = hops[0] + 1;
       Ha[k-1] = Ha[k2-1];
       j     = Hj[k2-1];
       Hj[k-1] =  j;
       Hk[j-1] =  k;
       k     = k2;
    }

    Ha[k-1]  =  v;
    Hj[k-1]  = jv;
    Hk[jv-1] =  k;

    } // Hup
    
    private void lu6chk(int mode, int m, int n, double w[], int lena, int luparm[], double parmlu[],
            double a[], int indc[], int indr[], int p[], int q[], int lenc[], int lenr[], int locc[], int locr[], int inform[]) {

//integer(ip),   intent(in)    :: mode, m, n, lena
//integer(ip),   intent(inout) :: inform
//integer(ip),   intent(inout) :: luparm(30)
//integer(ip),   intent(in)    :: indc(lena), indr(lena), p(m), q(n), &
//                           lenc(n), lenr(m), locc(n), locr(m)
//real(rp),      intent(in)    :: a(lena)
//real(rp),      intent(inout) :: parmlu(30)
//real(rp),      intent(out)   :: w(n)

// -----------------------------------------------------------------
// lu6chk  looks at the LU factorization  A = L*U.

// If mode = 1, lu6chk is being called by lu1fac.
// (Other modes not yet implemented.)
// The important input parameters are

// lprint = luparm(2)
// luparm(6) = 1 if TRP
// keepLU = luparm(8)
// Utol1  = parmlu(4)
// Utol2  = parmlu(5)

// and the significant output parameters are

// inform = luparm(10)
// nsing  = luparm(11)
// jsing  = luparm(12)
// jumin  = luparm(19)
// Lmax   = parmlu(11)
// Umax   = parmlu(12)
// DUmax  = parmlu(13)
// DUmin  = parmlu(14)
// and      w(*).

// Lmax  and Umax  return the largest elements in L and U.
// DUmax and DUmin return the largest and smallest diagonals of U
// (excluding diagonals that are exactly zero).

// In general, w(j) is set to the maximum absolute element in
// the j-th column of U.  However, if the corresponding diagonal
// of U is small in absolute terms or relative to w(j)
// (as judged by the parameters Utol1, Utol2 respectively),
// then w(j) is changed to - w(j).

// Thus, if w(j) is not positive, the j-th column of A
// appears to be dependent on the other columns of A.
// The number of such columns, and the position of the last one,
// are returned as nsing and jsing.

// Note that nrank is assumed to be set already, and is not altered.
// Typically, nsing will satisfy      nrank + nsing = n,  but if
// Utol1 and Utol2 are rather large,  nsing > n - nrank   may occur.

// If keepLU = 0,
//              Lmax  and Umax  are already set by lu1fac.
//              The diagonals of U are in the top of A.
//              Only Utol1 is used in the singularity test to set w(*).

// inform = 0  if A appears to have full column rank (nsing = 0).
// inform = 1  otherwise (nsing > 0).

// 00 Jul 1987: Early version.
// 09 May 1988: f77 version.
// 11 Mar 2001: Allow for keepLU = 0.
// 17 Nov 2001: Briefer output for singular factors.
// 05 May 2002: Comma needed in format 1100 (via Kenneth Holmstrom).
// 06 May 2002: With keepLU = 0, diags of U are in natural order.
//              They were not being extracted correctly.
// 23 Apr 2004: TRP can judge singularity better by comparing
//              all diagonals to DUmax.
// 27 Jun 2004: (PEG) Allow write only if nout > 0 .
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

char mnkey;
boolean keepLU, TRP;
int i, j, jsing, jumin, k, l, l1, l2, ldiagU, lenL,
                  lprint, ndefic, nrank, nsing;
double aij, diag, DUmax, DUmin, Lmax, Umax, Utol1, Utol2;
final double zero = 0.0;


lprint = luparm[1];
TRP    = luparm[5] == 1;  // Threshold Rook Pivoting
keepLU = luparm[7] != 0;
nrank  = luparm[15];
lenL   = luparm[22];
Utol1  = parmlu[3];
Utol2  = parmlu[4];

inform[0] = 0;
Lmax   = zero;
Umax   = zero;
nsing  = 0;
jsing  = 0;
jumin  = 0;
DUmax  = zero;
DUmin  = 1.0E+30;
for (i = 0; i < n; i++) {
	w[i] = zero;
}


if (keepLU) {
// --------------------------------------------------------------
// Find  Lmax.
// --------------------------------------------------------------
for (l = lena + 1 - lenL; l <= lena; l++) {
 Lmax  = Math.max( Lmax, Math.abs(a[l-1]) );
}

// --------------------------------------------------------------
// Find Umax and set w(j) = maximum element in j-th column of U.
// --------------------------------------------------------------
for (k = 1; k <= nrank; k++) {
 i     = p[k-1];
 l1    = locr[i-1];
 l2    = l1 + lenr[i-1] - 1;

 for (l = l1; l <= l2; l++) {
    j     = indr[l-1];
    aij   = Math.abs( a[l-1] );
    w[j-1]  = Math.max( w[j-1], aij );
    Umax  = Math.max( Umax, aij );
 } // for (l = l1; l <= l2; l++) 
} // for (k = 1; k <= nrank; k++)

parmlu[11-1] = Lmax;
parmlu[12-1] = Umax;

// --------------------------------------------------------------
// Find DUmax and DUmin, the extreme diagonals of U.
// --------------------------------------------------------------
for (k = 1; k <= nrank; k++) {
 j      = q[k-1];
 i      = p[k-1];
 l1     = locr[i-1];
 diag   = Math.abs( a[l1-1] );
 DUmax  = Math.max( DUmax, diag );
 if (DUmin > diag) {
    DUmin  =   diag;
    jumin  =   j;
 } // if (DUmin > diag) 
} // for (k = 1; k <= nrank; k++)
} // if (keepLU)
else { // !keepLU
// --------------------------------------------------------------
// keepLU = 0.
// Only diag(U) is stored.  Set w(*) accordingly.
// Find DUmax and DUmin, the extreme diagonals of U.
// --------------------------------------------------------------
ldiagU = lena - n;

for (k = 1; k <= nrank; k++) {
 j      = q[k-1];
// diag   = abs( a(ldiagU + k) ) ! 06 May 2002: Diags
 diag   = Math.abs( a[ldiagU + j-1] ); // are in natural order
 w[j-1]   = diag;
 DUmax  = Math.max( DUmax, diag );
 if (DUmin > diag) {
    DUmin  =   diag;
    jumin  =   j;
 } // if (DUmin > diag)
} // for (k = 1; k <= nrank; k++)
} // else !keepLU


// --------------------------------------------------------------
// Negate w(j) if the corresponding diagonal of U is
// too small in absolute terms or relative to the other elements
// in the same column of  U.

// 23 Apr 2004: TRP ensures that diags are NOT small relative to
//              other elements in their own column.
//              Much better, we can compare all diags to DUmax.
// --------------------------------------------------------------
if (mode == 1 &&  TRP) {
Utol1 = Math.max( Utol1, Utol2*DUmax );
}

if (keepLU) {
for (k = 1; k <= n; k++) {
 j     = q[k-1];
 if (k > nrank) {
    diag   = zero;
 }
 else {
    i      = p[k-1];
    l1     = locr[i-1];
    diag   = Math.abs( a[l1-1] );
 }

 if (diag <= Utol1 ||  diag <= Utol2*w[j-1]) {
    nsing  =   nsing + 1;
    jsing  =   j;
    w[j-1]   = - w[j-1];
 } // if (diag <= Utol1 ||  diag <= Utol2*w[j-1])
} // for (k = 1; k <= n; k++)
} // if (keepLU)
else { // !keepLU

for (k = 1; k <= n; k++) {
 j      = q[k-1];
 diag   = w[j-1];

 if (diag <= Utol1) {
    nsing  =   nsing + 1;
    jsing  =   j;
    w[j-1]   = - w[j-1];
 } // if (diag <= Utol1)
} // for (k = 1; k <= n; k++) 
} // else !keepLU


// -----------------------------------------------------------------
// Set output parameters.
// -----------------------------------------------------------------
if (jumin == 0) DUmin = zero;
luparm[10] = nsing;
luparm[11] = jsing;
luparm[18] = jumin;
parmlu[12] = DUmax;
parmlu[13] = DUmin;

if (nsing > 0) {  // The matrix has been judged singular.
inform[0] = 1;
ndefic = n - nrank;
if (lprint >= 0) {
 if (m > n) {
    mnkey  = '>';
 }
 else if (m == n) {
    mnkey  = '=';
 }
 else {
    mnkey  = '<';
 }
 UI.setDataText("Singular m " + mnkey + " n " + " rank = " + nrank + "n-rank = " + ndefic + " nsing = " + nsing + "\n");
} // if (lprint >= 0)
} // if (nsing > 0)

// Exit.

luparm[9] = inform[0];
return;
} // lu6chk


}