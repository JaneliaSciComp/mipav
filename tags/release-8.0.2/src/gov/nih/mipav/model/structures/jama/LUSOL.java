package gov.nih.mipav.model.structures.jama;


import java.io.RandomAccessFile;
import java.text.DecimalFormat;
import java.util.Random;

import gov.nih.mipav.view.MipavUtil;
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
	private long seed = 1234567L;
	private Random rn = new Random(seed);
	private boolean doubleCheck = true;
	
	public LUSOL() {
		
	}
	
	/*private void simpleTest() {
		  final int maxm = 100000;
		  final int maxn = 100000;
			
		  double rhsval[] = new double[maxm];
		  double xexact[] = new double[maxn];

		  // Storage for LUSOL

		  final int lena  = 10000000;
		  double parmlu[] = new double[30];
		  double a[] = new double[lena];
		  double w[] = new double[maxn];
		  int luparm[] = new int[30];
		  int indc[] = new int[lena];
		  int indr[] = new int[lena];
		  int lenc[] = new int[maxn];
		  int lenr[] = new int[maxm];
		  int p[] = new int[maxm];
		  int q[] = new int[maxn];
		  int iploc[] = new int[maxn];
		  int iqloc[] = new int[maxm];
		  int ipinv[] = new int[maxm];
		  int iqinv[] = new int[maxn];
		  int locc[] = new int[maxn];
		  int locr[] = new int[maxm];

		  // Local storage

		  int inform[] = new int[1];
		  int itn, iprint, k     , lenL  , lenU,
		      m     , maxitn, mode  , 
		      n     , nelem , nnzero, Scale , TPiv;
		  // double bnorm;
		  // double xenorm;
		  double factol, dxnorm, enorm , rnorm , rnorm0,
		         snorm , xnorm;	
	}*/
	
	// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// File        testflat.f90
	//
	//             testflat (main program)
	//
	// Contains    Aprod, dnormj, jdamax
	//
	// This program reads data (i, j, Aij) from a flat text file
	// and solves a system Ax = b using LUSOL.
	//
	// 06 Apr 2002: First version of testlusol.f for SIAM Optimization 2002.
	//              Michael Saunders, Dept of MS&E, Stanford University.
	//              Derived from testma48.f.
	//              Data is read in Harwell-Boeing format.
	// 23 Apr 1998: Iterative refinement added.
	// 20 Mar 2000: ||A|| = 1.  dnrmx (infinity norm) used everywhere.
	//              All printed norms are absolute, not relative.
	// 09 Mar 2013: Converted to f90.
	// 10 Jun 2013: testflat derived from testlusol.f to debug lu1fac on
	//              nick424.txt, a 424 x 424 example from Nick Sahinidis.
	//              To compile and run:
	//   gfortran -g -o flat90 snModulePrecision.f90 sn28lusol.f90 testflat.f90
	//   ./runflat 90 toymat
	//   ./runflat 90 nick424
	// -----------------------------------------------------------------------

	public void testflat() {
		
	// The FORTRAN runs gave:
//		 ip = 8     rp = 8
//		 ----------------------------------------------------
//		 Problem    C:/LUSOL/lusol-master/src/testlusolflat/toymat
//		         0  Scale:  0=Noscale  1=Scale
//		    2.0000  factol: Max Lij (> 1.0)
//		         0  TPiv:   0=TPP      1=TRP      2=TCP


//		 A rows         5    A cols         5    A nonz         9
//		 L nonz         0    U nonz         9    L+U            9
//		 Factor      0.00    Solve       0.00

//		 snorm    0.0E+00    xnorm    1.0E+00
//		 Residual 0.0E+00    Error    0.0E+00

//		 Refine    dxnorm     rnorm     enorm
//		      1   0.0E+00   0.0E+00   0.0E+00
//		      2   0.0E+00   0.0E+00   0.0E+00
//		 Refine      0.00
	
//		ip = 8     rp = 8
//	 	----------------------------------------------------
//		 Problem    C:/LUSOL/lusol-master/src/testlusolflat/cavity05
//		         0  Scale:  0=Noscale  1=Scale
//		    2.0000  factol: Max Lij (> 1.0)
//		         0  TPiv:   0=TPP      1=TRP      2=TCP


//		 A rows      1182    A cols      1182    A nonz     32632
//		 L nonz     51529    U nonz     92570    L+U       144099
//		 Factor      0.19    Solve       0.00

//		 snorm    0.0E+00    xnorm    1.0E+00
//		 Residual 1.6E-12    Error    1.3E-10

//		 Refine    dxnorm     rnorm     enorm
//		      1   5.8E-10   1.2E-12   5.5E-10
//		      2   6.4E-10   1.2E-12   4.8E-10
//		 Refine      0.00
		
//		ip = 8     rp = 8
//		 ----------------------------------------------------
//		 Problem    C:/LUSOL/lusol-master/src/testlusolflat/toymat
//		         0  Scale:  0=Noscale  1=Scale
//		    2.0000  factol: Max Lij (> 1.0)
//		         1  TPiv:   0=TPP      1=TRP      2=TCP


//		 A rows         5    A cols         5    A nonz         9
//		 L nonz         0    U nonz         9    L+U            9
//		 Factor      0.00    Solve       0.00

//		 snorm    0.0E+00    xnorm    1.0E+00
//		 Residual 0.0E+00    Error    0.0E+00

//		 Refine    dxnorm     rnorm     enorm
//		      1   0.0E+00   0.0E+00   0.0E+00
//		      2   0.0E+00   0.0E+00   0.0E+00
//		 Refine      0.00
		
//		 ip = 8     rp = 8
//		 ----------------------------------------------------
//		 Problem    C:/LUSOL/lusol-master/src/testlusolflat/cavity05
//		         0  Scale:  0=Noscale  1=Scale
//		    2.0000  factol: Max Lij (> 1.0)
//		         1  TPiv:   0=TPP      1=TRP      2=TCP


//		 A rows      1182    A cols      1182    A nonz     32632
//		 L nonz    130882    U nonz    134741    L+U       265623
//		 Factor      2.39    Solve       0.00

//		 snorm    0.0E+00    xnorm    1.0E+00
//		 Residual 9.5E-12    Error    1.5E-08

//		 Refine    dxnorm     rnorm     enorm
//		      1   1.2E-08   5.0E-13   3.1E-09
//		      2   1.5E-13   3.1E-13   3.1E-09
//		 Refine      0.02
		
//		ip = 8     rp = 8
//		 ----------------------------------------------------
//		 Problem    C:/LUSOL/lusol-master/src/testlusolflat/toymat
//		         0  Scale:  0=Noscale  1=Scale
//		    2.0000  factol: Max Lij (> 1.0)
//		         2  TPiv:   0=TPP      1=TRP      2=TCP


//		 A rows         5    A cols         5    A nonz         9
//		 L nonz         0    U nonz         9    L+U            9
//		 Factor      0.00    Solve       0.00

//		 snorm    0.0E+00    xnorm    1.0E+00
//		 Residual 0.0E+00    Error    0.0E+00

//		 Refine    dxnorm     rnorm     enorm
//		      1   0.0E+00   0.0E+00   0.0E+00
//		      2   0.0E+00   0.0E+00   0.0E+00
//		 Refine      0.00
		
//		ip = 8     rp = 8
//		 ----------------------------------------------------
//		 Problem    C:/LUSOL/lusol-master/src/testlusolflat/cavity05
//		         0  Scale:  0=Noscale  1=Scale
//		    2.0000  factol: Max Lij (> 1.0)
//		         2  TPiv:   0=TPP      1=TRP      2=TCP


//		 A rows      1182    A cols      1182    A nonz     32632
//		 L nonz    132159    U nonz    135993    L+U       268152
//		 Factor      2.67    Solve       0.00

//		 snorm    0.0E+00    xnorm    1.0E+00
//		 Residual 1.9E-11    Error    1.2E-07

//		 Refine    dxnorm     rnorm     enorm
//		      1   1.2E-07   2.9E-13   8.6E-10
//		      2   0.0E+00   2.9E-13   8.6E-10
//		 Refine      0.00


	
	//  The Java runs gave:
//		fileName = toymat.txt
//		Scale = 0
//		factol = 2.0
//		TPP Threshold Partial Pivoting
//		A rows = 5 A cols = 5 A nonzero = 9
//		m = 5 mnkey = = n = 5
//		nelem = 9 Amax[0] = 1.00000E0 densty = 3.60000E1
//		avgmer = 0.00000E0
//		lenL[0] = 0
//		(lenL[0] + lenU[0]) = 9
//		ncp = 0
//		dincr = 0.00000E0
//		nUtri[0] = 5
//		lenU[0] = 9
//		Ltol = 2.00000E0
//		Umax[0] = 1.00000E0
//		Ugrwth = 1.00000E0
//		nLtri[0] = 0
//		ndens1[0] = 0
//		Lmax[0] = 0.00000E0
//		nbump = 0
//		ndens2 = 0
//		DUmax[0] = 1.00000E0
//		DUmin[0] = 1.00000E0
//		condU = 1.00000E0
//		L  nonz = 0 U nonz = 9 L+U = 9
//		Factor time = 3 Solve time = 0
//		snorm = 0.00000E0 xnorm = 1.00000E0 Residual = 0.00000E0 Error = 0.00000E0
//		Refine   dxnorm     rnorm     enorm
//		1 0.00000E0 0.00000E0 0.00000E0
//		2 0.00000E0 0.00000E0 0.00000E0
//		Refine time = 1
		


		
//		fileName = cavity05.txt
//		Scale = 0
//		factol = 2.0
//		TPP Threshold Partial Pivoting
//		A rows = 1182 A cols = 1182 A nonzero = 32632
//		m = 1182 mnkey = = n = 1182
//		nelem = 32632 Amax[0] = 8.53333E0 densty = 2.33566E0
//		avgmer = 1.20603E3
//		lenL[0] = 51529
//		(lenL[0] + lenU[0]) = 144099
//		ncp = 3
//		dincr = 3.41588E2
//		nUtri[0] = 161
//		lenU[0] = 92570
//		Ltol = 2.00000E0
//		Umax[0] = 2.68928E1
//		Ugrwth = 3.15150E0
//		nLtri[0] = 0
//		ndens1[0] = 411
//		Lmax[0] = 2.00000E0
//		nbump = 1021
//		ndens2 = 341
//		DUmax[0] = 1.18537E1
//		DUmin[0] = 1.13314E-3
//		condU = 1.04610E4
//		L  nonz = 51529 U nonz = 92570 L+U = 144099
//		Factor time = 321 Solve time = 5
//		snorm = 0.00000E0 xnorm = 1.00000E0 Residual = 1.55920E-12 Error = 1.41688E-10
//		Refine   dxnorm     rnorm     enorm
//		1 7.82940E-10 9.79966E-13 7.68187E-10
//		2 1.04665E-9 1.09848E-12 7.26515E-10
//		Refine time = 10
		
//		fileName = toymat.txt
//		Scale = 0
//		factol = 2.0
//		TRP Threshold Rook Pivoting
//		A rows = 5 A cols = 5 A nonzero = 9
//		m = 5 mnkey = = n = 5
//		nelem = 9 Amax[0] = 1.00000E0 densty = 3.60000E1
//		kPiv[lPiv] = RP
//		avgmer = 0.00000E0
//		lenL[0] = 0
//		(lenL[0] + lenU[0]) = 9
//		ncp = 0
//		dincr = 0.00000E0
//		nUtri[0] = 5
//		lenU[0] = 9
//		Ltol = 2.00000E0
//		Umax[0] = 1.00000E0
//		Ugrwth = 1.00000E0
//		nLtri[0] = 0
//		ndens1[0] = 0
//		Lmax[0] = 0.00000E0
//		Akmax[0] = 0.00000E0
//		Agrwth = 0.00000E0
//		nbump = 0
//		ndens2 = 0
//		DUmax[0] = 1.00000E0
//		DUmin[0] = 1.00000E0
//		condU = 1.00000E0
//		L  nonz = 0 U nonz = 9 L+U = 9
//		Factor time = 3 Solve time = 0
//		snorm = 0.00000E0 xnorm = 1.00000E0 Residual = 0.00000E0 Error = 0.00000E0
//		Refine   dxnorm     rnorm     enorm
//		1 0.00000E0 0.00000E0 0.00000E0
//		2 0.00000E0 0.00000E0 0.00000E0
//		Refine time = 1
		
//		fileName = cavity05.txt
//		Scale = 0
//		factol = 2.0
//		TRP Threshold Rook Pivoting
//		A rows = 1182 A cols = 1182 A nonzero = 32632
//		m = 1182 mnkey = = n = 1182
//		nelem = 32632 Amax[0] = 8.53333E0 densty = 2.33566E0
//		kPiv[lPiv] = RP
//		avgmer = 3.33012E3
//		lenL[0] = 130882
//		(lenL[0] + lenU[0]) = 265623
//		ncp = 6
//		dincr = 7.13995E2
//		nUtri[0] = 161
//		lenU[0] = 134741
//		Ltol = 2.00000E0
//		Umax[0] = 8.53333E0
//		Ugrwth = 1.00000E0
//		nLtri[0] = 0
//		ndens1[0] = 510
//		Lmax[0] = 1.99519E0
//		Akmax[0] = 0.00000E0
//		Agrwth = 0.00000E0
//		nbump = 1021
//		ndens2 = 445
//		DUmax[0] = 8.53333E0
//		DUmin[0] = 1.14047E-3
//		condU = 7.48229E3
//		L  nonz = 130882 U nonz = 134741 L+U = 265623
//		Factor time = 1065 Solve time = 9
//		snorm = 0.00000E0 xnorm = 1.00000E0 Residual = 9.47925E-12 Error = 1.51042E-8
//		Refine   dxnorm     rnorm     enorm
//		1 1.19625E-8 5.00683E-13 3.14449E-9
//		2 1.45983E-13 3.09735E-13 3.14449E-9
//		Refine time = 10

//		fileName = toymat.txt
//		Scale = 0
//		factol = 2.0
//		TCP Threshold Complete Pivoting
//		A rows = 5 A cols = 5 A nonzero = 9
//		m = 5 mnkey = = n = 5
//		nelem = 9 Amax[0] = 1.00000E0 densty = 3.60000E1
//		kPiv[lPiv] = CP
//		avgmer = 0.00000E0
//		lenL[0] = 0
//		(lenL[0] + lenU[0]) = 9
//		ncp = 0
//		dincr = 0.00000E0
//		nUtri[0] = 5
//		lenU[0] = 9
//		Ltol = 2.00000E0
//		Umax[0] = 1.00000E0
//		Ugrwth = 1.00000E0
//		nLtri[0] = 0
//		ndens1[0] = 0
//		Lmax[0] = 0.00000E0
//		Akmax[0] = 1.00000E0
//		Agrwth = 1.00000E0
//		nbump = 0
//		ndens2 = 0
//		DUmax[0] = 1.00000E0
//		DUmin[0] = 1.00000E0
//		condU = 1.00000E0
//		L  nonz = 0 U nonz = 9 L+U = 9
//		Factor time = 9 Solve time = 0
//		snorm = 0.00000E0 xnorm = 1.00000E0 Residual = 0.00000E0 Error = 0.00000E0
//		Refine   dxnorm     rnorm     enorm
//		1 0.00000E0 0.00000E0 0.00000E0
//		2 0.00000E0 0.00000E0 0.00000E0
//		Refine time = 1
		
//		fileName = cavity05.txt
//		Scale = 0
//		factol = 2.0
//		TCP Threshold Complete Pivoting
//		A rows = 1182 A cols = 1182 A nonzero = 32632
//		m = 1182 mnkey = = n = 1182
//		nelem = 32632 Amax[0] = 8.53333E0 densty = 2.33566E0
//		kPiv[lPiv] = CP
//		avgmer = 4.11292E3
//		lenL[0] = 132159
//		(lenL[0] + lenU[0]) = 268152
//		ncp = 9
//		dincr = 7.21746E2
//		nUtri[0] = 161
//		lenU[0] = 135993
//		Ltol = 2.00000E0
//		Umax[0] = 8.53333E0
//		Ugrwth = 1.00000E0
//		nLtri[0] = 0
//		ndens1[0] = 547
//		Lmax[0] = 5.09690E-1
//		Akmax[0] = 8.53333E0
//		Agrwth = 1.00000E0
//		nbump = 1021
//		ndens2 = 440
//		DUmax[0] = 8.53333E0
//		DUmin[0] = 1.14047E-3
//		condU = 7.48229E3
//		L  nonz = 132159 U nonz = 135993 L+U = 268152
//		Factor time = 1081 Solve time = 9
//		snorm = 0.00000E0 xnorm = 1.00000E0 Residual = 1.85128E-11 Error = 1.18321E-7
//		Refine   dxnorm     rnorm     enorm
//		1 1.17463E-7 2.91971E-13 8.64421E-10
//		2 0.00000E0 2.91971E-13 8.64421E-10
//		Refine time = 9



	// use   hbdataModule
	//  use   snModulePrecision,  only : ip, rp
	//  use   sn28lusol,          only : lu1fac, lu6sol

	//  implicit none

	//  intrinsic                     :: cpu_time

	  // Storage for data excluding the flat file

	  final int maxm = 100000;
	  final int maxn = 100000;
		
	  double rhsval[] = new double[maxm];
	  double xexact[] = new double[maxn];

	  // Storage for LUSOL

	  final int lena  = 10000000;
	  double parmlu[] = new double[30];
	  double a[] = new double[lena];
	  double w[] = new double[maxn];
	  int luparm[] = new int[30];
	  int indc[] = new int[lena];
	  int indr[] = new int[lena];
	  int lenc[] = new int[maxn];
	  int lenr[] = new int[maxm];
	  int p[] = new int[maxm];
	  int q[] = new int[maxn];
	  int iploc[] = new int[maxn];
	  int iqloc[] = new int[maxm];
	  int ipinv[] = new int[maxm];
	  int iqinv[] = new int[maxn];
	  int locc[] = new int[maxn];
	  int locr[] = new int[maxm];

	  // Local storage

	  int inform[] = new int[1];
	  int itn, iprint, k     , lenL  , lenU,
	      m     , maxitn, mode  , 
	      n     , nelem , nnzero, Scale , TPiv;
	  // double bnorm;
	  // double xenorm;
	  double factol, dxnorm, enorm , rnorm , rnorm0,
	         snorm , xnorm;
	  
	  long       time1 , time2 , timeF , timeR , timeS;
	  int indc2[] = new int[lena];
	  int indr2[] = new int[lena];  
	  // Extra copy of A for refinement
	  double A2[] = new double[lena];
	  double rhs[] = new double[maxm];
	  double r[] = new double[maxn];
	  double x[] = new double[maxn];
	  double dx[] = new double[maxn];
	  final double zero = 0.0;
	  final double one = 1.0;
	  int fileChoice = 2;
	  String fileDir;
	  String fileName = null;
	  RandomAccessFile raFile = null;
	  String line;
	  int i;
	  int start;

	  // ------------------------------------------------------------------
	  // Define files
	  // ------------------------------------------------------------------
	  iprint = 20;

	  fileDir = "C:/LUSOL/lusol-master/src/testlusolflat/";
	  if (fileChoice == 1) {
	      fileName = "toymat.txt";
	  }
	  else if (fileChoice == 2) {
	      fileName = "cavity05.txt";
	  }
	  UI.setDataText("fileName = " + fileName + "\n");
	
	  Scale = 0;      // 0=Noscale  1=Scale
	  factol = 2.0;    // > 1.0
	  TPiv  = 2;     // 0=TPP      1=TRP      2=TCP
	  UI.setDataText("Scale = " + Scale + "\n");
	  UI.setDataText("factol = " + factol + "\n");
	  if (TPiv == 0) {
		  UI.setDataText("TPP Threshold Partial Pivoting\n");
	  }
	  else if (TPiv == 1) {
		  UI.setDataText("TRP Threshold Rook Pivoting\n");  
	  }
	  else if (TPiv == 2) {
		  UI.setDataText("TCP Threshold Complete Pivoting\n");
	  }

	  // ------------------------------------------------------------------
	  // Read a matrix A from Probname.txt
	  // as a set of triples (i, j, Aij).
	  // A is loaded into the LUSOL input arrays (indc, indr, a).
	  //------------------------------------------------------------------
	  
	  k = 0;
	  try {

			raFile = new RandomAccessFile(fileDir + fileName, "r");
			while((line=raFile.readLine())!= null) {
				line = line.trim();
				if(!line.equals("")) {
					k++;
					if (k > lena) {
						MipavUtil.displayError("Too many entries, more than lena");
						raFile.close();
						return;
					}
					// Read i, j, Aij
					i = 0;
					while (!Character.isWhitespace(line.charAt(i))) {
						i++;
					}
					indc[k-1] = new Integer(line.substring(0,i)).intValue();
					while (Character.isWhitespace(line.charAt(i))) {
						i++;
					}
					start = i;
					while (!Character.isWhitespace(line.charAt(i))) {
						i++;
					}
					indr[k-1] = new Integer(line.substring(start, i)).intValue();
					while (Character.isWhitespace(line.charAt(i))) {
						i++;
					}
					a[k-1] = new Double(line.substring(i).trim()).doubleValue();
				}
			}
			raFile.close();
		}catch(Exception e) {
			try {
				if(raFile != null) {
					raFile.close();
				}
			}catch(Exception ex) {
				
			}
			e.printStackTrace();
			return;
		}

        nnzero = k;
	    m      = 0;
	    n      = 0;

	  for (k = 0; k < nnzero; k++) {
	     m = Math.max(m, indc[k]);
	     n = Math.max(n, indr[k]);
	  }

	  UI.setDataText("A rows = " + m + " A cols = " + n + " A nonzero = " + nnzero + "\n");
	  
	  for (k = 0; k < nnzero; k++) {
	      A2[k] = a[k];    // Save a copy of A for refinement
	      indc2[k] = indc[k];
	      indr2[k] = indr[k];
	  }

	  // ------------------------------------------------------------------
	  // Set xexact, then rhsval = A*xexact
	  // (over-riding any known HB rhs and solution).
	  // ------------------------------------------------------------------
	  for (k = 0; k < n; k++) {
	      xexact[k] = one;
	  }
	  
	  for (k = 0; k < m; k++) {
	      rhsval[k] = zero;
	  }

	  Aprod(m, n, nnzero, indc, indr, a, xexact, rhsval);

	  //bnorm  = dnormj( m, rhsval, 1);

	  // ------------------------------------------------------------------
	  // Set parameters for LUSOL's lu1fac.
	  // ------------------------------------------------------------------
	  luparm[0] = iprint;     // File number for printed messages
	  luparm[1] = 10;         // Print level. >= 0 to get singularity info.
	                          //              >=10 to get more LU statistics.
	                          //              >=50 to get info on each pivot.
	  luparm[2] = 5;          // maxcol
	  luparm[5] = TPiv;       // Threshold Pivoting: 0 = TPP, 1 = TRP, 2 = TCP
	  luparm[7] = 1;          // keepLU
	  parmlu[0] = factol;     // Ltol1:  max |Lij| during Factor
	  parmlu[1] = factol;     // Ltol2:  max |Lij| during Update 
	  parmlu[2] = 3.0e-13;    // small:  drop tolerance
	  parmlu[3] = 3.7e-11;    // Utol1:  absolute tol for small Uii
	  parmlu[4] = 3.7e-11;    // Utol2:  relative tol for small Uii
	  parmlu[5] = 3.0;        // Uspace: 
	  parmlu[6] = 0.3;        // dens1
	  parmlu[7] = 0.5;        // dens2
	  nelem     = nnzero;

	  // ------------------------------------------------------------------
	  // Factor  A = L U.
	  // ------------------------------------------------------------------
	  time1 = System.currentTimeMillis();
	  lu1fac( m    , n    , nelem, lena , luparm, parmlu,
	               a    , indc , indr , p    , q     , 
	               lenc , lenr , locc , locr ,           
	               iploc, iqloc, ipinv, iqinv, w     , inform );
	  time2 = System.currentTimeMillis();
	  timeF  = time2 - time1;

	  lenL   = luparm[20];
	  lenU   = luparm[21];
	  UI.setDataText("L  nonz = " + lenL + " U nonz = " + lenU + " L+U = " + (lenL + lenU) + "\n");

	  if (inform[0] > 1) {
	     UI.setDataText("lu1fac error inform[0] = " + inform[0] + "\n");

	     timeS  = 0L;
	     UI.setDataText("Factor time = " + timeF + " Solve time = " + timeS + "\n");
	     return;
	  }

	  // ------------------------------------------------------------------
	  // SOLVE  A x = rhs.
	  // ------------------------------------------------------------------
	  for (k = 0; k < m; k++) {
	      rhs[k] = rhsval[k];
	  }
	  mode   = 5;

	  time1 = System.currentTimeMillis();
	  lu6sol( mode, m, n, rhs, x, 
	               lena, luparm, parmlu,
	               a, indc, indr, p, q,
	               lenc, lenr, locc, locr,
	               inform );
	  time2 = System.currentTimeMillis();
	  timeS  = time2 - time1;

	  UI.setDataText("Factor time = " + timeF + " Solve time = " + timeS + "\n");

	  // ------------------------------------------------------------------
	  //  Set r = b - Ax.
	  // Find norms of r and error in x.
	  // ------------------------------------------------------------------
	  snorm  = zero;
	  xnorm  = dnormj( n, x, 1 );
	  //xenorm = dnormj( n, xexact, 1 );

	  for (k = 0; k < m; k++) {
	      r[k] = rhsval[k];
	  }
	  
	  for (k = 0; k < n; k++) {
	      w[k] = - x[k];
	  }

	  Aprod( m     , n     , nnzero,
	              indc2 , indr2 , A2,
	              w     , r     );

	  rnorm  = dnormj( m, r, 1);
	  for (k = 0; k < n; k++) {
	      w[k] = x[k] - xexact[k];
	  }
	  enorm  = dnormj( n, w, 1 );

	  UI.setDataText("snorm = " + nf.format(snorm) + " xnorm = " + nf.format(xnorm) + 
	  " Residual = " + nf.format(rnorm) + " Error = " + nf.format(enorm) + "\n");

	  // ------------------------------------------------------------------
	  // Iterative refinement.
	  // ------------------------------------------------------------------
	  time1 = System.currentTimeMillis();
	  UI.setDataText("Refine   dxnorm     rnorm     enorm\n");
	  rnorm0 = 1.0e+30;
	  maxitn = 10;

	  for (itn = 1; itn <= maxitn; itn++) {
	     // ---------------------------------------------------------------
	     //  Solve A dx = r.
	     // Set      x = x + dx.
	     // ---------------------------------------------------------------
	     lu6sol( mode, m, n, r, dx,
	                  lena, luparm, parmlu,
	                  a, indc, indr, p, q, 
	                  lenc, lenr, locc, locr,
	                  inform );
	     for (k = 0; k < n; k++) {
	         x[k] = x[k] + dx[k];
	     }
	     dxnorm = dnormj( n, dx, 1 );

	     // ---------------------------------------------------------------
	     // Set r = b - Ax.
	     // Find max residual.
	     // ---------------------------------------------------------------
	     for (k = 0; k < m; k++) {
	         r[k] = rhsval[k];
	     }
	     for (k = 0; k < n; k++) {
	         w[k] = - x[k];
	     }

	     Aprod( m     , n     , nnzero,
	                 indc2 , indr2 , A2,
	                 w     , r      );

	     rnorm  = dnormj( m, r, 1 );
	     for (k = 0; k < n; k++) {
	         w[k] = x[k] - xexact[k];
	     }
	     enorm  = dnormj( n, w, 1 );

	     UI.setDataText(itn + " " + nf.format(dxnorm) + " " + nf.format(rnorm) + " " + nf.format(enorm) + "\n");

	     if (rnorm >= 0.5 * rnorm0) {
	    	 break;
	     }
	     rnorm0 = rnorm;
	  } // for (itn = 1; itn <= maxitn; itn++)

	  time2 = System.currentTimeMillis();
	  timeR  = time2 - time1;
	  UI.setDataText("Refine time = " + timeR + "\n");

	  return;
	} // testflat
	
	private void Aprod(int m, int n, int nnzero,
            int indc[], int indr[], double a[],
            double x[], double y[]) {

//integer(ip), intent(in)    :: m, n, nnzero
//integer(ip), intent(in)    :: indc(nnzero), indr(nnzero)
//real(rp),    intent(in)    :: a(nnzero), x(n)
//real(rp),    intent(inout) :: y(m)

// Aprod computes y = y + A*x

int i, j, k;

for (k = 1; k <= nnzero; k++) {
i    = indc[k-1];
j    = indr[k-1];
y[i-1] = y[i-1] + a[k-1]*x[j-1];
}

	} // Aprod
	
	private double dnormj(int n, double x[], int incx ) {

    // integer(ip), intent(in)    :: n, incx
    // real(rp),    intent(in)    :: x(:)

    // ===========================================================================
    // norminf returns the infinity-norm of the vector  x  in most cases.
    // realmax is returned if x(*) contains any NaNs or Infs.
    //
    // 29 Jul 2003: First version of dnormj for use in s5setx.
    // 15 Mar 2008: First f90 version.
    // ===========================================================================
    int kmax;
    final double zero    = 0.0;
    final double realmax = Double.MAX_VALUE;

    kmax   = jdamax( n, x, incx );
    if (kmax == 0) {
       return zero;
    }
    else if (kmax > 0) {
       return Math.abs( x[kmax-1] );
    }
    else {
       return realmax;
    }

	} // dnormj



	
	private double random() {
	    double r, t1, t2;
	    double sprsty = 0.4;
	    double r1 = -100.0;
	    double r2 = 100.0;
	    t1 = rn.nextDouble();
	    t2 = rn.nextDouble();
	    if (t2 > sprsty) {
	    	r = r1 + (r2 - r1) * t1;
	    }
	    else {
	    	r = 0.0;
	    }
	    return r;
	}
	
	public void lutest_driver() {
		final int mxm = 20;
		final int mxn = 20;
		final int lena = 1000;
		final int nrowb = mxm;
		final int ncolb = 2 * mxn;
		double B[][] = new double[nrowb][ncolb];
		double d[] = new double[mxn];
		double x[] = new double[mxn];
		double y[] = new double[mxm];
		int kb[] = new int[mxn];
		int luparm[] = new int[30];
		double parmlu[] = new double[30];
		double a[] = new double[lena];
		double v[] = new double[mxm];
		double w[] = new double[mxn];
		int indc[] = new int[lena];
		int indr[] = new int[lena];
		int ip[] = new int[mxm];
		int iq[] = new int[mxn];
		int lenc[] = new int[mxn];
		int lenr[] = new int[mxm];
		int iploc[] = new int[mxm];
		int iqloc[] = new int[mxn];
		int locc[] = new int[mxn];
		int locr[] = new int[mxm];
		int ipinv[] = new int[mxm];
		int iqinv[] = new int[mxn];
		int inform[] = new int[1];
		int mtest;
		int m;
		int n;
		// Running the original FORTRAN with LUPARM(8) = 0 gave:
//		------------------------------------------
//		 LUTEST   1.      M =    20       N =    20
//		 ------------------------------------------

//		 LU1FAC...

//		 LUCHEK.      Rank =    20           LenL =    37     LenU =    39
//		   A - L*U   error =   1.1E+03      (A - L*U)(t) error =   1.1E+03
//		   Ax = b    error =   2.6E+07       A(t)y = b   error =   2.7E+07

//		 Column     1  replaced by column    40

//		 LUCHEK.      Rank =    20           LenL =    37     LenU =    59
//		   A - L*U   error =   1.6E+09      (A - L*U)(t) error =   1.6E+04
//		   Ax = b    error =   1.8E+08       A(t)y = b   error =   1.9E+08
//		 Fatal error in LUSOL lu7rnk.  Stopping now
		
		// Running the original FORTRAN with LUPARAM(8) = 1 gave:
//		------------------------------------------
//		 LUTEST   1.      M =    20       N =    20
//		 ------------------------------------------

//		 LU1FAC...

//		 LUCHEK.      Rank =    20           LenL =   190     LenU =   210
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   5.9E-14       A(t)y = b   error =   0.0E+00

//		 lu8rpc  warning.  Singularity after replacing column.    jrep =       1    diag =    0.00E+00

//		 Column     1  replaced by column    40

//		 LUCHEK.      Rank =    19           LenL =   209     LenU =   192
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   1.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     2  replaced by column    39

//		 LUCHEK.      Rank =    18           LenL =   227     LenU =   190
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   4.5E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column     3  replaced by column    38

//		 LUCHEK.      Rank =    17           LenL =   243     LenU =   187
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     4  replaced by column    37

//		 LUCHEK.      Rank =    16           LenL =   258     LenU =   182
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column     5  replaced by column    36

//.      Rank =    15           LenL =   272     LenU =   175
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     6  replaced by column    35

//		 LUCHEK.      Rank =    14           LenL =   285     LenU =   166
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     7  replaced by column    34

//		 LUCHEK.      Rank =    13           LenL =   297     LenU =   155
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   4.5E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     8  replaced by column    33

//		 LUCHEK.      Rank =    12           LenL =   308     LenU =   142
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     9  replaced by column    32

//		 LUCHEK.      Rank =    11           LenL =   318     LenU =   127
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.1E-13       A(t)y = b   error =   0.0E+00

//		 Column    10  replaced by column    31

//		 LUCHEK.      Rank =    10           LenL =   327     LenU =   110
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.1E-13       A(t)y = b   error =   0.0E+00

//		 Column    11  replaced by column    30

//		 LUCHEK.      Rank =    11           LenL =   335     LenU =    93
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column    12  replaced by column    29

//		 LUCHEK.      Rank =    12           LenL =   342     LenU =    78
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    13  replaced by column    28

//		 LUCHEK.      Rank =    13           LenL =   348     LenU =    65
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    14  replaced by column    27

//		 LUCHEK.      Rank =    14           LenL =   353     LenU =    54
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column    15  replaced by column    26

//		 LUCHEK.      Rank =    15           LenL =   357     LenU =    45
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    16  replaced by column    25

//		 LUCHEK.      Rank =    16           LenL =   360     LenU =    38
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    17  replaced by column    24

//		 LUCHEK.      Rank =    17           LenL =   362     LenU =    33
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    18  replaced by column    23

//		 LUCHEK.      Rank =    18           LenL =   363     LenU =    30
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   2.6E-13       A(t)y = b   error =   0.0E+00

//		 Column    19  replaced by column    22

//		 LUCHEK.      Rank =    19           LenL =   363     LenU =    29
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    20  replaced by column    21

//		 LUCHEK.      Rank =    20           LenL =   381     LenU =    30
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.2E-15       A(t)y = b   error =   0.0E+00


//		 ------------------------------------------
//		 LUTEST   2.      M =    20       N =    20
//		 ------------------------------------------

//		 LU1FAC...

//		 LUCHEK.      Rank =    20           LenL =   190     LenU =   210
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   5.9E-14       A(t)y = b   error =   0.0E+00

//		 lu8rpc  warning.  Singularity after replacing column.    jrep =       1    diag =    0.00E+00

//		 Column     1  replaced by column    40

//		 LUCHEK.      Rank =    19           LenL =   209     LenU =   192
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   1.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     2  replaced by column    39

//		 LUCHEK.      Rank =    18           LenL =   227     LenU =   190
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   4.5E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column     3  replaced by column    38

//		 LUCHEK.      Rank =    17           LenL =   243     LenU =   187
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     4  replaced by column    37

//		 LUCHEK.      Rank =    16           LenL =   258     LenU =   182
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column     5  replaced by column    36

//		 LUCHEK.      Rank =    15           LenL =   272     LenU =   175
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     6  replaced by column    35

//		 LUCHEK.      Rank =    14           LenL =   285     LenU =   166
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     7  replaced by column    34

//		 LUCHEK.      Rank =    13           LenL =   297     LenU =   155
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   4.5E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     8  replaced by column    33

//		 LUCHEK.      Rank =    12           LenL =   308     LenU =   142
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column     9  replaced by column    32

//		 LUCHEK.      Rank =    11           LenL =   318     LenU =   127
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.1E-13       A(t)y = b   error =   0.0E+00

//		 Column    10  replaced by column    31

//		 LUCHEK.      Rank =    10           LenL =   327     LenU =   110
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.1E-13       A(t)y = b   error =   0.0E+00

//		 Column    11  replaced by column    30

//		 LUCHEK.      Rank =    11           LenL =   335     LenU =    93
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column    12  replaced by column    29

//		 LUCHEK.      Rank =    12           LenL =   342     LenU =    78
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    13  replaced by column    28

//		 LUCHEK.      Rank =    13           LenL =   348     LenU =    65
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    14  replaced by column    27

//		 LUCHEK.      Rank =    14           LenL =   353     LenU =    54
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.2E-14       A(t)y = b   error =   0.0E+00

//		 Column    15  replaced by column    26

//		 LUCHEK.      Rank =    15           LenL =   357     LenU =    45
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    16  replaced by column    25

//		 LUCHEK.      Rank =    16           LenL =   360     LenU =    38
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    17  replaced by column    24

//		 LUCHEK.      Rank =    17           LenL =   362     LenU =    33
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    18  replaced by column    23

//		 LUCHEK.      Rank =    18           LenL =   363     LenU =    30
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   2.6E-13       A(t)y = b   error =   0.0E+00

//		 Column    19  replaced by column    22

//		 LUCHEK.      Rank =    19           LenL =   363     LenU =    29
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    20  replaced by column    21

//		 LUCHEK.      Rank =    20           LenL =   381     LenU =    30
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.2E-15       A(t)y = b   error =   0.0E+00

//		 --------------------------------------------------------
//		 LUTEST.  Maximum error after   2  tests =   4.5E-13
//		 --------------------------------------------------------


//		 ------------------------------------------
//		 LUTEST   1.      M =    20       N =    10
//		 ------------------------------------------

//		 LU1FAC...

//		 LUCHEK.      Rank =    10           LenL =   145     LenU =    55
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   4.9E-13       A(t)y = b   error =   0.0E+00

//		 lu8rpc  warning.  Singularity after replacing column.    jrep =       1    diag =    0.00E+00

//		 Column     1  replaced by column    20

//		 LUCHEK.      Rank =     9           LenL =   154     LenU =    47
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   1.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     2  replaced by column    19

//		 LUCHEK.      Rank =     8           LenL =   162     LenU =    45
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.5E-13       A(t)y = b   error =   0.0E+00

//		 Column     3  replaced by column    18

//		 LUCHEK.      Rank =     7           LenL =   168     LenU =    42
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     4  replaced by column    17

//		 LUCHEK.      Rank =     6           LenL =   173     LenU =    37
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     5  replaced by column    16

//		 LUCHEK.      Rank =     5           LenL =   177     LenU =    30
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     6  replaced by column    15

//		 LUCHEK.      Rank =     6           LenL =   180     LenU =    23
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     7  replaced by column    14

//		 LUCHEK.      Rank =     7           LenL =   182     LenU =    18
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     8  replaced by column    13

//		 LUCHEK.      Rank =     8           LenL =   183     LenU =    15
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   2.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     9  replaced by column    12

//		 LUCHEK.      Rank =     9           LenL =   183     LenU =    14
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column    10  replaced by column    11

//		 LUCHEK.      Rank =    10           LenL =   191     LenU =    15
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   7.4E-15       A(t)y = b   error =   0.0E+00


//		 ------------------------------------------
//		 LUTEST   2.      M =    20       N =    10
//		 ------------------------------------------

//		 LU1FAC...

//		 LUCHEK.      Rank =    10           LenL =   145     LenU =    55
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   4.9E-13       A(t)y = b   error =   0.0E+00

//		 lu8rpc  warning.  Singularity after replacing column.    jrep =       1    diag =    0.00E+00

//		 Column     1  replaced by column    20

//		 LUCHEK.      Rank =     9           LenL =   154     LenU =    47
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   1.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     2  replaced by column    19

//		 LUCHEK.      Rank =     8           LenL =   162     LenU =    45
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.5E-13       A(t)y = b   error =   0.0E+00

//		 Column     3  replaced by column    18

//		 LUCHEK.      Rank =     7           LenL =   168     LenU =    42
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   2.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     4  replaced by column    17

//		 LUCHEK.      Rank =     6           LenL =   173     LenU =    37
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     5  replaced by column    16

//		 LUCHEK.      Rank =     5           LenL =   177     LenU =    30
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     6  replaced by column    15

//		 LUCHEK.      Rank =     6           LenL =   180     LenU =    23
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     7  replaced by column    14

//		 LUCHEK.      Rank =     7           LenL =   182     LenU =    18
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column     8  replaced by column    13

//		 LUCHEK.      Rank =     8           LenL =   183     LenU =    15
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   2.3E-13       A(t)y = b   error =   0.0E+00

//		 Column     9  replaced by column    12

//		 LUCHEK.      Rank =     9           LenL =   183     LenU =    14
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   6.7E-15       A(t)y = b   error =   0.0E+00

//		 Column    10  replaced by column    11

//		 LUCHEK.      Rank =    10           LenL =   191     LenU =    15
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   7.4E-15       A(t)y = b   error =   0.0E+00

//		 --------------------------------------------------------
//		 LUTEST.  Maximum error after   2  tests =   4.9E-13
//		 --------------------------------------------------------


//		 ------------------------------------------
//		 LUTEST   1.      M =    10       N =    20
//		 ------------------------------------------

//		 LU1FAC...
//		 Singular(m<n)  rank       10  n-rank      10  nsing       10

//		 LUCHEK.      Rank =    10           LenL =    24     LenU =    71
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   1.3E-13       A(t)y = b   error =   6.1E-05

//		 Column     1  replaced by column    40

//		 LUCHEK.      Rank =    10           LenL =    26     LenU =    54
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.4E-14       A(t)y = b   error =   0.0E+00

//		 Column     2  replaced by column    39

//		 LUCHEK.      Rank =     9           LenL =    28     LenU =    35
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.9E-14       A(t)y = b   error =   0.0E+00

//		 Column     3  replaced by column    38

//		 LUCHEK.      Rank =     8           LenL =    29     LenU =    33
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.3E-14       A(t)y = b   error =   0.0E+00

//		 Column     4  replaced by column    37

//		 LUCHEK.      Rank =     7           LenL =    30     LenU =    31
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     5  replaced by column    36

//		 LUCHEK.      Rank =     6           LenL =    31     LenU =    29
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     6  replaced by column    35

//		 LUCHEK.      Rank =     5           LenL =    32     LenU =    27
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     7  replaced by column    34

//		 LUCHEK.      Rank =     4           LenL =    33     LenU =    25
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     8  replaced by column    33

//		 LUCHEK.      Rank =     3           LenL =    34     LenU =    23
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     9  replaced by column    32

//		 LUCHEK.      Rank =     2           LenL =    35     LenU =    21
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column    10  replaced by column    31

//		 LUCHEK.      Rank =     1           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column    11  replaced by column    30

//		 LUCHEK.      Rank =     2           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   2.7E-14       A(t)y = b   error =   0.0E+00

//		 Column    12  replaced by column    29

//		 LUCHEK.      Rank =     3           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   5.6E-14       A(t)y = b   error =   0.0E+00

//		 Column    13  replaced by column    28

//		 LUCHEK.      Rank =     4           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    14  replaced by column    27

//		 LUCHEK.      Rank =     5           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.9E-14       A(t)y = b   error =   0.0E+00

//		 Column    15  replaced by column    26

//		 LUCHEK.      Rank =     6           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.5E-14       A(t)y = b   error =   0.0E+00

//		 Column    16  replaced by column    25

//		 LUCHEK.      Rank =     7           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   1.3E-14       A(t)y = b   error =   0.0E+00

//		 Column    17  replaced by column    24

//		 LUCHEK.      Rank =     8           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.6E-14       A(t)y = b   error =   0.0E+00

//		 Column    18  replaced by column    23

//		 LUCHEK.      Rank =     9           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    19  replaced by column    22

//		 LUCHEK.      Rank =    10           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   1.2E-14       A(t)y = b   error =   0.0E+00

//		 Column    20  replaced by column    21

//		 LUCHEK.      Rank =    10           LenL =    44     LenU =    38
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   1.1E-13       A(t)y = b   error =   0.0E+00


//		 ------------------------------------------
//		 LUTEST   2.      M =    10       N =    20
//		 ------------------------------------------

//		 LU1FAC...
//		 Singular(m<n)  rank       10  n-rank      10  nsing       10

//		 LUCHEK.      Rank =    10           LenL =    24     LenU =    71
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   2.3E-13
//		   Ax = b    error =   1.3E-13       A(t)y = b   error =   6.1E-05

//		 Column     1  replaced by column    40

//		 LUCHEK.      Rank =    10           LenL =    26     LenU =    54
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.4E-14       A(t)y = b   error =   0.0E+00

//		 Column     2  replaced by column    39

//		 LUCHEK.      Rank =     9           LenL =    28     LenU =    35
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.9E-14       A(t)y = b   error =   0.0E+00

//		 Column     3  replaced by column    38

//		 LUCHEK.      Rank =     8           LenL =    29     LenU =    33
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.3E-14       A(t)y = b   error =   0.0E+00

//		 Column     4  replaced by column    37

//		 LUCHEK.      Rank =     7           LenL =    30     LenU =    31
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     5  replaced by column    36

//		 LUCHEK.      Rank =     6           LenL =    31     LenU =    29
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     6  replaced by column    35

//		 LUCHEK.      Rank =     5           LenL =    32     LenU =    27
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     7  replaced by column    34

//		 LUCHEK.      Rank =     4           LenL =    33     LenU =    25
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     8  replaced by column    33

//		 LUCHEK.      Rank =     3           LenL =    34     LenU =    23
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column     9  replaced by column    32

//		 LUCHEK.      Rank =     2           LenL =    35     LenU =    21
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column    10  replaced by column    31

//		 LUCHEK.      Rank =     1           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.0E-14       A(t)y = b   error =   0.0E+00

//		 Column    11  replaced by column    30

//		 LUCHEK.      Rank =     2           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   2.7E-14       A(t)y = b   error =   0.0E+00

//		 Column    12  replaced by column    29

//		 LUCHEK.      Rank =     3           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   5.6E-14       A(t)y = b   error =   0.0E+00

//		 Column    13  replaced by column    28

//		 LUCHEK.      Rank =     4           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.1E-14       A(t)y = b   error =   0.0E+00

//		 Column    14  replaced by column    27

//		 LUCHEK.      Rank =     5           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.9E-14       A(t)y = b   error =   0.0E+00

//		 Column    15  replaced by column    26

//		 LUCHEK.      Rank =     6           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.5E-14       A(t)y = b   error =   0.0E+00

//		 Column    16  replaced by column    25

//		 LUCHEK.      Rank =     7           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   1.3E-14       A(t)y = b   error =   0.0E+00

//		 Column    17  replaced by column    24

//		 LUCHEK.      Rank =     8           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   4.6E-14       A(t)y = b   error =   0.0E+00

//		 Column    18  replaced by column    23

//		 LUCHEK.      Rank =     9           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   3.1E-14       A(t)y = b   error =   0.0E+00
//
//		 Column    19  replaced by column    22

//		 LUCHEK.      Rank =    10           LenL =    35     LenU =    20
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   1.2E-14       A(t)y = b   error =   0.0E+00

//		 Column    20  replaced by column    21

//		 LUCHEK.      Rank =    10           LenL =    44     LenU =    38
//		   A - L*U   error =   0.0E+00      (A - L*U)(t) error =   0.0E+00
//		   Ax = b    error =   1.1E-13       A(t)y = b   error =   0.0E+00

//		 --------------------------------------------------------
//		 LUTEST.  Maximum error after   2  tests =   6.1E-05
//		 --------------------------------------------------------

		
		//     ------------------------------------------------------------------
		//     Test program for LUSOL.
		
		//     25 Sep 1987:  First version.
		//     LUTEST is asked to do MTEST tests for each specified M and N.
		//     ------------------------------------------------------------------

		//     Initialize MTEST and the random number sequence.

		      mtest  =  2;

		//     Test square systems.

		      m      = mxm;
		      n      = mxn;

		      lutest( m, n, nrowb, ncolb, mtest, inform,
		              kb, B, d, v, w, x, y,
		              lena, luparm, parmlu,
		              a, indc, indr, ip, iq,
		              lenc, lenr, locc, locr, iploc, iqloc,
		              ipinv, iqinv);

			 if (inform[0] > 2) {
			      return;
		      }

		//     More rows than columns.

		      m      = mxm;
		      n      = mxn / 2;

		      lutest( m, n, nrowb, ncolb, mtest, inform,
		              kb, B, d, v, w, x, y,
		              lena, luparm, parmlu,
		              a, indc, indr, ip, iq,
		              lenc, lenr, locc, locr, iploc, iqloc,
		              ipinv, iqinv);

		      if (inform[0] > 2) {
			      return;
		      }

		//     More columns than rows.

		      m      = mxm / 2;
		      n      = mxn;

		      lutest( m, n, nrowb, ncolb, mtest, inform,
		              kb, B, d, v, w, x, y,
		              lena, luparm, parmlu,
		              a, indc, indr, ip, iq,
		              lenc, lenr, locc, locr, iploc, iqloc,
		              ipinv, iqinv);
		      
		     return;
	} // lutest_driver
	
	private void lutest(int m, int n, int nrowb, int ncolb, int mtest, int inform[],
		          int kb[], double B[][], double d[], double v[], double w[], double x[], double y[],
		          int lena, int luparm[], double parmlu[],
		          double a[], int indc[], int indr[], int ip[], int iq[],
		          int lenc[], int lenr[], int locc[], int locr[], int iploc[], int iqloc[],
		          int ipinv[], int iqinv[]) {

		      //IMPLICIT		 DOUBLE PRECISION (A-H,O-Z)

		      //INTEGER   	 KB(N)
		      //DOUBLE PRECISION	 B(NROWB,NCOLB), D(N), X(N), Y(M)

		      //INTEGER            LUPARM(30)
		      //DOUBLE PRECISION   PARMLU(30), A(LENA), V(M), W(N)
		      //INTEGER*2          INDC(LENA), INDR(LENA), IP(M), IQ(N)
		      //INTEGER*2          LENC(N), LENR(M), IPLOC(M), IQLOC(N)
		      //INTEGER            LOCC(N), LOCR(M)

		//     ------------------------------------------------------------------
		//     LUTEST is a test program for LUSOL.
		
		//     Jul 31 1987: Test LU1FAC, LU6MUL and LU8RPC.
		//     Sep 25 1987: Test LU2FAC, LU6SOL also.
		//     ------------------------------------------------------------------
		      int nelem[] = new int[1];
		      int i, j, jnew, jrep, ntest;
		      double diag[] = new double[1];
		      double emax[] = new double[1];
		      double vnorm[] = new double[1];
              double errmax;
              double arr[][];
		      errmax = 0.0;

		      for (ntest = 1;  ntest <= mtest; ntest++) {

		         UI.setDataText("lutest ntest = " + ntest + " m = " + m + " n = " + n + "\n");
		    	 for (i = 0; i < nrowb; i++) {
		    		 for (j = 0; j < ncolb; j++) {
		    			 B[i][j] = 0.0;
		    		 }
		    	 }
		         setmat( m, n, nrowb, B, d, v, w);
		         arr = new double[nrowb][n];
		         setmat( m, n, nrowb, arr, d, v, w);
		         for (i = 0; i < nrowb; i++) {
		        	 for (j = 0; j < n; j++) {
		        		 B[i][j+n] = arr[i][j];
		        	 }
		         }

		//        ---------------------------------------------------------------
		//        Test LU1FAC.
		//        ---------------------------------------------------------------

		         UI.setDataText("Testing lu1fac\n");

		         luinit( m, n, nrowb, B, kb,
		                 lena, luparm, parmlu,
		                 nelem, a, indc, indr);

		         lu1fac( m, n, nelem[0],
		                 lena, luparm, parmlu,
		                 a, indc, indr, ip, iq,
		                 lenc, lenr, locc, locr,
		                 iploc, iqloc, ipinv, iqinv, w, inform);

		         if (inform[0] > 1) {
		        	 UI.setDataText("Error in lu1fac inform[0] = " + inform[0] + "\n");
		        	 return;
		         }

		         luchek( m, n, nrowb, B, kb, emax, v, w, x, y,
		                 lena, luparm, parmlu,
		                 a, indc, indr, ip, iq,
		                 lenc, lenr, locc, locr);

		         errmax = Math.max(errmax, emax[0]);

		
		//        ---------------------------------------------------------------
		//        Replace columns of the LU one by one.
		//        ---------------------------------------------------------------

		         // Stop if nrank[0] <= 0
		         // If nrank[0] = 0 lu8rpc crashes on q[krep[0]-1] = q[nrank[0]-1];
		         // If you increase the size of arrays v, w, ip, iq, and lenr by 1
		         // and increase their indices by 1, then lu8rpc no longer crashes on
		         // nrank[0] = 0 but it then crashes on nrank[0] = -1
		         // Running the original FORTRAN with LUPARM(8) = 1 and ISEED = 1 gives:
		         // At entry to lu8rpc nrank =      -19

		         // mode1 =        1 mode2 =        1 jrep =       20

		         // At q(krep) = q(nrank) nrank =      -19
                 // After q(nrank) = jrep:
		         // q(nrank) =       20
		         // so the FORTRAN 90 allows an nrank array index of -19 in q and still keeps
		         // running, but this cannot be tolerated by Java.
		         for (j = 1; j <= n && luparm[15] > 0; j++) {
		            jrep = j;
		            jnew  = 2*n + 1 - j;
		            kb[jrep-1] = jnew;

		            for (i = 1; i <= m; i++) {
		               v[i-1] = B[i-1][jnew-1];
		            } // for (i = 1; i <= m; i++)

		            lu8rpc( 1, 1, m, n, jrep, v, w,
		                    lena, luparm, parmlu,
		                    a, indc, indr, ip, iq,
		                    lenc, lenr, locc, locr,
		                    inform, diag, vnorm);

		            UI.setDataText("Column " + jrep + " replaced by column " + jnew + "\n");
		            if (inform[0] > 2) {
		            	UI.setDataText("Error in lu8rpc inform[0] = " + inform[0] + "\n");
		            	return;
		            }
		            

		            luchek( m, n, nrowb, B, kb, emax, v, w, x, y,
		                    lena, luparm, parmlu,
		                    a, indc, indr, ip, iq,
		                    lenc, lenr, locc, locr);

		            errmax = Math.max(errmax, emax[0]);
		        } // for (j = 1; j <= n; j++)
		      } // for (ntest = 1;  ntest <= mtest; ntest++)
		      
		      UI.setDataText("Maximum error after " + mtest + " tests = " + nf.format(errmax) + "\n");
		      return;
} // lutest

	
	private void setmat(int m, int n, int nrowb, double B[][], double d[], double v[], double w[]) {
    //IMPLICIT		 DOUBLE PRECISION (A-H,O-Z)
    //DOUBLE PRECISION   B(NROWB,N), D(N), V(M), W(N)

//     SETMAT sets   B  =  D  +  vw'
//     where D = diag(d) and d, v, w are random vectors.
		int i, j;
		double wj;

    for (i = 0; i < m; i++) {
       v[i] = random();
    }

    for (j = 0; j < n; j++) {
    	d[j] = random();
    	w[j] = random();
    }
    
    for (j = 0; j < n; j++) {
        wj = w[j];
        for (i = 0; i < m; i++) {
        	B[i][j] = v[i] * wj;
        }
        B[j][j] = B[j][j] + d[j];
    }
    
    return;
	} // setmat

	
	private void luinit( int m, int n, int nrowb, double B[][], int kb[],
                         int lena, int luparm[], double parmlu[],
                         int nelem[], double a[], int indc[], int indr[]) {

		      //IMPLICIT           DOUBLE PRECISION(A-H,O-Z)
		      //INTEGER            LUPARM(30), KB(N)
		      //DOUBLE PRECISION   PARMLU(30), A(LENA), B(NROWB,N)
		      //INTEGER*2          INDC(LENA), INDR(LENA)
		int i, j;

		      luparm[0] = 6;
		      luparm[1] = 0;
		      luparm[2] = 10;
		      luparm[7] = 1;
		      parmlu[0] = 10.0;
		      parmlu[1] = 10.0;
		      parmlu[2] = 1.0E-13;
 		      parmlu[3] = 1.0E-11;
		      parmlu[4] = 1.0E-11;
		      parmlu[5] = 3.0;
		      nelem[0]     = 0;

		      for (j = 1; j <= n; j++) {
		         kb[j-1]   = j;
		         for (i = 1; i <= m; i++) {
			        if (B[i-1][j-1] != 0.0) {
		               nelem[0] = nelem[0] + 1;
		               a[nelem[0]-1] = B[i-1][j-1];
			           indc[nelem[0]-1] = i;
			           indr[nelem[0]-1] = j;
			        } // if (B[i-1][j-1] != 0.0)
		         } // for (i = 1; i <= m; i++)
		      } // for (j = 1; j <= n; j++)
		      return;

	} // luinit
	
	private void luchek(int m, int n, int nrowb, double B[][], int kb[], double emax[], double v[], double w[], double x[], double y[],
	                    int lena, int luparm[], double parmlu[],
	                    double a[], int indc[], int indr[], int ip[], int iq[],
                        int lenc[], int lenr[], int locc[], int locr[]) {

		      //IMPLICIT           REAL*8(A-H,O-Z)

		      //DOUBLE PRECISION   B(NROWB,*), X(N), Y(M)
		      //INTEGER            KB(N)

		      //INTEGER            LUPARM(30)
		      //DOUBLE PRECISION   PARMLU(30), A(LENA)   , V(M) , W(N)
		      //INTEGER*2          INDC(LENA), INDR(LENA), IP(M), IQ(N)
		      //INTEGER*2          LENC(N)   , LENR(M)
		      //,,,,,,,,,INTEGER            LOCC(N)   , LOCR(M)

		//     ------------------------------------------------------------------
		//     LUCHEK computes the matrix A = L*U column by column
		//     and checks that it agrees with the relevant columns of B,
		//     specified by KB.
		
		//     LUCHEK then computes A = L*U row by row and compares with B.
		
		//     This constitutes a test of  LU6MUL  (with all values of MODE).
		//     ------------------------------------------------------------------

		final double zero = 0.0;
		final double one = 1.0;
		int inform[] = new int[1];
		int i, j, k, l, lenl, lenu, nrank;
		double cmax, err, rmax, xmax, ymax;

		nrank  = luparm[15];
		cmax = zero;
		rmax = zero;

		//     ------------------------------------------------------------------
		//     Check A = L*U by columns.
		//     ------------------------------------------------------------------

		  for (k = 1; k <= n; k++) {

		//        Set  W = K-th unit vector.

		         for (i = 0; i < n; i++) {
		        	 w[i] = zero;
		         }
		         w[k-1]   = one;

		//        Set  V  =  A * W  =  K-th column of A.
		//        Find the maximum deviation from the corresponding column of B.

		         lu6mul( 5, m, n, v, w,
		                 lena, luparm, parmlu,
		                 a, indc, indr, ip, iq, lenc, lenr, locc, locr);

		         j    = kb[k-1];
		         for (i = 1; i <= m; i++) {
		            err  = Math.abs( B[i-1][j-1] - v[i-1]);
		            cmax = Math.max(cmax, err);
		         } // for (i = 1; i <= m; i++)
		  } // for (k = 1; k <= n; k++)

		//     ------------------------------------------------------------------
		//     Check A = L*U by rows.
		//     ------------------------------------------------------------------

		  for (k = 1; k <= m; k++) {

		//        Set  V = K-th unit vector.

		         for (i = 0; i < m; i++) {
		        	 v[i] = zero;
		         }
		         v[k-1] = one;

		//        Set  W = A(transpose) * V  =  K-th row of A.
		//        Find the maximum deviation from the corresponding row of B.

		         lu6mul( 6, m, n, v, w,
		                 lena, luparm, parmlu,
		                 a, indc, indr, ip, iq, lenc, lenr, locc, locr);

		         for (j = 1; j <= n; j++) {
		            l     = kb[j-1];
		            err   = Math.abs(B[k-1][l-1] - w[j-1]);
		            rmax  = Math.max(rmax, err);
		         } // for (j = 1; j <= n; j++)
		  } // for (k = 1; k <= m; k++)

		//     ------------------------------------------------------------------
		//     Check solution of A*X = V.
		//     ------------------------------------------------------------------

		      x[0] = one;
		      for (j = 2; j <= n; j++) {
		         x[j-1]  = -x[j-2] / 2.0;
		      } // for (j = 2; j <= n; j++)

		      for (i = 0; i < m; i++) {
		    	  v[i] = zero;
		      }

		      for (k = 1; k <= n; k++) {
		         j     = kb[k-1];
		         for (i = 1; i <= m; i++) {
		        	 v[i-1] = v[i-1] + x[k-1] * B[i-1][j-1];
		         }
		      } // for (k = 1; k <= n; k++)

		//     Save V in Y.
		//     Solve  A*W = V = Y,
		//     then compute the residual  Y - A*W.

		      for (i = 0; i < m; i++) {
		          y[i] = v[i];
		      }
		      
		      if ((m == n) && doubleCheck) {
		    	  // Can only be used for A a square matrix
		          LinearEquations2 le2 = new LinearEquations2();
		          double A[][] = new double[n][n];
		          for (k = 1; k <= n; k++) {
		        	  j = kb[k-1];
		        	  for (i= 1; i <= m; i++) {
		        		  A[i-1][k-1] = B[i-1][j-1];
		        	  }
		          }
		          double Bmat[][] = new double[n][1];
		          for (i = 0; i < n; i++) {
		        	  Bmat[i][0] = v[i];
		          }
		          // AF is an output argument and on exit
		          // returns the factors L and U from the factorization A = P*L*U
		          // of the original matrix A.
		          double AF[][] = new double[n][n];
		          // ipiv is an output argument and on exit
		          // contains the pivot indices from the factorization A = P*L*U
		          // of the original matrix A.
		          int ipiv[] = new int[n];
		          // equed always outputs 'N' for no equilibration
		          char[] equed = new char[1];
		          // r is not accessed
		          double r[] = new double[n];
		          // c is not accessed
		          double c[] = new double[n];
		          // Bmat is not modified
		          // If info[0] = 0 or info[0] = n+1, the n-by-1 solution matrix X
		          // to the original system of equations.
		          double X[][] = new double[n][1];
		          // The estimate of the reciprocal condition number of the matrix
		          // A after equilibration (if done).  If rcond[0] is less than the
		          // machine precision (in particular, if rcond[0] = 0), the matrix
		          // is singular to working precision.  This condition is
		          // indicated by a return code of info[0] > 0.
		          double rcond[] = new double[1];
		          // The estimated forward error bound for each solution vector
		          // X(j) (the j-th column of the solution matrix X).
		          // If XTRUE is the true solution corresponding to X(j), ferr[j]
		          // is an estimated upper bound for the magnitude of the largest
		          // element in (X(j) - XTRUE) divided by the magnitude of the
		          // largest element in X(j).  The estimate is as reliable as
		          // the estimate for rcond[0], and is almost always a slight
		          // overestimate of the true error.
		          double ferr[] = new double[1];
		          // The componentwise relative backward error of each solution
		          // vector X(j) (i.e., the smallest relative change in
		          // any element of A or B that makes X(j) an exact solution).
		          double berr[] = new double[1];
		          // On exit, work[0] contains the reciprocal pivot growth
		          // factor norm(A)/norm(U). The "max absolute element" norm is
		          // used. If work[0] is much less than 1, then the stability
		          // of the LU factorization of the (equilibrated) matrix A
		          // could be poor. This also means that the solution X, condition
		          // estimator rcond[0], and forward error bound ferr could be
		          // unreliable. If factorization fails with 0<info[0]<=n, then
		          // work[0] contains the reciprocal pivot growth factor for the
		          // leading info[0] columns of A.
		          double work[] = new double[4*n];
		          int iwork[] = new int[n];
		          //= 0:  successful exit
		          //        < 0:  if info[0] = -i, the i-th argument had an illegal value
		          //       > 0:  if info[0] = i, and i is
		          //              <= n:  U[i-1][i-1] is exactly zero.  The factorization has
		          //                     been completed, but the factor U is exactly
		          //                     singular, so the solution and error bounds
		          //                     could not be computed. rcond[0] = 0 is returned.
		          //              = n+1: U is nonsingular, but rcond[0] is less than machine
		          //                     precision, meaning that the matrix is singular
		          //                     to working precision.  Nevertheless, the
		          //                     solution and error bounds are computed because
		          //                     there are a number of situations where the
		          //                     computed solution can be more accurate than the
		          //                     value of rcond[0] would suggest.
		          int info[] = new int[1];
		          le2.dgesvx('N', 'N', n, 1, A, n, AF, n, ipiv, equed, r, c, Bmat, n,
		        		  X, n, rcond, ferr, berr, work, iwork, info);
		          UI.setDataText("dgesvx solving A*W = V:\n");
		          if (info[0] == 0) {
		        	  UI.setDataText("Successful exit\n");
		          }
		          else if (info[0] <= n) {
		        	  i = info[0] - 1;
		        	  UI.setDataText("U["+i+"]["+i+"] is exactly zero\n");
		        	  UI.setDataText("The factorization has been completed, but the factor U is exactly singular,\n");
		        	  UI.setDataText("so the solution and error bounds could not be computed.  rcond[0] = 0 is returned\n");
		          }
		          else if (info[0] == n+1) {
		        	  UI.setDataText("U is nonsingular, but rcond[0] is less than machine precision,\n");
		        	  UI.setDataText("meaning that the matrix is singular to working precision.\n");
		        	  UI.setDataText("Nevertheless, the solution and error bounds are computed because there are a\n");
		        	  UI.setDataText("situations where the computed solution can be more accurate than the value of\n");
		        	  UI.setDataText("rcond[0] would suggest\n");
		          }
		          UI.setDataText("The reciprocal condition number of the matrix A = " + nf.format(rcond[0]) + "\n");
		          if ((info[0] == 0) || (info[0] == n+1)) {
		        	  UI.setDataText("The estimated forward error bound for the solution vector x = " + nf.format(ferr[0]) + "\n");
		        	  UI.setDataText("The relative backward error of the solution vector x = " + nf.format(berr[0]) + "\n");
		          }
		          UI.setDataText("Reciprocal pivot growth factor normA/normU = " + nf.format(work[0]) + "\n");
		          if (work[0] < 1.0) {
		              UI.setDataText("If much less than 1, the stability of the LU factorization could be poor\n");
		              UI.setDataText("This also means that the solution X, condition estimator rcond[0], and\n");
		              UI.setDataText("the forward error bound ferr[0] could be unreliable\n");
		          }
		      } // if ((m == n) && doubleCheck)

		      lu6sol( 5, m, n, v, w,
		              lena, luparm, parmlu,
		              a, indc, indr, ip, iq,
		              lenc, lenr, locc, locr, inform);

		      for (k = 1; k <= n; k++) {
		         j     = kb[k-1];
		         for (i = 1; i <= m; i++) {
		        	 y[i-1] = y[i-1] - w[k-1] * B[i-1][j-1];
		         }
		      } // for (k = 1; k <= n; k++)

		      ymax = 0.0;
		      for (i = 0; i < m; i++) {
		    	  if (Math.abs(y[i]) > ymax) {
		    		  ymax = Math.abs(y[i]);
		    	  }
		      }

		//     ------------------------------------------------------------------
		//     Check solution of A(t)*Y = W.
		//     ------------------------------------------------------------------

		      y[0] = one;
		      for (i = 2; i <= m; i++) {
		    	  y[i-1] = -y[i-2]/2.0;
		      } // for (i = 2; i <= m; i++)

		      for (k = 1; k <= n; k++) {
		         j     = kb[k-1];
		         w[k-1] = 0;
		         for (i = 1; i <= m; i++) {
		        	 w[k-1] = w[k-1] + B[i-1][j-1] * y[i-1];
		         }
		      } // for (k = 1; k <= n; k++)

		//     Save W in X.
		//     Solve  A(t)*V = W = X,
		//     then compute the residual  X - A(t)*V.

		      for (i = 0; i < n; i++) {
		    	  x[i] = w[i];
		      }

		      lu6sol( 6, m, n, v, w,
		              lena, luparm, parmlu,
		              a, indc, indr, ip, iq,
		              lenc, lenr, locc, locr, inform);

		      for (k = 1; k <= n; k++) {
		         j     = kb[k-1];
		         for (i = 1; i <= m; i++) {
		        	 x[k-1] = x[k-1] - B[i-1][j-1] * v[i-1];
		         }
		      } // for (k = 1; k <= n; k++)

		      xmax = 0.0;
		      for (i = 0; i < n; i++) {
		    	  if (Math.abs(x[i]) > xmax) {
		    		  xmax = Math.abs(x[i]);
		    	  }
		      }

		//     Print maximum errors.

		      emax[0] = Math.max(cmax, rmax);
		      emax[0] = Math.max(emax[0], ymax);
		      emax[0] = Math.max(emax[0], xmax);
		      lenl = luparm[22];
		      lenu = luparm[23];
		      UI.setDataText("luchek rank = " + nrank + "\n");
		      UI.setDataText("LenL = " + lenl + " LenU = " + lenu + "\n");
		      UI.setDataText("A - L*U error = " + nf.format(cmax) + "\n");
		      UI.setDataText("(A - L*U)transpose error = " + nf.format(rmax) + "\n");
		      UI.setDataText("Ax = b error = " + nf.format(ymax) + "\n");
		      UI.setDataText("Atranspose * y  = b error = " + nf.format(xmax) + "\n");
		      return;
	} // luchek


	
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
	        nmove,
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
	
		    if (lprint >= 10) {
		       densty = 100.0 * delem / (dm * dn);
		       UI.setDataText("m = " + m + " mnkey = " + mnkey + " n = " + n + "\n");
		       UI.setDataText("nelem = " + nelem + " Amax[0] = " + nf.format(Amax[0]) + 
		    		   " densty = " + nf.format(densty) + "\n");
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
		    // Ha, Hj, and Hk are only used in lu1fad if TCP.
		    double Ha[];
		    int Hj[];
		    int Hk[];
		    if (TCP) {
			    Ha = new double[lenH];
			    Hj = new int[lenH];
			    Hk = new int[lenH];
		    } // if (TCP)
		    else {
		       Ha = null;
		       Hj = null;
		       Hk = null;
		    }
		    // Amaxr only used if TRP.
		    double Amaxr[];
		    if (TRP) {
		    	Amaxr = new double[m];
		    }
		    else {
		        Amaxr = null;	
		    }
		    if (TCP) {
			    for (i = 0; i < lenH; i++) {
			    	Ha[i] = a[locH - 1 + i];
			    	Hj[i] = indc[locH-1+i];
			    	Hk[i] = indr[locH-1+i];
			    }
		    } // if (TCP)
		    if (TRP) {
			    for (i = 0; i < m; i++) {
			    	Amaxr[i] = a[lmaxr-1+i];
			    }
		    } // if (TRP)
		    lu1fad( m, n, numnz[0] , lena2, luparm, parmlu,
		                 a     , indc  , indr  , p     , q,
		                 lenc  , lenr  , locc  , locr,
		                 iploc , iqloc , ipinv , iqinv , w,
		                 lenH, Ha, Hj, Hk, Amaxr,
		                 inform, lenL  , lenU  , minlen, mersum,
		                 nUtri , nLtri , ndens1, ndens2, nrank ,
		                 Lmax  , Umax  , DUmax , DUmin , Akmax );
		    if (TCP) {
			    for (i = 0; i < lenH; i++) {
			    	a[locH - 1 + i] = Ha[i];
			    	indc[locH-1+i] = Hj[i];
			    	indr[locH-1+i] = Hk[i];
			    }
		    } // if (TCP)
		    if (TRP) {
			    for (i = 0; i < m; i++) {
			    	a[lmaxr-1+i] = Amaxr[i];
			    }
		    } // if (TRP)
	
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

	    if (lprint >= 10) {
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
	          UI.setDataText("nLtri[0] = " + nLtri[0] + "\n");
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
	       UI.setDataText("ndens2 = " + ndens2[0] + "\n");
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
int ind2[];
int lenold[];

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
lu[0]       = lu1;

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

lu1gau( m     , melim , ncold , nspare, small ,
           lpivc1, lpivc2, lfirst, lpivr2, lfree , minfre,
           ilast , jlast , lrow  , lcol  , lu    , nfill ,
           a     , indc  , indr  ,  
           lenc  , lenr  , locc  , locr  , 
           iqloc , ll1, lu1);


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
 
 lu1pen(m, melim , ncold , nspare, ilast,
              lpivc1, lpivc2, lpivr1, lpivr2, lrow ,
              lenc  , lenr  , locc  , locr  , 
              indc  , indr  , ll1, lu1);
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
lu1ful( m     , n    , lena , lenD , lu1 , TPP,
           mleft , nleft, nrank[0], nrowu,
           lenL  , lenU , nsing,
           keepLU, small,
           a, lD , indc , indr , p   , q,
           lenc  , lenr , locc , ipinv, locr );

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
            double a[], int lDInput, int indc[], int indr[], int p[], int q[],
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
	a[lDInput-1+i] = zero;
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
 a[lDInput+ld-2]  = a[lc-1];
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
		arr[i][j] = a[lDInput-1+index];
		index++;
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
		a[lDInput-1+index] = arr[i][j];
		index++;
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
	a[i] = a[lDInput-1+i];
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
 lenc[jbest-1] = - nrowd;
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
            int mark[], int ll1, int lu1) {

//integer(ip),   intent(in)    :: m, melim, ncold, nspare,          &
//                           lpivc1, lpivc2, lpivr2, lfree, minfre
// In fact, ilast is never changed in lu1gau
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
int l_final;

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
uj     = a[lu1 + lu[0]-2];
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
    indc[ll1 + ll-2] = j;
    a[l-1]      = a[l-1]  +  a[ll1+ll-2] * uj;
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

l_final = lcol[0] + nspare;
for (l = lcol[0] + 1; l <= l_final; l++) {
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
 if (indc[ll1+ll-2] ==  j  ) continue;
 aij        = a[ll1+ll-2]*uj;
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

    if (indr[ll1+ll-2] == 0) nfill[0]     = nfill[0] + leni + nspare;
    if (indr[lu1+lu[0]-2] == 0) indr[lu1+lu[0]-2] = lenj;
    nfill[0]      = nfill[0]     + 1;
    indr[ll1+ll-2]  = indr[ll1+ll-2] + 1;
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
            int indc[], int indr[], int ll1, int lu1) {

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

int i, j, l, last, lc, lc1, lc2, ll, lr, lr1, lr2, lu, l_final;

ll     = 0;

for (lc = lpivc1; lc <= lpivc2; lc++) {
ll = ll + 1;
if (indr[ll1+ll-2] == 0) continue;

// Another row has pending fill.
// First, add some spare space at the end
// of the current last row.

l_final = lrow[0] + nspare;
for (l = lrow[0] + 1; l <= l_final; l++) {
 lrow[0]    = l;
 indr[l-1] = 0;
} // for (l = lrow[0] + 1; l <= final_l; l++)

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

lrow[0]    = lrow[0] + indr[ll1+ll-2];
} // for (lc = lpivc1; lc <= lpivc2; lc++)

// Scan all columns of  D  and insert the pending fill-in
// into the row file.

lu     = 1;

for (lr = lpivr1; lr <= lpivr2; lr++) {
lu     = lu + 1;
if (indr[lu1+lu-2] == 0) continue;
j      = indr[lr-1];
lc1    = locc[j-1] + indr[lu1+lu-2] - 1;
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

parmlu[10] = Lmax;
parmlu[11] = Umax;

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
 UI.setDataText("Singular m " + mnkey + " n " + " rank = " + nrank + "n - rank = " + ndefic + " nsing = " + nsing + "\n");
} // if (lprint >= 0)
} // if (nsing > 0)

// Exit.

luparm[9] = inform[0];
return;
} // lu6chk
    
    // *********************************************************************
    // Original file: lusol8a.f
    
    // lu8rpc
    
    // Sparse LU update: Replace Column
    // LUSOL's sparse implementation of the Bartels-Golub update.
    
    // 01 May 2002: Derived from LUSOL's original lu8a.f file.
    // 01 May 2002: Current version of lusol8a.f.
    // 15 Sep 2004: Test nout. gt. 0 to protect write statements.
    // 13 Dec 2011: First f90 version.
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    private void lu8rpc(int mode1, int mode2, int m, int n, int jrep, double v[], double w[],
                       int lena, int luparm[], double parmlu[],
                       double a[], int indc[], int indr[], int p[], int q[],
                       int lenc[], int lenr[], int locc[], int locr[],
                       int inform[], double diag[], double vnorm[]) {

//      integer(ip),   intent(in)    :: mode1, mode2, m, n, jrep, lena
//      integer(ip),   intent(inout) :: luparm(30), &
//                                      indc(lena), indr(lena), p(m), q(n), &
//                                      lenc(n), lenr(m), locc(n), locr(m)
//      integer(ip),   intent(out)   :: inform
//      real(rp),      intent(inout) :: parmlu(30), a(lena), v(m), &
//                                      w(n)  ! not used
//      real(rp),      intent(out)   :: diag, vnorm

      // ------------------------------------------------------------------
      // lu8rpc  updates the LU factorization A = L*U when column jrep
      // is replaced by some vector v = a(new).
      
      // lu8rpc  is an implementation of the Bartels-Golub update,
      // designed for the case where A is rectangular and/or singular.
      // L is a product of stabilized eliminations (m x m, nonsingular).
      // P U Q is upper trapezoidal (m x n, rank nrank).
      
      // If  mode1 = 0,  the old column is taken to be zero
      // (so it does not have to be removed from U).
      
      // If  mode1 = 1,  the old column need not have been zero.
      
      // If  mode2 = 0,  the new column is taken to be zero.
      //                 v(*) is not used or altered.
      
      // If  mode2 = 1,  v(*) must contain the new column a(new).
      // On exit,  v(*)  will satisfy L*v = a(new).
      
      // If  mode2 = 2,  v(*) must satisfy L*v = a(new).
      
      // The array w(*) is not used or altered.
      
      // On entry, all elements of locc are assumed to be zero.
      // On a successful exit (inform /= 7), this will again be true.
      
      // On exit:
      // inform = -1  if the rank of U decreased by 1.
      // inform =  0  if the rank of U stayed the same.
      // inform =  1  if the rank of U increased by 1.
      // inform =  2  if the update seemed to be unstable
      //              (diag much bigger than vnorm).
      // inform =  7  if the update was not completed (lack of storage).
      // inform =  8  if jrep is not between 1 and n.
      
      // -- Jan 1985: Original F66 version.
      // -- Jul 1987: Modified to maintain U in trapezoidal form.
      // 10 May 1988: First f77 version.
      // 16 Oct 2000: Added test for instability (inform = 2).
      // 13 Dec 2011: First f90 version.
      // ------------------------------------------------------------------

      boolean singlr;
      int klast[] = new int[1];
      int krep[] = new int[1];
      int lenU[] = new int[1];
      int lenL[] = new int[1];
      int lrow[] = new int[1];
      int nrank[] = new int[1];
      int iw, j1, jsing,
          l1, lprint, nrank0;
      double Utol1, Utol2;
      final int i1 = 1;
      final double zero = 0.0;

      lprint = luparm[1];
      nrank[0]  = luparm[15];
      lenL[0]   = luparm[22];
      lenU[0]   = luparm[23];
      lrow[0]   = luparm[24];
      Utol1  = parmlu[3];
      Utol2  = parmlu[4];
      nrank0 = nrank[0];
      diag[0]   = zero;
      vnorm[0]  = zero;
      if ((jrep < 1) || (jrep > n)) {
    	  inform[0] = 8;
	      if (lprint >= 0) {
	    	  UI.setDataText("lu8rpc  error...  jrep  is out of range.\n");
	    	  UI.setDataText("m = " + m + " n = " + n + " jrep = " + jrep + "\n");
	      }

	      // Exit.

	      luparm[9] = inform[0];
	      luparm[14] = luparm[14] + 1;
	      luparm[15] = nrank[0];
	      luparm[22] = lenL[0];
	      luparm[23] = lenU[0];
	      luparm[24] = lrow[0];
	      return;
      }

      // ------------------------------------------------------------------
      // If mode1 = 0, there are no elements to be removed from  U
      // but we still have to set  krep  (using a backward loop).
      // Otherwise, use lu7zap to remove column  jrep  from  U
      // and set  krep  at the same time.
      // ------------------------------------------------------------------
      if (mode1 == 0) {
         krep[0]   = n + 1;

         do {
         krep[0]   = krep[0] - 1;
         } while (q[krep[0]-1] != jrep);
      } // if (mode1 == 0)
      else {
         lu7zap( m, n, jrep, krep, lena, lenU, lrow, nrank[0],
                 a, indr, p, q, lenr, locr );
      }

      // ------------------------------------------------------------------
      // Insert a new column of u and find klast.
      // ------------------------------------------------------------------

      if (mode2 == 0) {
         klast[0]  = 0;
      }
      else { // mode2 != 0
         if (mode2 == 1) {

            // Transform v = a(new) to satisfy  L*v = a(new).

            lu6sol( i1, m, n, v, w, lena, luparm, parmlu,
                    a, indc, indr, p, q,         
                    lenc, lenr, locc, locr, inform );
         } // if (mode2 == 1)

         // Insert into U any nonzeros in the top of v.
         // row p(klast) will contain the last nonzero in pivotal order.
         // Note that klast will be in the range ( 0, nrank ).

         lu7add( m, n, jrep, v, 
                 lena, luparm, parmlu,
                 lenL, lenU, lrow, nrank[0],
                 a, indr, p, lenr, locr,
                 inform, klast, vnorm );
         if (inform[0] == 7) {
        	 // Not enough storage.
	         if (lprint >= 0) {
	    	     UI.setDataText("lu8rpc  error...  Insufficient storage. lena = " + lena + "\n");
	         }
	         // Exit.
		     luparm[9] = inform[0];
		     luparm[14] = luparm[14] + 1;
		     luparm[15] = nrank[0];
		     luparm[22] = lenL[0];
		     luparm[23] = lenU[0];
		     luparm[24] = lrow[0];
	    	 return;
         } // if (inform[0] == 7)
      } // else mode2 != 0

      // ------------------------------------------------------------------
      // In general, the new column causes U to look like this:
      //
      //                 krep        n                 krep  n
      //
      //                ....a.........          ..........a...
      //                 .  a        .           .        a  .
      //                  . a        .            .       a  .
      //                   .a        .             .      a  .
      //        P U Q =     a        .    or        .     a  .
      //                    b.       .               .    a  .
      //                    b .      .                .   a  .
      //                    b  .     .                 .  a  .
      //                    b   ......                  ..a...  nrank
      //                    c                             c
      //                    c                             c
      //                    c                             c     m
      //
      //     klast points to the last nonzero "a" or "b".
      //     klast = 0 means all "a" and "b" entries are zero.
      // ------------------------------------------------------------------

      if (mode2 == 0) {
         if (krep[0] > nrank[0]) {
        	 if (nrank[0] == nrank0) {
             inform[0] =  0;
        	 }
	         else if (nrank[0] < nrank0) {
	             inform[0] = -1;
	             if (nrank0 == n) {
	                if (lprint >= 0) {
	                	UI.setDataText("lu8rpc  warning.  Singularity after replacing column.\n");
	                	UI.setDataText("jrep = " + jrep + " diag[0] = " + nf.format(diag[0]) + "\n");
	                } // if (lprint >= 0)
	             } // if (nrank0 == n)
	         }  // else if (nrank[0] < nrank0)
	         else {
	             inform[0] =  1;
             }
        	 
        	 // Exit.

		     luparm[9] = inform[0];
		     luparm[14] = luparm[14] + 1;
		     luparm[15] = nrank[0];
		     luparm[22] = lenL[0];
		     luparm[23] = lenU[0];
		     luparm[24] = lrow[0];
	    	 return;
         } // if (krep[0] > nrank[0])
      } // if (mode2 == 0)
      else if (nrank[0] < m) {

         // Eliminate any "c"s (in either case).
         // Row nrank + 1 may end up containing one nonzero.

         lu7elm( m, n, jrep, v, lena, luparm, parmlu,
                 lenL, lenU[0], lrow, nrank[0], 
                 a, indc, indr, p, q, lenr, locc, locr,
                 inform, diag );
         if (inform[0] == 7) {
        	 // Not enough storage.
	         if (lprint >= 0) {
	    	     UI.setDataText("lu8rpc  error...  Insufficient storage. lena = " + lena + "\n");
	         }
	         // Exit.
		     luparm[9] = inform[0];
		     luparm[14] = luparm[14] + 1;
		     luparm[15] = nrank[0];
		     luparm[22] = lenL[0];
		     luparm[23] = lenU[0];
		     luparm[24] = lrow[0];
	    	 return;
         } // if (inform[0] == 7)

         if (inform[0] == 1) {

            // The nonzero is apparently significant.
            // Increase nrank by 1 and make klast point to the bottom.

            nrank[0] = nrank[0] + 1;
            klast[0] = nrank[0];
         } // if (inform[0] == 1)
      } // else if (nrank[0] < m)

      if (nrank[0] < n) {

         // The column rank is low.
         
         // In the first case, we want the new column to end up in
         // position nrank, so the trapezoidal columns will have a chance
         // later on (in lu7rnk) to pivot in that position.
         
         // Otherwise the new column is not part of the triangle.  We
         // swap it into position nrank so we can judge it for singularity.
         // lu7rnk might choose some other trapezoidal column later.

         if (krep[0] < nrank[0]) {
            klast[0]    = nrank[0];
         }
         else {
            q[krep[0]-1] = q[nrank[0]-1];
            q[nrank[0]-1] = jrep;
            krep[0]     = nrank[0];
         }
      } // if (nrank[0] < n)

      // ------------------------------------------------------------------
      // If krep < klast, there are some "b"s to eliminate:
      //
      //                  krep
      //
      //                ....a.........
      //                 .  a        .
      //                  . a        .
      //                   .a        .
      //        P U Q =     a        .  krep
      //                    b.       .
      //                    b .      .
      //                    b  .     .
      //                    b   ......  nrank
      //
      //     If krep == klast, there are no "b"s, but the last "a" still
      //     has to be moved to the front of row krep (by lu7for).
      // ------------------------------------------------------------------

      if (krep[0] <= klast[0]) {

         // Perform a cyclic permutation on the current pivotal order,
         // and eliminate the resulting row spike.  krep becomes klast.
         // The final diagonal (if any) will be correctly positioned at
         // the front of the new krep-th row.  nrank stays the same.

         lu7cyc( krep[0], klast[0], p );
         lu7cyc( krep[0], klast[0], q );

         lu7for( m, n, krep[0], klast[0],
                 lena, luparm, parmlu,
                 lenL, lenU, lrow, 
                 a, indc, indr, p, q, lenr, locc, locr,
                 inform, diag );
         if (inform[0] == 7) {
        	 // Not enough storage.
	         if (lprint >= 0) {
	    	     UI.setDataText("lu8rpc  error...  Insufficient storage. lena = " + lena + "\n");
	         }
	         // Exit.
		     luparm[9] = inform[0];
		     luparm[14] = luparm[14] + 1;
		     luparm[15] = nrank[0];
		     luparm[22] = lenL[0];
		     luparm[23] = lenU[0];
		     luparm[24] = lrow[0];
	    	 return;
         }
         krep[0]   = klast[0];

         // Test for instability (diag much bigger than vnorm).

         singlr = vnorm[0] < Utol2 * Math.abs(diag[0]);
         if ( singlr ) {
        	 // Instability.
        	 inform[0] = 2;
        	 if (lprint >= 0) {
                 UI.setDataText("lu8rpc  warning.  Instability after replacing column.\n");
        	     UI.setDataText("jrep = " + jrep + " diag[0] = " + nf.format(diag[0]) + "\n");
        	 }
        	 // Exit.
		     luparm[9] = inform[0];
		     luparm[14] = luparm[14] + 1;
		     luparm[15] = nrank[0];
		     luparm[22] = lenL[0];
		     luparm[23] = lenU[0];
		     luparm[24] = lrow[0];
	    	 return;
         } // if (singlr)
      } // if (krep[0] <= klast[0)

      // ------------------------------------------------------------------
      // Test for singularity in column krep (where krep .le. nrank).
      // ------------------------------------------------------------------

      diag[0]   = zero;
      iw     = p[krep[0]-1];
      singlr = lenr[iw-1] == 0;

      if (!singlr) {
         l1     = locr[iw-1];
         j1     = indr[l1-1];
         singlr = j1 != jrep;

         if (!singlr) {
            diag[0]   = a[l1-1];
            singlr = Math.abs( diag[0] ) <= Utol1 ||
                     Math.abs( diag[0] ) <= Utol2 * vnorm[0];
         } // if (!singlr)
      } // if (!singlr)

      if (singlr &&  krep[0] < nrank[0]) {

         // Perform cyclic permutations to move column jrep to the end.
         // Move the corresponding row to position nrank
         // then eliminate the resulting row spike.

         lu7cyc( krep[0], nrank[0], p );
         lu7cyc( krep[0], n    , q );

         lu7for( m, n, krep[0], nrank[0],
                 lena, luparm, parmlu,
                 lenL, lenU, lrow,
                 a, indc, indr, p, q, lenr, locc, locr,
                 inform, diag );
         if (inform[0] == 7) {
        	// Not enough storage.
	         if (lprint >= 0) {
	    	     UI.setDataText("lu8rpc  error...  Insufficient storage. lena = " + lena + "\n");
	         }
	         // Exit.
		     luparm[9] = inform[0];
		     luparm[14] = luparm[14] + 1;
		     luparm[15] = nrank[0];
		     luparm[22] = lenL[0];
		     luparm[23] = lenU[0];
		     luparm[24] = lrow[0];
	    	 return;	 
         } // if (inform[0] == 7)
      } // if (singlr &&  krep[0] < nrank[0])

      // Find the best column to be in position nrank.
      // If singlr, it can't be the new column, jrep.
      // If nothing satisfactory exists, nrank will be decreased.

      if (singlr ||  nrank[0] < n) {
         jsing  = 0;
         if ( singlr ) jsing = jrep;

         lu7rnk( m, n, jsing, lena, parmlu,
                 lenL, lenU, lrow, nrank, 
                 a, indc, indr, p, q, lenr, locc, locr,
                 inform, diag );
      } // if (singlr ||  nrank[0] < n)

      // ------------------------------------------------------------------
      // Set inform for exit.
      // ------------------------------------------------------------------
      if (nrank[0] == nrank0) {
         inform[0] =  0;
      }
      else if (nrank[0] < nrank0) {
         inform[0] = -1;
         if (nrank0 == n) {
            if (lprint >= 0) {
            	UI.setDataText("lu8rpc  warning.  Singularity after replacing column.\n");
            	UI.setDataText("jrep = " + jrep + " diag[0] = " + nf.format(diag[0]) + "\n");
            }
         } // if (nrank0 == n)
      } // else if (nrank[0] < nrank0)
      else {
         inform[0] =  1;
      }
      // Exit.
	  luparm[9] = inform[0];
	  luparm[14] = luparm[14] + 1;
	  luparm[15] = nrank[0];
	  luparm[22] = lenL[0];
	  luparm[23] = lenU[0];
	  luparm[24] = lrow[0];
      return;
} // lu8rpc

    private void lu7zap(int m, int n, int jzap, int kzap[], int lena, int lenU[], int lrow[], int nrank,
            double a[], int indr[], int p[], int q[], int lenr[], int locr[]) {

//integer(ip),   intent(in)    :: m, n, jzap, lena, nrank, &
//                           p(m)
//integer(ip),   intent(inout) :: lenU, lrow, &
//                           indr(lena), q(n), lenr(m), locr(m)
//integer(ip),   intent(out)   :: kzap
//real(rp),      intent(inout) :: a(lena)

// ------------------------------------------------------------------
// lu7zap  eliminates all nonzeros in column  jzap  of  U.
// It also sets  kzap  to the position of  jzap  in pivotal order.
// Thus, on exit we have  q(kzap) = jzap.

// -- Jul 1987: nrank added.
// 10 May 1988: First f77 version.
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

int i, k, leni, l, lr1, lr2;
boolean seg1 = true;

for (k = 1; k <= nrank; k++) {
i      = p[k-1];
leni   = lenr[i-1];
if (leni != 0) {
lr1    = locr[i-1];
lr2    = lr1 + leni - 1;
for (l = lr1; l <= lr2; l++) {
 if (indr[l-1] == jzap) break;
} // for (l = lr1; l <= lr2; l++)
if (l <= lr2) {

// Delete the old element.

a[l-1]      = a[lr2-1];
indr[l-1]   = indr[lr2-1];
indr[lr2-1] = 0;
lenr[i-1]   = leni - 1;
lenU[0]      = lenU[0] - 1;
} // if (l <= lr2)
} // if (leni != 0)

// Stop if we know there are no more rows containing  jzap.

kzap[0]   = k;
if (q[k-1] == jzap) {
	seg1 = false;
	break;
}
} // for (k = 1; k <= nrank; k++) 

if (seg1) {
// nrank must be smaller than n because we haven't found kzap yet.

for (k = nrank+1; k <= n; k++) {
kzap[0]  = k;
if (q[k-1] == jzap) break;
} // for (k = nrank+1; k <= n; k++)
} // if (seg1)

// See if we zapped the last element in the file.

if (lrow[0] > 0) {
if (indr[lrow[0]-1] == 0) lrow[0] = lrow[0] - 1;
} // if (lrow[0] > 0)

} // lu7zap

    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // Original file:  lusol6a.f
    
    // lu6sol   lu6L     lu6Lt     lu6U     Lu6Ut   lu6LD    lu6chk
    
    // 26 Apr 2002: lu6 routines put into a separate file.
    // 15 Dec 2002: lu6sol modularized via lu6L, lu6Lt, lu6U, lu6Ut.
    //              lu6LD implemented to allow solves with LDL' or L|D|L'.
    // 23 Apr 2004: lu6chk modified.  TRP can judge singularity better
    //              by comparing all diagonals to DUmax.
    // 27 Jun 2004: lu6chk.  Allow write only if nout > 0 .
    // 13 Dec 2011: First f90 version.
    // ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    public void lu6sol(int mode, int m, int n, double v[], double w[],
                       int lena, int luparm[], double parmlu[],
                       double a[], int indc[], int indr[], int p[], int q[],
                       int lenc[], int lenr[], int locc[], int locr[],
                       int inform[]) {

      //integer(ip),   intent(in)    :: mode, m, n, lena
      //integer(ip),   intent(inout) :: luparm(30)
      //integer(ip),   intent(out)   :: inform
      //integer(ip),   intent(in)    :: indc(lena), indr(lena), p(m), q(n),   &
      //                                lenc(n), lenr(m), locc(n), locr(m)
      //real(rp),      intent(in)    :: a(lena)
      //real(rp),      intent(inout) :: parmlu(30), v(m), w(n)

      // -----------------------------------------------------------------------
      // lu6sol  uses the factorization  A = L U  as follows:
      
      // mode
      //  1    v  solves   L v = v(input).   w  is not touched.
      //  2    v  solves   L'v = v(input).   w  is not touched.
      //  3    w  solves   U w = v.          v  is not altered.
      //  4    v  solves   U'v = w.          w  is destroyed.
      //  5    w  solves   A w = v.          v  is altered as in 1.
      //  6    v  solves   A'v = w.          w  is destroyed.
      
      // If mode = 3,4,5,6, v and w must not be the same arrays.
      
      // If lu1fac has just been used to factorize a symmetric matrix A
      // (which must be definite or quasi-definite), the factors A = L U
      // may be regarded as A = LDL', where D = diag(U).  In such cases,
      
      // mode
      //  7    v  solves   A v = L D L'v = v(input).   w  is not touched.
      //  8    v  solves       L |D| L'v = v(input).   w  is not touched.
      
      // p(*), q(*)        hold row and column numbers in pivotal order.
      // lenc(k)           is the length of the k-th column of initial L.
      // lenr(i)           is the length of the i-th row of U.
      // locc(*)           is not used.
      // locr(i)           is the start  of the i-th row of U.
      
      // U is assumed to be in upper-trapezoidal form (nrank by n).
      // The first entry for each row is the diagonal element
      // (according to the permutations p, q).  It is stored at
      // location locr(i) in a(*), indr(*).
      
      // On exit, inform = 0 except as follows.
      // If mode = 3,4,5,6 and if U (and hence A) is singular, then
      // inform = 1 if there is a nonzero residual in solving the system
      // involving U.  parmlu(20) returns the norm of the residual.
      //
      // July 1987:   Early version.
      // 09 May 1988: f77 version.
      // 27 Apr 2000: Abolished the dreaded "computed go to".
      //              But hard to change other "go to"s to "if then else".
      // 15 Dec 2002: lu6L, lu6Lt, lu6U, lu6Ut added to modularize lu6sol.
      // 13 Dec 2011: First f90 version.
      // --------------------------------------------------------------------

      final int i1 = 1;
      final int i2 = 2;

      if      (mode == 1) {             // Solve  L v(new) = v.
         lu6L  ( inform, m, n, v,
                 lena, luparm, parmlu, a, indc, indr, lenc );
      }
      else if (mode == 2) {            // Solve  L'v(new) = v.
         lu6Lt ( inform, m, n, v, 
                 lena, luparm, parmlu, a, indc, indr, lenc );
      }
      else if (mode == 3) {             // Solve  U w = v.
         lu6U  ( inform, m, n, v, w,
                 lena, luparm, parmlu, a, indr, p, q, lenr, locr );
      }
      else if (mode == 4) {             // Solve  U'v = w.
         lu6Ut ( inform, m, n, v, w,
                 lena, luparm, parmlu, a, indr, p, q, lenr, locr );
      }
      else if (mode == 5) {             // Solve  A w      = v
                                        // via    L v(new) = v
                                        // and    U w = v(new).
         lu6L  ( inform, m, n, v,
                 lena, luparm, parmlu, a, indc, indr, lenc );
         lu6U  ( inform, m, n, v, w, 
                 lena, luparm, parmlu, a, indr, p, q, lenr, locr );
      }
      else if (mode == 6) {             // Solve  A'v = w
                                        // via    U'v = w
                                        // and    L'v(new) = v.
         lu6Ut ( inform, m, n, v, w,
                 lena, luparm, parmlu, a, indr, p, q, lenr, locr );
         lu6Lt ( inform, m, n, v,
                 lena, luparm, parmlu, a, indc, indr, lenc );
      }
      else if (mode == 7) {             // Solve  LDv(bar) = v
                                        // and    L'v(new) = v(bar).
         lu6LD ( inform, i1, m, n, v,
                 lena, luparm, parmlu, a, indc, indr, lenc, locr );
         lu6Lt ( inform, m, n, v,
                 lena, luparm, parmlu, a, indc, indr, lenc );
      }
      else if (mode == 8) {            // Solve  L|D|v(bar) = v
                                       // and    L'v(new) = v(bar).
         lu6LD ( inform, i2, m, n, v,
                 lena, luparm, parmlu, a, indc, indr, lenc, locr );
         lu6Lt ( inform, m, n, v,
                 lena, luparm, parmlu, a, indc, indr, lenc );
      }

      } // lu6sol
    
    public void lu6L  (int inform[], int m, int n, double v[],
            int lena, int luparm[], double parmlu[], double a[], int indc[], int indr[], int lenc[]) {

//integer(ip),   intent(in)    :: m, n, lena
//integer(ip),   intent(inout) :: luparm(30)
//integer(ip),   intent(out)   :: inform
//integer(ip),   intent(in)    :: indc(lena), indr(lena), lenc(n)
//real(rp),      intent(in)    :: a(lena)
//real(rp),      intent(inout) :: parmlu(30), v(m)

// ------------------------------------------------------------------
// lu6L   solves   L v = v(input).
//
// 15 Dec 2002: First version derived from lu6sol.
// 15 Dec 2002: Current version.
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

int i, ipiv, j, k, l, l1, ldummy, len, lenL, lenL0, numL, numL0;
double small, vpiv;

numL0  = luparm[19];
lenL0  = luparm[20];
lenL   = luparm[22];
small  = parmlu[2];
inform[0] = 0;
l1     = lena + 1;

for (k = 1; k <= numL0; k++) {
len   = lenc[k-1];
l     = l1;
l1    = l1 - len;
ipiv  = indr[l1-1];
vpiv  = v[ipiv-1];

if (Math.abs(vpiv) > small) {
 // ***** This loop could be coded specially.
 for (ldummy = 1; ldummy <= len; ldummy++) {
    l    = l - 1;
    j    = indc[l-1];
    v[j-1] = v[j-1] + a[l-1]*vpiv;
 } // for (ldummy = 1; ldummy <= len; ldummy++)
} // if (Math.abs(vpiv) > small)
} // for (k = 1; k <= numL0; k++)

l      = lena - lenL0 + 1;
numL   = lenL - lenL0;

//***** This loop could be coded specially.

for (ldummy = 1; ldummy <= numL; ldummy++) {
l      = l - 1;
i      = indr[l-1];
if (Math.abs(v[i-1]) > small) {
 j    = indc[l-1];
 v[j-1] = v[j-1] + a[l-1]*v[i-1];
} // if (Math.abs(v[i-1]) > small)
} // for (ldummy = 1; ldummy <= numL; ldummy++)

// Exit.

luparm[9] = inform[0];

    } // lu6L

    private void lu6Lt (int inform[], int m, int n, double v[],
            int lena, int luparm[], double parmlu[], double a[], int indc[], int indr[], int lenc[]) {

//integer(ip),   intent(in)    :: m, n, lena
//integer(ip),   intent(inout) :: luparm(30)
//integer(ip),   intent(out)   :: inform
//integer(ip),   intent(in)    :: indc(lena), indr(lena), lenc(n)
//real(rp),      intent(in)    :: a(lena)
//real(rp),      intent(inout) :: parmlu(30), v(m)

// ------------------------------------------------------------------
// lu6Lt  solves   L'v = v(input).

// 15 Dec 2002: First version derived from lu6sol.
// 15 Dec 2002: Current version.
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

int i, ipiv, j, k, l, l1, l2, len, lenL, lenL0, numL0;
double small, sum;
final double zero = 0.0;

numL0  = luparm[19];
lenL0  = luparm[20];
lenL   = luparm[22];
small  = parmlu[2];
inform[0] = 0;
l1     = lena - lenL + 1;
l2     = lena - lenL0;

// ***** This loop could be coded specially.
for (l = l1; l <= l2; l++) {
j     = indc[l-1];
if (Math.abs(v[j-1]) > small) {
 i     = indr[l-1];
 v[i-1]  = v[i-1] + a[l-1]*v[j-1];
} // if (Math.abs(v[j-1]) > small)
} // for (l = l1; l <= l2; l++) 

for (k = numL0; k >= 1; k--) {
len   = lenc[k-1];
sum   = zero;
l1    = l2 + 1;
l2    = l2 + len;

// ***** This loop could be coded specially.
for (l = l1; l <= l2; l++) {
 j     = indc[l-1];
 sum   = sum + a[l-1]*v[j-1];
} // for (l = l1; l <= l2; l++)

ipiv    = indr[l1-1];
v[ipiv-1] = v[ipiv-1] + sum;
} // for (k = numL0; k >= 1; k--)

// Exit.

luparm[9] = inform[0];

} // lu6Lt
    
    public void lu6U  (int inform[], int m, int n, double v[], double w[],
            int lena, int luparm[], double parmlu[], double a[], int indr[], int p[], int q[], int lenr[], int locr[]) {

//integer(ip),   intent(in)    :: m, n, lena
//integer(ip),   intent(inout) :: luparm(30)
//integer(ip),   intent(out)   :: inform
//integer(ip),   intent(in)    :: indr(lena), p(m), q(n), lenr(m), locr(m)
//real(rp),      intent(in)    :: a(lena)
//real(rp),      intent(in)    :: v(m)
//real(rp),      intent(inout) :: parmlu(30)
//real(rp),      intent(out)   :: w(n)

// ------------------------------------------------------------------
// lu6U   solves   U w = v.          v  is not altered.
//
// 15 Dec 2002: First version derived from lu6sol.
// 15 Dec 2002: Current version.
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

int i, j, k, klast, l, l1, l2, l3, nrank, nrank1;
double resid, small, t;
final double zero = 0.0;

nrank  = luparm[15];
small  = parmlu[2];
inform[0] = 0;
nrank1 = nrank + 1;
resid  = zero;

// Find the first nonzero in v(1:nrank), counting backwards.

for (klast = nrank; klast >= 1; klast--) {
i     = p[klast-1];
if (Math.abs(v[i-1]) > small) break;
} // for (klast = nrank; klast >= 1; klast--)

for (k = klast + 1; k <= n; k++) {
j    = q[k-1];
w[j-1] = zero;
} // for (k = klast + 1; k <= n; k++) 

// Do the back-substitution, using rows 1:klast of U.

for (k  = klast; k >= 1; k--) {
i  = p[k-1];
t  = v[i-1];
l1 = locr[i-1];
l2 = l1 + 1;
l3 = l1 + lenr[i-1] - 1;

// ***** This loop could be coded specially.
for (l = l2; l <= l3; l++) {
 j = indr[l-1];
 t = t - a[l-1]*w[j-1];
} // for (l = l2; l <= l3; l++)

j  = q[k-1];
if (Math.abs(t) <= small) {
 w[j-1] = zero;
}
else {
 w[j-1] = t/a[l1-1];
}
} // for (k  = klast; k >= 1; k--)

// Compute residual for overdetermined systems.

for (k = nrank1; k <= m; k++) {
i     = p[k-1];
resid = resid + Math.abs(v[i-1]);
} // for (k = nrank1; k <= m; k++)

// Exit.

if (resid > zero) inform[0] = 1;
luparm[9] = inform[0];
parmlu[19] = resid;

} // lu6U

    private void lu6Ut (int inform[], int m, int n, double v[], double w[],
            int lena, int luparm[], double parmlu[], double a[], int indr[], int p[], int q[], int lenr[], int locr[]) {

//integer(ip),   intent(in)    :: m, n, lena
//integer(ip),   intent(inout) :: luparm(30)
//integer(ip),   intent(out)   :: inform
//integer(ip),   intent(in)    :: indr(lena), p(m), q(n), lenr(m), locr(m)
//real(rp),      intent(in)    :: a(lena)
//real(rp),      intent(inout) :: parmlu(30), w(n)
//real(rp),      intent(out)   :: v(m)

// ------------------------------------------------------------------
// lu6Ut  solves   U'v = w.          w  is destroyed.
//
// 15 Dec 2002: First version derived from lu6sol.
// 15 Dec 2002: Current version.
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

int i, j, k, l, l1, l2, nrank, nrank1;
double resid, small, t;
final double zero = 0.0;


nrank  = luparm[15];
small  = parmlu[2];
inform[0] = 0;
nrank1 = nrank + 1;
resid  = zero;

for (k = nrank1; k <= m; k++) {
i     = p[k-1];
v[i-1]  = zero;
} // for (k = nrank1; k <= m; k++)

// Do the forward-substitution, skipping columns of U(transpose)
// when the associated element of w(*) is negligible.

for (k = 1; k <= nrank; k++) {
i      = p[k-1];
j      = q[k-1];
t      = w[j-1];
if (Math.abs(t) <= small) {
 v[i-1] = zero;
 continue;
} // if (Math.abs(t) <= small)

l1     = locr[i-1];
t      = t/a[l1-1];
v[i-1]   = t;
l2     = l1 + lenr[i-1] - 1;
l1     = l1 + 1;

//***** This loop could be coded specially.
for (l = l1; l <= l2; l++) {
 j    = indr[l-1];
 w[j-1] = w[j-1] - t*a[l-1];
} // for (l = l1; l <= l2; l++)
} // for (k = 1; k <= nrank; k++)

// Compute residual for overdetermined systems.

for (k = nrank1; k <= n; k++) {
j     = q[k-1];
resid = resid + Math.abs(w[j-1]);
} // for (k = nrank1; k <= n; k++)

// Exit.

if (resid > zero) inform[0] = 1;
luparm[9] = inform[0];
parmlu[19] = resid;

} // lu6Ut
    
    private void lu6LD (int inform[], int mode, int m, int n, double v[],
            int lena, int luparm[], double parmlu[], double a[], int indc[], int indr[], int lenc[], int locr[]) {

//integer(ip),   intent(in)    :: mode, m, n, lena
//integer(ip),   intent(inout) :: luparm(30)
//integer(ip),   intent(out)   :: inform
//integer(ip),   intent(in)    :: indc(lena), indr(lena), lenc(n), locr(m)
//real(rp),      intent(in)    :: a(lena)
//real(rp),      intent(inout) :: parmlu(30), v(m)

// -------------------------------------------------------------------
// lu6LD  assumes lu1fac has computed factors A = LU of a
// symmetric definite or quasi-definite matrix A,
// using Threshold Symmetric Pivoting (TSP),   luparm(6) = 3,
// or    Threshold Diagonal  Pivoting (TDP),   luparm(6) = 4.
// It also assumes that no updates have been performed.
// In such cases,  U = D L', where D = diag(U).
// lu6LDL returns v as follows:

// mode
// 1    v  solves   L D v = v(input).
// 2    v  solves   L|D|v = v(input).

// 15 Dec 2002: First version of lu6LD.
// 15 Dec 2002: Current version.
// 13 Dec 2011: First f90 version.
// -----------------------------------------------------------------------

// Solve L D v(new) = v  or  L|D|v(new) = v, depending on mode.
// The code for L is the same as in lu6L,
// but when a nonzero entry of v arises, we divide by
// the corresponding entry of D or |D|.

int ipiv, j, k, l, l1, ldummy, len, numL0;
double diag, small, vpiv;


numL0  = luparm[19];
small  = parmlu[2];
inform[0] = 0;
l1     = lena + 1;

for (k = 1; k <= numL0; k++) {
len   = lenc[k-1];
l     = l1;
l1    = l1 - len;
ipiv  = indr[l1-1];
vpiv  = v[ipiv-1];

if (Math.abs(vpiv) > small) {
 // ***** This loop could be coded specially.
 for (ldummy = 1; ldummy <= len; ldummy++) {
    l    = l - 1;
    j    = indc[l-1];
    v[j-1] = v[j-1] + a[l-1]*vpiv;
 } // for (ldummy = 1; ldummy <= len; ldummy++)

 // Find diag = U(ipiv,ipiv) and divide by diag or |diag|.

 l    = locr[ipiv-1];
 diag = a[l-1];
 if (mode == 2) diag = Math.abs(diag);
 v[ipiv-1] = vpiv/diag;
} // if (Math.abs(vpiv) > small)
} // for (k = 1; k <= numL0; k++)

} // lu6LD

    private void lu7add(int m, int n, int jadd, double v[], int lena, int luparm[], double parmlu[],
                       int lenL[], int lenU[], int lrow[], int nrank,
                       double a[], int indr[], int p[], int lenr[], int locr[],
                       int inform[], int klast[], double vnorm[]) {

      //integer(ip),   intent(in)    :: m, n, jadd, lena, nrank, &
      //                                p(m)
      //integer(ip),   intent(inout) :: luparm(30), lenL, lenU, lrow, &
      //                                indr(lena), lenr(m), locr(m)
      //integer(ip),   intent(out)   :: inform, klast
      //real(rp),      intent(inout) :: parmlu(30), a(lena), v(m)
      //real(rp),      intent(out)   :: vnorm

      // ------------------------------------------------------------------
      // lu7add  inserts the first nrank elements of the vector v(*)
      // as column jadd of U.  We assume that U does not yet have any
      // entries in this column.
      // Elements no larger than parmlu(3) are treated as zero.
      // klast  will be set so that the last row to be affected
      // (in pivotal order) is row p(klast).
      
      // 09 May 1988: First f77 version.
      // 13 Dec 2011: First f90 version.
      // ------------------------------------------------------------------

      int i, j, k, leni, l, lr1, lr2, minfre, nfree;
      double small;
      final double zero = 0.0;
      boolean seg1 = true;
      boolean seg2 = true;
      boolean seg3 = true;

      small  = parmlu[2];
      vnorm[0]  = zero;
      klast[0]  = 0;

      for (k = 1; k <= nrank; k++) {
         i      = p[k-1];
         if (Math.abs(v[i-1]) <= small) continue;
         klast[0]  = k;
         vnorm[0]  = vnorm[0] + Math.abs(v[i-1]);
         leni   = lenr[i-1];

         // Compress row file if necessary.

         minfre = leni + 1;
         nfree  = lena - lenL[0] - lrow[0];
         if (nfree < minfre) {
            lu1rec( m, true, luparm, lrow, lena, a, indr, lenr, locr );
            nfree  = lena - lenL[0] - lrow[0];
            if (nfree < minfre) {
            	// Not enough storage
            	inform[0] = 7;
            	return;
            }
         }

         // Move row i to the end of the row file,
         // unless it is already there.
         // No need to move if there is a gap already.

         if (leni == 0) locr[i-1] = lrow[0] + 1;
         lr1    = locr[i-1];
         lr2    = lr1 + leni - 1;
         if (lr2    ==   lrow[0]) {
        	 seg1 = false;
         }
         if (seg1) {
         if (indr[lr2] == 0) {
        	 seg2 = false;
        	 seg3 = false;
         }
         if (seg2) {
         locr[i-1] = lrow[0] + 1;

         for (l = lr1; l <= lr2; l++) {
            lrow[0]       = lrow[0] + 1;
            a[lrow[0]-1]    = a[l-1];
            j          = indr[l-1];
            indr[l-1]    = 0;
            indr[lrow[0]-1] = j;
         } // for (l = lr1; l <= lr2; l++)
         } // if (seg2)
         seg2 = true;
         } // if (seg1)
         seg1 = true;
         if (seg3) {
         lr2     = lrow[0];
         lrow[0]    = lrow[0] + 1;
         } // if (seg3)
         seg3 = true;

         // Add the element of  v.

         lr2       = lr2 + 1;
         a[lr2-1]    = v[i-1];
         indr[lr2-1] = jadd;
         lenr[i-1]   = leni + 1;
         lenU[0]      = lenU[0] + 1;
      } // for (k = 1; k <= nrank; k++);

      // Normal exit.

      inform[0] = 0;
      return;
} // lu7add

    private void lu7elm(int m, int n, int jelm, double v[], int lena, int luparm[], double parmlu[],
            int lenL[], int lenU, int lrow[], int nrank, 
            double a[], int indc[], int indr[], int p[], int q[], int lenr[], int locc[], int locr[],
            int inform[], double diag[]) {

//integer(ip),   intent(in)    :: m, n, jelm, lena, nrank
//integer(ip),   intent(in)    :: lenU, q(n)   ! not used
//integer(ip),   intent(inout) :: luparm(30), lenL, lrow,       &
//                           indc(lena), indr(lena), p(m), &
//                           lenr(m), locc(n), locr(m)
//integer(ip),   intent(out)   :: inform
//real(rp),      intent(in)    :: v(m)
//real(rp),      intent(inout) :: parmlu(30), a(lena)
//real(rp),      intent(out)   :: diag

// ------------------------------------------------------------------
// lu7elm  eliminates the subdiagonal elements of a vector  v(*),
// where  L*v = y  for some vector y.
// If  jelm > 0,  y  has just become column  jelm  of the matrix  A.
// lu7elm  should not be called unless  m  is greater than  nrank.

// inform = 0 if y contained no subdiagonal nonzeros to eliminate.
// inform = 1 if y contained at least one nontrivial subdiagonal.
// inform = 7 if there is insufficient storage.

// 09 May 1988: First f77 version.
//              No longer calls lu7for at end.  lu8rpc, lu8mod do so.
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

int lmax = 0;
    	int i, imax, k, kmax, l, l1, l2,
    minfre, nfree, nrank1;
double small, vi, vmax;
final double zero = 0.0;

small  = parmlu[2];
nrank1 = nrank + 1;
diag[0]   = zero;

// Compress row file if necessary.

minfre = m - nrank;
nfree  = lena - lenL[0] - lrow[0];
if (nfree < minfre) {
lu1rec( m, true, luparm, lrow, lena, a, indr, lenr, locr );
nfree  = lena - lenL[0] - lrow[0];
if (nfree < minfre) {
	// Not enough storage.
    inform[0] = 7;
    return;
} // if (nfree < minfre)
} // if (nfree < minfre)

// Pack the subdiagonals of  v  into  L,  and find the largest.

vmax   = zero;
kmax   = 0;
l      = lena - lenL[0] + 1;

for (k = nrank1; k <= m; k++) {
i       = p[k-1];
vi      = Math.abs(v[i-1]);
if (vi <= small) continue;
l       = l - 1;
a[l-1]    = v[i-1];
indc[l-1] = i;
if (vmax >= vi ) continue;
vmax    = vi;
kmax    = k;
lmax    = l;
} // for (k = nrank1; k <= m; k++)

if (kmax == 0) {
	//  No elments to eliminate
	inform[0] = 0;
	return;
}

// ------------------------------------------------------------------
// Remove  vmax  by overwriting it with the last packed  v(i).
// Then set the multipliers in  L  for the other elements.
// ------------------------------------------------------------------
imax       = p[kmax-1];
vmax       = a[lmax-1];
a[lmax-1]    = a[l-1];
indc[lmax-1] = indc[l-1];
l1         = l + 1;
l2         = lena - lenL[0];
lenL[0]       = lenL[0] + (l2 - l);

for (l = l1; l <= l2; l++) {
a[l-1]    = - a[l-1] / vmax;
indr[l-1] =   imax;
} // for (l = l1; l <= l2; l++)

// Move the row containing vmax to pivotal position nrank + 1.

p[kmax-1] = p[nrank1-1];
p[nrank1-1] = imax;
diag[0]      = vmax;

// ------------------------------------------------------------------
// If jelm is positive, insert  vmax  into a new row of  U.
// This is now the only subdiagonal element.
// ------------------------------------------------------------------

if (jelm > 0) {
lrow[0]       = lrow[0] + 1;
locr[imax-1] = lrow[0];
lenr[imax-1] = 1;
a[lrow[0]-1]    = vmax;
indr[lrow[0]-1] = jelm;
} // if (jelm > 0)

inform[0] = 1;
return;
} // lu7elm
    
    private void lu7cyc(int kfirst, int klast, int p[]) {

    //integer(ip),   intent(in)    :: kfirst, klast
    //integer(ip),   intent(inout) :: p(klast)

    // ------------------------------------------------------------------
    // lu7cyc performs a cyclic permutation on the row or column ordering
    // stored in p, moving entry kfirst down to klast.
    // If kfirst .ge. klast, lu7cyc should not be called.
    // Sometimes klast = 0 and nothing should happen.
    
    // 09 May 1988: First f77 version.
    // 13 Dec 2011: First f90 version.
    // ------------------------------------------------------------------

    int ifirst, k;

    if (kfirst < klast) {
       ifirst = p[kfirst-1];

       for (k = kfirst; k <= klast - 1; k++) {
          p[k-1] = p[k];
       }

       p[klast-1] = ifirst;
    } // if (kfirst < klast)

} // lu7cyc
    
    private void lu7for(int m, int n, int kfirst, int klast, int lena, int luparm[], double parmlu[],
            int lenL[], int lenU[], int lrow[],
            double a[], int indc[], int indr[], int p[], int q[], int lenr[], int locc[], int locr[],
            int inform[], double diag[]) {

//integer(ip),   intent(in)    :: m, n, kfirst, klast, lena
//integer(ip),   intent(in)    :: q(n)
//integer(ip),   intent(inout) :: luparm(30), lenL, lenU, lrow
//integer(ip),   intent(inout) :: indc(lena), indr(lena),     &
//                           p(m), lenr(m), locc(n), locr(m)
//integer(ip),   intent(out)   :: inform
//real(rp),      intent(inout) :: parmlu(30), a(lena)
//real(rp),      intent(out)   :: diag

// ------------------------------------------------------------------
// lu7for  (forward sweep) updates the LU factorization A = L*U
// when row iw = p(klast) of U is eliminated by a forward
// sweep of stabilized row operations, leaving p*U*q upper triangular.

// The row permutation p is updated to preserve stability and/or
// sparsity.  The column permutation q is not altered.

// kfirst  is such that row p(kfirst) is the first row involved
// in eliminating row  iw.  (Hence,  kfirst  marks the first nonzero
// in row  iw  in pivotal order.)  If  kfirst  is unknown it may be
// input as  1.

// klast   is such that row p(klast) is the row being eliminated.
// klast   is not altered.

// lu7for  should be called only if  kfirst .le. klast.
// If  kfirst = klast,  there are no nonzeros to eliminate, but the
// diagonal element of row p(klast) may need to be moved to the
// front of the row.

// On entry,  locc(*)  must be zero.

// On exit:
// inform = 0  if row iw has a nonzero diagonal (could be small).
// inform = 1  if row iw has no diagonal.
// inform = 7  if there is not enough storage to finish the update.

// On a successful exit (inform le 1),  locc(*)  will again be zero.

//    Jan 1985: Final f66 version.
// 09 May 1988: First f77 version.
// 13 Dec 2011: First f90 version.
// ------------------------------------------------------------------

boolean swappd;
int ldiag = 1;
int iv, iw, j, jfirst, jlast, jv,
    k, kbegin, kstart, kstop,
    l, lenv, lenw, lfirst, limit,
    lv, lv1, lv2, lv3, lw, lw1, lw2,
    minfre, nfree;
double amult, Ltol, Uspace, small, vj, wj;
final double zero = 0.0;
boolean seg1 = true;
boolean seg2 = true;
boolean seg3 = true;
boolean seg4 = true;

Ltol   = parmlu[1];
small  = parmlu[2];
Uspace = parmlu[5];
kbegin = kfirst;
swappd = false;

// We come back here from below if a row interchange is performed.

do {
iw     = p[klast-1];
lenw   = lenr[iw-1];
if (lenw   ==   0  ) {
	seg1 = false;
	break;
}
lw1    = locr[iw-1];
lw2    = lw1 + lenw - 1;
jfirst = q[kbegin-1];
if (kbegin >= klast) {
	seg2 = false;
}
if (seg2) {
// Make sure there is room at the end of the row file
// in case row  iw  is moved there and fills in completely.

minfre = n + 1;
nfree  = lena - lenL[0] - lrow[0];
if (nfree < minfre) {
lu1rec( m, true, luparm, lrow, lena, a, indr, lenr, locr );
lw1    = locr[iw-1];
lw2    = lw1 + lenw - 1;
nfree  = lena - lenL[0] - lrow[0];
if (nfree < minfre) {
	 // Not enough storage.
     inform[0] = 7;
     return;
} // if (nfree < minfre)
} // if (nfree < minfre)

// Set markers on row iw.

for (l = lw1; l <= lw2; l++) {
j       = indr[l-1];
locc[j-1] = l;
} // for (l = lw1; l <= lw2; l++)

// ==================================================================
// Main elimination loop.
// ==================================================================
kstart = kbegin;
kstop  = Math.min( klast, n );

for (k = kstart; k <= kstop; k++) {
jfirst = q[k-1];
lfirst = locc[jfirst-1];
if (lfirst == 0) {
	swappd = false;
	continue;
}

// Row  iw  has its first element in column  jfirst.

wj     = a[lfirst-1];
if (k == klast) {
	swappd = false;
	continue;
}

// ---------------------------------------------------------------
// We are about to use the first element of row  iv
// to eliminate the first element of row  iw.
// However, we may wish to interchange the rows instead,
// to preserve stability and/or sparsity.
// ---------------------------------------------------------------
iv     = p[k-1];
lenv   = lenr[iv-1];
lv1    = locr[iv-1];
vj     = zero;
if (lenv      !=   0   ) {
	if (indr[lv1-1] == jfirst) {
		vj     = a[lv1-1];
		if (         swappd          ) {
			seg3 = false;
		}
		else { // not swappd
			if (Ltol * Math.abs(wj) <  Math.abs(vj)) {
				seg3 = false;
			}
			else {
				if (Ltol * Math.abs(vj) >=  Math.abs(wj)) {
					if (          lenv <= lenw   ) {
						seg3 = false;
					}
				}
			}
		}  // else not swappd
	} // if (indr[lv1-1] == jfirst)
} // if (lenv != 0)

if (seg3) {
// ---------------------------------------------------------------
// Interchange rows  iv  and  iw.
// ---------------------------------------------------------------
p[klast-1] = iv;
p[k-1]     = iw;
kbegin   = k;
swappd   = true;
break;
} // if (seg3)
seg3 = true;

// ---------------------------------------------------------------
// Delete the eliminated element from row  iw
// by overwriting it with the last element.
// ---------------------------------------------------------------
a[lfirst-1]    = a[lw2-1];
jlast        = indr[lw2-1];
indr[lfirst-1] = jlast;
indr[lw2-1]    = 0;
locc[jlast-1]  = lfirst;
locc[jfirst-1] = 0;
lenw         = lenw - 1;
lenU[0]         = lenU[0] - 1;
if (lrow[0] == lw2) lrow[0] = lrow[0] - 1;
lw2          = lw2  - 1;

// ---------------------------------------------------------------
// Form the multiplier and store it in the  L  file.
// ---------------------------------------------------------------
if (Math.abs(wj) <= small) {
    swappd = false;
    continue;
}
amult   = - wj/vj;
l       = lena - lenL[0];
a[l-1]    = amult;
indr[l-1] = iv;
indc[l-1] = iw;
lenL[0]    = lenL[0] + 1;

// ---------------------------------------------------------------
// Add the appropriate multiple of row  iv  to row  iw.
// We use two different inner loops.  The first one is for the
// case where row  iw  is not at the end of storage.
// ---------------------------------------------------------------
if (lenv == 1) {
	swappd = false;
	continue;
}
lv2    = lv1 + 1;
lv3    = lv1 + lenv - 1;
if (lw2 == lrow[0]) {
	seg4 = false;
}

if (seg4) {
// ...............................................................
// This inner loop will be interrupted only if
// fill-in occurs enough to bump into the next row.
// ...............................................................
for (lv = lv2; lv <= lv3; lv++) {
 jv = indr[lv-1];
 lw = locc[jv-1];

 if (lw > 0) {         // No fill-in.
    a[lw-1] = a[lw-1] + amult*a[lv-1];
    if (Math.abs(a[lw-1]) <= small) {  // Delete small computed element.
       a[lw-1]     = a[lw2-1];
       j         = indr[lw2-1];
       indr[lw-1]  = j;
       indr[lw2-1] = 0;
       locc[j-1]   = lw;
       locc[jv-1]  = 0;
       lenU[0]      = lenU[0] - 1;
       lenw      = lenw - 1;
       lw2       = lw2  - 1;
    } // if (Math.abs(a[lw-1]) <= small)
 } // if (lw > 0)
 else  {  // Row iw doesn't have an element in column jv yet
          // so there is a fill-in.
    if (indr[lw2] != 0) break;
    lenU[0]      = lenU[0] + 1;
    lenw      = lenw + 1;
    lw2       = lw2  + 1;
    a[lw2-1]    = amult * a[lv-1];
    indr[lw2-1] = jv;
    locc[jv-1]  = lw2;
 } // else lw <= 0
} // for (lv = lv2; lv <= lv3; lv++)
if (lv > lv3) {
	swappd = false;
	continue;
}

// Fill-in interrupted the previous loop.
// Move row  iw  to the end of the row file.

lv2      = lv;
locr[iw-1] = lrow[0] + 1;

for (l = lw1; l <= lw2; l++) {
 lrow[0]       = lrow[0] + 1;
 a[lrow[0]-1]    = a[l-1];
 j          = indr[l-1];
 indr[l-1]    = 0;
 indr[lrow[0]-1] = j;
 locc[j-1]    = lrow[0];
} // for (l = lw1; l <= lw2; l++)

lw1    = locr[iw-1];
lw2    = lrow[0];
} // if (seg4)
seg4 = true;

// ...............................................................
// Inner loop with row iw at the end of storage.
// ...............................................................
for (lv = lv2; lv <= lv3; lv++) {
 jv     = indr[lv-1];
 lw     = locc[jv-1];

 if (lw > 0) {       // No fill-in
    a[lw-1] = a[lw-1] + amult*a[lv-1];
    if (Math.abs(a[lw-1]) <= small) {    // Delete small computed element
       a[lw-1]     = a[lw2-1];
       j         = indr[lw2-1];
       indr[lw-1]  = j;
       indr[lw2-1] = 0;
       locc[j-1]   = lw;
       locc[jv-1]  = 0;
       lenU[0]      = lenU[0] - 1;
       lenw      = lenw - 1;
       lw2       = lw2  - 1;
    } // if (Math.abs(a[lw-1]) <= small)
 } // if (lw > 0)
 else   {        // Row iw doesn't have an element in column jv yet
                 // so there is a fill-in
    lenU[0]      = lenU[0] + 1;
    lenw      = lenw + 1;
    lw2       = lw2  + 1;
    a[lw2-1]    = amult * a[lv-1];
    indr[lw2-1] = jv;
    locc[jv-1]  = lw2;
 } // else lw <= 0
} // for (lv = lv2; lv <= lv3; lv++)

lrow[0]   = lw2;

// The  k-th  element of row  iw  has been processed.
// Reset  swappd  before looking at the next element.

swappd = false;
} // for (k = kstart; k <= kstop; k++)

// =================================================================
// End of main elimination loop.
// ==================================================================

// Cancel markers on row  iw.

lenr[iw-1] = lenw;
if (lenw == 0) {
	seg1 = false;
	break;
}
for (l = lw1; l <= lw2; l++) {
j       = indr[l-1];
locc[j-1] = 0;
} // for (l = lw1; l <= lw2; l++)
} // if (seg2)
seg2 = true;

// Move the diagonal element to the front of row iw.
// At this stage, lenw > 0 and klast <= n.

for (l = lw1; l <= lw2; l++) {
ldiag = l;
if (indr[l-1] == jfirst) break;
} // for (l = lw1; l <= lw2; l++)
if (l > lw2) {
	seg1 = false;
	break;
}

diag[0]        = a[ldiag-1];
a[ldiag-1]    = a[lw1-1];
a[lw1-1]      = diag[0];
indr[ldiag-1] = indr[lw1-1];
indr[lw1-1]   = jfirst;

// If an interchange is needed, repeat from the beginning with the
// new row iw, knowing that the opposite interchange cannot occur.

} while (swappd);
if (seg1) {
inform[0] = 0;
//Force a compression if the file for U is much longer than the
//no. of nonzeros in U (i.e. if lrow is much bigger than lenU).
//This should prevent memory fragmentation when there is far more
//memory than necessary (i.e. when lena is huge).

limit  = (int)(Uspace*(double)(lenU[0])) + m + n + 1000;
if (lrow[0] > limit) {
lu1rec( m, true, luparm, lrow, lena, a, indr, lenr, locr );
}
return;
} // if (seg1)

// Singular

diag[0]   = zero;
inform[0] = 1;

// Force a compression if the file for U is much longer than the
// no. of nonzeros in U (i.e. if lrow is much bigger than lenU).
// This should prevent memory fragmentation when there is far more
// memory than necessary (i.e. when lena is huge).

limit  = (int)(Uspace*(double)(lenU[0])) + m + n + 1000;
if (lrow[0] > limit) {
lu1rec( m, true, luparm, lrow, lena, a, indr, lenr, locr );
}
return;
} // lu7for

    private void lu7rnk(int m, int n, int jsing, int lena, double parmlu[],
            int lenL[], int lenU[], int lrow[], int nrank[],
            double a[], int indc[], int indr[], int p[], int q[], int lenr[], int locc[], int locr[],
            int inform[], double diag[]) {

//integer(ip),   intent(in)    :: m, n, jsing, lena,      &
//                           p(m)
//integer(ip),   intent(inout) :: lenL, lenU, lrow, nrank, &
//                           indc(lena), indr(lena), q(n),        &
//                           lenr(m), locc(n), locr(m)
//integer(ip),   intent(out)   :: inform
//real(rp),      intent(inout) :: parmlu(30)  ! not used
//real(rp),      intent(inout) :: diag, a(lena)

// ------------------------------------------------------------------
// lu7rnk (check rank) assumes U is currently nrank by n
// and determines if row nrank contains an acceptable pivot.
// If not, the row is deleted and nrank is decreased by 1.

// jsing is an input parameter (not altered).  If jsing is positive,
// column jsing has already been judged dependent.  A substitute
// (if any) must be some other column.

// On exit,
// inform = -1 if nrank decreases by 1
//        =  0 if nrank stays the same
//        =  1 if there's a fatal error.  (Currently we stop.)

// -- Jul 1987: First version.
// 09 May 1988: First f77 version.
// 13 Dec 2011: First f90 version.
// 01 Jan 2012: luparm not used.
// ------------------------------------------------------------------

int l1 = 1;
int l2 = 1;
int iw, jmax, kmax, l, lenw, lmax;
double Umax, Utol1;
final double zero = 0.0;

Utol1    = parmlu[3];
diag[0]     = zero;

// Find Umax, the largest element in row nrank.

iw       = p[nrank[0]-1];
lenw     = lenr[iw-1];
if (lenw != 0) {
l1       = locr[iw-1];
l2       = l1 + lenw - 1;
Umax     = zero;
lmax     = l1;

for (l = l1; l <= l2; l++) {
if (Umax < Math.abs(a[l-1])) {
 Umax  = Math.abs(a[l-1]);
 lmax  = l;
} //if (Umax < Math.abs(a[l-1]))
} // for (l = l1; l <= l2; l++)

// Find which column that guy is in (in pivotal order).
// Interchange him with column nrank, then move him to be
// the new diagonal at the front of row nrank.

diag[0]   = a[lmax-1];
jmax   = indr[lmax-1];
l      = 0;

for (kmax = nrank[0]; kmax <= n; kmax++) {
if (q[kmax-1] == jmax) {
 l = kmax;   // l allows check below for fatal error
 break;
} // if (q[kmax-1] == jmax)
} // for (kmax = nrank[0]; kmax <= n; kmax++)

if (l == 0) {
	// Fatal error
	// 15 Dec 2011: Fatal error (should never happen!).
	// This is a safeguard during work on the f90 version.

    inform[0] = 1;
	UI.setDataText("Fatal error in LUSOL lu7rnk.  Stopping now\n");
	//stop
    return;
} // if (l == 0)

q[kmax-1]    = q[nrank[0]-1];
q[nrank[0]-1]   = jmax;
a[lmax-1]    = a[l1-1];
a[l1-1]      = diag[0];
indr[lmax-1] = indr[l1-1];
indr[l1-1]   = jmax;

// See if the new diagonal is big enough.

if ((Umax > Utol1) && (jmax != jsing)) {

// ------------------------------------------------------------------
// The rank stays the same.
// ------------------------------------------------------------------
inform[0] = 0;
return;
} // if ((Umax > Utol1) && (jmax != jsing))
} // if (lenw != 0)

// ------------------------------------------------------------------
// The rank decreases by one.
// ------------------------------------------------------------------
inform[0] = -1;
nrank[0]  = nrank[0] - 1;

if (lenw > 0) {       // Delete row nrank from U.

lenU[0]     = lenU[0] - lenw;
lenr[iw-1] = 0;
for (l = l1; l <= l2; l++) {
 indr[l-1] = 0;
} // for (l = l1; l <= l2; l++) 

if (l2 == lrow[0]) {

 // This row was at the end of the data structure.
 // We have to reset lrow.
 // Preceding rows might already have been deleted, so we
 // have to be prepared to go all the way back to 1.

 for (l = 1; l <= l2; l++) {
    if (indr[lrow[0]-1] > 0) return;
    lrow[0]  = lrow[0] - 1;
 } // for (l = 1; l <= l2; l++)
} // if (l2 == lrow[0])
} // if (lenW > 0)
return;
} // lu7rnk

    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //
    //     File  lusol6b.f
    //
    //     lu6mul   lu6prt   lu6set
    //
    // 03 Mar 2004: lusol6b.f is essentially lu6b.for from VMS days.
    //              integer*4 changed to integer  .
    // 14 Jul 2011: lu6mul's v = L'*v fixed.
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

          private void lu6mul(int mode, int m, int n, double v[], double w[],
                             int lena, int luparm[], double parmlu[],
                             double a[], int indc[], int indr[], int ip[], int iq[],
                             int lenc[], int lenr[], int locc[], int locr[]) {

          //implicit           double precision (a-h,o-z)
          //integer            luparm(30)
          //double precision   parmlu(30), a(lena), v(m), w(n)
          //integer            indc(lena), indr(lena), ip(m), iq(n)
          //integer            lenc(n), lenr(m)
          //integer            locc(n), locr(m)

    //     ------------------------------------------------------------------
    //     lu6mul  uses the factorization   A = L*U   as follows...
    
    //     mode
    //     ----
    //      1    v  is changed to  L*v.             w  is not touched.
    //      2    v  is changed to  L(t)*v.          w  is not touched.
    //      3    v  is set to      U*w.             w  is not altered.
    //      4    w  is set to      U(t)*v.          v  is not altered.
    //      5    v  is set to      A*w.             w  is not altered.
    //      6    w  is set to      A(t)*v.          v  is changed to  L(t)*v.
    //
    //     If  mode .ge. 3,  v  and  w  must not be the same arrays.
    
    //     lenc(*)  and  locc(*)  are not used.
    
    //     09 May 1988: First F77 version.
    //     30 Jan 1996: Converted to lower case (finally!).
    //     03 Mar 2004: Time to abolish the computed go to!!
    //     ------------------------------------------------------------------
          int i, j, k, klast, l, l1, l2, lenl, nrank;
          double t;
          final double zero = 0.0;

          nrank  = luparm[15];
          lenl   = luparm[22];
          //!!! go to (100, 200, 300, 400, 300, 200), mode
          //if (mode .eq. 1) go to 100
          //if (mode .eq. 2) go to 200
          //if (mode .eq. 3) go to 300
          //if (mode .eq. 4) go to 400
          //if (mode .eq. 5) go to 300
          //if (mode .eq. 6) go to 200

    //     ==================================================================
    //     mode = 1 or 5.    Set  v = L*v.
    //     ==================================================================
      if (mode == 1) {
          l1     = lena + 1 - lenl;
          for (l = l1; l <= lena; l++) {
             i     = indr[l-1];
             if (v[i-1] != zero) {
                j     = indc[l-1];
                v[j-1]  = v[j-1] - a[l-1]*v[i-1];
             } // if (v[i-1] != zero)
          } // for (l = l1; l <= lena; l++)

          return;
      } // if (mode == 1)

    //     ==================================================================
    //     mode = 2 or 6.    Set  v = L(t)*v.
    //     14 Jul 2011: We have to run forward thru the columns of L.
    //                  The first column is at the end of memory.
    //     ==================================================================
      else if ((mode == 2) || (mode == 6)) {
          l1 = lena + 1 - lenl;
          for (l = lena; l >= l1; l--) {
             j     = indc[l-1];
             if (v[j-1] != zero) {
                i     = indr[l-1];
                v[i-1]  = v[i-1]  -  a[l-1] * v[j-1];
             } // if (v[j-1] != zero) 
          } // for (l = lena; l >= l1; l--) 

          //if (mode .eq. 6) go to 400
          if (mode == 2) {
        	  return;
          }
          
          // Find the last nonzero in  v(*).

          for (klast = m; klast >= 1; klast--) {
             i     = ip[klast-1];
             if (v[i-1] != zero) break;
          } // for (klast = m; klast >= 1; klast--)

          klast  = Math.min( klast, nrank );

          for (j = 1; j <= n; j++) {
             w[j-1]  = zero;
          } // for (j = 1; j <= n; j++)

          for (k = 1; k <= klast; k++) {
             i     = ip[k-1];
             t     = v[i-1];
             if (t == zero) continue;
             l1    = locr[i-1];
             l2    = l1 + lenr[i-1] - 1;

             for (l = l1; l <= l2; l++) {
                j     = indr[l-1];
                w[j-1]  = w[j-1]  +  a[l-1] * t;
             } // for (l = l1; l <= l2; l++)
          } // for (k = 1; k <= klast; k++)
          return;
      } // if ((mode == 2) || (mode == 6))

    //     ==================================================================
    //     mode = 3 or 5.    set  v = U*w.
    //     ==================================================================

          // Find the last nonzero in  w(*).
      else if ((mode == 3) || (mode == 5)) {
          for (klast = n; klast >= 1; klast--) {
             j     = iq[klast-1];
             if (w[j-1] != zero) break;
          } //  for (klast = n; klast >= 1; klast--)

          klast  = Math.min( klast, nrank );
          for (k = klast + 1; k <= m; k++) {
             i     = ip[k-1];
             v[i-1]  = zero;
          } // for (k = klast + 1; k <= m; k++)

          // Form U*w, using rows 1 to klast of U.

          for (k = 1; k <= klast; k++) {
             t     = zero;
             i     = ip[k-1];
             l1    = locr[i-1];
             l2    = l1 + lenr[i-1] - 1;

             for (l = l1; l <= l2; l++) {
                j     = indr[l-1];
                t     = t  +  a[l-1] * w[j-1];
             } //  for (l = l1; l <= l2; l++)

             v[i-1]  = t;
          } // for (k = 1; k <= klast; k++)

          //if (mode .eq. 5) go to 100
          if (mode == 3) {
              return;
          }
          
          l1     = lena + 1 - lenl;
          for (l = l1; l <= lena; l++) {
             i     = indr[l-1];
             if (v[i-1] != zero) {
                j     = indc[l-1];
                v[j-1]  = v[j-1] - a[l-1]*v[i-1];
             } // if (v[i-1] != zero)
          } // for (l = l1; l <= lena; l++)

          return;
      } // else if ((mode == 3) || (mode == 5))

    //     ==================================================================
    //     mode = 4.    set  w = U(transpose)*v.
    //     ==================================================================
      else if (mode == 4) {

          // Find the last nonzero in  v(*).

          for (klast = m; klast >= 1; klast--) {
             i     = ip[klast-1];
             if (v[i-1] != zero) break;
          } // for (klast = m; klast >= 1; klast--)

          klast  = Math.min( klast, nrank );

          for (j = 1; j <= n; j++) {
             w[j-1]  = zero;
          } // for (j = 1; j <= n; j++)

          for (k = 1; k <= klast; k++) {
             i     = ip[k-1];
             t     = v[i-1];
             if (t == zero) continue;
             l1    = locr[i-1];
             l2    = l1 + lenr[i-1] - 1;

             for (l = l1; l <= l2; l++) {
                j     = indr[l-1];
                w[j-1]  = w[j-1]  +  a[l-1] * t;
             } // for (l = l1; l <= l2; l++)
          } // for (k = 1; k <= klast; k++)
          return;
      } // else if (mode == 4)

} // lu6mul
          
//          !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

          private void lu6prt(int m, int n, double v[], double w[],
                             int lena, int luparm[], double parmlu[],
                             double a[], int indc[], int indr[], int ip[], int iq[],
                             int lenc[], int lenr[], int locc[], int locr[] ) {

//          implicit           double precision (a-h,o-z)
//          integer            luparm(30)
//          double precision   parmlu(30), a(lena), v(m), w(n)
//          integer            indc(lena), indr(lena), ip(m), iq(n)
//          integer            lenc(n), lenr(m)
//          integer            locc(n), locr(m)

//    !     ------------------------------------------------------------------
//    !     lu6prt  prints details of the current LU factorization, and
//    !     prints the matrix A = L*U row by row.  The amount of output
//    !     is controlled by lprint = luparm(2).
//    !
//    !     If  lprint = 0,  nothing is printed.
//    !     If  lprint = 1,  current LU statistics are printed.
//    !     If  lprint = 2,  the leading 10 by 10 submatrix is printed.
//    !     If  lprint = 3   or more, all rows are printed.
//    !
//    !     lenc(*), locc(*)  are not used.
//    !
//    !     09 May 1988: First F77 version.
//    !     03 Mar 2004: Current version.
//    !     ------------------------------------------------------------------

          final double zero = 0.0;
          final double one = 1.0;
          int lprint, imax, jmax, lamin, nrank, lenl, lenu, lrow, ncp, mersum;
          double amax, elmax, umax, dumin, avgmer, floatm, growth;
          int i, j, k;

          lprint = luparm[1];
          imax   = m;
          jmax   = n;
          if (lprint <= 0) {
        	  return;
          }
          if (lprint <= 2) {
        	  imax = Math.min( imax, 10 );
          }
          if (lprint <= 2) {
        	  jmax = Math.min( jmax, 10 );
          }
          UI.setDataText("m = " + m + " n = " + n + " lena = " + lena + "\n");

//    !     --------------------------------
//    !     Print LU statistics.
//    !     --------------------------------
          lamin  = luparm[12];
          nrank  = luparm[15];
          lenl   = luparm[22];
          lenu   = luparm[23];
          lrow   = luparm[24];
          ncp    = luparm[25];
          mersum = luparm[26];
          amax   = parmlu[9];
          elmax  = parmlu[10];
          umax   = parmlu[11];
          dumin  = parmlu[13];

          avgmer = mersum;
          floatm = m;
          avgmer = avgmer / floatm;
          growth = umax / (amax + 1.0e-20);
          UI.setDataText("LU factorization statistics:\n");
          UI.setDataText("lamin = " + lamin + " lrow = " + lrow + "\n");
          UI.setDataText("ncp = " + ncp + " avgmer = " + nf.format(avgmer) + "\n");
          UI.setDataText("lenl = " + lenl + " lenu = " + lenu + "  nrank = " + nrank + "\n");
          UI.setDataText("elmax = " + nf.format(elmax) + " amax = " + nf.format(amax) + " umax = " + nf.format(umax) + "\n");
          UI.setDataText("dumin = " + nf.format(dumin) + " growth = " + nf.format(growth) + "\n");
          UI.setDataText("Row permutation ip:\n");
          for (i = 0; i < imax; i++) {
              if ((i != 0) && ((i % 10) == 0)) {
            	  UI.setDataText("\n" + ip[i] + " ");
              }
              else {
            	  UI.setDataText(ip[i] + " ");
              }
          }
          UI.setDataText("\n");
          UI.setDataText("Column permutation iq:\n");
          for (j = 0; j < jmax; j++) {
              if ((j != 0) && ((j % 10) == 0)) {
            	  UI.setDataText("\n" + iq[j] + " ");
              }
              else {
            	  UI.setDataText(iq[j] + " ");
              }
          }
          UI.setDataText("\n");
          if (lprint <= 1) {
        	  return;
          }

//    !     -------------------------------------------------------
//    !     lprint = 2 or more.    Print the first imax rows of  A.
//    !     -------------------------------------------------------
          for (i = 0; i < imax; i++) {
             for (k = 0; k < m; k++) {
                v[k] = zero;
             } // for (k = 0; k < m; k++)
             v[i]   = one;  // v = i-th unit vector

             // Set  w = A(t)*v = U(t)*L(t)*v.

             lu6mul( 6, m, n, v, w,
                     lena, luparm, parmlu,
                     a, indc, indr, ip, iq, lenc, lenr, locc, locr );

             UI.setDataText("A row " + (i+1) + ":\n");
             for (j = 0; j < jmax; j++) {
                 if ((j != 0) && ((j % 10) == 0)) {
               	  UI.setDataText("\n" + w[j] + " ");
                 }
                 else {
               	  UI.setDataText(w[j] + " ");
                 }
             }
             UI.setDataText("\n");
          } // for (i = 0; i < imax; i++)
          return;

          } // lu6prt

}