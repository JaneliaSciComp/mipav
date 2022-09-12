package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
This is a port of the FORTRAN 77 L-BFGS-B code to Java
L-BFGS-B is released under the “New BSD License” (aka “Modified BSD License”        
or “3-clause license”) 
===========   L-BFGS-B (version 3.0.  April 25, 2011  ===================

     This is a modified version of L-BFGS-B. Minor changes in the updated 
     code appear preceded by a line comment as follows 
  
     c-jlm-jn 

     Major changes are described in the accompanying paper:

         Jorge Nocedal and Jose Luis Morales, Remark on "Algorithm 778: 
         L-BFGS-B: Fortran Subroutines for Large-Scale Bound Constrained 
         Optimization"  (2011). To appear in  ACM Transactions on 
         Mathematical Software,

     The paper describes an improvement and a correction to Algorithm 778. 
     It is shown that the performance of the algorithm can be improved 
     significantly by making a relatively simple modication to the subspace 
     minimization phase. The correction concerns an error caused by the use 
     of routine dpmeps to estimate machine precision. 

     The total work space **wa** required by the new version is 
 
                  2*m*n + 11m*m + 5*n + 8*m 

     the old version required 

                  2*m*n + 12m*m + 4*n + 12*m 


            J. Nocedal  Department of Electrical Engineering and
                        Computer Science.
                        Northwestern University. Evanston, IL. USA


           J.L Morales  Departamento de Matematicas, 
                        Instituto Tecnologico Autonomo de Mexico
                        Mexico D.F. Mexico.

                        March  2011    
                                                             

3-clause license ("New BSD License" or "Modified BSD License")
New BSD License
Author	Regents of the University of California
Publisher	Public Domain
Published	July 22, 1999[8]
DFSG compatible	Yes[7]
FSF approved	Yes[1]
OSI approved	Yes[3]
GPL compatible	Yes[1]
Copyleft	No[1]
Copyfree	Yes
Linking from code with a different license	Yes

The advertising clause was removed from the license text in the official BSD on July 22, 1999 by William Hoskins, Director of 
the Office of Technology Licensing for UC Berkeley.[8] Other BSD distributions removed the clause, but many similar clauses 
remain in BSD-derived code from other sources, and unrelated code using a derived license.

While the original license is sometimes referred to as "BSD-old", the resulting 3-clause version is sometimes referred to by 
"BSD-new." Other names include "New BSD", "revised BSD", "BSD-3", or "3-clause BSD". This version has been vetted as 
an Open source license by the OSI as the "The BSD License".[3] The Free Software Foundation, which refers to the license 
as the "Modified BSD License", states that it is compatible with the GNU GPL. The FSF encourages users to be specific 
when referring to the license by name (i.e. not simply referring to it as "a BSD license" or "BSD-style") to avoid confusion with 
the original BSD license.[1]

This version allows unlimited redistribution for any purpose as long as its copyright notices and the license's disclaimers of 
warranty are maintained. The license also contains a clause restricting use of the names of contributors for endorsement of a 
derived work without specific permission.

Copyright (c) <year>, <copyright holder>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

public abstract class L_BFGS_B {
	
	public L_BFGS_B() {
		
	}
	
	private String fileDir;
	
	public void setulb(int n, int m, double x[], double l[], double u[], int nbd[], 
			double f[], double g[], double factr, double pgtol,
		    String task[], int iprint, String csave[], boolean lsave[], int isave[], 
		    double dsave[], final String fileDir) {
		this.fileDir = fileDir;
		 
		//      character*60     task, csave
		//      logical          lsave(4)
		//      integer          n, m, iprint, 
		//     +                 nbd(n), iwa(3*n), isave(44)
		//      double precision f, factr, pgtol, x(n), l(n), u(n), g(n),
		
		//-jlm-jn
		//     +                 wa(2*m*n + 5*n + 11*m*m + 8*m), dsave(29)
		 
		//     ************
		
		//     Subroutine setulb
		
		//     This subroutine partitions the working arrays wa and iwa, and 
		//       then uses the limited memory BFGS method to solve the bound
		//       constrained optimization problem by calling mainlb.
		//       (The direct method will be used in the subspace minimization.)
		
		//     n is an integer variable.
		//       On entry n is the dimension of the problem.
		//       On exit n is unchanged.
		
		//     m is an integer variable.
		//       On entry m is the maximum number of variable metric corrections
		//         used to define the limited memory matrix.
		//       On exit m is unchanged.
		
		//     x is a double precision array of dimension n.
		//       On entry x is an approximation to the solution.
		//       On exit x is the current approximation.
		
		//     l is a double precision array of dimension n.
		//       On entry l is the lower bound on x.
		//       On exit l is unchanged.
		
		//     u is a double precision array of dimension n.
		//       On entry u is the upper bound on x.
		//       On exit u is unchanged.
		
		//     nbd is an integer array of dimension n.
		//       On entry nbd represents the type of bounds imposed on the
		//         variables, and must be specified as follows:
		//         nbd(i)=0 if x(i) is unbounded,
		//                1 if x(i) has only a lower bound,
		//                2 if x(i) has both lower and upper bounds, and
		//                3 if x(i) has only an upper bound.
		//       On exit nbd is unchanged.
		
		//     f is a double precision variable.
		//       On first entry f is unspecified.
		//       On final exit f is the value of the function at x.
		
		//     g is a double precision array of dimension n.
		//       On first entry g is unspecified.
		//       On final exit g is the value of the gradient at x.
		
		//     factr is a double precision variable.
		//       On entry factr >= 0 is specified by the user.  The iteration
		//         will stop when
		
		//         (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
		
		//         where epsmch is the machine precision, which is automatically
		//         generated by the code. Typical values for factr: 1.d+12 for
		//         low accuracy; 1.d+7 for moderate accuracy; 1.d+1 for extremely
		//         high accuracy.
		//       On exit factr is unchanged.
		
		//     pgtol is a double precision variable.
		//       On entry pgtol >= 0 is specified by the user.  The iteration
		//         will stop when
		
		//                 max{|proj g_i | i = 1, ..., n} <= pgtol
		
		//         where pg_i is the ith component of the projected gradient.   
		//       On exit pgtol is unchanged.
		
		//     wa is a double precision working array of length 
		//       (2mmax + 5)nmax + 12mmax^2 + 12mmax.
		
		//     iwa is an integer working array of length 3nmax.
		
		//     task is a working string of characters of length 60 indicating
		//       the current job when entering and quitting this subroutine.
		
		//     iprint is an integer variable that must be set by the user.
		//       It controls the frequency and type of output generated:
		//        iprint<0    no output is generated;
		//        iprint=0    print only one line at the last iteration;
		//        0<iprint<99 print also f and |proj g| every iprint iterations;
		//        iprint=99   print details of every iteration except n-vectors;
		//        iprint=100  print also the changes of active set and final x;
		//        iprint>100  print details of every iteration including x and g;
		//       When iprint > 0, the file iterate.dat will be created to
		//                        summarize the iteration.
		
		//     csave is a working string of characters of length 60.
		
		//     lsave is a logical working array of dimension 4.
		//       On exit with 'task' = NEW_X, the following information is 
		//                                                             available:
		//         If lsave(1) = .true.  then  the initial X has been replaced by
		//                                     its projection in the feasible set;
		//         If lsave(2) = .true.  then  the problem is constrained;
		//         If lsave(3) = .true.  then  each variable has upper and lower
		//                                     bounds;
		
		//     isave is an integer working array of dimension 23.
		//       On exit with 'task' = NEW_X, the following information is 
		//                                                             available:
		//         isave[0] = the total number of intervals explored in the 
		//                         search of Cauchy points;
		//         isave[4] = the total number of skipped BFGS updates before 
		//                         the current iteration;
		//         isave[8] = the number of current iteration;
		//         isave[9] = the total number of BFGS updates prior the current
		//                         iteration;
		//         isave[11] = the number of intervals explored in the search of
		//                         Cauchy point in the current iteration;
		//         isave[12] = the total number of function and gradient 
		//                         evaluations;
		//         isave[14] = the number of function value or gradient
		//                                  evaluations in the current iteration;
		//         if isave[15] = 0  then the subspace argmin is within the box;
		//         if isave[15] = 1  then the subspace argmin is beyond the box;
		//         isave[16] = the number of free variables in the current
		//                         iteration;
		//         isave[17] = the number of active constraints in the current
		//                         iteration;
		//         n + 1 - isave[18] = the number of variables leaving the set of
		//                           active constraints in the current iteration;
		//         isave[19] = the number of variables entering the set of active
		//                         constraints in the current iteration.
		
		//     dsave is a double precision working array of dimension 29.
		//       On exit with 'task' = NEW_X, the following information is
		//                                                             available:
		//         dsave(1) = current 'theta' in the BFGS matrix;
		//         dsave(2) = f(x) in the previous iteration;
		//         dsave(3) = factr*epsmch;
		//         dsave(4) = 2-norm of the line search direction vector;
		//         dsave(5) = the machine precision epsmch generated by the code;
		//         dsave(7) = the accumulated time spent on searching for
		//                                                         Cauchy points;
		//         dsave(8) = the accumulated time spent on
		//                                                 subspace minimization;
		//         dsave(9) = the accumulated time spent on line search;
		//         dsave(11) = the slope of the line search function at
		//                                  the current point of line search;
		//         dsave(12) = the maximum relative step length imposed in
		//                                                           line search;
		//         dsave(13) = the infinity norm of the projected gradient;
		//         dsave(14) = the relative step length in the line search;
		//         dsave(15) = the slope of the line search function at
		//                                 the starting point of the line search;
		//         dsave(16) = the square of the 2-norm of the line search
		//                                                      direction vector.
		
		//     Subprograms called:
		
		//       L-BFGS-B Library ... mainlb.    
		
		
		//     References:
		
		//       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
		//       memory algorithm for bound constrained optimization'',
		//       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
		
		//       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: a
		//       limited memory FORTRAN code for solving bound constrained
		//       optimization problems'', Tech. Report, NAM-11, EECS Department,
		//       Northwestern University, 1994.
		
		//       (Postscript files of these papers are available via anonymous
		//        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.)
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		//
		//     ************
		//-jlm-jn 
		      /*int   lws,lr,lz,lt,ld,lxp,lwa,
		            lwy,lsy,lss,lwt,lwn,lsnd;
		      
		      if (task[0].equalsIgnoreCase("START")) {
		         isave[0]  = m*n;
		         isave[1]  = m*m;
		         isave[2]  = 4*m*m;
		         isave[3]  = 1;                      // ws      m*n
		         isave[4]  = isave[3]  + isave[0];   // wy      m*n
		         isave[5]  = isave[4]  + isave[0];   // wsy     m**2
		         isave[6]  = isave[5]  + isave[1];   // wss     m**2
		         isave[7]  = isave[6]  + isave[1];   // wt      m**2
		         isave[8]  = isave[7]  + isave[1];   // wn      4*m**2
		         isave[9] = isave[8]  + isave[2];   // wsnd    4*m**2
		         isave[10] = isave[9] + isave[2];   // wz      n
		         isave[11] = isave[10] + n;         // wr      n
		         isave[12] = isave[11] + n;         // wd      n
		         isave[13] = isave[12] + n;         // wt      n
		         isave[14] = isave[13] + n;         // wxp     n
		         isave[15] = isave[15] + n;         // wa      8*m
		      } // if (task[0].equalsIgnoreCase("START"))
		      lws  = isave[3];
		      lwy  = isave[4];
		      lsy  = isave[5];
		      lss  = isave[6];
		      lwt  = isave[7];
		      lwn  = isave[8];
		      lsnd = isave[9];
		      lz   = isave[10];
		      lr   = isave[11];
		      ld   = isave[12];
		      lt   = isave[13];
		      lxp  = isave[14];
		      lwa  = isave[15];
		      */
		      double ws[][] = new double[n][m];
		      double wy[][] = new double[n][m];
		      double sy[][] = new double[m][m];
		      double ss[][] = new double[m][m];
		      double wt[][] = new double[m][m];
		      double wn[][] = new double[2*m][2*m];
		      double snd[][] = new double[2*m][2*m];
		      double z[] = new double[n];
		      double r[] = new double[n];
		      double d[] = new double[n];
		      double t[] = new double[n];
		      double xp[] = new double[n];
		      double wa1[] = new double[2*m];
		      double wa2[] = new double[2*m];
		      double wa3[] = new double[2*m];
		      double wa4[] = new double[2*m];
		      int index[] = new int[n];
		      int iwhere[] = new int[n];
		      int indx2[] = new int[n];
		      

		      mainlb(n,m,x,l,u,nbd,f,g,factr,pgtol,
		             ws,wy,sy,ss,wt,
		             wn,snd,z,r,d,t,xp,wa1,wa2,wa3,wa4,
		             index, iwhere,indx2,task,iprint, 
		             csave,lsave,isave,dsave);

		      return;

	}
	
	private void mainlb(int n, int m, double x[], double l[], double u[], 
			int nbd[], double f[], double g[], double factr, double pgtol, double ws[][], double wy[][],
		     double sy[][], double ss[][], double wt[][], double wn[][], double snd[][], 
		     double z[], double r[], double d[], double t[], double xp[],
		     double wa1[], double wa2[], double wa3[], double wa4[],
		     int index[], int iwhere[], int indx2[], String task[],
		     int iprint, String csave[], boolean lsave[], int isave[], double dsave[]) {
		      // implicit none
		      // character*60     task, csave
		      // logical          lsave(4)
		      // integer          n, m, iprint, nbd(n), index(n),
		     //                 iwhere(n), indx2(n), isave(23)
		     // double precision f, factr, pgtol,
		     //                 x(n), l(n), u(n), g(n), z(n), r(n), d(n), t(n), 
	//-jlm-jn
		     //                 xp(n), 
		     //                 wa(8*m), 
		     //                 ws(n, m), wy(n, m), sy(m, m), ss(m, m), 
		     //                 wt(m, m), wn(2*m, 2*m), snd(2*m, 2*m), dsave(29)

		//     ************
		
		//     Subroutine mainlb
		
		//     This subroutine solves bound constrained optimization problems by
		//       using the compact formula of the limited memory BFGS updates.
		       
		//     n is an integer variable.
		//       On entry n is the number of variables.
		//       On exit n is unchanged.
		
		//     m is an integer variable.
		//       On entry m is the maximum number of variable metric
		//          corrections allowed in the limited memory matrix.
		//       On exit m is unchanged.
		
		//     x is a double precision array of dimension n.
		//       On entry x is an approximation to the solution.
		//       On exit x is the current approximation.
		
		//     l is a double precision array of dimension n.
		//       On entry l is the lower bound of x.
		//       On exit l is unchanged.
		
		//     u is a double precision array of dimension n.
		//       On entry u is the upper bound of x.
		//       On exit u is unchanged.
		
		//     nbd is an integer array of dimension n.
		//       On entry nbd represents the type of bounds imposed on the
		//         variables, and must be specified as follows:
		//         nbd(i)=0 if x(i) is unbounded,
		//                1 if x(i) has only a lower bound,
		//                2 if x(i) has both lower and upper bounds,
		//                3 if x(i) has only an upper bound.
		//       On exit nbd is unchanged.
		
		//     f is a double precision variable.
		//       On first entry f is unspecified.
		//       On final exit f is the value of the function at x.
		
		//     g is a double precision array of dimension n.
		//       On first entry g is unspecified.
		//       On final exit g is the value of the gradient at x.
		
		//     factr is a double precision variable.
		//       On entry factr >= 0 is specified by the user.  The iteration
		//         will stop when
		
		//         (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
		
		//         where epsmch is the machine precision, which is automatically
		//         generated by the code.
		//       On exit factr is unchanged.
		
		//     pgtol is a double precision variable.
		//       On entry pgtol >= 0 is specified by the user.  The iteration
		//         will stop when
		
		//                 max{|proj g_i | i = 1, ..., n} <= pgtol
		
		//         where pg_i is the ith component of the projected gradient.
		//       On exit pgtol is unchanged.
		
		//     ws, wy, sy, and wt are double precision working arrays used to
		//       store the following information defining the limited memory
		//          BFGS matrix:
		//          ws, of dimension n x m, stores S, the matrix of s-vectors;
		//          wy, of dimension n x m, stores Y, the matrix of y-vectors;
		//          sy, of dimension m x m, stores S'Y;
		//          ss, of dimension m x m, stores S'S;
		//          yy, of dimension m x m, stores Y'Y;
		//          wt, of dimension m x m, stores the Cholesky factorization
		//                                  of (theta*S'S+LD^(-1)L'); see eq.
		//                                  (2.26) in [3].
		
		//     wn is a double precision working array of dimension 2m x 2m
		//       used to store the LEL^T factorization of the indefinite matrix
		//                 K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
		//                     [L_a -R_z           theta*S'AA'S ]
		
		//       where     E = [-I  0]
		//                     [ 0  I]
		
		//     snd is a double precision working array of dimension 2m x 2m
		//       used to store the lower triangular part of
		//                 N = [Y' ZZ'Y   L_a'+R_z']
		//                     [L_a +R_z  S'AA'S   ]
		            
		//     z(n),r(n),d(n),t(n), xp(n),wa(8*m) are double precision working arrays.
		//       z  is used at different times to store the Cauchy point and
		//          the Newton point.
		//       xp is used to safeguard the projected Newton direction
		
		//     sg(m),sgo(m),yg(m),ygo(m) are double precision working arrays. 
		
		//     index is an integer working array of dimension n.
		//       In subroutine freev, index is used to store the free and fixed
		//          variables at the Generalized Cauchy Point (GCP).
		
		//     iwhere is an integer working array of dimension n used to record
		//       the status of the vector x for GCP computation.
		//       iwhere(i)=0 or -3 if x(i) is free and has bounds,
		//                 1       if x(i) is fixed at l(i), and l(i) .ne. u(i)
		//                 2       if x(i) is fixed at u(i), and u(i) .ne. l(i)
		//                 3       if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i)
		//                -1       if x(i) is always free, i.e., no bounds on it.
		
		//     indx2 is an integer working array of dimension n.
		//       Within subroutine cauchy, indx2 corresponds to the array iorder.
		//       In subroutine freev, a list of variables entering and leaving
		//       the free set is stored in indx2, and it is passed on to
		//       subroutine formk with this information.
		
		//     task is a working string of characters of length 60 indicating
		//       the current job when entering and leaving this subroutine.
		
		//     iprint is an INTEGER variable that must be set by the user.
		//       It controls the frequency and type of output generated:
		//        iprint<0    no output is generated;
		//        iprint=0    print only one line at the last iteration;
		//        0<iprint<99 print also f and |proj g| every iprint iterations;
		//        iprint=99   print details of every iteration except n-vectors;
		//        iprint=100  print also the changes of active set and final x;
		//        iprint>100  print details of every iteration including x and g;
		//       When iprint > 0, the file iterate.dat will be created to
		//                        summarize the iteration.
		
		//     csave is a working string of characters of length 60.
		
		//     lsave is a logical working array of dimension 4.
		
		//     isave is an integer working array of dimension 23.
		
		//     dsave is a double precision working array of dimension 29.
		
		
		//     Subprograms called
		
		//       L-BFGS-B Library ... cauchy, subsm, lnsrlb, formk, 
		
		//        errclb, prn1lb, prn2lb, prn3lb, active, projgr,
		
		//        freev, cmprlb, matupd, formt.
		
		//       Minpack2 Library ... timer
		
		//       Linpack Library ... dcopy, ddot.
		
		
		//     References:
		
		//       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
		//       memory algorithm for bound constrained optimization'',
		//       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
		
		//       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: FORTRAN
		//       Subroutines for Large Scale Bound Constrained Optimization''
		//       Tech. Report, NAM-11, EECS Department, Northwestern University,
		//       1994.
		 
		//       [3] R. Byrd, J. Nocedal and R. Schnabel "Representations of
		//       Quasi-Newton Matrices and their use in Limited Memory Methods'',
		//       Mathematical Programming 63 (1994), no. 4, pp. 129-156.
		
		//       (Postscript files of these papers are available via anonymous
		//        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.)
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************
		 
		      boolean          updatd;
		      boolean prjctd[] = new boolean[1];
		      boolean cnstnd[] = new boolean[1];
		      boolean boxed[] = new boolean[1];
		      boolean wrk[] = new boolean[1];
		      //character*3      word
		      String word[] = new String[1];
		      int          i,nintol,itfile,nskip,
		                      head,col,iter,itail,iupdat,
		                      nact;
		      int iback[] = new int[1];
		      int ifun[] = new int[1];
		      int ileave[] = new int[1];
		      int nenter[] = new int[1];
		      int info[] = new int[1];
		      int iword[] = new int[1];
		      int k[] = new int[1];
		      int nfgv[] = new int[1];
		      int nfree[] = new int[1];
		      int nseg[] = new int[1];
		      double theta,ddot,dr,rr,tol,
		                      ddum,epsmch,
		                      cachyt,sbtime,lnscht,time2,
		                      time;
		      long cpu1, cpu2;
		      double dnorm[] = new double[1];
		      double dtd[] = new double[1];
		      double fold[] = new double[1];
		      double gd[] = new double[1];
		      double gdold[] = new double[1];
		      double sbgnrm[] = new double[1];
		      double stp[] = new double[1];
		      double stpmx[] = new double[1];
		      double xstep[] = new double[1];
		      long time1;
		      File file;
		      RandomAccessFile raFile = null;
		      final double one = 1.0;
		      final double zero = 0.0;
		      boolean do0 = true;
		      boolean do111 = true;
		      boolean do222 = true;
		      boolean do333 = true;
		      boolean do444 = true;
		      boolean do555 = true;
		      boolean do666 = true;
		      boolean do777 = true;
		      boolean do888 = true;
		      boolean do222to888 = true;
		      boolean do999 = true;
		      int isave2[] = new int[2];
		      double dsave13[] = new double[13];
		      
		      if (task[0].equalsIgnoreCase("START")) {

		         epsmch = epsilon();

		         time1 = System.currentTimeMillis();

		//        Initialize counters and scalars when task='START'.

		//           for the limited memory BFGS matrices:
		         col    = 0;
		         head   = 1;
		         theta  = one;
		         iupdat = 0;
		         updatd = false;
		         iback[0]  = 0;
		         itail  = 0;
		         iword[0]  = 0;
		         nact   = 0;
		         ileave[0] = 0;
		         nenter[0] = 0;
		         fold[0]   = zero;
		         dnorm[0]  = zero;
		         cpu1   = 0L;
		         gd[0]     = zero;
		         stpmx[0]  = zero;
		         sbgnrm[0] = zero;
		         stp[0]    = zero;
		         gdold[0]  = zero;
		         dtd[0]    = zero;

		//           for operation counts:
		         iter   = 0;
		         nfgv[0]   = 0;
		         nseg[0]   = 0;
		         nintol = 0;
		         nskip  = 0;
		         nfree[0]  = n;
		         ifun[0]   = 0;
		//           for stopping tolerance:
		         tol = factr*epsmch;

		//           for measuring running time:
		         cachyt = 0;
		         sbtime = 0;
		         lnscht = 0;
		 
		//           'word' records the status of subspace solutions.
		         word[0] = "---";

		//           'info' records the termination information.
		         info[0] = 0;

		         itfile = 8;
		         if (iprint >= 1) {
		//                                open a summary file 'iterate.dat'
		            file = new File(fileDir + "iterate.data");
		            try {
		                raFile = new RandomAccessFile(file, "rw");
		            }
		            catch (IOException e) {
		            	System.err.println("At raFile = new Random AccessFile(file, rw) IOException " + e);
		            	System.exit(-1);
		            }
		            // Necessary so that if this is an overwritten file there isn't any
		            // junk at the end
		            try {
		                raFile.setLength(0);
		            }
		            catch (IOException e) {
		            	System.err.println("At raFile.setLength(0) IOException " + e);
		            	System.exit(-1);	
		            }
		         } // if (iprint >= 1)          

		//        Check the input arguments for errors.

		         errclb(n,m,factr,l,u,nbd,task,info,k);
		         if (task[0].substring(0,5).equalsIgnoreCase("ERROR")) {
		            prn3lb(n,x,f[0],task[0],iprint,info[0],raFile,
		                       iter,nfgv[0],nintol,nskip,nact,sbgnrm[0],
		                       0.0,nseg[0],word[0],iback[0],stp[0],xstep[0],k[0],
		                       cachyt,sbtime,lnscht);
		            return;
		         }

		         prn1lb(n,m,l,u,x,iprint,raFile,epsmch);
		 
		//        Initialize iwhere & project x onto the feasible set.
		 
		          active(n,l,u,nbd,x,iwhere,iprint,prjctd,cnstnd,boxed); 

		//        The end of the initialization.
		      } // if (task[0].equalsIgnoreCase("START"))
		      else { //  not task[0] equals START
		    	  //          restore local variables.

		    	           prjctd[0] = lsave[0];
		    	           cnstnd[0] = lsave[1];
		    	           boxed[0]  = lsave[2];
		    	           updatd = lsave[3];

		    	           nintol = isave[0];
		    	           itfile = isave[2];
		    	           iback[0]  = isave[3];
		    	           nskip  = isave[4];
		    	           head   = isave[5];
		    	           col    = isave[6];
		    	           itail  = isave[7];
		    	           iter   = isave[8];
		    	           iupdat = isave[9];
		    	           nseg[0]   = isave[11];
		    	           nfgv[0]   = isave[12];
		    	           info[0]   = isave[13];
		    	           ifun[0]   = isave[14];
		    	           iword[0]  = isave[15];
		    	           nfree[0]  = isave[16];
		    	           nact   = isave[17];
		    	           ileave[0] = isave[18];
		    	           nenter[0] = isave[19];

		    	           theta  = dsave[0];
		    	           fold[0]   = dsave[1];
		    	           tol    = dsave[2];
		    	           dnorm[0]  = dsave[3];
		    	           epsmch = dsave[4];
		    	           cpu1   = (long)dsave[5];
		    	           cachyt = dsave[6];
		    	           sbtime = dsave[7];
		    	           lnscht = dsave[8];
		    	           time1  = (long)dsave[9];
		    	           gd[0]     = dsave[10];
		    	           stpmx[0]  = dsave[11];
		    	           sbgnrm[0] = dsave[12];
		    	           stp[0]    = dsave[13];
		    	           gdold[0]  = dsave[14];
		    	           dtd[0]    = dsave[15];
		    	     
		    	  //        After returning from the driver go to the point where execution
		    	  //        is to resume.

		    	           if (task[0].substring(0,5).equalsIgnoreCase("FG_LN")) {
		    	        	   //goto 666
		    	        	   do0 = false;
		    	        	   do111 = false;
		    	        	   do222 = false;
		    	        	   do333 = false;
		    	        	   do444 = false;
		    	        	   do555 = false;
		    	           }
		    	           if (task[0].substring(0,5).equalsIgnoreCase("NEW_X")) {
		    	        	   //goto 777
		    	        	   do0 = false;
		    	        	   do111 = false;
		    	        	   do222 = false;
		    	        	   do333 = false;
		    	        	   do444 = false;
		    	        	   do555 = false;
		    	        	   do666 = false;
		    	           }
		    	           if (task[0].substring(0,5).equalsIgnoreCase("FG_ST")) {
		    	               //goto 111
		    	        	   do0 = false;
		    	           }
		    	           if (task[0].substring(0,4).equalsIgnoreCase("STOP")) {
		    	        	   if (task[0].substring(6,9).equalsIgnoreCase("CPU"))  {
		    	  //                                          restore the previous iterate.
		    	        		 for (i = 0; i < n; i++) {
		    	        			 x[i] = t[i];
		    	        			 g[i] = r[i];
		    	        		 }
		    	                 f[0] = fold[0];
		    	        	   } // if (task[0].substring(6,9).equalsIgnoreCase("CPU"))  
		    	              do0 = false;
		    	              do222to888 = false;
		    	           } // if (task[0].substring(0,4).equalsIgnoreCase("STOP"))
		      } // not task[0] equals START

		      //     Compute f0 and g0.

		      if (do0) {
		      task[0] = "FG_START"; 
		//          return to the driver to calculate f and g; reenter at 111.
		      do111 = false;
		      do222to888 = false;
		      do999 = false;
		      } // if (do0)
		      if (do111) {
		      nfgv[0] = 1;
		 
		//     Compute the infinity norm of the (-) projected gradient.
		 
		      projgr(n,l,u,nbd,x,g,sbgnrm);
		  
		      if (iprint >= 1) {
		    	 System.out.println("At iteration number " + iter + " function value = " + f + " norm of projected gradient = " + sbgnrm[0]);
		    	 try {
			    	 raFile.writeChars("At iteration number " + iter + ":\n");
			    	 raFile.writeChars("Number of function evaluations = " + nfgv + "\n");
			    	 raFile.writeChars("Norm of the projected gradient = " + sbgnrm[0] + "\n");
			    	 raFile.writeChars("Function value = " + f + "\n");
		    	 }
		    	 catch (IOException e) {
		    		 System.err.println("In mainlb raFile.writeChars IOExcpeption " + e);
		    		 System.exit(-1);
		    	 }
		      } // if (iprint >= 1)
		      if (sbgnrm[0] <= pgtol) {
		//                                terminate the algorithm.
		         task[0] = "CONVERGENCE: NORM_OF_PROJECTED_GRADIENT_<=_PGTOL";
		         do222to888 = false;
		      } // if (sbgnrm[0] <= pgtol)
		      } // if (do111)
		 
		// ----------------- the beginning of the loop --------------------------
		 
		 //222  continue
	    /*
		if (do222to888) {
		loop222: do {
			if (do222) {
		      if (iprint >= 99) {
		    	  System.out.println("Iteration = " + iter);
		      }
		      iword = -1;
		
		      if ((!cnstnd[0]) && (col > 0)) { 
		//                                            skip the search for GCP.
		         for (i = 0; i < n; i++) {
		        	 z[i] = x[i];
		         }
		         wrk[0] = updatd;
		         nseg[0] = 0;
		         do222 = false;
		         do333 = true;
		         do444 = true;
		         do555 = true;
		         do666 = true;
		         do777 = true;
		         do888 = true;
		         do999 = true;
		         continue loop222;
		      } // if ((!cnstnd[0]) && (col > 0))

		//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		
		//     Compute the Generalized Cauchy Point (GCP).
		
		//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

		      cpu1 = System.currentTimeMillis(); 
		      cauchy(n,x,l,u,nbd,g,indx2,iwhere,t,d,z,
		                 m,wy,ws,sy,wt,theta,col,head,
		                 wa1,wa2,wa3,wa4,nseg,
		                 iprint, sbgnrm[0], info, epsmch);
		      if (info[0] != 0) {
		//         singular triangular system detected; refresh the lbfgs memory.
		         if(iprint >= 1) {
		        	 System.out.println("Singular triangular system detected");
				     System.out.println("Refresh the lbfgs memory and restart the iteration.");
		         }
		         info[0]   = 0;
		         col    = 0;
		         head   = 1;
		         theta  = one;
		         iupdat = 0;
		         updatd = false;
		         cpu2 = System.currentTimeMillis(); 
		         cachyt = cachyt + (cpu2 - cpu1)/1000.0;
		         do222 = true;
		         do333 = true;
		         do444 = true;
		         do555 = true;
		         do666 = true;
		         do777 = true;
		         do888 = true;
		         do999 = true;
		         continue loop222;
		      } // if (info[0] != 0)
		      cpu2 = System.currentTimeMillis(); 
		       cachyt = cachyt + (cpu2 - cpu1)/1000.0;
		      nintol = nintol + nseg[0];

		//     Count the entering and leaving variables for iter > 0; 
		//     find the index set of free and active variables at the GCP.

		      freev(n,nfree,index,nenter,ileave,indx2,
		                iwhere,wrk,updatd,cnstnd[0],iprint,iter);
		      nact = n - nfree[0];
			} // if (do222)

		 if (do333) {
		 
		//     If there are no free variables or B=theta*I, then
		//                                        skip the subspace minimization.
		 
		      if ((nfree[0] == 0) || (col == 0)) {
		    	  do444 = false;
		      }
		      else {
		 
		//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		
		//     Subspace minimization.
		
		//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

		      cpu1 = System.currentTimeMillis(); 

		//     Form  the LEL^T factorization of the indefinite
		//       matrix    K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
		//                     [L_a -R_z           theta*S'AA'S ]
		//       where     E = [-I  0]
		//                     [ 0  I]

		      if (wrk[0]) formk(n,nfree[0],index,nenter[0],ileave[0],indx2,iupdat,
		                        updatd,wn,snd,m,ws,wy,sy,theta,col,head,info);
		      if (info[0] != 0) {
		//          nonpositive definiteness in Cholesky factorization;
		//          refresh the lbfgs memory and restart the iteration.
		         if (iprint >= 1) {
		        	 System.out.println("Nonpositive definiteness in Cholesky factorization in formk.");
				     System.out.println("Refresh the lbfgs memory and restart the iteration.");
		         }
		         info[0]   = 0;
		         col    = 0;
		         head   = 1;
		         theta  = one;
		         iupdat = 0;
		         updatd = false;
		         cpu2 = System.currentTimeMillis(); 
		         sbtime = sbtime + (cpu2 - cpu1)/1000.0; 
		         do222 = true;
		         do333 = true;
		         do444 = true;
		         do555 = true;
		         do666 = true;
		         do777 = true;
		         do888 = true;
		         do999 = true;
		         continue loop222;
		      } // if (info[0] != 0) 

		//        compute r=-Z'B(xcp-xk)-Z'g (using wa(2m+1)=W'(xcp-x)
		//                                                   from 'cauchy').
		      cmprlb(n,m,x,g,ws,wy,sy,wt,z,r,wa1,wa2,index,
		             theta,col,head,nfree,cnstnd,info);
		      if (info[0] ==  0) {

			// c-jlm-jn   call the direct method. 
	
			      subsm( n, m, nfree, index, l, u, nbd, z, r, xp, ws, wy,
			             theta, x, g, col, head, iword, wa, wn, iprint, info);
		      } // if (info[0] == 0)
		      } // else
		 } // if (do333)
		 if (do444) {
		      if (info[0] != 0) {
		//          singular triangular system detected;
		//          refresh the lbfgs memory and restart the iteration.
		         if (iprint >= 1) {
		        	 System.out.println("Singular triangular system detected.");
				     System.out.println("Refresh the lbfgs memory and restart the iteration.");
		         }
		         info[0]   = 0;
		         col    = 0;
		         head   = 1;
		         theta  = one;
		         iupdat = 0;
		         updatd = false;
		         cpu2 = System.currentTimeMillis();
		         sbtime = sbtime + (cpu2 - cpu1)/1000.0; 
		         do222 = true;
		         do333 = true;
		         do444 = true;
		         do555 = true;
		         do666 = true;
		         do777 = true;
		         do888 = true;
		         do999 = true;
		         continue loop222;
		      } // if (info[0] != 0)
		 
		      cpu2 = System.currentTimeMillis();
		      sbtime = sbtime + (cpu2 - cpu1)/1000.0; 
		 } // if (do444)
		 if (do555) {
		 
		//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		
		//     Line search and optimality tests.
		
		//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		 
		//     Generate the search direction d:=z-x.

		      for (i = 0; i < n; i++) {
		         d[i] = z[i] - x[i];
		      }
		      cpu1 = System.currentTimeMillis();
		 } // if (do555)
		 if (do666) {
			  isave2[0] = isave[21];
			  isave2[1] = isave[22];
			  for (i = 0; i < 16; i++) {
				  dsave13[i] = dsave[i + 16];
			  }
		      lnsrlb(n,l,u,nbd,x,f,fold,gd,gdold,g,d,r,t,z,stp,dnorm,
		             dtd,xstep,stpmx,iter,ifun,iback,nfgv,info,task,
		             boxed[0],cnstnd[0],csave,isave2,dsave13);
		      isave[21] = isave2[0];
		      isave[22] = isave2[1];
		      for (i = 0; i < 16; i++) {
				  dsave[i + 16] = dsave13[i];
			  }
		      if (info .ne. 0 .or. iback .ge. 20) then
		c          restore the previous iterate.
		         call dcopy(n,t,1,x,1)
		         call dcopy(n,r,1,g,1)
		         f = fold
		         if (col .eq. 0) then
		c             abnormal termination.
		            if (info .eq. 0) then
		               info = -9
		c                restore the actual number of f and g evaluations etc.
		               nfgv = nfgv - 1
		               ifun = ifun - 1
		               iback = iback - 1
		            endif
		            task = 'ABNORMAL_TERMINATION_IN_LNSRCH'
		            iter = iter + 1
		            do999 = true;
		            break loop222;
		         else
		c             refresh the lbfgs memory and restart the iteration.
		            if(iprint .ge. 1) write (6, 1008)
		            if (info .eq. 0) nfgv = nfgv - 1
		            info   = 0
		            col    = 0
		            head   = 1
		            theta  = one
		            iupdat = 0
		            updatd = .false.
		            task   = 'RESTART_FROM_LNSRCH'
		            call timer(cpu2)
		            lnscht = lnscht + cpu2 - cpu1
		             do222 = true;
			         do333 = true;
			         do444 = true;
			         do555 = true;
			         do666 = true;
			         do777 = true;
			         do888 = true;
			         do999 = true;
			         continue loop222;
		         endif
		      else if (task(1:5) .eq. 'FG_LN') then
		c          return to the driver for calculating f and g; reenter at 666.
				 do999 = false;
				 break loop222;
		      else 
		c          calculate and print out the quantities related to the new X.
		         call timer(cpu2) 
		         lnscht = lnscht + cpu2 - cpu1
		         iter = iter + 1
		 
		c        Compute the infinity norm of the projected (-)gradient.
		 
		         call projgr(n,l,u,nbd,x,g,sbgnrm)
		 
		c        Print iteration information.

		         call prn2lb(n,x,f,g,iprint,itfile,iter,nfgv,nact,
		     +               sbgnrm,nseg,word,iword,iback,stp,xstep)
		         do999 = false;
		         break loop222;
		      endif
		 } // if (do666)
		 if (do777) {

		c     Test for termination.

		      if (sbgnrm .le. pgtol) then
		c                                terminate the algorithm.
		         task = 'CONVERGENCE: NORM_OF_PROJECTED_GRADIENT_<=_PGTOL'
		         break loop222;
		      endif 

		      ddum = max(abs(fold), abs(f), one)
		      if ((fold - f) .le. tol*ddum) then
		c                                        terminate the algorithm.
		         task = 'CONVERGENCE: REL_REDUCTION_OF_F_<=_FACTR*EPSMCH'
		         if (iback .ge. 10) info = -5
		c           i.e., to issue a warning if iback>10 in the line search.
		         break loop222;
		      endif 

		c     Compute d=newx-oldx, r=newg-oldg, rr=y'y and dr=y's.
		 
		      do 42 i = 1, n
		         r(i) = g(i) - r(i)
		  42  continue
		      rr = ddot(n,r,1,r,1)
		      if (stp .eq. one) then  
		         dr = gd - gdold
		         ddum = -gdold
		      else
		         dr = (gd - gdold)*stp
		         call dscal(n,stp,d,1)
		         ddum = -gdold*stp
		      endif
		 
		      if (dr <= epsmch*ddum) {
		c                            skip the L-BFGS update.
		         nskip = nskip + 1
		         updatd = .false.
		         if (iprint .ge. 1) write (6,1004) dr, ddum
		         do222 = true;
		         do333 = true;
		         do444 = true;
		         do555 = true;
		         do666 = true;
		         do777 = true;
		         do888 = true;
		         do999 = true;
		         continue loop222;
		      }
		 
		cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		c
		c     Update the L-BFGS matrix.
		c
		cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		 
		      updatd = .true.
		      iupdat = iupdat + 1

		c     Update matrices WS and WY and form the middle matrix in B.

		      call matupd(n,m,ws,wy,sy,ss,d,r,itail,
		     +            iupdat,col,head,theta,rr,dr,stp,dtd)

		c     Form the upper half of the pds T = theta*SS + L*D^(-1)*L';
		c        Store T in the upper triangular of the array wt;
		c        Cholesky factorize T to J*J' with
		c           J' stored in the upper triangular of wt.

		      call formt(m,wt,sy,ss,col,theta,info)
		 
		      if (info .ne. 0) then 
		c          nonpositive definiteness in Cholesky factorization;
		c          refresh the lbfgs memory and restart the iteration.
		         if(iprint .ge. 1) write (6, 1007)
		         info = 0
		         col = 0
		         head = 1
		         theta = one
		         iupdat = 0
		         updatd = .false.
		         do222 = true;
		         do333 = true;
		         do444 = true;
		         do555 = true;
		         do666 = true;
		         do777 = true;
		         do888 = true;
		         do999 = true;
		         continue loop222;
		      endif
		 } // if (do777)

		c     Now the inverse of the middle matrix in B is

		c       [  D^(1/2)      O ] [ -D^(1/2)  D^(-1/2)*L' ]
		c       [ -L*D^(-1/2)   J ] [  0        J'          ]

		 if (do888) {
		 
		// -------------------- the end of the loop -----------------------------
		 
		      // goto 222
			 do222 = true;
			 do333 = true;
			 do444 = true;
			 do555 = true;
			 do666 = true;
			 do777 = true;
			 do888 = true;
			 do999 = true;
		 } // if (do888)
		} while (true); // for loop222
		} // if (do222to888)
		if (do999) {
		      call timer(time2)
		      time = time2 - time1
		      call prn3lb(n,x,f,task,iprint,info,itfile,
		     +            iter,nfgv,nintol,nskip,nact,sbgnrm,
		     +            time,nseg,word,iback,stp,xstep,k,
		     +            cachyt,sbtime,lnscht)
		} // if (do999)
		 // 1000 continue

		c     Save local variables.

		      lsave(1)  = prjctd
		      lsave(2)  = cnstnd
		      lsave(3)  = boxed
		      lsave(4)  = updatd

		      isave(1)  = nintol 
		      isave(3)  = itfile 
		      isave(4)  = iback 
		      isave(5)  = nskip 
		      isave(6)  = head 
		      isave(7)  = col 
		      isave(8)  = itail 
		      isave(9)  = iter 
		      isave(10) = iupdat 
		      isave(12) = nseg
		      isave(13) = nfgv 
		      isave(14) = info 
		      isave(15) = ifun 
		      isave(16) = iword 
		      isave(17) = nfree 
		      isave(18) = nact 
		      isave(19) = ileave 
		      isave(20) = nenter 

		      dsave(1)  = theta 
		      dsave(2)  = fold 
		      dsave(3)  = tol 
		      dsave(4)  = dnorm 
		      dsave(5)  = epsmch 
		      dsave(6)  = cpu1 
		      dsave(7)  = cachyt 
		      dsave(8)  = sbtime 
		      dsave(9)  = lnscht 
		      dsave(10) = time1 
		      dsave(11) = gd 
		      dsave(12) = stpmx 
		      dsave(13) = sbgnrm
		      dsave(14) = stp
		      dsave(15) = gdold
		      dsave(16) = dtd  

		 1001 format (//,'ITERATION ',i5)
		 1002 format
		     +  (/,'At iterate',i5,4x,'f= ',1p,d12.5,4x,'|proj g|= ',1p,d12.5)
		 1003 format (2(1x,i4),5x,'-',5x,'-',3x,'-',5x,'-',5x,'-',8x,'-',3x,
		     +        1p,2(1x,d10.3))
		 1004 format ('  ys=',1p,e10.3,'  -gs=',1p,e10.3,' BFGS update SKIPPED')
		 1005 format (/, 
		     +' Singular triangular system detected;',/,
		     +'   refresh the lbfgs memory and restart the iteration.')
		 1006 format (/, 
		     +' Nonpositive definiteness in Cholesky factorization in formk;',/,
		     +'   refresh the lbfgs memory and restart the iteration.')
		 1007 format (/, 
		     +' Nonpositive definiteness in Cholesky factorization in formt;',/,
		     +'   refresh the lbfgs memory and restart the iteration.')
		 1008 format (/, 
		     +' Bad direction in the line search;',/,
		     +'   refresh the lbfgs memory and restart the iteration.')
		     */
		      return;   

		   
	} // private void mainlb
	
	private double epsilon() {
		// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        double epsilon = 1.0;
        double neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                return epsilon;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
	}

	private void errclb(int n, int m, double factr, double l[], 
			double u[], int nbd[], String task[], int info[], int k[]) {
	 
	    //character*60     task
		//String[1] task
	    //integer          n, m, info, k, nbd(n)
	    //double precision factr, l(n), u(n)
	
	//     ************
	
	//     Subroutine errclb
	
	//     This subroutine checks the validity of the input data.
	
    
	//                           *  *  *
	
	//     NEOS, November 1994. (Latest revision June 1996.)
	//     Optimization Technology Center.
	//     Argonne National Laboratory and Northwestern University.
	//     Written by
	//                        Ciyou Zhu
	//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
	
	//
	//     ************
	
	    int          i;
	    final double one = 1.0;
	    final double zero = 0.0;
	
	//     Check the input arguments for errors.
	
	    if (n <= 0) task[0] = "ERROR: N .LE. 0";
	    if (m <= 0) task[0] = "ERROR: M .LE. 0";
	    if (factr < zero) task[0] = "ERROR: FACTR .LT. 0";
	
	//     Check the validity of the arrays nbd(i), u(i), and l(i).
	
	    for (i = 0; i < n; i++) {
	       if ((nbd[i] < 0) || (nbd[i] > 3)) {
	//                                                   return
	          task[0] = "ERROR: INVALID NBD";
	          info[0] = -6;
	          k[0] = i;
	       }
	       if (nbd[i] == 2) {
	          if (l[i] > u[i]) {
	//                                    return
	             task[0] = "ERROR: NO FEASIBLE SOLUTION";
	             info[0] = -7;
	             k[0] = i;
	          }
	       }
	    } // for (i = 0; i < n; i++)
	
	    return;
	}
	
	private void prn3lb(int n, double x[], double f, String task, int iprint, int info, RandomAccessFile raFile, 
		                       int iter, int nfgv, int nintol, int nskip,int nact, double sbgnrm, 
		                       double time, int nseg, String word, int iback, double stp, double xstep, int k, 
		                       double cachyt, double sbtime, double lnscht) {
		 
		      // character*60     task
		      // character*3      word
		      // integer          n, iprint, info, itfile, iter, nfgv, nintol,
		     //                 nskip, nact, nseg, iback, k
		     // double precision f, sbgnrm, time, stp, xstep, cachyt, sbtime,
		     //                 lnscht, x(n)

		//     ************
		
		//     Subroutine prn3lb
		
		//     This subroutine prints out information when either a built-in
		//       convergence test is satisfied or when an error message is
		//       generated.
		       
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************

		      int i;
		      int mul6;

		      if (!task.substring(0,5).equalsIgnoreCase("ERROR")) {

			      if (iprint >= 0) {
			         System.out.println("Total number of iterations = " + iter);
			         System.out.println("Total number of function evaluations = " + nfgv);
			         System.out.println("Total number of segments explored during Cauchy searches = " + nseg);
			         // nintol
			         System.out.println("Number of BFGS updates skipped = " + nskip);
			         System.out.println("Number of active bounds at final generalized Cauchy point = " + nact);
			         System.out.println("Norm of the final projected gradient = " + sbgnrm);
			         System.out.println("Final function value = " + f);
			         
			         if (iprint >= 100) {
			        	System.out.print("X = ");
			        	mul6 = 6;
			        	for (i = 0; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + x[i]);
			        	}
			        	System.out.print("\n");
			        	mul6 += 6;
			            while (i < n) {
			            	System.out.print("    ");
			            	for (; i < Math.min(n, mul6); i++) {
				        		System.out.print("    " + x[i]);
				        	}
			            	System.out.print("\n");
				        	mul6 += 6;
			            }
			         } // if (iprint >= 100)  
			      } // if (iprint >= 0) 
		      } // if (!task.substring(0,5).equalsIgnoreCase("ERROR"))
		      if (iprint >= 0) {
		         System.out.println("task = " + task);
	             if (info == -1) {
	            	 System.out.println("Matrix in 1st Cholesky factorization in formk is not Pos. Def.");
	             }
	             else if (info == -2) {
	            	 System.out.println("Matrix in 2st Cholesky factorization in formk is not Pos. Def.");
	             }
	             else if (info == -3) {
	            	 System.out.println("Matrix in the Cholesky factorization in formt is not Pos. Def.");
	             }
	             else if (info == -4) {
	            	 System.out.println("Derivative >= 0, backtracking line search impossible.");
	            	 System.out.println("Previous x, f and g restored.");
	            	 System.out.println("Possible causes: 1 error in function or gradient evaluation;");
	            	 System.out.println("2 rounding errors dominate computation.");
	             }
	             else if (info == -5) {
	                 System.out.println("Warning:  more than 10 function and gradient");
	                 System.out.println("evaluations in the last line search.  Termination");
	                 System.out.println("may possibly be caused by a bad search direction.");
	             }
	             else if (info == -6) {
	            	System.out.println("Input nbd["+k+"] is invalid.");
	             }
	             else if (info == -7)  {
	                 System.out.println("l["+k+"] > u["+k+"].  No feasible solution.");
	             }
	             else if (info == -8) {
	                 System.out.println("The triangular system is singular.");	 
	             }
	             else if (info == -9) {
	            	 System.out.println("Line search cannot locate an adequate point after 20 function");
	            	 System.out.println("and gradient evaluations.  Previous x, f and g restored.");
	            	 System.out.println("Possible causes: 1 error in function or gradient evaluation;");
	            	 System.out.println("2 rounding error dominate computation.");
	             }
		      } // if (iprint >= 0)
		      if (iprint >= 1) {
		    	  System.out.println("Cauchy time = " + cachyt + " seconds");
		    	  System.out.println("Subspace minimization time = "+ sbtime + " seconds");
		    	  System.out.println("Line search time = " + lnscht + " seconds");
		      }
		      System.out.println("Total user time = " + time + " seconds");
		      if (iprint >= 1) {
		    	  try {
			          if ((info == -4) || (info == -9)) {
			        	  raFile.writeChars("Total number of iterations = " + iter + "\n");
			        	  raFile.writeChars("Total number of function evaluations = " + nfgv + "\n");
			        	  raFile.writeChars("Total number of segments explored during Cauchy searches = " + nseg + "\n");
			        	  raFile.writeChars("Number of active bounds at final generalized Cauchy point = " + nact + "\n");
			        	  raFile.writeChars("word = " + word + "\n");
			        	  raFile.writeChars("iback = " + iback + "\n");
			        	  raFile.writeChars("stp = " + stp + "\n");
			        	  raFile.writeChars("xstep = " + xstep + "\n");
			          } // if ((info == -4) || (info == -9))
			          raFile.writeChars("task = " + task + "\n");
			          if (info == -1) {
			        	  raFile.writeChars("Matrix in 1st Cholesky factorization in formk is not Pos. Def.\n");
			          }
			          else if (info == -2) {
			              raFile.writeChars("Matrix in 2st Cholesky factorization in formk is not Pos. Def.\n");
			          }
			          else if (info == -3) {
			              raFile.writeChars("Matrix in the Cholesky factorization in formt is not Pos. Def.\n");
			          }
			          else if (info == -4) {
			              raFile.writeChars("Derivative >= 0, backtracking line search impossible.\n");
			              raFile.writeChars("Previous x, f and g restored.\n");
			              raFile.writeChars("Possible causes: 1 error in function or gradient evaluation;\n");
			              raFile.writeChars("2 rounding errors dominate computation.\n");
			          }
			          else if (info == -5) {
			        	  raFile.writeChars("Warning:  more than 10 function and gradient\n");
			        	  raFile.writeChars("evaluations in the last line search.  Termination\n");
			        	  raFile.writeChars("may possibly be caused by a bad search direction.\n");
			          }
			          else if (info == -6) {
			        	  raFile.writeChars("Input nbd["+k+"] is invalid.\n");
			          }
			          else if (info == -7)  {
			        	  raFile.writeChars("l["+k+"] > u["+k+"].  No feasible solution.\n");
			          }
			          else if (info == -8) {
			        	  raFile.writeChars("The triangular system is singular.\n");	 
			          }
			          else if (info == -9) {
			        	  raFile.writeChars("Line search cannot locate an adequate point after 20 function\n");
			        	  raFile.writeChars("and gradient evaluations.  Previous x, f and g restored.\n");
			        	  raFile.writeChars("Possible causes: 1 error in function or gradient evaluation;\n");
			        	  raFile.writeChars("2 rounding error dominate computation.\n");
			          }
			          raFile.writeChars("Total user time = " + time + " seconds\n");
		    	  }
		    	  catch (IOException e) {
		               System.err.println("In raFile.writeChars IOException " + e);
		               try {
		                   raFile.close();
		               }
		               catch (IOException ex) {
		            	   System.err.println("At raFile.close() IOException " + ex);
		               }
		               System.exit(-1);
		    	  }
		      } // if (iprint >= 1)
		      return;
	}

	public void prn1lb(int n, int m, double l[], double u[], 
			double x[], int iprint, RandomAccessFile raFile, double epsmch) {
	 
	    // integer n, m, iprint
	    // double precision epsmch, x(n), l(n), u(n)
	
	//     ************
	
	//     Subroutine prn1lb
	
	//     This subroutine prints the input data, initial point, upper and
	//       lower bounds of each variable, machine precision, as well as 
	//       the headings of the output.
	
	
	//                           *  *  *
	
	//     NEOS, November 1994. (Latest revision June 1996.)
	//     Optimization Technology Center.
	//     Argonne National Laboratory and Northwestern University.
	//     Written by
	//                        Ciyou Zhu
	//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
	
	//
	//     ************
	
	    int i, mul6;
	
	    if (iprint >= 0) {
	       System.out.println("Runnng the L-BFGS-B code with machine precision = " + epsmch);
	       System.out.println("n = " + n + "  m = " + m);
	       if (iprint >= 1) {
	    	  try {
	              raFile.writeChars("Running the L-BFGS-B code with machine precision = " + epsmch + "\n");
	              raFile.writeChars("n = " + n + "  m = " + m + "\n");
	    	  }
	    	  catch (IOException e) {
	              System.err.println("In prn1lb raFile.writeChars IOException " + e);
	              System.exit(-1);
	    	  }
	          if (iprint > 100) {
	        	  System.out.print("l = ");
		        	mul6 = 6;
		        	for (i = 0; i < Math.min(n, mul6); i++) {
		        		System.out.print("    " + l[i]);
		        	}
		        	System.out.print("\n");
		        	mul6 += 6;
		            while (i < n) {
		            	System.out.print("    ");
		            	for (; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + l[i]);
			        	}
		            	System.out.print("\n");
			        	mul6 += 6;
		            } 
		            
	        	  System.out.print("x = ");
		        	mul6 = 6;
		        	for (i = 0; i < Math.min(n, mul6); i++) {
		        		System.out.print("    " + x[i]);
		        	}
		        	System.out.print("\n");
		        	mul6 += 6;
		            while (i < n) {
		            	System.out.print("    ");
		            	for (; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + x[i]);
			        	}
		            	System.out.print("\n");
			        	mul6 += 6;
		            } 
		            
		            System.out.print("u = ");
		        	mul6 = 6;
		        	for (i = 0; i < Math.min(n, mul6); i++) {
		        		System.out.print("    " + u[i]);
		        	}
		        	System.out.print("\n");
		        	mul6 += 6;
		            while (i < n) {
		            	System.out.print("    ");
		            	for (; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + u[i]);
			        	}
		            	System.out.print("\n");
			        	mul6 += 6;
		            } 
	          } // if (iprint > 100) 
	       } // if (iprint >= 1)
	    } // if (iprint >= 0)
	
	    return;
	}

	private void prn2lb(int n, double x[], double f, double g[], int iprint, RandomAccessFile raFile, 
			int iter, int nfgv, int nact, 
		     double sbgnrm, int nseg, String word[], int iword, 
		     int iback, double stp, double xstep) {
		 
		      // character*3      word
		      // integer          n, iprint, itfile, iter, nfgv, nact, nseg,
		      //                 iword, iback
		     // double precision f, sbgnrm, stp, xstep, x(n), g(n)

		//     ************
		
		//     Subroutine prn2lb
		
		//     This subroutine prints out new information after a successful
		//       line search. 
		
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************

		      int i,imod,mul6;

		//           'word' records the status of subspace solutions.
		      if (iword == 0) {
		//                            the subspace minimization converged.
		         word[0] = "con";
		      }
		      else if (iword == 1) {
		//                          the subspace minimization stopped at a bound.
		         word[0] = "bnd";
		      }
		      else if (iword == 5) {
		//                             the truncated Newton step has been used.
		         word[0] = "TNT";
		      }
		      else {
		         word[0] = "---";
		      }
		      if (iprint >= 99) {
		         System.out.println("LINE SEARCH " + iback + " times; norm of step = " + xstep);
		         System.out.println("At iterate " + iter + " function value = " + f);
		         System.out.println("Norm of the projected gradient sbgnrm = " + sbgnrm);
		         if (iprint > 100) { 
		        	 System.out.print("x = ");
			        	mul6 = 6;
			        	for (i = 0; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + x[i]);
			        	}
			        	System.out.print("\n");
			        	mul6 += 6;
			            while (i < n) {
			            	System.out.print("    ");
			            	for (; i < Math.min(n, mul6); i++) {
				        		System.out.print("    " + x[i]);
				        	}
			            	System.out.print("\n");
				        	mul6 += 6;
			            } 
			            
			            System.out.print("g = ");
			        	mul6 = 6;
			        	for (i = 0; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + g[i]);
			        	}
			        	System.out.print("\n");
			        	mul6 += 6;
			            while (i < n) {
			            	System.out.print("    ");
			            	for (; i < Math.min(n, mul6); i++) {
				        		System.out.print("    " + g[i]);
				        	}
			            	System.out.print("\n");
				        	mul6 += 6;
			            } 
		         } // if (iprint > 100)
		      } // if (iprint >= 99)
		      else if (iprint > 0) { 
		         imod = iter % iprint;
		         if (imod == 0) {
		        	 System.out.println("At iterate " + iter + " function value = " + f);
		        	 System.out.println("Norm of the projected gradient = " + sbgnrm);	 
		         }
		      } // else if (iprint > 0)
		      if (iprint >= 1) {
		    	  try {
			    	  raFile.writeChars("At iteration number " + iter + " :\n");
			    	  raFile.writeChars("Number of function evaluations = " + nfgv + "\n");
			    	  raFile.writeChars("Number of segments explored during Cauchy search = " + nseg + "\n");
			    	  raFile.writeChars("Number of active bounds at the generalized Cauchy point = " + nact + "\n");
			    	  raFile.writeChars("Status of subspace solutions:\n");
			    	  if (iword == 0) {
			    		  raFile.writeChars("The subspace minimization converged.\n");
			    	  }
			    	  else if (iword == 1) {
			    		  raFile.writeChars("The subspace minimization stopped at a bound.\n");
			    	  }
			    	  else if (iword == 5) {
			    		  raFile.writeChars("The truncated Newton step has been used.\n");
			    	  }
			    	  else {
			    		  raFile.writeChars("---\n");
			    	  }
			    	  raFile.writeChars("Number of iterations performed in the line search = " + iback + "\n");
			    	  raFile.writeChars("Step length used = " + stp + "\n");
			    	  raFile.writeChars("Norm of the displacement (total step) = " + xstep + "\n");
			    	  raFile.writeChars("Norm of the projected gradient = " + sbgnrm + "\n");
			    	  raFile.writeChars("Function value = " + f + "\n");
		    	  }
		    	  catch (IOException e) {
		    		  System.err.println("In prn2lb raFile.writeChars IOException " + e);
		    		  System.exit(-1);
		    	  }
		      } // if (iprint >= 1)

		      return;
	}
	
	private void active(int n, double l[], double u[], int nbd[], double x[],
			int iwhere[], int iprint, boolean prjctd[], boolean cnstnd[], boolean boxed[]) {

		      // logical          prjctd, cnstnd, boxed
		      // integer          n, iprint, nbd(n), iwhere(n)
		      // double precision x(n), l(n), u(n)

		//     ************
		
		//     Subroutine active
		
		//     This subroutine initializes iwhere and projects the initial x to
		//       the feasible set if necessary.
		
		//     iwhere is an integer array of dimension n.
		//       On entry iwhere is unspecified.
		//       On exit iwhere(i)=-1  if x(i) has no bounds
		//                         3   if l(i)=u(i)
		//                         0   otherwise.
		//       In cauchy, iwhere is given finer gradations.
		
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************

		      int          nbdd,i;
		      final double zero = 0.0;

		//     Initialize nbdd, prjctd, cnstnd and boxed.

		      nbdd = 0;
		      prjctd[0] = false;
		      cnstnd[0] = false;
		      boxed[0] = true;

		//     Project the initial x to the easible set if necessary.

		      for (i = 0; i <  n; i++) {
		         if (nbd[i] > 0) {
		            if ((nbd[i] <= 2) && (x[i] <= l[i])) {
		               if (x[i] < l[i]) {
		                  prjctd[0] = true;
		                  x[i] = l[i];
		               }
		               nbdd = nbdd + 1;
		            }
		            else if ((nbd[i] >= 2) && (x[i] >= u[i])) {
		               if (x[i] > u[i]) {
		                  prjctd[0] = true;
		                  x[i] = u[i];
		               }
		               nbdd = nbdd + 1;
		            }
		         } // if (nbd[i] > 0)
		      } // for (i = 0; i <  n; i++)

		//     Initialize iwhere and assign values to cnstnd and boxed.

		      for (i = 0; i < n; i++) {
		         if (nbd[i] != 2) boxed[0] = false;
		         if (nbd[i] == 0) {
		//                                this variable is always free
		            iwhere[i] = -1;

		//           otherwise set x(i)=mid(x(i), u(i), l(i)).
		         }
		         else {
		            cnstnd[0] = true;
		            if ((nbd[i] == 2) && ((u[i] - l[i]) <= zero)) {
		//                   this variable is always fixed
		               iwhere[i] = 3;
		            }
		            else {
		               iwhere[i] = 0;
		            }
		         }
		      } // for (i = 0; i < n; i++)

		      if (iprint >= 0) {
		         if (prjctd[0]) {
		             System.out.println("Initial X is infeasible.  Restart with its projection.");
		         }
		         if (! cnstnd[0]) {
		             System.out.println("This problem is unconstrained.");
		         }
		      } // if (iprint >= 0)

		      if (iprint > 0) {
		    	  System.out.println("At X0 " + nbdd + " variables are exactly at the bounds");
		      }

		      return;
	}

	private void projgr(int n, double l[], double u[], int nbd[], 
			double x[], double g[], double sbgnrm[]) {

	    // integer          n, nbd(n)
	    // double precision sbgnrm, x(n), l(n), u(n), g(n)
	
	//     ************
	
	//     Subroutine projgr
	
	//     This subroutine computes the infinity norm of the projected
	//       gradient.
	
	
	//                           *  *  *
	
	//     NEOS, November 1994. (Latest revision June 1996.)
	//     Optimization Technology Center.
	//     Argonne National Laboratory and Northwestern University.
	//     Written by
	//                        Ciyou Zhu
	//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
	
	
	//     ************
	
	    int i;
	    double gi;
	    final double zero = 0.0;
	
	    sbgnrm[0] = zero;
	    for (i = 0; i < n; i++) {
	      gi = g[i];
	      if (nbd[i] != 0) {
	         if (gi < zero) {
	            if (nbd[i] >= 2) gi = Math.max((x[i]-u[i]),gi);
	         }
	         else {
	            if (nbd[i] <= 2) gi = Math.min((x[i]-l[i]),gi);
	         }
	      } // if (nbd[i] != 0)
	      sbgnrm[0] = Math.max(sbgnrm[0],Math.abs(gi));
	    } // for (i = 0; i < n; i++)
	
	    return;
	}

	private void cauchy(int n, double x[], double l[], double u[], int nbd[],
			double g[], int iorder[], int iwhere[], double t[], double d[], double xcp[], 
		     int m, double wy[][], double ws[][], double sy[][], double wt[][], 
		     double theta, int col, int head, double p[], double c[], double wbp[], 
		     double v[], int nseg[], int iprint, double sbgnrm, int info[], double epsmch) {
		      // implicit none
		      // integer          n, m, head, col, nseg, iprint, info, 
		     //                 nbd(n), iorder(n), iwhere(n)
		     // double precision theta, epsmch,
		     //                 x(n), l(n), u(n), g(n), t(n), d(n), xcp(n),
		     //                 wy(n, col), ws(n, col), sy(m, m),
		     //                 wt(m, m), p(2*m), c(2*m), wbp(2*m), v(2*m)

		//     ************
		
		//     Subroutine cauchy
		
		//     For given x, l, u, g (with sbgnrm > 0), and a limited memory
		//       BFGS matrix B defined in terms of matrices WY, WS, WT, and
		//       scalars head, col, and theta, this subroutine computes the
		//       generalized Cauchy point (GCP), defined as the first local
		//       minimizer of the quadratic
		
		//                  Q(x + s) = g's + 1/2 s'Bs
		
		//       along the projected gradient direction P(x-tg,l,u).
		//       The routine returns the GCP in xcp. 
		       
		//     n is an integer variable.
		//       On entry n is the dimension of the problem.
		//       On exit n is unchanged.
		
		//     x is a double precision array of dimension n.
		//       On entry x is the starting point for the GCP computation.
		//       On exit x is unchanged.
		
		//     l is a double precision array of dimension n.
		//       On entry l is the lower bound of x.
		//       On exit l is unchanged.
		
		//     u is a double precision array of dimension n.
		//       On entry u is the upper bound of x.
		//       On exit u is unchanged.
		
		//     nbd is an integer array of dimension n.
		//       On entry nbd represents the type of bounds imposed on the
		//         variables, and must be specified as follows:
		//         nbd(i)=0 if x(i) is unbounded,
		//                1 if x(i) has only a lower bound,
		//                2 if x(i) has both lower and upper bounds, and
		//                3 if x(i) has only an upper bound. 
		//       On exit nbd is unchanged.
		
		//     g is a double precision array of dimension n.
		//       On entry g is the gradient of f(x).  g must be a nonzero vector.
		//       On exit g is unchanged.
		
		//     iorder is an integer working array of dimension n.
		//       iorder will be used to store the breakpoints in the piecewise
		//       linear path and free variables encountered. On exit,
		//         iorder(1),...,iorder(nleft) are indices of breakpoints
		//                                which have not been encountered; 
		//         iorder(nleft+1),...,iorder(nbreak) are indices of
		//                                     encountered breakpoints; and
		//         iorder(nfree),...,iorder(n) are indices of variables which
		//                 have no bound constraits along the search direction.
		
		//     iwhere is an integer array of dimension n.
		//       On entry iwhere indicates only the permanently fixed (iwhere=3)
		//       or free (iwhere= -1) components of x.
		//       On exit iwhere records the status of the current x variables.
		//       iwhere(i)=-3  if x(i) is free and has bounds, but is not moved
		//                 0   if x(i) is free and has bounds, and is moved
		//                 1   if x(i) is fixed at l(i), and l(i) .ne. u(i)
		//                 2   if x(i) is fixed at u(i), and u(i) .ne. l(i)
		//                 3   if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i)
		//                 -1  if x(i) is always free, i.e., it has no bounds.
		
		//     t is a double precision working array of dimension n. 
		//       t will be used to store the break points.
		
		//     d is a double precision array of dimension n used to store
		//       the Cauchy direction P(x-tg)-x.
		
		//     xcp is a double precision array of dimension n used to return the
		//       GCP on exit.
		
		//     m is an integer variable.
		//       On entry m is the maximum number of variable metric corrections 
		//         used to define the limited memory matrix.
		//       On exit m is unchanged.
		
		//     ws, wy, sy, and wt are double precision arrays.
		//       On entry they store information that defines the
		//                             limited memory BFGS matrix:
		//         ws(n,m) stores S, a set of s-vectors;
		//         wy(n,m) stores Y, a set of y-vectors;
		//         sy(m,m) stores S'Y;
		//         wt(m,m) stores the
		//                 Cholesky factorization of (theta*S'S+LD^(-1)L').
		//       On exit these arrays are unchanged.
		
		//     theta is a double precision variable.
		//       On entry theta is the scaling factor specifying B_0 = theta I.
		//       On exit theta is unchanged.
		
		//     col is an integer variable.
		//       On entry col is the actual number of variable metric
		//         corrections stored so far.
		//       On exit col is unchanged.
		
		//     head is an integer variable.
		//       On entry head is the location of the first s-vector (or y-vector)
		//         in S (or Y).
		//       On exit col is unchanged.
		
		//     p is a double precision working array of dimension 2m.
		//       p will be used to store the vector p = W^(T)d.
		
		//     c is a double precision working array of dimension 2m.
		//       c will be used to store the vector c = W^(T)(xcp-x).
		
		//     wbp is a double precision working array of dimension 2m.
		//       wbp will be used to store the row of W corresponding
		//         to a breakpoint.
		
		//     v is a double precision working array of dimension 2m.
		
		//     nseg is an integer variable.
		//       On exit nseg records the number of quadratic segments explored
		//         in searching for the GCP.
		
		//     sg and yg are double precision arrays of dimension m.
		//       On entry sg  and yg store S'g and Y'g correspondingly.
		//       On exit they are unchanged. 
		 
		//     iprint is an INTEGER variable that must be set by the user.
		//       It controls the frequency and type of output generated:
		//        iprint<0    no output is generated;
		//        iprint=0    print only one line at the last iteration;
		//        0<iprint<99 print also f and |proj g| every iprint iterations;
		//        iprint=99   print details of every iteration except n-vectors;
		//        iprint=100  print also the changes of active set and final x;
		//        iprint>100  print details of every iteration including x and g;
		//       When iprint > 0, the file iterate.dat will be created to
		//                        summarize the iteration.
		
		//     sbgnrm is a double precision variable.
		//       On entry sbgnrm is the norm of the projected gradient at x.
		//       On exit sbgnrm is unchanged.
		
		//     info is an integer variable.
		//       On entry info is 0.
		//       On exit info = 0       for normal return,
		//                    = nonzero for abnormal return when the the system
		//                              used in routine bmv is singular.
		
		//     Subprograms called:
		 
		//       L-BFGS-B Library ... hpsolb, bmv.
		
		//       Linpack ... dscal dcopy, daxpy.
		
		
		//     References:
		
		//       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
		//       memory algorithm for bound constrained optimization'',
		//       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
		
		//       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: FORTRAN
		//       Subroutines for Large Scale Bound Constrained Optimization''
		//       Tech. Report, NAM-11, EECS Department, Northwestern University,
		//       1994.
		
		//       (Postscript files of these papers are available via anonymous
		//        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.)
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************

		      boolean          xlower,xupper,bnded;
		      int          i,j,col2,nfree,nbreak,pointr,
		                      ibp,nleft,ibkmin,iter;
		      int mul6;
		      double f1,f2,dt,dtm,tsum,dibp,zibp,dibp2,bkmin,
		                     wmc,wmp,wmw,tj,tj0,neggi,
		                     f2_org, sum;
		      double tl = 0.0;
		      double tu = 0.0;
		      final double one = 1.0;
		      final double zero = 0.0;
		      boolean do888 = true;
		 
		//     Check the status of the variables, reset iwhere(i) if necessary;
		//       compute the Cauchy direction d and the breakpoints t; initialize
		//       the derivative f1 and the vector p = W'd (for theta = 1).
		 
		      if (sbgnrm <= zero) {
		         if (iprint >= 0) System.out.println("Subgnorm = 0.  GCP = X.");
		         for (i = 0; i < n; i++) {
		        	 xcp[i] = x[i];
		         }
		         return;
		      } // if (sbgnrm <= zero) 
		      bnded = true;
		      nfree = n + 1;
		      nbreak = 0;
		      ibkmin = 0;
		      bkmin = zero;
		      col2 = 2*col;
		      f1 = zero;
		      if (iprint >= 99) {
		    	  System.out.println("cauchy entered");
		      }

		//     We set p to zero and build it up as we determine d.

		      for (i = 0; i < col2; i++) {
		         p[i] = zero;
		      }

		//     In the following loop we determine for each variable its bound
		//        status and its breakpoint, and update p accordingly.
		//        Smallest breakpoint is identified.

		      for (i = 0; i < n; i++) { 
		         neggi = -g[i];      
		         if ((iwhere[i] != 3) && (iwhere[i] != -1)) {
		//             if x(i) is not a constant and has bounds,
		//             compute the difference between x(i) and its bounds.
		            if (nbd[i] <= 2) tl = x[i] - l[i];
		            if (nbd[i] >= 2) tu = u[i] - x[i];

		//           If a variable is close enough to a bound
		//             we treat it as at bound.
		            xlower = (nbd[i] <= 2) && (tl <= zero);
		            xupper = (nbd[i] >= 2) && (tu <= zero);

		//              reset iwhere(i).
		            iwhere[i] = 0;
		            if (xlower) {
		               if (neggi <= zero) iwhere[i] = 1;
		            }
		            else if (xupper) {
		               if (neggi >= zero) iwhere[i] = 2;
		            }
		            else {
		               if (Math.abs(neggi) <= zero) iwhere[i] = -3;
		            }
		         } // if ((iwhere[i] != 3) && (iwhere[i] != -1))
		         pointr = head;
		         if ((iwhere[i] != 0) && (iwhere[i] != -1)) {
		            d[i] = zero;
		         }
		         else {
		            d[i] = neggi;
		            f1 = f1 - neggi*neggi;
		//             calculate p := p - W'e_i* (g_i).
		            for (j = 1; j <= col; j++) {
		               p[j-1] = p[j-1] +  wy[i][pointr-1]* neggi;
		               p[col + j-1] = p[col + j-1] + ws[i][pointr-1]*neggi;
		               pointr = (pointr%m) + 1;
		            } // for (j = 1; j <= col; j++)
		            if ((nbd[i] <= 2) && (nbd[i] != 0)
		                             && (neggi < zero)) {
		//                                 x(i) + d(i) is bounded; compute t(i).
		               nbreak = nbreak + 1;
		               iorder[nbreak-1] = i;
		               t[nbreak-1] = tl/(-neggi);
		               if ((nbreak == 1) || (t[nbreak-1] < bkmin)) {
		                  bkmin = t[nbreak-1];
		                  ibkmin = nbreak;
		               }
		            }
		            else if ((nbd[i] >= 2) && (neggi > zero)) {
		//                                 x(i) + d(i) is bounded; compute t(i).
		               nbreak = nbreak + 1;
		               iorder[nbreak-1] = i;
		               t[nbreak-1] = tu/neggi;
		               if ((nbreak == 1) || (t[nbreak-1] < bkmin)) {
		                  bkmin = t[nbreak-1];
		                  ibkmin = nbreak;
		               }
		            }
		            else {
		//                x(i) + d(i) is not bounded.
		               nfree = nfree - 1;
		               iorder[nfree-1] = i;
		               if (Math.abs(neggi) > zero) bnded = false;
		            }
		         } // else
		      } // for (i = 0; i < n; i++) 
		 
		//     The indices of the nonzero components of d are now stored
		//       in iorder(1),...,iorder(nbreak) and iorder(nfree),...,iorder(n).
		//       The smallest of the nbreak breakpoints is in t(ibkmin)=bkmin.
		 
		      if (theta != one) {
		//                   complete the initialization of p for theta not= one.
		    	 for (i = 0; i < col; i++) {
		    	     p[col+i] = theta*p[col+i];	 
		    	 }
		      }
		 
		//     Initialize GCP xcp = x.

		      for (i = 0; i < n; i++) {
		    	  xcp[i] = x[i];
		      }

		      if ((nbreak == 0) && (nfree == (n + 1))) {
		//                  is a zero vector, return with the initial xcp as GCP.
		         if (iprint > 100) {
		        	 System.out.print("Cauchy X = ");
			        	mul6 = 6;
			        	for (i = 0; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + xcp[i]);
			        	}
			        	System.out.print("\n");
			        	mul6 += 6;
			            while (i < n) {
			            	System.out.print("    ");
			            	for (; i < Math.min(n, mul6); i++) {
				        		System.out.print("    " + xcp[i]);
				        	}
			            	System.out.print("\n");
				        	mul6 += 6;
			            } 
		         }
		         return;
		      }  
		 
		//     Initialize c = W'(xcp - x) = 0.
		  
		      for (j = 0; j < col2; j++) {
		         c[j] = zero;
		      }
		 
		//     Initialize derivative f2.
		 
		      f2 =  -theta*f1; 
		      f2_org  =  f2;
		      if (col > 0) {
		         bmv(m,sy,wt,col,p,v,info);
		         if (info[0] != 0) return;
		         sum = 0.0;
		         for (i = 0; i < col2; i++) {
		        	 sum += v[i]*p[i];
		         }
		         f2 = f2 - sum;
		      }
		      dtm = -f1/f2;
		      tsum = zero;
		      nseg[0] = 1;
		      if (iprint >= 99) { 
		          System.out.println("There are " + nbreak + " breakpoints");
		      }
		 
		//     If there are no breakpoints, locate the GCP and return. 
		 
		      if (nbreak != 0) {
		             
			      nleft = nbreak;
			      iter = 1;
			 
			 
			      tj = zero;
			 
			// ------------------- the beginning of the loop -------------------------
			 
		         do {
			 
			//     Find the next smallest breakpoint;
			//       compute dt = t(nleft) - t(nleft + 1).
			 
			      tj0 = tj;
			      if (iter == 1) {
			//         Since we already have the smallest breakpoint we need not do
			//         heapsort yet. Often only one breakpoint is used and the
			//         cost of heapsort is avoided.
			         tj = bkmin;
			         ibp = iorder[ibkmin-1];
			      }
			      else {
			         if (iter == 2) {
			//             Replace the already used smallest breakpoint with the
			//             breakpoint numbered nbreak > nlast, before heapsort call.
			            if (ibkmin != nbreak) {
			               t[ibkmin-1] = t[nbreak-1];
			               iorder[ibkmin-1] = iorder[nbreak-1];
			            }
			//        Update heap structure of breakpoints
		    //           (if iter=2, initialize heap).
			         }
			         hpsolb(nleft,t,iorder,iter-2);
			         tj = t[nleft-1];
			         ibp = iorder[nleft-1];  
			      }
			         
			      dt = tj - tj0;
			 
			      if ((dt != zero) && (iprint >= 100)) {
			    	 System.out.println("Piece " + nseg[0] + "f1, f2 at start point = " + f1 + " , " + f2);
			         System.out.println("Distance to the next break point = " + dt);
			         System.out.println("Distance to the stationary point " + dtm);
			      } //  if ((dt != zero) && (iprint >= 100))        
			 
			//     If a minimizer is within this interval, locate the GCP and return. 
			 
			      if (dtm < dt) {
			    	  break;
			      }
			 
			//     Otherwise fix one variable and
			//       reset the corresponding component of d to zero.
			    
			      tsum = tsum + dt;
			      nleft = nleft - 1;
			      iter = iter + 1;
			      dibp = d[ibp];
			      d[ibp] = zero;
			      if (dibp > zero) {
			         zibp = u[ibp] - x[ibp];
			         xcp[ibp] = u[ibp];
			         iwhere[ibp] = 2;
			      }
			      else {
			         zibp = l[ibp] - x[ibp];
			         xcp[ibp] = l[ibp];
			         iwhere[ibp] = 1;
			      }
			      if (iprint >= 100) {
			    	  System.out.println("Variable " + ibp + " is fixed");
			      }
			      if ((nleft == 0) && (nbreak == n)) {
			//                                             all n variables are fixed,
			//                                                return with xcp as GCP.
			         dtm = dt;
			         do888 = false;
			         break;
			      }
			 
			//     Update the derivative information.
			 
			      nseg[0] = nseg[0] + 1;
			      dibp2 = dibp*dibp;
			 
			//     Update f1 and f2.
			 
			//        temporarily set f1 and f2 for col=0.
			      f1 = f1 + dt*f2 + dibp2 - theta*dibp*zibp;
			      f2 = f2 - theta*dibp2;
	
			      if (col > 0) {
			//                          update c = c + dt*p.
			    	 for (i = 0; i < col2; i++) {
			    		 c[i] = c[i] + dt*p[i];
			    	 }
			 
			//           choose wbp,
			//           the row of W corresponding to the breakpoint encountered.
			         pointr = head;
			         for (j = 1; j <= col; j++) {
			            wbp[j-1] = wy[ibp][pointr-1];
			            wbp[col + j - 1] = theta*ws[ibp][pointr-1];
			            pointr = (pointr%m) + 1;
			         } // for (j = 1; j <= col; j++) 
			 
			//           compute (wbp)Mc, (wbp)Mp, and (wbp)M(wbp)'.
			         bmv(m,sy,wt,col,wbp,v,info);
			         if (info[0] != 0) return;
			         wmc = 0.0;
			         wmp = 0.0;
			         wmw = 0.0;
			         for (i = 0; i < col2; i++) {
			        	 wmc += c[i]*v[i];
			        	 wmp += p[i]*v[i];
			        	 wmw += wbp[i]*v[i];
			         }
			         
			 
			//           update p = p - dibp*wbp.
			         for (i = 0; i < col2 ; i++) {
			        	 p[i] = p[i] -dibp*wbp[i];
			         }
			 
			//           complete updating f1 and f2 while col > 0.
			         f1 = f1 + dibp*wmc;
			         f2 = f2 + 2.0*dibp*wmp - dibp2*wmw;
			      } // if (col > 0)
	
			      f2 = Math.max(epsmch*f2_org,f2);
			      if (nleft > 0) {
			         dtm = -f1/f2;
			         continue;
			//                 to repeat the loop for unsearched intervals.
			      }
			      else if(bnded) {
			         f1 = zero;
			         f2 = zero;
			         dtm = zero;
			         break;
			      }
			      else {
			         dtm = -f1/f2;
			         break;
			      }
		         } while (true);
	
			     // ------------------- the end of the loop -------------------------------
		      } // if (nbreak != 0)
		 
		 if (do888) {
		      if (iprint >= 99) {
		         System.out.println("GCP found in this segment");
		         System.out.println("Piece " + nseg[0] + " f1, f2 at start point = " + f1 + " , " + f2);
		         System.out.println("Distance to the stationary point = " + dtm);
		      } // if (iprint >= 99)
		      if (dtm <= zero) dtm = zero;
		      tsum = tsum + dtm;
		 
		//     Move free variables (i.e., the ones w/o breakpoints) and 
		//       the variables whose breakpoints haven't been reached.
		 
		      for (i = 0; i < n; i++) {
		    	  xcp[i] = xcp[i] + tsum*d[i];
		      }
		 } // if (do888)
		 
		 
		//     Update c = c + dtm*p = W'(x^c - x) 
		//       which will be used in computing r = Z'(B(x^c - x) + g).
		 
		      if (col > 0) {
		    	  for (i = 0; i < col2; i++) {
		    		  c[i] = c[i] + dtm*p[i];
		    	  }
		      }
		      if (iprint > 100) {
		    	  System.out.print("Cauchy X = ");
		        	mul6 = 6;
		        	for (i = 0; i < Math.min(n, mul6); i++) {
		        		System.out.print("    " + xcp[i]);
		        	}
		        	System.out.print("\n");
		        	mul6 += 6;
		            while (i < n) {
		            	System.out.print("    ");
		            	for (; i < Math.min(n, mul6); i++) {
			        		System.out.print("    " + xcp[i]);
			        	}
		            	System.out.print("\n");
			        	mul6 += 6;
		            } 
		      }
		      if (iprint >= 99) {
		    	  System.out.println("Exit cauchy");
		      }
		 
		      return;
	}
	
	private void bmv(int m, double sy[][], double wt[][], int col,
			double v[], double p[], int info[]) {

	    // integer m, col, info
	    // double precision sy(m, m), wt(m, m), v(2*col), p(2*col)
	
	//     ************
	
	//     Subroutine bmv
	
	//     This subroutine computes the product of the 2m x 2m middle matrix 
	//       in the compact L-BFGS formula of B and a 2m vector v;  
	//       it returns the product in p.
	       
	//     m is an integer variable.
	//       On entry m is the maximum number of variable metric corrections
	//         used to define the limited memory matrix.
	//       On exit m is unchanged.
	
	//     sy is a double precision array of dimension m x m.
	//       On entry sy specifies the matrix S'Y.
	//       On exit sy is unchanged.
	
	//     wt is a double precision array of dimension m x m.
	//       On entry wt specifies the upper triangular matrix J' which is 
	//         the Cholesky factor of (thetaS'S+LD^(-1)L').
	//       On exit wt is unchanged.
	
	//     col is an integer variable.
	//       On entry col specifies the number of s-vectors (or y-vectors)
	//         stored in the compact L-BFGS formula.
	//       On exit col is unchanged.
	
	//     v is a double precision array of dimension 2col.
	//       On entry v specifies vector v.
	//       On exit v is unchanged.
	
	//     p is a double precision array of dimension 2col.
	//       On entry p is unspecified.
	//       On exit p is the product Mv.
	//
	//     info is an integer variable.
	//       On entry info is unspecified.
	//       On exit info = 0       for normal return,
	//                    = nonzero for abnormal return when the system
	//                                to be solved by dtrsl is singular.
	
	//     Subprograms called:
	
	//       Linpack ... dtrsl.
	
	
	//                           *  *  *
	
	//     NEOS, November 1994. (Latest revision June 1996.)
	//     Optimization Technology Center.
	//     Argonne National Laboratory and Northwestern University.
	//     Written by
	//                        Ciyou Zhu
	//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
	
	
	//     ************
	
	    int          i,k,i2;
	    double sum;
	    double b[];
	
	    if (col == 0) return;
	
	//     PART I: solve [  D^(1/2)      O ] [ p1 ] = [ v1 ]
	//                   [ -L*D^(-1/2)   J ] [ p2 ]   [ v2 ].
	
	//       solve Jp2=v2+LD^(-1)v1.
	    p[col] = v[col];
	    for (i = 2; i <= col; i++) {
	       i2 = col + i;
	       sum = 0.0;
	       for (k = 1; k <= i - 1; k++) {
	          sum = sum + sy[i-1][k-1]*v[k-1]/sy[k-1][k-1];
	       }
	       p[i2-1] = v[i2-1] + sum;
	    } // for (i = 2; i <= col; i++)  
	//     Solve the triangular system
	    b = new double[col];
	    for (i = 0; i < col; i++) {
	    	b[i] = p[col+i];
	    }
	    dtrsl(wt,m,col,b,11,info);
	    for (i = 0; i < col; i++) {
	    	p[col+i] = b[i];
	    }
	    if (info[0] != 0) return;
	
	//       solve D^(1/2)p1=v1.
	    for (i = 0; i < col; i++) {
	       p[i] = v[i]/Math.sqrt(sy[i][i]);
	    }
	
	//     PART II: solve [ -D^(1/2)   D^(-1/2)*L'  ] [ p1 ] = [ p1 ]
	//                    [  0         J'           ] [ p2 ]   [ p2 ]. 
	
	//       solve J^Tp2=p2. 
	    for (i = 0; i < col; i++) {
	    	b[i] = p[col+i];
	    }
	    dtrsl(wt,m,col,b,01,info);
	    for (i = 0; i < col; i++) {
	    	p[col+i] = b[i];
	    }
	    if (info[0] != 0) return;
	
	//       compute p1=-D^(-1/2)(p1-D^(-1/2)L'p2)
	//                 =-D^(-1/2)p1+D^(-1)L'p2.  
	    for (i = 0; i < col; i++) {
	       p[i] = -p[i]/Math.sqrt(sy[i][i]);
	    }
	    for (i = 1; i <= col; i++) {
	       sum = 0.0;
	       for (k = i + 1; k <= col; k++) {
	          sum = sum + sy[k-1][i-1]*p[col+k-1]/sy[i-1][i-1];
	       }
	       p[i-1] = p[i-1] + sum;
	    }
	
	    return;
	}
	
	private void dtrsl(double t[][],int ldt,int n,double b[],int job,int info[]) {
	    // integer ldt,n,job,info
	    // double precision t(ldt,*),b(*)
	

	//     dtrsl solves systems of the form
	
	//                   t * x = b
	//     or
	//                   trans(t) * x = b
	
	//     where t is a triangular matrix of order n. here trans(t)
	//     denotes the transpose of the matrix t.
	
	//     on entry
	
	//         t         double precision(ldt,n)
	//                   t contains the matrix of the system. the zero
	//                   elements of the matrix are not referenced, and
	//                   the corresponding elements of the array can be
	//                   used to store other information.
	
	//         ldt       integer
	//                   ldt is the leading dimension of the array t.
	
	//         n         integer
	//                   n is the order of the system.
	
	//         b         double precision(n).
	//                   b contains the right hand side of the system.
	
	//         job       integer
	//                   job specifies what kind of system is to be solved.
	//                   if job is
	
	//                        00   solve t*x=b, t lower triangular,
	//                        01   solve t*x=b, t upper triangular,
	//                        10   solve trans(t)*x=b, t lower triangular,
	//                        11   solve trans(t)*x=b, t upper triangular.
	
	//     on return
	
	//         b         b contains the solution, if info .eq. 0.
	//                   otherwise b is unaltered.
	
	//         info      integer
	//                   info contains zero if the system is nonsingular.
	//                   otherwise info contains the index of
	//                   the first zero diagonal element of t.
	
	//     linpack. this version dated 08/14/78 .
	//     g. w. stewart, university of maryland, argonne national lab.
	
	//     subroutines and functions
	
	//     blas daxpy,ddot
	//     fortran mod
	
	//     internal variables
	
	    double temp,sum;
	    int i, ccase,j,jj;
	
	//     begin block permitting ...exits to 150
	
	//        check for zero diagonal elements.
	
	      for (i = 0; i < n; i++) {
	//     ......exit
	          if (t[i][i] == 0.0) {
	        	  return;
	          }
	      }
	       info[0] = 0;
	
	//        determine the task and go to it.
	
	       ccase = 1;
	       if ((job%10) != 0) ccase = 2;
	       if ((job%100)/10 != 0) ccase = ccase + 2;
	       if (ccase == 1) {

	//        solve t*x=b for t lower triangular
	
	          b[0] = b[0]/t[0][0];
	          if (n < 2) {
	        	  return;
	          }
	          for (j = 2; j <= n; j++) {
	             temp = -b[j-2];
	             for (i = 0; i < n-j+1; i++) {
	            	 b[j+i-1] = b[j+i-1] + temp * t[j-1+i][j-2];
	             }
	             b[j-1] = b[j-1]/t[j-1][j-1];
	          } // for (j = 2; j <= n; j++)
	       return;
	       }
	       
	       if (ccase == 2) {
	
	//        solve t*x=b for t upper triangular.
	
	          b[n-1] = b[n-1]/t[n-1][n-1];
	          if (n < 2) {
	        	  return;
	          }
	          for (jj = 2; jj <= n; jj++) {
	             j = n - jj + 1;
	             temp = -b[j];
	             for (i = 0; i < j; i++) {
	            	 b[i] = b[i] + temp * t[i][j];
	             }
	             b[j-1] = b[j-1]/t[j-1][j-1];
	          }
	          return;
	       }
	       
	       if (ccase == 3) {
	
	//        solve trans(t)*x=b for t lower triangular.

	          b[n-1] = b[n-1]/t[n-1][n-1];
	          if (n < 2) {
	        	  return;
	          }
	          for (jj = 2; jj <= n; jj++) {
	             j = n - jj + 1;
	             sum = 0;
	             for (i = 0; i < jj-1; i++) {
	            	 sum += t[j+i][j-1]*b[j+i];
	             }
	             b[j-1] = b[j-1] - sum;
	             b[j-1] = b[j-1]/t[j-1][j-1];
	          }
	          return;
	       }
	       
	       if (ccase == 4) {
	
	//        solve trans(t)*x=b for t upper triangular.
	          b[0] = b[0]/t[0][0];
	          if (n < 2) {
	        	  return;
	          }
	          for (j = 2; j <= n; j++) {
	        	 sum = 0.0;
	        	 for (i = 0; i < j-1; i++) {
	        		 sum += t[i][j-1]*b[i];
	        	 }
	        	 b[j-1] = b[j-1] - sum;
	        	 b[j-1] = b[j-1]/t[j-1][j-1];
	          }
	    return;
	       }
	}

	private void hpsolb(int n, double t[], int iorder[], int iheap) {
	    // integer          iheap, n, iorder(n)
	    // double precision t(n)
	
	//     ************
	
	//     Subroutine hpsolb 
	
	//     This subroutine sorts out the least element of t, and puts the
	//       remaining elements of t in a heap.
	 
	//     n is an integer variable.
	//       On entry n is the dimension of the arrays t and iorder.
	//       On exit n is unchanged.
	
	//     t is a double precision array of dimension n.
	//       On entry t stores the elements to be sorted,
	//       On exit t(n) stores the least elements of t, and t(1) to t(n-1)
	//         stores the remaining elements in the form of a heap.
	
	//     iorder is an integer array of dimension n.
	//       On entry iorder(i) is the index of t(i).
	//       On exit iorder(i) is still the index of t(i), but iorder may be
	//         permuted in accordance with t.
	
	//     iheap is an integer variable specifying the task.
	//       On entry iheap should be set as follows:
	//         iheap .eq. 0 if t(1) to t(n) is not in the form of a heap,
	//         iheap .ne. 0 if otherwise.
	//       On exit iheap is unchanged.
	
	
	//     References:
	//       Algorithm 232 of CACM (J. W. J. Williams): HEAPSORT.
	
	//                           *  *  *
	
	//     NEOS, November 1994. (Latest revision June 1996.)
	//     Optimization Technology Center.
	//     Argonne National Laboratory and Northwestern University.
	//     Written by
	//                        Ciyou Zhu
	//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
	
	//     ************
	
	    int          i,j,k,indxin,indxou;
	    double ddum,out;
	
	    if (iheap == 0) {
	
	//        Rearrange the elements t(1) to t(n) to form a heap.
	
	       for (k = 2; k <= n; k++) {
	          ddum  = t[k-1];
	          indxin = iorder[k-1];
	
	//           Add ddum to the heap.
	          i = k;
	          do {
		          if (i > 1) {
		             j = i/2;
		             if (ddum < t[j-1]) {
		                t[i-1] = t[j-1];
		                iorder[i-1] = iorder[j-1];
		                i = j;
		                continue;
		             } 
		          } // if (i > 1)
		          break;
	          } while (true);
	          t[i-1] = ddum;
	          iorder[i-1] = indxin;
	       } // for (k = 2; k <= n; k++)
	    } // if (iheap == 0)
	
	//     Assign to 'out' the value of t(1), the least member of the heap,
	//        and rearrange the remaining members to form a heap as
	//        elements 1 to n-1 of t.
	
	    if (n > 1) {
	       i = 1;
	       out = t[0];
	       indxou = iorder[0];
	       ddum  = t[n-1];
	       indxin  = iorder[n-1];
	
	//        Restore the heap 
	       do {
		       j = i+i;
		       if (j <= n-1) {
		          if (t[j] < t[j-1]) j = j+1;
		          if (t[j-1] < ddum ) {
		             t[i-1] = t[j-1];
		             iorder[i-1] = iorder[j-1];
		             i = j;
		             continue;
		          } // if (t[j-1] < ddum ) 
		       } // if (j <= n-1)
		       break;
	       } while (true);
	       t[i-1] = ddum;
	       iorder[i-1] = indxin;
	
	//     Put the least member in t(n). 
	
	       t[n-1] = out;
	       iorder[n-1] = indxou;
	    } // if (n > 1) 
	
	    return;
	}

	private void freev(int n, int nfree[], int index[], int nenter[], int ileave[], int indx2[], 
                       int iwhere[], boolean wrk[], boolean updatd, boolean cnstnd, 
                       int iprint, int iter) {

		      // integer n, nfree, nenter, ileave, iprint, iter, 
		     //        index(n), indx2(n), iwhere(n)
		      // logical wrk, updatd, cnstnd

		//     ************
		
		//     Subroutine freev 
		
		//     This subroutine counts the entering and leaving variables when
		//       iter > 0, and finds the index set of free and active variables
		//       at the GCP.
		
		//     cnstnd is a logical variable indicating whether bounds are present
		
		//     index is an integer array of dimension n
		//       for i=1,...,nfree, index(i) are the indices of free variables
		//       for i=nfree+1,...,n, index(i) are the indices of bound variables
		//       On entry after the first iteration, index gives 
		//         the free variables at the previous iteration.
		//       On exit it gives the free variables based on the determination
		//         in cauchy using the array iwhere.
		
		//     indx2 is an integer array of dimension n
		//       On entry indx2 is unspecified.
		//       On exit with iter>0, indx2 indicates which variables
		//          have changed status since the previous iteration.
		//       For i= 1,...,nenter, indx2(i) have changed from bound to free.
		//       For i= ileave+1,...,n, indx2(i) have changed from free to bound.
		 
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************
		 
		      int iact,i,k;

		      nenter[0] = 0;
		      ileave[0] = n + 1;
		      if ((iter > 0) && cnstnd) {
		//                           count the entering and leaving variables.
		         for (i = 0; i < nfree[0]; i++) {
		            k = index[i];

		//            write(6,*) ' k  = index(i) ', k
		//            write(6,*) ' index = ', i

		            if (iwhere[k-1] > 0) {
		               ileave[0] = ileave[0] - 1;
		               indx2[ileave[0]-1] = k;
		               if (iprint >= 100) {
		                   System.out.println("Variable " + k + " leaves the set of free variables");
		               }
		            } // if (iwhere[k-1] > 0)
		         } // for (i = 0; i < nfree; i++)
		         for (i = nfree[0]; i < n; i++) {
		            k = index[i];
		            if (iwhere[k-1] <= 0) {
		               nenter[0] = nenter[0] + 1;
		               indx2[nenter[0]-1] = k;
		               if (iprint >= 100) {
		                   System.out.println("Variable " +k + " enters the set of free variables");
		               }
		            } // if (iwhere[k-1] <= 0)
		         } // for (i = nfree; i < n; i++)
		         if (iprint >= 99) {
		             System.out.println((n+1-ileave[0]) + " variables leave; " + nenter[0] + " variables enter");
		         }
		      } // if ((iter > 0) && cnstnd)
		      wrk[0] = (ileave[0] < n+1) || (nenter[0] > 0) || updatd;
		 
		//     Find the index set of free and active variables at the GCP.
		 
		      nfree[0] = 0; 
		      iact = n + 1;
		      for (i = 1; i <= n; i++) {
		         if (iwhere[i-1] <= 0) {
		            nfree[0] = nfree[0] + 1;
		            index[nfree[0]-1] = i;
		         }
		         else {
		            iact = iact - 1;
		            index[iact-1] = i;
		         }
		      } // for (i = 1; i <= n; i++)
		      if (iprint >= 99) {
		          System.out.println(nfree[0] + " variables are free at GCP " + (iter + 1));
		      }

		      return;
	}
	
	private void formk(int n, int nsub, int ind[], int nenter, int ileave, int indx2[], int iupdat, 
	                   boolean updatd, double wn[][], double wn1[][], int m, double ws[][], 
	                   double wy[][], double sy[][], double theta, int col, int head, int info[]) {

		      // integer          n, nsub, m, col, head, nenter, ileave, iupdat,
		     //                 info, ind(n), indx2(n)
		      // double precision theta, wn(2*m, 2*m), wn1(2*m, 2*m),
		     //                 ws(n, m), wy(n, m), sy(m, m)
		      // logical          updatd

		//     ************
		
		//     Subroutine formk 
		
		//     This subroutine forms  the LEL^T factorization of the indefinite
		
		//       matrix    K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
		//                     [L_a -R_z           theta*S'AA'S ]
		//                                                    where E = [-I  0]
		//                                                              [ 0  I]
		//     The matrix K can be shown to be equal to the matrix M^[-1]N
		//       occurring in section 5.1 of [1], as well as to the matrix
		//       Mbar^[-1] Nbar in section 5.3.
		
		//     n is an integer variable.
		//       On entry n is the dimension of the problem.
		//       On exit n is unchanged.
		
		//     nsub is an integer variable
		//       On entry nsub is the number of subspace variables in free set.
		//       On exit nsub is not changed.
		
		//     ind is an integer array of dimension nsub.
		//       On entry ind specifies the indices of subspace variables.
		//       On exit ind is unchanged. 
		
		//     nenter is an integer variable.
		//       On entry nenter is the number of variables entering the 
		//         free set.
		//       On exit nenter is unchanged. 
		
		//     ileave is an integer variable.
		//       On entry indx2(ileave),...,indx2(n) are the variables leaving
		//         the free set.
		//       On exit ileave is unchanged. 
		
		//     indx2 is an integer array of dimension n.
		//       On entry indx2(1),...,indx2(nenter) are the variables entering
		//         the free set, while indx2(ileave),...,indx2(n) are the
		//         variables leaving the free set.
		//       On exit indx2 is unchanged. 
		
		//     iupdat is an integer variable.
		//       On entry iupdat is the total number of BFGS updates made so far.
		//       On exit iupdat is unchanged. 
		
		//     updatd is a logical variable.
		//       On entry 'updatd' is true if the L-BFGS matrix is updatd.
		//       On exit 'updatd' is unchanged. 
		
		//     wn is a double precision array of dimension 2m x 2m.
		//       On entry wn is unspecified.
		//       On exit the upper triangle of wn stores the LEL^T factorization
		//         of the 2*col x 2*col indefinite matrix
		//                     [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
		//                     [L_a -R_z           theta*S'AA'S ]
		
		//     wn1 is a double precision array of dimension 2m x 2m.
		//       On entry wn1 stores the lower triangular part of 
		//                     [Y' ZZ'Y   L_a'+R_z']
		//                     [L_a+R_z   S'AA'S   ]
		//         in the previous iteration.
		//       On exit wn1 stores the corresponding updated matrices.
		//       The purpose of wn1 is just to store these inner products
		//       so they can be easily updated and inserted into wn.
		
		//     m is an integer variable.
		//       On entry m is the maximum number of variable metric corrections
		//         used to define the limited memory matrix.
		//       On exit m is unchanged.
		
		//     ws, wy, sy, and wtyy are double precision arrays;
		//     theta is a double precision variable;
		//     col is an integer variable;
		//     head is an integer variable.
		//       On entry they store the information defining the
		//                                          limited memory BFGS matrix:
		//         ws(n,m) stores S, a set of s-vectors;
		//         wy(n,m) stores Y, a set of y-vectors;
		//         sy(m,m) stores S'Y;
		//         wtyy(m,m) stores the Cholesky factorization
		//                                   of (theta*S'S+LD^(-1)L')
		//         theta is the scaling factor specifying B_0 = theta I;
		//         col is the number of variable metric corrections stored;
		//         head is the location of the 1st s- (or y-) vector in S (or Y).
		//       On exit they are unchanged.
		
		//     info is an integer variable.
		//       On entry info is unspecified.
		//       On exit info =  0 for normal return;
		//                    = -1 when the 1st Cholesky factorization failed;
		//                    = -2 when the 2st Cholesky factorization failed.
		//
		//     Subprograms called:
		
		//       Linpack ... dcopy, dpofa, dtrsl.
		
		
		//     References:
		//       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
		//       memory algorithm for bound constrained optimization'',
		//       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
		
		//       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: a
		//       limited memory FORTRAN code for solving bound constrained
		//       optimization problems'', Tech. Report, NAM-11, EECS Department,
		//       Northwestern University, 1994.
		
		//       (Postscript files of these papers are available via anonymous
		//        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.)
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************

		      int          m2,ipntr,jpntr,iy,is,jy,js,is1,js1,k1,i,k,
		                   col2,pbegin,pend,dbegin,dend,upcl;
		      int j;
		      double temp1,temp2,temp3,temp4;
		      double b[] = new double[col];
		      double a[][] = new double[2*m-col][2*m-col];
		      final double one = 1.0;
		      final double zero = 0.0;
		      double sum;

		//     Form the lower triangular part of
		//               WN1 = [Y' ZZ'Y   L_a'+R_z'] 
		//                     [L_a+R_z   S'AA'S   ]
		//        where L_a is the strictly lower triangular part of S'AA'Y
		//              R_z is the upper triangular part of S'ZZ'Y.
		      
		      if (updatd) {
		         if (iupdat > m) { 
		//                                 shift old part of WN1.
		            for (jy = 1; jy <= m - 1; jy++) {
		               js = m + jy;
		               for (i = 0; i < m-jy; i++) {
		            	   wn1[jy-1+i][jy-1] = wn1[jy + i][jy];
		               }
		               for (i = 0; i < m-jy; i++) {
		            	   wn1[js-1+i][js-1] = wn1[js+i][js];
		               }
		               for (i = 0; i < m-1; i++) {
		            	   wn1[m+i][jy-1] = wn1[m+1][jy];
		               }
		            } // for (jy = 1; jy <= m - 1; jy++)
		         } // if (iupdat > m)
		 
		//          put new rows in blocks (1,1), (2,1) and (2,2).
		         pbegin = 1;
		         pend = nsub;
		         dbegin = nsub + 1;
		         dend = n;
		         iy = col;
		         is = m + col;
		         ipntr = head + col - 1;
		         if (ipntr > m) ipntr = ipntr - m;    
		         jpntr = head;
		         for (jy = 1; jy <= col; jy++) {
		            js = m + jy;
		            temp1 = zero;
		            temp2 = zero;
		            temp3 = zero;
		//             compute element jy of row 'col' of Y'ZZ'Y
		            for (k = pbegin; k <= pend; k++) {
		               k1 = ind[k-1];
		               temp1 = temp1 + wy[k1-1][ipntr-1]*wy[k1-1][jpntr-1];
		            } // for (k = pbegin; k <= pend; k++)
		//             compute elements jy of row 'col' of L_a and S'AA'S
		            for (k = dbegin; k <= dend; k++) {
		               k1 = ind[k-1];
		               temp2 = temp2 + ws[k1-1][ipntr-1]*ws[k1-1][jpntr-1];
		               temp3 = temp3 + ws[k1-1][ipntr-1]*wy[k1-1][jpntr-1];
		            } // for (k = dbegin; k <= dend; k++)
		            wn1[iy-1][jy-1] = temp1;
		            wn1[is-1][js-1] = temp2;
		            wn1[is-1][jy-1] = temp3;
		            jpntr = (jpntr%m) + 1;
		         } // for (jy = 1; jy <= col; jy++)
		 
		//          put new column in block (2,1).
		         jy = col;       
		         jpntr = head + col - 1;
		         if (jpntr > m) jpntr = jpntr - m;
		         ipntr = head;
		         for (i = 1; i <= col; i++) {
		            is = m + i;
		            temp3 = zero;
		//             compute element i of column 'col' of R_z
		            for (k = pbegin; k <= pend; k++) {
		               k1 = ind[k-1];
		               temp3 = temp3 + ws[k1-1][ipntr-1]*wy[k1-1][jpntr-1];
		            } // for (k = pbegin; k <= pend; k++) 
		            ipntr = (ipntr%m) + 1;
		            wn1[is-1][jy-1] = temp3;
		         } // for (i = 1; i <= col; i++)
		         upcl = col - 1;
		      } // if (updatd)
		      else {
		         upcl = col;
		      }
		 
		//       modify the old parts in blocks (1,1) and (2,2) due to changes
		//       in the set of free variables.
		      ipntr = head;      
		      for (iy = 1; iy <= upcl; iy++) {
		         is = m + iy;
		         jpntr = head;
		         for (jy = 1; jy <= iy; jy++) {
		            js = m + jy;
		            temp1 = zero;
		            temp2 = zero;
		            temp3 = zero;
		            temp4 = zero;
		            for (k = 1; k <= nenter; k++) {
		               k1 = indx2[k-1];
		               temp1 = temp1 + wy[k1-1][ipntr-1]*wy[k1-1][jpntr-1];
		               temp2 = temp2 + ws[k1-1][ipntr-1]*ws[k1-1][jpntr-1];
		            } // for (k = 1; k <= nenter; k++) 
		            for (k = ileave; k <= n; k++) {
		               k1 = indx2[k-1];
		               temp3 = temp3 + wy[k1-1][ipntr-1]*wy[k1-1][jpntr-1];
		               temp4 = temp4 + ws[k1-1][ipntr-1]*ws[k1-1][jpntr-1];
		            } // for (k = ileave; k <= n; k++) 
		            wn1[iy-1][jy-1] = wn1[iy-1][jy-1] + temp1 - temp3; 
		            wn1[is-1][js-1] = wn1[is-1][js-1] - temp2 + temp4; 
		            jpntr = (jpntr%m) + 1;
		         } // for (jy = 1; jy <= iy; jy++)
		         ipntr = (ipntr%m) + 1;
		      } // for (iy = 1; iy <= upcl; iy++)
		 
		//       modify the old parts in block (2,1).
		      ipntr = head;      
		      for (is = m + 1; is <= m + upcl; is++) {
		         jpntr = head; 
		         for (jy = 1; jy <= upcl; jy++) {
		            temp1 = zero;
		            temp3 = zero;
		            for (k = 1; k <= nenter; k++) {
		               k1 = indx2[k-1];
		               temp1 = temp1 + ws[k1-1][ipntr-1]*wy[k1-1][jpntr-1];
		            } // for (k = 1; k <= nenter; k++)
		            for (k = ileave; k <= n; k++) {
		               k1 = indx2[k-1];
		               temp3 = temp3 + ws[k1-1][ipntr-1]*wy[k1-1][jpntr-1];
		            } // for (k = ileave; k <= n; k++)
		            if (is <= (jy + m)) {
		               wn1[is-1][jy-1] = wn1[is-1][jy-1] + temp1 - temp3;
		            }
		            else {
		               wn1[is-1][jy-1] = wn1[is-1][jy-1] - temp1 + temp3;  
		            }
		            jpntr = (jpntr%m) + 1;
		         } // for (jy = 1; jy <= upcl; jy++)
		         ipntr = (ipntr%m) + 1;
		      } // for (is = m + 1; is <= m + upcl; is++)
		 
		//     Form the upper triangle of WN = [D+Y' ZZ'Y/theta   -L_a'+R_z' ] 
		//                                     [-L_a +R_z        S'AA'S*theta]

		      m2 = 2*m;
		      for (iy = 1; iy <= col; iy++) {
		         is = col + iy;
		         is1 = m + iy;
		         for (jy = 1; jy <= iy; jy++) {
		            js = col + jy;
		            js1 = m + jy;
		            wn[jy-1][iy-1] = wn1[iy-1][jy-1]/theta;
		            wn[js-1][is-1] = wn1[is1-1][js1-1]*theta;
		         } // for (jy = 1; jy <= iy; jy++)
		         for (jy = 1; jy <= iy - 1; jy++) {
		            wn[jy-1][is-1] = -wn1[is1-1][jy-1];
		         }
		         for (jy = iy; jy <= col; jy++) {
		            wn[jy-1][is-1] = wn1[is1-1][jy-1];
		         }
		         wn[iy-1][iy-1] = wn[iy-1][iy-1] + sy[iy-1][iy-1];
		      } // for (iy = 1; iy <= col; iy++)

		//     Form the upper triangle of WN= [  LL'            L^-1(-L_a'+R_z')] 
		//                                    [(-L_a +R_z)L'^-1   S'AA'S*theta  ]

		//        first Cholesky factor (1,1) block of wn to get LL'
        //                          with L' stored in the upper triangle of wn.
		      dpofa(wn,m2,col,info);
		      if (info[0] != 0) {
		         info[0] = -1;
		         return;
		      }
		//        then form L^-1(-L_a'+R_z') in the (1,2) block.
		      col2 = 2*col;
		      for (js = col+1; js <= col2; js++) {
		    	 for (i = 0; i < col; i++) {
		    		 b[i] = wn[i][js-1];
		    	 }
		         dtrsl(wn,m2,col,b,11,info);
		         for (i = 0; i < col; i++) {
		    		 wn[i][js-1] = b[i];
		    	 }
		      }

		//     Form S'AA'S*theta + (L^-1(-L_a'+R_z'))'L^-1(-L_a'+R_z') in the
		//        upper triangle of (2,2) block of wn.
		                      

		      for (is = col+1; is <= col2; is++) {
		         for (js = is; js <= col2; js++) {
		        	   sum = 0.0;
		        	   for (i = 0; i < col; i++) {
		        		   sum += wn[i][is-1]*wn[i][js-1];
		        	   }
		               wn[is-1][js-1] = wn[is-1][js-1] + sum;
		         } // for (js = is; js <= col2; js++)
		      } // for (is = col+1; is <= col2; is++) 

		//     Cholesky factorization of (2,2) block of wn.
              for (i = 0; i < 2*m-col; i++) {
            	  for (j = 0; j < 2*m-col; j++) {
            		  a[i][j] = wn[col+i][col+j];
            	  }
              }
		      dpofa(a,m2,col,info);
		      for (i = 0; i < 2*m-col; i++) {
            	  for (j = 0; j < 2*m-col; j++) {
            		  wn[col+i][col+j] = a[i][j];
            	  }
              }
		      if (info[0] != 0) {
		         info[0] = -2;
		         return;
		      }

		      return;
	}
	
	private void dpofa(double a[][],int lda,int n, int info[]) {
	    // integer lda,n,info
	    // double precision a(lda,*)
	
	//     dpofa factors a double precision symmetric positive definite
	//     matrix.
	
	//     dpofa is usually called by dpoco, but it can be called
	//     directly with a saving in time if  rcond  is not needed.
	//     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
	
	//     on entry
	
	//        a       double precision(lda, n)
	//                the symmetric matrix to be factored.  only the
	//                diagonal and upper triangle are used.
	
	//        lda     integer
	//                the leading dimension of the array  a .
	
	//        n       integer
	//                the order of the matrix  a .
	
	//     on return
	
	//        a       an upper triangular matrix  r  so that  a = trans(r)*r
	//                where  trans(r)  is the transpose.
	//                the strict lower triangle is unaltered.
	//                if  info .ne. 0 , the factorization is not complete.
	
	//        info    integer
	//                = 0  for normal return.
	//                = k  signals an error condition.  the leading minor
	//                     of order  k  is not positive definite.
	
	//     linpack.  this version dated 08/14/78 .
	//     cleve moler, university of new mexico, argonne national lab.
	
	//     subroutines and functions
	
	//     blas ddot
	//     fortran sqrt
	
	//     internal variables
	
	    double t;
	    double s;
	    double sum;
	    int i,j,jm1,k;
	//     begin block with ...exits to 40
	
	
	       for (j = 1; j <= n; j++) {
	          info[0] = j;
	          s = 0.0;
	          jm1 = j - 1;
	          if (jm1 >= 1) {
		          for (k = 1; k <= jm1; k++) {
		        	 sum = 0.0;
		        	 for (i = 0; i < k-1; i++) {
		        		 sum += a[i][k-1]*a[i][j-1];
		        	 }
		        	 t = a[k-1][j-1] - sum;
		             t = t/a[k-1][k-1];
		             a[k-1][j-1] = t;
		             s = s + t*t;
		          } // for (k = 1; k <= jm1; k++)
	          } // if (jm1 >= 1)
	          s = a[j-1][j-1] - s;
	//     ......exit
	          if (s <= 0.0) {
	        	  return;
	          }
	          a[j-1][j-1] = Math.sqrt(s);
	       } // for (j = 1; j <= n; j++)
	       info[0] = 0;
	       return;
	}
	
	private void cmprlb(int n, int m, double x[], double g[], double ws[][], 
			double wy[][], double sy[][], double wt[][], double z[], double r[],
			double wa1[], double wa2[], int index[], double theta, int col, int head, 
			int nfree, boolean cnstnd, int info[]) {
		 
		      // logical          cnstnd
		      // integer          n, m, col, head, nfree, info, index(n)
		      // double precision theta, 
		     //                 x(n), g(n), z(n), r(n), wa(4*m), 
		     //                 ws(n, m), wy(n, m), sy(m, m), wt(m, m)

		//     ************
		
		//     Subroutine cmprlb 
		
		//       This subroutine computes r=-Z'B(xcp-xk)-Z'g by using 
		//         wa(2m+1)=W'(xcp-x) from subroutine cauchy.
		
		//     Subprograms called:
		
		//       L-BFGS-B Library ... bmv.
		
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		//
		//     ************
		 
		      int          i,j,k,pointr;
		      double a1,a2;

		      if ((! cnstnd) && (col > 0)) { 
		         for (i = 0; i < n; i++) {
		            r[i] = -g[i];
		         } // for (i = 0; i < n; i++)
		      } // if ((! cnstnd) && (col > 0))
		      else {
		         for (i = 0; i < nfree; i++) {
		            k = index[i];
		            r[i] = -theta*(z[k-1] - x[k-1]) - g[k-1];
		         } // for (i = 0; i < nfree; i++)
		         bmv(m,sy,wt,col,wa2,wa1,info);
		         if (info[0] != 0) {
		            info[0] = -8;
		            return;
		         }
		         pointr = head; 
		         for (j = 1; j <= col; j++) {
		            a1 = wa1[j-1];
		            a2 = theta*wa1[col + j -1];
		            for (i = 0; i < nfree; i++) {
		               k = index[i];
		               r[i] = r[i] + wy[k-1][pointr-1]*a1 + ws[k-1][pointr-1]*a2;
		            } // for (i = 0; i < nfree; i++)
		            pointr = (pointr%m) + 1;
		         } // for (j = 1; j <= col; j++)
		      }

		      return;
	}
	
	private void subsm (int n, int m, int nsub, int ind[], double l[], double u[],
			int nbd[], double x[], double d[], double xp[], double ws[][], 
			double wy[][], double theta, double xx[], double gg[],
		    int col, int head, int iword[], double wv[], double wn[][],
		    int iprint, int info[]) {
		      // implicit none
		      // integer          n, m, nsub, col, head, iword, iprint, info, 
		     //                 ind(nsub), nbd(n)
		     // double precision theta, 
		     //                 l(n), u(n), x(n), d(n), xp(n), xx(n), gg(n),
		     //                 ws(n, m), wy(n, m), 
		     //                 wv(2*m), wn(2*m, 2*m)

		//     **********************************************************************
		
		//     This routine contains the major changes in the updated version.
		//     The changes are described in the accompanying paper
		
		//      Jose Luis Morales, Jorge Nocedal
		//      "Remark On Algorithm 788: L-BFGS-B: Fortran Subroutines for Large-Scale
		//       Bound Constrained Optimization". Decemmber 27, 2010.
		
		//             J.L. Morales  Departamento de Matematicas, 
		//                           Instituto Tecnologico Autonomo de Mexico
		//                           Mexico D.F.
		
		//             J, Nocedal    Department of Electrical Engineering and
		//                           Computer Science.
		//                           Northwestern University. Evanston, IL. USA
		
		//                           January 17, 2011
		
		//      **********************************************************************
		                           
		
		//     Subroutine subsm
		
		//     Given xcp, l, u, r, an index set that specifies
		//       the active set at xcp, and an l-BFGS matrix B 
		//       (in terms of WY, WS, SY, WT, head, col, and theta), 
		//       this subroutine computes an approximate solution
		//       of the subspace problem
		
		//       (P)   min Q(x) = r'(x-xcp) + 1/2 (x-xcp)' B (x-xcp)
		
		//             subject to l<=x<=u
		//                       x_i=xcp_i for all i in A(xcp)
		                     
		//       along the subspace unconstrained Newton direction 
		       
		//          d = -(Z'BZ)^(-1) r.
		
		//       The formula for the Newton direction, given the L-BFGS matrix
		//       and the Sherman-Morrison formula, is
		
		//          d = (1/theta)r + (1/theta*2) Z'WK^(-1)W'Z r.
		 
		//       where
		//                 K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
		//                     [L_a -R_z           theta*S'AA'S ]
		
		//     Note that this procedure for computing d differs 
		//     from that described in [1]. One can show that the matrix K is
		//     equal to the matrix M^[-1]N in that paper.
		
		//     n is an integer variable.
		//       On entry n is the dimension of the problem.
		//       On exit n is unchanged.
		
		//     m is an integer variable.
		//       On entry m is the maximum number of variable metric corrections
		//         used to define the limited memory matrix.
		//       On exit m is unchanged.
		
		//     nsub is an integer variable.
		//       On entry nsub is the number of free variables.
		//       On exit nsub is unchanged.
		
		//     ind is an integer array of dimension nsub.
		//       On entry ind specifies the coordinate indices of free variables.
		//       On exit ind is unchanged.
		
		//     l is a double precision array of dimension n.
		//       On entry l is the lower bound of x.
		//       On exit l is unchanged.
		
		//     u is a double precision array of dimension n.
		//       On entry u is the upper bound of x.
		//       On exit u is unchanged.
		
		//     nbd is a integer array of dimension n.
		//       On entry nbd represents the type of bounds imposed on the
		//         variables, and must be specified as follows:
		//         nbd(i)=0 if x(i) is unbounded,
		//                1 if x(i) has only a lower bound,
		//                2 if x(i) has both lower and upper bounds, and
		//                3 if x(i) has only an upper bound.
		//       On exit nbd is unchanged.
		
		//     x is a double precision array of dimension n.
		//       On entry x specifies the Cauchy point xcp. 
		//       On exit x(i) is the minimizer of Q over the subspace of
		//                                                        free variables. 
		
		//     d is a double precision array of dimension n.
		//       On entry d is the reduced gradient of Q at xcp.
		//       On exit d is the Newton direction of Q. 
		
		//    xp is a double precision array of dimension n.
		//       used to safeguard the projected Newton direction 
		
		//    xx is a double precision array of dimension n
		//       On entry it holds the current iterate
		//       On output it is unchanged

		//    gg is a double precision array of dimension n
		//       On entry it holds the gradient at the current iterate
		//       On output it is unchanged
		
		//     ws and wy are double precision arrays;
		//     theta is a double precision variable;
		//     col is an integer variable;
		//     head is an integer variable.
		//       On entry they store the information defining the
		//                                          limited memory BFGS matrix:
		//         ws(n,m) stores S, a set of s-vectors;
		//         wy(n,m) stores Y, a set of y-vectors;
		//         theta is the scaling factor specifying B_0 = theta I;
		//         col is the number of variable metric corrections stored;
		//         head is the location of the 1st s- (or y-) vector in S (or Y).
		//       On exit they are unchanged.
		
		//     iword is an integer variable.
		//       On entry iword is unspecified.
		//       On exit iword specifies the status of the subspace solution.
		//         iword = 0 if the solution is in the box,
		//                 1 if some bound is encountered.
		
		//     wv is a double precision working array of dimension 2m.
		
		//     wn is a double precision array of dimension 2m x 2m.
		//       On entry the upper triangle of wn stores the LEL^T factorization
		//         of the indefinite matrix
		
		//              K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
		//                  [L_a -R_z           theta*S'AA'S ]
		//                                                    where E = [-I  0]
		//                                                              [ 0  I]
		//       On exit wn is unchanged.
		
		//     iprint is an INTEGER variable that must be set by the user.
		//       It controls the frequency and type of output generated:
		//        iprint<0    no output is generated;
		//        iprint=0    print only one line at the last iteration;
		//        0<iprint<99 print also f and |proj g| every iprint iterations;
		//        iprint=99   print details of every iteration except n-vectors;
		//        iprint=100  print also the changes of active set and final x;
		//        iprint>100  print details of every iteration including x and g;
		//       When iprint > 0, the file iterate.dat will be created to
		//                        summarize the iteration.
		
		//     info is an integer variable.
		//       On entry info is unspecified.
		//       On exit info = 0       for normal return,
		//                    = nonzero for abnormal return 
		//                                  when the matrix K is ill-conditioned.
		
		//     Subprograms called:
		
		//       Linpack dtrsl.
		
		
		//     References:
		
		//       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
		//       memory algorithm for bound constrained optimization'',
		//       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
		
		
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     ************

		      int          pointr,m2,col2,ibd,jy,js,i,j,k;
		      double alpha, xk, dk, temp1, temp2; 
		      final double one = 1.0;
		      final double zero = 0.0;
		
		      double dd_p;

		      if (nsub <= 0) return;
		      if (iprint >= 99) {
		    	  System.out.println("subsm entered");
		      }

		//     Compute wv = W'Zd.

		      pointr = head; 
		      for (i = 0; i < col; i++) {
		         temp1 = zero;
		         temp2 = zero;
		         for (j = 0; j < nsub; j++) {
		            k = ind[j];
		            temp1 = temp1 + wy[k-1][pointr-1]*d[j];
		            temp2 = temp2 + ws[k-1][pointr-1]*d[j];
		         } // for (j = 0; j < nsub; j++)
		         wv[i] = temp1;
		         wv[col + i] = theta*temp2;
		         pointr = (pointr%m) + 1;
		      } // for (i = 0; i < col; i++)
		 
		//     Compute wv:=K^(-1)wv.

		      m2 = 2*m;
		      col2 = 2*col;
		      dtrsl(wn,m2,col2,wv,11,info);
		      if (info[0] != 0) return;
		      for (i = 0; i < col; i++) {
		         wv[i] = -wv[i];
		      }
		      dtrsl(wn,m2,col2,wv,01,info);
		      if (info[0] != 0) return;
		 
		//     Compute d = (1/theta)d + (1/theta**2)Z'W wv.
		 
		      pointr = head;
		      for (jy = 1; jy <= col; jy++) {
		         js = col + jy;
		         for (i = 0; i < nsub; i++) {
		            k = ind[i];
		            d[i] = d[i] + wy[k-1][pointr-1]*wv[jy-1]/theta     
		                        + ws[k-1][pointr-1]*wv[js-1];
		         } // for (i = 0; i < nsub; i++) 
		         pointr = (pointr%m) + 1;
		      } // for (jy = 1; jy <= col; jy++)

		      for (i = 0; i < nsub; i++) {
		    	  d[i] = (one/theta)*d[i];
		      }
		 
		// -----------------------------------------------------------------
		//     Let us try the projection, d is the Newton direction

		      iword[0] = 0;

		      for (i = 0; i < n; i++) {
		    	  xp[i] = x[i];
		      }
		
		      for (i = 0; i < nsub; i++) {
		         k  = ind[i];
		         dk = d[i];
		         xk = x[k-1];
		         if ( nbd[k-1] != 0 ) {
		
		            if ( nbd[k-1] == 1 ) {          // lower bounds only
		               x[k-1] = Math.max( l[k-1], xk + dk );
		               if ( x[k-1] == l[k-1] ) iword[0] = 1;
		            }
		            else {
		     
		               if ( nbd[k-1] == 2 ) {      // upper and lower bounds
		                  xk   = Math.max( l[k-1], xk + dk ); 
		                  x[k-1] = Math.min( u[k-1], xk );
		                  if (( x[k-1] == l[k-1]) || (x[k-1] == u[k-1]) ) iword[0] = 1;
		               }
		               else {
		
		                  if ( nbd[k-1] == 3 ) {    // upper bounds only
		                     x[k-1] = Math.min( u[k-1], xk + dk );
		                     if ( x[k-1] == u[k-1] ) iword[0] = 1;
		                  }
		               }
		            }
		         } // if ( nbd[k-1] != 0 )   
		         else {                               // free variables
		            x[k-1] = xk + dk;
		         }
		      } // for (i = 0; i < nsub; i++)
		
		      if ( iword[0] == 0 ) {
		    	  if (iprint >= 99) {
			    	  System.out.println("Exit subsm");
			      }

			      return;
		      }
		
		//     check sign of the directional derivative
		
		      dd_p = zero;
		      for (i = 0; i < n; i++) {
		         dd_p  = dd_p + (x[i] - xx[i])*gg[i];
		      }
		      if ( dd_p > zero ) {
		    	 for (i = 0; i < n; i++) {
		    		 x[i] = xp[i];
		    	 }
		         System.out.println("Positive dir derivative in projection");
		         System.out.println("Using the backtracking step");
		      }
		      else {
		    	  if (iprint >= 99) {
			    	  System.out.println("Exit subsm");
			      }

			      return;   
		      }
		
		// -----------------------------------------------------------------
		
		      alpha = one;
		      temp1 = alpha;
		      ibd   = -1; 
		      for (i = 0; i < nsub; i++) {
		         k = ind[i];
		         dk = d[i];
		         if (nbd[k-1] != 0) {
		            if ((dk < zero) && (nbd[k-1] <= 2)) {
		               temp2 = l[k-1] - x[k-1];
		               if (temp2 >= zero) {
		                  temp1 = zero;
		               }
		               else if (dk*alpha < temp2) {
		                  temp1 = temp2/dk;
		               }
		            }
		            else if ((dk > zero) && (nbd[k-1] >= 2)) {
		               temp2 = u[k-1] - x[k-1];
		               if (temp2 <= zero) {
		                  temp1 = zero;
		               }
		               else if (dk*alpha > temp2) {
		                  temp1 = temp2/dk;
		               }
		            }
		            if (temp1 < alpha) {
		               alpha = temp1;
		               ibd = i;
		            }
		         } // if (nbd[k-1] != 0)
		      } // for (i = 0; i < nsub; i++)
		      
		      if (alpha < one) {
		         dk = d[ibd];
		         k = ind[ibd];
		         if (dk > zero) {
		            x[k-1] = u[k-1];
		            d[ibd] = zero;
		         }
		         else if (dk < zero) {
		            x[k-1] = l[k-1];
		            d[ibd] = zero;
		         }
		      } // if (alpha < one)
		      for (i = 0; i < nsub; i++) {
		         k    = ind[i];
		         x[k-1] = x[k-1] + alpha*d[i];
		      } // for (i = 0; i < nsub; i++)

		      if (iprint >= 99) {
		    	  System.out.println("Exit subsm");
		      }

		      return;
	}

     private void lnsrlb(int n, double l[], double u[], int nbd[], double x[], double f[],
    		 double fold[], double gd[], double gdold[], double g[], double d[], double r[], double t[],
		     double z[], double stp[], double dnorm[], double dtd[], double xstep[], 
		     double stpmx[], int iter, int ifun[], int iback[], int nfgv[], int info[], String task[],
		     boolean boxed, boolean cnstnd, String csave[], int isave[], double dsave[]) {

		      // character*60     task, csave
		      // logical          boxed, cnstnd
		      // integer          n, iter, ifun, iback, nfgv, info,
		       //                 nbd(n), isave(2)
		      // double precision f, fold, gd, gdold, stp, dnorm, dtd, xstep,
		     //                 stpmx, x(n), l(n), u(n), g(n), d(n), r(n), t(n),
		     //                 z(n), dsave(13)
		//     **********
		
		//     Subroutine lnsrlb
		
		//     This subroutine calls subroutine dcsrch from the Minpack2 library
		//       to perform the line search.  Subroutine dscrch is safeguarded so
		//       that all trial points lie within the feasible region.
		
		//     Subprograms called:
		
		//       Minpack2 Library ... dcsrch.
		
		//       Linpack ... dtrsl, ddot.
		
		
		//                           *  *  *
		
		//     NEOS, November 1994. (Latest revision June 1996.)
		//     Optimization Technology Center.
		//     Argonne National Laboratory and Northwestern University.
		//     Written by
		//                        Ciyou Zhu
		//     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
		
		
		//     **********

		      int          i;
		      double       ddot,a1,a2;
		      final double one = 1.0;
		      final double zero = 0.0;
		      final double big = 1.0E10;
		      final double ftol = 1.0E-3;
		      final double gtol = 0.9;
		      final double xtol = 0.1;
		 /*

		      if (task(1:5) .eq. 'FG_LN') goto 556

		      dtd = ddot(n,d,1,d,1)
		      dnorm = sqrt(dtd)

		c     Determine the maximum step length.

		      stpmx = big
		      if (cnstnd) then
		         if (iter .eq. 0) then
		            stpmx = one
		         else
		            do 43 i = 1, n
		               a1 = d(i)
		               if (nbd(i) .ne. 0) then
		                  if (a1 .lt. zero .and. nbd(i) .le. 2) then
		                     a2 = l(i) - x(i)
		                     if (a2 .ge. zero) then
		                        stpmx = zero
		                     else if (a1*stpmx .lt. a2) then
		                        stpmx = a2/a1
		                     endif
		                  else if (a1 .gt. zero .and. nbd(i) .ge. 2) then
		                     a2 = u(i) - x(i)
		                     if (a2 .le. zero) then
		                        stpmx = zero
		                     else if (a1*stpmx .gt. a2) then
		                        stpmx = a2/a1
		                     endif
		                  endif
		               endif
		  43        continue
		         endif
		      endif
		 
		      if (iter .eq. 0 .and. .not. boxed) then
		         stp = min(one/dnorm, stpmx)
		      else
		         stp = one
		      endif 

		      call dcopy(n,x,1,t,1)
		      call dcopy(n,g,1,r,1)
		      fold = f
		      ifun = 0
		      iback = 0
		      csave = 'START'
		 556  continue
		      gd = ddot(n,g,1,d,1)
		      if (ifun .eq. 0) then
		         gdold=gd
		         if (gd .ge. zero) then
		c                               the directional derivative >=0.
		c                               Line search is impossible.
		            write(6,*)' ascent direction in projection gd = ', gd
		            info = -4
		            return
		         endif
		      endif

		      call dcsrch(f,gd,stp,ftol,gtol,xtol,zero,stpmx,csave,isave,dsave)

		      xstep = stp*dnorm
		      if (csave(1:4) .ne. 'CONV' .and. csave(1:4) .ne. 'WARN') then
		         task = 'FG_LNSRCH'
		         ifun = ifun + 1
		         nfgv = nfgv + 1
		         iback = ifun - 1 
		         if (stp .eq. one) then
		            call dcopy(n,z,1,x,1)
		         else
		            do 41 i = 1, n
		               x(i) = stp*d(i) + t(i)
		  41        continue
		         endif
		      else
		         task = 'NEW_X'
		      endif
		      */

		      return;
	}

	
}


