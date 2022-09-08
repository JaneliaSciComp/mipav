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
		      double wa[] = new double[8*m];
		      int index[] = new int[n];
		      int iwhere[] = new int[n];
		      int indx2[] = new int[n];
		      

		      mainlb(n,m,x,l,u,nbd,f,g,factr,pgtol,
		             ws,wy,sy,ss,wt,
		             wn,snd,z,r,d,t,xp,wa,
		             index, iwhere,indx2,task,iprint, 
		             csave,lsave,isave,dsave);

		      return;

	}
	
	private void mainlb(int n, int m, double x[], double l[], double u[], 
			int nbd[], double f[], double g[], double factr, double pgtol, double ws[][], double wy[][],
		     double sy[][], double ss[][], double wt[][], double wn[][], double snd[][], 
		     double z[], double r[], double d[], double t[], double xp[], double wa[], 
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
		 
		      boolean          updatd,wrk;
		      boolean prjctd[] = new boolean[1];
		      boolean cnstnd[] = new boolean[1];
		      boolean boxed[] = new boolean[1];
		      //character*3      word
		      String word[] = new String[1];
		      int          i,nintol,itfile,iback,nskip,
		                      head,col,iter,itail,iupdat,
		                      nseg,nfgv,ifun,
		                      iword,nfree,nact,ileave,nenter;
		      int info[] = new int[1];
		      int k[] = new int[1];
		      double theta,fold,ddot,dr,rr,tol,
		                      ddum,dnorm,dtd,epsmch,
		                      cpu2,cachyt,sbtime,lnscht,time2,
		                      gd,gdold,stp,stpmx,time;
		      long cpu1;
		      double sbgnrm[] = new double[1];
		      double xstep = 0.0;
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
		         iback  = 0;
		         itail  = 0;
		         iword  = 0;
		         nact   = 0;
		         ileave = 0;
		         nenter = 0;
		         fold   = zero;
		         dnorm  = zero;
		         cpu1   = 0L;
		         gd     = zero;
		         stpmx  = zero;
		         sbgnrm[0] = zero;
		         stp    = zero;
		         gdold  = zero;
		         dtd    = zero;

		//           for operation counts:
		         iter   = 0;
		         nfgv   = 0;
		         nseg   = 0;
		         nintol = 0;
		         nskip  = 0;
		         nfree  = n;
		         ifun   = 0;
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
		                       iter,nfgv,nintol,nskip,nact,sbgnrm[0],
		                       0.0,nseg,word[0],iback,stp,xstep,k[0],
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
		    	           iback  = isave[3];
		    	           nskip  = isave[4];
		    	           head   = isave[5];
		    	           col    = isave[6];
		    	           itail  = isave[7];
		    	           iter   = isave[8];
		    	           iupdat = isave[9];
		    	           nseg   = isave[11];
		    	           nfgv   = isave[12];
		    	           info[0]   = isave[13];
		    	           ifun   = isave[14];
		    	           iword  = isave[15];
		    	           nfree  = isave[16];
		    	           nact   = isave[17];
		    	           ileave = isave[18];
		    	           nenter = isave[19];

		    	           theta  = dsave[0];
		    	           fold   = dsave[1];
		    	           tol    = dsave[2];
		    	           dnorm  = dsave[3];
		    	           epsmch = dsave[4];
		    	           cpu1   = (long)dsave[5];
		    	           cachyt = dsave[6];
		    	           sbtime = dsave[7];
		    	           lnscht = dsave[8];
		    	           time1  = (long)dsave[9];
		    	           gd     = dsave[10];
		    	           stpmx  = dsave[11];
		    	           sbgnrm[0] = dsave[12];
		    	           stp    = dsave[13];
		    	           gdold  = dsave[14];
		    	           dtd    = dsave[15];
		    	     
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
		    	                 f[0] = fold;
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
		      nfgv = 1;
		 
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
		      if (iprint >+ 99) {
		    	  System.out.println("Iteration = " + iter);
		      }
		      iword = -1;
		
		      if ((!cnstnd[0]) && (col > 0)) { 
		//                                            skip the search for GCP.
		         for (i = 0; i < n; i++) {
		        	 z[i] = x[i];
		         }
		         wrk = updatd;
		         nseg = 0;
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
		      call cauchy(n,x,l,u,nbd,g,indx2,iwhere,t,d,z,
		     +            m,wy,ws,sy,wt,theta,col,head,
		     +            wa(1),wa(2*m+1),wa(4*m+1),wa(6*m+1),nseg,
		     +            iprint, sbgnrm, info, epsmch)
		      if (info .ne. 0) then 
		c         singular triangular system detected; refresh the lbfgs memory.
		         if(iprint .ge. 1) write (6, 1005)
		         info   = 0
		         col    = 0
		         head   = 1
		         theta  = one
		         iupdat = 0
		         updatd = .false.
		         call timer(cpu2) 
		         cachyt = cachyt + cpu2 - cpu1
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
		      call timer(cpu2) 
		      cachyt = cachyt + cpu2 - cpu1
		      nintol = nintol + nseg

		c     Count the entering and leaving variables for iter > 0; 
		c     find the index set of free and active variables at the GCP.

		      call freev(n,nfree,index,nenter,ileave,indx2,
		     +           iwhere,wrk,updatd,cnstnd,iprint,iter)
		      nact = n - nfree
			} // if (do222)

		 if (do333) {
		 
		c     If there are no free variables or B=theta*I, then
		c                                        skip the subspace minimization.
		 
		      if (nfree .eq. 0 .or. col .eq. 0) goto 555
		 
		cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		c
		c     Subspace minimization.
		c
		cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

		      call timer(cpu1) 

		c     Form  the LEL^T factorization of the indefinite
		c       matrix    K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
		c                     [L_a -R_z           theta*S'AA'S ]
		c       where     E = [-I  0]
		c                     [ 0  I]

		      if (wrk) call formk(n,nfree,index,nenter,ileave,indx2,iupdat,
		     +                 updatd,wn,snd,m,ws,wy,sy,theta,col,head,info)
		      if (info .ne. 0) then
		c          nonpositive definiteness in Cholesky factorization;
		c          refresh the lbfgs memory and restart the iteration.
		         if(iprint .ge. 1) write (6, 1006)
		         info   = 0
		         col    = 0
		         head   = 1
		         theta  = one
		         iupdat = 0
		         updatd = .false.
		         call timer(cpu2) 
		         sbtime = sbtime + cpu2 - cpu1 
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

		c        compute r=-Z'B(xcp-xk)-Z'g (using wa(2m+1)=W'(xcp-x)
		c                                                   from 'cauchy').
		      call cmprlb(n,m,x,g,ws,wy,sy,wt,z,r,wa,index,
		     +           theta,col,head,nfree,cnstnd,info)
		      if (info .ne. 0) goto 444

		c-jlm-jn   call the direct method. 

		      call subsm( n, m, nfree, index, l, u, nbd, z, r, xp, ws, wy,
		     +           theta, x, g, col, head, iword, wa, wn, iprint, info)
		 } // if (do333)
		 if (do444) {
		      if (info .ne. 0) then 
		c          singular triangular system detected;
		c          refresh the lbfgs memory and restart the iteration.
		         if(iprint .ge. 1) write (6, 1005)
		         info   = 0
		         col    = 0
		         head   = 1
		         theta  = one
		         iupdat = 0
		         updatd = .false.
		         call timer(cpu2) 
		         sbtime = sbtime + cpu2 - cpu1 
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
		 
		      call timer(cpu2) 
		      sbtime = sbtime + cpu2 - cpu1 
		 } // if (do444)
		 if (do555) {
		 
		cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		c
		c     Line search and optimality tests.
		c
		cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		 
		c     Generate the search direction d:=z-x.

		      do 40 i = 1, n
		         d(i) = z(i) - x(i)
		  40  continue
		      call timer(cpu1) 
		 } // if (do555)
		 if (do666) {
		      call lnsrlb(n,l,u,nbd,x,f,fold,gd,gdold,g,d,r,t,z,stp,dnorm,
		     +            dtd,xstep,stpmx,iter,ifun,iback,nfgv,info,task,
		     +            boxed,cnstnd,csave,isave(22),dsave(17))
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
	    final double one = 1.0;
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

	
}