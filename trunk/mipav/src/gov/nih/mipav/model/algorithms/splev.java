package gov.nih.mipav.model.algorithms;

public class splev {
	// ported from scipy package
	/**
	Copyright (c) 2001-2002 Enthought, Inc. 2003-2022, SciPy Developers.
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions
	are met:

	1. Redistributions of source code must retain the above copyright
	   notice, this list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above
	   copyright notice, this list of conditions and the following
	   disclaimer in the documentation and/or other materials provided
	   with the distribution.

	3. Neither the name of the copyright holder nor the names of its
	   contributors may be used to endorse or promote products derived
	   from this software without specific prior written permission.

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
	*/
	private double t[];
	private int n;
	private double c[];
	private int k;
	private double x[];
	private double y[];
	private int m;
	private int e;
	private int ier[];
	
	public splev(double t[], int n, double c[], int k, double x[],
			double y[], int m, int e, int ier[]) {
	    this.t = t;
	    this.n = n;
	    this.c = c;
	    this.k = k;
	    this.x = x;
	    this.y = y;
	    this.m = m;
	    this.e = e;
	    this.ier = ier;
	}
	
	public void run() {
		//  subroutine splev evaluates in a number of points x(i),i=1,2,...,m
		//  a spline s(x) of degree k, given in its b-spline representation.
		
		//  calling sequence:
		//     call splev(t,n,c,k,x,y,m,e,ier)
		
		//  input parameters:
		//    t    : array,length n, which contains the position of the knots.
		//    n    : integer, giving the total number of knots of s(x).
		//    c    : array,length n, which contains the b-spline coefficients.
		//    k    : integer, giving the degree of s(x).
		//    x    : array,length m, which contains the points where s(x) must
		//           be evaluated.
		//    m    : integer, giving the number of points where s(x) must be
		//           evaluated.
		//    e    : integer, if 0 the spline is extrapolated from the end
		//           spans for points not in the support, if 1 the spline
		//           evaluates to zero for those points, if 2 ier is set to
		//           1 and the subroutine returns, and if 3 the spline evaluates
		//           to the value of the nearest boundary point.
		
		//  output parameter:
		//    y    : array,length m, giving the value of s(x) at the different
		//           points.
		//    ier  : error flag
		//      ier = 0 : normal return
		//      ier = 1 : argument out of bounds and e == 2
		//      ier =10 : invalid input data (see restrictions)
		
		//  restrictions:
		//    m >= 1
		//--    t(k+1) <= x(i) <= x(i+1) <= t(n-k) , i=1,2,...,m-1.
		
		//  other subroutines required: fpbspl.
		
		//  references :
		//    de boor c  : on calculating with b-splines, j. approximation theory
		//                 6 (1972) 50-62.
		//    cox m.g.   : the numerical evaluation of b-splines, j. inst. maths
		//                 applics 10 (1972) 134-149.
		//    dierckx p. : curve and surface fitting with splines, monographs on
		//                 numerical analysis, oxford university press, 1993.
		
		//  author :
		//    p.dierckx
		//    dept. computer science, k.u.leuven
		//    celestijnenlaan 200a, b-3001 heverlee, belgium.
		//    e-mail : Paul.Dierckx@cs.kuleuven.ac.be
		
		//  latest update : march 1987
		
		//++ pearu: 11 aug 2003
		//++   - disabled cliping x values to interval [min(t),max(t)]
		//++   - removed the restriction of the orderness of x values
		//++   - fixed initialization of sp to double precision value
		
		//  ..scalar arguments..
		//      integer n, k, m, e, ier
		//  ..array arguments..
		//      real*8 t(n), c(n), x(m), y(m)
		//  ..local scalars..
		      int i, j, k1, l, ll, l1, nk1;
		//++..
		      int k2;
		//..++
		      double arg, sp, tb, te;
		//  ..local array..
		      double h[] = new double[20];
		
		//  before starting computations a data check is made. if the input data
		//  are invalid control is immediately repassed to the calling program.
		      ier[0] = 10;
		//--      if(m-1) 100,30,10
		//++..
		      if (m < 1) {
		    	  return;
		      }
		//..++
		//--  10  do 20 i=2,m
		//--        if(x(i).lt.x(i-1)) go to 100
		//--  20  continue
		      ier[0] = 0;
		//  fetch tb and te, the boundaries of the approximation interval.
		      k1 = k + 1;
		//++..
		      k2 = k1 + 1;
		//..++
		      nk1 = n - k1;
		      tb = t[k1-1];
		      te = t[nk1];
		      l = k1;
		      l1 = l + 1;
		//  main loop for the different points.
		      for (i = 1; i <=  m; i++) {
		//  fetch a new x-value arg.
		        arg = x[i-1];
		//  check if arg is in the support
		        if (((arg < tb) || (arg > te)) && (e != 0)) {
		            if (e == 1) {
		                y[i-1] = 0;
		                continue;
		            }
		            else if (e == 2) {
		                ier[0] = 1;
		                return;
		            }
		            else if (e == 3) {
		                if (arg < tb) {
		                    arg = tb;
		                }
		                else {
		                    arg = te;
		                }
		            }
		        } // if (((arg < tb) || (arg > te)) && (e != 0))
		//  search for knot interval t(l) <= arg < t(l+1)
		//++..
		        while (!((arg >= t[l-1]) || (l1 == k2))) {
		            l1 = l;
		            l = l - 1;    
		        }
		//..++
		        while (!((arg < t[l1-1]) || (l == nk1))) {
		            l = l1;
		            l1 = l + 1;
		        }
		//  evaluate the non-zero b-splines at arg.
		        fpbspl(t, n, k, arg, l, h);
		//  find the value of s(x) at x=arg.
		        sp = 0.0;
		        ll = l - k1;
		        for (j = 1; j <= k1; j++) {
		              ll = ll + 1;
		              sp = sp + c[ll-1]*h[j-1];
		        } // for (j = 1; j <= k1; j++)
		        y[i-1] = sp;
		      } // for (i = 1; i <=  m; i++)
		      return;
	}
	
	public void fpbspl(double t[],int n, int k, double x,int l,double h[]) {
		//  subroutine fpbspl evaluates the (k+1) non-zero b-splines of
		//  degree k at t(l) <= x < t(l+1) using the stable recurrence
		//  relation of de boor and cox.
		//  Travis Oliphant  2007
		//    changed so that weighting of 0 is used when knots with
		//      multiplicity are present.
		//    Also, notice that l+k <= n and 1 <= l+1-k
		//      or else the routine will be accessing memory outside t
		//      Thus it is imperative that that k <= l <= n-k but this
		//      is not checked.
	
		//  ..scalar arguments..
		//      real*8 x
		//      integer n,k,l
		//  ..array arguments..
		//      real*8 t(n),h(20)
		//  ..local scalars..
		      double f,one;
		      int i,j,li,lj;
		//  ..local arrays..
		      double hh[] = new double[19];
		
		      one = 0.1d+01;
		      h[0] = one;
		      for (j = 1; j <= k; j++) {
		        for  (i = 1; i <= j; i++) {
		          hh[i-1] = h[i-1];
		        }
		        h[0] = 0.0;
		        for (i = 1; i <= j; i++) {
		          li = l+i;
		          lj = li-j;
		          if (t[li-1] == t[lj-1]) {
		              h[i] = 0.0; 
		          }
		          else {
		              f = hh[i-1]/(t[li-1]-t[lj-1]); 
		              h[i-1] = h[i-1]+f*(t[li-1]-x);
		              h[i] = f*(x-t[lj-1]);
		          }
		        } // for (i = 1; i <= j; i++)
		      } // for (j = 1; j <= k; j++);
		      return;
    }




}