package gov.nih.mipav.model.algorithms;

public class curev {
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
	
	private int idim;
	private double t[];
	private int n;
	private double c[];
	private int nc;
	private int k;
	private double u[];
	private int m;
	private double x[];
	private int mx;
	private int ier[];
	
	public curev(int idim, double t[], int n, double c[], int nc, int k,
			     double u[], int m, double x[], int mx, int ier[]) {
	    this.idim = idim;
	    this.t = t;
	    this.n = n;
	    this.c = c;
	    this.nc = nc;
	    this.k = k;
	    this.u = u;
	    this.m = m;
	    this.x = x;
	    this.mx = mx;
	    this.ier = ier;
	}
	
	public void run() {
		// implicit none
		//  subroutine curev evaluates in a number of points u(i),i=1,2,...,m
		//  a spline curve s(u) of degree k and dimension idim, given in its
		//  b-spline representation.
		
		//  calling sequence:
		//     call curev(idim,t,n,c,nc,k,u,m,x,mx,ier)
		
		//  input parameters:
		//    idim : integer, giving the dimension of the spline curve.
		//    t    : array,length n, which contains the position of the knots.
		//    n    : integer, giving the total number of knots of s(u).
		//    c    : array,length nc, which contains the b-spline coefficients.
		//    nc   : integer, giving the total number of coefficients of s(u).
		//    k    : integer, giving the degree of s(u).
		//    u    : array,length m, which contains the points where s(u) must
		//           be evaluated.
		//    m    : integer, giving the number of points where s(u) must be
		//           evaluated.
		//    mx   : integer, giving the dimension of the array x. mx >= m*idim
		
		//  output parameters:
		//    x    : array,length mx,giving the value of s(u) at the different
		//           points. x(idim*(i-1)+j) will contain the j-th coordinate
		//           of the i-th point on the curve.
		//    ier  : error flag
		//      ier = 0 : normal return
		//      ier =10 : invalid input data (see restrictions)
		
		//  restrictions:
		//    m >= 1
		//    mx >= m*idim
		//    t(k+1) <= u(i) <= u(i+1) <= t(n-k) , i=1,2,...,m-1.
		
		//  other subroutines required: fpbspl.
		
		//  references :
		//    de boor c : on calculating with b-splines, j. approximation theory
		//                6 (1972) 50-62.
		//    cox m.g.  : the numerical evaluation of b-splines, j. inst. maths
		//                applics 10 (1972) 134-149.
		//    dierckx p. : curve and surface fitting with splines, monographs on
		//                 numerical analysis, oxford university press, 1993.
		
		//  author :
		//    p.dierckx
		//    dept. computer science, k.u.leuven
		//    celestijnenlaan 200a, b-3001 heverlee, belgium.
		//    e-mail : Paul.Dierckx@cs.kuleuven.ac.be
		
		//  latest update : march 1987
		
		//  ..scalar arguments..
		//      integer idim,n,nc,k,m,mx,ier
		//  ..array arguments..
		//      real*8 t(n),c(nc),u(m),x(mx)
		//  ..local scalars..
		      int i,j,jj,j1,k1,l,ll,l1,mm,nk1;
		      double arg,sp,tb,te;
		//  ..local array..
		      double h[] = new double[6];
		//  ..
		//  before starting computations a data check is made. if the input data
		//  are invalid control is immediately repassed to the calling program.
		      ier[0] = 10;
		      if (m < 1) {
		    	  return;
		      }
		      if (m != 1) {
		          for (i = 1; i <= m-1; i++) {
		              if (u[i] < u[i-1]) {
		            	  return;
		              }
		          }
		      } // if (m != 1)
		      if(mx < (m*idim)) {
		    	  return;
		      }
		      ier[0] = 0;
		//  fetch tb and te, the boundaries of the approximation interval.
		      k1 = k+1;
		      nk1 = n-k1;
		      tb = t[k1-1];
		      te = t[nk1];
		      l = k1;
		      l1 = l+1;
		//  main loop for the different points.
		      mm = 0;
		      for (i = 0; i <= m-1; i++) {
		//  fetch a new u-value arg.
		        arg = u[i];
		        if(arg < tb) arg = tb;
		        if(arg > te) arg = te;
		//  search for knot interval t(l) <= arg < t(l+1)
		      while (!(arg < t[l1-1] || l == nk1)) {
		        l = l1;
		        l1 = l+1;
		      }
		//  evaluate the non-zero b-splines at arg.
		      fpbspl(t,n,k,arg,l,h);
		//  find the value of s(u) at u=arg.
		        ll = l-k1;
		        for (j1=1; j1 <= idim; j1++) {
		          jj = ll;
		          sp = 0.0;
		          for (j = 1; j <= k1; j++) {
		            jj = jj+1;
		            sp = sp+c[jj-1]*h[j-1];
		          } // for (j = 1; j <= k1; j++)
		          mm = mm+1;
		          x[mm-1] = sp;
		          ll = ll+n;
		        } // for (j1=1; j1 <= idim; j1++)
		      } // for (i = 0; i <= m-1; i++)
		      return;	
	}
	
	public void fpbspl(double t[], int n,int k,double x,int l, double h[]) {
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
	           for (j=1; j <= k; j++) {
	             for (i=1; i <= j; i++) {
	               hh[i-1] = h[i-1];
	             }
	             h[0] = 0.0;
	             for (i=1; i <= j; i++) {
	               li = l+i;
	               lj = li-j;
	               if (t[li-1] == t[lj-1]) {
	                   h[i] = 0.0; 
	               }
	               else {
		               f = hh[i-1]/(t[li-1]-t[lj-1]); 
		               h[i-1] = h[i-1]+f*(t[li-1]-x);
		               h[i] = f*(x-t[lj-1]);
	               } // else 
	             } // for (i=1; i <= j; i++)
	           } //  for (j=1; j <= k; j++)
	           return;
    }
}