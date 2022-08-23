package gov.nih.mipav.model.algorithms;

public class curfit {
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
	private int iopt;
	private int m;
	private double x[];
	private double y[];
	private double w[];
	private double xb;
	private double xe;
	private int k;
	private double s;
	private int nest;
	private int n[];
	private double t[];
	private double c[];
	private double fp[];
	private int lwrk;
	private int iwrk[];
	private int ier[];
	private double wrk[][][];
	
     public curfit(int iopt,int m, double x[], double y[], double w[],
    		 double xb,double xe, int k, double s, int nest,int n[],
		     double t[], double c[], double fp[], int lwrk,
		     int iwrk[], int ier[]) {
    	 this.iopt = iopt;
    	 this.m = m;
    	 this.x = x;
    	 this.y = y;
    	 this.w = w;
    	 this.xb = xb;
    	 this.xe = xe;
    	 this.k = k;
    	 this.s = s;
    	 this.nest = nest;
    	 this.n = n;
    	 this.t = t;
    	 this.c = c;
    	 this.fp = fp;
    	 this.lwrk = lwrk;
    	 this.iwrk = iwrk;
    	 this.ier = ier;
    	 wrk = new double[6][][];
     }
     
     public void run() {
    	 /**
		      implicit none
		c  given the set of data points (x(i),y(i)) and the set of positive
		c  numbers w(i),i=1,2,...,m,subroutine curfit determines a smooth spline
		c  approximation of degree k on the interval xb <= x <= xe.
		c  if iopt=-1 curfit calculates the weighted least-squares spline
		c  according to a given set of knots.
		c  if iopt>=0 the number of knots of the spline s(x) and the position
		c  t(j),j=1,2,...,n is chosen automatically by the routine. the smooth-
		c  ness of s(x) is then achieved by minimalizing the discontinuity
		c  jumps of the k-th derivative of s(x) at the knots t(j),j=k+2,k+3,...,
		c  n-k-1. the amount of smoothness is determined by the condition that
		c  f(p)=sum((w(i)*(y(i)-s(x(i))))**2) be <= s, with s a given non-
		c  negative constant, called the smoothing factor.
		c  the fit s(x) is given in the b-spline representation (b-spline coef-
		c  ficients c(j),j=1,2,...,n-k-1) and can be evaluated by means of
		c  subroutine splev.
		c
		c  calling sequence:
		c     call curfit(iopt,m,x,y,w,xb,xe,k,s,nest,n,t,c,fp,wrk,
		c    * lwrk,iwrk,ier)
		c
		c  parameters:
		c   iopt  : integer flag. on entry iopt must specify whether a weighted
		c           least-squares spline (iopt=-1) or a smoothing spline (iopt=
		c           0 or 1) must be determined. if iopt=0 the routine will start
		c           with an initial set of knots t(i)=xb, t(i+k+1)=xe, i=1,2,...
		c           k+1. if iopt=1 the routine will continue with the knots
		c           found at the last call of the routine.
		c           attention: a call with iopt=1 must always be immediately
		c           preceded by another call with iopt=1 or iopt=0.
		c           unchanged on exit.
		c   m     : integer. on entry m must specify the number of data points.
		c           m > k. unchanged on exit.
		c   x     : real array of dimension at least (m). before entry, x(i)
		c           must be set to the i-th value of the independent variable x,
		c           for i=1,2,...,m. these values must be supplied in strictly
		c           ascending order. unchanged on exit.
		c   y     : real array of dimension at least (m). before entry, y(i)
		c           must be set to the i-th value of the dependent variable y,
		c           for i=1,2,...,m. unchanged on exit.
		c   w     : real array of dimension at least (m). before entry, w(i)
		c           must be set to the i-th value in the set of weights. the
		c           w(i) must be strictly positive. unchanged on exit.
		c           see also further comments.
		c   xb,xe : real values. on entry xb and xe must specify the boundaries
		c           of the approximation interval. xb<=x(1), xe>=x(m).
		c           unchanged on exit.
		c   k     : integer. on entry k must specify the degree of the spline.
		c           1<=k<=5. it is recommended to use cubic splines (k=3).
		c           the user is strongly dissuaded from choosing k even,together
		c           with a small s-value. unchanged on exit.
		c   s     : real.on entry (in case iopt>=0) s must specify the smoothing
		c           factor. s >=0. unchanged on exit.
		c           for advice on the choice of s see further comments.
		c   nest  : integer. on entry nest must contain an over-estimate of the
		c           total number of knots of the spline returned, to indicate
		c           the storage space available to the routine. nest >=2*k+2.
		c           in most practical situation nest=m/2 will be sufficient.
		c           always large enough is  nest=m+k+1, the number of knots
		c           needed for interpolation (s=0). unchanged on exit.
		c   n     : integer.
		c           unless ier =10 (in case iopt >=0), n will contain the
		c           total number of knots of the spline approximation returned.
		c           if the computation mode iopt=1 is used this value of n
		c           should be left unchanged between subsequent calls.
		c           in case iopt=-1, the value of n must be specified on entry.
		c   t     : real array of dimension at least (nest).
		c           on successful exit, this array will contain the knots of the
		c           spline,i.e. the position of the interior knots t(k+2),t(k+3)
		c           ...,t(n-k-1) as well as the position of the additional knots
		c           t(1)=t(2)=...=t(k+1)=xb and t(n-k)=...=t(n)=xe needed for
		c           the b-spline representation.
		c           if the computation mode iopt=1 is used, the values of t(1),
		c           t(2),...,t(n) should be left unchanged between subsequent
		c           calls. if the computation mode iopt=-1 is used, the values
		c           t(k+2),...,t(n-k-1) must be supplied by the user, before
		c           entry. see also the restrictions (ier=10).
		c   c     : real array of dimension at least (nest).
		c           on successful exit, this array will contain the coefficients
		c           c(1),c(2),..,c(n-k-1) in the b-spline representation of s(x)
		c   fp    : real. unless ier=10, fp contains the weighted sum of
		c           squared residuals of the spline approximation returned.
		c   wrk   : real array of dimension at least (m*(k+1)+nest*(7+3*k)).
		c           used as working space. if the computation mode iopt=1 is
		c           used, the values wrk(1),...,wrk(n) should be left unchanged
		c           between subsequent calls.
		c           use wrk[6][] at input
		c   lwrk  : integer. on entry,lwrk must specify the actual dimension of
		c           the array wrk as declared in the calling (sub)program.lwrk
		c           must not be too small (see wrk). unchanged on exit.
		c   iwrk  : integer array of dimension at least (nest).
		c           used as working space. if the computation mode iopt=1 is
		c           used,the values iwrk(1),...,iwrk(n) should be left unchanged
		c           between subsequent calls.
		c   ier   : integer. unless the routine detects an error, ier contains a
		c           non-positive value on exit, i.e.
		c    ier=0  : normal return. the spline returned has a residual sum of
		c             squares fp such that abs(fp-s)/s <= tol with tol a relat-
		c             ive tolerance set to 0.001 by the program.
		c    ier=-1 : normal return. the spline returned is an interpolating
		c             spline (fp=0).
		c    ier=-2 : normal return. the spline returned is the weighted least-
		c             squares polynomial of degree k. in this extreme case fp
		c             gives the upper bound fp0 for the smoothing factor s.
		c    ier=1  : error. the required storage space exceeds the available
		c             storage space, as specified by the parameter nest.
		c             probably causes : nest too small. if nest is already
		c             large (say nest > m/2), it may also indicate that s is
		c             too small
		c             the approximation returned is the weighted least-squares
		c             spline according to the knots t(1),t(2),...,t(n). (n=nest)
		c             the parameter fp gives the corresponding weighted sum of
		c             squared residuals (fp>s).
		c    ier=2  : error. a theoretically impossible result was found during
		c             the iteration process for finding a smoothing spline with
		c             fp = s. probably causes : s too small.
		c             there is an approximation returned but the corresponding
		c             weighted sum of squared residuals does not satisfy the
		c             condition abs(fp-s)/s < tol.
		c    ier=3  : error. the maximal number of iterations maxit (set to 20
		c             by the program) allowed for finding a smoothing spline
		c             with fp=s has been reached. probably causes : s too small
		c             there is an approximation returned but the corresponding
		c             weighted sum of squared residuals does not satisfy the
		c             condition abs(fp-s)/s < tol.
		c    ier=10 : error. on entry, the input data are controlled on validity
		c             the following restrictions must be satisfied.
		c             -1<=iopt<=1, 1<=k<=5, m>k, nest>2*k+2, w(i)>0,i=1,2,...,m
		c             xb<=x(1)<x(2)<...<x(m)<=xe, lwrk>=(k+1)*m+nest*(7+3*k)
		c             if iopt=-1: 2*k+2<=n<=min(nest,m+k+1)
		c                         xb<t(k+2)<t(k+3)<...<t(n-k-1)<xe
		c                       the schoenberg-whitney conditions, i.e. there
		c                       must be a subset of data points xx(j) such that
		c                         t(j) < xx(j) < t(j+k+1), j=1,2,...,n-k-1
		c             if iopt>=0: s>=0
		c                         if s=0 : nest >= m+k+1
		c             if one of these conditions is found to be violated,control
		c             is immediately repassed to the calling program. in that
		c             case there is no approximation returned.
		c
		c  further comments:
		c   by means of the parameter s, the user can control the tradeoff
		c   between closeness of fit and smoothness of fit of the approximation.
		c   if s is too large, the spline will be too smooth and signal will be
		c   lost ; if s is too small the spline will pick up too much noise. in
		c   the extreme cases the program will return an interpolating spline if
		c   s=0 and the weighted least-squares polynomial of degree k if s is
		c   very large. between these extremes, a properly chosen s will result
		c   in a good compromise between closeness of fit and smoothness of fit.
		c   to decide whether an approximation, corresponding to a certain s is
		c   satisfactory the user is highly recommended to inspect the fits
		c   graphically.
		c   recommended values for s depend on the weights w(i). if these are
		c   taken as 1/d(i) with d(i) an estimate of the standard deviation of
		c   y(i), a good s-value should be found in the range (m-sqrt(2*m),m+
		c   sqrt(2*m)). if nothing is known about the statistical error in y(i)
		c   each w(i) can be set equal to one and s determined by trial and
		c   error, taking account of the comments above. the best is then to
		c   start with a very large value of s ( to determine the least-squares
		c   polynomial and the corresponding upper bound fp0 for s) and then to
		c   progressively decrease the value of s ( say by a factor 10 in the
		c   beginning, i.e. s=fp0/10, fp0/100,...and more carefully as the
		c   approximation shows more detail) to obtain closer fits.
		c   to economize the search for a good s-value the program provides with
		c   different modes of computation. at the first call of the routine, or
		c   whenever he wants to restart with the initial set of knots the user
		c   must set iopt=0.
		c   if iopt=1 the program will continue with the set of knots found at
		c   the last call of the routine. this will save a lot of computation
		c   time if curfit is called repeatedly for different values of s.
		c   the number of knots of the spline returned and their location will
		c   depend on the value of s and on the complexity of the shape of the
		c   function underlying the data. but, if the computation mode iopt=1
		c   is used, the knots returned may also depend on the s-values at
		c   previous calls (if these were smaller). therefore, if after a number
		c   of trials with different s-values and iopt=1, the user can finally
		c   accept a fit as satisfactory, it may be worthwhile for him to call
		c   curfit once more with the selected value for s but now with iopt=0.
		c   indeed, curfit may then return an approximation of the same quality
		c   of fit but with fewer knots and therefore better if data reduction
		c   is also an important objective for the user.
		c
		c  other subroutines required:
		c    fpback,fpbspl,fpchec,fpcurf,fpdisc,fpgivs,fpknot,fprati,fprota
		c
		c  references:
		c   dierckx p. : an algorithm for smoothing, differentiation and integ-
		c                ration of experimental data using spline functions,
		c                j.comp.appl.maths 1 (1975) 165-184.
		c   dierckx p. : a fast algorithm for smoothing data on a rectangular
		c                grid while using spline functions, siam j.numer.anal.
		c                19 (1982) 1286-1304.
		c   dierckx p. : an improved algorithm for curve fitting with spline
		c                functions, report tw54, dept. computer science,k.u.
		c                leuven, 1981.
		c   dierckx p. : curve and surface fitting with splines, monographs on
		c                numerical analysis, oxford university press, 1993.
		c
		c  author:
		c    p.dierckx
		c    dept. computer science, k.u. leuven
		c    celestijnenlaan 200a, b-3001 heverlee, belgium.
		c    e-mail : Paul.Dierckx@cs.kuleuven.ac.be
		c
		c  creation date : may 1979
		c  latest update : march 1987
		c
		c  ..
		c  ..scalar arguments..
		      real*8 xb,xe,s,fp
		      integer iopt,m,k,nest,n,lwrk,ier
		c  ..array arguments..
		      real*8 x(m),y(m),w(m),t(nest),c(nest),wrk(lwrk)
		      integer iwrk(nest)
		*/
		// local scalars..
		      double tol;
		      // int ia, ib, ifp, ig, iq, iz;
		      int i,j,k1,k2,lwest,maxit,nmin;
		//  ..
		//  we set up the parameters tol and maxit
		      maxit = 20;
		      tol = 0.1d-02;
		//  before starting computations a data check is made. if the input data
		//  are invalid, control is immediately repassed to the calling program.
		      ier[0] = 10;
		      if((k <= 0) || (k > 5)) {
		    	  return;
		      }
		      k1 = k+1;
		      k2 = k1+1;
		      if((iopt < (-1)) || (iopt > 1)) {
		    	  return;
		      }
		      nmin = 2*k1;
		      if((m < k1) || (nest < nmin)) {
		    	  return;
		      }
		      lwest = m*k1+nest*(7+3*k);
		      if(lwrk < lwest) {
		    	  return;
		      }
		      if((xb > x[0]) || (xe < x[m-1])) {
		    	  return;
		      }
		      for  (i=1; i <= m-1; i++) {
		         if(x[i-1] > x[i]) {
		        	 return;
		         }
		      }
		      if(iopt < 0) {
			      if((n[0] < nmin) || (n[0] >nest)) {
			    	  return;
			      }
			      j = n[0]-1;
			      for (i=0; i <=k1-1; i++) {
			         t[i] = xb;
			         t[j] = xe;
			         j = j-1;
			      } // for (i=0; i <=k1-1; i++)
			      fpchec(x,m,t,n[0],k,ier);
			      if (ier[0] != 0) {
			    	  return;
			      }
		      } // if (iopt < 0)
		      else { // iopt >= 0
			      if(s < 0.0) {
			    	  return;
			      }
			      if((s == 0.0) && (nest < (m+k1))) {
			    	  return;
			      }
		      } // else iopt >= 0
		  // we partition the working space and determine the spline approximation.
		      //ifp = 1;
		      //iz = ifp+nest;
		      //ia = iz+nest;
		      //ib = ia+nest*k1;
		      //ig = ib+nest*k2;
		      //iq = ig+nest*k2;
		      wrk[0] = new double[1][nest];
		      wrk[1] = new double[1][nest];
		      wrk[2] = new double[nest][k1];
		      wrk[3] = new double[nest][k2];
		      wrk[4] = new double[nest][k2];
		      wrk[5] = new double[m][k1];
		      fpcurf(iopt,x,y,w,m,xb,xe,k,s,nest,tol,maxit,k1,k2,n,t,c,fp,
		      wrk[0][0],wrk[1][0],wrk[2],wrk[3],wrk[4],wrk[5],iwrk,ier);
		      return;
     }
     
     public void fpchec(double x[],int m,double t[],int n, int k,int ier[]) {
     /**
     implicit none
		c  subroutine fpchec verifies the number and the position of the knots
		c  t(j),j=1,2,...,n of a spline of degree k, in relation to the number
		c  and the position of the data points x(i),i=1,2,...,m. if all of the
		c  following conditions are fulfilled, the error parameter ier is set
		c  to zero. if one of the conditions is violated ier is set to ten.
		c      1) k+1 <= n-k-1 <= m
		c      2) t(1) <= t(2) <= ... <= t(k+1)
		c         t(n-k) <= t(n-k+1) <= ... <= t(n)
		c      3) t(k+1) < t(k+2) < ... < t(n-k)
		c      4) t(k+1) <= x(i) <= t(n-k)
		c      5) the conditions specified by schoenberg and whitney must hold
		c         for at least one subset of data points, i.e. there must be a
		c         subset of data points y(j) such that
		c             t(j) < y(j) < t(j+k+1), j=1,2,...,n-k-1
		c  ..
		c  ..scalar arguments..
		     integer m,n,k,ier
		c  ..array arguments..
		     real*8 x(m),t(n)
		     */
		//local scalars..
		     int i,j,k1,k2,l,nk1,nk2,nk3;
		     double tj,tl;
		
		     k1 = k+1;
		     k2 = k1+1;
		     nk1 = n-k1;
		     nk2 = nk1+1;
		     ier[0] = 10;
		//  check condition no 1
		     if((nk1 < k1) || (nk1 > m)) {
		    	 return;
		     }
		//  check condition no 2
		     j = n-1;
		     for  (i=0; i <= k-1; i++) {
		       if(t[i] > t[i+1]) {
		    	   return;
		       }
		       if(t[j] < t[j-1]) {
		    	   return;
		       }
		       j = j-1;
		     } // for  (i=0; i <= k-1; i++)
		//  check condition no 3
		     for (i=k2-1; i <= nk2-1; i++) {
		       if(t[i] <= t[i-1]) {
		    	   return;
		       }
		     } // for (i=k2-1; i <= nk2-1; i++)
		//  check condition no 4
		     if((x[0] < t[k1-1]) || (x[m-1] > t[nk2-1])) {
		    	 return;
		     }
		//  check condition no 5
		     if((x[0] >= t[k2-1]) || (x[m-1] <= t[nk1-1])) {
		    	 return;
		     }
		     i = 1;
		     l = k2;
		     nk3 = nk1-1;
		     if(nk3 < 2) {
		    	 ier[0] = 0;
		    	 return;
		     }
		     for  (j=2; j <= nk3; j++) {
		       tj = t[j-1];
		       l = l+1;
		       tl = t[l-1];
		       do {
			       i = i+1;
			       if(i >= m) {
			    	   return;
			       }
		       } while (x[i-1] <= tj);
		       if(x[i-1] >= tl) {
		    	   return;
		       }
		     } // for  (j=2; j <= nk3; j++)
		     ier[0] = 0;
		     return;
    }


     public void fpcurf(int iopt, double x[], double y[], double w[],int m,double xb,double xe,
    		 int k,double s,int nest, double tol,
    	     int maxit,int k1,int k2,int n[], double t[],double c[], double fp[], double fpint[],
    	     double z[], double a[][], double b[][], double g[][], double q[][], int nrdata[], int ier[]) {
    	 /**
    	      implicit none
    	c  ..
    	c  ..scalar arguments..
    	      real*8 xb,xe,s,tol,fp
    	      integer iopt,m,k,nest,maxit,k1,k2,n,ier
    	c  ..array arguments..
    	      real*8 x(m),y(m),w(m),t(nest),c(nest),fpint(nest),
    	     * z(nest),a(nest,k1),b(nest,k2),g(nest,k2),q(m,k1)
    	      integer nrdata(nest)
    	      */
    	// local scalars..
    	      double con1,con4,con9,half,fpart,f2,
    	     one,p,pinv,piv,p2,rn,store,term,wi,xi;
    	      double acc = 0.0;
    	      double fp0 = 0.0;
    	      double fpold = 0.0;
    	      double fpms = 0.0;
    	      double cos[] = new double[1];
    	      double sin[] = new double[1];
    	      double yi[] = new double[1];
    	      double temp[] = new double[1];
    	      double temp2[] = new double[1];
    	      double f1[] = new double[1];
    	      double f3[] = new double[1];
    	      double p1[] = new double[1];
    	      double p3[] = new double[1];
    	      double prod;
    	      int i,ich1,ich3,it,iter,i1,i2,i3,j,k3,l,l0,
    	      mk1,newi,nmin,npl1,n8;
    	      int nk1 = 0;
    	      int nmax = 0;
    	      int nplus = 0;
    	      int nrint[] = new int[1];
    	//  ..local arrays..
    	      double h[] = new double[7];
    	      boolean do50 = true;
    	      boolean loopback = false;
    	//  ..function references
    	//      double abs,fprati
    	//      integer max0,min0
    	//  ..subroutine references..
    	//    fpback,fpbspl,fpgivs,fpdisc,fpknot,fprota
    
    	//  set constants
    	      one = 0.1d+01;
    	      con1 = 0.1;
    	      con9 = 0.9;
    	      con4 = 0.4d-01;
    	      half = 0.5;
    	//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    	//  part 1: determination of the number of knots and their position     c
    	//  **************************************************************      c
    	//  given a set of knots we compute the least-squares spline sinf(x),   c
    	//  and the corresponding sum of squared residuals fp=f(p=inf).         c
    	//  if iopt=-1 sinf(x) is the requested approximation.                  c
    	//  if iopt=0 or iopt=1 we check whether we can accept the knots:       c
    	//    if fp <=s we will continue with the current set of knots.         c
    	//    if fp > s we will increase the number of knots and compute the    c
    	//       corresponding least-squares spline until finally fp<=s.        c
    	//    the initial choice of knots depends on the value of s and iopt.   c
    	//    if s=0 we have spline interpolation; in that case the number of   c
    	//    knots equals nmax = m+k+1.                                        c
    	//    if s > 0 and                                                      c
    	//      iopt=0 we first compute the least-squares polynomial of         c
    	//      degree k; n = nmin = 2*k+2                                      c
    	//      iopt=1 we start with the set of knots found at the last         c
    	//      call of the routine, except for the case that s > fp0; then     c
    	//      we compute directly the least-squares polynomial of degree k.   c
    	//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    	//  determine nmin, the number of knots for polynomial approximation.
    	      nmin = 2*k1;
    	      doloop: do {
    	      if ((iopt >= 0) || loopback) {
	    	//  calculation of acc, the absolute tolerance for the root of f(p)=s.
    	    	  if (!loopback) {
	    	      acc = tol*s;
	    	//  determine nmax, the number of knots for spline interpolation.
	    	      nmax = m+k1;
    	    	  }
	    	      if ((s <= 0.0) || loopback) {
		    	//  if s=0, s(x) is an interpolating spline.
		    	//  test whether the required storage space exceeds the available one.
	    	    	  if (!loopback) {
			    	      n[0] = nmax;
			    	      if(nmax > nest) {
			    	    	  ier[0] = 1;
			    	    	  return;
			    	      }
	    	    	  } // if (!loopback)
	    	    	  else {
	    	    		  loopback = false;
	    	    	  }
		    	//  find the position of the interior knots in case of interpolation.
		    	      mk1 = m-k1;
		    	      if (mk1 != 0) {
			    	      k3 = k/2;
			    	      i = k2;
			    	      j = k3+2;
			    	      if (k3*2 != k) {
			    	        for  (l=1; l <= mk1; l++) {
			    	            t[i-1] = x[j-1];
			    	            i = i+1;
			    	            j = j+1;
			    	        } // for  (l=1; l <= mk1; l++)
			    	      } // if (k3*2 != k)
			    	      else { // k3*2 == ki
				    	      for (l=1; l <= mk1; l++) {
				    	        t[i-1] = (x[j-1]+x[j-2])*half;
				    	        i = i+1;
				    	        j = j+1;
				    	      } // for (l=1; l <= mk1; l++)
			    	      } // else k3*2 == k
		    	      } // if (mk1 != 0)
	    	      } // if ((s <= 0.0) || loopback)
	    	      else { // s > 0.0 && !loopback
			    	//  if s>0 our initial choice of knots depends on the value of iopt.
			    	//  if iopt=0 or iopt=1 and s>=fp0, we start computing the least-squares
			    	//  polynomial of degree k which is a spline without interior knots.
			    	//  if iopt=1 and fp0>s we start computing the least squares spline
			    	//  according to the set of knots found at the last call of the routine.
			    	  if(iopt != 0) {
				    	      if(n[0] != nmin) {
					    	      fp0 = fpint[n[0]-1];
					    	      fpold = fpint[n[0]-2];
					    	      nplus = nrdata[n[0]-1];
					    	      if(fp0 > s) {
					    	    	  do50 = false;
					    	      }
				    	      } // if (n[0] != nmin)
			    	  } // if (iopt != 0)
				      if (do50) {
			    	      n[0] = nmin;
			    	      fpold = 0.0;
			    	      nplus = 0;
			    	      nrdata[0] = m-2;
				      } // if (do50)
				      else {
				    	  do50 = true;
				      }
	    	      } // else s > 0.0 
    	      } // if ((iopt >= 0) || loopback)
    	//  main loop for the different sets of knots. m is a save upper bound
    	//  for the number of trials.
    	  iterloop: for (iter = 1; iter <= m; iter++) {
    	        if(n[0] == nmin) ier[0] = -2;
    	//  find nrint, tne number of knot intervals.
    	        nrint[0] = n[0]-nmin+1;
    	//  find the position of the additional knots which are needed for
    	//  the b-spline representation of s(x).
    	        nk1 = n[0]-k1;
    	        i = n[0];
    	        for (j=1; j <= k1; j++) {
    	          t[j-1] = xb;
    	          t[i-1] = xe;
    	          i = i-1;
    	        } // for (j=1; j <= k1; j++)
    	//  compute the b-spline coefficients of the least-squares spline
    	//  sinf(x). the observation matrix a is built up row by row and
    	//  reduced to upper triangular form by givens transformations.
    	//  at the same time fp=f(p=inf) is computed.
    	        fp[0] = 0.0;
    	//  initialize the observation matrix a.
    	        for (i=1; i <= nk1; i++) {
    	          z[i-1] = 0.0;
    	          for (j=1; j <= k1; j++) {
    	            a[i-1][j-1] = 0.0;
    	          }
    	        } // for (i=1; i <= nk1; i++)
    	        l = k1;
    	        for (it=1; it <= m; it++) {
    	//  fetch the current data point x(it),y(it).
    	          xi = x[it-1];
    	          wi = w[it-1];
    	          yi[0] = y[it-1]*wi;
    	//  search for knot interval t(l) <= xi < t(l+1).
    	          do {
    	              if((xi < t[l]) || (l == nk1)) {
    	            	  break;
    	              }
    	              l = l+1;
    	          } while(true);
    	//  evaluate the (k+1) non-zero b-splines at xi and store them in q.
    	          fpbspl(t,n[0],k,xi,l,h);
    	          for (i=1; i <= k1; i++) {
    	            q[it-1][i-1] = h[i-1];
    	            h[i-1] = h[i-1]*wi;
    	          } // for (i=1; i <= k1; i++)
    	//  rotate the new row of the observation matrix into triangle.
    	          j = l-k1;
    	          for (i=1; i <= k1; i++) {
    	            j = j+1;
    	            piv = h[i-1];
    	            if(piv != 0.0) {
	    	//  calculate the parameters of the givens transformation.
    	            	temp[0] = a[j-1][0];
	    	            fpgivs(piv,temp,cos,sin);
	    	            a[j-1][0] = temp[0];
	    	//  transformations to right hand side.
	    	            temp[0] = z[j-1];
	    	            fprota(cos[0],sin[0],yi,temp);
	    	            z[j-1] = temp[0];
	    	            if(i == k1) {
	    	            	break;
	    	            }
	    	            i2 = 1;
	    	            i3 = i+1;
	    	            for  (i1 = i3; i1 <= k1; i1++) {
	    	              i2 = i2+1;
	    	//  transformations to left hand side.
	    	              temp[0] = h[i1-1];
	    	              temp2[0] = a[j-1][i2-1];
	    	              fprota(cos[0],sin[0],temp,temp2);
	    	              h[i1-1] = temp[0];
	    	              a[j-1][i2-1] = temp2[0];
	    	            } // for  (i1 = i3; i1 <= k1; i1++)
    	            } // if (piv != 0.0)
    	          } // for (i=1; i <= k1; i++)
    	//  add contribution of this row to the sum of squares of residual
    	//  right hand sides.
    	          fp[0] = fp[0]+yi[0]*yi[0];
    	        } // for (it=1; it <= m; it++)
    	        if(ier[0] == (-2)) fp0 = fp[0];
    	        fpint[n[0]-1] = fp0;
    	        fpint[n[0]-2] = fpold;
    	        nrdata[n[0]-1] = nplus;
    	//  backward substitution to obtain the b-spline coefficients.
    	        fpback(a,z,nk1,k1,c,nest);
    	//  test whether the approximation sinf(x) is an acceptable solution.
    	        if(iopt < 0) {
    	        	return;
    	        }
    	        fpms = fp[0]-s;
    	        if(Math.abs(fpms) < acc) {
    	        	return;
    	        }
    	//  if f(p=inf) < s accept the choice of knots.
    	        if(fpms < 0.0) {
    	        	break doloop;
    	        }
    	//  if n = nmax, sinf(x) is an interpolating spline.
    	        if(n[0] == nmax) {
    	        	ier[0] = -1;
    	        	return;
    	        }
    	//  increase the number of knots.
    	//  if n=nest we cannot increase the number of knots because of
    	//  the storage capacity limitation.
    	        if(n[0] == nest) {
    	        	ier[0] = 1;
    	        	return;
    	        }
    	//??  determine the number of knots nplus we are going to add.
    	        if(ier[0] != 0) {
	    	        nplus = 1;
	    	        ier[0] = 0;
    	        }
    	        else {
	    	        npl1 = nplus*2;
	    	        rn = nplus;
	    	        if(fpold-fp[0] > acc) npl1 = (int)(rn*fpms/(fpold-fp[0]));
	    	        nplus = Math.min(nplus*2,Math.max(npl1,Math.max(nplus/2,1)));
    	        }
    	        fpold = fp[0];
    	//  compute the sum((w(i)*(y(i)-s(x(i))))**2) for each knot interval
    	//  t(j+k) <= x(i) <= t(j+k+1) and store it in fpint(j),j=1,2,...nrint.
    	        fpart = 0.0;
    	        i = 1;
    	        l = k2;
    	        newi = 0;
    	        for (it=1; it <= m; it++) {
    	          if(!((x[it-1] < t[l-1]) || (l > nk1))) {
    	              newi = 1;
    	              l = l+1;
    	          }
    	          term = 0.0;
    	          l0 = l-k2;
    	          for (j=1; j <= k1; j++) {
    	            l0 = l0+1;
    	            term = term+c[l0-1]*q[it-1][j-1];
    	          } //  for (j=1; j <= k1; j++)
    	          term = w[it-1]*(term-y[it-1]);
    	          term = term * term;
    	          fpart = fpart+term;
    	          if(newi != 0) {
	    	          store = term*half;
	    	          fpint[i-1] = fpart-store;
	    	          i = i+1;
	    	          fpart = store;
	    	          newi = 0;
    	          } // if (newi != 0)
    	        } // for (it=1; it <= m; it++)
    	        fpint[nrint[0]-1] = fpart;
    	        for (l=1; l <= nplus; l++) {
    	//  add a new knot.
    	          fpknot(x,m,t,n,fpint,nrdata,nrint,nest,1);
    	//  if n=nmax we locate the knots as for interpolation.
    	          if(n[0] == nmax) {
    	        	  do50 = true;
    	        	  loopback = true;
    	        	  break iterloop;
    	          }
    	//  test whether we cannot further increase the number of knots.
    	          if(n[0] == nest) {
    	        	  break;
    	          }
    	        } // for (l=1; l <= nplus; l++)
    	//  restart the computations with the new set of knots.
    	  } // for (iter = 1; iter <= m; iter++)
    	} while (loopback);
    	//  test whether the least-squares kth degree polynomial is a solution
    	//  of our approximation problem.
    	if(ier[0] == (-2)) {
    		 return;
    	 }
    	//ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    	//  part 2: determination of the smoothing spline sp(x).                c
    	//  ***************************************************                 c
    	//  we have determined the number of knots and their position.          c
    	//  we now compute the b-spline coefficients of the smoothing spline    c
    	//  sp(x). the observation matrix a is extended by the rows of matrix   c
    	//  b expressing that the kth derivative discontinuities of sp(x) at    c
    	//  the interior knots t(k+2),...t(n-k-1) must be zero. the corres-     c
    	//  ponding weights of these additional rows are set to 1/p.            c
    	//  iteratively we then have to determine the value of p such that      c
    	//  f(p)=sum((w(i)*(y(i)-sp(x(i))))**2) be = s. we already know that    c
    	//  the least-squares kth degree polynomial corresponds to p=0, and     c
    	//  that the least-squares spline corresponds to p=infinity. the        c
    	//  iteration process which is proposed here, makes use of rational     c
    	//  interpolation. since f(p) is a convex and strictly decreasing       c
    	//  function of p, it can be approximated by a rational function        c
    	//  r(p) = (u*p+v)/(p+w). three values of p(p1,p2,p3) with correspond-  c
    	//  ing values of f(p) (f1=f(p1)-s,f2=f(p2)-s,f3=f(p3)-s) are used      c
    	//  to calculate the new value of p such that r(p)=s. convergence is    c
    	//  guaranteed by taking f1>0 and f3<0.                                 c
    	// ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    	//  evaluate the discontinuity jump of the kth derivative of the
    	//  b-splines at the knots t(l),l=k+2,...n-k-1 and store in b.
    
    	      fpdisc(t,n[0],k2,b,nest);
    	//  initial value for p.
    	      p1[0] = 0.0;
    	      f1[0] = fp0-s;
    	      p3[0] = -one;
    	      f3[0] = fpms;
    	      p = 0.0;
    	      for (i = 1; i <= nk1; i++) {
    	         p = p+a[i-1][0];
    	      }
    	      rn = nk1;
    	      p = rn/p;
    	      ich1 = 0;
    	      ich3 = 0;
    	      n8 = n[0]-nmin;
    	//  iteration process to find the root of f(p) = s.
    	      for (iter = 1; iter <= maxit; iter++) {
    	//  the rows of matrix b with weight 1/p are rotated into the
    	//  triangularised observation matrix a which is stored in g.
    	        pinv = one/p;
    	        for  (i = 1; i <= nk1; i++) {
    	          c[i-1] = z[i-1];
    	          g[i-1][k2-1] = 0.0;
    	          for (j = 1; j <= k1; j++) {
    	            g[i-1][j-1] = a[i-1][j-1];
    	          }
    	        } // for  (i = 1; i <= nk1; i++)
    	        for (it = 1; it <= n8; it++) {
    	//  the row of matrix b is rotated into triangle by givens transformation
    	          for (i = 1; i <= k2; i++) {
    	            h[i-1] = b[it-1][i-1]*pinv;
    	          } // for (i = 1; i <= k2; i++)
    	          yi[0] = 0.0;
    	          for (j = it; j <= nk1; j++) {
    	            piv = h[0];
    	//  calculate the parameters of the givens transformation.
    	            temp[0] = g[j-1][0];
    	            fpgivs(piv,temp,cos,sin);
    	            g[j-1][0] = temp[0];
    	//  transformations to right hand side.
    	            temp[0] = c[j-1];
    	            fprota(cos[0],sin[0],yi,temp);
    	            c[j-1] = temp[0];
    	            if(j == nk1) {
    	            	break;
    	            }
    	            i2 = k1;
    	            if(j > n8) i2 = nk1-j;
    	            for (i=1; i <= i2; i++) {
    	//  transformations to left hand side.
    	              i1 = i+1;
    	              temp[0] = h[i1-1];
    	              temp2[0] = g[j-1][i1-1];
    	              fprota(cos[0],sin[0],temp,temp2);
    	              h[i1-1] = temp[0];
    	              g[j-1][i1-1] = temp2[0];
    	              h[i-1] = h[i1-1];
    	            } //  for (i=1; i <= i2; i++)
    	            h[i2] = 0.0;
    	          } // for (j = it; j <= nk1; j++)
    	        } // for (it = 1; it <= n8; it++)
    	//  backward substitution to obtain the b-spline coefficients.
    	        fpback(g,c,nk1,k2,c,nest);
    	//  computation of f(p).
    	        fp[0] = 0.0;
    	        l = k2;
    	        for (it = 1; it <= m; it++) {
    	          if(!((x[it-1] < t[l-1]) || (l > nk1))) {
    	              l = l+1;
    	          }
    	          l0 = l-k2;
    	          term = 0.0;
    	          for (j = 1; j <= k1; j++) {
    	            l0 = l0+1;
    	            term = term+c[l0-1]*q[it-1][j-1];
    	          } // for (j = 1; j <= k1; j++)
    	          prod = w[it-1]*(term - y[it-1]);
    	          fp[0] = fp[0]+ prod * prod;
    	        } // for (it = 1; it <= m; it++)
    	//  test whether the approximation sp(x) is an acceptable solution.
    	        fpms = fp[0]-s;
    	        if(Math.abs(fpms) < acc) {
    	        	return;
    	        }
    	//  test whether the maximal number of iterations is reached.
    	        if(iter == maxit) {
    	        	ier[0] = 3;
    	        	return;
    	        }
    	//  carry out one more step of the iteration process.
    	        p2 = p;
    	        f2 = fpms;
    	        if (ich3 == 0) {
	    	        if((f2-f3[0]) <= acc) {
	    	//  our initial choice of p is too large.
	    	        p3[0] = p2;
	    	        f3[0] = f2;
	    	        p = p*con4;
	    	        if(p <= p1[0]) p=p1[0]*con9 + p2*con1;
	    	        continue;
	    	        } // if((f2-f3) <= acc)
	    	        if(f2 < 0.0) ich3=1;
    	        } // if (ich3 == 0)
    	        if (ich1 == 0) {
	    	        if ((f1[0]-f2) <= acc) {
	    	//  our initial choice of p is too small
	    	        p1[0] = p2;
	    	        f1[0] = f2;
	    	        p = p/con4;
	    	        if(p3[0] < 0.0) {
	    	        	continue;
	    	        }
	    	        if(p >= p3[0]) p = p2*con1 + p3[0]*con9;
	    	        continue;
	    	        } // if ((f1-f2) <= acc)
    	        if(f2 > 0.0) ich1=1;
    	        } //  if (ich1 == 0)
    	//  test whether the iteration process proceeds as theoretically
    	//  expected.
    	        if ((f2 >= f1[0]) || (f2 <= f3[0])) {
    	        	ier[0] = 2;
    	        	return;
    	        }
    	//  find the new value for p.
    	        p = fprati(p1,f1,p2,f2,p3,f3);
    	      } // for (iter = 1; iter <= maxit; iter++)
    	      ier[0] = 3;
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

     public void fpgivs(double piv,double ww[],double cos[], double sin[]) {
		     // implicit none
		//  subroutine fpgivs calculates the parameters of a givens
		//  transformation .
		
		//  ..scalar arguments..
		//     real*8 piv,ww,cos,sin
		//  ..local scalars..
		     double dd,one,store,ratio;
		//  ..function references..
		//     real*8 abs,sqrt
	
		     one = 0.1e+01;
		     store = Math.abs(piv);
		     if(store >= ww[0]) {
		    	 ratio = ww[0]/piv;
		    	 dd = store*Math.sqrt(one+ratio*ratio);
		     }
		     else {
		    	 ratio = piv/ww[0];
		    	 dd = ww[0]*Math.sqrt(one+ratio*ratio);
		     }
		     cos[0] = ww[0]/dd;
		     sin[0] = piv/dd;
		     ww[0] = dd;
             return;
     }
     
     public void fprota(double cos, double sin,double a[], double b[]) {
	     //  subroutine fprota applies a givens rotation to a and b.
	 
	     //  ..scalar arguments..
	     //      real*8 cos,sin,a,b
	     // ..local scalars..
	           double stor1,stor2;
	 
	           stor1 = a[0];
	           stor2 = b[0];
	           b[0] = cos*stor2+sin*stor1;
	           a[0] = cos*stor1-sin*stor2;
	           return;
     }

     public void fpback(double a[][],double z[], int n, int k, double c[],int nest) {
		     // implicit none
		//  subroutine fpback calculates the solution of the system of
		//  equations a*c = z with a a n x n upper triangular matrix
		//  of bandwidth k.
		
		//..scalar arguments..
		//     integer n,k,nest
		//  ..array arguments..
		//     real*8 a(nest,k),z(n),c(n)
		//  ..local scalars..
		     double store;
		     int i,i1,j,k1,l,m;
		
		     k1 = k-1;
		     c[n-1] = z[n-1]/a[n-1][0];
		     i = n-1;
		     if(i == 0) {
		    	 return;
		     }
		     for (j=2; j <= n; j++) {
		       store = z[i-1];
		       i1 = k1;
		       if(j <= k1) i1 = j-1;
		       m = i;
		       for (l=1; l <= i1; l++) {
		         m = m+1;
		         store = store-c[m-1]*a[i-1][l];
		       } // for (l=1; l <= i1; l++)
		       c[i-1] = store/a[i-1][0];
		       i = i-1;
		     } // for (j=2; j <= n; j++)
		     return;
     }
     
     public void fpknot(double x[], int m,double t[] ,int n[], double fpint[], int nrdata[], int nrint[], int nest,
    	     int istart) {
    	//      implicit none
    	//  subroutine fpknot locates an additional knot for a spline of degree
    	//  k and adjusts the corresponding parameters,i.e.
    	//    t     : the position of the knots.
    	//    n     : the number of knots.
    	//    nrint : the number of knotintervals.
    	//    fpint : the sum of squares of residual right hand sides
    	//            for each knot interval.
    	//    nrdata: the number of data points inside each knot interval.
    	//  istart indicates that the smallest data point at which the new knot
    	//  may be added is x(istart+1)
    
    	//  ..scalar arguments..
    	//      integer m,n,nrint,nest,istart
    	//  ..array arguments..
	    //	real*8 x(m),t(nest),fpint(nest)
	    //    integer nrdata(nest)
    	//  ..local scalars..
        double an,am,fpmax;
         int ihalf,j,jbegin,jj,jk,jpoint,k,
             next,nrx;
         int maxbeg = 0;
         int maxpt = 0;
         int number = 0;

   //  note: do not initialize on the same line to avoid saving between calls
         boolean iserr;
         iserr = true;

         k = (n[0]-nrint[0]-1)/2;
   //  search for knot interval t(number+k) <= x <= t(number+k+1) where
   //  fpint(number) is maximal on the condition that nrdata(number)
   //  not equals zero.
         fpmax = 0.0;
         jbegin = istart;
         for (j = 1; j <= nrint[0]; j++) {
           jpoint = nrdata[j-1];
           if (!((fpmax >= fpint[j-1]) || (jpoint == 0))) {
	           iserr = false;
	           fpmax = fpint[j-1];
	           number = j;
	           maxpt = jpoint;
	           maxbeg = jbegin;
           }
           jbegin = jbegin+jpoint+1;
         } // for (j = 1; j <= nrint; j++)
   //  error condition detected, go to exit
         if (iserr) {
        	 n[0] = n[0]+1;
        	 nrint[0] = nrint[0]+1;
        	 return;
         }
   //  let coincide the new knot t(number+k+1) with a data point x(nrx)
   //  inside the old knot interval t(number+k) <= x <= t(number+k+1).
         ihalf = maxpt/2+1;
         nrx = maxbeg+ihalf;
         next = number+1;
         if(next <= nrint[0]) {
   //  adjust the different parameters.
	         for (j=next; j <= nrint[0]; j++) {
	           jj = next+nrint[0]-j;
	           fpint[jj] = fpint[jj-1];
	           nrdata[jj] = nrdata[jj-1];
	           jk = jj+k;
	           t[jk] = t[jk-1];
	         } // for (j=next; j <= nrint; j++)
         }
         nrdata[number-1] = ihalf-1;
         nrdata[next-1] = maxpt-ihalf;
         am = maxpt;
         an = nrdata[number-1];
         fpint[number-1] = fpmax*an/am;
         an = nrdata[next-1];
         fpint[next-1] = fpmax*an/am;
         jk = next+k;
         t[jk-1] = x[nrx-1];
         n[0] = n[0]+1;
         nrint[0] = nrint[0]+1;
         return;
     }

     public void fpdisc(double t[], int n, int k2, double b[][], int nest) {
	     // implicit none
	//  subroutine fpdisc calculates the discontinuity jumps of the kth
	//  derivative of the b-splines of degree k at the knots t(k+2)..t(n-k-1)
	//  ..scalar arguments..
	//     integer n,k2,nest
	//  ..array arguments..
	//     real*8 t(n),b(nest,k2)
	//  ..local scalars..
	     double an,fac,prod;
	     int i,ik,j,jk,k,k1,l,lj,lk,lmk,lp,nk1,nrint;
	//  ..local array..
	     double h[] = new double[12];
	
	     k1 = k2-1;
	     k = k1-1;
	     nk1 = n-k1;
	     nrint = nk1-k;
	     an = nrint;
	     fac = an/(t[nk1]-t[k1-1]);
	     for (l = k2; l <= nk1; l++) {
	       lmk = l-k1;
	       for  (j = 1; j <= k1; j++) {
	         ik = j+k1;
	         lj = l+j;
	         lk = lj-k2;
	         h[j-1] = t[l-1]-t[lk-1];
	         h[ik-1] = t[l-1]-t[lj-1];
	       } // for  (j = 1; j <= k1; j++)
	       lp = lmk;
	       for  (j = 1; j <= k2; j++) {
	         jk = j;
	         prod = h[j-1];
	         for (i = 1; i <= k; i++) {
	           jk = jk+1;
	           prod = prod*h[jk-1]*fac;
	         } // for (i = 1; i <= k; i++)
	         lk = lp+k1;
	         b[lmk-1][j-1] = (t[lk-1]-t[lp-1])/prod;
	         lp = lp+1;
	       } // for  (j = 1; j <= k2; j++)
	     } // for (l = k2; l <= nk1; l++) 
	     return;
     }

     public double fprati(double p1[], double f1[], double p2,double f2,
    		 double p3[], double f3[]) { 
	     // result(fprati_res)
		     // implicit none
		     // real*8 :: fprati_res
		//  given three points (p1,f1),(p2,f2) and (p3,f3), function fprati
		//  gives the value of p such that the rational interpolating function
		//  of the form r(p) = (u*p+v)/(p+w) equals zero at p.
	
		//  ..scalar arguments..
		//     real*8 p1,f1,p2,f2,p3,f3
		//  ..local scalars..
		     double h1,h2,h3,p;
		
		     if (p3[0] <= 0.0) {
		//  value of p in case p3 = infinity.
		         p = (p1[0]*(f1[0]-f3[0])*f2-p2*(f2-f3[0])*f1[0])/((f1[0]-f2)*f3[0]);
		     }
		     else {
		//  value of p in case p3 ^= infinity.
		         h1 = f1[0]*(f2-f3[0]);
		         h2 = f2*(f3[0]-f1[0]);
		         h3 = f3[0]*(f1[0]-f2);
		         p = -(p1[0]*p2*h3+p2*p3[0]*h1+p3[0]*p1[0]*h2)/(p1[0]*h1+p2*h2+p3[0]*h3);
		     }
		//  adjust the value of p1,f1,p3 and f3 such that f1 > 0 and f3 < 0.
		     if (f2 >= 0.0) {
			     p1[0] = p2;
			     f1[0] = f2;
		     }
		     else {
		         p3[0] = p2;
		         f3[0] = f2;
             }
		     return p;
     }



}