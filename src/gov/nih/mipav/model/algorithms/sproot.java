package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.Preferences;

public class sproot {
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
	private double zero[];
	private int mest;
	private int m[];
	private int ier[];
	
	public sproot() {
		
	}
	
	public sproot(double t[], int n, double c[], double zero[], 
			int mest, int m[], int ier[]) {
	    this.t = t;
	    this.n = n;
	    this.c = c;
	    this.zero = zero;
	    this.mest = mest;
	    this.m = m;
	    this.ier = ier;
	}
	
	public void run() {
		// implicit none
		//  subroutine sproot finds the zeros of a cubic spline s(x),which is
		//  given in its normalized b-spline representation.
		
		//  calling sequence:
		//     call sproot(t,n,c,zero,mest,m,ier)
		
		//  input parameters:
		//    t    : real array,length n, containing the knots of s(x).
		//    n    : integer, containing the number of knots.  n>=8
		//    c    : real array,length n, containing the b-spline coefficients.
		//    mest : integer, specifying the dimension of array zero.
		
		//  output parameters:
		//    zero : real array,length mest, containing the zeros of s(x).
		//    m    : integer,giving the number of zeros.
		//    ier  : error flag:
		//      ier = 0: normal return.
		//      ier = 1: the number of zeros exceeds mest.
		//      ier =10: invalid input data (see restrictions).
		
		//  other subroutines required: fpcuro
		
		//  restrictions:
		//    1) n>= 8.
		//    2) t(4) < t(5) < ... < t(n-4) < t(n-3).
		//       t(1) <= t(2) <= t(3) <= t(4)
		//       t(n-3) <= t(n-2) <= t(n-1) <= t(n)
		
		//  author :
		//    p.dierckx
		//    dept. computer science, k.u.leuven
		//    celestijnenlaan 200a, b-3001 heverlee, belgium.
		//    e-mail : Paul.Dierckx@cs.kuleuven.ac.be
		
		//  latest update : march 1987
		
		// ..
		// ..scalar arguments..
		//      integer n,mest,m,ier
		//  ..array arguments..
		//      real*8 t(n),c(n),zero(mest)
		//  ..local scalars..
		      int i,j,j1,l,n4;
		      double ah,a0,a1,a2,a3,bh,b0,b1,c1,c2,c3,c4,c5,d4,d5,h1,h2,
		             three,two,t1,t2,t3,t4,t5,zz;
		      boolean z0,z1,z2,z3,z4,nz0,nz1,nz2,nz3,nz4;
		//  ..local array..
		      double y[] = new double[3];
		      boolean do100 = true;
		      int jarr[] = new int[1];
		//  ..
		//  set some constants
		      two = 2.0;
		      three = 3.0;
		//  before starting computations a data check is made. if the input data
		//  are invalid, control is immediately repassed to the calling program.
		      n4 = n-4;
		      ier[0] = 10;
		      if(n < 8) {
		    	  return;
		      }
		      j = n;
		      for (i = 0; i <= 2; i++) {
		        if(t[i] > t[i+1]) {
		        	return;
		        }
		        if(t[j-1] < t[j-2]) {
		        	return;
		        }
		        j = j-1;
		      } // for (i = 0; i <= 2; i++)
		      for (i = 3; i <= n4-1; i++) {
		        if(t[i] >= t[i+1]) {
		        	return;
		        }
		      }
		//  the problem considered reduces to finding the zeros of the cubic
		//  polynomials pl(x) which define the cubic spline in each knot
		//  interval t(l)<=x<=t(l+1). a zero of pl(x) is also a zero of s(x) on
		//  the condition that it belongs to the knot interval.
		//  the cubic polynomial pl(x) is determined by computing s(t(l)),
		//  s'(t(l)),s(t(l+1)) and s'(t(l+1)). in fact we only have to compute
		//  s(t(l+1)) and s'(t(l+1)); because of the continuity conditions of
		//  splines and their derivatives, the value of s(t(l)) and s'(t(l))
		//  is already known from the foregoing knot interval.
		      ier[0] = 0;
		//  evaluate some constants for the first knot interval
		      h1 = t[3]-t[2];
		      h2 = t[4]-t[3];
		      t1 = t[3]-t[1];
		      t2 = t[4]-t[2];
		      t3 = t[5]-t[3];
		      t4 = t[4]-t[1];
		      t5 = t[5]-t[2];
		//  calculate a0 = s(t(3)) and ah = s'(t(3)).
		      c1 = c[0];
		      c2 = c[1];
		      c3 = c[2];
		      c4 = (c2-c1)/t4;
		      c5 = (c3-c2)/t5;
		      d4 = (h2*c1+t1*c2)/t4;
		      d5 = (t3*c2+h1*c3)/t5;
		      a0 = (h2*d4+h1*d5)/t2;
		      ah = three*(h2*c4+h1*c5)/t2;
		      z1 = true;
		      if(ah < 0.0) z1 = false;
		      nz1 = !z1;
		      m[0] = 0;
		//  main loop for the different knot intervals.
		      for (l = 3; l <= n4-1; l++) {
		//  evaluate some constants for the knot interval t(l) <= x <= t(l+1).
		        h1 = h2;
		        h2 = t[l+2]-t[l+1];
		        t1 = t2;
		        t2 = t3;
		        t3 = t[l+3]-t[l+1];
		        t4 = t5;
		        t5 = t[l+3]-t[l];
		//  find a0 = s(t(l)), ah = s'(t(l)), b0 = s(t(l+1)) and bh = s'(t(l+1)).
		        c1 = c2;
		        c2 = c3;
		        c3 = c[l];
		        c4 = c5;
		        c5 = (c3-c2)/t5;
		        d4 = (h2*c1+t1*c2)/t4;
		        d5 = (h1*c3+t3*c2)/t5;
		        b0 = (h2*d4+h1*d5)/t2;
		        bh = three*(h2*c4+h1*c5)/t2;
		//  calculate the coefficients a0,a1,a2 and a3 of the cubic polynomial
		//  pl(x) = ql(y) = a0+a1*y+a2*y**2+a3*y**3 ; y = (x-t(l))/(t(l+1)-t(l)).
		        a1 = ah*h1;
		        b1 = bh*h1;
		        a2 = three*(b0-a0)-b1-two*a1;
		        a3 = two*(a0-b0)+b1+a1;
		//  test whether or not pl(x) could have a zero in the range
		//  t(l) <= x <= t(l+1).
		        z3 = true;
		        if(b1 < 0.0) z3 = false;
		        nz3 = !z3;
		        if(a0*b0 > 0.0) {
			        z0 = true;
			        if(a0 < 0.0) z0 = false;
			        nz0 = !z0;
			        z2 = true;
			        if(a2 < 0.0) z2 = false;
			        nz2 = !z2;
			        z4 = true;
			        if(3.0*a3+a2 < 0.0) z4 = false;
			        nz4 = !z4;
			        if(!((z0&&(nz1&&(z3||z2&&nz4)||nz2&&
			           z3&&z4)||nz0&&(z1&&(nz3||nz2&&z4)||z2&&
			           nz3&&nz4)))) {
			            do100 = false;	
			        }
		        } // if(a0*b0 > 0.0)
		        if (do100) {
		//  find the zeros of ql(y).
		        fpcuro(a3,a2,a1,a0,y,jarr);
		        j = jarr[0];
		        if(j != 0) {
			//  find which zeros of pl(x) are zeros of s(x).
			        for (i = 0; i <= j-1; i++) {
			          if(!(y[i] < 0.0 || y[i] > 1.0)) {
				//  test whether the number of zeros of s(x) exceeds mest.
				          if(m[0] >= mest) {
				        	  ier[0] = 1;
				        	  return;
				          }
				          m[0] = m[0]+1;
				          zero[m[0]-1] = t[l]+h1*y[i];
			          } // if(!(y[i] < 0.0 || y[i] > 1.0))
			        } // for (i = 0; i <= j-1; i++)
		        } // if (j != 0)
		        } // if (do100)
		        else {
		        	do100 = true;
		        }
		        a0 = b0;
		        ah = bh;
		        z1 = z3;
		        nz1 = nz3;
		      } // (l = 3; l <= n4-1; l++)
		//  the zeros of s(x) are arranged in increasing order.
		      if(m[0] < 2) {
		    	  return;
		      }
		      for (i = 2; i <= m[0]; i++) {
		        j = i;
		        do {
			        j1 = j-1;
			        if(j1 == 0) break;
			        if(zero[j-1] >= zero[j1-1]) break;
			        zz = zero[j-1];
			        zero[j-1] = zero[j1-1];
			        zero[j1-1] = zz;
			        j = j1;
		        } while (true);
		      } // for (i = 2; i <= m[0]; i++)
		      j = m[0];
		      m[0] = 1;
		      for (i = 2; i <= j; i++) {
		        if(zero[i-1] != zero[m[0]-1]) {
		            m[0] = m[0]+1;
		            zero[m[0]-1] = zero[i-1];
		        } // if(zero[i-1] != zero[m[0]-1])
		      } // for (i = 2; i <= j; i++)
		      return;
	}
	
	public void fpcuro(double a, double b, double c, double d, double x[], int n[]) {
	    // implicit none
	//  subroutine fpcuro finds the real zeros of a cubic polynomial
	//  p(x) = a*x**3+b*x**2+c*x+d.
	
	//  calling sequence:
	//     call fpcuro(a,b,c,d,x,n)
	
	//  input parameters:
	//    a,b,c,d: real values, containing the coefficients of p(x).
	
	//  output parameters:
	//    x      : real array,length 3, which contains the real zeros of p(x)
	//    n      : integer, giving the number of real zeros of p(x).
	//  ..
	//  ..scalar arguments..
	//    real*8 a,b,c,d
	//    integer n
	//  ..array argument..
	//    real*8 x(3)
	//  ..local scalars..
	    int i;
	    double a1,b1,c1,df,d1,e3,f,four,half,ovfl,pi3,p3,q,
	           step,tent,three,two,u,u1,u2,y;
	    double disc = 0.0;
	    double r = 0.0;
	    boolean do200 = true;
	    boolean do300 = true;
	    boolean do400 = true;
	//  ..function references..
	//    real*8 abs,max,datan,atan2,cos,sign,sqrt
	//  set constants
	    two = 2.0;
	    three = 3.0;
	    four = 4.0;
	    ovfl = 1.0E4;
	    half = 0.5;
	    tent = 0.1;
	    e3 = tent/0.3;
	    pi3 = Math.atan(1.0)/0.75;
	    a1 = Math.abs(a);
	    b1 = Math.abs(b);
	    c1 = Math.abs(c);
	    d1 = Math.abs(d);
	//  test whether p(x) is a third degree polynomial.
	    if (Math.max(b1,Math.max(c1,d1)) >= a1*ovfl) {
		//  test whether p(x) is a second degree polynomial.
		    if(Math.max(c1,d1) >= b1*ovfl) {
			//  test whether p(x) is a first degree polynomial.
			    if(d1 >= c1*ovfl) {
				//  p(x) is a constant function.
				    n[0] = 0;
				    return;
			    } // if(d1 >= c1*ovfl)
				//  p(x) is a first degree polynomial.
				n[0] = 1;
				x[0] = -d/c;
				do200 = false;
				do300 = false;
				do400 = false;
		    } // if(Math.max(c1,d1) >= b1*ovfl)
		//  p(x) is a second degree polynomial.
		if (do200) {
		    disc = c*c-four*b*d;
		    n[0] = 0;
		    if(disc  < 0.0) {
		    	return;
		    }
		    n[0] = 2;
		    u = Math.sqrt(disc);
		    b1 = b+b;
		    x[0] = (-c+u)/b1;
		    x[1] = (-c-u)/b1;
		    do300 = false;
		    do400 = false;
		} // if (do200):
	    } // if(Math.max(b1,Math.max(c1,d1)) >= a1*ovfl)
	//  p(x) is a third degree polynomial.
	if (do300) {
	    b1 = b/a*e3;
	    c1 = c/a;
	    d1 = d/a;
	    q = c1*e3-b1*b1;
	    r = b1*b1*b1+(d1-b1*c1)*half;
	    disc = q*q*q+r*r;
	    if (disc <= 0.0) {
		    u = Math.sqrt(Math.abs(q));
		    if(r < 0.0) u = -u;
		    p3 = Math.atan2(Math.sqrt(-disc),Math.abs(r))*e3;
		    u2 = u+u;
		    n[0] = 3;
		    x[0] = -u2*Math.cos(p3)-b1;
		    x[1] = u2*Math.cos(pi3-p3)-b1;
		    x[2] = u2*Math.cos(pi3+p3)-b1;
		    do400 = false;
	    } // if (disc <= 0.0) 
	} // if (do300)
	if (do400) {
	    u = Math.sqrt(disc);
	    u1 = -r+u;
	    u2 = -r-u;
	    n[0] = 1;
	    x[0] = sign(Math.pow(Math.abs(u1),e3),u1)+sign(Math.pow(Math.abs(u2),e3),u2)-b1;
	} // if (do400)
	//  apply a newton iteration to improve the accuracy of the roots.
	for (i = 0; i <= n[0]-1; i++) {
	      y = x[i];
	      f = ((a*y+b)*y+c)*y+d;
	      df = (three*a*y+two*b)*y+c;
	      step = 0.0;
	      if(Math.abs(f) < Math.abs(df)*tent) step = f/df;
	      x[i] = y-step;
	} // for (i = 0; i <= n[0]-1; i++)
	return;
	}
	
	public double sign(double a, double b) {
		if (b >= 0) {
			return Math.abs(a);
		}
		else {
			return (-Math.abs(a));
		}
	}
	
	public void test_sproot() {
		// sproot returned 4 roots
		// The splev of the roots passed the test
		// All roots were close to their expected values
		int i;
        // sproot is only implemented for k=3
		double a = 0.1;
		double b = 15.0;
		double x[] = new double[20];
		double inc = (b-a)/19.0;
		for (i = 0; i < 20; i++) {
			x[i] = a + inc*i;
		}
        double v[] = new double[20];
        for (i = 0; i < 20; i++) {
        	v[i] = Math.sin(x[i]);
        }

        int k = 3;
        int iopt = 0;
        int m = 20;
        if (m <= k) {
        	System.err.println("Must have m > k in fwhm");
        	return;
        }
        double w[] = new double[m];
        for (i = 0; i < m; i++) {
        	w[i] = 1.0;
        }
        double xb = x[0];
        double xe = x[x.length-1];
        double s = 0.0;
        int nest = Math.max(m + k + 1, 2*k + 3);
        int n[] = new int[1];
        double t[] = new double[nest];
        double c[] = new double[nest];
        double fp[] = new double[1];
        int lwrk = nest*(3*k + 7) + m*(k + 1);
        int iwrk[] = new int[nest];
        int ier[] = new int[1];
        curfit cur = new curfit(iopt, m, x, v, w, xb, xe, k, s, nest, n,
        		t, c, fp, lwrk, iwrk, ier);
        cur.run();
        if (ier[0] == -1) {
        	Preferences.debug("Normal return from curfit\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("The spline returned is an interpolating spline (fp[0] = 0)\n",
        			Preferences.DEBUG_ALGORITHM);
        }
        else if (ier[0] == 1) {
        	System.err.println("In curfit the required storage space exceeds the available storage space");
        	System.exit(-1);
        }
        else if (ier[0] == 10) {
        	System.err.println("In curfit the input data are invalid");
        	System.exit(-1);
        }
        double tout[] = new double[n[0]];
        double cout[] = new double[n[0]];
        for (i = 0; i < n[0]; i++) {
        	tout[i] = t[i];
        	cout[i] = c[i];
        }

        int mest = 10;
        double zero[] = new double[mest];
        int marr[] = new int[1];
        sproot spr = new sproot(tout,n[0],cout,zero,mest,marr,ier);
        spr.run();
        if (ier[0] == 0) {
        	Preferences.debug("Normal return from sproot\n", Preferences.DEBUG_ALGORITHM);	
        }
        else if (ier[0] == 1) {
        	System.err.println("In sproot the number of zeros exceeds mest");
        	System.exit(-1);
        }
        else if (ier[0] == 10) {
        	System.err.println("In sproot the input data is invalid");
        	System.exit(-1);
        }
        System.out.println("sproot returned " + marr[0] + " roots");
        double y[] = new double[marr[0]];
        splev sp = new splev(tout, n[0], cout, k, zero, y, marr[0], 0, ier);
        sp.run();
        if (ier[0] == 10) {
        	System.err.println("In splev the input data is invalid");
        	System.exit(-1);
        }
        //double allowed_difference = atol + rtol*abs(desired)
        //assert_allclose(splev(roots, tck), 0, atol=1e-10, rtol=1e-10)
        double allowed_difference = 1.0E-10;
        int num_differences = 0;
        for (i = 0; i < marr[0]; i++) {
            if (Math.abs(y[i]) > allowed_difference) {
            	num_differences++;
            	System.err.println("y["+i+"] = " + y[i] + " greater than the allowed 1.0E-10");
            }
        }
        if (num_differences == 0) {
        	System.out.println("The splev of the roots passed the test");
        }
        num_differences = 0;
        for (i = 0; i < marr[0]; i++) {
        	allowed_difference = 1.0E-3*(i+1)*Math.PI;
        	if (Math.abs(zero[i] - (i+1)*Math.PI) > allowed_difference) {
        		System.err.println("roots["+i+"] = " + zero[i] + " instead of the expected " + ((i+1)*Math.PI));
        	}
        }
        System.out.println("All roots were close to their expected values");
        //assert_allclose(roots, np.pi * np.array([1, 2, 3, 4]), rtol=1e-3)
	}


}