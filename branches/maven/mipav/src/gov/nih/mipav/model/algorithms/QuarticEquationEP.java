package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.util.DoubleDouble;
import gov.nih.mipav.view.Preferences;

/**
 * Copyright Yair Chuchem 2011.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Yair Chuchem nor the names of other
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

The equation is a*x**4 + b*x**3 + c*x**2 + d*x + e = 0
/* Solve Polynomials of up to the fourth degree. (over complex numbers)
 * Algorithms by Ferrari, Tartaglia, Cardano, et al. (16th century Italy)


@author ilb

 */
public class QuarticEquationEP {
	private int degree;
	private DoubleDouble areal;
	private DoubleDouble aimag;
	private DoubleDouble breal;
	private DoubleDouble bimag;
	private DoubleDouble creal;
	private DoubleDouble cimag;
	private DoubleDouble dreal;
	private DoubleDouble dimag;
	private DoubleDouble ereal;
	private DoubleDouble eimag;
	private int num_sols[];
	private double solsreal[]; // up to 4 elements
	private double solsimag[]; // up to 4 elements
	private final int MAX_DEGREE = 4;
	final int ONLY_REAL_ROOTS = 1;
	final int COMPLEX_ROOTS = 2;
	int inputType;
	
	public QuarticEquationEP() {
		
	}
	
	public QuarticEquationEP(int degree, double areal, double aimag, double breal, double bimag,
			double creal, double cimag, double dreal, double dimag, double ereal, double eimag,
			int num_sols[], double solsreal[], double solsimag[]) {
		this.degree = degree;
		this.areal = DoubleDouble.valueOf(areal);
		this.aimag = DoubleDouble.valueOf(aimag);
		this.breal =  DoubleDouble.valueOf(breal);
		this.bimag =  DoubleDouble.valueOf(bimag);
		this.creal =  DoubleDouble.valueOf(creal);
		this.cimag =  DoubleDouble.valueOf(cimag);
		this.dreal =  DoubleDouble.valueOf(dreal);
		this.dimag =  DoubleDouble.valueOf(dimag);
		this.ereal =  DoubleDouble.valueOf(ereal);
		this.eimag =  DoubleDouble.valueOf(eimag);
		this.num_sols = num_sols;
		this.solsreal = solsreal;
		this.solsimag = solsimag;
		inputType = COMPLEX_ROOTS;
	}
	
	public QuarticEquationEP(int degree, double areal, double breal, double creal, double dreal,
			double ereal, int num_sols[], double sols[]) {
		this.degree = degree;
		this.areal =  DoubleDouble.valueOf(areal);
        this.breal =  DoubleDouble.valueOf(breal);
		this.creal =  DoubleDouble.valueOf(creal);
		this.dreal =  DoubleDouble.valueOf(dreal);
		this.ereal =  DoubleDouble.valueOf(ereal);
		this.num_sols = num_sols;
		this.solsreal = sols;
		inputType = ONLY_REAL_ROOTS;
	}
	
	public void selfTest() {
	    // x**4	+ 6x**3 - 5x**2 - 10x - 3 = 0
		// has the roots (-7 +- sqrt(37))/2, (1 +- sqrt(5))/2
		// -0.4586187348508902, -6.54138126514911, 1.618033988749895, -0.6180339887498948
		int i;
		degree = 4;
		double areal =  1.0;
		double aimag =  0.0;
		double breal =  6.0;
		double bimag =  0.0;
		double creal =  -5.0;
		double cimag =  0.0;
		double dreal =  -10.0;
		double dimag =  0.0;
		double ereal =  -3.0;
		double eimag =  0.0;
		num_sols =  new int[1];
		solsreal = new double[4];
		solsimag = new double[4];
		QuarticEquation qe = new QuarticEquation(degree, areal, aimag, breal, bimag, creal, cimag,
				dreal, dimag, ereal, eimag, num_sols, solsreal, solsimag);
		qe.run();
		
		QuarticEquation qe2 = new QuarticEquation(degree, areal, breal, creal, dreal, ereal, num_sols,
				solsreal);
		qe2.run();
		
		// x**4 + 6x**3 + 7x**2 -7x - 12 = 0
		// has the roots x = 1.1478990357,   x = -1.5739495179 +/- 0.3689894075 i,   and   x = -4.0000000000, 
		// x = -4 is an exact root
		creal = 7.0;
		dreal = -7.0;
		ereal = -12.0;
		qe = new QuarticEquation(degree, areal, aimag, breal, bimag, creal, cimag,
				dreal, dimag, ereal, eimag, num_sols, solsreal, solsimag);
		qe.run();
	}
	
	public void run() {
		if (inputType == ONLY_REAL_ROOTS) {
			runReal();
		}
		else {
			runComplex();
		}
	}
	
	public void runReal() {
		int i;
	    DoubleDouble poly[] = new DoubleDouble[degree+1];
	    for (i = 0; i < degree+1; i++) {
	    	poly[i] = new DoubleDouble();
	    }
	    poly[0] = ereal;
	    if (degree >= 1) {
	        poly[1] = dreal;
	        if (degree >= 2) {
	        	poly[2] = creal;
	        	if (degree >= 3) {
	        		poly[3] = breal;
	        		if (degree == 4) {
	        			poly[4] = areal;
	        		}
	        	} // if (degree >= 3)
	        } // if (degree >= 2)
	    } // if (degree >= 1)
		num_sols[0] = solve_real_poly(degree, poly, solsreal);
		for (i = 0; i < num_sols[0]; i++) {
		    Preferences.debug(solsreal[i] + "\n", Preferences.DEBUG_ALGORITHM);	
		}
	}
	
	/* poly: pointer to coefficients array of size degree + 1.
	 * results: pointer to results output array of size degree.
	 */
	int solve_real_poly(int degree, DoubleDouble[] poly, double[] results) {
		int i;
	    DoubleDouble normalized_poly[] = new DoubleDouble[MAX_DEGREE + 1];
		for (i = 0; i < MAX_DEGREE + 1; i++) {
			normalized_poly[i] = new DoubleDouble();
		}
	    DoubleDouble a = poly[degree];
	    if (degree == 0)
	        return 0;
	    if (a.isZero())
	        return solve_real_poly(degree - 1, poly, results);
	    if (degree > MAX_DEGREE)
	        return -1;
	    if (degree > 2 && stableness_score(poly[degree], poly[degree - 1]).gt(stableness_score(poly[0], poly[1]))) {
	        DoubleDouble rev_poly[] = new DoubleDouble[MAX_DEGREE + 1];
	        for (i = 0; i < MAX_DEGREE + 1; i++) {
	        	rev_poly[i] = new DoubleDouble();
	        }
	        int num_results;
	        for (i = 0; i <= degree; ++i)
	            rev_poly[i] = poly[degree - i];
	        num_results = solve_real_poly(degree, rev_poly, results);
	        for (i = 0; i < num_results; ++i)
	            results[i] = 1.0 / results[i];
	        return num_results;
	    }
	    for (i = 0; i < degree; ++i)
	        normalized_poly[i] = poly[i].divide(a);
	    normalized_poly[degree] = DoubleDouble.valueOf(1.0);
	    return solve_normalized_poly(degree, normalized_poly, results);
	}
	
	int solve_real_poly(int degree, DoubleDouble[] poly, DoubleDouble[] results) {
		int i;
	    DoubleDouble normalized_poly[] = new DoubleDouble[MAX_DEGREE + 1];
		for (i = 0; i < MAX_DEGREE + 1; i++) {
			normalized_poly[i] = new DoubleDouble();
		}
	    DoubleDouble a = poly[degree];
	    if (degree == 0)
	        return 0;
	    if (a.isZero())
	        return solve_real_poly(degree - 1, poly, results);
	    if (degree > MAX_DEGREE)
	        return -1;
	    if (degree > 2 && stableness_score(poly[degree], poly[degree - 1]).gt(stableness_score(poly[0], poly[1]))) {
	        DoubleDouble rev_poly[] = new DoubleDouble[MAX_DEGREE + 1];
	        for (i = 0; i < MAX_DEGREE + 1; i++) {
	        	rev_poly[i] = new DoubleDouble();
	        }
	        int num_results;
	        for (i = 0; i <= degree; ++i)
	            rev_poly[i] = poly[degree - i];
	        num_results = solve_real_poly(degree, rev_poly, results);
	        for (i = 0; i < num_results; ++i)
	            results[i] = results[i].reciprocal();
	        return num_results;
	    }
	    for (i = 0; i < degree; ++i)
	        normalized_poly[i] = poly[i].divide(a);
	    normalized_poly[degree] = DoubleDouble.valueOf(1.0);
	    return solve_normalized_poly(degree, normalized_poly, results);
	}
	
	private DoubleDouble stableness_score(DoubleDouble a, DoubleDouble b) {
	    DoubleDouble t = (a.divide(b)).abs();
	    return t.add(t.reciprocal());
	}
	
	/* Normalized polynomials have the form of
	 *   x^n + a*x^(n-1) + ..
	 * The coefficient for x^n is one.
	 * solve_normalized_poly does expect to get this coefficient despite it being known.
	 */
	private int solve_normalized_poly(int degree, DoubleDouble[] poly, double[] results) {
		int i, num_results;
	    DoubleDouble shift = (poly[degree - 1].negate()).divide(DoubleDouble.valueOf(degree));
	    DoubleDouble shifted_coefs[] = new DoubleDouble[MAX_DEGREE + 1];
	    for (i = 0; i < MAX_DEGREE + 1; i++) {
	    	shifted_coefs[i] = new DoubleDouble();
	    }
	    calc_shifted_coefs(shift, degree, poly, shifted_coefs);
	    DoubleDouble ddresults[] = new DoubleDouble[results.length];
	    for (i = 0; i < results.length; i++) {
	    	ddresults[i] = new DoubleDouble();
	    }
	    num_results = solve_depressed_poly(degree, shifted_coefs, ddresults);
	    for (i = 0; i < num_results; ++i)
	        ddresults[i] = ddresults[i].add(shift);
	    for (i = 0; i < results.length; i++) {
	    	results[i] = ddresults[i].doubleValue();
	    }
	    return num_results;
	}
	
	private int solve_normalized_poly(int degree, DoubleDouble[] poly, DoubleDouble[] results) {
		int i, num_results;
	    DoubleDouble shift = (poly[degree - 1].negate()).divide(DoubleDouble.valueOf(degree));
	    DoubleDouble shifted_coefs[] = new DoubleDouble[MAX_DEGREE + 1];
	    for (i = 0; i < MAX_DEGREE + 1; i++) {
	    	shifted_coefs[i] = new DoubleDouble();
	    }
	    calc_shifted_coefs(shift, degree, poly, shifted_coefs);
	    num_results = solve_depressed_poly(degree, shifted_coefs, results);
	    return num_results;
	}
	
	private void calc_shifted_coefs(DoubleDouble shift, int degree, DoubleDouble[] src, DoubleDouble[] dst) {
		int i, j;
	    DoubleDouble binomials[][] = new DoubleDouble[MAX_DEGREE + 1][MAX_DEGREE + 1];
	    for (i = 0; i < MAX_DEGREE + 1; i++) {
	    	for (j = 0; j < MAX_DEGREE + 1; j++) {
	    		binomials[i][j] = new DoubleDouble();
	    	}
	    }
	    	
	    DoubleDouble shift_powers[] = new DoubleDouble[MAX_DEGREE + 1];
	    for (i = 0; i < MAX_DEGREE + 1; i++) {
	    	shift_powers[i] = new DoubleDouble();
	    }
	    int dst_i, src_i;
	    for (dst_i = 0; dst_i <= degree; ++dst_i)
	        dst[dst_i] = DoubleDouble.valueOf(0);
	    calc_binomials(degree+1, binomials[0].length, binomials);
	    calc_powers(shift, degree, shift_powers);
	    for (src_i = 0; src_i <= degree; ++src_i)
	        for (dst_i = 0; dst_i <= src_i; ++dst_i)
	            dst[dst_i] = dst[dst_i].add(src[src_i].multiply(shift_powers[src_i - dst_i].multiply(binomials[src_i][dst_i])));
	}


	private void calc_powers(DoubleDouble x, int max_power, DoubleDouble[] dst) {
	    int i;
	    dst[0] = DoubleDouble.valueOf(1.0);
	    if (max_power >= 1)
	        dst[1] = x;
	    for (i = 2; i <= max_power; ++i)
	        dst[i] = x.multiply(dst[i - 1]);
	}
	
	/* Depressed polynomials have the form of:
	 *   x^n + a*x^(n-2) + ..
	 * The coefficient for x^n is 1 and for x^(n-1) is zero.
	 * So it gets 3 coefficients for a depressed quartic polynom.
	 */
	private int solve_depressed_poly(int degree, DoubleDouble[] poly, DoubleDouble[] results) {
		int i;
		int num;
	    if (degree > 0 && poly[0].isZero()) {
	        results[0] = DoubleDouble.valueOf(0);
	        DoubleDouble poly1[] = new DoubleDouble[poly.length-1];
	        for (i = 0; i < poly1.length; i++) {
	        	poly1[i] = new DoubleDouble();
	        	poly1[i] = poly[i+1];
	        }
	        DoubleDouble results1[] = new DoubleDouble[results.length-1];
	        for (i = 0; i < results1.length; i++) {
	        	results1[i] = new DoubleDouble();
	        	results1[i] = results[i+1];
	        }
	        num = solve_depressed_poly(degree-1, poly1, results1);
	        for (i = 0; i < poly1.length; i++) {
	        	poly[i+1] = poly1[i];
	        }
	        for (i = 0; i < results1.length; i++) {
	            results[i+1] = results1[i];;
	        }
	        return 1 + num;
	    }
	    switch (degree) {
	    case 4:
	        return solve_depressed_quartic(poly, results);
	    case 3:
	        return solve_depressed_cubic(poly, results);
	    case 2:
	        return solve_depressed_quadratic(poly, results);
	    case 1:
	        results[0] = DoubleDouble.valueOf(0.0);
	        return 1;
	    case 0:
	        return 0;
	    default:
	        return -1;
	    }
	}
	
	/* Based on http://en.wikipedia.org/wiki/Quartic_function#Quick_and_memorable_solution_from_first_principles */
	private int solve_depressed_quartic(DoubleDouble[] poly, DoubleDouble[] results) {
		int i;
		int num;
	    DoubleDouble helper_cubic[] = new DoubleDouble[4];
	    for (i = 0; i < 4; i++) {
	    	helper_cubic[i] = new DoubleDouble();
	    }
	    DoubleDouble helper_results[] = new DoubleDouble[3];
	    DoubleDouble quadratic_factor[] = new DoubleDouble[3];
	    for (i = 0; i < 3; i++) {
	    	helper_results[i] = new DoubleDouble();
	    	quadratic_factor[i] = new DoubleDouble();
	    }
	    DoubleDouble p;
	    DoubleDouble e = poly[0];
	    DoubleDouble d = poly[1];
	    DoubleDouble c = poly[2];
	    int num_results;
	    if (d.isZero()) {
	        int num_quad_results;
	        DoubleDouble quadratic[] = new DoubleDouble[3];
	        for (i = 0; i < 3; i++) {
	        	quadratic[i] = new DoubleDouble();
	        }
	        DoubleDouble quadratic_results[] = new DoubleDouble[2];
	        for (i = 0; i < 2; i++) {
	        	quadratic_results[i] = new DoubleDouble();
	        }
	        quadratic[0] = e;
	        quadratic[1] = c;
	        quadratic[2] = DoubleDouble.valueOf(1);
	        num_quad_results = solve_real_poly(2, quadratic, quadratic_results);
	        for (i = 0; i < num_quad_results; ++i) {
	            DoubleDouble s = (quadratic_results[i]).sqrt();
	            results[2*i] = s.negate();
	            results[2*i + 1] = s;
	        }
	        return 2 * num_quad_results;
	    }
	    helper_cubic[0] = (d.multiply(d)).negate();
	    helper_cubic[1] = (c.multiply(c)).subtract((DoubleDouble.valueOf(4)).multiply(e));
	    helper_cubic[2] = (DoubleDouble.valueOf(2)).multiply(c);
	    helper_cubic[3] = DoubleDouble.valueOf(1);
	    if (solve_real_poly(3, helper_cubic, helper_results) < 1)
	        return 0;
	    p = (helper_results[0]).sqrt();
	    quadratic_factor[0] = (c.add(p.multiply(p))).subtract(d.divide(p));
	    quadratic_factor[1] = (DoubleDouble.valueOf(2)).multiply(p);
	    quadratic_factor[2] = DoubleDouble.valueOf(2);
	    num_results = solve_real_poly(2, quadratic_factor, results);
	    quadratic_factor[0] = (c.add(p.multiply(p))).add(d.divide(p));
	    quadratic_factor[1] = (DoubleDouble.valueOf(-2)).multiply(p);
	    DoubleDouble resultsn[] = new DoubleDouble[2];
	    for (i = 0; i < 2; i++) {
	    	resultsn[i] = new DoubleDouble();
	    	resultsn[i] = results[i + num_results];
	    }
	    num = solve_real_poly(2, quadratic_factor, resultsn);
	    for (i = 0; i < 2; i++) {
	    	results[i + num_results] = resultsn[i];
	    }
	    return num_results + num;
	}

	
	/* Based on http://en.wikipedia.org/wiki/Cubic_equation#Cardano.27s_method
	 *
	 * Only provides one real result out of 3 possibly complex results.
	 */
	private int solve_depressed_cubic(DoubleDouble[] poly, DoubleDouble[] results) {
	    DoubleDouble q = poly[0];
	    DoubleDouble p = poly[1];
	    DoubleDouble t, s_real, s_abs, s_phase, u_abs, u_phase, u_real;
	    if (p.isZero()) {
	        results[0] = cubic_root(q.negate());
	        return 1;
	    }
	    t = ((q.multiply(q)).divide(DoubleDouble.valueOf(4))).add(((p.multiply(p)).multiply(p)).divide(DoubleDouble.valueOf(27)));
	    if (t.ge(DoubleDouble.valueOf(0.0))) {
	        DoubleDouble u = cubic_root(((q.negate()).divide(DoubleDouble.valueOf(2))).add(t.sqrt()));
	        results[0] = u.subtract((p.divide(DoubleDouble.valueOf(3.0))).divide(u));
	        return 1;
	    }
	    s_real = (q.negate()).divide(DoubleDouble.valueOf(2));
	    s_abs = ((s_real.multiply(s_real)).subtract(t)).sqrt();
	    s_phase = (((((t.negate()).sqrt()).divide(s_real))).atan()).add(s_real.ge(DoubleDouble.valueOf(0)) ? DoubleDouble.valueOf(0) : DoubleDouble.PI);
	    u_abs = cubic_root(s_abs);
	    u_phase = s_phase.divide(DoubleDouble.valueOf(3.0));
	    u_real = u_abs.multiply(u_phase.cos());
	    results[0] = (DoubleDouble.valueOf(2)).multiply(u_real);
	    return 1;
	}

	private DoubleDouble cubic_root(DoubleDouble x) {
	    DoubleDouble t = (x.abs()).pow(DoubleDouble.valueOf( 1.0 / 3.0));
	    return x.ge(DoubleDouble.valueOf( 0.0)) ? t : t.negate();
	}



	private int solve_depressed_quadratic(DoubleDouble[] poly, DoubleDouble[] results) {
	    DoubleDouble t = (poly[0].negate()).sqrt();
	    results[0] = t.negate();
	    results[1] = t;
	    return 2;
	}

	
	public void runComplex() {
		int i;
	    complex_t poly[] = new complex_t[degree+1];
	    complex_t sols[] = new complex_t[4];
	    for (i = 0; i < degree+1; i++) {
	    	poly[i] = new complex_t();
	    }
	    for (i = 0; i < 4; i++) {
	    	sols[i] = new complex_t();
	    }
	    poly[0].real = ereal;
	    poly[0].imag = eimag;
	    if (degree >= 1) {
	        poly[1].real = dreal;
	        poly[1].imag = dimag;
	        if (degree >= 2) {
	        	poly[2].real = creal;
	        	poly[2].imag = cimag;
	        	if (degree >= 3) {
	        		poly[3].real = breal;
	        		poly[3].imag = bimag;
	        		if (degree == 4) {
	        			poly[4].real = areal;
	        			poly[4].imag = aimag;
	        		}
	        	} // if (degree >= 3)
	        } // if (degree >= 2)
	    } // if (degree >= 1)
		num_sols[0] = solve_poly(degree, poly, sols);
		for (i = 0; i < num_sols[0]; i++) {
			solsreal[i] = (sols[i].real).doubleValue();
			solsimag[i] = (sols[i].imag).doubleValue();
		    Preferences.debug(sols[i].real + " + i * " + sols[i].imag + "\n",
		    		Preferences.DEBUG_ALGORITHM);	
		}
	}
	
	/* poly: pointer to coefficients array of size degree + 1.
	 * results: pointer to results output array of size degree.
	 */
	private int solve_poly(int degree, complex_t[] poly, complex_t[] results) {
	    complex_t normalized_poly[] = new complex_t[MAX_DEGREE + 1];
	    int i;
	    for (i = 0; i < MAX_DEGREE + 1; i++) {
	    	normalized_poly[i] = new complex_t();
	    }
	    final complex_t a = poly[degree];
	    if (degree == 0)
	        return 0;
	    if (complex_eq(a, complex_from_real(DoubleDouble.valueOf(0.0))))
	        return solve_poly(degree - 1, poly, results);
	    if (degree > MAX_DEGREE)
	        return -1;
	    if (degree > 2 && stableness_score(poly[degree], poly[degree - 1]).gt(stableness_score(poly[0], poly[1]))) {
	        complex_t rev_poly[] = new complex_t[MAX_DEGREE + 1];
	        for (i = 0; i < MAX_DEGREE+1; i++) {
	        	rev_poly[i] = new complex_t();
	        }
	        int num_results;
	        for (i = 0; i <= degree; ++i)
	            rev_poly[i] = poly[degree - i];
	        num_results = solve_poly(degree, rev_poly, results);
	        for (i = 0; i < num_results; ++i)
	            results[i] = complex_inverse(results[i]);
	        return num_results;
	    }
	    for (i = 0; i < degree; ++i)
	        normalized_poly[i] = complex_div(poly[i], a);
	    normalized_poly[degree] = complex_from_real(DoubleDouble.valueOf(1.0));
	    return solve_normalized_poly(degree, normalized_poly, results);
	}
	
	/* Normalized polynomials have the form of
	 *   x^n + a*x^(n-1) + ..
	 * The coefficient for x^n is one.
	 * solve_normalized_poly does expect to get this coefficient despite it being known.
	 */
	private int solve_normalized_poly(int degree, complex_t[] poly, complex_t[] results) {
	    complex_t shift = complex_mult_real((DoubleDouble.valueOf(-1.0)).divide(DoubleDouble.valueOf(degree)), poly[degree - 1]);
	    complex_t shifted_coefs[] = new complex_t[MAX_DEGREE + 1];
	    int i, num_results;
	    for (i = 0; i < MAX_DEGREE+1; i++) {
	    	shifted_coefs[i] = new complex_t();
	    }
	    calc_shifted_coefs(shift, degree, poly, shifted_coefs);
	    num_results = solve_depressed_poly(degree, shifted_coefs, results);
	    for (i = 0; i < num_results; ++i)
	        results[i] = complex_add(results[i], shift);
	    return num_results;
	}
	
	private void calc_shifted_coefs(complex_t shift, int degree, complex_t[] src, complex_t[] dst) {
		int i,j;
	    DoubleDouble binomials[][] = new DoubleDouble[MAX_DEGREE + 1][MAX_DEGREE + 1];
	    for (i = 0; i < MAX_DEGREE + 1; i++) {
	    	for (j = 0; j < MAX_DEGREE + 1; j++) {
	    		binomials[i][j] = new DoubleDouble();
	    	}
	    }
	    complex_t shift_powers[] = new complex_t[MAX_DEGREE + 1];
	    for (i = 0; i < MAX_DEGREE+1; i++) {
	    	shift_powers[i] = new complex_t();
	    }
	    int dst_i, src_i;
	    for (dst_i = 0; dst_i <= degree; ++dst_i)
	        dst[dst_i] = complex_from_real(DoubleDouble.valueOf(0.0));
	    calc_binomials(degree+1, binomials[0].length, binomials);
	    calc_powers(shift, degree, shift_powers);
	    for (src_i = 0; src_i <= degree; ++src_i)
	        for (dst_i = 0; dst_i <= src_i; ++dst_i)
	            dst[dst_i] = complex_add(dst[dst_i], complex_mult_real(binomials[src_i][dst_i], complex_mult(src[src_i], shift_powers[src_i - dst_i])));
	}


	private void calc_binomials(int num_binoms, int stride, DoubleDouble[][] dst) {
	    int row;
	    for (row = 0; row < num_binoms; ++row) {
	        int col;
	        dst[row][0] = DoubleDouble.valueOf(1);
	        for (col = 1; col < row; ++col) {
	            dst[row][col] = dst[row-1][col - 1].add(dst[row-1][ col]);
	        }
	        dst[row][ row] = DoubleDouble.valueOf(1);
	    }
	}
	
	/* Depressed polynomials have the form of:
	 *   x^n + a*x^(n-2) + ..
	 * The coefficient for x^n is 1 and for x^(n-1) is zero.
	 * So it gets 3 coefficients for a depressed quartic polynom.
	 */
	private int solve_depressed_poly(int degree, complex_t[] poly, complex_t[] results) {
		int i;
		int num;
	    if (degree > 0 && complex_eq(poly[0], complex_from_real(DoubleDouble.valueOf(0.0)))) {
	        results[0] = complex_from_real(DoubleDouble.valueOf(0.0));
	        complex_t poly1[] = new complex_t[poly.length-1];
	        for (i = 0; i < poly1.length; i++) {
	        	poly1[i] = new complex_t();
	        	poly1[i].real = poly[i+1].real;
	        	poly1[i].imag = poly[i+1].imag;
	        }
	        complex_t results1[] = new complex_t[results.length-1];
	        for (i = 0; i < results1.length; i++) {
	        	results1[i] = new complex_t();
	        	results1[i].real = results[i+1].real;
	        	results1[i].imag = results[i+1].imag;
	        }
	        num = solve_depressed_poly(degree-1, poly1, results1);
	        for (i = 0; i < poly1.length; i++) {
	        	poly[i+1].real = poly1[i].real;
	        	poly[i+1].imag = poly1[i].imag;
	        }
	        for (i = 0; i < results1.length; i++) {
	        	results[i+1].real = results1[i].real;
	        	results[i+1].imag = results1[i].imag;
	        }
	        return 1 + num;
	    }
	    switch (degree) {
	    case 4:
	        return solve_depressed_quartic(poly, results);
	    case 3:
	        return solve_depressed_cubic(poly, results);
	    case 2:
	        return solve_depressed_quadratic(poly, results);
	    case 1:
	        results[0] = complex_from_real(DoubleDouble.valueOf(0.0));
	        return 1;
	    case 0:
	        return 0;
	    default:
	        return -1;
	    }
	}
	
	/* Based on http://en.wikipedia.org/wiki/Quartic_function#Quick_and_memorable_solution_from_first_principles */
	private int solve_depressed_quartic(complex_t[] poly, complex_t[] results)
	{
		int i;
		int num;
	    complex_t helper_cubic[] = new complex_t[4];
	    for (i = 0; i < 4; i++) {
	    	helper_cubic[i] = new complex_t();
	    }
	    complex_t helper_results[] = new complex_t[3];
	    complex_t quadratic_factor[] = new complex_t[3];
	    for (i = 0; i < 3; i++) {
	    	helper_results[i] = new complex_t();
	    	quadratic_factor[i] = new complex_t();
	    }
	    complex_t p, c_plus_p_sqr, d_div_p;
	    complex_t e = new complex_t();
	    e.real = poly[0].real;
	    e.imag = poly[0].imag;
	    complex_t d = new complex_t();
	    d.real = poly[1].real;
	    d.imag = poly[1].imag;
	    complex_t c = new complex_t();
	    c.real = poly[2].real;
	    c.imag = poly[2].imag;
	    DoubleDouble helper_norm, t;
	    int num_helper_results, num_results, best_helper_result;

	    if (complex_eq(d, complex_from_real(DoubleDouble.valueOf(0.0))))
	    {
	        int num_quad_results;
	        complex_t quadratic[] = new complex_t[3];
	        for (i = 0; i < 3; i++) {
	        	quadratic[i] = new complex_t();
	        }
	        complex_t quadratic_results[] = new complex_t[2];
	        for (i = 0; i < 2; i++) {
	        	quadratic_results[i] = new complex_t();
	        }
	        quadratic[0] = e;
	        quadratic[1] = c;
	        quadratic[2] = complex_from_real(DoubleDouble.valueOf(1.0));
	        num_quad_results = solve_poly(2, quadratic, quadratic_results);
	        for (i = 0; i < num_quad_results; ++i)
	        {
	            complex_t s = complex_sqrt(quadratic_results[i]);
	            results[2*i] = complex_negate(s);
	            results[2*i + 1] = s;
	        }
	        return 2 * num_quad_results;
	    }

	    helper_cubic[0] = complex_negate(complex_mult(d, d));
	    helper_cubic[1] = complex_add(complex_mult(c, c), complex_mult_real(DoubleDouble.valueOf(-4.0), e));
	    helper_cubic[2] = complex_mult_real(DoubleDouble.valueOf(2.0), c);
	    helper_cubic[3] = complex_from_real(DoubleDouble.valueOf(1.0));
	    num_helper_results = solve_poly(3, helper_cubic, helper_results);
	    if (num_helper_results < 1)
	        return 0;

	    // Pick the result of helper_cubic which has the highest norm,
	    // For more stable calculation. Fixes https://github.com/yairchu/quartic/issues/2
	    best_helper_result = 0;
	    helper_norm = complex_sqr_norm(helper_results[0]);
	    for (i = 1; i < num_helper_results; ++i)
	    {
	        t = complex_sqr_norm(helper_results[i]);
	        if (t.gt(helper_norm))
	        {
	            helper_norm = t;
	            best_helper_result = i;
	        }
	    }

	    p = complex_sqrt(helper_results[best_helper_result]);
	    c_plus_p_sqr = complex_add(c, complex_mult(p, p));
	    d_div_p = complex_div(d, p);
	    quadratic_factor[0] = complex_add(c_plus_p_sqr, complex_negate(d_div_p));
	    quadratic_factor[1] = complex_mult_real(DoubleDouble.valueOf(2.0), p);
	    quadratic_factor[2] = complex_from_real(DoubleDouble.valueOf(2.0));
	    num_results = solve_poly(2, quadratic_factor, results);
	    quadratic_factor[0] = complex_add(c_plus_p_sqr, d_div_p);
	    quadratic_factor[1] = complex_negate(quadratic_factor[1]);
	    complex_t resultsn[] = new complex_t[2];
	    for (i = 0; i < 2; i++) {
	    	resultsn[i] = new complex_t();
	    	resultsn[i].real = results[i + num_results].real;
	    	resultsn[i].imag = results[i + num_results].imag;
	    }
	    num = solve_poly(2, quadratic_factor, resultsn);
	    for (i = 0; i < 2; i++) {
	    	results[i + num_results].real = resultsn[i].real;
	    	results[i + num_results].imag = resultsn[i].imag;
	    }
	    return num_results + num;
	}

	
	/* Based on http://en.wikipedia.org/wiki/Cubic_equation#Cardano.27s_method */
	private int solve_depressed_cubic(complex_t[] poly, complex_t[] results) {
		DoubleDouble oneThird = (DoubleDouble.valueOf(3.0)).reciprocal();
	    complex_t q = poly[0];
	    complex_t p = poly[1];
	    complex_t t;
	    complex_t u = new complex_t();
	    complex_t cubic_root_of_unity = new complex_t();
	    int i;
	    if (complex_eq(p, complex_from_real(DoubleDouble.valueOf(0.0)))) {
	        results[0] = complex_pow_real(complex_negate(q), oneThird);
	        return 1;
	    }
	    t = complex_add(
	        complex_mult_real(DoubleDouble.valueOf(0.25), complex_mult(q, q)),
	        complex_mult_real((DoubleDouble.valueOf(27.0)).reciprocal(), complex_mult(p, complex_mult(p, p))));
	    cubic_root_of_unity.real = DoubleDouble.valueOf(-0.5);
	    cubic_root_of_unity.imag = (DoubleDouble.valueOf(0.5)).multiply((DoubleDouble.valueOf(3.0)).sqrt());
	    for (i = 0; i < 3; ++i) {
	        if (i == 0)
	            u = complex_pow_real(complex_add(complex_mult_real(DoubleDouble.valueOf(-0.5), q), complex_sqrt(t)), oneThird);
	        else
	            u = complex_mult(u, cubic_root_of_unity);
	        results[i] = complex_add(u, complex_div(p, complex_mult_real(DoubleDouble.valueOf(-3.0), u)));
	    }
	    return 3;
	}

	
	private int solve_depressed_quadratic(complex_t[] poly, complex_t[] results) {
	    complex_t t = complex_sqrt(complex_negate(poly[0]));
	    results[0] = complex_negate(t);
	    results[1] = t;
	    return 2;
	}


	
	private void calc_powers(complex_t x, int max_power, complex_t[] dst) {
	    int i;
	    dst[0] = complex_from_real(DoubleDouble.valueOf(1.0));
	    if (max_power >= 1)
	        dst[1] = x;
	    for (i = 2; i <= max_power; ++i)
	        dst[i] = complex_mult(x, dst[i - 1]);
	}


	
	class complex_t {
		DoubleDouble real;
		DoubleDouble imag;
	}
	
	private static boolean complex_eq(complex_t a, complex_t b) {
	    return a.real.equals(b.real) && a.imag.equals(b.imag);
	}

	private complex_t complex_from_real(DoubleDouble x) {
	    complex_t result = new complex_t();
	    result.real = x;
	    result.imag = DoubleDouble.valueOf(0);
	    return result;
	}
	
	private static DoubleDouble stableness_score(complex_t a, complex_t b) {
	    DoubleDouble t = ((complex_sqr_norm(a)).divide(complex_sqr_norm(b))).abs();
	    return t.add(t.reciprocal());
	}
	
	private static DoubleDouble complex_sqr_norm(complex_t x) {
	    return (x.real.multiply(x.real)).add(x.imag.multiply(x.imag));
	}


	private complex_t complex_inverse(complex_t x) {
	    DoubleDouble sqr_norm = complex_sqr_norm(x);
	    complex_t result = new complex_t();
	    result.real = x.real.divide(sqr_norm);
	    result.imag = (x.imag.negate()).divide(sqr_norm);
	    return result;
	}
	
	private complex_t complex_div(complex_t a, complex_t b) {
	    return complex_mult(a, complex_inverse(b));
	}
	
	private complex_t complex_add(complex_t a, complex_t b) {
	    complex_t result = new complex_t();
	    result.real = a.real.add(b.real);
	    result.imag = a.imag.add(b.imag);
	    return result;
	}

	
    private complex_t complex_mult(complex_t a, complex_t b) {
	    complex_t result = new complex_t();
	    result.real = (a.real.multiply(b.real)).subtract(a.imag.multiply(b.imag));
	    result.imag = (a.real.multiply(b.imag)).add(a.imag.multiply(b.real));
	    return result;
	}

    private complex_t complex_mult_real(DoubleDouble c, complex_t x) {
        complex_t result = new complex_t();
        result.real = c.multiply(x.real);
        result.imag = c.multiply(x.imag);
        return result;
    }

    private complex_t complex_negate(complex_t x) {
        complex_t result = new complex_t();
        result.real = x.real.negate();
        result.imag = x.imag.negate();
        return result;
    }
    
    /* Based on http://en.wikipedia.org/wiki/Square_root#Algebraic_formula */
    private complex_t complex_sqrt(complex_t x) {
        DoubleDouble r = (complex_sqr_norm(x)).sqrt();
        complex_t result = new complex_t();
        result.real = ((DoubleDouble.valueOf(0.5)).multiply(r.add(x.real))).sqrt();
        DoubleDouble t = ((DoubleDouble.valueOf(0.5)).multiply(r.subtract(x.real))).sqrt();
        result.imag = x.imag.ge(DoubleDouble.valueOf(0)) ? t : t.negate();
        return result;
    }

    private complex_t complex_pow_real(complex_t x, DoubleDouble power) {
    	    DoubleDouble norm = (((complex_sqr_norm(x)).pow( power))).sqrt();
    	    DoubleDouble arg = (x.imag.atan2(x.real)).multiply(power);
    	    complex_t result = new complex_t();
    	    result.real = norm.multiply(arg.cos());
    	    result.imag = norm.multiply(arg.sin());
    	    return result;
    }


}
