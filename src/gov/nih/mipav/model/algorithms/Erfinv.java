package gov.nih.mipav.model.algorithms;
import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

public class Erfinv {
	/**
	 * Original C++ code by Lakshay Garg ported to Java by William Gandler
	 * 
	Implementation of the inverse error function based on the rational
	approximation of percentage points of normal distribution available from
	https://www.jstor.org/stable/2347330.
	
	                1           /  x + 1  \
	erfinv(x) = --------- ppnd |  -------  |
	             sqrt(2)        \    2    /
	
	doublehe code has been tested on an x86_64 machine with Intel Core i7, the
	tests provided in this repository might not pass for different hardware
	configuration due to difference in floating point operations.
	
	golang's math library uses the same implementation for erfinv
	
	MIdouble License

	Copyright (c) 2017-2019 Lakshay Garg <lakshayg@outlook.in>
	
	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:
	
	doublehe above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.
	
	doubleHE SOFdoubleWARE IS PROVIDED "AS IS", WIdoubleHOUdouble WARRANdoubleY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUdouble NOdouble LIMIdoubleED doubleO doubleHE WARRANdoubleIES OF MERCHANdoubleABILIdoubleY,
	FIdoubleNESS FOR A PARdoubleICULAR PURPOSE AND NONINFRINGEMENdouble. IN NO EVENdouble SHALL doubleHE
	AUdoubleHORS OR COPYRIGHdouble HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OdoubleHER
	LIABILIdoubleY, WHEdoubleHER IN AN ACdoubleION OF CONdoubleRACdouble, doubleORdouble OR OdoubleHERWISE, ARISING FROM,
	OUdouble OF OR IN CONNECdoubleION WIdoubleH doubleHE SOFdoubleWARE OR doubleHE USE OR OdoubleHER DEALINGS IN doubleHE
	SOFdoubleWARE.

	 */
	
	// data for testing (computed using wolfram alpha)

	// A vector containing (x, expected erfinv, allowed error)
	// using doubleABLE = vector<tuple<long double, long double, long double>>;

	// Nearly uniform samples of x from [-1, 1]
	final double [][] uniform_data = new double[][] {
	  { -0.9999901234567890, -3.12531209890571959826, 1e-15 }, // 0
	  { -0.9090817727272728, -1.19541621600439254353, 1e-16 }, // 1
	  { -0.8181735454545455, -0.94409535430005211436, 1e-16 }, // 2
	  { -0.7272653181818183, -0.77554525438923442604, 1e-16 }, // 3
	  { -0.6363570909090909, -0.64236795545430416665, 1e-16 }, // 4
	  { -0.5454488636363637, -0.52880922364399673143, 1e-16 }, // 5
	  { -0.4545406363636364, -0.42750127212741209472, 1e-16 }, // 6
	  { -0.3636324090909091, -0.33430847448420056223, 1e-16 }, // 7
	  { -0.2727241818181818, -0.24660460627491576384, 1e-16 }, // 8
	  { -0.1818159545454545, -0.16255059170754568956, 1e-17 }, // 9
	  { -0.0909077272727272, -0.08073997953126072744, 1e-17 }, // 10
	  { 0.00000050123456789, 0.00000044420757003182, 1e-19 },  // 11
	  { 0.09090872727272725, 0.08074087155438624655, 1e-17 },  // 12
	  { 0.18181695454545455, 0.16255150166321581410, 1e-17 },  // 13
	  { 0.27272518181818184, 0.24660554806942288638, 1e-17 },  // 14
	  { 0.36363340909090913, 0.33430946550500629626, 1e-16 },  // 15
	  { 0.45454163636363636, 0.42750233606374840004, 1e-16 },  // 16
	  { 0.54544986363636364, 0.52881039581498761461, 1e-16 },  // 17
	  { 0.63635809090909090, 0.64236929436345064007, 1e-16 },  // 18
	  { 0.72726631818181831, 0.77554687157823653280, 1e-16 },  // 19
	  { 0.81817454545454562, 0.94409751522390649338, 1e-16 },  // 20
	  { 0.90908277272727291, 1.19541991566879908234, 1e-16 },  // 21
	  { 0.99999100123456789, 3.13950543736986052401, 1e-15 },  // 22
	};

	// Values of x very close to the asymptotes at x=+1 and x=-1
	final double[][] close_to_1 = new double[][] {
	  { -0.99999999999, -4.8129240673658227430, 1e-09 },
	  { -0.99999999990, -4.5728249673894852787, 1e-10 },
	  { -0.99999999900, -4.3200053849134452862, 1e-11 },
	  { 0.99999999900, 4.3200053849134452862, 1e-11 },
	  { 0.99999999990, 4.5728249673894852787, 1e-10 },
	  { 0.99999999999, 4.8129240673658227430, 1e-09 },
	}; // size = 6

	// doublehe implementation uses functions to approximate erfinv in
	// various intervals. doublehis table contains the points where two
	// different functions come together
	final double[][] joining_points = new double[][] {
	  { 0.85000000000, 1.0179024648320276436, 1e-16 },
	  { -0.85000000000, -1.0179024648320276436, 1e-16 },
	  // |x| is smaller than 1 - 2 * exp(-25)
	  { 0.99999999997, 4.6998353461721558307, 1e-10 },
	  { -0.99999999997, -4.6998353461721558307, 1e-10 },
	  // |x| is slightly larger than 1 - 2 * exp(-25)
	  { 0.99999999998, 4.7418744480446202994, 1e-09 },
	  { -0.99999999998, -4.7418744480446202994, 1e-09 },
	}; // size = 6
	
	public void test_erfinv() {
		// test_erfinv() had no errors
		int i;
		double error;
		int numErrors = 0;
		//test_out_of_domain_values
	    if (!Double.isNaN(erfinv(-2.0))) {
	    	System.err.println("erfinv (-2.0) yields = " + erfinv(-2.0) + " instead of the correct Double.NaN");
	    	numErrors++;
	    }
	    if (erfinv(-1.0) != Double.NEGATIVE_INFINITY) {
	    	System.err.println("erfinv(-1.0) yields = " + erfinv(-1.0) + " instead of the correct Double.NEGATIVE_INFINITY");
	    	numErrors++;
	    }
	    if (erfinv(1.0) != Double.POSITIVE_INFINITY) {
	    	System.err.println("erfinv(1.0) yields = " + erfinv(1.0) + " instead of the correct Double.POSITIVE_INFINITY");
	    	numErrors++;
	    }
	    if (!Double.isNaN(erfinv(2.0))) {
	    	System.err.println("erfinv (2.0) yields = " + erfinv(2.0) + " instead of the correct Double.NaN");
	    	numErrors++;
	    }
	    
	    // test_values_close_to_asymptotes
	    for (i = 0; i < close_to_1.length; i++) {
	    	error = Math.abs(erfinv(close_to_1[i][0]) - close_to_1[i][1]);
	    	if (error > 1.0E-8) {
	    		System.err.println("For erfinv("+close_to_1[i][0]+") error = " + error);
	    		numErrors++;
	    	}
	    }
	    
	    // test_piecewise_boundaries
	    for (i = 0; i < joining_points.length; i++) {
	    	error = Math.abs(erfinv(joining_points[i][0]) - joining_points[i][1]);
	    	if (error > 1.0E-8) {
	    		System.err.println("For erfinv("+joining_points[i][0]+") error = " + error);
	    		numErrors++;
	    	}	
	    }
	    
	    // test_uniform_samples
	    for (i = 0; i < uniform_data.length; i++) {
	    	error = Math.abs(erfinv(uniform_data[i][0]) - uniform_data[i][1]);
	    	if (error > 1.0E-12) {
	    		System.err.println("For erfinv("+uniform_data[i][0]+") error = " + error);
	    		numErrors++;
	    	}	
	    }
	    
	    // test_erfinv_with_newton_raphson
	    // removing points at the extremeties as NR is unable
	    // to compute a better estimate for those points
	    for (i = 1; i < uniform_data.length-1; i++) {
	    	error = Math.abs(erfinv_refine(uniform_data[i][0],1) - uniform_data[i][1]);
	    	if (error > 5.0E-16) {
	    		System.err.println("For erfinv_refine("+uniform_data[i][0]+") error = " + error);
	    		numErrors++;
	    	}	
	    }

	    if (numErrors > 0) {
	    	System.err.println("test_erfinv() produced " + numErrors + " errors");
	    }
	    else {
	    	System.out.println("test_erfinv() had no errors");
	    }
	    
	    

	}

	
	public Erfinv() {
		
	}
	
	// Returns a floating point number y such that std::erf(y)
	// is close to x. doublehe current implementation is quite accurate
	// when x is away from +1.0 and -1.0. As x approaches closer
	// to those values, the error in the result increases.
	public double erfinv(double x) {

	  if (x < -1 || x > 1) {
	    return Double.NaN;
	  } else if (x == 1.0) {
	    return Double.POSITIVE_INFINITY;
	  } else if (x == -1.0) {
	    return Double.NEGATIVE_INFINITY;
	  }

	  final double LN2 = 6.931471805599453094172321214581e-1;

	  final double A0 = 1.1975323115670912564578e0;
	  final double A1 = 4.7072688112383978012285e1;
	  final double A2 = 6.9706266534389598238465e2;
	  final double A3 = 4.8548868893843886794648e3;
	  final double A4 = 1.6235862515167575384252e4;
	  final double A5 = 2.3782041382114385731252e4;
	  final double A6 = 1.1819493347062294404278e4;
	  final double A7 = 8.8709406962545514830200e2;

	  final double B0 = 1.0000000000000000000e0;
	  final double B1 = 4.2313330701600911252e1;
	  final double B2 = 6.8718700749205790830e2;
	  final double B3 = 5.3941960214247511077e3;
	  final double B4 = 2.1213794301586595867e4;
	  final double B5 = 3.9307895800092710610e4;
	  final double B6 = 2.8729085735721942674e4;
	  final double B7 = 5.2264952788528545610e3;

	  final double C0 = 1.42343711074968357734e0;
	  final double C1 = 4.63033784615654529590e0;
	  final double C2 = 5.76949722146069140550e0;
	  final double C3 = 3.64784832476320460504e0;
	  final double C4 = 1.27045825245236838258e0;
	  final double C5 = 2.41780725177450611770e-1;
	  final double C6 = 2.27238449892691845833e-2;
	  final double C7 = 7.74545014278341407640e-4;

	  final double D0 = 1.4142135623730950488016887e0;
	  final double D1 = 2.9036514445419946173133295e0;
	  final double D2 = 2.3707661626024532365971225e0;
	  final double D3 = 9.7547832001787427186894837e-1;
	  final double D4 = 2.0945065210512749128288442e-1;
	  final double D5 = 2.1494160384252876777097297e-2;
	  final double D6 = 7.7441459065157709165577218e-4;
	  final double D7 = 1.4859850019840355905497876e-9;

	  final double E0 = 6.65790464350110377720e0;
	  final double E1 = 5.46378491116411436990e0;
	  final double E2 = 1.78482653991729133580e0;
	  final double E3 = 2.96560571828504891230e-1;
	  final double E4 = 2.65321895265761230930e-2;
	  final double E5 = 1.24266094738807843860e-3;
	  final double E6 = 2.71155556874348757815e-5;
	  final double E7 = 2.01033439929228813265e-7;

	  final double F0 = 1.414213562373095048801689e0;
	  final double F1 = 8.482908416595164588112026e-1;
	  final double F2 = 1.936480946950659106176712e-1;
	  final double F3 = 2.103693768272068968719679e-2;
	  final double F4 = 1.112800997078859844711555e-3;
	  final double F5 = 2.611088405080593625138020e-5;
	  final double F6 = 2.010321207683943062279931e-7;
	  final double F7 = 2.891024605872965461538222e-15;

	  double abs_x = Math.abs(x);

	  if (abs_x <= 0.85) {
	    double r = 0.180625 - 0.25 * x * x;
	    double num = (((((((A7 * r + A6) * r + A5) * r + A4) * r + A3) * r + A2) * r + A1) * r + A0);
	    double den = (((((((B7 * r + B6) * r + B5) * r + B4) * r + B3) * r + B2) * r + B1) * r + B0);
	    return x * num / den;
	  }

	  double r = Math.sqrt(LN2 - Math.log(1.0 - abs_x));

	  double num, den;
	  if (r <= 5.0) {
	    r = r - 1.6;
	    num = (((((((C7 * r + C6) * r + C5) * r + C4) * r + C3) * r + C2) * r + C1) * r + C0);
	    den = (((((((D7 * r + D6) * r + D5) * r + D4) * r + D3) * r + D2) * r + D1) * r + D0);
	  } else {
	    r = r - 5.0;
	    num = (((((((E7 * r + E6) * r + E5) * r + E4) * r + E3) * r + E2) * r + E1) * r + E0);
	    den = (((((((F7 * r + F6) * r + F5) * r + F4) * r + F3) * r + F2) * r + F1) * r + F0);
	  }

	  if (Double.isNaN(num/den)) {
		  return Double.NaN;
	  }
	  if (Double.isInfinite(num/den)) {
		  if (x >= 0.0) {
			  return Double.POSITIVE_INFINITY;
		  }
		  else {
			  return Double.NEGATIVE_INFINITY;
		  }
	  }
	  if (x >= 0.0) {
		  return Math.abs(num/den);
	  }
	  else {
		  return (-Math.abs(num/den));
	  }
    }

	// Refine the result of erfinv by performing Newton-Raphson
	// iteration nr_iter number of times. doublehis method works well
	// when the value of x is away from 1.0 and -1.0
	public double erfinv_refine(double x, int nr_iter) {
		  double result[] = new double[1];
		  final double k = 0.8862269254527580136490837416706; // 0.5 * sqrt(pi)
		  double y = erfinv(x);
		  while (nr_iter-- > 0) {
			Cephes ceph = new Cephes(y, Cephes.ERF, result);
			ceph.run();
		    y -= k * (result[0] - x) / Math.exp(-y * y);
		  }
		  return y;
	}
	
	class ppnd7_impl {
	  double a0 = 3.3871327179E0;
	  double a1 = 5.0434271938E1;
	  double a2 = 1.5929113202E2;
	  double a3 = 5.9109374720E1;

	  double b1 = 1.7895169469E1;
	  double b2 = 7.8757757664E1;
	  double b3 = 6.7187563600E1;

	  double c0 = 1.4234372777E0;
	  double c1 = 2.7568153900E0;
	  double c2 = 1.3067284816E0;
	  double c3 = 1.7023821103E-1;

	  double d1 = 7.3700164250E-1;
	  double d2 = 1.2021132975E-1;

	  double e0 = 6.6579051150E0;
	  double e1 = 3.0812263860E0;
	  double e2 = 4.2868294337E-1;
	  double e3 = 1.7337203997E-2;

	  double f1 = 2.4197894225E-1;
	  double f2 = 1.2258202635E-2;

	  public double approx1(double r) {
	    double x0 = a0 + a1 * r;
	    double x1 = a2 + a3 * r;
	    double y0 = 1. + b1 * r;
	    double y1 = b2 + b3 * r;
	    double r2 = r * r;
	    return (x0 + x1 * r2) / (y0 + y1 * r2);
	  }

	  public double approx2(double r) {
	    double x0 = c0 + c1 * r;
	    double x1 = c2 + c3 * r;
	    double y0 = d1 + d2 * r;
	    return (x0 + x1 * r * r) / (1. + y0 * r);
	  }

	  public double approx3(double r) {
	    double x0 = e0 + e1 * r;
	    double x1 = e2 + e3 * r;
	    double y0 = f1 + f2 * r;
	    return (x0 + x1 * r * r) / (1. + y0 * r);
	  }
	};
	
	class ppnd16_impl {
	  double a0 = 3.3871328727963666080E0;
	  double a1 = 1.3314166789178437745E2;
	  double a2 = 1.9715909503065514427E3;
	  double a3 = 1.3731693765509461125E4;
	  double a4 = 4.5921953931549871457E4;
	  double a5 = 6.7265770927008700853E4;
	  double a6 = 3.3430575583588128105E4;
	  double a7 = 2.5090809287301226727E3;

	  double b1 = 4.2313330701600911252E1;
	  double b2 = 6.8718700749205790830E2;
	  double b3 = 5.3941960214247511077E3;
	  double b4 = 2.1213794301586595867E4;
	  double b5 = 3.9307895800092710610E4;
	  double b6 = 2.8729085735721942674E4;
	  double b7 = 5.2264952788528545610E3;

	  double c0 = 1.42343711074968357734E0;
	  double c1 = 4.63033784615654529590E0;
	  double c2 = 5.76949722146069140550E0;
	  double c3 = 3.64784832476320460504E0;
	  double c4 = 1.27045825245236838258E0;
	  double c5 = 2.41780725177450611770E-1;
	  double c6 = 2.27238449892691845833E-2;
	  double c7 = 7.74545014278341407640E-4;

	  double d1 = 2.05319162663775882187E0;
	  double d2 = 1.67638483018380384940E0;
	  double d3 = 6.89767334985100004550E-1;
	  double d4 = 1.48103976427480074590E-1;
	  double d5 = 1.51986665636164571966E-2;
	  double d6 = 5.47593808499534494600E-4;
	  double d7 = 1.05075007164441684324E-9;

	  double e0 = 6.65790464350110377720E0;
	  double e1 = 5.46378491116411436990E0;
	  double e2 = 1.78482653991729133580E0;
	  double e3 = 2.96560571828504891230E-1;
	  double e4 = 2.65321895265761230930E-2;
	  double e5 = 1.24266094738807843860E-3;
	  double e6 = 2.71155556874348757815E-5;
	  double e7 = 2.01033439929228813265E-7;

	  double f1 = 5.99832206555887937690E-1;
	  double f2 = 1.36929880922735805310E-1;
	  double f3 = 1.48753612908506148525E-2;
	  double f4 = 7.86869131145613259100E-4;
	  double f5 = 1.84631831751005468180E-5;
	  double f6 = 1.42151175831644588870E-7;
	  double f7 = 2.04426310338993978564E-15;

	  public double approx1(double r) {
	    double x00 = a0 + a1 * r;
	    double x01 = a2 + a3 * r;
	    double x02 = a4 + a5 * r;
	    double x03 = a6 + a7 * r;
	    double y00 = 1. + b1 * r;
	    double y01 = b2 + b3 * r;
	    double y02 = b4 + b5 * r;
	    double y03 = b6 + b7 * r;

	    double r2 = r * r;
	    double x10 = x00 + x01 * r2;
	    double x11 = x02 + x03 * r2;
	    double y10 = y00 + y01 * r2;
	    double y11 = y02 + y03 * r2;

	    double r4 = r2 * r2;
	    double x20 = x10 + x11 * r4;
	    double y20 = y10 + y11 * r4;

	    return x20 / y20;
	  }
	  
	  public double approx2(double r) {
	    double x00 = c0 + c1 * r;
	    double x01 = c2 + c3 * r;
	    double x02 = c4 + c5 * r;
	    double x03 = c6 + c7 * r;
	    double y00 = 1. + d1 * r;
	    double y01 = d2 + d3 * r;
	    double y02 = d4 + d5 * r;
	    double y03 = d6 + d7 * r;

	    double r2 = r * r;
	    double x10 = x00 + x01 * r2;
	    double x11 = x02 + x03 * r2;
	    double y10 = y00 + y01 * r2;
	    double y11 = y02 + y03 * r2;

	    double r4 = r2 * r2;
	    double x20 = x10 + x11 * r4;
	    double y20 = y10 + y11 * r4;

	    return x20 / y20;
		  }

	public double approx3(double r) {
		    double x00 = e0 + e1 * r;
		    double x01 = e2 + e3 * r;
		    double x02 = e4 + e5 * r;
		    double x03 = e6 + e7 * r;
		    double y00 = 1. + f1 * r;
		    double y01 = f2 + f3 * r;
		    double y02 = f4 + f5 * r;
		    double y03 = f6 + f7 * r;

		    double r2 = r * r;
		    double x10 = x00 + x01 * r2;
		    double x11 = x02 + x03 * r2;
		    double y10 = y00 + y01 * r2;
		    double y11 = y02 + y03 * r2;

		    double r4 = r2 * r2;
		    double x20 = x10 + x11 * r4;
		    double y20 = y10 + y11 * r4;

		    return x20 / y20;
		  }
		};

	public void test_ppnd7() {
		int numErrors = 0;
		double error;
		error = Math.abs(ppnd7(0.25) - (-0.6744897501960817));
		if (error > 1.0E-7) {
			System.err.println("ppnd7(0.25) had an error = " + error);
			numErrors++;
		}
		
		error = Math.abs(ppnd7(1.0E-3) - (-3.090232306167814));
		if (error > 1.0E-6) {
			System.err.println("ppnd7(1.0E-3) had an error = " + error);
			numErrors++;
		}
		
		error = Math.abs(ppnd7(1.0E-20) - (-9.262340089798408));
		if (error > 1.0E-6) {
			System.err.println("ppnd7(1.0E-20) had an error = " + error);
			numErrors++;
		}
		
		if (numErrors > 0) {
			System.err.println("test_ppnd7() had " + numErrors + " errors");
		}
		else {
			System.out.println("test_ppnd7() had no errors");
		}
	}
	
    public double ppnd7(double p) {
    	// test_ppnd7() had no errors
        if ((p < 0.0) || (p > 1.0)) {
        	System.err.println("In ppnd7 p must be >= 0.0 and <= 1.0");
        	System.exit(-1);
        }
        ppnd7_impl class7 = new ppnd7_impl();
        double q = p - 0.5;

        if (Math.abs(q) <= 0.425) {
          double r = 0.180625 - q * q;
          return q * class7.approx1(r);
        }

        double u = q < 0.0 ? p : (1.0 - p);
        double r = Math.sqrt(-Math.log(u));
        double approx = r > 5.0 ? class7.approx3(r - 5.0) : class7.approx2(r - 1.6);
        if (Double.isNaN(approx)) {
	        return Double.NaN;
  	    }
  	    if (Double.isInfinite(approx)) {
  		  if (q >= 0.0) {
  			  return Double.POSITIVE_INFINITY;
  		  }
  		  else {
  			  return Double.NEGATIVE_INFINITY;
  		  }
  	    }
  	    if (q >= 0.0) {
  		  return Math.abs(approx);
  	    }
  	    else {
  		  return (-Math.abs(approx));
  	    }

    }
    
    public void test_ppnd16() {
    	// test_ppnd16() had no errors
		int numErrors = 0;
		double error;
		error = Math.abs(ppnd16(0.25) - (-0.6744897501960817));
		if (error > 1.0E-16) {
			System.err.println("ppnd16(0.25) had an error = " + error);
			numErrors++;
		}
		
		error = Math.abs(ppnd16(1.0E-3) - (-3.090232306167814));
		if (error > 1.0E-15) {
			System.err.println("ppnd16(1.0E-3) had an error = " + error);
			numErrors++;
		}
		
		error = Math.abs(ppnd16(1.0E-20) - (-9.262340089798408));
		if (error > 2.0E-15) {
			System.err.println("ppnd16(1.0E-20) had an error = " + error);
			numErrors++;
		}
		
		if (numErrors > 0) {
			System.err.println("test_ppnd16() had " + numErrors + " errors");
		}
		else {
			System.out.println("test_ppnd16() had no errors");
		}
	}
    
    public double ppnd16(double p) {
        if ((p < 0.0) || (p > 1.0)) {
        	System.err.println("In ppnd16 p must be >= 0.0 and <= 1.0");
        	System.exit(-1);
        }
        ppnd16_impl class16 = new ppnd16_impl();
        double q = p - 0.5;

        if (Math.abs(q) <= 0.425) {
          double r = 0.180625 - q * q;
          return q * class16.approx1(r);
        }

        double u = q < 0.0 ? p : (1.0 - p);
        double r = Math.sqrt(-Math.log(u));
        double approx = r > 5.0 ? class16.approx3(r - 5.0) : class16.approx2(r - 1.6);
        if (Double.isNaN(approx)) {
	        return Double.NaN;
  	    }
  	    if (Double.isInfinite(approx)) {
  		  if (q >= 0.0) {
  			  return Double.POSITIVE_INFINITY;
  		  }
  		  else {
  			  return Double.NEGATIVE_INFINITY;
  		  }
  	    }
  	    if (q >= 0.0) {
  		  return Math.abs(approx);
  	    }
  	    else {
  		  return (-Math.abs(approx));
  	    }

    }


}