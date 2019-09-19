package gov.nih.mipav.model.algorithms;


/**
 * This is a port of code wirtten by Gerolamo Cardano under the Code Project Open Licence.
 * A copy of the license if found at https://www.codeproject.com/info/cpol10.aspx.
 * The equation is a1*x**3 + b*x**2 + c*x + d = 0
 * If result = 0, a1 = 0
 * If result = 1, x1 is real and x2 and x3 are complex with x3 = complex conjugate of x2
 * If result = 2, there are 3 real roots of which at least 2 are equal
 * If result = 3, 3 real and unequal roots are found
 * @author ilb
 *
 */
public class CubicEquation {
	private double a1;
	private double b;
	private double c;
	private double d;
	private double x1real[];
	private double x2real[];
	private double x2imag[];
	private double x3real[];
	private double x3imag[];
	private int result[];
	
	public CubicEquation() {
	    
	}
	
	public CubicEquation(double a1, double b, double c, double d, double x1real[], 
			double x2real[], double x2imag[], double x3real[], double x3imag[],
			int result[]) {
		this.a1 = a1;
		this.b = b;
		this.c = c;
		this.d = d;
		this.x1real = x1real;
		this.x2real = x2real;
		this.x2imag = x2imag;
		this.x3real = x3real;
		this.x3imag = x3imag;
		this.result = result;
	}
	
	public void selfTest() {
		// x**3 - x + 2 has roots -1.520, 0.760 + i(0.858), and 0.760 - i(0.858)  Expect result = 1
		a1 = 1.0;
		b = 0.0;
		c = -1.0;
		d = 2.0;
		x1real = new double[1];
		x2real = new double[1];
		x2imag = new double[1];
		x3real = new double[1];
		x3imag = new double[1];
		result = new int[1];
		CubicEquation ce = new CubicEquation(a1, b, c, d, x1real, x2real, x2imag,
				x3real, x3imag, result);
		ce.run();
		System.out.println("result = " + result[0]);
		System.out.println("x1 = " + x1real[0]);
		System.out.println("x2 = " + x2real[0] + " + i * " + x2imag[0]);
		System.out.println("x3 = " + x3real[0] + " i * " + x3imag[0]);
		
		// x**3 - 6*x**2 + 11*x - 6 has the roots 1, 2, and 3 with result = 3
		b = -6.0;
		c = 11.0;
		d = -6.0;
		ce = new CubicEquation(a1, b, c, d, x1real, x2real, x2imag,
				x3real, x3imag, result);
		ce.run();
		System.out.println("result = " + result[0]);
		System.out.println("x1 = " + x1real[0]);
		System.out.println("x2 = " + x2real[0] + " + i * " + x2imag[0]);
		System.out.println("x3 = " + x3real[0] + " + i * " + x3imag[0]);
		
		// x**3 - 5*x**2 + 8*x - 4 has the roots 1, 2, and 2 with result = 2
		b = -5.0;
		c = 8.0;
		d = -4.0;
		ce = new CubicEquation(a1, b, c, d, x1real, x2real, x2imag,
				x3real, x3imag, result);
		ce.run();
		System.out.println("result = " + result[0]);
		System.out.println("x1 = " + x1real[0]);
		System.out.println("x2 = " + x2real[0] + " + i * " + x2imag[0]);
		System.out.println("x3 = " + x3real[0] + " + i * " + x3imag[0]);
	}
	
	public void run() {
		double a;
		double p, q, u, v;
		double r, alpha;
		double r13;
		result[0] = 0;
		if (a1 != 0) {
		    a = b/a1;
		    b = c/a1;
		    c = d/a1;
		    
		    p = -(a * a/3.0) + b;
		    q = (2.0/27.0 * a * a * a) - (a * b / 3.0) + c;
		    d = q * q / 4.0 + p * p * p / 27.0;
		    if (Math.abs(d) < 1.0E-12) {
		    	d = 0;
		    }
		    // 3 cases d > 0, d == 0, d < 0
		    if (d > 0) {
		    	u = Xroot(-q/2.0 + Math.sqrt(d), 3.0);
		    	v = Xroot(-q/2.0 - Math.sqrt(d), 3.0);
		    	x1real[0] = u + v - a/3.0;
		    	x2real[0] = -(u + v)/2.0 - a/3.0;
		    	x2imag[0] = Math.sqrt(3.0) / 2.0 * (u - v);
		    	x3real[0] = x2real[0];
		    	x3imag[0] = -x2imag[0];
		    	result[0] = 1;
		    } // if (d > 0)
		    else if (d == 0) {
		    	u = Xroot(-q/2.0, 3.0);
		    	v = u;
		    	x1real[0] = u + v - a/3.0;
		    	x2real[0] = -(u + v) / 2.0 - a / 3.0;
		    	x3real[0] = x2real[0];
		    	x2imag[0] = 0;
		    	x3imag[0] = 0;
		    	result[0] = 2;
		    } // else if (d == 0)
		    else { // d < 0
		    	r = Math.sqrt(- p * p * p / 27.0);
		    	alpha = Math.atan(Math.sqrt(-d) / -q * 2.0);
		    	if (q > 0) {
		    		// If q > 0 the angle becomes 2 * PI - alpha
		    		alpha = 2.0 * Math.PI - alpha;
		    	}
		    	r13 = Xroot(r, 3.0);
		    	x1real[0] = r13 * (Math.cos((6.0 * Math.PI - alpha) / 3.0) + Math.cos(alpha/3.0)) - a / 3.0;
		    	x2real[0] = r13 * (Math.cos((2.0 * Math.PI + alpha) / 3.0) + Math.cos((4.0 * Math.PI - alpha) / 3.0)) - a / 3.0;
		    	x3real[0] = r13 * (Math.cos((4.0 * Math.PI + alpha) / 3.0) + Math.cos((2.0 * Math.PI - alpha) / 3.0)) - a / 3.0;
		    	x2imag[0] = 0;
		    	x3imag[0] = 0;
		    	result[0] = 3;
		    } // else d < 0
		} 
		else {// a1 == 0
			return;
		}
		
	}
	
	 public double Xroot(double a, double x)
     {
         double i = 1;
         if (a < 0)
             i = -1;
         return (i * Math.exp( Math.log(a*i)/x));
     }

}
