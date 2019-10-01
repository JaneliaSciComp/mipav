package gov.nih.mipav.model.algorithms;

/**
 * Solution of cubic, quartic, and quintic equations
 * Ported from code written by (c) Khashin S.I. http://math.ivanovo.ac.ru/dalgebra/Khashin/index.html
// khash2 (at) gmail.com
// Thanks to Alexandr Rakhmanin <rakhmanin (at) gmail.com>
// public domain
 * @author ilb
 *
 */

public class Poly {
	
	private final double TwoPi = 6.28318530717958648;
	private final double eps = 1.0E-14;
	
	public Poly() {
	    
	}
	
	public void selfTest() {
		double x[] = new double[5];
		int type;
		type = SolveP2(x, -4.0, 3.0);
		System.out.println("Solved quadratic answer type = " + type + " x[0] = " + x[0]  + " x[1] = " + x[1]);
		System.out.println("True quadratic answer type = 2 x[0] = 1 x[1] = 3");
		type = SolveP2(x, -4.0, 10.0);
		System.out.println("Solved quadratic answer type = " + type + " x[0] = " + x[0]  + " x[1] = " + x[1]);
		System.out.println("True quadratic answer type = 0 x[0] = 2 x[1] = 2.4494897");
		
		type = SolveP3(x, 0.0, -1.0, 2.0);
		System.out.println("Solved cubic answer type = " + type + " x[0] = " + x[0] + " x[1] = " + x[1] + " x[2] = " + x[2]);
		System.out.println("True cubic answer type = 1 x[0] = -1.520 x[1] = .760 x[2] = 0.858");
		
		type = SolveP3(x,-5.0,8.0,-4.0);
		System.out.println("Solved cubic answer type = " + type + " x[0] = " + x[0] + " x[1] = " + x[1] + " x[2] = " + x[2]);
		System.out.println("True cubic answer type = 2 x[0] = 1.0 x[1] = 2.0 x[2] = 2.0");
		
		type = SolveP3(x, -6.0, 11.0, -6.0);
		System.out.println("Solved cubic answer type = " + type + " x[0] = " + x[0] + " x[1] = " + x[1] + " x[2] = " + x[2]);
		System.out.println("True cubic answer type = 3 x[0] = 1.0 x[1] = 2.0 x[2] = 3.0");
		
		// x**4	+ 6x**3 - 5x**2 - 10x - 3 = 0
	    // has the roots (-7 +- sqrt(37))/2, (1 +- sqrt(5))/2
		// -0.4586187348508902, -6.54138126514911, 1.618033988749895, -0.6180339887498948
		type = SolveP4(x, 6.0, -5.0, -10.0, -3.0);
		System.out.println("Solved quartic answer type = " + type + " x[0] = " + x[0] + " x[1] = " + x[1] + " x[2] = " + x[2] + " x[3] = " + x[3]);
		System.out.println("True quartic answer type = 4 x[0] = -0.4586187348508902 x[1] = -6.54138126514911 x[2] = 1.618033988749895 x[3] = -0.6180339887498948");
		
		// x**4 + 6x**3 + 7x**2 -7x - 12 = 0
		// has the roots x = 1.1478990357,   x = -1.5739495179 +/- 0.3689894075 i,   and   x = -4.0000000000, 
		// x = -4 is an exact root
		type = SolveP4(x, 6.0, 7.0, -7.0, -12.0);
		System.out.println("Solved quartic answer type = " + type + " x[0] = " + x[0] + " x[1] = " + x[1] + " x[2] = " + x[2] + " x[3] = " + x[3]);
		System.out.println("True quartic answer type = 2 x[0] = 1.1478990357 x[1] = -4.0 x[2] = -1.5739495179 x[3] = 0.3689894075");
		
		// x**4 + 2x**3 + 3x**2 + 4x + 5 = 0
		// has roots 0.28781547955765   +- i*1.41609308017191,  -1.287815479557650   +- i*0.857896758328492
		type = SolveP4(x, 2.0, 3.0, 4.0, 5.0);
		System.out.println("Solved quartic answer type = " + type + " x[0] = " + x[0] + " x[1] = " + x[1] + " x[2] = " + x[2] + " x[3] = " + x[3]);
		System.out.println("True quartic answer type = 0 x[0] = 0.28781547955765 x[1] = 1.41609308017191 x[2] = -1.287815479557650 x[3] = 0.857896758328492");
		
		// x**5 - 15x**4 + 85x**3 - 225x**2 + 274x - 120 has roots 1, 2, 3, 4, 5
		type = SolveP5(x, -15.0, 85.0, -225.0, 274.0, -120.0);
		System.out.println("Solved quintic answer type = " + type + " x[0] = " + x[0] + " x[1] = " + x[1] + " x[2] = " + x[2] + " x[3] = " + x[3] + " x[4] = " + x[4]);
		System.out.println("True quintic answer type = 5 x[0] = 1.0  x[1] = 2.0 x[2] = 3.0 x[3] = 4.0 x[5] = 5.0");
	}
	
	//=============================================================================
	// _root3, root3 from http://prografix.narod.ru
	//=============================================================================
	private double _root3 ( double x )
	{
	    double s = 1.;
	    while ( x < 1. )
	    {
	        x *= 8.;
	        s *= 0.5;
	    }
	    while ( x > 8. )
	    {
	        x *= 0.125;
	        s *= 2.;
	    }
	    double r = 1.5;
	    r -= 1./3. * ( r - x / ( r * r ) );
	    r -= 1./3. * ( r - x / ( r * r ) );
	    r -= 1./3. * ( r - x / ( r * r ) );
	    r -= 1./3. * ( r - x / ( r * r ) );
	    r -= 1./3. * ( r - x / ( r * r ) );
	    r -= 1./3. * ( r - x / ( r * r ) );
	    return r * s;
	}

	private double root3 ( double x )
	{
	    if ( x > 0 ) return _root3 ( x ); else
	    if ( x < 0 ) return-_root3 (-x ); else
	    return 0.;
	}


	// x - array of size 2
	// return 2: 2 real roots x[0], x[1]
	// return 0: pair of complex roots: x[0]켲*x[1]
	public int   SolveP2(double x[], double a, double b) {			// solve equation x^2 + a*x + b = 0
		double D = 0.25*a*a - b;
		if (D >= 0) {
			D = Math.sqrt(D);
			x[0] = -0.5*a + D;
			x[1] = -0.5*a - D;
			return 2;
		}
		x[0] = -0.5*a;
		x[1] = Math.sqrt(-D);
		return 0;
	}
	//---------------------------------------------------------------------------
	// x - array of size 3
	// In case 3 real roots: => x[0], x[1], x[2], return 3
//	         2 real roots: x[0], x[1],          return 2
//	         1 real root : x[0], x[1] � i*x[2], return 1
	public int SolveP3(double x[],double a,double b,double c) {	// solve cubic equation x^3 + a*x^2 + b*x + c = 0
		double a2 = a*a;
	    double q  = (a2 - 3*b)/9; 
		double r  = (a*(2*a2-9*b) + 27*c)/54;
		// equation x^3 + q*x + r = 0
	    double r2 = r*r;
		double q3 = q*q*q;
		double A,B;
		if (r2 <= (q3 + eps)) {//<<-- FIXED!
			double t=r/Math.sqrt(q3);
			if( t<-1) t=-1;
			if( t> 1) t= 1;
	        t=Math.acos(t);
	        a/=3; q=-2*Math.sqrt(q);
	        x[0]=q*Math.cos(t/3)-a;
	        x[1]=q*Math.cos((t+TwoPi)/3)-a;
	        x[2]=q*Math.cos((t-TwoPi)/3)-a;
	        return(3);
	    } else {
	        //A =-pow(fabs(r)+sqrt(r2-q3),1./3); 
	        A =-root3(Math.abs(r)+Math.sqrt(r2-q3)); 
			if( r<0 ) A=-A;
			if (A == 0.0) {
				B = 0.0;
			}
			else {
				B = q/A;
			}
			a/=3;
			x[0] =(A+B)-a;
	        x[1] =-0.5*(A+B)-a;
	        x[2] = 0.5*Math.sqrt(3.)*(A-B);
			if(Math.abs(x[2])<eps) { x[2]=x[1]; return(2); }
	        return(1);
	    }
	}// SolveP3(double *x,double a,double b,double c) {	
	//---------------------------------------------------------------------------
	// a>=0!
	private void  CSqrt( double x, double y, double a[], double b[]) // returns:  a+i*s = sqrt(x+i*y)
	{
		double r  = Math.sqrt(x*x+y*y);
		if( y==0 ) { 
			r = Math.sqrt(r);
			if(x>=0) { a[0]=r; b[0]=0; } else { a[0]=0; b[0]=r; }
		} else {		// y != 0
			a[0] = Math.sqrt(0.5*(x+r));
			b[0] = 0.5*y/a[0];
		}
	}
	//---------------------------------------------------------------------------
	private int   SolveP4Bi(double x[], double b, double d)	// solve equation x^4 + b*x^2 + d = 0
	{
		double D = b*b-4*d;
		if( D>=0 ) 
		{
			double sD = Math.sqrt(D);
			double x1 = (-b+sD)/2;
			double x2 = (-b-sD)/2;	// x2 <= x1
			if( x2>=0 )				// 0 <= x2 <= x1, 4 real roots
			{
				double sx1 = Math.sqrt(x1);
				double sx2 = Math.sqrt(x2);
				x[0] = -sx1;
				x[1] =  sx1;
				x[2] = -sx2;
				x[3] =  sx2;
				return 4;
			}
			if( x1 < 0 )				// x2 <= x1 < 0, two pair of imaginary roots
			{
				double sx1 = Math.sqrt(-x1);
				double sx2 = Math.sqrt(-x2);
				x[0] =    0;
				x[1] =  sx1;
				x[2] =    0;
				x[3] =  sx2;
				return 0;
			}
			// now x2 < 0 <= x1 , two real roots and one pair of imginary root
				double sx1 = Math.sqrt( x1);
				double sx2 = Math.sqrt(-x2);
				x[0] = -sx1;
				x[1] =  sx1;
				x[2] =    0;
				x[3] =  sx2;
				return 2;
		} else { // if( D < 0 ), two pair of compex roots
			double sD2 = 0.5*Math.sqrt(-D);
			double x0[] = new double[1];
			double x1[] = new double[1];
			double x2[] = new double[1];
			double x3[] = new double[1];
			CSqrt(-0.5*b, sD2, x0,x1);
			CSqrt(-0.5*b,-sD2, x2,x3);
			x[0] = x0[0];
			x[1]= x1[0];
			x[2] = x2[0];
			x[3] = x3[0];
			return 0;
		} // if( D>=0 ) 
	} // SolveP4Bi(double *x, double b, double d)	// solve equation x^4 + b*x^2 d
	//---------------------------------------------------------------------------
	private void SWAP(double a[], double b[]) {
		double t;
			t=b[0]; 
			b[0]=a[0]; 
			a[0]=t; 
	}
	private void  dblSort3( double a[], double b[], double c[]) // make: a <= b <= c
	{
		if( a[0]>b[0] ) SWAP(a,b);	// now a<=b
		if( c[0]<b[0] ) {
			SWAP(b,c);			// now a<=b, b<=c
			if( a[0]>b[0] ) SWAP(a,b);// now a<=b
		}
	}
	//---------------------------------------------------------------------------
	private int   SolveP4De(double x[], double b, double c, double d)	// solve equation x^4 + b*x^2 + c*x + d
	{
		//if( c==0 ) return SolveP4Bi(x,b,d); // After that, c!=0
		if( Math.abs(c)<1e-14*(Math.abs(b)+Math.abs(d)) ) return SolveP4Bi(x,b,d); // After that, c!=0

		int res3 = SolveP3( x, 2*b, b*b-4*d, -c*c);	// solve resolvent
		// by Viet theorem:  x1*x2*x3=-c*c not equals to 0, so x1!=0, x2!=0, x3!=0
		if( res3>1 )	// 3 real roots, 
		{	
			double x0[] = new double[]{x[0]};
			double x1[] = new double[]{x[1]};
			double x2[] = new double[]{x[2]};
			dblSort3(x0, x1, x2);	// sort roots to x[0] <= x[1] <= x[2]
			x[0] = x0[0];
			x[1] = x1[0];
			x[2] = x2[0];
			// Note: x[0]*x[1]*x[2]= c*c > 0
			if( x[0] > 0) // all roots are positive
			{
				double sz1 = Math.sqrt(x[0]);
				double sz2 = Math.sqrt(x[1]);
				double sz3 = Math.sqrt(x[2]);
				// Note: sz1*sz2*sz3= -c (and not equal to 0)
				if( c>0 )
				{
					x[0] = (-sz1 -sz2 -sz3)/2;
					x[1] = (-sz1 +sz2 +sz3)/2;
					x[2] = (+sz1 -sz2 +sz3)/2;
					x[3] = (+sz1 +sz2 -sz3)/2;
					return 4;
				}
				// now: c<0
				x[0] = (-sz1 -sz2 +sz3)/2;
				x[1] = (-sz1 +sz2 -sz3)/2;
				x[2] = (+sz1 -sz2 -sz3)/2;
				x[3] = (+sz1 +sz2 +sz3)/2;
				return 4;
			} // if( x[0] > 0) // all roots are positive
			// now x[0] <= x[1] < 0, x[2] > 0
			// two pair of complex roots
			double sz1 = Math.sqrt(-x[0]);
			double sz2 = Math.sqrt(-x[1]);
			double sz3 = Math.sqrt( x[2]);

			if( c>0 )	// sign = -1
			{
				x[0] = -sz3/2;					
				x[1] = ( sz1 -sz2)/2;		// x[0]켲*x[1]
				x[2] =  sz3/2;
				x[3] = (-sz1 -sz2)/2;		// x[2]켲*x[3]
				return 0;
			}
			// now: c<0 , sign = +1
			x[0] =   sz3/2;
			x[1] = (-sz1 +sz2)/2;
			x[2] =  -sz3/2;
			x[3] = ( sz1 +sz2)/2;
			return 0;
		} // if( res3>1 )	// 3 real roots, 
		// now resoventa have 1 real and pair of compex roots
		// x[0] - real root, and x[0]>0, 
		// x[1]켲*x[2] - complex roots, 
		// x[0] must be >=0. But one times x[0]=~ 1e-17, so:
		if (x[0] < 0) x[0] = 0;
		double sz1 = Math.sqrt(x[0]);
		double szr[] = new double[1];
		double szi[] = new double[1];;
		CSqrt(x[1], x[2], szr, szi);  // (szr+i*szi)^2 = x[1]+i*x[2]
		if( c>0 )	// sign = -1
		{
			x[0] = -sz1/2-szr[0];			// 1st real root
			x[1] = -sz1/2+szr[0];			// 2nd real root
			x[2] = sz1/2; 
			x[3] = szi[0];
			return 2;
		}
		// now: c<0 , sign = +1
		x[0] = sz1/2-szr[0];			// 1st real root
		x[1] = sz1/2+szr[0];			// 2nd real root
		x[2] = -sz1/2;
		x[3] = szi[0];
		return 2;
	} // SolveP4De(double *x, double b, double c, double d)	// solve equation x^4 + b*x^2 + c*x + d
	//-----------------------------------------------------------------------------
	private double N4Step(double x, double a,double b,double c,double d)	// one Newton step for x^4 + a*x^3 + b*x^2 + c*x + d
	{
		double fxs= ((4*x+3*a)*x+2*b)*x+c;	// f'(x)
		if (fxs == 0) return x;	//return 1e99; <<-- FIXED!
		double fx = (((x+a)*x+b)*x+c)*x+d;	// f(x)
		return x - fx/fxs;
	} 
	//-----------------------------------------------------------------------------
	// x - array of size 4
	// return 4: 4 real roots x[0], x[1], x[2], x[3], possible multiple roots
	// return 2: 2 real roots x[0], x[1] and complex x[2]켲*x[3], 
	// return 0: two pair of complex roots: x[0]켲*x[1],  x[2]켲*x[3], 
	public int   SolveP4(double x[],double a,double b,double c,double d) {	// solve equation x^4 + a*x^3 + b*x^2 + c*x + d by Dekart-Euler method
		// move to a=0:
		double d1 = d + 0.25*a*( 0.25*b*a - 3./64*a*a*a - c);
		double c1 = c + 0.5*a*(0.25*a*a - b);
		double b1 = b - 0.375*a*a;
		int res = SolveP4De( x, b1, c1, d1);
		if( res==4) { x[0]-= a/4; x[1]-= a/4; x[2]-= a/4; x[3]-= a/4; }
		else if (res==2) { x[0]-= a/4; x[1]-= a/4; x[2]-= a/4; }
		else             { x[0]-= a/4; x[2]-= a/4; }
		// one Newton step for each real root:
		if( res>0 )
		{
			x[0] = N4Step(x[0], a,b,c,d);
			x[1] = N4Step(x[1], a,b,c,d);
		}
		if( res>2 )
		{
			x[2] = N4Step(x[2], a,b,c,d);
			x[3] = N4Step(x[3], a,b,c,d);
		}
		return res;
	}
	//-----------------------------------------------------------------------------
	public double F5(double t, double a, double b, double c, double d, double e) {
	    return (((((t+a)*t+b)*t+c)*t+d)*t+e);	
	}
	//-----------------------------------------------------------------------------
	private double SolveP5_1(double a,double b,double c,double d,double e)	// return real root of x^5 + a*x^4 + b*x^3 + c*x^2 + d*x + e = 0
	{
		int cnt;
		if( Math.abs(e)<eps ) return 0;

		double brd =  Math.abs(a);			// brd - border of real roots
		if( Math.abs(b)>brd ) brd = Math.abs(b);
		if( Math.abs(c)>brd ) brd = Math.abs(c);
		if( Math.abs(d)>brd ) brd = Math.abs(d);
		if( Math.abs(e)>brd ) brd = Math.abs(e);
		brd++;							// brd - border of real roots

		double x0, f0;					// less than root
		double x1, f1;					// greater than root
		double x2, f2, f2s;				// next values, f(x2), f'(x2)
		double dx = 0.0;

		if( e<0 ) { x0 =   0; x1 = brd; f0=e; f1=F5(x1, a, b, c, d, e); x2 = 0.01*brd; }	// positive root
		else	  { x0 =-brd; x1 =   0; f0=F5(x0, a, b, c, d, e); f1=e; x2 =-0.01*brd; }	// negative root

		if( Math.abs(f0)<eps ) return x0;
		if( Math.abs(f1)<eps ) return x1;

		// now x0<x1, f(x0)<0, f(x1)>0
		// Firstly 10 bisections
		for( cnt=0; cnt<10; cnt++)
		{
			x2 = (x0 + x1) / 2;					// next point
			//x2 = x0 - f0*(x1 - x0) / (f1 - f0);		// next point
			f2 = F5(x2, a, b, c, d, e);				// f(x2)
			if( Math.abs(f2)<eps ) return x2;
			if( f2>0 ) { x1=x2; f1=f2; }
			else       { x0=x2; f0=f2; }
		}

		// At each step:
		// x0<x1, f(x0)<0, f(x1)>0.
		// x2 - next value
		// we hope that x0 < x2 < x1, but not necessarily
		do {
			if(cnt++>50) break;
			if( x2<=x0 || x2>= x1 ) x2 = (x0 + x1)/2;	// now  x0 < x2 < x1
			f2 = F5(x2, a, b, c, d, e);								// f(x2)
			if( Math.abs(f2)<eps ) return x2;
			if( f2>0 ) { x1=x2; f1=f2; }
			else       { x0=x2; f0=f2; }
			f2s= (((5*x2+4*a)*x2+3*b)*x2+2*c)*x2+d;		// f'(x2)
			if( Math.abs(f2s)<eps ) { x2=1e99; continue; }
			dx = f2/f2s;
			x2 -= dx;
		} while(Math.abs(dx)>eps);
		return x2;
	} // SolveP5_1(double a,double b,double c,double d,double e)	// return real root of x^5 + a*x^4 + b*x^3 + c*x^2 + d*x + e = 0
	//-----------------------------------------------------------------------------
	public int   SolveP5(double x[],double a,double b,double c,double d,double e)	// solve equation x^5 + a*x^4 + b*x^3 + c*x^2 + d*x + e = 0
	{
		int i;
		double r = x[0] = SolveP5_1(a,b,c,d,e);
		double a1 = a+r, b1=b+r*a1, c1=c+r*b1, d1=d+r*c1;
		double x1[] = new double[x.length-1];
		for (i = 0; i < x1.length; i++) {
			x1[i] = x[i+1];
		}
		int answer = 1+SolveP4(x1, a1,b1,c1,d1);
		for (i = 0; i < x1.length; i++) {
			x[i+1] = x1[i];
		}
		return answer;
	} // SolveP5(double *x,double a,double b,double c,double d,double e)	// solve equation x^5 + a*x^4 + b*x^3 + c*x^2 + d*x + e = 0
	//-----------------------------------------------------------------------------
	
}