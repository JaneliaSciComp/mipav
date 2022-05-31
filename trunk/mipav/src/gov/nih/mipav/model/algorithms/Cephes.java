package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**
/ M. Ullner and M. Lund, 2012-2016

Copyright notice
----------------

~~~~
Copyright (c) 1991-95 Paul J Turner, Portland, OR
Copyright (c) 1996-98 ACE/gr Development Team

Currently maintained by Evgeny Stambulchik, Rehovot, Israel

                             All Rights Reserved

Permission  to  use, copy, modify, and  distribute  this software  and  its
documentation  for any purpose and  without fee is hereby granted, provided
that  the above copyright notice  appear in  all copies and  that both that
copyright  notice  and   this  permission  notice   appear  in   supporting
documentation.

PAUL J TURNER AND OTHER CONTRIBUTORS DISCLAIM ALL WARRANTIES WITH REGARD TO
THIS SOFTWARE, INCLUDING,  BUT  NOT LIMITED  TO, ALL  IMPLIED WARRANTIES OF
MERCHANTABILITY  AND  FITNESS. IN NO EVENT SHALL PAUL J TURNER  OR  CURRENT
MAINTAINER  BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR  OTHER TORTUOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

public class Cephes {
	public final static int CHDTRI = 1;
	public final static int IGAMI = 2;
	public final static int IGAMC = 3;
	public final static int IGAM = 4;
	// For IEEE arithmetic (IBMPC):
    private final static double MACHEP =  1.11022302462515654042E-16; // 2**-53
    private final static double MAXLOG =  7.09782712893383996843E2;   // log(2**1024)
    private final static double MINLOG = -7.08396418532264106224E2;   // log(2**-1022)
    private final static double MAXNUM =  Double.MAX_VALUE; // 1.7976931348623158E308 2**1024
    private final static double big = 4.503599627370496e15;
	private final static double biginv =  2.22044604925031308085e-16;
	private double result[];
	
	private int version;
	
	private double par1;
	
	private double par2;
	
	public Cephes() {
		
	}
	
	public Cephes(double par1, double par2, int version, double result[]) {
		this.par1 = par1;
		this.par2 = par2;
		this.version = version;
		this.result = result;
	}
	
	public void run() {
		if (version == CHDTRI) {
			chdtri(par1, par2);
		}
		else if (version == IGAMI) {
			igami(par1, par2);
		}
		else if (version == IGAMC) {
			igamc(par1, par2);
		}
		else if (version == IGAM) {
			igam(par1,par2);
		}
	}
	
	/*							chdtri()
	 *
	 *	Inverse of complemented Chi-square distribution
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double df, x, y, chdtri();
	 *
	 * x = chdtri( df, y );
	 *
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Finds the Chi-square argument x such that the integral
	 * from x to infinity of the Chi-square density is equal
	 * to the given cumulative probability y.
	 *
	 * This is accomplished using the inverse gamma integral
	 * function and the relation
	 *
	 *    x/2 = igami( df/2, y );
	 *
	 *
	 *
	 *
	 * ACCURACY:
	 * 
	 * See igami.c.
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * chdtri domain   y < 0 or y > 1        0.0
	 *                     v < 1
	 *
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1984, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	private void chdtri(double df, double y) {
	    double x;
	    if ((y < 0) || ( y > 1.0) || (df < 1.0)) {
	    	MipavUtil.displayError("Domain error in chdtri()");
	    	result[0] = 0.0;
	    }
	    
	    igami( 0.5 * df, y );
	    x = result[0];
	    result[0] = (2.0 * x );
	    
	}
	
	/*							igami()
	 *
	 *      Inverse of complemented imcomplete gamma integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, x, p, igami();
	 *
	 * x = igami( a, p );
	 *
	 * DESCRIPTION:
	 *
	 * Given p, the function finds x such that
	 *
	 *  igamc( a, x ) = p.
	 *
	 * Starting with the approximate value
	 *
	 *         3
	 *  x = a t
	 *
	 *  where
	 *
	 *  t = 1 - d - ndtri(p) sqrt(d)
	 * 
	 * and
	 *
	 *  d = 1/9a,
	 *
	 * the routine performs up to 10 Newton iterations to find the
	 * root of igamc(a,x) - p = 0.
	 *
	 * ACCURACY:
	 *
	 ** Tested at random a, p in the intervals indicated.
	 *
	 *                a        p                      Relative error:
	 * arithmetic   domain   domain     # trials      peak         rms
	 *    IEEE     0.5,100   0,0.5       100000       1.0e-14     1.7e-15
	 *    IEEE     0.01,0.5  0,0.5       100000       9.0e-14     3.4e-15
	 *    IEEE    0.5,10000  0,0.5        20000       2.3e-13     3.8e-14
	 */
	
	/*
	Cephes Math Library Release 2.3:  March, 1995
	Copyright 1984, 1987, 1995 by Stephen L. Moshier
	*/
	private void igami(double a, double y0) {
		double x0, x1, x, yl, yh, y, d, lgm, dithresh;
		int i, dir;

		/* bound the solution */
		x0 = MAXNUM;
		yl = 0;
		x1 = 0;
		yh = 1.0;
		dithresh = 5.0 * MACHEP;
		
		/* approximation to inverse function */
		d = 1.0/(9.0*a);
		ndtri(y0);
		y = ( 1.0 - d - result[0] * Math.sqrt(d) );
		x = a * y * y * y;
		
		double ansG[] = new double[1];
		Gamma gam = new Gamma(a, 0, ansG);
		gam.run();
		lgm = ansG[0];

		for( i=0; i<10; i++ )
		{
		if( x > x0 || x < x1 )
			break;
		igamc(a,x);
		y = result[0];
		if( y < yl || y > yh )
			break;
		if( y < y0 )
			{
			x0 = x;
			yl = y;
			}
		else
			{
			x1 = x;
			yh = y;
			}
	/* compute the derivative of the function at this point */
		d = (a - 1.0) * Math.log(x) - x - lgm;
		if( d < -MAXLOG )
			break;
		d = -Math.exp(d);
	/* compute the step to the next approximation of x */
		d = (y - y0)/d;
		if( Math.abs(d/x) < MACHEP ) {
			result[0] = x;
			return;
		}
		x = x - d;
		} // for( i=0; i<10; i++ )
		
		/* Resort to interval halving if Newton iteration did not converge. */

		d = 0.0625;
		if( x0 == MAXNUM )
			{
			if( x <= 0.0 )
				x = 1.0;
			while( x0 == MAXNUM )
				{
				x = (1.0 + d) * x;
				igamc( a, x );
				y = result[0];
				if( y < y0 )
					{
					x0 = x;
					yl = y;
					break;
					}
				d = d + d;
				}
			}
		d = 0.5;
		dir = 0;

		for( i=0; i<400; i++ )
			{
			x = x1  +  d * (x0 - x1);
			igamc( a, x );
			y = result[0];
			lgm = (x0 - x1)/(x1 + x0);
			if( Math.abs(lgm) < dithresh )
				break;
			lgm = (y - y0)/y0;
			if( Math.abs(lgm) < dithresh )
				break;
			if( x <= 0.0 )
				break;
			if( y >= y0 )
				{
				x1 = x;
				yh = y;
				if( dir < 0 )
					{
					dir = 0;
					d = 0.5;
					}
				else if( dir > 1 )
					d = 0.5 * d + 0.5; 
				else
					d = (y0 - yl)/(yh - yl);
				dir += 1;
				}
			else
				{
				x0 = x;
				yl = y;
				if( dir > 0 )
					{
					dir = 0;
					d = 0.5;
					}
				else if( dir < -1 )
					d = 0.5 * d;
				else
					d = (y0 - yl)/(yh - yl);
				dir -= 1;
				}
			}
		if( x == 0.0 ) {
			MipavUtil.displayError( "igami UNDERFLOW ERROR");
			result[0] = 0.0;
			return;
		}
        result[0] = x;
		return;
	}
	
	/*							igamc()
	 *
	 *	Complemented incomplete gamma integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, x, y, igamc();
	 *
	 * y = igamc( a, x );
	 *
	 * DESCRIPTION:
	 *
	 * The function is defined by
	 *
	 *
	 *  igamc(a,x)   =   1 - igam(a,x)
	 *
	 *                            inf.
	 *                              -
	 *                     1       | |  -t  a-1
	 *               =   -----     |   e   t   dt.
	 *                    -      | |
	 *                   | (a)    -
	 *                             x
	 *
	 *
	 * In this implementation both arguments must be positive.
	 * The integral is evaluated by either a power series or
	 * continued fraction expansion, depending on the relative
	 * values of a and x.
	 *
	 * ACCURACY:
	 *
	 * Tested at random a, x.
	 *                a         x                      Relative error:
	 * arithmetic   domain   domain     # trials      peak         rms
	 *    IEEE     0.5,100   0,100      200000       1.9e-14     1.7e-15
	 *    IEEE     0.01,0.5  0,100      200000       1.4e-13     1.6e-15
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1985, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/

	private void igamc(double  a, double x )
	{
	double ans, ax, c, yc, r, t, y, z;
	double pk, pkm1, pkm2, qk, qkm1, qkm2;

	if( (x <= 0) || ( a <= 0) ) {
		result[0] = 1.0;
		return;
	}

	if( (x < 1.0) || (x < a) ) {
		igam(a,x);
		result[0] = 1.0 - result[0];
		return;
	}

	double ansG[] = new double[1];
	Gamma gam = new Gamma(a, 0, ansG);
	gam.run();
	double lgm = ansG[0];
	ax = a * Math.log(x) - x - lgm;
	if( ax < -MAXLOG )
		{
		MipavUtil.displayError("igamc UNDERFLOW");
		result[0] = 0.0;
		return;
		}
	ax = Math.exp(ax);

	/* continued fraction */
	y = 1.0 - a;
	z = x + y + 1.0;
	c = 0.0;
	pkm2 = 1.0;
	qkm2 = x;
	pkm1 = x + 1.0;
	qkm1 = z * x;
	ans = pkm1/qkm1;

	do
		{
		c += 1.0;
		y += 1.0;
		z += 2.0;
		yc = y * c;
		pk = pkm1 * z  -  pkm2 * yc;
		qk = qkm1 * z  -  qkm2 * yc;
		if( qk != 0 )
			{
			r = pk/qk;
			t = Math.abs( (ans - r)/r );
			ans = r;
			}
		else
			t = 1.0;
		pkm2 = pkm1;
		pkm1 = pk;
		qkm2 = qkm1;
		qkm1 = qk;
		if( Math.abs(pk) > big )
			{
			pkm2 *= biginv;
			pkm1 *= biginv;
			qkm2 *= biginv;
			qkm1 *= biginv;
			}
		}
	while( t > MACHEP );
     
	result[0] = ans * ax;
	return;
	}

	/*							igam.c
	 *
	 *	Incomplete gamma integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, x, y, igam();
	 *
	 * y = igam( a, x );
	 *
	 * DESCRIPTION:
	 *
	 * The function is defined by
	 *
	 *                           x
	 *                            -
	 *                   1       | |  -t  a-1
	 *  igam(a,x)  =   -----     |   e   t   dt.
	 *                  -      | |
	 *                 | (a)    -
	 *                           0
	 *
	 *
	 * In this implementation both arguments must be positive.
	 * The integral is evaluated by either a power series or
	 * continued fraction expansion, depending on the relative
	 * values of a and x.
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    IEEE      0,30       200000       3.6e-14     2.9e-15
	 *    IEEE      0,100      300000       9.9e-14     1.5e-14
	 */
	
	/* left tail of incomplete gamma function:
	 *
	 *          inf.      k
	 *   a  -x   -       x
	 *  x  e     >   ----------
	 *           -     -
	 *          k=0   | (a+k+1)
	 *
	 */

	private void igam(double a, double x)
	{
	double ans, ax, c, r;

	if( (x <= 0) || ( a <= 0) ) {
		result[0] = 0.0;
		return;
	}

	if( (x > 1.0) && (x > a ) ) {
		igamc(a,x);
		result[0] = 1.0 - result[0];
		return;
	}

	/* Compute  x**a * exp(-x) / gamma(a)  */
	double ansG[] = new double[1];
	Gamma gam = new Gamma(a, 0, ansG);
	gam.run();
	double lgm = ansG[0];
	ax = a * Math.log(x) - x - lgm;
	if( ax < -MAXLOG )
		{
		MipavUtil.displayError( "igam UNDERFLOW");
		result[0] = 0.0;
		return;
		}
	ax = Math.exp(ax);

	/* power series */
	r = a;
	c = 1.0;
	ans = 1.0;

	do
		{
		r += 1.0;
		c *= x/r;
		ans += c;
		}
	while( c/ans > MACHEP );

	result[0] = ans * ax/a;
	return;
	}

	private void ndtri(double y0) {
		result[0] = 0.0;
		return;
	}
	
}