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
	// For IEEE arithmetic (IBMPC):
    private final static double MACHEP =  1.11022302462515654042E-16; // 2**-53
    private final static double MAXLOG =  7.09782712893383996843E2;   // log(2**1024)
    private final static double MINLOG = -7.08396418532264106224E2;   // log(2**-1022)
    private final static double MAXNUM =  Double.MAX_VALUE; // 1.7976931348623158E308 2**1024
	private double result[];
	
	private int version;
	
	private double df;
	
	private double y;
	
	public Cephes() {
		
	}
	
	public Cephes(double df, double y, int version, double result[]) {
		this.df = df;
		this.y = y;
		this.version = version;
		this.result = result;
	}
	
	public void run() {
		if (version == CHDTRI) {
			chdtri();
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
	private void chdtri() {
	    double x;
	    if ((y < 0) || ( y > 1.0) || (df < 1.0)) {
	    	MipavUtil.displayError("Domain error in chdtri()");
	    	result[0] = Double.NaN;
	    }
	    
	    x = igami( 0.5 * df, y );
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
	double igami(double a, double y0) {
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
		double df = Double.MAX_VALUE;
		double ansR[] = new double[1];
		Statistics stat = new Statistics(Statistics.GAUSSIAN_INVERSE_CUMULATIVE_DISTRIBUTION_FUNCTION, y0, df, ansR);
		stat.run();
		y = ( 1.0 - d - ansR[0] * Math.sqrt(d) );
		x = a * y * y * y;
		
		double ansG[] = new double[1];
		Gamma gam = new Gamma(a, 0, ansG);
		gam.run();
		lgm = ansG[0];

		for( i=0; i<10; i++ )
		{
		if( x > x0 || x < x1 )
			break;
		y = igamc(a,x);
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
		if( Math.abs(d/x) < MACHEP )
			return x;
		x = x - d;
		} // for( i=0; i<10; i++ )
		
		/* Resort to interval halving if Newton iteration did not converge. */
		ihalve:

		d = 0.0625;
		if( x0 == MAXNUM )
			{
			if( x <= 0.0 )
				x = 1.0;
			while( x0 == MAXNUM )
				{
				x = (1.0 + d) * x;
				y = igamc( a, x );
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
			y = igamc( a, x );
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
			return Double.NaN;
		}

		return( x );
	}
	
	private double igamc(double a, double x) {
		return 0.0;
	}
	
}