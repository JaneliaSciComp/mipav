package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

public class CubicSpline1D extends Interpolate1D
{
	public CubicSpline1D()
	{
	}

	public void interpolate(final double[] x, final double[] y, 
	                        final double[] X, double[] Y)
	{
		double[] x2 = new double[ x.length + 2];
		double[] y2 = new double[ y.length + 2];

		x2[0] = x[0]; x2[x2.length-1] = x[x.length-1];
		y2[0] = y[0]; y2[y2.length-1] = y[y.length-1];
		System.arraycopy(x,0,x2,1,x.length);
		System.arraycopy(y,0,y2,1,y.length);

		interpolateINTERNAL(x2,y2,X,Y);
	}

	private void interpolateINTERNAL(final double[] x, final double[] y, 
	                        final double[] X, double[] Y)
	{
		double u = 0.0;
		int j = 0;

		// Make sure that Y is the correct size.
		if( Y == null || Y.length != X.length )
		{
			Y = new double[ X.length ];
		}

		// Compute the cubic coefficients
		Cubic[] cc = calcNaturalCubic(y);

		for(int ii=0; ii<X.length; ii++)
		{
			if( ii<X.length-1 )
			{
				int jj = 0;
				while( x[jj] < X[ii] ) jj++;
				if( jj==0) jj = 2;
				u = (X[ii] - x[jj-1]) / (x[jj]-x[jj-1]);
				Y[ii] = cc[jj-1].eval(u);
			}
			else
			{
				u = 1;
				Y[ii] = cc[cc.length-1].eval(u);
			}
		}

	}

	private Cubic[] calcNaturalCubic(double[] x) 
	{
		int n = x.length-1;
		double[] gamma = new double[n+1];
		double[] delta = new double[n+1];
		double[] D = new double[n+1];
		int i;

		gamma[0] = 1.0f/2.0f;
		for ( i = 1; i < n; i++) 
		{
			gamma[i] = 1/(4-gamma[i-1]);
		}
		gamma[n] = 1/(2-gamma[n-1]);

		delta[0] = 3*(x[1]-x[0])*gamma[0];
		for ( i = 1; i < n; i++) 
		{
			delta[i] = (3*(x[i+1]-x[i-1])-delta[i-1])*gamma[i];
		}
		delta[n] = (3*(x[n]-x[n-1])-delta[n-1])*gamma[n];

		D[n] = delta[n];
		for ( i = n-1; i >= 0; i--) 
		{
			D[i] = delta[i] - gamma[i]*D[i+1];
		}

		/* now compute the coefficients of the cubics */
		Cubic[] C = new Cubic[n];
		for ( i = 0; i < n; i++) 
		{
			C[i] = new Cubic(x[i], D[i], 3*(x[i+1] - x[i]) - 2*D[i] - D[i+1],
							2*(x[i] - x[i+1]) + D[i] + D[i+1]);
		}

		return C;
	}

	public class Cubic {

		double a,b,c,d;         /* a + b*u + c*u^2 +d*u^3 */

		public Cubic(double a, double b, double c, double d)
		{
			this.a = a;
			this.b = b;
			this.c = c;
			this.d = d;
		}


		/** evaluate cubic */
		public double eval(double u) {
			return (((d*u) + c)*u + b)*u + a;
		}
	}

}
