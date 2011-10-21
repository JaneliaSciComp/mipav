package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

public class BSpline1D extends Interpolate1D
{
	public BSpline1D()
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
		double yA, yB, yC, yD;
		double b3, b2, b1, b0;
		double t;

		// Make sure that Y is the correct size.
		if( Y == null || Y.length != X.length )
		{
			Y = new double[ X.length ];
		}

		// Loop over each segment
		for(int ii=1; ii<x.length-2; ii++)
		{
			yA = y[ii-1];
			yB = y[ii];
			yC = y[ii+1];
			yD = y[ii+2];

			b3=(-yA+3.0*(yB-yC)+yD)/6.0;
			b2=(yA-2.0*yB+yC)/2.0;
			b1=(yC-yA)/2.0;
			b0=(yA+4.0*yB+yC)/6.0;

			for(int jj=0; jj<X.length; jj++)
			{
				if( X[jj] >= x[ii] && X[jj] <= x[ii+1] )
				{
					t = (X[jj]-x[ii]) / (x[ii+1]-x[ii]);
					Y[jj] = ((b3*t+b2)*t+b1)*t+b0;
				}
			}
		}
	}
}
