package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

/**
 *  Sinc interpolation of one row.
 */
public class GaussianRow extends InterpolateRow
{
	final static double K = 0.3989422802; // 1/sqrt(2*pi)
	private int windowSize;
	private int halfWindowSize;
	private double sigma = 0.0, ssq, ksi;

	public GaussianRow( final double sigma )
	{
		if (sigma <= 0) { this.sigma = 0; windowSize = 2; return; }

		this.sigma = sigma;
        halfWindowSize = (int)(3.5*sigma+1);
		windowSize = 2*halfWindowSize;
		ssq = 0.5/(sigma*sigma);
		ksi = K/sigma;
	}

	public int getWindowSize() { return windowSize; }

	final public double interpolate(final double x)
	{
		if (x < 0) return 0.0; else if (x > 1) return 0.0;
        else if (sigma <= 0) return (x<0.5)?data[0]:data[1];

		else
		{
			double t = 0, s = 0, r;
			for(int i = -halfWindowSize+1; i <= halfWindowSize; i++) 
				{ r=R(x-i); s += r; t += r * data[i+halfWindowSize-1]; }
			if (s > 0) t /= s;
			return t;
		}
	}

	final private double R(double x)
	{
		final double y = - x*x * ssq;
		return StrictMath.exp(y); //* ksi;
	}
}
