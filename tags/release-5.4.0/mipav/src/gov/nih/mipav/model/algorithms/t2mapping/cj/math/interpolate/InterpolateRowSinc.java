package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

/**
 *  Sinc interpolation of one row.
 */
public class InterpolateRowSinc extends InterpolateRow
{
	private int windowSize = 7;
	private int halfWindowSize = windowSize/2;

	public InterpolateRowSinc() { super(); }

	public InterpolateRowSinc( final double[] data )
	{
		super(data);
	}

	final public void setWindowSize(final int windowSize) 
		{ this.windowSize = windowSize; this.halfWindowSize = windowSize/2; }
	
	final public int getWindowSize() { return windowSize; }
	
	final public double interpolate(final double x)
	{
		if (x < 0) return 0.0; else if (x > 1) return 0.0;
		else
		{
			double t = 0;
			for(int i = -halfWindowSize; i < halfWindowSize+1; i++) 
				t += R(x-i) * data[i+halfWindowSize];
			return t;
		}
	}

	final private double R(double x)
	{
		if( x == 0.0 ) return 1.0;
		final double y = StrictMath.PI * x;
		return StrictMath.sin(y) / ( y );
	}
}
