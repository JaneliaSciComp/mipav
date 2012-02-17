package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

public class InterpolateRowCubic extends InterpolateRow
{
	public InterpolateRowCubic() { super(); }

	public InterpolateRowCubic( final double[] data )
	{
		super(data);
	}

	final public int getWindowSize() { return 4; }
	
	final public double interpolate(final double x)
	{
		if (x < 0) return 0.0; else if (x > 1) return 0.0;
		else
		{
			double t = 0;
			for(int i = -1; i <= 2; i++) t += R(x-i) * data[i+1];
			return t;
		}
	}

	final private double R(double x)
	{
		final double y = Math.abs(x);
		return (y<1) ? (1+y*y*(-2+y)) : (4+y*(-8+y*(5-y)));
	}
}
