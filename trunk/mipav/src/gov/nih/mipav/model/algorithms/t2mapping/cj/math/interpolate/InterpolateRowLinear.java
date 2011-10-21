package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

public class InterpolateRowLinear extends InterpolateRow
{
	public InterpolateRowLinear() { super(); }

	public InterpolateRowLinear( final double[] data )
	{
		super(data);
	}

	final public int getWindowSize() { return 2; }
	
	final public double interpolate(final double x)
	{
		if (x < 0) return 0.0; else if (x > 1) return 0.0;
		else return (1.0-x)*data[0] + x*data[1];
	}
}
