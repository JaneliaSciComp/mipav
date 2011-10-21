package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

/**
 *  Nearest neighbour interpolation.
 */
public class InterpolateRowNN extends InterpolateRow
{
	public InterpolateRowNN() { super(); }

	public InterpolateRowNN( final double[] data )
	{
		super(data);
	}

	final public int getWindowSize() { return 2; }
	
	final public double interpolate(final double x)
	{
		if (x < 0) return 0.0; else if (x > 1) return 0.0;
		else return (x<0.5)?data[0]:data[1]; 
	}
}
