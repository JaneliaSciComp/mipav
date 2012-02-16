package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

abstract public class Interpolate1D
{
	Interpolate1D()
	{
	}

	abstract public void interpolate(final double[] x, final double[] y, 
	                                 final double[] X, double[] Y);
}
