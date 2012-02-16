package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

public class InterpolateRow1D extends InterpolateRow
{
	private double[] x = null;
	private double[] X = new double[1], Y = new double[1];
	protected Interpolate1D I;

	public InterpolateRow1D( final Interpolate1D I, final double[] data )
	{
		setInterpolator(I); setRow(data);
	}

	final public void setInterpolator( final Interpolate1D I )
	{
		this.I = I;
	}

	final public void setRow( final double[] data )
	{
		if (data == null) throw new NullPointerException("data is null");

		if (x == null || x.length != data.length)
		{
			x = new double[data.length];

			for(int i=0;i<data.length;i++) { x[i] = (double)i; }
		}

		this.data = data;
	}

	final public int getWindowSize() { return -1; }
	
	final public double interpolate(final double x)
	{
	    X[0] = x;
		I.interpolate(this.x, this.data, X, Y);
		return Y[0];
	}
}
