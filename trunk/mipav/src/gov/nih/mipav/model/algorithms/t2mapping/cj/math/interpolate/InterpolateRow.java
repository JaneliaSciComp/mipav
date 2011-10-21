package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

abstract public class InterpolateRow implements Cloneable
{
	protected double[] data = null;

	public InterpolateRow()
	{
	}

	public Object clone()
	{
		Object o = null;
		try {
			o = super.clone();
			if (this.data != null)
		   		{ ((InterpolateRow)o).data = (double[])this.data.clone(); }
		}
		catch( CloneNotSupportedException e) {
			System.out.println("InterpolateRow can not clone");
		}

		return o;
	}

	public InterpolateRow( final double[] data )
	{
		setRow(data);
	}

	public void setRow( final double[] data )
	{
		this.data = data;
	}

	// Diameter of window (e.g. 2 for linear, 4 for cubic)
	// If getWindowSize() is -1, uses all data

	public abstract int getWindowSize();

	public abstract double interpolate(final double x);

	static public InterpolateRow createInterpolator( String name )
	{
		if( name.toLowerCase().trim().startsWith("l") )
			return new InterpolateRowLinear();

		else if ( name.toLowerCase().trim().startsWith("si") )
			return new InterpolateRowSinc();

		else if ( name.toLowerCase().trim().startsWith("sf") )
			return new InterpolateRowSincTable();

		else if ( name.toLowerCase().trim().startsWith("n") )
			return new InterpolateRowNN();

		else if ( name.toLowerCase().trim().startsWith("c") )
			return new InterpolateRowCubic();

		else
			throw new UnsupportedOperationException("interpolate: '" + name + 
				"' is not a valid interpolator, must be 'linear', 'sinc' or 'cubic'");
	}
}
