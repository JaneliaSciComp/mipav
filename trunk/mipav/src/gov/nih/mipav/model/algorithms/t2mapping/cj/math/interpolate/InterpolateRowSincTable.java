package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

/**
 *  Sinc interpolation of one row.
 */
public class InterpolateRowSincTable extends InterpolateRow
{
	private int windowSize = 7;
	private int halfWindowSize = windowSize/2;

	//  Create a table to be used for the data lookup.
	private double[] table = null;

	public InterpolateRowSincTable() 
	{ 	
		super(); 
		setWindowSize(13);
	}

	public InterpolateRowSincTable( final double[] data )
	{
		super(data);
		setWindowSize(13);
	}

	/**
	 *  Setup the table of sinc values.
	 */
	private void setupTable()
	{
		//  Allocate the space.
		table = new double[(halfWindowSize+1)*100+1];

		//  Set the 0 entry.
		table[0] = 1.0;

		//  Fill in the rest of the table.
		for(int ii=1; ii<(halfWindowSize+1)*100+1; ii++)
		{
			final double y = StrictMath.PI * (double)ii/100.0;
			table[ii] = StrictMath.sin(y) / ( y );
		}
	}

	/**
	 *  Set the window size of the kernel.
	 */
	final public void setWindowSize(final int windowSize) 
	{ 	
		this.windowSize = windowSize; 
		this.halfWindowSize = windowSize/2; 
		setupTable();
	}
	
	/**  Retrieve the window size. */
	final public int getWindowSize() { return windowSize; }
	
	/**
	 *  The interpolator.
	 */
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

	/**
	 *  The multiplier, in this case, defined in the table.
	 */
	final private double R(double x)
		{ return table[(int)(((x>=0)?x:-x)*100.0)]; }
}
