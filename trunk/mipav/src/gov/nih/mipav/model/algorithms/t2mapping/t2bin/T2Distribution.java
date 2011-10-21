package gov.nih.mipav.model.algorithms.t2mapping.t2bin;

/**
 *  This class should be used to encompass the idea of a pair of 
 *  vectors that represent amplitudes and T2s that is a distribution.
 *  They must be of the same length, but really that is the only
 *  constraint that is imposed (besides existence).  The amplitudes
 *  can have zeros and therefore it can be sparse.
 */
public class T2Distribution implements Cloneable
{
	private double[] amplitudes = null;
	private double[] t2s = null;
	private double baseline = 0.0;

	/**
	 *  just create it.
	 */
	public T2Distribution()
	{
	}

	/**
	 *  Create the distribution and set amplitudes and t2s, the baseline is 0.
	 */
	public T2Distribution(final T2Distribution dist)
	{
		set(dist);
	}

	/**
	 *  Create the distribution and set amplitudes and t2s, the baseline is 0.
	 */
	public T2Distribution(final double[] amplitudes, final double[] t2s)
	{
		setAmplitudes(amplitudes);	
		setT2s(t2s);	

		sanity_check();
	}

	/**
	 *  Create the distribution and set amplitudes, t2s and baseline.
	 */
	public T2Distribution(final double[] amplitudes, final double[] t2s, double baseline)
	{
		setAmplitudes(amplitudes);	
		setT2s(t2s);	
		this.baseline = baseline;

		sanity_check();
	}

	/** 
   	 *  Cloner
 	 */
    public Object clone()
    {
        Object o = null;
        try {
            o = super.clone();
			((T2Distribution)o).amplitudes = (double[])amplitudes.clone();
			((T2Distribution)o).t2s = (double[])t2s.clone();
        }
        catch( CloneNotSupportedException e) {
            System.out.println("T2Distribution can not clone");
        }

        return o;
    }

	/**
	 *  Create the distribution and set amplitudes and t2s, the baseline is 0.
	 */
	public void set(final T2Distribution dist)
	{
		setAmplitudes(dist.getAmplitudes());	
		setT2s(dist.getT2s());	
		setBaseline(dist.getBaseline());

		sanity_check();
	}

	/**
	 *  Set the Amplitude.
	 */
	final public void setAmplitudes(final double amplitudes)
	{
	   if( this.amplitudes == null || this.amplitudes.length != 1 )
	   {
		   this.amplitudes = new double[ 1 ];
	   }

		this.amplitudes[0] = amplitudes;
	}

	/**
	 *  Set the amplitudes.
	 */
	final public void setAmplitudes(final double[] amplitudes)
	{
		setAmplitudes(amplitudes, amplitudes.length);
	}

	/**
	 *  Set the amplitudes.
	 */
	final public void setAmplitudes(final double[] amplitudes, final int num)
	{
		if( this.amplitudes == null || this.amplitudes.length != num );
		{
			this.amplitudes = new double[ num ];
		}

		System.arraycopy(amplitudes, 0, this.amplitudes, 0, num);
	}

	/**
	 *  Set the T2s.
	 */
	final public void setT2s(final double t2s)
	{
	   if( this.t2s == null || this.t2s.length != 1 )
	   {
		   this.t2s = new double[ 1 ];
	   }

		this.t2s[0] = t2s;
	}

	/**
	 *  Set the T2s.
	 */
	final public void setT2s(final double[] t2s)
	{
		setT2s(t2s, t2s.length);
	}

	/**
	 *  Set the T2s.
	 */
	final public void setT2s(final double[] t2s, final int num)
	{
		if( this.t2s == null || this.t2s.length != num )
		{
			this.t2s = new double[ num ];
		}

		System.arraycopy(t2s, 0, this.t2s, 0, num);
	}

	/**
	 *  Set the baseline.
	 */
	final public void setBaseline(double baseline)
	{
		this.baseline = baseline;
	}

	/**
	 *  Get a copy of the amplitudes.
	 */
	final public double[] getAmplitudes()
	{
		return (double[])amplitudes.clone();
	}

	/**
	 *  Get a copy of the amplitudes.
	 */
	final public double getAmplitudes(int ii)
	{
		return amplitudes[ii];
	}

	/**
	 *  Get a copy of the T2s.
	 */
	final public double getT2s(int ii)
	{
		return t2s[ii];
	}

	/**
	 *  Get a copy of the T2s.
	 */
	final public double[] getT2s()
	{
		return (double[])t2s.clone();
	}

	/**
	 *  Get a copy of the baseline.
	 */
	final public double getBaseline()
	{
		return baseline;
	}

	final private void sanity_check()
	{
		if( amplitudes == null ) 
		{
			System.out.println("T2Distribution: Amplitude is null");
			System.exit(-1);
		}
		
		if( t2s == null ) 
		{
			System.out.println("T2Distribution: Amplitude is null");
			System.exit(-1);
		}
		
		if( amplitudes.length != t2s.length ) 
		{
			System.out.println("T2Distribution: Amplitude does not have the same length as the T2s.");
			System.exit(-1);
		}
	}
}
