package gov.nih.mipav.model.algorithms.t2mapping.t2bin;
 
import java.io.*;
import java.lang.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;

/** 
 *  This represents a T2 bin from a starting time (inclusive) to an 
 *  ending time (exclusive) and also a name.  All times are in ms.
 */
public class T2Bin {

	/** Constructor to set the number of rows,  columns and slices.
	 *
	 */
	public T2Bin() 
	{
	}

	public T2Bin(double start, double stop, String name) 
	{
		this.start = start;
		this.stop = stop;
		this.name = name;
	}

	/**  Define the start and stopping point of the bin as well as the name.
	 *
	 */
	public void define(double start, double stop, String name)
	{
		this.start = start;
		this.stop = stop;
		this.name = name;
	}

	/**  Add a info for a row/col/slice.
	 *
	 */
	public void calculate(final T2Distribution dist)
	{
		calculate(dist.getAmplitudes(), dist.getT2s());
	}

	/**  Add a info for a row/col/slice.
	 *
	 */
	public void calculate(final double[] spec_amp, final double[] spec_time)
	{
		int ii;
		double log_time, tmp;
	 	
		fr = dn = am = gm = hm = l2 = va = 0.0;
	 
		for(ii=0; ii<spec_time.length; ii++) {
			if( spec_amp[ii] > 1e-8 && 
				spec_time[ii] >= start && spec_time[ii] < stop ) 
			{
				log_time = (float)Math.log((double)spec_time[ii]);
				fr += spec_amp[ii];
				dn += spec_amp[ii];
				am += (spec_amp[ii]*spec_time[ii]);
				gm += (spec_amp[ii]*log_time);
				hm += (spec_amp[ii]/spec_time[ii]);
				l2 += (spec_amp[ii]*spec_amp[ii]);
			}
		}
	 
		va = 0.0;
		for(ii=0; ii<spec_time.length; ii++) {
			if( dn > 0.0 && spec_amp[ii] > 1e-8 &&
				spec_time[ii] >= start && spec_time[ii] < stop ) 
			{
				log_time = Math.log(spec_time[ii]);
				tmp = gm / dn;
				va += ((log_time-tmp)*(log_time-tmp)*spec_amp[ii]);
			}
		}

		// Compute the total density
		double total_density = 0.0;
		for(ii=0; ii<spec_time.length; ii++) {
			total_density += spec_amp[ii];
		}
	 
		// Do the finalize.
		if( total_density > 0.0 )
		{
			fr /= total_density;
		}
		else 
		{
			fr = 0.0;
		}

		if( dn > 0.0 )
		{
			am /= dn;
			gm = Math.exp( gm / dn);
			l2 = l2 / (dn*dn);
			va = Math.exp(va / dn) - 1.0;
		}
		else
		{
			am = gm = l2 = va = 0.0;
		}

		if( hm > 0.0 )
		{
			hm = dn/hm;
		}
		else
		{
			hm = 0.0;
		}
	}

	/** Requisite toString to show what the bin is...
	 *
	 */
	public String toString()
	{
		StringBuffer toreturn = new StringBuffer();

		toreturn.append("Bin (").append(name).append("): ");
		toreturn.append(start).append(" ").append(stop); 
		toreturn.append("\n");

		toreturn.append("  fr: ").append(fr).append("\n");
		toreturn.append("  dn: ").append(dn).append("\n");
		toreturn.append("  am: ").append(am).append("\n");
		toreturn.append("  gm: ").append(gm).append("\n");
		toreturn.append("  hm: ").append(hm).append("\n");
		toreturn.append("  l2: ").append(l2).append("\n");
		toreturn.append("  va: ").append(va).append("\n");

		return toreturn.toString();
	}

	public double getFR()
	{
		return fr;
	}

	public double getDN()
	{
		return dn;
	}

	public double getAM()
	{
		return am;
	}

	public double getGM()
	{
		return gm;
	}

	public double getHM()
	{
		return hm;
	}

	public double getL2()
	{
		return l2;
	}

	public double getVA()
	{
		return va;
	}

	final public String getName()
	{
		return name;
	}

	final public double getStart()
	{
		return start;
	}

	final public double getStop()
	{
		return stop;
	}

	/** Show what the bin is.
	 *
	 */
	public void show()
	{
		System.out.println( toString() );
	}

	private double start = 0.0;
	private double stop = 0.0;
	private String name = null;

	private double fr = 0.0;
	private double dn = 0.0;
	private double am = 0.0;
	private double gm = 0.0;
	private double hm = 0.0;
	private double l2 = 0.0;
	private double va = 0.0;
}
