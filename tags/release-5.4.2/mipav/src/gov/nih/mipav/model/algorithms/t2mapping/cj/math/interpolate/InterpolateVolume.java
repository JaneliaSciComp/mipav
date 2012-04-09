package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import java.util.*;

public class InterpolateVolume
{
	protected Volume data = null;
	protected InterpolateRow Is = null;
	protected InterpolateSlice Irc = new InterpolateSlice();
	protected int ns,swin;

	// Note: rdata is data(:,col,slice), cdata is data(row,:,slice)
	protected double[] sdata;

	public InterpolateVolume()
	{
	}

	public InterpolateVolume( final Volume data, InterpolateRow Ir )
	{
		setVolume(data);
		setRowInterpolator(Ir);
		setColInterpolator((InterpolateRow)Ir.clone());
		setSliceInterpolator((InterpolateRow)Ir.clone());
	}

	public InterpolateVolume( final Volume data, InterpolateRow Ir, InterpolateRow Ic )
	{
		setVolume(data);
		setRowInterpolator(Ir);
		setColInterpolator(Ic);
		setSliceInterpolator(new InterpolateRowLinear());
	}

	public InterpolateVolume( final Volume data, InterpolateRow Ir, InterpolateRow Ic, InterpolateRow Is )
	{
		setVolume(data);
		setRowInterpolator(Ir);
		setColInterpolator(Ic);
		setSliceInterpolator(Is);
	}

	private void init()
	{
		if (data != null) { ns = data.getNSlices(); }

		if (Is != null)
		{
		    swin = Is.getWindowSize();

			if (swin > 0)
			{
				if (sdata == null || sdata.length != swin) sdata = new double[swin];
			}
			else if (data != null)
			{
				if (sdata == null || sdata.length != ns)
					{ sdata = new double[ns]; }
			}
		}
	}

	public int getNRows()
		{  return data.getNRows(); }

	public int getNCols()
		{  return data.getNCols(); }

	public int getNSlices()
		{  return data.getNSlices(); }

	public void setRowInterpolator(InterpolateRow I)
	{
		Irc.setRowInterpolator(I);
	}

	public void setColInterpolator(InterpolateRow I)
	{
		Irc.setColInterpolator(I);
	}

	public void setSliceInterpolator(InterpolateRow I)
	{
		this.Is = I; init();
	}

	public void setVolume(final Volume data )
	{
		this.data = data; init();
	}

	public double interpolate(final double row, final double col, final double slice)
	{
		int islice = (int)Math.floor(slice);
		int smin, smax;

		if (swin < 0)
			{ smin = 0; smax = ns-1; }
		else
			{ smin = islice - (swin-1)/2; smax = islice + swin/2; }

		for( int j=0,sli=smin; sli <= smax; sli++,j++ ) 
		{
		    if (sli < 0 || sli >= ns) { sdata[j] = 0.0; continue; }

			Irc.setSlice(data.getSlice(sli));

			sdata[j] = Irc.interpolate(row,col);
		}

		Is.setRow(sdata);

		if (swin < 0) return Is.interpolate(slice);
		else return Is.interpolate(slice-islice);
	}
}
