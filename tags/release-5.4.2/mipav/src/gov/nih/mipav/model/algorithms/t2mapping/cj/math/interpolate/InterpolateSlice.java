package gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import java.util.*;

public class InterpolateSlice
{
	protected Slice data = null;
	protected InterpolateRow Ir = null, Ic = null;
	protected int nr,nc,cwin,rwin;

	// Note: rdata is data(:,col,slice), cdata is data(row,:,slice)
	protected double[] rdata, cdata;

	public InterpolateSlice()
	{
	}

	public InterpolateSlice( final Slice data, InterpolateRow Ir )
	{
		setSlice(data);
		setRowInterpolator(Ir);
		setColInterpolator((InterpolateRow)Ir.clone());
	}

	public InterpolateSlice( final Slice data, InterpolateRow Ir, InterpolateRow Ic )
	{
		setSlice(data);
		setRowInterpolator(Ir);
		setColInterpolator(Ic);
	}

	private void init()
	{
		if (Ir != null)
		{
		    rwin = Ir.getWindowSize();

			if (rwin > 0)
			{
				if (rdata == null || rdata.length != rwin) rdata = new double[rwin];
			}
			else if (data != null)
			{
				if (rdata == null || rdata.length != data.getNRows())
					{ rdata = new double[data.getNRows()]; }
			}
		}
		if (Ic != null)
		{
		    cwin = Ic.getWindowSize();

			if (cwin > 0)
			{
				if (cdata == null || cdata.length != cwin) cdata = new double[cwin];
			}
			else if (data != null)
			{
				if (cdata == null || cdata.length != data.getNCols())
					{ cdata = new double[data.getNCols()]; }
			}
		}

		if (data != null) {nc = data.getNCols(); nr = data.getNRows();}
	}

	public void setRowInterpolator(InterpolateRow I)
	{
		this.Ir = I; init();
	}

	public void setColInterpolator(InterpolateRow I)
	{
		this.Ic = I; init();
	}

	public void setSlice(final Slice data )
	{
		this.data = data; init();
	}

	public double interpolate(final double row, final double col)
	{
		int irow = (int)Math.floor(row), icol = (int)Math.floor(col);
		int cmin, cmax, rmin, rmax;

		if (rwin < 0)
			{ rmin = 0; rmax = nr-1; }
		else
			{ rmin = irow - (rwin-1)/2; rmax = irow + rwin/2; }

		if (cwin < 0)
			{ cmin = 0; cmax = nc-1; }
		else
			{ cmin = icol - (cwin-1)/2; cmax = icol + cwin/2; }

		for( int j=0,rowi=rmin; rowi <= rmax; rowi++,j++ ) 
		{
			if (rowi < 0 || rowi >= nr) { rdata[j] = 0.0; continue; }

			if ( cwin < 0 )
			{
				data.getRow(cdata, rowi);
			}
			else
			{
				Arrays.fill(cdata,0.0);

				for(int i=0,coli=cmin; coli <= cmax; coli++,i++)
				{
					if (coli < 0 || coli >= nc) cdata[i] = 0.0;
					else cdata[i] = data.getData(rowi,coli);
				}
			}

			Ic.setRow(cdata);

			if (cwin < 0) rdata[j] = Ic.interpolate(col);
			else rdata[j] = Ic.interpolate(col-icol);
		}

		Ir.setRow(rdata);
		if (rwin < 0) return Ir.interpolate(row);
		else return Ir.interpolate(row-irow);
	}
}
