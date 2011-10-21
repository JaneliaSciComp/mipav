package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit.BiExpFit;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;

public class T2FitBiExp extends T2Fit
{
	/* 
	 *   Two things necessary for the fit.
	 */
	private double[] params = null;
	private double[] ampout = new double[2]; // temp storage
	private double[] t2out = new double[2]; // temp storage

	private BiExpFit bef = null;

	public T2FitBiExp()
	{
		bef = new BiExpFit();
	}

	public T2FitBiExp(double[] te)
	{
		bef = new BiExpFit();
		setTE(te);
	}

	public void setLowerConstraints(final double[] temp )
	{
		bef.lowerConstraints( temp );
	}

	public void setUpperConstraints(final double[] temp )
	{
		bef.upperConstraints( temp );
	}

	public int getNParams() { return 2; }

	public double solveSimple(final double[] data, T2Distribution dist)
	{
		bef.doFit(te, data);

		double[] tempresult = bef.getParams();

		if( ampout == null )
		{
			throw new NullPointerException();
		}

//		System.arraycopy(tempresult, 0, result, 0, tempresult.length-1);

		ampout[0] = tempresult[0];
		t2out[0] = tempresult[1];
		ampout[1] = tempresult[2];
		t2out[1] = tempresult[3];

		dist.setAmplitudes(ampout);
		dist.setT2s(t2out);
		dist.setBaseline(tempresult[4]);

		return getChi2(data, tempresult);
	}

	private double getChi2( final double[] data, final double[] params )
	{
		double chi2 = 0.0;

		for(int ii=0; ii<data.length; ii++)
		{
			double tt= params[0]*Math.exp(-te[ii]/params[1]);
			tt +=  params[2]*Math.exp(-te[ii]/params[3]);
			tt +=  params[4];
			chi2 += (data[ii]-tt)*(data[ii]-tt)/(stds[ii]*stds[ii]);
		}

		return chi2;
	}
}