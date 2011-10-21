package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;

/**  This routine calculates the pair of T<sub>2</sub> amplitudes 
 *   and times that minimize the chi2 over a range of T<sub>2</sub>.
 *   This algorithm uses the T2FitBiExp and sweeps through the range
 *   of times to find the lowest chi2.
 */
public class T2FitBiExpSweep extends T2Fit
{
	/* 
	 *   Two things necessary for the fit.
	 */
	private double[] t2times = null;
	private double[] ampout = new double[2];
	private double[] t2out = new double[2];

	/** The bi-exponential fixed T2 solver. */
	private T2FitBiExpFixedT2 biexp = null;

	/** 
	 *  No parameter constructor.
	 */
	public T2FitBiExpSweep()
	{
		biexp = new T2FitBiExpFixedT2();
		setStandardT2Times();
	}

	/**
	 *  Constructor to set the echo times.
	 */
	public T2FitBiExpSweep(final double[] te)
	{
		biexp = new T2FitBiExpFixedT2(te);

		setTE(te);
		setStandardT2Times();
	}

	public void setTE(final double[] te )
	{
		super.setTE(te);
		biexp.setTE(te);
	}

	/**
	 *  Two amp/t2 values.
	 */
	public int getNParams() { return 2; }

	/**
	 *  This should be removed when the flip angle stuff is 
	 *  going for T2FitBiExpFixedT2.
	 */
	public void setAlpha(double alpha)
	{
		System.out.print("Variable flip angle is not implemented for");
		System.out.println(" T2FitBiExpSweep.");
	}

	public void setStdDev( double[] stds )
	{
		super.setStdDev(stds);
		biexp.setStdDev(stds);
	}

	/**
	 *  Set the standard T2 times that we ant to loop over.
	 */
	public void setT2Times(final double[] t2times)
	{
		if( this.t2times == null || this.t2times.length != t2times.length )
		{
			this.t2times = new double[ t2times.length ];
		}

		System.arraycopy(t2times, 0, this.t2times, 0, t2times.length);
	}

	/**
	 *  The main solver.
	 */
	public double solveSimple(final double data[], T2Distribution dist)
	{
		double chi2 = 0.0;
		double min_chi2 = Double.MAX_VALUE;
		T2Distribution best_distribution = new T2Distribution();
		double[] params = new double[2];
		double[] out = new double[2];
		double[] junk = new double[2];
		double prev_chi2;

		for(int ii=0; ii<t2times.length; ii++)
		{
			prev_chi2 = Double.POSITIVE_INFINITY;

			for(int jj=ii+1; jj<t2times.length; jj++)
			{
				/*
				 *  Compute the solution.
				 */
				params[0] = t2times[ii]; params[1] = t2times[jj];
				biexp.setParameters(params);

				chi2 = biexp.solveSimple(data, dist);

				/*
				 *  See if it is the current best...
				 */
				if( chi2 < min_chi2 )
				{
					best_distribution.set(dist);
					min_chi2 = chi2;

					if( verbose > 0 )
					{
						System.out.println("min_chi2 = " + min_chi2 + " t2: " + 
					                    t2times[ii] + "/" + t2times[jj]);
					}

				}
				
				// This is to speed it up.... if the 
				// chi2 is > than the previous then we are probably
				// going in the wrong direction.
				if( chi2 > prev_chi2 ) jj = t2times.length;

				prev_chi2 = chi2;
			}
		}

		dist.set(best_distribution);

		return min_chi2;
	}

	/**  Set an initial set of T<sub>2</sub> times to
	 *   be swept through.
	 *
	 */
	private void setStandardT2Times()
	{
		if( t2times == null || t2times.length != 200 )
		{
			t2times = new double[ 200 ];
		}

		for(int ii=0; ii<200; ii++)
		{
			t2times[ii] = (double)((ii+1)*10);
		}
	}
}
