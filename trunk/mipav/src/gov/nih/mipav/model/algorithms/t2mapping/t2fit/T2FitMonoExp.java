package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.mri.hennig.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.golden.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;

/**
 *  Fit a decay curve to a mono-exponential term solving for rho, T2 and 
 *  a baseline.  All must be positive.  Optionally, we can also solve for
 *  a refocusing pulse flip angle if requested.
 */
public class T2FitMonoExp extends T2Fit
{
	/* 
	 *   Two things necessary for the fit.
	 */
	private T2FitMonoExpGolden meg = null;

	/**
	 *  Constructor that creates the MonoExpFit object.
	 */
	public T2FitMonoExp ()
	{
		meg = new T2FitMonoExpGolden();
	}

	/**
	 *  Constructor that creates the MonoExpFit object.
	 */
	public T2FitMonoExp(double[] te)
	{
		meg = new T2FitMonoExpGolden();
		setTE( te );
	}

	/**
	 *  There are three parameters, rho, t2 and baseline.
	 */
	public int getNParams() { return 3; }


	public void setAlpha(double alpha)
	{
		this.alpha = alpha;
		meg.setAlpha( alpha );
	}

	public void setTE( double[] te )
	{
		super.setTE( te );
		meg.setTE( te );
	}

	public void setStdDev( double[] stddev)
	{
		meg.setStdDev( stddev );
	}

	/**
	 *  Solver.
	 */
	public double solveSimple(final double[] data, T2Distribution dist)
	{
		double[] tempresult = new double[4];

		double chi2 = meg.doMinimize(data, tempresult);
		if(Double.isNaN(chi2)) {
			chi2 = 0;
		}
		for(int i=0; i<tempresult.length; i++) {
			if(Double.isNaN(tempresult[i])) {
				tempresult[i] = 0;
			}
		}
		dist.setAmplitudes( tempresult[0] );
		dist.setT2s( tempresult[1] );
		dist.setBaseline( tempresult[2] );

		return chi2;
	}
}

class T2FitMonoExpGolden extends Golden
{
	private double chi2 = 0.0;
	private double[] te = null;
	private double[] data = null;
	private double[] stddev = null;
	private double[] xin = null;
	T2Fit t2fitter = null;
	private int func_evals = 0;
	private Decay decay = null;
	private double alpha = 180.0;

	private double best_pd = 0.0;
	private double best_t2 = 0.0;
	private double best_baseline = 0.0;

	/**
	 *  The constructor which must be passed
	 *  the fitting method (which is just the
	 *  "this" of the caller.
	 */
	T2FitMonoExpGolden()
	{
		/* Set the decay thing. */
		decay = new Decay();
	}

	public void setAlpha( double alpha )
	{
		this.alpha = alpha;

		System.out.println("T2MonoExpFit: So far, I can't deal with a flip angle other than 180 degrees!");
		System.out.println("T2MonoExpFit: Therefore I am ignoring your request for " + alpha );
	}

	public void setTE( double[] te )
	{
		if( this.te == null || this.te.length != te.length ) 
		{
			this.te = new double[ te.length ];
		}

		System.arraycopy(te, 0, this.te, 0, te.length);

		decay.setTE( te[0] );
	}

	public void setStdDev( double[] stddev )
	{
		this.stddev = new double[ stddev.length ];
		System.arraycopy(stddev, 0, this.stddev, 0, stddev.length);
	}

	/**
	 *  This is the actual minimizer code.  We first
	 *  must allocate the data if it is not already 
	 *  allocated.  Then run the minimizer and finally
	 *  copy out the result.
	 */
	public double doMinimize(final double[] b, double[] x)
	{
		if( data == null || data.length != b.length )
		{
			data = new double[ b.length ];
		}
		
		System.arraycopy(b, 0, data, 0, b.length);

		// We are passing in R2 not T2...
		best_t2 = golden(1.0, 120.0, 3000.0);

		x[0] = computeS(best_t2);
		x[1] = best_t2;
		x[2] = 0.0;

		// Compute the Chi2 value.
		chi2 = function(best_t2);

		return chi2;
	}

	/**
	 *  The objective function of the minimizer.
	 */
	public double function(double param)
	{
		double chi2 = 0.0;

		double s = computeS( param );

		for(int ii=0; ii<te.length; ii++)
		{
			chi2 += Math.pow( data[ii] - s*Math.exp(-te[ii] / param) , 2.0) / Math.pow(stddev[ii], 2.0);
		}

		return chi2;
	}

	private double computeS(double param)
	{
		// Compute the amplitude.
		double num = 0.0;
		for(int ii=0; ii<te.length; ii++)
		{
			num+=(te[ii]*data[ii])/(stddev[ii]*stddev[ii])*Math.exp(-te[ii]/param);
		}

		double denom = 0.0;
		for(int ii=0; ii<te.length; ii++)
		{
			denom+=te[ii]/(stddev[ii]*stddev[ii])*Math.exp(-2.0*te[ii]/param);
		}
		return num / denom;
	}

	/**
	 *  Must be defined as per the simplex method.
	 */
	public boolean watchProgress(int StepNr)
	{
		return true;
	}

	/*
	 *  At the end, set the best flip angle found.
	 */
	public void makeReport(int cParams, String ParamName[], 
	                        double ParamValue[], boolean Terminated)
	{
		best_pd = ParamValue[1];
		best_t2 = ParamValue[2];
		best_baseline = ParamValue[3];
		func_evals = 0;
	}
}
