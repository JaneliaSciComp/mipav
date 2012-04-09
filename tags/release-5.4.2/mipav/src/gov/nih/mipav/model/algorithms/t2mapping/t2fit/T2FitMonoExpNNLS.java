package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.nnls.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.mri.hennig.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.golden.*;

/** 
 *  This algorithm finds the minimum chi2 solution 
 *  based on a mono-exponential fit using the NNLS 
 *  algorithm.  At this point it basically sweeps 
 *  through all possible T2 values and returns
 *  the distribution that has the minimum chi2.
 */
public class T2FitMonoExpNNLS extends T2FitMultiT2
{
	protected double[] a = null;
	protected double[] bnorm = null;
	protected NNLSWrapper nnls = null;

	protected double mu = 0.01;
	protected double frac_low = 0.0;
	protected double frac_high = 0.0;

	/** A temporary variable for the solveSimple routine. */
	private double[] x = null;

	private double[] xmin = null;

	/** Constructor
	 *
	 */
	public T2FitMonoExpNNLS()
	{
		nnls = new NNLSWrapper();
	}

	/** Constructor
	 *
	 */
	public T2FitMonoExpNNLS(final double[] te)
	{
		super(te);
		nnls = new NNLSWrapper();
	}

	/**
	 *  Get the number of parameters.
	 */
	public int getNParams()
	{
		return 2;
	}

	/**  Set the lower and upper scaling for the chi2.
	 *
	 */
	public void setParameters(final double[] params)
	{
		if( params == null || params.length < 2 )
		{
			System.out.println("T2FitMonoExpNNLS: Must have at least two parameters.");
			System.exit(-1);
		}

		if( params[0] < 0.0 || params[1] < 0.0 )
		{
			System.out.println("T2FitMonoExpNNLS: Both parameters must be positive.");
			System.exit(-1);
		}

		if( params[0] > params[1] )
		{
			System.out.println("T2FitMonoExpNNLS: First parameter must be smaller than second.");
			System.exit(-1);
		}

		frac_low = params[0];	
		frac_high = params[1];	
	}

	/** 
	 *  Set the TE times.
	 */
	public void setTE( final double[] te )
	{
		super.setTE( te );

        if( decay != null )
        {
            decay.setTE(te[0]);
        }

		// Set a to null to force the makeA routine to 
		// recalculate the matrix.
		a = null;
	}

	public double solveSimple(final double[] b, T2Distribution dist)
	{
		double chi2_best = Double.MAX_VALUE;
		T2Distribution dist_best = new T2Distribution();

		x = new double[ 1 ];
		t2 = new double[ 1 ];

		T2FitMonoExpNNLSGolden tfmeng = new T2FitMonoExpNNLSGolden(this);

		tfmeng.setTolerance( 10e-4 );
		tfmeng.setTE( te );
		tfmeng.setAlpha( alpha );
		tfmeng.setStdDev( stds );

		double[] params = new double[3];
		chi2_best = tfmeng.doMinimize(b, params);

		for(int i=0; i<params.length; i++) {
			if(Double.isNaN(params[i])) {
				params[i] = 0;
			}
		}
		// Copy out the best distribution.
		dist.setAmplitudes( params[0] );
		dist.setT2s( params[1] );

		return chi2_best;
	}

	/**  This will calculate the T2 distribution 
	 *   of the data in b given the refocusing pulse
	 *   flip angle that is set.
	 */
	public double solveSimpleInternal(final double[] b, T2Distribution dist)
	{
		double chi2 = 0.0;
		makeA();

		if( x == null || x.length != t2.length )
		{
			x = new double[t2.length];
		}
		Arrays.fill(x,0.0);

        //
        //  Initial Solution
        //
        if( bnorm == null || bnorm.length != (b.length + t2.length) )
        {
            bnorm = new double[b.length + t2.length];
        }

		Arrays.fill(bnorm, 0.0);
        for(int ii=0; ii<stds.length; ii++) {
            bnorm[ii] = b[ii] / stds[ii];
        }

		if( verbose > 0 )
		{
			System.out.println("T2FitMonoExpNNLS: Trying flip angle of " + alpha);
		}

        nnls.solve(bnorm, x);
		double chi2_min = getChi2(b,x);
		chi2 = chi2_min;

		if( verbose > 1 )
		{
			System.out.println("Minimum chi2 is " + chi2_min);
		}

		if( frac_low != 0.0 && frac_high != 0.0 ) 
		{
			/*
			 *  This is my own bisection type of technique...
			 */
			int number_in_mu = 0;
			double mu_lower = 0.0, mu_upper = 0.01, mu_mid = 0.01;

			/* 
			 *  Find mu_upper.
			 */
			do {
				mu_upper*=10.0;
				mu=mu_upper;
				updateMu();
				Arrays.fill(x, 0.0);
				nnls.solve(bnorm, x);
				chi2 = getChi2(b,x);
			} while( chi2 < frac_high*chi2_min );

			do {
				number_in_mu++;
				mu=mu_mid;
				updateMu();
				Arrays.fill(x, 0.0);
				nnls.solve(bnorm, x);
				chi2 = getChi2(b,x);

				if( chi2 > frac_high*chi2_min )
				{
					mu_upper = mu_mid;
					mu_mid = (mu_upper + mu_lower) / 2.0;
				}
				else if( chi2 < frac_low*chi2_min )
				{
					mu_lower = mu_mid;
					mu_mid = (mu_upper + mu_lower) / 2.0;
				}
			} while( (chi2 > frac_high*chi2_min) || 
			         (chi2 < frac_low*chi2_min));

//			System.out.println("chi2=" + chi2 +" lower=" + (frac_low*chi2_min) + " upper=" + (frac_high*chi2_min));
//			System.out.println("Number of iterations was " + number_in_mu);
		}

		dist.setAmplitudes(x, x.length);
		dist.setT2s(t2, x.length);
//		dist.setBaseline(x[x.length-1]);

        return getChi2(b,x);
	}

	/**  Update the MU factor.
	 */
	protected void updateMu(double mu)
	{
		this.mu = mu;
		updateMu();
	}

	/**  Update the MU factor.
	 */
	protected void updateMu()
	{
		if( a == null )
		{
		System.out.println("updateMu: A was null");
			makeA();
		}

		/* Now add in the small norm */
		for(int ii=0; ii<t2.length; ii++) 
		{
			a[(te.length+t2.length)*ii+ii+te.length] = mu;
		}

		nnls.update(a, te.length+t2.length, t2.length);
	}

	/** Compute the chi2 of the solution.
	 */
    public double getChi2(final double[] b, final double[] x)
    {
        double chi2 = 0.0;

        /*
         *  Compute chi2.
         */
        double temp = 0.0;

        for(int ii=0; ii<te.length; ii++)
        {
            temp = 0.0;
            for(int jj=0; jj<x.length; jj++)
            {
				temp += a[jj*(te.length+t2.length)+ii]*stds[ii]*x[jj];
            }

            chi2 += (temp-b[ii])*(temp-b[ii])/(stds[ii]*stds[ii]);
        }

        return chi2;
    }

	public String toString()
	{
		String str = "";

		for(int ii=0;ii<te.length; ii++) 
		{
			str += te[ii] + ", ";
		}
		str += "\n";
		for(int ii=0;ii<t2.length; ii++) 
		{
			str += t2[ii] + ", ";
		}

		return str;
	}

	/**   makeA - Actuallly create the A matrix (protected).
	 *
	 */
	protected void makeA()
	{
		//
		//  Fill the standard deviation vector with 1's if
		//  it has not actually been defined yet.
		//
		if(stds == null && te != null )
		{
			stds = new double[te.length];
			Arrays.fill(stds, 1.0);
		}

		if( te != null && t2 != null ) 
		{
			if( a == null || 
			    (te.length*t2.length + t2.length*t2.length) != a.length )
			{
				a = new double[te.length*t2.length + t2.length*t2.length];
			}

			Arrays.fill(a, 0.0);
			
			if( alpha > 179.999 && alpha < 180.001 )
			{

				for(int ii=0; ii<t2.length; ii++)
				{
					for(int jj=0; jj<te.length; jj++)
					{
						a[ii*(te.length+t2.length)+jj] = 
							Math.exp(-te[jj] / t2[ii]) / stds[jj];
					}
				}
			}
			else 
			{
				double[] temp = null;

				// Create based on Hennig
				for(int ii=0; ii<t2.length; ii++)
				{
					decay.setT2(t2[ii]);
					temp = decay.calculateEchoes();
					for(int jj=0; jj<te.length; jj++)
					{
						a[ii*(te.length+t2.length)+jj] = temp[jj] / stds[jj];
					}
				}
			}

			nnls.update(a, te.length+t2.length, t2.length);
		}
		else
		{
			if( te == null )
			{
				System.out.println("makeA: te is null");
			}

			if( t2 == null )
			{
				System.out.println("makeA: t2 is null");
			}
		}
	}
}

class T2FitMonoExpNNLSGolden extends Golden
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

	private T2FitMonoExpNNLS tfmen = null;
	private T2Distribution dist = new T2Distribution();
	private double[] tempDouble = new double[1];

	/**
	 *  The constructor which must be passed
	 *  the fitting method (which is just the
	 *  "this" of the caller.
	 */
	T2FitMonoExpNNLSGolden(T2FitMonoExpNNLS tfmen)
	{
		/* Set the decay thing. */
		decay = new Decay();

		this.tfmen = tfmen;
	}

	public void setAlpha( double alpha )
	{
		this.alpha = alpha;
		decay.setRefocusFlip( alpha );
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
		//  Copy the data vector to an internal class variable.
		if( data == null || data.length != b.length )
		{
			data = new double[ b.length ];
		}
		
		System.arraycopy(b, 0, data, 0, b.length);

		//  Setup the distribution.
		tempDouble[0] = best_t2;
		dist.setT2s(tempDouble);
		dist.setAmplitudes(tempDouble);

		// We are passing in R2 not T2...
		best_t2 = golden(1.0, 120.0, 3000.0);

		tempDouble[0] = best_t2;
		tfmen.setT2(tempDouble);
		tfmen.solveSimpleInternal(data, dist);

		x[0] = dist.getAmplitudes(0);
		x[1] = best_t2;
		x[2] = dist.getBaseline();

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

		tempDouble[0] = param;
		tfmen.setT2( tempDouble );
		tfmen.solveSimpleInternal(data, dist);

		decay.setT2( param );
		decay.setPD( dist.getAmplitudes(0) );
		double[] tempData = decay.calculateEchoes( );
		for(int ii=0; ii<te.length; ii++)
		{
			chi2 += Math.pow( data[ii] - tempData[ii], 2.0) / Math.pow(stddev[ii], 2.0);
		}

		return chi2;
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
