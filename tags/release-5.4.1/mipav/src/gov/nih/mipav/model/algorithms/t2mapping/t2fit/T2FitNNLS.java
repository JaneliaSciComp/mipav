package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.nnls.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.mri.hennig.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;

/** This is the main T2 fitting class that uses the NNLS
 *  algorithm.  There are many, many options that can 
 *  be set.
 *
 *  Here are the options:
 *
 *    standard NNLS fit:  
 *         - just call the solve routine.
 *
 *    small norm NNLS fit:
 *         - set the parameters to be 1.02 and 1.025 (e.g.)
 *         - call the solve routine.
 *
 *    standard NNLS fit given a flip angle:
 *         - set the flip angle with setAlpha( angle )
 *         - call the solve routine.
 *
 *    small norm NNLS fit given a flip angle:
 *         - set the parameters to be 1.02 and 1.025 (e.g.)
 *         - set the flip angle with setAlpha( angle )
 *         - call the solve routine.
 *
 *    standard NNLS fit and estimate the flip angle:  
 * 		   - do estimateFlip( true )
 *         - just call the solve routine.
 *
 *    ... etc etc....
 *
 *    So you can even do small norm solutions that estimate the flip 
 *    angle.
 *
 *     Cool, eh?
 *
 */
public class T2FitNNLS extends T2FitMultiT2
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
	public T2FitNNLS()
	{
		nnls = new NNLSWrapper();
	}

	/** Constructor
	 *
	 */
	public T2FitNNLS(final double[] te)
	{
		super(te);
		nnls = new NNLSWrapper();
	}

	/** Constructor
	 *
	 */
	public T2FitNNLS(final double[] te, final double[] t2)
	{
		super(te, t2);
		nnls = new NNLSWrapper();
	}

	/**
	 *  Get the number of parameters.
	 */
	public int getNParams()
	{
		return t2.length;
	}

	/**  Set the lower and upper scaling for the chi2.
	 *
	 */
	public void setParameters(final double[] params)
	{
		if( params == null || params.length < 2 )
		{
			System.out.println("T2FitNNLS: Must have at least two parameters.");
			System.exit(-1);
		}

		if( params[0] < 0.0 || params[1] < 0.0 )
		{
			System.out.println("T2FitNNLS: Both parameters must be positive.");
			System.exit(-1);
		}

		if( params[0] > params[1] )
		{
			System.out.println("T2FitNNLS: First parameter must be smaller than second.");
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

	/**  This will calculate the T2 distribution 
	 *   of the data in b given the refocusing pulse
	 *   flip angle that is set.
	 */
	public double solveSimpleBasic(final double[] b, T2Distribution dist)
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

        nnls.solve(bnorm, x);
		double chi2_min = getChi2(b,x);
		chi2 = chi2_min;

		if( verbose > 1 )
		{
			System.out.println("Minimum chi2 is " + chi2_min);
		}

//		mu =  0.01;
		if( frac_low != 0.0 && frac_high != 0.0 ) 
		{
			int number_in_mu = 0;
			while( chi2 < frac_low*chi2_min || chi2 > frac_high*chi2_min )
			{
				updateMu();
				Arrays.fill(x, 0.0);
				nnls.solve(bnorm, x);
				chi2 = getChi2(b,x);

				if( chi2 < frac_low*chi2_min ) mu *= 1.5;
				if( chi2 > frac_high*chi2_min ) mu /= 1.4;
				number_in_mu++;
			}

			System.out.println("Number of iterations was " + number_in_mu);

			if( verbose > 1 )
			{
				System.out.println("Smoothed chi2 is " + chi2);
			}
		}

		dist.setAmplitudes(x, x.length-1);
		dist.setT2s(t2, x.length-1);
		dist.setBaseline(x[x.length-1]);

        return getChi2(b,x);
	}

	/**  This will calculate the T2 distribution 
	 *   of the data in b given the refocusing pulse
	 *   flip angle that is set.
	 */
	public double solveSimple(final double[] b, T2Distribution dist)
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
			System.out.println("T2FitNNLS: Trying flip angle of " + alpha);
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

		dist.setAmplitudes(x, x.length-1);
		dist.setT2s(t2, x.length-1);
		dist.setBaseline(x[x.length-1]);

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
