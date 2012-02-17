package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.t2roi;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.PixFile;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

/**
 *  T2 analysis over regions.
 */
abstract public class T2ROI
{
	protected T2Distribution dist = null; // used by all
	protected int verbose = 0;
	protected T2Fit t2fitter = null;
	protected double mean = 0.0;
	protected double std = 0.0;
	protected int iterations = 1000;
	protected Vector t2bins = null;
	protected boolean estimateFlip = false;
	
	/** The threshold on the signal intensity. */
	protected double threshold = 0;

	public T2ROI()
	{
		t2fitter = new T2FitNNLS();
		dist = new T2Distribution();

		// Vector of bin partitions
		t2bins = new Vector();
	}

	public void setIterations(final int iterations)
	{
		this.iterations = iterations;
	}

	/**
	 *  Add a bin to which we will calculate the fraction, density and T2s.
	 */
	public void addBin(final double start, final double stop, final String name)
	{
		t2bins.add( new T2Bin(start, stop, name) );
	}

	public void setEstimateFlip(final boolean estimateFlip)
	{
		this.estimateFlip = estimateFlip;
	}

	/**
	 *  Set the method of estimating the flip angle.
	 */
	public void setEstimateFlipMethod(final String method)
	{
		if( method.toLowerCase().compareTo("spline") == 0 )
		{
			t2fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_SPLINE );
		}
		else if( method.toLowerCase().compareTo("quadratic") == 0 )
		{
			t2fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_QUADRATIC );
		}
		else if( method.toLowerCase().compareTo("minimize") == 0 )
		{
			t2fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_MINIMIZE );
		}
		else if( method.toLowerCase().compareTo("golden") == 0 )
		{
			t2fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_GOLDEN );
		}
		else
		{
			System.out.println("bad method");
			System.exit(-1);
		}
	}

	/**
	 *  Set the fitting method.
	 */
	public void setT2FitMethod(final String fitmethod)
	{
		if( fitmethod.compareTo("nnls") == 0 )
		{
			t2fitter = new T2FitNNLS();
		}
		else if( fitmethod.compareTo("l1") == 0 )
		{
			t2fitter = new T2FitL1();
		}
	}

	/**
	 *  Set the TE times.
	 */
	public void setTE(final double[] te)
	{
		t2fitter.setTE(te);
	}

	/**
	 *  Set the T1 time.
	 */
	public void setT1(final double t1)
	{
		t2fitter.setT1(t1);
	}

	public void setVerbose(final int verbose )
	{
		t2fitter.setVerbose( verbose );
		this.verbose = verbose;
	}

	public void setParameters( double[] params )
	{
		t2fitter.setParameters(params);
	}

	public double getMean()
	{
		return mean;
	}

	public double getStd()
	{
		return std;
	}

	public abstract String getMethodName();

	/**
	 *  Convert an index into a row column pair.  This has been
	 *  tested against Matlab and is correct.
	 *
	 *  rowcol[0] is the row.
	 *  rowcol[1] is the column.
	 */
	static public int[] index2sub( final int index, final int nrows, final int ncols)
	{
		int[] rowcol = new int[2];
		rowcol[0] = index%nrows-1;
		rowcol[1] = index/nrows-1;
		return rowcol;
	}

	final double calcMean(final double[] data)
	{
		int[] ii = {0};  // Though not used.
		return calcMean(data, true, ii);
	}

	/**
	 *  calcMean will calculate the mean with/without 
	 *  including the zeros.  Either way it returns 
	 *  the number of zeros that there are in the vector.
	 */
	final double calcMean(final double[] data, boolean includeZeros, int[] numNonZero)
	{
		if( numNonZero == null || numNonZero.length < 1 ) 
		{
			System.out.println("calcMean: numNonZero not defined");
			System.exit(-1);
		}

		double tt = 0.0;
		int N = 0;
		for(int ii=0; ii<data.length; ii++) 
		{
			if( data[ii] == 0.0 && includeZeros )
			{
				tt += data[ii];
				N++;
			}
			else if( data[ii] != 0.0 )
			{
				tt += data[ii];
				N++;
			}
		}

		numNonZero[0] = N;

		return tt/(double)N;
	}

	final double calcStd(final double[] data)
	{
		return calcStd(data, true);
	}

	final double calcStd(final double[] data, boolean includeZeros)
	{
		double tt = 0.0;
		double tt2 = 0.0;
		int N = 0;
		for(int ii=0; ii<data.length; ii++) 
		{
//			if( includeZeros || data[ii] != 0.0 )
			if( ( data[ii] == 0.0 && includeZeros ) || data[ii] != 0.0 )
			{
				tt += data[ii];
				tt2 += data[ii]*data[ii];
				N++;
			}
		}

		return Math.sqrt((tt2-(tt*tt)/(double)N)/(double)(N-1) );
	}

	abstract protected void calculateMeanStd(final MCVolume vol, final int slice, 
	                                         final PixFile pix, double[] means, 
											 double[] stds);

	public void solve(final MCVolume vol, final PixFile pix)
	{
		solve(vol, 0, pix);
	}

	public void solve(final MCVolume vol, final int slice, final PixFile pix)
	{
		long start = 0, stop = 0;

		double[] stds = new double[vol.getNChannels()];
		double[] means = new double[vol.getNChannels()];

		double[][] fr = new double[ t2bins.size() ][ iterations ];
		double[][] dn = new double[ t2bins.size() ][ iterations ];
		double[][] gm = new double[ t2bins.size() ][ iterations ];
		double[] flip = new double[ iterations ];
		double[] chi2 = new double[ iterations ];
		double[] time = new double[ iterations ];

		t2fitter.setEstimateFlip( estimateFlip );

		/* 
		 *  Iterate over the number of animals.
		 */
		long total_start = System.currentTimeMillis();
		for(int ii=0; ii<iterations; ii++)
		{
//			if( iterations > 1 ) 
//				System.out.print("\rIteration: " + ii + "/" + iterations);

			start = System.currentTimeMillis();

			/*
			 *  1. Need to create a random animal.
			 *
			 *  2.  Average the data over the channels in the 
			 *      animal.
			 *
			 *  3.  Calculate the NNLS solution to this data.
			 */

			// Create the mean and standard deviation 
			// of the data at the pixels.
			Arrays.fill(means, 0.0);
			Arrays.fill(stds, 0.0);
			int row=0, col=0;

			/*
			 *  Could be mean, animal, bootstrap....
			 */
			calculateMeanStd(vol, slice, pix, means, stds);

			/*
			 *  Actually do the solution.
			 */
			t2fitter.setStdDev( stds );
			chi2[ii] = t2fitter.solve(means, dist);	
			flip[ii] = t2fitter.getBestAlpha();

			/*
			 *  Compute the distributions
			 */
            for(int bin=0; bin<t2bins.size(); bin++)
            {
				((T2Bin)t2bins.get(bin)).calculate(dist);

				fr[bin][ii] = ((T2Bin)t2bins.get(bin)).getFR();
				dn[bin][ii] = ((T2Bin)t2bins.get(bin)).getDN();
				gm[bin][ii] = ((T2Bin)t2bins.get(bin)).getGM();

                if( verbose > 1 )
                {
                    System.out.print( ((T2Bin)t2bins.get(bin)).getName() + ": ");
                    Format.print(System.out, "%4.3f ",
                        ((T2Bin)t2bins.get(bin)).getFR());
                    Format.print(System.out, "%4.3f ",
                        ((T2Bin)t2bins.get(bin)).getDN());
                    Format.print(System.out, "%4.3f ",
                        ((T2Bin)t2bins.get(bin)).getGM());
                }
			}

            // Show the chi2.
            if( verbose > 1 )
            {
                Format.print(System.out, "chi2: %4.3f", chi2[ii]);
                System.out.print(" ");
            }

            // Show the flip angle, if calculated.
            if( estimateFlip && verbose > 1 )
            {
                Format.print(System.out, "flip: %4.3f", flip[ii]);
                System.out.print(" ");
            }

			stop = System.currentTimeMillis();

            // Show the time.
			time[ii] = (stop-start)/1000.0;
			if( verbose > 1 )
			{
				Format.print(System.out, "time: %4.3f", time[ii]);
				System.out.println(" ");
			}
        }

		long total_stop = System.currentTimeMillis();

		int[] numZeros = new int[1];
//		System.out.print("\r                                      \r");
		for(int bin=0; bin<t2bins.size(); bin++)
		{
			System.out.print( ((T2Bin)t2bins.get(bin)).getName() );
			Format.print(System.out, " [%9.9s]", getMethodName());
			System.out.print(" -> ");
			Format.print(System.out, "fr: %4.3f ", calcMean( fr[bin], false, numZeros) );
			Format.print(System.out, "(%4.3f", calcStd( fr[bin], false ) );
			Format.print(System.out, ", %d) ", numZeros[0]);

			Format.print(System.out, "dn: %4.3f ", calcMean( dn[bin], false, numZeros) );
			Format.print(System.out, "(%4.3f", calcStd( dn[bin], false ) );
			Format.print(System.out, ", %d) ", numZeros[0]);

			Format.print(System.out, "gm: %4.3f ", calcMean( gm[bin], false, numZeros ) );
			Format.print(System.out, "(%4.3f", calcStd( gm[bin], false ) );
			Format.print(System.out, ", %d)\n", numZeros[0]);
		}
	
		if( estimateFlip )
		{
			Format.print(System.out, "Flip: %4.3f", calcMean( flip ) );
			Format.print(System.out, " (%4.3f)", calcStd( flip ) );
			System.out.println("");
		}

		if( verbose > 0 )
		{
			Format.print(System.out, "Chi2: %4.3f", calcMean( chi2 ) );
			Format.print(System.out, " (%4.3f)", calcStd( chi2 ) );
			System.out.println("");
			Format.print(System.out, "Time per solution (s): %4.3f", calcMean( time ) );
			Format.print(System.out, " (%4.3f)", calcStd( time ) );
			System.out.println("");
			Format.print(System.out, "Total time (s): %4.3f", 
				(double)(total_stop-total_start)/1000.0);
			System.out.println("");
		}
	}


}
