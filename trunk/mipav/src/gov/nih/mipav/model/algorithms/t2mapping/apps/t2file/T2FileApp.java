package gov.nih.mipav.model.algorithms.t2mapping.apps.t2file;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;

final public class T2FileApp {
	public static void main(String argv[]) 
	{ 
		String[] bob = new String[ argv.length];
		for(int ii=0; ii<argv.length; ii++) bob[ii] = new String(argv[ii]);
		new T2FileKit().main(bob); 
	}
}


class T2FileKit extends AppKit
{
	/** The T2 bin information. */
	private ArrayList t2bins = new ArrayList();

	/** The T2 bin information. */
	private double true_alpha = -1.0;  // i.e. undefined

	/** Do we want to estimate the flip angle? */
	private boolean estimateFlip = false;

	/** The TE times. */
	private ArrayList tes = null;

	/** Standard deviations */
	private ArrayList stds = null;

	/** Data */
	private ArrayList data = null;

	/** TR */
	private double tr = 3000;  // in ms

	/** T1 */
	private double t1 = 1000;  // in ms

	/** Method from which to estimate the flip angle. */
	private String estimateMethod = "golden";

	/** Method from which to fit the decay curve. */
	private ArrayList fitMethod = null;
	private ArrayList fitMethod_names = null;

	double biexp_first = 0.0;
	double biexp_second = 0.0;

	double nlower = 0.0;
	double nupper = 0.0;

	public void help() 
	{ 
		System.out.println(getAppName() + " [Options]");
		printOptions();
		
		System.out.println("Examples:");
		System.out.println("WM: -i 100 -b s 0 50 -b m 50 120 -c 200 1000 20 -c 800 1000 80 -f nnls ");
	}

	public String getDialogName() { return ""; }
	public String getAppName() { return "t2simulate"; }

	protected void moreOpts()
	{
		addOpt_reqArg("fit-method", 'f', "Decay curve fit method (def: nnls, must be one of 'nnls', 'nnls_small <arg> <arg>', 'l1', 'l1_small <arg> <arg>','monoexp', 'monoexp_nonlin', 'monoexp_nnls', 'biexp', 'biexp_fixed <arg> <arg>','biexp_sweep')");
		addOpt_reqArg("bin", 'b', 3, "Define a T2 bin (def: none)");
		addOpt_reqArg("alpha", 'a', "Set the true refocusing pulse flip angle (def: 180)");
		addOpt_reqArg("tr", 't', "Set the TR (def: 3000)");
		addOpt_reqArg("t1", '1', "Set the T1 (def: 1000)");
		addOpt_reqArg("compartment", 'c', 3, "Add a T2 water compartment (def: none)");
		addOpt_noArg("estimate-flip", 'e', "Estimate the flip angle (def: false)");
		addOpt_reqArg("estimate-flip-method", 800, "Flip estimation method (def: minimize, must be one of 'minimize', 'spline', 'quadratic', 'golden')");
	}

	protected boolean parseArg(int c)
	{
		if( t2bins == null )
		{
			t2bins = new ArrayList();
		}

		switch (c)
		{
			case '1': 
				t1 = getDoubleArg("t1");
				break;
			case 't': 
				tr = getDoubleArg("tr");
				break;
			case 800: 
				estimateMethod = getStringArg("flip estimation method");
				break;
			case 'e': 
				estimateFlip = true;
				break;
			case 'f': 
				addFitMethod( getStringArg("decay curve fit method") ) ;
				break;
			case 'a': 
				true_alpha = getDoubleArg("true alpha");
				break;
			case 'b':
				String name = getStringArg("bin name");
				t2bins.add( new T2Bin( getDoubleArg("bin start"), 
									   getDoubleArg("bin stop"),
									   name)); 
				break;
		}

		return true;
	}

	private void readData(String filename)
	{
		LEDataInputStream ldis = openInput(filename);
		String line = "";
		Pattern spaces = Pattern.compile("\\s+");

		tes = new ArrayList();
		data = new ArrayList();
		stds = new ArrayList();

		try
		{
			while( line != null )
			{
				line = ldis.readLine();

				if( line != null )
				{
					String[] nums = spaces.split(line.trim(), 3);

					tes.add( Double.valueOf( nums[0].trim() ) );
					data.add( Double.valueOf( nums[1].trim() ) );

					if( Double.valueOf( nums[2].trim() ).doubleValue() == 0.0 )
					{
						System.out.println("t2file: A standard deviation was 0.0, setting to 1.0 instead.");
						stds.add( new Double(1.0) );
					}
					else
					{
						stds.add( Double.valueOf( nums[2].trim() ) );
					}
				}
			}
		}
		catch( EOFException e)
		{
		}
		catch( IOException e )
		{
			System.out.println("IO problem " + e);
			System.exit(-1);
		}
	}

	public void run()
	{
		long start, end;
		double time;
		double[] tt = null;

		for(int ii=0; ii<otherArgs.length; ii++)
		{

			readData(otherArgs[ii]);

			// Check to make sure they defined at least one method.
			if( fitMethod == null || fitMethod.size() == 0 )
			{
				System.out.println("t2simulate: You must define at least one fit algorithm");
				System.exit(0);
			}

			T2Distribution distribution = new T2Distribution();

			long total_start = System.currentTimeMillis();

			// Do the solution for each fitter.
			for(int fi = 0; fi < fitMethod.size(); fi++) 
			{
				T2Fit fitter = (T2Fit)fitMethod.get(fi);

				fitter.setVerbose(verbose);

				// Set the standard deviations.
				tt = new double[ tes.size() ];
				for(int asdf=0; asdf<tes.size(); asdf++) 
					tt[asdf] = ((Double)stds.get(asdf)).doubleValue();
				fitter.setStdDev( tt );


				// We can't both calcualte a flip angle and
				// assume we know what it is!
				if( true_alpha > 0.0 && estimateFlip )
				{
					System.out.println("t2file: -a and -e are mutually exclusive");
					System.exit(-1);
				}

				if( estimateFlip )
				{
					fitter.setEstimateFlip(estimateFlip);

					fitter.setTR( tr );
					fitter.setT1( t1 );

					if( estimateMethod.compareTo("spline") == 0 )
					{
						fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_SPLINE );
					}
					else if( estimateMethod.compareTo("quadratic") == 0 )
					{
						fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_QUADRATIC );
					}
					else if( estimateMethod.compareTo("minimize") == 0 )
					{
						fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_MINIMIZE );
					}
					else if( estimateMethod.compareTo("golden") == 0 )
					{
						fitter.setEstimateFlipMethod( T2Fit.ESTIMATE_FLIP_GOLDEN );
					}
					else
					{
						System.out.print("The flip angle estimation method you ");
						System.out.println("specified " + estimateMethod + " is not known.");
					}
				}
				
				if( true_alpha > 0.0 )
				{
					fitter.setAlpha( true_alpha );
				}

				// Set the TE times.
				tt = new double[ tes.size() ];
				for(int asdf=0; asdf<tes.size(); asdf++) 
					tt[asdf] = ((Double)tes.get(asdf)).doubleValue();
				fitter.setTE(tt);

				// Solve 
				tt = new double[ tes.size() ];
				for(int asdf=0; asdf<tes.size(); asdf++) 
					tt[asdf] = ((Double)data.get(asdf)).doubleValue();
				start = System.currentTimeMillis();

				double chi2 = fitter.solve(tt, distribution);
				end = System.currentTimeMillis();

				// If estimate flip then get best flip angle.
				double flip = 0.0;
				if( estimateFlip )
				{
					flip = fitter.getBestAlpha();
				}

				if( verbose > 2 )
				{
					int len = distribution.getAmplitudes().length;
					for(int kk=0; kk<len; kk++)
					{
						if( distribution.getAmplitudes(kk) > 0.0 )
						{
							System.out.println(distribution.getAmplitudes(kk) + " " + 
								distribution.getT2s(kk));
						}
					}
				}

				/* 
				 *  Calculate and display for each bins.
				 */
				for(int bin=0; bin<t2bins.size(); bin++)
				{
					((T2Bin)t2bins.get(bin)).calculate(distribution);

					System.out.print( ((T2Bin)t2bins.get(bin)).getName() + ": ");
					Format.print(System.out, "%4.3f ", 
						((T2Bin)t2bins.get(bin)).getFR());
					Format.print(System.out, "%4.3f ", 
						((T2Bin)t2bins.get(bin)).getDN());
					Format.print(System.out, "%4.3f ", 
						((T2Bin)t2bins.get(bin)).getGM());
				}

				// Show the chi2.
				Format.print(System.out, "chi2: %4.3f", chi2);
				System.out.print(" ");

				// Show the flip angle, if calculated.
				if( estimateFlip )
				{
					Format.print(System.out, "flip: %4.3f", flip);
					System.out.print(" ");
				}

				// Show the time.
				time = (end-start)/1000.0;
				if( verbose > 0 )
				{
					Format.print(System.out, "time: %4.3f", time);
					System.out.println(" ");
				}
			}
		}

		long total_end = System.currentTimeMillis();

		// Now do the summary report.
		System.out.println("");
	}

	/**
	 *  Compute the mean of any vector.
	 */
    private final double calcMean(final double[] data)
    {
        double tt = 0.0;
        for(int ii=0; ii<data.length; ii++)
        {
            tt += data[ii];
        }

        return tt/(double)data.length;
    }

	/**
	 *  Compute the relative error of any vector.
	 */
    private final double calcRe(final double[] data, double true_value)
    {
        double tt = 0.0;
        for(int ii=0; ii<data.length; ii++)
        {
            tt += (data[ii]-true_value)/true_value*100.0;
        }

        return tt/(double)data.length;
    }

	/**
	 *  Compute the standard deviation of any vector.
	 */
    private final double calcStd(final double[] data)
    {
        double tt = 0.0;
        double tt2 = 0.0;
        int N = data.length;
        for(int ii=0; ii<N; ii++)
        {
            tt += data[ii];
            tt2 += data[ii]*data[ii];
        }

        return Math.sqrt((tt2-(tt*tt)/(double)N)/(double)(N-1) );
    }

	/**
	 *  Add a fitting routine to the list of routines.
	 */
	private void addFitMethod( final String fitname )
	{
		// Create the vectors if not created yet.
		if( fitMethod == null )
		{
			fitMethod = new ArrayList();
			fitMethod_names = new ArrayList();
		}

		fitMethod_names.add( fitname );

		// Figure out what type it is.
		if( fitname.compareTo("nnls") == 0 ) 
		{
			fitMethod.add( new T2FitNNLS( ) );
		}
		else if( fitname.compareTo("nnls_small") == 0 ) 
		{
			nlower = getDoubleArg("nnls_small first parameter");
			nupper = getDoubleArg("nnls_small second parameter");
			
			T2Fit temp = new T2FitNNLS( );
			double[] tt = {nlower, nupper};

			temp.setParameters(tt);
			fitMethod.add( temp );
		}
		else if( fitname.compareTo("l1") == 0 ) 
		{
			fitMethod.add( new T2FitL1( ) );
		}
		else if( fitname.compareTo("l1_small") == 0 ) 
		{
			nlower = getDoubleArg("l1_small first parameter");
			nupper = getDoubleArg("1l_small second parameter");
			
			T2Fit temp = new T2FitL1( );
			double[] tt = {nlower, nupper};

			temp.setParameters(tt);
			fitMethod.add( temp );
		}
		else if( fitname.compareTo("monoexp") == 0 ) 
		{
			fitMethod.add( new T2FitMonoExp( ) );
		}
//		else if( fitname.compareTo("monoexp_nonlin") == 0 ) 
//		{
//			fitMethod.add( new T2FitMonoExpNonLinear( ) );
//		}
		else if( fitname.compareTo("monoexp_nnls") == 0 ) 
		{
			fitMethod.add( new T2FitMonoExpNNLS( ) );
		}
		else if( fitname.compareTo("biexp") == 0 ) 
		{
			fitMethod.add( new T2FitBiExp( ) );
		}
		else if( fitname.compareTo("biexp_sweep") == 0 ) 
		{
			fitMethod.add( new T2FitBiExpSweep( ) );
		}
		else if( fitname.compareTo("biexp_fixed") == 0 ) 
		{
			biexp_first = getDoubleArg("biexp first parameter");
			biexp_second = getDoubleArg("biexp second parameter");

			T2Fit temp = new T2FitBiExpFixedT2( );
			double[] tt = {biexp_first, biexp_second};
			temp.setParameters(tt);
			fitMethod.add( temp );
		}
		else
		{
			System.out.println("t2simulate: Unknown fit method " + fitname);
			System.exit(-1);
		}
	}
}
