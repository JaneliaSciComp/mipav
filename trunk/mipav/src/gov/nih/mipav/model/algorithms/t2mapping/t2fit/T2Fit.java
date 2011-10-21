package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.awt.Dimension;
import java.util.Arrays;
import java.util.Random;

import javax.swing.JFrame;

import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2DistributionPanel;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit.QuadraticFit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.golden.Golden;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate.CubicSpline1D;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.simplex.TSimplexRoot;
import gov.nih.mipav.model.algorithms.t2mapping.cj.mri.hennig.Decay;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;

/**  T2Fit is the abstract base class for all
 *   the other fitting routines.  It stores
 *   the primary variables that are needed for 
 *   fitting T2 decay curves.
 *
 */
abstract public class T2Fit 
{
	/** Distribution Frame. */
	protected JFrame distFrame = null;
	protected T2DistributionPanel distPanel = null;

	/** Array of echo times */
	protected double[] te;

	/** Array of standard deviations for normalizing */
	protected double[] stds;

	/** Do we divide by the standard deviations */
	protected boolean normalize_f = false;

	/** Chi2 of the current solution. */
	protected double chi2 = 0.0;

	/** How verbose do you want to be? */
	protected int verbose = 0;

	/** Are we to estimate the flip angle as well? */
	protected boolean estimateFlip = false;

	/** The baseline of the fit. */
	protected double baseline = 0.0;

	/** ALL derived classes must tell us how 
	 *  many parameters it is going to return.
	 */
	abstract public int getNParams();

	/** Set the flip angle estimator */
	final static public int ESTIMATE_FLIP_SPLINE = 0;
	final static public int ESTIMATE_FLIP_QUADRATIC = 1;
	final static public int ESTIMATE_FLIP_MINIMIZE = 2;
	final static public int ESTIMATE_FLIP_GOLDEN = 3;
	protected int estimateFlipMethod = ESTIMATE_FLIP_GOLDEN;

	private QuadraticFit quadfit = null;
	private CubicSpline1D spline = null;
	private T2FitMinimize t2fm = null;
	private T2FitGolden t2golden = null;

	private double alpha_best;
	public double getBestAlpha() { return alpha_best; }
	private double[] xmin = null;

	protected Decay decay = new Decay();

	protected double t1 = 1000.0;
	protected double tr = 3000.0;

	protected double alpha = 180.0;
	
	public double getAlpha() 
	{ 
		return alpha;
	}
	
	public void setAlpha(double alpha) 
	{ 
		this.alpha = alpha; 

		if( alpha != 180.0 )
		{
			decay.setRefocusFlip(alpha);
		}
	}

	/**
	 *  Set the TR for the decay routine if needed.
	 */
	public void setTR( double tr )
	{
		this.tr = tr;
		decay.setTR( tr );
	}

	/** 
	 *  Default constructor, sets up a basic TE
	 *  vector and T2 vector.
	 */
	public T2Fit()
	{
		
	}

	/**
	 * Constructor with TE and T2.
	 *     
	 * @author Craig Jones
	 * @param  element the element to add
	 * @return condition:
	 * <PRE>
	 * </PRE>
	**/
	public T2Fit(final double te_in[])
	{
		setTE( te_in );
		decay.setNechoes(te_in.length);
	}

	/**
	 * Constructor with TE and T2.
	 *     
	 * @author Craig Jones
	 * @param  element the element to add
	 * @return condition:
	 * <PRE>
	 * </PRE>
	**/
//	public T2Fit(final double te_in[], final double t2_in[] )
//	{
//		setTE( te_in );
//		setT2( t2_in );
//	}

	/**
	 *  Set the method of estimating the flip angle.
	 */
	public void setEstimateFlipMethod(final String method)
	{
		if( method.toLowerCase().compareTo("spline") == 0 )
		{
			setEstimateFlipMethod( ESTIMATE_FLIP_SPLINE );
		}
		else if( method.toLowerCase().compareTo("quadratic") == 0 )
		{
			setEstimateFlipMethod( ESTIMATE_FLIP_QUADRATIC );
		}
		else if( method.toLowerCase().compareTo("minimize") == 0 )
		{
			setEstimateFlipMethod( ESTIMATE_FLIP_MINIMIZE );
		}
		else if( method.toLowerCase().compareTo("golden") == 0 )
		{
			setEstimateFlipMethod( ESTIMATE_FLIP_GOLDEN );
		}
		else
		{
			System.out.println("bad method");
			System.exit(-1);
		}
	}

	/**
	 *  Set the method of estimating the flip angle.
	 */
	public void setEstimateFlipMethod(int method)
	{
		estimateFlipMethod = method;
	}

	/**  Set the verbosity of this object.
	 *
	 */
	public void setVerbose( final int verbose )
	{
		this.verbose = verbose;
	}

	public int getVerbose( )
	{
		return verbose;
	}

	public void setEstimateFlip(final boolean do_estimate )
	{
		estimateFlip = do_estimate;
	}

	public boolean getEstimateFlip()
	{
		return estimateFlip;
	}

	/**  Set the parameters for the class.  The parameters
	 *   are used by the class and will differ between
	 *   each class.
	 *
	 */
    public void setParameters( final double[] params )
	{
	}

//	abstract double getMisfit();

//	/** Get the chi2.
//	 */
//	public double getChi2( )
//	{
//		return 0.0; 
//	} 

	/** Set the TE times.
	 */
	public void setT1( final double t1 )
	{
		this.t1 = t1;
		decay.setT1(t1);
	}

	/** Set the TE times.
	 */
	public void setTE( final double[] te )
	{
		if( this.te == null || this.te.length != te.length )
		{
			this.te = new double[ te.length ];
		}

		System.arraycopy(te, 0, this.te, 0, te.length);

		decay.setTE(te[0]);
		decay.setNechoes(te.length);

		if( stds == null )
        {
            stds = new double[ te.length ];
            Arrays.fill(stds, 1.0);
        }

	}

	/** Get the TE times.
	 */
	public double[] getTE( )
	{
		return (double[])te.clone();
	}

	/** Set the standard deviation array 
	 */
	public void setStdDev( final double[] stds_in ) 
	{
		if( stds == null || stds.length != stds_in.length )
		{
			stds = new double[ stds_in.length ];
		}

		System.arraycopy(stds_in, 0, stds, 0, stds_in.length);
	}

	/** Get the standard deviation array
	 */
	final public double[] getStdDev( ) 
	{
		return (double[])stds.clone();
	}

	final public double getBaseline() { return baseline; }

	/**  Main method that needs to be defined that does
	 *   the solving of the data (stored in b) and returns
	 *   the solution in x.  Again, what is stored in x
	 *   will differ depending on the subclass.
	 */
	abstract public double solveSimple(final double data[], T2Distribution dist);

	/**
	 *  This method determines if we are solving with 
	 *  an unknown flip angle or with a known flip angle.
	 *  If it is known, then we just call solveSimple, if
	 *  it is uknown, then we use the appropriate solver.
	 */
	public double solveBootstrap(final double[] data, T2Distribution dist)
	{
		/*
		 *  Calculate the initial solution.
		 */
		double chi2 = solve(data, dist);

		/*
		 *  Reconstruct the data based on the distribution.
		 */
		double[] fittedData = new double[ data.length ];
		Arrays.fill( fittedData, 0.0 );

		int numAmps = dist.getT2s().length;
		for(int ii=0; ii<fittedData.length; ii++)
		{
			for(int jj=0; jj < numAmps; jj++) 
			{
				fittedData[ii] += dist.getAmplitudes(jj) * 
				                  Math.exp( -te[ii] / dist.getT2s(jj) );
			}
		}

		/*
		 *  Now compute the residuals.
		 */
		
		double[] residuals = new double[ fittedData.length ];

		for(int ii=0; ii<fittedData.length; ii++)
		{
			residuals[ii] = fittedData[ii] - data[ii];
		}

		/*
		 *  Now run through a bunch of iterations 
		 *  and calculate up an average distribution.
		 */
		double[] tempAmps = new double[ numAmps ];
		Arrays.fill(tempAmps, 0.0);

		double[] newCurve = new double[ fittedData.length ];
		Random rand = new Random();

		for(int ii=0; ii<100; ii++)
		{
			/*
			 *  Create a new curve.
			 */
			for( int jj=0; jj < fittedData.length; jj++)
			{
				int randNum = rand.nextInt( residuals.length );
				newCurve[jj] = fittedData[jj] + residuals[ randNum ];
			}

			/*
			 *  Now solve it.
			 */
			solve(newCurve, dist);

			/* 
			 *  Now deal with the distribution.
			 */
			for(int jj=0; jj < numAmps; jj++)
			{
				tempAmps[jj] += dist.getAmplitudes(jj);
			}
		}

		for(int jj=0; jj < numAmps; jj++)
		{
			tempAmps[jj] /= 100.0; // number of bootstrap iterations
		}

		dist.setAmplitudes(tempAmps);

		return chi2;
	}

	/**
	 *  This method determines if we are solving with 
	 *  an unknown flip angle or with a known flip angle.
	 *  If it is known, then we just call solveSimple, if
	 *  it is unknown, then we use the appropriate solver.
	 */
	public double solve(final double[] data, T2Distribution dist)
	{
		double chi2 = 0.0;

		if( !estimateFlip )
		{
			chi2 = solveSimple(data, dist);
		}
		else 
		{
			switch( estimateFlipMethod )
			{
				case ESTIMATE_FLIP_SPLINE:
					chi2 = solveSpline(data, dist);
					break;
				case ESTIMATE_FLIP_QUADRATIC:
					chi2 = solveQuadratic(data, dist);

					// If it is a bad result, then we are going to 
					// use the minimizer.
					if( alpha_best < 50.0 )
					{
						chi2 = solveMinimize(data, dist);
					}

					if( alpha_best > 180.0 )
					{
						alpha_best = 180.0;
						setAlpha( alpha_best );
						chi2 = solveSimple(data, dist);
					}

					break;
				case ESTIMATE_FLIP_MINIMIZE:
					chi2 = solveMinimize(data, dist);
					break;
				case ESTIMATE_FLIP_GOLDEN:
					chi2 = solveGolden(data, dist);
					break;
				default:
					System.out.println("T2Fit: Unknown flip angle estimator");
					System.exit(-1);
			}

		}

//		showDistribution( dist );

		return chi2;
	}

	private void showDistribution(T2Distribution dist)
	{
		if( distFrame == null )
		{
			distFrame = new JFrame();
			distFrame.setDefaultCloseOperation(3);

			distPanel = new T2DistributionPanel();
			distFrame.getContentPane().add( distPanel );

			distFrame.setSize( new Dimension(256, 256));
			distFrame.show();
		}

		distPanel.setXY( dist.getT2s(), dist.getAmplitudes() );
	}

	/**
	 *  Solve the T2 problem using a spline based method for
	 *  estimating the flip angle.
	 */
	private double solveSpline( final double[] b, T2Distribution dist)
	{
		/* 
		 *  Allocate space for the result of the 
		 *  <b>current</b> minimum chi2 solution.
		 */
//		if( xmin == null || xmin.length != x.length )
//		{
//			xmin = new double[ x.length ];
//		}

		if( spline == null )
		{
			spline = new CubicSpline1D();
		}
	
		double[] alphas = {50, 90, 120, 140, 160, 170, 175, 180};
		double[] chi2 = new double[ alphas.length ];

		for(int ii=0; ii <alphas.length; ii++)
		{
			setAlpha((double)alphas[ii]);
			chi2[ii] = solveSimple(b, dist);
		}

		// Calculate the spline fit ...
		double[] newalphas = new double[101];
		double[] newchi2s = new double[101];

		for(int ii=0; ii<101; ii++)  newalphas[ii] = 80.0+(double)ii;
		spline.interpolate(alphas, chi2, newalphas, newchi2s);

		//  Now find the minimum chi2 and calculate the new solution.
		int index_min = 0;
		double chi2_min = Double.POSITIVE_INFINITY;

		for(int ii=0; ii<newalphas.length; ii++)
		{
			if( newchi2s[ii] < chi2_min )
			{
				chi2_min = newchi2s[ii];
				index_min = ii;
			}
		}

		alpha_best = newalphas[index_min];
		setAlpha(alpha_best);
		chi2[0] = solveSimple(b, dist);

		return chi2[0];
	}

	/**
	 *  Solve the T2 problem using a quadratic based method for
	 *  estimating the flip angle.
	 */
	public double solveQuadratic( final double[] b, T2Distribution dist)
	{
		double[] alphas = new double[12];
		double[] chi2 = new double[12];

		for(int ii=0; ii<12; ii++) alphas[ii] = 70.0 + (double)ii*10.0;

		if( quadfit == null ) 
		{
			quadfit = new QuadraticFit();
		}

		/*
		 *  Compute the solutions...
		 */
		double min_chi2 = Double.POSITIVE_INFINITY;
		int min_chi2_index = 0;
		for(int ii=0; ii<alphas.length; ii++ )
		{
			setAlpha(alphas[ii]);
			chi2[ii] = solveSimple(b, dist);

			if( chi2[ii] < min_chi2 )
			{
				min_chi2 = chi2[ii];
				min_chi2_index = ii;
			}

			if( verbose > 0 )
			{
				System.out.println("T2FitNNLSAlphasmall: Flip angle of " + 
				    alphas[ii] + " had a chi2 of " + chi2[ii]);
			}
		}

		/* 
		 *  Find the minimum of them and copy 2 to the 
		 *  left and 2 to the right.  This is then
		 *  what will be used in the quadratic fit.
		 */
		double[] aa = new double[5];
		double[] cc = new double[5];

		int start = (min_chi2_index>=2)?(min_chi2_index-2):0;
		int end = start + 5;
		if( end >= chi2.length )
		{
			end = chi2.length-1;
			start = end-5;
		}
	
		System.arraycopy(alphas, start, aa, 0, 5);
		System.arraycopy(chi2, start, cc, 0, 5);

		/*
		 *  Now fit the quadratic.
		 */
		quadfit.doFit(aa, cc);
		double[] p = quadfit.getParams();

		/*
		 *  The minimum of a quadratic a x^2 + b x + c is
		 *  at   2 a x + b = 0  and therefore x = -b / ( 2 a )
		 */
		alpha_best = -p[1]/(2.0*p[2] );
		setAlpha( alpha_best );
		chi2[0] = solveSimple(b, dist);
		
		if( verbose > 0 )
		{
			System.out.println("Quad solver: flip angle is " + alpha_best);
		}

		return chi2[0];
	}

	/**
	 *  Solve the T2 problem using a minimizer based method for
	 *  estimating the flip angle.
	 */
	public double solveMinimize( final double[] b, T2Distribution dist)
	{
		double chi2 = 0.0;
		
		if( t2fm == null ) 
		{
			t2fm = new T2FitMinimize(this);
		}

		chi2 = t2fm.doMinimize(b, dist);

		alpha_best = t2fm.getAlphaBest();

		return chi2;
	}

	/**
	 *  Solve the T2 problem using a golden searcdh based method for
	 *  estimating the flip angle.
	 */
	public double solveGolden( final double[] b, T2Distribution dist)
	{
		double chi2 = 0.0;
		
		if( t2golden == null ) 
		{
			t2golden = new T2FitGolden(this);
		}

		chi2 = t2golden.doMinimize(b, dist);

		alpha_best = t2golden.getAlphaBest();

		return chi2;
	}
}

/**
 *  The class does a minimization to find the best
 *  flip angle given the data.  It uses the code
 *  in cj.Math.simplex (which really needs lots of
 *  work, in particular in the speed with which it
 *  runs).
 */
class T2FitGolden extends Golden
{
	private double chi2 = 0.0;
	private double[] data = null;
	private T2Distribution distinternal = null;
	T2Fit t2fitter = null;
	private double best_alpha = 0.0;
	private int func_evals = 0;

	/**
	 *  The constructor which must be passed
	 *  the fitting method (which is just the
	 *  "this" of the caller.
	 */
	T2FitGolden(T2Fit t2fitter)
	{
		data = new double[ t2fitter.getTE().length ];
		setTolerance(0.01);
		distinternal = new T2Distribution();
		this.t2fitter = t2fitter;
	}

	public double getAlphaBest() { return best_alpha; }

	/**
	 *  This is the actual minimizer code.  We first
	 *  must allocate the data if it is not already 
	 *  allocated.  Then run the minimizer and finally
	 *  copy out the result.
	 */
	public double doMinimize(final double[] b, T2Distribution dist)
	{
		func_evals = 0;
		System.arraycopy(b, 0, data, 0, b.length);
		golden(50, 140, 180);
		dist.set(distinternal);

		return chi2;
	}

	/**
	 *  The objective function of the minimizer.
	 */
	protected double function(double alpha)
	{
		best_alpha = alpha;
		func_evals++;
		t2fitter.setAlpha( alpha );
		chi2 = t2fitter.solveSimple(data, distinternal);
		return chi2;
	}
}

/**
 *  The class does a minimization to find the best
 *  flip angle given the data.  It uses the code
 *  in cj.Math.simplex (which really needs lots of
 *  work, in particular in the speed with which it
 *  runs).
 */
class T2FitMinimize extends TSimplexRoot
{
	private double chi2 = 0.0;
	private double[] data = null;
	private T2Distribution distinternal = null;
	T2Fit t2fitter = null;
	private double best_alpha = 0.0;
	private int func_evals = 0;

	/**
	 *  The constructor which must be passed
	 *  the fitting method (which is just the
	 *  "this" of the caller.
	 */
	T2FitMinimize(T2Fit t2fitter)
	{
		super(2);

		this.t2fitter = t2fitter;

		setParamName(1, "alpha");
		setMinimum(1, 50.0);
		setMaximum(1, 180.0);

		setFitnessName("Name");
		setFitnessMinimum(0.0);
		setFitnessMaximum(9999999999.0);
		setOptionMinimize(true);

//		xin = new double[ t2fitter.t2.length ];
		distinternal = new T2Distribution();
	}

	public double getAlphaBest() { return best_alpha; }

	/**
	 *  This is the actual minimizer code.  We first
	 *  must allocate the data if it is not already 
	 *  allocated.  Then run the minimizer and finally
	 *  copy out the result.
	 */
	public double doMinimize(final double[] b, T2Distribution dist)
	{
		if( data == null || data.length != b.length )
		{
			data = new double[ b.length ];
		}
		
		System.arraycopy(b, 0, data, 0, b.length);

		Run(0.01, 100);

		dist.set(distinternal);

		return chi2;
	}

	/**
	 *  The objective function of the minimizer.
	 */
	public double evalParams(int cParams, double aParamValue[])
	{
		func_evals++;

		t2fitter.setAlpha( aParamValue[1] );
		chi2 = t2fitter.solveSimple(data, distinternal);
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
		best_alpha = ParamValue[1];
		func_evals = 0;
	}
}
