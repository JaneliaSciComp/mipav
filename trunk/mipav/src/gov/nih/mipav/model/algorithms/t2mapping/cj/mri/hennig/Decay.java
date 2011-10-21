package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.hennig;

import java.util.logging.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

public class Decay
{
	/* Number of echoes */
	private int nechoes = 10;

	/** Excitation pluse flip angle (in degrees) */
	private double excite = 90.0;

	/** Refocusing pluse flip angle (in degrees) */
	private double refocus = 180.0;

	/** Echo time (in ms) */
	private double te = 10.0;  // te = 2 * tau

	/** T1 time (in ms) */
	private double t1 = 1000.0;

	/** T2 time (in ms) */
	private double t2 = 80.0;

	/** Repitition time (in ms) */
	private double tr = Double.POSITIVE_INFINITY;

	private double e2 = 0.0;
	private double e1 = 0.0;

    /** Crusher efficiency (ie use stimulated echoes or not) */
	private double crushers = 0.0;

	/** PD (TE=0) signal intensity. (PD OR first_echo can be defined, not both.) */
	private double pd = 1.0;

	/** First echo amplitude. (PD OR first_echo can be defined, not both.) */
	private double firstEcho = 1.0;

	private double t1factor = 1.0;

	/** Verbosity */
	private int verbose = 0;

	private double[] F;
	private double[] Fstar;
	private double[] Z;
	private double[] Zstar;
	
	private double T11, T12, T13, T14;
	private double T21, T22, T23, T24;
	private double T31, T32, T33, T34;
	private double T41, T42, T43, T44;

	private double[] echoData=null;

	/** Create the logger. */
	static private Logger logger = CJLogger.createLogger("");
	static { CJLogger.setLevel(logger, Level.OFF); }
		
	/**  Default Constructor. */
	public Decay()
	{
		logger.entering("Decay", "Decay");

		init();

		logger.exiting("Decay", "Decay");
	}

	/**  Constructor and set number of echoes. */
	public Decay(int nechoes)
	{ 
		logger.entering("Decay", "Decay", "int nechoes");

		setNechoes(nechoes); 

		logger.exiting("Decay", "Decay", "int nechoes");
	}

	/**  Intialize the arrays and E1/E2. */
	private void init()
	{
		logger.entering("Decay", "init");

		if( F == null || nechoes != F.length )
		{
			F = new double[nechoes];
			Fstar = new double[nechoes];
			Z = new double[nechoes];
			Zstar = new double[nechoes];

			echoData = new double[nechoes];
		}

		updateE1();
		updateE2();

		logger.exiting("Decay", "init");
	}

	/**  Set the number of echoes. */
	public void setNechoes( final int nechoes )
		{ this.nechoes = nechoes; init(); updateT1Factor(); }

	/**  Set the refocusing pulse flip angle. */
	public void setRefocusFlip( final double refocus ) 
		{ this.refocus = refocus; }

	/**  Set the excitation pulse flip angle. */
	public void setExciteFlip( final double excite ) 
		{ this.excite = excite; }

	/**  Set the T2 (ms). */
	public void setT2( final double t2 )
		{ this.t2 = t2; updateE2(); }

	/**  Set the T1 (ms). */
	public void setT1( final double t1 )
		{ this.t1 = t1; updateE1(); updateT1Factor(); }

	/**  Set the TE (ms). */
	public void setTE( final double te )
		{ this.te = te; updateE1(); updateE2(); updateT1Factor(); }

	/**  Set the TR (ms). */
	public void setTR( final double tr )
		{ this.tr = tr; updateT1Factor(); }

	/**  Set the crusher efficiency. */
	public void setCrushers( final double crushers )
		{ this.crushers = crushers; }

	/**  Set the first echo scaling. */
	public void setFirstEcho( final double firstEcho )
		{ this.firstEcho = firstEcho; }

	/**  Set the proton density (signal at TE=0 ms). */
	public void setPD( final double pd )
		{ this.pd = pd; }

	/**  Set the verbosity. */
	public void setVerbose( final int verbose )
		{ this.verbose = verbose; }

	public double getTE() { return te; }
	public double getTR() { return tr; }
	public double getT1() { return t1; }
	public double getT2() { return t2; }
	public double getRefocusFlip() { return refocus; }
	// public double getCrushers() { return crushers; }

	public double[]  calculateEchoes()
	{
		logger.entering("Decay", "calculateEchoes");

		java.util.Arrays.fill(F, 0.0);
		java.util.Arrays.fill(Fstar, 0.0);
		java.util.Arrays.fill(Z, 0.0);
		java.util.Arrays.fill(Zstar, 0.0);

		Z[0] = pd;

		/*
		 *  Do excitation pulse
		 */
		applyRotation(0, excite);
		t1relax(0);
		t2relax(0);

		calculateTp( StrictMath.toRadians(refocus) );

		for(int ii=0; ii<nechoes; ii++)
		{
			logger.fine("Iteration " + (ii+1) + " of " + nechoes);

			// Do refocusing pulse.
			applyRotation(ii);

			// Do relaxation.
			t1relax(ii);
			t2relax(ii);

			// Get the data.
			echoData[ii] = StrictMath.abs(Fstar[0]) * t1factor;

			// Do relaxation.
			t1relax(ii);
			t2relax(ii);

			// Do evolution.
			evolve();
		}

		/*
		 *  Copy the data over to the output array.
		 */
		double[] data = new double[ echoData.length ];
		System.arraycopy(echoData, 0, data, 0, echoData.length);

		/*
		 *  If the user required that the first echo
		 *  be scaled to a specific value, then do
		 *  the scaling here.
		 */
		if( firstEcho != 1.0 )
		{
			double first = data[0];
			for(int ii=0; ii<echoData.length; ii++)
				data[ii] = data[ii] / first * firstEcho;
		}

		logger.exiting("Decay", "calculateEchoes");

		return data;
	}

	private void evolve()
	{
		logger.entering("Decay", "evolve");

		// F(n) -> F(n+1)
		System.arraycopy(F, 0, F, 1, F.length-2);

		F[0] = Fstar[0];

		// Fstar(n+1) -> Fstar(n)
		System.arraycopy(Fstar, 1, Fstar, 0, F.length-1);

		logger.exiting("Decay", "evolve");
	}

	/**  Update the T2 factor.  */
	private void updateE2()
		{ e2 = StrictMath.exp(-(te/2.0) / t2); }

	/**  Update the T1 factor.  */
	private void updateE1()
		{ e1 = StrictMath.exp(-(te/2.0) / t1); }

	/**  Compute the T1 factor.  */
	private void updateT1Factor()
	{
		double tt = 0.0;
		for(int k=1; k <= nechoes; k++)
		{
			tt += StrictMath.pow(-1.0, (double)(nechoes-k)) * 
							StrictMath.exp( -(tr - (2.0*(double)k-1.0)*(te/2.0)/2.0) / t1 );
		}
		t1factor = (1.0 - 2.0*tt - StrictMath.pow(-1.0,nechoes)*StrictMath.exp(-tr/t1));
	}

	private void applyRotation(final int iter, final double flip)
	{
		calculateTp( StrictMath.toRadians(flip) );
		applyRotation(iter);
	}

	private void applyRotation(final int iter)
	{
		for(int ii=0; ii<(iter+1); ii++)
		{
			double F_temp = F[ii], Fstar_temp = Fstar[ii], Z_temp = Z[ii], Zstar_temp = Zstar[ii];

			// F[ii] = T11*F_temp + T12*Fstar_temp + T13*Z_temp + T14*Zstar_temp;
			// Fstar[ii] = T21*F_temp + T22*Fstar_temp + T23*Z_temp + T24*Zstar_temp;
			// Z[ii] = T31*F_temp + T32*Fstar_temp + T33*Z_temp + T34*Zstar_temp;
			// Zstar[ii] = T41*F_temp + T42*Fstar_temp + T43*Z_temp + T44*Zstar_temp;

			F[ii] = T11*F_temp + T12*Fstar_temp + T13*Z_temp;
			Fstar[ii] = T21*F_temp + T22*Fstar_temp + T24*Zstar_temp;
			Z[ii] = T31*F_temp + T32*Fstar_temp + T33*Z_temp;
			Zstar[ii] = T41*F_temp + T42*Fstar_temp + T44*Zstar_temp;
		}
	}

	private void calculateTp(final double flip)
	{
		//Tone = [[ (cos(refocus/2))^2, (sin(refocus/2))^2, sin(refocus), 0]; ...
	    //        [ (sin(refocus/2))^2, (cos(refocus/2))^2, 0, sin(refocus)]; ...
		// 		  [ -1/2*sin(refocus), 1/2*sin(refocus), cos(refocus), 0]; ...
		// 	      [ 1/2*sin(refocus), -1/2*sin(refocus), 0, cos(refocus)]];

		T11 = StrictMath.pow( StrictMath.cos(flip/2.0), 2.0 ); 
		T12 = StrictMath.pow( StrictMath.sin(flip/2.0), 2.0 ); 
		T13 = StrictMath.sin(flip);
		T14 = 0.0;
		
		T21 = StrictMath.pow( StrictMath.sin(flip/2.0), 2.0 ); 
		T22 = StrictMath.pow( StrictMath.cos(flip/2.0), 2.0 ); 
		T23 = 0.0;
		T24 = StrictMath.sin(flip);
		
		T31 = -1.0/2.0*StrictMath.sin(flip);
		T32 = 1.0/2.0*StrictMath.sin(flip);
		T33 = StrictMath.cos(flip);
		T34 = 0.0;
		
		T41 = 1.0/2.0*StrictMath.sin(flip);
		T42 = -1.0/2.0*StrictMath.sin(flip);
		T43 = 0.0;
		T44 = StrictMath.cos(flip);
	}


	private void t2relax(final int iter)
	{
		for(int ii=0; ii<(iter+1); ii++) 
		{
			F[ii] *= e2;
			Fstar[ii] *= e2;
		}
	}

	private void t1relax(final int iter)
	{
		for(int ii=0; ii<(iter+1); ii++) 
		{
			Z[ii] *= e1;
			Zstar[ii] *= e1;
		}
	}
}
