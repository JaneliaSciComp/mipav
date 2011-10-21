package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process.register;

import java.util.logging.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MIF4;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.transform.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.golden.Golden;
import gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.nr.*;
import gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.*;

public class RegisterRigid extends Register
{
	static private Logger logger = CJLogger.createLogger("");
	static { CJLogger.setLevel(logger, Level.FINE); }

	SimilarityMeasure sm;

	/** The lower and upper bounds on the rotations. */
	double[] rotationLower = {-20.0, -20.0, -20.0};
	double[] rotationUpper = {20.0, 20.0, 20.0};

	/** The lower and upper bounds on the translations. */
	double[] translationLower = {-20.0, -20.0, -20.0};
	double[] translationUpper = {20.0, 20.0, 20.0};

	/** Skipping of rows. */
	private int[] rowSkip = {5};

	/** Skipping of columns. */
	private int[] colSkip = {5};
	
	/** Skipping of slices. */
	private int[] sliceSkip = {1};

	/** Initial guesses for the translations. */
	private double initTranslationX = 0.0;
	private double initTranslationY = 0.0;
	private double initTranslationZ = 0.0;

	/** Initial guesses for the rotations. */
	private double initRotationX = 0.0;
	private double initRotationY = 0.0;
	private double initRotationZ = 0.0;

	private String similarity = "woods";

	private double ftol = 10^-6;

	/**
	 *  Constructor that takes two data volumes.
	 */
	public RegisterRigid()
	{
		super();
	}

	/** Set the number of rows to skip. */
	public void setSimilarityMeasure(final String similarity) 	
		{ this.similarity = similarity; }

	public void setTolerance( final double ftol )
		{ this.ftol = ftol; }

	/*------------------------------------------------------------------
	 *
	 *  Set the initial translations and rotations.
	 *
	 *------------------------------------------------------------------
	 */

	/** Set the initial X translation. */
	public void setInitialTranslationX(final double initTranslationX ) 
		{ this.initTranslationX  = initTranslationX; }

	/** Set the initial Y translation. */
	public void setInitialTranslationY(final double initTranslationY ) 
		{ this.initTranslationY  = initTranslationY; }

	/** Set the initial Z translation. */
	public void setInitialTranslationZ(final double initTranslationZ ) 
		{ this.initTranslationZ  = initTranslationZ; }

	/** Set the initial X rotation. */
	public void setInitialRotationX(final double initRotationX ) 
		{ this.initRotationX  = initRotationX; }

	/** Set the initial Y rotation. */
	public void setInitialRotationY(final double initRotationY ) 
		{ this.initRotationY  = initRotationY; }

	/** Set the initial Z rotation. */
	public void setInitialRotationZ(final double initRotationZ ) 
		{ this.initRotationZ  = initRotationZ; }

	/*------------------------------------------------------------------
	 *
	 *  Set the rows, columns and slices to skip.
	 *
	 *------------------------------------------------------------------
	 */

	/** Set the number of rows to skip. */
	public void setRowSkip(final int[] rowSkip) 
	{ 	
		if( this.rowSkip.length != rowSkip.length )
			this.rowSkip = new int[ rowSkip.length ];

		System.arraycopy(rowSkip, 0, this.rowSkip, 0, rowSkip.length);
	}

	/** Set the number of columns to skip. */
	public void setColSkip(final int[] colSkip) 
	{ 	
		if( this.colSkip.length != colSkip.length )
			this.colSkip = new int[ colSkip.length ];

		System.arraycopy(colSkip, 0, this.colSkip, 0, colSkip.length);
	}


	/** Set the number of columns to skip. */
	public void setSliceSkip(final int[] sliceSkip) 
	{ 	
		if( this.sliceSkip.length != sliceSkip.length )
			this.sliceSkip = new int[ sliceSkip.length ];

		System.arraycopy(sliceSkip, 0, this.sliceSkip, 0, sliceSkip.length);
	}


	/*------------------------------------------------------------------
	 *
	 *  Set the bounds on the rotations and translations.
	 *
	 *------------------------------------------------------------------
	 */

	/** Set the lower bound on the rotations. */
	public void setRotationLower(final double[] rotationLower)
		{ System.arraycopy(rotationLower, 0, this.rotationLower, 0, 3); }

	/** Set the lower bound on the rotations. */
	public void setRotationUpper(final double[] rotationUpper)
		{ System.arraycopy(rotationUpper, 0, this.rotationUpper, 0, 3); }

	/** Set the lower bound on the rotations. */
	public void setTranslationLower(final double[] rotationLower)
		{ System.arraycopy(rotationLower, 0, this.rotationLower, 0, 3); }

	/** Set the lower bound on the rotations. */
	public void setTranslationUpper(final double[] rotationUpper)
		{ System.arraycopy(rotationUpper, 0, this.rotationUpper, 0, 3); }

	public Volume register(final Volume fixed, double[] voxFixed, 
	                       final Volume dynamic, double[] voxDynamic)
	{
		// Setup the matrices
		double[] p = new double[6];
		double[][] xi = new double[6][6];
		for(int i=0;i<xi.length;i++) xi[i][i] = 1.0;

		p[0] = initRotationX;
		p[1] = initRotationY;
		p[2] = initRotationZ;
		p[3] = initTranslationX;
		p[4] = initTranslationY;
		p[5] = initTranslationZ;

		//  Setup the function minimizer.
		if( similarity.toLowerCase().startsWith("wo") )
			sm = new WoodsSimilarity();
		else if( similarity.toLowerCase().startsWith("mu") || 
		         similarity.toLowerCase().startsWith("sh") )
			sm = new ShannonSimilarity();
		else if( similarity.toLowerCase().startsWith("l1") )
			sm = new L1Similarity();
		else if( similarity.toLowerCase().startsWith("l2") )
			sm = new L2Similarity();
		else
			throw new IllegalArgumentException("Unknown similarity measure "+similarity);

		OurFunction toMinimize = new OurFunction(fixed, voxFixed, dynamic, voxDynamic, sm);

		toMinimize.verbose = this.verbose;

		toMinimize.setRowInterpolator( internalInterpY );
		toMinimize.setColInterpolator( internalInterpX );
		toMinimize.setSliceInterpolator( internalInterpZ );

		//  Set the lower and upper "bounds" on the rotation
		//  and translations.
		toMinimize.setRotationLower( rotationLower );
		toMinimize.setRotationUpper( rotationUpper );
		toMinimize.setTranslationLower( translationLower );
		toMinimize.setTranslationUpper( translationUpper );

		Powell pp = new Powell();
		pp.setVerbose(this.verbose);

		for(int skipi=0; skipi < rowSkip.length; skipi++) 
		{
			sm.setRowSkip(rowSkip[skipi]);
			sm.setColSkip(colSkip[skipi]);
			sm.setSliceSkip(sliceSkip[skipi]);

			//  Create the Powell routine.
			pp.powell(p, xi, ftol, toMinimize);
			
			//  Printout the results.
			if ( verbose >= 1 )
			{
				System.out.println("Final parameters (rx ry rz tx ty tz)");
				for(int ii=0; ii<p.length; ii++) System.out.print(p[ii]+" ");
				System.out.println();
			}
		}

		//  Interpolate the new output volume based on the
		//  rotation and translation results.
		VolumeResampler out = new VolumeResampler(dynamic, voxDynamic);

		out.setOutputVoxelSize(voxFixed);
		out.setDimensions(fixed.getNRows(),fixed.getNCols(),fixed.getNSlices());

		// Setup the transform
		out.setRotationX(p[0]); 
		out.setRotationY(p[1]); 
		out.setRotationZ(p[2]); 
		out.setTranslationX(p[3]); 
		out.setTranslationY(p[4]); 
		out.setTranslationZ(p[5]); 

		out.setRowInterpolator( interpY );
		out.setColInterpolator( interpX );
		out.setSliceInterpolator( interpZ );

		return out.getVolume();
	}
}

class OurFunction implements MvFunction
{
	int verbose=0;

	private Volume vs, vd;
	private double[] voxVS = {0.0, 0.0, 0.0};
	private double[] voxVD = {0.0, 0.0, 0.0};

	/** The lower and upper bounds on the rotations. */
	private double[] rotationLower = {-8.0, -8.0, -8.0};
	private double[] rotationUpper = {8.0, 8.0, 8.0};

	/** The lower and upper bounds on the translations. */
	private double[] translationLower = {-10.0, -10.0, -10.0};
	private double[] translationUpper = {10.0, 10.0, 10.0};

	private VolumeResampler vr;
	private SimilarityMeasure sm;

	/**
	 *  Constructor that takes the static and dynamic volumes
	 *  as well as the interpolator.
	 */
	public OurFunction(final Volume stat, final double[] statVox,
                       final Volume dyn, final double[] dynVox,
					   final SimilarityMeasure sm)
	{
		super();

		// Copy over the input parameters.
		this.vs = stat; this.vd = dyn;

		System.arraycopy(statVox, 0, voxVS, 0, 3);
		System.arraycopy(dynVox, 0, voxVD, 0, 3);

		this.sm = sm;
		this.vr = new VolumeResampler(dyn, dynVox);
		this.vr.setOutputVoxelSize(statVox);
		this.vr.setDimensions(stat.getNRows(),stat.getNCols(),stat.getNSlices());
	}

	/** Set the interpolator for the rows. */
	public void setRowInterpolator( final InterpolateRow I)
		{ vr.setRowInterpolator( I ); }
	
	/** Set the interpolator for the columns. */
	public void setColInterpolator( final InterpolateRow I)
		{ vr.setColInterpolator( I ); }
	
	/** Set the interpolator for the slices. */
	public void setSliceInterpolator( final InterpolateRow I)
		{ vr.setSliceInterpolator( I ); }
	
	/** Set the lower bound on the rotations. */
	public void setRotationLower(final double[] rotationLower)
		{ System.arraycopy(rotationLower, 0, this.rotationLower, 0, 3); }

	/** Set the lower bound on the rotations. */
	public void setRotationUpper(final double[] rotationUpper)
		{ System.arraycopy(rotationUpper, 0, this.rotationUpper, 0, 3); }

	/** Set the lower bound on the rotations. */
	public void setTranslationLower(final double[] translationLower)
		{ System.arraycopy(translationLower, 0, this.translationLower, 0, 3); }

	/** Set the lower bound on the translations. */
	public void setTranslationUpper(final double[] translationUpper)
		{ System.arraycopy(translationUpper, 0, this.translationUpper, 0, 3); }

	/** Method to minimize as defined in MvFunction. */
	public double f(double[] x)
	{
		if ( verbose >= 2 )
			for(int ii=0; ii<x.length; ii++)
				{ Format.print(System.out, "%4.3f ", x[ii]); }

		vr.setRotationX(x[0]); 
		vr.setRotationY(x[1]); 
		vr.setRotationZ(x[2]); 
		vr.setTranslationX(x[3]); 
		vr.setTranslationY(x[4]); 
		vr.setTranslationZ(x[5]); 

		double bob = sm.distance(vs,vr);

		//  Compute the penalty for the rotations
		for(int i=0;i<3;i++)
			{ bob += Math.abs(bob)*(penalty(x[i],rotationLower[i],rotationUpper[i]) - 1); }

		//  Compute the penalty for the translations
		for(int i=0;i<3;i++)
			{ bob += Math.abs(bob)*(penalty(x[i+3],translationLower[i],translationUpper[i]) - 1); }

		if ( verbose >= 2 ) System.out.println(bob);
//		if ( verbose >= 2 ) Format.print(System.out, "%4.3f\n", bob);

		return bob;
	}

	/**  Gradient information, of which there is none, for now. */
	public void g(double[] x, double[] g)
		{ }

	/**
	 *  Penalty function on the bounds of the rotation or 
	 *  translation.  Basically this penalizes the value
	 *  of x if it is less than the lower bound or greater
	 *  than the upper bound.  It is a multiplicative factor.
	 */
	private double penalty(double x,double lb,double ub)
	{
		double scale = 4*0.693/((ub-lb)*(ub-lb));
		double sscale = (ub-lb)/(2*0.833);
		if (x > ub + sscale*4) return 1.0e7;
		else if ( x < lb - sscale*4) return 1.0e7;

		else if (x > ub) return Math.exp(scale*(x-ub)*(x-ub));
		else if (x < lb) return Math.exp(scale*(lb-x)*(lb-x));
		else return 1.0;
	}

}
