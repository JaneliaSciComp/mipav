package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit.*;

/** Normal (Gaussian) fit to a histogram of the data in a volume.
 *
 *
 *  @author  Craig Jones
 *  @version 0.9
 */
public class NormalFit {

	private long start = 0;
	private long end = 0;

	/** Construct a new NormalFit */
	public NormalFit()
	{
	}

	/** Fit a Gaussian to a histogram of the volume.  The
	 *  threshold will be calculated automatically.
	 *
	 */
	public double[] fitHistogram(final Volume volume) 
	{
		NoiseEstimate ne = new NoiseEstimate();
		float noise = ne.histogramMethod( volume );

		return fitHistogram( volume, noise );
	}

	/** Fit a Gaussian to a histogram of the volume based on data 
	 *  greater than the specified threshold.
	 *
	 */
	public double[] fitHistogram(final Volume volume, final float threshold ) 
	{
		Histogram hh = new Histogram( );
		hh.histogram( volume , threshold, Float.MAX_VALUE );

		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		double[] dbins = new double[ bins.length ];
		double[] dcounts = new double[ counts.length ];
		for(int ii=0; ii<dbins.length; ii++) {
			dbins[ii] = (double)bins[ii];
			dcounts[ii] = (double)counts[ii];
		}
		 
		CurveFit fit = new GaussianFit();
		start = System.currentTimeMillis();
		fit.doFit(dbins, dcounts);
		end = System.currentTimeMillis();

		return fit.getParams();
	}

	/** Fit a Gaussian to a histogram of the volume.  The
	 *  threshold will be calculated automatically.
	 *
	 */
	public double[] fitHistogram(final MCVolume volume) 
	{
		NoiseEstimate ne = new NoiseEstimate();
		float noise = ne.histogramMethod( volume );

		return fitHistogram( volume, noise );
	}

	/** Fit a Gaussian to a histogram of the volume based on data 
	 *  greater than the specified threshold.
	 *
	 */
	public double[] fitHistogram(MCVolume volume, float threshold ) 
	{
		Histogram hh = new Histogram( );
		hh.histogram( volume , threshold, Float.MAX_VALUE );

		float[] bins = hh.getBins();
		float[] counts = hh.getCounts();

		double[] dbins = new double[ bins.length ];
		double[] dcounts = new double[ counts.length ];
		for(int ii=0; ii<dbins.length; ii++) {
			dbins[ii] = (double)bins[ii];
			dcounts[ii] = (double)counts[ii];
		}
		 
		CurveFit fit = new GaussianFit();
		start = System.currentTimeMillis();
		fit.doFit(dbins, dcounts);
		end = System.currentTimeMillis();

		return fit.getParams();
	}

	/** Return the number of seconds that this took.
	 *
	 */
	public float getTiming() { 
		return (float)((double)(end-start)/1000.0);
	}
}
