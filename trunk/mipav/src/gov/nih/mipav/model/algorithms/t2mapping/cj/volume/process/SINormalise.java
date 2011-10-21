package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process.*;

public class SINormalise {

	private double s1;
	private double s2;

	private double pc1;
	private double pc2;

	private double[] means = null;
	private int num_means = 0;

	private int verbose = 0;

	public final static int METHOD_UDUPA = 1;
	public final static int METHOD_GAMMA = 2;
	public final static int METHOD_SIMPLE = 3;
	final static int MAX_METHOD = 4;

	private int method = METHOD_GAMMA;

	/** 
	 *  Udupa correction default constructor.
	 */
	public SINormalise()
	{
		s1 = 0;
		s2 = 4095;
		pc1 = 0.0;
		pc2 = 99.8;

		means = new double[1000];
	}

	/** 
	 *  Set the verbose value.
	 */
	public void setMethod( final int method )
	{
		if( method >= 1 && method < MAX_METHOD )
		{
			this.method = method;
		}
		else
		{
			System.out.println("SINormalise::setMethod: Unknown method requested");
		}
	}

	/** 
	 *  Set the verbose value.
	 */
	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	/** 
	 *  Set the s1 value.
	 */
	public void setS1( double s1 )
	{
		this.s1 = s1;
	}

	/** 
	 *  Set the s2 value.
	 */
	public void setS2( double s2 )
	{
		this.s2 = s2;
	}

	/** 
	 *  Set the pc1 value.
	 */
	public void setPC1( double pc1 )
	{
		this.pc1 = pc1;
	}

	/** 
	 *  Set the pc2 value.
	 */
	public void setPC2( double pc2 )
	{
		this.pc2 = pc2;
	}

	/** 
	 *  Train based on this volume.
	 */
	public void train(Volume vol)
	{
        if (method == METHOD_SIMPLE)
        {
			if (verbose > 0)
				System.out.println("train: no training for simple");
        	return;
        }

		NormalFit nf = new NormalFit();

		//  Compute the two percentiles.
		double p1 = Statistics.prctile(vol, pc1);
		double p2 = Statistics.prctile(vol, pc2);

		if( verbose > 0 )
		{
			System.out.println("train: Percentile is " + p1);
			System.out.println("train: Percentile is " + p2);
		}

		double[] params = nf.fitHistogram( vol );
		
		means[num_means] = s1 + (params[1]-p1) / (p2-p1) * (s2-s1);
		num_means++;

		if( verbose > 0 )
		{
			System.out.println("train: Mean is " + params[1]);
		}
	}

	/**
	 *  Obtain the standardized mean.
	 */
	public double getStandardizedMean()
	{
		if( num_means == 0 )
		{
			System.out.println("Udupa::getStandardizedMean: No means defined!");
			System.exit(-1);
		}

		//  Compute the standardized mean.
		double standardized_mean = 0.0;
		for(int ii=0; ii<num_means; ii++) 
		{
			standardized_mean += means[ii];
		}
		standardized_mean /= (double)num_means;

		return standardized_mean;
	}

	/**
	 *  Set the standardized mean.
	 */
	public void setStandardizedMean(final double smean)
	{
		means = new double[1];
		means[0] = smean;
		num_means = 1;
	}

	/** 
	 *  Correct the volume based on the standardized mean and
	 *  the s1/s1/pc1/pc2 parameters.
	 */
	public Volume correct(final Volume vol)
	{
		Volume out = null;

		switch( method )
		{
			case METHOD_UDUPA:
				out = correct_udupa(vol);
			break;

			case METHOD_GAMMA:
				out = correct_gamma(vol);
			break;

			case METHOD_SIMPLE:
				out = correct_simple(vol);
			break;

			default:
				System.out.println("Udupa::correct: Unknown correction algorithm");
				System.exit(-1);
			break;
		}

		return out;
	}

	/** 
	 *  Correct the volume based on the standardized mean and
	 *  the s1/s1/pc1/pc2 parameters.
	 */
	private Volume correct_udupa(final Volume vol)
	{
		Volume newvol = new Volume(vol.getNRows(),vol.getNCols(),vol.getNSlices());

		// Get the standardized mean
		double standardized_mean = getStandardizedMean();

		// Compute the mode of the histogram.
		double data_mode = 0.0;
		NormalFit nf = new NormalFit();
		double[] params = nf.fitHistogram( vol );
		data_mode = params[1];

		if( verbose > 0 )
		{
			System.out.println("correct: data_mode is " + data_mode);
			System.out.println("correct: standardized_mean is " + standardized_mean);
		}

		//  Compute the two percentiles.
		double p1 = Statistics.prctile(vol, pc1);
		double p2 = Statistics.prctile(vol, pc2);

		if( verbose > 0 )
		{
			System.out.println("correct: Percentile is " + p1);
			System.out.println("correct: Percentile is " + p2);
		}

		double datum;
		double newdatum;
		for(int slice=0; slice<vol.getNSlices(); slice++) 
		{
			for(int col=0; col<vol.getNCols(); col++) 
			{
				for(int row=0; row<vol.getNRows(); row++) 
				{
					datum = vol.getData(row,col,slice);	

					if( datum > data_mode ) 
					{
						newdatum = Math.ceil( 
							standardized_mean + (datum - data_mode) * 
							(s2 - standardized_mean) / (p2 - data_mode)	
							);
					}
					else
					{
						newdatum = Math.ceil( 
							standardized_mean + (datum - data_mode) * 
							(s1 - standardized_mean) / (p1 - data_mode)	
							);
					}

					// Now make sure the data is between s1 and s2.
					if( newdatum < s1 ) { newdatum = s1; }
					if( newdatum > s2 ) { newdatum = s2; }

					newvol.setData((float)newdatum, row, col, slice);
				}
			}
		}

		return newvol;
	}

	/** 
	 *  Correct the volume based on the standardized mean and
	 *  the s1/s1/pc1/pc2 parameters.
	 */
	private Volume correct_gamma(final Volume vol)
	{
		Volume newvol = new Volume(vol.getNRows(),vol.getNCols(),vol.getNSlices());
		// Get the standardized mean
		double standardized_mean = getStandardizedMean();

		// Compute the mode of the histogram.
		double data_mode = 0.0;
		NormalFit nf = new NormalFit();
		double[] params = nf.fitHistogram( vol );
		data_mode = params[1];

		if( verbose > 0 )
		{
			System.out.println("correct: data_mode is " + data_mode);
			System.out.println("correct: standardized_mean is " + standardized_mean);
		}

		//  Compute the two percentiles.
		double p1 = Statistics.prctile(vol, pc1);
		double p2 = Statistics.prctile(vol, pc2);

		if( verbose > 0 )
		{
			System.out.println("correct: Percentile is " + p1);
			System.out.println("correct: Percentile is " + p2);
		}

		// Calculate the gamma factor.
		double data_p = (data_mode - p1) / (p2-p1);
		double std_p = (standardized_mean - s1) / (s2-s1);
		double gamma = Math.log(std_p)/Math.log(data_p);

		if( verbose > 0 )
		{
			System.out.println("correct: DataP is " + data_p);
			System.out.println("correct: std_p is " + std_p);
			System.out.println("correct: gamma is " + gamma);
		}


		double datum;
		double newdatum;
		for(int slice=0; slice<vol.getNSlices(); slice++) 
		{
			for(int col=0; col<vol.getNCols(); col++) 
			{
				for(int row=0; row<vol.getNRows(); row++) 
				{
					datum = vol.getData(row,col,slice);	

					newdatum = s1 + (s2-s1)* Math.pow((datum-p1)/(p2-p1), gamma);

					// Now make sure the data is between s1 and s2.
					if( newdatum < s1 ) 
					{ 
						newdatum = s1; 
					}

					if( newdatum > s2 ) 
					{ 
						newdatum = s2; // + gamma*(datum-p2)*(s2-s1)/(p2-p1); 
					}

					newvol.setData((float)newdatum, row, col, slice);
				}
			}
		}

		return newvol;
	}

	private Volume correct_simple(final Volume vol)
	{
		Volume newvol = new Volume(vol.getNRows(),vol.getNCols(),vol.getNSlices());
		//  Compute the two percentiles.
		double p1 = Statistics.prctile(vol, pc1);
		double p2 = Statistics.prctile(vol, pc2);

		if( verbose > 0 )
		{
			System.out.println("correct: Lower percentile is " + p1);
			System.out.println("correct: Upper percentile is " + p2);
		}

		double datum;

		for(int slice=0; slice<vol.getNSlices(); slice++) 
		{
			for(int col=0; col<vol.getNCols(); col++) 
			{
				for(int row=0; row<vol.getNRows(); row++) 
				{
					datum = vol.getData(row,col,slice);	

					datum = s1 + (s2-s1)*(datum-p1)/(p2-p1);

					// Now make sure the data is between s1 and s2.
					if( datum < s1 ) { datum = s1; }
					if( datum > s2 ) { datum = s2; }

					newvol.setData((float)datum, row, col, slice);
				}
			}
		}

		return newvol;
	}
}
