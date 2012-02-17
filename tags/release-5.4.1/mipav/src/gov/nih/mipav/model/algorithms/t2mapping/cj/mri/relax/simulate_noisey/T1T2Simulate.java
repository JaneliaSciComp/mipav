package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.simulate_noisey;

import gov.nih.mipav.model.algorithms.t2mapping.cj.mri.hennig.*;
import java.util.*;

public class T1T2Simulate
{
	private Random random = null;

	private Decay hennig_decay = null;

	private Vector rho = null;
	private Vector t1 = null;
	private Vector t2 = null;

	/** Types of noise we could add. */
	final private int NOISE_GAUSSIAN = 0;
	final private int NOISE_QUADRATURE = 1;
	private int noise_type = NOISE_QUADRATURE;

	/**  Number of echoes we are simulating. */
	private int num_echoes = 32;
	
	/**  Flip angle of the refocusing pulses. */
	private double alpha = 180.0;

	private double[] true_decay = null;

	/**  Instance of noise */
	private double[] noise = null;

	/** SNR */
	private double snr = 200.0;

	/** TE */
	private double te = 10.0;

	/**
	 *  At this point, just create the new vectors.
	 */
	public T1T2Simulate()
	{
		rho = new Vector();
		t1 = new Vector();
		t2 = new Vector();

		noise = new double[ num_echoes ];
		true_decay = new double[ num_echoes ];
		random = new Random();
		hennig_decay = new Decay(num_echoes);
	}

	/**
	 *  Add the water compartment to the list of multi-decay components
	 *  that make up the voxel we are simulating.
	 */
	public void addComponent(final double rho, final double t1, final double t2)
	{
		this.rho.add(new Double(rho));
		this.t1.add(new Double(t1));
		this.t2.add(new Double(t2));
	}

	/**
	 *  Set the seed of the random number generator.
	 */
	public void setRandomSeed(final int seed)
	{
		random.setSeed(seed);
	}

	/**
	 *  Unset the seed of the random number generator.
	 */
	public void unSetRandomSeed()
	{
		random = new Random();
	}

	/**
	 *  Set the type of noise to add.
	 */
	public void setNoiseType(final int noise_type)
	{
		this.noise_type = noise_type;
	}

	/**
	 *  Set the type of noise to add.
	 */
	public void setNoiseType(final String noise_type_string )
	{
		if( noise_type_string.toLowerCase().compareTo("quadrature") == 0 || 
		    noise_type_string.toLowerCase().compareTo("rice")  == 0)
		{
			noise_type = NOISE_QUADRATURE;
		}
		else if( noise_type_string.toLowerCase().compareTo("gaussian") == 0 )
		{
			noise_type = NOISE_QUADRATURE;
		}
	}

	/**
	 *  Set the number of echoes in the T2 decay.
	 */
	public void setNumEchoes( final int num_echoes )
	{
		this.num_echoes = num_echoes;

		noise = new double[ num_echoes ];
		true_decay = new double[ num_echoes ];
		hennig_decay.setNechoes( num_echoes );
	}

	/**
	 *  Set the SNR based on the total PD.
	 */
	public void setSNR( final double snr )
	{
		this.snr = snr;
	}

	/**
	 *  Set the flip angle of the refocusing pulses.
	 */
	public void setAlpha( final double alpha )
	{
		this.alpha = alpha;
	}

	public void setTE( final double te )
	{
		this.te = te;
	}

	public double[] getTEList()
	{
		double[] teout = new double[ num_echoes ];
		for(int ii=0; ii<num_echoes; ii++)
		{
			teout[ii] = (double)(ii+1)*te;
		}

		return teout;
	}


	public final double[] getStdDev( )
	{
		return (double[])noise.clone();
	}

	public void getDecay(double[] decay)
	{
		double total_pd = 0.0;

		// Make sure that decay has the correct number of echoes.
		if( decay == null || decay.length != num_echoes )
		{
			decay = new double[ num_echoes ];
		}

		// Create the true decay.
		hennig_decay.setRefocusFlip( alpha );

		Arrays.fill(decay, 0.0);
		for(int ii=0; ii<rho.size(); ii++)
		{
			hennig_decay.setPD( ((Double)rho.get(ii)).doubleValue() );
			hennig_decay.setT2( ((Double)t2.get(ii)).doubleValue() );
			hennig_decay.setT1( ((Double)t1.get(ii)).doubleValue() );
			hennig_decay.setTR( Double.POSITIVE_INFINITY );
			hennig_decay.setTE( te );

			double[] temp = hennig_decay.calculateEchoes();

			for(int jj=0; jj<temp.length; jj++)
			{
				decay[jj] += temp[jj];
				true_decay[jj] += temp[jj];
			}

			total_pd += ((Double)rho.get(ii)).doubleValue();
		}

		// Create the instance of noise.
		for(int ii=0; ii<num_echoes; ii++)
		{
			switch( noise_type )
			{
				case NOISE_GAUSSIAN:
					decay[ii] += (total_pd/snr)*random.nextGaussian();
					noise[ii] = (total_pd/snr);
				break;
				case NOISE_QUADRATURE:
					double n1 = (total_pd/snr)*random.nextGaussian();
					double n2 = (total_pd/snr)*random.nextGaussian();

					decay[ii] = Math.sqrt( 
						Math.pow(n1+decay[ii], 2.0) + Math.pow(n2, 2.0));
					noise[ii] = (total_pd/snr);
				break;
			}	
		}
	}
}
