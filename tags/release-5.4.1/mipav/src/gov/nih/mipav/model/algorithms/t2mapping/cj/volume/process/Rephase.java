package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *  The resphase class takes in an MCVolume that can have any number
 *  of slices and channels and will rephase the complex data to be
 *  all in the real axis.   The channel data is ASSUMED to be such that 
 *  each slice of real and imaginary data is beside each other.
 *
 */
public class Rephase {

	/** How verbose do you want me to be? */
	private int verbose = 0;

	/** Width of the Gaussian kernel used if Gaussian smoothing is chosen. */
	private double gaussian_sigma = 3.0;

	/** Width of the blurring window if Uniform smoothing is chosen. */
	private int uniform_blur_window = 17;

	/** Number of blurring iterations no matter what smoothing is chosen. */
	private int blur_iterations = 3;

	/* 
	 *  Setup the type of blurring.
	 */
	static public int BLUR_UNIFORM = 0;
	static public int BLUR_GAUSSIAN = 1;

	/** Type of blurring to use. */
	private int blur_method = BLUR_UNIFORM;

	public Rephase()
	{
	}

	/**
	 *  Set the blurring method.
	 * 
	 *  <b>It does a stupid check to see if the blur_method
	 *  passed in is valid.  Be Careful!</b>
	 */
	public void setBlurMethod( final int blur_method )
	{
		this.blur_method = blur_method;

		if( blur_method > BLUR_GAUSSIAN ) 
		{
			System.out.println("rephase: Unknown blur method.");
			System.exit(-1);
		}
	}

	/** 
	 *  Set the number of iterations of the blurring.
	 *
	 */
	public void setBlurIterations( final int blur_iterations )
	{
		this.blur_iterations = blur_iterations;
	}

	/** 
	 *  Set the window size of the blurring.
	 *
	 */
	public void setGaussianSigma( final int gaussian_sigma )
	{
		this.gaussian_sigma = gaussian_sigma;
	}

	/** 
	 *  Set the window size of the blurring.
	 *
	 */
	public void setUniformWindow( final int uniform_blur_window )
	{
		this.uniform_blur_window = uniform_blur_window;
	}

	/** 
	 *  Set the verbose'ness.
	 */
	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	/**
	 *  Rephase based on the MCVolume creates the ImageFilter
	 *  and then calls the real MCVolume.
	 *
	 *  @see public void rephase(MCVolume vol, ImageFilter2D ub)
	 *
	 */
	public void rephase(MCVolume vol)
	{
		ImageFilter2D ub = null;

		//  Smooth a copy of the data.
		if( blur_method == BLUR_UNIFORM )
		{
			ub = new UniformBlur2D(uniform_blur_window);
		}
		else
		{
			ub = new GaussianBlur2D(gaussian_sigma);
		}
		
		rephase(vol, ub );
	}

	/**  
	 *  Rephase the real and imaginary parts of the 
	 *  volume passed in.  It will be assumed that the
	 *  real and imaginary parts are paired as channels.
	 *
	 */
	public void rephase(MCVolume vol, ImageFilter2D ub)
	{
		Volume r = null;
		Volume i = null;

		for(int ch=0; ch<vol.getNChannels(); ch=ch+2)
		{
			r = vol.getVolume(ch);
			i = vol.getVolume(ch+1);

			rephase( r, i, ub );

			vol.setVolume(r, ch);
			vol.setVolume(i, ch+1);
		}
	}

	/**
	 *  Create the ImageFilter2D that is to be passed
	 *  in to the real rephase code.
	 *
	 *  @see public void rephase(Volume real, Volume imag, ImageFilter2D ub)
	 */
	public void rephase(Volume real, Volume imag)
	{
		ImageFilter2D ub = null;

		// Choose the blurring method.
		if( blur_method == BLUR_UNIFORM )
		{
			ub = new UniformBlur2D(uniform_blur_window);
		}
		else
		{
			ub = new GaussianBlur2D(gaussian_sigma);
		}
		
		rephase(real, imag, ub );
	}

	/**
	 *  Rephase the volumes that are passed in, but all this really
	 *  does is loop over the slices in each volume and calls 
	 *  the rephase method on the slices.
	 */
	public void rephase(Volume real, Volume imag, ImageFilter2D ub)
	{
		Slice r = null;
		Slice i = null;

		if( real.getNSlices() != imag.getNSlices() )
		{
			System.out.println("rephase: Volumes have different number of slices.");
			System.exit(-1);
		}

		for(int sl=0; sl<real.getNSlices(); sl++)
		{
			r = real.getSlice(sl);
			i = imag.getSlice(sl);
			rephase(r, i, ub);
			real.setSlice(r, sl);
			imag.setSlice(i, sl);
		}
	}

	/**
	 *  Rephase the pair of slices, but all this does is
	 *  create the ImageFilter that is needed for the real routine.
	 */
	public void rephase( Slice real, Slice imag )
	{
		ImageFilter2D ub = null;

		if( blur_method == BLUR_UNIFORM )
		{
			ub = new UniformBlur2D(uniform_blur_window);
		}
		else
		{
			ub = new GaussianBlur2D(gaussian_sigma);
		}
		
		rephase(real, imag, ub );
	}

	/**
	 *  The real rephase routine that rephases the real and imaginary
	 *  slice passed in based on the ImageFilter2D passed in.
	 */
	public void rephase( Slice real, Slice imag, ImageFilter2D ub )
	{
		final int nrows = real.getNRows();
		final int ncols = real.getNCols();

		// Copy over the data.
		Slice smooth_real = new Slice(real);
		Slice smooth_imag = new Slice(imag);
		
		//  Smooth the real and imaginary images.
		for(int ii=0; ii<blur_iterations; ii++) {
			ub.filter(smooth_real);
			ub.filter(smooth_imag);
		}
		
		// Create the phasor.
		Slice phasor_real = new Slice(nrows, ncols);
		Slice phasor_imag = new Slice(nrows, ncols);

		// Compute the phasor.s
		double phasor_length = 0.0;
		for(int ii=0; ii<nrows; ii++)
		{
			for(int jj=0; jj<ncols; jj++)
			{
				phasor_length = Math.sqrt( 
					(smooth_real.getData(ii,jj)*smooth_real.getData(ii,jj)) + 
					(smooth_imag.getData(ii,jj)*smooth_imag.getData(ii,jj)));

				phasor_real.setData(smooth_real.getData(ii,jj)/phasor_length, 
					ii, jj);
				phasor_imag.setData(-smooth_imag.getData(ii,jj)/phasor_length,
					ii, jj);
			}
		}

		// Apply the rephasor.
		double curr_real = 0.0;
		double curr_imag = 0.0;
		for(int ii=0; ii<nrows; ii++)
		{
			for(int jj=0; jj<ncols; jj++)
			{
				curr_real = real.getData(ii,jj);
				curr_imag = imag.getData(ii,jj);

				real.setData( curr_real*phasor_real.getData(ii,jj) - 
					curr_imag*phasor_imag.getData(ii,jj),
					ii, jj);
				imag.setData( curr_real*phasor_imag.getData(ii,jj) + 
					curr_imag*phasor_real.getData(ii,jj),
					ii, jj);
			}
		}
	}
}
