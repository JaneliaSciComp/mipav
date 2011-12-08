package gov.nih.mipav.model.algorithms.t2mapping.t2map;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.util.*;

import javax.swing.*;
import java.awt.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.*;

/**
 *  The class is the base class for all other T2Map classes
 *  and takes care of most things including storing the spectral
 *  volume, chi2 map, variable flip angle map (if needed) and estimating
 *  the noise in the image.
 *
 */
abstract public class T2Map
{
	protected int verbose = 0;

	Vector volumes = new Vector(5);
	//Fitting type subclasses
	public enum FittingType {
		BiExp(T2MapBiExp.class, "Bi-exponential"),
		BiExpFixedT2(T2MapBiExpFixedT2.class, "Bi-exp w/ fixed T2"),
		BiExpSweep(T2MapBiExpSweep.class, "Bi-exp w/ sweep"),
		L1(T2MapL1.class, "L1"),
		MonoExp(T2MapMonoExp.class, "Mono-exponential"),
		NNLS(T2MapNNLS.class, "NNLS"), 
		MultiExp(null, "Multi-exponential");
		
		private String name;
		
		FittingType(Class c, String name) {
			this.name = name;
			
;		}
		
		/* (non-Javadoc)
		 * @see java.lang.Enum#toString()
		 */
		@Override
		public String toString() {
			return name;
		}
	}
	
	
	// Noise Estimators.

	public enum NoiseEstimation {
		/** No noise estimation. */
		NE_NONE("None"),
		/** Estimate the noise across the rows */
		NE_ACROSS_ROW("Across Row"),
		/** Estimate the noise across the columns. */
		NE_ACROSS_COLUMN("Across Column"),
		/** Estimate the noise in the image (gradientMethod). */
		NE_ACROSS_IMAGE("Across Slice");
		
		private String name;

		NoiseEstimation(String name) {
			this.name = name;
		}

		/* (non-Javadoc)
		 * @see java.lang.Enum#toString()
		 */
		@Override
		public String toString() {
			return name;
		}
	}

    private NoiseEstimation noise_estimate = NoiseEstimation.NE_ACROSS_ROW;

	// Used to speed up the standard deviation calculations
	private int ne_prev_row = -1;
	private int ne_prev_col = -1;
	private boolean ne_image_calculated = false;

	/** The T2 fitting object. */
	protected T2Fit t2fitter = null;

    /** Amplitude solutions to NNLS */
    protected SpectralVolume svol = null;
 
    /** Chi2 solution to NNLS */
    protected MCVolume chi2 = null;

    /** Best estimated flip angle if estimateFlip is true. */
    protected MCVolume alpha_map = null;

	// NOTE:  estimateFlip AND useFlip CAN'T BOTH BE TRUE!

    /** Determine if we are going to estimate the flip angle. */
    protected boolean estimateFlip = false;
	
    /** Determine if we are going to estimate the flip angle. */
    protected boolean useFlip = false;
	
	/** The threshold on the signal intensity. */
	protected double threshold = 20;

	/** Myelin slice, if the user wants to display intermediate results. */
	protected MCVolume myelin_slice = null;
	protected boolean show_myelin_slice = false;
	protected JFrame frame = null;
	protected MCVolumePanel dm = null;

	/** Progress bar, if the user wants to display intermediate results. */
	protected boolean show_progress_bar = false;
	protected JFrame progressFrame = null;
	protected JProgressBar progressBar = null;

	private int xDim;
	private int yDim;
	private int numSlices;
	private double[] TEValues;
	
	
	
	public T2Map(int xDimIn, int yDimIn, int numSlicesIn, double[] TEValuesIn)
	{
		setXDim(xDimIn);
		setYDim(yDimIn);
		setNumSlices(numSlicesIn);
		setTEValues(TEValuesIn);
		
		t2fitter = new T2FitMonoExp(TEValues);
	}
	
	/**
	 * Default constructor.
	 */
	public T2Map(){}

	/*
	 *  Abstract methods....
	 *
	 */

	abstract public void setTE( double[] te );

	/** Return the number of spectral channels to be defined for the 
	 *  spectral volume. 
	 */
	abstract protected int getNSpectralChannels();

	/** Deal with the distribution and chi2. */
	abstract protected void dealWithData(final T2Distribution dist, 
		final double chi2, int row, int col, int slice);

	/** Deal with the flip angle that was estimated. */
	abstract protected void dealWithAlpha(final double flip, 
		int row, int col, int slice);

	/** Save the calculated data to a file. */
	abstract public void saveData( String base, Vector header, final String comment );

	/** Set the parameters, if there are any... */
	abstract public void setParameters( double[] params );

	/** Allow the person to show the myelin map as we calculate it. */
	public void setInteractiveMyelin( final boolean yup )
	{
		show_myelin_slice = yup;
	}

	/** Set the threshold... */
	public void setThreshold( final double threshold )
	{
		this.threshold = threshold;
	}

	/** Set verbose */
	public void setVerbose( final int verbose )
	{
		this.verbose = verbose;	
		t2fitter.setVerbose( verbose );
	}

	/** Should we do flip angle estimation? */
	public void setEstimateFlip( final boolean estimateFlip )
	{
		if( useFlip )
		{
			System.out.println("t2map: Can't use a flip angle map *and* estimate the flip angle at the same time.");
			System.exit(-1);
		}


		this.estimateFlip = estimateFlip;
		t2fitter.setEstimateFlip( estimateFlip );
	}

	/** Should we do flip angle estimation? */
	public void setUseFlip( final MCVolume alpha_map )
	{
		if( estimateFlip ) {
			System.out.println("t2map: Can't estimate flip angle *and* use a flip angle map at the same time.");
	  	}

		this.alpha_map = (MCVolume)alpha_map.clone();

		useFlip = true;
	}

	/**
	 *  Apply the t2fit algorithm over the whole volume.
	 */
	
	
    public ModelImage map(final ModelImage img)
    {
        return map(img, 0, xDim-1, 0, yDim-1, 0, numSlices-1, TEValues.length);
    }

	/**
	 *  Apply the t2fit algorithm over the specified sub volume.
	 */

    public ModelImage map(final ModelImage img, 
						final int start_row, final int end_row,
						final int start_col, final int end_col,
						final int start_slice, final int end_slice,
						final int nChannels)
	{
		T2Distribution dist = new T2Distribution();
		T2Bin myelin_bin = new T2Bin(0.0, 50.0, "short");

		double chi2_temp;

		svol = new SpectralVolume(xDim, yDim, numSlices, getNSpectralChannels());
		chi2 = new MCVolumeFile(xDim, yDim, numSlices, 1);

		ModelImage chi2img = new ModelImage(ModelStorageBase.FLOAT, new int[] {xDim, yDim, numSlices, 1}, "chi2");
		ModelImage svolImg = new ModelImage(ModelStorageBase.FLOAT, new int[] {xDim, yDim, numSlices, getNSpectralChannels()}, "svol");

		/*
		 *  If we are going to show the slice, then create the frame and
		 *  MCVolumePanel to display it.
		 */
		if( show_myelin_slice )
		{
			try {
				frame = new JFrame("Myelin Image");
				frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );

				// Set the initial location in the middle of the screen.
				frame.setSize( xDim + 5, yDim + 40);
				Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
				frame.setLocation(screen.width/2-256/2,screen.height/2-256/2);

				int nRows=yDim, nCols=xDim;
				
				frame.setSize(nCols+5,nRows+40);
				frame.setLocation(screen.width/2-nCols/2,screen.height/2-nRows/2);

				frame.setResizable(true);
				frame.setVisible(true);
			}
			catch( InternalError e )
			{ 
				System.out.println("Couldn't open a window, continuing anyway");
			  	show_myelin_slice = false;
			}
		}

		if( estimateFlip )
		{
			alpha_map = new MCVolumeFile(xDim, yDim, numSlices, 1);
		}

		if( show_progress_bar)
		{
			try{
				progressFrame = new JFrame("Progress...");
				progressFrame.setSize(256,50);
				progressFrame.setResizable(false);
				progressBar = new JProgressBar(0, 100); 
				progressBar.setValue(0);
				progressBar.setMaximum(100);
				progressFrame.getContentPane().add( progressBar );
				Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
				progressFrame.setLocation(screen.width/2-128,screen.height/2-128);
				progressFrame.setVisible(true);
			}
			catch( InternalError e )
			{
				System.out.println("Couldn't open a window, continuing anyway");
			  	show_progress_bar = false;
			}
		}

		for(int slice=start_slice; slice < end_slice; slice++) 
		{

			// Are we going to display while we are doing the stuff?
			if( show_myelin_slice )
			{
				myelin_slice = new MCVolume(xDim, yDim, 1, 1);

				dm = new MCVolumePanel(myelin_slice, false, true);
				frame.getContentPane().add(dm);
			}

			for(int row=start_row; row < end_row; row++) 
			{
				if( verbose > 1 ) 
				{
					System.out.println("-------------------------------------");
				}

				System.out.print("\rrow/slice = " + row + "/" + slice);

				if( verbose > 1 ) 
				{
					System.out.println("");
				}

				/*
				 *  Calculate the nnls solution for each row.
				 */
				for(int col=start_col; col < end_col; col++) 
				{
					if( show_progress_bar)
					{
						progressBar.setValue( 100*
							((slice-start_slice)*(end_row-start_row)*(end_col-start_col) + 
							 (row-start_row)*(end_col-start_col) + col-start_col) / 
							 ((end_row-start_row)*(end_col-start_col)*(end_slice-start_slice))
						);
					}
					
					double[] stds = new double[nChannels];
					
//TODO				//  Compute the standard deviation for each channel.
					calculateStd(img, row, col, slice, stds);

					// Set the standard deviations.

					t2fitter.setStdDev( stds );

					// Make sure we are above the threshold.
					if( img.getInt(row,col,slice,0) > threshold*stds[0] ) 
					{

						// If we are using a flip angle map, then set
						// the flip angle.
						if( useFlip )
						{

							double alphatt = alpha_map.getData(row,col,slice,0);
							if( alphatt < 2.0 )
							{
								t2fitter.setAlpha( 180.0 );
							}
							else
							{
								t2fitter.setAlpha( alphatt );
							}
						}

						double[] channelAr = getChannelDouble(img,row,col,slice);
						boolean noneZero = true;
						for(int i=0; i<channelAr.length; i++) {
							if(channelAr[i] == 0) {
								noneZero = false;
							}
						}
						if(noneZero) {
							System.out.println("Found");
						}
						
						chi2_temp = t2fitter.solve(channelAr, dist);	
//						chi2_temp = t2fitter.solveBootstrap(vol.getChannelDouble(row,col,slice), dist);	

						// Are we going to display while we are doing the stuff?
						if( show_myelin_slice )
						{
							myelin_bin.calculate(dist);

							if( myelin_bin.getFR() < 0.8 )
							myelin_slice.setData(myelin_bin.getFR(),row,col,0,0);
						}

						// Put results in proper place.
						dealWithData(dist, chi2_temp, row,col,slice);

						// If we must do something with the estimated 
						// flip angle, then do it.
						if( estimateFlip )
						{
							dealWithAlpha(t2fitter.getBestAlpha(),row,col,slice);
						}

						// Show what the result is if we really, really 
						// want to.
						if( verbose > 1 )
						{
							double[] x = dist.getAmplitudes();
							double[] t2s = dist.getT2s();
							for(int ii=0; ii<x.length; ii++) 
							{
								if( x[ii] > 0.0 )
								{
									System.out.println(x[ii] + "  " + t2s[ii]);
								}
							}
							System.out.println("chi2 = " + chi2_temp);
						}
					}
				}

				// Are we going to display while we are doing the stuff?
				if( show_myelin_slice )
				{
				    // This is the old way and might be a little too slow.
					frame.getContentPane().remove(dm);
					dm = new MCVolumePanel(myelin_slice, false, true);
					frame.getContentPane().add(dm);
					dm.setRow(myelin_slice.getRow(row,0,0),row,0,0);
				}

			}
		}
		
		for (int channel = 0; channel < svol.getNT2(); channel++) {
			for (int slice = 0; slice < svol.getNSlices(); slice++) {
				for (int row = 0; row < svol.getNRows(); row++) {
					for (int col = 0; col < svol.getNCols(); col++) {
						svolImg.set(row, col, slice, channel, svol.getMCVolume().getData(row, col, slice, channel));
					}
				}
			}
		}
		
		ViewJFrameImage svol2frame = new ViewJFrameImage(svolImg);
		
		return svolImg;
	}

//TODO    
	/** 
	 *  Compute the standard deviation in the specified way.
	 */
	private boolean calculateStd(final ModelImage img, 
		final int row, final int col, final int slice, double[] stds)
	{
		boolean updated = false;

		switch( noise_estimate )
		{
			case NE_NONE:
				Arrays.fill(stds, 1.0);
				break;
			case NE_ACROSS_ROW:
				if( ne_prev_row != row )
				{
					stdAcrossRow(img, row, slice, stds);
					ne_prev_row = row;
					updated = true;
				}
				break;
			case NE_ACROSS_COLUMN:
				if( ne_prev_col != col )
				{
					stdAcrossColumn(img, col, slice, stds);
					ne_prev_col = col;
					updated = true;
				}
				break;
			case NE_ACROSS_IMAGE:
					if( !ne_image_calculated )
					{
						stdAcrossImage(img, slice, stds);
						ne_image_calculated = true;
					}
				break;
			default:
				MipavUtil.displayError("T2Map: Unknown noise estimate type.");
				
				break;
		}

		return updated;
	}

	public Slice getSlice(int slice, int channel, ModelImage img)
	{
		Slice toreturn = null;

		if( !(slice<0|| slice>numSlices )|| !( channel >= 0 && channel < TEValues.length ) ) {
			throw new ArrayIndexOutOfBoundsException("ModelImage::getSlice, out of bounds");
		}

		try 
		{
			volumes.setSize(5);
		}
		catch( OutOfMemoryError e )
		{
			MipavUtil.displayError("ModelImage: Could not allocate " + xDim +
				" " + yDim + " " + numSlices + " " + TEValues.length );
			
		}
		
		if( volumes.elementAt(channel) == null )
			allocVolume(channel);

		toreturn = ((Volume)volumes.elementAt(channel)).getSlice(slice);

		return toreturn;
	}
	
	private void allocVolume(int channel) {
		volumes.set(channel, new Volume(xDim, yDim , numSlices) );
		
	}

	/** stdRowSlice - Compute the standard deviation across a row
	 *                given the row and slice.  Do this for each
	 *                channel.
	 */
	protected void stdAcrossRow(final ModelImage img, final int row, 
								 final int slice, double[] stds)
	{
		int n = 0;
		double x = 0.0;
		double x2 = 0.0;
		double datum = 0;

		for(int ch=0; ch < stds.length; ch++) 
		{
			n = 0;
			x = x2 = 0.0;
			for(int col=0; col < xDim; col++) 
			{
				datum = img.getInt(row,col,slice,ch);

				if( datum < 50.0 )
				{
					x += datum;
					x2 += (datum*datum);
					n++;
				}
			}

			if( x == 0 || n == 0 )
			{
				stds[ch] = 1.0;
			}
			else
			{
				stds[ch] = Math.sqrt((double)(x2 - (x*x)/(double)n)/(double)(n-1));
			}
		}
	}

	/** stdColSlice - Compute the standard deviation across a column
	 *                given the column and slice.  Do this for each
	 *                channel.
	 */
	protected void stdAcrossColumn(final ModelImage img, final int col, 
								 final int slice, double[] stds)
	{
		int n = 0;
		double x = 0.0;
		double x2 = 0.0;
		double datum = 0.0;

		for(int ch=0; ch < stds.length; ch++) 
		{
			n = 0;
			x = x2 = 0.0;
			for(int row=0; row < yDim; row++) 
			{
				datum = img.getInt(row,col,slice,ch);

				if( datum < 50.0 )
				{
					x += datum;
					x2 += (datum*datum);
					n++;
				}
			}

			if( x == 0 || n == 0 )
			{
				stds[ch] = 1.0;
			}
			else
			{
				stds[ch] = Math.sqrt((double)(x2 - (x*x)/(double)n)/(double)(n-1));
			}
		}
	}

	/** stdAcrossImage - Compute the standard deviation across the 
	 *                whole slice (do it for each channel).
	 */
	protected void stdAcrossImage(final ModelImage img, final int slice, 
								  double[] stds)
	{
		NoiseEstimate ne = new NoiseEstimate();

		for(int ch=0; ch < stds.length; ch++) 
		{
			stds[ch] = (double)ne.gradientMethod( getSlice(slice, ch, img) );
		}
	}

	public double[] getChannelDouble(ModelImage img, int row, int col, int slice)
	{ 
		if( (row<0 || row>=yDim) || (col<0 || col>=xDim) || (slice<0 || slice>=numSlices) ) 
		{
			throw new ArrayIndexOutOfBoundsException("MCVolume::getChannel");
		}

		double[] chdata = new double[ TEValues.length ];

		for(int ii=0; ii<TEValues.length; ii++) 
		{
			chdata[ii] = (double)img.getInt(row,col,slice,ii);
		}
			
		return chdata;
	}
	
	
	/** 
	 *  Set the noise estimation method.
	 */
	public void setNoiseEstimateMethod( final String ne )
	{
		if( ne.indexOf("row") != -1 )
		{
			setNoiseEstimateMethod( NoiseEstimation.NE_ACROSS_ROW.toString() );
		}
		else if( ne.indexOf("col") != -1 )
		{
			setNoiseEstimateMethod( NoiseEstimation.NE_ACROSS_COLUMN.toString() );
		}
		else if( ne.indexOf("image") != -1 )
		{
			setNoiseEstimateMethod( NoiseEstimation.NE_ACROSS_IMAGE.toString() );
		}
		else if( ne.indexOf("none") != -1 )
		{
			setNoiseEstimateMethod( NoiseEstimation.NE_NONE.toString() );
		}
		else   
		{
			MipavUtil.displayError("t2map: Unknown noise estimate type " + ne);
		}
	}

	/** 
	 *  Set the noise estimation method.
	 */
	public void setNoiseEstimateMethod( final NoiseEstimation noise_estimate )
	{
		this.noise_estimate = noise_estimate;
	}


	public void setXDim(int xDim) {
		this.xDim = xDim;
	}

	public int getXDim() {
		return xDim;	
	}

	public void setYDim(int yDim) {
		this.yDim = yDim;
	}

	public int getYDim() {
		return yDim;
	}

	public void setNumSlices(int numSlices) {
		this.numSlices = numSlices;	
	}

	public int getNumSlices() {
		return numSlices;
	}

	public void setTEValues(double[] tEValuesIn) {
		TEValues = tEValuesIn;
	}

	public double[] getTEValues() {
		return TEValues;
	}
}
