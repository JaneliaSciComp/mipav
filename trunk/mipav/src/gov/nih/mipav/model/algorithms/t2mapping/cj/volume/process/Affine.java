package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import java.util.*;

/**
 *  Affine changes to Slices, Volumes and MCVolumes.  This includes
 *  translations and rotations.
 */
public class Affine 
{
	/** Do Linear Interpolation. */
	final static public int NEAREST_NEIGHBOR_INTERPOLATION = 0;
	final static public int LINEAR_INTERPOLATION = 1;
	
	// The interpolation method of choice.
	private int interpolationMethod = LINEAR_INTERPOLATION; 

	/* 
	 *  Set whether the translation rotates around to the
	 *  other side or whether we should set the empty parts 
	 *  to 0.
	 */
	static public int AFFINE_ZERO = 0;
	static public int AFFINE_ROTATE = 1;

	private int affine_method = AFFINE_ZERO;

	/** How verbose do you want me to be? */
	private int verbose = 0;

	public Affine()
	{
	}

	/** 
	 *  Set the verbose'ness.
	 */
	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	/**
	 *  Set the interpolation method.
	 */
	public void setInterpolationMethod( final int interpolationMethod )
	{
		this.interpolationMethod = interpolationMethod;
	}

	public void setAffineMethod( final int affine_method )
	{
		this.affine_method = affine_method;
	}
	/*
	 *  The translate MCVOLUME code.
	 *
	 */

	/**
	 *
	 */
	public void translateX(MCVolume vol, final int offset)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			translateX( vol.getVolume(ii), offset);	
		}
	}

	public void translateX(MCVolume vol, final double offset)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			translateX( vol.getVolume(ii), offset);	
		}
	}

	/**
	 *
	 */
	public void translateY(MCVolume vol, final int offset)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			translateY( vol.getVolume(ii), offset);	
		}
	}

	/**
	 *
	 */
	public void translateY(MCVolume vol, final double offset)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			translateY( vol.getVolume(ii), offset);	
		}
	}

	/**
	 *
	 */
	public void translateZ(MCVolume vol, final int offset)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			translateZ( vol.getVolume(ii), offset);	
		}
	}

	/**
	 *
	 */
	public void translateZ(MCVolume vol, final double offset)
	{
		for(int ii=0; ii<vol.getNChannels(); ii++)
		{
			translateZ( vol.getVolume(ii), offset);	
		}
	}

	/*
	 *  The translate VOLUME code.
	 *
	 */

	/**
	 */
	public void translateX(Volume real, final int offset)
	{
		for(int ii=0; ii<real.getNSlices(); ii++)
		{
			translateX( real.getSlice(ii), offset);	
		}
	}

	/**
	 */
	public void translateX(Volume real, final double offset)
	{
		for(int ii=0; ii<real.getNSlices(); ii++)
		{
			translateX( real.getSlice(ii), offset);	
		}
	}

	/**
	 */
	public void translateY(Volume vol, final int offset)
	{
		for(int ii=0; ii<vol.getNSlices(); ii++)
		{
			translateY( vol.getSlice(ii), offset);	
		}
	}

	/**
	 */
	public void translateY(Volume vol, final double offset)
	{
		for(int ii=0; ii<vol.getNSlices(); ii++)
		{
			translateY( vol.getSlice(ii), offset);	
		}
	}

	/**
	 *  Shift the slices up if positive and down if negative.
	 *
	 */
	public void translateZ(Volume vol, final int offset)
	{
		Volume newvol = (Volume)vol.clone();

		if( offset > 0 )
		{
			for( int ii=offset; ii<vol.getNSlices(); ii++ )
			{
				newvol.setSlice( vol.getSlice( ii ), ii-offset );	
			}
		}
		else if( offset < 0 ) 
		{
			for( int ii=0; ii<vol.getNSlices()+offset; ii++ )
			{
				newvol.setSlice( vol.getSlice( ii + offset ), ii);	
			}
		}
		else 
		{
		}
	}

	/**
	 *  Shift the slices up if positive and down if negative.
	 *
	 */
	public void translateZ(Volume vol, final double offset)
	{
	}

	/*
	 *  The translate SLICE code.
	 *
	 */

	/**
	 *  Translate the data across the columns by the integer amount offset.
	 */
	public void translateX( Slice slice, final int offset )
	{
		translateXFixed( slice, offset );
	}

	/**
	 *  Translate the data across the columns by the float amount offset.
	 *
	 *  This will call the appropriate translate code whether the offset
	 *  is an integer (hidden as a float) or if it is not an integer.
	 */
	public void translateX( Slice slice, final double offset )
	{
		if( (double)Math.round(offset) == offset )
		{
			translateXFixed( slice, (int)Math.round(offset) );
		}
		else
		{
			translateXInterp( slice, offset );
		}
	}

	/**
	 *  Translate the image in the column direction, positive
	 *  is to the left and negative is to the right (ala Matlab).
	 */
	private void translateXFixed( Slice real, final int offset )
	{
		float[] temp = null;
		float[] temp2 = new float[real.getNCols() ];

		// If offset is positive...
		for(int row=0; row<real.getNRows(); row++)
		{
			Arrays.fill( temp2, (float)0.0 );

			temp = real.getRow( row );
			if( offset < 0 )
			{
				System.arraycopy(temp, 0, temp2, -offset, temp.length+offset);

				if( affine_method == AFFINE_ROTATE )
				{
					System.arraycopy(temp, temp.length-offset, temp2, 0, 
									 offset);
				}
			}
			else if( offset > 0 )
			{
				System.arraycopy(temp, offset, temp2, 0, temp.length-offset);

				if( affine_method == AFFINE_ROTATE )
				{
					System.arraycopy(temp, 0, temp2, temp.length-offset, 
									 offset);
				}
			}
			else
			{
				System.arraycopy(temp, 0, temp2, 0, temp.length);
			}

			real.setRow( temp2, row );
		}

	}

	private void translateXInterp(Slice slice, final double offset )
	{
		/*
		 *  First, translate the slice by the integral amount.
		 */
		if( verbose > 1 )
		{
			System.out.println("translateXInterp: Doing fixed shift of " +  
				(int)Math.floor(offset) + " pixels.");
		}
		translateXFixed( slice, (int)Math.floor(offset) );

		/*
		 *  Now, shift the pixels by what is left.
		 */
		double shift = offset - (double)Math.floor( offset );

		if( verbose > 1 )
		{
			System.out.println("translateXInterp: Now doing fractional shift of " + 
								shift);
		}

		float[] temp = null;
		float[] temp2 = new float[ slice.getNCols() ];

		// Now do the interpolation.
		for(int row=0; row<slice.getNRows(); row++)
		{
			temp = slice.getRow( row );
			interpolate(temp, temp2, shift);
			slice.setRow( temp2, row );
		}
	}

	/**
	 *  Translate the data across the rows by the integer amount offset.
	 */
	public void translateY( Slice slice, final int offset )
	{
		translateYFixed( slice, offset );
	}

	/**
	 *  Translate the data across the rows by the float amount offset.
	 *
	 *  This will call the appropriate translate code whether the offset
	 *  is an integer (hidden as a float) or if it is not an integer.
	 */
	public void translateY( Slice slice, final double offset )
	{
		if( (double)Math.round(offset) == offset )
		{
			translateYFixed( slice, (int)Math.round(offset) );
		}
		else
		{
			translateYInterp( slice, offset );
		}
	}

	/**
	 *  Translate the image in the column direction, positive
	 *  is to the right and negative is to the left.
	 */
	private void translateYFixed( Slice real, final int offset )
	{
		float[] temp = null;
		float[] temp2 = new float[real.getNRows() ];

		// If offset is positive...
		for(int col=0; col<real.getNCols(); col++)
		{
			Arrays.fill( temp2, (float)0.0 );

			temp = real.getCol( col );
			if( offset < 0 )
			{
				System.arraycopy(temp, 0, temp2, -offset, temp.length+offset);
				if( affine_method == AFFINE_ROTATE )
				{
					System.arraycopy(temp, temp.length-offset, temp2, 0, 
									 offset);
				}
			}
			else if( offset > 0 )
			{
				System.arraycopy(temp, offset, temp2, 0, temp.length-offset);

				if( affine_method == AFFINE_ROTATE )
				{
					System.arraycopy(temp, 0, temp2, temp.length-offset, 
									 offset);
				}
			}
			else
			{
				System.arraycopy(temp, 0, temp2, 0, temp.length);
			}

			real.setCol( temp2, col );
		}

	}

	private void translateYInterp(Slice slice, final double offset )
	{
		/*
		 *  First, translate the slice by the integral amount.
		 */
		if( verbose > 1 )
		{
			System.out.println("translateYInterp: Doing fixed shift of " +  
				(int)Math.floor(offset) + " pixels.");
		}
		translateYFixed( slice, (int)Math.floor(offset) );

		/*
		 *  Now, shift the pixels by what is left.
		 */
		double shift = offset - (double)Math.floor( offset );

		if( verbose > 1 )
		{
			System.out.println("translateYInterp: Now doing fractional shift of " + 
								shift);
		}

		float[] temp = null;
		float[] temp2 = new float[ slice.getNRows() ];

		// Now do the interpolation.
		for(int col=0; col<slice.getNCols(); col++)
		{
			temp = slice.getCol( col );
			interpolate(temp, temp2, shift);
			slice.setCol( temp2, col );
		}
	}

	/**
	 *  Does nothing, just here for completeness, probably should remove it.
	 */
	public void translateZ( Slice real, final int offset ) { }
	public void translateZ( Slice real, final double offset ) { }

	/**
	 *  Do the interpolation based on the chosen method.
	 */
	private void interpolate(final float[] original, float[] output, 
	                         final double offset)
	{
		switch( interpolationMethod )
		{
			case NEAREST_NEIGHBOR_INTERPOLATION:
				nearestNeighborInterpolation(original, output, offset);
				break;
			case LINEAR_INTERPOLATION:
				linearInterpolation(original, output, offset);
				break;
			default:
				System.out.println("Affine: Interpolation method not defined.");
				System.exit(-1);
		}
	}

	/**
	 *  Nearest neighbor interpolation.
	 *
	 *  Untested.
	 */
	private void nearestNeighborInterpolation(final float[] original, float[] output, 
	                                 final double offset)
	{
		for(int ii=0; ii<original.length-1; ii++)
		{
			output[ii] = (float)((offset<0.5)?original[ii+1]:original[ii]);
		}
	}

	/**
	 *  Linear interpolation.
	 */
	private void linearInterpolation(final float[] original, float[] output, 
	                                 final double offset)
	{
		for(int ii=0; ii<original.length-1; ii++)
		{
			output[ii] = (float)((1.0-offset) * (double)original[ii] + 
				   				offset * (double)original[ii+1]);
		}
	}

}
