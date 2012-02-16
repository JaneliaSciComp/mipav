package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

abstract public class ImageFilter2D 
{
	protected int verbose = 0;

	public ImageFilter2D() { }

	/** 
	 *  Set the verbose'ness.
	 */
	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	abstract public void filter(Slice slice);

	public void filter(Volume vol)
	{
	//  Loop over the slices and channels.
	for(int slice=0; slice<vol.getNSlices(); slice++)
		{
			if( verbose > 1 )
			{
				System.out.println("Filtering slice " + slice);
			}

			// Get the slice of interest.
			Slice s = vol.getSlice(slice);
			filter(s);
			// Not really necessary
			vol.setSlice(s, slice);
		}
	}

	public void filter(MCVolume vol)
	{

    for(int slice=0; slice<vol.getNSlices(); slice++) {
		for(int channel=0; channel<vol.getNChannels(); channel++) {

			if( verbose > 1 )
			{
				System.out.println("Filtering channel " + channel +
				", slice " + slice);
			}

			// Get the slice of interest.
			Slice s = vol.getSlice(slice, channel);
			filter(s);
			// Not really necessary
			vol.setSlice(s, slice, channel);
            }
        }
    }
}
