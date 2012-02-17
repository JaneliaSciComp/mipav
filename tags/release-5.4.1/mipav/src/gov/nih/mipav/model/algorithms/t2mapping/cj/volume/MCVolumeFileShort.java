package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.util.*;
import java.lang.*;
import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;

/**
 *  An MCVolume is a multi-channel volume in which multiple
 *  volumes are stored internally.
 *
 *  The vector of volumes will have an initial capacity set
 *  to the number of channels specified, but will not 
 *  allocate the volumes until there are non-zero elements
 *  in the volume.
 *
 */
public class MCVolumeFileShort extends MCVolume
{
	private File tempFile = null;
	ShortBuffer buf = null;
	int volSize;

	/**
	 *  Constructor in which we know the number of rows, columns,
	 *  slices and channels.
	 */
	public MCVolumeFileShort(File ff, int nrows_in, int ncols_in, int nslices_in, 
	                int nchannels_in) 
	{
		super(nrows_in,ncols_in,nslices_in,nchannels_in);
		tempFile = ff;
		init();
	}

	private void init()
	{
		volSize = nRows*nCols*nSlices;

		try
		{
			FileChannel fc = new RandomAccessFile(tempFile,"r").getChannel();
			ByteBuffer bb = fc.map(FileChannel.MapMode.READ_WRITE, 0, nChannels * volSize * 4);
			bb.order(ByteOrder.nativeOrder());
			buf = bb.asShortBuffer();
		}
		catch (IOException e)
		{
			System.err.println("MCVolumeFileShort: Could not create tempfile");
			System.exit(-1);
		}
	}

	protected void finalize()
	{
		tempFile.delete();
	}

	protected void allocVolume(int channel)
	{
		buf.position( channel * volSize );
		volumes.set(channel, new VolumeFileShort(nRows, nCols, nSlices,
                    (ShortBuffer)buf.slice().limit(volSize)) );
	}

	/**
	 *  Clone the object.
	 */
	public Object clone()
	{
		return super.clone();
	}
}
