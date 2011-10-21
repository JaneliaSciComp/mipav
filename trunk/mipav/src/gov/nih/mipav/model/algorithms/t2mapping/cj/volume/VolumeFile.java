package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.util.*;
import java.nio.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *   Volume class that creates a slice only when there
 *   are non-zero elements in the slice, otherwise, 
 *   the assumption is that all elements in the slice
 *   are zero.
 */
public class VolumeFile extends Volume
{
	FloatBuffer buf = null;
	int sliceSize;

	public VolumeFile(int nrows, int ncols, int nslices, FloatBuffer fb)
	{
		super(nrows,ncols,nslices);
		sliceSize = nRows * nCols;
		buf = fb;
	}

	public Object clone()
	{
		return super.clone();
	}

	protected void allocSlice(int slice)
	{
		buf.position( slice * sliceSize );
		slices.set(slice, new SliceFile(nRows, nCols,
                  (FloatBuffer)buf.slice().limit(sliceSize)) );
	}
}
