package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *   Volume class that creates a slice only when there
 *   are non-zero elements in the slice, otherwise, 
 *   the assumption is that all elements in the slice
 *   are zero.
 */
public class Volume extends VolumeBase implements Cloneable 
{
	/** Number of rows */
	protected int nRows = 0;

	/** Number of columns */
	protected int nCols = 0;

	/** Number of slices */
	protected int nSlices = 0;

	/** The vector of slices. */
	protected Vector slices = null;

	/**
	 *  Allocate the vector of slices.
	 */
	public Volume(int nrows, int ncols, int nslices)
	{
		nRows = nrows;
		nCols = ncols;
		nSlices = nslices;

		/*
		 *  Set the vector up, but don't actually
		 *  create the empty slices.
		 */
		slices = new Vector(nslices);
		slices.setSize(nSlices);
	}

	public Volume( final VolumeBase vb )
	{
		slices = new Vector(vb.getNSlices());
		slices.setSize(vb.getNSlices());

		nRows = vb.getNRows();
		nCols = vb.getNCols();
		nSlices = vb.getNSlices();

		for(int ii=0; ii<nRows; ii++) 
			for(int jj=0; jj<nCols; jj++) 
				for(int kk=0; kk<nSlices; kk++) 
				{
					//System.out.println(vb.getData(ii,jj,kk));
					setData(vb.getData(ii,jj,kk), ii, jj, kk);
				}

	}

	public Volume( final Volume in )
	{
		slices = new Vector(in.getNSlices());
		slices.setSize(in.getNSlices());

		for(int ii=0; ii<in.getNSlices(); ii++ )
		{
			if( in.getSlice(ii) != null )
				{ slices.add(ii, new Slice(in.getSlice(ii))); }
//				{ slices.add(ii, (in.getSlice(ii).clone()); }
		}

		nRows = in.nRows;
		nCols = in.nCols;
		nSlices = in.nSlices;
	}

    /**
     * Be careful using Volume.clone() because VolumeFile.clone() is
     * not supported!
     */
	public Object clone()
	{
		Object o = null;
		try {
			o = super.clone();
		}
		catch( CloneNotSupportedException e) {
			System.out.println("Volume can not clone");
		}

		return o;
	}

	/**
	 *  Get the number of slices.
	 */
	final public int getNSlices() 
	{ 
		return nSlices;
	}

	/**
	 *  Get the number of rows.
	 */
	final public int getNRows() 
	{ 
		return nRows;
	}

	/** 
	 *  Get the number of columns.
	 */
	final public int getNCols() 
	{ 
		return nCols;
	}

	protected void allocSlice(int slice)
	{
		slices.set(slice, new Slice(nRows, nCols));
	}

	/**
	 *  Determine if the row is in bounds.
	 */
	final protected boolean rowInBounds(int row) 
	{
		if( row >= 0 && row < getNRows() ) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 *  Determine if the column is in bounds.
	 */
	final protected boolean colInBounds(int col) 
	{
		if( col >= 0 && col < getNCols() ) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 *  Determine if the slice is in bounds.
	 */
	final protected boolean sliceInBounds(int slice) 
	{
		if( slice >= 0 && slice < getNSlices() ) {
			return true;
		}
		else {
			return false;
		}
	}
	
	/** Threshold the data and return a BinarySlice that is true 
	 *  where the Slice had data less than the threshold and false
	 *  otherwise.
	 */
	public BinaryVolume threshold(final double lower, final double upper)
	{
		BinaryVolume b = new BinaryVolume( getNRows(), getNCols(), getNSlices() );

		for(int kk=0; kk<getNSlices(); kk++)
		{
			for(int ii=0; ii<getNRows(); ii++)
			{
				for(int jj=0; jj<getNCols(); jj++)
				{
					if( getData(ii,jj,kk) < upper && getData(ii,jj,kk) > lower )
						b.setData(true,ii,jj,kk);
					else
						b.setData(false,ii,jj,kk);
				}
			}
		}

		return b;
	}

	/** Apply a mask to the data in place, which means
	 *  to set all data to zero where the mask is false.
	 */
	public void applyMask( final BinaryVolume v )
	{
		for(int ii=0; ii<getNSlices(); ii++)
		{
			/*
			 *  If the slice is not null, apply the mask.
			 *  Otherwise the slice is all zeros and it doesn't
			 *  matter what the mask is, zeros * anything is still
			 *  zeros.
			 */
			if( slices.get(ii) != null )
			{
				((Slice)slices.get(ii)).applyMask( v.getSlice(ii) );
			}
		}
	}


	final public void setData(double val, int row, int col, int slice )
	{
	    setData((float)val,row,col,slice);
	}

	final public void setData(float val, int row, int col, int slice )
	{
		if( !rowInBounds(row) || !colInBounds(col) ||
			!sliceInBounds(slice) ) {
			throw new ArrayIndexOutOfBoundsException("Volume::setData");
		}

		if( val != 0.0 || slices.get(slice) != null )
		{
			//  If that channel is not allocated yet,  then allocate it before
			//  setting the data.
			if( slices.get(slice) == null ) allocSlice(slice);

			((Slice)slices.get(slice)).setData(val, row, col);
		}
	}

	final public float getData(int row, int col, int slice )
	{
		float toreturn;

		if( slices.get(slice) == null )
		{
			toreturn = (float)0.0;
		}
		else
		{
			toreturn = ((Slice)slices.get(slice)).getData(row,col);
		}

		return toreturn;
	}

	public float[] getRow(int row, int slice)
	{
		float[] toreturn = null;

		if( !rowInBounds(row) || !sliceInBounds(slice) ) { 
			throw new ArrayIndexOutOfBoundsException("Volume::getRow");
		}

		if( slices.get(slice) == null )
		{
			toreturn = new float[ nCols ];
			Arrays.fill(toreturn, (float)0.0);
		}
		else 
		{
			toreturn = getSlice(slice).getRow(row);
		}

		return toreturn;
	}

	public void setRow(float[] dd, int row, int slice)
	{
		if( !rowInBounds(row) || !sliceInBounds(slice) ) { 
			throw new ArrayIndexOutOfBoundsException("Volume::getRow");
		}

		if( slices.get(slice) == null ) allocSlice(slice);

		getSlice(slice).setRow(dd, row);
	}

	public float[] getCol(int col, int slice)
	{
		float[] toreturn = null;

		if( !colInBounds(col) || !sliceInBounds(slice) ) { 
			throw new ArrayIndexOutOfBoundsException("Volume::getCol");
		}

		if( slices.get(slice) == null )
		{
			toreturn = new float[ nRows ];
			Arrays.fill(toreturn, (float)0.0);
		}
		else 
		{
			toreturn = getSlice(slice).getCol(col);
		}

		return toreturn;
	}

	public Slice getSlice(final int slice) 
	{
		if( !sliceInBounds(slice) ) {
			throw new ArrayIndexOutOfBoundsException("Volume::getSlice");
		}

		if( slices.get(slice) == null ) allocSlice(slice);

		return (Slice)slices.get(slice);
	}

	public void setSlice(final Slice slice, final int slice_index) 
	{
		if( !sliceInBounds(slice_index) ) {
			throw new ArrayIndexOutOfBoundsException("Volume::setSlice");
		}

		slices.set(slice_index, slice);
	}

	public void setSlice(final BinarySlice binary_slice, final int slice_index) 
	{
		if( !sliceInBounds(slice_index) ) {
			throw new ArrayIndexOutOfBoundsException("Volume::setSlice");
		}

		Slice s = new Slice( binary_slice.getNRows(), binary_slice.getNCols() );	

		for(int row=0; row<s.getNRows(); row++)
		{
			for(int col=0; col<s.getNCols(); col++)
			{
				if( binary_slice.getData(row,col) )
				{
					s.setData(1.0, row,col);
				}
			}
		}

		slices.set(slice_index, s);
	}

	public void show()
	{
		for(int ii=0; ii<slices.size(); ii++) {
			System.out.println("Showing slice " + ii );
			((Slice)slices.get(ii)).show();
		}
	}
}
