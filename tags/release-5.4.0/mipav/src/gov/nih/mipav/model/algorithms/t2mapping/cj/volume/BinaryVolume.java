package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.util.*;
//import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class BinaryVolume implements Cloneable {

	private Vector slices;

	public BinaryVolume(int nrows_in, int ncols_in, int nslices_in)
	{
		slices = new Vector(nslices_in);
		for(int ii=0; ii<nslices_in; ii++) {
			slices.addElement( new BinarySlice(nrows_in, ncols_in) );
		}
	}

	public Object clone()
	{
		Object o = null;
		try {
			o = super.clone();
		}
		catch( CloneNotSupportedException e) {
			System.out.println("BinaryVolume can not clone");
		}

		return o;
	}

	final public int getNSlices() { return slices.size(); }

	final public int getNRows() 
	{ 
		if( slices.size() > 0 ) {
			return ((BinarySlice)slices.elementAt(0)).getNRows();
		}
		else {
			return 0;
		}
	}

	final public int getNCols() 
	{ 
		if( slices.size() > 0 ) {
			return ((BinarySlice)slices.elementAt(0)).getNCols();
		}
		else {
			return 0;
		}
	}

	private boolean rowInBounds(int row) {
		if( row >= 0 && row < getNRows() ) {
			return true;
		}
		else {
			return false;
		}
	}

	private boolean colInBounds(int col) {
		if( col >= 0 && col < getNCols() ) {
			return true;
		}
		else {
			return false;
		}
	}

	private boolean sliceInBounds(int slice) {
		if( slice >= 0 && slice < getNSlices() ) {
			return true;
		}
		else {
			return false;
		}
	}
	
	final public void setData(boolean val, int row, int col, int slice )
	{
		((BinarySlice)slices.elementAt(slice)).setData(val, row,col);
	}

	final public boolean getData(int row, int col, int slice )
	{
		return ((BinarySlice)slices.elementAt(slice)).getData(row,col);
	}

	public boolean[] getRow(int row, int slice)
	{
		if( !rowInBounds(row) || !sliceInBounds(slice) ) { 
			throw new ArrayIndexOutOfBoundsException("BinaryVolume::getRow");
		}

		return getSlice(slice).getRow(row);
	}

	public boolean[] getCol(int col, int slice)
	{
		if( !colInBounds(col) || !sliceInBounds(slice) ) { 
			throw new ArrayIndexOutOfBoundsException("BinaryVolume::getCol");
		}

		return getSlice(slice).getCol(col);
	}

	public BinarySlice getSlice(final int slice) 
	{
		if( !sliceInBounds(slice) ) {
			throw new ArrayIndexOutOfBoundsException("BinaryVolume::getBinarySlice");
		}

		return (BinarySlice)slices.get(slice);
	}

	public void setSlice(final BinarySlice slice, final int slice_index) 
	{
		if( !sliceInBounds(slice_index) ) {
			throw new ArrayIndexOutOfBoundsException("BinaryVolume::setBinarySlice");
		}

		slices.set(slice_index, slice);
	}

	public void erodeSlices(int radius, boolean l_inf)
	{
		for(int ii=0; ii<slices.size(); ii++) {
			((BinarySlice)slices.elementAt(ii)).erode(radius, l_inf);
		}
	}

	public void show()
	{
		for(int ii=0; ii<slices.size(); ii++) {
			System.out.println("Showing slice " + ii );
			((BinarySlice)slices.elementAt(ii)).show();
		}
	}
}
