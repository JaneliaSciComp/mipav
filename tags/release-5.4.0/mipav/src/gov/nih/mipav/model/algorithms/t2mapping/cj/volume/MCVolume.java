package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.util.*;
import java.lang.*;
import java.io.*;
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
public class MCVolume implements Cloneable 
{

	Vector volumes=null;
	int nRows=0, nCols=0, nSlices=0, nChannels=0;

	/**
	 *  Constructor in which we know the number of rows, columns,
	 *  slices and channels.
	 */
	public MCVolume(int nrows_in, int ncols_in, int nslices_in, 
	                int nchannels_in) 
	{
		int ii = 0;
		nRows = nrows_in; nCols = ncols_in; nSlices = nslices_in;
		nChannels = nchannels_in;

		try 
		{
			volumes = new Vector(nchannels_in);
			volumes.setSize( nchannels_in );
		}
		catch( OutOfMemoryError e )
		{
			System.out.println("MCVolume: Could not allocate " + nrows_in +
				" " + ncols_in + " " + nslices_in + " " + nchannels_in );
			System.out.println("   died on channel " + (ii+1) );
			System.exit(-1);
		}
	}

	/**
	 *  Clone the object.
	 */
	public Object clone()
	{
		Object o = null;
		try {
			o = super.clone();
		}
		catch( CloneNotSupportedException e) {
			System.out.println("MCVolume can not clone");
		}

		return o;
	}

	/**
	 *  Append the MCVolume specified in the parameter
	 *  onto the volumes currently stored.
	 */
	public void concatenate(MCVolume v)
	{
		volumes.addAll(v.volumes);
		nChannels += v.volumes.size();
	}

	/**
	 *  Make sure the row is correctly set.
	 */
	protected boolean rowInBounds(int row) {
		if( row >= 0 && row < nRows ) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 *  Make sure the column is correctly set.
	 */
	protected boolean colInBounds(int col)
	{
		if( col >= 0 && col < nRows ) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 *  Make sure the slice is correctly set.
	 */
	protected boolean sliceInBounds(int slice) {
		if( slice >= 0 && slice < nSlices ) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 *  Make sure the channel is correctly set.
	 */
	protected boolean channelInBounds(int channel) {
		if( channel >= 0 && channel < nChannels ) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 *  Retrieve the number of rows.
	 */
	final public int getNRows() 
	{
		return nRows;
	}

	/**
	 *  Retrieve the number of columns.
	 */
	final public int getNCols() 
	{
		return nCols;
	}

	/**
	 *  Retrieve the number of slices.
	 */
	final public int getNSlices() 
	{
		return nSlices;
	}

	/**
	 *  Retrieve the number of channels.
	 */
	final public int getNChannels() 
	{
		if (volumes != null) return nChannels;
		else return 0;
	}

	protected void allocVolume(int channel)
	{
		volumes.set(channel, new Volume(nRows, nCols, nSlices) );
	}

	/**
	 *  Set the data passed in that is located at the row, column
	 *  slice and channel specified.
	 */
	public void setData(float val, int row, int col, 
	                    int slice, int channel)
	{
		if( !rowInBounds(row) || !colInBounds(col) ||
			!sliceInBounds(slice) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getData");
		}

		if( val != 0.0 || volumes.get(channel) != null )
		{
			//  If that channel is not allocated yet,  then allocate it before
 			//  setting the data.
			if( volumes.get(channel) == null ) allocVolume(channel);

			((Volume)volumes.elementAt(channel)).setData(val, row,col,slice);
		}
	}

	/**
	 *  Set the data passed in that is located at the row, column
	 *  slice and channel specified.
	 */
	public void setData(double val, int row, int col, int slice, int channel)
	{
		setData((float)val,row,col,slice,channel);
	}

	/**
	 *  Get the data located at the row, column
	 *  slice and channel specified.
	 */
	public float getData(int row, int col, int slice, int channel)
	{
		float toreturn = (float)0.0;

		if( !rowInBounds(row) || !colInBounds(col) ||
		    !sliceInBounds(slice) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getData");
		}

		if( volumes.get(channel) == null )
		{
			toreturn = (float)0.0;
		}
		else
		{
			toreturn = ((Volume)volumes.elementAt(channel)).getData(row,col,slice);
		}

		return toreturn;
	}

	/**
	 *  Get the row of data located at the row, 
	 *  slice and channel specified.
	 */
	public float[] getRow(int row, int slice, int channel)
	{
		float[] toreturn = null;

		if( !rowInBounds(row) ||
		    !sliceInBounds(slice) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getRow");
		}
		
		if( volumes.elementAt(channel) == null )
		{
			toreturn = new float[ nCols ];
			Arrays.fill( toreturn, (float)0.0 );
		}
		else 
		{
			toreturn = ((Volume)volumes.elementAt(channel)).getRow(row, slice);
		}

		return toreturn;
	}

	/**
	 *  Get the row of data located at the row, 
	 *  slice and channel specified.
	 */
	public void setRow(float[] dd, int row, int slice, int channel)
	{
		if( !rowInBounds(row) ||
		    !sliceInBounds(slice) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getRow");
		}

		if( volumes.elementAt(channel) == null ) allocVolume(channel);
		
		((Volume)volumes.elementAt(channel)).setRow(dd, row, slice);
	}

	/**
	 *  Get the column of data located at the row, 
	 *  slice and channel specified.
	 */
	public float[] getCol(int col, int slice, int channel)
	{
		float[] toreturn = null;

		if( !colInBounds(col) ||
		    !sliceInBounds(slice) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getCol");
		}

		if( volumes.elementAt(channel) == null )
		{
			toreturn = new float[ nRows ];
			Arrays.fill( toreturn, (float)0.0 );
		}
		else 
		{
			toreturn = ((Volume)volumes.elementAt(channel)).getCol(col, slice);
		}

		return toreturn;
	}

	/**
	 *  Get the slice located at the slice and channel specified.
	 */
	public Slice getSlice(int slice, int channel)
	{
		Slice toreturn = null;

		if( !sliceInBounds(slice) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getSlice");
		}

		if( volumes.elementAt(channel) == null ) allocVolume(channel);

		toreturn = ((Volume)volumes.elementAt(channel)).getSlice(slice);

		return toreturn;
	}
	
	/**
	 *  Set the slice located at the slice and channel specified.
	 */
	public void setSlice(Slice slice, int slice_index, int channel)
	{
		if( !sliceInBounds(slice_index) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getSlice");
		}

		if( volumes.elementAt(channel) == null ) allocVolume(channel);

		((Volume)volumes.elementAt(channel)).setSlice(slice, slice_index);
	}
	
	/**
	 *  Get the channel of data located at the row, column and slice specified.
	 */
	public double[] getChannelDouble(int row, int col, int slice)
	{
		if( !rowInBounds(row) || !colInBounds(col) || !sliceInBounds(slice) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getChannel");
		}

		double[] chdata = new double[ nChannels ];

		for(int ii=0; ii<nChannels; ii++) {
			chdata[ii] = (double)getData(row,col,slice,ii);
		}
			
		return chdata;
	}

	/**
	 *  Set the channel of data located at the row, column and slice specified.
	 */
	public void setChannel(float[] data_in, int row, int col, int slice)
	{
		if( !rowInBounds(row) || !colInBounds(col) || !sliceInBounds(slice) || data_in.length != nChannels ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::setChannel");
		}

		for(int ii=0; ii<data_in.length; ii++) {
			setData(data_in[ii], row,col,slice,ii);
		}
	}
	
	/** Set the channel information based on that passed in.
	 *
	 */
	public void setChannel(final double[] data_in, 
	                       final int row, final int col, final int slice)
	{
		//  Bounds checking
		if( !rowInBounds(row) || !colInBounds(col) || 
		    !sliceInBounds(slice) || data_in.length > nChannels ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::setChannel");
		}

		//  Actual setting.......
		for(int ii=0; ii<data_in.length; ii++) {
			setData((float)data_in[ii], row,col,slice,ii);
		}
	}
	
	/** Get the channel information.
	 *
	 */
	public float[] getChannel(int row, int col, int slice)
	{

		//  Bounds checking
		if( !rowInBounds(row) || !colInBounds(col) || !sliceInBounds(slice) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getChannel");
		}

		//  Copy over the data.
		float[] chdata = new float[ nChannels ];
		for(int ii=0; ii<nChannels; ii++) {
			chdata[ii] = getData(row,col,slice,ii);
		}
			
		return chdata;
	}
	
	public void getChannel(float[] out, int row, int col, int slice)
	{

		//  Bounds checking
		if( !rowInBounds(row) || !colInBounds(col) || !sliceInBounds(slice) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getChannel");
		}

		//  Copy over the data.
		for(int ii=0; ii<nChannels; ii++) {
			out[ii] = getData(row,col,slice,ii);
		}
	}

	public void getChannel(double[] out, int row, int col, int slice)
	{

		//  Bounds checking
		if( !rowInBounds(row) || !colInBounds(col) || !sliceInBounds(slice) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::getChannel");
		}

		//  Copy over the data.
		for(int ii=0; ii<nChannels; ii++) {
			out[ii] = getData(row,col,slice,ii);
		}
	}

	/**
	 *  Get the specified volume.
	 */
	public Volume getVolume(int channel)
	{
		Volume toreturn = null;

		if( !channelInBounds(channel) ) 
		{
			throw new ArrayIndexOutOfBoundsException("MCVolume::getVolume");
		}

		if( volumes.elementAt(channel) == null ) allocVolume(channel);

		toreturn = ((Volume)volumes.elementAt(channel));

		return toreturn;
	}
	
	/**
	 *  Set the specified volume.
	 */
	public void setVolume(Volume vol, int channel)
	{
		if( !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::setVolume");
		}

		volumes.set(channel, vol);
	}
	
	/**
	 *  Remove the volume at the specified location.
	 */
	public void removeVolume(int channel)
	{
		if( !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::removeVolume");
		}

		volumes.remove(channel);
		nChannels--;
	}
	
	/** 
	 *  Read in the slice and channel and default to reading it in as
	 *  a short.
	 */
	public void read(DataInput ds, int slice, int channel)
	{
		read(ds, slice,  channel, MRIFile.DATATYPE_SHORT );
	}

	/** 
	 *  Read in the slice and channel in the specified data format.
	 */
	public void read(DataInput ds, int slice, int channel, int datatype )
	{
		if( !sliceInBounds(slice) || !channelInBounds(channel) ) {
			throw new ArrayIndexOutOfBoundsException("MCVolume::read");
		}

		getSlice(slice, channel).read(ds, datatype);
	}

	/**
	 *  Write out the slice and channel in the specified format.
	 */
	public void write(DataOutput ds, int slice, int channel, int datatype )
	{
		getSlice(slice, channel).write(ds, datatype);
	}

	/**
	 *  Show the data.
	 */
	public void show()
	{
		for(int ii=0; ii<nChannels; ii++) {
			System.out.println("Showing Channel " + ii );
			((Volume)volumes.elementAt(ii)).show();
		}
	}
}
