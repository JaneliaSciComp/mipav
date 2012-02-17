package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import java.io.*;
import java.util.Arrays;
import javax.imageio.stream.*;

/** 
 *  The slice class that stores data for one single image.
 *
 *  Key thought here is that the data is stored in ROW-MAJOR
 *  format as that is the way most of the MRI files store 
 *  their data.  Therefore, A(row,col) is at 
 *  data[ row*nCols + col].
 */
public class Slice implements Cloneable
{
	protected float[] data = null;
	protected int nRows;
	protected int nCols;

	public Slice() { }

	public Slice( Slice s )
	{
		this.nRows = s.nRows;
		this.nCols = s.nCols;

		this.data = new float[ nRows * nCols ];

		for(int ii=0; ii<nRows; ii++)
		{
			for(int jj=0; jj<nCols; jj++)
			{
				data[ii*nCols+jj] = s.getData(ii,jj);
			}
		}
	}

	/** 
	 *  Slice constructor with nRows and cols.
	 *
	 */

	public Slice(int nrows_in, int ncols_in) 
	{
		nRows = nrows_in;
		nCols = ncols_in;
		data = new float[nRows*nCols];
		Arrays.fill(data, (float)0.0);
	}

	/** 
	 *  Cloner
	 *
	 */
	public Object clone()
	{
		Object o = null;
		try {
			o = super.clone();

			if( data != null )
			{
				((Slice)o).data = new float[data.length];
				System.arraycopy(data, 0, ((Slice)o).data, 0, data.length);
			}
		}
		catch( CloneNotSupportedException e) {
			System.out.println("Slice can not clone");
		}

		return o;
	}

	/** 
	 *  Get the number of rows.
	 *
	 */
	final public int getNRows() 
	{ 
		return nRows; 
	}

	/** 
	 *  Get the number of columns.
	 *
	 */
	final public int getNCols() 
	{ 
		return nCols; 
	}
	

	/**
	 *  Get the row of data.
	 */
	public float[] getRow(int row)
	{
		if( !rowInBounds(row)  ) {
			throw new ArrayIndexOutOfBoundsException("Slice::getRow");
		}

		float[] tempcol = new float[ nCols ];
		System.arraycopy(data, nCols*row, tempcol, 0, nCols);

		return tempcol;
	}

	public void getRow(float[] t, int row)
	{
		if( !rowInBounds(row) || t.length < nCols ) {
			throw new ArrayIndexOutOfBoundsException("Slice::getRow");
		}

		System.arraycopy(data, nCols*row, t, 0, nCols);
	}

	public void getRow(double[] t, int row)
	{
		if( !rowInBounds(row) || t.length < nCols ) {
			throw new ArrayIndexOutOfBoundsException("Slice::getRow");
		}

		int offset = nCols*row;

		for(int ii=0;ii<nCols;ii++)
		{
			t[ii] = (double)data[offset + ii];
		}
	}

	/**
	 *  Set the row of data.
	 */
	public void setRow(final float[] newrow, final int row)
	{
		if( !rowInBounds(row) || newrow == null || newrow.length != nCols ) {
			throw new ArrayIndexOutOfBoundsException("Slice::setRow");
		}

		System.arraycopy(newrow, 0, data, nCols*row, nCols);
	}

	/**
	 *  Get the column of data.
	 */
	public float[] getCol(final int col)
	{
		if( !colInBounds(col)  ) {
			throw new ArrayIndexOutOfBoundsException("Slice::getCol");
		}

		float[] tempcol = new float[ nRows ];

		for(int ii=0; ii<nRows; ii++) 
		{
			tempcol[ii] = data[ii*nRows + col];
		}

		return tempcol;
	}

	public void getCol(float[] t, final int col)
	{
		if( !colInBounds(col) || t.length < nRows ) {
			throw new ArrayIndexOutOfBoundsException("Slice::getCol");
		}

		for(int ii=0; ii<nRows; ii++) 
		{
			t[ii] = data[ii*nRows + col];
		}
	}
	/**
	 *  Set the column of data.
	 */
	public void setCol(final float[] tempcol, final int col)
	{
		if( !colInBounds(col) || tempcol == null || tempcol.length != nRows ) {
			throw new ArrayIndexOutOfBoundsException("Slice::setCol");
		}

		for(int ii=0; ii<nRows; ii++) 
		{
			data[ii*nRows + col] = tempcol[ii];
		}
	}

	/**
	 *  Get the data element.
	 */
	public float getData(final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("Slice::getData");
		}

		return data[row*nCols + col];
	}

	/**
	 *  Set the data element.
	 */
	public void setData(final float newval, final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("Slice::setData");
		}

		data[row*nCols + col] = newval;
	}

	/**
	 *  Set the data element.
	 */
	public void setData(final double newval, final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("Slice::setData");
		}

		data[row*nCols + col] = (float)newval;
	}

	public void read(DataInput ds)
	{
		read( ds, MRIFile.DATATYPE_USHORT );
	}

	/** 
	 *
	 */
	public void read(DataInput ds, final int datatype )
	{
        if (ds instanceof ImageInputStream) 
            readIIS((ImageInputStream)ds, datatype);
       	else 
            readDI(ds, datatype);
	}

	protected void readDI(DataInput ds, final int datatype )
	{
        final int sliceSize = nRows*nCols;

			System.out.println("blah balah");
        try {
			if( datatype == MRIFile.DATATYPE_BYTE ) {
			System.out.println("readDI  Datatype_ubyte");
				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = ds.readUnsignedByte();
				}
			}
			else if( datatype == MRIFile.DATATYPE_USHORT ) {
			System.out.println("readDI  Datatype_ushort");
				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = ds.readUnsignedShort();
				}
			}
			else if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = (float)ds.readDouble();
				}
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = ds.readFloat();
				}
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
			System.out.println("readDI  Datatype_short");
				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = ds.readShort();
				}
			}
			else {
				System.out.println("Slice::read: Could not figure out type.");
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("Slice::write: Could not read in the MRI data.");
            System.out.println(e);
            System.exit(0);
        }
	}


	protected void readIIS(ImageInputStream ds, final int datatype )
	{
        final int sliceSize = nRows*nCols;

			System.out.println("blah balah balh");
        try {
			if( datatype == MRIFile.DATATYPE_BYTE ) {
			System.out.println("readIIS  Datatype_ubyte should be fast");
				byte[] temp = new byte[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = (float)(temp[ii]);
				}
			}
			if( datatype == MRIFile.DATATYPE_USHORT ) {
			System.out.println("readIIS  Datatype_ushort should be fast");
				short[] temp = new short[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = Unsigned.floatFromUShort(temp[ii]);
				}
			}
			else if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				double[] temp = new double[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = (float)temp[ii];
				}
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				ds.readFully(data, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
			System.out.println("readIIS  Datatype_short should be fast");
				short[] temp = new short[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					data[ii] = temp[ii];
				}
			}
			else {
				System.out.println("Slice::read: Could not figure out type.");
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("Slice: Could not read in the MRI data.");
            System.out.println(e);
            System.exit(0);
        }
	}

	public void write(DataOutput ds)
	{
		write( ds, MRIFile.DATATYPE_USHORT );
	}

	public void write(DataOutput ds, final int datatype)
	{
		if( ds instanceof ImageOutputStream )
			writeIOS( (ImageOutputStream)ds, datatype );
		else
			writeDO( ds, datatype );
	}

	/** 
	 *  writeDO - Write the data to an ImageOutputStream. 
	 */
	protected void writeDO(DataOutput dos, final int datatype )
	{
        final int sliceSize = nRows*nCols;
	
        try {
			if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				for( int ii=0; ii<sliceSize; ii++) {
					dos.writeDouble((double)data[ii]);
				}
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					dos.writeFloat(data[ii]);
				}
			}
			else if( datatype == MRIFile.DATATYPE_USHORT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					dos.writeShort(Unsigned.toUShort(data[ii]));
				}
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					dos.writeShort((short)data[ii]);
				}
			}
			else {
				System.out.println("Slice::writeFast: Could not figure out type " + datatype);
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("Slice: Could not read in the MRI data.");
            System.out.println(e);
            System.exit(0);
        }
	}

	/** 
	 *  writeIOS - Write the data to an ImageOutputStream. 
	 */
	protected void writeIOS(ImageOutputStream ios, final int datatype )
	{
        final int sliceSize = nRows*nCols;
	
        try {
			if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				double[] temp = new double[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = (double)data[ii];
				}
				ios.writeDoubles(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				ios.writeFloats(data, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_USHORT ) {
				short[] temp = new short[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = Unsigned.toUShort(data[ii]);
				}
				ios.writeShorts(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
				short[] temp = new short[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = (short)data[ii];
				}
				ios.writeShorts(temp, 0, sliceSize);
			}
			else {
				System.out.println("Slice::writeFast: Could not figure out type " + datatype);
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("Slice: Could not read in the MRI data.");
            System.out.println(e);
            System.exit(0);
        }
	}

	/** Threshold the data and return a BinarySlice that is true 
	 *  where the Slice had data less than the threshold and false
	 *  otherwise.
	 */
	public BinarySlice threshold(final double lower, final double upper)
	{
		BinarySlice b = new BinarySlice( getNRows(), getNCols() );

		for(int ii=0; ii<getNRows(); ii++)
		{
			for(int jj=0; jj<getNCols(); jj++)
			{
				if( getData(ii,jj) <= upper && getData(ii,jj) >= lower )
					b.setData(true,ii,jj);
				else
					b.setData(false,ii,jj);
			}
		}

		return b;
	}


	/** Threshold the data and return a BinarySlice that is true 
	 *  where the Slice had data less than the threshold and false
	 *  otherwise.
	 */
	public BinarySlice thresholdLower(final double threshold)
	{
		BinarySlice b = new BinarySlice( getNRows(), getNCols() );

		for(int ii=0; ii<getNRows(); ii++)
		{
			for(int jj=0; jj<getNCols(); jj++)
			{
				if( getData(ii,jj) < threshold )
					b.setData(true,ii,jj);
				else
					b.setData(false,ii,jj);
			}
		}

		return b;
	}

	/** Threshold the data and return a BinarySlice that is true 
	 *  where the Slice had data greater than the threshold and false
	 *  otherwise.
	 */
	public BinarySlice thresholdUpper(final double threshold)
	{
		BinarySlice b = new BinarySlice( getNRows(), getNCols() );

		for(int ii=0; ii<getNRows(); ii++)
		{
			for(int jj=0; jj<getNCols(); jj++)
			{
				if( getData(ii,jj) > threshold )
					b.setData(true,ii,jj);
				else
					b.setData(false,ii,jj);
			}
		}

		return b;
	}
	
	/** Apply a mask to the data in place, which means
	 *  to set all data to zero where the mask is false.
	 */
	public void applyMask( final BinarySlice s )
	{
		for(int ii=0; ii<getNRows(); ii++)
		{
			for(int jj=0; jj<getNCols(); jj++)
			{
				if( !s.getData(ii,jj) )
				{
					setData((float)0.0, ii,jj);
				}
			}
		}
	}

	public void show()
	{
		for(int ii=0; ii<nRows; ii++) {
			for(int jj=0; jj<nCols; jj++) {
				System.out.print(data[ii*nCols+jj] + " ");
			}
			System.out.println("");
		}
	}

	private void writeFloat(DataOutput ds)
	{
        try {
			for( int ii=0; ii<nRows*nCols; ii++) {
				ds.writeFloat(data[ii]);
			}
		}
        catch( IOException e ) {
            System.out.println("Volume: Could not write the MRI data.");
            System.exit(0);
        }
	}

	private void writeUShort(DataOutput ds)
	{
		final int UShortMax = (int)Short.MAX_VALUE - (int)Short.MIN_VALUE;

        try {
			for( int ii=0; ii<nRows*nCols; ii++) {
                if (data[ii] < 0)
                  ds.writeShort(0);
                else if (data[ii] > UShortMax)
                  ds.writeShort(-1); // Unsigned max cast as signed
				else
				{
					final int t = (int)data[ii];

					if (t > Short.MAX_VALUE)
				  		ds.writeShort( t - UShortMax - 1 );
					else
                  		ds.writeShort(t);
				}
			}
		}
        catch( IOException e ) {
            System.out.println("Volume: Could not write the MRI data.");
            System.exit(0);
        }
	}

	private void writeShort(DataOutput ds)
	{
        try {
			for( int ii=0; ii<nRows*nCols; ii++) {
                if (data[ii] > Short.MAX_VALUE) 
				{
                  ds.writeShort(Short.MAX_VALUE);
				}
				else if (data[ii] < Short.MIN_VALUE) 
				{
                  ds.writeShort(Short.MIN_VALUE);
				}
				else
				{
                  ds.writeShort((int)data[ii]);
				}
			}
		}
        catch( IOException e ) {
            System.out.println("Volume: Could not write the MRI data.");
            System.exit(0);
        }
	}

	final boolean colInBounds(final int col) 
	{
		if( col >= 0 && col < nCols ) {
			return true;
		}
		else {
			return false;
		}
	}

	final boolean rowInBounds(final int row) {
		if( row >= 0 && row < nRows ) {
			return true;
		}
		else {
			return false;
		}
	}

}
