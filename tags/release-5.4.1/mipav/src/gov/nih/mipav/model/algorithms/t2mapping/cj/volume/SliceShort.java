package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import java.io.*;
import java.nio.*;
import java.util.Arrays;
import javax.imageio.stream.*;

/** 
 *  The slice class that stores data for one single image.
 *
 *  Key thought here is that the data is stored in ROW-MAJOR
 *  format as that is the way most of the MRI files store 
 *  their data.  Therefore, A(row,col) is at 
 *  data[ row*ncols + col].
 */
public class SliceShort extends Slice
{
	ShortBuffer buf = null;

	public SliceShort(int nrows_in, int ncols_in, ShortBuffer fb) 
	{
		super(); // Call alloc-free constructor
		nRows = nrows_in; nCols = ncols_in; buf = fb;
	}

	/** 
	 *  Cloner
	 *
	 */
	public Object clone()
	{
		return super.clone();
	}

	/**
	 *  Get the row of data.
	 */
	public float[] getRow(int row)
	{
		if( !rowInBounds(row)  ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::getRow");
		}

		short[] tempcol = new short[ nCols ];
		buf.position(nCols*row);
		buf.get(tempcol,0,nCols);

		float[] tt = new float[nCols];
		for(int ii=0; ii<nCols;ii++) { tt[ii] = (float)tempcol[ii]; }

		return tt;
	}

	public void getRow(float[] t, int row)
	{
		if( !rowInBounds(row) || t == null || t.length < nCols ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::getRow");
		}

		short[] tempcol = new short[ nCols ];

		buf.position(nCols*row);
		buf.get(tempcol,0,nCols);

		for(int ii=0; ii<nCols;ii++) { t[ii] = (float)tempcol[ii]; }
	}

	/**
	 *  Set the row of data.
	 */
	public void setRow(final float[] newrow, final int row)
	{
		if( !rowInBounds(row) || newrow == null || newrow.length != nCols ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::setRow");
		}

		short[] tempcol = new short[ nCols ];
		for(int ii=0; ii<nCols;ii++) { tempcol[ii] = (short)newrow[ii]; }

		buf.position(nCols*row);
		buf.put(tempcol,0,nCols);
	}

	/**
	 *  Get the column of data.
	 */
	public float[] getCol(final int col)
	{
		if( !colInBounds(col)  ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::getCol");
		}

		float[] tempcol = new float[ nRows ];

		for(int ii=0; ii<nRows; ii++) tempcol[ii] = (float)buf.get(ii*nRows + col);

		return tempcol;
	}

	public void getCol(float[] t, final int col)
	{
		if( !colInBounds(col) || t.length < nRows ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::getCol");
		}

		for(int ii=0; ii<nRows; ii++) 
		{
			t[ii] = (float)buf.get(ii*nRows + col);
		}
	}
	/**
	 *  Set the column of data.
	 */
	public void setCol(final float[] tempcol, final int col)
	{
		if( !colInBounds(col) || tempcol == null || tempcol.length != nRows ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::setCol");
		}

		for(int ii=0; ii<nRows; ii++) 
		{
			buf.put(ii*nRows + col, (short)tempcol[ii]);
		}
	}

	/**
	 *  Get the data element.
	 */
	public float getData(final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::getData");
		}

		return (float)buf.get(row*nCols + col);
	}

	/**
	 *  Set the data element.
	 */
	public void setData(final float newval, final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::setData");
		}

		buf.put(row*nCols + col, (short)newval);
	}

	/**
	 *  Set the data element.
	 */
	public void setData(final double newval, final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("SliceShort::setData");
		}

		buf.put(row*nCols + col, (short)newval);
	}

	/*=======================================================================
	 *
	 *    Reading and Writing.
	 *
	 *=======================================================================
	 */

	/** 
	 *
	 */
	protected void readDI(DataInput ds, final int datatype )
	{
        final int sliceSize = nRows*nCols;

		buf.position(0);
        try {
			if( datatype == MRIFile.DATATYPE_USHORT ) {
			System.out.println("SliceShort readDI  Ushort");
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)ds.readUnsignedShort());
				}
			}
			else if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)ds.readDouble());
				}
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)ds.readShort());
				}
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
			System.out.println("SliceShort readDI  short");
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)ds.readShort());
				}
			}
			else {
				System.out.println("SliceShort::read: Could not figure out type " + datatype);
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("Slice: Could not read in the MRI data.");
            System.out.println(e);
            System.exit(0);
        }
	}

	protected void readIIS(ImageInputStream ds, final int datatype )
	{
        final int sliceSize = nRows*nCols;

		buf.position(0);
        try {
			if( datatype == MRIFile.DATATYPE_USHORT ) {
				short[] temp = new short[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)Unsigned.floatFromUShort(temp[ii]));
				}
			}
			else if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				double[] temp = new double[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)temp[ii]);
				}
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				float[] temp = new float[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)temp[ii]);
				}
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
				short[] temp = new short[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((short)temp[ii]);
				}
			}
			else {
				System.out.println("SliceShort::readFast: Could not figure out type.");
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("SliceShort: Could not read in the MRI data.");
            System.out.println(e);
            System.exit(0);
        }
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
					dos.writeDouble((double)buf.get(ii));
				}
		}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					dos.writeShort(buf.get(ii));
				}
			}
			else if( datatype == MRIFile.DATATYPE_USHORT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					dos.writeShort(Unsigned.toUShort(buf.get(ii)));
				}
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					dos.writeShort((short)buf.get(ii));
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
	 *
	 */
	protected void writeIOS(ImageOutputStream ios, final int datatype )
	{
        final int sliceSize = nRows*nCols;

		buf.position(0);
        try {
			if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				double[] temp = new double[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = (double)buf.get(ii);
				}
				ios.writeDoubles(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				short[] temp = new short[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = (short)buf.get(ii);
				}
				ios.writeShorts(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_USHORT ) {
				short[] temp = new short[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = Unsigned.toUShort(buf.get(ii));
				}
				ios.writeShorts(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
				short[] temp = new short[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = (short)buf.get(ii);
				}
				ios.writeShorts(temp, 0, sliceSize);
			}
			else {
				System.out.println("SliceShort::write: Could not figure out type.");
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("SliceShort:write: Could not write the MRI data.");
            System.out.println(e);
            System.exit(0);
        }

		//try {  ios.flush(); } catch(Exception e ){ System.out.println(e); System.exit(-1); }
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
		for(int ii=0; ii<nRows; ii++)
		{
			for(int jj=0; jj<nCols; jj++)
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
		buf.position(0);
		for(int ii=0; ii<nRows; ii++) {
			for(int jj=0; jj<nCols; jj++) {
				System.out.print(buf.get() + " ");
			}
			System.out.println("");
		}
	}


	private void writeUShort(DataOutput ds)
	{
		final int UShortMax = (int)Short.MAX_VALUE - (int)Short.MIN_VALUE;

        try {
			for( int ii=0; ii<nRows*nCols; ii++) {
                if (buf.get(ii) < 0)
                  ds.writeShort(0);
                else if (buf.get(ii) > UShortMax)
                  ds.writeShort(-1); // Unsigned max cast as signed
				else
				{
					final int t = (int)buf.get(ii);

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
                if (buf.get(ii) > Short.MAX_VALUE) 
				{
                  ds.writeShort(Short.MAX_VALUE);
				}
				else if (buf.get(ii) < Short.MIN_VALUE) 
				{
                  ds.writeShort(Short.MIN_VALUE);
				}
				else
				{
                  ds.writeShort((int)buf.get(ii));
				}
			}
		}
        catch( IOException e ) {
            System.out.println("Volume: Could not write the MRI data.");
            System.exit(0);
        }
	}
}
