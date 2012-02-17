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
public class SliceFile extends Slice
{
	FloatBuffer buf = null;

	public SliceFile(int nrows_in, int ncols_in, FloatBuffer fb) 
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
			throw new ArrayIndexOutOfBoundsException("SliceFile::getRow");
		}

		float[] tempcol = new float[ nCols ];
		buf.position(nCols*row);
		buf.get(tempcol,0,nCols);

		return tempcol;
	}

	public void getRow(float[] t, int row)
	{
		if( !rowInBounds(row) || t == null || t.length < nCols ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::getRow");
		}

		buf.position(nCols*row);
		buf.get(t,0,nCols);
	}

	/**
	 *  Set the row of data.
	 */
	public void setRow(final float[] newrow, final int row)
	{
		if( !rowInBounds(row) || newrow == null || newrow.length != nCols ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::setRow");
		}

		buf.position(nCols*row);
		buf.put(newrow,0,nCols);
	}

	/**
	 *  Get the column of data.
	 */
	public float[] getCol(final int col)
	{
		if( !colInBounds(col)  ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::getCol");
		}

		float[] tempcol = new float[ nRows ];

		for(int ii=0; ii<nRows; ii++) tempcol[ii] = buf.get(ii*nRows + col);

		return tempcol;
	}

	public void getCol(float[] t, final int col)
	{
		if( !colInBounds(col) || t.length < nRows ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::getCol");
		}

		for(int ii=0; ii<nRows; ii++) 
		{
			t[ii] = buf.get(ii*nRows + col);
		}
	}
	/**
	 *  Set the column of data.
	 */
	public void setCol(final float[] tempcol, final int col)
	{
		if( !colInBounds(col) || tempcol == null || tempcol.length != nRows ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::setCol");
		}

		for(int ii=0; ii<nRows; ii++) 
		{
			buf.put(ii*nRows + col, tempcol[ii]);
		}
	}

	/**
	 *  Get the data element.
	 */
	public float getData(final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::getData");
		}

		return buf.get(row*nCols + col);
	}

	/**
	 *  Set the data element.
	 */
	public void setData(final float newval, final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::setData");
		}

		buf.put(row*nCols + col, newval);
	}

	/**
	 *  Set the data element.
	 */
	public void setData(final double newval, final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("SliceFile::setData");
		}

		buf.put(row*nCols + col, (float)newval);
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
			System.out.println("SliceFile readDI  Ushort");
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put(ds.readUnsignedShort());
				}
			}
			else if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((float)ds.readDouble());
				}
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put(ds.readFloat());
				}
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
			System.out.println("SliceFile readDI  short");
				for( int ii=0; ii<sliceSize; ii++) {
					buf.put(ds.readShort());
				}
			}
			else {
				System.out.println("SliceFile::read: Could not figure out type " + datatype);
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
			if( datatype == MRIFile.DATATYPE_BYTE ) {
				byte[] temp = new byte[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put(Unsigned.floatFromUByte(temp[ii]));
				}
			}
			else if( datatype == MRIFile.DATATYPE_USHORT ) {
				short[] temp = new short[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put(Unsigned.floatFromUShort(temp[ii]));
				}
			}
			else if( datatype == MRIFile.DATATYPE_DOUBLE ) {
				double[] temp = new double[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put((float)temp[ii]);
				}
			}
			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
				float[] temp = new float[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put(temp[ii]);
				}
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
				short[] temp = new short[sliceSize];
				ds.readFully(temp, 0, sliceSize);

				for( int ii=0; ii<sliceSize; ii++) {
					buf.put(temp[ii]);
				}
			}
			else {
				System.out.println("SliceFile::readFast: Could not figure out type.");
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("SliceFile: Could not read in the MRI data.");
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
					dos.writeFloat(buf.get(ii));
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
				float[] temp = new float[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = buf.get(ii);
				}
				ios.writeFloats(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_USHORT ) {
				short[] temp = new short[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = Unsigned.toUShort(buf.get(ii));
				}
				ios.writeShorts(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_BYTE ) {
				byte[] temp = new byte[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = (byte)Unsigned.toUByte(buf.get(ii));
				}
				ios.write(temp, 0, sliceSize);
			}
			else if( datatype == MRIFile.DATATYPE_SHORT ) {
				short[] temp = new short[sliceSize];
				for( int ii=0; ii<sliceSize; ii++) {
					temp[ii] = (short)buf.get(ii);
				}
				ios.writeShorts(temp, 0, sliceSize);
			}
			else {
				System.out.println("SliceFile::write: Could not figure out type.");
				System.exit(0);
			}
		}
        catch( IOException e ) {
            System.out.println("SliceFile:write: Could not write the MRI data.");
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

	private void writeFloat(DataOutput ds)
	{
		buf.position(0);
        try {
			for( int ii=0; ii<nRows*nCols; ii++) {
				ds.writeFloat(buf.get());
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
