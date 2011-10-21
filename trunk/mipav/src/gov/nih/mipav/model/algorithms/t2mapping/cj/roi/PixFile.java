package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.*;
import java.util.*;
import java.io.*;

/**  The PixFile class is a very simple file format that stores
 *   indices of an ROI in row-major format.
 *
 *   It is not meant to be very full and all the real information should
 *   be stored in the filename.
 */
public class PixFile
{
	private int[] indices;
	private int nrows=256, ncols=256;

	/** 
	 *  Constructor, set the number of rows and columns
	 *  which are needed for reading in teh pxiel file
	 *  properly.
	 */
	public PixFile(final int nrows, final int ncols)
	{
		this.nrows = nrows;
		this.ncols = ncols;
	}

	/**
	 *  Read in the pixel file.  All white space will be trimmed.
	 *
	 */
	public void read(String filename)
	{
        LEDataInputStream ldis;
        String line;
		Vector temp = new Vector();

		// Open the file.
		ldis = AppKit.openInput(filename);

        if (ldis == null) {
            System.out.println("ROIList: Could not open " + filename + " to read.");
            System.exit(0);
        }

		try
		{
			line = "";
			while(line != null)
			{
				line = ldis.readLine();

				if( line != null && line.length() > 0 )
				{
					temp.add( new Integer(line.trim()));
				}
			}
		}
		catch( EOFException e )
		{
			System.out.println("ROIList: Done reading in the file.");
		}
		catch( IOException e )
		{
			System.err.println("ROIList: Problem reading in the file.");
			System.exit(-1);
		}

		/*
		 *  This SHOULD be correct, but there might be
		 *  an out-by-one error here.
		 */
		indices = new int[ temp.size() ];
		for(int ii=0; ii<indices.length; ii++)
		{
			int tt = ((Integer)temp.get(ii)).intValue();
			int r = tt%256;
			int c = tt/256;
			indices[ii] = r*nrows+256-c;
		}
	}

	public void write(String filename)
	{

	}

	/**
	 *  Get a copy of the indices.
	 */
	public int[] getIndices()
	{
		return (int[])indices.clone();
	}

	/**
	 *  Create a binary slice out of the indices listed.
	 */
	public BinarySlice getBinarySlice(final int nrows, final int ncols)
	{
		BinarySlice bs = new BinarySlice( nrows, ncols );

		for(int ii=0; ii<indices.length; ii++) 
		{
			bs.setData( true, ii/nrows, ii%nrows);
		}

		return bs;
	}
}
