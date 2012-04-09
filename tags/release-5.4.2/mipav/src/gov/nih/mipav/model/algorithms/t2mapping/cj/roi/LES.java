package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import java.io.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class LES extends Region
{
	private int nrows = 0;
	private int ncols = 0;
	
	public LES( LEDataInputStream ldis ) throws IOException
	{
		read(ldis);
	}
	
	public void setSize(int nrows, int ncols )
	{
		this.nrows = nrows;
		this.ncols = ncols;
	}

	public void read( LEDataInputStream ldis ) throws IOException
	{
		image_file_name_length = ldis.readShort();
		image = ldis.readShort();
		roi_name_length = ldis.readShort();
		area = ldis.readFloat();
		comment_length = ldis.readShort();
		date_length = ldis.readShort();
		center_x = ldis.readFloat();
		center_y = ldis.readFloat();
		area_polygon = ldis.readFloat();
		partial_volume = ldis.readFloat();
		intensity_mean = ldis.readFloat();
		intensity_std = ldis.readFloat();
		screen_pixel_x = ldis.readFloat();
		screen_pixel_y = ldis.readFloat();
		slice_thickness  = ldis.readFloat();
		expand = ldis.readFloat();
		border_x = (short)(ldis.readShort()+1);
		border_y = (short)(ldis.readShort()+1);
		chain_length = ldis.readShort();
		method = ldis.readShort();
		model_type = ldis.readShort();
		slide_ul = ldis.readShort();
		slide_ll = ldis.readShort();

		byte[] temp = new byte[1000];
		ldis.read(temp, 0, image_file_name_length);
		image_file_name = new String(temp, 0, image_file_name_length);

		ldis.read(temp, 0, date_length);
		roi_date = new String(temp, 0, date_length);

		ldis.read(temp, 0, roi_name_length);
		roi_name = new String(temp, 0, roi_name_length);

		ldis.read(temp, 0, comment_length);
		roi_comments = new String(temp, 0, comment_length);

		chain = new byte[chain_length];
		for(int ii=0; ii<chain_length; ii++)
		{
			chain[ii] = (byte)ldis.readByte();
		}
	}

	BinarySlice getBinarySlice()
	{
		if( nrows == 0 || ncols == 0 )
		{
			System.out.println("les: Must set the number of rows and columns first.");
			System.exit(-1);
		}

		BinarySlice bs = new BinarySlice(512, 512);

		int loc_x = border_x-1;
		int loc_y = border_y-1;
		bs.setData(true, loc_y, loc_x);

		for(int ii=0; ii<chain_length; ii++)
		{
			for(int jj=0; jj<2; jj++)
			{
				int dir = 0;

				if( jj == 1 )
				{
					dir = chain[ii] & 15;
				}
				else
				{
					dir = (chain[ii]>>4) & 15;
				}

				switch( dir )
				{
					case 0:
						loc_x = loc_x + 1;
					break;
					case 1:
						loc_x = loc_x + 1;
						loc_y = loc_y - 1;
					break;
					case 2:
						loc_y = loc_y - 1;
					break;
					case 3:
						loc_x = loc_x - 1;
						loc_y = loc_y - 1;
					break;
					case 4:
						loc_x = loc_x - 1;
					break;
					case 5:
						loc_x = loc_x - 1;
						loc_y = loc_y + 1;
					break;
					case 6:
						loc_y = loc_y + 1;
					break;
					case 7:
						loc_x = loc_x + 1;
						loc_y = loc_y + 1;
					break;
				}

				bs.setData(true, loc_y, loc_x);
			}
		}

		BinarySlice outline = (BinarySlice)bs.clone();
		bs.fillAt(true, true, border_y-1, border_x);
		//bs.fillHoles(true,true);

		outline.not();
		bs.and( outline );

		return bs;
	}

	/** 
	 *  This should return the floating point value
	 *  for this LES file given the row, column and slice.
	 */
	public double getData(int row, int col, int slice)
	{
		double toreturn = 0.0;

		if( slice != image - 1 )
		{
			toreturn = 0.0;
		}
		else
		{
			BinarySlice bs = getBinarySlice();
			if( bs.getData(row,col) ) toreturn = 1.0;
		}

		return toreturn;
	}

	public POI[] getPOI()
	{
		BinarySlice bs = getBinarySlice();

		POI[] pois = new POI[ bs.numberTrue() ];
		int ii=0;
		for(int row=0; row<bs.getNRows(); row++)
		{
			for(int col=0; col<bs.getNCols(); col++)
			{
				if( bs.getData(row,col) )
				{
					pois[ii] = new POI( (short)((double)row),
					  (short)((double)col), (short)(image-1), 1.0f);
					ii++;
				}
			}
		}

		return pois;
	}

	public short getImageNumber() { return image; }

	public double getSliceThickness()
	{
		return (double)slice_thickness;
	}

	short image_file_name_length ;
	short image ;
	short roi_name_length ;
	float area ;
	short comment_length ;
	short date_length ;
	float center_x ;
	float center_y ;
	float area_polygon ;
	float partial_volume ;
	float intensity_mean ;
	float intensity_std ;
	float screen_pixel_x ;
	float screen_pixel_y ;
	float slice_thickness  ;
	float expand ;
	short border_x ;
	short border_y ;
	short chain_length ;
	short method ;
	short model_type ;
	short slide_ul ;
	short slide_ll ;

	String image_file_name = "";
	String roi_date = "";
	String roi_name = "";
	String roi_comments = "";
	byte[] chain = null;

}
