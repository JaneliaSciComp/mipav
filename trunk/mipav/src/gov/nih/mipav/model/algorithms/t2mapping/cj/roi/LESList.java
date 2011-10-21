package gov.nih.mipav.model.algorithms.t2mapping.cj.roi; 

import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import java.util.*;
import java.io.*;

/**
 *  LESList is a class that will read in an LES file.  
 *  The primary thing necessary is to be able to do a getData
 *  and getVolume.
 *
 *  Now the stupid part... The LES file format does not contain
 *  the size of the voxel nor the size of the volume, so that
 *  is why LESList must have a constructor that takes in 
 *  the necessary information.
 */
public class LESList extends RegionList
{
	/**
	 *  THIS IS MEANT TO BE PRIVATE AS NO ONE SHOULD
	 *  ACTUALLY USE IT!
	 */ 
	private LESList()
	{
		//leslist = new Vector();
	}

	public LESList(VoxelInfo vi)
	{
		setVoxelInfo(vi);
	}

	public LESList(final String filename, VoxelInfo vi)
	{
		read(filename);

		setVoxelInfo(vi);
	}

	private LESList(String filename)
	{
		//read(filename);
	}

	public void read(String filename)
	{
		leslist = new Vector();

        LEDataInputStream ldis;
		ldis = AppKit.openInput(filename);

		try
		{
			read(ldis);
		}
		catch( IOException e)
		{
			System.out.println("LESList: problem reading file " + filename);
			System.exit(-1);
		}
	}

	public void read( LEDataInputStream ldis ) throws IOException
	{

        try {
			// First read in the header part.
			byte[] temp = new byte[1000];

			ldis.read(temp, 0, 20);
			id = new String(temp, 0, 20);

			version = ldis.readShort();

			sub_version = ldis.readShort();
			length_creation_date = ldis.readShort();

			ldis.read(temp, 0, length_creation_date);
			creation_date = new String(temp, 0, length_creation_date);

			length_patient_name = ldis.readShort();

			ldis.read(temp, 0, length_patient_name);
			patient_name = new String(temp, 0, length_patient_name);

			length_operator_name = ldis.readShort();

			ldis.read(temp, 0, length_operator_name);
			operator_name = new String(temp, 0, length_operator_name);

			length_scan_date= ldis.readShort();

			ldis.read(temp, 0, length_scan_date);
			scan_date = new String(temp, 0, length_scan_date);



			// Now read in each LES
            do {
				leslist.add( new LES(ldis) );
			} while (true);
        }
		catch( EOFException e )
		{
		}
        catch( IOException e ) 
		{
			throw(e);
        }
	}

	/** Retrieve the total volume of the regions. */
	public double getTotalVolume() {
		if (leslist.size() == 0) return 0.0;
		LES l = (LES)leslist.get(0);
		return l.slice_thickness * 1000 * getTotalArea();
	}

	/** Retrieve the total area of the regions. */
	public double getTotalArea() {
		double t=0.0;
		for(int i=0;i<leslist.size();i++)
		{
  		LES l = (LES)leslist.get(i);
  		t += l.area * 1000 * 1000;
		}
		return t;
	}

	/** Retrieve the total number of pixels of the regions. */
	public double getTotalPixels() { return 0.0; }


//	public MCVolume getMCVolume()
//	{
//		MCVolume mcv = new MCVolume(256, 256, 24, 1);
//		
//		for(int ii=0; ii<leslist.size(); ii++)
//		{
//			POI[] pois = ((LES)leslist.get(ii)).getPOI();	
//
//			for(int jj=0; jj<pois.length; jj++)
//			{
//				mcv.setData(100.0, pois[jj].y, pois[jj].x, pois[jj].z, 0);
//			}
//		}
//
//		System.out.println("leslist: getMCVOlume is not defined");
//		return new MCVolume();
//	}

	public double getData(int row, int col, int slice)
	{
//		for(int ii=0; ii<leslist.size(); ii++)
//		{
//			POI[] pois = ((LES)leslist.get(ii)).getPOI();	
//
//			for(int jj=0; jj<pois.length; jj++)
//			{
//				mcv.setData(100.0, pois[jj].y, pois[jj].x, pois[jj].z);
//			}
//		}
//
		return 0.0;
	}

	//public Volume getVolume(int nrows, int ncols, int nslices)
	public Volume getVolume()
	{
		Volume mcv = new Volume(512, 512, 30);
		float myRow[] = new float[512];
		
		for(int ii=0; ii<leslist.size(); ii++)
		{
			LES l = (LES)leslist.get(ii);
			l.setSize(512, 512);	

			BinarySlice bs = l.getBinarySlice();

			int slice = l.getImageNumber() - 1;
			Slice s = mcv.getSlice(slice);

			for(int row=0; row<512; row++)
			{
				s.getRow(myRow, row);
				boolean[] B = bs.getRow(row);
				for(int j=0;j<512;j++) if(B[j]) myRow[j]=1;
				s.setRow(myRow, row);
			}

			/* POI[] pois = ((LES)leslist.get(ii)).getPOI();	

			for(int jj=0; jj<pois.length; jj++)
			{
				mcv.setData(pois[jj].f, pois[jj].y, pois[jj].x, pois[jj].z);
			} */
		}

		return mcv;
	}

	String id = null;
	short version;
	short sub_version;
	short length_creation_date;
	String creation_date = null;
	short length_patient_name;
	String patient_name = null;
	short length_operator_name;
	String operator_name = null;
	short length_scan_date;
	String scan_date = null;

	Vector leslist = null;
}
