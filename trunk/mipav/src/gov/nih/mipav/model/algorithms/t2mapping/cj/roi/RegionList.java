package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import java.io.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;

public abstract class RegionList
{
	/** Dimensions in the X direction. */
	protected int xDimension = -1;

	/** Dimensions in the Y direction. */
	protected int yDimension = -1;

	/** Dimensions in the Z direction. */
	protected int zDimension = -1;

	/** Size of the pixel in the X direction (in mm). */
	protected double xSize = -1.0;

	/** Size of the pixel in the Y direction (in mm). */
	protected double ySize = -1.0;

	/** Size of the pixel in the Z direction (in mm). */
	protected double zSize = -1.0;

	public RegionList() 
	{ 
	}

	abstract public Volume getVolume();

	abstract public double getData(int x, int y, int z);

	abstract public void read(String filename);

	/** Get the number of pixels in the X direction. */
	public int getXDimension() { return xDimension; }

	/** Get the number of pixels in the Y direction. */
	public int getYDimension() { return yDimension; }

	/** Get the number of pixels in the Z direction. */
	public int getZDimension() { return zDimension; }

	/** Set the number of pixels in the X direction. */
	public void setXDimension(int xDimension) { this.xDimension = xDimension; }

	/** Set the number of pixels in the Y direction. */
	public void setYDimension(int yDimension) { this.yDimension = yDimension; }

	/** Set the number of pixels in the Z direction. */
	public void setZDimension(int zDimension) { this.zDimension = zDimension; }

	/** Get the size of the pixel in the X direction. */
	public double getXSize() { return xSize; }

	/** Get the size of the pixel in the Y direction. */
	public double getYSize() { return ySize; }

	/** Get the size of the pixel in the Z direction. */
	public double getZSize() { return zSize; }

	/** Set the size of the pixel in the X direction. */
	public void setXSize(double xSize) { this.xSize = xSize; }

	/** Set the size of the pixel in the Y direction. */
	public void setYSize(double ySize) { this.ySize = ySize; }

	/** Set the size of the pixel in the Z direction. */
	public void setZSize(double zSize) { this.zSize = zSize; }

	public double getVoxelVolume() { return xSize * ySize * zSize; }

	/** Retrieve the total volume of the regions. */
	public double getTotalVolume() 
	{ 
		return getTotalArea()*zSize;
	}

	/** Retrieve the total area of the regions. */
	public double getTotalArea() 
	{
		double counter = 0.0;

        Volume v = getVolume();

        for(int row=0; row<v.getNRows(); row++)
        {
            for(int col=0; col<v.getNCols(); col++)
            {
                for(int slice=0; slice<v.getNSlices(); slice++)
                {
                    counter+=v.getData(row,col,slice);
                }
            }
        }

        return counter * xSize * ySize;
    }


	/** Retrieve the total number of pixels of the regions. */
	public double getTotalPixels() 
	{
		double counter = 0.0;

        Volume v = getVolume();

        for(int row=0; row<v.getNRows(); row++)
        {
            for(int col=0; col<v.getNCols(); col++)
            {
                for(int slice=0; slice<v.getNSlices(); slice++)
                {
                    if (v.getData(row,col,slice)>0) counter++;
                }
            }
        }

        return counter;
    }

	public void setVoxelInfo(VoxelInfo vi)
	{
		setXDimension(vi.xDimension);
		setYDimension(vi.yDimension);
		setZDimension(vi.zDimension);

		setXSize(vi.xSize);
		setYSize(vi.ySize);
		setZSize(vi.zSize);
	}

	public VoxelInfo getVoxelInfo() 
	{ 
		VoxelInfo vi = new VoxelInfo();

		vi.xDimension = getXDimension();
		vi.yDimension = getYDimension();
		vi.zDimension = getZDimension();

		vi.xSize = getXSize();
		vi.ySize = getYSize();
		vi.zSize = getZSize();

		return vi; 
	}

	public static RegionList readRegionList( String filename, VoxelInfo vi )
	{
		long start = 0;
		long end = 0;
		String line = "";

		if( filename.endsWith(".les") )
		{
			// Is an LES file...
			return (new LESList(filename, vi));
		}
		else
		{
			try
			{
				LEDataInputStream ldis = AppKit.openInput( filename );
				line = ldis.readLine();
				ldis.close();
			}
			catch( IOException e)
			{
				System.out.println("RegionList: Could not open " + filename );
				System.exit(-1);
			}

			if( line.startsWith("FileFormat") )
			{
				return (new ROIList001(filename, vi));
			}
			else if( line.startsWith("#<<<<") )
			{
				return (new ROIList(filename));
			}
			else
			{
				System.out.println("RegionList: Unknown ROI file type " + filename);
				System.exit(-1);
			}
		}

		return null;
	}
}
