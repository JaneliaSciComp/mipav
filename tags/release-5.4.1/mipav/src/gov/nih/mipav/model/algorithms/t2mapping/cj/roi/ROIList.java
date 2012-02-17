package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import java.lang.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import java.io.*;
import java.util.zip.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;

public class ROIList extends RegionList 
{

	public ROIList()
	{
	}

	public ROIList(String filename)
	{
		this.read(filename);
	}

	/**
	 *  Read in the ROI list.
	 */
	public void read(String filename)
	{
		ROI tempROI = null;
        LEDataInputStream ldis;
        String line;

		clear();

		// Open the file.
		ldis = AppKit.openInput(filename);

        if (ldis == null) {
            System.out.println("ROIList: Could not open " + filename + " to read.");
            System.exit(0);
        }

        try {
            line = ldis.readLine();

            if (line==null) throw new EOFException("ROI header line");

            if( !line.startsWith("#<<<<<<") )
            {
                System.out.println("ROIList: Improper header: "+line);
	            System.exit(-1);
            }
        }
        catch (IOException e) {
            System.out.println("ROIList: Could not read in the header line.");
            System.out.println(e);
            System.exit(0);
        }

		try
		{
			while(true)
			{
				tempROI = new ROI(ldis);
				add(tempROI);
			}

		//  Read in the ROIs.
		}
		catch( EOFException e )
		{
//			System.out.println("ROIList: Done reading in the file.");
		}
		catch( IOException e )
		{
			System.out.println("ROIList: Could not read in " + filename);
			System.exit(-1);
		}

		// Now set the {x,y,z}Size {x,y,z}Dimension
		xDimension = tempROI.getNRows();
		yDimension = tempROI.getNCols();
		zDimension = tempROI.getNSlices();

		xSize = tempROI.getXDim();
		ySize = tempROI.getYDim();
		zSize = tempROI.getZDim();

	}

	public void write(String filename)
	{
		LEDataOutputStream ldos = null;

		ldos = AppKit.openOutput(filename);
		if (ldos == null)
		{
			System.err.println("ROIList: couldn't open "+filename+" for writing.");
			System.exit(-1);
		}

		PrintStream ps = new PrintStream(ldos);

		ROI r;

		try {
			for(int i=0;i<size();i++)
				{ r = get(i); r.write(ps); }
			ps.println("#<<<<<<<REOF>>>>>>>#");
			ps.flush();
			ps.close();
		}
		catch (IOException e)
			{ System.err.println("ROIList: couldn't write "+filename); }
	}

	final public void clear()
	{
		rois.clear();
	}

	final private double getDistanceSq(ROI r, int x, int y, int z)
	{
		r.getPOI(0,mypoi);    double t, t1;
		t1 = (mypoi.x - x)*xdim; t = t1*t1;
		t1 = (mypoi.y - y)*ydim; t += t1*t1;
		t1 = (mypoi.z - z)*zdim; t += t1*t1;
		return t;
	}

	public double getVoxelVolume()
	{
		ROI r = (ROI)rois.get(0);
		return r.getZDim()*r.getYDim()*r.getXDim();
	}

	/** Retrieve the total volume of the regions. */
	public double getTotalVolume()
	{ 
		return getTotalArea()*((ROI)rois.get(0)).getZDim();
	}

	/** Retrieve the total area of the regions. */
	public double getTotalArea()
	{ 
		Volume v = getVolume();
        double counter = Statistics.sum(v);

		return counter * ((ROI)rois.get(0)).getXDim()*((ROI)rois.get(0)).getYDim(); 
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
					if( v.getData(row,col,slice) > 0.0 ) counter++;
				}
			}
		}

		return counter; 
	}

	final public ROI[] findAllWithComment(String tissue, String comment)
	{
		Vector v = new Vector(); ROI r;
		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (r.isGrown() && r.getTissueCode().equals(tissue)
               && r.getComments().equals(comment)) v.add(r);
		}
		ROI[] R = new ROI[v.size()];
		v.toArray(R); return R;
	}

	final public ROI[] findAllContaining(String tissue, int x, int y, int z)
	{
		Vector v = new Vector(); ROI r;
		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (r.isGrown() && r.getTissueCode().equals(tissue)
               && r.contains(x,y,z)) v.add(r);
		}
		ROI[] R = new ROI[v.size()];
		v.toArray(R); return R;
	}

	/**
	 *  Find all the ROIs that are of type "tissue" and that
	 *  were grown.
	 */
	final public double getData(int row, int col, int slice)
	{
		double toreturn = 0.0;

		ROI r;
		for(int i=0; i<size(); i++)
		{
			r = (ROI)get(i);
			toreturn = Math.max(r.getData(row,col,slice), toreturn);
		}

		return toreturn;
	}

	/**
	 *  Find all the ROIs that are of type "tissue" and that
	 *  were grown and not deleted.
	 */
	final public ROI[] findAllGrown(String tissue)
	{
		Vector v = new Vector(); ROI r;
		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (!r.isDeleted() && r.isGrown() && r.getTissueCode().equals(tissue)) 
			{
				v.add(r);
			}
		}

		ROI[] R = new ROI[v.size()];
		v.toArray(R); 
		
		return R;
	}

	final public ROI[] findAllUser(String tissue)
	{
		Vector v = new Vector(); ROI r;

		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (!r.isGrown() && r.getTissueCode().equals(tissue))
				v.add(r);
		}
		ROI[] R = new ROI[v.size()];
		v.toArray(R); return R;
	}

	final public ROI findClosest(int x, int y, int z, String tissue)
	{
		ROI r = null, r2; double t,m=Double.MAX_VALUE;

		for(int i=0; i<size(); i++)
		{
			r2 = get(i);
			if (r2.isGrown()) continue;
			if (!r2.getTissueCode().equals(tissue)) continue;

			t = getDistanceSq(r2,x,y,z);
			if (t < m) { m=t; r=r2; }
		}

		return r;
	}

	final public void setDimensions(double xin, double yin, double zin)
	{
		xdim = xin;
		ydim = yin;
		zdim = zin;
	}

	final public void add(ROI roi) {
		rois.add( roi );
		hm.put(roi.getIdentifier(), roi);
	}

	final public ROI[] get() {
		return (ROI[]) rois.toArray(new ROI[ rois.size() ]);
	}

	final public ROI getByID(String id)	{
		Set s = hm.keySet();
		Object[] S = s.toArray();

		ROI r = (ROI)hm.get(id);
		return r;
	}

	final public Volume getVolume(int nrows, int ncols, int nslices)
	{
		return getVolume();
	}

	/**
	 *  This will create an MCVolume from all the grown
	 *  "tissue" tissues.
	 */
	final public Volume getVolume()
	{
		int nrows = ((ROI)rois.get(0)).getNRows();
		int ncols = ((ROI)rois.get(0)).getNCols();
		int nslices = ((ROI)rois.get(0)).getNSlices();

		Volume mcv = new Volume(nrows, ncols, nslices);

		ROI[] rois = findAllGrown("les");

		for(int ii=0; ii<rois.length; ii++)
		{
			POI[] pois = rois[ii].getPOI();

			for(int jj=0; jj<pois.length; jj++)
			{
				if( mcv.getData(pois[jj].y,pois[jj].x,pois[jj].z) < pois[jj].f)
				{
					mcv.setData(pois[jj].f, pois[jj].y,pois[jj].x,pois[jj].z);
				}
			}
		}

		return mcv;
	}

	/**
	 *  This will create an MCVolume from all the grown
	 *  "tissue" tissues.
	 */
	final public MCVolume getMCVolume(String tissue)
	{
		System.out.println("ROIList: getMCVolume is defined as 256 256");
		MCVolume mcv = new MCVolume(256, 256, 24, 1);

		ROI[] rois = findAllGrown(tissue);

		for(int ii=0; ii<rois.length; ii++)
		{
			POI[] pois = rois[ii].getPOI();

			for(int jj=0; jj<pois.length; jj++)
			{
				if( mcv.getData(pois[jj].y,pois[jj].x,pois[jj].z, 0) < pois[jj].f)
				{
					mcv.setData(100.0*pois[jj].f, pois[jj].y,pois[jj].x,pois[jj].z, 0);
				}
			}
		}

		return mcv;
	}

	final public ROI get(int i) { return (ROI)rois.get(i); }

	final public int size() { return rois.size(); }

	double xdim = 1.0;
	double ydim = 1.0;
	double zdim = 5.0;

	private HashMap hm = new HashMap();
	private static POI mypoi = new POI();
	private Vector rois = new Vector();

public static void main(String[] argv) throws IOException
{
	ROIList rl = new ROIList();
	Runtime rt = Runtime.getRuntime();

	for(int ii=0;ii<argv.length;ii++) {
	System.err.println(argv[ii]);
	rl.read(argv[ii]);
//    System.err.println("Total Mem = "+rt.totalMemory());
//    System.err.println("Free Mem =  "+rt.freeMemory());
//    System.err.println("Used Mem =  "+(rt.totalMemory()-rt.freeMemory()));
//    rt.gc();
    System.err.println("----");
    System.err.println("Total Mem = "+rt.totalMemory());
    System.err.println("Free Mem =  "+rt.freeMemory());
    System.err.println("Used Mem =  "+(rt.totalMemory()-rt.freeMemory()));
	//for(int i=0;i<rl.size();i++) rl.get(i).write(System.out);
	ROI r = rl.findClosest(100,100,10,"les");
	if (r!=null) r.write(System.out);
	r = rl.findClosest(100,100,10,"gm");
	if (r!=null) r.write(System.out);
	r = rl.findClosest(100,100,10,"wm");
	if (r!=null) r.write(System.out);
	r = rl.findClosest(100,100,10,"csf");
	if (r!=null) r.write(System.out);
	rl.write("/tmp/foo.roi");
	}
}

}
