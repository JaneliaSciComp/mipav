// THIS CODE DOES NOT YET HANDLE THE BUG IN ROI FILE FORMAT!

package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import java.lang.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import java.io.*;
import java.util.zip.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;

public class ROIList001 extends RegionList 
{

	private ROIList001()
	{
	}

	public ROIList001(String filename, VoxelInfo vi)
	{
		setVoxelInfo(vi);
		this.read(filename);
	}

	public void read(String filename)
	{
		ROI001 tempROI;
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
			String s = ldis.readLine();
			if (!s.equals("FileFormat: 0.0.1"))
			{
				System.err.println("ROIList: Unrecognised file format " + s);
			}
		}
		catch( IOException e) {
			System.err.println("ROIList: Could not read from " + filename);
		}

		try
		{
			while(true)
			{
				tempROI = new ROI001();
				tempROI.read(ldis);
				//tempROI.write(System.out);
				add(tempROI);
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

		ps.println("FileFormat: 0.0.1");

		ROI001 r;

		try {
			for(int i=0;i<size();i++)
				{ r = get(i); r.write(ps); }
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

	final public double getData(int row, int col, int slice)
	{
		System.out.println("not implemented yet");
		return 0.0;
	}

	/**
	 *  Retrieve the volume defined by this guy.
	 */
	final public Volume getVolume()
	{
		Volume mcv = new Volume(xDimension, yDimension, zDimension);

		ROI001[] rois = findAllGrown("les");

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

	final private double getDistanceSq(ROI001 r, int x, int y, int z)
	{
		r.getPOI(0,mypoi);    double t, t1;
		t1 = (mypoi.x - x)*xDimension; t = t1*t1;
		t1 = (mypoi.y - y)*yDimension; t += t1*t1;
		t1 = (mypoi.z - z)*zDimension; t += t1*t1;
		return t;
	}

	final public ROI001[] findAllWithComment(String tissue, String comment)
	{
		Vector v = new Vector(); ROI001 r;
		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (r.isGrown() && r.getTissueCode().equals(tissue)
               && r.getComments().equals(comment)) v.add(r);
		}
		ROI001[] R = new ROI001[v.size()];
		v.toArray(R); return R;
	}

	final public ROI001[] findAllContaining(String tissue, int x, int y, int z)
	{
		Vector v = new Vector(); ROI001 r;
		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (r.isGrown() && r.getTissueCode().equals(tissue)
               && r.contains(x,y,z)) v.add(r);
		}
		ROI001[] R = new ROI001[v.size()];
		v.toArray(R); return R;
	}

	/**
	 *  Find all the ROIs that are of type "tissue" and that
	 *  were grown.
	 */
	final public ROI001[] findAllGrown(String tissue)
	{
		Vector v = new Vector(); ROI001 r;
		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (r.isGrown() && r.getTissueCode().equals(tissue)) v.add(r);
		}
		ROI001[] R = new ROI001[v.size()];
		v.toArray(R); return R;
	}

	final public ROI001[] findAllUser(String tissue)
	{
		Vector v = new Vector(); ROI001 r;

		for(int i=0; i<size(); i++)
		{
			r = get(i);
			if (!r.isGrown() && r.getTissueCode().equals(tissue))
				v.add(r);
		}
		ROI001[] R = new ROI001[v.size()];
		v.toArray(R); return R;
	}

	final public ROI001 findClosest(int x, int y, int z, String tissue)
	{
		ROI001 r = null, r2; double t,m=Double.MAX_VALUE;

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

//	final public void setDimensions(double xin, double yin, double zin)
//	{
//		xDimension = xin;
//		yDimension = yin;
//		zDimension = zin;
//	}

	final public void add(ROI001 roi) {
		rois.add( roi );
	}

	final public ROI001[] get() {
		return (ROI001[]) rois.toArray(new ROI001[ rois.size() ]);
	}

	/**
	 *  This will create an MCVolume from all the grown
	 *  "tissue" tissues.
	 */
	final public MCVolume getMCVolume(String tissue)
	{
		MCVolume mcv = new MCVolume(256, 256, 24, 1);

		ROI001[] rois = findAllGrown(tissue);

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

	final public ROI001 get(int i) { return (ROI001)rois.get(i); }

	final public int size() { return rois.size(); }

//	double xDimension = 1.0;
//	double yDimension = 1.0;
//	double zDimension = 5.0;

	private static POI mypoi = new POI();
	private Vector rois = new Vector();

public static void main(String[] argv) throws IOException
{
	ROIList001 rl = new ROIList001();
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
	ROI001 r = rl.findClosest(100,100,10,"les");
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
