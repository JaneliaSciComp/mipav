// THIS CODE DOES NOT YET HANDLE THE BUG IN ROI FILE FORMAT!

package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import java.lang.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import java.io.*;
import java.util.zip.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;

public class BasisSet {
	public BasisSet(ROIList rl, String[] tl)
	{
		init(rl,tl,true);
	}

	public BasisSet(ROIList rl, String[] tl, boolean bySlice)
	{
		init(rl,tl,bySlice);
	}

	private void init(ROIList rl, String[] tl, boolean bySlice)
	{
		this.xdim = rl.xdim;
		this.ydim = rl.ydim;
		this.zdim = rl.zdim;
		this.rl = rl;
		this.tl = tl;

		this.PS = new PointSet[tl.length];

		for(int i=0;i<tl.length;i++)
		{
			if (bySlice) PS[i] = new SlicedPointSet(this);
			else PS[i] = new GlobalPointSet(this);
		}

		ROI[] R = rl.get();

		for(int j=0;j<R.length;j++)
		{
			if (!R[j].isGrown() && !R[j].isDeleted())
			{
				BasisPoint bp = new BasisPoint(R[j],j);
				int i = tissueToInt(R[j].getTissueCode());
				if (i >=0) PS[i].add(bp);
			}
		}
	}

	private int tissueToInt(String t)
	{
		for(int i=0;i<tl.length;i++) if (tl[i].equals(t)) return i;
		return -1;
	}

	final double getDistanceSq(BasisPoint p, int x, int y, int z)
	{
		double t, t1;
		t1 = (p.x - x)*xdim; t = t1*t1;
		t1 = (p.y - y)*ydim; t += t1*t1;
		t1 = (p.z - z)*zdim; t += t1*t1;
		return t;
	}

	final public ROI findClosest(int x, int y, int z, int tis)
	{
		BasisPoint b = PS[tis].findClosest(x,y,z);
		if (b==null) return null; else return rl.get(b.index);
	}

	private double xdim;
	private double ydim;
	private double zdim;

	private ROIList rl;
	private String[] tl;
	private PointSet[] PS;
}

class BasisPoint
{
	static POI p = new POI();

	BasisPoint(ROI r, int n)
	{
		r.getPOI(0,p);
		this.x = p.x; this.y = p.y; this.z = p.z;
		this.index = n;
	}

	public int x,y,z;
	public int index;
}

interface PointSet
{
	void add(BasisPoint b);
	BasisPoint findClosest(int x, int y, int z);
}
  
class SlicedPointSet implements PointSet
{
	private BasisSet parent;
	private ArrayList slices=new ArrayList();
	private boolean empty = true;

	SlicedPointSet(BasisSet my) { this.parent = my; }

	public void add(BasisPoint b)
	{
		empty = false;
		if (b.z >= slices.size())
		{
			for(int i=slices.size(); i<=b.z;i++)
				slices.add(null);
		}
		GlobalPointSet p = (GlobalPointSet) slices.get(b.z);

		if (p == null) 
			slices.set(b.z, p=new GlobalPointSet(parent));
		p.add(b);
	}

	public BasisPoint findClosest(int x, int y, int z)
	{
		if (empty) return null;

		if (z >= slices.size()) z = slices.size()-1;

		GlobalPointSet p = (GlobalPointSet) slices.get(z);

		if (p == null)
		{
			int i,j;
			for(i=z;i<slices.size();i++) if (slices.get(i)!=null) break;
			for(j=z;j>=0;j--) if (slices.get(j)!=null) break;

			if (j<0) z=i; else if (i>=slices.size()) z=j;
			else z = (z-j < i-z)? j : i;

			p = (GlobalPointSet) slices.get(z);
		}

		return p.findClosest(x,y,z);
	}
}

// Really no different from old PointSet
class GlobalPointSet implements PointSet
{
	private ArrayList v = new ArrayList();
	private boolean dirty = false;
	private BasisPoint[] cache = null;
	private BasisSet parent;

	GlobalPointSet(BasisSet my) { this.parent = my; }

	public void add(BasisPoint b)
	{
		v.add(b);
		dirty = true;
	}

	private BasisPoint[] get()
	{
		if (dirty)
		{
			cache = new BasisPoint[v.size()];
			v.toArray(cache);
			dirty = false;
		}
		return cache;
	}

	public BasisPoint findClosest(int x, int y, int z)
	{
		BasisPoint[] BP = get();
		BasisPoint b;

		double t, m = Double.MAX_VALUE;
		int i,j=-1;

		for(i=0; i<BP.length; i++)
		{
			t = parent.getDistanceSq(BP[i],x,y,z);
			if (t < m) { m=t; j=i; }
		}

		if (j<0) return null; else return BP[j];
	}
}
