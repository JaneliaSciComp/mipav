package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.convexp.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;

public class RegionGrow
{
	/** travel - the number of slices above and below the current slice
	             that the region growing is allowed to travel.  
				 A value of -1 means that it can grow any number of slices
				 above and below.
	 */
	private double threshold = 0.55;
	private double tol;
	private int travel = 0;
	private int num_points = 0;
	private int[] xQ = null, yQ = null, zQ = null;
	private int[] curr = new int[3];
	private int[] dims = new int[3];
	private double[][] A;
	private double[] b, result;
	private int numTissues, numScans;
	private MCVolume v;
	private ConvexP c;
	private ROIList rl;
	private Mask2D doneMask;
	private String[] tl;

	public RegionGrow(MCVolume v, ConvexP c, ROIList rl, String[] tissues)
	{
		numTissues = c.getNumTissues();
		numScans = c.getNumScans();

		if ( v.getNChannels() < numScans )
		{
			System.err.println("RegionGrow: MCVolume doesn't have enough channels to support ConvexP");
		}

		if ( tissues.length != numTissues )
		{
			System.err.println("RegionGrow: MCVolume doesn't have enough channels to support ConvexP");
		}

		xQ = new int[128];
		yQ = new int[128];
		zQ = new int[128];

		dims[0] = v.getNCols();
		dims[1] = v.getNRows();
		dims[2] = v.getNSlices();

		doneMask = new Mask2D(dims);

		b = new double[numScans];
		result = new double[numTissues];
		A = new double[numTissues][numScans];

		this.v = v;
		this.c = c;
		this.rl = rl;
		this.tl = tissues;

		/*NoiseEstimate ne = new NoiseEstimate();
		tol = 2 * ne.gradientMethod(v); */
        tol = 0;

		System.out.println("Using tol = " + tol);
		c.setTol(tol);
	}

	public void setThreshold(double t)
	{
		this.threshold = t;
	}

	public double getThreshold()
	{
		return threshold;
	}

	public void setTravel(int travel)
	{
		this.travel = travel;
	}

	public int getTravel()
	{
		return travel;
	}

	final private void push(int x, int y, int z)
	{
		xQ[num_points] = x;
		yQ[num_points] = y;
		zQ[num_points] = z;
		num_points++;
	}

	final private void push(int[] xyz)
	{
		push(xyz[0],xyz[1],xyz[2]);
	}

	final private boolean pop(int[] xyz)
	{
		if (num_points <= 0) return false;
		num_points--;
		xyz[0] = xQ[num_points];
		xyz[1] = yQ[num_points];
		xyz[2] = zQ[num_points];
		return true;
	}

	public ROI grow(int start_x, int start_y, int start_z, String type)
	{
		String comment = "";
		int target = -1;
		ROI roi = new ROI();
		POI p = new POI();

		roi.setDate();
		roi.setTissueCode(type);
		roi.setAlgorithm("segment:autogrow:Travel=" +travel+":RG_Thresh="+threshold+":tol="+tol);

		for(int i=0; i<numTissues; i++)
		{
			ROI r = rl.findClosest(start_x, start_y, start_z, tl[i]);
			if (type.equals(tl[i])) target = i;

			if (r == null)
			{
				System.err.println("RegionGrow: Couldn't find "+tl[i]);
				return null;
			}

			roi.setAssociation(r);

			r.getPOI(0,p);

			comment += tl[i] + ": " + p.x + " " + p.y + " " + p.z + " ";

			for(int j=0; j<numScans; j++)
			{
				A[i][j] = v.getData(p.y, p.x, p.z, j);
			}
		}

		roi.setComments(comment);

		if (target < 0)
			System.err.println("RegionGrow: Unknown tissue type "+type);

		//-----------------------------------------------------
		//
		//  Initialize the original number of points.
		//
		num_points=0;
		doneMask.clear();

		c.setA(A);

		//-----------------------------------------------------
		//
		//  Initialize the queues to have the x and y values.
		//
		push(start_x, start_y, start_z);

		int iter = 0;
		int curr_x, curr_y, curr_z;

		//-----------------------------------------------------
		//
		//  Create the output lesion.
		//
		while( pop(curr) ) 
		{
			//-----------------------------------------------------
			//
			//  Get the first point off of the stack.
			//
			curr_x = curr[0];
			curr_y = curr[1];
			curr_z = curr[2];

			if (doneMask.contains(curr_x,curr_y,curr_z)) continue;

			//-----------------------------------------------------
			//
			//  Set b.
			//
			// b = v.getChannelDouble(curr_y,curr_x,curr_z);

			for(int ii=0; ii<numScans; ii++) 
			{        
				b[ii] = v.getData(curr_y, curr_x, curr_z, ii);
			}

			//-----------------------------------------------------
			//
			//  Compute the answer.
			//
			c.solve(b, result);

			doneMask.set(curr_x,curr_y,curr_z);
			//-----------------------------------------------------
			//
			//  Go North, South, East and West.
			//
			if( result[target] >= threshold ) 
			{
				roi.add((short)curr_x, (short)curr_y, (short)curr_z, (float)result[target]);

				if ( curr_x < dims[0]-1 )
				{
					push(curr_x+1, curr_y, curr_z);
				}        

				if ( curr_x > 0 )
				{
					push(curr_x-1, curr_y, curr_z);
				}        

				if ( curr_y < dims[1]-1 )
				{
					push(curr_x, curr_y+1, curr_z);
				}        

				if ( curr_y > 0 )
				{
					push(curr_x, curr_y-1, curr_z);
				}        

				if ( curr_z < dims[2]-1 )
				{

			// travel = -1 -> no restrictions
			// Second part will only be tested if travel >= 0
					if ((travel<0) || (curr_z < start_z+travel) )
					{
						push(curr_x, curr_y, curr_z+1);
					}
				}        

				if ( curr_z > 0 )
				{
			// Second part will only be tested if travel >= 0
					if ((travel<0) || (curr_z > start_z-travel) )
					{
						push(curr_x, curr_y, curr_z-1);
					}
				}        

				if( (num_points+6) > xQ.length ) 
				{
					xQ = reallocate(xQ);
					yQ = reallocate(yQ);
					zQ = reallocate(zQ);
				}

			}        

			iter++; // Just for testing.
		} 

		return roi;
	}

	public ROI grow(ROI[] rois, String type)
	{
		int start_x=-1,start_y=-1,start_z=-1;
		String comment = "";
		int target = -1;
		ROI roi = new ROI();
		POI p = new POI();

		roi.setDate();
		roi.setTissueCode(type);
		roi.setAlgorithm("segment:autogrow:Travel=" +travel+":RG_Thresh="+threshold+":tol="+tol);

		for(int i=0; i<numTissues; i++)
		{
			ROI r = null;

			for(int j=0;j<rois.length;j++)
				{ if (tl[i].equals(rois[j].getTissueCode())) r=rois[j]; } 

			if (tl[i].equals(type))
			{
				start_x = r.getPOI(0).getX();
				start_y = r.getPOI(0).getY();
				start_z = r.getPOI(0).getZ();
				target = i;
			}

			if (r == null)
			{
				System.err.println("RegionGrow: Couldn't find "+tl[i]);
				return null;
			}

			roi.setAssociation(r);

			r.getPOI(0,p);

			comment += tl[i] + ": " + p.x + " " + p.y + " " + p.z + " ";

			for(int j=0; j<numScans; j++)
			{
				A[i][j] = v.getData(p.y, p.x, p.z, j);
			}
		}

		roi.setComments(comment);

		if (target < 0)
			System.err.println("RegionGrow: Unknown tissue type "+type);

		//-----------------------------------------------------
		//
		//  Initialize the original number of points.
		//
		num_points=0;
		doneMask.clear();

		c.setA(A);

		//-----------------------------------------------------
		//
		//  Initialize the queues to have the x and y values.
		//
		push(start_x, start_y, start_z);

		int iter = 0;
		int curr_x, curr_y, curr_z;

		//-----------------------------------------------------
		//
		//  Create the output lesion.
		//
		while( pop(curr) ) 
		{
			//-----------------------------------------------------
			//
			//  Get the first point off of the stack.
			//
			curr_x = curr[0];
			curr_y = curr[1];
			curr_z = curr[2];

			if (doneMask.contains(curr_x,curr_y,curr_z)) continue;

			//-----------------------------------------------------
			//
			//  Set b.
			//
			// b = v.getChannelDouble(curr_y,curr_x,curr_z);

			for(int ii=0; ii<numScans; ii++) 
			{        
				b[ii] = v.getData(curr_y, curr_x, curr_z, ii);
			}

			//-----------------------------------------------------
			//
			//  Compute the answer.
			//
			c.solve(b, result);

			doneMask.set(curr_x,curr_y,curr_z);
			//-----------------------------------------------------
			//
			//  Go North, South, East and West.
			//
			if( result[target] >= threshold ) 
			{
				roi.add((short)curr_x, (short)curr_y, (short)curr_z, (float)result[target]);

				if ( curr_x < dims[0]-1 )
				{
					push(curr_x+1, curr_y, curr_z);
				}        

				if ( curr_x > 0 )
				{
					push(curr_x-1, curr_y, curr_z);
				}        

				if ( curr_y < dims[1]-1 )
				{
					push(curr_x, curr_y+1, curr_z);
				}        

				if ( curr_y > 0 )
				{
					push(curr_x, curr_y-1, curr_z);
				}        

				if ( curr_z < dims[2]-1 )
				{

			// travel = -1 -> no restrictions
			// Second part will only be tested if travel >= 0
					if ((travel<0) || (curr_z < start_z+travel) )
					{
						push(curr_x, curr_y, curr_z+1);
					}
				}        

				if ( curr_z > 0 )
				{
			// Second part will only be tested if travel >= 0
					if ((travel<0) || (curr_z > start_z-travel) )
					{
						push(curr_x, curr_y, curr_z-1);
					}
				}        

				if( (num_points+6) > xQ.length ) 
				{
					xQ = reallocate(xQ);
					yQ = reallocate(yQ);
					zQ = reallocate(zQ);
				}

			}        

			iter++; // Just for testing.
		} 

		return roi;
	}
	private int[] reallocate(int[] vec)
	{
		int[] temp = new int[ 2 * vec.length ];

		// Temporarily copy over the vector.
		System.arraycopy(vec, 0, temp, 0, vec.length );

		return temp;
	}

public static void main(String[] argv)
{
	MRIFile mf = MRIFile.readMRIFile("/tmp/C0511D10.MIF");
	ROIList rl = new ROIList("/tmp/C0511s10.roi");
	MCVolume v = mf.getMCVolume();
	ConvexP cp = new ConvexP(4,2);
	String[] tl = {"wm","gm","csf","les"};

	RegionGrow rg = new RegionGrow(v,cp,rl,tl);

	ROI[] R = rl.findAllUser("les");
	POI p = new POI();
	
	System.out.println("Found "+R.length+" lesion points.");

	for(int i=0; i<R.length; i++)
	{
		R[i].getPOI(0,p);
		ROI r = rg.grow(p.x, p.y, p.z, "les");
		int n = r.size();
		if (n<5) System.out.print(".");
		else if (n<20) System.out.print(",");
		else if (n<100) System.out.print(":");
		else if (n<500) System.out.print(";");
		else System.out.print("#");
		System.out.flush();
		rl.add(r);
	}

	System.out.println("\nDone!");

	rl.write("/tmp/autoout.roi");
}

}

class Mask2D
{
	int mx, my;
	boolean[][] mask;

	Mask2D(int[] dims)
	{
		mx = dims[0]+1; my = dims[1]+1;
		mask = new boolean[mx][];
		for(int i=0;i<mx;i++) mask[i] = new boolean[my];
		clear();
	}

	void clear()
	{
		for(int i=0;i<mx;i++) Arrays.fill(mask[i],false);
	}

	final void set(int x, int y, int z)
	{
		mask[x][y] = true;
	}

	final boolean contains(int x, int y, int z)
	{
		return mask[x][y];
	}

}
