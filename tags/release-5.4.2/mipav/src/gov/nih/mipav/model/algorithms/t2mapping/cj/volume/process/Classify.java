package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.convexp.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;

public class Classify extends PointFilter
{
	private boolean useGrown;
	private double tol, thresh;
	private double[][] A;
	private int numTissues, numScans;
	private MCVolume v;
	private Volume grown;
	private ConvexP c;
	private BasisSet bs;
	private String[] tl;
	private POI p = new POI();

	protected void onSlice() { System.out.print("."); System.out.flush(); }

	public Classify(MCVolume v, ConvexP c, ROIList rl, String[] tissues)
		{ super(v); init(v,c,rl,tissues,false); }

	public Classify(MCVolume v, ConvexP c, ROIList rl, String[] tissues,
					boolean useGrown)
		{ super(v); init(v,c,rl,tissues,useGrown); }

	// ASSUMPTION: if useGrown true, the last tissue type in tissues is
    // assumed to be the one that is pre-grown in rl.
	// NEW ASSUMPTION: first tissue class is white matter.
	protected void init(MCVolume v, ConvexP c, ROIList rl, String[] tissues,
                      boolean useGrown)
	{
		this.useGrown = useGrown;
		this.numTissues = c.getNumTissues();
		this.numScans = c.getNumScans();

		if ( v.getNChannels() != numScans )
		{
			throw new IllegalArgumentException("Expecting "+numScans+" channels, got "+v.getNChannels());
		}

		if ( !useGrown && tissues.length != numTissues )
		{
			throw new IllegalArgumentException("Expecting "+numTissues+" tissues, got "+tissues.length);
		}

		if ( useGrown && tissues.length != numTissues + 1 )
		{
			throw new IllegalArgumentException("Expecting "+(numTissues+1)+" tissues, got "+tissues.length);
		}

		A = new double[numTissues][numScans];

		this.v = v; this.c = c;
		this.bs = new BasisSet(rl,tissues);
		this.tl = tissues;

		NoiseEstimate ne = new NoiseEstimate();
		tol = 2 * ne.gradientMethod(v);
		thresh = ne.histogramMethod(v.getVolume(0));
		System.out.println("Using tol = " + tol + ", thresh = "+thresh);
		c.setTol(tol);

		if (useGrown)
		{
			System.out.println("Using pre-grown "+tl[tl.length-1]+"...");
			grown = rl.getVolume();
		}
	}

	/**
	 *  Number of output channels.
	 *     The extra one is for the distance measure from the convex hull.
	 */
	final public int getOutputDepth(int in) {return tl.length+1;}

	final public void compute(double[] in, double[] out)
	{
		if (in[0] < thresh)
			{ java.util.Arrays.fill(out,0); return; }

		boolean solveIt = true;

		if (useGrown)
		{
			out[tl.length-1] = grown.getData(theRow, theCol, theSlice);
			if (out[tl.length-1] > 0)
			{
				for(int i=1;i<tl.length-1;i++) out[i] = 0;

				out[0] = 1.0-out[tl.length-1];
				out[out.length-1] = 0;
				solveIt = false;
			}
		}

		if (solveIt)
		{
		for(int i=0; i<numTissues; i++)
		{
			// REMEMBER TO CHECK FOR TRANSPOSE ERROR!
			ROI r = bs.findClosest(theRow, theCol, theSlice, i);

			if (r == null)
			{
				System.err.println("Classify: Couldn't find " + tl[i]);
				return;
			}

			r.getPOI(0,p);

			// IMPORTANT - THIS MUST CHANGE ONCE ROI TRANSPOSE IS FIXED!
			// Update - This did change, now ROI001 is currently broken
			v.getChannel(A[i],p.y,p.x,p.z);

			// for(int j=0; j<numScans; j++)
			//	 { A[i][j] = v.getData(p.y, p.x, p.z, j); }
		}

		c.setA(A); double dist = c.solve(in,out);
		out[out.length-1] = dist;	
		}

		for(int i=0; i<out.length-1; i++) out[i] *= 1000;
	}

}
