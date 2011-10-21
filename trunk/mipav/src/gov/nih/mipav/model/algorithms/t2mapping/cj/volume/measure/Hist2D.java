package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;

public class Hist2D extends HistBase
{
	protected int total=0;
	protected int nBins0, nBins1;
	protected int[][] counts;
	protected double skip0, lower0, upper0;
	protected double skip1, lower1, upper1;

	private void init()
	{
		counts = new int[nBins0][nBins1];
		clear();
	}

	public int getNDims() { return 2; }

	public Hist2D(double lower[], double upper[], double nbins[])
	{
		super();

		this.lower0 = lower[0]; this.lower1 = lower[1];
		this.upper0 = upper[0]; this.upper1 = upper[1];

		this.skip0 = (upper0-lower0)/nbins[0];
		this.skip1 = (upper1-lower1)/nbins[1];

		this.nBins0 = (int) Math.ceil(nbins[0]);
		this.nBins1 = (int) Math.ceil(nbins[1]);

		init();
	}

	public String toString()
	{
		return Matrix.toString(counts);
	}

	public void clear()
	{
		total=0; Matrix.fill(counts,0);
	}

	public double entropy()
	{
		double t=0;

		for(int i=0;i<nBins0;i++) for(int j=0;j<nBins1;j++)
			t += xlog(counts[i][j]);

		return (xlog(total)-t)/total;
	}

	public double mutualInfo()
	{
		double t=0;
		int[] c0 = new int[nBins0], c1 = new int[nBins1];

		for(int i=0;i<nBins0;i++) for(int j=0;j<nBins1;j++)
		{
			c0[i] += counts[i][j];
			c1[j] += counts[i][j];
			t -= xlog(counts[i][j]);
		}

		for(int i=0;i<nBins0;i++) { t += xlog(c0[i]); }
		for(int j=0;j<nBins1;j++) { t += xlog(c1[j]); }

		return (xlog(total)-t)/total;
	}

	public void add(double[] X) { add(X[0], X[1]); }

	public void add(double x, double y)
	{
		if (x < lower0 || x > upper0 || y < lower1 || y > upper1) return;

		int bin0 = (int)((x-lower0)/skip0);
		int bin1 = (int)((y-lower1)/skip1);

		if (bin0 >= nBins0) bin0--;  if (bin1 >= nBins1) bin1--;

		counts[bin0][bin1]++; total++;
	}

	public void add(Slice s1, Slice s2) { tS[0] = s1; tS[1] = s2; add(tS); }
	public void add(Slice s1, Slice s2, BinarySlice mask)
		{ tS[0] = s1; tS[1] = s2; add(tS,mask); }

	public void add(Volume v1, Volume v2) { tV[0] = v1; tV[1] = v2; add(tV); }
	public void add(Volume v1, Volume v2, BinaryVolume mask)
		{ tV[0] = v1; tV[1] = v2; add(tV,mask); }

	public void set(Slice s1, Slice s2) {clear(); add(s1,s2); }
	public void set(Slice s1, Slice s2, BinarySlice mask)
		{ clear(); add(s1,s2,mask); }

	public void set(Volume v1, Volume v2) {clear(); add(v1,v2); }
	public void set(Volume v1, Volume v2, BinaryVolume mask)
		{ clear(); add(v1,v2,mask); }

	public void set(MCVolume v) {clear(); add(v);}
	public void set(MCVolume v, BinaryVolume mask) {clear(); add(v,mask);}
}
