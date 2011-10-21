package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class Hist1D extends HistBase
{
	protected int total=0;
	protected int nBins;
	protected int counts[];
	protected double bins[];
	protected double skip, lower, upper;

	private void init()
	{
		counts = new int[nBins]; bins = new double[nBins];
		double f=lower;

		for(int i=0; i<nBins; i++, f+=skip) { counts[i] = 0; bins[i] = f; }
	}

	public int getNDims() { return 1; }

	public Hist1D(double lower, double upper, double nbins)
	{
		super();

		this.lower = lower;
		this.upper = upper;
		this.skip = (upper-lower)/nbins;
		this.nBins = (int) Math.ceil(nbins);

		init();
	}

	public void clear()
	{
		total=0; for(int i=0;i<counts.length;i++) counts[i]=0;
	}

	public double entropy()
	{
		double t=0;

		for(int i=0;i<nBins;i++) t += xlog(counts[i]);

		return (xlog(total)-t)/total;
	}

	public void add(double[] X) { add(X[0]); }

	public void add(double x)
	{
		if (x < lower || x > upper) return;

		int bin = (int)((x-lower)/skip);
		if (bin >= nBins) bin--;

		counts[bin]++; total++;
	}

	public String toString()
	{
		String s = "";
		for(int i=0;i<nBins;i++)
		{
			s += counts[i] + "\t - " + bins[i] + "\n";
		}
		return s;
	}

	public void add(Slice s) { tS[0] = s; add(tS); }
	public void add(Slice s, BinarySlice mask) { tS[0] = s; add(tS,mask); }

	public void set(Slice s) {clear(); add(s);}
	public void set(Volume v) {clear(); add(v);}
	public void set(MCVolume v) {clear(); add(v);}

	public void set(Slice s, BinarySlice mask) {clear(); add(s,mask);}
	public void set(Volume v, BinaryVolume mask) {clear(); add(v,mask);}
	public void set(MCVolume v, BinaryVolume mask) {clear(); add(v,mask);}
	
	public void add(Volume v) { tV[0] = v; add(tV); }
	public void add(Volume v, BinaryVolume mask) { tV[0] = v; add(tV,mask); }
}
