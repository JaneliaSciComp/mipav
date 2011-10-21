package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

abstract public class HistBase
{
	static final int MAX_TABLE = 1000;
	static double[] logTable = null;

	protected double[] tX;
	protected Slice[] tS;
	protected Volume[] tV;

	protected HistBase()
	{
		int d = getNDims();
        tX = new double[d]; tS = new Slice[d]; tV = new Volume[d];
		initLogs();
	}

	static void initLogs()
	{
		if (logTable != null) return;
		logTable = new double[MAX_TABLE];
		for (int i=1;i<MAX_TABLE;i++) logTable[i] = i*Math.log(i);
		logTable[0] = 0;
	}

	final static double xlog(int n)
	{
		return (n<MAX_TABLE) ? logTable[n] : n*Math.log(n);
	}

	abstract double entropy();
	abstract public void clear();
	abstract public void add(double[] X);
	abstract public int getNDims();

	public void add(Slice[] S) { add(S,null,false); }
	public void add(Slice[] S, BinarySlice mask) { add(S,mask,true); }

	public void add(Volume[] V) { add(V,null,false); }
	public void add(Volume[] V, BinaryVolume mask) { add(V,mask,true); }

	public void add(MCVolume MV) { add(MV,null,false); }
	public void add(MCVolume MV, BinaryVolume mask) { add(MV,mask,true); }

	private void add(Slice[] S, BinarySlice mask, boolean useMask)
	{
		final int z = S.length, m = S[0].getNRows(), n = S[0].getNCols();

		float T[][] = new float[z][]; double X[] = new double[z];
		boolean tb[] = null;

		for(int k=0;k<z;k++) T[k] = new float[n];

		for(int i=0;i<m;i++)
		{
			if (useMask) tb=mask.getRow(i);

			for(int k=0;k<z;k++) S[k].getRow(T[k],i);

			for(int j=0;j<n;j++)
			{
				if (useMask && !tb[j]) continue;

				for(int k=0;k<z;k++) X[k]=T[k][j];
				add(X);
			}
		}
	}

	private void add(Volume[] V, BinaryVolume mask, boolean useMask)
	{
		final int z = V.length, n = V[0].getNSlices();

		Slice[] T = new Slice[z];

		for(int i=0;i<n;i++)
		{
			for(int k=0;k<z;k++) T[k] = V[k].getSlice(i);

			if (useMask) add(T,mask.getSlice(i),useMask);
			else add(T,null,useMask);
		}
	}

	private void add(MCVolume MV, BinaryVolume mask, boolean useMask)
	{
		final int z = MV.getNChannels();

		if (z != getNDims())
		{
			throw new IndexOutOfBoundsException("HistBase::add " + z);
		}

		Volume[] T = new Volume[z];

		for(int k=0;k<z;k++) T[k] = MV.getVolume(k);

		add(T, mask, useMask);
	}
}
