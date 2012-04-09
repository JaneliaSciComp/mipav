package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

abstract public class PointFilter
{
	protected MCVolume v;
	protected int nRows, nCols, nSlices, nChannels;
	protected int theRow, theCol, theSlice;

    // Override these to modify parameters between rows and slices
	protected void onRow() {}
	protected void onSlice() {}

	PointFilter(MCVolume v)
	{
		setVolume(v);
	}

	abstract public int getOutputDepth(int inputDepth);
	abstract public void compute(double[] in, double[] out);

    public void setVolume(MCVolume v)
	{
		this.v = v;
		nRows = v.getNRows();
		nCols = v.getNCols();
		nSlices = v.getNSlices();
		nChannels = v.getNChannels();
	}

	public MCVolume filter()
	{
		int k = getOutputDepth(nChannels);

		double[] in = new double[nChannels],
                out = new double[k];

		MCVolume v2 = new MCVolume(nRows,nCols,nSlices,k);

		for(theSlice=0;theSlice<nSlices;theSlice++)
		{
			onSlice();
			for(theRow=0;theRow<nRows;theRow++)
			{
				onRow();
				for(theCol=0;theCol<nCols;theCol++)
				{
					v.getChannel(in,theRow,theCol,theSlice);
					compute(in,out);
					v2.setChannel(out,theRow,theCol,theSlice);
				}
			}
		}
		return v2;
	}
}
