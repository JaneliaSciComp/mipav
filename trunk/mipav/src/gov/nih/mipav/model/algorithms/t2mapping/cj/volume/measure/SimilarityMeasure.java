package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

abstract public class SimilarityMeasure
{
	protected int rowSkip=5;
	protected int colSkip=5;
	protected int sliceSkip=1;

	int num_evals = 0;


	/** Set the number of rows to skip. */
	public void setRowSkip(final int rowSkip) 
		{ 	this.rowSkip = rowSkip; }

	/** Set the number of cols to skip. */
	public void setColSkip(final int colSkip) 
		{ 	this.colSkip = colSkip; }

	/** Set the number of slices to skip. */
	public void setSliceSkip(final int sliceSkip) 
		{ 	this.sliceSkip = sliceSkip; }


	abstract public double distance(VolumeBase v1, VolumeBase v2);

	public double distance(VolumeBase v1, MCVolume v2)
	{
		double t=0;

		for(int i=0; i<v2.getNChannels(); i++)
		{
			t += distance(v1,v2.getVolume(i));
		}
		return t;
	}

	public double distance(MCVolume v1, MCVolume v2)
	{
		double t=0;

		int ns = Math.min(v1.getNChannels(), v2.getNChannels());

		for(int i=0; i<ns; i++)
		{
			t += distance(v1.getVolume(i),v2.getVolume(i));
		}
		return t;
	}

	public int getNumEvals() { return num_evals; }
}
