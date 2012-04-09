package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class L2Similarity extends SimilarityMeasure
{
	public L2Similarity() { }

	public double distance(VolumeBase v1, VolumeBase v2)
	{
		double t = 0;
		double d1, d2;

		for(int s=0;s<v1.getNSlices();s+=sliceSkip)
			for(int r=0;r<v1.getNRows();r+=rowSkip) for(int c=0;c<v1.getNCols();c+=colSkip)
			{
				d1 = v1.getData(r,c,s); d2 = v2.getData(r,c,s);
				t += ((d1-d2)*(d1-d2));
			}
		
		t = Math.sqrt(t / (v1.getNSlices()/sliceSkip) /
            (v1.getNRows()/rowSkip) / (v1.getNCols()/colSkip));

		return t;
	}
}
