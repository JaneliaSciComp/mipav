package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class ShannonSimilarity extends SimilarityMeasure
{
	public ShannonSimilarity()
	{
	}

	public double distance(VolumeBase v1, VolumeBase v2)
	{
		double d1, d2;

		double[] lb={0,0}, ub = {0,0}, bins = new double[2];

		for(int s=0;s<v1.getNSlices();s+=sliceSkip)
			for(int r=0;r<v1.getNRows();r+=rowSkip) for(int c=0;c<v1.getNCols();c+=colSkip)
		{
			d1 = v1.getData(r,c,s); if (ub[0] < d1) ub[0] = d1;
			d2 = v2.getData(r,c,s); if (ub[1] < d2) ub[1] = d2;
		}

		bins[0] = ub[0]/10.0; bins[1] = ub[1]/10.0;

		Hist2D h = new Hist2D(lb,ub,bins); h.clear();

		for(int s=0;s<v1.getNSlices();s+=sliceSkip)
			for(int r=0;r<v1.getNRows();r+=rowSkip) for(int c=0;c<v1.getNCols();c+=colSkip)
			{
				d1 = v1.getData(r,c,s); d2 = v2.getData(r,c,s);
				h.add(d1,d2);
			}
		
		num_evals++;

		return 1.0/(0.0001+h.mutualInfo());
	}

}
