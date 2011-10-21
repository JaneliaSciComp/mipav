package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class WoodsSimilarity extends SimilarityMeasure
{
	final int MAXVAL=4096;
	private double[] vcf_tot = new double[MAXVAL], vcf_sqr_tot = new double[MAXVAL];
	private long[] vcf_count = new long[MAXVAL];

	public WoodsSimilarity()
	{
	}

	public double distance(VolumeBase v1, VolumeBase v2)
	{
		java.util.Arrays.fill(vcf_tot,0.0);
		java.util.Arrays.fill(vcf_sqr_tot,0.0);
		java.util.Arrays.fill(vcf_count,0);

		double t = 0;
		double d1, d2;

		for(int s=0;s<v1.getNSlices();s+=sliceSkip)
			for(int r=0;r<v1.getNRows();r+=rowSkip) for(int c=0;c<v1.getNCols();c+=colSkip)
			{
				d1 = v1.getData(r,c,s); d2 = v2.getData(r,c,s);
				int i = (int)d1;
				if (i >=0  && i < MAXVAL)
				{
					vcf_tot[i] += d2; vcf_sqr_tot[i] += d2*d2;
					vcf_count[i]++;
				}
			}
		
		double vcf_total_dev=0, vcf_total_count=0;

		for(int i=0;i<MAXVAL;i++)
		{
			if (vcf_count[i]==0) continue;
			double vcf_mean = vcf_tot[i]/vcf_count[i];
			double vcf_std_dev = vcf_sqr_tot[i] - vcf_mean*vcf_mean*vcf_count[i];
			//System.out.println("("+i+","+vcf_mean+","+vcf_std_dev+")");
			vcf_total_dev += vcf_std_dev;
			vcf_total_count += vcf_count[i];
		}

		t = vcf_total_dev/vcf_total_count;

		num_evals++;

		return t;
	}
}
