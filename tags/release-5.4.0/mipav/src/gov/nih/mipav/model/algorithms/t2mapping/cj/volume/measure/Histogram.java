package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 * @deprecated - Migrate to HistBase hierarchy
 */

public class Histogram {

	float[] bins = null;
	float[] counts = null;
	
	public Histogram()
	{
	}

	public float[] getBins() {
		float[] bins_out = new float[bins.length];
		System.arraycopy(bins, 0, bins_out, 0, bins.length);
		return bins_out;
	}

	public float[] getCounts() {
		float[] counts_out = new float[counts.length];
		System.arraycopy(counts, 0, counts_out, 0, counts.length);
		return counts_out;
	}

	/** Histogram a slice of data.
	 */
	public void histogram(Slice slice)
	{
		histogram(slice, -Float.MAX_VALUE, Float.MAX_VALUE);
	}

	/** Histogram an MCVolume of data with thresholds.
	 */
	public void histogram(Slice slice, float lower_threshold, float upper_threshold)
	{
		float mindata = (float)Statistics.min(slice);
		float maxdata = (float)Statistics.max(slice);

		int r = (int)Math.floor( maxdata - mindata + 0.5);
		int b = (int)Math.floor(r/100);
		int nbins = r/b;

		bins = new float[ nbins ];
		counts = new float[ nbins ];

		histogram(slice, lower_threshold, upper_threshold, nbins);
	}

	/** Histogram a Slice of data with thresholds and number of bins.
	 */
	public void histogram(Slice slice, float lower_threshold, 
	                      float upper_threshold, int nbins)
	{
		float mindata = (float)Statistics.min(slice);
		float maxdata = (float)Statistics.max(slice);

		float binsize = (maxdata - mindata) / (nbins); 

		bins = new float[ nbins ];
		counts = new float[ nbins ];

		for(int ii=0; ii<bins.length; ii++) {
			bins[ii] = ii*binsize + binsize/2 + mindata;
		}

		int index;
		int junk = 0;
		float d = (float)0.0;
		for(int ri=0; ri<slice.getNRows(); ri++) {
			for(int ci=0; ci<slice.getNCols(); ci++) {
				d = slice.getData(ri,ci);
				if( d > lower_threshold && d < upper_threshold ) {
					index = (int)Math.floor( (double)(d-mindata)/(binsize)  );

					if( index > nbins-1 ||  index < 0) {
						junk++;
					}
					else {
						counts[ index ]++;
					}
				}
			}
		}

		int totalcounts=0;
		for(int ii=0;ii<counts.length;ii++) {
			totalcounts += (int)counts[ii];
		}

		return;
	}

	/** Histogram an MCVolume of data.
	 */
	public void histogram(Volume mm)
	{
		histogram(mm, (float)-99999999999.0, (float)99999999999999.0);
	}


	/** Histogram an MCVolume of data with thresholds.
	 */
	public void histogram(Volume mm, float lower_threshold, float upper_threshold)
	{
		float mindata = (float)Statistics.min(mm);
		float maxdata = (float)Statistics.max(mm);

		int r = (int)Math.floor( maxdata - mindata + 0.5);
		int b = (int)Math.floor(r/100);
		int nbins = r/b;

		bins = new float[ nbins ];
		counts = new float[ nbins ];

		histogram(mm, lower_threshold, upper_threshold, nbins);
	}

	/** Histogram an MCVolume of data with thresholds and number of bins.
	 */
	public void histogram(Volume mm, float lower_threshold, 
	                      float upper_threshold, int nbins)
	{
		float mindata = (float)Statistics.min(mm);
		float maxdata = (float)Statistics.max(mm);

		float binsize = (maxdata - mindata) / (nbins); 

		bins = new float[ nbins ];
		counts = new float[ nbins ];

		for(int ii=0; ii<bins.length; ii++) {
			bins[ii] = ii*binsize + binsize/2 + mindata;
		}

		int index;
		int junk = 0;
		float d = (float)0.0;
		for(int si=0; si<mm.getNSlices(); si++) {
		for(int ri=0; ri<mm.getNRows(); ri++) {
		for(int ci=0; ci<mm.getNCols(); ci++) {
			d = mm.getData(ri,ci,si);
			if( d > lower_threshold && d < upper_threshold ) {
				index = (int)Math.floor( (double)(d-mindata)/(binsize)  );

				if( index > nbins-1 ||  index < 0) {
					junk++;
				}
				else {
					counts[ index ]++;
				}
			}
		}
		}
		}

		int totalcounts=0;
		for(int ii=0;ii<counts.length;ii++) {
			totalcounts += (int)counts[ii];
		}

		return;
	}

	/** Histogram an MCVolume of data.
	 */
	public void histogram(MCVolume mm)
	{
		histogram(mm, (float)-99999999999.0, (float)99999999999999.0);
	}


	/** Histogram an MCVolume of data with thresholds.
	 */
	public void histogram(MCVolume mm, float lower_threshold, float upper_threshold)
	{
		float mindata = (float)Statistics.min(mm);
		float maxdata = (float)Statistics.max(mm);

		int r = (int)Math.floor( maxdata - mindata + 0.5);
		int b = (int)Math.floor(r/100);
		int nbins = r/b;

		bins = new float[ nbins ];
		counts = new float[ nbins ];

		histogram(mm, lower_threshold, upper_threshold, nbins);
	}

	/** Histogram an MCVolume of data with thresholds and number of bins.
	 */
	public void histogram(MCVolume mm, float lower_threshold, 
	                      float upper_threshold, int nbins)
	{
		float mindata = (float)Statistics.min(mm);
		float maxdata = (float)Statistics.max(mm);

		float binsize = (maxdata - mindata) / (nbins); 

		bins = new float[ nbins ];
		counts = new float[ nbins ];

		for(int ii=0; ii<bins.length; ii++) {
			bins[ii] = ii*binsize + binsize/2 + mindata;
		}

		int index;
		int junk = 0;
		float d = (float)0.0;
		for(int ei=0; ei<mm.getNChannels(); ei++) {
		for(int si=0; si<mm.getNSlices(); si++) {
		for(int ri=0; ri<mm.getNRows(); ri++) {
		for(int ci=0; ci<mm.getNCols(); ci++) {
			d = mm.getData(ri,ci,si,ei);
			if( d > lower_threshold && d < upper_threshold ) {
				index = (int)Math.floor( (double)(d-mindata)/(binsize)  );

				if( index > nbins-1 ||  index < 0) {
					junk++;
				}
				else {
					counts[ index ]++;
				}
			}
		}
		}
		}
		}

		int totalcounts=0;
		for(int ii=0;ii<counts.length;ii++) {
			totalcounts += (int)counts[ii];
		}

		return;
	}
}
