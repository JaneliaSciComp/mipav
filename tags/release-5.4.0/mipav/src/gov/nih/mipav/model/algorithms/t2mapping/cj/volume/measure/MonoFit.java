package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class MonoFit {

	private float slope;
	private float intercept;

	private int nRows;
	private int nCols;
	private int nSlices;
	private int nChannels;

	public MonoFit()
	{
	}

	public MCVolume monoLogFit(MCVolume mmmm, double[] x_in)
	{
		float[] x = new float[x_in.length];
		for(int ii=0; ii<x_in.length; ii++) {
			x[ii] = (float)x_in[ii];
		}

		return monoLogFit(mmmm, x);
	}

	public MCVolume monoLogFit(MCVolume mmmm, float[] x)
	{
		float[] channel = null;

		nRows = mmmm.getNRows();
		nCols = mmmm.getNCols();
		nSlices = mmmm.getNSlices();
		nChannels = mmmm.getNChannels();

		MCVolume fitted = new MCVolume(nRows,nCols,nSlices,2);

		for(int row=0; row<nRows; row++) {
			for(int col=0; col<nCols; col++) {
				for(int slice=0; slice<nSlices; slice++) {
					channel = mmmm.getChannel(row,col,slice);

					for(int ch=0; ch<nChannels; ch++) {
						channel[ch] = (float)Math.log((double)channel[ch]);
					}

					monoFit(x, channel);
					fitted.setData(slope, row,col,slice, 0);
					fitted.setData(intercept, row,col,slice, 1);
				}
			}
		}

		return fitted;
	}

	public MCVolume monoFit(MCVolume mmmm, float[] x)
	{
		float[] channel = null;

		nRows = mmmm.getNRows();
		nCols = mmmm.getNCols();
		nSlices = mmmm.getNSlices();
		nChannels = mmmm.getNChannels();

		MCVolume fitted = new MCVolume(nRows,nCols,nSlices,2);

		for(int row=0; row<nRows; row++) {
			for(int col=0; col<nCols; col++) {
				for(int slice=0; slice<nSlices; slice++) {
					channel = mmmm.getChannel(row,col,slice);
					monoFit(x, channel);

					fitted.setData(slope, row,col,slice, 0);
					fitted.setData(intercept, row,col,slice, 1);
				}
			}
		}

		return fitted;
	}

	public void monoFit(float[] x, float[] y)
	{
		float sumx = (float)0.0;
		float sumy = (float)0.0;
		float sumxy = (float)0.0;
		float sumx2 = (float)0.0;
		intercept = (float)0.0;
		slope = (float)0.0;

		for(int ii=0; ii<x.length; ii++) {
			sumx += x[ii];
			sumy += x[ii];
			sumxy += (x[ii]*y[ii]);
			sumx2 += (x[ii]*x[ii]);
		}

		slope = x.length * sumxy - sumx * sumy;
		intercept = x.length * sumx2 - sumx * sumx;

		slope /= intercept;

		intercept = sumy - slope * sumx;

		intercept /= x.length;
	}
}
