package gov.nih.mipav.model.algorithms.t2mapping.t2map;

import java.util.Vector;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.T2FitMonoExp;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;

public class T2MapMonoExp extends T2Map
{
	public T2MapMonoExp()
	{
		t2fitter = new T2FitMonoExp();
	}
	
	public T2MapMonoExp(int xDimIn, int yDimIn, int numSlicesIn, double[] TEValuesIn)
	{
		super(xDimIn, yDimIn, numSlicesIn, TEValuesIn);
	}

	public void setParameters( double[] pp )
	{
	}

	public void setTE(final double[] te)
	{
		t2fitter.setTE(te);
	}

	protected int getNSpectralChannels() { return 2; }

	protected void dealWithData(final T2Distribution dist, 
		final double chi2_temp, int row, int col,  int slice )
	{
		svol.setData(dist.getAmplitudes(0), row,col,slice,0);
		svol.setData(dist.getT2s(0), row,col,slice,1);

		chi2.setData(chi2_temp, row, col, slice, 0);
	}

	protected void dealWithAlpha(final double flip,
           int row, int col, int slice)
	{
	}

	public void saveData(final String basename, final Vector header, final String comment)
	{
		BFF bff_monoexp = new BFF(header);
		bff_monoexp.setDataType( MRIFile.DATATYPE_FLOAT );
		bff_monoexp.setMCVolume(svol.getMCVolume());
		bff_monoexp.addComment(comment);

		String outfilename = basename + "_monoexp.bff.gz";
		bff_monoexp.write(outfilename, MRIFile.DATATYPE_FLOAT );

		/*
		 *  Save the chi2 volume.
		 */
		BFF bff_chi2 = new BFF(header);
		bff_chi2.setDataType( MRIFile.DATATYPE_FLOAT );
		bff_chi2.setMCVolume(chi2);
		bff_chi2.addComment("T2Map monoexp chi2 map");

		outfilename = basename + "_monoexp_chi2.bff.gz";
		bff_chi2.write(outfilename, MRIFile.DATATYPE_FLOAT );

	}
}
