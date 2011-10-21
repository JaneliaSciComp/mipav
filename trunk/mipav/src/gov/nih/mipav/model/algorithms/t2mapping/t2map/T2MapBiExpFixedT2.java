package gov.nih.mipav.model.algorithms.t2mapping.t2map;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class T2MapBiExpFixedT2 extends T2Map
{
	private double t2_1 = 0.0;
	private double t2_2 = 0.0;

	public T2MapBiExpFixedT2()
	{
		t2fitter = new T2FitBiExpFixedT2();
	}

	public T2MapBiExpFixedT2(final double[] te)
	{
		t2fitter = new T2FitBiExpFixedT2(te);
	}

	public void setParameters(final double[] params)
	{
		t2fitter.setParameters(params);
		t2_1 = params[0];
		t2_2 = params[1];
	}

	public void setTE(final double[] te)
	{
		t2fitter.setTE(te);
	}

	protected int getNSpectralChannels() { return 4; }

	protected void dealWithData(final T2Distribution dist,
		final double chi2_temp, int row, int col, int slice)
	{ 
		svol.setData(dist.getAmplitudes(0), row,col,slice,0);
		svol.setData(t2_1, row,col,slice,1);
		svol.setData(dist.getAmplitudes(1), row,col,slice,2);
		svol.setData(t2_2, row,col,slice,3);

		chi2.setData(chi2_temp, row, col, slice, 0);
	}

    protected void dealWithAlpha(final double flip,
	        int row, int col, int slice)
	{
	}

	public void saveData(final String basename, final Vector header, final String comment)
	{
		BFF bff= new BFF(header);
		bff.setDataType( MRIFile.DATATYPE_FLOAT );
		bff.setMCVolume(svol.getMCVolume());
		bff.addComment(comment);

		String outfilename = basename + "_biexp_fixed.bff.gz";
		bff.write(outfilename, MRIFile.DATATYPE_FLOAT );

		/*
		 *  Save the chi2 volume.
		 */
		BFF bff_chi2 = new BFF(header);
		bff_chi2.setDataType( MRIFile.DATATYPE_FLOAT );
		bff_chi2.setMCVolume(chi2);
		bff_chi2.addComment("T2Map biexp Fixed chi2 map");

		outfilename = basename + "_biexp_fixed_chi2.bff.gz";
		bff_chi2.write(outfilename, MRIFile.DATATYPE_FLOAT );

	}
}
