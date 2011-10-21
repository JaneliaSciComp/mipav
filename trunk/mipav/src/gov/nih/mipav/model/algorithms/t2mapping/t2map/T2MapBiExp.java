package gov.nih.mipav.model.algorithms.t2mapping.t2map;

import java.util.Vector;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.T2FitBiExp;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class T2MapBiExp extends T2Map
{
	private double[] output = null;

	public T2MapBiExp()
	{
		t2fitter = new T2FitBiExp();
	}

	public void setParameters( double[] pp )
	{
	}

	public void setTE(final double[] te)
	{
		t2fitter.setTE(te);
	}

	protected int getNSpectralChannels() { return 5; }

	protected void dealWithData(final T2Distribution dist,
		final double chi2_temp, int row, int col, int slice)
	{
		if( output == null ) 
			output = new double[5];

		output[0] = dist.getAmplitudes()[0];
		output[2] = dist.getAmplitudes()[1];
		output[1] = dist.getT2s()[0];
		output[3] = dist.getT2s()[1];
		output[4] = dist.getBaseline();

		svol.setData(output, row, col, slice);
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

		String outfilename = basename + "_biexp.bff.gz";
		bff.write(outfilename, MRIFile.DATATYPE_FLOAT );

		/*
		 *  Save the chi2 volume.
		 */
		BFF bff_chi2 = new BFF(header);
		bff_chi2.setDataType( MRIFile.DATATYPE_FLOAT );
		bff_chi2.setMCVolume(chi2);
		bff_chi2.addComment("biexp chi2 map");

		outfilename = basename + "_biexp_chi2.bff.gz";
		bff_chi2.write(outfilename, MRIFile.DATATYPE_FLOAT );

	}
}
