package gov.nih.mipav.model.algorithms.t2mapping.t2map;

import java.util.Vector;
import java.lang.System;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.T2FitL1;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;

/** T2MapClass is responsible to calculate the T2 spectrum through a volume
 *  of points, to contain the results and to save the results.
 */
public class T2MapL1 extends T2Map
{
	private double[] t2s = null;
	private int num_solutions = 0;

	public T2MapL1()
	{
		t2fitter = new T2FitL1();
	}

	public void setParameters( double[] params )
	{
//		t2fitter.setParameters( params );
	}

	public void setNorm(final double frac_low, final double frac_high)
	{
		double[] params = new double[2];
		params[0] = frac_low;
		params[1] = frac_high;
		t2fitter.setParameters(params);
	}

	public void setTE(final double[] te)
	{
		t2fitter.setTE(te);
	}

    public int getNSpectralChannels() { return t2fitter.getNParams(); }

    protected void dealWithData(final T2Distribution dist,
            final double chi2_temp, int row, int col, int slice)
    {
        chi2.setData(chi2_temp, row,col,slice,0);
        svol.setData(dist.getAmplitudes(), row,col,slice);
 
        t2s = dist.getT2s();
    }
 
    protected void dealWithAlpha(final double alpha,
            int row, int col, int slice)
    {
        alpha_map.setData(alpha, row,col,slice,0);
    }

	/** Save the resulting spectral volume and chi2 map. 
	 */
	public void saveData(final String basename, final Vector header, final String comment)
	{
		String outfilename = null;

		if( verbose > 1 )
		{
			System.out.println("Saving based on basename " + basename);
		}

		/*
		 *  Save the spectral volume.
		 */
		BFFSpectral bfs = new BFFSpectral(header);
		bfs.setT2(t2s);
		bfs.setSpectralVolume(svol);
		bfs.addComment(comment);

		outfilename = basename + "_t2.bff.gz";
		bfs.write(outfilename, MRIFile.DATATYPE_FLOAT );

		/*
		 *  Save the chi2 volume.
		 */
		BFF bff_chi2 = new BFF(header);
		bff_chi2.setDataType( MRIFile.DATATYPE_FLOAT );
		bff_chi2.setMCVolume(chi2);
		bff_chi2.addComment("T2Map L1 chi2 map");

		outfilename = basename + "_t2_chi2.bff.gz";
		bff_chi2.write(outfilename, MRIFile.DATATYPE_FLOAT );
	}
}
