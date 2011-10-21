package gov.nih.mipav.model.algorithms.t2mapping.t2map;

import java.util.Vector;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.T2FitNNLS;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;

/** T2MapClass is responsible to calculate the T2 spectrum through a volume
 *  of points, to contain the results and to save the results.
 */
public class T2MapNNLS extends T2Map
{
//	/** The T2Fit routine to use */
//	private T2FitNNLS t2fitter = null;

//	public static int ORDER_ROWCOL = 1;
//	public static int ORDER_COLROW = 2;
//	public int order = ORDER_ROWCOL;

	private double[] t2s = null;
	
	public T2MapNNLS()
	{
		t2fitter = new T2FitNNLS();
	}

	public int getNSpectralChannels() { return t2fitter.getNParams(); }

	public void setEstimateFlip( boolean estimateFlip )
	{
		this.estimateFlip = estimateFlip;
		t2fitter.setEstimateFlip( estimateFlip );
	}

	public void setParameters( double[] params )
	{
		t2fitter.setParameters(params);
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

		/*
		 *  Save the spectral volume.
		 */
		for(int ii=0; ii<header.size(); ii++)
			System.out.println((String)header.get(ii));

		System.out.println("There are " + t2s.length + " t2s.");

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
		bff_chi2.addComment("T2Map nnls chi2 map");

		outfilename = basename + "_t2_chi2.bff.gz";
		bff_chi2.write(outfilename, MRIFile.DATATYPE_FLOAT );

		/*
		 *  Save the chi2 volume.
		 */
		if( estimateFlip )
		{
			BFF bff_am = new BFF(header);
			bff_am.setDataType( MRIFile.DATATYPE_FLOAT );
			bff_am.setMCVolume(alpha_map);
			bff_am.addComment("T2Map nnls estimate flip angle map");

			outfilename = basename + "_t2_alphas.bff.gz";
			bff_am.write(outfilename, MRIFile.DATATYPE_FLOAT );
		}
	}
}
