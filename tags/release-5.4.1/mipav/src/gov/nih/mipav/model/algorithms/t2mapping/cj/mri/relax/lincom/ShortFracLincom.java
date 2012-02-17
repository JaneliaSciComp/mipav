package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.lincom;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class ShortFracLincom
{
	private double threshold = 200.0; 
	private LinComCoefs coefs = null;
	private int verbose = 1;

	public ShortFracLincom(final double[] te)
	{
		chooseCoefs(te);
	}

	public void setVerbose( int verbose )
	{
		this.verbose = verbose;
	}

	final private boolean near( final double a, final double target )
	{
		boolean isnear = true;

		if( Math.abs( a-target ) <= 1.0 )
		{
			isnear = true;
		}
		else
		{
			isnear = false;
		}

		return isnear;
	}

	private void chooseCoefs(final double[] te)
	{
		if( te.length == 4 )
		{
			if( near(te[0], 10.0) && near(te[1], 33.0) &&
			    near(te[2], 124.0) && near(te[3], 272.0) )
			{
				if( verbose > 0 )
				{
					System.out.println("t2lincom: Using coefficients for 4 echo.");
				}
				coefs = new LinComCoefs10_33_124_272();
			}
			else
			{
				System.out.print("t2lincom: Haven't defined coefficients given ");
				System.out.println("that set of TE times.");
				System.exit(-1);
			}

		}
		else if( te.length == 32 )
		{
			boolean correct = true;
			for(int ii=0; ii<te.length; ii++)
			{
				if( !near(te[ii], (double)((ii+1)*10.0) ) )
				{
					correct = false;
				}
			}

			if( correct ) 
			{
				coefs = new LinComCoefs10x10x320();
			}
			else
			{
				System.out.print("t2lincom: Haven't defined coefficients given ");
				System.out.println("that set of TE times.");
				System.exit(-1);
			}
		}
		else if( te.length == 48 )
		{
			boolean correct = true;
			for(int ii=0; ii<32; ii++)
			{
				if( !near(te[ii], (double)((ii+1)*10.0) ) )
				{
					correct = false;
				}
			}

			for(int ii=32; ii<48; ii++)
			{
				if( !near(te[ii], (double)((ii+1)*10.0 + 40.0*(ii-32+1)) ) )
				{
					correct = false;
				}
			}

			if( correct ) 
			{
				coefs = new LinComCoefs10x10x320_370x50x1120();
			}
			else
			{
				System.out.print("t2lincom: Haven't defined coefficients given ");
				System.out.println("that set of TE times.");
				System.exit(-1);
			}
		}
		else
		{
			System.out.println("t2lincom: Coefficients are not defined for this.");
			System.exit(-1);
		}
	}

	public Volume calculateAll(final MCVolume vol)
	{
		Volume allout = new Volume(vol.getNRows(), vol.getNCols(), vol.getNSlices());
		double temp_all = 0.0;

		double[] all_coef = coefs.getAllT2Coefs();

		for(int slice=0; slice<vol.getNSlices(); slice++) 
		{
			for(int row=0; row<vol.getNRows(); row++) 
			{
				for(int col=0; col<vol.getNCols(); col++) 
				{
					temp_all = 0.0;
					for(int channel=0; channel < vol.getNChannels(); channel++)
					{
						temp_all += vol.getData(row,col,slice,channel) * all_coef[channel];	
					}

					allout.setData((float)temp_all, row, col, slice);
				}

			}

		}

		return allout;
	}

	public Volume calculateShort(final MCVolume vol)
	{
		Volume fracout = new Volume(vol.getNRows(), vol.getNCols(), vol.getNSlices());
		double temp_short = 0.0;

		double[] short_coef = coefs.getShortT2Coefs();

		for(int slice=0; slice<vol.getNSlices(); slice++) 
		{
			for(int row=0; row<vol.getNRows(); row++) 
			{
				for(int col=0; col<vol.getNCols(); col++) 
				{
					temp_short = 0.0;
					for(int channel=0; channel < vol.getNChannels(); channel++)
					{
						temp_short += vol.getData(row,col,slice,channel) * short_coef[channel];	
					}

					fracout.setData((float)temp_short, row, col, slice);
				}

			}

		}

		return fracout;

	}

	public Volume calculateShortFrac(final MCVolume vol)
	{
		Volume fracout = new Volume(vol.getNRows(), vol.getNCols(), vol.getNSlices());
		double temp_short = 0.0;
		double temp_all = 0.0;

		double[] short_coef = coefs.getShortT2Coefs();
		double[] all_coef = coefs.getAllT2Coefs();

		for(int slice=0; slice<vol.getNSlices(); slice++) 
		{
			for(int row=0; row<vol.getNRows(); row++) 
			{
				for(int col=0; col<vol.getNCols(); col++) 
				{
					if( vol.getData(row,col,slice,0) > threshold )
					{
						temp_short = 0.0;
						temp_all = 0.0;
						for(int channel=0; channel < vol.getNChannels(); channel++)
						{
							temp_short +=vol.getData(row,col,slice,channel)*short_coef[channel];
							temp_all += vol.getData(row,col,slice,channel) * all_coef[channel];
						}

						fracout.setData((temp_short / temp_all), row, col, slice);

//						if( (temp_short/temp_all) > 1.0 ) 
//						{
//							fracout.setData((float)(1.0), row, col, slice);
//						}
//
//						if( (temp_short/temp_all) < 0.0 ) 
//						{
//							fracout.setData((float)(0.0), row, col, slice);
//						}
					}
					else
					{
						fracout.setData((float)0.0, row, col, slice);
					}
				}

			}

		}

		return fracout;
	}
}
