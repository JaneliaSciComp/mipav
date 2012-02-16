package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit.LogFit;

public class TECorrect {

	private double[] new_echo_times = null;
	private int verbose=0;
		
	public TECorrect(final double[] new_echo_times_in)
	{
		new_echo_times = new double[ new_echo_times_in.length ];
		System.arraycopy(new_echo_times_in, 0, new_echo_times, 0, 
						 new_echo_times_in.length);
	}

	public int getVerbose()
	{
		return verbose;
	}

	public void setVerbose(int verbose)
	{
		this.verbose = verbose;
	}

	public void monoCorrect(MCVolume mmmm, final double[] echo_times)
	{
		double[] decay = null;
		int ii = 0;
		double[] params = new double[2];

		LogFit lf = new LogFit();

		for(int slice=0; slice<mmmm.getNSlices(); slice++) {
			for(int row=0; row<mmmm.getNRows(); row++) {

				if( verbose > 0 )
				{
					System.out.print("\rSlice " + (slice+1) + "/" + mmmm.getNSlices());
					System.out.print("Row " + (row+1) + "/" + mmmm.getNRows());
				}

				for(int col=0; col<mmmm.getNCols(); col++) {

					//
					//  Get the decay curve.
					//
					decay = mmmm.getChannelDouble(row,col,slice);

					if( decay[0] > 100 ) {
						// Same number of echoes and channels??
						if( decay.length != echo_times.length ) {
							System.out.println("The number of echoes (" + decay.length +
											   ") is not the same as the number of " +
											   "new echo times (" + echo_times.length);
							System.exit(-1);
						}

						// At least one channel??
						if( decay.length <= 1 ) {
							System.out.println("I can't fit to less than two points!!!");
							System.exit(-1);
						}

						if( verbose > 2 ) 
						{
							System.out.println("-----------------------------");
							System.out.println("TE\tSI");
							for(int kk=0; kk<decay.length;kk++)
							{
								System.out.println(echo_times[kk] + "\t" + decay[kk]);
							}
						}

						//  DO the fit.
						lf.doFit(echo_times, decay);

						// params[0] = intercept
						// params[1] = slope
						params = lf.getParams();

						//  Apply the fit.
						for(ii=0; ii<mmmm.getNChannels(); ii++) {
							decay[ii] = Math.exp( params[1]*new_echo_times[ii] + 
							                      params[0] );
						}

						if( verbose > 2 ) 
						{
							System.out.println("Slope intercept is " + params[1] + 
							                    "  " + params[0]);
							System.out.println("New TE\tSI");
							for(int kk=0; kk<decay.length;kk++)
							{
								System.out.println(new_echo_times[kk] + "\t" + 
								                    decay[kk]);
							}
						}

						//
						//  Set it back into the volume.
						//
						mmmm.setChannel( decay, row, col, slice );
					}
				}
			}
		}
	}

}
