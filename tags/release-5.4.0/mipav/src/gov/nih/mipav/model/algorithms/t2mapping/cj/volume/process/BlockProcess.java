package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *  Block processing over an NxN size block in 2D, 3D or 4D.
 *
 */
public abstract class BlockProcess {

// To save on method calls
	private int nRows;
	private int nCols;
    private int nSlices;
    private int nChannels;

	// Define the size of the block.
	protected int blockSizeRows = 1;
	protected int blockSizeCols = 1;
	protected int blockSizeSlices = 1;
	protected int blockSizeChannels = 1;

	MCVolume data = null;

	/**
	 *  Default constructor.
	 */
	public BlockProcess(int N)
	{
		blockSizeRows = N;
		blockSizeCols = N;
		blockSizeSlices = 1;
		blockSizeChannels = 1;

		data = new MCVolume(blockSizeRows, blockSizeCols, 
						    blockSizeSlices, blockSizeChannels);
	}

	/**
	 *   This must be defined in all subclasses and will do the appropriate
	 *   type of filtering as required.
	 */
	abstract float filter(MCVolume v);

	/**
	 *  Process a multi-channel volume slice by slice.
	 */
	public void process2D(MCVolume v, int niters)
	{
		for(int ii=0; ii<niters; ii++)
			process2D(v);
	}

	/**
	 *  Process a multi-channel volume slice by slice.
	 */
	public void process2D(MCVolume v)
	{
		for(int ii=0; ii<v.getNSlices(); ii++)
			for(int jj=0; jj<v.getNChannels(); jj++)
				process2D(v.getSlice(ii,jj));
	}

	/**
	 *  Process a volume slice by slice.
	 */
	public void process2D(Volume v)
	{
		for(int ii=0; ii<v.getNSlices(); ii++)
			process2D(v.getSlice(ii));
	}

	/**
	 *  Block process a single slice.
	 */
	public void process2D(Slice slice)
	{
		Slice stemp = new Slice(slice);

		//  Run over every row and column
		for(int ii=0; ii<slice.getNRows(); ii++)
		{
			for(int jj=0; jj<slice.getNCols(); jj++)
			{
//				System.out.println("======================");
//				System.out.println("Row/Col " + (ii+1) + " " + (jj+1));
				// Now fill in the parts that are real.
				for(int kk=-blockSizeRows/2; kk<=blockSizeRows/2; kk++)
				{
					for(int ll=-blockSizeCols/2; ll<=blockSizeCols/2; ll++)
					{
						if( (ii+kk) >= 0 && (jj+ll) >= 0 &&
						    (ii+kk) < slice.getNRows() &&
						    (jj+ll) < slice.getNCols() )
						{
							data.setData(stemp.getData(ii+kk, jj+ll), kk+blockSizeRows/2, ll+blockSizeCols/2, 0, 0);
//							System.out.println("setting to " + data.getData(kk+blockSizeRows/2,ll+blockSizeCols/2,0,0));
						}
						else 
						{ 	
							data.setData((float)0.0, (kk+blockSizeRows/2), (ll+blockSizeCols/2), 0, 0);
//							System.out.println("setting to (0) " + data.getData((kk+blockSizeRows/2),(ll+blockSizeCols/2),0,0));
						}
					}
				}
//				System.out.println("----------------------");

				// Now compute the median
				slice.setData( filter(data), ii, jj);
			}
		}
	}
}

