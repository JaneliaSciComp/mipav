package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *  Compute the median over a slice.
 *
 */
public class MedianFilter extends BlockProcess
{
	// To save on method calls
	private int nRows;
	private int nCols;
    private int nSlices;
    private int nChannels;

	// Cache data
	private float data[];  // temp storage for crude median algorithm

	public MedianFilter()
	{
		super(3);

		data = new float[ blockSizeRows * blockSizeCols * blockSizeSlices * blockSizeChannels ];
	}

	/**
	 *  Default constructor.
	 */
	public MedianFilter(int N)
	{
		super(N);

		data = new float[ blockSizeRows * blockSizeCols * blockSizeSlices * blockSizeChannels ];
	}

	final protected float filter(final MCVolume v)
    {
		int mm=0;
		for(int ii=0; ii< blockSizeRows; ii++) 
			for(int jj=0; jj< blockSizeCols; jj++) 
				for(int kk=0; kk< blockSizeSlices; kk++) 
					for(int ll=0; ll< blockSizeChannels; ll++) 
						{ data[mm] = v.getData(ii,jj,kk,ll);mm++; }//System.out.println(data[mm-1]);}

		java.util.Arrays.sort(data);
//		System.out.println("Median is " + data[data.length/2]);
		return data[data.length/2];
    }
}

