package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class SusanFilter {

    private double ksqr;  // kappa squared
    private double ssqr;  // sigma squared
    private int r;

// To save on method calls
	private int nRows;
	private int nCols;
    private int nSlices;
    private int nChannels;

// Cache data
    private double t_exp[];
	private float blah[];  // temp storage for crude median algorithm
	private final int SIZE_EXPTABLE=65536;

	public SusanFilter(double kappa_in, double sigma_in, int r_in)
	{
		ksqr = kappa_in * kappa_in;
		ssqr = sigma_in * sigma_in;
		r = r_in;

		blah = new float[9];
	}

	private void susan_table()
	{
		final double K2 = 2.0/ksqr;
		double t, t0;
		int i;

        t_exp = new double[SIZE_EXPTABLE];

		t_exp[0] = 1.0;
		
		for(i=1, t=0.5*K2, t0=3.0*t; i<SIZE_EXPTABLE; t+=t0, t0+=K2)
			{ t_exp[i++] = Math.exp(-t); }
	}

	public void susan3d(MCVolume mmmm, int iters)
	{
        nRows = mmmm.getNRows();
        nCols = mmmm.getNCols();
        nSlices = mmmm.getNSlices();
        nChannels = mmmm.getNChannels();

        System.out.println("SUSAN 3-D is not implemented yet!");

		for(int ii=0; ii<iters; ii++) {
			susan3d(mmmm);
		}
	}

	private void susan3d(MCVolume mmmm)
	{
    }

	public void susan2d(MCVolume mmmm, int iters)
	{
        nRows = mmmm.getNRows();
        nCols = mmmm.getNCols();
        nSlices = mmmm.getNSlices();
        nChannels = mmmm.getNChannels();

		for(int ii=0; ii<iters; ii++) {
			susan2d(mmmm);
		}
	}

	private void susan2d(MCVolume mmmm)
	{
		final int sliceSize = nRows * nCols;
        final int totalSlice = sliceSize * nChannels;
		double[] num = new double[totalSlice];
		double[] den = new double[sliceSize];

        Slice[] mySlices = new Slice[nChannels];

        float[][] myRows = new float[nChannels][],
             myOtherRows = new float[nChannels][];

		double t0, t1, t2, t3, k_denom = 1.0/(ksqr*nChannels);

		int channel, slice, col, row;
		int i, j, k, ind, ind0;
		int j_min, colm, colM;
		int pos0, pos1, offset;

		for(slice=0; slice<nSlices; slice++) {
			// Precompute gradient table and apply exponential weighting
			java.util.Arrays.fill(num, 0);
			java.util.Arrays.fill(den, 0);

			// This loop uses tricky logic to exploit symmetry in the
			// (2r+1)x(2r+1) kernel and cut looping in half.
			j_min = 1;

			// Cache all channels of a given slice
			for(channel=0; channel < nChannels; channel++)
			{
				mySlices[channel] = mmmm.getSlice(slice, channel);
			}

			for(i=0; i<=r; i++)
			{
				for(j=j_min; j<=r; j++)
				{
					t3 = (double)(i*i + j*j)/(2*ssqr);

					// Precompute upper and lower bounds for col
					colm = ((j<0)?-j:0); colM = nCols - ((j>0)?j:0);

					for(row=0; row<nRows-i; row++)
					{
						// Scarf rows from each channel
						for(k=0; k<nChannels; k++)
						{
							myRows[k] = mySlices[k].getRow(row);
							myOtherRows[k] = mySlices[k].getRow(row+i);
						}

						// Precompute flat indices into den
						pos0 = row*nCols+colm; pos1 = (row+i)*nCols+colm+j;

						for(col=colm; col<colM; col++, pos0++, pos1++)
						{
							t2 = 0;

							for(channel=0; channel<nChannels; channel++)
							{
								t0 = myRows[channel][col];
								t1 = myOtherRows[channel][col+j];
								t2 += (t0-t1)*(t0-t1);
							}

							t2 *= k_denom;
							t2 = Math.exp(-t3-t2);

							den[pos0] += t2; den[pos1] += t2;

							for(channel=0, offset=0; channel<nChannels;
                                channel++, offset+=sliceSize)
							{
								t0 = (double) myRows[channel][col];
								t1 = (double) myOtherRows[channel][col+j];

								num[offset+pos0] += t1*t2;
								num[offset+pos1] += t0*t2;
							}

						}
					}
				}
				j_min = -r;  // Only needs to happen on first iteration, but
							 // an if statement is more expensive.
			}
							
			// Have to be REALLY careful here to make sure the row cache
			// is not affected by the incremental setRows().

			float[] tempRow = new float[nCols];

			myRows = new float[3][];  // row window of radius 1

			for(channel=0,ind=0; channel<nChannels; channel++)
			{
				Slice s = mySlices[channel]; ind0 = 0;

				// Prefill row cache
				java.util.Arrays.fill(myRows[0]=new float[nCols], 0);
				java.util.Arrays.fill(myRows[1]=new float[nCols], 0);
				myRows[2] = s.getRow(0);

				for(row=0; row<nRows; row++) {
					// Rotate row cache and load next
					myRows[0] = myRows[1]; myRows[1] = myRows[2];
					if (row<nRows-1)
	 					myRows[2] = s.getRow(row+1);
					else
						java.util.Arrays.fill(myRows[2], 0);

					for(col=0; col<nCols; col++,ind++,ind0++) {
						if (den[ind0] > 0.0)
							tempRow[col] = (float)(num[ind]/den[ind0]);
						else
							tempRow[col] = median(myRows, col);
					}
                    s.setRow(tempRow, row);
				}
			}
		}
	}

	private float median(float[][] rows, int col)
    {
		int i,k;

		for(i=k=0;i<3;i++) {
			blah[k++] =       (col>0)?rows[i][col-1]:0;
			blah[k++] =               rows[i][col];
			blah[k++] = (col<nCols-1)?rows[i][col+1]:0;
		}
		
		java.util.Arrays.sort(blah);
		return blah[4];
    }
}

/*
	private void susan2d_perChannel(float[] t_exp)
	{
		int sliceSize = nCols*nRows;
		double[] num = new double[sliceSize];
		double[] den = new double[sliceSize];
		double t2, t3;
		int t0,t1;

		int echo, slice, col, row;
		int ind, ind0;
		int i,j;
		int j_min, colm, colM;
		int pos0, pos1;

		med.medfilt();
		// Precompute gradient table and apply exponential weighting

		for(echo=0,ind=0; echo<getNChannels(); echo++)
		{
		for(slice=0; slice<getNSlices(); slice++)
		{
			for(int ii=0; ii<sliceSize; ii++) {
				num[ii] = den[ii] = 0.0;
			}

			ind0 = 0; j_min = 1;

			for(i=0;i<=r;i++)
			{
				for(j=j_min;j<=r;j++)
				{
					t3 = exp(-(double)(i*i + j*j)/(2*ssqr));

					colm = -j*(j<0); colM = nCols - j*(j>0);

					for(row=0; row<nRows-i; row++)
					{
						for(col=colm; col<colM; col++)
						{
							pos0 = row*nCols + col;
							pos1 = (row+i)*nCols + (col+j);
							t0 = (int) data[ind + pos0];
							t1 = (int) data[ind + pos1];

							if (t0 > t1)
								t2 = t_exp[t0-t1] * t3;
							else
								t2 = t_exp[t1-t0] * t3;

							den[pos0] += t2; den[pos1] += t2;
							num[pos0] += t2*t1; num[pos1] += t2*t0;
						}
					}
				}
				j_min = -r;
			}

			ind = slice*sliceSize; ind0 = 0;

			for(row=0; row<nRows; row++) {
				for(col=0; col<nCols; col++,ind++,ind0++) {
					if (den[ind0] > 0.0)
						data[ind] = (T)((double)num[ind0]/den[ind0]);
					else
						data[ind] = med.data[ind];
				}
			}
		}
		}
	}
*/
