package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import java.nio.*;
import java.nio.channels.*;
import java.io.*;

public class AnisotropicFilter {

	private double kappa;

// To save on method calls
	private int nCols;
	private int nRows;
	private int nSlices;
	private int nChannels;

// Cache data
    private double t_exp[];
//	private double cSN[];
//	private double cEW[];
	TempFloatBuffer cEW = null;
	TempFloatBuffer cSN = null;
    private final int SIZE_EXP_TABLE = 65536;

	public AnisotropicFilter(double kappa_in)
	{
		kappa = kappa_in;
	}

	private void aniso_table(double kappa)
	{
	   final double K2 = 2.0/(kappa*kappa);
	   double t, t0;
	   int i;

	   t_exp = new double[SIZE_EXP_TABLE];

       java.util.Arrays.fill(t_exp, 0.0);

	   for(i=1, t=K2*0.5, t0=3.0*t; i<SIZE_EXP_TABLE; i++, t+=t0, t0+=K2)
	   {
	   	  if ((t_exp[i] = Math.exp(-t)*i) == 0.0) return;
	   }
	}

	public void aniso2d(MCVolume mmmm, int iters)
	{
        if (mmmm.getNChannels() == 1)
          { aniso2d_perChannel(mmmm,iters); return; }

		nCols = mmmm.getNCols();
		nRows = mmmm.getNRows();
		nSlices = mmmm.getNSlices();
		nChannels = mmmm.getNChannels();

		cSN = new TempFloatBuffer(nCols*nRows);
		cEW = new TempFloatBuffer(nCols*nRows);
//		cSN = new double[nCols*nRows];
//		cEW = new double[nCols*nRows];

		for(int ii=0; ii<iters; ii++) {
			aniso2d(mmmm);
		}

		cSN = cEW = null;
	}

    private void aniso2d(MCVolume mmmm)
    {
		final int sliceSize = nCols*nRows;
        final int totalSlice = sliceSize*nChannels;

        int col, row, slice, channel;
        int ind, ind0, ind1, ind2;
        double t0, K_denom = 1.0/(kappa*kappa*nChannels);

//        float dSN[] = new float[totalSlice];
//        float dEW[] = new float[totalSlice];
		TempFloatBuffer dSN = new TempFloatBuffer(totalSlice);
		TempFloatBuffer dEW = new TempFloatBuffer(totalSlice);

        float[] myRow, myOtherRow;

        Slice s, s2;

        for(slice=0; slice < nSlices; slice++) {
			cSN.fillZeros();
			cEW.fillZeros();
//	        java.util.Arrays.fill(cSN, 0.0);
//	        java.util.Arrays.fill(cEW, 0.0);
	        
	        for(channel=0,ind=0; channel<nChannels; channel++) {
	            s = mmmm.getSlice(slice, channel);
                myOtherRow = s.getRow(0);
	            ind0 = 0;

	            for(row=0; row<nRows; row++) {
	                myRow = myOtherRow;
                    if (row < nRows-1) { myOtherRow = s.getRow(row+1); }
	
	                for(col=0; col<nCols; col++, ind++, ind0++) {
	                    if (col<nCols-1) {
	                      t0 = myRow[col+1] - myRow[col];
	                      dSN.put(ind, (float)t0);
	                      cSN.put(ind0, (float)(cSN.get(ind0) + t0*t0)); 
						}
	
	                    if (row<nRows-1) {
	                      t0 = myOtherRow[col] - myRow[col];
	                      dEW.put(ind, (float)t0);
	                      cEW.put(ind0, (float)(cEW.get(ind0) + t0*t0)); 
						}
	                }
	            }
	        }
	
	        for(ind=0;ind<sliceSize;ind++)
	        {
	            cSN.put(ind, (float)Math.exp(-cSN.get(ind) * K_denom ));
	            cEW.put(ind, (float)Math.exp(-cEW.get(ind) * K_denom ));
	        }
	
	        for(channel=0,ind=0,ind2 = -nCols; channel<nChannels; channel++) {
	            ind0 = 0; ind1 = -nCols;
                s = mmmm.getSlice(slice, channel);

	            for(row=0; row<nRows; row++) {
                    myRow = s.getRow(row);

	                for(col=0; col<nCols; col++, ind++, ind0++, ind1++, ind2++) {
	                    t0 = 0.0;
	
	                    if (col<nCols-1) t0 += dSN.get(ind)*cSN.get(ind0);
	                    if (col>0) t0 -= dSN.get(ind-1)*cSN.get(ind0-1);
      	
	                    if (row<nRows-1) t0 += dEW.get(ind)*cEW.get(ind0);
	                    if (row>0) t0 -= dEW.get(ind2)*cEW.get(ind1);
	
	                    myRow[col] = (float)(myRow[col] + t0 * 0.2);
	                }
					s.setRow(myRow, row);
	            }
				//mmmm.setSlice(s, slice, channel);
	        }
        }
    }

	public void aniso2d_perChannel(MCVolume mmmm, int iters)
	{
		nCols = mmmm.getNCols();
		nRows = mmmm.getNRows();
		nSlices = mmmm.getNSlices();
		nChannels = mmmm.getNChannels();

		cSN = new TempFloatBuffer(nCols*nRows);
		cEW = new TempFloatBuffer(nCols*nRows);

	    aniso_table(kappa);

		for(int ii=0; ii<iters; ii++) {
			aniso2d_perChannel(mmmm);
		}

		cSN = cEW = null;
	}

	private void aniso2d_perChannel(MCVolume mmmm)
	{
		double t, t0, t1;

		int slice, col, row, channel=0;
		int ind, ind0, ind2, start_ind;

	    Slice s;
        float[] myRow, myOtherRow;

		// Precompute gradient table and apply exponential weighting

		for(channel=0,ind=0; channel<nChannels;channel++) {
			//start_ind = ind; ind0 = 0; ind2 = ind + nCols;
			for(slice=0; slice<nSlices; slice++) {
				ind0 = 0;
				s = mmmm.getSlice(slice, channel);
                myOtherRow = s.getRow(0);

				for(row=0; row<nRows; row++) {
	                myRow = myOtherRow;
                    if (row < nRows-1) { myOtherRow = s.getRow(row+1); }

					for(col=0; col<nCols; col++, ind0++) {
						//t0 = mmmm.getData(col,row,slice, channel);
						t0 = myRow[col];

						if (col<nCols-1) {
						  t1 = myRow[col+1];
						if (t1 > t0)
						  cSN.put(ind0, (float)(t_exp[(int)(t1-t0)]));
						else
						  cSN.put(ind0, (float)(-t_exp[(int)(t0-t1)]));
						}

						if (row<nRows-1) {
						  t1 = myOtherRow[col];
						if (t1 > t0)
						  cEW.put(ind0, (float)(t_exp[(int)(t1-t0)]));
						else
						  cEW.put(ind0, (float)(-t_exp[(int)(t0-t1)]));
						}
					}
				}

				ind0 = 0; ind2 = -nCols;
				for(row=0; row<nRows; row++) {
					myRow = s.getRow(row);

					for(col=0; col<nCols; col++,ind0++,ind2++) {
						t = 0.0;

						if (col<nCols-1) t += cSN.get(ind0);
						if (col>0)       t -= cSN.get(ind0-1);

						if (row<nRows-1) t += cEW.get(ind0);
						if (row>0)       t -= cEW.get(ind2);

						myRow[col] += (float) t*0.2;
					}
					s.setRow(myRow, row);
				}
				//mmmm.setSlice(slice,channel)
			} // slice
		} // channel
	}

	public void aniso3d(MCVolume mmmm, int numIterations)
	{
        if (mmmm.getNChannels() == 1)
          { aniso3d_perChannel(mmmm,numIterations); return; }

		nCols = mmmm.getNCols();
		nRows = mmmm.getNRows();
		nSlices = mmmm.getNSlices();
		nChannels = mmmm.getNChannels();

		for(int ii=0; ii<numIterations; ii++) {
			aniso3d(mmmm);
		}

	}

    private void aniso3d(MCVolume mmmm)
    {
        final int sliceSize = nCols * nRows;
        final int volSize = sliceSize * nSlices;
        final int totalSize = volSize * nChannels;

        int col, row, slice, channel;
        int ind, ind0, ind1, ind2, ind3, ind4;

        double t0, K_denom = 1.0/(kappa*kappa*nChannels);

		TempFloatBuffer cEW = new TempFloatBuffer(volSize);
		TempFloatBuffer cSN = new TempFloatBuffer(volSize);
		TempFloatBuffer cUD = new TempFloatBuffer(volSize);

		TempFloatBuffer dSN = new TempFloatBuffer(totalSize);
		TempFloatBuffer dEW = new TempFloatBuffer(totalSize);
		TempFloatBuffer dUD = new TempFloatBuffer(totalSize);

        float[] myRow, myOtherRow, myThirdRow = null;

        Slice s, s2;

		for(channel=0,ind=0; channel<nChannels; channel++) {
			ind0 = 0;
			s2 = mmmm.getSlice(0, channel);
           
            for(slice=0; slice < nSlices; slice++) {
				s = s2;
				myOtherRow = s.getRow(0);

                if (slice < nSlices-1) s2 = mmmm.getSlice(slice+1,channel);

				for(row=0; row<nRows; row++) {
					myRow = myOtherRow;
					if (row < nRows-1) { myOtherRow = s.getRow(row+1); }
                    if (slice < nSlices-1) { myThirdRow = s2.getRow(row); }

					for(col=0; col<nCols; col++, ind++, ind0++) {
						if (col<nCols-1) {
						  t0 = myRow[col+1] - myRow[col];
						  dSN.put(ind, (float)t0);
						  cSN.put(ind0, (float)(cSN.get(ind0)+t0*t0));
					//	  cSN[ind0] += t0*t0; 
						}

						if (row<nRows-1) {
						  t0 = myOtherRow[col] - myRow[col];
						  dEW.put(ind, (float)t0);
						  cEW.put(ind0, (float)(cEW.get(ind0)+t0*t0));
						}

						if (slice<nSlices-1) {
						  t0 = myThirdRow[col] - myRow[col];
						  dUD.put(ind, (float)t0);
						  cUD.put(ind0, (float)(cUD.get(ind0)+t0*t0));
						}
					}
				}
            }
		}

		for(ind=0; ind<volSize; ind++)
		{
			cSN.put(ind, (float) Math.exp( -cSN.get(ind) * K_denom ));
			cEW.put(ind, (float) Math.exp( -cEW.get(ind) * K_denom ));
			cUD.put(ind, (float) Math.exp( -cUD.get(ind) * K_denom ));
		}

        ind = 0; ind2 = -nCols; ind4 = -sliceSize;
		for(channel=0; channel < nChannels; channel++) {
			ind0 = 0; ind1 = -nCols; ind3 = -sliceSize;

            for(slice=0; slice<nSlices; slice++) {
				s = mmmm.getSlice(slice, channel);

				for(row=0; row<nRows; row++) {
					myRow = s.getRow(row);

					for(col=0; col<nCols; col++, ind++,ind0++,ind1++,ind2++,ind3++,ind4++) {
						t0 = 0.0;

						if (col<nCols-1) t0 += dSN.get(ind)*cSN.get(ind0);
						if (col>0) t0 -= dSN.get(ind-1)*cSN.get(ind0-1);
		
						if (row<nRows-1) t0 += dEW.get(ind)*cEW.get(ind0);
						if (row>0) t0 -= dEW.get(ind2)*cEW.get(ind1);

						if (slice<nSlices-1) t0 += dUD.get(ind)*cUD.get(ind0);
						if (slice>0) t0 -= dUD.get(ind4)*cUD.get(ind3);

						myRow[col] = (float)(myRow[col] + t0 * 0.142857);
					}
					s.setRow(myRow, row);
				}
			//mmmm.setSlice(s, slice, channel);
            }
		}
    }

	public void aniso3d_perChannel(MCVolume mmmm, int numIterations)
	{
		nCols = mmmm.getNCols();
		nRows = mmmm.getNRows();
		nSlices = mmmm.getNSlices();
		nChannels = mmmm.getNChannels();

	    aniso_table(kappa);

		for(int ii=0; ii<numIterations; ii++) {
			aniso3d_perChannel(mmmm);
		}

	}

	//--------------------------------------------------------------
	//
	//  Anisotropic Filtering - 3D
	//
	private void aniso3d_perChannel(MCVolume mmmm)
	{
		final int sliceSize = nCols*nRows;
		final int volSize = sliceSize*nSlices;

		Slice s, s2;

		int channel,slice,row,col,ind,ind2,ind3;

		TempFloatBuffer cSN = new TempFloatBuffer(volSize);
		TempFloatBuffer cEW = new TempFloatBuffer(volSize);
		TempFloatBuffer cUD = new TempFloatBuffer(volSize);

		float[] myRow, myOtherRow, myThirdRow = null;

		double t;
		int t0, t1;

		for(channel=0; channel<nChannels; channel++) {
            s2 = mmmm.getSlice(0, channel);

			for(slice=0, ind=0; slice<nSlices; slice++) {
				s = s2;
				if (slice < nSlices-1) { s2 = mmmm.getSlice(slice+1, channel); }

                myOtherRow = s.getRow(0);

				for(row=0; row<nRows; row++) {
					myRow = myOtherRow;
					if (row < nRows-1) { myOtherRow = s.getRow(row+1); }
                    if (slice < nSlices-1) { myThirdRow = s2.getRow(row); }

					for(col=0; col<nCols; col++, ind++) {
						t0 = (int)myRow[col];

						if (col<nCols-1) {
						  t1 = (int)myRow[col+1];
						  if (t1 > t0) cSN.put(ind, (float)t_exp[t1-t0]);
						  else         cSN.put(ind, (float)-t_exp[t0-t1]);
						}

						if (row<nRows-1) {
						  t1 = (int)myOtherRow[col];

						  if (t1 > t0) cEW.put(ind, (float)t_exp[t1-t0]);
						  else         cEW.put(ind, (float)-t_exp[t0-t1]);
						}

						if (slice<nSlices-1) {
						  t1 = (int)myThirdRow[col];
						  if (t1 > t0) cUD.put(ind, (float)t_exp[t1-t0]);
						  else         cUD.put(ind, (float)-t_exp[t0-t1]);
						}
					}
				}
			}

			for(slice=0,ind=0,ind2=-nCols,ind3=-sliceSize; slice<nSlices; slice++) {
                s = mmmm.getSlice(slice,channel);

				for(row=0; row<nRows; row++) {
                    myRow = s.getRow(row);

					for(col=0; col<nCols; col++,ind++,ind2++,ind3++) {
						t = 0.0;

						if (col<nCols-1) t += cSN.get(ind);
						if (col>0)       t -= cSN.get(ind-1);

						if (row<nRows-1) t += cEW.get(ind);
						if (row>0)       t -= cEW.get(ind2);

						if (slice<nSlices-1) t += cUD.get(ind);
						if (slice>0)         t -= cUD.get(ind3);

						myRow[col] = (float)(myRow[col] + t*0.142857);
					}
					s.setRow(myRow, row);
				}
				//mmmm.setSlice(s, slice, channel);
			}
		}
	}

}

class TempFloatBuffer
{
	FloatBuffer fb = null;
	float[] ab = null;
	boolean use_array = false;
	int nelems = 0;

	final float get(int index)
	{
		if( use_array ) return ab[ index ];
		else return fb.get(index);
	}

	final void put(int index, float f)
	{
		if( use_array ) ab[ index ] = f;
		else fb.put(index, f);
	}

	TempFloatBuffer(int nelems)
	{
		if( nelems <= 256*256 )
		{
			use_array = true;
			ab = new float[ nelems ];
		}
		else
		{
			use_array = false;
			fb = setupFloatBuffer(nelems);
		}

		this.nelems = nelems;
	}

	public void fillZeros()
	{
		if( use_array ) 
		{
			for(int ii=0; ii<ab.length; ii++) ab[ii] = (float)0.0;
		}
		else
		{
			for(int ii=0; ii<nelems; ii++) fb.put(ii, (float)0.0);
		}

	}

	private FloatBuffer setupFloatBuffer(int nelems)
	{
		ByteBuffer bb = null;

		try {
			File tempFile = File.createTempFile("vol",null);
			tempFile.deleteOnExit();

			FileChannel fc = new RandomAccessFile(tempFile,"rw").getChannel();
			bb = fc.map(FileChannel.MapMode.READ_WRITE, 0, nelems * 4);
			bb.order(ByteOrder.nativeOrder());
		}
		catch(IOException e)
		{
			System.out.println("Could not create temp file: " + e);
			System.exit(-1);
		}

		return bb.asFloatBuffer();
	}

}
