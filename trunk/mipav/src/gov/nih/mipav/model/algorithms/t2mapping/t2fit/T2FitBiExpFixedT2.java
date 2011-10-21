package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;

public class T2FitBiExpFixedT2 extends T2Fit
{
	/* 
	 *   Two things necessary for the fit.
	 */
	private double t2_1 = 0.0;
	private double t2_2 = 0.0;

	private double total1 = 0.0;
	private double total2 = 0.0;
	private double total3 = 0.0;
	private double[] temp1=null;
	private double[] temp2=null;
	private double is2 = 0.0;
	private double tt;
	private double e1, e2;

	private double[] ampout = new double[2];
	private double[] t2out = new double[2];

	public T2FitBiExpFixedT2()
	{
	}

	public T2FitBiExpFixedT2(final double[] te)
	{
		setTE(te);
	}

	public int getNParams() { return 2; }

    public void setParameters( final double[] params )
	{

		if( params == null || params.length != 2 )
		{
			System.out.println("T2FitBiExpFixedT2: Must pass in two parameters.");
			System.exit(-1);
		}

		t2_1 = params[0];
		t2_2 = params[1];

		initialize();
	}

	public void setAlpha(double alpha)
	{
		System.out.print("Variable flip angle is not implemented for");
		System.out.println(" T2FitBiExpFixedT2.");
	}

	private void initialize()
	{
		if( te != null && stds != null && t2_1 != 0.0 && t2_2 != 0.0 )
		{
			if( temp1 == null ) temp1 = new double[ te.length ];
			if( temp2 == null ) temp2 = new double[ te.length ];

			/* 
			 *  Calculate D.
			 */
			total1 = 0.0;
			total2 = 0.0;
			total3 = 0.0;
			for(int ii=0; ii<te.length; ii++)
			{
				is2 = 1.0/(stds[ii]*stds[ii]);

				e1 = Math.exp(-te[ii]/t2_1);
				e2 = Math.exp(-te[ii]/t2_2);

				temp1[ii] = is2*e1;
				temp2[ii] = is2*e2;

				total1 += temp1[ii]*e1;
				total2 += temp2[ii]*e2;
				total3 += is2*e1*e2;
			}
		}
	}

	public double solveSimple(final double data[], T2Distribution dist)
	{
		double iD = 0.0;

		initialize();

		if( ampout == null ) throw new NullPointerException("ampout is null");
		if( t2out == null ) throw new NullPointerException("t2out is null");


		/* 
		 *  Calculate D.
		 */
		double temp4 = 0.0;
		double temp5 = 0.0;

		for(int ii=0; ii<te.length; ii++)
		{
			temp4 += data[ii]*temp1[ii];
			temp5 += data[ii]*temp2[ii];
		}

		iD = 1./(total1*total2-total3*total3);

		ampout[0] = iD*(total2*temp4 - total3*temp5 );
		ampout[1] = iD*(total1*temp5 - total3*temp4 );
		t2out[0] = t2_1;
		t2out[1] = t2_2;

		dist.setAmplitudes(ampout);
		dist.setT2s(t2out);

		return getChi2T(data, ampout);
	}

	/** Calculate the chi2 based on the equation in Ken's work.
	 *
	 */
    public double getChi2T(final double[] b, final double[] ampout)
    {
        double chi2 = 0.0;

        for(int ii=0; ii<te.length; ii++)
        {
			double tt = ampout[0]*Math.exp(-te[ii]/t2_1);
			tt += ampout[1]*Math.exp(-te[ii]/t2_2);

			chi2 += (tt-b[ii])*(tt-b[ii])/(stds[ii]*stds[ii]);
        }

        return chi2;
    }

}
