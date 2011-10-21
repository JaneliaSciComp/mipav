package gov.nih.mipav.model.algorithms.t2mapping.t2fit;

import java.lang.*;
import java.util.Arrays;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.dcl1.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.mri.hennig.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.T2Distribution;

/** T2FitL1 will fit the given curve using an L1 minimziation.
 *
 *  The problem is setup with N (number of TE) slack variables
 *  and the goal is to minimize the slack variables.
 *
 */
public class T2FitL1 extends T2FitMultiT2
{
	protected double[] a = null;
	protected double[] bnorm = null;
    protected DCL1Wrapper dcl1 = null;
	protected double[] x = null;

	/** Constructor
	 *
	 */
	public T2FitL1()
	{
		dcl1 = new DCL1Wrapper();
	}

	/** Constructor
	 *
	 */
	public T2FitL1(final double[] te)
	{
		super(te);
		dcl1 = new DCL1Wrapper();
	}

	/** Constructor
	 *
	 */
	public T2FitL1(final double[] te, final double[] t2)
	{
		super(te, t2);
		dcl1 = new DCL1Wrapper();
	}

	public int getNParams()
	{
		return t2.length;
	}


	public double solveSimple(final double[] b, T2Distribution dist)
	{
		long start = 0;
		makeA();

		if( bnorm == null || bnorm.length != b.length )
		{
			bnorm = new double[ b.length ];
		}

		if( x == null || x.length != t2.length )
		{
			x = new double[ t2.length ];
		}

		/* 
		 *  Set b in DCL1.
		 */
        for(int ii=0; ii<b.length; ii++) {
            bnorm[ii] =  b[ii] / stds[ii];
        }

		if( verbose > 1 )
		{
			start = System.currentTimeMillis();
		}

		dcl1.solve(bnorm, x);

		if( verbose > 1 )
		{
			long end = System.currentTimeMillis();
			System.out.println("T2FitL1 solve: start-end " + (float)(end-start)/1000.0 + " seconds");
		}

		// Now copy oer the result.
		dist.setAmplitudes(x, x.length-1);
		dist.setT2s(t2, x.length-1);
		dist.setBaseline(x[x.length-1]);

        return getChi1(b,x);
	}

    public double getChi1(final double[] b, final double[] x)
    {
        double chi1 = 0.0;

        /*
         *  Compute chi2.
         */
        double temp = 0.0;

        for(int ii=0; ii<te.length; ii++)
        {
            temp = 0.0;
            for(int jj=0; jj<x.length; jj++)
            {
                temp += a[jj*b.length+ii]*stds[ii]*x[jj];
            }

            chi1 += Math.abs(temp-b[ii])/stds[ii];
        }

        return chi1;
    }

	public String toString()
	{
		String str = "";

		for(int ii=0;ii<te.length; ii++) 
		{
			str += te[ii] + ", ";
		}
		str += "\n";
		for(int ii=0;ii<t2.length; ii++) 
		{
			str += t2[ii] + ", ";
		}

		return str;
	}

	/**   makeA - Actuallly create the A matrix (protected).
	 *
	 */
	protected void makeA()
	{
		//
		//  Fill the standard deviation vector with 1's if
		//  it has not actually been defined yet.
		//
		if(stds == null && te != null )
		{
			stds = new double[te.length];
			Arrays.fill(stds, 1.0);
		}

		if( te != null && t2 != null ) 
		{
			if( a == null || 
			    (te.length*t2.length) != a.length )
			{
				a = new double[te.length*t2.length];
			}

			Arrays.fill(a, 0.0);
			
			if( alpha > 179.999 && alpha < 180.001 )
			{
				for(int ii=0; ii<t2.length; ii++)
				{
					for(int jj=0; jj<te.length; jj++)
					{
						a[ii*te.length+jj] = 
							Math.exp(-te[jj] / t2[ii]) / stds[jj];
					}
				}
			}
			else 
			{
				double[] temp = null;

				// Create based on Hennig
				for(int ii=0; ii<t2.length; ii++)
				{
					decay.setT2(t2[ii]);
					temp = decay.calculateEchoes();
					for(int jj=0; jj<te.length; jj++)
					{
						a[ii*te.length+jj] = temp[jj] / stds[jj];
					}
				}
			}

			dcl1.setA(a, te.length, t2.length);
		}
		else
		{
			if( te == null )
			{
				System.out.println("makeA: te is null");
			}

			if( t2 == null )
			{
				System.out.println("makeA: t2 is null");
			}
		}
	}

}
