package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.lincom;

/**
 *  Coefficients for calcualting a short T<sub>2</sub> myelin
 *  water map.
 *
 *  The coefficients were calculated in Matlab.
 *
 */
public class LinComCoefs10x10x320_370x50x1120 extends LinComCoefs
{
	private int default_set = 3;

	private double short_t2_set3[] = { 0.083987, -0.006332, -0.041634, -0.048726, 
		-0.041533, -0.028677, -0.014631, -0.001992, 0.008257, 0.015758, 0.020515, 
		0.023119, 0.023818, 0.022906, 0.020755, 0.017995, 0.014708, 0.010996, 
		0.007322, 0.003751, 0.000122, -0.003015, -0.005864, -0.007986, -0.009947, 
		-0.011674, -0.013020, -0.013938, -0.014181, -0.014437, -0.014214, 
		-0.013905, -0.009080, -0.002215, 0.004438, 0.009158, 0.011744, 0.012390, 
		0.011156, 0.009213, 0.006548, 0.003473, 0.000483, -0.001993, -0.004260, 
		-0.005901, -0.007070, -0.007634}; 
	private double short_t2_max_set3 = 0.030346;

	private double all_t2_set3[] = { 0.052165, -0.055404, -0.021148, -0.012739, 
		0.061000, 0.033622, 0.003594, -0.014566, -0.015966, -0.023403, -0.016462, 
		-0.009808, -0.004362, -0.001724, 0.004320, 0.006368, 0.007254, 0.004863, 
		0.005845, 0.006449, 0.006674, 0.006369, 0.005936, 0.005335, 0.001678, 
		0.000689, -0.000483, -0.001775, -0.003148, -0.004270, -0.005047, -0.004713, 
		-0.003199, -0.009165, -0.002374, -0.004401, -0.002082, 0.002641, 0.005727, 
		0.005122, 0.004814, 0.005606, 0.001602, 0.005188, 0.001046, -0.003933, 
		-0.006199, -0.007534};
	private double all_t2_max_set3 = 0.011175;


	public LinComCoefs10x10x320_370x50x1120()
	{
	}


    public double[] getShortT2Coefs()
	{
		return getShortT2Coefs(default_set);
	}

    public double[] getShortT2Coefs(int coef_set_number)
	{
		double[] coefs = new double[48];

		if( coef_set_number == 3 )
		{
			for(int ii=0; ii<48;ii++)
			{
				coefs[ii] = short_t2_set3[ii] / short_t2_max_set3;
			}
		}

		return coefs;
	}

    public double[] getAllT2Coefs()
	{
		return getAllT2Coefs(default_set);
	}

	public double[] getAllT2Coefs(int coef_set_number)
	{
		double[] coefs = new double[48];

		if( coef_set_number == 3 )
		{
			for(int ii=0; ii<48;ii++)
			{
				coefs[ii] = all_t2_set3[ii] / all_t2_max_set3;
			}
		}

		return coefs;

	}
}
