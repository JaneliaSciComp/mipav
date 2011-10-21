package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.lincom;

public class LinComCoefs10_33_124_272 extends LinComCoefs
{
	private int default_set = 1;

	private double short_t2_set1[] = { 3.1872,-4.7952,2.3515,-0.7539};
	private double short_t2_max_set1 = 1.1065;

	private double all_t2_set1[] = {2.5642,-2.5133,1.8698,-1.1654};
	private double all_t2_max_set1 = 1.0764;


	public LinComCoefs10_33_124_272()
	{
	}

	/** Return the default set of coefficients that should
	 *  be applied to get the short T2 image.
	 */
    public double[] getShortT2Coefs()
	{
		return getShortT2Coefs(default_set);
	}

	/** Return the specified set of coefficients that should
	 *  be applied to get the short T2 image.
	 */
    public double[] getShortT2Coefs(int coef_set_number)
	{
		double[] coefs = new double[short_t2_set1.length];

		if( coef_set_number == 1 )
		{
			for(int ii=0; ii<short_t2_set1.length;ii++)
			{
				coefs[ii] = short_t2_set1[ii] / short_t2_max_set1;
			}
		}

		return coefs;
	}

	/** Return the default set of coefficients that should
	 *  be applied to get the "all" T2 image.
	 */
	public double[] getAllT2Coefs()
	{
		return getAllT2Coefs(default_set);
	}

	/** Return the specified set of coefficients that should
	 *  be applied to get the "all" T2 image.
	 */
	public double[] getAllT2Coefs(int coef_set_number)
	{
		double[] coefs = new double[short_t2_set1.length];

		if( coef_set_number == 1 )
		{
			for(int ii=0; ii<short_t2_set1.length;ii++)
			{
				coefs[ii] = all_t2_set1[ii] / all_t2_max_set1;
			}
		}

		return coefs;

	}
}
