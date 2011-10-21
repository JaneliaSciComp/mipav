package gov.nih.mipav.model.algorithms.t2mapping.lincom;

public class LinComCoefs10x10x320 extends LinComCoefs
{
	private int default_set = 3;

	private double short_t2_set3[] = {
		0.1390,-0.0257,-0.0829,-0.0857,-0.0641,-0.0345,
		-0.0060,0.0174,0.0342,0.0440,0.0477,0.0463,0.0411,0.0333,0.0234,
		0.0129,0.0026,-0.0071,-0.0157,-0.0227,-0.0283,-0.0313,-0.0324,-0.0314,
		-0.0282,-0.0230,-0.0158,-0.0065,0.0046,0.0170,0.0312,0.0466};
	private double short_t2_max_set3 = 0.0451;

	private double all_t2_set3[] = {
		0.0474504,-0.0578936,-0.0136298,0.0192614,0.0247754,
		0.0122144, 0.0066576,-0.0046069,-0.0129667,-0.0140588,-0.0071949,
		-0.0093254,-0.0005276, 0.0018926,0.0063471,0.0025055,-0.0047501,
		-0.0044852,0.0107933,0.0142576,-0.0006796,
		-0.0016313,-0.0019357,0.0035056,-0.0012555,0.0040288,0.0029843,0.0011381,
		-0.0004129,-0.0046853,-0.0070264,-0.0012212};
	private double all_t2_max_set3 = 0.00973795862656;


	public LinComCoefs10x10x320()
	{
	}


    public double[] getShortT2Coefs()
	{
		return getShortT2Coefs(default_set);
	}

    public double[] getShortT2Coefs(int coef_set_number)
	{
		double[] coefs = new double[32];

		if( coef_set_number == 3 )
		{
			for(int ii=0; ii<32;ii++)
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
		double[] coefs = new double[32];

		if( coef_set_number == 3 )
		{
			for(int ii=0; ii<32;ii++)
			{
				coefs[ii] = all_t2_set3[ii] / all_t2_max_set3;
			}
		}

		return coefs;

	}
}
