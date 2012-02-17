package gov.nih.mipav.model.algorithms.t2mapping.lincom;

abstract public class LinComCoefs
{
	public LinComCoefs() { }

	/** Get the default coefficients 
	 */
    abstract public double[] getShortT2Coefs();
	abstract public double[] getAllT2Coefs();

	/** Get a specified set of coefficients
	 */
    abstract public double[] getShortT2Coefs(int coef_set_number);
	abstract public double[] getAllT2Coefs(int coef_set_number);
}
