package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.nr;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.Vect;

public class VectorValue
{
	public double x;
	public double f;
	public double[] v;

	public VectorValue(int n)
	{
		v = new double[n];
	}

	public void setValues(VectorValue newone)
	{
		x = newone.x;
		f = newone.f;
        Vect.copy(v,newone.v);
	}

    public String toString()
    {
		return Vect.toString(v) + " -> " + f;
	}
};
