package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.diffusion;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.jama.*;

public class DiffusionStatistics
{
	private Matrix D = null;
	private double[] E = null;

	public DiffusionStatistics()
	{
	}

	public DiffusionStatistics(final Matrix D)
	{
		setMatrix(D);
	}

	public void setMatrix(final Matrix D)
	{
		this.D = D.copy();

		SingularValueDecomposition svd = new SingularValueDecomposition();
   		svd.calculate(D); 

		E = (double[])svd.getSingularValues().clone();

		System.out.println("Singular values are ");
		for(int ii=0;ii<E.length; ii++) System.out.println(E[ii]);
	}

	private double sum(final double[] d )
	{
		double total = 0.0;
		for(int ii=0;ii<d.length; ii++) total+= d[ii];

		return total;
	}

	private double mean(final double[] d )
	{
		return sum(d)/d.length;
	}

	public double linear_diffusivity()
	{
		return (E[0]-E[1])/sum(E);
	}

	public double planar_diffusivity()
	{
		return 2.0*(E[1]-E[2])/sum(E);
	}

	public double spherical_diffusivity()
	{
		return 3.0*E[2]/sum(E);
	}

	public double fractionalAnisotropy()
	{
		double sde = Math.sqrt(Math.pow((E[0]-mean(E)),2.0) + 
		                	   Math.pow((E[1]-mean(E)),2.0) + 
					    	   Math.pow((E[2]-mean(E)),2.0) )/Math.sqrt(3.0);

		double temp = 0.0;
		for(int ii=0; ii<E.length; ii++) temp += (E[ii]*E[ii]);
		return Math.sqrt(3.0/2.0)*(Math.sqrt(3.0)*sde)/Math.sqrt(temp);
	}

	public double major_anisotropy()
	{
		double nu = 1.0/3.0 * ( E[2] - (E[0]+E[1])/2.0 );
		double epsilon = (E[0] - E[1] ) / 2.0;
		return nu / mean(E);
	}

	public double meanDiffusivity()
	{
		return D.trace() / 3.0;
	}

	public double sphericalAnisotropy()
	{
		double nu = 1.0/3.0 * ( E[2] - (E[0]+E[1])/2.0 );
		double epsilon = (E[0] - E[1])/2.0;

		return epsilon / mean(E);
	}

	public double pairedProduct()
	{
		return 1.0/2.0*( Math.pow(D.trace(),2.0) - D.times(D).trace());
	}

	public void singularValues(double[] s)
	{
		System.arraycopy(E, 0, s, 0, E.length);
	}

	public double stdTensor()
	{
		double sde = Math.sqrt(Math.pow((E[0]-mean(E)),2.0) + 
		                	   Math.pow((E[1]-mean(E)),2.0) + 
					    	   Math.pow((E[2]-mean(E)),2.0) )/Math.sqrt(3.0);

		return sde / ( Math.sqrt(2.0) * mean(E) );
	}

	public double volumeRatio()
	{
		double temp = 0.0;
		for(int ii=0; ii<E.length; ii++) temp *= E[ii];
		return temp / Math.pow(mean(E), 3.0);
	}
}
