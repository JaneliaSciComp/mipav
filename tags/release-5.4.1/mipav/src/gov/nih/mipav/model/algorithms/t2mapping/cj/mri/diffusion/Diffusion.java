package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.diffusion;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.jama.*;

public class Diffusion
{
	private DiffusionGradients gradients = null;
	private Matrix D = null;

	public Diffusion()
	{
		gradients = new DiffusionGradients();
		D = new Matrix(3,3);
	}

//	public Diffusion(double littleDelta, double bigDelta)
//	{
//		gradients = new DiffusionGradients(littleDelta, bigDelta);
//		D = new Matrix(3,3);
//	}

//	public Diffusion(double b)
//	{
//		gradients = new DiffusionGradients(b);
//		D = new Matrix(3,3);
//	}
//
	public void readGradientFile(String filename)
	{
		gradients.readGradientFile( filename );

		// Just testing here...
		Matrix A = gradients.getA();
		System.out.println("A matrix is ");
		A.print(5, 3);
	}

	public void addDirection(double x, double y, double z)
	{
		gradients.addDirection(x, y, z);
	}

	public void addGradientStrength(final double gradStrength)
	{
		gradients.addGradientStrength(gradStrength);
	}

	public void computeTensor(final double[] b)
	{
		Matrix bb = new Matrix(b.length, 1);
		for(int ii=0; ii<b.length; ii++)
		{
			bb.set(ii,0,b[ii]);
		}

		computeTensor(bb);
	}

	public void computeTensor(final Matrix b)
	{
		// Get the A matrix.
//		Matrix A = gradients.getA();

		// Now solve for the diffusion tensor.
		Matrix d = gradients.getA().solve(b);

//		D.set(0,0,d.get(0,0));
//		D.set(0,1,d.get(3,0));
//		D.set(0,2,d.get(4,0));
//
//		D.set(1,0,d.get(3,0));
//		D.set(1,1,d.get(1,0));
//		D.set(1,2,d.get(5,0));
//
//		D.set(2,0,d.get(4,0));
//		D.set(2,1,d.get(5,0));
//		D.set(2,2,d.get(2,0));
//
//		D.print(8,8);
	}

	public Matrix getD()
	{
		return D;
	}
}
