package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.diffusion;



import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.jama.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class Test
{
	DiffusionStatistics diffusion = null;

	static public void main( String[] args )
	{
		Test tt = new Test();
	}

	Test()
	{
		diffusion = new DiffusionStatistics();

//		test1();
		test2();
//		test3("gradfile.txt");
	}

	public void test1()
	{
		double[][] mm = {{2.333993e-05,1.861129e-05,-1.254559e-05},
					   {1.861129e-05,1.307341e-05,-3.446989e-06},
						  {-1.254559e-05,-3.446989e-06,1.894982e-05}};
		Matrix m = new Matrix(mm); 
		double[] s = new double[3];

		diffusion.setMatrix(m);
		diffusion.singularValues(s);
		
		System.out.println("Singular values");
		System.out.println("\ts1: " + s[0] + " 4.356150e-05");
		System.out.println("\ts2: " + s[1] + " 1.423228e-05");
		System.out.println("\ts3: " + s[2] + " 2.430607e-06");


		System.out.println("Diffusion Tensor Peled Decomposition");
		System.out.println("\tLinear diffusivity " + diffusion.linear_diffusivity() + " 0.48700" );
		System.out.println("\tPlanar diffusivity " + diffusion.planar_diffusivity() + " 0.39192" );
		System.out.println("\tSpherical diffusivity " + diffusion.spherical_diffusivity() + " 0.12108" );

		System.out.println("FA: " + diffusion.fractionalAnisotropy() + " 0.79933");
		System.out.println("major: " + diffusion.major_anisotropy() + " -0.43946");
		System.out.println("mean: " + diffusion.meanDiffusivity() );
		System.out.println(diffusion.sphericalAnisotropy());
		System.out.println("paired product: " + diffusion.pairedProduct() + " 4.795054e-10");
		System.out.println(diffusion.stdTensor());
		System.out.println("volume ratio: " + diffusion.volumeRatio() + " 0.18627");

//	Diffusion Tensor Invariants
//	Trace:    5.536317e-05
//	Determinant:   -1.506927e-15
//
//	Diffusion Anisotropy Measures
//	Deviation from Spherical:  0.87892
//	Minor Anisotropy:  0.73050
//	Relative Anisotropy/Sqrt(2):  0.60910
	}

	public void test2()
	{
		Diffusion diff = new Diffusion();

		diff.readGradientFile("gradfile.txt");

		//double[] b={4.90527477843843, 4.55387689160054, 3.13549421592915, 4.85203026391962, 4.35670882668959, 3.17805383034795, 4.91998092582813, 4.33073334028633, 3.17805383034795, 4.69134788222914, 3.17805383034795, 2.48490664978800, 4.91265488573605, 3.89182029811063, 2.30258509299405, 4.86753445045558, 2.94443897916644, 2.83321334405622, 4.81218435537242, 3.93182563272433, 2.89037175789616} ;

//		diff.computeTensor(b);

		DiffusionStatistics ds = new DiffusionStatistics(diff.getD());

		MIF4 mm = new MIF4("data/Z0106D01s1.MIF");
		MCVolume vol = mm.getMCVolume();

		int row=63; int col=63;

		double[] b = new double[ vol.getNSlices()/4 ];
		Arrays.fill(b, 0.0);
		int jj=0;
		for(int ni=0; ni<4; ni++)
		{
			for(int ii=0; ii<vol.getNSlices()/4; ii++) 
			{
				b[ii] += vol.getData(row, col, jj, 0)/4.0;
				jj++;
			}
		}

		for(int ii=0; ii<b.length; ii++) System.out.println("b[ii] = " + b[ii]);

		diff.computeTensor(b);

		System.out.println("fa: " + ds.fractionalAnisotropy() );

	}

	public void test3(String filename)
	{
		DiffusionGradients dd = new DiffusionGradients();

		dd.readGradientFile(filename);
	}
}
