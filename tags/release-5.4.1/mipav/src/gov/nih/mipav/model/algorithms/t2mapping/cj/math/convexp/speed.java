package gov.nih.mipav.model.algorithms.t2mapping.cj.math.convexp;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.convexp.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;

class speed {
public static void main(String argv[])
{
	double[][] basis = new double[4][2];
	double[] coeff = new double[4], x = new double[2];
	double tol,r,s;
	double[] R = new double[1000];
	final int MAX = 4000;
	int i,j;

	for(i=0;i<4;i++) for(j=0;j<2;j++) basis[i][j] = Math.random()*MAX;
	tol = Math.random()*100;

	for(i=0;i<R.length;i++) R[i] = Math.random()*MAX;
    
	System.out.println("Doing 10 million random points...");
	long start = System.currentTimeMillis();

	x[1] = 1729;

	for(i=0;i<10000000;i++)
	{
		//x[0] = Math.random() * MAX; x[1] = Math.random() * MAX;
		x[0] = x[1]; x[1] = R[i%1000];
		ConvexP.CPdo4x2(basis,x,coeff,tol);
	}

	long end = System.currentTimeMillis();
	System.out.println((10000000000L/(end-start)) + " vps");
}
}
