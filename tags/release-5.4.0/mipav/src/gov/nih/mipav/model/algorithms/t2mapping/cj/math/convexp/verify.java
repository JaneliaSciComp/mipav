package gov.nih.mipav.model.algorithms.t2mapping.cj.math.convexp;

import java.io.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.convexp.*;

class verify {

private static void dump(double[] foo)
{System.out.print(foo[0]);
for(int i=1;i<foo.length;i++) System.out.print(" "+foo[i]);
System.out.println("");}

public static void main(String[] argv) throws IOException
{
	double[][] basis = new double[4][2];
	double[] coeff = new double[4];
	double[] target = new double[4];
	double[] x = new double[2];
	double big = 1e20;
	double tol;

	String file;
	String junk;
	boolean error;

	int i,j,k,n;
	BufferedReader fp;
	StreamTokenizer st;

	for(n=0;n<argv.length;n++)
	{
		file = argv[n];
		fp = new BufferedReader(new InputStreamReader(new FileInputStream(file)));
		st = new StreamTokenizer(fp);
		st.parseNumbers();
		st.eolIsSignificant(true);

		junk = fp.readLine();
		for(i=0;i<2;i++)
			{
			for(j=0;j<4;j++)
				{st.nextToken(); basis[j][i] = st.nval;}
			st.nextToken();
			}

		junk = fp.readLine();
		for(i=0;i<2;i++)
			{st.nextToken(); x[i] = st.nval;}
		st.nextToken();

		junk = fp.readLine();
		st.nextToken(); tol = st.nval;
		st.nextToken();

		junk = fp.readLine();
		junk = fp.readLine(); big = Double.parseDouble(junk);

		junk = fp.readLine();
		for(i=0;i<4;i++)
			{st.nextToken(); target[i] = st.nval;}
		st.nextToken();

		fp.close();

		System.out.println(ConvexP.CPdo4x2(basis,x,coeff,tol));

		error = false;

		for(i=0;i<4;i++)
			if (Math.abs(coeff[i] - target[i]) > 0.0001)
				error = true;

		if (!error)
			System.out.println(file + " ok");
		else
			{
			System.out.println("*** " + file + " wrong");
			dump(coeff);
			dump(target);
			}
	}
}
}
