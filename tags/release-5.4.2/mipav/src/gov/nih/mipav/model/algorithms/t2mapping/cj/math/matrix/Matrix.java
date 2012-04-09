package gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix;

import java.util.Arrays;

public class Matrix
{
	final public static String toString(double[][] A)
	{
		String temp = "";
		int m = getNRows(A), n = getNCols(A);
		for(int i=0; i<m; i++)
		{
			for(int j=0; j<n; j++)
			{
				if (j>0) temp += "\t";
				temp += A[i][j];
			}
			temp += "\n";
		}
		return temp;
	}

	final public static String toString(int[][] A)
	{
		String temp = "";
		int m = getNRows(A), n = getNCols(A);
		for(int i=0; i<m; i++)
		{
			for(int j=0; j<n; j++)
			{
				if (j>0) temp += "\t";
				temp += A[i][j];
			}
			temp += "\n";
		}
		return temp;
	}

	/**
	 * @deprecated - Just use double[rows][cols]
	 */
	final public static double[][] alloc(int rows, int cols)
		{ return new double[rows][cols]; }

	final public static double[][] alloc(double[][] A)
		{ return new double[getNRows(A)][getNCols(A)]; }

	final public static int[][] alloc(int[][] A)
		{ return new int[getNRows(A)][getNCols(A)]; }

	final public static float[][] alloc(float[][] A)
		{ return new float[getNRows(A)][getNCols(A)]; }

	final public static void fill(double[][] A, double x)
		{ for(int i=0;i<A.length;i++) { Arrays.fill(A[i],x); } }

	final public static void fill(int[][] A, int x)
		{ for(int i=0;i<A.length;i++) { Arrays.fill(A[i],x); } }

	final public static void fill(float[][] A, float x)
		{ for(int i=0;i<A.length;i++) { Arrays.fill(A[i],x); } }

	final public static void copy(double[][] A, double[][] B)
	{
		int cols = getNCols(A);
		for(int i=0;i<A.length;i++) { System.arraycopy(B[i],0,A[i],0,cols); }
	}

	final public static void copy(int[][] A, int[][] B)
	{
		int cols = getNCols(A);
		for(int i=0;i<A.length;i++) { System.arraycopy(B[i],0,A[i],0,cols); }
	}

	final public static void copy(float[][] A, float[][] B)
	{
		int cols = getNCols(A);
		for(int i=0;i<A.length;i++) { System.arraycopy(B[i],0,A[i],0,cols); }
	}

	final public static double[][] clone(double[][] B)
		{ double[][] out=alloc(B); copy(out,B); return out; }

	final public static int[][] clone(int[][] B)
		{ int[][] out=alloc(B); copy(out,B); return out; }

	final public static float[][] clone(float[][] B)
		{ float[][] out=alloc(B); copy(out,B); return out; }

	final public static double[][] transpose(double[][] A)
	{
		int m=getNRows(A), n=getNCols(A);
		double[][] out=new double[n][m];

		for(int i=0;i<m;i++) for(int j=0;j<n;j++)
			out[j][i] = A[i][j];

		return out;
	}

	final public static int getNRows(double[][] A) { return A.length; }
	final public static int getNRows(int[][] A) { return A.length; }
	final public static int getNRows(float[][] A) { return A.length; }

	final public static int getNCols(double[][] A) { return A[0].length; }
	final public static int getNCols(int[][] A) { return A[0].length; }
	final public static int getNCols(float[][] A) { return A[0].length; }

	final public static double[][] mult(double[][] A, double[][] B)
	{
		double[][] temp = alloc(getNRows(A), getNCols(B));
		mult(A,B,temp); return temp;
	}

	final public static void mult(double[][] A, double[][] B, double[][] out)
	{
		int m=getNRows(A), n=getNRows(B), r=getNCols(B);
		double[] row,row2;

		for(int i=0;i<m;i++)
		{
			row=A[i]; row2=out[i];
			for(int k=0;k<r;k++)
			{
				row2[k]=0; for(int j=0;j<n;j++) row2[k] += row[j]*B[j][k];
			}
		}
	}

	final public static void mult(double[][] A, double[] x, double[] out)
	{
		int m=getNRows(A), n=getNCols(A);

		for(int i=0;i<m;i++)
		{
			out[i] = 0.0;

			for(int j=0;j<n;j++) out[i] += A[i][j] * x[j];
		}
	}

	final public static double[][] eye(int n)
	{ 
		double[][] out=alloc(n, n); 
		for(int ii=0; ii<out[0].length; ii++) out[ii][ii] = 1.0;
		return out; 
	}
}
