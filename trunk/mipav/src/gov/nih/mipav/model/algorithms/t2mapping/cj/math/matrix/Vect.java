package gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix;

import java.util.Arrays;

public class Vect
{
	final public static String toString(double[] x)
	{
		String temp = "";
		for(int i=0; i<x.length; i++)
		{
			if (i>0) temp += "\t";
            temp += x[i];
		}
		return temp;
	}

	final public static String toString(int[] x)
	{
		String temp = "";
		for(int i=0; i<x.length; i++)
		{
			if (i>0) temp += "\t";
			temp += x[i];
		}
		return temp;
	}

	final public static void copy(double[] out, double[] x)
		{ System.arraycopy(x,0,out,0,x.length); }

	final public static void copy(int[] out, int[] x)
		{ System.arraycopy(x,0,out,0,x.length); }

	final public static void copy(float[] out, float[] x)
		{ System.arraycopy(x,0,out,0,x.length); }

	final public static double[] alloc(double[] x)
		{ return new double[x.length]; }

	final public static int[] alloc(int[] x)
		{ return new int[x.length]; }

	final public static float[] alloc(float[] x)
		{ return new float[x.length]; }

	final public static double[] clone(double[] x)
		{ double[] out=alloc(x); copy(out,x); return out; }

	final public static int[] clone(int[] x)
		{ int[] out=alloc(x); copy(out,x); return out; }

	final public static float[] clone(float[] x)
		{ float[] out=alloc(x); copy(out,x); return out; }

	final public static double[] reverse(double[] x)
	{
		double[] out=alloc(x);

		for(int i=0,j=x.length-1; j>=0; i++,j--)
			out[j] = x[i];

		return out;
	}

    final public static double[] add(double[] x, double[] y)
		{ double[] temp = alloc(x); add(x,y,temp); return temp; }

    final public static double[] sub(double[] x, double[] y)
		{ double[] temp = alloc(x); sub(x,y,temp); return temp; }

	final public static double[] mult(double[] x, double[] y)
		{ double[] temp = alloc(x); mult(x,y,temp); return temp; }

	final public static double[] mult(double c, double[] x)
		{ double[] temp = alloc(x); mult(c,x,temp); return temp; }

	final public static void add(double[] x, double[] y, double[] out)
		{ for(int i=0;i<x.length;i++) out[i] = x[i]+y[i]; }

	final public static void sub(double[] x, double[] y, double[] out)
		{ for(int i=0;i<x.length;i++) out[i] = x[i]-y[i]; }

	final public static void mult(double[] x, double[] y, double[] out)
		{ for(int i=0;i<x.length;i++) out[i] = x[i]*y[i]; }

	final public static void mult(double c, double[] x, double[] out)
		{ for(int i=0;i<x.length;i++) out[i] = c*x[i]; }

	final public static double dot(double[] x, double[] y)
	{
		double t=0; for(int i=0;i<x.length;i++) t += x[i]*y[i];
		return t;
	}

	final public static double linfnorm(double[] x)
	{
		double t=0; for(int i=0;i<x.length;i++) t = Math.max(t,Math.abs(x[i]));
		return t;
	}

    // out = x + c*y
	final public static void lincomb(double[] x, double c, double[] y, double[] out)
		{ mult(c,y,out); add(x,out,out); }

	final public static double[] normalize(double[] x)
	{
		double[] out=alloc(x);

		double tt=0.0;
		for(int ii=0; ii<x.length; ii++)
			tt += x[ii]*x[ii];

		for(int ii=0; ii<x.length; ii++)
			out[ii] = x[ii] / tt;

		return out;
	}

}
