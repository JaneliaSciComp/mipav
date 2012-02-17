package gov.nih.mipav.model.algorithms.t2mapping.cj.math.transform;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;

public class TransformAffine extends Transform
{
	protected double A[][] = new double[4][4];
	protected double Ai[][] = new double[4][4];

	protected double ox = 0, oy = 0, oz = 0;

	private double T[][] = new double[4][4];
	private double Ti[][] = new double[4][4];
	private double C[][] = new double[4][4];

	private double xw[] = new double[4], outw[] = new double[4];

	public TransformAffine()
	{
		reset(A); reset(Ai);
	}

	final public TransformAffine inverse()
	{
		TransformAffine ti = new TransformAffine();
		ti.A = this.Ai; ti.Ai = this.A;
		ti.ox = this.ox; ti.oy = this.oy; ti.oz = this.oz;
		return ti;
	}

	public void reset()
	{
		reset(A); reset(Ai);
	}

	public void reset(double A[][])
	{
		for(int ii=0; ii<4;ii++)
			java.util.Arrays.fill(A[ii], 0.0);

		A[0][0] = 1.0;
		A[1][1] = 1.0;
		A[2][2] = 1.0;
		A[3][3] = 1.0;
	}

	// theta is in degrees
	final public TransformAffine rotateX(double theta)
	{
		final double rad = Math.toRadians(theta);
		final double c = Math.cos(rad), s = Math.sin(rad);

		originToZero();

		reset(T); T[1][1] = c; T[1][2] = -s; T[2][1] = s; T[2][2] = c;
		reset(Ti); Ti[1][1] = c; Ti[1][2] = s; Ti[2][1] = -s; Ti[2][2] = c;
		applyMatrix(T, Ti);

		zeroToOrigin();

		return this;
	}

	// theta is in degrees
	final public TransformAffine rotateY(double theta)
	{
		final double rad = Math.toRadians(theta);
		final double c = Math.cos(rad), s = Math.sin(rad);

		originToZero();

		reset(T); T[2][2] = c; T[2][0] = -s; T[0][2] = s; T[0][0] = c;
		reset(Ti); Ti[2][2] = c; Ti[2][0] = s; Ti[0][2] = -s; Ti[0][0] = c;
		applyMatrix(T, Ti);

		zeroToOrigin();

		return this;
	}

	final public TransformAffine rotateZ(double theta)
	{
		final double rad = Math.toRadians(theta);
		final double c = Math.cos(rad), s = Math.sin(rad);

		originToZero();

		reset(T); T[0][0] = c; T[0][1] = -s; T[1][0] = s; T[1][1] = c;
		reset(Ti); Ti[0][0] = c; Ti[0][1] = s; Ti[1][0] = -s; Ti[1][1] = c;

		applyMatrix(T, Ti);

		zeroToOrigin();

		return this;
	}

	final public TransformAffine scaleX(double scale)
	{
		originToZero();

		reset(T); T[0][0] = scale;
		reset(Ti); Ti[0][0] = 1.0/scale;
		applyMatrix(T, Ti);

		zeroToOrigin();

		return this;
	}

	final public TransformAffine scaleY(double scale)
	{
		originToZero();

		reset(T); T[1][1] = scale;
		reset(Ti); Ti[1][1] = 1.0/scale;
		applyMatrix(T, Ti);

		zeroToOrigin();

		return this;
	}

	final public TransformAffine scaleZ(double scale)
	{
		originToZero();

		reset(T); T[2][2] = scale;
		reset(Ti); Ti[2][2] = 1.0/scale;
		applyMatrix(T, Ti);

		zeroToOrigin();

		return this;
	}

	final public void setOrigin(double x, double y, double z)
	{
		this.ox = x; this.oy = y; this.oz = z;
	}

	private void originToZero()
	{
		//translateX(-ox); translateY(-oy); translateZ(-oz);
	}

	private void zeroToOrigin()
	{
		//translateX(ox); translateY(oy); translateZ(oz);
	}

	private void applyMatrix(double[][] T, double[][] Ti)
	{
		Matrix.copy(C,A); Matrix.mult(T,C,A);
		Matrix.copy(C,Ai); Matrix.mult(C,Ti,Ai);
	}

	final public TransformAffine translateX(double dist)
	{
		reset(T); reset(Ti);
		T[0][3] = dist; Ti[0][3] = -dist;

		applyMatrix(T,Ti);
		return this;
	}

	final public TransformAffine translateY(double dist)
	{
		reset(T); reset(Ti);
		T[1][3] = dist; Ti[1][3] = -dist;

		applyMatrix(T,Ti);
		return this;
	}

	final public TransformAffine translateZ(double dist)
	{
		reset(T); reset(Ti);
		T[2][3] = dist; Ti[2][3] = -dist;

		applyMatrix(T,Ti);
		return this;
	}

	final public void transform(double[] in, double[] out)
	{
		xw[0] = in[0] - ox;
		xw[1] = in[1] - oy;
		xw[2] = in[2] - oz;
		xw[3] = 1.0;

		Matrix.mult(A, xw, outw);

		out[0] = outw[0] + ox;
		out[1] = outw[1] + oy;
		out[2] = outw[2] + oz;
	}

	public String toString()
	{
		return Matrix.toString(A);
	}

	final public Transform compose(Transform t)
	{
		if (!(t instanceof TransformAffine))
			throw new IllegalArgumentException("TransformAffine can only compose other TransformAffines");
		TransformAffine s = (TransformAffine)t;

		applyMatrix(s.A,s.Ai); return this;
	}
}
