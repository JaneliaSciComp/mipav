package gov.nih.mipav.model.algorithms.t2mapping.cj.math.golden;

/**
function [xmin, fmin] = golden(f, ax, bx, cx, tol).
 
%GOLDEN   Minimize function of one variable using golden section search
%
%   [xmin, fmin] = golden(f, ax, bx, cx, tol) computes a local minimum
%   of f. xmin is the computed local minimizer of f and fmin is
%   f(xmin). xmin is computed to an relative accuracy of TOL.
%
%   The parameters ax, bx and cx must satisfy the following conditions:
%   ax < bx < cx, f(bx) < f(ax) and f(bx) < f(cx).
%
%   xmin satisfies ax < xmin < cx. golden is guaranteed to succeed if f
%   is continuous between ax and cx
%
%   Roman Geus, ETH Zuerich, 9.12.97
*/ 
public abstract class Golden
{

	final double C = (3-Math.sqrt(5))/2;
	final double R = 1-C;
	protected double tol = 10e-10;

 	abstract protected double function(double x);

	final public void setTolerance(final double tol)
	{
		if( tol > 0 )
			this.tol = tol;
	}

	public boolean stopCriterion(double x0, double f0, double x1, double f1, 
			double x2, double f2, double x3, double f3)
	{
		return (Math.abs(x3-x0) <= tol*(Math.abs(x1)+Math.abs(x2)));
	}

	final public double golden(double ax, double bx, double cx)
	{
		double x0=0.0, x1=0.0, x2=0.0, x3=0.0;
		double f0=0.0, f1=0.0, f2=0.0, f3=0.0;
		double xmin, fmin;

		x0 = ax;
		x3 = cx;
		if((Math.abs(cx-bx) > Math.abs(bx-ax)))
		{
			x1 = bx;
			x2 = bx + C*(cx-bx);
		}
		else
		{
			x2 = bx;
			x1 = bx - C*(bx-ax);
		}
		f1 = function(x1);
		f2 = function(x2);

		int k = 1;
		while( !stopCriterion(x0, f0, x1, f1, x2, f2, x3, f3) )
		{
//			System.out.println("k " + k + " |a-b|= " + Math.abs(x3-x0));
			if (f2 < f1)
			{
				x0 = x1;
				x1 = x2;
				x2 = R*x1 + C*x3;   // x2 = x1+c*(x3-x1)
				f1 = f2;
				f2 = function(x2);
			}
			else
			{
				x3 = x2;
				x2 = x1;
				x1 = R*x2 + C*x0;   // x1 = x2+c*(x0-x2)
				f2 = f1;
				f1 = function(x1);
			}
			k++;
		}

		if(f1 < f2)
		{
			xmin = x1;
			fmin = f1;
		}
		else
		{
			xmin = x2;
			fmin = f2;
		}

		return xmin;
	}
}
