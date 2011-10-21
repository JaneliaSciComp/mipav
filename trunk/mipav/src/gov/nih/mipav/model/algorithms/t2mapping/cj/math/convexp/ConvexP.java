package gov.nih.mipav.model.algorithms.t2mapping.cj.math.convexp;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;

public class ConvexP {

final private int nTissues, nChannels;
final private double[][] basis;

private double tolSqr = 0.0;
final static double bigNum = 1.e20;

final static double[][] A = new double[2][2];
final static double[] temp = new double[3], near = new double[2],
	a = new double[2], b = new double[2], c = new double[2];
final static int[] edge = new int[2];

static double t;

private void validate()
{
    if (nTissues < 3 || nTissues > 4 || nChannels != 2)
		throw new IllegalArgumentException("ConvexP can only handle 4x2 or 3x2");
}

public ConvexP(int nTissues, int nChannels)
{
	this.nTissues = nTissues; this.nChannels = nChannels;
	this.basis = new double[nTissues][nChannels];
	validate();
}

public ConvexP(double[][] A)
{
	this.nTissues = Matrix.getNRows(A); this.nChannels = Matrix.getNCols(A);
	this.basis = new double[nTissues][nChannels];
	validate(); setA(A);
}

public int getNumTissues() { return nTissues; }
public int getNumScans() { return 2; }

public void setA(double[][] A)
{
	if (Matrix.getNRows(A) != nTissues || Matrix.getNCols(A) != nChannels)
		throw new IllegalArgumentException("mismatched matrix dimensions");
	else
	{
		for(int i=0;i<nTissues;i++) for(int j=0;j<nChannels;j++)
			basis[i][j] = A[i][j];
	}
}

public void setTol(double tol) { tolSqr = tol*tol; }

public double solve(double[] b, double[] ans)
{
	return (nTissues==4) ?
		CPdo4x2(basis, b, ans, tolSqr) :
		CPdo3x2(basis, b, ans, tolSqr) ;
}

// 2 cmps
final static int sign(double x) {return (x>0)?1:(x<0)?-1:0;}

// 2 mults, 1 add
final static double sumsqr(double x, double y) {return x*x+y*y;}

// 2 mults, 1 add
final static double det2x2(double a, double b, double c, double d)
  {return a*d-b*c;}

// 6 mults, 2 divs, 3 adds, 1 cmp
final static int solve2x2(double[][] A, double[] b, double[] out)
{
    double temp = det2x2(A[0][0],A[0][1],A[1][0],A[1][1]);
    if (temp==0) return(-1);

    out[0] = det2x2(b[0],A[0][1],b[1],A[1][1])/temp;
    out[1] = det2x2(A[0][0],b[0],A[1][0],b[1])/temp;
    return(0);
}

// 2 mults, 3 adds
final static double pp_dist(double[] p, double[] q)
  { return sumsqr(p[0]-q[0],p[1]-q[1]); }

// 6 mults, 3 adds, 2 cmps
final static int ppp_turn(double[] p, double[] q, double[] r)
{
    return sign(p[0]*q[1] - p[1]*q[0]
              + q[0]*r[1] - q[1]*r[0]
              + r[0]*p[1] - r[1]*p[0]);
}

// 6 mults, 2 divs, 11 adds, 4 cmps
final static int affine(double[] p, double[] q, double[] r, double[] t, double[] coeffs)
{
    int res;

    A[0][0] = p[0]-r[0]; A[0][1] = q[0]-r[0];
    A[1][0] = p[1]-r[1]; A[1][1] = q[1]-r[1];
    c[0] = t[0]-r[0]; c[1] = t[1]-r[1];
    res = solve2x2(A,c,coeffs); if (res<0) return(-1);

    coeffs[2] = 1-coeffs[0]-coeffs[1];
    if (coeffs[0]<0) return(0);
    if (coeffs[1]<0) return(0);
    if (coeffs[2]<0) return(0);
    return 1;
}

// 8 mults, 1 div, 11 adds, 3 cmps
final static double ps_dist(double[] p, double[] l0, double[] l1)
{
    double denom,numer;

    a[0] = l1[0] - l0[0]; a[1] = l1[1] - l0[1];
    b[0] = p[0]  - l0[0]; b[1] = p[1]  - l0[1];

    denom = sumsqr(a[0],a[1]);

    if (denom <= 0) { t=0; }
    else
    {
        numer = a[0]*b[0] + a[1]*b[1];
        t = numer/denom;
        t = (t<0)?0:(t>1)?1:t;
    }

    near[0]=l0[0]+t*a[0]; near[1]=l0[1]+t*a[1];

    return pp_dist(p,near);
}

// 68 mults, 10 divs, 106 adds, 40 cmps
final static double CPdo4x2(double[][] basis, double[] x, double[] coeff, double tol)
{
    int i,j,k;
    double m,r,s;

	if (sumsqr(x[0],x[1]) < 5*tol)
	{
		coeff[0]=coeff[1]=coeff[2]=coeff[3]=0.0;
		return 0.0;
	}

    // Find nearest point: 8 mults, 12 adds, 5 cmps
    m = bigNum;
    for(i=0;i<4;i++)
    {
        r = pp_dist(x,basis[i]);
        if (r < m)
		{
			m = r;
			coeff[0]=coeff[1]=coeff[2]=coeff[3]=0.0;
			coeff[i]=1;
		}
    }
    if (m<tol) { return 0.0; }

    // Find nearest edge: 48 mults, 6 divs, 72 adds, 25 cmps
    m = bigNum;
    for(i=0;i<4;i++)
    {
        for(j=i+1;j<4;j++)
        {
            r = ps_dist(x,basis[i],basis[j]);

            if (r < m)
            {
                m = r;
				coeff[0]=coeff[1]=coeff[2]=coeff[3]=0.0;
                edge[0] = i; edge[1] = j;
                coeff[i] = 1-t; coeff[j] = t;
            }
        }
    }
    if (m<tol) return 0.0;  // Already found it
    double edge_dist = m;

    // Try a three-way: 12 mults, 4 divs, 22 adds, 10 cmps
    i = edge[0]; j = edge[1]; m = 1;

    boolean in_hull = false;

    for(k=0;k<4;k++)
    {
        if (k==i || k==j) continue;
        if (affine(basis[i],basis[j],basis[k],x,temp) > 0)
        {
            in_hull = true;
            if (temp[2] < m)
            {
                m = temp[2];
				coeff[0]=coeff[1]=coeff[2]=coeff[3]=0.0;
                coeff[i] = temp[0]; coeff[j] = temp[1]; coeff[k] = temp[2];
            }
        }
    }

    if (in_hull) return 0.0; else return Math.sqrt(edge_dist);
}

// 36 mults, 5 divs, 53 adds, 21 cmps
final static double CPdo3x2(double[][] basis, double[] x, double[] coeff, double tol)
{
    int i,j,k;
    double m,r,s;

	if (sumsqr(x[0],x[1]) < 5*tol)
	{
		coeff[0]=coeff[1]=coeff[2]=0.0;
		return 0.0;
	}

    // Find nearest point: 6 mults, 9 adds, 4 cmps
    m = bigNum;
    for(i=0;i<3;i++)
    {
        r = pp_dist(x,basis[i]);
        if (r < m)
		{
			m = r;
			coeff[0]=coeff[1]=coeff[2]=0.0;
			coeff[i]=1;
		}
    }
    if (m<tol) { return 0.0; }

    // Find nearest edge: 24 mults, 3 divs, 33 adds, 13 cmps
    m = bigNum;
    for(i=0;i<3;i++)
    {
		j = (i+1)%3;
		r = ps_dist(x,basis[i],basis[j]);

		if (r < m)
		{
			m = r;
			coeff[0] = coeff[1] = coeff[2] = 0.0;
			edge[0] = i; edge[1] = j;
			coeff[i] = 1-t; coeff[j] = t;
        }
    }
    if (m<tol) return 0.0;  // Already found it
    double edge_dist = m;

    // Try a three-way: 6 mults, 2 divs, 11 adds, 4 cmps
    if (affine(basis[0],basis[1],basis[2],x,temp) > 0)
	{
		coeff[0]=temp[0]; coeff[1]=temp[1]; coeff[2]=temp[2];
		return 0.0;
    }

    return Math.sqrt(edge_dist);
}
}
