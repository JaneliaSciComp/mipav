package gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit;
 
public class BiExpFit extends CurveFit {    

	private double[] lower = null;
	private double[] upper = null;

    /** Construct a new CurveFitter. */
    public BiExpFit () 
	{ 
		lower = new double[getNumParams()];
		upper = new double[getNumParams()];

		for(int ii=0; ii<getNumParams(); ii++) 
		{
			lower[ii] = -99999999;
			upper[ii] = 99999999;
		}
	}
    
	public void lowerConstraints(final double[] lower)
	{
		System.arraycopy(lower, 0, this.lower, 0, lower.length);
	}
    
	public void upperConstraints(final double[] upper)
	{
		System.arraycopy(upper, 0, this.upper, 0, upper.length);
	}

    void initialize() {
        // Calculate some things that might be useful for predicting parametres
        numParams = getNumParams();
        numVertices = numParams + 1;      // need 1 more vertice than parametres,
        simp = new double[numVertices][numVertices];
        next = new double[numVertices];
        
        double firstx = xData[0];
        double firsty = yData[0];
        double lastx = xData[numPoints-1];
        double lasty = yData[numPoints-1];
        double xmean = (firstx+lastx)/2.0;
        double ymean = (firsty+lasty)/2.0;
        maxIter = IterFactor * numParams * numParams;  // Where does this estimate come from?
        restarts = 1;
        maxError = 1e-9;

		simp[0][0] = yData[getMax(yData)] / 2;
		simp[0][1] = xmean;
		simp[0][2] = yData[getMax(yData)] / 2;
		simp[0][3] = xmean;
		simp[0][4] = 10;
		double ab = xData[getMax(yData)] - firstx;
		simp[0][5] = Math.sqrt(ab);
    }


    public int getNumParams() 
	{
		return 5;
    }
        
    public double f(double[] p, double x) 
	{
		if (p[0] <= lower[0]) return -100000.0;
		if (p[1] <= lower[1]) return -100000.0;
		if (p[2] <= lower[2]) return -100000.0;
		if (p[3] <= lower[3]) return -100000.0;
		if (p[4] <= lower[4]) return -100000.0;

		if (p[0] >= upper[0]) return -100000.0;
		if (p[1] >= upper[1]) return -100000.0;
		if (p[2] >= upper[2]) return -100000.0;
		if (p[3] >= upper[3]) return -100000.0;
		if (p[4] >= upper[4]) return -100000.0;

		if( p[1] > p[3] ) return -10000000.0;

		return p[0]*Math.exp(- x / p[1] ) + p[2]*Math.exp(-x/p[3]) + p[4];
    }
}
