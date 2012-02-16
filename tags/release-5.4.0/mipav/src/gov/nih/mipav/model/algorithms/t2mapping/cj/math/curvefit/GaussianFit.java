package gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit;
 
public class GaussianFit extends CurveFit {    

    /** Construct a new CurveFitter. */
    public GaussianFit () 
	{ 
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

		simp[0][0] = yData[getMax(yData)];
		simp[0][1] = xmean;
		double ab = xData[getMax(yData)] - firstx;
		simp[0][2] = Math.sqrt(ab);
    }
    
    public int getNumParams() 
	{
		return 3;
    }
        
    public double f(double[] p, double x) 
	{
		if (p[0] <= 0) return -100000.0;
		if (p[1] <= 0) return -100000.0;
		if (p[2] <= 0) return -100000.0;

		return p[0]*Math.exp(- (x-p[1])*(x-p[1]) / (p[2]*p[2]) );
    }
}
