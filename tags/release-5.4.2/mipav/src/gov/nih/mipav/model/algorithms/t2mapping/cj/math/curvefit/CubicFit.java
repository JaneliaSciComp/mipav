package gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit;
 
public class CubicFit extends CurveFit {    

    /** Construct a new CurveFitter. */
    public CubicFit () { }
    
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
        double slope;
        if ((lastx - firstx) != 0.0)
            slope = (lasty - firsty)/(lastx - firstx);
        else
            slope = 1.0;
        double yintercept = firsty - slope * firstx;

        maxIter = IterFactor * numParams * numParams;  // Where does this estimate come from?
        restarts = 1;
        maxError = 1e-9;

		simp[0][0] = yintercept;
		simp[0][1] = slope;
		simp[0][2] = 0.0;
		simp[0][3] = 0.0;
    }
    
    public int getNumParams() 
	{
		return 4;
    }
        
    public double f(double[] p, double x) 
	{
		double xx = x*x;
		return p[0] + p[1]*x + p[2]*xx + p[3]*x*xx;
    }
}
