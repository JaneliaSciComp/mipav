package gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit;

/** The LogFit will take in a set of (x,y) data and compute
 *  a linear fit to the log of the data.
 */
public class LogFit {    

	private double intercept = 0.0;
	private double slope = 0.0;
	private double[] xData = null;
	private double[] yData = null;

    /** Construct a new CurveFitter. */
    public LogFit() { }
    
    public void doFit(final double[] xData_in, final double[] yData_in) 
	{
		xData = new double[ xData_in.length ];
		yData = new double[ yData_in.length ];

		System.arraycopy(xData_in, 0, xData, 0, xData_in.length);

		for(int ii=0; ii<yData.length; ii++) 
		{
			yData[ii] = Math.log(yData_in[ii]);
		}

		double sumx = 0.0;
		double sumy = 0.0;
		double sumxy = 0.0;
		double sumx2 = 0.0;
		intercept = 0.0;
		slope = 0.0;

		for(int ii=0; ii<yData.length; ii++) {
			sumx += xData[ii];
			sumy += yData[ii];
			sumxy += (xData[ii]*yData[ii]);
			sumx2 += (xData[ii]*xData[ii]);
		}

		slope = yData.length * sumxy - sumx * sumy;
		intercept = yData.length * sumx2 - sumx * sumx;

		slope /= intercept;

		intercept = sumy - slope * sumx;

		intercept /= yData.length;
    }
        
    /** Initialise the simplex
     */
    void initialize() { }
    void restart(int n) { }
    public int getNumParams() { return 2;}
    public double f(double[] p, double x) { return 0.0; }

    public double[] getParams() 
	{
		double params[] = new double[2];

		params[0] = intercept;
		params[1] = slope;

        return params;
    }
    
    public double[] getResiduals() 
	{
		double[] residuals = new double[xData.length];

		for(int ii=0; ii<xData.length; ii++) 
		{
			residuals[ii] = (yData[ii] - (slope*xData[ii] +intercept));
		}

		return residuals;
	}
    
    final public double getSumResidualsSqr() 
	{
		return 0.0;
    }
    
    /**  SD = sqrt(sum of residuals squared / number of params+1)
     */
    final public double getSD() 
	{
		return 0.0;
    }
    
    /**  Get a measure of "goodness of fit" where 1.0 is best.
     *
     */
    public double getFitGoodness() 
	{
		return 0.0;
    }
    
    /** Get a string description of the curve fitting results
     * for easy output.
     */
    public String getResultString() 
	{
		return "";
    }
        
    /** Adds sum of square of residuals to end of array of parameters */
    void sumResiduals (double[] x) 
	{
    }

    /** Keep the "next" vertex */
    void newVertex() {
    }
    
    /** Find the worst, nextWorst and best current set of parameter estimates */
    void order() {
    }

    /** Get number of iterations performed */
    final public int getIterations() {
        return 0;
    }
    
    /** Get maximum number of iterations allowed */
    final public int getMaxIterations() {
        return 0;
    }
    
    /** Set maximum number of iterations allowed */
    public void setMaxIterations(int x) {
    }
    
    /** Get number of simplex restarts to do */
    final public int getRestarts() {
        return 0;
    }
    
    /** Set number of simplex restarts to do */
    public void setRestarts(int x) {
    }

    /**
     * Gets index of highest value in an array.
     * 
     * @param              Double array.
     * @return             Index of highest value.
     */
    final public int getMax(double[] array) {
        return 0;
    }
 
}
