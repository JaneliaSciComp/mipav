package gov.nih.mipav.model.algorithms.t2mapping.cj.math.curvefit;

abstract public class CurveFit {    

    public static final int IterFactor = 500;
    
    private static final double alpha = -1.0;	  // reflection coefficient
    private static final double beta = 0.5;	  // contraction coefficient
    private static final double gamma = 2.0;	  // expansion coefficient
    private static final double root2 = 1.414214; // square root of 2
    
    protected double[] xData, yData;  // x,y data to fit
    protected int numPoints;          // number of data points
    protected double[][] simp; 		// the simplex (the last element of the array at each vertice is the sum of the square of the residuals)
    protected int numVertices;        // numParams+1 (includes sumLocalResiduaalsSqrd)
    protected int maxIter; 	// maximum number of iterations per restart
    protected int restarts; 	// number of times to restart simplex after first soln.
    protected double maxError;     // maximum error tolerance
    protected int numParams;          // number of parametres
    protected double[] next;		// new vertex to be tested

    private int worst;			// worst current parametre estimates
    private int nextWorst;		// 2nd worst current parametre estimates
    private int best;			// best current parametre estimates
    private int numIter;		// number of iterations so far
    
    /** Construct a new CurveFitter. */
    public CurveFit() { }
    
    public void doFit(double[] xData, double[] yData) {

        this.xData = xData;
        this.yData = yData;
        numPoints = xData.length;

        initialize();
        restart(0);
        
        numIter = 0;
        boolean done = false;
        double[] center = new double[numParams];  // mean of simplex vertices
        while (!done) {
            numIter++;
            for (int i = 0; i < numParams; i++) center[i] = 0.0;
            // get mean "center" of vertices, excluding worst
            for (int i = 0; i < numVertices; i++)
                if (i != worst)
                    for (int j = 0; j < numParams; j++)
                        center[j] += simp[i][j];
            // Reflect worst vertex through centre
            for (int i = 0; i < numParams; i++) {
                center[i] /= numParams;
                next[i] = center[i] + alpha*(simp[worst][i] - center[i]);
            }
            sumResiduals(next);
            // if it's better than the best...
            if (next[numParams] <= simp[best][numParams]) {
                newVertex();
                // try expanding it
                for (int i = 0; i < numParams; i++)
                    next[i] = center[i] + gamma * (simp[worst][i] - center[i]);
                sumResiduals(next);
                // if this is even better, keep it
                if (next[numParams] <= simp[worst][numParams])
                    newVertex();
            }
            // else if better than the 2nd worst keep it...
            else if (next[numParams] <= simp[nextWorst][numParams]) {
                newVertex();
            }
            // else try to make positive contraction of the worst
            else {
                for (int i = 0; i < numParams; i++)
                    next[i] = center[i] + beta*(simp[worst][i] - center[i]);
                sumResiduals(next);
                // if this is better than the second worst, keep it.
                if (next[numParams] <= simp[nextWorst][numParams]) {
                    newVertex();
                }
                // if all else fails, contract simplex in on best
                else {
                    for (int i = 0; i < numVertices; i++) {
                        if (i != best) {
                            for (int j = 0; j < numVertices; j++)
                                simp[i][j] = beta*(simp[i][j]+simp[best][j]);
                            sumResiduals(simp[i]);
                        }
                    }
                }
            }
            order();
            
            double rtol = 2 * Math.abs(simp[best][numParams] - simp[worst][numParams]) /
            (Math.abs(simp[best][numParams]) + Math.abs(simp[worst][numParams]) + 0.0000000001);
            
            if (numIter >= maxIter) done = true;
            else if (rtol < maxError) {
                //System.out.print(getResultString());
                restarts--;
                if (restarts < 0) {
                    done = true;
                }
                else {
                    restart(best);
                }
            }
        }
    }
        
    /** Initialise the simplex
     */
    abstract void initialize(); 
    
    /** Restart the simplex at the nth vertex */
    void restart(int n) {
        // Copy nth vertice of simplex to first vertice
        for (int i = 0; i < numParams; i++) {
            simp[0][i] = simp[n][i];
        }
        sumResiduals(simp[0]);          // Get sum of residuals^2 for first vertex
        double[] step = new double[numParams];
        for (int i = 0; i < numParams; i++) {
            step[i] = simp[0][i] / 2.0;     // Step half the parametre value
            if (step[i] == 0.0)             // We can't have them all the same or we're going nowhere
                step[i] = 0.01;
        }
        // Some kind of factor for generating new vertices
        double[] p = new double[numParams];
        double[] q = new double[numParams];
        for (int i = 0; i < numParams; i++) {
            p[i] = step[i] * (Math.sqrt(numVertices) + numParams - 1.0)/(numParams * root2);
            q[i] = step[i] * (Math.sqrt(numVertices) - 1.0)/(numParams * root2);
        }
        // Create the other simplex vertices by modifing previous one.
        for (int i = 1; i < numVertices; i++) {
            for (int j = 0; j < numParams; j++) {
                simp[i][j] = simp[i-1][j] + q[j];
            }
            simp[i][i-1] = simp[i][i-1] + p[i-1];
            sumResiduals(simp[i]);
        }
        // Initialise current lowest/highest parametre estimates to simplex 1
        best = 0;
        worst = 0;
        nextWorst = 0;
        order();
    }
        
    /** Get number of parameters for current fit function */
    abstract public int getNumParams();
        
    /** Returns "fit" function value for parametres "p" at "x" */
    abstract public double f(double[] p, double x); 

    /** Get the set of parameter values from the best corner of the simplex */
    public double[] getParams() {
        order();
        return simp[best];
    }
    
    /** Returns residuals array ie. differences between data and curve */
    public double[] getResiduals() {
        double[] params = getParams();
        double[] residuals = new double[numPoints];
        for (int i = 0; i < numPoints; i++)
            residuals[i] = yData[i] - f(params, xData[i]);
        return residuals;
    }
    
    /* Last "parametre" at each vertex of simplex is sum of residuals
     * for the curve described by that vertex
     */
    final public double getSumResidualsSqr() {
        return (getParams())[getNumParams()];
    }
    
    /**  SD = sqrt(sum of residuals squared / number of params+1)
     */
    final public double getSD() {
        return Math.sqrt(getSumResidualsSqr() / numVertices);
    }
    
    /**  Get a measure of "goodness of fit" where 1.0 is best.
     *
     */
    public double getFitGoodness() {
        double sumY = 0.0;
        for (int i = 0; i < numPoints; i++) sumY += yData[i];
        double mean = sumY / numVertices;
        double sumMeanDiffSqr = 0.0;
        int degreesOfFreedom = numPoints - getNumParams();
        double fitGoodness = 0.0;
        for (int i = 0; i < numPoints; i++) {
            sumMeanDiffSqr += sqr(yData[i] - mean);
        }
        if (sumMeanDiffSqr > 0.0 && degreesOfFreedom != 0)
            fitGoodness = 1.0 - (getSumResidualsSqr() / degreesOfFreedom) * ((numParams) / sumMeanDiffSqr);
        
        return fitGoodness;
    }
    
    /** Get a string description of the curve fitting results
     * for easy output.
     */
    public String getResultString() {
        StringBuffer results = new StringBuffer("\nNumber of iterations: " + getIterations() +
        "\nMaximum number of iterations: " + getMaxIterations() +
        "\nSum of residuals squared: " + getSumResidualsSqr() +
        "\nStandard deviation: " + getSD() +
        "\nGoodness of fit: " + getFitGoodness() +
        "\nParameters:");
        char pChar = 'a';
        double[] pVal = getParams();
        for (int i = 0; i < numParams; i++) {
            results.append("\n" + pChar + " = " + pVal[i]);
            pChar++;
        }
        return results.toString();
    }
        
    double sqr(double d) { return d * d; }
    
    /** Adds sum of square of residuals to end of array of parameters */
    void sumResiduals (double[] x) {
        x[numParams] = 0.0;
        for (int i = 0; i < numPoints; i++) {
            x[numParams] = x[numParams] + sqr(f(x,xData[i])-yData[i]);
            //        if (IJ.debugMode) ij.IJ.write(i+" "+x[n-1]+" "+f(fit,x,xData[i])+" "+yData[i]);
        }
    }

    /** Keep the "next" vertex */
    void newVertex() {
        for (int i = 0; i < numVertices; i++)
            simp[worst][i] = next[i];
    }
    
    /** Find the worst, nextWorst and best current set of parameter estimates */
    void order() {
        for (int i = 0; i < numVertices; i++) {
            if (simp[i][numParams] < simp[best][numParams])	best = i;
            if (simp[i][numParams] > simp[worst][numParams]) worst = i;
        }
        nextWorst = best;
        for (int i = 0; i < numVertices; i++) {
            if (i != worst) {
                if (simp[i][numParams] > simp[nextWorst][numParams]) nextWorst = i;
            }
        }
    }

    /** Get number of iterations performed */
    final public int getIterations() {
        return numIter;
    }
    
    /** Get maximum number of iterations allowed */
    final public int getMaxIterations() {
        return maxIter;
    }
    
    /** Set maximum number of iterations allowed */
    public void setMaxIterations(int x) {
        maxIter = x;
    }
    
    /** Get number of simplex restarts to do */
    final public int getRestarts() {
        return restarts;
    }
    
    /** Set number of simplex restarts to do */
    public void setRestarts(int x) {
        restarts = x;
    }

    /**
     * Gets index of highest value in an array.
     * 
     * @param              Double array.
     * @return             Index of highest value.
     */
    final public int getMax(double[] array) {
        double max = array[0];
        int index = 0;
        for(int i = 1; i < array.length; i++) {
            if(max < array[i]) {
            	max = array[i];
            	index = i;
            }
        }
        return index;
    }
 
}
