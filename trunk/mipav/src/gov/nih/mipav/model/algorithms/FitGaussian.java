package gov.nih.mipav.model.algorithms;


import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * FitGaussian -fits an array of points to a normal curve, general from f = a*exp(-(x-b)^2/2sigma^2)
 * Will also perform thresholding techniques to determine useful data points for fitting
 *
 * @author   senseneyj
 * @see      NLEngine
 * @version  0.1
 */
public class FitGaussian extends NLEngine {

	//~ Instance fields ------------------------------------------------------------------------------------------------
    /** Original x-data */
    private double[] xDataOrg;

    /** Original y-data */
    private double[] yDataOrg;
    
    /** Interpolated y-data from Gaussian*/
    private double[] yDataInt;
    
    /**Amplitude parameter*/
    private double amp;
    
    /**Center parameter*/
    private double xInit;
    
    /**Sigma parameter*/
    private double sigma;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FitGaussian - test constructor, builds and test function.
     */
    public FitGaussian() {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(5, 3);
        testData();
    }

    /**
     * FitGaussian.
     *
     * @param  nPoints  number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitGaussian(int nPoints, double[] xData, double[] yData) {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints, 3);

        this.xDataOrg = xData;
        this.yDataOrg = yData;
        
        estimateInitial();
        
    }

    /**
     * Constructs new fit gaussian.
     *
     * @param  nPoints  Number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitGaussian(int nPoints, float[] xData, float[] yData) {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints, 3);

        this.xDataOrg = new double[nPoints];
        this.yDataOrg = new double[nPoints];
        for (int i = 0; i < nPoints; i++) {
            xDataOrg[i] = xData[i];
            yDataOrg[i] = yData[i];
        }
        
        estimateInitial();

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    private void estimateInitial() {
    	amp = 2500;
    	xInit = 980;
    	sigma = 50;
    	
    	a[0] = amp;
    	a[1] = xInit;
    	a[2] = sigma;
    }
    
    /**
     * Starts the analysis. For some reason a guess with the wrong sign for a2 will not converge. Therefore, try both
     * sign and take the one with the lowest chi-squared value.
     */
    public void driver() {
        
    	for(int i=0; i<10; i++) {
    	
	    	Matrix jacobian = generateJacobian();
	    	Matrix residuals = generateResiduals();
	    	
	    	Matrix lhs = jacobian.transpose().times(jacobian);
	    	Matrix rhs = jacobian.transpose().times(residuals);
	    	
	    	Matrix dLambda = lhs.solve(rhs);
	    	
	    	amp = amp + dLambda.get(0, 0);
	    	xInit = xInit + dLambda.get(1, 0);
	    	sigma = sigma + dLambda.get(2, 0);
    	}
    	
    	//a already initialized in super constructor, used to hold parameters for output
    	a[0] = amp;
    	a[1] = xInit;
    	a[2] = sigma;
    	
    }

    /**
     * Display results of displaying exponential fitting parameters.
     */
    public void dumpResults() {
        Preferences.debug(" ******* FitGaussian ********* \n\n");
        Preferences.debug("Number of iterations: " + String.valueOf(kk) + "\n");
        Preferences.debug("Chi-squared: " + String.valueOf(chisq) + "\n");

        // Preferences.debug("Final lamda: " + String.valueOf(flamda));
        Preferences.debug("a0 " + String.valueOf(1) + "\n"); // + " +/- " + String.valueOf(Math.sqrt(covar[0][0])));
        Preferences.debug("a1 " + String.valueOf(1) + "\n"); // + " +/- " + String.valueOf(Math.sqrt(covar[1][1])));
        Preferences.debug("a2 " + String.valueOf(1) + "\n\n"); // + " +/- " +
                                                                  // String.valueOf(Math.sqrt(covar[2][2])));
    }
    
    @Override
	public double fitToFunction(double x1, double[] atry, double[] dyda) {
		// TODO Auto-generated method stub
		return 0;
	}

    /**
     * Test data to test fitting of gaussian.
     */
    private void testData() {

        // Setup for a function of the type y = c1 + c2*exp(c3*x)
        int i;

        stdv = 100;

        // ia[0] equals 0 for fixed c1; ia[0] is nonzero for fitting c1
        ia[0] = 1;

        // ia[1] equals 0 for fixed c2; ia[1] is nonzero for fitting c2
        ia[1] = 1;

        // ia[2] equals 0 for fixed c3; ia[2] is nonzero for fitting c3
        ia[2] = 1;

        xseries[0] = 0;
        yseries[0] = 1 + 2;

        xseries[1] = 1;
        yseries[1] = 1 + (2 * Math.exp(-3));

        xseries[2] = 2;
        yseries[2] = 1 + (2 * Math.exp(-6));

        xseries[3] = 3;
        yseries[3] = 1 + (2 * Math.exp(-9));

        xseries[4] = 4;
        yseries[4] = 1 + (2 * Math.exp(-12));

        // guess();
        gues[0] = -1;
        gues[1] = 2;
        gues[2] = -5;

        // Assign the same standard deviations to all data points.
        for (i = 0; i < 5; i++) {
            sig[i] = stdv;
        }
    }
    
    /**
     * Gaussian evaluated at a point with given parameters
     */
    private double gauss(double x) {
    	double exp = -Math.pow(x-xInit, 2) / (2 * Math.pow(sigma, 2));
    	
    	double f = amp*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Partial derivative of gaussian with respect to A.
     */
    private double dgdA(double x) {
    	double exp = -Math.pow(x-xInit, 2) / (2 * Math.pow(sigma, 2));
    	
    	double f = Math.exp(exp);
    	
    	return f;
    	
    }
    
    /**
     * Partial derivative of gaussian with respect to x.
     */
    private double dgdx(double x) {
    	double exp = -Math.pow(x-xInit, 2) / (2 * Math.pow(sigma, 2));
    	
    	double coeff = (amp * (x-xInit))/(Math.pow(sigma, 2));
    	
    	double f = coeff*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Partial derivative of gaussian with respect to sigma.
     */
    private double dgdsigma(double x) {
    	double exp = -Math.pow(x-xInit, 2) / (2 * Math.pow(sigma, 2));
    	
    	double coeff = (amp * Math.pow(x-xInit, 2))/(Math.pow(sigma, 3));
    	
    	double f = coeff*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Jacobian used for non-linear least squares fitting.
     */
    private Matrix generateJacobian() {
    	Matrix jacobian = new Matrix(xDataOrg.length, 3);
    	for(int i=0; i<xDataOrg.length; i++) {
    		jacobian.set(i, 0, dgdA(xDataOrg[i]));
    		jacobian.set(i, 1, dgdx(xDataOrg[i]));
    		jacobian.set(i, 2, dgdsigma(xDataOrg[i]));
    	}
    	
    	return jacobian;
    }
    
    private Matrix generateResiduals() {
    	Matrix residuals = new Matrix(yDataOrg.length, 1);
    	for(int i=0; i<yDataOrg.length; i++) {
    		double r = yDataOrg[i] - gauss(xDataOrg[i]);
    		residuals.set(i, 0, r);
    	}
    	
    	return residuals;
    }
    
}
