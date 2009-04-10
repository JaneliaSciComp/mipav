package gov.nih.mipav.model.algorithms;


import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * FitExponential -fits an array of points to an exponential of the type y = a0 + a1*exp(a2*x).
 *
 * @author   William Gandler
 * @see      NLEngine
 * @version  0.9
 */
public class FitExponential extends NLFittedFunction {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FitExponential - test constructor, builds and test function.
     */
    public FitExponential() {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(5, 3);
        testData();
    }

    /**
     * FitExponential.
     *
     * @param  nPoints  number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitExponential(int nPoints, double[] xData, double[] yData) {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints, 3);

        int i;

        // ia[0] equals 0 for fixed c1; ia[0] is nonzero for fitting c1
        ia[0] = 1;

        // ia[1] equals 0 for fixed c2; ia[1] is nonzero for fitting c2
        ia[1] = 1;

        // ia[2] equals 0 for fixed c3; ia[2] is nonzero for fitting c3
        ia[2] = 1;

        for (i = 0; i < nPoints; i++) {
            xseries[i] = xData[i];
            yseries[i] = yData[i];
        }

        this.xDataOrg = xData;
        this.yDataOrg = yData;

        stdv = 1;

        for (i = 0; i < nPoints; i++) {
            sig[i] = stdv;
        }
    }

    /**
     * Constructs new fit exponential.
     *
     * @param  nPoints  Number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitExponential(int nPoints, float[] xData, float[] yData) {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints, 3);

        // super(5, 3, NLEngine.EXPONENTIAL);
        int i;

        // ia[0] equals 0 for fixed c1; ia[0] is nonzero for fitting c1
        ia[0] = 1;

        // ia[1] equals 0 for fixed c2; ia[1] is nonzero for fitting c2
        ia[1] = 1;

        // ia[2] equals 0 for fixed c3; ia[2] is nonzero for fitting c3
        ia[2] = 1;

        xDataOrg = new double[nPoints];
        yDataOrg = new double[nPoints];

        for (i = 0; i < nPoints; i++) {
            xseries[i] = xData[i];
            yseries[i] = yData[i];
            xDataOrg[i] = xData[i];
            yDataOrg[i] = yData[i];
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Starts the analysis. For some reason a guess with the wrong sign for a2 will not converge. Therefore, try both
     * sign and take the one with the lowest chi-squared value.
     */
    public void driver() {
        int i;
        double oldChiSq;
        double[] oldA = new double[a.length];
        int nPoints = xDataOrg.length;

        gues[0] = 5;
        gues[1] = 50;
        gues[2] = 0.75;
        stdv = 1;

        for (i = 0; i < nPoints; i++) {
            sig[i] = stdv;
        }

        super.driver();

        for (i = 0; i < a.length; i++) {
            oldA[i] = a[i];
        }

        oldChiSq = chisq;

        gues[0] = 5;
        gues[1] = 50;
        gues[2] = -0.75;
        stdv = 1;

        for (i = 0; i < nPoints; i++) {
            sig[i] = stdv;
        }

        for (i = 0; i < nPoints; i++) {
            xseries[i] = xDataOrg[i];
            yseries[i] = yDataOrg[i];
        }

        super.driver();

        if (chisq < oldChiSq) 
            return;
           
        for (i = 0; i < a.length; i++) 
            a[i] = oldA[i];

        chisq = oldChiSq;
    }

    /**
     * Display results of displaying exponential fitting parameters.
     */
    public void displayResults() {
        Preferences.debug(" ******* FitExponential ********* \n\n");
        Preferences.debug("Number of iterations: " + String.valueOf(kk) + "\n");
        Preferences.debug("Chi-squared: " + String.valueOf(chisq) + "\n");

        // Preferences.debug("Final lamda: " + String.valueOf(flamda));
        Preferences.debug("a0 " + String.valueOf(a[0]) + "\n"); // + " +/- " + String.valueOf(Math.sqrt(covar[0][0])));
        Preferences.debug("a1 " + String.valueOf(a[1]) + "\n"); // + " +/- " + String.valueOf(Math.sqrt(covar[1][1])));
        Preferences.debug("a2 " + String.valueOf(a[2]) + "\n\n"); // + " +/- " +
                                                                  // String.valueOf(Math.sqrt(covar[2][2])));
        
    }

    /**
     * Fit to function - a0 + a1*exp(a2*x).
     *
     * @param   x1    The x value of the data point.
     * @param   atry  The best guess parameter values.
     * @param   dyda  The derivative values of y with respect to fitting parameters.
     *
     * @return  The calculated y value.
     */
    public double fitToFunction(double x1, double[] atry, double[] dyda) {

        // mrqcof calls function
        // mrqcof supplies x1 and best guess parameters atry[]
        // function returns the partial derivatives dyda and the calculated ymod
        double ymod = 0;

        try {
            ymod = atry[0] + (atry[1] * Math.exp(atry[2] * x1));
            dyda[0] = 1; // a0 partial derivative
            dyda[1] = Math.exp(atry[2] * x1); // a1 partial derivative
            dyda[2] = atry[1] * x1 * Math.exp(atry[2] * x1); // a2 partial derivative
        } catch (Exception e) {
            Preferences.debug("function error: " + e.getMessage());
        }

        return ymod;
    }

    /**
     * Test data to test fitting of exponential.
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

	@Override
	protected void calculateFittedY() {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected Matrix generateResiduals() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void calculateChiSq() {
		// TODO Auto-generated method stub
		
	}
}
