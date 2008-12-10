package gov.nih.mipav.model.algorithms;


import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * y = mx + b;.
 */
public class FitLine extends NLFittedFunction {

    //~ Constructors ---------------------------------------------------------------------------------------------------

	/**
     * Creates a new FitLine object.
     */
    public FitLine() {

        // 10 points, 2 parameters, and function type
        super(10, 2);

        setupData();
    }

    /**
     * FitLine.
     *
     * @param  nPoints  number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitLine(int nPoints, float[] xData, float[] yData) {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints, 2);

        int i;

        // ia[0] equals 0 for fixed c1; ia[0] is nonzero for fitting c1
        ia[0] = 1;

        // ia[1] equals 0 for fixed c2; ia[1] is nonzero for fitting c2
        ia[1] = 1;

        for (i = 0; i < nPoints; i++) {
            xseries[i] = (double) xData[i];
            yseries[i] = (double) yData[i];
        }

        gues[0] = 0; // y intercept
        gues[1] = -1; // slope

        stdv = 1;

        for (i = 0; i < nPoints; i++) {
            sig[i] = stdv;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Display results of displaying linear fitting parameters.
     */
    public void displayResults() {
        Preferences.debug(" ******* Dump FitLine ********* \n");

        Preferences.debug("Number of iterations: " + String.valueOf(kk) + "\n");
        Preferences.debug("Chi-squared: " + String.valueOf(chisq) + "\n");
        Preferences.debug("Final lamda: " + String.valueOf(flamda) + "\n");
        Preferences.debug("a0 " + String.valueOf(a[0]) + " +/- " + String.valueOf(Math.sqrt(covar[0][0])) + "\n");
        Preferences.debug("a1 " + String.valueOf(a[1]) + " +/- " + String.valueOf(Math.sqrt(covar[1][1])) + "\n\n");

    }

    /**
     * Fits line to function.
     *
     * @param   x1    The x value of the data point.
     * @param   atry  The best guess parameter values.
     * @param   dyda  The derivative values of y with respect to fitting parameters.
     *
     * @return  The calculated y value.
     */
    public double fitToFunction(double x1, double[] atry, double[] dyda) {

        // called by mrqcof
        // mrqcof supplies x1 and atry[]
        // function returns partial derivatives dyda[] and the calculated ymod
        double fac, ymod = 0;

        try {
            fac = atry[1] * x1;
            ymod = atry[0] + fac;
            dyda[0] = 1; // a0 partial derivative (y intecept)
            dyda[1] = x1; // a1 partial derivative (slope)
        } catch (Exception e) {
            Preferences.debug("function error: " + e.getMessage());
        }

        return ymod;
    }

    /**
     * Sets up data to test fitting of exponential.
     */
    private void setupData() {
        int i;

        stdv = 1;

        // ia[0] = 0 for fixed offset parameter; ia[0] is nonzero for fitting offset parameter
        ia[0] = 1;

        // ia[1] = 0 for fixed slope parameter; ia[1] is nonzero for fitting slope parameter
        ia[1] = 1;
        gues[0] = 1;
        gues[1] = 1;

        xseries[0] = 0;
        yseries[0] = 0;

        xseries[1] = 1;
        yseries[1] = 1.5;

        xseries[2] = 2;
        yseries[2] = 1.75;

        xseries[3] = 3;
        yseries[3] = 3.25;

        xseries[4] = 4;
        yseries[4] = 4;

        xseries[5] = 5;
        yseries[5] = 5.5;

        xseries[6] = 6;
        yseries[6] = 5.75;

        xseries[7] = 7;
        yseries[7] = 8;

        xseries[8] = 8;
        yseries[8] = 8;

        xseries[9] = 9;
        yseries[9] = 8.5;

        // Equal standard deviations are assigned to all 10 points
        for (i = 0; i < 10; i++) {
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
