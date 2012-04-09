package gov.nih.mipav.model.algorithms;


import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * FitExponential -fits an array of points to an exponential of the type y = a0 + a1*exp(a2*x).
 *
 * @author   William Gandler
 * @see      NLConstrainedEngine
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
        
        bounds = 0; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        
        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        outputMes = false;
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
        this.xSeries = xData;
        this.ySeries = yData;
        
        bounds = 0; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        
        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        outputMes = false;    
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
        xSeries = new double[xData.length];
        ySeries = new double[yData.length];
        int i;
        for (i = 0; i < xData.length; i++) {
        	xSeries[i] = xData[i];
        }
        for (i = 0; i < yData.length; i++) {
        	ySeries[i] = yData[i];
        }
        
        bounds = 0; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters

        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        outputMes = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Starts the analysis. For some reason a guess with the wrong sign for a2 will not converge. Therefore, try both
     * sign and take the one with the lowest chi-squared value.
     */
    public void driver() {
        int i;
        double oldChiSq;
        double chisq;
        double[] oldA = new double[a.length];

        gues[0] = 5;
        gues[1] = 50;
        gues[2] = 0.75;

        super.driver();

        for (i = 0; i < a.length; i++) {
            oldA[i] = a[i];
        }

        oldChiSq = getChiSquared();

        gues[0] = 5;
        gues[1] = 50;
        gues[2] = -0.75;

        super.driver();
        chisq = getChiSquared();

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
        Preferences.debug(" ******* FitExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);

        // Preferences.debug("Final lamda: " + String.valueOf(flamda) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM); 
        // + " +/- " + String.valueOf(Math.sqrt(covar[0][0])));
        Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM); 
        // + " +/- " + String.valueOf(Math.sqrt(covar[1][1])));
        Preferences.debug("a2 " + String.valueOf(a[2]) + "\n\n", Preferences.DEBUG_ALGORITHM); // + " +/- " +
                                                                  // String.valueOf(Math.sqrt(covar[2][2])));
        
    }
    
    /** 
     * Fit to function - a0 + a1*exp(a2*x).
     * @param a The best guess parameter values.
     * @param residuals ymodel - yData.
     * @param covarMat The derivative values of y with respect to fitting parameters.
     */
    public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
        int ctrl;
        int j;
        double ymod = 0;

        try {
            ctrl = ctrlMat[0];
            // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n", 
            // Preferences.DEBUG_ALGORITHM);
            if ( (ctrl == -1) || (ctrl == 1)) {
                
                // evaluate the residuals[j] = ymod - yData[j]
                for (j = 0; j < nPts; j++) {
                	ymod = a[0] + (a[1] * Math.exp(a[2] * xSeries[j]));
                    residuals[j] = ymod - ySeries[j];
                    // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                }
            } // if ((ctrl == -1) || (ctrl == 1))
            else if (ctrl == 2) {
                // Calculate the Jacobian analytically
                for (j = 0; j < nPts; j++) {
                	covarMat[j][0] = 1; // a0 partial derivative
                	covarMat[j][1] = Math.exp(a[2] * xSeries[j]); // a1 partial derivative
                	covarMat[j][2] = a[1] * xSeries[j] * Math.exp(a[2] * xSeries[j]); // a2 partial derivative
                }
            }
            // Calculate the Jacobian numerically
            // else if (ctrl == 2) {
            // ctrlMat[0] = 0;
            // }
        } catch (final Exception exc) {
            Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
        }

        return;
    }

    /**
     * Test data to test fitting of exponential.
     */
    private void testData() {

        // Setup for a function of the type y = c1 + c2*exp(c3*x)

        xSeries[0] = 0;
        ySeries[0] = 1 + 2;

        xSeries[1] = 1;
        ySeries[1] = 1 + (2 * Math.exp(-3));

        xSeries[2] = 2;
        ySeries[2] = 1 + (2 * Math.exp(-6));

        xSeries[3] = 3;
        ySeries[3] = 1 + (2 * Math.exp(-9));

        xSeries[4] = 4;
        ySeries[4] = 1 + (2 * Math.exp(-12));

        // guess();
        gues[0] = -1;
        gues[1] = 2;
        gues[2] = -5;
    }

	@Override
	protected void calculateFittedY() {
		int j;
		yDataFitted = new double[nPts];
		for (j = 0; j < nPts; j++) {
			yDataFitted[j] = a[0] + (a[1] * Math.exp(a[2] * xSeries[j]));	
		}
		
	}

	@Override
	protected Matrix generateResiduals() {
		Matrix residuals = new Matrix(nPts, 1);
    	for(int i=0; i<nPts; i++) {
    		double r = ySeries[i] - (a[0] + (a[1] * Math.exp(a[2] * xSeries[i])));
    		residuals.set(i, 0, r);
    	}
    	
    	return residuals;
	}

	@Override
	protected void calculateChiSq() {
		chisq = getChiSquared();
		System.out.println("Chi-squared = " + chisq);	
	}
}
