package gov.nih.mipav.model.algorithms;


import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * FitMultiExponential -fits an array of points to an multiexponential of the type 
 * y = a0 + a1*exp(a2*x) + a3*exp(a4*x) + ...
 * where all the exponentials are negative decaying exponentials.
 *
 * @author   William Gandler
 * @see      NLConstrainedEngine
 * @version  0.9
 */
public class FitMultiExponential extends NLFittedFunction {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FitMultiExponential - test constructor, builds and test function.
     */
    public FitMultiExponential() {

        // nPoints data points, 5 coefficients, and multiexponential fitting
        super(5, 5);
        xSeries = new double[5];
        ySeries = new double[5];
        
        bounds = 2; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new double[5];
        bu = new double[5];
        
        bl[0] = -Double.MAX_VALUE;
        bu[0] = Double.MAX_VALUE;
        for (int k = 0; k < (5 - 1)/2; k++) {
        	bl[2*k+1] = -Double.MAX_VALUE;
        	bu[2*k+1] = Double.MAX_VALUE;
        	bl[2*k+2] = -Double.MAX_VALUE;
        	bu[2*k+2] = 0.0;
        }

        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        outputMes = false;
        maxIterations = 5000;
        testData();
    }

    /**
     * FitMultiExponential assuming all exponentials are negative
     *
     * @param  nPoints  number of points in the function
     * @param  nCoefficients number of variables in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitMultiExponential(int nPoints, int nCoefficients, double[] xData, double[] yData) {
        // nPoints data points, nCoefficients coefficients, and exponential fitting
        super(nPoints, nCoefficients);
        this.xSeries = xData;
        this.ySeries = yData;
        
        bounds = 2; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new double[nCoefficients];
        bu = new double[nCoefficients];
        
        bl[0] = -Double.MAX_VALUE;
        bu[0] = Double.MAX_VALUE;
        for (int k = 0; k < (nCoefficients - 1)/2; k++) {
        	bl[2*k+1] = -Double.MAX_VALUE;
        	bu[2*k+1] = Double.MAX_VALUE;
        	bl[2*k+2] = -Double.MAX_VALUE;
        	bu[2*k+2] = 0.0;
        }

        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        maxIterations = 5000;
        outputMes = false;
    }

    /**
     * Constructs new fit multiexponential assuming all exponentials are negative.
     *
     * @param  nPoints  Number of points in the function
     * @param  nCoefficients Number of variables in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitMultiExponential(int nPoints, int nCoefficients, float[] xData, float[] yData) {

        // nPoints data points, nCoefficients coefficients, and exponential fitting
        super(nPoints,nCoefficients);
        xSeries = new double[xData.length];
        ySeries = new double[yData.length];
        int i;
        for (i = 0; i < xData.length; i++) {
        	xSeries[i] = xData[i];
        }
        for (i = 0; i < yData.length; i++) {
        	ySeries[i] = yData[i];
        }
        
        bounds = 2; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new double[nCoefficients];
        bu = new double[nCoefficients];
        
        bl[0] = -Double.MAX_VALUE;
        bu[0] = Double.MAX_VALUE;
        for (int k = 0; k < (nCoefficients - 1)/2; k++) {
        	bl[2*k+1] = -Double.MAX_VALUE;
        	bu[2*k+1] = Double.MAX_VALUE;
        	bl[2*k+2] = -Double.MAX_VALUE;
        	bu[2*k+2] = 0.0;
        }

        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        maxIterations = 5000;
        outputMes = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Starts the analysis.
     */
    public void driver() {
    	// At the last time point assume exponentials are largely decayed and most of what is
    	// left is the constant a[0] term.
        gues[0] = ySeries[ySeries.length-1];
        for (int i = 0; i < (a.length-1)/2; i++) {
        	gues[2*i+1] = 1.0;
        	gues[2*i+2] = -1.0;
        }
        super.driver();
    }

    /**
     * Display results of displaying exponential fitting parameters.
     */
    public void displayResults() {
        Preferences.debug(" ******* FitMultiExponential ********* \n\n", Preferences.DEBUG_ALGORITHM);
        dumpTestResults();
    }
    
    /** 
     * Fit to function - a0 + a1*exp(a2*x) + a3*exp(a4*x) + ...
     * @param a The best guess parameter values.
     * @param residuals ymodel - yData.
     * @param covarMat The derivative values of y with respect to fitting parameters.
     */
    public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
        int ctrl;
        int j, k;
        double ymod = 0;

        try {
            ctrl = ctrlMat[0];
            // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0]  + "\n", 
            // Preferences.DEBUG_ALGORITHM);
            // for (k = 1; k < a.length; k++) {
            //    Preferences.debug("a[k] = " + a[k] + "\n", Preferences.DEBUG_ALGORITHM);
            // }
            if ( (ctrl == -1) || (ctrl == 1)) {
                
                // evaluate the residuals[j] = ymod - yData[j]
                for (j = 0; j < nPts; j++) {
                	ymod = a[0];
                	for (k = 0; k < (a.length - 1)/2; k++) {
                	    ymod += (a[2*k+1] * Math.exp(a[2*k+2] * xSeries[j]));
                	}
                    residuals[j] = ymod - ySeries[j];
                    //Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                }
            } // if ((ctrl == -1) || (ctrl == 1))
            else if (ctrl == 2) {
                // Calculate the Jacobian analytically
                for (j = 0; j < nPts; j++) {
                	covarMat[j][0] = 1; // a0 partial derivative
                	for (k = 0; k < (a.length - 1)/2; k++) {
                	    covarMat[j][2*k+1] = Math.exp(a[2*k+2] * xSeries[j]); // a[2*k+1] partial derivative
                	    covarMat[j][2*k+2] = a[2*k+1] * xSeries[j] * Math.exp(a[2*k+2] * xSeries[j]); // a[2*k+2] partial derivative
                	}
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

        // Setup for a function of the type y = c0 + c1*exp(c2*x) + c3*exp(c4*x)

        xSeries[0] = 0;
        ySeries[0] = 1 + 2 + 3;

        xSeries[1] = 1;
        ySeries[1] = 1 + (2 * Math.exp(-0.3)) + (3*Math.exp(-0.5));

        xSeries[2] = 2;
        ySeries[2] = 1 + (2 * Math.exp(-0.6)) + (3*Math.exp(-1.0));

        xSeries[3] = 3;
        ySeries[3] = 1 + (2 * Math.exp(-0.9)) + (3 * Math.exp(-1.5));

        xSeries[4] = 4;
        ySeries[4] = 1 + (2 * Math.exp(-1.2)) + (3 * Math.exp(-2.0));
    }

	@Override
	protected void calculateFittedY() {
		int j;
		int k;
		yDataFitted = new double[nPts];
		for (j = 0; j < nPts; j++) {
			yDataFitted[j] = a[0];
			for (k = 0; k < (a.length - 1)/2; k++) {
			    yDataFitted[j] += (a[2*k+1] * Math.exp(a[2*k+2] * xSeries[j]));
		    }
		}
		
	}

	@Override
	protected Matrix generateResiduals() {
		Matrix residuals = new Matrix(nPts, 1);
    	for(int i=0; i<nPts; i++) {
    		double r = ySeries[i] - a[0];
    		for (int k = 0; k < (a.length - 1)/2; k++) {
    		    r -= (a[2*k+1] * Math.exp(a[2*k+2] * xSeries[i]));
    		}
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
