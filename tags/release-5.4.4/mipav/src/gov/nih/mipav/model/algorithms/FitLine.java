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

        gues[0] = 0; // y intercept
        gues[1] = -1; // slope

        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Display results of displaying linear fitting parameters.
     */
    public void displayResults() {
        Preferences.debug(" ******* Dump FitLine ********* \n", Preferences.DEBUG_ALGORITHM);

        Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("a0 " + String.valueOf(a[0]) + " +/- " + String.valueOf(Math.sqrt(covarMat[0][0])) + "\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("a1 " + String.valueOf(a[1]) + " +/- " + String.valueOf(Math.sqrt(covarMat[1][1])) + "\n\n", 
        		Preferences.DEBUG_ALGORITHM);

    }
    
    /** 
     * Fit line to function.
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
            // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + "\n", Preferences.DEBUG_ALGORITHM);
            if ( (ctrl == -1) || (ctrl == 1)) {
                
                // evaluate the residuals[j] = ymod - yData[j]
                for (j = 0; j < nPts; j++) {
                	ymod = a[1] * xSeries[j] + a[0];
                    residuals[j] = ymod - ySeries[j];
                    // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                }
            } // if ((ctrl == -1) || (ctrl == 1))
            else if (ctrl == 2) {
                // Calculate the Jacobian analytically
                for (j = 0; j < nPts; j++) {
                	covarMat[j][0] = 1; // a0 partial derivative
                	covarMat[j][1] = xSeries[j]; // a1 partial derivative
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
     * Sets up data to test fitting of exponential.
     */
    private void setupData() {
        
        gues[0] = 1;
        gues[1] = 1;

        xSeries[0] = 0;
        ySeries[0] = 0;

        xSeries[1] = 1;
        ySeries[1] = 1.5;

        xSeries[2] = 2;
        ySeries[2] = 1.75;

        xSeries[3] = 3;
        ySeries[3] = 3.25;

        xSeries[4] = 4;
        ySeries[4] = 4;

        xSeries[5] = 5;
        ySeries[5] = 5.5;

        xSeries[6] = 6;
        ySeries[6] = 5.75;

        xSeries[7] = 7;
        ySeries[7] = 8;

        xSeries[8] = 8;
        ySeries[8] = 8;

        xSeries[9] = 9;
        ySeries[9] = 8.5;

    }

	@Override
	protected void calculateFittedY() {
		int j;
		yDataFitted = new double[nPts];
		for (j = 0; j < nPts; j++) {
			yDataFitted[j] = a[1] * xSeries[j] + a[0];
		}
		
	}

	@Override
	protected Matrix generateResiduals() {
		Matrix residuals = new Matrix(nPts, 1);
    	for(int i=0; i<nPts; i++) {
    		double r = ySeries[i] - (a[1] * xSeries[i] + a[0]);
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
