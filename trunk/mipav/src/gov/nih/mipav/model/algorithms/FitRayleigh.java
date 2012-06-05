package gov.nih.mipav.model.algorithms;


import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * For -infinity < a < +infinity and b > 0
 * y = (2/b)*(x - a)*exp(-((x-a)**2)/b)*u(x - a)
 * where u(x-a) = 0 for x < a and = 1 for x >= a.
 * cumulative function = [1 - exp(-((x-a)**2)/b)]*u(x-a)
 * Mean = a + sqrt(PI*b/4)
 * Variance = b*(4 - PI)/4
 */
public class FitRayleigh extends NLFittedFunction {
    //~ Constructors ---------------------------------------------------------------------------------------------------

	/**
     * Creates a new FitRayleigh object.
     */
    public FitRayleigh() {

        // 10 points, 2 parameters, and function type
        super(10, 2);
        
        bounds = 2; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new double[2];
        bu = new double[2];
        bl[0] = -Double.MAX_VALUE;
        bu[0] = +Double.MAX_VALUE;
        bl[1] = Double.MIN_VALUE;
        bu[1] = Double.MAX_VALUE;
        
        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        outputMes = false;

        setupData();
    }

    /**
     * FitRayleigh.
     *
     * @param  nPoints  number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitRayleigh(int nPoints, float[] xData, float[] yData) {

        // nPoints data points, 2 coefficients, and exponential fitting
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
        
        bounds = 2; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new double[2];
        bu = new double[2];
        bl[0] = -Double.MAX_VALUE;
        bu[0] = +Double.MAX_VALUE;
        bl[1] = Double.MIN_VALUE;
        bu[1] = Double.MAX_VALUE;
        
        // The default is internalScaling = false
        // To make internalScaling = true and have the columns of the
        // Jacobian scaled to have unit length include the following line.
        // internalScaling = true;
        // Suppress diagnostic messages
        outputMes = false;      

        gues[0] = 0; // starting point of distribution
        gues[1] = 1; // Variance * 4/(4 - PI) 

        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Display results of displaying linear fitting parameters.
     */
    public void displayResults() {
        Preferences.debug(" ******* Dump FitRayleigh ********* \n", Preferences.DEBUG_ALGORITHM);

        Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("a0 " + String.valueOf(a[0]) + " +/- " + String.valueOf(Math.sqrt(covarMat[0][0])) + "\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("a1 " + String.valueOf(a[1]) + " +/- " + String.valueOf(Math.sqrt(covarMat[1][1])) + "\n\n", 
        		Preferences.DEBUG_ALGORITHM);

    }
    
    /** 
     * Fit Rayleigh to function.
     * @param a The best guess parameter values.
     * @param residuals ymodel - yData.
     * @param covarMat The derivative values of y with respect to fitting parameters.
     */
    public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
        int ctrl;
        int j;
        double ymod = 0;
        double diff;
        double expon;
        double diffSquared;

        try {
            ctrl = ctrlMat[0];
            // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + "\n", Preferences.DEBUG_ALGORITHM);
            if ( (ctrl == -1) || (ctrl == 1)) {
                
                // evaluate the residuals[j] = ymod - yData[j]
                for (j = 0; j < nPts; j++) {
                    if (xSeries[j] >= a[0]) {
                        diff = xSeries[j] - a[0];
                        ymod = (2.0/a[1])*diff*Math.exp(-diff*diff/a[1]);
                    }
                    else {
                        ymod = 0.0;
                    }
                    residuals[j] = ymod - ySeries[j];
                    // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                }
            } // if ((ctrl == -1) || (ctrl == 1))
            else if (ctrl == 2) {
                // Calculate the Jacobian analytically
                for (j = 0; j < nPts; j++) {
                    if (xSeries[j] >= a[0]) {
                        diff = xSeries[j] - a[0];
                        diffSquared = diff * diff;
                        expon = Math.exp(-diffSquared/a[1]);
                        covarMat[j][0] = (2.0/a[1])*expon*((2.0*diffSquared/a[1]) - 1.0); // a0 partial derivative
                        covarMat[j][1] = (2.0*diff/(a[1]*a[1]))*expon*((diffSquared/a[1]) - 1.0); // a1 partial derivative
                    }
                    else {
                        covarMat[j][0] = 0.0;
                        covarMat[j][1] = 0.0;
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
     * Sets up data to test fitting of Rayleigh distribution.
     */
    private void setupData() {
        
        gues[0] = 0.5;
        gues[1] = 0.5;
        // Actual a[0] = 0 and a[1] = 1

        xSeries[0] = 0;
        ySeries[0] = 0;

        xSeries[1] = 0.25;
        ySeries[1] = 0.5*Math.exp(-0.25*0.25);

        xSeries[2] = 0.5;
        ySeries[2] = Math.exp(-0.5*0.5);

        xSeries[3] = 0.75;
        ySeries[3] = 1.5*Math.exp(-0.75*0.75);

        xSeries[4] = 1.0;
        ySeries[4] = 2.0*Math.exp(-1.0);

        xSeries[5] = 1.25;
        ySeries[5] = 2.5*Math.exp(-1.25*1.25);

        xSeries[6] = 1.5;
        ySeries[6] = 3.0*Math.exp(-1.5*1.5);

        xSeries[7] = 1.75;
        ySeries[7] = 3.5*Math.exp(-1.75*1.75);

        xSeries[8] = 2.0;
        ySeries[8] = 4.0*Math.exp(-2.0*2.0);

        xSeries[9] = 2.25;
        ySeries[9] = 4.5*Math.exp(-2.25*2.25);

    }

	@Override
	protected void calculateFittedY() {
		int j;
		double diff;
		yDataFitted = new double[nPts];
		for (j = 0; j < nPts; j++) {
		    diff = xSeries[j] - a[0];
            yDataFitted[j] = (2.0/a[1])*diff*Math.exp(-diff*diff/a[1]);
		}
		
	}

	@Override
	protected Matrix generateResiduals() {
	    double diff;
		Matrix residuals = new Matrix(nPts, 1);
    	for(int i=0; i<nPts; i++) {
    	    diff = xSeries[i] - a[0];
    		double r = ySeries[i] -  (2.0/a[1])*diff*Math.exp(-diff*diff/a[1]);
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
