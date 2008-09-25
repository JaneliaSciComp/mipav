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
    
    /**Location in xDataOrg where Gaussian data starts */
    private int dataStart;
    
    /**Location in xDataOrg where Gaussian data ends */
    private int dataEnd;
    
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
    	
    	//determine location of start data, note 
    	//basic thresholding will already have been performed
    	dataStart = 0;
    	for(int i=0; i<yDataOrg.length; i++) {
    		if(yDataOrg[i] != 0 && i > 0) {
    			dataStart = i-1;
    			break;
    		}		
    	}
    	
    	//estimate xInit
    	int maxIndex = 0;
    	double totalDataCount = 0;
    	int numIndexWithData = 0;
    	for(int i=dataStart; i<yDataOrg.length; i++) {
    		if(yDataOrg[i] > yDataOrg[maxIndex]) {
    			maxIndex = i;
    		}
    		if(yDataOrg[i] > 0) {
    			numIndexWithData++;
    			totalDataCount += yDataOrg[i];
    		}
    	}	
    	xInit = xDataOrg[maxIndex];
    	
    	//determine location of end data
    	dataEnd = 0;
    	for(int i=maxIndex; i<yDataOrg.length; i++) {
    		if(yDataOrg[i] == 0) {
    			dataEnd = i;
    			break;
    		}
    	}

    	//find location of one sigma data collection point
    	double dataCollectedOneSigma = yDataOrg[maxIndex], dataCollectedTwoSigma = yDataOrg[maxIndex];
    	int xStopLeftIndex = maxIndex, xStopRightIndex = maxIndex;
    	boolean left = true;
    	while(dataCollectedOneSigma / totalDataCount < .68 && 
    			xStopLeftIndex > dataStart+1 && xStopRightIndex < dataEnd-1) {
    		if(left) 
    			dataCollectedOneSigma += yDataOrg[--xStopLeftIndex];
    		if(!left)
    			dataCollectedOneSigma += yDataOrg[++xStopRightIndex];
    		left = !left;
    	}
    	
    	//estimate one sigma from stopping locations
    	double oneSigmaEstimate = 0;
    	if(dataCollectedOneSigma / totalDataCount >= .68) {
    		double sigmaLeft = Math.abs(xDataOrg[maxIndex] - xDataOrg[xStopLeftIndex]);
    		double sigmaRight = Math.abs(xDataOrg[maxIndex] - xDataOrg[xStopLeftIndex]);
    		oneSigmaEstimate = sigmaLeft + sigmaRight / 2.0;
    	}
    	
    	//find location of two sigma data collection point
    	dataCollectedTwoSigma = dataCollectedOneSigma;
    	while(dataCollectedTwoSigma / totalDataCount < .95 && 
    			xStopLeftIndex > dataStart+1 && xStopRightIndex < dataEnd-1) {
    		if(left) 
    			dataCollectedTwoSigma += yDataOrg[--xStopLeftIndex];
    		if(!left)
    			dataCollectedTwoSigma += yDataOrg[++xStopRightIndex];
    		left = !left;
    	}
    	
    	//estimate two sigma from stopping location
    	double twoSigmaEstimate = 0;
    	if(dataCollectedOneSigma / totalDataCount >= .68) {
    		double sigmaLeft = Math.abs(xDataOrg[maxIndex] - xDataOrg[xStopLeftIndex]);
    		double sigmaRight = Math.abs(xDataOrg[maxIndex] - xDataOrg[xStopLeftIndex]);
    		twoSigmaEstimate = sigmaLeft + sigmaRight / 2.0;
    	}
    	
    	//use both measurements to estimate stdev
    	if(twoSigmaEstimate != 0)
    		sigma = (oneSigmaEstimate + .5*twoSigmaEstimate) / 2;
    	else 
    		sigma = oneSigmaEstimate;
    	
    	//estimate for amplitude
    	amp = yDataOrg[maxIndex];
    	
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
    		System.out.println(i);
    	
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
    	Matrix jacobian = new Matrix(dataEnd - dataStart, 3);
    	for(int i=dataStart; i<dataEnd; i++) {
    		jacobian.set(i-dataStart, 0, dgdA(xDataOrg[i]));
    		jacobian.set(i-dataStart, 1, dgdx(xDataOrg[i]));
    		jacobian.set(i-dataStart, 2, dgdsigma(xDataOrg[i]));
    	}
    	
    	return jacobian;
    }
    
    private Matrix generateResiduals() {
    	Matrix residuals = new Matrix(dataEnd - dataStart, 1);
    	for(int i=dataStart; i<dataEnd; i++) {
    		double r = yDataOrg[i] - gauss(xDataOrg[i]);
    		residuals.set(i-dataStart, 0, r);
    	}
    	
    	return residuals;
    }
    
}
