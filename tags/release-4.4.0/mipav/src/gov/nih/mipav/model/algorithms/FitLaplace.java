package gov.nih.mipav.model.algorithms;

import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * FitLaplace -fits an array of points to a normal curve, general from f = a*exp(Math.abs(x-mu)/beta)
 * Will also perform thresholding techniques to determine useful data points for fitting
 *
 * @author   senseneyj
 * @see      NLEngine
 * @version  0.1
 */
public class FitLaplace extends NLFittedFunction {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
    
    /**Location in xDataOrg where Gaussian data starts */
    private int dataStart;
    
    /**Location in xDataOrg where Gaussian data ends */
    private int dataEnd;
    
    /**Amplitude parameter*/
    private double amp;
    
    /**Center parameter*/
    private double mu;
    
    /**Beta parameter*/
    private double beta;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FitLaplace - test constructor, builds and test function.
     */
    public FitLaplace() {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(5, 3);
        testData();
    }

    /**
     * FitLaplace.
     *
     * @param  nPoints  number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitLaplace(int nPoints, double[] xData, double[] yData) {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints, 3);

        this.xDataOrg = xData;
        this.yDataOrg = yData;
        
        estimateInitial();
        
    }

    /**
     * Constructs new fit laplace distribution.
     *
     * @param  nPoints  Number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitLaplace(int nPoints, float[] xData, float[] yData) {
    	
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
    	int offset = 20;
    	
    	//determine location of start data, note 
    	//basic thresholding will already have been performed
    	dataStart = 0;
    	for(int i=0; i<yDataOrg.length; i++) {
    		if(yDataOrg[i] != 0 && i > 0) {
    			dataStart = i > offset ? i-offset : 0;
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
    	mu = xDataOrg[maxIndex];
    	
    	//determine location of end data
    	dataEnd = 0;
    	for(int i=maxIndex; i<yDataOrg.length; i++) {
    		if(yDataOrg[i] == 0) {
    			dataEnd = i+offset < yDataOrg.length-1 ? i+offset : yDataOrg.length-1;
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
    		beta = (oneSigmaEstimate + .5*twoSigmaEstimate) / 2;
    	else 
    		beta = oneSigmaEstimate;
    	
    	//estimate for amplitude
    	amp = yDataOrg[maxIndex];
    	
    	a[0] = amp;
    	a[1] = mu;
    	a[2] = beta;
    }
    
    /**
     * Starts the analysis. For some reason a guess with the wrong sign for a2 will not converge. Therefore, try both
     * sign and take the one with the lowest chi-squared value.
     */
    public void driver() {
        
    	boolean converged = false;
    	kk = 0;
    	
    	System.out.println("Initial guess\tAmp: "+amp+"\tmu: "+mu+"\tBeta: "+beta);
    	
    	while(!converged && kk < MAX_ITR) {
    		double oldAmp = amp;
        	double oldMu = mu;
        	double oldBeta = beta;
    	
	    	Matrix jacobian = generateJacobian();
	    	Matrix residuals = generateResiduals();
	    	
	    	Matrix lhs = jacobian.transpose().times(jacobian);
	    	Matrix rhs = jacobian.transpose().times(residuals);
	    	
	    	Matrix dLambda = lhs.solve(rhs);
	    	
	    	amp = amp + dLambda.get(0, 0);
	    	mu = mu + dLambda.get(1, 0);
	    	beta = beta + dLambda.get(2, 0);
	    	
	    	System.out.println("Iteration "+kk+"\tAmp: "+amp+"\tmu: "+mu+"\tBeta: "+beta);
	    	
	    	if(Math.abs(Math.abs(oldAmp - amp) / ((oldAmp + amp) / 2)) < EPSILON && 
	    			Math.abs(Math.abs(oldMu - mu) / ((oldMu + mu) / 2)) < EPSILON && 
	    			Math.abs(Math.abs(oldBeta - beta) / ((oldBeta + beta) / 2)) < EPSILON && kk > MIN_ITR) {
	    		converged = true;    		
	    		Preferences.debug("Converged after "+kk+" iterations.");
	    		System.out.println("Converged after "+kk+" iterations.");
	    	} else {
	    		oldAmp = amp;
	    		oldMu = mu;
	    		oldBeta = beta;
	    		kk++;
	    	}
	    	
	    	//always calculate fitted y first, to be used in chisq
	    	calculateFittedY();
	    	calculateChiSq();
    	}
    	
    	if(!converged) {
    		Preferences.debug("Did not converge after "+kk+" iterations.");
    		System.out.println("Did not converge after "+kk+" iterations.");
    	} else {
    		calculateFittedY();
    		calculateChiSq();
    	}
    	
    	//a already initialized in super constructor, used to hold parameters for output
    	a[0] = amp;
    	a[1] = mu;
    	a[2] = beta;
    	
    }
    
    protected void calculateChiSq() {
    	Matrix residuals = generateResiduals();
    	double sum = 0;
    	for(int i=dataStart; i<dataEnd; i++) {
    		double resTemp = residuals.get(i-dataStart, 0);
    		residuals.set(i-dataStart, 0, Math.pow(resTemp, 2)/laplace(xDataOrg[i]));
    		System.out.println("xValue: "+xDataOrg[i]+"\tActual: "+yDataOrg[i]+"\tExpected: "+laplace(xDataOrg[i])+"\tResidual: "+resTemp+"\tChi squared value: "+residuals.get(i-dataStart, 0));
    		sum += Math.pow(resTemp, 2)/laplace(xDataOrg[i]);
    	}
    	chisq = residuals.norm1();
    }

    @Override
	protected void calculateFittedY() {
		yDataFitted = new double[xDataOrg.length];
		for(int i=0; i<xDataOrg.length; i++) {
			yDataFitted[i] = laplace(xDataOrg[i]);
		}
	}

	/**
     * Display results of displaying exponential fitting parameters.
     */
    public void displayResults() {
    	ViewJFrameMessageGraph messageGraph = new ViewJFrameMessageGraph("Fitting Data");
    	
    	messageGraph.append(" ******* FitLaplace Distribution ********* \n\n");
    	messageGraph.append("Number of iterations: " + kk + "\n");
        messageGraph.append("Chi-squared: " + chisq + "\n\n");

        messageGraph.append("Valid for data from "+xDataOrg[dataStart]+" to "+xDataOrg[dataEnd]+" in "+(dataEnd-dataStart)+" parts\n\n");
        
        messageGraph.append("Fitting of gaussian function\n");
        messageGraph.append(" y = " + amp + " * exp(-Math.abs(x- (" + mu +
                            ")))/(" + beta + "))\n");
        messageGraph.append("\n");
        
        messageGraph.append("amp: " + amp + "\n"); 
        messageGraph.append("mu: " + mu + "\n");
        messageGraph.append("beta: " + beta + "\n\n");
        
        if (messageGraph.isVisible() == false) {
        	messageGraph.setLocation(100, 50);
            messageGraph.setSize(500, 300);
            messageGraph.setVisible(true);
        }
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
    private double laplace(double x) {
    	double exp = -Math.abs(x - mu)/beta;
    	
    	double f = amp*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Partial derivative of laplace dist with respect to A.
     */
    private double dLdA(double x) {
    	double exp = -Math.abs(x - mu)/beta;
    	
    	double f = Math.exp(exp);
    	
    	return f;
    	
    }
    
    /**
     * Partial derivative of laplace dist with respect to mu.
     */
    private double dLdmu(double x) {
    	double exp = -Math.abs(x - mu)/beta;
    	
    	//note switch of x-mu to mu-x 
    	double coeff = -(amp * Math.signum(mu-x))/beta;
    	
    	double f = coeff*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Partial derivative of laplace dist with respect to beta.
     */
    private double dLdbeta(double x) {
    	double exp = -Math.abs(x - mu)/beta;
    	
    	double coeff = (amp * Math.abs(x-mu))/Math.pow(beta, 2);
    	
    	double f = coeff*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Jacobian used for non-linear least squares fitting.
     */
    protected Matrix generateJacobian() {
    	Matrix jacobian = new Matrix(dataEnd - dataStart, 3);
    	for(int i=dataStart; i<dataEnd; i++) {
    		jacobian.set(i-dataStart, 0, dLdA(xDataOrg[i]));
    		jacobian.set(i-dataStart, 1, dLdmu(xDataOrg[i]));
    		jacobian.set(i-dataStart, 2, dLdbeta(xDataOrg[i]));
    	}
    	
    	return jacobian;
    }
    
    protected Matrix generateResiduals() {
    	Matrix residuals = new Matrix(dataEnd - dataStart, 1);
    	for(int i=dataStart; i<dataEnd; i++) {
    		double r = yDataOrg[i] - laplace(xDataOrg[i]);
    		residuals.set(i-dataStart, 0, r);
    	}
    	
    	return residuals;
    }
    
}
