package gov.nih.mipav.model.algorithms;



import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * FitLorentz -fits an array of points to a lorentz distribution, thresholding techniques come from ViewJFrameGraph,
 * no need to implement here
 *
 * @author   senseneyj
 * @see      NLConstrainedEngine
 * @version  1.0
 */
public class FitLorentz extends NLFittedFunction {	
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
    
    /**Location in xSeries where Gaussian data starts */
    private int dataStart;
    
    /**Location in xSeries where Gaussian data ends */
    private int dataEnd;
    
    /**Amplitude parameter*/
    private double amp;
    
    /**Center parameter*/
    private double xInit;
    
    /**Gamma parameter*/
    private double gamma;
    
    /** Iterations performed */
    private int iters;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FitLorentz - test constructor, builds and test function.
     */
    public FitLorentz() {

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
    }

    /**
     * FitLorentz.
     *
     * @param  nPoints  number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitLorentz(int nPoints, double[] xData, double[] yData) {

        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints, 3);
        
        this.xSeries = xData;
        this.ySeries = yData;
        
        bounds = 0; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters

        // Suppress diagnostic messages
        outputMes = false;    
        
        //ySeries = applyKernel();
        
        estimateInitial();
        
    }

    /**
     * Constructs new fit lorentz distribution.
     *
     * @param  nPoints  Number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitLorentz(int nPoints, float[] xData, float[] yData) {
    	
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
        
        //ySeries = applyKernel();
        
        estimateInitial();

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Apply small Gaussian kernel to smooth out data.  Note should not be called if doing data comparison.
     */
    private double[] applyKernel() {
    	
        int size = 7;
        int start = size / 2;
        double[] sortY = new double[size];
        double[] newY = new double[ySeries.length];
        double[] gaussY = new double[ySeries.length];
        int end = ySeries.length - start;

        newY[0] = ySeries[0];
        newY[1] = getMedian(new double[] { ySeries[0], ySeries[1], ySeries[2] });
        newY[2] = getMedian(new double[] { ySeries[0], ySeries[1], ySeries[2], ySeries[3], ySeries[4] });

        newY[end] = getMedian(new double[] { ySeries[end - 2], ySeries[end - 1], ySeries[end], ySeries[end + 1], ySeries[end + 2] });
        newY[end + 1] = getMedian(new double[] { ySeries[end], ySeries[end + 1], ySeries[end + 2] });
        newY[end + 2] = ySeries[end + 2];

        for (int i = start; i < end; i++) {
            for (int j = 0; j < size; j++) {
                sortY[j] = ySeries[i + j - start];
            }
            newY[i] = getMedian(sortY);
        }

        GaussianOneDimKernel g = new  GaussianOneDimKernel();
        float[] kernel = g.make(1.0f);
        gaussY[0] = newY[0];
        gaussY[1] = newY[1];
        gaussY[2] = newY[2];

        // System.err.println("gaussY length: " + gaussY.length + " end: " + end);
        end = gaussY.length - 3;

        gaussY[end] = newY[end];
        gaussY[end + 1] = newY[end + 1];
        gaussY[end + 2] = newY[end + 2];

        for (int i = 3; i < end; i++) {
            gaussY[i] = (newY[i - 3] * kernel[0]) + (newY[i - 2] * kernel[1]) + (newY[i - 1] * kernel[2]) +
                        (newY[i] * kernel[3]) + (newY[i + 1] * kernel[4]) + (newY[i + 2] * kernel[5]) +
                        (newY[i + 3] * kernel[6]);
        }
        
        return gaussY;
    }
    
    private void estimateInitial() {
    	int offset = 15;
    	
    	//determine location of start data, note 
    	//basic thresholding will already have been performed
    	dataStart = 0;
    	for(int i=0; i<ySeries.length; i++) {
    		if(ySeries[i] != 0 && i > 0) {
    			dataStart = i > offset ? i-offset : 0;
    			break;
    		}		
    	}
    	
    	//estimate xInit
    	int maxIndex = 0;
    	double totalDataCount = 0;
    	int numIndexWithData = 0;
    	for(int i=dataStart; i<ySeries.length; i++) {
    		if(ySeries[i] > ySeries[maxIndex]) {
    			maxIndex = i;
    		}
    		if(ySeries[i] > 0) {
    			numIndexWithData++;
    			totalDataCount += ySeries[i];
    		}
    	}	
    	xInit = xSeries[maxIndex];
    	
    	//determine location of end data
    	dataEnd = 0;
    	for(int i=maxIndex; i<ySeries.length; i++) {
    		if(ySeries[i] == 0) {
    			dataEnd = i+offset < ySeries.length-1 ? i+offset : ySeries.length-1;
    			break;
    		}
    	}

    	//find location of one sigma data collection point
    	double dataCollectedOneSigma = ySeries[maxIndex], dataCollectedTwoSigma = ySeries[maxIndex];
    	int xStopLeftIndex = maxIndex, xStopRightIndex = maxIndex;
    	boolean left = true;
    	while(dataCollectedOneSigma / totalDataCount < .68 && 
    			xStopLeftIndex > dataStart+1 && xStopRightIndex < dataEnd-1) {
    		if(left) 
    			dataCollectedOneSigma += ySeries[--xStopLeftIndex];
    		if(!left)
    			dataCollectedOneSigma += ySeries[++xStopRightIndex];
    		left = !left;
    	}
    	
    	//estimate one sigma from stopping locations
    	double oneSigmaEstimate = 0;
    	if(dataCollectedOneSigma / totalDataCount >= .68) {
    		double sigmaLeft = Math.abs(xSeries[maxIndex] - xSeries[xStopLeftIndex]);
    		double sigmaRight = Math.abs(xSeries[maxIndex] - xSeries[xStopLeftIndex]);
    		oneSigmaEstimate = sigmaLeft + sigmaRight / 2.0;
    	}
    	
    	//find location of two sigma data collection point
    	dataCollectedTwoSigma = dataCollectedOneSigma;
    	while(dataCollectedTwoSigma / totalDataCount < .95 && 
    			xStopLeftIndex > dataStart+1 && xStopRightIndex < dataEnd-1) {
    		if(left) 
    			dataCollectedTwoSigma += ySeries[--xStopLeftIndex];
    		if(!left)
    			dataCollectedTwoSigma += ySeries[++xStopRightIndex];
    		left = !left;
    	}
    	
    	//estimate two sigma from stopping location
    	double twoSigmaEstimate = 0;
    	if(dataCollectedOneSigma / totalDataCount >= .68) {
    		double sigmaLeft = Math.abs(xSeries[maxIndex] - xSeries[xStopLeftIndex]);
    		double sigmaRight = Math.abs(xSeries[maxIndex] - xSeries[xStopLeftIndex]);
    		twoSigmaEstimate = sigmaLeft + sigmaRight / 2.0;
    	}
    	
    	//use both measurements to estimate stdev
    	if(twoSigmaEstimate != 0)
    		gamma = (oneSigmaEstimate + .5*twoSigmaEstimate) / 2;
    	else 
    		gamma = oneSigmaEstimate;
    	
    	//estimate for amplitude
    	amp = ySeries[maxIndex];
    	
    	a[0] = amp;
    	a[1] = xInit;
    	a[2] = gamma;
    }
    
    /**
     * Starts the analysis. For some reason a guess with the wrong sign for a2 will not converge. Therefore, try both
     * sign and take the one with the lowest chi-squared value.
     */
    public void driver() {
        
    	boolean converged = false;
    	iters = 0;
    	
    	System.out.println("Initial guess\tAmp: "+amp+"\txInit: "+xInit+"\tGamma: "+gamma);
    	
    	while(!converged && iters < MAX_ITR) {
    		double oldAmp = amp;
        	double oldXInit = xInit;
        	double oldSigma = gamma;
    	
	    	Matrix jacobian = generateJacobian();
	    	Matrix residuals = generateResiduals();
	    	
	    	Matrix lhs = jacobian.transpose().times(jacobian);
	    	Matrix rhs = jacobian.transpose().times(residuals);
	    	
	    	Matrix dLambda = lhs.solve(rhs);
	    	
	    	amp = amp + dLambda.get(0, 0);
	    	xInit = xInit + dLambda.get(1, 0);
	    	gamma = gamma + dLambda.get(2, 0);
	    	
	    	System.out.println("Iteration "+iters+"\tAmp: "+amp+"\txInit: "+xInit+"\tGamma: "+gamma);
	    	
	    	if(Math.abs(Math.abs(oldAmp - amp) / ((oldAmp + amp) / 2)) < EPSILON && 
	    			Math.abs(Math.abs(oldXInit - xInit) / ((oldXInit + xInit) / 2)) < EPSILON && 
	    			Math.abs(Math.abs(oldSigma - gamma) / ((oldSigma + gamma) / 2)) < EPSILON && iters > MIN_ITR) {
	    		converged = true;    		
	    		Preferences.debug("Converged after "+iters+" iterations.", Preferences.DEBUG_ALGORITHM);
	    		System.out.println("Converged after "+iters+" iterations.");
	    	} else {
	    		oldAmp = amp;
	    		oldXInit = xInit;
	    		oldSigma = gamma;
	    		iters++;
	    	}
    	}
    	
    	if(!converged) {
    		Preferences.debug("Did not converge after "+iters+" iterations.", Preferences.DEBUG_ALGORITHM);
    		System.out.println("Did not converge after "+iters+" iterations.");
    	} else {
    		calculateFittedY();
    		calculateChiSq();
    	}
    	
    	//a already initialized in super constructor, used to hold parameters for output
    	a[0] = amp;
    	a[1] = xInit;
    	a[2] = gamma;
    	
    }
    
    protected void calculateChiSq() {
    	Matrix residuals = generateResiduals();
    	double sum = 0;
    	for(int i=dataStart; i<dataEnd; i++) {
    		double resTemp = residuals.get(i-dataStart, 0);
    		residuals.set(i-dataStart, 0, Math.pow(resTemp, 2)/lorentz(xSeries[i]));
    		System.out.println("xValue: "+xSeries[i]+"\tActual: "+ySeries[i]+"\tExpected: "+lorentz(xSeries[i])+"\tResidual: "+resTemp+"\tChi squared value: "+residuals.get(i-dataStart, 0));
    		sum += Math.pow(resTemp, 2)/lorentz(xSeries[i]);
    	}
    	System.out.println("Sum "+sum+"\tcompared to chisq "+chisq);
    	chisq = residuals.norm1();
    }

    @Override
	protected void calculateFittedY() {
		yDataFitted = new double[xSeries.length];
		for(int i=0; i<xSeries.length; i++) {
			yDataFitted[i] = lorentz(xSeries[i]);
		}
	}

	/**
     * Display results of displaying exponential fitting parameters.
     */
    public void displayResults() {
    	ViewJFrameMessageGraph messageGraph = new ViewJFrameMessageGraph("Fitting Data");
    	
    	messageGraph.append(" ******* FitLorentz ********* \n\n");
    	messageGraph.append("Number of iterations: " + iters + "\n");
        messageGraph.append("Chi-squared: " + chisq + "\n\n");

        messageGraph.append("Valid for data from "+xSeries[dataStart]+" to "+xSeries[dataEnd]+" in "+(dataEnd-dataStart)+" parts\n\n");
        
        messageGraph.append("Fitting of gaussian function\n");
        messageGraph.append(" y = .5 * " + amp + " * exp(sqrt(x-" + xInit +
                            ")/(" + gamma + "^3))\n");
        messageGraph.append("\n");
        
        messageGraph.append("amp: " + amp + "\n"); 
        messageGraph.append("Xo: " + xInit + "\n");
        messageGraph.append("sigma: " + gamma + "\n\n");
        
        if (messageGraph.isVisible() == false) {
        	messageGraph.setLocation(100, 50);
            messageGraph.setSize(500, 300);
            messageGraph.setVisible(true);
        }
    }
    
    public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
    	// not used since explicit derivs are calculated, see driver()
    }
    
    /**
     * Lorentz distribution evaluated at a point with given parameters
     */
    private double lorentz(double x) {
    	double exp = -Math.pow(x-xInit, .5) / (Math.pow(gamma, 3));
    	
    	double f = amp*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Partial derivative of Lorentz distribution with respect to A.
     */
    private double dLdA(double x) {
    	double exp = -Math.pow(x-xInit, .5) / (Math.pow(gamma, 3));
    	
    	double f = Math.exp(exp);
    	
    	return f;
    	
    }
    
    /**
     * Partial derivative of Lorentz distribution with respect to x.
     */
    private double dLdx(double x) {
    	double exp = -.5*Math.pow(x-xInit, -.5) / (Math.pow(gamma, 3));
    	
    	double coeff = (amp * (x-xInit))/(Math.pow(gamma, 2));
    	
    	double f = coeff*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Partial derivative of Lorentz distribution with respect to gamma.
     */
    private double dLdgamma(double x) {
    	double exp = -Math.pow(x-xInit, .5) / (Math.pow(gamma, 3));
    	
    	double coeff = (amp/3 * Math.pow(x-xInit, .5))/(Math.pow(gamma, 4));
    	
    	double f = coeff*Math.exp(exp);
    	
    	return f;
    }
    
    /**
     * Jacobian used for non-linear least squares fitting.
     */
    protected Matrix generateJacobian() {
    	Matrix jacobian = new Matrix(dataEnd - dataStart, 3);
    	for(int i=dataStart; i<dataEnd; i++) {
    		jacobian.set(i-dataStart, 0, dLdA(xSeries[i]));
    		jacobian.set(i-dataStart, 1, dLdx(xSeries[i]));
    		jacobian.set(i-dataStart, 2, dLdgamma(xSeries[i]));
    	}
    	
    	return jacobian;
    }
    
    protected Matrix generateResiduals() {
    	Matrix residuals = new Matrix(dataEnd - dataStart, 1);
    	for(int i=dataStart; i<dataEnd; i++) {
    		double r = ySeries[i] - lorentz(xSeries[i]);
    		residuals.set(i-dataStart, 0, r);
    	}
    	
    	return residuals;
    }
    
}
