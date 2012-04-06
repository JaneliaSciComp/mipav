package gov.nih.mipav.model.algorithms;



import Jama.Matrix;
import gov.nih.mipav.view.*;


/**
 * FitMultiExp - set of points to multi-exponential, general from f = a*exp(-(x-b)^2/2sigma^2)
 * Will also perform thresholding techniques to determine useful data points for fitting
 *
 * @author   senseneyj
 * @see      NLConstrainedEngine
 * @version  0.1
 */
public class FitMultiExp extends NLFittedFunction {

    /**Location in xSeries where Gaussian data starts */
    private int dataStart;
    
    /**Location in xSeries where Gaussian data ends */
    private int dataEnd;
    
    /**Amplitude parameter*/
    private double amp;
    
    /**Center parameter*/
    private double xInit;
    
    /**Sigma parameter*/
    private double sigma;
    
    /**R squared*/
    private double rSquared;
    
    private int iters;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FitGaussian - test constructor, builds and test function.
     */
    public FitMultiExp() {

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
     * FitGaussian.
     *
     * @param  nPoints  number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitMultiExp(int nPoints, double[] xData, double[] yData) {

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
        
        //ySeries = applyKernel();
        
        estimateInitial();
        
    }

    /**
     * Constructs new fit gaussian.
     *
     * @param  nPoints  Number of points in the function
     * @param  xData    DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public FitMultiExp(int nPoints, float[] xData, float[] yData) {
        
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
            sigma = (oneSigmaEstimate + .5*twoSigmaEstimate) / 2;
        else 
            sigma = oneSigmaEstimate;
        
        //estimate for amplitude
        amp = ySeries[maxIndex];
        
        a[0] = amp;
        a[1] = xInit;
        a[2] = sigma;
    }
    
    /**
     * Starts the analysis. For some reason a guess with the wrong sign for a2 will not converge. Therefore, try both
     * sign and take the one with the lowest chi-squared value.
     */
    public void driver() {
        
        boolean converged = false;
        iters = 0;
        
        System.out.println("Initial guess:\tAmp: "+amp+"\txInit: "+xInit+"\tSigma: "+sigma);
        
        while(!converged && iters < MAX_ITR) {
            double oldAmp = amp;
            double oldXInit = xInit;
            double oldSigma = sigma;
        
            Matrix jacobian = generateJacobian();
            Matrix residuals = generateResiduals();
            
            Matrix lhs = jacobian.transpose().times(jacobian);
            Matrix rhs = jacobian.transpose().times(residuals);
            
            Matrix dLambda = lhs.solve(rhs);
            
            amp = amp + dLambda.get(0, 0);
            xInit = xInit + dLambda.get(1, 0);
            sigma = sigma + dLambda.get(2, 0);
            
            System.out.println("Iteration "+iters+"\tAmp: "+amp+"\txInit: "+xInit+"\tSigma: "+sigma);
            
            if(Math.abs(Math.abs(oldAmp - amp) / ((oldAmp + amp) / 2)) < EPSILON && 
                    Math.abs(Math.abs(oldXInit - xInit) / ((oldXInit + xInit) / 2)) < EPSILON && 
                    Math.abs(Math.abs(oldSigma - sigma) / ((oldSigma + sigma) / 2)) < EPSILON && iters > MIN_ITR) {
                converged = true;           
                Preferences.debug("Converged after "+iters+" iterations.", Preferences.DEBUG_ALGORITHM);
                System.out.println("Converged after "+iters+" iterations.");
            } else {
                oldAmp = amp;
                oldXInit = xInit;
                oldSigma = sigma;
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
        a[2] = sigma;
    }
    
    protected void calculateChiSq() {
        Matrix residuals = generateResiduals();
        double sum = 0;
        double resSum = 0;
        for(int i=dataStart; i<dataEnd; i++) {
            double resTemp = residuals.get(i-dataStart, 0);
            resSum += Math.abs(resTemp);
            if(gauss(xSeries[i]) > .01)
                residuals.set(i-dataStart, 0, Math.pow(resTemp, 2)/gauss(xSeries[i]));
            else
                residuals.set(i-dataStart, 0, 0);
            System.out.println("xValue: "+xSeries[i]+"\tActual: "+ySeries[i]+"\tExpected: "+gauss(xSeries[i])+"\tResidual: "+resTemp+"\tChi squared value: "+residuals.get(i-dataStart, 0));
            sum += Math.pow(resTemp, 2)/gauss(xSeries[i]);
        }
        
        chisq = residuals.norm1();
        System.out.println("Sum "+sum+"\tcompared to chisq "+chisq);
        rSquared = 1.0-(chisq/resSum);
        System.out.println("Residual sum: "+resSum+"\tChisquared: "+chisq+"\trSquared: "+rSquared);
        
    }

    @Override
    protected void calculateFittedY() {
         yDataFitted = new double[xSeries.length];
         for(int i=0; i<xSeries.length; i++) {
             yDataFitted[i] = gauss(xSeries[i]);
         }
    }

    /**
     * Display results of displaying exponential fitting parameters.
     */
    public void displayResults() {
        ViewJFrameMessageGraph messageGraph = new ViewJFrameMessageGraph("Fitting Data");
        
        messageGraph.append(" ******* FitGaussian ********* \n\n");
        messageGraph.append("Number of iterations: " + iters + "\n");
        messageGraph.append("Chi-squared: " + chisq + "\n");
        //messageGraph.append("R-squared: "+ rSquared +"\n\n");

        messageGraph.append("Valid for data from "+xSeries[dataStart]+" to "+xSeries[dataEnd]+" in "+(dataEnd-dataStart)+" parts\n\n");
        
        messageGraph.append("Fitting of gaussian function\n");
        messageGraph.append(" y = " + amp + " * exp(-(x-" + xInit +
                            ")^2/(2*" + sigma + "^2))\n");
        messageGraph.append("\n");
        
        messageGraph.append("amp: " + amp + "\n"); 
        messageGraph.append("Xo: " + xInit + "\n");
        messageGraph.append("sigma: " + sigma + "\n\n");
        
        if (messageGraph.isVisible() == false) {
            messageGraph.setLocation(100, 50);
            messageGraph.setSize(500, 300);
            messageGraph.setVisible(true);
        }
    }
    
    public double getRSquared() {
        return rSquared;
    }

    public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
        // not used
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
    protected Matrix generateJacobian() {
        Matrix jacobian = new Matrix(dataEnd - dataStart, 3);
        for(int i=dataStart; i<dataEnd; i++) {
            jacobian.set(i-dataStart, 0, dgdA(xSeries[i]));
            jacobian.set(i-dataStart, 1, dgdx(xSeries[i]));
            jacobian.set(i-dataStart, 2, dgdsigma(xSeries[i]));
        }
        
        return jacobian;
    }
    
    protected Matrix generateResiduals() {
        Matrix residuals = new Matrix(dataEnd - dataStart, 1);
        for(int i=dataStart; i<dataEnd; i++) {
            double r = ySeries[i] - gauss(xSeries[i]);
            residuals.set(i-dataStart, 0, r);
        }
        
        return residuals;
    }
    
}
