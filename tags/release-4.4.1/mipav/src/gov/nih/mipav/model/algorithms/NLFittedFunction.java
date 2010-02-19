package gov.nih.mipav.model.algorithms;

import java.util.Arrays;

import Jama.Matrix;

public abstract class NLFittedFunction extends NLEngine {

	//~ Static fields -------------------------------------------------------------------------------------------
	
	/**Max number of iterations to perform. */
	public static final int MAX_ITR = 50;
	
	/**Min number of iterations to perform. */
	public static final int MIN_ITR = 5;
	
	/**Minimum allowable distance between iterations of a coefficient before considered converged. */
	public static final double EPSILON = .005;
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	
    /** Original x-data */
    protected double[] xDataOrg;

    /** Original y-data */
    protected double[] yDataOrg;
    
    /**Fitted y-data, based on original x points.*/
    protected double[] yDataFitted;
	
	public NLFittedFunction(int pts, int _ma) {
		super(pts, _ma);
	}
	
	public double[] getOriginalX() {
    	return xDataOrg;
    }
    
    public double[] getOriginalY() {
    	return yDataOrg;
    }
    
    public double[] getFittedY() {
    	if(yDataFitted == null)
    		calculateFittedY();
    	return yDataFitted;
    }
    
    /**
     * Calculates chi squared
     */
    protected abstract void calculateChiSq();
    
    /**
     * Calculates yDataFitted
     */
    protected abstract void calculateFittedY();
    
    /**
     * Displays results in a panel with relevant parameters.
     */
    public abstract void displayResults();

	/**
	 * get median of given array
	 */
	protected double getMedian(double[] toSort) {
	    int length = toSort.length;
	
	    Arrays.sort(toSort);
	
	    return toSort[(length / 2)];
	}
	
	/**
	 * Calculates the residuals for a given function, not implemented since some functions might prefer to only use
	 * a subset of data points, or not use yDataFitted if working during an iteration
	 * @return
	 */
	protected abstract Matrix generateResiduals();

}
