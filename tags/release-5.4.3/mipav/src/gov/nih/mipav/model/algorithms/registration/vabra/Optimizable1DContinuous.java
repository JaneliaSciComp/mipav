package gov.nih.mipav.model.algorithms.registration.vabra;

/**
 * Interface for 1D functions.
 * @author Yufeng Guo, Hanlin Wan
 */
public interface Optimizable1DContinuous {
	
	/**
	 * Gets the value of a function.
	 * @param x the input
	 * @return the value of the function
	 */
	public double getValue(double x); 
	
	/**
	 * Gets the minimum input of the function.
	 * @return minimum domain
	 */
	public double getDomainMin(); 
	
	/**
	 * Gets the maximum input of the function.
	 * @return maximum domain
	 */
	public double getDomainMax(); 

	/**
	 * Gets the tolerance.
	 * @return domain tolerance
	 */
	public double getDomainTolerance();

}
