package gov.nih.mipav.model.algorithms.registration.vabra;

/**
 * Interface for 1D optimizers.
 * @author Yufeng Guo, Hanlin Wan
 */
public interface Optimizer1DContinuous {
	
	/**
	 * Initializes an Optimizable1DContinuous function.
	 * @param function 1-dimensional continuous function to optimize
	 */
	public void initialize(Optimizable1DContinuous function);
	
	/**
	 * Optimizes a function.
	 * @param findMinima true if find min, false if find max
	 * @return true if optimization successful, false if failed
	 */
	public boolean optimize(boolean findMinima);
	
	/**
	 * Gets the optimized value.
	 * @return the extrema value
	 */
	public double getExtrema();
	
	/**
	 * Gets the number of iterations done.
	 * @return number of iterations
	 */
	public int getIterations();
	
	/**
	 * Gets any status message.
	 * @return status message
	 */
	public String statusMessage(); 
	

}
