package gov.nih.mipav.model.algorithms.registration.vabra;

/**
 * Finds the maximum of 1D functions by negating the function and finding the minimum.
 * @author Hanlin Wan
 */
public class MinToMax1D implements Optimizable1DContinuous {

	private Optimizable1DContinuous myFunction;
	
	/**
	 * Constructor to create the negated function
	 * @param func function to negate
	 */
	public MinToMax1D(Optimizable1DContinuous func) {
		myFunction = func;
	}
	
	/**
	 * Gets the domain maximum
	 * @return domain max
	 */
	public double getDomainMax() {	
		return myFunction.getDomainMax();
	}

	/**
	 * Gets the domain minimum
	 * @return domain min
	 */
	public double getDomainMin() {
		return myFunction.getDomainMin();
	}

	/**
	 * Gets the tolerance
	 * @return tolerance
	 */
	public double getDomainTolerance() {
		return myFunction.getDomainTolerance();
	}
	
	/**
	 * Gets the negated value
	 * @param x location to get value at
	 * @return negated value at location x
	 */
	public double getValue(double x) {
		return -1 * myFunction.getValue(x);
	}
}
