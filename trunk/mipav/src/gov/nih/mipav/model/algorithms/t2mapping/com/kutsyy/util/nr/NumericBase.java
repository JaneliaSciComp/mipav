package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.nr;

public class NumericBase
{
    private static String[] errorStrs = {
    "SUCCESS",
    "ERROR_MAXIMUM_ITERATIONS_REACHED",
    "ERROR_OBJECTIVE_FUNCTION_FAILED",
    "ERROR_BRACKETING_FAILED",
    "ERROR_LINE_MINIMIZATION_FAILED",
	};
    
    public final double GOLD = 0.381966011;   // 2-phi
    public final double INVGOLD = 2.61803399; // 1+phi

	public NumericBase() {}

	protected int _maxiter=5, _verbose = 0;
	protected double _tol = 0.001;
	protected ObjectiveFunction _fn;

    public void setTolerance(double tol) { _tol = tol; }
    public double getTolerance() { return _tol; }

    public void setMaxIterations(int mi) { _maxiter = mi; }
    public int getMaxIterations() { return _maxiter; }

    public void setVerbose(int v) { _verbose = v; }
    public int getVerbose() { return _verbose; }

    public void setObjFunction(ObjectiveFunction of) { _fn = of; }
    public ObjectiveFunction getObjFunction() { return _fn; }

	final public static int SUCCESS = 0;
    final public static int ERROR_MAXIMUM_ITERATIONS_REACHED = 1;
	final public static int ERROR_OBJECTIVE_FUNCTION_FAILED = 2;
 	final public static int ERROR_BRACKETING_FAILED = 3;
	final public static int ERROR_LINE_MINIMIZATION_FAILED = 4;

    public static String toString(int code)
    	{ return errorStrs[code]; }
}
