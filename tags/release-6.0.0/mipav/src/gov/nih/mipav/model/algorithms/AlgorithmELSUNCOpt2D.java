package gov.nih.mipav.model.algorithms;


import java.util.Hashtable;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmConstELSUNCOpt3D.FitOAR3DConstrainedModel;
import gov.nih.mipav.model.structures.TransMatrixd;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.Preferences;


/**
 * Runs ELSUNC, LEVENBERG_MARQUARDT, or NL2SOL for a 2D image.
 * 
 * <hr>
 * 
 * <p>Based on ELSUNC allowed by the author with acknowledgement:</p>
 * 
 * <p>Gauss-Newton Based Algorithms For Constrained Nonlinear Least Squares Problems by Per Lindstrom and Per-Ake Wedin,
 * Institute of Information Processing, University of Umea, S-901 87 Umea, Sweden This can be downleaded from
 * http://www.cs.umu.se/~perl/reports/alg.ps.gz</p>
 *
 * @version  0.1 March 22, 2012
 * @author William Gandler
 */
public class AlgorithmELSUNCOpt2D extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	// Different search algorithms
	private final int ELSUNC = 1;
	
	private final int LEVENBERG_MARQUARDT = 2;
	
	private final int NL2SOL = 3;
	
	private int searchAlgorithm;

    /** Flag indicating this is a rigid transformation. */
    private boolean rigid = false;
    
    /** Degress of freedom. */
    private int nDims;
    
    /** Cost function called to measure cost - 1D. */
    private AlgorithmOptimizeFunctionBase costFunction;
    
    /** Array of tolerances for each dimension. */
    private double[] OARTolerance;
    
    /** The maximum number of iterations the optimization allows. */
    private int maxIterations;
    
    /** Parent algorithm that called this optimization. */
    private AlgorithmBase parent;
    
    /** The transformation matrix to the origin of the input image. */
    private TransMatrixd toOrigin;
    
    /** The transformation matrix from the origin of the input image. */
    private TransMatrixd fromOrigin;
    
    /**
     * Array used to hold the initial points, final points and costs
     */
    private Vectornd[] points;
    
    /** The cost of the function at the best minimum. */
    private double functionAtBest;
    
    private double minFunctionAtBest;
    
    FitOAR2DELSUNCModel eModel;
    
    FitOAR2DNL2solModel nModel;
    
    FitOAR2DLMModel lmModel;
    
    private int status;
    
    /** Point that was initially passed into function. */
    private double[] start;
    
    /**
     * The flag to indicate whether the searching path need to be recorded.
     */
    private boolean pathRecorded = false;
    
    /**
     * Store the paths for every thread.
     */
    protected Hashtable<Long, Vector<Vector<Vector3f>>> paths = new Hashtable<Long, Vector<Vector<Vector3f>>>();
    
 // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = Math.pow(2, -52);
    private double huge = Double.MAX_VALUE; 

    /**
     * Constructs a new algorithm with the given centers of mass (needed for setting the transformations), the given
     * cost function (which was constructed with the proper images), the initial point we're looking at, some tolerance
     * within that point to look for the minimum, and the maximum number of iterations.
     *
     * @param  parent           Algorithm that called this optimization.
     * @param  com              Center of Mass of the input image.
     * @param  degreeOfFreedom  Degree of freedom for transformation (must be 2, 3, 4, 5, 7).
     * @param  costFunc         Cost function to use.
     * @param  tols             Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param  maxIter          Maximum number of iterations.
     * @param  _rigid           <code>true</code> means this was a rigid transformation
     * @param searchAlgorithm   ELSUNC, LEVENBERG_MARQUARDT, or NL2SOL;
     */
    public AlgorithmELSUNCOpt2D(AlgorithmBase parent, Vector2f com, int degreeOfFreedom,
                                AlgorithmOptimizeFunctionBase costFunc, double[] tols, int maxIter,
                                boolean _rigid, int searchAlgorithm) {
    	nDims = degreeOfFreedom;
        costFunction = costFunc;
        OARTolerance = tols;
        maxIterations = maxIter;
        this.parent = parent;

        this.rigid = _rigid;
        this.searchAlgorithm = searchAlgorithm;
        toOrigin = new TransMatrixd(3);
        toOrigin.setTranslate(com.X, com.Y);

        fromOrigin = new TransMatrixd(3);
        fromOrigin.setTranslate(-com.X, -com.Y);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Sets everything to null and prepares this class for destruction.
     */
    public void disposeLocal() {
        costFunction = null;
        OARTolerance = null;
        toOrigin = null;
        fromOrigin = null;
    }
    
    /**
     * Return an array of transformation vector. The meanings of those 
     * transformation vector are as following:
     *      the initial transformation vector:  before algorithm is performed
     *      the final transformation vector:    after algorithm was performed.
     * @return
     */
    public Vectornd[] getPoints() {
        return points;
    }

    /**
     * Sets the transformation vectors.
     * @param points    the transformation vectors.
     */
    public void setPoints(Vectornd[] points) {
        this.points = points;
    }
    
    public void runAlgorithm() {
    	switch(searchAlgorithm) {
    	case ELSUNC:
    		runELSUNC();
    		break;
    	case NL2SOL:
    		runNL2sol();
    		break;
    	case LEVENBERG_MARQUARDT:
    		runLM();
    		break;
    	}
    }
    
    /**
     * Runs ELSUNC along one dimension at a time as long as the costFunction improves during one cycle
     * of runs along every dimension.
     */
    private void runELSUNC() {
    	int i, j;
    	boolean anotherCycle;
    	double[] lastPoint = new double[nDims];
        // Initialize data.
        functionAtBest = Double.MAX_VALUE;
        minFunctionAtBest = Double.MAX_VALUE;
        
        for(i = 0; i < points.length; i++){
        	if (points[i] == null) {
        		continue;
        	}
        	anotherCycle = true;
        	start = points[i].getPoint();
        	double[] point = extractPoint(points[i].getPoint());
        	
        	/**
             * Prepare for recording the search path.
             */
            Vector<Vector3f> path = null;
            if (pathRecorded && (nDims == 3)) {
                path = new Vector<Vector3f>();
            }

	        while (anotherCycle) {
	        	anotherCycle = false;
		        for (j = 0; j < nDims; j++) {
		        	lastPoint[j] = point[j];
			        eModel = new FitOAR2DELSUNCModel(j,point);
			        eModel.driver();
			        status = eModel.getExitStatus();
			        //eModel.statusMessage(status);
			        //eModel.dumpResults();
			        //status == -2 if maxIterations reached
			        if ((status > 0) || (status == -2)) {
				        double params[] = eModel.getParameters();
				        point[j] = params[0];
				        double[]fullPoint = getFinal(point);
				        functionAtBest = costFunction.cost(convertToMatrix(fullPoint));
				        if (functionAtBest < minFunctionAtBest) {
				        	minFunctionAtBest = functionAtBest;
				        	if (Math.abs(point[j] - lastPoint[j]) > OARTolerance[j]) {
				        	    anotherCycle = true;
				        	}
				        	if (pathRecorded && nDims == 3) {
		                        path.add(new Vector3f((float)point[0], (float)point[1], (float)point[2]));
		                    }
				        }
				        else {
				        	point[j] = lastPoint[j];
				        }
			        } // if (status > 0)
			        else {
			        	point[j] = lastPoint[j];
			        }
		        } // for (j = 0; j < nDims; j++)
	        } // while (anotherCycle)
	        /**
	         * Store the minimum cost and corresponding vector to v.
	         */
	        updatePoint(point, minFunctionAtBest, points[i]);

	        if (pathRecorded && nDims == 3) {
	            long threadId = Thread.currentThread().getId();
	            Vector<Vector<Vector3f>> pathList = paths.get(threadId);
	            if (pathList == null) {
	                pathList = new Vector<Vector<Vector3f>>();
	                paths.put(threadId, pathList);
	            }
	            pathList.add(path);
	        }
        } // for i = 0; i < points.length; i++)
        
    }  // private void runELSUNC()
    
    private void runNL2sol() {
    	int i, j;
    	boolean anotherCycle;
    	double[] lastPoint = new double[nDims];
        // Initialize data.
        functionAtBest = Double.MAX_VALUE;
        minFunctionAtBest = Double.MAX_VALUE;
        // Number of parameters plus 1
        // x[1] receives the initial guess and returns the final output
        double x[] = new double[2];
        // 61 + number of parameters
        int iv[] = new int[62];
        // n = number of equations or functions = 1
        // p = number of parameters = 1
        // v........ (input/output) a floating-point value array of length at
		//                  least 93 + n*p + 3*n + p*(3*p+33)/2 that helps con-
		//                 trol the nl2sol algorithm and that is used to store
	    //                  various intermediate quantities.
        // Add 1 more to vLength to go from index base 0 to index base 1.
        int vLength = 93 + 1*1 + 3*1 + 1*(3*1+33)/2 + 1;
        double v[] = new double[vLength];
        final int mxfcal = 17;
        int maxFunctionEvaluations = 270;
        final int mxiter = 18;
        int maxIterations = 200;
        final int prunit = 21;
        final int xctol = 33;
        
        for(i = 0; i < points.length; i++){
        	if (points[i] == null) {
        		continue;
        	}
        	anotherCycle = true;
        	start = points[i].getPoint();
        	double[] point = extractPoint(points[i].getPoint());
        	
        	/**
             * Prepare for recording the search path.
             */
            Vector<Vector3f> path = null;
            if (pathRecorded && (nDims == 3)) {
                path = new Vector<Vector3f>();
            }

	        while (anotherCycle) {
	        	anotherCycle = false;
		        for (j = 0; j < nDims; j++) {
		        	lastPoint[j] = point[j];
		        	x[1] = point[j];
		        	dfault(iv,v);
		        	// iv(mxfcal)... iv(17) gives the maximum number of function evaluations
		    		//             (calls on calcr, excluding those used to compute the co-
		    		//             variance matrix) allowed.  if this number does not suf-
		    		//             fice, then nl2sol returns with iv(1) = 9.  default = 200.
		        	iv[mxfcal] = maxFunctionEvaluations;
		        	//iv(mxiter)... iv(18) gives the maximum number of iterations allowed.
		    		//             it also indirectly limits the number of gradient evalua-
		    		//             tions (calls on calcj, excluding those used to compute
		    		//             the covariance matrix) to iv(mxiter) + 1.  if iv(mxiter)
		    		//             iterations do not suffice, then nl2sol returns with
		    		//             iv(1) = 10.  default = 150.
		        	iv[mxiter] = maxIterations;
		        	//iv(prunit)... iv(21) is the output unit number on which all printing
		    		//             is done.  iv(prunit) = 0 means suppress all printing.
		    		//             (setting iv(prunit) to 0 is the only way to suppress the
		    		//             one-line termination reason message printed by itsmry.)
		    		//             default = standard output unit (unit 6 on most systems).
		        	iv[prunit] = 0;
		        	//v(xctol).... v(33) is the x-convergence tolerance.  if a Newton step
		    		//             (see v(nreduc)) is tried that has v(reldx) <= v(xctol)
		    		//             and if this step yields at most twice the predicted func-
		    		//             tion decrease, then nl2sol returns with iv(1) = 3 (or 5).
		    		//             (see the description of v(reldx) below.)
		    		//             default = machep**0.5, where machep is the unit roundoff.
		        	v[xctol] = OARTolerance[j];
		        	//iv(1)...  on input, iv(1) should have a value between 0 and 12......
		    		//             0 and 12 mean this is a fresh start.  0 means that
		    		//             dfault(iv, v) is to be called to provide all default
		    		//             values to iv and v.  12 (the value that dfault assigns to
		    		//             iv(1)) means the caller has already called dfault(iv, v)
		    		//             and has possibly changed some iv and/or v entries to non-
		    		//             default values.  default = 12.
		        	iv[1] = 12;
			        nModel = new FitOAR2DNL2solModel(x,iv,v,j,point);
			        nModel.nl2sno();
			        status = iv[1];
			        //nModel.statusMessageNL2sol(status, 1);
			        //nModel.dumpResults();
			        /* iv(1)........ on output, iv(1) is a return code....
					!             3 = x-convergence.  the scaled relative difference be-
					!                  tween the current parameter vector x and a locally
					!                  optimal parameter vector is very likely at most
					!                  v(xctol).
					!             4 = relative function convergence.  the relative differ-
					!                  ence between the current function value and its lo-
					!                  cally optimal value is very likely at most v(rfctol).
					!             5 = both x- and relative function convergence (i.e., the
					!                  conditions for iv(1) = 3 and iv(1) = 4 both hold).
					!             6 = absolute function convergence.  the current function
					!                  value is at most v(afctol) in absolute value.
					!             7 = singular convergence.  the hessian near the current
					!                  iterate appears to be singular or nearly so, and a
					!                  step of length at most v(lmax0) is unlikely to yield
					!                  a relative function decrease of more than v(rfctol).
					!             8 = false convergence.  the iterates appear to be converg-
					!                  ing to a noncritical point.  this may mean that the
					!                  convergence tolerances (v(afctol), v(rfctol),
					!                  v(xctol)) are too small for the accuracy to which
					!                  the function and gradient are being computed, that
					!                  there is an error in computing the gradient, or that
					!                  the function or gradient is discontinuous near x.
					!             9 = function evaluation limit reached without other con-
					!                  vergence (see iv(mxfcal)).
					!            10 = iteration limit reached without other convergence
					!                  (see iv(mxiter)).
					!            11 = stopx returned .true. (external interrupt).  see the
					!                  usage notes below.
					!            13 = f(x) cannot be computed at the initial x.
					!            14 = bad parameters passed to assess (which should not
					!                  occur).
					!            15 = the jacobian could not be computed at x (see calcj
					!                  above).
					!            16 = n or p (or parameter nn to nl2itr) out of range --
					!                  p <= 0 or n < p or nn < n.
					!            17 = restart attempted with n or p (or par. nn to nl2itr)
					!                  changed.
					!            18 = iv(inits) is out of range.
					!            19...45 = v(iv(1)) is out of range.
					!            50 = iv(1) was out of range.
					!            87...(86+p) = jtol(iv(1)-86) (i.e., v(iv(1)) is not
					!                  positive (see v(dfac) below).*/
			        if (((status >= 3) && (status <= 6)) || (status == 9) || (status == 10)) {
				        point[j] = x[1];
				        double[]fullPoint = getFinal(point);
				        functionAtBest = costFunction.cost(convertToMatrix(fullPoint));
				        if (functionAtBest < minFunctionAtBest) {
				        	minFunctionAtBest = functionAtBest;
				        	if (Math.abs(point[j] - lastPoint[j]) > OARTolerance[j]) {
				        	    anotherCycle = true;
				        	}
				        	if (pathRecorded && nDims == 3) {
		                        path.add(new Vector3f((float)point[0], (float)point[1], (float)point[2]));
		                    }
				        }
				        else {
				        	point[j] = lastPoint[j];
				        }
			        } // if (((status >= 3) && (status <= 6)) || (status == 9) || (status == 10))
			        else {
			        	point[j] = lastPoint[j];
			        }
		        } // for (j = 0; j < nDims; j++)
	        } // while (anotherCycle)
	        /**
	         * Store the minimum cost and corresponding vector to v.
	         */
	        updatePoint(point, minFunctionAtBest, points[i]);

	        if (pathRecorded && nDims == 3) {
	            long threadId = Thread.currentThread().getId();
	            Vector<Vector<Vector3f>> pathList = paths.get(threadId);
	            if (pathList == null) {
	                pathList = new Vector<Vector<Vector3f>>();
	                paths.put(threadId, pathList);
	            }
	            pathList.add(path);
	        }
        } // for i = 0; i < points.length; i++)
        	
    }
    
    private void dfault ( int iv[], double v[] ) {

    	/***********************************************************************
    	!
    	!! DFAULT supplies default values to IV and V.
    	!
    	!  Discussion:
    	!
    	!    Only entries in the first 25 positions of IV and the first 45
    	!    positions of V are reset.
    	! 
    	!  Modified:
    	!
    	!    05 April 2006
    	!
    	!  Author:
    	!
    	!    David Gay
    	!
    	!  Parameters:
    	!
    	!    Output, integer IV(25), contains default values for specific entries.
    	!
    	!    Output, real V(45), contains default values for specific values.
    	*/

    	  int afctol = 31;
    	  int cosmin = 43;
    	  int covprt = 14;
    	  int covreq = 15;
    	  int d0init = 37;
    	  int decfac = 22;
    	  int delta0 = 44;
    	  int dfac = 41;
    	  int dinit = 38;
    	  int dltfdc = 40;
    	  int dltfdj = 36;
    	  int dtype = 16;
    	  int inits = 25;
    	  int epslon = 19;
    	  int fuzz = 45;
    	  int incfac = 23;
    	  int jtinit = 39;
    	  int lmax0 = 35;
    	  double machep;
    	  double mepcrt;
    	  int mxfcal = 17;
    	  int mxiter = 18;
    	  int outlev = 19;
    	  int parprt = 20;
    	  int phmnfc = 20;
    	  int phmxfc = 21;
    	  int prunit = 21;
    	  int rdfcmn = 24;
    	  int rdfcmx = 25;
    	  int rfctol = 32;
    	  int rlimit = 42;
    	  int solprt = 22;
    	  double sqteps;
    	  int statpr = 23;
    	  int tuner1 = 26;
    	  int tuner2 = 27;
    	  int tuner3 = 28;
    	  int tuner4 = 29;
    	  int tuner5 = 30;
    	  int x0prt = 24;
    	  int xctol = 33;
    	  int xftol = 34;

    	  iv[1] = 12;
    	  iv[covprt] = 1;
    	  iv[covreq] = 1;
    	  iv[dtype] = 1;
    	  iv[inits] = 0;
    	  iv[mxfcal] = 200;
    	  iv[mxiter] = 150;
    	  iv[outlev] = -1;
    	  iv[parprt] = 1;
    	  iv[prunit] = 6;
    	  iv[solprt] = 1;
    	  iv[statpr] = 1;
    	  iv[x0prt] = 1;

    	  machep = epsilon;
    	  v[afctol] = 1.0e-20;
    	  if ( 1.0e-10 < machep ) { 
    	    v[afctol] = machep*machep;
    	  }
    	  v[cosmin] = Math.max ( 1.0e-06, 1.0e+02 * machep );
    	  v[decfac] = 0.5;
    	  sqteps = Math.sqrt (epsilon);
    	  v[delta0] = sqteps;
    	  v[dfac] = 0.6;
    	  v[dinit] = 0.0;
    	  mepcrt = Math.pow(machep ,( 1.0 / 3.0) );
    	  v[dltfdc] = mepcrt;
    	  v[dltfdj] = sqteps;
    	  v[d0init] = 1.0;
    	  v[epslon] = 0.1;
    	  v[fuzz] = 1.5;
    	  v[incfac] = 2.0;
    	  v[jtinit] = 1.0e-6;
    	  v[lmax0] = 100.0;
    	  v[phmnfc] = -0.1;
    	  v[phmxfc] = 0.1;
    	  v[rdfcmn] = 0.1;
    	  v[rdfcmx] = 4.0;
    	  v[rfctol] = Math.max ( 1.0E-10, mepcrt*mepcrt );
    	  v[rlimit] = Math.sqrt ( 0.999 * huge);
    	  v[tuner1] = 0.1;
    	  v[tuner2] = 1.0e-4;
    	  v[tuner3] = 0.75;
    	  v[tuner4] = 0.5;
    	  v[tuner5] = 0.75;
    	  v[xctol] = sqteps;
    	  v[xftol] = 1.0e+2 * machep;

    	  return;
    	} // private void dfault
    
    /**
     * Runs Levneberg-Marqaurdt along one dimension at a time as long as the costFunction improves during one cycle
     * of runs along every dimension.
     */
    private void runLM() {
    	int i, j;
    	boolean anotherCycle;
    	double[] lastPoint = new double[nDims];
        // Initialize data.
        functionAtBest = Double.MAX_VALUE;
        minFunctionAtBest = Double.MAX_VALUE;
        int maxIterations = 200;
        double x[] = new double[1];
        
        for(i = 0; i < points.length; i++){
        	if (points[i] == null) {
        		continue;
        	}
        	anotherCycle = true;
        	start = points[i].getPoint();
        	double[] point = extractPoint(points[i].getPoint());
        	
        	/**
             * Prepare for recording the search path.
             */
            Vector<Vector3f> path = null;
            if (pathRecorded && (nDims == 3)) {
                path = new Vector<Vector3f>();
            }

	        while (anotherCycle) {
	        	anotherCycle = false;
		        for (j = 0; j < nDims; j++) {
		        	lastPoint[j] = point[j];
		        	x[0] = point[j];
			        lmModel = new FitOAR2DLMModel(x,j,point, maxIterations);
			        lmModel.driver();
			        status = lmModel.getStatus();
			        //lmModel.statusMessage(status);
			        //lmModel.dumpResults();
			        // status = 0 success (sum of squares below underflow limit)
		    	    // status = 1 success (the relative error in the sum of squares is at most tol)
		    	    // status = 2 success (the relative error between x and the solution is at most tol)
		    	    // status = 3 success (both errors are at most tol)",
		    	    // status = 4 trapped by degeneracy (fvec is orthogonal to the columns of the jacobian)
		    	    // status = 5 timeout (number of calls to fcn has reached maxcall*(n+1))
		    	    // status = 6 failure (ftol<tol: cannot reduce sum of squares any further)
		    	    // status = 7 failure (xtol<tol: cannot improve approximate solution any further)
		    	    // status = 8 failure (gtol<tol: cannot improve approximate solution any further)
		    	    // status = 9 exception (not enough memory)
		    	    // status = 10 fatal coding error (improper input parameters)
		    	    // status = 11 exception (break requested within function evaluation)"
			        if (((status >= 0) && (status <= 3)) || (status == 5)) {
				        double params[] = lmModel.getParameters();
				        point[j] = params[0];
				        double[]fullPoint = getFinal(point);
				        functionAtBest = costFunction.cost(convertToMatrix(fullPoint));
				        if (functionAtBest < minFunctionAtBest) {
				        	minFunctionAtBest = functionAtBest;
				        	if (Math.abs(point[j] - lastPoint[j]) > OARTolerance[j]) {
				        	    anotherCycle = true;
				        	}
				        	if (pathRecorded && nDims == 3) {
		                        path.add(new Vector3f((float)point[0], (float)point[1], (float)point[2]));
		                    }
				        }
				        else {
				        	point[j] = lastPoint[j];
				        }
			        } // if (((status >= 0) && (status <= 3)) || (status == 5))
			        else {
			        	point[j] = lastPoint[j];
			        }
		        } // for (j = 0; j < nDims; j++)
	        } // while (anotherCycle)
	        /**
	         * Store the minimum cost and corresponding vector to v.
	         */
	        updatePoint(point, minFunctionAtBest, points[i]);

	        if (pathRecorded && nDims == 3) {
	            long threadId = Thread.currentThread().getId();
	            Vector<Vector<Vector3f>> pathList = paths.get(threadId);
	            if (pathList == null) {
	                pathList = new Vector<Vector<Vector3f>>();
	                paths.put(threadId, pathList);
	            }
	            pathList.add(path);
	        }
        } // for i = 0; i < points.length; i++)
        
    }  // private void runLM()
    
    public boolean isPathRecorded() {
    	return pathRecorded;
    }

    public void setPathRecorded(boolean pathRecorded) {
    	this.pathRecorded = pathRecorded;
    }
    
    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     *
     * @return  vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal(double[] point) {

        double []finalPoint = new double[start.length];
    	for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }
        
        if(nDims == 1){
            /**
             * rotation only.
             */
            finalPoint[0] = point[0];
        } else if(nDims == 2){
            /**
             * x and y translations.
             */
            finalPoint[1] = point[0];
            finalPoint[2] = point[1];
        } else if (nDims == 3 && rigid) {
            /**
             * rotation, and x, y translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 3) {
            /**
             * global scaling and x, y translations.
             */
            finalPoint[3] = finalPoint[4] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 4) {
            /**
             * Global scaling factor and x, y  translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
            finalPoint[3] = finalPoint[4] = point[3];
        } else if (nDims > 4) {
            /**
             * rotation, x, y translations, scalings and skews.
             */
            for (int j = 0; j < point.length; j++) {
                finalPoint[j] = point[j];
            }
        }
        
        return finalPoint;
    }

    /**
     * Construct a full 7-dimension transformation vector from the partial transformation vector.
     * For missing values in point, the values in defaultPoint will be used.
     * 
     * Different degree of freedom has different meanings:
     *      2: only 2 translations
     *      3: rigid:      1 rotation and 2 translations
     *         non-rigid:    global scaling and 2 translations
     *      4: 1 rotation, 2 translations and global scaling
     *      5: 1 rotation, 2 translations and scalings
     *      7: 1 rotation, 2 translations, scalings and skews
     *      
     * @param defaultPoint  a default full 7-dimension transformation vector.
     * @param point         a partial or full transformation vector.
     * @return              a full transformation vector.
     */
    public double[] constructPoint(double[] defaultPoint, double[] point) {
    	if(point == null){
    	    gov.nih.mipav.view.MipavUtil.displayError("The transformation vector either is null!");
    		return null;
    	}
    	
    	if(defaultPoint == null || defaultPoint.length != 7){
    	    gov.nih.mipav.view.MipavUtil.displayError("The default transformation vector either is null or the length is not 7!");
    		return null;
    	}
    	double[] workingPoint = new double[defaultPoint.length];
    	System.arraycopy(defaultPoint, 0, workingPoint, 0, defaultPoint.length);
    	
    	//for ( int i = 0; i < point.length; i++ )
    	//{
    	//    System.err.print( point[i] + " " );
    	//}
    	//System.err.println("");
    	
        // set up parts of transform properly
    	if (point.length == 1) {
            workingPoint[0] = point[0];
        } else if (point.length == 2) {
            workingPoint[1] = point[0];
            workingPoint[2] = point[1];
        } else if ((point.length == 3) && (rigid == true)) {
            workingPoint[0] = point[0];
            workingPoint[1] = point[1];
            workingPoint[2] = point[2];
        } else if ((point.length == 3) && (rigid == false)) {
            workingPoint[3] = workingPoint[4] = point[0];
            workingPoint[1] = point[1];
            workingPoint[2] = point[2];
        } else if (point.length == 4) {
            workingPoint[0] = point[0];
            workingPoint[1] = point[1];
            workingPoint[2] = point[2];
            workingPoint[3] = workingPoint[4] = point[3];
        } else if (point.length > 4) {
            System.arraycopy(point, 0, workingPoint, 0, point.length);
        }
        return workingPoint;
    }
    
    /**
     * Convert a 7-dimension transformation vector to a 3x3 transformation matrix.
     * 
     * @param vector	a 7-dimension transformation vector including 1 rotation, 2 translations, 2 scalings and 2 skews.
     * @return			a 3x3 transformation matrix
     */
    public TransMatrixd convertToMatrix(TransMatrixd toOrigin, TransMatrixd fromOrigin, double[] vector) {

		if (vector == null || vector.length != 7) {
			return null;
		}
		TransMatrixd matrix = new TransMatrixd(3);

		//System.err.println( vector[0] + " " + vector[1] + " " + vector[2] + " " + 0.0 + " " +
		//        vector[3] + " " + vector[4] + " " + vector[5] + " " + vector[6] );
		
		
		matrix.setTransform(vector[1], vector[2], vector[0]);
		matrix.setSkew(vector[5], vector[6]);
		matrix.setZoom(vector[3], vector[4]);

        //System.err.println( matrix.ToString() );
        matrix.MultLeft(toOrigin);
        matrix.Mult(fromOrigin);

        //System.err.println( matrix.ToString() );
        return matrix;
	}
    
    /**
     * Extract the partial or full transformation vector from the start transformation vector,
     * which will be optimized.
     * 
     * @param startPoint    the start full 7-dimension transformation vector.
     * @return              the partial or full transformation vector which will be optimized. 
     */
    public double[] extractPoint(double[] startPoint){
        if(startPoint == null || startPoint.length != 7){
            gov.nih.mipav.view.MipavUtil.displayError("The start transformation vector either is null or the length is not 7!");
            return null;
        }
        
        double[] point = new double[nDims];
        
        // set up initial point properly
        if (nDims == 1) {
            point[0] = startPoint[0]; // rotation
        }
        if (nDims == 2) {
            point[0] = startPoint[1];
            point[1] = startPoint[2];
        }

        if ((nDims == 3) && rigid) {
            point[0] = startPoint[0]; // rotation
            point[1] = startPoint[1]; // translation x
            point[2] = startPoint[2]; // translation y
        } else if (nDims == 3) {
            point[0] = startPoint[3]; // global scaling factor
            point[1] = startPoint[1]; // translation x
            point[2] = startPoint[2]; // translation y
        } else if (nDims >= 4) {
            for (int i = 0; i < nDims; i++) {
                point[i] = startPoint[i]; // 1 rotation, then 2 translations, then 2 scalings, then 2 skews
            }
        }
        return point;
    }
    
    /**
     * Returns the cost for the transformation vector.
     * @param index     the index of transformation vector.
     * @return          the cost for the transformation vector.
     */
    public final double getCost(int index){
        if(points == null){
            gov.nih.mipav.view.MipavUtil.displayError("There is no transformation vector.");
            return Double.MAX_VALUE;
        }
        
        if(index < 0 || index >= points.length){
            gov.nih.mipav.view.MipavUtil.displayError("The index is out of the boundary: " + index + "[" + 0 + "," + points.length + ")");
            return Double.MAX_VALUE;
        }
        return points[index].getCost();
    }
    
    /**
     * Accessor that sets the maximum number of iterations.
     *
     * @param  max  The max number of iterations.
     */
    public void setMaxIterations(int max) {
        maxIterations = max;
    }

    /**
     * @see AlgorithmELSUNCOptBase#adjustTranslation(TransMatrix, float)
     */
    public void adjustTranslation(TransMatrixd mat, float sample){
        double transX = mat.get(0, 2) * sample;
        double transY = mat.get(1, 2) * sample;

        mat.set(0, 2, transX);
        mat.set(1, 2, transY);
    }
    
    /**
     * @see AlgorithmELSUNCOptBase#getMatrix(int, float)
     */
    public TransMatrixd getMatrix(int index,float sample) {
        TransMatrixd mat = getMatrix(index);
        adjustTranslation(mat, sample);
        return mat;
    }
    
    /**
     * Obtain the transformation vector and convert to the matrix representation.
     * @param index     the index of transformation vector.
     * @return          the transformation matrix
     */
    public final TransMatrixd getMatrix(int index){
        double[] point = getPoint(index);
        return convertToMatrix(point);
    }
    
    /**
     * Return the full transformation vector. 
     * @param index     the index of the transformation vector.
     * @return          the full transformation vector.
     */
    public double[] getPoint(int index){
        if(points == null){
            gov.nih.mipav.view.MipavUtil.displayError("There is no transformation vector.");
            return null;
        }
        
        if(index < 0 || index >= points.length){
            gov.nih.mipav.view.MipavUtil.displayError("The index is out of the boundary: " + index + "[" + 0 + "," + points.length + ")");
            return null;
        }
        
        double[] arHolder = points[index].getPoint();
        double[] pointsCopy = new double[arHolder.length];
        for(int i=0; i<pointsCopy.length; i++) {
        	pointsCopy[i] = arHolder[i];
        }
        
        return pointsCopy;
    }
    
    /**
     * Convert a transformation vector to a transformation matrix.
     * 
     * @param vector    a transformation vector.
     * @return          a transformation matrix
     */
    public TransMatrixd convertToMatrix(double[] vector){
        return convertToMatrix(toOrigin, fromOrigin, vector);
    }

    /**
     * @see AlgorithmELSUNCOptBase#updatePoint(double[], double, Vectornd).
     */
    public void updatePoint(double[] point, double cost, Vectornd v) {
        double[] finalPoint = v.getPoint();
        if(nDims == 1){
            /**
             * rotation only.
             */
            finalPoint[0] = point[0];
        } else if(nDims == 2){
            /**
             * x and y translations.
             */
            finalPoint[1] = point[0];
            finalPoint[2] = point[1];
        } else if (nDims == 3 && rigid) {
            /**
             * rotation, and x, y translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 3) {
            /**
             * global scaling and x, y translations.
             */
            finalPoint[3] = finalPoint[4] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 4) {
            /**
             * Global scaling factor and x, y  translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
            finalPoint[3] = finalPoint[4] = point[3];
        } else if (nDims > 4) {
            /**
             * rotation, x, y translations, scalings and skews.
             */
            for (int j = 0; j < point.length; j++) {
                finalPoint[j] = point[j];
            }
        }
        v.setCost(cost);
    }
    
    public float[] createTerrain(final float transXFrom, final float transXTo,
            final float transXStep, final float transYFrom,
            final float transYTo, final float transYStep, final float rotFrom,
            final float rotTo, final float rotStep) {
        final int xdim = (int) ((transXTo - transXFrom) / transXStep);
        final int ydim = (int) ((transYTo - transYFrom) / transYStep);
        final int zdim = (int) ((rotTo - rotFrom) / rotStep);
        final float[] terrain = new float[xdim * ydim * zdim];
        final CountDownLatch doneSignal = new CountDownLatch(8);
        for (int i = 0; i < 2; i++) {
            final float zFrom = rotFrom + i * (rotTo - rotFrom) / 2;
            final float zTo = rotFrom + (i + 1) * (rotTo - rotFrom) / 2;
            final int zstart = i * zdim / 2;
            for (int j = 0; j < 2; j++) {
                final float yFrom = transYFrom + j * (transYTo - transYFrom)
                        / 2;
                final float yTo = transYFrom + (j + 1)
                        * (transYTo - transYFrom) / 2;
                final int ystart = j * ydim / 2;
                for (int k = 0; k < 2; k++) {
                    final float xFrom = transXFrom + k
                            * (transXTo - transXFrom) / 2;
                    final float xTo = transXFrom + (k + 1)
                            * (transXTo - transXFrom) / 2;
                    final int xstart = k * xdim / 2;
                    Runnable task = new Runnable() {
                        public void run() {
                            createTerrain(terrain, xFrom, xTo, transXStep,
                                    xstart, xdim, yFrom, yTo, transYStep,
                                    ystart, ydim, zFrom, zTo, rotStep, zstart,
                                    zdim);
                            doneSignal.countDown();
                        }
                    };
                    ThreadUtil.mipavThreadPool.execute(task);
                }
            }
        }

        try {
            doneSignal.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return terrain;
    }

    private void createTerrain(float[] terrain, float transXFrom,
            float transXTo, float transXStep, int xstart, int xdim,
            float transYFrom, float transYTo, float transYStep, int ystart,
            int ydim, float rotFrom, float rotTo, float rotStep, int zstart,
            int zdim) {
        int nxs = (int) ((transXTo - transXFrom) / transXStep);
        int nys = (int) ((transYTo - transYFrom) / transYStep);
        int nzs = (int) ((rotTo - rotFrom) / rotStep);
        double[] pt = new double[3];
        for (int i = 0; i < nzs; i++) {
            pt[0] = rotFrom + i * rotStep;
            for (int j = 0; j < nys; j++) {
                pt[2] = transYFrom + j * transYStep;
                for (int k = 0; k < nxs; k++) {
                    pt[1] = transXFrom + k * transXStep;
                    terrain[(i + zstart) * xdim * ydim + (j + ystart) * xdim
                            + k + xstart] = (float) (255 * (1.0 - costFunction
                            .cost(convertToMatrix(pt))));
                }
            }
        }
    }
    
    /**
     * Measure the cost value for the given transformation vector.
     * 
     * @param point     a transformation vector.
     * @return          the cost value.
     */
    public final double measureCost(double[] point){
        if(costFunction == null){
            gov.nih.mipav.view.MipavUtil.displayError("The cost function is null.");
            return Double.MAX_VALUE;
        }
        return costFunction.cost(convertToMatrix(point));
    }

    /**
     * Measure the cost value for the given transformation matrix.
     * 
     * @param m     a transformation matrix.
     * @return      the cost value.
     */
    public final double measureCost(TransMatrixd m){
        if(costFunction == null){
            gov.nih.mipav.view.MipavUtil.displayError("The cost function is null.");
            return Double.MAX_VALUE;
        }

        return costFunction.cost(m);
    }
    
    class FitOAR2DELSUNCModel extends NLConstrainedEngine {
        private int currentDim;
        private double point[];
        /**
         * Creates a new FitOAR3DConstrainedModel object.
         * 
         * @param currentDim
         * Only optimize along 1 dimension at a time
         */
        public FitOAR2DELSUNCModel(int currentDim, double point[]) {

            super(1, 1);
            this.currentDim = currentDim;
            this.point = point;

            bounds = 0; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            gues[0] = point[currentDim];
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            //internalScaling = true;
            // Suppress diagnostic messages
            //outputMes = true;
            parameterConvergence = OARTolerance[currentDim];
            maxIterations = 200;
        }

        /**
         * Display results of displaying OAR2D fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitOAR2DModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * 
         * 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            try {
                ctrl = ctrlMat[0];
                if ( (ctrl == -1) || (ctrl == 1)) {
                	point[currentDim] = a[0];
                	double[]fullPoint = getFinal(point);
                    residuals[0] = costFunction.cost(convertToMatrix(fullPoint));
                    //Preferences.debug("currentDim = " + currentDim + "\n");
                    //Preferences.debug("a[0] = " + a[0] + "\n");
                    //Preferences.debug("residuals[0] = " + residuals[0] + "\n");
                } // if ((ctrl == -1) || (ctrl == 1))
                
                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
    
    class FitOAR2DNL2solModel extends NL2sol {
    	int iv[];

        double v[];

        double x[];
        
        private int currentDim;
        private double point[];
        
        static final int numEquations = 1;
        
        static final int numParameters = 1;
        
        static final boolean useAnalyticJacobian = false;
        
        /**
         * Creates a new FitSM2nl2solModel object.
         * 
         * @param nPoints DOCUMENT ME!
         * @param yData DOCUMENT ME!
         * @param x DOCUMENT ME!
         * @param iv
         * @param v
         * @param useAnalyticJacobian
         */
        public FitOAR2DNL2solModel(final double[] x, final int iv[], final double v[], int currentDim, double point[]) {

            // nPoints data points
            // 3 coefficients
            // x[] is a length 4 initial guess at input and best estimate at output
            // data starts at x[1]
            // iv[] has length 61 + number of coefficients = 64
            // v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
            // uiparm, integer parameter array = null
            // urparm, double parameter array = null
            super(numEquations, numParameters, x, iv, v, useAnalyticJacobian, null, null);
            this.x = x;
            this.iv = iv;
            this.v = v;
            this.currentDim = currentDim;
            this.point = point;
        }
        
        /**
         * Display results of displaying OAR2D fitting parameter.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitOAR2DNL2solModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(2.0 * v[10]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("x[1] " + String.valueOf(x[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        // meqn, input, the number of functions
    	// nvar, input, the number of variables
    	// x, input of dimension nvar, the current value of the variables
    	// nf, input, the number of times the residual routine has been called so far.
    	// r, output of dimension meqn, the residual vector, that is, the value of the
    	//    functions for the given input value of the variables.
    	// uiparm, input, an integer user array
    	// urparm, input, a double user array
        public void calcr(final int meqn, final int nvar, final double x[], final int nf, final double r[],
                final int uiparm[], final double urparm[]) {
        	point[currentDim] = x[1];
        	double[]fullPoint = getFinal(point);
            r[1] = costFunction.cost(convertToMatrix(fullPoint));
        	
        }
        
        public void calcj(final int meqn, final int nvar, final double x[], final int nf, final double jac[][],
                final int uiparm[], final double urparm[]) {
        	
        }
    	
    }
    
    class FitOAR2DLMModel extends Lmmin {
        
        private int currentDim;
        private double point[];
        
        static final int numEquations = 1;
        
        static final int numParameters = 1;
        
        public FitOAR2DLMModel(double x[], int currentDim, double point[], int maxIterations) {
            super(numEquations, numParameters, x);
            this.currentDim = currentDim;
            this.point = point; 
            xtol = OARTolerance[currentDim];
            maxcall = maxIterations;
            printflags = 0;
        }
        
        public void fitToFunction(double var[], double fvec[], int info[]) {
        	point[currentDim] = var[0];
        	double[]fullPoint = getFinal(point);
            fvec[0] = costFunction.cost(convertToMatrix(fullPoint));	
        }
    }
}
