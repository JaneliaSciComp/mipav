package gov.nih.mipav.model.algorithms;

import java.util.Vector;

import Jama.Matrix;
import gov.nih.mipav.view.*;

/**
    This is a port of nesolve.m and supporting files coded in MATLAB by Richard T. Behrens in 1988
    with portions replaced to be closer to original pseudocode in appendix A of
    Numerical Methods for Unconstrained Optimization and Nonlinear Equations.
    NESolve produces the solution to a system of nonlinear equations
    For example, find the x, y, and z that solve the simultaneous equations:
    sin(x) + y^2 + log(z) - 7 = 0
    3*x + 2^y - z^3 + 1 = 0;
    x + y + z - 5 = 0
    nesolve.m coded in MATLAB by Ricaard T. Behrens, April 1988.
    Revised 11/278/88 JNL
    Hookstep option added 8/21/90 RTB.
    Hello William, 

Thank you for contacting MathWorks Technical Support. My name is Stephen and I am writing in reference to your 
Technical Support Case #02445410 regarding 'Permission to port nesolve.m'. 

Thank you for contacting us about this. It appears that "nesolve" is a File Exchange function that is not part
of the MATLAB code base. The author included parts of a MATLAB function help header, but none of its source code.
Therefore, we neither grant nor deny permission to copy the contents of that file as it does not pertain to
the MATLAB code base.

If you have any more related questions, let me know and I would be happy to help.

Sincerely, 
Stephen Jue
MathWorks Technical Support Department

 In a document with the title
                             NONLINPK
                      NESOLVE  and  UMSOLVE
                  Release 1.1, August 31, 1990
by Richard T. Behrens
He states:
The UMSOLVE files are distributed as Shareware (but NESOLVE is
public domain).

 nesolve.m is based on Algorithm D6.1.3: Part of the modular software system from the 
 appendix of the book "Numerical Methods for Unconstrained Optimization and Nonlinear Equations"
 by Dennis & Schnabel, 1983.  Refer to that book for more detailed information about the
 algorithms.   
*/

    public abstract class NESolve {
    	
    	public int NO_SCALING = 0;
    	public int SCALING_WITHOUT_SCALE = 1;
    	public int SCALING_WITH_SCALE = 2;
    	
    	public int LINE_SEARCH = 1;
    	public int TRUST_REGION = 2;
    
	    // An initial guess of the solution (starting point for iterations)
    	protected double x0[];
    	// In the Schwarz-Christoffel toolbox:
    	// JAC and SCALE inputs are never used.
    	// PATH output is never used.
    	// crparam calls nesolvei
    	// deparam, dparam, hpparam, rparam, rsparam, stparam call nesolve
    	
    	// crparam has:
    	// opt = zeros(16,1)
    	// opt(1) = trace;
    	// opt(2) = method;
    	// opt(6) = 100*(n-3); % max # of iterations
    	// opt(8) = tol;
    	// opt(9) = tol/10;
    	// opt(11) = 12;  % max step size
    	// opt(12) = nqpts;
    	
    	// deparam, dparam, hpparam, rparam, rsparam have:
    	// opt = zeros(16,1)
    	// opt(1) = trace;
    	// opt(2) = method;
    	// opt(6) = 100*(n-3); % max # of iterations
    	// opt(8) = tol;
    	// opt(9) = min(eps^(2/3),tol/10);
    	// opt(12) = nqpts;
    	
    	// stparam has:
    	// opt = zeros(16,1)
    	// opt(1) = trace;
    	// opt(2) = method;
    	// opt(6) = 100*(n-1); % max # of iterations
    	// opt(8) = tol;
    	// opt(9) = tol/10;
    	// opt(12) = nqpts;
    	
    	// Trace solution default: on, alternatives: full, off.  How much progress information to show during
    	// and after the solution to the parameter problem.
    	// Solver method: default: trustregion alternative: linesearch Different strategies in the nonlinear
    	// solver that attempt to globalize convergence.
    	// Tolerance default: 1.0e-8.  Desired accuracy in the map.  This may not be met exactly.
    	// nqpts default: 4  Number of quadrature points per integration or subinterval.  Approximately equals
    	// -log10(error).  Increase if plot has false little zigzags in curves.
    	
    	// (Optional) A vector whose elements select various algorithmic options
    	// and specify various tolerances
    	// details[0] trace = 2 => on, set btrack null
    	//                  = 1 =>, on, leave btrack alone
    	//                  = 0 => off
    	// details[1] method, trustregion or linesearch 1 = linesearch 2 = trustregion
    	//            Default of trustregion in Schwarz-Christoffel package.
    	//            Program defaults to line search if not specified.
    	// details[3] = 1 => analytic jacobian is present
    	// details[4] > 0 => Factored secant method present, not implemented in this software.
    	// details[5] maximum number of iterations, default to 100 if not specified.
    	// details[6] = delta
    	// details[7] = fvectol, defaults to eps ^ (1/3) if not specified.
    	// details[8] = steptol, defaults to eps ^ (2/3) if not specified.
    	// details[9] = mintol, defaults to eps ^ (2/3) if non specified
    	// details[10] maximum step size 
    	// details[11] is not directly used by the program.  It is only used in setting details[12] in neinck:
    	// Step 4
    	// if (dout[11] <= 0) {
    		// dout[12] = eps;
    	// }
    	// else {
    		// dout[12] = Math.max(eps, Math.pow(10.0, -dout[11]));
    	// }
    	// sqrteta = sqrt(details[1]2) in nefdjac
    	// details[13] = 1 => path is present
    	// details[14] = 1 => fparam is present
    	// details[15] = scaling
    	protected double details[] = new double[16];
    	
    	protected boolean initialJacobianIdentity = false;
    	
    	// (Optional) A set of parameters (constants) which if nonemepty
    	// is passed on to the function and Jacobian
    	protected double fparam[] = null;
    	
    	protected boolean analyticJacobian = false;
    	
    	// Typical values of X (1st column) and F (second column)
    	protected double scale[][] = null;
    	
    	// Outputs
    	// The final approximation of the solution
    	protected double xf[];
    	
    	protected double fvc[];
    	
    	protected double chiSquared;
    	
    	// Indicates the stopping reason (equals 1 for normal)
    	// termcode = 0 no termination criterion satisfied
    	// termcode > 0: some termination criteria satisfied
    	// termcode = 1: Normal termination, Norm of scaled function value is less than fvectol, 
    	//               xp is probably an approximate root of F(x) (unless fvectol is too large)
    	// termcode = 2: Scaled distance between last two steps is less than steptol;
    	//               xp may be an approximate root of F(x), but it is also possible that the
    	//               algorithm is making very slow progress and is not near a root, or that
    	//               steptol is too large.
    	// termcode = 3: last global step failed to decrease ||F(x)||2 sufficiently; either xc is
    	//               close to a root of F(x) and no more accuracy is possible, or an incorrectly
    	//               coded analytic Jacobian is being used, or the secant approximation to the
    	//               Jacobian is inaccurate, or steptol is too large.
    	// termcode = 4: if iteration count has reached maximum number of iterations.
    	// termcode = 5: Five consecutive steps of length maxstep have been taken: either ||F(x)||2 
    	//               asymptotes from above to a finite value in some direction, or maxstep is
    	//               too small.
    	// termcode = 6: xc seems to be an approximate local minimizer of ||F(x)||2 that is not a root
    	//               of F(x) (or mintol is too large): to find a root of F(x), the algorithm must be
    	//               restarted from a different region.
    	protected int termcode[] = new int[1];
    	
    	// (Optional) Returns the sequence of iterates
    	protected Vector<Double> path = null;
    	
    	protected double btrack[] = null;
    	
    	// Number of function evaluations
    	protected int nofun[] = new int[1];
    	
    	// Number of iterations
    	protected int itncount;
    	
    	// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
    	private double eps;
    	
    	private boolean testMode = false; 
    	
    	private int testCase;
    	
    	private final int ROSENBROCK = 1;
    	
    	private final int FREUDENSTEIN_AND_ROTH = 2;
    	
    	private final int HELICAL_VALLEY = 7;
    	
    	private final int POWELL_SINGULAR = 13;
    	
    	private final int BROWN_ALMOST_LINEAR = 27;
    	
    	private final int CHEBYQUAD = 35;
    	
    	private final int LEVMAR_ROSENBROCK = 50;
    	
    	private final int POWELL_2_PARAMETER = 52;
    	
    	private final int HOCK1 = 61;
    	
    	private final int TRIGONOMETRIC = 66;
    	
    	private double tol;
    	
    	private int param;
    	
    	private int nPts;
    	
    	// To run the self tests put the following code in another file:
    	// boolean netest = true;
        // if (netest) {
        	// FitAll fa = new FitAll();
        	// return;	
        // }
    	
    	// class FitAll extends NESolve {

 	       
            // public FitAll() {

                //  super();

                
            // }

            
            // public void fitToFunction(double fvplus[], double xplus[], double fparam[]) {
            	
            // }

           
            // public void fitToJacobian(double jc[][], int addfun[], double x0[], double fparam[]) {
            	
            // }
        // }
    	
    	public NESolve() {
    		int i;
    		int j;
    		// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
	    	// epsilon = D1MACH(4)
	        // Machine epsilon is the smallest positive epsilon such that
	        // (1.0 + epsilon) != 1.0.
	        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
	        // epsilon = 2.2204460e-16
	        // epsilon is called the largest relative spacing
	        eps = 1.0;
	        double neweps = 1.0;


	        while (true) {

	            if (1.0 == (1.0 + neweps)) {
	                break;
	            } else {
	                eps = neweps;
	                neweps = neweps / 2.0;
	            }
	        } // while(true)
	        
	        // For the self tests:
	        // NESolve passes ROSENBROCK at standard starting point, 10 * standard starting point, and 100 * standard starting point.
	        // In FREUNDENSTEIN_AND_ROTH at standard starting point and 10 * standard starting point
	        // trust region has correct second solution answers with chiSquared = 49.9842 even though termcode[0] = 3, 
	        // but line search gives incorrect answers with termcode[0] = 4.
	        // In FREUNDENSTEIN_AND_ROTH at 100 * standard starting point
	        // trust region has correct second solution answers with chiSquared = 49.9842 even though termcode[0] = 3, 
	        // but line search gives incorrect answers with termcode[0] = 5.
	        // NESolve passes HELICAL_VALLEY at standard starting point, 10 * standard starting point, and 100 * standard starting point.
	        // NESolve passes POWELL_SINGULAR at standard starting point, 10 * standard starting point, and 100 * standard starting point.
	        // NESolve passes BROWN_ALMOST_LINEAR 10 parameters at standard starting point and 10 * standard starting point.
	        //               At 100 * standard starting point NESolve passes for trust region,
	        //               but fails for line search with termcode[0] = 3..
	        // NESolve passes BROWN_ALMOST_LINEAR 30 parameters at standard starting point.
	        // NESolve passes BROWN_ALMOST_LINEAR 40 parameters at standard starting point
	        // NESolve passes CHEBYQUAD with 8 parameters and 8 points for trust region with termcode[0] = 2 and termcode[0] = 3,
	        //               but fails for line search with termcode[0] = 4.
	        // NESolve passes CHEBYQUAD with 9 parameters and 9 points for trust region with termcode[0] = 1,
	        //               but fails for line search with termcode[0] = 3.
	        // NESolve passes CHEBYQUAD with 10 parameters and 10 points for trust region with termcode[0] = 2 and termcode[0] = 3,
	        //               but fails for line search with termcode[0] = 3.
	        // NESolve passes LEVMAR_ROSENBROCK for trust region with termcode[0] = 1 with 8396 and 8573 iterations,
	        //               but fails for line search with termcode[0] = 3.
	        // NESolve passes POWELL_2_PARAMETER with termcode[0] = 1.
	        // NESolve passes HOCK1 with termcode[0] = 1.
	        // NESolve passes TRIGONOMETRIC at starting point, 10 * starting point, and 100 * start9ng point
	        // for param = 1 to 10 with termcode[0] = 1.
	        
	        details[0] = 0; // trace
	        tol = 1.0E-8;
	        details[7] = tol; // fvectol
	        details[8] = Math.min(Math.pow(eps,(2.0/3.0)),tol/10); // steptol
	        details[11] = 4;
	        testMode = true;
    		// Below is an example to fit y(0) = 10.0*(a1 - a0**2)
            //                            y(1) = 1.0 - a[0]
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Rosenbrock function standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = 10*(a1 - a0**2)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = 1.0 - a0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
            testCase = ROSENBROCK;
            x0 = new double[2];
            x0[0] = -1.2;
            x0[1] = 1.0;
            driverCalls();
            
            // Below is an example to fit y(0) = 10.0*(a1 - a0**2)
            //                            y(1) = 1.0 - a[0]
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Rosenbrock function at 10 * standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = 10*(a1 - a0**2)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = 1.0 - a0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
            testCase = ROSENBROCK;
            x0 = new double[2];
            x0[0] = -12.0;
            x0[1] = 10.0;
            driverCalls();
            
            // Below is an example to fit y(0) = 10.0*(a1 - a0**2)
            //                            y(1) = 1.0 - a[0]
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Rosenbrock function at 100 * standard starting point unconstrained\n",
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = 10*(a1 - a0**2)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = 1.0 - a0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
            testCase = ROSENBROCK;
            x0 = new double[2];
            x0[0] = -120.0;
            x0[1] = 100.0;
            driverCalls();
            
            // Below is an example to fit y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1
            //                            y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            
            
            // analyticJacobian = false
            // Method = line search
            // termcode[0] = 4 indicating maximum iteration count reached.
            // iteration count = 2000
            // Final output values:
            // xf[0] = 13.523136663183204
            // xf[1] = -0.8968137650695506
            // chiSquared = 57.89243419329399
            // analyticJacobian = true
            // Method = line search
            // termcode[0] = 4 indicating maximum iteration count reached.
            // iteration count = 2000
            // Final output values:
            // xf[0] = 13.523136663183204
            // xf[1] = -0.8968137650695506
            // chiSquared = 57.89243419329399
            // analyticJacobian = false
            // Method = trust region
            // termcode[0] = 3 indicating last global step failed to decrease ||F(x)||2 sufficiently;
            // either xc is close to a root of F(x) and no more accuracy is possible, or an incorrectly
            // coded analytic Jacobian is being used, or the secant approximation to the Jacobian is
            // inaccuate, or steptol is too large.
            // iteration count = 37
            // Final output values:
            // xf[0] = 11.412778913314154
            // xf[1] = -0.8968052594525763
            // chiSquared = 48.98425367924003
            // analyticJacobian = true
            // Method = trust region
            // termcode[0] = 3 indicating last global step failed to decrease ||F(x)||2 sufficiently;
            // either xc is close to a root of F(x) and no more accuracy is possible, or an incorrectly
            // coded analytic Jacobian is being used, or the secant approximation to the Jacobian is
            // inaccuate, or steptol is too large.
            // iteration count = 31
            // Final output values:
            // xf[0] = 11.412778985045028
            // xf[1] = -0.8968052544000359
            // chiSquared = 48.984253679240034
            
            Preferences.debug("Freudenstein and Roth function at standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 5, a1 = 4\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Also Chi-squared = 48.9842... at a0 = 11.41..., a1 = -0.8968...\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Second solution obtained by original ELSUNC\n", Preferences.DEBUG_ALGORITHM);
            testCase = FREUDENSTEIN_AND_ROTH;
            x0 = new double[2];
            x0[0] = 0.5;
            x0[1] = -2.0;
            driverCalls();
            
            // Below is an example to fit y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1
            //                            y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Freudenstein and Roth function at 10 * standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 5, a1 = 4\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Also Chi-squared = 48.9842... at a0 = 11.41..., a1 = -0.8968...\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Second solution obtained by original ELSUNC\n", Preferences.DEBUG_ALGORITHM);
            testCase = FREUDENSTEIN_AND_ROTH;
            x0 = new double[2];
            x0[0] = 5.0;
            x0[1] = -20.0;
            driverCalls();
            
         // Below is an example to fit y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1
            //                            y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Freudenstein and Roth function at 100 * standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = -13 + a0 + ((5 - a1)*a1 - 2)*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = -29 + a0 + ((a1 + 1)*a1 - 14)*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 5, a1 = 4\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Also Chi-squared = 48.9842... at a0 = 11.41..., a1 = -0.8968...\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Second solution obtained by original ELSUNC\n", Preferences.DEBUG_ALGORITHM);
            testCase = FREUDENSTEIN_AND_ROTH;
            x0 = new double[2];
            x0[0] = 50.0;
            x0[1] = -200.0;
            driverCalls();
            
            // Below is an example to fit y(0) = 10*[a2 - 10*theta(a0,a1)]
            //                            y(1) = 10*[sqrt(a0**2 + a1**2) - 1]
            //                            y(2) = a2
            // where theta(a0,a1) = (1/(2*PI))*arctan(a1/a0) if a0 > 0
            //                    = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0
            //                    = 0.25 if a0 = 0 and a1 >= 0
            //                    = -0.25 if a0 = 0 and a1 < 0
            Preferences.debug("Helical valley function at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = 10*[a2 - 10*theta(a0,a1)]\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = 10*{Math.sqrt(a0*a0 + a1*a1) - 1]\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(2) = a2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) if a0 > 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) if a0 + 0.5 < 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = 0.25 if a0 = 0 and a1 >= 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = -0.25 if a0 = 0 and a1 < 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 0, a2 = 0\n", Preferences.DEBUG_ALGORITHM);
            testCase = HELICAL_VALLEY;
            x0 = new double[3];
            x0[0] = -1.0;
            x0[1] = 0.0;
            x0[2] = 0.0;
            driverCalls();
            
            // Below is an example to fit y(0) = 10*[a2 - 10*theta(a0,a1)]
            //                            y(1) = 10*[sqrt(a0**2 + a1**2) - 1]
            //                            y(2) = a2
            // where theta(a0,a1) = (1/(2*PI))*arctan(a1/a0) if a0 > 0
            //                    = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0
            //                    = 0.25 if a0 = 0 and a1 >= 0
            //                    = -0.25 if a0 = 0 and a1 < 0
            Preferences.debug("Helical valley function at 10 * standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = 10*[a2 - 10*theta(a0,a1)]\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = 10*{Math.sqrt(a0*a0 + a1*a1) - 1]\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(2) = a2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) if a0 > 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = 0.25 if a0 = 0 and a1 >= 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = -0.25 if a0 = 0 and a1 < 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 0, a2 = 0\n", Preferences.DEBUG_ALGORITHM);
            testCase = HELICAL_VALLEY;
            x0 = new double[3];
            x0[0] = -10.0;
            x0[1] = 0.0;
            x0[2] = 0.0;
            driverCalls();
            
            // Below is an example to fit y(0) = 10*[a2 - 10*theta(a0,a1)]
            //                            y(1) = 10*[sqrt(a0**2 + a1**2) - 1]
            //                            y(2) = a2
            // where theta(a0,a1) = (1/(2*PI))*arctan(a1/a0) if a0 > 0
            //                    = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0
            //                    = 0.25 if a0 = 0 and a1 >= 0
            //                    = -0.25 if a0 = 0 and a1 < 0
            Preferences.debug("Helical valley function at 100 * standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = 10*[a2 - 10*theta(a0,a1)]\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = 10*{Math.sqrt(a0*a0 + a1*a1) - 1]\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(2) = a2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) if a0 > 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = (1/(2*PI))*arctan(a1/a0) + 0.5 if a0 < 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = 0.25 if a0 = 0 and a1 >= 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("theta = -0.25 if a0 = 0 and a1 < 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 0, a2 = 0\n", Preferences.DEBUG_ALGORITHM);
            testCase = HELICAL_VALLEY;
            x0 = new double[3];
            x0[0] = -100.0;
            x0[1] = 0.0;
            x0[2] = 0.0;
            driverCalls();
            
            // Below is an example to fit y(0) = a0 + 10*a1
            //                            y(1) = sqrt(5)*(a2 - a3)
            //                            y(2) = (a1 - 2*a2)**2
            //                            y(3) = sqrt(10)*(a0 - a3)**2
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            
            Preferences.debug("Powell singular function at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = a0 + 10*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = sqrt(5)*(a2 - a3)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(2) = (a1 - 2*a2)**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(3) = sqrt(10)*(a0 - a3)**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer has chi-squared = 0 at a0 = 0, a1= 0, a2 = 0, a3 = 0\n", 
            		Preferences.DEBUG_ALGORITHM);
            testCase = POWELL_SINGULAR;
            x0 = new double[4];
            x0[0] = 3.0;
            x0[1] = -1.0;
            x0[2] = 0.0;
            x0[3] = 1.0;
            driverCalls();
            
            // Below is an example to fit y(0) = a0 + 10*a1
            //                            y(1) = sqrt(5)*(a2 - a3)
            //                            y(2) = (a1 - 2*a2)**2
            //                            y(3) = sqrt(10)*(a0 - a3)**2
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Powell singular function at 10 * standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = a0 + 10*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = sqrt(5)*(a2 - a3)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(2) = (a1 - 2*a2)**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(3) = sqrt(10)*(a0 - a3)**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer has chi-squared = 0 at a0 = 0, a1= 0, a2 = 0, a3 = 0\n", 
            		Preferences.DEBUG_ALGORITHM);
            testCase = POWELL_SINGULAR;
            x0 = new double[4];
            x0[0] = 30.0;
            x0[1] = -10.0;
            x0[2] = 0.0;
            x0[3] = 10.0;
            driverCalls();
            
            // Below is an example to fit y(0) = a0 + 10*a1
            //                            y(1) = sqrt(5)*(a2 - a3)
            //                            y(2) = (a1 - 2*a2)**2
            //                            y(3) = sqrt(10)*(a0 - a3)**2
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Powell singular function at 100 * standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = a0 + 10*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = sqrt(5)*(a2 - a3)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(2) = (a1 - 2*a2)**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(3) = sqrt(10)*(a0 - a3)**2\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer has chi-squared = 0 at a0 = 0, a1= 0, a2 = 0, a3 = 0\n", 
            		Preferences.DEBUG_ALGORITHM);
            testCase = POWELL_SINGULAR;
            x0 = new double[4];
            x0[0] = 300.0;
            x0[1] = -100.0;
            x0[2] = 0.0;
            x0[3] = 100.0;
            driverCalls();
            
            // Below is an example to fit the Brown almost linear function with 10 parameters
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Brown almost linear with 10 parameters at standard staring point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
            testCase = BROWN_ALMOST_LINEAR;
            param = 10;
            // Guess all parameters are 0.5
            x0 = new double[param];
            for (i = 0; i < param; i++) {
            	x0[i] = 0.5;
            }
            driverCalls();
            
            // Below is an example to fit the Brown almost linear function with 10 parameters
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Brown almost linear with 10 parameters at 10 * standard staring point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
            testCase = BROWN_ALMOST_LINEAR;
            param = 10;
            // Guess all parameters are 5.0
            x0 = new double[param];
            for (i = 0; i < param; i++) {
            	x0[i] = 5.0;
            }
            driverCalls();
            
            // Below is an example to fit the Brown almost linear function with 10 parameters
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Brown almost linear with 10 parameters at 100 * standard staring point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
            testCase = BROWN_ALMOST_LINEAR;
            param = 10;
            // Guess all parameters are 50.0
            x0 = new double[param];
            for (i = 0; i < param; i++) {
            	x0[i] = 50.0;
            }
            driverCalls();
            
            // Below is an example to fit the Brown almost linear function with 30 parameters
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Brown almost linear with 30 parameters at standard staring point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
            testCase = BROWN_ALMOST_LINEAR;
            param = 30;
            // Guess all parameters are 0.5
            x0 = new double[param];
            for (i = 0; i < param; i++) {
            	x0[i] = 0.5;
            }
            driverCalls();
            
            // Below is an example to fit the Brown almost linear function with 40 parameters
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Brown almost linear with 40 parameters at standard staring point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 0 at (alpha, ..., alpha, alpha**(1 - n)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("where alpha satisfies n*alpha**n - (n+1)*alpha**(n-1) + 1 = 0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("in particular , alpha = 1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared = 1 at (0,...,0,n+1)\n", Preferences.DEBUG_ALGORITHM);
            testCase = BROWN_ALMOST_LINEAR;
            param = 40;
            // Guess all parameters are 0.5
            x0 = new double[param];
            for (i = 0; i < param; i++) {
            	x0[i] = 0.5;
            }
            driverCalls();
            
            // Below is an example to fit the Chebyquad function with 8 parameters and 8 points
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            // Correct answer from NLConstrainedEngined:
            // a0 = 0.043152745306997475
            // a1 = 0.19309080895383032
            // a2 = 0.2663286909535495
            // a3 = 0.5000000012476513
            // a4 = 0.4999999274501084
            // a5 = 0.7336712516240429
            // a6 = 0.8069090604951162
            // a7 = 0.9568471106755537
            Preferences.debug("Chebyquad function with 8 parameters and 8 points\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct chi-squared = 3.51687E-3\n", Preferences.DEBUG_ALGORITHM);
            testCase = CHEBYQUAD;
            nPts = 8;
            param = 8;
            x0 = new double[param];
            for (i = 1; i <= param; i++) {
               x0[i-1] = i/(param + 1.0);
            }
            driverCalls();
            
            // Below is an example to fit the Chebyquad function with 9 parameters and 9 points
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Chebyquad function with 9 parameters and 9 points\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct chi-squared = 0.0\n", Preferences.DEBUG_ALGORITHM);
            testCase = CHEBYQUAD;
            nPts = 9;
            param = 9;
            x0 = new double[param];
            for (i = 1; i <= param; i++) {
                x0[i-1] = i/(param + 1.0);
            }
            driverCalls();
            
            // Below is an example to fit the Chebyquad function with 10 parameters and 10 points
            // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
            Preferences.debug("Chebyquad function with 10 parameters and 10 points\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct chi-squared = 6.50395E-3\n", Preferences.DEBUG_ALGORITHM);
            testCase = CHEBYQUAD;
            nPts = 10;
            param = 10;
            x0 = new double[param];
            for (i = 1; i <= param; i++) {
                x0[i-1] = i/(param + 1.0);
            }
            driverCalls();
            
            Preferences.debug("Rosenbrock function used as LEVMAR example standard starting point unconstrained\n", 
            		Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = ((1.0 - a0)*(1.0 - a0) + 105.0*(a1 - a0*a0)*(a1 - a0*a0));\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(1) = y(0)\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer is chi-squared = 0 at a0 = 1, a1 = 1\n", Preferences.DEBUG_ALGORITHM);
            testCase = LEVMAR_ROSENBROCK;
            x0 = new double[2];
            x0[0] = -1.2;
            x0[1] = 1.0;
            driverCalls();
            
            Preferences.debug("Powell's 2 parameter function\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = a0\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("y(0) = 10.0*a0/(a0 + 0.1) + 2*a1*a1\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Correct answer a0 = 0 a1 = 0\n", Preferences.DEBUG_ALGORITHM);
            testCase = POWELL_2_PARAMETER;
            x0 = new double[2];
            x0[0] = 3.0;
            x0[1] = 1.0;
            driverCalls();
            
            Preferences.debug("Hock - Schittkowski problem #1\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("Correct answer has a0 = a1 = 1\n", Preferences.DEBUG_ALGORITHM);
        	testCase = HOCK1;
        	x0 = new double[2];
        	x0[0] = -2.0;
        	x0[1] = 1.0;
        	driverCalls();
        	
        	Preferences.debug("Trigonometric function at standard starting point\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("for i = 1,...n: fi(x) = n - sum from j = 1 to n of:\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("cos(xj) + i*(1-cos(xi)) - sin(xi)\n", Preferences.DEBUG_ALGORITHM);
        	testCase = TRIGONOMETRIC;
        	for (param = 1; param <= 10; param++) {
        		x0 = new double[param];
        		for (j = 0; j < param; j++) {
        			x0[j] = 1.0/param;
        		}
        		driverCalls();
        	} // for (param = 1; param <= 10; param++)
        	
        	Preferences.debug("Trigonometric function at 10 * standard starting point\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("for i = 1,...n: fi(x) = n - sum from j = 1 to n of:\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("cos(xj) + i*(1-cos(xi)) - sin(xi)\n", Preferences.DEBUG_ALGORITHM);
        	testCase = TRIGONOMETRIC;
        	for (param = 1; param <= 10; param++) {
        		x0 = new double[param];
        		for (j = 0; j < param; j++) {
        			x0[j] = 10.0/param;
        		}
        		driverCalls();
        	} // for (param = 1; param <= 10; param++)
        	
        	Preferences.debug("Trigonometric function at 100 * standard starting point\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("for i = 1,...n: fi(x) = n - sum from j = 1 to n of:\n", Preferences.DEBUG_ALGORITHM);
        	Preferences.debug("cos(xj) + i*(1-cos(xi)) - sin(xi)\n", Preferences.DEBUG_ALGORITHM);
        	testCase = TRIGONOMETRIC;
        	for (param = 1; param <= 10; param++) {
        		x0 = new double[param];
        		for (j = 0; j < param; j++) {
        			x0[j] = 100.0/param;
        		}
        		driverCalls();
        	} // for (param = 1; param <= 10; param++)
    	}
    	
    	private void fitTestModel() {
    		details[5] = 10000 * x0.length; // maxIterations	
    	}
    	
    	private void driverCalls() {
    		analyticJacobian = false;
    		details[1] = 1; // linesearch
    		fitTestModel();
            driver();
            dumpTestResults();
    		analyticJacobian = true;
    		details[1] = 1; // linesearch
    		fitTestModel();
            driver();
            dumpTestResults();
    		analyticJacobian = false;
    		details[1] = 2; // trustregion
    		fitTestModel();
            driver();
            dumpTestResults();
    		analyticJacobian = true;
    		details[1] = 2; // trustregion;
    		fitTestModel();
            driver();
            dumpTestResults();
    	}
    	
    	public void dumpTestResults() {
    		int i;
            Preferences.debug("analyticJacobian = " + analyticJacobian + "\n", Preferences.DEBUG_ALGORITHM);
            switch((int)details[1]) {
            case 1:
            	Preferences.debug("Method = line search\n", Preferences.DEBUG_ALGORITHM);
            	break;
            case 2:
            	Preferences.debug("Method = trust region\n", Preferences.DEBUG_ALGORITHM);
            	break;
            default:
            	Preferences.debug("Illegal method = " + details[1] + "\n", Preferences.DEBUG_ALGORITHM);
            }
    		switch(termcode[0]) {
    		case -1:
    			Preferences.debug("termcode[0] = -1 indicating x0.length < 1\n", Preferences.DEBUG_ALGORITHM);
    			break;
    		case 0:
    			Preferences.debug("termcode[0] = 0 indicating no termination criterion satisfied\n", Preferences.DEBUG_ALGORITHM);
    			break;
    		case 1:
    			Preferences.debug("termcode[0] = 1 indicating normal termination.\n", Preferences.DEBUG_ALGORITHM);
    			break;
    		case 2:
    			Preferences.debug("termcode[0] = 2 indicating scaled distance between last two steps less than steptol.\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("xp may be an approximate root of F(x), but it is also possible that the algorithm\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("is making very slow progress and is not near a root, or that steptol is too large.\n",
    					Preferences.DEBUG_ALGORITHM);
    	        break;
    		case 3:
    			Preferences.debug("termcode[0] = 3 indicating last global step failed to decrease ||F(x)||2 sufficiently;\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("either xc is close to a root of F(x) and no more accuracy is possible, or an incorrectly\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("coded analytic Jacobian is being used, or the secant approximation to the Jacobian is\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("inaccuate, or steptol is too large.\n", Preferences.DEBUG_ALGORITHM);
    			break;
    		case 4:
    			Preferences.debug("termcode[0] = 4 indicating maximum iteration count reached.\n", Preferences.DEBUG_ALGORITHM);
    			break;
    		case 5:
    			Preferences.debug("termcode[0] = 5 indicating five consecutive steps of length maxstep have been taken:\n",
    			Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("either ||F(x)||2 asymptotes from above to a finite value in some direction, or\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("maxstep is too small.\n", Preferences.DEBUG_ALGORITHM);
    		    break;
    		case 6:
    			Preferences.debug("termcode[0] = 6 indicating xc seems to be an approximate local minimizer of ||F(x)||2\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("that is not a root of F(x) (or mintol is too large); to find a root of F(x),\n",
    					Preferences.DEBUG_ALGORITHM);
    			Preferences.debug("the algorithm must be restarted from a different region.\n", Preferences.DEBUG_ALGORITHM);
    			break;
    		default:
    			Preferences.debug("termcode[0] = " + termcode[0] + " indicating illegal termination\n", Preferences.DEBUG_ALGORITHM);
    		}
    		Preferences.debug("iteration count = " + itncount + "\n", Preferences.DEBUG_ALGORITHM);
    		Preferences.debug("Final output values:\n", Preferences.DEBUG_ALGORITHM);
    		for (i = 0; i < x0.length; i++) {
    			Preferences.debug("xf["+i+"] = " + xf[i] + "\n",Preferences.DEBUG_ALGORITHM);
    		}
    		chiSquared = 0.0;
    		for (i = 0; i < x0.length; i++) {
    			chiSquared += (fvc[i]*fvc[i]);
    		}
    		Preferences.debug("chiSquared = " + chiSquared + "\n", Preferences.DEBUG_ALGORITHM);
    	}
    	
    	public double[] getParameters() {
    		return xf;
    	}
    	
    	public int getExitStatus() {
    		return termcode[0];
    	}
    	
    	public void printExitStatus(int exitStatus) {
    		switch(exitStatus) {
    		case -1:
    			System.err.println("termcode[0] = -1 indicating x0.length < 1");
    			break;
    		case 0:
    			System.err.println("termcode[0] = 0 indicating no termination criterion satisfied");
    			break;
    		case 1:
    			System.err.println("termcode[0] = 1 indicating normal termination.");
    			break;
    		case 2:
    			System.err.println("termcode[0] = 2 indicating scaled distance between last two steps less than steptol.");
    			System.err.println("xp may be an approximate root of F(x), but it is also possible that the algorithm");
    			System.err.println("is making very slow progress and is not near a root, or that steptol is too large.");
    	        break;
    		case 3:
    			System.err.println("termcode[0] = 3 indicating last global step failed to decrease ||F(x)||2 sufficiently;");
    			System.err.println("either xc is close to a root of F(x) and no more accuracy is possible, or an incorrectly");
    			System.err.println("coded analytic Jacobian is being used, or the secant approximation to the Jacobian is");
    			System.err.println("inaccuate, or steptol is too large.");
    			break;
    		case 4:
    			System.err.println("termcode[0] = 4 indicating maximum iteration count reached.");
    			break;
    		case 5:
    			System.err.println("termcode[0] = 5 indicating five consecutive steps of length maxstep have been taken:");
    			System.err.println("either ||F(x)||2 asymptotes from above to a finite value in some direction, or");
    			System.err.println("maxstep is too small.");
    		    break;
    		case 6:
    			System.err.println("termcode[0] = 6 indicating xc seems to be an approximate local minimizer of ||F(x)||2");
    			System.err.println("that is not a root of F(x) (or mintol is too large); to find a root of F(x),");
    			System.err.println("the algorithm must be restarted from a different region.");
    			break;
    		default:
    			System.err.println("termcode[0] = " + termcode[0] + " indicating illegal termination");
    		}	
    	}
    	
	    public NESolve(boolean initialJacobianIdentity, double x0[], double fparam[], 
	    		boolean analyticJacobian, double scale[][], Vector<Double> path,
	    		double btrack[], int trace, int method, int maxIterations, double fvectol, 
	    		double steptol, double maxStepSize, double details11, int scaling) {
	    	this.initialJacobianIdentity = initialJacobianIdentity;
	    	this.x0 = x0;
	 	    this.fparam = fparam;
	 	    this.analyticJacobian = analyticJacobian;
	 	    this.scale = scale;
	 	    this.path = path;
	 	    this.btrack = btrack;
	 	    details[0] = trace;
	 	    details[1] = method;
	 	    details[5] = maxIterations;
	 	    details[7] = fvectol;
	 	    details[8] = steptol;
	 	    details[10] = maxStepSize;
	 	    details[11] = details11;
	 	    details[15] = scaling;
	    }
	    
	    // fvplus is an output
	    // xplus and fparam are inputs
	    public abstract void fitToFunction(double fvplus[], double xplus[], double fparam[]);
	    
	    public void fitToTestFunction(double fvplus[], double xplus[], double fparam[]) {
	    	int i;
	    	int j;
	        switch(testCase) {
	        case ROSENBROCK:
	        	fvplus[0] = 10.0*(xplus[1] - xplus[0]*xplus[0]);
         	    fvplus[1] = 1.0 - xplus[0];
	        	break;
	        case FREUDENSTEIN_AND_ROTH:
	        	fvplus[0] = -13.0 + xplus[0] + ((5.0 - xplus[1])*xplus[1] - 2.0)*xplus[1];
        		fvplus[1] = -29.0 + xplus[0] + ((xplus[1] + 1)*xplus[1] - 14.0)*xplus[1];
        		break;
	        case HELICAL_VALLEY:
	        	double theta;
	        	if (xplus[0] > 0) {
        	        theta = Math.atan(xplus[1]/xplus[0])/(2.0*Math.PI);
        		}
        		else if (xplus[0] < 0) {
        	    	theta = Math.atan(xplus[1]/xplus[0])/(2.0*Math.PI) + 0.5;
        	    }
        	    else if (xplus[1] >= 0) {
        	    	theta = 0.25;
        	    }
        	    else {
        	    	theta = -0.25;
        	    }
        	    fvplus[0] = 10.0*(xplus[2] - 10.0*theta);
        	    fvplus[1] = 10.0*(Math.sqrt(xplus[0]*xplus[0] + xplus[1]*xplus[1]) - 1.0);
        	    fvplus[2] = xplus[2];
	        	break;
	        case POWELL_SINGULAR:
	        	fvplus[0] = xplus[0] + 10.0*xplus[1];
        	    fvplus[1] = Math.sqrt(5.0)*(xplus[2] - xplus[3]);
        	    fvplus[2] = (xplus[1] - 2.0*xplus[2])*(xplus[1] - 2.0*xplus[2]);
        	    fvplus[3] = Math.sqrt(10.0)*(xplus[0] - xplus[3])*(xplus[0] - xplus[3]);
	        	break;
	        case BROWN_ALMOST_LINEAR:
	        	double sumParam = 0.0;
    			double prodParam = 1.0;
    			for (i = 0; i < param; i++) {
    				sumParam += xplus[i];
    				prodParam *= xplus[i];
    			}
    		    for (i = 0; i < param -1; i++) {
    		    	fvplus[i] = xplus[i] + sumParam - (param + 1.0);
    		    } // for (i = 0; i < nPts - 1; i++)
    		    fvplus[param-1] = prodParam - 1.0;	
	        	break;
	        case CHEBYQUAD:
	        	double chebySum;
        	    for (i = 1; i <= nPts; i++) {
        	        chebySum = 0.0;
        	        for (j = 0; j < param; j++) {
        	        	chebySum += shiftedChebyshev(xplus[j],i);
        	        }
        	        if ((i % 2) == 1) {
        	        	fvplus[i-1] = chebySum/param;
        	        }
        	        else {
        	        	fvplus[i-1] = chebySum/param + 1.0/(i*i - 1.0);
        	        }
        	    }
	        	break;
	        case LEVMAR_ROSENBROCK:
	        	fvplus[0] = ((1.0 - xplus[0])*(1.0 - xplus[0]) 
		    			+ 105.0*(xplus[1]- xplus[0]*xplus[0])*(xplus[1] - xplus[0]*xplus[0]));
        		fvplus[1] = fvplus[0];
	        	break;
	        case POWELL_2_PARAMETER:
	        	fvplus[0] = xplus[0];
		    	fvplus[1] = 10.0*xplus[0]/(xplus[0] + 0.1) + 2.0*xplus[1]*xplus[1];	
	        	break;
	        case HOCK1:
	        	fvplus[0] = 10.0*(xplus[1] - xplus[0]*xplus[0]);
        	    fvplus[1] = 1.0 - xplus[0];
	        	break;
	        case TRIGONOMETRIC:
	        	for (i = 1; i <= param; i++) {
	        		double sum = 0.0;
	        		for (j = 1; j <= param; j++) {
	        		    sum += (Math.cos(xplus[j-1]) + i*(1.0 - Math.cos(xplus[i-1])) - Math.sin(xplus[i-1]));	
	        		}
	        		fvplus[i-1] = param - sum;
	        	}
	        	break;
	        }
	    }
	    
	    // jc and addfun are outputs addfun is the number of additional function evaluations
	    // x0 and fparam are inputs
	    public abstract void fitToJacobian(double jc[][], int addfun[], double x0[], double fparam[]);
	    
	    public void fitToTestJacobian(double jc[][], int addfun[], double x0[], double fparam[]) {
	    	int i;
	        switch(testCase) {
	        case ROSENBROCK:
	        	jc[0][0] = -20.0*x0[0];
    		    jc[0][1] = 10.0;
    		    jc[1][0] = -1.0;
    		    jc[1][1] = 0.0;
    		    addfun[0] = 1;
	        	break;
	        case FREUDENSTEIN_AND_ROTH:
	        	jc[0][0] = 1.0;
    		    jc[0][1] = 10.0*x0[1] - 3.0*x0[1]*x0[1] - 2.0;
    		    jc[1][0] = 1.0;
    		    jc[1][1] = 3.0*x0[1]*x0[1] + 2.0*x0[1] - 14.0;
    		    addfun[0] = 2;
	        	break;
	        case HELICAL_VALLEY:
	        	double tmp;
    			tmp = x0[0]*x0[0] + x0[1]*x0[1];
    		    jc[0][0] = (50.0*x0[1])/(Math.PI * tmp);
    			jc[0][1] = (-50.0*x0[0])/(Math.PI * tmp);
    			jc[0][2] = 10.0;
    			jc[1][0]= 10.0*x0[0]/Math.sqrt(tmp);
    			jc[1][1] = 10.0*x0[1]/Math.sqrt(tmp);
    			jc[1][2] = 0.0;
    			jc[2][0] = 0.0;
    			jc[2][1] = 0.0;
    			jc[2][2] = 1.0;
    			addfun[0] = 5;
	        	break;
	        case POWELL_SINGULAR:
	        	jc[0][0] = 1.0;
    		    jc[0][1] = 10.0;
    		    jc[0][2] = 0.0;
    		    jc[0][3] = 0.0;
    		    jc[1][0] = 0.0;
    		    jc[1][1] = 0.0;
    		    jc[1][2] = Math.sqrt(5.0);
    		    jc[1][3] = -Math.sqrt(5.0);
    		    jc[2][0] = 0.0;
    		    jc[2][1] = 2.0*x0[1] - 4.0*x0[2];
    		    jc[2][2] = 8.0*x0[2] - 4.0*x0[1];
    		    jc[2][3] = 0.0;
    		    jc[3][0] = 2.0*Math.sqrt(10.0)*x0[0] - 2.0*Math.sqrt(10.0)*x0[3];
    		    jc[3][1] = 0.0;
    		    jc[3][2] = 0.0;
    		    jc[3][3] = 2.0*Math.sqrt(10.0)*x0[3] - 2.0*Math.sqrt(10.0)*x0[0];
    		    addfun[0] = 4;
	        	break;
	        case BROWN_ALMOST_LINEAR:
	        	double prodParam;
    			for (i = 0; i < param - 1; i++) {
    				for (int j = 0; j < param; j++) {
    				    if (i == j) {
    				    	jc[i][j] = 2.0;
    				    }
    				    else {
    				    	jc[i][j] = 1.0;
    				    }
    				}
    			}
    			for (i = 0; i < param; i++) {
    				prodParam = 1.0;
    				for (int j = 0; j < param; j++) {
    					if (i != j) {
    						prodParam = prodParam*x0[j];
    					}
    				}
    			    jc[param-1][i] = prodParam;	
    			}
    			addfun[0] = param-1;
	        	break;
	        case CHEBYQUAD:
	        	for (i = 1; i <= nPts; i++) {
        		    for (int j = 0; j < param; j++) {
        		    	jc[i-1][j] = shiftedChebyshevDerivative(x0[j],i)/param;
        		    }
        		}
	        	addfun[0] = nPts*param*(param+1)/2;
	        	break;
	        case LEVMAR_ROSENBROCK:
	        	jc[0][0] = (-2.0 + 2.0*x0[0] - 4.0*105.0*(x0[1] - x0[0]*x0[0])*x0[0]);
	        	jc[0][1] = (2*105.0*(x0[1] - x0[0]*x0[0]));
	        	jc[1][0] = (-2.0 + 2.0*x0[0] - 4.0*105.0*(x0[1] - x0[0]*x0[0])*x0[0]);
	        	jc[1][1] = (2*105.0*(x0[1] - x0[0]*x0[0]));	
	        	addfun[0] = 4;
	        	break;
	        case POWELL_2_PARAMETER:
	        	jc[0][0] = 1.0;
	        	jc[0][1] = 0.0;
	        	jc[1][0] = 1.0/((x0[0] + 0.1)*(x0[0] + 0.1));
	        	jc[1][1] = 4.0*x0[1];	
	        	addfun[0] = 2;
	        	break;
	        case HOCK1:
	        	jc[0][0] = -20.0*x0[0];
    		    jc[0][1] = 10.0;
    		    jc[1][0] = -1.0;
    		    jc[1][1] = 0.0;
    		    addfun[0] = 1;
    		    break;
	        case TRIGONOMETRIC:
	        	for (i = 1; i <= param; i++) {
	        		jc[i-1][i-1] = 0;
	        		for (int j = 1; j <= param; j++) {
	        		    if (i != j) {
	        		    	jc[i-1][j-1] = Math.sin(x0[j-1]);
	        		    	jc[i-1][i-1] += (-i*Math.sin(x0[i-1]) + Math.cos(x0[i-1]));
	        		    }
	        		    else {
	        		    	jc[i-1][i-1] += (-(i-1)*Math.sin(x0[i-1]) + Math.cos(x0[i-1]));
	        		    }
	        		}
	        	}
	        	break;
	        }
	    }
	    
	 // Shifted Chebyshev polynomial
        // Used over the half interval 0 <= x <= 1 instead of the full Chebyshev interval of -1 <= x <= 1
        private double shiftedChebyshev(double x, int n) {
        	// T*n+1(x) = (4x-2)*T*n(x) - T*n-1(x), where T* represents a shifted Chebyshev polynomial
        	double sc = 1.0;
        	double x2, x3, x4, x5, x6, x7, x8, x9, x10;
        	switch (n) {
        	    case 0:
        	    	sc = 1.0;
        	    	break;
        	    case 1:
        	        sc = 2.0*x - 1.0;
        	        break;
        	    case 2:
        	    	x2 = x*x;
        	    	sc = 8.0*x2 - 8.0*x + 1.0;
        	    	break;
        	    case 3:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	sc = 32.0*x3 - 48.0*x2 + 18.0*x - 1.0;
        	    	break;
        	    case 4:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	sc = 128.0*x4 - 256.0*x3 + 160.0*x2 - 32.0*x + 1.0;
        	    	break;
        	    case 5:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	sc = 512.0*x5 - 1280.0*x4 + 1120.0*x3 - 400.0*x2 + 50.0*x - 1.0;
        	    	break;
        	    case 6:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	sc = 2048.0*x6 - 6144.0*x5 + 6912.0*x4 - 3584.0*x3 + 840.0*x2 - 72.0*x + 1.0;
        	    	break;
        	    case 7:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	x7 = x6*x;
        	    	sc = 8192.0*x7 - 28672.0*x6 + 39424.0*x5 - 26880.0*x4 + 9408.0*x3 - 1568.0*x2 + 98.0*x - 1.0;
        	    	break;
        	    case 8:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	x7 = x6*x;
        	    	x8 = x7*x;
        	    	sc = 32768.0*x8 - 131072.0*x7 + 212992.0*x6 - 180224.0*x5 + 84480.0*x4 - 21504.0*x3
        	    	     + 2688.0*x2 - 128.0*x + 1.0;
        	    	break;
        	    case 9:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	x7 = x6*x;
        	    	x8 = x7*x;
        	    	x9 = x8*x;
        	    	sc = 131072.0*x9 - 589824.0*x8 + 1105920.0*x7 - 1118208.0*x6 + 658944.0*x5
        	    	     - 228096.0*x4 + 44352.0*x3 - 4320.0*x2 + 162.0*x - 1.0;
        	    	break;
        	    case 10:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	x7 = x6*x;
        	    	x8 = x7*x;
        	    	x9 = x8*x;
        	    	x10 = x9*x;
        	    	sc = 524288.0*x10 - 2621440.0*x9 + 5570560.0*x8 - 6553600.0*x7 + 4659200.0*x6
        	    	     - 2050048.0*x5 + 549120.0*x4 - 84480.0*x3 + 6600.0*x2 - 200.0*x + 1.0;
        	} // switch (n)
        	return sc;
        } // private double shiftedChebyshev
        
     // Shifted Chebyshev polynomial derivative
        private double shiftedChebyshevDerivative(double x, int n) {
        	double sc = 0.0;
        	double x2, x3, x4, x5, x6, x7, x8, x9;
        	switch (n) {
        	    case 0:
        	    	sc = 0.0;
        	    	break;
        	    case 1:
        	    	sc = 2.0;
        	        break;
        	    case 2:
        	    	sc = 16.0*x - 8.0;
        	    	break;
        	    case 3:
        	    	x2 = x*x;
        	    	sc = 96.0*x2 - 96.0*x + 18.0;
        	    	break;
        	    case 4:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	sc = 512.0*x3 - 768.0*x2 + 320.0*x - 32.0;
        	    	break;
        	    case 5:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	sc = 2560.0*x4 - 5120.0*x3 + 3360.0*x2 - 800.0*x + 50.0;
        	    	break;
        	    case 6:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	sc = 12288.0*x5 - 30720.0*x4 + 27648.0*x3 - 10752.0*x2 + 1680.0*x - 72.0;
        	    	break;
        	    case 7:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	sc = 57344.0*x6 - 172032.0*x5 + 197120.0*x4 - 107520.0*x3 + 28224.0*x2 - 3136.0*x + 98.0;
        	    	break;
        	    case 8:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	x7 = x6*x;
        	    	sc = 262144.0*x7 - 917504.0*x6 + 1277952.0*x5 - 901120.0*x4 + 337920.0*x3
        	    	     - 64512.0*x2 + 5376.0*x - 128.0;
        	    	break;
        	    case 9:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	x7 = x6*x;
        	    	x8 = x7*x;
        	    	sc = 1179648.0*x8 - 4718592.0*x7 + 7741440.0*x6 - 6709248.0*x5 + 3294720.0*x4
        	    	     - 912384.0*x3 + 133056.0*x2 - 8640.0*x + 162.0;
        	    	break;
        	    case 10:
        	    	x2 = x*x;
        	    	x3 = x2*x;
        	    	x4 = x3*x;
        	    	x5 = x4*x;
        	    	x6 = x5*x;
        	    	x7 = x6*x;
        	    	x8 = x7*x;
        	    	x9 = x8*x;
        	    	sc = 5242880.0*x9 - 23592960.0*x8 + 44564480.0*x7 - 45875200.0*x6 + 27955200.0*x5
        	    	     - 10250240.0*x4 + 2196480.0*x3 - 253440.0*x2 + 13200.0*x - 200.0;
        	} // switch (n)
        	return sc;
        } // private double shiftedChebyshevDerivative
	    
	    public void driver() {
	    	// Contains port from nesolve.m
	    	int i;
	    	int j;
	    	// Variables for trust region methods
	    	double trustvars[] = new double[4];
	    	double fvplus[];
	        double fc[] = new double[1];
	    	double sin[];
	    	double sx[];
	    	double sf[];
	    	int consecmax[] = new int[1];
	    	double prod;
	    	double maxProd;
	    	int addfun[] = new int[1];
	    	double jc[][];
	    	double gc[];
	    	double xc[];
	    	boolean restart;
	    	double normfv0 = 0.0;
	    	double fracdone;
	    	double maxfvc;
	    	double M[][];
	    	double hc[][];
	    	double sn[];
	    	int retcode[] = new int[1];
	    	double xplus[];
	    	double fplus[] = new double[1];
	    	boolean maxtaken[] = new boolean[1];
	    	double trilm[][];
	    	// Initialization
	    	// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
	    	// epsilon = D1MACH(4)
	        // Machine epsilon is the smallest positive epsilon such that
	        // (1.0 + epsilon) != 1.0.
	        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
	        // epsilon = 2.2204460e-16
	        // epsilon is called the largest relative spacing
	        eps = 1.0;
	        double neweps = 1.0;

	        while (true) {

	            if (1.0 == (1.0 + neweps)) {
	                break;
	            } else {
	                eps = neweps;
	                neweps = neweps / 2.0;
	            }
	        } // while(true)
	    	
	        if (fparam != null) {
	        	details[14] = 1;
	        }
	        if (analyticJacobian) {
	        	details[3] = 1;
	        }
	        if ((scale == null) && (details[15] == SCALING_WITH_SCALE)) {
	            details[15] = SCALING_WITHOUT_SCALE;	
	        }
	        if (path != null) {
	        	details[13] = 1;
	        }
	        if (details[13] == 1) {
	        	for (i = 0; i < x0.length; i++) {
	        		path.add(x0[i]);
	        	}
	        }
	        if (details[0] == 2) {
	        	btrack = null;
	        }
	        xf = new double[x0.length];
	        fvplus = new double[x0.length];
	        sin = new double[x0.length];
	        sx = new double[x0.length];
	        sf = new double[x0.length];
	        jc = new double[x0.length][x0.length];
	        gc = new double[x0.length];
	        fvc = new double[x0.length];
	        xc = new double[x0.length];
	        M = new double[x0.length][x0.length];
	        hc = new double[x0.length][x0.length];
	        sn = new double[x0.length];
	        xplus = new double[x0.length];
	        trilm = new double[x0.length][x0.length];
	        // Algorithm step 2
	        if (details[15] > 0) { // Might need F(x0) for scaling
	           for (i = 0; i < x0.length; i++) {
	        	   sin[i] = 1.0;
	           }
	           nefn(fc, fvplus, nofun, x0, sin, fparam);
	        } // if (details[15] > 0)
	        neinck(details, sx, sf, termcode, x0, fvplus, details, scale);
	        
	        // Algorithm step 3
	        if (termcode[0] < 0) {
	        	for (i = 0; i < x0.length; i++) {
	        		xf[i] = x0[i];
	        	}
	        	if (details[13] > 0) {
	        		for (i = 0; i < xf.length; i++) {
	        			path.add(xf[i]);
	        		}
	        	}
	        	return;
	        } // if (termcode[0] < 0)
	        
	        // Algorithm step 4.
	        itncount = 0;
	        
	        // Algorithm step 5.
	        nefn(fc, fvplus, nofun, x0, sf, fparam);
	        
	        // Algorithm step 6.
	        consecmax[0] = 0;
	        maxProd = 0.0;
	        for (i = 0; i < x0.length; i++) {
	            prod = sf[i] * Math.abs(fvplus[i]);
	            if (prod > maxProd) {
	            	maxProd = prod;
	            }
	        } // for (i = 0; i < x0.length; i++)
	        if (maxProd <= 1.0e-2 * details[7]) {
	        	termcode[0] = 1;
	        }
	        else {
	        	termcode[0] = 0;
	        }
	        
	        // Algorithm step 7.
	        if (termcode[0] > 0) {
	        	for (i = 0; i < x0.length; i++) {
	        		xf[i] = x0[i];
	            }
	        	if (details[13] > 0) {
	        		for (i = 0; i < xf.length; i++) {
	        			path.add(xf[i]);
	        	    }
	        	} // if (details[13] > 0)
	        } // if (termcode[0] > 0)
	        else { // termcode[0] <= 0
	            if (details[3] > 0) {
	            	if (testMode) {
	            		fitToTestJacobian(jc, addfun, x0, fparam);	
	            	}
	            	else {
	            	    fitToJacobian(jc, addfun, x0, fparam);
	            	}
	            	nofun[0] = nofun[0] + addfun[0];
	            } // if (details[3] > 0)
	            else if (initialJacobianIdentity) {
	            	for (i = 0; i < x0.length; i++) {
	            		for (j = 0; j < x0.length; j++) {
	            			if (i == j) {
	            				jc[i][j] = 1.0;
	            			}
	            			else {
	            				jc[i][j] = 0.0;
	            			}
	            		}
	            	}
	            }
	            else { 
	                nefdjac(jc, nofun, fvplus, x0, sx, details, fparam); 
	            } // else
	            for (i = 0; i < x0.length; i++) {
	            	gc[i] = 0.0;
	            	for (j = 0; j < x0.length; j++) {
	            		gc[i] = gc[i] + jc[j][i] * (fvplus[j] * sf[j] * sf[j]);
	            	}
	            } // for (i = 0; i < x0.length; i++)
	            for (i = 0; i < x0.length; i++) {
	            	fvc[i] = fvplus[i];
	            }
	        } // else termcode[0] <= 0
	        
	        // Algorithm step 8.
	        for (i = 0; i < x0.length; i++) {
	        	xc[i] = x0[i];
	        }
	        
	        // Algorithm step 9.
	        restart = true;
	        
	        // Algorithm step 10 (iteration).
	        if (details[0] > 0) {
	            normfv0 = 0.0;
	            for (i = 0; i < x0.length; i++) {
	            	if (Math.abs(fvc[i]) > normfv0) {
	            		normfv0 = Math.abs(fvc[i]);
	            	}
	            }
	        } // if (details[0] > 0)
	        
	        while (termcode[0] == 0) {
	            if (details[0] > 0) {
	                maxfvc = 0.0;
	                for (i = 0; i < x0.length; i++) {
	                	if (Math.abs(fvc[i]) > maxfvc) {
	                		maxfvc = Math.abs(fvc[i]);
	                	}
	                } // for (i = 0; i < x0.length; i++)
	                Preferences.debug("itncount = " + itncount + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("nofun[0] = " + nofun[0] + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("max(abs(fvc)) = " + maxfvc + "\n", Preferences.DEBUG_ALGORITHM);
	                fracdone = Math.log(maxfvc/normfv0)/Math.log(details[7]/normfv0);
	                Preferences.debug("fracdone = " + fracdone + "\n", Preferences.DEBUG_ALGORITHM);
	            } // if (details[0] > 0)
	            itncount++;
	            if ((details[3] > 0) || (details[2] > 0) || ((1 - details[4]) > 0)) {
	                nemodel(M, hc, sn, fvc, jc, gc, sf, sx, details[1]);
	            } // if ((details[3] > 0) || (details[2] > 0) || ((1 - details[4]) > 0))
	            else {
	            	MipavUtil.displayError("Factored model not implemented.");
	            }
	            if (details[1] == 1) {
	                nelnsrch(retcode, xplus, fplus, fvplus, maxtaken, nofun, btrack, xc, fc[0], gc, sn, 
	                		 sx, sf, details, fparam);
	            } // if (details[1] == 1)
	            else if (details[1] == 2) {
	                for (i = 0; i < x0.length; i++) {
	                	for (j = 0; j <= i; j++) {
	                		trilm[i][j] = M[i][j];
	                	}
	                }
	                nehook(retcode, xplus, fplus, fvplus, maxtaken, details, trustvars, nofun, xc, fc, gc, trilm,
	                		hc, sn, sx, sf, itncount, fparam);
	            } // else if (details[1] == 2)
	            else {
	            	MipavUtil.displayError("Dogleg not implemented");
	            }
	            if ((retcode[0] != 1) || (restart) || (details[3] > 0) || (details[2] > 0)) {
	                if (details[3] > 0) {
	                	if (testMode) {
	                		fitToTestJacobian(jc, addfun, xplus, fparam);	
	                	}
	                	else {
	                	    fitToJacobian(jc, addfun, xplus, fparam);
	                	}
	                	nofun[0] = nofun[0] + addfun[0];
	                } // if (details[3] > 0)
	                else if (details[2] > 0) {
	                    nefdjac(jc, nofun, fvplus, xplus, sx, details, fparam);	
	                } // else if (details[2] > 0)
	                else if (details[4] > 0) {
	                	MipavUtil.displayError("Factored secant method not implemented.");
	                }
	                else {
	                    nebroyuf(jc, xc, xplus, fvc, fvplus, sx, details[12]); // Broyden update
	                }
	                if (details[4] > 0) {
	                	MipavUtil.displayError("Gradient method for factored method not implemented");
	                	// Calculate gc using QR factorization (see book).
	                }
	                else {
	                	for (i = 0; i < x0.length; i++) {
	    	            	gc[i] = 0.0;
	    	            	for (j = 0; j < x0.length; j++) {
	    	            		gc[i] = gc[i] + jc[j][i] * (fvplus[j] * sf[j] * sf[j]);
	    	            	}
	    	            } // for (i = 0; i < x0.length; i++)
	                }
	                nestop(consecmax, termcode, xc, xplus, fvplus, fplus, gc, sx, sf, retcode[0],
	                		details, itncount, maxtaken[0]);
	            } // if ((retcode[0] != 1) || (restart) || (details[3] > 0) || (details[2] > 0))
	            if (((retcode[0] == 1) || (termcode[0] == 2)) && (!restart) && ((1 - details[3]) > 0) &&
	            		((1 - details[2]) > 0)) {
	            	nefdjac(jc, nofun, fvc, xc, sx, details, fparam);
	            	for (i = 0; i < x0.length; i++) {
    	            	gc[i] = 0.0;
    	            	for (j = 0; j < x0.length; j++) {
    	            		gc[i] = gc[i] + jc[j][i] * (fvc[j] * sf[j] * sf[j]);
    	            	}
    	            } // for (i = 0; i < x0.length; i++)
                	if ((details[1] == 2) || (details[1] == 3)) {
                		details[6] = -1;
                	}
                	restart = true;
                	if (termcode[0] == 2) {
                		termcode[0] = 0; // added by TAD
                	}
	            } // if (((retcode[0] == 1) || (termcode[0] == 2)) && (!restart) && ((1 - details[3]) > 0) &&
	            else {
	                if (termcode[0] > 0) {
	                    for (i = 0; i < xf.length; i++) {
	                    	xf[i] = xplus[i];
	                    }
	                    if (details[13] > 0) {
	                    	for (i = 0; i < xf.length; i++) {
	                    		path.add(xf[i]);
	                    	}
	                    }
	                } // if (termcode[0] > 0)
	                else {
	                	restart = false;
	                	if (details[13] > 0) {
	                		for (i = 0; i < xplus.length; i++) {
	                			path.add(xplus[i]);
	                		}
	                	}
	                } // else
	                for (i = 0; i < xc.length; i++) {
	                	xc[i] = xplus[i];
	                }
	                fc[0] = fplus[0];
	                for (i = 0; i < fvc.length; i++) {
	                	fvc[i] = fvplus[i];
	                }
	            } // else
	        } // while (termcode[0] == 0)
	    } // driver
	    
	    private void nefn(double fplus[], double fvplus[], int nofun[], double xplus[], double sf[], double fparam[]) {
	    	// fplus and fplus are outputs
	    	// nofun is an input/output
	    	// xplus, sf, and fparam are inputs
	    	// This function is part of the Nonlinear Equations package, see nesolve.m
	    	// It evaluates the vector function and calculates the sum of squares for nonlinear equations
	    	// Part of the modular software system from the appendix of the book "Numerical Methods for 
	    	// Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnable, 1983.
	    	// Originally coded in MATLAB by RIchard T. Behrens, April, 1988.
	    	int i;
	    	double prod;
	    	if (testMode) {
	    		fitToTestFunction(fvplus, xplus, fparam);	
	    	}
	    	else {
	    	    fitToFunction(fvplus, xplus, fparam);
	        }
	    	fplus[0] = 0.0;
	    	for (i= 0; i < sf.length; i++) {
	    	    prod = sf[i] * fvplus[i];
	    	    fplus[0] += (prod * prod);
	    	}
	    	fplus[0] = 0.5 * fplus[0];
	    	nofun[0]++;
	    } // nefn
	    
	    private void neinck(double dout[], double sx[], double sf[], int termcode[], double x0[], double f0[], 
	    		double din[], double scale[][]) {
	    	// dout, sx, sf, and termcode are outputs
	    	// x0, f0, din, and scale are inputs
	    	// This function is part of the Nonlinear Equations package, see nesolve.m.
	    	// It checks the input arguments and sets the various tolerances and limits.
	    	// Part of the modular software system from the appendix of the book "Numerical
	    	// Methods for Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnabel, 1983.
	    	// Originally coded in MATLAB by Richard T. Behrens, April, 1988.
	    	int i;
	    	termcode[0] = 0;
	    	for (i = 0; i < din.length; i++) {
	    		dout[i] = din[i];
	    	}
	    	
	    	// Step 1
	    	int n = x0.length;
	    	if (n < 1) {
	    		termcode[0] = -1;
	    		return;
	    	}
	    	
	    	// Steps 2 & 3
	    	int l = 0;
	    	int m = 0;
	    	if (scale != null) {
	    	    l = scale.length;
	    	    m = scale[0].length;
	    	}
	    	if (dout[15] == 2) {
	    		if ((l == n) && (m == 2)) {
	    			for (i = 0; i < n; i++) {
	    				sx[i] = 1.0/Math.abs(scale[i][0]);
	    				sf[i] = 1.0/Math.abs(scale[i][1]);
	    			}
	    		}
	    		else {
	    			dout[15] = 1;
	    		}
	    	} // if (dout[15] == 2)
	    	if (dout[15] == 0) {
	    		for (i = 0; i < n; i++) {
	    			sx[i] = 1.0;
	    			sf[i] = 1.0;
	    		}
	    	} // if (dout[15] == 0)
	    	if (dout[15] == 1) {
	    	    for (i = 0; i < n; i++) {
	    	    	if (x0[i] == 0) {
	    	    		x0[i] = 1;
	    	    	}
	    	    	if (f0[i] == 0) {
	    	    		f0[i] = 1;
	    	    	}
	    	    	sx[i] = 1.0/Math.abs(x0[i]);
	    	    	sf[i] = 1.0/Math.abs(f0[i]);
	    	    } // for (i = 0; i < n; i++)
	    	} // if (dout[15] == 1)
	    	
	    	// Step 4
	    	if (dout[11] <= 0) {
	    		dout[12] = eps;
	    	}
	    	else {
	    		dout[12] = Math.max(eps, Math.pow(10.0, -dout[11]));
	    	}
	    	if (dout[12] > 0.01) {
	    		termcode[0] = -2;
	    		return;
	    	}
	    	
	    	// Step 5
	    	if (dout[1] <= 0) {
	    		dout[1] = 1; // Default to linesearch
	    	}
	    	if (((dout[1] == 2) || (dout[1] == 3)) && (dout[6] <= 0)) {
	    		dout[6] = -1;
	    	}
	    	
	    	// Step 6
	    	if (dout[5] <= 1) {
	    		dout[5] = 100; // Default to 100 iteration limit.
	    	}
	    	if (dout[7] <= 0) {
	    		dout[7] = Math.pow(eps, (1.0/3.0)); // fvectol
	    	}
	    	if (dout[8] <= 0) {
	    		dout[8] = Math.pow(eps, (2.0/3.0)); // steptol
	    	}
	    	if (dout[9] <= 0) {
	    		dout[9] = eps; // mintol
	    	}
	    	if (dout[10] <= 0) {
	    		double normsxx0 = 0.0;
	    		for (i = 0; i < n; i++) {
	    			double prod = sx[i] * x0[i];
	    			normsxx0 += (prod * prod);
	    		}
	    		normsxx0 = Math.sqrt(normsxx0);
	    		double maxsx = -Double.MAX_VALUE;
	    		for (i = 0; i < n; i++) {
	    			if (sx[i] > maxsx) {
	    				maxsx = sx[i];
	    			}
	    		}
	    		dout[10] = 1000.0 * Math.max(normsxx0, maxsx); // maxstep
	    	} // if (dout[10] <= 0)
	    } // neinck
	    
	    private void nefdjac(double J[][], int nofun[], double fc[], double xc[], double sx[], double details[], double fparam[]) {
	        // J is an output
	    	// nofun is an input/output
	    	// fc, xc, sx, details, and fparam are inputs
	    	// This function is part of the Nonlinear Equations package.
	    	// This is a "Finite Difference Jacobian Approximation".  It calculates a finite 
	    	// difference approximation to J(xc), the Jacobian of F(X) at x = xc.
	    	// Algorithm A5.4.1: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Sherkat Masoum M., March, 1988.
	    	// Edited by Richard T. Behrens, June, 1988.
	    	
	    	// Algorithm step 1.
	    	double fj[] = new double[xc.length];
	    	int n = fc.length;
	    	double sqrteta = Math.sqrt(details[12]);
	    	
	    	// Algorithm step 2.
	    	for (int j = 0; j < n; j++) {
	    	    double addOne = 0.0;
	    	    if (xc[j] == 0) {
	    	    	addOne = 1.0;
	    	    }
	    	    double stepsizej = sqrteta * Math.max(Math.abs(xc[j]), 1.0/sx[j]) * (sign(xc[j]) + addOne);
	    	    // To incorporate a different stepsize rule, change the previous line.
	    	    double tempj = xc[j];
	    	    xc[j] = xc[j] + stepsizej;
	    	    stepsizej = xc[j] - tempj;
	    	    // The previous line reduces finite precision error slightly, see section 5.4 of the book.
	    	    if (testMode) {
	    	    	fitToTestFunction(fj, xc, fparam);	
	    	    }
	    	    else {
	    	        fitToFunction(fj, xc, fparam);
	    	    }
	    	    nofun[0]++;
	    	    for (int i = 0; i < n; i++) {
	    	    	J[i][j] = (fj[i] - fc[i])/stepsizej;
	    	    }
	    	    xc[j] = tempj;
	    	} // for (int j = 0; j < n; j++)
	    } // nefdjac
	    
	    private double sign(double x) {
	    	if (x > 0) {
	    		return 1;
	    	}
	    	else if (x == 0) {
	    		return 0;
	    	}
	    	else {
	    		return -1;
	    	}
	    } // sign
	    
	    private void nemodel(double m[][], double h[][], double sn[], double fc[], double J[][], double g[],
	    		double sf[], double sx[], double globmeth) {
	        // m, h, and sn are outputs
	    	// fc, J, g, sf, sx, and globmeth are inputs
	    	// This function is part of the Nonlinear Equations package
	    	// It forms the affine model for use in solving nonlinear equations.
	    	// Algorithm A6.5.1: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Sherkat Masoum M., March, 1988.
	    	// Edited by Richard T. Behrens, June, 1988.
	    	
	    	// Algorithm step 1.
	    	int i;
	    	int j;
	    	int k;
	    	double est;
	    	int n = Math.max(J.length, J[0].length);
	    	for (i = 0; i < n; i++) {
	    		for (j = 0; j < n; j++) {
	    			m[i][j] = sf[i] * J[i][j];
	    		}
	    	}
	    	
	    	// Algorithm step 2.
	    	int n2 = Math.max(m.length, m[0].length);
	    	double m1[] = new double[n2];
	    	double m2[] = new double[n2];
	    	int sing[] = new int[1];
	    	neqrdcmp(m, m1, m2, sing);
	    	
	    	// Algorithm step 3.
	    	if (sing[0] == 0) {
	    	    for (j = 1; j < n; j++) {
	    	    	for (i = 0; i <= j-1; i++) {
	    	    		m[i][j] = m[i][j]/sx[j];
	    	    	}
	    	    } // for (j = 1; j < n; j++)
	    	    for (i = 0; i < m2.length; i++) {
	    	    	m2[i] = m2[i]/sx[i];
	    	    }
	    	    est = neconest(m,m2);
	    	} // if (sing[0] == 0)
	    	else {
	    		est = 0.0;
	    	}
	    	
	    	// Algorithm step 4.
	    	// MATLAB has (est > 1.0/eps)
	    	if ((sing[0] == 1) || (est > 1.0/Math.sqrt(eps)) || Double.isNaN(est)) {
	    		for (i = 0; i < n; i++) {
	    			for (j = i; j < n; j++) {
	    				h[i][j] = 0.0;
	    				for (k = 0; k < n; k++) {
	    					h[i][j] += J[k][i] * J[k][j] * sf[k] * sf[k];
	    				}
	    			}
	    		}
	    	    // Calculate hnorm = norm(invDxHinvDx)
	    	    double tem = 0.0;
	    	    for (i = 0; i < n; i++) {
	    	    	tem += (Math.abs(h[0][i])/sx[i]);
	    	    }
	    	    double hnorm = (1.0/sx[0]) * tem;
	    	    for (i = 1; i < n; i++) {
	    	    	double tem1 = 0.0;
	    	    	for (j = 0; j <= i; j++) {
	    	    		tem1 += (Math.abs(h[j][i])/sx[j]);
	    	    	}
	    	    	double tem2 = 0.0;
	    	    	for (j = i+1; j < n; j++) {
	    	    		tem2 += (Math.abs(h[i][j])/sx[j]);
	    	    	}
	    	    	// MATLAB has (1.0/sx[i])/(tem1 + tem2)
	    	    	double temp = (1.0/sx[i]) * (tem1+tem2);
	    	    	hnorm = Math.max(temp, hnorm);
	    	    } // for (i = 1; i < n; i++)
	    	    for (i = 0; i < n; i++) {
	    	    	h[i][i] = h[i][i] + Math.sqrt(n*eps) * hnorm * sx[i] * sx[i];
	    	    } 
	    	    // Calculate sn = inv(H) * g, and keep m (the cholesky factor) for later use.
	    	    double maxadd[] = new double[1];
	    	    nechdcmp(m, maxadd, h, 0);
	    	    Matrix matM = new Matrix(m);
	    	    double mInverse[][] = (matM.inverse()).getArray();
	    	    double mg[] = new double[n];
	    	    for (i = 0; i < n; i++) {
	    	    	for (j = 0; j < n; j++) {
	    	    		mg[i] += mInverse[i][j] * g[j];
	    	    	}
	    	    }
	    	    double mTInverse[][] = ((matM.transpose()).inverse()).getArray();
	    	    for (i = 0; i < n; i++) {
	    	    	for (j = 0; j < n; j++) {
	    	    		sn[i] -= mTInverse[i][j] * mg[j];
	    	    	}
	    	    }
	    	} // if ((sing[0] == 1) || (est > 1.0/eps) || Double.isNaN(est))
	    	else {
	    		// Calculate normal Newton step
	    		for (j = 1; j < n; j++) {
	    			for (i = 0; i <= j-1; i++) {
	    				m[i][j] = m[i][j] * sx[j];
	    			}
	    		}
	    		for (i = 0; i < m2.length; i++) {
	    			m2[i] = m2[i] * sx[i];
	    		}
	    		for (i = 0; i < sf.length; i++) {
	    			sn[i] = -sf[i] * fc[i];
	    		}
	    		neqrsolv(sn,m,m1,m2);
	    		if ((globmeth == 2) || (globmeth == 3)) {
	    		    // The cholesky factor (for later use) is the same as R' from QR.
	    			for (i = 0; i < n; i++) {
	    				m[i][i] = m2[i];
	    				for (j = 0; j <= i-1; j++) {
	    					m[i][j] = m[j][i];
	    				}
	    			}
	    		} // if ((globmeth == 2) || (globmeth == 3))
	    		if (globmeth == 2) {
	    			for (i = 0; i < n; i++) {
	    				h[i][i] = 0;
	    				for (k = 0; k <= i; k++) {
	    					h[i][i] += (m[i][k]*m[i][k]);
	    				}
	    				for (j=i+1; j < n; j++) {
	    					h[i][j] = 0;
	    					for (k = 0; k <= i; k++) {
	    						h[i][j] += (m[i][k] * m[j][k]);
	    					}
	    				}
	    			} // for (i = 0; i < n; i++)
	    		} // if (globmeth == 2)
	    		else {
	    			h = null;
	    		}
	    	} // else
	    	return;
	    } // nemodel
	    
	    private void neqrdcmp(double M[][], double M1[], double M2[], int sing[]) {
	        // M is input/output
	        // M1, M2, and sing are output
	    	// This function is part of the Nonlinear Equations package.
	    	// It is a QR decomposition function.  It differs from the one built into MATLAB
	    	// in that the result is encoded as rotation angles.  Also, it is designed for
	    	// square matrices only.
	    	// Algorithm A3.2.1: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, March, 1988.
	    	int i;
	    	int j;
	    	
	    	int n = Math.max(M.length, M[0].length);
	    	for (i = 0; i < n; i++) {
	    		M1[i] = 0;
	    		M2[i] = 0;
	    	}
	    	// Algorithm step 1.
	    	// sing becomes true if singularity of M is detected during the decomposition
	    	sing[0] = 0;
	    	
	    	// Algorithm step 2.
	    	for (int k = 0; k < n-1; k++) {
	    	    double eta = -Double.MAX_VALUE;
	    	    for (i = k; i < n; i++) {
	    	    	// Absolute value present in book is missing in MATLAB code.
	    	    	if (Math.abs(M[i][k]) > eta) {
	    	    		eta = Math.abs(M[i][k]);
	    	        }
	    	    } // for (i = k; i < n; i++)
	    	    if (eta == 0.0) { // matrix is singular
	    	    	M1[k] = 0;
	    	    	M2[k] = 0;
	    	    	sing[0] = 1;
	    	    }
	    	    else {
	    	    	// Form Qk and premultiply M by it.`
	    	    	for (i = k; i < n; i++) {
	    	    		M[i][k] = M[i][k]/eta;
	    	    	}
	    	    	double normM = 0;
	    	    	for (i = k; i < n; i++) {
	    	    		normM += (M[i][k]*M[i][k]);
	    	    	}
	    	    	normM = Math.sqrt(normM);
	    	    	double addOne = 0.0;
	    	    	if (M[k][k] == 0.0) {
	    	    		addOne = 1.0;
	    	    	}
	    	    	double sigma = (sign(M[k][k]) + addOne) * normM;
	    	    	M[k][k] = M[k][k] + sigma;
	    	    	M1[k] = sigma * M[k][k];
	    	    	M2[k] = -eta * sigma;
	    	    	for (j = k+1; j < n; j++) {
	    	    		double tau = 0.0;
	    	    		for (i = k; i < n; i++) {
	    	    			tau += (M[i][k] * M[i][j]);
	    	    		}
	    	    		tau = tau / M1[k];
	    	    		for (i = k; i < n; i++) {
	    	    			M[i][j] = M[i][j] - tau * M[i][k];
	    	    		}
	    	    	} // for (j = k+1; j < n; j++)
	    	    } // else
	    	} // for (int k = 0; k < n-1; k++)
	    	
	    	// Algorithm step 3
	    	M2[n-1] = M[n-1][n-1];
	    	return;
	    } // neqrdcmp
	    
	    private double neconest(double M[][], double M2[]) {
	    	// This function is part of the Nonlinear Equations package.
	    	// This is an estimate of the l-1 condition number of an upper triangular matrix.
	    	// Algorithm A3.3.1: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization Methods and Nonlinear Equations"
	    	// by Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, March, 1988.
	    	int i;
	    	int j;
	    	double est;
	    	
	    	// Allocate variables
	    	int n = Math.max(M.length, M[0].length);
	    	double p[] = new double[n];
	    	double pm[] = new double[n];
	    	double x[] = new double[n];
	    	
	    	// Algorithm steps 1 & 2
	    	est = Math.abs(M2[0]);
	    	for (j = 1; j < n; j++) {
	    		double temp = Math.abs(M2[j]);
	    		for (i = 0; i <= j-1; i++) {
	    			temp += Math.abs(M[i][j]);
	    		}                      
	    		est = Math.max(temp,  est);
	    	} // for (j = 1; j < n; j++)
	    	
	    	// Algorithm step 3.
	    	x[0] = 1.0/M2[0];
	    	
	    	// Algorithm step 4.
	    	for (i = 1; i < n; i++) {
	    		p[i] = M[0][i] * x[0];
	    	}
	    	
	    	// Algorithm step 5.
	    	for (j = 1; j < n; j++) {
	    		double xp = (+1 - p[j])/M2[j];
	    		double xm = (-1 - p[j])/M2[j];
	    		double temp = Math.abs(xp);
	    		double tempm = Math.abs(xm);
	    		for (i = j+1; i < n; i++) {
	    			pm[i] = p[i] + M[j][i] * xm;
	    			// MATLAB has tempm = tempm + Math.abs(p[i])/Math.abs(M2[i]);
	    			tempm = tempm + Math.abs(pm[i])/Math.abs(M2[i]);
	    			p[i] = p[i] + M[j][i] * xp;
	    			temp = temp + Math.abs(p[i])/Math.abs(M2[i]);
	    		} // for (i = j+1; i < n; i++)
	    		if (temp > tempm) {
	    			x[j] = xp;
	    		}
	    		else {
	    			x[j] = xm;
	    			for (i = j+1; i < n; i++) {
	    				p[i] = pm[i];
	    			}
	    		}
	    	} // for (j = 1; j < n; j++)
	    	
	    	// Algorithm steps 6 & 7.
	    	double sum = 0.0;
	    	for (i = 0; i < n; i++) {
	    		sum += Math.abs(x[i]);
	    	}
	    	est = est / sum;
	    	
	    	// Algorithm step 8.
	    	nersolv(x, M, M2);
	    	
	    	// Algorithm steps 9 & 10.
	    	sum = 0.0;
	    	for (i = 0; i < n; i++) {
	    		sum += Math.abs(x[i]);
	    	}
	    	est = est * sum;
	    	return est;
	    } // neconest
	    
	    private void nersolv(double b[], double M[][], double M2[]) {
	        // b is input/output
	    	// input M and M2
	    	// This routine is part of the Nonlinear Equations package.
	    	// It a a linear equation solve function for upper triangular systems.
	    	// Algorithm A3.2.2a: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, March, 1988.
	    	int i;
	    	int j;
	    	// Algorithm step 1.
	    	int n = Math.max(M.length,M[0].length);
	    	b[n-1] = b[n-1]/M2[n-1];
	    	
	    	// Algorithm step 2.
	    	for (i = (n-2); i >= 0; i--) {
	    		double sum = 0;
	    	    for (j = i+1; j < n; j++) {
	    	    	sum += (M[i][j] * b[j]);
	    	    }
	    	    b[i] = (b[i] - sum)/M2[i];
	    	} // for (i = (n-2); i >= 0; i--) 
	    } // nersolv
	    
	    private void nechdcmp(double L[][], double maxadd[], double H[][], double maxoffl) {
	    	// outputs L and maxadd
	    	// inputs H and maxoff1
	    	// This routine is part of the Nonlinear Equations package.
	    	// This is a "Perturbed Cholesky Decomposition".  It finds a lower triangular
	    	// matrix L such that LL' is a factorization of H+D, where D is a diagonal (non-negative)
	    	// matrix that is added to H if necessary to make it positive definite (so that the
	    	// factorization is possible).  If H is already positive definite, the ordinary Cholesky
	    	// decomposition (D=0) is carried out.
	    	// Algorithm A5.5.2: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Sherkat Masoum M., April, 1988.
	    	// Edited by Richard T. Behrens, June, 1988.
	    	int i;
	    	int j;
	    	int k;
	    	double sum;
	    	double minl2 = 0.0;
	    	
	    	// Check input arguments
	    	int m = H.length;
	    	int n = H[0].length;
	    	if (m != n) {
	    		MipavUtil.displayError("Matrix H Must be square");
	    	}
	    	
	    	// Algorithm step 1.
	    	double minl = Math.pow(eps, 0.25) * maxoffl;
	    	
	    	// Algorithm step 2.
	    	if (maxoffl == 0) {
	    	    // This is the case when H is known to be positive def.
	    	    double maxdiagH = -Double.MAX_VALUE;
	    	    for (i = 0; i < n; i++) {
	    	    	// Book has absolute value missing in MATLAB.
	    	    	if (Math.abs(H[i][i]) > maxdiagH) {
	    	    		maxdiagH = Math.abs(H[i][i]);
	    	    	}
	    	    }
	    	    maxoffl = Math.sqrt(maxdiagH);
	    	    minl2 = Math.sqrt(eps) * maxoffl;
	    	} // if (maxoffl == 0)
	    	
	    	// Algorithm step 3.
	    	maxadd[0] = 0; // The maximum diagonal element (so far) in D.
	    	
	    	// Algorithm step 4.
	    	for (j = 0; j < n; j++) {
	    		if (j == 0) {
	    			L[j][j] = H[j][j];
	    		}
	    		else {
	    		    sum = 0.0;
	    		    for (i = 0; i <= j-1; i++) {
	    		    	sum += (L[j][i] *L[j][i]);
	    		    }
	    		    L[j][j] = H[j][j] - sum;
	    		} // else
	    		double minljj = 0.0;
	    		for (i = j+1; i < n; i++) {
	    		    if (j == 0) {
	    		    	L[i][j] = H[j][i];
	    		    }
	    		    else {
	    		    	sum = 0.0;
	    		    	for (k = 0; k <= j-1; k++) {
	    		    		sum += (L[i][k]*L[j][k]);
	    		    	}
	    		    	L[i][j] = H[j][i] - sum;
	    		    } // else
	    		    minljj = Math.max(Math.abs(L[i][j]), minljj);
	    		} // for (i = j+1; i < n; i++) 
	    		minljj = Math.max(minljj/maxoffl, minl);
	    		if (L[j][j] > minljj*minljj) {
	    		    // Normal Cholesky iteration
	    			L[j][j] = Math.sqrt(L[j][j]);
	    		}
	    		else {
	    			// Augment H[j][j]
	    			if (minljj < minl2) {
	    				minljj = minl2;
	    				// Only possible when input maxoffl == 0
	    			}
	    			maxadd[0] = Math.max(maxadd[0],  (minljj*minljj - L[j][j]));
	    			L[j][j] = minljj;
	    		} // else
	    		for (i = j+1; i < n; i++) {
	    			L[i][j] = L[i][j]/L[j][j];
	    		}
	    	} // for (j = 0; j < n; j++)
	    	return;
	    } // nechdcmp
	    
	    private void neqrsolv(double b[], double M[][], double M1[], double M2[]) {
	    	// b is input/output
	    	// input M, M1, and M2
	    	// This routine is part of the Nonlinear Equations package.
	    	// It is a linear equation solve function using the QR decomposition.
	    	// Algorithm A3.2.2: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, March, 1988.
	    	int i;
	    	int j;
	    	double sum;
	    	// Algorithm step 1.
	    	int n = Math.max(M.length, M[0].length);
	    	for (j = 0; j < n-1; j++) {
	    	    sum = 0.0;
	    	    for (i = j; i < n; i++) {
	    	    	sum += (M[i][j] * b[i]);
	    	    }
	    	    double tau = sum/M1[j];
	    	    for (i = j; i < n; i++) {
	    	    	b[i] = b[i] - tau * M[i][j];
	    	    }
	    	} // for (j = 0; j < n-1; j++)
	    	
	    	// Algorithm step 2.
	    	nersolv(b, M, M2);
	    	return;
	    } // neqrsolv
	    
	    private void nelnsrch(int retcode[], double xp[], double fp[], double Fp[], boolean maxtaken[], int nofun[],
	    		double btrack[], double xc[], double fc, double g[], double p[], double sx[], double sf[],
	    		double details[], double fparam[]) {
	    	// input/output nofun, btrack
	    	// output retcode, xp, fp, Fp, maxtaken
	    	// input xc, fc, g, p, sx, sf, details, fparam
	    	// This routine is part of the Nonlinear Equations package and the Unconstrained Minimization package.
	    	// It is a line search for use with Newton's Method of solving nonlinear equations.  For function
	    	// evaluations, it needs to know whether it is doing Nonlinear Equations (NE) or Unconstrained Minimization
	    	// (UM); it distinguishes the two by the length of details which is 16 for NE and 17 for UM.
	    	// Algorithm A6.3.1: Part of the modular software system from the appendix of the book "Numerical Methods
	    	// for Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, March, 1988.
	    	// Modified slightly for UM usage, January, 1989.
	    	
	    	// retcode[0] = 0: satisfactory xp found
	    	// retcode[0] = 1: routine failed to locate satisfactory xp sufficiently distinct from xc.
	    	int i;
	    	double prod;
	    	double val;
	    	double lambdatemp;
	    	double lambdaprev = 1.0;
	    	double fpprev = 0.0;
	    	double a[] = new double[2];
	    	
	    	// Initialization
	    	for (i = 0; i < xp.length; i++) {
	    		xp[i] = 0;
	    	}
	    	fp[0] = 0;
	    	boolean umflag = (details.length == 17);  // This is how we tell NE from UM.
	    	
	    	// Algorithm step 1.
	    	maxtaken[0] = false;
	    	
	    	// Algorithm step 2.
	    	retcode[0] = 2;
	    	
	    	// Algorithm step 3.
	    	double alpha = 1.0E-4;
	    	
	    	// Algorithm step 4.
	    	double newtlen = 0.0;
	    	for (i = 0; i < sx.length; i++) {
	    	    prod = sx[i] * p[i];
	    	    newtlen += (prod * prod);
	    	}
	    	newtlen = Math.sqrt(newtlen);
	    	
	    	// Algorithm step 5
	    	if (newtlen > details[10]) {
	    		double ratio = details[10]/newtlen;
	    		for (i = 0; i < p.length; i++) {
	    			p[i] = p[i] * ratio;
	    		}
	    		newtlen = details[10];
	    	} // if (newtlen > details[10])
	    	
	    	// Algorithm step 6.
	    	double initslope = 0.0;
	    	for (i = 0; i < g.length; i++) {
	    		initslope += (g[i] * p[i]);
	    	}
	    	if (initslope >= 0.0) {
	    		System.err.println("nelnsrch requires gTp < 0, but gTp = " + initslope);
	    		Preferences.debug("nelnsrch requires gTp < 0, but gTp = " + initslope + "\n", Preferences.DEBUG_ALGORITHM);
	    		retcode[0] = 1;
	    		return;
	    	}
	    	
	    	// Algorithm step 7.
	    	double rellength = -Double.MAX_VALUE;
	    	for (i = 0; i < p.length; i++) {
	    		val = Math.abs(p[i])/Math.max(Math.abs(xc[i]), 1.0/sx[i]);
	    		if (val > rellength) {
	    			rellength = val;
	    		}
	    	} // for (i = 0; i < p.length; i++) 
	    	
	    	// Algorithm step 8.
	    	double minlambda = details[8]/rellength;
	    	
	    	// Algorithm step 9.
	    	double lambda = 1.0;
	    	
	    	// Algorithm step 10.
	    	int bt = 0;
	    	while (retcode[0] >= 2) {
	    		for (i = 0; i < xc.length; i++) { // step 10.1
	    			xp[i] = xc[i] + lambda * p[i];
	    		}
	    		if (umflag) { // step 10.2
	    			// fp must be a single value in umflag
	    			if (testMode) {
	    				fitToTestFunction(fp, xp, fparam);	
	    			}
	    			else {
	    	            fitToFunction(fp, xp, fparam);
	    			}
	    	        nofun[0]++;
	    		} // if (umflag)
	    		else { // !umflag
	    			// fp is a single value
	    			nefn(fp, Fp, nofun, xp, sf, fparam);
	    		} // else !umflag
	    		if (fp[0] <= fc + alpha*lambda*initslope) { // step 10.3a
	    			retcode[0] = 0;
	    			maxtaken[0] = ((lambda == 1) && (newtlen > 0.99 * details[10]));
	    		}
	    		else if (lambda < minlambda) { // step 10.3b
	    			retcode[0] = 1;
	    			for (i = 0; i < xc.length; i++) {
	    				xp[i] = xc[i];
	    			}
	    		} // else if (lambda < minlambda) 
	    		else { // step 10.3c
	    		    if (lambda == 1.0) {
	    		        //if (details[0] > 0) {
	    		        	//System.out.println("Quadratic Backtrack");
	    		        //}
	    		        //Preferences.debug("Quadratic Backtrack\n", Preferences.DEBUG_ALGORITHM);
	    		        bt++;
	    		        lambdatemp = -initslope / (2*(fp[0]-fc-initslope));
	    		    } // if (lambda == 1.0)
	    		    else { // lambda != 1.0
	    		        //if (details[0] > 0) {
	    		        	//System.out.println("Cubic Backtrack");
	    		        //}
	    		        //Preferences.debug("Cubic Backtrack\n", Preferences.DEBUG_ALGORITHM);
	    		        bt++;
	    		        double k = (1.0/(lambda - lambdaprev));
	    		        double lambdaSquared = lambda * lambda;
	    		        double lambdaprevSquared = lambdaprev*lambdaprev;
	    		        double b00 = 1.0/(lambdaSquared);
	    		        double b01 = -1.0/lambdaprevSquared;
	    		        double b10 = -lambdaprev/lambdaSquared;
	    		        double b11 = lambda/lambdaprevSquared;
	    		        double c00 = fp[0] - fc - lambda*initslope;
	    		        double c10 = fpprev - fc - lambdaprev*initslope;
	    		        a[0] = k*(b00*c00 + b01*c10);
	    		        a[1] = k*(b10*c00 + b11*c10);
	    		        double disc = a[1]*a[1] - 3.0*a[0]*initslope;
	    		        if (a[0] == 0) {
	    		        	lambdatemp = -initslope/(2.0*a[1]);
	    		        }
	    		        else {
	    		        	lambdatemp = (-a[1] + Math.sqrt(disc))/(3.0*a[0]);
	    		        }
	    		        if (lambdatemp > 0.5 * lambda) {
	    		        	lambdatemp = 0.5 * lambda;
	    		        }
	    		    } // else lambda != 1.0
	    		    lambdaprev = lambda;
	    		    fpprev = fp[0];
	    		    if (lambdatemp <= 0.1 * lambda) {
	    		    	lambda = 0.1 * lambda;
	    		    }
	    		    else {
	    		    	lambda = lambdatemp;
	    		    }
	    		} // else step 10.3c
	    	} // while (retcode[0] >= 2)
	    	if (btrack != null) {
		    	if (bt < btrack.length) {
		    		btrack[bt] = btrack[bt] + 1;
		    	}
		    	else {
		    		btrack[bt] = 1;
		    	}
	    	} // if (btrack != null)
	    	return;
	    } // nelnsrch
	    
	    private void nehook (int retcode[], double xp[], double fp[], double Fp[], boolean maxtaken[], double details[], 
	    		double trustvars[], int nofun[], double xc[], double fc[], double g[], double L[][], double H[][],
	    		double sn[], double sx[], double sf[], int itn, double fparam[]) {
	    	// input/output details, trustvars, nofun
	    	// output retcode, xp, fp, Fp, maxtaken
	    	// input xc, fc, g, L, H, sn, sx, sf, itn, fparam
	    	// This routine is part of the Nonlinear Equations package and Unconstrained Minimization package.
	    	// It is a driver for locally constrained optimal ("hook") steps for use with Newton's Method of
	    	// solving nonlinear equations.  For function evaluations, it needs to know whether it is doing
	    	// Nonlinear Equations (NE) or Unconstrained Minimization (UM).  it distinguishes the two by the
	    	// length of details which is 16 for NE and 17 for UM.
	    	// trustvars is a vector of variables that, though not used externally, needs to be preserved between calls
	    	// to nehook.  The elements are defined as:
	    	// 1 = mu
	    	// 2 = deltaprev
	    	// 3 = phi
	    	// 4 = phiprime
	    	// Algorithms A6.4.1 and A6.4.2: Incorporates both the "hookdriver" and "hookstep" algorithms.  Part of
	    	// the modular software system from the appendix of the book "Numerical Methods for Unconstrained Optimization
	    	// and Nonlinear Equations" by Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, August, 1990.
	    	int i;
	    	int j;
	    	double prod;
	    	double val;
	    	double alpha;
	    	
	    	// Initialization
	    	int n = xc.length;
	    	boolean umflag = (details.length == 17); // This is how we tell NE from UM.
	    	double xpprev[] = new double[n];
	    	double fpprev[] = new double[1];
	    	double Fpprev[] = new double[n];
	    	boolean newttaken;
	    	double s[] = null;
	    	double phiprimeinit = 0.0;
	    	double mulow;
	    	double muup;
	    	double div;
	    	double L642[][] = new double[sx.length][sx.length];
	        double maxadd[] = new double[1];
	        double Hadd[][] = new double[sx.length][sx.length];
	    	double sxdiag[][] = new double[sx.length][sx.length];
	    	// Algorithm steps 1-3.
	    	retcode[0] = 4;
	    	int firsthook = 1;
	    	double newtlen = 0.0;
	    	double beta;
	    	double temp;
	    	double sum;
	    	double tempvec[][];
	    	for (i = 0; i < sx.length; i++) {
	    	    prod = sx[i] * sn[i];
	    	    newtlen += (prod*prod);
	    	}
	    	newtlen = Math.sqrt(newtlen);
	    	
	    	// Algorithm step 4.
	    	if ((itn == 1) || (details[6] == -1)) { // details[6] is delta.
	    	    trustvars[0] = 0; // trustvars[0] is mu.
	    	    if (details[6] == -1) {
	    	    	alpha = 0.0;
	    	    	for (i = 0; i < g.length; i++) {
	    	    		val = g[i]/sx[i];
	    	    		alpha += (val*val);
	    	    	}
	    	    	beta = 0.0;
	    	    	for (i = 0; i < n; i++) {
	    	    		temp = 0.0;
	    	    		for (j = i; j < n; j++) {
	    	    	        temp += (L[j][i] * g[j]/(sx[j] * sx[j]));
	    	    		}
	    	    		beta = beta + temp * temp;
	    	    	} // for (i = 0; i < n; i++)
	    	    	details[6] = Math.pow(alpha, 1.5)/beta;
	    	    	if (details[6] > details[10]) { // details[10] is maxstep
	    	    		details[6] = details[10];
	    	    	}
	    	    } // if (details[6] == -1)
	    	} // if ((itn == 1) || (details[6] == -1))
	    	
	    	// Algorithm step 5 (incorporating algorithm A6.4.2).
	    	while (retcode[0] >= 2) { // Calculate and check a new step
	    		double hi = 1.5; // Start of A.6.4.2.
	    		double lo = 0.75;
	    		if (newtlen <= hi * details[6]) {
	    		    newttaken = true;	
	    		    s = new double[sn.length];
	    		    for (i = 0; i < sn.length; i++) {
	    		    	s[i] = sn[i];
	    		    }
	    		    trustvars[0] = 0; // trustvars[0] is mu
	    		    details[6] = Math.min(details[6],  newtlen);
	    		} // if (newtlen <= hi * details[6])
	    		else { // newtlen > hi * details[6]
	    		    newttaken = false;
	    		    if (trustvars[0] > 0) {
	    		        trustvars[0] = trustvars[0] - ((trustvars[2] + trustvars[1])/details[6]) *
	    		        		(((trustvars[1] - details[6]) + trustvars[2])/trustvars[3]);
	    		    } // if (trustvars[0] > 0)
	    		    trustvars[2] = newtlen - details[6];
	    		    if (firsthook > 0) {
	    		    	firsthook = 0;
	    		    	double sxsn[][] = new double[sx.length][1];
	    		    	for (i = 0; i < sx.length; i++) {
	    		    		sxsn[i][0] = (sx[i]*sx[i])*sn[i];
	    		    	}
	    		    	Matrix matL = new Matrix(L);
	    		    	Matrix matSxsn = new Matrix(sxsn);
	    		    	tempvec = ((matL.inverse()).times(matSxsn)).getArray();
	    		        sum = 0.0;
	    		        for (i = 0; i < tempvec.length; i++) {
	    		        	sum += (tempvec[i][0] * tempvec[i][0]);
	    		        }
	    		    	phiprimeinit = -sum/newtlen;
	    		    }
	    		    mulow = -trustvars[2]/phiprimeinit;
	    		    muup = 0.0;
	    		    for (i = 0; i < g.length; i++) {
	    		    	div = g[i]/sx[i];
	    		    	muup += (div*div);
	    		    }
	    		    muup = Math.sqrt(muup);
	    		    muup = muup/details[6];
	    		    boolean done = false;
	    		    while (!done) {
	    		        if ((trustvars[0] < mulow) || (trustvars[0] > muup)) {
	    		            if (mulow < 0) {
	    		            	System.err.println("Warning, mulow < 0");
	    		            	Preferences.debug("Warning, mulow < 0\n", Preferences.DEBUG_ALGORITHM);
	    		            }
	    		            trustvars[0] = Math.max(Math.sqrt(mulow*muup), muup*1.0E-3);
	    		        } // if ((trustvars[0] < mulow) || (trustvars[0] > muup))
	    		        for (i = 0; i < sx.length; i++) {
	    		            sxdiag[i][i] = trustvars[0]*sx[i]*sx[i];	
	    		        }
	    		        for (i = 0; i < sx.length; i++) {
	    		        	for (j = 0; j < sx.length; j++) {
	    		        	    Hadd[i][j] = H[i][j] + sxdiag[i][j];	
	    		        	}
	    		        }
	    		        nechdcmp(L642, maxadd, Hadd, 0);
	    		        // L642 is a copy of L local to A6.4.2.
	    		        double g2[][] = new double[g.length][1];
	    		        for (i = 0; i < g.length; i++) {
	    		        	g2[i][0] = g[i];
	    		        }
	    		        Matrix matG = new Matrix(g2);
	    		        Matrix matL642 = new Matrix(L642);
	    		        Matrix matLInverse = matL642.inverse();
	    		        Matrix matLG = (matLInverse).times(matG);
	    		        Matrix matS = ((matL642.transpose()).inverse()).times(matLG);
	    		        double s2[][] = matS.getArray();
	    		        s = new double[s2.length];
	    		        for (i = 0; i < s.length; i++) {
	    		        	s[i] = -s2[i][0];
	    		        }
	    		        double steplen = 0.0;
	    		        for (i = 0; i < sx.length; i++) {
	    		        	prod = sx[i]* s[i];
	    		        	steplen += (prod*prod);
	    		        }
	    		        steplen = Math.sqrt(steplen);
	    		        trustvars[2] = steplen - details[6];
	    		        double sxs[][] = new double[sx.length][1];
	    		        for (i = 0; i < sx.length; i++) {
	    		        	sxs[i][0] = (sx[i]*sx[i])*s[i];
	    		        }
	    		        Matrix matSXS = new Matrix(sxs);
	    		        tempvec = (matLInverse.times(matSXS)).getArray();
	    		        sum = 0.0;
	    		        for (i = 0; i < tempvec.length; i++) {
	    		        	sum += (tempvec[i][0]*tempvec[i][0]);
	    		        }
	    		        trustvars[3] = -sum/steplen;
	    		        if (((steplen >= lo*details[6]) && (steplen <= hi*details[6])) || (muup - mulow <= 0) ||
	    		        		(muup == trustvars[0]) && (trustvars[2] < 0)) {
	    		        	done = true;
	    		        }
	    		        else {
	    		        	mulow = Math.max(mulow,  trustvars[0] - (trustvars[2]/trustvars[3]));
	    		        	if (trustvars[2] < 0) {
	    		        		muup = trustvars[0];
	    		        	}
	    		        	trustvars[0] = trustvars[0] - ((steplen/details[6]) * (trustvars[2]/trustvars[3]));
	    		        }
	    		    } // while (!done)
	    		} // else newtlen > hi * details[6] End of A6.4.2
	    		trustvars[1] = details[6]; // trustvars[1] is deltaprev
	    		netrust(xp, fp, Fp, maxtaken, retcode, xpprev, fpprev, Fpprev, details, nofun, xc, fc, g, L, s, sx, sf,
	    				newttaken, 1, H, umflag, fparam);
	    	} // while (retcode[0] >= 2)
	    } // nehook
	    
	    private void netrust(double xp[], double fp[], double Fp[], boolean maxtaken[], int retcode[], double xpprev[],
	    		double fpprev[], double Fpprev[], double details[], int nofun[], double xc[], double fc[], double g[], 
	    		double L[][], double s[], double sx[], double sf[], boolean newttaken, int steptype, double H[][], boolean umflag,
	    		double fparam[]) {
	    	// input/output retcode, xpprev, fpprev, Fpprev, details, nofun
	    	// output xp, fp, Fp, maxtaken
	    	// input xc, fc, g, L, s, sx, sf, newttaken, steptype, H, umflag, fparam
	    	// This routine is part of the Nonlinear Equations package and the Unconstrained Minimization package.
	    	// It decides whether or not the proposed step is acceptable, and adjusts the trust radius accordingly.
	    	// Algorithm A6.4.5: Part of the modular software system from the appendix of the book "Numerical Methods
	    	// for Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, August, 1990.
	    	int i;
	    	int j;
	    	double prod;
	    	double div;
	    	double temp;
	    	
	    	// Initialization
	    	int n = xc.length;
	    	for (i = 0; i < n; i++) {
	    		xp[i] = 0;
	    	}
	    	fp[0] = 0;
	    	
	    	// Algorithm steps 1-4.
	    	maxtaken[0] = false;
	    	double alpha = 1.0E-4;
	    	double steplen = 0.0;
	    	for (i = 0; i < sx.length; i++) {
	    		prod = sx[i] * s[i];
	    		steplen += (prod * prod);
	    	}
	    	steplen = Math.sqrt(steplen);
	    	for (i = 0; i < n; i++) {
	    		xp[i] = xc[i] + s[i];
	    	}
	    	
	    	// Algorithm step 5.
	    	if (umflag) {
	    		if (testMode) {
	    			fitToTestFunction(fp, xp, fparam);	
	    		}
	    		else {
	    		    fitToFunction(fp, xp, fparam);
	    		}
	    		nofun[0]++;
	    	}
	    	else {
	    		nefn(fp, Fp, nofun, xp, sf, fparam);
	    	}
	    	
	    	// Algorithm steps 6-8.
	    	double deltaf = fp[0] - fc[0];
	    	
	    	double initslope = 0.0;
	    	for (i = 0; i < g.length; i++) {
	    		initslope += (g[i] * s[i]);
	    	}
	    	if (retcode[0] != 3) {
	    		fpprev[0] = 0;
	    	}
	    	
	    	// Algorithm step 9.
	    	if ((retcode[0] == 3) && ((fp[0] >= fpprev[0]) || (deltaf > alpha * initslope))) { // step 9a.
	    		retcode[0] = 0;
	    		for (i = 0; i < xp.length; i++) {
	    			xp[i] = xpprev[i];
	    		}
	    		fp[0] = fpprev[0];
	    		for (i = 0; i < Fp.length; i++) {
	    			Fp[i] = Fpprev[i];
	    		}
	    		details[6] = details[6]/2;
	    	} // if ((retcode[0] == 3) && ((fp[0] >= fpprev[0]) || (deltaf > alpha * initslope)))
	    	else {
	    	    if (deltaf >= alpha * initslope) { // step 9b.
	    	        double rellength = -Double.MAX_VALUE;
	    	        for (i = 0; i < s.length; i++) {
	    	        	div = Math.abs(s[i])/Math.max(Math.abs(xp[i]), 1.0/sx[i]);
	    	        	if (div > rellength) {
	    	        		rellength = div;
	    	        	}
	    	        } // for (i = 0; i < s.length; i++)
	    	        if (rellength < details[8]) { // details[8] is steptol
	    	        	retcode[0] = 1;
	    	        	for (i = 0; i < xp.length; i++) {
	    	        		xp[i] = xc[i];
	    	        	}
	    	        } // if (rellength < details[8])
	    	        else { // rellength >= details[8]
	    	            retcode[0] = 2;
	    	            double deltatemp = -initslope*steplen/(2*(deltaf-initslope));
	    	            if (deltatemp < 0.1 * details[6]) {
	    	            	details[6] = 0.1 * details[6];
	    	            }
	    	            else if (deltatemp > 0.5 * details[6]) {
	    	            	details[6] = 0.5 * details[6];
	    	            }
	    	            else {
	    	            	details[6] = deltatemp;
	    	            }
	    	        } // else rellength >= details[8]
	    	    } // if (deltaf >= alpha * initslope)
	    	    else { // step 9c.
	    	        double deltafpred = initslope;
	    	        if (steptype == 1) {
	    	        	for (i = 0; i < n; i++) {
	    	        		temp = 0.5 * H[i][i] * s[i] * s[i];
	    	        		for (j = i+1; j < n; j++) {
	    	        		    temp += H[i][j] * s[i] * s[j];    	
	    	        		}
	    	        		deltafpred = deltafpred + temp;
	    	        	} // for (i = 0; i < n; i++)
	    	        } // if (steptype == 1)
	    	        else {
	    	        	for (i = 0; i < n; i++) {
	    	        	    temp = 0.0;
	    	        	    for (j = i; j < n; j++) {
	    	        	    	temp += (L[j][i] * s[j]);
	    	        	    }
	    	        	    deltafpred = deltafpred + (temp * temp/2);
	    	        	} // for (i = 0; i < n; i++)
	    	        }
	    	        if ((retcode[0] != 2) && ((Math.abs(deltafpred-deltaf) <= 0.1*Math.abs(deltaf)) ||
	    	        		(deltaf <= initslope)) && (!newttaken) && (details[6] <= 0.99*details[10])) {
	    	        	retcode[0] = 3;
	    	        	for (i = 0; i < xp.length; i++) {
	    	        		xpprev[i] = xp[i];
	    	        	}
	    	        	fpprev[0] = fp[0];
	    	        	for (i = 0; i < Fp.length; i++) {
	    	        		Fpprev[i] = Fp[i];
	    	        	}
	    	        	details[6] = Math.min(2.0 * details[6], details[10]); // details[10] is maxstep
	    	        } // if ((retcode[0] != 2) && ((Math.abs(deltafpred-deltaf) <= 0.1*Math.abs(deltaf)) ||
	    	        else {
	    	        	retcode[0] = 0;
	    	        	if (steplen > 0.99 * details[10]) {
	    	        		maxtaken[0] = true;
	    	        	}
	    	        	if (deltaf >= 0.1 * deltafpred) {
	    	        		details[6] = details[6]/2;
	    	        	}
	    	        	else if (deltaf <= 0.75 * deltafpred) {
	    	        		details[6] = Math.min(2.0*details[6], details[10]);
	    	        	}
	    	        } // else
	    	    } // else step 9c.
	    	} // else
	    } // netrust
	    
	    private void nebroyuf(double A[][], double xc[], double xp[], double fc[], double fp[], double sx[], double eta) {
	        // input/output A
	    	// input xc, xp, fc, fp, sx, eta
	    	// This routine is part of the Nonlinear Equations package.
	    	// This updates A, a secant approximation to the jacobian, using Broyden's unfactored secant update.
	    	// Algorithm A8.3.1: Part of the modular software system from the appendix of the book "Numerical Methods
	    	// for Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Sherkat Masoum M., April, 1988.
	    	// Edited by Richard T. Behrens, June, 1988.
	    	int i;
	    	int j;
	    	double prod;
	    	double tempi;
	    	
	    	// Algorithm step 1.
	    	int n = Math.max(A.length, A[0].length);
	    	double s[] = new double[xp.length];
	    	for (i = 0; i < xp.length; i++) {
	    		s[i] = xp[i] - xc[i];
	    	}
	    	
	    	// Algorithm step 2.
	    	double denom = 0.0;
	    	for (i = 0; i < sx.length; i++) {
	    		prod = sx[i] * s[i];
	    		denom  += (prod * prod);
	    	}
	    	
	    	// Algorithm step 3.
	    	for (i = 0; i < n; i++) {
	    	    tempi = fp[i] - fc[i];
	    	    for (j = 0; j < n; j++) {
	    	    	tempi = tempi - (A[i][j] * s[j]);
	    	    }
	    	    if (Math.abs(tempi) >= eta * (Math.abs(fp[i]) + Math.abs(fc[i]))) {
	    	        tempi = tempi/denom;
	    	        for (j = 0; j < n; j++) {
	    	        	A[i][j] = A[i][j] + tempi * s[j] * sx[j] * sx[j];
	    	        }
	    	    }
	    	} // for (i = 0; i < n; i++)
	    	return;
	    } // nebroyuf
	    
	    private void nestop(int consecmax[], int termcode[], double xc[], double xp[], double F[], double Fnorm[], double g[],
	    		double sx[], double sf[], int retcode, double details[], int itncount, boolean maxtaken) {
	    	// input/ output consecmax
	    	// output termcode
	    	// input xc, xp, F, Fnorm, g, sx, sf, retcode, details, itncount, maxtaken
	    	// This routine is part of the Nonlinear Equations package.
	    	// It decides whether or not to stop iterating when solving a set of nonlinear equations.
	    	// Algorithm A7.2.3: Part of the modular software system from the appendix of the book "Numerical Methods for
	    	// Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Richard T. Behrens, March, 1988.
	    	int i;
	    	double term;
	    	double maxProd;
	    	double maxDiv;
	    	
	    	// Algorithm step 1.
	    	int n = xc.length;
	    	termcode[0] = 0;
	    	
	    	// Algorithm step 2.
	    	maxProd = -Double.MAX_VALUE;
	    	for (i = 0; i < sf.length; i++) {
	    		term = sf[i] * Math.abs(F[i]);
	    		if (term > maxProd) {
	    			maxProd = term;
	    		}
	    	}
	    	maxDiv = -Double.MAX_VALUE;
	    	for (i = 0; i < n; i++) {
	    		term = Math.abs(xp[i] - xc[i]) / Math.max(Math.abs(xp[i]), 1.0/sx[i]);
	    		if (term > maxDiv) {
	    			maxDiv = term;
	    		}
	    	}
	    	if (retcode == 1) {
	    		termcode[0] = 3;
	    	}
	    	else if (maxProd <= details[7]) {
	    		termcode[0] = 1;
	    	}
	    	else if (maxDiv <= details[8]) {
	    		termcode[0] = 2;
	    	}
	    	else if (itncount >= details[5]) {
	    		termcode[0] = 4;
	    	}
	    	else if (maxtaken) {
	    		consecmax[0]++;
	    		if (consecmax[0] == 5) {
	    			termcode[0] = 5;
	    		}
	    	} // else if (maxtaken)
	    	else {
	    		consecmax[0] = 0;
	    		if ((details[3] > 0) || (details[2] > 0)) {
	    		    maxProd = -Double.MAX_VALUE;
	    		    for (i = 0; i < n; i++) {
	    		    	term = Math.abs(g[i]) * Math.max(Math.abs(xp[i]), 1.0/sx[i])/Math.max(Fnorm[0], n/2.0);
	    		    	if (term > maxProd) {
	    		    		maxProd = term;
	    		    	}
	    		    }
	    		    if (maxProd <= details[9]) {
	    		    	termcode[0] = 6;
	    		    }
	    		} // if ((details[3] > 0) || (details[2] > 0))
	    	} // else
	    } // nestop
    }