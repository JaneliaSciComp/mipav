package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation.
 * 
  * AAM-API LICENSE  -  file: license.txt
 * 
 * This software is freely available for non-commercial use such as
 * research and education. Please see the full disclaimer below. 
 * 
 * All publications describing work using this software should cite 
 * the reference given below. 
 * 	
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling
 * DTU, Technical University of Denmark
 * Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where 
 * the AAM-API has been used. A draft version the article is available from 
 * the homepage. 
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * -------------
 * M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance 
 * Modelling Environment", IEEE Transactions on Medical Imaging, IEEE, 2003
 * (to appear)
 * -------------
 * 
 *
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK
 * - LAPACK
 * 
 *
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the author be held liable for any damages arising from the
 * use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial 
 * purpose, and to alter it, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim
 *  that you wrote the original software. 
 *
 * 2. Altered source versions must be plainly marked as such, and must not be 
 *  misrepresented as being the original software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 *
 * No guarantees of performance accompany this software, nor is any 
 * responsibility assumed on the part of the author or IMM. 
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any 
 * express or implied warranties, including, but not limited to, the implied 
 * warranties of merchantability and fitness for a particular purpose are 
 * disclaimed. In no event shall IMM or Mikkel B. Stegmann be liable for any 
 * direct, indirect, incidental, special, exemplary, or consequential damages
 * (including, but not limited to, procurement of substitute goods or services;
 * loss of use, data, or profits; or business interruption) however caused and 
 * on any theory of liability, whether in contract, strict liability, or tort 
 * (including negligence or otherwise) arising in any way out of the use of 
 * this software, even if advised of the possibility of such damage.
 * 
 * 
 * 
 *
 * $Revision: 1.4 $ 
 * $Date: 2003/04/23 14:49:15 $ 
 * 
 * 
 * Optimization base class. This abstract class is the base class for all
 * classes implementing specific optimization procedures. 
 * author: Rune Fisker, 26/1-1999
 * 
 * @author Ruida Cheng
 */
public abstract class CDOptimizeBase {

	// methods for estimating the gradient nummericaly
	public int ENumGrad;

	public static int ForwardDifference = 0;
	public static int CentralDifference = 1;
	public static int FitLine = 2;

	// optimization methods
	public int EOptMethod;
	public static int eoptUnknown = 0;
	public static int eoptSteepestDescent = 1;
	public static int eoptConjugateGradient = 2;
	public static int eoptBFGS = 3;
	public static int eoptSimulatedAnnealing = 4;
	public static int eoptPatternSearch = 5;

	// termination code
	public int ETermCode;
	public static int etermNoStop = 0; // no stop
	public static int etermGradTol = 1; // abs max gradient defined as max_i(
										// abs( g[i]*xplus[i]/f(xplus) )) <
										// m_dGradTol
	public static int etermStepTol = 2; // step tolerance defined as max_i( abs(
										// (xplus[i] - x[i])/xplus[i] )) <
										// m_dStepTol
	public static int etermLineSearch = 4; // termination from line search
	public static int etermConsecMaxStepMax = 8; // max number of conseccutive
													// past steps whose scaled
													// length was equal to
													// maxstep
	public static int etermDeltaFuncVal = 16; // change in function value
												// defined as (abs(f - fplus) <
												// m_dDeltaFuncVal)
	public static int etermMaxFuncEval = 32; // max number of function
												// evaluations
	public static int etermMaxIterations = 64; // max iterations
	public static int etermUnknown = 128; // unknow stop

	/**
	 * the Minimize function using analytic gradient
	 * 
	 * @param x
	 * @param pFuncEvalBase
	 * @return
	 */
	public abstract int Minimize(CDVector x, CDOptimizeFuncBase pFuncEvalBase);

	/**
	 * name of optimization methode
	 * 
	 * @return
	 */
	public String Name() {
		return "Unknown method";
	}

	/** name of optimization method */
	public int OptMethod() {
		return eoptUnknown;
	}

	/**
	 * function and gradient evaluation methods
	 * 
	 * @param x
	 * @return
	 */
	public double EvalFunction(CDVector x) {
		assert (m_pFuncEvalBase != null);
		m_nFuncEval++;
		return m_pFuncEvalBase.EvalFunction(x);
	}

	/**
	 * function and gradient evaluation methods
	 * 
	 * @param x
	 * @param gc
	 * @param dFuncVal
	 */
	public void EvalGradient(CDVector x, CDVector gc, final double dFuncVal) {
		assert (m_pFuncEvalBase != null);
		if (m_fAnalyticGrad)
			m_pFuncEvalBase.EvalGradient(x, gc);
		else
			NumGrad(x, gc, dFuncVal);
		m_nGradEval++;
	}

	/**
	 * get numerical gradient function
	 * 
	 * @return
	 */
	public CDVector MethodPar() {
		return m_vMethodPar;
	}

	/**
	 * set numerical gradient function
	 * 
	 * @param vMethodPar
	 */
	public void SetMethodPar(final CDVector vMethodPar) {
		m_vMethodPar = vMethodPar;
	}

	/**
	 * get limit for the number of iterations
	 * 
	 * @return
	 */
	public int MaxIterations() {
		return m_nMaxIterations;
	}

	/**
	 * get the number of evaluations
	 * 
	 * @return
	 */
	public int MaxFuncEval() {
		return m_nMaxFuncEval;
	}

	/**
	 * set point to func eval. base.
	 * 
	 * @param pFuncEval
	 */
	public void SetFuncEvalBase(CDOptimizeFuncBase pFuncEval) {
		m_pFuncEvalBase = pFuncEval;
	}

	/**
	 * get point to func eval. base.
	 * 
	 * @return
	 */
	public CDOptimizeFuncBase GetFuncEvalBase() {
		return m_pFuncEvalBase;
	}

	/** Machine precision */
	public double m_dMachEps;

	/**
	 * positive scalar estimating the magnitude of f(x) near he minimizer
	 * x-star. It is only used in the gradient stopping condition given below.
	 * typf should be approximately |f(x*)|
	 */
	public double m_dTypF;

	/**
	 * A positive integer specifying the number of reliable digits returned by
	 * the objective function FN. fdigits is used to set the parameter n (eta)
	 * that is used in the code to specify the relative noise in f(x); the main
	 * use of eta is in calculation finite difference step size. eta is set to
	 * macheps if fdigits = -1. If f(x) is suspected to be noisy but the
	 * approximate value of fdigits is unknown, it should be estimated be the
	 * routine of Hamming[1973] given in Gill, Murray and Wright[1981]
	 */
	public int m_nFDigits;

	/**
	 * A positive scalar giving the maximum allowable scaled steplength at any
	 * iteration. maxstep is used to prevent steps that would cause the
	 * optimazation algorithm to overflow or leave the domain of interest, as
	 * well as to detect divergence. It should be chosen small enough to prevent
	 * the first two of these occurrences but larger than any anticipated
	 * reasonable stepsize. The algorithm will halt if it takes steps of length
	 * maxstep on m_nConsecMaxStepMax conseccutive iterations
	 */
	public double m_dMaxStep;

	/**
	 * Number of conseccutive past steps whose scaled length was equal to
	 * maxstep
	 */
	public int m_nConsecMax;

	/** machine precision. */
	public double m_dEta;

	/**
	 * flag indicating if the gradient is to be calc. analyticaly or nummericaly
	 */
	public boolean m_fAnalyticGrad;

	/** methods for estimating the gradient nummericaly */
	public int m_eNumGrad;

	/**
	 * holds the stop criteria in use, e.g m_iStopCriteria = etermMaxFuncEval |
	 * etermMaxIterations
	 */
	public int m_iStopCriteria;

	/**
	 * A positive scalar giving the tolerance at which the scaled gradient in
	 * considered close enough to zero to terminate the algorithm
	 */
	public double m_dGradTol;

	/**
	 * A positive scalar giving the tolerance at which the scaled distance
	 * between two successive iterated is considered close enough to zero to
	 * terminate the algorithm
	 */
	public double m_dStepTol;

	/**
	 * max number of conseccutive past steps whose scaled length was equal to
	 * maxstep to terminate
	 */
	public int m_nConsecMaxStepMax;

	/** value for stop criterion: abs(f - fplus) < m_dDeltaFuncVal */
	public double m_dDeltaFuncVal;

	/** counter to the number of iterations. */
	public int m_nIterations;

	/** counter to the number of function evaluations. */
	public int m_nFuncEval;

	/** counter to the number of gradient evaluations. */
	public int m_nGradEval;

	/**
	 * Logical variable which determines whether function parameters and
	 * corresponding return values should be stored for later analysis.
	 */
	public boolean m_fLogFuncValues;

	// vectors for saving the FuncVal, parameter values and
	// the corresponding number of func evaluations
	public CDVector m_vFuncVal = new CDVector();
	public CDVector m_vNFuncEval = new CDVector();
	public Vector<CDVector> m_vvFuncParm = new Vector<CDVector>();

	/** pointer to the function to be minimized. */
	private CDOptimizeFuncBase m_pFuncEvalBase;

	/**
	 * special parameters used by each optimization method e.g. step size used
	 * for calc. nummerical gradient
	 */
	private CDVector m_vMethodPar = new CDVector();

	/**
	 * A positive integer specifying the maximum number of iterations that may
	 * be performed before the algorithm is halted. Appropriate values depend
	 * strongly on the dimension and difficulty of the problem, and the cost of
	 * evaluating the nonlinear function.
	 */
	private int m_nMaxIterations;

	/** limit for number of function evaluations. */
	private int m_nMaxFuncEval;

	/**
	 * Constructor
	 */
	public CDOptimizeBase() {
		m_nMaxFuncEval = 1; // need to be assign a value before call to
							// SetMaxIterarions() or SetMaxFuncEval()
		m_nMaxIterations = 1; // need to be assign a value before call to
								// SetMaxIterarions() or SetMaxFuncEval()

		// set iteration and func eval. pars
		SetMaxFuncEval(300);
		SetMaxIterations(300);
		m_nFuncEval = 0; // counter to the number of function evaluations
		m_nGradEval = 0; // counter to the number of function evaluations

		// set other pars
		SetMachineEps(); // set eps
		m_nFDigits = -1;

		m_dGradTol = Math.pow(m_dMachEps, 1.0 / 3.0);
		m_dStepTol = Math.pow(m_dMachEps, 2.0 / 3.0);
		m_nConsecMaxStepMax = 5;
		m_iStopCriteria = etermMaxFuncEval | etermMaxIterations
				| etermLineSearch | etermDeltaFuncVal;

		m_nIterations = 0;
		m_nConsecMax = 0;
		m_dDeltaFuncVal = 0.0001;
		m_dMaxStep = 1e3;

		m_fAnalyticGrad = true;

		m_eNumGrad = CentralDifference;
		// m_eNumGrad = FitLine;
		// m_eNumGrad = ForwardDifference;

		m_fLogFuncValues = true; // Store function evaluation data

		if (m_nFDigits < 0) {
			m_dEta = m_dMachEps;
		} else {
			m_dEta = Math.max(m_dMachEps, Math.pow(10, -m_nFDigits));
		}
	}

	/**
	 * set limit for the number of iterations
	 * 
	 * @param nMaxIterations
	 *            max iterations
	 */
	public void SetMaxIterations(final int nMaxIterations) {
		m_nMaxIterations = nMaxIterations;

		int nSize = Math.max(m_nMaxFuncEval, m_nMaxIterations);

		m_vFuncVal.Resize(nSize + 1);
		m_vNFuncEval.Resize(nSize + 1);

	}

	/**
	 * set limit for the number of function evaluations
	 * 
	 * @param nMaxFuncEval
	 *            max number of evaluations.
	 */
	public void SetMaxFuncEval(final int nMaxFuncEval) {
		m_nMaxFuncEval = nMaxFuncEval;

		int nSize = Math.max(m_nMaxFuncEval, m_nMaxIterations);

		m_vFuncVal.Resize(nSize + 1);
		m_vNFuncEval.Resize(nSize + 1);
	}

	/**
	 * ----------------------------[ MachineEps ]----------------------------
	 * Calculate machine epsilon
	 * 
	 * Algorithm A1.3.1 - p. 303 Dennis and Schnabel, Numerical Methods for
	 * Unconstrained Optimization and Nonlinear Equations 1983, Prentice-Hall
	 */
	public void SetMachineEps() {
		double macheps = 1.0;

		do {
			macheps /= 2.0;
		} while (1.0 + macheps > 1.0);

		m_dMachEps = 2.0 * macheps;
	}

	/*
	 * Decide wether to terminate minimization at iteration zero
	 * 
	 * Algorithm A7.2.1 p. 348 Dennis and Schnabel, Numerical Methods for
	 * Unconstrained Optimization and Nonlinear Equations 1983, Prentice-Hall
	 * 
	 * intput: x0: parameter functionValue: function value gradient: gradient
	 * 
	 * output: return termination code
	 */
	public int UmStop0(CDVector x0, final double functionValue,
			CDVector gradient) {
		int eTermCode = etermNoStop;
		double maxScaledLength = 0;
		double tmpMaxScaledLength;
		int i;

		for (i = x0.Length() - 1; i >= 0; --i) {
			tmpMaxScaledLength = Math.abs(gradient.get(i))
					* Math.max(Math.abs(x0.get(i)), 1.0)
					/ Math.max(Math.abs(functionValue), m_dTypF);

			maxScaledLength = Math.max(maxScaledLength, tmpMaxScaledLength);
		}

		if (maxScaledLength <= 1e-3 * m_dGradTol) {
			eTermCode = etermGradTol;
		}

		return eTermCode;

	}

	/**
	 * Decide wether to terminate minimization
	 * 
	 * Modified version of Algorithm A7.2.1 p. 347 Dennis and Schnabel,
	 * Numerical Methods for Unconstrained Optimization and Nonlinear Equations
	 * 1983, Prentice-Hall
	 * 
	 * Input: x: parameter xplus: new parameter fplus: function value for new
	 * parameter g: gradient at x retcode: return code from line search
	 * maxtaken: max taken in line search
	 * 
	 * Output: return termination code
	 */
	public int UmStop(final CDVector x, final CDVector xplus, final double f,
			final double fplus, final CDVector g, final int retcode,
			final boolean maxtaken) {
		if (m_iStopCriteria == 0) {
			System.err.println("No stop criteria enabled!!" + "UmStop");
		}

		int eTermCode = etermNoStop;

		if (m_iStopCriteria == etermLineSearch) {
			if (retcode == 1) {
				eTermCode = etermLineSearch;
			}
		}

		if ((m_iStopCriteria == etermGradTol) && (eTermCode == etermNoStop)) {
			double absMaxGrad = 0.0;
			for (int i = x.Length() - 1; i >= 0; --i) {
				absMaxGrad = Math.max(absMaxGrad,
						Math.abs(g.m_data[i] * xplus.m_data[i] / fplus));
			}

			if (absMaxGrad <= m_dGradTol) {
				eTermCode = etermGradTol;
			}

		}

		if ((m_iStopCriteria == etermStepTol) && (eTermCode == etermNoStop)) {
			double absMaxStep = 0.0;
			for (int i = x.Length() - 1; i >= 0; --i) {
				absMaxStep = Math.max(
						absMaxStep,
						Math.abs((xplus.m_data[i] - x.m_data[i])
								/ xplus.m_data[i]));
			}

			if (absMaxStep <= m_dStepTol) {
				eTermCode = etermStepTol;
			}
		}

		if ((m_iStopCriteria == etermMaxIterations)
				&& (eTermCode == etermNoStop)) {
			if (m_nIterations >= MaxIterations()) {
				eTermCode = etermMaxIterations;
			}
		}

		if ((m_iStopCriteria == etermDeltaFuncVal)
				&& (eTermCode == etermNoStop)) {
			if (Math.abs(f - fplus) < m_dDeltaFuncVal) {
				eTermCode = etermDeltaFuncVal;
			}
		}

		if ((m_iStopCriteria == etermMaxFuncEval) && (eTermCode == etermNoStop)) {
			if (m_nFuncEval >= MaxFuncEval()) {
				eTermCode = etermMaxFuncEval;
			}
		}

		if ((m_iStopCriteria == etermConsecMaxStepMax)
				&& (eTermCode == etermNoStop)) {
			if (maxtaken) {
				++m_nConsecMax;
				if (m_nConsecMax >= m_nConsecMaxStepMax) {
					eTermCode = etermConsecMaxStepMax;
				}
			} else {
				m_nConsecMax = 0;
			}
		}

		return eTermCode;
	}

	/**
	 * Warpper for line search
	 * 
	 * @param xc
	 *            parameter
	 * @param fc
	 *            function value at xc
	 * @param g
	 *            gradient at xc
	 * @param p
	 *            search direction
	 * @param xplus
	 *            new parameter
	 * @param fplus
	 *            function value for new parameter
	 * @param maxtaken
	 *            max taken in line search
	 * @return termination code
	 */
	public int LineSearch(final CDVector xc, final double fc, final CDVector g,
			final CDVector p, CDVector xplus, double[] fplus, boolean[] maxtaken) {
		boolean fSoft = true;
		return LineSearch(xc, fc, g, p, xplus, fplus, maxtaken, fSoft);
	}

	/*
	 * Perform line search
	 * 
	 * Input: xc: parameter fc: function value at xc g: gradient at xc p: search
	 * direction xplus: new parameter fplus: function value for new parameter
	 * 
	 * Output: maxtaken: max taken in line search return termination code
	 */
	public int LineSearch(final CDVector xc, final double fc, final CDVector g,
			final CDVector p, CDVector xplus, double[] fplus,
			boolean[] maxtaken, boolean fSoft) {
		if (fSoft) {
			return SoftLineSearch(xc, fc, g, p, xplus, fplus, maxtaken);
		} else {
			return ExactLineSearch(xc, fc, g, p, xplus, fplus, maxtaken);
		}
	}

	/**
	 * Perform exact line search
	 * 
	 * Input: xc: parameter fc: function value at xc g: gradient at xc p: search
	 * direction
	 * 
	 * Output: xplus: new parameter fplus: function value for new parameter
	 * maxtaken: max taken in line search return termination code
	 */
	public int ExactLineSearch(final CDVector xc, final double fc,
			final CDVector g, final CDVector p, CDVector xplus, double[] fplus,
			boolean[] maxtaken) {
		double dLambda, dLambdaMax = 0.5, dLambdaStep = 0.05, dFTmp;

		CDVector vXTmp = new CDVector(xc.Length());

		fplus[0] = fc;
		xplus.assign(xc);

		for (dLambda = dLambdaStep; dLambda <= dLambdaMax; dLambda += dLambdaStep) {
			CVisDVector tmp = p.mult(dLambda);
			vXTmp.assign(xc.add(tmp));

			dFTmp = EvalFunction(vXTmp);

			if (dFTmp < fplus[0]) {
				fplus[0] = dFTmp;
				xplus.assign(vXTmp);
				maxtaken[0] = true;
			}
		}

		return 0;
	}

	/**
	 * Perform line search
	 * 
	 * Given g'p < 0 and alpha < 1/2 (alpha = 1e-4 is used), find plus = xc +
	 * lambda p,lambda in [0;1], such that f(xplus) <= f(xc) + alpha * lambda *
	 * g'p, using backtracking line search
	 * 
	 * Algorithm A6.3.1 p. 325 Dennis and Schnabel, Numerical Methods for
	 * Unconstrained Optimization and Nonlinear Equations 1983, Prentice-Hall
	 * 
	 * Input: xc: parameter fc: function value at xc g: gradient at xc sn:
	 * search direction
	 * 
	 * Output: xplus: new parameter fplus: function value for new parameter
	 * maxtaken: max taken in line search return termination code
	 */
	public int SoftLineSearch(final CDVector xc, final double fc,
			final CDVector g, final CDVector sn, CDVector xplus,
			double[] fplus, boolean[] maxtaken) {
		// create a copy of the search dir., which is normalized later
		CDVector p = new CDVector(sn.Length());
		p.assign(sn);

		int retcode; /*
					 * retcode = 0: satisfactory xplus found retcode = 1:
					 * routine failed to locate satisfactory xplus sufficiently
					 * distinct from x
					 */

		double alpha = 1e-4;
		double initslope = g.mult(p); // initslope = g' dot p
		int i;
		double rellength; // Relative length of p as calculated in the stopping
							// routine
		double minlambda; // minlambda is the minimum allowable steplength
		double lambda, lambdaPrev = 0, lambdaTemp;
		double a, b, disc;
		double fplusPrev = 0;

		maxtaken[0] = false;
		retcode = 2; // initial value to keep the do..while loop 'alive'

		// definition of dnrm2 found in /usr/include/cblas.h
		// Newtlen = dnrm2(p.Size(),p.Addr(),1); // calculates 2-norm of the
		// vector p;

		// calculates 2-norm of the vector p, i.e. Norm2 := sqrt( x'*x )
		double Newtlen = Math.sqrt(p.mult(p));

		if (Newtlen > m_dMaxStep) {
			p.mult_into(m_dMaxStep / Newtlen);
			Newtlen = m_dMaxStep;
		}

		// initslope = g*p; - allready done
		rellength = 0.0;
		for (i = xc.Length() - 1; i >= 0; --i) {
			rellength = Math.max(
					rellength,
					Math.abs(p.m_data[i])
							/ Math.max(Math.abs(xc.m_data[i]), 1.0));
		}

		minlambda = m_dStepTol / rellength;

		lambda = 1.0;
		boolean fExact = false, fTerminate = false;
		double dSlopeLambda;

		do {
			xplus.assign(xc.add(p.mult(lambda)));
			// fplus = (*m_pFN)(xplus);
			fplus[0] = EvalFunction(xplus);

			if (fExact) {
				// dos not very well
				CDVector vGradPlus = new CDVector(xplus.Length());

				EvalGradient(xplus, vGradPlus, fplus[0]);

				dSlopeLambda = vGradPlus.mult(p);

				fTerminate = (Math.abs(dSlopeLambda) < -0.25 * initslope)
						&& (fplus[0] <= fc);
			} else {
				fTerminate = (fplus[0] <= fc + alpha * lambda * initslope);
			}

			// if (fplus <= fc + alpha * lambda * initslope)
			if (fTerminate) {
				// satisfactory xplus found
				retcode = 0;
				if (lambda == 1.0 && Newtlen > 0.99 * m_dMaxStep) {
					maxtaken[0] = true;
				}
			} else {
				if (lambda < minlambda) {
					// No satisfactory xplus can be found sufficiently distinct
					// from x
					retcode = 1;
					xplus.assign(xc);
				} else { // reduce lambda
					if (lambda == 1.0) {
						// first backtrack, quadratic fit
						lambdaTemp = -initslope
								/ (2.0 * (fplus[0] - fc - initslope));
					} else {
						// All subsequent backtracks, cubic fit
						//
						// a & b is found in Mathematica (notebook named
						// LineSeach.nb)
						//
						a = (-(fplusPrev * Math.pow(lambda, 2))
								+ lambdaPrev
								* (initslope * Math.pow(lambda, 2) + fplus[0]
										* lambdaPrev - initslope * lambda
										* lambdaPrev) + fc
								* (Math.pow(lambda, 2) - Math
										.pow(lambdaPrev, 2)))
								/ (Math.pow(lambda, 2) * (lambda - lambdaPrev) * Math
										.pow(lambdaPrev, 2));

						b = (fplusPrev * Math.pow(lambda, 3) - initslope
								* Math.pow(lambda, 3) * lambdaPrev - fplus[0]
								* Math.pow(lambdaPrev, 3) + initslope * lambda
								* Math.pow(lambdaPrev, 3) + fc
								* (-Math.pow(lambda, 3) + Math.pow(lambdaPrev,
										3)))
								/ (Math.pow(lambda, 2) * (lambda - lambdaPrev) * Math
										.pow(lambdaPrev, 2));

						disc = b * b - 3.0 * a * initslope;

						if (a == 0.0) {
							// cubic is quadratic
							lambdaTemp = -initslope / (2.0 * b);
						} else {
							// legitimate cubic
							lambdaTemp = (-b + Math.sqrt(disc)) / (3.0 * a);
						}

						if (lambdaTemp > 0.5 * lambda) {
							lambdaTemp = 0.5 * lambda;
						}
					}

					lambdaPrev = lambda;
					fplusPrev = fplus[0];

					if (lambdaTemp <= 0.1 * lambda) {
						lambda = 0.1 * lambda;
					} else {
						lambda = lambdaTemp;
					}
				}
			}
		} while (retcode == 2);

		return retcode;
	}

	/**
	 * calculate nummerical gradient function usingdecided gradient calculation
	 * method
	 * 
	 * input: x: parameter dFuncVal: function value in x
	 * 
	 * output: gradient: gradient
	 * 
	 */
	public void NumGrad(CDVector x, CDVector gradient, final double dFuncVal) {
		// calculate gradient using forward difference
		if (m_eNumGrad == ForwardDifference) {
			double dPlus;
			CDVector vecTemp = new CDVector(x.Length());

			for (int i = 0; i < x.Length(); i++) {
				vecTemp.assign(x);

				vecTemp.m_data[i] += m_vMethodPar.m_data[i];
				dPlus = EvalFunction(vecTemp);

				gradient.m_data[i] = (dPlus - dFuncVal)
						/ m_vMethodPar.m_data[i];
			}
		}
		// calculate gradient using central difference
		else if (m_eNumGrad == CentralDifference) {
			double dPlus, dMinus;
			CDVector vecTemp = new CDVector(x.Length());

			for (int i = 0; i < x.Length(); i++) {
				vecTemp.assign(x);

				vecTemp.m_data[i] -= m_vMethodPar.m_data[i];
				dMinus = EvalFunction(vecTemp);

				vecTemp.m_data[i] += 2 * m_vMethodPar.m_data[i];
				dPlus = EvalFunction(vecTemp);

				gradient.m_data[i] = (dPlus - dMinus)
						/ (2 * m_vMethodPar.m_data[i]);
			}
		}
		// calculate gradient by fitting a line throu a number of function
		// values.
		// The gradient is then found as the slope. This is equavilent to
		// fitting
		// a parabel and differentated it.
		else if (m_eNumGrad == FitLine) {
			int nFitPoints = 5; // 3 points equals central difference
			CDVector vTemp = new CDVector(x.Length());

			CDVector vY = new CDVector(nFitPoints);
			CDVector vTheta = new CDVector(2);
			CDMatrix mA = new CDMatrix(nFitPoints, 2);
			mA.assign(1);

			CDMatrix tempMatrix = new CDMatrix(nFitPoints, 2);

			// estimate the gradient by fitting line to each parameter dimension
			for (int i = 0; i < x.Length(); i++) {
				CDVector vX = new CDVector(nFitPoints);
				vTemp.assign(x);

				// calc. function vaules for differet setting in one parameter
				// dim.
				int iFP = 0;
				for (int j = -nFitPoints / 2; j <= nFitPoints / 2; j++) {
					vX.m_data[iFP] = x.m_data[i] - j * m_vMethodPar.m_data[i];
					vTemp.m_data[i] = vX.m_data[iFP];
					vY.m_data[iFP++] = EvalFunction(vTemp);

					// do not calc. func. value equal to j = 0 again
					if (j == -1) {
						j++;
						vX.m_data[iFP] = x.m_data[i] - j
								* m_vMethodPar.m_data[i];
						vY.m_data[iFP++] = dFuncVal;
					}
				}

				// find LSQ solution (the fit of a line)
				mA.SetColumn(1, vX);

				tempMatrix.VisDMatrixLeastSquares(mA, vY, vTheta);

				gradient.m_data[i] = vTheta.m_data[1];
			}
		}
	}

	/*
	 * the Minimize function using nummerical gradient
	 * 
	 * input: x: parameter dFuncVal: function value in x
	 * 
	 * output: gradient: gradient
	 */
	public int MinimizeNum(CDVector x, CDOptimizeFuncBase pFuncEvalBase,
			CDVector vMethodPar) {
		// set gradient to nummerical
		m_fAnalyticGrad = false;

		// set the method par
		if (vMethodPar.Length() != x.Length()) {
			System.err
					.println("The number of parameters to optimize and the number of method parameters should be the same."
							+ "MinimizeNum");
		} else {
			m_vMethodPar.Resize(x.Length());
			m_vMethodPar.assign(vMethodPar);
		}

		// set function to minized
		SetFuncEvalBase(pFuncEvalBase);

		// minimize using NumGrad to calc. gradient
		return Minimize(x, pFuncEvalBase);
	}

}