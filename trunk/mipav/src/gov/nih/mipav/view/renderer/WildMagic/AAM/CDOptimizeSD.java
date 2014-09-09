package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation.
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B. Stegmann IMM, Informatics &
 * Mathmatical Modelling Technical University of Denmark Richard Petersens Plads
 * Building 321 DK-2800 Lyngby, Denmark http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * Copyright © 2000
 * 
 * DTU Image Viever and Analyser (DIVA) Department of Mathematical Modelling
 * Technical University of Denmark (DTU), Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/~diva
 * 
 * author: Rune Fisker
 * 
 * This file contains the implementation of the steepest descent optimization
 * algorithm. Steepest descent is implemented with soft line search. See e.g.
 * Dennis and Schnabel, Numerical Methods for Unconstrained Optimization and
 * Nonlinear Equations 1983, Prentice-Hall for a description of steepest
 * descent.
 * 
 * @author Ruida Cheng
 * 
 */
public class CDOptimizeSD extends CDOptimizeBase {

	/**
	 * constructor
	 */
	public CDOptimizeSD() {
		super();
	}

	/**
	 * Dispose memory.
	 */
	public void dispose() {

	}

	/**
	 * name of optimization method
	 */
	public String Name() {
		return "Steepest Descent";
	}

	/**
	 * type of optimization method.
	 */
	public int OptMethod() {
		return eoptSteepestDescent;
	}

	/**
	 * Steepest decent minimization.
	 * 
	 * Input: start point for x Output in x: x* - optimal point
	 */
	public int Minimize(CDVector x, CDOptimizeFuncBase pFuncEvalBase) {
		// set the optimization function pointer
		SetFuncEvalBase(pFuncEvalBase);

		// initialize counters and vector
		m_nFuncEval = 0;
		m_nGradEval = 0;
		m_vNFuncEval.assign(-1);
		m_vvFuncParm.clear();

		// eval func.
		double fc = EvalFunction(x);

		// and gradient
		CDVector gc = new CDVector(x.Length());
		EvalGradient(x, gc, fc);

		int eTermCode = UmStop0(x, fc, gc);

		if (eTermCode != etermNoStop) {
			System.err.println("Minimum at initial position");
		}

		double[] fplus = new double[1];
		CDVector xplus = new CDVector(x.Length());
		CDVector gcOld = new CDVector(x.Length());
		CDVector gplus = new CDVector(x.Length());
		CDVector sn = new CDVector(x.Length());
		boolean[] maxtaken = new boolean[1];
		int retcode;

		// save func. value and number of func. values
		if (m_fLogFuncValues) {
			m_vFuncVal.m_data[0] = fc;
			m_vNFuncEval.m_data[0] = 0;
			m_vvFuncParm.add(x);
		}

		//
		// Iteration section
		//

		m_nIterations = 1;
		while (eTermCode == etermNoStop) {
			// set the decent direction equal to the negative gradient
			sn.assign(gc.neg());

			// perform line search
			retcode = LineSearch(x, fc, gc, sn, xplus, fplus, maxtaken);

			// calc. new gradient (if needed)
			if (retcode == 0) {
				EvalGradient(xplus, gplus, fplus[0]);
			}

			// stop ??
			eTermCode = UmStop(x, xplus, fc, fplus[0], gplus, retcode,
					maxtaken[0]);

			if (eTermCode > 0) {
				x.assign(xplus);

				// update
				pFuncEvalBase.Update(x);

			} else {
				x.assign(xplus);
				fc = fplus[0];
				gc.assign(gplus);

				// update
				pFuncEvalBase.Update(x);
			}

			// save func. value and number of func. values
			if (m_fLogFuncValues) {
				m_vFuncVal.m_data[m_nIterations] = fc;
				m_vNFuncEval.m_data[m_nIterations] = m_nFuncEval;
				m_vvFuncParm.add(x);
			}

			// increase number of number iterations
			m_nIterations++;
		}

		return eTermCode;
	}

}