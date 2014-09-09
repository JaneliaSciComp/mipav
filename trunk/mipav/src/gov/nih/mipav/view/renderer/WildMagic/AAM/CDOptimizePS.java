package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B.
 * Stegmann IMM, Informatics & Mathmatical Modelling Technical University of
 * Denmark Richard Petersens Plads Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * Copyright © 2000
 *
 * DTU Image Viever and Analyser (DIVA)
 * Department of Mathematical Modelling
 * Technical University of Denmark (DTU), Building 321
 * DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/~diva
 *
 * author: Rune Fisker 
 *
 * This file contains the implementation of the pattern search
 * optimization algorithm.
 * 
 * @author Ruida Cheng
 */
public class CDOptimizePS extends CDOptimizeBase {

	/**
	 * Constructor
	 */
	public CDOptimizePS(){ 
		super();
		m_dStepTol = 0.1; 
	}
	
	/**
	 * dispose memory
	 */
	public void dispose() {
		
	}
	
    /** 
     * name of optimization methode
     */
	public String Name() {
		return "Pattern Search";
	}
	
	/**
	 * Optimization function type.
	 */
	public int OptMethod() {
		return eoptPatternSearch;
	}
	
	/**
	 * Minimization using pattern search. 
	 *@param x     parameter vector
	 * @param pFuncEvalBase     optimization function pointer 
	 */
	public int Minimize(CDVector x, CDOptimizeFuncBase pFuncEvalBase)
	{
		// set the optimization function pointer
		SetFuncEvalBase(pFuncEvalBase);
		
		// initialize log counters and vector
		m_nFuncEval = 0;
		m_nGradEval = 0;
		m_vNFuncEval.assign(-1);
		m_vvFuncParm.clear();

		CDVector vXHat = new CDVector(x.Length());
		CDVector vDelta = new CDVector(x.Length());
		CDVector vGPlus = new CDVector(x.Length());
		vGPlus.assign(1);

		vXHat.assign(x); 
		vDelta.assign(MethodPar());

	    int eTermCode = etermNoStop;

		double dFX = EvalFunction(x);
		double[] dFXHat = new double[1];
		dFXHat[0] = dFX;

		// save func. value and number of func. values
		if (m_fLogFuncValues) 
		{
			m_vFuncVal.m_data[0] = dFX;
			m_vNFuncEval.m_data[0] = 0;  			
			m_vvFuncParm.add(x);
		}

		m_nIterations = 1;
		while (eTermCode == etermNoStop)
		{
			// generate new x
			Explore(vXHat,vDelta,dFXHat);

			// check for stop
			if (dFX == dFXHat[0])
				eTermCode = UmStop(x, vXHat, dFX*1.1, dFXHat[0], vGPlus, 0, false); // cheating for stopping because dFX == dFXHat
			else
				eTermCode = UmStop(x, vXHat, dFX, dFXHat[0], vGPlus, 0, false);

			// accept new x if better
			if (dFX > dFXHat[0])
			{		
				x.assign(vXHat); 
				dFX = dFXHat[0];
	 
				// update
				pFuncEvalBase.Update(x);

			}
			else // or update delta
			{
				vDelta.div_into(2.0);
			}

			// save func. value and number of func. values
			if (m_fLogFuncValues) 
			{
				m_vFuncVal.m_data[m_nIterations] = dFX;
				m_vNFuncEval.m_data[m_nIterations] = m_nFuncEval; 
				m_vvFuncParm.add(x);
			}

			// increase number of number iterations
			m_nIterations++;
		}

		return eTermCode;
	}


	public void Explore(CDVector vX,CDVector vDelta, double[] dFVal)
	{
		double dFMin,dFValPlus,dFValMinus;

		CDVector vE = new CDVector(vX.Length());
		CDVector vTmp = new CDVector(vX.Length());

		// change each paramter
		for (int i=0; i<vX.Length(); i++)
		{
			vE.assign(0);
			vE.m_data[i] = vDelta.m_data[i];

			vTmp.assign(vX.add(vE));
			dFValPlus = EvalFunction(vTmp);
			vTmp.assign(vX.sub(vE));
			dFValMinus = EvalFunction(vTmp);

			dFMin = Math.min(dFValPlus,dFValMinus);

			// update if better
			if (dFMin < dFVal[0])
			{
				if (dFMin == dFValPlus)
				{
					vX.assign(vX.add(vE));
					dFVal[0] = dFMin;
				}
				else if (dFMin == dFValMinus)
				{
					vX.assign(vX.sub(vE));
					dFVal[0] = dFMin;
				}
			}
		}
	}
	
	
}