package gov.nih.mipav.view.renderer.WildMagic.AAM;

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