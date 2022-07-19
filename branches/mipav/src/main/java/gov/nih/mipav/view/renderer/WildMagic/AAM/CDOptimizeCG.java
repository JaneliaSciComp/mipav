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
 * This file contains the implementation of the conjugate gradient
 * optimization algorithm. Conjugate gradient is implemented 
 * with Fletcher-Reeves update, soft line search and optional resetting. 
 * See e.g. Dennis and Schnabel, Numerical Methods for Unconstrained Optimization and 
 * Nonlinear Equations 1983, Prentice-Hall for a description of conjugate gradient.
 * 
 * @author Ruida Cheng
 */
public class CDOptimizeCG extends CDOptimizeBase {
	
	/** reset flag.  */
	private boolean m_fReset;

	/** reset every n iterations. */
	private int m_nResetIteN;
	
	
    /**
     * Constructor
     */
	public CDOptimizeCG()
	{
		super();
		// reset flag
		m_fReset = true;

		// reset every n iterations
		m_nResetIteN = 4;
	}

	
	/**
	 * dispose memory
	 */
	public void dispose() {
		
	}
	
    /**
     * name of optimization method
     */
	public String Name() {
		return "Conjugate Gradient";
	}
	
	/**
	 * Optimization method type
	 */
	public int OptMethod() {
		return eoptConjugateGradient;
	}

	/**
	 * resetting function
	 * @return
	 */
	public boolean Resetting() { 
		return m_fReset; 
	}
	
	/**
	 * 
	 * @param fReset
	 */
	public void SetResetting(final boolean fReset) { 
		m_fReset = fReset; 
	}

	/**
	 * reset every n iterations
	 * @param nResetIteN
	 */
	public void ResetIteNumber(final int nResetIteN) { 
		m_nResetIteN = nResetIteN; 
	}
	
	/**
	 * 
	 * @return
	 */
	public int SetResetIteNumber() { 
		return m_nResetIteN; 
	} 
	
	/**
	 * the Minimize function using Conjugate Gradient
	 * @param x     parameter vector
	 * @param pFuncEvalBase     optimization function pointer 
	 */
	public int Minimize(CDVector x,CDOptimizeFuncBase pFuncEvalBase)
	{
		double fc;
		double[] fplus = new double[1];
		CDVector xplus = new CDVector(x.Length());
		CDVector gc = new CDVector(x.Length());
		CDVector gplus = new CDVector(x.Length());
		CDVector sn = new CDVector(x.Length()); 
		CVisDMatrix Hc = new CVisDMatrix(x.Length(),x.Length());
		boolean[] maxtaken = new boolean[1];
		int retcode;

		// set the optimization function pointer
		SetFuncEvalBase(pFuncEvalBase);
		
		// initialize log counters and vector
		m_nFuncEval = 0;
		m_nGradEval = 0;
		m_vNFuncEval.assign(-1);
		m_vvFuncParm.clear();

		fc = EvalFunction(x);	

		EvalGradient(x,gc,fc);

		int eTermCode = UmStop0(x, fc, gc);
		
		if (eTermCode != etermNoStop) 
		{
			System.err.println("Minimum at initial position");
		}
		
		// save func. value and number of func. values
		if (m_fLogFuncValues) 
		{		
			m_vFuncVal.m_data[0] = fc;
			m_vNFuncEval.m_data[0] = 0;  
			m_vvFuncParm.add(x);
		}

		double dBeta;
		CDVector gcOld = new CDVector(gc.Length());

		//
		// Iteration section
		//

		m_nIterations = 1;
		while (eTermCode == etermNoStop) 
		{	

			// set/reset update
			if ( (m_nIterations == 1) || ( ((m_nIterations % m_nResetIteN) == 1) && m_fReset) )
			{
				sn.assign(gc.neg());
				gcOld.assign(gc);
			}
			else
			{
				// Fletcher-Reeves update
				dBeta = (gc.mult(gc))/(gcOld.mult(gcOld));
				sn.assign(gc.neg().add(sn.mult(dBeta)));
				gcOld.assign(gc);
			}

			// perform line search
			
			retcode = LineSearch(x, fc, gc, sn, xplus, fplus, maxtaken);
			
			// calc. new gradient (if needed)
			if (retcode == 0)
			{
				EvalGradient(xplus,gplus,fplus[0]);
			}

			// stop ??
			eTermCode = UmStop(x, xplus, fc, fplus[0], gplus, retcode, maxtaken[0]);
			
			if ( eTermCode > 0 ) 
			{
				x.assign(xplus);

				// update
				pFuncEvalBase.Update(x);
			}
			else 
			{	
				assert(fplus[0] < fc);
				x.assign(xplus);
				fc = fplus[0];
				gc.assign(gplus);

				// update
				pFuncEvalBase.Update(x);

			}

			// save func. value and number of func. values
			if (m_fLogFuncValues) 
			{
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