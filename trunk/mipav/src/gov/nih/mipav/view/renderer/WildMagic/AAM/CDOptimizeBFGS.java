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
 * DTU Image Viever and Analyser (DIVA) Department of Mathematical Modelling
 * Technical University of Denmark (DTU), Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/~diva
 * 
 * author: Per Andresen and Rune Fisker
 * 
 * This file contains the implementation of the quasi-newton BFGS optimization
 * algorithm. BFGS is implemented according to
 * 
 * Dennis and Schnabel, Numerical Methods for Unconstrained Optimization and
 * Nonlinear Equations 1983, Prentice-Hall
 * 
 * @author Ruida Cheng
 * 
 */
public class CDOptimizeBFGS extends CDOptimizeBase {

	/**
	 * Constructor
	 */
	public CDOptimizeBFGS() {
		super();
	};

	/**
	 * dispose memory
	 */
	public void dispose() {

	};

	/**
	 * intialize the hessian Algorithm A9.4.3 p. 359
	 * 
	 * @param fc
	 * @param Hc
	 */
	private void InitHessUnFac(double fc, CVisDMatrix Hc) {
		// const double temp = max(abs(fc), m_dTypF);
		final double temp = 1;

		Hc.assign(0); // set all elements to zero
		for (int i = Hc.NRows() - 1; i >= 0; --i) {
			Hc.m_data[i][i] = temp; // temp * m_Sx(i) * m_Sx(i);
		}
	}

	/**
	 * Algorithm D5.5.1 (look at page 315-318 for code)
	 * 
	 * Description: Make Hc has positive definite Only step 13 is implemented,
	 * because of shortcut by the use of LAPACK
	 */
	private void ModelHess(CVisDMatrix mH) {
		// step 13
		double dMaxEv = mH.m_data[0][0];
		double dMinEv = mH.m_data[0][0];
		double dMaxAdd = mH.m_data[0][0];

		for (int i = 0; i < mH.NRows(); i++) {
			double dOffRow = 0;

			for (int j = 0; j < i; j++)
				dOffRow += mH.m_data[j][i];

			for (int j = i + 1; j < mH.NCols(); j++)
				dOffRow += mH.m_data[j][i];

			dMaxEv = Math.max(dMaxEv, dOffRow + mH.m_data[i][i]);
			dMinEv = Math.min(dMaxEv, dOffRow - mH.m_data[i][i]);
			dMaxAdd = Math.max(dMaxAdd, mH.m_data[i][i]);
		}

		double dSdd = (dMaxEv - dMinEv) * Math.sqrt(m_dMachEps) - dMinEv;
		dSdd = Math.max(dSdd, 0);

		double dMy = Math.min(dMaxAdd, dSdd);

		assert (dMy != 0);

		// add my to diagonal
		for (int i = 0; i < mH.NRows(); i++) {
			mH.m_data[i][i] += dMy;
		}
	}

	/**
	 * Algorithm A9.4.1 p. 355
	 * 
	 * Input: x, xplus, gc, gplus Input-Output: H
	 * 
	 * Description: The BFGS update is made unless either: i) y'*s <
	 * (macheps)^0.5*||y|| or ii) for every i, |(y-Hs)[i]| is less than the
	 * estimated noise in y[i]. The estimated noise in y[i] is calculated to
	 * tol*(|gc[i]| + |gplus[i]|) where tol=eta if analytic gradients are being
	 * used, tol=eta^0.5 if finite difference gradients are being used.
	 */
	public void BFGSUnFac(CDVector xc, CDVector xplus, CDVector gc,
			CDVector gplus, CVisDMatrix Hc) {
		CDVector s = new CDVector(xc.Length());
		CDVector y = new CDVector(xc.Length());
		CDVector t = new CDVector(xc.Length());
		double temp1, temp2;
		double tol = m_dEta;
		boolean skipupdate;
		int i, j;

		s.assign(xplus.sub(xc));
		y.assign(gplus.sub(gc));
		temp1 = y.mult(s); // y^T dot s

		if (temp1 > Math.sqrt(m_dMachEps) * Math.sqrt(s.mult(s))
				* Math.sqrt(y.mult(y))) // sqrt(y*y) calculates 2-norm of the
										// vector y;
		{
			skipupdate = true;
			t.assign(Hc.mult(s)); // I'm not using that H is symmetric !!!

			for (i = xc.Length() - 1; i >= 0; --i) {
				if (Math.abs(y.m_data[i] - t.m_data[i]) >= tol
						* Math.max(Math.abs(gc.m_data[i]),
								Math.abs(gplus.m_data[i]))) {
					skipupdate = false;
				}
			}

			if (skipupdate == false) {
				assert (temp1 != 0);

				temp2 = s.mult(t); // s^T dot t <=> s^T*H*s
				assert (temp2 != 0);

				for (i = xc.Length() - 1; i >= 0; --i) {
					for (j = xc.Length() - 1; j >= 0; --j) {
						Hc.m_data[i][j] += y.m_data[i] * y.m_data[j] / temp1
								- t.m_data[i] * t.m_data[j] / temp2;
					}
				}
				assert (Hc.IsSymmetric() == true);
			}
		}
	}

	/**
	 * Algorithm D6.1.1 (look at page 274-275 for code) Input: start point for x
	 * Output in x: x* - optimal point
	 */
	public int Minimize(CDVector x, CDOptimizeFuncBase pFuncEvalBase) {
		double fc;
		double[] fplus = new double[1];
		CDVector xplus = new CDVector(x.Length());
		CDVector gc = new CDVector(x.Length());
		CDVector gplus = new CDVector(x.Length());
		CDVector sn = new CDVector(x.Length());
		CVisDMatrix Hc = new CVisDMatrix(x.Length(), x.Length());
		CVisDMatrix tempMatrix = new CVisDMatrix(x.Length(), x.Length());
		boolean[] maxtaken = new boolean[1];
		int retcode;

		// set the optimization function pointer
		SetFuncEvalBase(pFuncEvalBase);

		// initialize counters and vector
		m_nFuncEval = 0;
		m_nGradEval = 0;
		m_vNFuncEval.assign(-1);
		m_vvFuncParm.clear();

		fc = EvalFunction(x);

		EvalGradient(x, gc, fc);

		int eTermCode = UmStop0(x, fc, gc);

		if (eTermCode == etermNoStop) {
			InitHessUnFac(fc, Hc);
			assert (Hc.IsSymmetric() == true);

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
				try {
					// compared to the original algorithme we do a shortcut by
					// letting LAPACK
					// solve Hc*sn = -gc for sn. If Hc is singular
					// VisDMatrixSolve throws an exception
					sn.assign(tempMatrix.VisDMatrixSolve(Hc, gc.neg()));
				} catch (Exception e) {
					// make Hc have positive definite
					ModelHess(Hc);

					// solve again
					sn.assign(tempMatrix.VisDMatrixSolve(Hc, gc.neg()));
				}

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
					x = xplus;

					// update
					pFuncEvalBase.Update(x);

				} else {
					BFGSUnFac(x, xplus, gc, gplus, Hc);
					assert (Hc.IsSymmetric() == true);

					assert (fplus[0] < fc);
					x = xplus;
					fc = fplus[0];
					gc = gplus;

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
		}

		return eTermCode;
	}

}