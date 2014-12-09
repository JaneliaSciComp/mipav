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
 * IMM, Department of Mathematical Modelling Technical University of Denmark,
 * Building 321 DK-2800 Lyngby, Denmark http://www.imm.dtu.dk/~diva
 * 
 * author: Rune Fisker
 * 
 * Header-file for the simulated annealing optimization class. Simulated
 * annealing [1,2] is implemented with the Metroplis algorithm [3], random walk
 * and temperature scheme T_t+1 = k T_t [1]
 * 
 * [1] Kirkpatrick, S. and Gellant, C. D. and Vecchi, M. P., Optimization by
 * simulated annealing, Science, 1983, vol. 220, pp.671-680 
 * 
 * [2] Cerny, V., Thermodynamical approach to the traveling salesman problem: an efficient
 * simulation algorithm}, Jour. of Optimization Theory and Applications, 1985,
 * vol. 45, pp. 41-45 
 * 
 * [3] Metropolis, N. and Rosenbluth, A. W. and Rosenbluth, M. N. and Teller A. H. and 
 * Teller E., Equations of state calculations by fast
 * computing machines, Jour. Chemical Physics, 1953, 21, 1087-1092
 * 
 * 
 * @author Ruida Cheng
 * 
 */
public class CDOptimizeSA extends CDOptimizeBase {

	public static int RAND_MAX = 32767;

	// seed strategy for random generator
	// enum eSeedToRandom
	public int eSeedToRandom;
	public static int eseedNo = 0; // no seed
	public static int eseedRandom = 1; // random seed
	public static int essedConstant = 2;// constant seed

	/** start temperature. */
	private double m_dStartTemperature;

	/** decrease factor in the temperature scheme. */
	private double m_dK;

	/** flag for random seed in random walk. */
	private int m_eSeedToRand;

	/** dispose memory. */
	public void dispose() {

	}

	/** name of optimization method. */
	public String Name() {
		return "Simulated Annealing";
	}

	/** optimizaitn method type. */
	public int OptMethod() {
		return eoptSimulatedAnnealing;
	}

	/** start temperature. */
	public void SetStartTemperature(final double dStartTemperature) {
		m_dStartTemperature = dStartTemperature;
	}

	/** get start tempature. */
	public double StartTemperature() {
		return m_dStartTemperature;
	}

	/** decrease factor in the temperature scheme. */
	public void SetDecFac(final double dK) {
		m_dK = dK;
	}

	/** get decreasing factor. */
	public double DecFac() {
		return m_dK;
	}

	/** flag for random seed in random walk. */
	public int RandomSeed() {
		return m_eSeedToRand;
	}

	/** get random seeding flag. */
	public void SetRandomSeed(final int eSeedToRand) {
		m_eSeedToRand = eSeedToRand;
	}

	/** constructor. */
	public CDOptimizeSA() {
		super();
		// set start temperature
		m_dStartTemperature = 0.01;

		// decay factor
		m_dK = 0.98;

		// flag for random seed in random walk
		m_eSeedToRand = eseedRandom;
	}

	/**
	 * minimization using steepest annealing.
	 */
	public int Minimize(CDVector x, CDOptimizeFuncBase pFuncEvalBase) {
		// set the optimization function pointer
		SetFuncEvalBase(pFuncEvalBase);

		// initialize log counters and vector
		m_nFuncEval = 0;
		m_nGradEval = 0;
		m_vNFuncEval.assign(-1);
		m_vvFuncParm.clear();

		CDVector xNew = new CDVector(x.Length());

		CDVector vParStd = new CDVector(x.Length());
		vParStd = MethodPar();

		// random, constant or no seed
		if (m_eSeedToRand == eseedRandom) {
			// srand( (unsigned)time( NULL ) );
		} else if (m_eSeedToRand == essedConstant) {
			// srand( 10000 );
		}

		// eval func.
		double dFx = EvalFunction(x);

		// log if enabled
		if (m_fLogFuncValues) {
			// save func. value and number of func. values
			m_vFuncVal.m_data[0] = dFx;
			m_vNFuncEval.m_data[0] = 0;
			m_vvFuncParm.add(x);
		}

		//
		// Iteration section
		//
		double dTemperature = m_dStartTemperature;

		m_nIterations = 1;
		while (m_nIterations < MaxIterations()) {
			// generate new configuration by random walk
			for (int iPar = 0; iPar < x.Length(); iPar++) {
				double dG = Gauss();
				xNew.m_data[iPar] = x.m_data[iPar] + vParStd.m_data[iPar] * dG;
			}

			// eval new configuration
			double dFxNew = EvalFunction(xNew);

			// calc. acceptance probability before exp
			double dExpon = -(dFxNew - dFx) / dTemperature;

			// stop if dProbAccept equals zero anyway
			// note exp(-20) = 2.0612e-009
			if (dExpon > -20) {
				double dProbAccept = 1;
				double UniRand = 0;

				// only change if needed
				if (dExpon < 0) {
					// update dProbAccept
					dProbAccept = Math.min(1, Math.exp(dExpon));

					// generate uniform rand number [0;1]
					UniRand = ((double) Math.random()) / RAND_MAX;
				}

				// accept if better
				if (dProbAccept >= UniRand) {
					x.assign(xNew);
					dFx = dFxNew;

					// update
					pFuncEvalBase.Update(x);

				}
			}

			// update temperature
			dTemperature = m_dK * dTemperature;

			// save func. value and number of func. values
			if (m_fLogFuncValues) {
				m_vFuncVal.m_data[m_nIterations] = dFx;
				m_vNFuncEval.m_data[m_nIterations] = m_nFuncEval;
				m_vvFuncParm.add(x);
			}

			// increase iterations
			m_nIterations++;
		}

		return etermMaxIterations;
	}

	/** make randomize gauss. */
	public double Gauss() {
		int iset = 0;
		double gset = 0;
		double fac, r, v1, v2;

		if (iset == 0) {
			do {
				double dRand1 = ((double) Math.random()) / RAND_MAX;
				double dRand2 = ((double) Math.random()) / RAND_MAX;
				v1 = 2.0 * dRand1 - 1.0;
				v2 = 2.0 * dRand2 - 1.0;
				r = v1 * v1 + v2 * v2;
			} while (r >= 1.0);

			fac = Math.sqrt(-2.0 * Math.log(r) / r);
			gset = v1 * fac;
			iset = 1;
			return v2 * fac;
		} else {
			iset = 0;
			return gset;
		}
	}

}