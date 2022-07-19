package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.structures.*;

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
 * General purpose optimization of the AAM.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMOptimize implements CDOptimizeFuncBase {

	/** The AAM model. */
	private final CAAMModel m_pModel;

	/** The initial shape pose. */
	private final CAAMShape m_pShape;

	/** The image where the optimization shuold be done. */
	private final ModelSimpleImage m_pImage;

	/** The similarity measure to be used. */
	private int m_iSimilarityMeasure;

	/** The difference vector. */
	private CDVector m_vDiff = new CDVector();

	/** The minimum fit. */
	private double m_dMinFit;

	/** The minimum fit shape. */
	private CAAMShape m_sMinShape = new CAAMShape();

	/** The minimum fit model parameters. */
	private CDVector m_vMinC = new CDVector();

	/** scale fit of AAM */
	private double m_dFit;

	/**
	 * evaluates the analytic gradient at postion vX (if exists)
	 */
	public void EvalGradient(CDVector vX, CDVector vGradient) {

	}

	/**
	 * function used to update e.g. interface
	 */
	public void Update(CDVector vX) {

	}

	/**
	 * Return scale fit of AAM
	 * @return
	 */
	public double FinalFrror() {
		return m_dFit;
	}

	/**
	 * Constructor for the AAM optimizer. Sets uo initial info about AAM, shape
	 * and the image the model should optimized on.
	 * 
	 * @param aammodel
	 *            The AAM.
	 * @param s
	 *            The initial shape pose being optimized.
	 * @param image
	 *            The image on which the optimization shall be done.
	 * @param similaritym
	 *            The simlarity measure to be used
	 */
	public CAAMOptimize(final CAAMModel aammodel, final CAAMShape s,
			final ModelSimpleImage image, final int similaritym) {

		m_dFit = .0;
		m_pModel = aammodel;
		m_pShape = s;
		m_pImage = image;
		m_iSimilarityMeasure = similaritym;

		m_dMinFit = 1e306;
	}

	/**
	 * Fuction to be optimized.
	 * Function providing a scalar interpretation of the AAM fit based on a
	 *      set of paramters.
	 * @param vX
	 *            The independent parameters that will be optimized. The first
	 *            n-4 elements constitues the normal model parameters of the
	 *            AAM. The last four are the pose parameters.
	 * @return The scalar fit of the AAM.
	 */
	public double EvalFunction(CDVector vX) {

		int n = m_pModel.CombinedPCA().NParameters();

		// extract the model parameters
		CDVector c = new CDVector(n);
		for (int i = 0; i < n; i++)
			c.m_data[i] = vX.m_data[i];

		// extract pose parameters
		CDVector pose = new CDVector(4);
		CDVector tmpPose = new CDVector(4);
		for (int i = 0; i < 4; i++)
			pose.m_data[i] = vX.m_data[i + n];

		// we use the initial shape as reference, i.e. pose = [0 0 0 0]
		CAAMShape s = new CAAMShape();
		s.assign(m_pShape);
		s.Displace(pose);

		m_dFit = m_pModel.ModelEstimateTexDiff(m_pImage, c, s, m_vDiff,
				m_iSimilarityMeasure);

		if (m_dFit < 0) {

			// we're outside the image
			m_dFit = 1e306;
		}

		if (m_dFit < m_dMinFit) {

			m_dMinFit = m_dFit;
			m_sMinShape.assign(s);
			m_vMinC.assign(c);
		}

		return m_dFit;
	}

	/**
	 * Returns the optimisation results as c, shape and error. This is to
	 *      avoid and extra conversion after ended optimisation, and worse, an
	 *      extra image sampling to get the error.
	 * @param c
	 *            The optimal model parameters.
	 * @param s
	 *            The optimal shape.
	 * @param s
	 *            The optimal error.
	 */
	public void OptResults(CDVector c, CAAMShape s, double[] fit) {

		c.Resize(m_vMinC.Length());

		c.assign(m_vMinC);
		s.assign(m_sMinShape);
		fit[0] = m_dMinFit;
	}

}