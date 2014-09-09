package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.structures.*;

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