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
 * Evaluation result class.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMEvalRes {

	/** model shape */
	private CAAMShape m_sModelShape = new CAAMShape();

	/** ground truth shape */
	private CAAMShape m_sGroundTruth = new CAAMShape();

	/** Average point to point landmark error. */
	public double[] m_dPtPt = new double[1];

	/** Average point to curve landmark error. */
	public double[] m_dPtCrv = new double[1];

	public double m_dOverlap, m_dTime;

	/** curve landmark vector */
	private CDVector m_vPtCrvDists = new CDVector();

	/** optimizatino resutls */
	public CAAMOptRes m_OptRes = new CAAMOptRes();

	/**
	 * Constructor Compute the evaluation optimization results.
	 * 
	 * @param model_shape
	 *            Model shape
	 * @param ground_truth
	 *            Groud truth shape
	 * @param time
	 *            running time
	 * @param optRes
	 *            Optimization results.
	 */
	public CAAMEvalRes(final CAAMShape model_shape,
			final CAAMShape ground_truth, final double time,
			final CAAMOptRes optRes) {

		m_sModelShape.assign(model_shape);
		m_sGroundTruth.assign(ground_truth);
		m_dTime = time;
		m_OptRes = optRes;

		CAAMUtil.CalcShapeDistances(m_sModelShape, m_sGroundTruth, m_dPtPt,
				m_dPtCrv, m_vPtCrvDists);
		m_dOverlap = CAAMUtil.ShapeOverlap(m_sModelShape, m_sGroundTruth);
	}

}