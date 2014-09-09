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
 * Stores optimization results.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMOptRes {

	/** The number of iterations. */
	int m_nIterations;

	/** The Mahalanobis Distance of the model parameters. */
	double m_dMaha;

	/** The similarity measure value at optimum. */
	double m_dSimilarityMeasure;

	/**
	 * Constructor
	 */
	public CAAMOptRes() {
		m_dMaha = .0;
		m_dSimilarityMeasure = .0;
		m_nIterations = 0;
	}

	/**
	 * Constructor
	 * 
	 * @param maha
	 *            Mahalanobis distance
	 * @param nIterations
	 *            number iterations
	 * @param sm
	 *            similarity measure
	 */
	public CAAMOptRes(final double maha, final int nIterations, final double sm) {

		m_dMaha = maha;
		m_nIterations = nIterations;
		m_dSimilarityMeasure = sm;
	}

	/**
	 * Returns the number of iterations.
	 * 
	 * @return number of iterations.
	 */
	public int NIter() {
		return m_nIterations;
	}

	/**
	 * Returns the Mahalanobis Distance of the model parameters.
	 * 
	 * @return Mahalanobis distance.
	 */
	public double Mahalanobis() {
		return m_dMaha;
	}

	/**
	 * Returns the similarity measure.
	 * 
	 * @return similarity measure
	 */
	public double SimilarityMeasure() {
		return m_dSimilarityMeasure;
	}

}