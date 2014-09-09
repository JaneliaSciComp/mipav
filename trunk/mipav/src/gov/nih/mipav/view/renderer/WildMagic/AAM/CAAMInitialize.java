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
 * Abstract base class for all AAM initialization classes.
 * 
 * @author Ruida Cheng
 * 
 */
public abstract class CAAMInitialize extends CAAMObject {

	/** AAM model */
	protected static CAAMModel m_pModel;

	/**
	 * Constructor.
	 */
	public CAAMInitialize() {
		m_pModel = null;
	}

	/**
	 * Copy constructor.
	 * 
	 * @param model
	 *            Reference to a model.
	 */
	public CAAMInitialize(final CAAMModel aammodel) {

		m_pModel = aammodel;
	}

	/**
	 * General initialization function.
	 */
	public abstract int Initialize(final ModelSimpleImage image, CAAMShape s,
			CDVector c);

}