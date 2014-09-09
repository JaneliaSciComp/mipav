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
 * Performs a uniform stretch into [0;255] on the demapping side (yes, a really
 * degenerate class... I know...).
 */
public class CAAMTFUniformStretch extends CAAMTransferFunction {

	/**
	 * Constructor.
	 */
	public CAAMTFUniformStretch() {
		m_Id = tfUniformStretch;
	}

	/**
	 * Dispose memory.
	 */
	public void dispose() {

	};

	/**
	 * Clone function (conveys type info).
	 */
	public CAAMTransferFunction Clone() {
		return new CAAMTFUniformStretch();
	}

	/**
	 * Returns the clear-text type name.
	 */
	public String TypeName() {
		return new String("uniform stretch");
	}

	/**
	 * Does nothing.
	 * 
	 * @param v
	 *            Input vector.
	 * @return Nothing.
	 */
	public void Map(CDVector v) {

	}

	/**
	 * Maps the vector into [0;255].
	 * 
	 * @param v
	 *            Input vector. The result is returned in v.
	 */
	public void DeMap(CDVector v) {

		CAAMMathUtil.LinearStretchMinMax(v, 0, 255);
	}

};
