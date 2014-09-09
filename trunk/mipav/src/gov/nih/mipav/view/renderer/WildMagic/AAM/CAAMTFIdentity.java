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
 * Transfer function that implements the identity transformation. Actually this
 * function is one of the reasons for the design of the Map and DeMap methods.
 * An identity transform should not induce any computational overhead
 * whatsoever.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMTFIdentity extends CAAMTransferFunction {

	/**
	 * Constructor
	 */
	public CAAMTFIdentity() {
		m_Id = tfIdentity;
	}

	/**
	 * Dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Clone this instance
	 * @return tsf  new clone object from this
	 */
	public CAAMTransferFunction Clone() {
		CAAMTFIdentity tsf = new CAAMTFIdentity();
		tsf.m_Id = this.m_Id;
		return tsf;
	}

	/**
	 * Get the transfer function identity name
	 */
	public String TypeName() {
		return new String("identity");
	}

	// Mapping
	public void Map(CDVector v) {

	}

	public void DeMap(CDVector v) {

	}

}