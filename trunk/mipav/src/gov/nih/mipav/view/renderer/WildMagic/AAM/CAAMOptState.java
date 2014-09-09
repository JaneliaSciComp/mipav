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
 * Stores iterations from the optimization process.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMOptState {

	/** error dependent upon the chosen similarity measure. */
	public double error;

	/** shape. */
	public CAAMShape shape = new CAAMShape();

	/** model parameters. */
	public CDVector c = new CDVector();

	/** number of dampings used in the iteration. */
	public int damps;

	/**
	 * Constructor
	 */
	public CAAMOptState() {

	}

	/**
	 * Constructor
	 * 
	 * @param e
	 *            error
	 * @param s
	 *            shape
	 * @param _c
	 *            model parameters
	 * @param d
	 *            number of damping factors
	 */
	public CAAMOptState(double e, final CAAMShape s, final CDVector _c, int d) {
		error = e;
		shape.assign(s);
		c.assign(_c);
		damps = d;
	}

	/**
	 * Assignment operator
	 * 
	 * @param os
	 *            iterations of optimization processes
	 * @return this
	 */
	public CAAMOptState assign(final CAAMOptState os) {

		error = os.error;
		shape.assign(os.shape);
		c.Resize(os.c.Length());
		c.assign(os.c);
		damps = os.damps;

		return this;
	}

}