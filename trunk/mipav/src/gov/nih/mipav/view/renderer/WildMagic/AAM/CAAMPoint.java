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
 * Point container. This class act as a very simple container for the concept 'a
 * real-precision point'. It's merely a struct with a constructor. Used in the
 * mesh and triangle representation.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMPoint {

	/** x-position. */
	public double x;

	/** y position. */
	public double y;

	/**
	 * Constructor
	 */
	public CAAMPoint() {
		x = .0;
		y = .0;
	}

	/**
	 * Constructor
	 * @param _x  x coordinate
	 * @param _y  y coordinate
	 */
	public CAAMPoint(final double _x, final double _y) {
		x = _x;
		y = _y;
	}

	/**
	 * Assignment operator 
	 * @param p   point reference
	 */
	public void assign(CAAMPoint p) {
		this.x = p.x;
		this.y = p.y;
	}

}