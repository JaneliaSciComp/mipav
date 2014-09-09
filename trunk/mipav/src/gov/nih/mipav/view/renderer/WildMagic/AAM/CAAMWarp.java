package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation.
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B. Stegmann IMM, Informatics &
 * Mathmatical Modelling Technical University of Denmark Richard Petersens Plads
 * Building 321 DK-2800 Lyngby, Denmark http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * Base class for 2D warp classes. CAAMWarp defines a 2D warp function between
 * two shapes with an equal amount of points.
 * 
 * @author Ruida Cheng
 * 
 */
public abstract class CAAMWarp extends CAAMObject {

	/** Source shape extents. */
	private double m_dSrcShapeMinX;

	/** Source shape extents. */
	private double m_dSrcShapeMaxX;

	/** Source shape extents. */
	private double m_dSrcShapeMinY;

	/** Source shape extents. */
	private double m_dSrcShapeMaxY;

	/** Allows warping inside the convex hull. */
	protected boolean m_bUseConvexHull;

	/** source shape voi */
	protected CAAMShape m_SrcShape = new CAAMShape();

	/**
	 * Constructor
	 */
	public CAAMWarp() {
	}

	/**
	 * Dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Warps the point 'in' to the point 'out' using the two shapes as irregular
	 * point clouds defining a 2D warp function. 'in' defines a point contained
	 * in the source shape and 'out' is the corrosponding point in the
	 * destination shape.
	 * 
	 * @param in
	 *            Input point.
	 * @param out
	 *            Output point.
	 * @return True if the warp can be done, false if not.
	 */
	public abstract boolean Warp(final CAAMPoint in, CAAMPoint out);

	/**
	 * Sets the shape to warp from.
	 */
	public void SetSrcShape(final CAAMShape s) {

		m_SrcShape.assign(s);

		// get src shape extents
		m_dSrcShapeMinX = s.MinX();
		m_dSrcShapeMaxX = s.MaxX();
		m_dSrcShapeMinY = s.MinY();
		m_dSrcShapeMaxY = s.MaxY();
	}

	/** Sets the shape to warp to. */
	public abstract void SetDestShape(final CAAMShape s);

	/** Allows warping inside the convex hull (default=off). */
	public void UseConvexHull(boolean enable) {
		m_bUseConvexHull = enable;
	}

	/**
	 * Get the shape min x coordinate
	 * 
	 * @return x min
	 */
	public double SrcMinX() {
		return m_dSrcShapeMinX;
	}

	/**
	 * Get the shape max x coordinate
	 * 
	 * @return x max
	 */
	public double SrcMaxX() {
		return m_dSrcShapeMaxX;
	}

	/**
	 * Get the shape min x coordinate
	 * 
	 * @return x min
	 */
	public double SrcMinY() {
		return m_dSrcShapeMinY;
	}

	/**
	 * Get the shape max x coordinate
	 * 
	 * @return x max
	 */
	public double SrcMaxY() {
		return m_dSrcShapeMaxY;
	}

}