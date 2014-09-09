package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;

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
 * Piece-wise affine warping between two shapes. This class implements a
 * piece-wise affine warping between two shapes using a Delaunay Triangulization
 * of the source shape. This triangulization is the used for both the source and
 * destination shape. The correspondance between points and triangles gives a
 * continuous deformation field inside the triangles. Since the deformation is
 * performed on a linear per-triangle basis, the field is not smooth across
 * triangles.
 * 
 * @see CAAMWarp
 */
public class CAAMWarpLinear extends CAAMWarp {

	/** The Delaunay Triangulization of the source or dest shape. */
	private CAAMMesh m_Mesh = new CAAMMesh();

	/** flag to indicate use src Delaunay. */
	private boolean m_bUseSrcDelaunay;

	/** The points of the destination shape. */
	private Vector<CAAMPoint> m_vDestPoints = new Vector<CAAMPoint>();

	// private int m_PreviousTriangleHit;

	/**
	 * Constructor.
	 * 
	 * @param useSrcDelaunay
	 *            If true the Delaunay of the source shape is used.
	 */
	public CAAMWarpLinear(boolean useSrcDelaunay) {
		// m_PreviousTriangleHit = 0;
		m_bUseSrcDelaunay = useSrcDelaunay;
	}

	/**
	 * Returns true if the source shape has been set.
	 * 
	 * @return check valid triangle
	 */
	public boolean HasSrcShape() {
		return m_Mesh.NPoints() > 0 && m_Mesh.NTriangles() > 0;
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Sets the shape to warp from.
	 * 
	 * @param s
	 *            Input shape.
	 */
	public void SetSrcShape(final CAAMShape s) {

		// get src shape extents
		super.SetSrcShape(s);

		// delete any old content in the mesh member variable
		m_Mesh.Clear();

		if (m_bUseSrcDelaunay) {

			// make a mesh using a Delaunay Triangulation.
			CAAMDelaunay.MakeMesh(s, m_Mesh, !m_bUseConvexHull);
		}
	}

	/**
	 * Sets the shape to warp to.
	 * 
	 * @param s
	 *            Input shape.
	 * @return Nothing.
	 */
	public void SetDestShape(final CAAMShape s) {

		int nbPoints = s.NPoints();

		// ensure that the mesh and the point set
		// has an equal amount of points
		assert (m_bUseSrcDelaunay == false || nbPoints == m_Mesh.NPoints());

		// delete any old destination points
		m_vDestPoints.clear();

		// add the destination points
		for (int i = 0; i < nbPoints; i++) {

			m_vDestPoints.add(s.GetPoint(i));
		}

		if (m_bUseSrcDelaunay == false) {

			// make a mesh using a Delaunay Triangulation.
			CAAMDelaunay.MakeMesh(s, m_Mesh, !m_bUseConvexHull);
			m_Mesh.ReplacePoints(m_SrcShape);
		}
	}

	/**
	 * Warps point 'in' to point 'out' (if possible).
	 * 
	 * @param in
	 *            Input point.
	 * @param out
	 *            Output point.
	 * @return True if 'in' is inside the source mesh, false if not.
	 */
	public boolean Warp(final CAAMPoint in, CAAMPoint out) {

		double[] alpha = new double[1];
		double[] beta = new double[1];
		double[] gamma = new double[1];
		int[] triangle = new int[1];

		boolean bFound = m_Mesh.IsInside(in, triangle, alpha, beta, gamma);

		if (!bFound) {
			return false;
		}

		// get triangle
		final CAAMTriangle tri = m_Mesh.Triangles().get(triangle[0]);

		// calculate warped point
		out.x = alpha[0] * m_vDestPoints.get(tri.V1()).x + beta[0]
				* m_vDestPoints.get(tri.V2()).x + gamma[0]
				* m_vDestPoints.get(tri.V3()).x;
		out.y = alpha[0] * m_vDestPoints.get(tri.V1()).y + beta[0]
				* m_vDestPoints.get(tri.V2()).y + gamma[0]
				* m_vDestPoints.get(tri.V3()).y;

		return true;
	}

}