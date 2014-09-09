package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;

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
 * This class implements the concept of a 2D triangular mesh. Included
 * functionality is a hit test and matlab export capability.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMMesh extends CAAMObject {

	/** A vector of triangles. */
	private Vector<CAAMTriangle> m_vTriangles = new Vector<CAAMTriangle>();

	/** A vector of points. */
	private Vector<CAAMPoint> m_vPoints = new Vector<CAAMPoint>();

	/** previous triangle index. */
	private int m_iPrevTriangle;

	/**
	 * Constructor.
	 */
	public CAAMMesh() {
		m_iPrevTriangle = 0;
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Adds a point to the end.
	 * 
	 * @param p
	 *            Point to add.
	 */
	public void Insert(final CAAMPoint p) {
		m_vPoints.add(p);
	}

	/**
	 * Replaces the points in the mesh with the one given by a shape. Preserves
	 * the triangles.
	 * 
	 * @param s
	 *            Shape to fecth points from.
	 */
	public void ReplacePoints(final CAAMShape s) {

		// delete the current points
		m_vPoints.clear();

		for (int i = 0; i < s.NPoints(); i++) {
			Insert(s.GetPoint(i));
		}

		for (int j = 0; j < m_vTriangles.size(); j++) {

			m_vTriangles.get(j).Calc_dD();
		}
	}

	/**
	 * Adds a triangle to the end.
	 * 
	 * @param t
	 *            Triangle to add.
	 */
	public void Insert(final CAAMTriangle t) {
		m_vTriangles.add(t);
	}

	/**
	 * Writes mesh structure to a matlab file containing three vectors:
	 * 
	 * xTri X-points. yTri Y-points. Tri Triangles defined as an (ntriangles x
	 * 3) matrix. Thus each row defines a triangle using three indices pointing
	 * to the point-vectors.
	 * 
	 * @param sFilename
	 *            The filename to be written. The file is overwritten if it
	 *            already exists.
	 * @return Zero on succes, non-zero if the mesh is empty.
	 */
	public int ToMatlab(final String sFilename) {

		assert (NPoints() > 0);

		if (NPoints() < 1)
			return -1;

		CDVector xVec = new CDVector(NPoints());
		CDVector yVec = new CDVector(NPoints());
		CDMatrix mTriangles = new CDMatrix(NTriangles(), 3);

		for (int i = 0; i < NPoints(); i++) {

			xVec.m_data[i] = m_vPoints.get(i).x;
			yVec.m_data[i] = m_vPoints.get(i).y;
		}

		for (int i = 0; i < NTriangles(); i++) {

			mTriangles.m_data[i][0] = m_vTriangles.get(i).V1();
			mTriangles.m_data[i][1] = m_vTriangles.get(i).V2();
			mTriangles.m_data[i][2] = m_vTriangles.get(i).V3();
		}

		xVec.ToMatlab(sFilename, "xTri", "x-positions", false);
		yVec.ToMatlab(sFilename, "yTri", "y-positions", true);
		mTriangles.ToMatlab(sFilename, "Tri", "the triangles", true);

		return 0;
	}

	/**
	 * Returns the total area of all triangles in the mesh.
	 * 
	 * @return The area.
	 */
	public double Area() {

		double sum = .0;

		for (int i = 0; i < m_vTriangles.size(); i++) {

			sum += m_vTriangles.get(i).Area();
		}

		return sum;
	}

	/**
	 * Assignment operator.
	 */
	// operator=
	public CAAMMesh assign(final CAAMMesh m) {

		int i;
		// m_vPoints = m.m_vPoints;
		// this.m_vPoints.clear();
		/*
		 * m_vPoints = new Vector<CAAMPoint>(); int p_len = m.m_vPoints.size();
		 * for ( i = 0; i < p_len; i++ ) { CAAMPoint temp = new CAAMPoint();
		 * temp.assign(m.m_vPoints.get(i)); this.m_vPoints.add(temp); }
		 * 
		 * // m_vTriangles = m.m_vTriangles; m_vTriangles = new
		 * Vector<CAAMTriangle>(); int t_len = m.m_vTriangles.size(); //
		 * this.m_vTriangles.clear(); for ( i = 0; i < t_len; i++ ) {
		 * CAAMTriangle temp = new CAAMTriangle();
		 * temp.assign(m.m_vTriangles.get(i)); m.m_vTriangles.add(temp); }
		 */

		m_vPoints = m.m_vPoints;
		m_vTriangles = m.m_vTriangles;
		m_iPrevTriangle = 0;

		for (i = 0; i < m_vTriangles.size(); i++) {

			// update pointer member
			m_vTriangles.get(i).m_pPoints = m_vPoints;
		}

		return this;
	}

	/**
	 * Copy constructor.
	 * 
	 * @param m
	 *            Mesh to copy.
	 */
	public CAAMMesh(final CAAMMesh m) {
		m_iPrevTriangle = 0;
		this.assign(m);
	}

	/**
	 * Returns a vector of triangles
	 * 
	 * @return triangle vector
	 */
	public Vector<CAAMTriangle> Triangles() {
		return m_vTriangles;
	}

	/**
	 * Returns a vector of points
	 * 
	 * @return mesh points vector
	 */
	public Vector<CAAMPoint> Points() {
		return m_vPoints;
	}

	/**
	 * Returns the total number of points in the mesh.
	 * 
	 * @return number of points
	 */
	public int NPoints() {
		return m_vPoints.size();
	}

	/**
	 * Returns the total number of triangles in the mesh.
	 * 
	 * @return number of triangles
	 */
	public int NTriangles() {
		return m_vTriangles.size();
	}

	/**
	 * Deletes all points and triangles.
	 */
	public void Clear() {
		m_vTriangles.clear();
		m_vPoints.clear();
	}

	/**
	 * Performs a hit test on the point p.
	 * 
	 * @param p
	 *            The point to test
	 * @return True if the point is inside, otherwise false.
	 */
	public boolean IsInside(final CAAMPoint p) {

		int[] triangle = new int[1];
		double[] alpha = new double[1];
		double[] beta = new double[1];
		double[] gamma = new double[1];

		return this.IsInside(p, triangle, alpha, beta, gamma);
	}

	/**
	 * Performs a hit test on the point p.
	 * 
	 * @param p
	 *            The point to test
	 * @param triangle
	 *            Triangle index.
	 * @param alpha
	 *            Relative position on triangle (barycentric coordinate).
	 * @param beta
	 *            Relative position on triangle (barycentric coordinate).
	 * @param gamme
	 *            Relative position on triangle (barycentric coordinate).
	 * @return True if the point is inside, otherwise false.
	 */
	public boolean IsInside(final CAAMPoint p, int[] triangle, double[] alpha,
			double[] beta, double[] gamma) {

		// try the previous triangle
		boolean bFound = m_vTriangles.get(m_iPrevTriangle).IsInside(p, alpha,
				beta, gamma);

		if (bFound) {

			triangle[0] = m_iPrevTriangle;
			return true;
		}

		// do *slow* linear search of all triangles
		//
		// LATER: use a kD-tree for this (i.e. quad-tree)
		//
		int n = m_vTriangles.size();
		for (int i = 0; i < n; i++) {

			if (i == m_iPrevTriangle) {
				continue;
			}

			if (m_vTriangles.get(i).IsInside(p, alpha, beta, gamma)) {

				((CAAMMesh) (this)).m_iPrevTriangle = i;
				triangle[0] = i;
				return true;
			}
		}

		return false;
	}

}
