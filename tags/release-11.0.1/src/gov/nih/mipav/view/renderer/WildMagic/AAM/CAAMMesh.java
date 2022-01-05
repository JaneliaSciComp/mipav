package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
 * AAM-API LICENSE  -  file: license.txt
 * 
 * This software is freely available for non-commercial use such as
 * research and education. Please see the full disclaimer below. 
 * 
 * All publications describing work using this software should cite 
 * the reference given below. 
 * 	
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling
 * DTU, Technical University of Denmark
 * Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where 
 * the AAM-API has been used. A draft version the article is available from 
 * the homepage. 
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * -------------
 * M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance 
 * Modelling Environment", IEEE Transactions on Medical Imaging, IEEE, 2003
 * (to appear)
 * -------------
 * 
 *
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK
 * - LAPACK
 * 
 *
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the author be held liable for any damages arising from the
 * use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial 
 * purpose, and to alter it, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim
 *  that you wrote the original software. 
 *
 * 2. Altered source versions must be plainly marked as such, and must not be 
 *  misrepresented as being the original software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 *
 * No guarantees of performance accompany this software, nor is any 
 * responsibility assumed on the part of the author or IMM. 
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any 
 * express or implied warranties, including, but not limited to, the implied 
 * warranties of merchantability and fitness for a particular purpose are 
 * disclaimed. In no event shall IMM or Mikkel B. Stegmann be liable for any 
 * direct, indirect, incidental, special, exemplary, or consequential damages
 * (including, but not limited to, procurement of substitute goods or services;
 * loss of use, data, or profits; or business interruption) however caused and 
 * on any theory of liability, whether in contract, strict liability, or tort 
 * (including negligence or otherwise) arising in any way out of the use of 
 * this software, even if advised of the possibility of such damage.
 * 
 * 
 * 
 *
 * $Revision: 1.4 $ 
 * $Date: 2003/04/23 14:49:15 $ 
 * 
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
