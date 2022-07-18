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