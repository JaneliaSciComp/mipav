package gov.nih.mipav.view.renderer.WildMagic.AAM;

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
 * This class performs delaunay triangulation.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMDelaunay extends CAAMObject {

	/**
	 * constructor
	 */
	public CAAMDelaunay() {

	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Generates a triangular mesh, where all triangles satisfies the Delaunay
	 * property.
	 * 
	 * @param s
	 *            The input shape one wants to triangulate.
	 * @param m
	 *            The output Delaunay mesh.
	 * @param bConcaveCleanUp
	 *            If true convex triangles of concave shapes are removed
	 *            (default false).
	 * 
	 */
	public static void MakeMesh(final CAAMShape s, CAAMMesh m,
			boolean bConcaveCleanUp) {

		// setup input structures for the c routine
		int nbPoints = s.NPoints();
		int nbTriangles = nbPoints * 2;
		int[] triangles;
		double[] xVec = new double[nbPoints];
		double[] yVec = new double[nbPoints];
		triangles = new int[3 * nbTriangles];
		double[] x = new double[1];
		double[] y = new double[1];

		assert (nbPoints > 0);

		for (int i = 0; i < nbPoints; i++) {

			s.GetPoint(i, x, y);
			xVec[i] = x[0];
			yVec[i] = y[0];
			m.Insert(new CAAMPoint(x[0], y[0]));
		}

		// int outTriangles = ATTDelaunay(xVec,yVec,nbPoints,triangles);
		// int outTriangles =
		// DelaunayTriangulation.ATTDelaunay(xVec,yVec,nbPoints,triangles);
		int[] numTriangles = new int[1];
		delaunay d = new delaunay();
		triangles = d.ATTDelaunay(xVec, yVec, nbPoints, numTriangles);

		int outTriangles = numTriangles[0];

		assert (outTriangles <= nbTriangles);

		for (int i = 0; i < 3 * outTriangles; i += 3) {

			m.Insert(new CAAMTriangle(triangles[i], triangles[i + 1],
					triangles[i + 2], m.Points()));
		}

		// deallocate
		xVec = null;
		yVec = null;
		triangles = null;

		// do clean up
		if (bConcaveCleanUp) {

			// clean up the mesh for concave triangles
			// produced by the Delaunay Triangulisation
			for (int i = 0; i < m.NTriangles(); i++) {

				CAAMTriangle t = m.Triangles().get(i);

				// check if the triangle belongs to the shape
				if (!s.IsInside(t.CenterPoint())) {

					// delete triangle
					m.Triangles().remove(t);

					// correct the counter
					--i;
				}
			}
		}
	}

}