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