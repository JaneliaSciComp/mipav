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
 * Triangle container with built-in hit test. This class is a simple triangle
 * structure defined by three indexes to a list of points. Included
 * functionality is a hit test.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMTriangle {

	/** The point vector from which the triangle is defined. */
	public Vector<CAAMPoint> m_pPoints = new Vector<CAAMPoint>();

	/** Index to point 1. */
	private int m_v1;

	/** Index to point 2. */
	private int m_v2;

	/** Index to point 3. */
	private int m_v3;

	private double m_dD;

	/**
	 * Constructor
	 * 
	 * @param _v1
	 *            first point index
	 * @param _v2
	 *            second point index
	 * @param _v3
	 *            third point index
	 * @param pPoints
	 *            point vector for triangle
	 */
	public CAAMTriangle(int _v1, int _v2, int _v3, Vector<CAAMPoint> pPoints) {

		m_v1 = _v1;
		m_v2 = _v2;
		m_v3 = _v3;
		m_pPoints = pPoints;

		Calc_dD();
	}

	/**
	 * Constructor
	 */
	public CAAMTriangle() {

	}

	/**
	 * Assignment operator
	 * 
	 * @param tri
	 *            triangle reference.
	 */
	public void assign(CAAMTriangle tri) {
		this.m_v1 = tri.m_v1;
		this.m_v2 = tri.m_v2;
		this.m_v3 = tri.m_v3;
		this.m_dD = tri.m_dD;

		this.m_pPoints.clear();
		int len = tri.m_pPoints.size();
		for (int i = 0; i < len; i++) {
			CAAMPoint temp = new CAAMPoint();
			temp.assign(tri.m_pPoints.get(i));
			this.m_pPoints.add(temp);
		}
	}

	/**
	 * Cache function.
	 */
	public void Calc_dD() {

		double x1, x2, x3, y1, y2, y3;

		x1 = m_pPoints.get(m_v1).x;
		x2 = m_pPoints.get(m_v2).x;
		x3 = m_pPoints.get(m_v3).x;
		y1 = m_pPoints.get(m_v1).y;
		y2 = m_pPoints.get(m_v2).y;
		y3 = m_pPoints.get(m_v3).y;

		m_dD = -x2 * y3 + x2 * y1 + x1 * y3 + x3 * y2 - x3 * y1 - x1 * y2;
	}

	/**
	 * Returns true if the point 'p' is inside the triangle.
	 * 
	 * @param p
	 *            The point to test
	 * @return True if the point is inside, otherwise false.
	 */
	public boolean IsInside(final CAAMPoint p) {

		double[] alpha = new double[1];
		double[] beta = new double[1];
		double[] gamma = new double[1];

		return IsInside(p, alpha, beta, gamma);
	}

	/**
	 * Performs a hit test on the point p. If p is inside -- the position of p
	 * relative to the triangle is returned.
	 * 
	 * The relative position is:
	 * 
	 * p = alpha*p1 + beta*p2 + gamma*p3
	 * 
	 * Where p1-3 is the three points of the triangle.
	 * 
	 * @param p
	 *            The point to test alpha Relative x1 position. beta Relative x2
	 *            position. gamma Relative x3 position.
	 * @return True if the point is inside, otherwise false.
	 */
	public boolean IsInside(final CAAMPoint p, double[] alpha, double[] beta,
			double[] gamma) {

		double x, y, x1, x2, x3, y1, y2, y3;
		boolean inSide;

		x = p.x;
		y = p.y;

		x1 = m_pPoints.get(m_v1).x;
		x2 = m_pPoints.get(m_v2).x;
		x3 = m_pPoints.get(m_v3).x;

		// perform bounding box test on x
		if (x < AAMdef.AAM_MIN3(x1, x2, x3) || x > AAMdef.AAM_MAX3(x1, x2, x3))
			return false;

		y1 = m_pPoints.get(m_v1).y;
		y2 = m_pPoints.get(m_v2).y;
		y3 = m_pPoints.get(m_v3).y;

		// perform bounding box test on y
		if (y < AAMdef.AAM_MIN3(y1, y2, y3) || y > AAMdef.AAM_MAX3(y1, y2, y3))
			return false;

		alpha[0] = -y * x3 + y3 * x - x * y2 + x2 * y - x2 * y3 + x3 * y2;
		beta[0] = y * x3 - x1 * y - x3 * y1 - y3 * x + x1 * y3 + x * y1;
		gamma[0] = x * y2 - x * y1 - x1 * y2 - x2 * y + x2 * y1 + x1 * y;

		inSide = alpha[0] >= 0.0 && alpha[0] <= m_dD && beta[0] >= 0.0
				&& beta[0] <= m_dD && gamma[0] >= 0.0 && gamma[0] <= m_dD;

		if (inSide) {

			alpha[0] /= m_dD;
			beta[0] /= m_dD;
			gamma[0] /= m_dD;
		}

		return inSide;
	}

	/**
	 * Returns the center point of the triangle.
	 * 
	 * @return center point
	 */
	public CAAMPoint CenterPoint() {

		CAAMPoint out = new CAAMPoint();

		out.x = (m_pPoints.get(m_v1).x + m_pPoints.get(m_v2).x + m_pPoints
				.get(m_v3).x) / 3.0;
		out.y = (m_pPoints.get(m_v1).y + m_pPoints.get(m_v2).y + m_pPoints
				.get(m_v3).y) / 3.0;

		return out;
	}

	/**
	 * Returns the area.
	 * 
	 * @return The area.
	 */
	public double Area() {

		// get points
		double x1, x2, x3, y1, y2, y3;
		x1 = m_pPoints.get(m_v1).x;
		x2 = m_pPoints.get(m_v2).x;
		x3 = m_pPoints.get(m_v3).x;
		y1 = m_pPoints.get(m_v1).y;
		y2 = m_pPoints.get(m_v2).y;
		y3 = m_pPoints.get(m_v3).y;

		// sidelengths
		double a = Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
		double b = Math.sqrt((x2 - x3) * (x2 - x3) + (y2 - y3) * (y2 - y3));
		double c = Math.sqrt((x3 - x1) * (x3 - x1) + (y3 - y1) * (y3 - y1));

		// perimeter
		double s = (a + b + c) / 2;

		// Return the area.
		//
		// This formula is attributed to Heron of Alexandria
		// but can be traced back to Archimedes :-)
		return Math.sqrt(s * (s - a) * (s - b) * (s - c));
	}

	/**
	 * Returns index to point 1.
	 * 
	 * @return index 1
	 */
	public int V1() {
		return m_v1;
	}

	/**
	 * Returns index to point 2.
	 * 
	 * @return index 2
	 */
	public int V2() {
		return m_v2;
	}

	/**
	 * Returns index to point 3.
	 * 
	 * @return index 3
	 */
	public int V3() {
		return m_v3;
	}

}