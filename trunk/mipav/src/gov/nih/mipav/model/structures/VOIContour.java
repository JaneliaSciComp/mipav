package gov.nih.mipav.model.structures;

import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.view.MipavUtil;



import java.awt.Polygon;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import Jama.Matrix;
import WildMagic.LibFoundation.Approximation.ApprEllipsoidFit3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/**
 * This class is fundamental to the VOI class in which points are stored that
 * describe a curve of an VOI. The points are 3D and are floats (see Vector3f).
 * It extends VOIBase and therefore it extends Vector. Vector makes it very easy
 * to add points and remove points from the contour. An VOI is formed from one
 * or many contours.
 * 
 * <p>
 * Routines that wish to sample all integer x,y values along a line must sample
 * the line at 1/2 unit increments. As an example, suppose we have a line with
 * the following (x,y) values: (0.2,0.7),(0.7,1.2),(1.2,1.7), (1.7,2.2). If we
 * sample at unit increments with rounding we could either obtain the points
 * (0,1),(1,2) or (1,1),(2,2). By sampling at 1/2 unit increments along the line
 * we sample at (0,1),(1,1),(1,2),(2,2).
 * </p>
 * 
 * @version 0.1 Oct 27, 1997
 * @author Matthew J. McAuliffe, Ph.D.
 * @see VOIBase
 * @see VOILine
 * @see VOI
 * @see VOIPoint
 * 
 * <p>
 * $Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIContour.java $
 * $Revision: 114 $ $Date: 2/24/06 3:34p $
 * </p>
 */
public class VOIContour extends VOIBase {

	// ~ Static fields/initializers
	// -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = -6479418081230834617L;

	/**
	 * Construct a new closed or open VOI.
	 * @param flag whether the voi contour is closed
	 */
	public VOIContour(boolean flag) {
		super();
		setClosed(flag);
	}

	/**
	 * Construct a new VOIContour.
	 * @param bFixed, true if this VOIContour is fixed (cannot be changed).
	 * @param bClosed, true if this VOIContour is closed.
	 */
	public VOIContour( boolean bFixed, boolean bClosed  )
	{
		super( bFixed, bClosed );
	}

	/**
	 * Construct a new VOIContour.
	 * @param bFixed, true if this VOIContour is fixed (cannot be changed).
	 * @param bClosed, true if this VOIContour is closed.
	 * @param kLocal
	 */
	public VOIContour( boolean bFixed, boolean bClosed, Vector<Vector3f> kPositions )
	{
		super( bFixed, bClosed, kPositions );
	}

	/**
	 * Copy Constructor.
	 * @param kVOI
	 */
	public VOIContour( VOIContour kVOI )
	{
		super(kVOI);
	}

	/**
	 * Copies the input contour and changes it's slice +/-
	 * @param kBase
	 * @param iPropDir
	 */
	public VOIContour( VOIContour kVOI, int iPropDir )
	{
		super(kVOI, iPropDir);
	}


	/**
	 * Copies the input VOIBase, transformed by the input TransMatrix.
	 * @param kBase VOI to copy.
	 * @param tMatrix transformation
	 */
	public VOIContour( VOIContour kVOI, TransMatrix tMatrix )
	{
		super(kVOI, tMatrix);
	}

	/**
	 * Calculate the distance of the largest line segment contained entirely
	 * within the slice of the VOI
	 * 
	 * @param xRes
	 * @param yRes
	 * @return largestDistance
	 */
	public double calcLargestSliceDistance(int[] extents, float[] res, Vector3f kPos1, Vector3f kPos2) {

		float[] xPts = new float[size()];
		float[] yPts = new float[size()];
		float[] zPts = new float[size()];
		int size = size();
		for (int i = 0; i < size; i++) {
			xPts[i] = elementAt(i).X;
			yPts[i] = elementAt(i).Y;
			zPts[i] = elementAt(i).Z;
		}

		int xDim = extents.length > 0 ? extents[0] : 1;
		int yDim = extents.length > 1 ? extents[1] : 1;
		int zDim = extents.length > 2 ? extents[2] : 1;
		BitSet mask = new BitSet(xDim*yDim*zDim); 
		setMask( mask, xDim, yDim, false, VOI.ADDITIVE );     
		return VOI.calcLargestDistance( mask, extents, res[0], res[1], res[2], xPts, yPts, zPts, kPos1, kPos2 );		
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.model.structures.VOIBase#clone()
	 */
	@Override
	public VOIBase clone() {
		return new VOIContour(this);
	}
	
	/* Code ported from http://cm.bell-labs.com/who/clarkson/2dch.c to
	 * Java in public void convexHull() and its supporting routines by William Gandler
	 * Original notices follow.
	 */
	
	/*
	 * Ken Clarkson wrote this.  Copyright (c) 1996 by AT&T..
	 * Permission to use, copy, modify, and distribute this software for any
	 * purpose without fee is hereby granted, provided that this entire notice
	 * is included in all copies of any software which is or includes a copy
	 * or modification of this software and in all copies of the supporting
	 * documentation for such software.
	 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
	 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
	 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
	 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
	 */


	/*
	 * two-dimensional convex hull
	 * read points from stdin,
	 *      one point per line, as two numbers separated by whitespace
	 * on stdout, points on convex hull in order around hull, given
	 *      by their numbers in input order
	 * the results should be "robust", and not return a wildly wrong hull,
	 *  despite using floating point
	 * works in O(n log n); I think a bit faster than Graham scan;
	 *  somewhat like Procedure 8.2 in Edelsbrunner's "Algorithms in Combinatorial
	 *  Geometry", and very close to:
	 *      A.M. Andrew, "Another Efficient Algorithm for Convex Hulls in Two Dimensions",
	 *      Info. Proc. Letters 9, 216-219 (1979)
	 *  (See also http://geometryalgorithms.com/Archive/algorithm_0110/algorithm_0110.htm)
	 */

	public void convexHull() {
	    int n;
	    double points[][];
	    double P[][];
	    int i;
	    int u;
	    int u2;
	    Vector3f pt;
	    
	    n = size();
	    
	    if (n == 3) {
	        // Contour is a triangle.  All triangles are convex.
	        return;
	    }
	    
	    points = new double[n][2];
	    P = new double[n+1][];
	    for (i = 0; i < n; i++) {
	        points[i][0] = elementAt(i).X;
	        points[i][1] = elementAt(i).Y;
	        P[i] = points[i];
	    } // for (i = 0; i < n; i++)
	    
	    // Make lower hull
        u = make_chain(P, 0, n, true);
        P[n] = P[0];
        // Make upper hull
        u2 = make_chain(P, u, n-u+1, false);
        float z = elementAt(0).Z;
        removeAllElements();
        for (i = 0; i < u + u2; i++) {
            pt = new Vector3f((float)P[i][0], (float)P[i][1], z);
            insertElementAt(pt, i);
        }
	}
	
	private int make_chain(double[][] V, int offset, int n, boolean lower) {
	    int i;
	    int j;
	    int s = offset + 1;
	    double t[];
	    ArrayList<positionItem> VList;
	    VList = new ArrayList<positionItem>();
	    
	    for (i = offset; i < offset+n; i++) {
	        VList.add(new positionItem(V[i][0], V[i][1]));
	    }
	    
	    if (lower) {
	        Collections.sort(VList, new positionLowerComparator());
	    }
	    else {
	        Collections.sort(VList, new positionUpperComparator());    
	    }
	    
	    for (i = offset; i < offset+n; i++) {
	        V[i][0] = VList.get(i-offset).getPositionX();
	        V[i][1] = VList.get(i-offset).getPositionY();
	    }
	    
	    for (i = offset + 2; i < offset+n; i++) {
	        for (j = s; j >= offset + 1 && ccw(V, i, j, j-1); j--){}
	        s = j+1;
	        t = V[s];
	        V[s] = V[i];
	        V[i] = t;
	    } // for (i = offset + 2; i < n; i++)
	    
	    return (s-offset);
	}
	
	private boolean ccw(double P[][], int i, int j, int k) {
	    double a;
	    double b;
	    double c;
	    double d;
	    
	    a = P[i][0] - P[j][0];
	    b = P[i][1] - P[j][1];
	    c = P[k][0] - P[j][0];
	    d = P[k][1] - P[j][1];
	    // true if points i, j, k counterclockwise
	    return ((a*d - b*c) <= 0.0);
	}
	
	private class positionLowerComparator implements Comparator<positionItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final positionItem o1, final positionItem o2) {
            final double ax = o1.getPositionX();
            final double bx = o2.getPositionX();
            final double ay = o1.getPositionY();
            final double by = o2.getPositionY();
            double v;

            v = ax - bx;
            if (v > 0) {
                return 1;
            }
            if (v < 0) {
                return -1;
            }
            
            v = by - ay;
            if (v > 0) {
                return 1;
            }
            if (v < 0) {
                return -1;
            }
            return 0;
        }

    }
	
	private class positionUpperComparator implements Comparator<positionItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final positionItem o1, final positionItem o2) {
            final double ax = o1.getPositionX();
            final double bx = o2.getPositionX();
            final double ay = o1.getPositionY();
            final double by = o2.getPositionY();
            double v;

            v = bx - ax;
            if (v > 0) {
                return 1;
            }
            if (v < 0) {
                return -1;
            }
            
            v = ay - by;
            if (v > 0) {
                return 1;
            }
            if (v < 0) {
                return -1;
            }
            return 0;
        }

    }
	
	private class positionItem {

        /** DOCUMENT ME! */
        private final double positionX;

        /** DOCUMENT ME! */
        private final double positionY;

        /**
         * Creates a new positionItem object.
         * 
         * @param positionX
         * @param positionY
         */
        public positionItem(final double positionX, final double positionY) {
            this.positionX = positionX;
            this.positionY = positionY;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getPositionX() {
            return positionX;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getPositionY() {
            return positionY;
        }

    }


	

	/**
	 * Return the VOI crop region's origin of rectangle.
	 * 
	 * @param fileInfo
	 *            DOCUMENT ME!
	 * @param dim
	 *            DOCUMENT ME!
	 * @param originX
	 *            DOCUMENT ME!
	 * @param originY
	 *            DOCUMENT ME!
	 * @param resols
	 *            DOCUMENT ME!
	 * 
	 * @return origin rectangle origin
	public float[] getOrigin(FileInfoBase fileInfo, int dim, float originX,
			float originY, float[] resols) {
		int nDims = dim;
		int direction;
		float temp;
		float[] origin = new float[3];

		for (int i = 0; i < nDims; i++) {
			direction = 1;

			switch (fileInfo.getAxisOrientation()[i]) {

			case FileInfoBase.ORI_L2R_TYPE:
				direction = -1;

			case FileInfoBase.ORI_R2L_TYPE:
				temp = originX + (direction * resols[0] * xBounds[0]);
				origin[0] = temp;
				break;

			case FileInfoBase.ORI_S2I_TYPE:
				direction = -1;

			case FileInfoBase.ORI_I2S_TYPE:
				temp = originY + (direction * resols[1] * yBounds[0]);
				origin[1] = temp;
				break;

			case FileInfoBase.ORI_P2A_TYPE:
				direction = -1;

			case FileInfoBase.ORI_A2P_TYPE:
				temp = originX + (direction * resols[0] * xBounds[0]);
				origin[2] = temp;
				break;
			}
		}

		for (int i = 0; i < nDims; i++) {

			switch (fileInfo.getAxisOrientation()[i]) {

			case FileInfoBase.ORI_A2P_TYPE:
				origin[0] = origin[2];
			}
		}

		return origin;
	}
	 */


	/**
	 * Exports the points of the contour as a polygon (z data can not be encoded
	 * in the polygon).
	 * 
	 * @return returns polygon
	 */
	public Polygon exportPolygon() {
		int i;
		int x, y;
		Polygon gon = null;

		try {
			gon = new Polygon();
		} catch (OutOfMemoryError error) {
			System.gc();
			throw error;
		}

		for (i = 0; i < size(); i++) {
			x = (int) (((elementAt(i))).X + 0.5);
			y = (int) (((elementAt(i))).Y + 0.5);
			gon.addPoint(x, y);
		}

		return gon;
	}

	/**
	 * Gets position/intensity along the boundary of this contour VOI.
	 * 
	 * @param position
	 *            array that is filled with all x,y coordinates
	 * @param intensity
	 *            the corresponding intensities along the line
	 * @param imageBuffer
	 *            image array in which VOIs and lines are found
	 * @param xDim
	 *            x-Dimension of image
	 * 
	 * @return the number of points in the position and intensity array that
	 *         have valid data.
	 */
	public int getPositionAndIntensity(Vector3f[] position, float[] intensity,
			float[] imageBuffer, int xDim) {
		int i, j, end, pt;
		int index, indexX = 0, indexY = 0;
		double myY, myX, yInc, xInc;
		double x0, x1, y0, y1;
		double distance;
		int len;
		end = size() - 1;
		pt = 0;
		i = 0;
		len = 0;

		for (j = 0; j < end; j++) {
			x0 = ((elementAt(j))).X;
			y0 = ((elementAt(j))).Y;
			x1 = ((elementAt(j + 1))).X;
			y1 = ((elementAt(j + 1))).Y;
			distance = Math.sqrt(((x1 - x0) * (x1 - x0))
					+ ((y1 - y0) * (y1 - y0)));
			myX = x0;
			myY = y0;
			xInc = (x1 - x0) / (distance * 2);
			yInc = (y1 - y0) / (distance * 2);
			len = (int) (2 * distance);

			for (i = 0; i < len; i++) {

				if ((indexX != MipavMath.round(myX))
						|| (indexY != Math.round(myY))) {
					indexY = MipavMath.round(myY);
					indexX = MipavMath.round(myX);
					position[pt].X = indexX;
					position[pt].Y = indexY;
					index = (indexY * xDim) + indexX;

					// position[pt] = index;
					intensity[pt] = imageBuffer[index];
					pt++;
				}

				myX = myX + xInc;
				myY = myY + yInc;
			}
		}

		if (closed == true) {
			x0 = ((elementAt(size() - 1))).X;
			y0 = ((elementAt(size() - 1))).Y;
			x1 = ((elementAt(0))).X;
			y1 = ((elementAt(0))).Y;
			distance = Math.sqrt(((x1 - x0) * (x1 - x0))
					+ ((y1 - y0) * (y1 - y0)));
			myX = x0;
			myY = y0;
			xInc = (x1 - x0) / (2 * distance);
			yInc = (y1 - y0) / (2 * distance);
			len = MipavMath.round(2 * distance);

			for (i = 0; i < len; i++) {

				if ((indexX != MipavMath.round(myX))
						|| (indexY != Math.round(myY))) {
					indexY = MipavMath.round(myY);
					indexX = MipavMath.round(myX);
					position[pt].X = indexX;
					position[pt].Y = indexY;
					index = (indexY * xDim) + indexX;

					// position[pt] = index;
					intensity[pt] = imageBuffer[index];
					pt++;
				}

				myX = myX + xInc;
				myY = myY + yInc;
			}
		}

		return pt;
	}

	/**
	 * Gets the position/intensity along the boundary of this contour VOI.
	 * 
	 * @param position
	 *            array that is filled with all the indices in the form of
	 *            indexY*xDim + indexX
	 * @param intensity
	 *            the corresponding intensities along the line
	 * @param imageBuffer
	 *            image array in which the VOis and lines are found
	 * @param xDim
	 *            x-Dimension of image
	 * 
	 * @return the number of points in the position and intensity array that
	 *         hava valid data.
	 */
	public int getPositionAndIntensityIndex(int[] position, float[] intensity,
			float[] imageBuffer, int xDim) {
		int i, j, end, pt;
		int index, indexX = 0, indexY = 0;
		double myY, myX, yInc, xInc;
		double x0, x1, y0, y1;
		double distance;
		int len;
		end = size() - 1;
		pt = 0;
		i = 0;
		len = 0;

		for (j = 0; j < end; j++) {
			x0 = ((elementAt(j))).X;
			y0 = ((elementAt(j))).Y;
			x1 = ((elementAt(j + 1))).X;
			y1 = ((elementAt(j + 1))).Y;
			distance = Math.sqrt(((x1 - x0) * (x1 - x0))
					+ ((y1 - y0) * (y1 - y0)));
			myX = x0;
			myY = y0;
			xInc = (x1 - x0) / (2 * distance);
			yInc = (y1 - y0) / (2 * distance);
			len = (int) (2 * distance);

			for (i = 0; i < len; i++) {

				if ((indexX != MipavMath.round(myX))
						|| (indexY != Math.round(myY))) {
					indexY = MipavMath.round(myY);
					indexX = MipavMath.round(myX);
					index = (indexY * xDim) + indexX;
					position[pt] = index;
					intensity[pt] = imageBuffer[index];
					pt++;
				}

				myX = myX + xInc;
				myY = myY + yInc;
			}
		}

		if (closed == true) {
			x0 = ((elementAt(size() - 1))).X;
			y0 = ((elementAt(size() - 1))).Y;
			x1 = ((elementAt(0))).X;
			y1 = ((elementAt(0))).Y;
			distance = Math.sqrt(((x1 - x0) * (x1 - x0))
					+ ((y1 - y0) * (y1 - y0)));
			myX = x0;
			myY = y0;
			xInc = (x1 - x0) / (2 * distance);
			yInc = (y1 - y0) / (2 * distance);
			len = MipavMath.round(distance);

			for (i = 0; i < len; i++) {

				if ((indexX != MipavMath.round(myX))
						|| (indexY != Math.round(myY))) {
					indexY = MipavMath.round(myY);
					indexX = MipavMath.round(myX);
					index = (indexY * xDim) + indexX;
					position[pt] = index;
					intensity[pt] = imageBuffer[index];
					pt++;
				}

				myX = myX + xInc;
				myY = myY + yInc;
			}
		}

		return pt;
	}

	/**
	 * Saves the polygon in the contour.
	 * 
	 * @param gon
	 *            polygon to be saved
	 * @param slice
	 *            index to save the polygon at
	 */
	public void importPolygon(Polygon gon, int slice) {
		int i;

		try {
			this.removeAllElements();

			for (i = 0; i < gon.npoints; i++) {
				this.addElement(new Vector3f(gon.xpoints[i], gon.ypoints[i],
						slice));
			}
		} catch (OutOfMemoryError error) {
			System.gc();
			throw error;
		}
	}

	/**
	 * Determines if the points are ordered in a counterClockwise manner.
	 * 
	 * @return true if points are counter clockwise and false if clockwise.
	 */
	public boolean isCounterClockwise() {
		int length = size();
		int k = 0;
		int i;
		int prev, next;
		float vAx, vAy, vBx, vBy, crossProd;

		for (i = 1; i < length; i++) {

			if ((((elementAt(i))).X <= ((elementAt(k))).X)
					&& ((((elementAt(i))).X < ((elementAt(k))).X) || (((elementAt(i))).Y < ((elementAt(k))).Y))) {
				k = i;
			}
		}

		// k is an index to a convex vertex.
		prev = k - 1;
		next = k + 1;

		if (prev == -1) {
			prev = length - 1;
		}

		if (next == length) {
			next = 0;
		}

		// Form two vectors
		vAx = ((elementAt(k))).X - ((elementAt(prev))).X;
		vAy = ((elementAt(k))).Y - ((elementAt(prev))).Y;
		vBx = ((elementAt(next))).X
		- ((elementAt(prev))).X;
		vBy = ((elementAt(next))).Y
		- ((elementAt(prev))).Y;

		// calc cross product 2*area if CCW or -2*area if CW
		crossProd = (vAx * vBy) - (vAy * vBx);

		if (crossProd > 0) {
			return false;
		}
		return true;
	}

	/**
	 * Make contour clockwise.
	 */
	public void makeClockwise() {
		int i;
		Vector3f obj;
		int size;

		if (isCounterClockwise() == false) {
			return;
		}

		// System.out.println("Making clockwise");
		size = size();

		for (i = 0; i < size; i++) {
			obj = elementAt(i);
			insertElementAt(obj, 0);
			removeElementAt(i + 1);
		}
	}

	/**
	 * Checks to see if Contour is counter-clockwise. If not it makes the
	 * contour counter-clockwise.
	 */
	public void makeCounterClockwise() {
		int i;
		Vector3f obj;
		int size;

		if (isCounterClockwise() == true) {
			return;
		}

		size = size();

		for (i = 0; i < size; i++) {
			obj = elementAt(i);
			insertElementAt(obj, 0);
			removeElementAt(i + 1);
		}
	}

	/**
	 * Checks to see if Contour is counter-clockwise. If not it makes the
	 * contour counter-clockwise.
	 * @param start, the index of the first point.
	 */
	public void makeCounterClockwise(int start) {
		int i;
		Vector3f obj;

		if (!isCounterClockwise())
			makeCounterClockwise();

		Vector<Vector3f> test = new Vector<Vector3f>();

		for (i = start; i >= 0; i--) {
			obj = elementAt(i);
			test.add(obj);
			removeElementAt(i);
		}

		for (i = test.size() - 1; i >= 0; i--) {
			insertElementAt(test.elementAt(i), size());
		}

	}

	/**
	 * Reloads points in this array for speed purposes.
	 * @param xPts loads the x-values into the xPts array.
	 * @param yPts loads the y-values into the yPts array.
	 */
	public void reloadPoints( float[] xPts, float[] yPts ) {
		int nPts = size();

		// reloads points in this array for speed purposes
		if ((yPts == null) || (xPts == null) || (size() != xPts.length)) {
			System.err.println("contains : xPts.len = " + xPts.length + " yPts.len = " + yPts.length);
		}

		for (int i = 0; i < nPts; i++) {
			xPts[i] = ((elementAt(i))).X;
			yPts[i] = ((elementAt(i))).Y;
		}
	}



	/**
	 * Forget about the last clicked (active) point in the contour.
	 */
	public void removeActivePt() {

		if ((nearPoint == NOT_A_POINT) && (lastPoint >= 0)
				&& (lastPoint < size())) {
			removeElementAt(lastPoint);
			lastPoint = 0;
		}
	}

	/**
	 * Find the least squares fit of the points in the contour to an ellipsoid.
	 * Uses a variant of Powell's algorithm to find the minimum of the 9D function:
	 * center x,y,z, rotation x,y,z, and axis x,y,z.
	 * @param kImage input image
	 * @param pAxis output principal axis
	 * @param eccentricity output eccentricity
	 * @param majorAxis output major axis length
	 * @param minorAxis output minor axis length
	 */
	public void secondOrderAttributes( ModelImage kImage, float[] pAxis,
			float[] eccentricity, float[] majorAxis, float[] minorAxis) {  	    

		Vector<Vector3f> kMaskPositions = getAllContourPoints();
		int nPts = kMaskPositions.size();
		
		float xRes = kImage.getResolutions(0).length > 0 ? kImage.getResolutions(0)[0] : 1;
		float yRes = kImage.getResolutions(0).length > 1 ? kImage.getResolutions(0)[1] : 1;
		float zRes = kImage.getResolutions(0).length > 2 ? kImage.getResolutions(0)[2] : 1;
		
		
		double m10 = 0.0;
		double m01 = 0.0;

		if ( m_iPlane == NOT_A_PLANE )
		{
			m_bUpdatePlane = true;
			getPlane();
		}
		if ( m_iPlane != NOT_A_PLANE )
		{
			for ( int i = 0; i < nPts; i++ )
			{
				if ( (m_iPlane&ZPLANE) == ZPLANE )
				{
					//if (xUnits == yUnits) {
						m10 += kMaskPositions.elementAt(i).X * xRes;
						m01 += kMaskPositions.elementAt(i).Y * yRes;
					//} else {
					//	m10 += m_kMaskPositions.elementAt(i).X;
					//	m01 += m_kMaskPositions.elementAt(i).Y;
					//}        	
				}
				else if ( (m_iPlane&XPLANE) == XPLANE )
				{
					//if (yUnits == zUnits) {
						m10 += kMaskPositions.elementAt(i).Y * yRes;
						m01 += kMaskPositions.elementAt(i).Z * zRes;
					//} else {
					//	m10 += m_kMaskPositions.elementAt(i).Y;
					//	m01 += m_kMaskPositions.elementAt(i).Z;
					//}        	
				}
				else if ( (m_iPlane&YPLANE) == YPLANE )
				{
					//if (xUnits == zUnits) {
						m10 += kMaskPositions.elementAt(i).X * xRes;
						m01 += kMaskPositions.elementAt(i).Z * zRes;
					//} else {
					//	m10 += m_kMaskPositions.elementAt(i).X;
					//	m01 += m_kMaskPositions.elementAt(i).Z;
					//}        	
				}
			}        

			m10 = m10 / nPts;
			m01 = m01 / nPts;

			double m20 = 0.0;
			double m11 = 0.0;
			double m02 = 0.0;
			double xdiff = 0;
			double ydiff = 0;

			for ( int i = 0; i < nPts; i++ )
			{
				if ( (m_iPlane&ZPLANE) == ZPLANE )
				{
					//if (xUnits == yUnits) {
						xdiff = (kMaskPositions.elementAt(i).X * xRes) - m10;
						ydiff = (kMaskPositions.elementAt(i).Y * yRes) - m01;
					//} else {
					//	xdiff = m_kMaskPositions.elementAt(i).X - m10;
					//	ydiff = m_kMaskPositions.elementAt(i).Y - m01;
					//}        	
				}
				else if ( (m_iPlane&XPLANE) == XPLANE )
				{
					//if (yUnits == zUnits) {
						xdiff = (kMaskPositions.elementAt(i).Y * yRes) - m10;
						ydiff = (kMaskPositions.elementAt(i).Z * zRes) - m01;
					//} else {
					//	xdiff = m_kMaskPositions.elementAt(i).Y - m10;
					//	ydiff = m_kMaskPositions.elementAt(i).Z - m01;
					//}        	
				}
				else if ( (m_iPlane&YPLANE) == YPLANE )
				{
					//if (xUnits == zUnits) {
						xdiff = (kMaskPositions.elementAt(i).X * xRes) - m10;
						ydiff = (kMaskPositions.elementAt(i).Z * zRes) - m01;
					//} else {
					//	xdiff = m_kMaskPositions.elementAt(i).X - m10;
					//	ydiff = m_kMaskPositions.elementAt(i).Z - m01;
					//}        	
				}
				m20 += xdiff * xdiff;
				m11 += xdiff * ydiff;
				m02 += ydiff * ydiff;
			}        

			m20 = (m20 / nPts);
			m11 = (m11 / nPts);
			m02 = (m02 / nPts);

			// The eigenvalues of m20 m11
			// m11 m02
			// are proportional to the square of the semiaxes
			// (m20 - e)*(m02 - e) - m11*m11 = 0;
			double root = Math.sqrt(((m20 - m02) * (m20 - m02)) + (4 * m11 * m11));
			double ma = Math.sqrt(2.0 * (m20 + m02 + root));
			double mi = Math.sqrt(2.0 * (m20 + m02 - root));

			double areaEllipse = (Math.PI / 4.0) * ma * mi;
			double normFactor = Math.sqrt(nPts / areaEllipse);

			if ( (m_iPlane&ZPLANE) == ZPLANE )
			{
				//if (xUnits == yUnits) {
					normFactor = Math.sqrt(xRes * yRes * nPts / areaEllipse);
				//}			
			}
			else if ( (m_iPlane&XPLANE) == XPLANE )
			{
				//if (yUnits == zUnits) {
					normFactor = Math.sqrt(yRes * zRes * nPts / areaEllipse);
				//} 				
			}
			else if ( (m_iPlane&YPLANE) == YPLANE )
			{
				//if (xUnits == zUnits) {
					normFactor = Math.sqrt(xRes * zRes * nPts / areaEllipse);
				//}				
			}

			ma = normFactor * ma;
			majorAxis[0] = (float)ma;
			mi = normFactor * mi;
			minorAxis[0] = (float)mi;
			eccentricity[0] = (float) Math.sqrt(1.0 - ((mi * mi) / (ma * ma)));

			pAxis[0] = (float) ((180.0 / Math.PI) * 0.5 * Math.atan2((2.0 * m11),
					(m20 - m02)));

			return;
		}

		// The contour is 3-dimensional (not entirely on one of the three orthogonal planes):
        Vector3f kUp = new Vector3f();
        Matrix3f kMat = new Matrix3f();
        float[] afScale = new float[3];

        // First translate into LPS Coordinates then calculate fit
        Vector<Vector3f> kLPSPoints = new Vector<Vector3f>();
        for ( int i = 0; i < size(); i++ )
        {
            Vector3f kLPS = new Vector3f(elementAt(i));
            kLPS.scale( xRes, yRes, zRes );
            kLPSPoints.add( kLPS );
        }

        // Try different Powell for fitting the points.
        new ApprEllipsoidFit3f( kLPSPoints.size(), kLPSPoints, kUp, kMat, afScale );

        double fMin = Float.MAX_VALUE;
        double fMax = -Float.MAX_VALUE;
        int iMax = 0;
        int iMin = 0;
        for ( int i = 0; i < 3; i++ )
        {
            if ( afScale[i] > fMax ) 
            {
                fMax = afScale[i];
                iMax = i;
            }
            if ( afScale[i] < fMin ) 
            {
                fMin = afScale[i];
                iMin = i;
            }
        }
        int iMid = 0;
        for ( int i = 0; i < 3; i++ )
        {
            if ( i != iMax && i != iMin )
            {
                iMid = i;
                break;
            }
        }

        //double volumeEllipse = (4.0 * Math.PI / 3.0) * afScale[2] * afScale[1] * afScale[0];
        double areaEllipse = (Math.PI / 4.0) * afScale[iMax] * afScale[iMid];
        double normFactor = Math.sqrt(nPts / areaEllipse);

        majorAxis[0] = (float) (normFactor * afScale[iMax]);
        minorAxis[0] = (float) (normFactor * afScale[iMid]);

        eccentricity[0] = (float) Math
        .sqrt(1.0 - ((afScale[iMid] * afScale[iMid]) / (afScale[iMax] * afScale[iMax])));


        Vector3f kX = new Vector3f( kMat.M00, kMat.M01, kMat.M02 );  kX.normalize();
        Vector3f kY = new Vector3f( kMat.M10, kMat.M11, kMat.M12 );  kY.normalize();
        Vector3f kZ = new Vector3f( kMat.M20, kMat.M21, kMat.M22 );  kZ.normalize();
        Vector3f[] kBasis = new Vector3f[]{ kX, kY, kZ };
        kMat = new Matrix3f( kX, kY, kZ, false );
        Vector3f kRot = kMat.mult( Vector3f.UNIT_Z_NEG );              
        pAxis[0] = (float)(Math.acos(kRot.dot(kBasis[iMax])) * 180f/Math.PI);
        
		/*
        Transformation kTransform = new Transformation();
        Vector3f kScale = new Vector3f(afScale[0]/2, afScale[1]/2, afScale[2]/2);
        kTransform.SetScale( kScale );
        kTransform.SetTranslate( kUp );
        kTransform.SetRotate( kMat );

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh kSphere = kSM.Sphere(64,64,1f);

        Vector3f center = getGeometricCenter();
        Vector<Vector3f> kPositions = new Vector<Vector3f>();
        for ( int i = 0; i < kSphere.VBuffer.GetVertexQuantity(); i++ )
        {
            Vector3f kPos = new Vector3f();
            kSphere.VBuffer.GetPosition3(i, kPos);
            kPositions.add(kTransform.ApplyForward(kPos));
            kPositions.elementAt(i).Z = center.Z;
        }
        VOIContour kVOI = new VOIContour( false, true, kPositions ); 
        short sID = (short)(kImage.getVOIs().size() + 1);
        String kName = getClass().getName();
        int index = kName.lastIndexOf('.') + 1;
        kName = kName.substring(index);
        VOI kGroup = new VOI( sID,  kName + "_" + sID, getType(), -1f );
        kGroup.importCurve(kVOI);
        kGroup.setColor( Color.blue );
        kImage.registerVOI(kGroup);
		 */

        

	}

	/**
	 * Finds the second order attributes.
	 * 
	 * <p>
	 * Reference: Fitzgibbon, Pilu, Fisher: Direct Least Squares Fitting of
	 * Ellipses at http://www.robots.ox.ac.uk/~awf/ellipse/fitellipse.html
	 * </p>
	 * 
	 * @param xRes
	 *            DOCUMENT ME!
	 * @param yRes
	 *            DOCUMENT ME!
	 * @param xUnits
	 *            DOCUMENT ME!
	 * @param yUnits
	 *            DOCUMENT ME!
	 * @param angleAxis
	 *            angle in radians with major axis
	 * @param eccentricity
	 *            shape (circle = 0; line = 1)
	 * @param majorAxis
	 *            diameter of major axis
	 * @param minorAxis
	 *            diameter of minor axis
	 * @param xCenter
	 * @param yCenter
	 */
	public void secondOrderAttributeslsq(float xRes, float yRes, int xUnits,
			int yUnits, double[] angleAxis, double[] eccentricity, double[] majorAxis,
			double[] minorAxis, double[] xCenter, double yCenter[]) {

		float[] xPts = new float[size()];
		float[] yPts = new float[size()];
		reloadPoints( xPts, yPts );

		int nPts = size();
		double mx = 0.0;
		double my = 0.0;
		double sx;
		double sy;
		double minX = Double.MAX_VALUE;
		double maxX = -Double.MAX_VALUE;
		double minY = Double.MAX_VALUE;
		double maxY = -Double.MAX_VALUE;
		int i, j, k;
		double[] x = new double[nPts];
		double[] y = new double[nPts];

		// Normalize data
		// It's important to scale the image coordinates to be near one before
		// running the algorithm or you'll get numerical instability
		if (xUnits == yUnits) {

			for (i = 0; i < nPts; i++) {
				mx += xPts[i] * xRes;
				my += yPts[i] * yRes;

				if ((xPts[i] * xRes) < minX) {
					minX = xPts[i] * xRes;
				}

				if ((xPts[i] * xRes) > maxX) {
					maxX = xPts[i] * xRes;
				}

				if ((yPts[i] * yRes) < minY) {
					minY = yPts[i] * yRes;
				}

				if ((yPts[i] * yRes) > maxY) {
					maxY = yPts[i] * yRes;
				}
			} // for (i = 0; i < nPts; i++)
		} // if (xUnits == yUnits)
		else {

			for (i = 0; i < nPts; i++) {
				mx += xPts[i];
				my += yPts[i];

				if (xPts[i] < minX) {
					minX = xPts[i];
				}

				if (xPts[i] > maxX) {
					maxX = xPts[i];
				}

				if (yPts[i] < minY) {
					minY = yPts[i];
				}

				if (yPts[i] > maxY) {
					maxY = yPts[i];
				}
			} // for (i = 0; i < nPts; i++)
		} // else

		mx = mx / nPts;
		my = my / nPts;
		sx = (maxX - minX) / 2.0;
		sy = (maxY - minY) / 2.0;

		if (xUnits == yUnits) {

			for (i = 0; i < nPts; i++) {
				x[i] = ((xPts[i] * xRes) - mx) / sx;
				y[i] = ((yPts[i] * yRes) - my) / sy;
			} // for (i = 0; i < nPts; i++)
		} // if (xUnits == yUnits)
		else {

			for (i = 0; i < nPts; i++) {
				x[i] = (xPts[i] - mx) / sx;
				y[i] = (yPts[i] - my) / sy;
			} // for (i = 0; i < nPts; i++)
		}

		// Build design array
		double[][] D = new double[nPts][6];

		for (i = 0; i < nPts; i++) {
			D[i][0] = x[i] * x[i];
			D[i][1] = x[i] * y[i];
			D[i][2] = y[i] * y[i];
			D[i][3] = x[i];
			D[i][4] = y[i];
			D[i][5] = 1.0;
		} // for (i = 0; i < nPts; i++)

		double[][] Dtrans = new double[6][nPts];

		for (i = 0; i < nPts; i++) {

			for (j = 0; j < 6; j++) {
				Dtrans[j][i] = D[i][j];
			}
		}

		// Build scatter array
		double[][] S = new double[6][6];

		for (i = 0; i < 6; i++) {

			for (j = 0; j < 6; j++) {

				for (k = 0; k < nPts; k++) {
					S[i][j] += Dtrans[i][k] * D[k][j];
				}
			}
		}

		double[][] tmpA = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {
				tmpA[i][j] = S[i][j];
			}
		}

		double[][] tmpB = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {
				tmpB[i][j] = S[i][j + 3];
			}
		}

		double[][] tmpC = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {
				tmpC[i][j] = S[i + 3][j + 3];
			}
		}

		double[][] tmpD = new double[3][3];
		tmpD[0][2] = -2;
		tmpD[1][1] = 1;
		tmpD[2][0] = -2;

		Matrix matC = new Matrix(tmpC);
		double[][] invC = matC.inverse().getArray();
		double[][] tmpBTrans = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {
				tmpBTrans[j][i] = tmpB[i][j];
			}
		}

		double[][] tmpE = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {

				for (k = 0; k < 3; k++) {
					tmpE[i][j] += invC[i][k] * tmpBTrans[k][j];
				}
			}
		}

		double[][] BE = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {

				for (k = 0; k < 3; k++) {
					BE[i][j] += tmpB[i][k] * tmpE[k][j];
				}
			}
		}

		double[][] ABE = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {
				ABE[i][j] = tmpA[i][j] - BE[i][j];
			}
		}

		Matrix matD = new Matrix(tmpD);
		double[][] invD = matD.inverse().getArray();
		double[][] DABE = new double[3][3];

		for (i = 0; i < 3; i++) {

			for (j = 0; j < 3; j++) {

				for (k = 0; k < 3; k++) {
					DABE[i][j] += invD[i][k] * ABE[k][j];
				}
			}
		}

		double[] eigenvalue = new double[DABE.length];
		double[][] eigenvector = new double[DABE.length][DABE.length];
		double temp;
		double[] tempCol = new double[3];
		int m, n, index;
		Eigenvalue.decompose( DABE, eigenvector, eigenvalue );

		// Arrange the eigenvalues and corresponding eigenvectors
		// in descending order so that e0 >= e1 >= e2
		for (m = 0; m < 3; m++) {
			index = m;

			for (n = m + 1; n < 3; n++) {

				if (eigenvalue[n] > eigenvalue[index]) {
					index = n;
				}
			} // for (m = m+1; n < 3; n++)

			if (index != m) {
				temp = eigenvalue[m];
				eigenvalue[m] = eigenvalue[index];
				eigenvalue[index] = temp;

				for (n = 0; n < 3; n++) {
					tempCol[n] = eigenvector[n][m];
					eigenvector[n][m] = eigenvector[n][index];
					eigenvector[n][index] = tempCol[n];
				}
			} // if (index != m)
		} // for (m = 0; m < 3; m++)

		// Place the eigenvector corresponding to the negative eigenvalue
		// in A
		double[] A = new double[3];

		for (i = 0; i < 3; i++) {
			A[i] = eigenvector[i][2];
		}

		// Recover the bottom half
		double[] evec_y = new double[3];

		for (i = 0; i < 3; i++) {

			for (k = 0; k < 3; k++) {
				evec_y[i] += -tmpE[i][k] * A[k];
			}
		}

		double[] Atemp = new double[3];

		for (i = 0; i < 3; i++) {
			Atemp[i] = A[i];
		}

		A = new double[6];

		for (i = 0; i < 3; i++) {
			A[i] = Atemp[i];
			A[i + 3] = evec_y[i];
		}

		// Unnormalize
		double[] par = new double[6];
		par[0] = A[0] * sy * sy;
		par[1] = A[1] * sx * sy;
		par[2] = A[2] * sx * sx;
		par[3] = (-2 * A[0] * sy * sy * mx) - (A[1] * sx * sy * my)
		+ (A[3] * sx * sy * sy);
		par[4] = (-A[1] * sx * sy * mx) - (2 * A[2] * sx * sx * my)
		+ (A[4] * sx * sx * sy);
		par[5] = (A[0] * sy * sy * mx * mx) + (A[1] * sx * sy * mx * my)
		+ (A[2] * sx * sx * my * my) - (A[3] * sx * sy * sy * mx)
		- (A[4] * sx * sx * sy * my) + (A[5] * sx * sx * sy * sy);

		// Convert to geometric radii, and centers
		angleAxis[0] = 0.5 * Math.atan2(par[1], (par[0] - par[2]));

		double cost = Math.cos(angleAxis[0]);
		double sint = Math.sin(angleAxis[0]);
		double sin_squared = sint * sint;
		double cos_squared = cost * cost;
		double cos_sin = sint * cost;
		double Ao = par[5];
		double Au = (par[3] * cost) + (par[4] * sint);
		double Av = (-par[3] * sint) + (par[4] * cost);
		double Auu = (par[0] * cos_squared) + (par[2] * sin_squared)
		+ (par[1] * cos_sin);
		double Avv = (par[0] * sin_squared) + (par[2] * cos_squared)
		- (par[1] * cos_sin);
		double tuCentre = -Au / (2.0 * Auu);
		double tvCentre = -Av / (2.0 * Avv);
		double wCentre = Ao - (Auu * tuCentre * tuCentre)
		- (Avv * tvCentre * tvCentre);

		xCenter[0] = tuCentre * cost - tvCentre * sint;
		yCenter[0] = tuCentre * sint + tvCentre * cost;
		double Ru = -wCentre / Auu;
		double Rv = -wCentre / Avv;
		Ru = Math.sqrt(Math.abs(Ru));
		Rv = Math.sqrt(Math.abs(Rv));

		if (Ru >= Rv) {
			majorAxis[0] = 2.0 * Ru;
			minorAxis[0] = 2.0 * Rv;
		} else {
			majorAxis[0] = 2.0 * Rv;
			minorAxis[0] = 2.0 * Ru;

			if (angleAxis[0] <= 0.0f) {
				angleAxis[0] = angleAxis[0] + Math.PI/2.0;
			} else {
				angleAxis[0] = angleAxis[0] - Math.PI/2.0;
			}
		}
		eccentricity[0] = Math.sqrt(1.0 - ((minorAxis[0] * minorAxis[0]) / (majorAxis[0] * majorAxis[0])));
	}
	
	/**
	 * This is a port of the MATLAB file Reisduals_ellipse written by Hui Ma on May 24, 2010.  This code is covered by a
	 * BSD license.  On the MATLAB exchange it was listed under distance from points to an ellipse.  file ID #27708.
	 * The text at the front of the file is as follows:
	 * %   Projecting a given set of points onto an ellipse
%   and computing the distances from the points to the ellipse
%
%   This is a modified version of an iterative algorithm published by D. Eberly
%     Internet publication: "Distance from a point to an ellipse in 2D" (2004)
%                           Geometric Tools, LLC, www.geometrictools.com
%     Book publication: "3D Game Engine Design", 2nd edition.
%                       Morgan Kaufmann Publishers, San Francisco, CA, 2007.
%                              (see Section 14.13.1)
%
%   Input:  XY(n,2) is the array of coordinates of n points x(i)=XY(i,1), y(i)=XY(i,2)
%           ParG is a vector 5x1 of the ellipse parameters
%           ParG =  [Center(1:2), Axes(1:2), Angle]'
%                Center - the coordinates of the ellipse's center
%                Axes   - the axes (major, minor)
%                Angle  - the angle of tilt of the ellipse
%
%   Output:  RSS is the Residual Sum of Squares (the sum of squares of the distances)
%            XYproj is the array of coordinates of projections
%
%   The algorithm is proven to converge and reaches an accuracy of 7-8 significant digit
%   It takes 4-5 iterations per point, on average.

    input angle is in radians
    Calling routine should supply xyproj as a double[][] = new double[xy.size()][2]
*/
	public void residuals_ellipse(double residualSumOfSquares[], double[][]xyproj, Vector<Vector3f> xy, 
	                               double centerX, double centerY, double majorAxis, double minorAxis, double angle) {
	    int n;
	    double tolerance;
	    double phiall[] = null;
	    int i;
	    double diffx;
	    double diffy;
	    double a;
	    double b;
	    double aa;
	    double bb;
	    double tol_a;
	    double tol_b;
	    double tol_aa;
	    double s;
	    double c;
	    //double Q[][];
	    //Matrix matQ;
	    double xy0[][];
	    double xya[][];
	    double tini[];
	    double u;
	    double v;
	    double ua;
	    double vb;
	    int z1;
	    int z2;
	    double xproj;
	    double t;
	    int iter;
	    double taa = 0.0;
	    double tbb;
	    double pp1;
	    double pp2;
	    double f;
	    double fder;
	    double ratio;
	    double temp;
	    boolean selfTest = false;
	    
	    if (selfTest) {
	        double alpha;
	        double cosalpha;
	        double sinalpha;
	        double semiMajor;
	        double semiMinor;
	        float x;
	        float y;
	        centerX = -100.0;
	        centerY = 123.0;
	        majorAxis = 500.0;
	        semiMajor = majorAxis/2.0;
	        minorAxis = 300.0;
	        semiMinor = minorAxis/2.0;
	        angle = 0.0;
	        c = Math.cos(angle);
	        s = Math.sin(angle);
	        xy.removeAllElements();
	        for (i = 0; i < 720; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (float)(centerX + semiMajor * cosalpha * c - semiMinor * sinalpha * s);
                y = (float)(centerY + semiMajor * cosalpha * s + semiMinor * sinalpha * c);
                xy.add(new Vector3f(x, y, 0.0f));
            }  
	        xyproj = new double[xy.size()][2];
	    }
	
	    residualSumOfSquares[0] = 0.0;
	    n = xy.size();
	    tolerance = 1.0e-9;
	    
	    // First handling the circle case
	    if (Math.abs((majorAxis - minorAxis)/majorAxis) < tolerance) {
	        phiall = new double[n];
	        for (i = 0; i < n; i++) {
	            phiall[i] = Math.atan2((xy.get(i).Y - centerY),(xy.get(i).X - centerX));
	            xyproj[i][0] = majorAxis*Math.cos(phiall[i]) + centerX;
	            xyproj[i][1] = minorAxis*Math.sin(phiall[i]) + centerY;
	            diffx = xy.get(i).X - xyproj[i][0];
	            diffy = xy.get(i).Y - xyproj[i][1];
	            residualSumOfSquares[0] += (diffx*diffx + diffy*diffy);
	        } // for (i = 0; i < n; i++)
	        return;
	    } // if (Math.abs((majorAxis - minorAxis)/majorAxis) < tolerance)
	    
	    // Now dealing with proper ellipses
	    a = majorAxis;
	    b = minorAxis;
	    aa = a * a;
	    bb = b * b;
	    tol_a = tolerance * a;
	    tol_b = tolerance * b;
	    tol_aa = tolerance * aa;
	    
	    // Matrix Q for rotating the points and the ellipse to the canonical system
	    s = Math.sin(angle);
	    c = Math.cos(angle);
	    //Q = new double[2][2];
	    //Q[0][0] = c;
	    //Q[0][1] = -s;
	    //Q[1][0] = s;
	    //Q[1][1] = c;
	    //matQ = new Matrix(Q);
	    
	    // Data points in canonical coordinates
	    xy0 = new double[n][2];
	    xya = new double[n][2];
	    tini = new double[n];
	    for (i = 0; i < n; i++) {
	        xy0[i][0] = (xy.get(i).X - centerX)*c + (xy.get(i).Y - centerY)*s;
	        xy0[i][1] = (xy.get(i).X - centerX)*(-s) + (xy.get(i).Y - centerY)*c;
	        xya[i][0] = Math.abs(xy0[i][0]);
	        xya[i][1] = Math.abs(xy0[i][1]);
	        tini[i] = Math.max(a*(xya[i][0] - a), b*(xya[i][1] - b));
	    }
	    
	    // Main loop over the data points
	    for (i = 0; i < n; i++) {
	        u = xya[i][0];
	        v = xya[i][1];
	        ua = u * a;
	        vb = v * b;
	        
	        if (u == 0) {
	            z1 = 1;
	        }
	        else if (xy0[i][0] > 0) {
	            z1 = 1;
	        }
	        else if (xy0[i][0] == 0.0) {
	            z1 = 0;
	        }
	        else {
	            z1 = -1;
	        }
	        
	        if (v == 0) {
	            z2 = 1;
	        }
	        else if (xy0[i][1] > 0) {
	            z2 = 1;
	        }
	        else if (xy0[i][1] == 0.0) {
	            z2 = 0;
	        }
	        else {
	            z2 = -1;
	        }
	        
	        // Does the point lie on the minor axis?
	        if (u < tol_a) {
	            if (xy0[i][1] < 0.0) {
	                xyproj[i][0] = 0.0;
	                xyproj[i][1] = -b;
	            }
	            else {
	                xyproj[i][0] = 0.0;
	                xyproj[i][1] = b;
	            }
	            continue;
	        } // if (u < tol_a)
	        
	        // Does the point lie on the major axis?
	        if (v < tol_b) {
	            if (u < a - bb/a) {
	                xproj = aa * u/(aa - bb);
	                ratio = xproj/a;
	                xyproj[i][0] = z1*xproj;
	                xyproj[i][1] = z2 * b * Math.sqrt(1.0 - ratio*ratio);
	            }
	            else {
	                xyproj[i][0] = z1 * a;
	                xyproj[i][1] = 0.0;
	            }
	            continue;
	        } // if (v < tol_b)
	        
	        // Generic case: start the iterative procedure
	        t = tini[i];
	        for (iter = 1; iter <= 100; iter++) {
	            taa = t + aa;
	            tbb = t + bb;
	            ratio = ua/taa;
	            pp1 = ratio * ratio;
	            ratio = vb/tbb;
	            pp2 = ratio * ratio;
	            f = pp1 + pp2 - 1.0;
	            if (f < 0.0) {
	                break;
	            }
	            fder = 2.0 * (pp1/taa + pp2/tbb);
	            ratio = f/fder;
	            if (ratio < tol_aa) {
	                break;
	            }
	            t = t + ratio;
	        } // for (iter = 1; iter <= 100; iter++)
	        
	        // Compute the projection of the point onto the ellipse
	        xproj = xy0[i][0]*aa/taa;
	        xyproj[i][0] = xproj;
	        if (xy0[i][1] > 0.0) {
	            z1 = 1;
	        }
	        else if (xy0[i][1] == 0.0) {
	            z1 = 0;
	        }
	        else {
	            z1 = -1;
	        }
	        ratio = xproj/a;
	        xyproj[i][1] = z1 * b * Math.sqrt(1.0 - ratio*ratio);
	    } // for (i = 0; i < n; i++)
	    
	    // Rotate back to the original system
	    for (i = 0; i < n; i++) {
            temp = xyproj[i][0]*c + xyproj[i][1]*(-s);
            xyproj[i][1] = xyproj[i][0]*s + xyproj[i][1]*c;
            xyproj[i][0] = temp;
            xyproj[i][0] = xyproj[i][0] + centerX;
            xyproj[i][1] = xyproj[i][1] + centerY;
            diffx = xy.get(i).X - xyproj[i][0];
            diffy = xy.get(i).Y - xyproj[i][1];
            residualSumOfSquares[0] += (diffx*diffx + diffy*diffy);
        }
	    if (selfTest) {
	        System.out.println("resdiualSumOfSquares = " + residualSumOfSquares[0]);
	    }
	    return;
	}
	

	/**
	 * Transforms self.
	 * 
	 * @param tMatrix
	 *            transformation matrix
	 * @param doRound
	 *            if true round point coordinates to integers
	 */
	public void transformContour(TransMatrix tMatrix, boolean doRound) {
		int i;
		Vector3f point = null;

		try {
			point = new Vector3f();
		} catch (OutOfMemoryError error) {
			System.gc();
			throw error;
		}

		for (i = 0; i < size(); i++) {
			tMatrix.transformAsPoint3Df((elementAt(i)), point);

			if (doRound) {
				setElementAt(new Vector3f(MipavMath.round(point.X), MipavMath
						.round(point.Y), MipavMath.round(point.Z)), i);
			} else {
				setElementAt(new Vector3f(point.X, point.Y, point.Z), i);
			}
		}
	}

	/**
	 * Translates a VOI by some amount in x and y.
	 * 
	 * @param xT
	 *            translate this amount in the x direction
	 * @param yT
	 *            translate this amount in the y direction
	 */
	public void translate(float xT, float yT, float zT) {
		int i;

		for (i = 0; i < size(); i++) {
			((elementAt(i))).X += xT;
			((elementAt(i))).Y += yT;
			((elementAt(i))).Z += zT;
		}
	}




}
