package gov.nih.mipav.model.structures;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.view.Preferences;

import java.awt.Color;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Random;
import java.util.Vector;

import Jama.EigenvalueDecomposition;
import Jama.Matrix;
import WildMagic.LibFoundation.Approximation.ApprEllipsoidFit3f;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

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
	public double calcLargestSliceDistance(float xRes, float yRes) {

        float[] xPts = new float[size()];
        float[] yPts = new float[size()];
        reloadPoints( xPts, yPts );
        
		long time = System.currentTimeMillis();
		double avg = 0, max = 0;
		double[] maxAvg = determineMaxAvg(xRes, yRes, xPts, yPts);
		max = maxAvg[0];
		avg = maxAvg[1];

		Preferences.debug("Traverse points\n");
		time = System.currentTimeMillis();
		double a = 0, lowerBound = max, upperBound = Double.MAX_VALUE;
		int iter = 0;
		boolean terminal = false;
		while (a == 0 && !terminal) {
			ArrayList<Integer> orig = new ArrayList<Integer>();
			ArrayList<Integer> term = new ArrayList<Integer>();
			upperBound = lowerBound;
			lowerBound = Math
					.max(0, max
							- iter
							* (avg / Math.max(1, (int) Math.pow(xPts.length,
									0.4) - 4)));
			gatherBoundedPoints(orig, term, lowerBound - 1, upperBound, xRes,
					yRes, xPts, yPts);
            
            Preferences.debug("Completed points in "
                    + (System.currentTimeMillis() - time) + "\tsize: "
                    + orig.size() + "\n");
            time = System.currentTimeMillis();
            
			a = getLargest(orig, term, xRes, yRes, xPts, yPts);
			
			
            Preferences.debug("Completed points in "
                    + (System.currentTimeMillis() - time) + "\tsize: "
                    + orig.size() + "\n");
            time = System.currentTimeMillis();
			if (lowerBound == 0.0) {
				terminal = true;
			}
			iter++;
		}
		return a;
	}


	/**
	 * Forms the convexHull based on the set of points that defines this
	 * contour. The contour can be either be clockwise or counter-clockwise.
	 */
	public void convexHull() {
		int i, j, k;
		int length, start;
		//float vAx, vAy, vBx, vBy, crossProd;
		boolean flag;
		Vector3f tmpPt;
		boolean repeat = true;
		boolean ccw = isCounterClockwise(); // ?
		length = size();

		if (length == 3) {
			return;
		} // Contour is a triangle. All triangles are convex.

		start = length / 4;

		if (start < 3) {
			start = 3;
		}

		if ((start % 2) == 0) {
			start++; // make start odd
		}
		
		
		Vector3f vec1, vec2;
		Vector3f cross = new Vector3f();
		float crossProd;
		for (k = start; k >= 3; k -= 2) {
			flag = true;

			while ((flag == true) && (size() > k)) {
				flag = false;

				for (i = 0; i < (length - (k - 1)); i++) {

					// Form two vectors
				    vec1 = elementAt(i + (k / 2));
				    vec2 = elementAt(i + (k - 1));
				    cross.Cross(vec1,vec2);
				    crossProd = cross.Length();
				    
				    /*				    
					vAx = ((Vector3f) (elementAt(i + (k / 2)))).X
							- ((Vector3f) (elementAt(i))).X;
					vAy = ((Vector3f) (elementAt(i + (k / 2)))).Y
							- ((Vector3f) (elementAt(i))).Y;
					vBx = ((Vector3f) (elementAt(i + (k - 1)))).X
							- ((Vector3f) (elementAt(i))).X;
					vBy = ((Vector3f) (elementAt(i + (k - 1)))).Y
							- ((Vector3f) (elementAt(i))).Y;

					// calc cross product
					crossProd = (vAx * vBy) - (vAy * vBx);
					*/

					if (ccw == false) {

						if (crossProd <= 0) {
							removeElementAt(i + (k / 2));
							flag = true;
							length = size();
						}
					} else {

						if (crossProd >= 0) {
							removeElementAt(i + (k / 2));
							flag = true;
							length = size();
						}
					}
				}
			}

			// Rotate points so that all concavities are removed.
			for (j = 0; j < (length / 2); j++) {
				tmpPt = elementAt(size() - 1);
				removeElementAt(size() - 1);
				insertElementAt(tmpPt, 0);
			}

			// Repeat to remove all local concavities
			if ((repeat == true) && (k == 3)) {
				k = 5;
				repeat = false;
			}
		}
	}

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.structures.VOIBase#clone()
     */
    public VOIBase clone() {
        return new VOIContour(this);
    }


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
		try {

			if ((yPts == null) || (xPts == null) || (size() != xPts.length)) {
				xPts = new float[size()];
				yPts = new float[size()];
			}
			// System.err.println("contains : xPts.len = " + xPts.length + "
			// yPts.len = " + yPts.length);
		} catch (OutOfMemoryError error) {
			System.out.println("VOIContour.contains: memory allocation error");
			System.gc();
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
	 * Finds the second order attributes or moments.
	 * 
	 * @param xDim
	 *            width of input image slice
	 * @param yDim
	 *            height of input image slice
	 * @param xRes
	 *            DOCUMENT ME!
	 * @param yRes
	 *            DOCUMENT ME!
	 * @param xUnits
	 *            DOCUMENT ME!
	 * @param yUnits
	 *            DOCUMENT ME!
	 * @param pAxis
	 *            angle in degrees with major axis
	 * @param eccentricity
	 *            shape (circle = 0; line = 1)
	 * @param majorAxis
	 *            diameter of major axis
	 * @param minorAxis
	 *            diameter of minor axis Reference:
	 *            http://www.caip.rutgers.edu/~xswang/object-segment/objhtml/node4.html
	 *            Object Quantification by Simon Xin Wang
	 */
	public void secondOrderAttributes( ModelImage kImage, float[] pAxis,
			float[] eccentricity, float[] majorAxis, float[] minorAxis) {  	    
	    
        getMask();
        int nPts = m_kMask.cardinality();
        
        Vector3f kUp = new Vector3f();
        Matrix3f kMat = new Matrix3f();
        float[] afScale = new float[3];
        
        
        ApprEllipsoidFit3f kFit = new ApprEllipsoidFit3f( this.size(), this, kUp, kMat, afScale );

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
        
        double volumeEllipse = (4.0 * Math.PI / 3.0) * afScale[2] * afScale[1] * afScale[0];
        double areaEllipse = (Math.PI / 4.0) * afScale[iMax] * afScale[iMid];
        double normFactor = Math.sqrt(nPts / areaEllipse);

        float majorA = (float) (normFactor * afScale[iMax]);
        float minorA = (float) (normFactor * afScale[iMid]);
        float e = (float) Math
        .sqrt(1.0 - ((afScale[iMid] * afScale[iMid]) / (afScale[iMax] * afScale[iMax])));



        Vector3f kX = new Vector3f( kMat.M00, kMat.M01, kMat.M02 );  kX.Normalize();
        Vector3f kY = new Vector3f( kMat.M10, kMat.M11, kMat.M12 );  kY.Normalize();
        Vector3f kZ = new Vector3f( kMat.M20, kMat.M21, kMat.M22 );  kZ.Normalize();
        Vector3f[] kBasis = new Vector3f[]{ kX, kY, kZ };
        kMat = new Matrix3f( kX, kY, kZ, false );
        Vector3f kRot = new Vector3f();
        kMat.Mult( Vector3f.UNIT_Z_NEG, kRot );              
        float p = (float)(Math.acos(kRot.Dot(kBasis[iMax])) * 180f/Math.PI);
        
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
        
        pAxis[0] = p;
        eccentricity[0] = e;
        majorAxis[0] = majorA;
        minorAxis[0] = minorA;
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
	 * @param pAxis
	 *            angle in degrees with major axis
	 * @param eccentricity
	 *            shape (circle = 0; line = 1)
	 * @param majorAxis
	 *            diameter of major axis
	 * @param minorAxis
	 *            diameter of minor axis
	 */
	public void secondOrderAttributeslsq(float xRes, float yRes, int xUnits,
			int yUnits, float[] pAxis, float[] eccentricity, float[] majorAxis,
			float[] minorAxis) {

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

		Matrix argMat = new Matrix(DABE);
		EigenvalueDecomposition eig;
		double[] eigenvalue;
		double[][] eigenvector;
		double temp;
		double[] tempCol = new double[3];
		int m, n, index;
		eig = new EigenvalueDecomposition(argMat);
		eigenvalue = eig.getRealEigenvalues();

		// In EigenvalueDecomposition the columns represent the
		// eigenvectors
		eigenvector = eig.getV().getArray();

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
		double thetarad = 0.5 * Math.atan2(par[1], (par[0] - par[2]));
		pAxis[0] = (float) ((180.0 / Math.PI) * thetarad);

		double cost = Math.cos(thetarad);
		double sint = Math.sin(thetarad);
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

		// double uCentre = tuCentre * cost - tvCentre * sint;
		// double vCentre = tuCentre * sint + tvCentre * cost;
		double Ru = -wCentre / Auu;
		double Rv = -wCentre / Avv;
		Ru = Math.sqrt(Math.abs(Ru));
		Rv = Math.sqrt(Math.abs(Rv));

		if (Ru >= Rv) {
			majorAxis[0] = (float) (2.0 * Ru);
			minorAxis[0] = (float) (2.0 * Rv);
		} else {
			majorAxis[0] = (float) (2.0 * Rv);
			minorAxis[0] = (float) (2.0 * Ru);

			if (pAxis[0] <= 0.0f) {
				pAxis[0] = pAxis[0] + 90.0f;
			} else {
				pAxis[0] = pAxis[0] - 90.0f;
			}
		}
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


    /**
	 * Gathers max and average statistics to build guessing intervals
	 */
	private double[] determineMaxAvg(float xRes, float yRes, float[] xPts,
			float[] yPts) {
		double max = 0, avg = 0;
		int n = xPts.length;
		Random r = new Random();
		// note that a stop that yields a non-representative average is fine,
		// since worst case is more computations
		int stop = n < 100 ? (int) (n * 0.50) : (int) (n * 0.10);
		int[] search = new int[stop];
		int[] end = new int[stop];
		double startX, startY;
		double endX, endY;
		double delX, delY;
		double distX, distY;
		double distanceSq;
		int i, j;
		for (i = 0; i < stop; i++) {
			search[i] = r.nextInt(n);
			end[i] = r.nextInt(n);
		}

		int numBegin = 0, numEnd = 0;
		;
		for (i = 0; i < search.length; i++) {

			numBegin = search[i];
			startX = xPts[numBegin];
			startY = yPts[numBegin];
			for (j = 0; j < end.length; j++) {
				numEnd = end[j];
				endX = xPts[numEnd];
				endY = yPts[numEnd];
				delX = endX - startX;
				delY = endY - startY;
				distX = xRes * delX;
				distY = yRes * delY;
				distanceSq = distX * distX + distY * distY;
				avg = avg + distanceSq;
				if (distanceSq > max) {
					max = distanceSq;
				} // if (distanceSq > largsestDistanceSq)
			} // for (j = i+1; j < xPts.length; j++)
		} // for (i = 0; i < xPts.length; i++)
		avg = avg / (end.length * search.length);
		return new double[] { max, avg };
	}

    /**
	 * Gathers the points that fall within the required bounds
	 */
	private void gatherBoundedPoints(ArrayList<Integer> orig,
			ArrayList<Integer> term, double lowerBound, double upperBound,
			float xRes, float yRes, float[] xPts, float[] yPts) {
		int i, j;
		double startX, startY;
		double endX, endY;
		double delX, delY;
		double distX, distY;
		double distanceSq;
		for (i = 0; i < xPts.length; i++) {
			startX = xPts[i];
			startY = yPts[i];
			for (j = i + 1; j < xPts.length; j++) {
				endX = xPts[j];
				endY = yPts[j];
				delX = endX - startX;
				delY = endY - startY;
				distX = xRes * delX;
				distY = yRes * delY;
				distanceSq = distX * distX + distY * distY;
				if (distanceSq > lowerBound && distanceSq < upperBound) {
					orig.add(Integer.valueOf(i));
					term.add(Integer.valueOf(j));
				} // if (distanceSq > largsestDistanceSq)
			} // for (j = i+1; j < xPts.length; j++)
		} // for (i = 0; i < xPts.length; i++)
	}
    
    /**
	 * 
	 * 2d version to find the largest distance of the collected values
	 * 
	 * @return the largest distance for this contour
	 */
	private double getLargest(ArrayList<Integer> orig, ArrayList<Integer> term,
			float xRes, float yRes, float[] xPoints, float[] yPoints) {
		Integer origPoint, termPoint;
		double startX, startY;
		double endX, endY;
		double delX, delY;
		double distX, distY;
		double x, y;
		double slope;
		double largestDistanceSq = 0, distanceSq;
		int xRound, yRound;
		int num = 0;
		int j;
		contains(0, 0);
		forj: for (j = 0; j < orig.size(); j++) {
			num++;
			origPoint = orig.get(j).intValue();
			startX = xPoints[origPoint];
			startY = yPoints[origPoint];
			termPoint = term.get(j).intValue();
			endX = xPoints[termPoint];
			endY = yPoints[termPoint];
			delX = endX - startX;
			delY = endY - startY;
			distX = xRes * delX;
			distY = yRes * delY;
			distanceSq = distX * distX + distY * distY;
			if (distanceSq > largestDistanceSq) {
				if (Math.abs(delX) >= Math.abs(delY)) {
					slope = delY / delX;
					if (endX >= startX) {
						for (x = startX + 0.5, y = startY + 0.5 * slope; x < endX; x += 0.5, y += 0.5 * slope) {
							xRound = (int) Math.round(x);
							yRound = (int) Math.round(y);
							if (!contains(xRound, yRound)) {
								continue forj;
							}
						} // for (x = startX + 0.5, y = startY + 0.5 * slope;
							// x < endX; x += 0.5, y += 0.5 * slope)
					} // if (endX >= startX)
					else { // endX < startX
						for (x = startX - 0.5, y = startY - 0.5 * slope; x > endX; x -= 0.5, y -= 0.5 * slope) {
							xRound = (int) Math.round(x);
							yRound = (int) Math.round(y);
							if (!contains(xRound, yRound)) {
								continue forj;
							}
						} // for (x = startX - 0.5, y = startY - 0.5 * slope;
							// x > endX; x -= 0.5, y -= 0.5 * slope)
					} // else endX < startX
				} // if (Math.abs(delX) >= Math.abs(delY))
				else { // Math.abs(delX) < Math.abs(delY)
					slope = delX / delY;
					if (endY >= startY) {
						for (y = startY + 0.5, x = startX + 0.5 * slope; y < endY; y += 0.5, x += 0.5 * slope) {
							xRound = (int) Math.round(x);
							yRound = (int) Math.round(y);
							if (!contains(xRound, yRound)) {
								continue forj;
							}
						} // for (y = startY + 0.5, x = startX + 0.5 * slope;
							// y < endY; y += 0.5, x += 0.5 * slope)
					} // if (endX >= startX)
					else { // endX < startX
						for (y = startY - 0.5, x = startX - 0.5 * slope; y > endY; y -= 0.5, x -= 0.5 * slope) {
							xRound = (int) Math.round(x);
							yRound = (int) Math.round(y);
							if (!contains(xRound, yRound)) {
								continue forj;
							}
						} // for (y = startY - 0.5, x = startX - 0.5 * slope;
							// y > endY; y -= 0.5, x -= 0.5 * slope)
					} // else endX < startX
				} // else Math.abs(delX) < Math.abs(delY)
				largestDistanceSq = distanceSq;
			}
		}
		return Math.sqrt(largestDistanceSq);
	}
    
}
