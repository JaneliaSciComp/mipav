package gov.nih.mipav.model.structures;

import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;



import java.awt.Polygon;
import java.io.IOException;
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
	
	public double pinpol(double xpoint, double ypoint, boolean snear[], int index1[], int index2[]) {
	    // Check if point is inside a polygon, on a side of the polygon, or outside the polygon
	    // This is an improved version of the Algorithm of Nordbeck and Rydstedt written by S. W. Sloan
	    // The original FORTRAN 77 code by S. W. Sloan appeared in "A point-in-polygon program",
	    // Advanced Engineering Software, 1985, Vol. 7, No. 1, pp. 45-47.
	    // Permission to port the pinpol routine was granted by Professor Sloan.
	    // xpoint is the x coordinate of the point to be tested
	    // ypoint is the y coordinate of the point to be tested
	    // Return mindst, the distance from the point to the nearest point of the polygon
	    // If mindst is less than zero, then the point is outside the polygon
	    // If mindst is equal to zero, then the point is on a side of the polygon
	    // If mindst is greater than zero, then the point is inside the polygon
	    double mindst;
	    // Return snear
	    // snear is true if distance to the nearest side is less than distance to nearest vertex
        // snear if false if distance to the nearest vertex is less than distance to nearest side
	    // Return index1 and index2 
	    // If snear is true index1 and index2 are indices of endpoints of side
	    // If snear if false index1 is index of vertex
	    // smalld is a small double precision number
	    // In the original FORTRAN 77 implementation a smalld value is not specified
	    // smalld is a calling argument in the original implementation
	    // The MIT licensed python 3 implementation sets smalld = 1.0E-12
	    double smalld = 1.0E-12;
	    // Original code specifies a counterclockwise contour, but I need a clockwise contour
	    // for my signs to match his signs.
	    // Need a clockwise contour and don't want to change the original contour
	    VOIContour newContour;
	    double x[];
	    double y[];
	    // n is the number of sides/vertices defining the polygon
	    int n;
	    // mindstSquared is the square of the distance to the closest point on the polygon
	    double mindstSquared;
	    int i;
	    double x1;
	    double y1;
	    double x21;
	    double y21;
	    double x1p;
	    double y1p;
	    double t;
	    double d;
	    int j = 0;
	    double dx;
	    double dy;
	    double area;
	    newContour = new VOIContour(this);
	    newContour.makeClockwise();
	    n = newContour.size();
	    x = new double[n+2];
	    y = new double[n+2];
	    for (i = 0; i < n; i++) {
	        x[i] = newContour.elementAt(i).X;
	        y[i] = newContour.elementAt(i).Y;
	    }
	    x[n] = x[0];
	    x[n+1] = x[1];
	    y[n] = y[0];
	    y[n+1] = y[1];
	    
	    mindstSquared = Double.MAX_VALUE;
	    
	    // Loop over each side defining the polygon
	    for (i = 0; i < n; i++) {
	        
	        // Start of side has coords (x1, y1)
	        // End of side has coords (x2, y2)
	        // Point has coords (xpoint, ypoint)
	        
	        x1 = x[i];
	        y1 = y[i];
	        x21 = x[i+1] - x1;
	        y21 = y[i+1] - y1;
	        x1p = x1 - xpoint;
	        y1p = y1 - ypoint;
	        
	        // Points on infinite line defined by
	        // x = x1 + t*(x1-x2)
	        // y = y1 + t*(y1-y2)
	        // where
	        // t = 0 at (x1, y1)
	        // t = 1 at (x2, y2)
	        // Find where normal passing through (xpoint, ypoint)
	        // intersects infinite line
	        
	        t = -(x1p*x21 + y1p*y21)/(x21*x21 + y21*y21);
	        if (t < 0.0) {
	        
	            // Normal does not intersect side
	            // Point is closest to vertex (x1, y1)
	            // Compute square of distance to this vertex
	            
	            d = x1p*x1p + y1p*y1p;
	            if (d < mindstSquared) {
	                
	                // Point is closer to (x1, y1) than any other vertex or
	                // side
	                
	                snear[0] = false;
	                mindstSquared = d;
	                j = i;
	            } // if (d < mindstSquared)
	        } // if (t < 0.0)
	        else if (t <= 1.0) {
	        
	            // Normal intersects side
	            
	            dx = x1p + t*x21;
	            dy = y1p + t*y21;
	            d = dx*dx + dy*dy;
	            if (d < mindstSquared) {
	                
	                // Point is closer to this side than to any
	                // other side or vertex
	                
	                snear[0] = true;
	                mindstSquared = d;
	                j = i;
	            } // if (d < mindstSquared)
	        } // else if (t <= 1.0) 
	    } // for (i = 0; i < n; i++)
	    mindst = Math.sqrt(mindstSquared);
	    if (mindst < smalld) {
	        
	        // Point is on side of polygon
	        mindst = 0.0;
	    } // if (mindst < smalld)
	    else if (snear[0]) {
	        
	         // Point is closer to its nearest side than to its nearest
	         // vertex.  Check if point is to left or right of this side.
	         // If point is to left of side, it is inside polygon, else
	         // point is outside polygon
	        area = det(x[j],x[j+1],xpoint,y[j],y[j+1],ypoint);
	        if (area >= 0.0) {
	            mindst = Math.abs(mindst);
	        }
	        else {
	            mindst = -Math.abs(mindst);
	        }
	    } // else if (snear[0])
	    else {
	        
	        // Point is closer to its nearest vertex than to its nearest side,
	        // Check if the nearest vertex is concave.
	        // If the nearest vertex is concave, then point is inside the
	        // polygon, else the point is outside the polygon
	        
	        if (j == 0) {
	            j = n;
	        }
	        area = det(x[j+1],x[j],x[j-1],y[j+1],y[j],y[j-1]);
	        if (area >= 0.0) {
                mindst = Math.abs(mindst);
            }
            else {
                mindst = -Math.abs(mindst);
            }
	    } // else
	    // Find indices in original contour that correspond to j and j+1
	    for (i = 0; i < n; i++) {
	        if ((this.elementAt(i).X == (float)x[j]) && (this.elementAt(i).Y == (float)y[j])) {
	            index1[0] = i;
	        }
	        if (snear[0]) {
	            if ((this.elementAt(i).X == (float)x[j+1]) && (this.elementAt(i).Y == (float)y[j+1])) {
	                index2[0] = i;
	            }
	        } // if (snear[0])
	    } // for (i = 0; i < n; i++)
	    return mindst;
	} // pinpol
	
	private double det(double x1, double x2, double x3, double y1, double y2, double y3) {
	    double determinant;
	    // Compute twice the area of the triangle defined by three points
	    // with coords (x1, y1), (x2, y2), and (x3, y3) using determinant
	    // formula
	    
	    // Input parameters: 
	    // x1, y1 coords of point 1
	    // x2, y2 coords of point 2
	    // x3, y3 coords of point 3
	    
	    // Output parameters:
	    // determinant twice the area of the triangle defined by the three points
	    
	    // Notes:
	    
	    // determinant is positive is points 1, 2, and 3 define the triangle in
	    // counterclockwise order
	    // determinant is negative if points 1, 2, and 3 define the triangle in
	    // clockwise order
	    // determinant is zero if at least 2 of the points are coincident or if
	    // all three points are collinear
	    
	    determinant = (x1-x3)*(y2-y3) - (x2-x3)*(y1-y3);
	    return determinant;    
	} // det
	
	public double euclideanDistance2D(int xDim, int yDim) {
	    // Calculates fractal dimensionality of a contour
	    // x and y padded by distance on each side so that distance from any 
	    // contour can be obtained without the user having to pad the image.
	    // Reference: Fractal and Multi-Scale Fractal Dimension analysis:
	    // a comparative study of the Bouligand-Minkowski method
	    // by Andre Ricardo Backes and Odemir Martinez Bruno
	    int i;
	    float xf;
	    float yf;
	    // Maximum distance from boundary considered on each side
	    int distance = 6;
	    // 1, 2,  ..., distance
	    int numLevels = distance;
	    int area[] = new int[numLevels];
	    double radius[] = new double[numLevels];
	    int xLow = Integer.MAX_VALUE;
        int xHigh = Integer.MIN_VALUE;
        int yLow = Integer.MAX_VALUE;
        int yHigh = Integer.MIN_VALUE;
        int n = size();
        double pin[][];
        boolean snear[] = new boolean[1];
        int i1[] = new int[1];
        int i2[] = new int[1];
        int x;
        int y;
        double logRadius[] = new double[numLevels];
        double sumx;
        double sumy;
        double sumxx;
        double sumxy;
        double bestFitSlope;
        double bestFitFD;
        int xfl;
        int xfc;
        int yfl;
        int yfc;
        int j;
        float xf2;
        float yf2;
        int xfl2;
        int yfl2;
        int xfc2;
        int yfc2;
        int extendedXDim = xDim + 2 * distance;
        int extendedYDim = yDim + 2 * distance;
        int sliceSize = extendedXDim * extendedYDim;
        BitSet includeSet = new BitSet(sliceSize);
        double gradr[] = new double[numLevels];
        double grada[] = new double[numLevels];
        double localSlope[] = new double[numLevels];
        double localFD[] = new double[numLevels];
        double logArea[] = new double[numLevels];
        boolean displayMethod = false;
        
        for (i = 0; i < n; i++) {
            xf = elementAt(i).X + distance;
            yf = elementAt(i).Y + distance;
            xfl = (int)Math.floor(xf);
            xfc = (int)Math.ceil(xf);
            yfl = (int)Math.floor(yf);
            yfc = (int)Math.ceil(yf);
            if (xfl < xLow) {
                xLow = xfl;
            }
            if (xfc > xHigh) {
                xHigh = xfc;
            }
            if (yfl < yLow) {
                yLow = yfl;
            }
            if (yfc > yHigh) {
                yHigh = yfc;
            }
            if (i < n-1) {
                j = i+1;
            }
            else {
                j = 0;
            }
            xf2 = elementAt(j).X + distance;
            yf2 = elementAt(j).Y + distance;
            xfl2 = (int)Math.floor(xf2);
            xfc2 = (int)Math.ceil(xf2);
            yfl2 = (int)Math.floor(yf2);
            yfc2 = (int)Math.ceil(yf2);
            for (y = Math.min(yfl, yfl2)-distance; y <= Math.max(yfc, yfc2) + distance; y++) {
                for (x = Math.min(xfl, xfl2)-distance; x <= Math.max(xfc, xfc2) + distance; x++) {
                    includeSet.set(x + y * extendedXDim);
                }
            }
        }
        
        xLow = xLow-distance;
        xHigh = xHigh + distance;
        yLow = yLow - distance;
        yHigh = yHigh + distance;
        
        for (i = 0; i < numLevels; i++) {
            radius[i] = (1.0 + i);
        }
        
        pin = new double[xHigh - xLow + 1][yHigh - yLow + 1];
        for (y = yLow; y <= yHigh; y++) {
            for (x = xLow; x <= xHigh; x++) {
                if (includeSet.get(x + y*extendedXDim)) {
                    pin[x - xLow][y - yLow] = Math.abs(pinpol(x-distance, y-distance, snear, i1, i2));
                }
            }
        } // for (y = yLow; y <= yHigh; y++)
        
        if (displayMethod) {
            int extents[] = new int[2];
            extents[0] = xHigh - xLow + 1;
            extents[1] = yHigh - yLow + 1;
            ModelImage pImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "pImage");
            double buffer[] = new double[extents[0]*extents[1]];
            for (y = yLow; y <= yHigh; y++) {
                for (x = xLow; x <= xHigh; x++) {
                    buffer[(x-xLow) + extents[0]* (y - yLow)] = pin[x - xLow][y - yLow];
                }
            }
            try {
                pImage.importData(0, buffer, true);
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException " + e + " on pImage.importData");
                return 0.0;
            }
            new ViewJFrameImage(pImage);
        } // if (displayMethod)
        
        for (i = 0; i < numLevels; i++) {
            for (y = yLow; y <= yHigh; y++) {
                for (x = xLow; x <= xHigh; x++) {
                    if (includeSet.get(x + y*extendedXDim)) {
                        if (pin[x - xLow][y - yLow] <= radius[i]) {
                            area[i]++;
                        }  
                    }
                }
            }  
        }
        
        for (i = 0; i < numLevels; i++) {
            // Note no need to use log(2*radius) since log(2*radius2) - log(2*radius1) = log(radius2) - log(radius1)
            logRadius[i] = Math.log(radius[i]);
            logArea[i] = Math.log(area[i]);
        }
        
        gradr[0] = logRadius[1] - logRadius[0];
        grada[0] = logArea[1] - logArea[0];
        gradr[numLevels-1] = logRadius[numLevels-1] - logRadius[numLevels-2];
        grada[numLevels-1] = logArea[numLevels-1] - logArea[numLevels-2];
        for (i = 1; i < numLevels-1; i++) {
            gradr[i] = (logRadius[i+1] - logRadius[i-1])/2.0;
            grada[i] = (logArea[i+1] - logArea[i-1])/2.0;
        }
        
        for (i = 0; i < numLevels; i++) {
            localSlope[i] = grada[i]/gradr[i];
            localFD[i] = 2.0 - localSlope[i];
            Preferences.debug("i = " + i + " radius = " + radius[i] + " area = " + area[i] + " local FD = " + localFD[i] + "\n",
                               Preferences.DEBUG_ALGORITHM);
        }
        
        
        sumxy = 0.0;
        sumx = 0.0;
        sumy = 0.0;
        sumxx = 0.0;
        for (i = 0; i < numLevels; i++) {
            sumxy += logRadius[i]*logArea[i];
            sumx += logRadius[i];
            sumy += logArea[i];
            sumxx += logRadius[i]*logRadius[i];
        }
        bestFitSlope = (sumxy - sumx*sumy/numLevels)/(sumxx - sumx*sumx/numLevels);
        bestFitFD = 2.0 - bestFitSlope;
        Preferences.debug("For 2D outlines a valid fractal dimension ranges from 1 to 2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Best fit fractal dimension = " + bestFitFD + "\n", Preferences.DEBUG_ALGORITHM);
	    return bestFitFD;
	}
	
	public double boxCountBoundary2D(int xDim, int yDim) {
        // Portions of boxcount.m revision 2.10, written by F. Moisy on 07/09/2008
        // were ported to Java in creating this routine.
        // n[] is the number n of 2D dimensional boxes of size r[] needed to cover the 
        // points on the boundary of the VOIContour.  The box sizes are powers of two, i.e.,
        // 1, 2, 4, ..., 2 ^p, where p is the smallest integer such that 
	    // max(x max of contour - x min + 1 of contour, y max of contour - y min of contour + 1) <= 2^p.
	    // The box counting method is used in determining fractal properties of the 
	    // contour boundary.  If the boundary is a fractal set, with fractal dimension
	    // fd < d, then n scales as r^(-fd).  fd is know as the Minkowski-Bouligand dimension,
	    // or Kolmogorov capacity, or Kolmogorov dimension, or simply box-counting dimension.
	    // Complex outlines can be defined in 2D using a fractal dimension ranging from 1 to 2.
	    
	    // Note that the reference "High precision  boundary fractal analysis for shape characterization"
	    // by Dominique Berube and Michel Jebrak found:
	    // "Dilation and euclidean distance mapping methods(EDM) produce the more reliable results with a
	    // low sensitivity to object size and resolution.  The precision of the EDM method (+-0.01) is
	    // higher than that for the dilation method(+-0.03).  Box-counting behaves erratically when used
	    // to analyze outlines."
	    
	    // For 1024 by 1024 Koch_snowflake.png generating a VOI with levelset the boxCountBoundary2D calculations were:
	    // Box size = 1 Box number = 8800 Local fractal dimension = 0.8786831268735233
	    // Box size = 2 Box number = 4786 Local fractal dimension = 0.9435741259934443
	    // Box size = 4 Box number = 2379 Local fractal dimension = 1.0521834020982703
	    // Box size = 8 Box number = 1113 Local fractal dimension = 1.0982744140336271
	    // Box size = 16 Box number = 519 Local fractal dimension = 1.2064393726305385
	    // Box size = 32 Box number = 209 Local fractal dimension = 1.2719286486957417
	    // Box size = 64 Box number = 89 Local fractal dimension = 1.3099481454152717
	    // Box size = 128 Box number = 34 Local fractal dimension = 1.2844214176789392
	    // Box size = 256 Box number = 15 Local fractal dimension = 1.5437314206251695
	    // Box size = 512 Box number = 4 Local fractal dimension = 1.953445297804259
	    // Box size = 1024 Box number = 1 Local fractal dimension = 1.9999999999999998
	    // Best fit fractal dimension = 1.2813819274567155
	    
	    // The actual answer is log(4)/log(3) = 1.26186
	    int x;
	    int y;
	    int numBoundaryPoints = 0;
	    boolean snear[] = new boolean[1];
	    int i1[] = new int[1];
	    int i2[] = new int[1];
	    Vector<Vector3f> boundaryVector = new Vector<Vector3f>();
	    int xLow = Integer.MAX_VALUE;
	    int xHigh = Integer.MIN_VALUE;
	    int yLow = Integer.MAX_VALUE;
	    int yHigh = Integer.MIN_VALUE;
	    int xRange;
	    int yRange;
	    int width;
	    double p;
	    int pCeil;
	    byte c[][];
	    int i;
	    int g;
	    int siz;
	    int siz2;
	    int j;
	    int nInit[];
	    double ln[];
	    double lr[];
	    double gradn[];
	    double gradr[];
	    double sumxy;
	    double sumx;
	    double sumy;
	    double sumxx;
	    double bestFitFD;
	    float xf;
	    float yf;
	    float xf2;
	    float yf2;
	    int xfl;
	    int xfc;
	    int yfl;
	    int yfc;
	    int xfl2;
        int xfc2;
        int yfl2;
        int yfc2;
	    int nPts = size();
	    int sliceSize = xDim*yDim;
	    BitSet includeSet = new BitSet(sliceSize);
	    
	    for (i = 0; i < nPts; i++) {
            xf = elementAt(i).X;
            yf = elementAt(i).Y;
            xfl = (int)Math.floor(xf);
            xfc = (int)Math.ceil(xf);
            yfl = (int)Math.floor(yf);
            yfc = (int)Math.ceil(yf);
            if (xfl < xLow) {
                xLow = xfl;
            }
            if (xfc > xHigh) {
                xHigh = xfc;
            }
            if (yfl < yLow) {
                yLow = yfl;
            }
            if (yfc > yHigh) {
                yHigh = yfc;
            }
            if (i < nPts-1) {
                j = i+1;
            }
            else {
                j = 0;
            }
            xf2 = elementAt(j).X;
            yf2 = elementAt(j).Y;
            xfl2 = (int)Math.floor(xf2);
            xfc2 = (int)Math.ceil(xf2);
            yfl2 = (int)Math.floor(yf2);
            yfc2 = (int)Math.ceil(yf2);
            // Decreasing the floors by 1 and increasing the ceilings by 1 in the Koch snowflake example
            // only changed the result for box size 1, increasing the count from 8800 to 8802
            // Not worth the extra time.
            for (y = Math.min(yfl, yfl2); y <= Math.max(yfc, yfc2); y++) {
                for (x = Math.min(xfl, xfl2); x <= Math.max(xfc, xfc2); x++) {
                    includeSet.set(x + y * xDim);
                }
            }
        }
	    
	    for (y = yLow; y <= yHigh; y++) {
            for (x = xLow; x <= xHigh; x++) {
                if (includeSet.get(x + y * xDim)) {
                    if (pinpol(x,y,snear,i1,i2) == 0.0) {
                        numBoundaryPoints++;
                        boundaryVector.add(new Vector3f(x, y, 0.0f));
                    }
                }
            }
        } // for (y = yLow; y <= yHigh; y++)
	    
	    xRange = xHigh - xLow + 1;
	    yRange = yHigh - yLow + 1;
	    width = Math.max(xRange, yRange); // largest size of box
	    p = Math.log(width)/Math.log(2.0); // number of generations
	    
        pCeil = (int)Math.ceil(p);
        width = (int)Math.round(Math.pow(2.0, pCeil));
        c = new byte[width][width];
        for (i = 0; i < numBoundaryPoints; i++) {
            x = Math.round(boundaryVector.get(i).X);
            y = Math.round(boundaryVector.get(i).Y);
            c[x - xLow][y - yLow] = 1;
        }
	    
	    // Preallocate the number of boxes of size r
	    int n[] = new int[pCeil+1];
	    int r[] = new int[pCeil+1];
	    nInit = new int[pCeil+1];
	    nInit[pCeil] = numBoundaryPoints;
	    for (g = pCeil-1; g >= 0; g--) {
	        siz = (int)Math.round(Math.pow(2.0, pCeil-g));
	        siz2 = (int)Math.round(siz/2.0);
	        for (i = 0; i < width-siz+1; i += siz) {
	            for (j = 0; j < width-siz+1; j += siz) {
	                if ((c[i+siz2][j] == 1) || (c[i][j+siz2] == 1) || (c[i+siz2][j+siz2] == 1)) {
	                    c[i][j] = 1;
	                }
	            }
	        }
	        for (i = 0; i < width-siz+1; i+= siz) {
	            for (j = 0; j < width-siz+1; j += siz) {
	                if (c[i][j] == 1) {
	                    nInit[g]++;
	                }
	            }
	        }
	    } // for (g = pCeil-1; g >= 0; g--)
	    
	    ln = new double[n.length];
	    lr = new double[n.length];
	    for (i = 0; i < n.length; i++) {
	        n[i] = nInit[n.length-1-i];
	        ln[i] = Math.log(n[i]);
	        if (i == 0) {
	            r[i] = 1;
	        }
	        else {
	            r[i] = 2 * r[i-1]; // box size (1, 2, 4, 8, ...)
	        }
	        lr[i] = Math.log(r[i]);
	    }
	    
	    gradn = new double[n.length];
	    gradr = new double[n.length];
	    double localFD[] = new double[n.length];
	    gradn[0] = ln[1] - ln[0];
	    gradr[0] = lr[1] - lr[0];
	    gradn[n.length-1] = ln[n.length-1] - ln[n.length-2];
	    gradr[n.length-1] = lr[n.length-1] - lr[n.length-2];
	    for (i = 1; i < n.length-1; i++) {
	        gradn[i] = (ln[i+1] - ln[i-1])/2.0;
	        gradr[i] = (lr[i+1] - lr[i-1])/2.0;
	    }
	    
	    for (i = 0; i < n.length; i++) {
	        localFD[i] = -gradn[i]/gradr[i];
	    }
	    
	    // log(n) = slope * log(r) + intercept
	    // fd = -slope
	    // slope = (sumXiYi - sumXi*sumYi/n)/(sumXiSquared - sumXi*sumXi/n)
	    sumxy = 0.0;
	    sumx = 0.0;
	    sumy = 0.0;
	    sumxx = 0.0;
	    for (i = 0; i < n.length; i++) {
	        sumxy += lr[i]*ln[i];
	        sumx += lr[i];
	        sumy += ln[i];
	        sumxx += lr[i]*lr[i];
	    }
	    bestFitFD = -(sumxy - sumx*sumy/n.length)/(sumxx - sumx*sumx/n.length);
	    Preferences.debug("For 2D outlines a valid fractal dimension ranges from 1 to 2\n", Preferences.DEBUG_ALGORITHM);
	    for (i = 0; i < n.length; i++) {
	        Preferences.debug("Box size = " + r[i] + " Box number = " + n[i] + " Local fractal dimension = " + localFD[i] + "\n",
	                           Preferences.DEBUG_ALGORITHM);
	    }
	    Preferences.debug("Best fit fractal dimension = " + bestFitFD + "\n", Preferences.DEBUG_ALGORITHM);
	    return bestFitFD;
    }
	
	/**
	 * From statics it is known that any planar shape or closed curve possesses two principal axes 90 degrees
	 * apart intersecting at the centroid of the area.  (For certain areas such as a circle, there may be more
	 * than two principal axes about the centroid, but there are always at least two.)  The moment of inertia is
	 * maximum about one principal axis and minimum about the other principal axis.  Here the angle in degrees 
	 * with the principal axis having the minimum moment of inertia is reported.  For an ellipse with semimajor
	 * axis = a and semiminor axis = b, the moment of inertia has its smallest value of PI*a*b^3/8 about the
	 * major axis and its largest value of PI*a^3*b/8 about the minor axis.
	 * Reference: On the Computation of the Moments of a Polygon, with Some Applications by Soerjadi
	 * @param xDim
	 * @param yDim
	 * @return
	 */
	public double principalAxis(int xDim, int yDim) {
	    double theta = 0.0;
        // Moment of inertia or second moment of the area A with respect to the x axis
        double Ix = 0.0;
        // Moment of inertia about the y axis
        double Iy = 0.0;
        // Product of inertia
        double Pxy = 0.0;
        double xCentroid;
        double yCentroid;
        double theta1;
        double theta2;
        double Iave;
        double Idiff;
        double Ixp1;
        double Ixp2;
        double R;
        double Imin;
        double Imax;
        int i;
        double exactTotalPixelCount;
        double term;
        double diffx1;
        double diffx2;
        double diffy1;
        double diffy2;
        int n = size();
        
        xCentroid = 0.0;
        yCentroid = 0.0;
        exactTotalPixelCount = 0.0;
        for (i = 0; i < n-1; i++) {
            term = (elementAt(i).X*elementAt(i+1).Y - elementAt(i+1).X*elementAt(i).Y);
            exactTotalPixelCount += term;
            xCentroid += (elementAt(i).X + elementAt(i+1).X) * term;
            yCentroid += (elementAt(i).Y + elementAt(i+1).Y) * term;
        }
        term =  (elementAt(n-1).X*elementAt(0).Y - elementAt(0).X*elementAt(n-1).Y);
        exactTotalPixelCount += term;
        exactTotalPixelCount = 0.5*Math.abs(exactTotalPixelCount);
        xCentroid += (elementAt(n-1).X + elementAt(0).X) * term;
        yCentroid += (elementAt(n-1).Y + elementAt(0).Y) * term;
        xCentroid = Math.abs(xCentroid/(6.0 * exactTotalPixelCount));
        yCentroid = Math.abs(yCentroid/(6.0 * exactTotalPixelCount));
        
        Ix = 0.0;
        Iy = 0.0;
        Pxy = 0.0;
        for (i = 0; i < n-1; i++) {
            diffx1 = elementAt(i).X - xCentroid;
            diffx2 = elementAt(i+1).X - xCentroid;
            diffy1 = elementAt(i).Y - yCentroid;
            diffy2 = elementAt(i+1).Y - yCentroid;
            term = (diffx1*diffy2 - diffx2*diffy1);
            Ix += (diffy1*diffy1 + diffy1*diffy2 + diffy2*diffy2) * term;
            Iy += (diffx1*diffx1 + diffx1*diffx2 + diffx2*diffx2) * term;
            Pxy += (diffx1*(2.0*diffy1 + diffy2) + diffx2*(diffy1 + 2.0*diffy2)) * term;
        }
        diffx1 = elementAt(n-1).X - xCentroid;
        diffx2 = elementAt(0).X - xCentroid;
        diffy1 = elementAt(n-1).Y - yCentroid;
        diffy2 = elementAt(0).Y - yCentroid;
        term = (diffx1*diffy2 - diffx2*diffy1);
        Ix += (diffy1*diffy1 + diffy1*diffy2 + diffy2*diffy2) * term;
        Iy += (diffx1*diffx1 + diffx1*diffx2 + diffx2*diffx2) * term;
        Pxy += (diffx1*(2.0*diffy1 + diffy2) + diffx2*(diffy1 + 2.0*diffy2)) * term;
        Ix = Ix/12.0;
        Iy = Iy/12.0;
        Pxy = Pxy/24.0;
        
        Preferences.debug("xCentroid = " + xCentroid + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("yCentroid = " + yCentroid + "\n", Preferences.DEBUG_ALGORITHM);
        
        theta1 = 0.5*Math.atan2(2.0*Pxy, Iy-Ix);
        if (theta1 >= 0.0) {
            theta2 = theta1 - Math.PI/2.0;
        }
        else {
            theta2 = theta1 + Math.PI/2.0;
        }
        Iave = (Ix + Iy)/2.0;
        Idiff = (Ix - Iy)/2.0;
        Ixp1 = Iave + Idiff*Math.cos(2.0*theta1) - Pxy*Math.sin(2.0*theta1);
        Preferences.debug("Ixp1 = " + Ixp1 + "\n", Preferences.DEBUG_ALGORITHM);
        Ixp2 = Iave + Idiff*Math.cos(2.0*theta2) - Pxy*Math.sin(2.0*theta2);
        Preferences.debug("Ixp2 = " + Ixp2 + "\n", Preferences.DEBUG_ALGORITHM);
        // Double check
        R = Math.sqrt(Idiff*Idiff + Pxy*Pxy);
        Imin = Iave - R;
        Preferences.debug("Imin = " + Imin + "\n", Preferences.DEBUG_ALGORITHM);
        Imax = Iave + R;
        Preferences.debug("Imax = " + Imax + "\n", Preferences.DEBUG_ALGORITHM);
        if (Ixp1 < Ixp2) {
            theta = theta1;
        }
        else {
            theta = theta2;
        }
        theta = (180.0/Math.PI)*theta;
	    return theta;
	}
	
	/**
	 * 
	 * @param xDim
	 * @param yDim
	 * @return
	 */
	public double calcAsymmetryIndex(int xDim, int yDim) {
	    // Reference: "Automatic Detection of Asymmetry In Skin Tumors" by William V. Stoecker,
	    // William Weiling Li, and Randy H. Moss, Computerized Medical Imaging and Graphics,
	    // Vol. 16, No. 3, pp. 191-197, 1992.
	    // Reference: On the Computation of the Moments of a Polygon, with Some Applications by Soerjadi
	    int sliceSize = xDim * yDim;
	    // Moment of inertia or second moment of the area A with respect to the x axis
	    double Ix = 0.0;
	    // Moment of inertia about the y axis
	    double Iy = 0.0;
	    // Product of inertia
	    double Pxy = 0.0;
	    double xCentroid;
	    double yCentroid;
	    // angle of 2 principal axes, perpendicular to each other, with respect to the x axis
	    // tan (2*theta) = 2*Pxy/(Iy - Ix)
	    double theta;
	    double slope;
	    double offset;
	    int numAxis;
	    int numIntersectFound;
	    int n;
	    int i;
	    int j;
	    int k;
	    float intersectX[] = new float[2];
	    float intersectY[] = new float[2];
	    int vertexUsed[] = new int[2];
	    int endPoint1[] = new int[2];
	    int endPoint2[] = new int[2];
	    boolean validSlope;
	    double segmentSlope;
	    boolean validSegmentSlope;
	    double segmentOffset;
	    double xInter;
	    double yInter;
        //Vector<Vector3f> kMaskPositions;
        int group1Num;
        int group2Num;
        Vector<Vector3f> group1;
        Vector<Vector3f> group2;
        Vector<Vector3f> group1Mirror;
        Vector<Vector3f> group2Mirror;
        VOIContour contour1;
        VOIContour contour2;
        VOIContour contour1Mirror;
        VOIContour contour2Mirror;
        BitSet mask1;
        BitSet mask2;
        BitSet mask1Mirror;
        BitSet mask2Mirror;
        boolean fixed = true;
        boolean closed = true;
        int index1;
        int index2;
        boolean indicesFromEndPoint10Increasing = true;
        boolean indicesFromEndPoint20Increasing = true;
        float mirrorX;
        float mirrorY;
        double slopeSquared;
        double denom;
        //int m1AndM2Mirror;
        int m1Only;
        int m2MirrorOnly;
        //int m2AndM1Mirror;
        int m2Only;
        int m1MirrorOnly;
        double asymmetryIndexArray[] = new double[2];
        double alternateAsymmetry;
        double asymmetryIndex;
        double largestXAboveCentroid;
        double smallestXBelowCentroid;
        double largestYAboveCentroid;
        double smallestYBelowCentroid;
        boolean isGroup1[];
        double epsilon = 1.0e-12;
        double exactTotalPixelCount;
        VOIBaseVector resultCurves;
        int numVertices;
        double exactContourPixelCount;
        double exactXORPixelCount;
        double term;
        double diffx1;
        double diffx2;
        double diffy1;
        double diffy2;
        
        n = size();
        isGroup1 = new boolean[n];
        
        xCentroid = 0.0;
        yCentroid = 0.0;
        exactTotalPixelCount = 0.0;
        for (i = 0; i < n-1; i++) {
            term = (elementAt(i).X*elementAt(i+1).Y - elementAt(i+1).X*elementAt(i).Y);
            exactTotalPixelCount += term;
            xCentroid += (elementAt(i).X + elementAt(i+1).X) * term;
            yCentroid += (elementAt(i).Y + elementAt(i+1).Y) * term;
        }
        term =  (elementAt(n-1).X*elementAt(0).Y - elementAt(0).X*elementAt(n-1).Y);
        exactTotalPixelCount += term;
        exactTotalPixelCount = 0.5*Math.abs(exactTotalPixelCount);
        xCentroid += (elementAt(n-1).X + elementAt(0).X) * term;
        yCentroid += (elementAt(n-1).Y + elementAt(0).Y) * term;
        xCentroid = Math.abs(xCentroid/(6.0 * exactTotalPixelCount));
        yCentroid = Math.abs(yCentroid/(6.0 * exactTotalPixelCount));
        
        Ix = 0.0;
        Iy = 0.0;
        Pxy = 0.0;
        for (i = 0; i < n-1; i++) {
            diffx1 = elementAt(i).X - xCentroid;
            diffx2 = elementAt(i+1).X - xCentroid;
            diffy1 = elementAt(i).Y - yCentroid;
            diffy2 = elementAt(i+1).Y - yCentroid;
            term = (diffx1*diffy2 - diffx2*diffy1);
            Ix += (diffy1*diffy1 + diffy1*diffy2 + diffy2*diffy2) * term;
            Iy += (diffx1*diffx1 + diffx1*diffx2 + diffx2*diffx2) * term;
            Pxy += (diffx1*(2.0*diffy1 + diffy2) + diffx2*(diffy1 + 2.0*diffy2)) * term;
        }
        diffx1 = elementAt(n-1).X - xCentroid;
        diffx2 = elementAt(0).X - xCentroid;
        diffy1 = elementAt(n-1).Y - yCentroid;
        diffy2 = elementAt(0).Y - yCentroid;
        term = (diffx1*diffy2 - diffx2*diffy1);
        Ix += (diffy1*diffy1 + diffy1*diffy2 + diffy2*diffy2) * term;
        Iy += (diffx1*diffx1 + diffx1*diffx2 + diffx2*diffx2) * term;
        Pxy += (diffx1*(2.0*diffy1 + diffy2) + diffx2*(diffy1 + 2.0*diffy2)) * term;
        Ix = Ix/12.0;
        Iy = Iy/12.0;
        Pxy = Pxy/24.0;
       
        Preferences.debug("xCentroid = " + xCentroid + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("yCentroid = " + yCentroid + "\n", Preferences.DEBUG_ALGORITHM);
        
        theta = 0.5*Math.atan2(2.0*Pxy, Iy-Ix);
        Preferences.debug("theta = " + (180.0*theta/Math.PI) + "\n", Preferences.DEBUG_ALGORITHM);
        for (numAxis = 0; numAxis <= 1; numAxis++) {
            largestXAboveCentroid = xCentroid;
            smallestXBelowCentroid = xCentroid;
            largestYAboveCentroid = yCentroid;
            smallestYBelowCentroid = yCentroid;
            if (numAxis == 1) {
                if (theta >= 0.0) {
                    theta = theta - Math.PI/2.0;
                }
                else {
                    theta = theta + Math.PI/2.0;
                }
            } // if (numAxis == 1)
            // (y - yCentroid)/(x - xCentroid) = tan(theta)
            // y = yCentroid + tan(theta)*(x - xCentroid)
            // offset = yCentroid - xCentroid*tan(theta)
            // slope = tan(theta)
            // y = x*slope + offset
            slope = Math.tan(theta);
            validSlope = true;
            if ((Double.isInfinite(slope)) || (Double.isNaN(slope) || (Math.abs(slope) > 1.0E6))) {
                validSlope = false;
            }
            offset = yCentroid - xCentroid*slope;
            Preferences.debug("On numAxis = " + numAxis + " slope = " + slope + " offset = " + offset + "\n",
                               Preferences.DEBUG_ALGORITHM);
            // Find 2 farthest intersection points of principal axis with contour
            // Note that axis could intersect contour many times
            numIntersectFound = 0;
            vertexUsed[0] = -1;
            vertexUsed[1] = -1;
            endPoint1[0] = -1;
            endPoint1[1] = -1;
            endPoint2[0] = -1;
            endPoint2[1] = -1;
            intersectX[0] = -1;
            intersectX[1] = -1;
            intersectY[0] = -1;
            intersectY[1] = -1;
            // See if any of the vertices are intersection points
            for (i = 0; i < n; i++) {
                if (i < n-1) {
                    j = i+1;
                }
                else {
                    j = 0;
                }
                if (i > 0) {
                    k = i - 1;
                }
                else {
                    k = n - 1;
                }
                if ((validSlope && (elementAt(i).Y == (float)(slope * elementAt(i).X + offset))) ||
                    (!validSlope && (elementAt(i).X == (float)xCentroid))) {
                    if (validSlope) {
                        if (elementAt(i).X > largestXAboveCentroid) {
                            largestXAboveCentroid = elementAt(i).X;
                            numIntersectFound = 0;
                        }
                        else if (elementAt(i).X < smallestXBelowCentroid) {
                            numIntersectFound = 1;
                            smallestXBelowCentroid = elementAt(i).X;
                        }
                        else {
                            continue;
                        }
                        if (elementAt(j).Y >= slope * elementAt(j).X + offset) {
                            endPoint1[numIntersectFound] = j;
                            endPoint2[numIntersectFound] = k;
                        }
                        else {
                            endPoint2[numIntersectFound] = j;
                            endPoint1[numIntersectFound] = k;
                        }    
                    } // if (validSlope)
                    else { // !validSlope
                        if (elementAt(i).Y > largestYAboveCentroid) {
                            numIntersectFound = 0;
                            largestYAboveCentroid = elementAt(i).Y;
                        }
                        else if (elementAt(i).Y < smallestYBelowCentroid) {
                            numIntersectFound = 1;
                            smallestYBelowCentroid = elementAt(i).Y;
                        }
                        else {
                            continue;
                        }
                        if (elementAt(j).X <= xCentroid) {
                            endPoint1[numIntersectFound] = j;
                            endPoint2[numIntersectFound] = k;
                        }
                        else {
                            endPoint2[numIntersectFound] = j;
                            endPoint1[numIntersectFound] = k;
                        }    
                    } // else !validSlope
                    intersectX[numIntersectFound] = elementAt(i).X;
                    intersectY[numIntersectFound] = elementAt(i).Y;
                    if (numIntersectFound == 0) {
                        if ((i == n-1) && (endPoint1[0] == 0)) {
                            indicesFromEndPoint10Increasing = true;
                        }
                        else if (endPoint1[0] == i+1) {
                            indicesFromEndPoint10Increasing = true;
                        }
                        else {
                            indicesFromEndPoint10Increasing = false;
                        }
                    }
                    vertexUsed[numIntersectFound] = i;
                }
            } // for (i = 0; i < n; i++)
            
            // Look for intersections on the sides
            for (i = 0; i < n; i++) {
               if (i < n-1) {
                   j = i+1;
               }
               else {
                   j = 0;
               }
               segmentSlope= (elementAt(j).Y - elementAt(i).Y)/(elementAt(j).X - elementAt(i).X);
               validSegmentSlope = true;
               segmentOffset = elementAt(i).Y - elementAt(i).X * segmentSlope;
               if ((Double.isInfinite(segmentSlope)) || Double.isNaN(segmentSlope)) {
                   validSegmentSlope = false;
               }
               if (validSlope && validSegmentSlope) {
                   if (slope == segmentSlope) {
                       // Either parallel and no intersection or
                       // intersect all along segment
                       if (offset != segmentOffset) {
                           // Parallel and no intersection
                       }
                       else {
                           // Intersect all along segment
                           // Should have previously found vertex as an intersection point
                       }
                   } // if (slope == segmentSlope)
                   else {
                       xInter = (segmentOffset - offset)/(slope - segmentSlope);
                       yInter = (slope * xInter + offset);
                       if ((xInter <= Math.max(elementAt(i).X, elementAt(j).X) + epsilon) &&
                           (xInter >= Math.min(elementAt(i).X, elementAt(j).X) - epsilon) &&
                           (yInter <= Math.max(elementAt(i).Y, elementAt(j).Y) + epsilon) &&
                           (yInter >= Math.min(elementAt(i).Y, elementAt(j).Y) - epsilon)) {
                           if (xInter > largestXAboveCentroid) {
                               numIntersectFound = 0;
                               largestXAboveCentroid = xInter;
                           }
                           else if (xInter < smallestXBelowCentroid) {
                               numIntersectFound = 1;
                               smallestXBelowCentroid = xInter;
                           }
                           else {
                               continue;
                           }
                           if (elementAt(i).Y >= slope * elementAt(i).X + offset) {
                               endPoint1[numIntersectFound] = i;
                               endPoint2[numIntersectFound] = j;
                           }
                           else {
                               endPoint2[numIntersectFound] = i;
                               endPoint1[numIntersectFound] = j;
                           }
                           if (numIntersectFound == 0) {
                               if ((endPoint2[0] == n-1) && (endPoint1[0] == 0)) {
                                   indicesFromEndPoint10Increasing = true;    
                               }
                               else if (endPoint1[0] == endPoint2[0] + 1) {
                                   indicesFromEndPoint10Increasing = true;
                               }
                               else {
                                   indicesFromEndPoint10Increasing = false;
                               }
                           } // if (numIntersectFound == 0)
                           intersectX[numIntersectFound] = (float)xInter;
                           intersectY[numIntersectFound] = (float)yInter;
                           vertexUsed[numIntersectFound] = -1;
                       }
                   }
                   
               } // if (validSlope && validSegmentSlope)
               else if ((!validSlope) && validSegmentSlope) {
                   if ((xCentroid <= Math.max(elementAt(i).X, elementAt(j).X) + epsilon) &&
                       (xCentroid >= Math.min(elementAt(i).X, elementAt(j).X) - epsilon)) {
                       yInter = xCentroid * segmentSlope + segmentOffset;
                       if (yInter > largestYAboveCentroid) {
                           numIntersectFound = 0;
                           largestYAboveCentroid = yInter;
                       }
                       else if (yInter < smallestYBelowCentroid) {
                           numIntersectFound = 1;
                           smallestYBelowCentroid = yInter;
                       }
                       else {
                           continue;
                       }
                       if (elementAt(i).X <= xCentroid) {
                           endPoint1[numIntersectFound] = i;
                           endPoint2[numIntersectFound] = j;
                       }
                       else {
                           endPoint2[numIntersectFound] = i;
                           endPoint1[numIntersectFound] = j;
                       }
                       if (numIntersectFound == 0) {
                           if ((endPoint2[0] == n-1) && (endPoint1[0] == 0)) {
                               indicesFromEndPoint10Increasing = true;    
                           }
                           else if (endPoint1[0] == endPoint2[0] + 1) {
                               indicesFromEndPoint10Increasing = true;
                           }
                           else {
                               indicesFromEndPoint10Increasing = false;
                           }
                       } // if (numIntersectFound == 0)
                       intersectX[numIntersectFound] = (float)xCentroid;
                       intersectY[numIntersectFound] = (float)yInter;
                       vertexUsed[numIntersectFound] = -1;
                   }
               } // else if ((!validSlope) && validSegmentSlope)
               else if (validSlope && (!validSegmentSlope)) {
                   yInter = slope * elementAt(i).X + offset;
                   if ((yInter <= Math.max(elementAt(i).Y, elementAt(j).Y) + epsilon) &&
                       (yInter >= Math.min(elementAt(i).Y, elementAt(j).Y) - epsilon)) {
                       if (elementAt(i).X > largestXAboveCentroid) {
                           largestXAboveCentroid = elementAt(i).X;
                           numIntersectFound = 0;
                       }
                       else if (elementAt(i).X < smallestXBelowCentroid) {
                           numIntersectFound = 1;
                           smallestXBelowCentroid = elementAt(i).X;
                       }
                       else {
                           continue;
                       }
                       if (elementAt(i).Y >= slope * elementAt(i).X + offset) {
                           endPoint1[numIntersectFound] = i;
                           endPoint2[numIntersectFound] = j;
                       }
                       else {
                           endPoint2[numIntersectFound] = i;
                           endPoint1[numIntersectFound] = j;
                       }
                       if (numIntersectFound == 0) {
                           if ((endPoint2[0] == n-1) && (endPoint1[0] == 0)) {
                               indicesFromEndPoint10Increasing = true;    
                           }
                           else if (endPoint1[0] == endPoint2[0] + 1) {
                               indicesFromEndPoint10Increasing = true;
                           }
                           else {
                               indicesFromEndPoint10Increasing = false;
                           }
                       } // if (numIntersectFound == 0)
                       intersectX[numIntersectFound] = elementAt(i).X;
                       intersectY[numIntersectFound] = (float)yInter;
                       vertexUsed[numIntersectFound] = -1;
                   }
               } // else if (validSlope && (!validSegmentSlope))
            } // for (i = 0; i < n; i++)
            
            Preferences.debug("For numAxis = " + numAxis + " intersectX[0] = " + intersectX[0] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("For numAxis = " + numAxis + " intersectY[0] = " + intersectY[0] + "\n", Preferences.DEBUG_ALGORITHM);
            if (endPoint1[0] >= 0) {
                Preferences.debug("For numAxis = " + numAxis + " endPoint1[0].X = " + elementAt(endPoint1[0]).X + "\n",
                        Preferences.DEBUG_ALGORITHM);
                Preferences.debug("For numAxis = " + numAxis + " endPoint1[0].Y = " + elementAt(endPoint1[0]).Y + "\n",
                        Preferences.DEBUG_ALGORITHM);
	        }
            else {
                Preferences.debug("For numAxis = " + numAxis + " endPoint1[0] = " + endPoint1[0] + "\n", Preferences.DEBUG_ALGORITHM);
            }
            if (endPoint2[0] > 0) {
                Preferences.debug("For numAxis = " + numAxis + " endPoint2[0].X = " + elementAt(endPoint2[0]).X + "\n",
                        Preferences.DEBUG_ALGORITHM);
                Preferences.debug("For numAxis = " + numAxis + " endPoint2[0].Y = " + elementAt(endPoint2[0]).Y + "\n",
                        Preferences.DEBUG_ALGORITHM);
            }
            else {
                Preferences.debug("For numAxis = " + numAxis + " endPoint2[0] = " + endPoint2[0] + "\n", Preferences.DEBUG_ALGORITHM);        
            }
            Preferences.debug("For numAxis = " + numAxis + " intersectX[1] = " + intersectX[1] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("For numAxis = " + numAxis + " intersectY[1] = " + intersectY[1] + "\n", Preferences.DEBUG_ALGORITHM);
            if (endPoint1[1] >= 0) {
                Preferences.debug("For numAxis = " + numAxis + " endPoint1[1].X = " + elementAt(endPoint1[1]).X + "\n",
                    Preferences.DEBUG_ALGORITHM);
                Preferences.debug("For numAxis = " + numAxis + " endPoint1[1].Y = " + elementAt(endPoint1[1]).Y + "\n",
                    Preferences.DEBUG_ALGORITHM);
            }
            else {
                Preferences.debug("For numAxis = " + numAxis + " endPoint1[1] = " + endPoint1[1] + "\n", Preferences.DEBUG_ALGORITHM);    
            }
            if (endPoint2[1] >= 0) {
                Preferences.debug("For numAxis = " + numAxis + " endPoint2[1].X = " + elementAt(endPoint2[1]).X + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("For numAxis = " + numAxis + " endPoint2[1].Y = " + elementAt(endPoint2[1]).Y + "\n", Preferences.DEBUG_ALGORITHM);
            }
            else {
                Preferences.debug("For numAxis = " + numAxis + " endPoint2[1] = " + endPoint2[1] + "\n", Preferences.DEBUG_ALGORITHM);        
            }
            // 2 points at end of axis belong to both groups
            group1Num = 2;
            group2Num = 2;
            for (i = 0; i < n; i++) {
                if ((i != vertexUsed[0]) && (i != vertexUsed[1])) {
                    if (validSlope) {
                        if (elementAt(i).Y >= slope * elementAt(i).X + offset) {
                            isGroup1[i] = true;
                            group1Num++;
                        }
                        else {
                            isGroup1[i] = false;
                            group2Num++;
                        }
                    } // if (validSlope)
                    else { // !validSlope
                        if (elementAt(i).X <= xCentroid) {
                            isGroup1[i] = true;
                            group1Num++;
                        }
                        else {
                            isGroup1[i] = false;
                            group2Num++;
                        }
                    }
                } // if ((i != vertexUsed[0]) && (i != vertexUsed[1]))
            } // for (i = 0; i < n; i++)
            
            group1 = new Vector<Vector3f>();
            group2 = new Vector<Vector3f>();
            group1.add(0,new Vector3f(intersectX[0], intersectY[0], 0.0f));
            group2.add(0,new Vector3f(intersectX[0], intersectY[0], 0.0f));
            if (indicesFromEndPoint10Increasing) {
                if (endPoint1[1] < endPoint1[0]) {
                    i = 1;
                    for (index1 = endPoint1[0]; index1 <= n-1; index1++) {
                        if (isGroup1[index1]) {
                            group1.add(i++,new Vector3f(elementAt(index1).X, elementAt(index1).Y, 0.0f));  
                        }
                    }
                    for (index1 = 0; index1 <= endPoint1[1]; index1++) {
                        if (isGroup1[index1]) {
                            group1.add(i++,new Vector3f(elementAt(index1).X, elementAt(index1).Y, 0.0f));
                        }
                    }
                } // if (endPoint1[1] < endPoint1[0])
                else { // endPoint1[1] > endPoint1[0]
                    for (index1 = endPoint1[0], i = 1; index1 <= endPoint1[1]; index1++) {
                        if (isGroup1[index1]) {
                            group1.add(i++,new Vector3f(elementAt(index1).X, elementAt(index1).Y, 0.0f));
                        }
                    }    
                } // endPoint1[1] > endPoint1[0]
            } // if (indicesFromEndPoint10Increasing)
            else { // indices from end point10 decreasing
                if (endPoint1[1] > endPoint1[0]) {
                    i = 1;
                    for (index1 = endPoint1[0]; index1 >= 0; index1--) {
                        if (isGroup1[index1]) {
                            group1.add(i++,new Vector3f(elementAt(index1).X, elementAt(index1).Y, 0.0f));
                        }
                    }
                    for (index1 = n-1; index1 >= endPoint1[1]; index1--) {
                        if (isGroup1[index1]) {
                            group1.add(i++,new Vector3f(elementAt(index1).X, elementAt(index1).Y, 0.0f));
                        }
                    }
                } // if (endPoint1[1] > endPoint1[0)
                else { // endPoint1[1] < endPoint1[0]
                    for (index1 = endPoint1[0], i = 1; index1 >= endPoint1[1]; index1--) {
                        if (isGroup1[index1]) {
                            group1.add(i++,new Vector3f(elementAt(index1).X, elementAt(index1).Y, 0.0f));
                        }
                    }       
                } // else endPoint1[1] < endPoint1[0]
            } // else indices from end point 10 decreasing
            if (i != group1Num-1) {
                return Double.NaN;
            }
            indicesFromEndPoint20Increasing = !indicesFromEndPoint10Increasing;
            if (indicesFromEndPoint20Increasing) {
                if (endPoint2[1] < endPoint2[0]) {
                    i = 1;
                    for (index2 = endPoint2[0]; index2 <= n-1; index2++) {
                        if (!isGroup1[index2]) {
                            group2.add(i++,new Vector3f(elementAt(index2).X, elementAt(index2).Y, 0.0f)); 
                        }
                    }
                    for (index2 = 0; index2 <= endPoint2[1]; index2++) {
                        if (!isGroup1[index2]) {
                            group2.add(i++,new Vector3f(elementAt(index2).X, elementAt(index2).Y, 0.0f));
                        }
                    }
                } // if (endPoint2[1] < endPoint2[0])
                else { // endPoint2[1] > endPoint2[0]
                    for (index2 = endPoint2[0], i = 1; index2 <= endPoint2[1]; index2++) {
                        if (!isGroup1[index2]) {
                            group2.add(i++,new Vector3f(elementAt(index2).X, elementAt(index2).Y, 0.0f));
                        }
                    }    
                } // endPoint2[1] > endPoint2[0]
            } // if (indicesFromEndPoint20Increasing)
            else { // indices from endPoint20 decreasing
                if (endPoint2[1] > endPoint2[0]) {
                    i = 1;
                    for (index2 = endPoint2[0]; index2 >= 0; index2--) {
                        if (!isGroup1[index2]) {
                            group2.add(i++,new Vector3f(elementAt(index2).X, elementAt(index2).Y, 0.0f)); 
                        }
                    }
                    for (index2 = n-1; index2 >= endPoint2[1]; index2--) {
                        if (!isGroup1[index2]) {
                            group2.add(i++,new Vector3f(elementAt(index2).X, elementAt(index2).Y, 0.0f)); 
                        }
                    }
                } // if (endPoint2[1] > endPoint2[0])
                else { // endPoint2[1] < endPoint2[0]
                    for (index2 = endPoint2[0], i = 1; index2 >= endPoint2[1]; index2--) {
                        if (!isGroup1[index2]) {
                            group2.add(i++,new Vector3f(elementAt(index2).X, elementAt(index2).Y, 0.0f));
                        }
                    }       
                } // else endPoint2[1] < endPoint2[0]
            } // else indices from endPoint20 decreasing
            if (i != group2Num-1) {
                return Double.NaN;
            }
            group1.add(group1Num-1,new Vector3f(intersectX[1], intersectY[1], 0.0f));
            group2.add(group2Num-1,new Vector3f(intersectX[1], intersectY[1], 0.0f));
            
            // For Valid slope
            // x0' = (-slope*slope*x0 + x0 + 2*slope*y0 - 2*slope*offset)/(slope*slope + 1)
            // y0' = (slope*slope*y0 - y0 + 2*slope*x0 + 2*offset)/(slope*slope + 1)
            // For invalid or infinite slope
            // x0' = -x0 + 2*xCentroid
            // y0' = y0
            group1Mirror = new Vector<Vector3f>();
            group2Mirror = new Vector<Vector3f>();
            group1Mirror.add(0,new Vector3f(intersectX[0], intersectY[0], 0.0f));
            group2Mirror.add(0,new Vector3f(intersectX[0], intersectY[0], 0.0f));
            
            slopeSquared = slope * slope;
            denom = slopeSquared + 1.0;
            for (i = 1; i <= group1Num-2; i++) {
                if (validSlope) {
                    mirrorX = (float)((-slopeSquared*group1.get(i).X + group1.get(i).X + 2.0*slope*group1.get(i).Y - 2.0*slope*offset)/denom);
                    mirrorY = (float)((slopeSquared*group1.get(i).Y - group1.get(i).Y + 2.0*slope*group1.get(i).X + 2.0*offset)/denom);
                } // if (validSlope)
                else {
                    mirrorX = (float)(-group1.get(i).X + 2.0 * xCentroid);
                    mirrorY = group1.get(i).Y;
                }
                group1Mirror.add(i,new Vector3f(mirrorX, mirrorY, 0.0f));
            } // for (i = 1; i <= group1Num-2; i++)
            for (i = 1; i <= group2Num-2; i++) {
                if (validSlope) {
                    mirrorX = (float)((-slopeSquared*group2.get(i).X + group2.get(i).X + 2.0*slope*group2.get(i).Y - 2.0*slope*offset)/denom);
                    mirrorY = (float)((slopeSquared*group2.get(i).Y - group2.get(i).Y + 2.0*slope*group2.get(i).X + 2.0*offset)/denom);
                } // if (validSlope)
                else {
                    mirrorX = (float)(-group2.get(i).X + 2.0 * xCentroid);
                    mirrorY = group2.get(i).Y;
                }
                group2Mirror.add(i,new Vector3f(mirrorX, mirrorY, 0.0f));
            } // for (i = 1; i <= group2Num-2; i++)
            group1Mirror.add(group1Num-1,new Vector3f(intersectX[1], intersectY[1], 0.0f));
            group2Mirror.add(group2Num-1,new Vector3f(intersectX[1], intersectY[1], 0.0f));
            contour1 = new VOIContour(fixed, closed, group1);
            mask1 = new BitSet(sliceSize); 
            contour1.setMask(mask1, xDim, yDim, false, VOI.ADDITIVE );
            contour2 = new VOIContour(fixed, closed, group2);
            mask2 = new BitSet(sliceSize);
            contour2.setMask(mask2, xDim, yDim, false, VOI.ADDITIVE);
            contour1Mirror = new VOIContour(fixed, closed, group1Mirror);
            mask1Mirror = new BitSet(sliceSize);
            contour1Mirror.setMask(mask1Mirror, xDim, yDim, false, VOI.ADDITIVE);
            contour2Mirror = new VOIContour(fixed, closed, group2Mirror);
            mask2Mirror = new BitSet(sliceSize);
            contour2Mirror.setMask(mask2Mirror, xDim, yDim, false, VOI.ADDITIVE);
            
            //m1AndM2Mirror = 0;
            m1Only = 0;
            m2MirrorOnly = 0;
            //m2AndM1Mirror = 0;
            m2Only = 0;
            m1MirrorOnly = 0;
            for (i = 0; i < sliceSize; i++) {
                if (mask1.get(i) && mask2Mirror.get(i)) {
                    //m1AndM2Mirror++;    
                }
                else if (mask1.get(i)) {
                    m1Only++;
                }
                else if (mask2Mirror.get(i)) {
                    m2MirrorOnly++;
                }
                if (mask2.get(i) && mask1Mirror.get(i)) {
                    //m2AndM1Mirror++;    
                }
                else if (mask2.get(i)) {
                    m2Only++;
                }
                else if (mask1Mirror.get(i)) {
                    m1MirrorOnly++;
                }
            } // for (i = 0; i < sliceSize; i++)
            asymmetryIndexArray[numAxis] = 100.0 * (m1Only + m2MirrorOnly)/exactTotalPixelCount;
            Preferences.debug("On axis " + numAxis + " asymmetry index = " + asymmetryIndexArray[numAxis] + "\n", Preferences.DEBUG_ALGORITHM);
            alternateAsymmetry = 100.0 * (m2Only + m1MirrorOnly)/exactTotalPixelCount;
            Preferences.debug("On axis " + numAxis + " alternate asymmetry index = " + alternateAsymmetry + "\n",
                               Preferences.DEBUG_ALGORITHM);
            asymmetryIndexArray[numAxis] = Math.min(asymmetryIndexArray[numAxis], alternateAsymmetry);
            // Exact calculation
            VOI voi1 = new VOI((short)0, "voi1", VOI.CONTOUR, -1.0f); 
            voi1.importCurve(contour1);
            VOI voi2Mirror = new VOI((short)1, "voi2Mirror", VOI.CONTOUR, -1.0f);
            voi2Mirror.importCurve(contour2Mirror);
            VOI voiResult = new VOI((short)2, "voiResult", VOI.CONTOUR, -1.0f);
            new GenericPolygonClipper(GenericPolygonClipper.gpc_op.GPC_XOR, voi1, voi2Mirror, voiResult);
            resultCurves = voiResult.getCurves();
            exactXORPixelCount = 0.0;
            for (i = 0; i < resultCurves.size(); i++) {
                numVertices = resultCurves.get(i).size();
                exactContourPixelCount = 0.0;
                for (j = 0; j < numVertices-1; j++) {
                    exactContourPixelCount += (resultCurves.get(i).get(j).X*resultCurves.get(i).get(j+1).Y - 
                                           resultCurves.get(i).get(j+1).X*resultCurves.get(i).get(j).Y);    
                }
                exactContourPixelCount += (resultCurves.get(i).get(numVertices-1).X*resultCurves.get(i).get(0).Y - 
                        resultCurves.get(i).get(0).X*resultCurves.get(i).get(numVertices-1).Y); 
                exactContourPixelCount = 0.5*Math.abs(exactContourPixelCount);
                exactXORPixelCount += exactContourPixelCount;
            }
            asymmetryIndexArray[numAxis] = 100.0 * exactXORPixelCount/exactTotalPixelCount;
            Preferences.debug("On axis " + numAxis + " exact asymmetry index = " + asymmetryIndexArray[numAxis] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (numAxis = 0; numAxis <= 1; numAxis++)
        asymmetryIndex = Math.min(asymmetryIndexArray[0], asymmetryIndexArray[1]);
        return asymmetryIndex;
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
		boolean selfTest = false;
		
		if (selfTest) {
            double alpha;
            double cosalpha;
            double sinalpha;
            double centerX = -100.0;
            double centerY = 123.0;
            majorAxis[0] = 500.0;
            double semiMajor = majorAxis[0]/2.0;
            minorAxis[0] = 300.0;
            double semiMinor = minorAxis[0]/2.0;
            angleAxis[0] = 0.0;
            double c = Math.cos(angleAxis[0]);
            double s = Math.sin(angleAxis[0]);
            nPts = 720;
            xPts = new float[nPts];
            yPts = new float[nPts];
            x = new double[nPts];
            y = new double[nPts];
            for (i = 0; i < 720; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                xPts[i] = (float)(centerX + semiMajor * cosalpha * c - semiMinor * sinalpha * s);
                yPts[i] = (float)(centerY + semiMajor * cosalpha * s + semiMinor * sinalpha * c);
            }  
        }

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

    input axes are diameters
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
	    double semiMajor;
        double semiMinor;
	    boolean selfTest = false;
	    
	    if (selfTest) {
	        double alpha;
	        double cosalpha;
	        double sinalpha;
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
	    semiMajor = majorAxis/2.0;
	    semiMinor = minorAxis/2.0;
	    
	    // First handling the circle case
	    if (Math.abs((majorAxis - minorAxis)/majorAxis) < tolerance) {
	        phiall = new double[n];
	        for (i = 0; i < n; i++) {
	            phiall[i] = Math.atan2((xy.get(i).Y - centerY),(xy.get(i).X - centerX));
	            xyproj[i][0] = semiMajor*Math.cos(phiall[i]) + centerX;
	            xyproj[i][1] = semiMinor*Math.sin(phiall[i]) + centerY;
	            diffx = xy.get(i).X - xyproj[i][0];
	            diffy = xy.get(i).Y - xyproj[i][1];
	            residualSumOfSquares[0] += (diffx*diffx + diffy*diffy);
	        } // for (i = 0; i < n; i++)
	        if (selfTest) {
	            System.out.println("resdiualSumOfSquares = " + residualSumOfSquares[0]);
	        }
	        return;
	    } // if (Math.abs((majorAxis - minorAxis)/majorAxis) < tolerance)
	    
	    // Now dealing with proper ellipses
	    a = semiMajor;
	    b = semiMinor;
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
